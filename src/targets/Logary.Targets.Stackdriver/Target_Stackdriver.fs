/// This module contains targets and configuration for logging to Google's Stackdriver logging and analytics package.
/// Documentation for Stackdriver Logging itself can be found here: https://cloud.google.com/logging/docs/
module Logary.Targets.Stackdriver

#nowarn "44"

open Google.Api
open Google.Api.Gax.Grpc
open Google.Cloud.Logging.Type
open Google.Cloud.Logging.V2
open Google.Protobuf
open Google.Protobuf.Collections
open Google.Protobuf.WellKnownTypes
open Hopac
open Hopac.Infixes
open Logary
open Logary.Configuration
open Logary.Internals
open Logary.Internals.TypeShape.Core
open Logary.Message
open Logary.Target
open Logary.Formatting
open System
open System.Collections.Generic
open System.Numerics
open System.Runtime.CompilerServices

[<assembly:InternalsVisibleTo("Logary.Targets.Stackdriver.Tests")>]
do()

/// A structure that enforces certain labeling invariants around monitored resources
/// TO CONSIDER: add more resource types.
/// SEE: https://cloud.google.com/logging/docs/api/v2/resource-list#service-names
/// SEE: https://cloud.google.com/logging/docs/migration-v2#monitored-resources
type ResourceType =
  /// An API provided by the producer.
  /// `service` (1): The API service name, such as "cloudsql.googleapis.com".
  /// `method` (2): The API method, such as "disks.list".
  /// `version`: The API version, such as "v1".
  /// `location`: The service specific notion of location. This can be the name of a zone, region, or "global".
  | Api of service:string * method:string * version:string * location:string
  /// A virtual machine instance hosted in Google Compute Engine (GCE).
  /// `instance_id` (1): The numeric VM instance identifier assigned by Compute Engine.
  /// `zone`: The Compute Engine zone in which the VM is running.
  | GCEInstance of zone:string * instance:string
  /// A Google Container Engine (GKE) container instance.
  /// `cluster_name` (1): An immutable name for the cluster the container is running in.
  /// `namespace_id` (2): Immutable ID of the cluster namespace the container is running in.
  /// `instance_id`: Immutable ID of the GCE instance the container is running in.
  /// `pod_id`: Immutable ID of the pod the container is running in.
  /// `container_name`: Immutable name of the container.
  /// `zone`: The GCE zone in which the instance is running.
  | Container of clusterName:string
               * namespaceId:string
               * instanceId:string
               * podId:string
               * containerName:string
               * zone:string
  /// A resource type used to indicate that a log is not associated with any specific resource.
  | Global
  /// AppEngine services are isolated units segregated by application module id
  /// and the version of the application
  | AppEngine of moduleId:string * versionId: string
  member x.key =
    match x with
    | Api _ ->
      "api"
    | GCEInstance _ ->
      "gce_instance"
    | Container _ ->
      "container"
    | Global ->
      "global"
    | AppEngine _ ->
      "gae_app"

  /// See https://cloud.google.com/logging/docs/api/v2/resource-list for the list of resources
  /// as well as required keys for each one
  member x.labels projectId =
    dict <|
    ("project_id", projectId) ::
    match x with
    | Api (service, method, version, location) ->
      [ "service", service
        "method", method
        "version", version
        "location", location ]

    | GCEInstance (zone, instance) ->
      [ "zone", zone
        "instance_id", instance ]

    | Container (cluster, ns, instance, pod, container, zone) ->
      [ "cluster_name", cluster
        "namespace_id", ns
        "instance_id", instance
        "pod_id", pod
        "container_name", container
        "zone", zone ]

    | Global ->
      []

    | AppEngine (moduleId, versionId) ->
      [ "module_id", moduleId
        "version_id", versionId ]

  member x.toMonitoredResource project: MonitoredResource =
    let r = MonitoredResource(Type = x.key)
    r.Labels.Add(x.labels project)
    r

  static member createGCEInstance(zone, instance) =
    GCEInstance(zone, instance)

  static member createComputeInstance (zone, instance) =
    GCEInstance (zone, instance)

  static member createContainer(cluster, ns, instance, pod, container, zone) =
    Container(cluster, ns, instance, pod, container, zone)

  static member createAppEngine(moduleId, version) =
    AppEngine(moduleId, version)

type StackdriverConf =
  { /// Google Cloud Project Id
    projectId: string
    /// Name of the log to which to write
    logId: string
    /// Resource currently being monitored
    resource: ResourceType
    /// Additional user labels to be added to the messages
    labels: HashMap<string,string>
    /// Maximum messages to send as a batch.
    maxBatchSize: uint16
    /// Max # of inflight requests
    maxInflight: uint16 }

  static member create (projectId, ?logId, ?resource, ?labels, ?batchSize, ?maxInflight) =
    { projectId = projectId
      logId = defaultArg logId "apps"
      resource = defaultArg resource Global
      labels = defaultArg labels HashMap.empty
      maxBatchSize = defaultArg batchSize 50us
      maxInflight = defaultArg maxInflight 5us }

let empty: StackdriverConf =
  StackdriverConf.create "CHANGE_PROJECT_ID"

module internal Impl =
  open Logary.Internals.Chiron

  type LogLevel with
    member x.toSeverity(): LogSeverity =
      match x with
      | LogLevel.Debug -> LogSeverity.Debug
      | LogLevel.Error -> LogSeverity.Error
      | LogLevel.Fatal -> LogSeverity.Critical
      | LogLevel.Info -> LogSeverity.Info
      | LogLevel.Warn -> LogSeverity.Warning
      | LogLevel.Verbose -> LogSeverity.Debug

  type V = WellKnownTypes.Value

  let rec toValue (json: Json) =
    match json with
    | Json.Number f ->
      match Double.TryParse f with
      | false, _ ->
        V.ForString f
      | true, f ->
        V.ForNumber f
    | Json.String s ->
      V.ForString s
    | Json.True ->
      V.ForBool true
    | Json.False ->
      V.ForBool false
    | Json.Null ->
      V.ForNull ()
    | Json.Array arr ->
      let vs =
        arr
          |> List.fold (fun acc item -> (toValue item) :: acc) []
          |> List.toArray
      V.ForList vs
    | Json.Object jObj ->
      jObj
        |> JsonObject.toMap
        |> Seq.fold (fun (acc: Struct) (KeyValue (k, v)) -> acc.Fields.[k] <- toValue v; acc) (Struct ())
        |> V.ForStruct

  let addToStruct (s: WellKnownTypes.Struct) (k, value: obj): WellKnownTypes.Struct =
    if isNull value then s else
    s.Fields.[k] <- Json.encode value |> toValue
    s

  [<Literal>]
  let MessageKey = "message"
  [<Literal>]
  let ValueKey = "value"

  type Message with
    member x.toLogEntry () =
      // Labels are used in Stackdriver for classification, so lets put context there.
      // They expect only simple values really in the labels, so it's ok to just stringify them
      let labels =
        Message.getOthers x
        |> Seq.map (fun (k, v) -> k, string v)
        |> dict

      let formatted = Logary.MessageWriter.verbatim.format x

      let addMessageFields (values: seq<string * obj>): seq<_> =
        seq {
          yield MessageKey, box formatted
          yield ValueKey, box x.value
          yield! values
        }

      let payloadFields =
        Message.getAllFields x
        |> addMessageFields
        |> Seq.fold addToStruct (WellKnownTypes.Struct())

      let entry = LogEntry(Severity = x.level.toSeverity(), JsonPayload = payloadFields)
      //entry.
      entry.Timestamp <- Timestamp.FromDateTime (DateTime.ofEpoch x.timestamp)
      entry.Labels.Add labels
      entry

  module Bound =
    type private BoundMessage =
      | Acquire of replyCh: Ch<unit> * nack: Promise<unit>
      | Release

    type T = private { commCh: Ch<BoundMessage> }

    let create (maxPar: uint32) =
      let comm = Ch ()
      let q = Queue<Ch<unit> * Promise<unit>>()
      let iter =
        Job.iterateServer 0u <| fun inflight ->
        comm ^=> function
          | Acquire (replyCh, nack) when inflight = maxPar ->
            printfn "Acquire, Inflight=%i (maxPar), q=%i" inflight q.Count
            q.Enqueue (replyCh, nack)
            Alt.always inflight

          | Acquire (replyCh, nack) ->
            printfn "Acquire, Inflight=%i, q=%i" inflight q.Count
            Alt.choosy [| replyCh *<- () ^->. inflight + 1u
                          nack ^->. inflight |]

          | Release when inflight <> 0u && q.Count > 0 ->
            printfn "Release (inflight <> 0, q <> 0), q=%i" q.Count
            let replyCh, nack = q.Dequeue()
            Alt.choosy [| replyCh *<- () ^->. inflight
                          nack ^->. inflight - 1u |]

          | Release when inflight <> 0u ->
            printfn "Release (inflight <> 0), q=%i" q.Count
            Alt.always (inflight - 1u)

          | Release ->
            failwithf "ERROR Release, Inflight=%i, q=%i" inflight q.Count

      iter >>-. { commCh = comm }

    let acquire x =
      x.commCh *<+->- fun replyCh nack -> Acquire (replyCh, nack)

    let release x =
      x.commCh *<- Release

  let collect (messages: _) =
    // Make a single pass over the array, accumulating the entries, acks and flushes.
    messages |> Array.fold (fun (entries, acks, flushes) -> function
      | Log (message, ack) ->
        message.toLogEntry() :: entries,
        ack *<= () :: acks,
        flushes
      | Flush (ackCh, nack) ->
        entries,
        acks,
        ackCh *<= () :: flushes)
      ([], [], [])

  type State =
    { logName: string
      logger: LoggingServiceV2Client
      resource: MonitoredResource
      labels: IDictionary<string, string>
      cancellation: System.Threading.CancellationTokenSource
      /// used as a wrapper around the above cancellation token
      callSettings: CallSettings
      bound: Bound.T }

    static member create (conf: StackdriverConf) bound =
      let source = new System.Threading.CancellationTokenSource()
      { logger = LoggingServiceV2Client.Create()
        resource = conf.resource.toMonitoredResource conf.projectId
        labels = conf.labels |> HashMap.toDictionary
        logName = sprintf "projects/%s/logs/%s" conf.projectId (System.Net.WebUtility.UrlEncode conf.logId)
        cancellation = source
        callSettings = CallSettings.FromCancellationToken(source.Token)
        bound = bound}

  let writeBatch (state: State) (entries: LogEntry list): Job<unit> =
    Job.Scheduler.isolate (fun () ->
      let request = WriteLogEntriesRequest(LogName = state.logName, Resource = state.resource)
      request.Entries.AddRange entries
      request.Labels.Add state.labels
      state.logger.WriteLogEntries(request, state.callSettings)
      |> ignore)

  let loop (conf: StackdriverConf) (api: TargetAPI) =
    let logger = api.runtime.logger

    logger.info (
      eventX "Started Stackdriver target with project {projectId}, writing to {logName}"
      >> setField "projectId" conf.projectId
      >> setField "logName" conf.logId)

    let shutdown (state: State) =
      api.shutdownCh ^=> fun ack ->
        logger.verbose (eventX "Shutting down Stackdriver target.")
        state.cancellation.Cancel()
        ack *<= () :> Job<_>

    let takeAndWriteBatch (state: State): Alt<State> =
      RingBuffer.takeBatch conf.maxBatchSize api.requests ^=> fun messages ->
        let entries, acks, flushes = collect messages

        let doWrite =
          logger.timeJob (
            writeBatch state entries,
            measurement="writeBatch",
            transform=(setValue "Writing {count} messages." >> setField "count" entries.Length))

        let doAcks =
          logger.verboseWithBP (eventX "Acking messages.")
          >>=. Job.conIgnore acks

        let onDone =
          Bound.release state.bound

        Job.start (doWrite >>=. doAcks >>=. Job.conIgnore flushes >>=. onDone)
        >>-. state

    let rec create (): Job<unit> =
      Bound.create (uint32 conf.maxInflight)
      >>- State.create conf
      >>= loop

    and loop (state: State): Job<unit> =
      let alternative =
            shutdown state
        <|> Bound.acquire state.bound ^=> fun () ->
            Alt.choose [
              takeAndWriteBatch state ^=> loop
              timeOutMillis 10000 ^=> fun () ->
                Bound.release state.bound ^=> fun () ->
                loop state
            ]
      alternative :> Job<_>

    create ()

/// Create a new StackDriver target
[<CompiledName "Create">]
let create conf name =
  { TargetConf.createSimple (Impl.loop conf) name with
      bufferSize = 2048us }

type Builder(conf, callParent: Target.ParentCallback<Builder>) =

  /// Assign the projectId for this logger
  member x.ForProject(project) =
    !(callParent <| Builder({ conf with projectId = project }, callParent))

  /// Name the feed that this logger will log to
  member x.Named(logName) =
    !(callParent <| Builder({ conf with logId = logName }, callParent))

  /// Assign a Monitored Resource to this logger
  member x.ForResource(resource) =
    !(callParent <| Builder({ conf with resource = resource }, callParent))

  member x.WithLabel(key, value) =
    !(callParent <| Builder({ conf with labels = conf.labels |> HashMap.add key value }, callParent))

  member x.WithLabels(labels: IDictionary<_,_>) =
    let labels' = labels |> Seq.fold (fun hm (KeyValue (key, value)) -> hm |> HashMap.add key value) conf.labels
    !(callParent <| Builder({ conf with labels = labels' }, callParent))

  member x.BatchesOf(size) =
    !(callParent <| Builder({ conf with maxBatchSize = size }, callParent))

  // c'tor, always include this one in your code
  new(callParent: Target.ParentCallback<_>) =
    Builder(empty, callParent)

  interface Target.SpecificTargetConf with
    member x.Build name = create conf name
