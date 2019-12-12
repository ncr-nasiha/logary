module Program

open System
open Logary
open Logary.Targets
open Logary.Tests
open Expecto

let tests =
  test "A simple test" {
    let subject = "Hello World"
    Expect.equal subject "Hello World" "The strings should equal"
  }

[<EntryPoint>]
let main args =
  runTestsWithArgs defaultConfig args tests

//let target =
//  EventStore.ClientAPI { isYes = bool }

//let exnMsg =
//  Message.event Error "Example message with exception"
//  |> withException Message.addExn

//[<Tests>]
//let tests =
//  testList "eventstore tests" [
//    TargetBaseline.basicTests "eventstore" target true

//    testList "getType" [
//      testCase "of non-exception message" <| fun _ ->
//        let msg = Message.event Info "User signed up" |> Message.setNameStr "A.B"
//        let typ = EventStore.Impl.getType msg
//        Expect.equal typ "A.B" "Should have name of Message as type"

//      testCase "of message with exception" <| fun _ ->
//        let typ = EventStore.Impl.getType exnMsg
//        Expect.equal typ "System.Exception" "Should have exception type as type"

//      testCase "formatting message captures exception details" <| fun _ ->
//        let str = EventStore.Impl.format exnMsg
//        Expect.stringContains str "middleWay" "Should contain parts of StackTrace."
//    ]
//  ]

//[<EntryPoint>]
//let main argv =
//  Tests.runTestsInAssembly defaultConfig argv