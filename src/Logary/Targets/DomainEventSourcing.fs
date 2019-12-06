module DomainEventSourcing

type EventSourcingEvent =
    {
        Type: string
        Data: byte []
    }
