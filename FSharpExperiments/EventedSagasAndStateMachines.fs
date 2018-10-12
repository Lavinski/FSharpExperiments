module EventedSagasAndStateMachines

open System

// ExternalEvent State -> InternalEvent Command

type ExternalEvents =
    | CompanyRegistrationReceived
    | RegistrationApproved
    | CompanyEnrolled

type ExternalCommands =
    | EnrollCompany

type ProcessEvents =
    | ReceivedRegistration
    | ReceivedApproval
    | Enrolled

type ProcessState =
    | WaitingForRegistration
    | WaitingForApproval
    | WaitingForEnrollment
    | Completed

let reactTo state externalEvent =
    match state, externalEvent with
    | WaitingForRegistration, CompanyRegistrationReceived -> [ReceivedRegistration], []
    | WaitingForApproval, RegistrationApproved -> [ReceivedApproval], [EnrollCompany]
    | WaitingForEnrollment, CompanyEnrolled -> [Enrolled], []
    | _ -> failwithf "Event %A could not be handled in state %A" externalEvent state

let apply state event =
    match event with
    | ReceivedRegistration -> WaitingForApproval
    | ReceivedApproval -> WaitingForEnrollment
    | Enrolled -> Completed

let flip f x y = f y x

let hydrate events =
    List.fold apply WaitingForRegistration events

let execute events externalEvent =
    hydrate events |> (flip reactTo) externalEvent


type State2 =
    | First
    | Second

type ExternalOne =
    | EventA of Guid

type ExternalTwo =
    | EventB of Guid

type ExternalEvent =
    | ExternalOneEventA
    | ExternalTwoEventB
    | Ignore

let conform (externalEvent: obj) = 
    match externalEvent with
    | (:? ExternalOne as x) when (match x with | EventA id -> true | _ -> false) -> (id, ExternalOneEventA) |> Some
    | (:? ExternalTwo as x) when (match x with | EventB id -> true | _ -> false) -> (id, ExternalTwoEventB) |> Some
    | _ -> None

//let external = conform externalEvent

let reactTo2 (external: ExternalEvent) (state: State2) =
    match state, external with
    | First, ExternalOneEventA -> [ReceivedRegistration], []
    | Second, ExternalTwoEventB -> [ReceivedApproval], [EnrollCompany]
    | _ -> failwithf "Event %A could not be handled in state %A" external state
