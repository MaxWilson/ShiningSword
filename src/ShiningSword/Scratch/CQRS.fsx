#I __SOURCE_DIRECTORY__
#I ".."
#I "..\Core"
#load @"Common.fs"
#load @"CQRS.fs"

type Person = { Name: string; Age: int }
type Employee = { PersonalInfo: Person; Salary: int; Title: string }
type Company = { Name: string; Employees: Map<string, Employee> }

type CompanyMsg =
    | ChangeEmployee of string * (Employee option -> Employee option)
    | ChangeName of string

let updateCompany msg (model: Company) =
    match msg with
    | ChangeEmployee(name, f) ->
        { model with Employees = model.Employees |> Map.change name f }
    | ChangeName(name) ->
        { model with Name = name }
open POC.CQRS
let company = EventSource.Create({ Name = "Acme"; Employees = Map.empty }, updateCompany)
company.Execute(ChangeName("Acme, Inc."))
company.Execute(ChangeEmployee("Bob", fun _ -> { PersonalInfo = { Name = "Bob Zarf"; Age = 49 }; Salary = 100000; Title = "President" } |> Some ))
let giveRaise name amountInPercent =
    ChangeEmployee(name, fun emp ->
        match emp with
        | Some emp -> Some { emp with Salary = emp.Salary + (amountInPercent * emp.Salary / 100) }
        | None -> None)
company.State.Employees["Bob"].Salary
company.Execute(giveRaise "Bob" 50)
company.State.Employees["Bob"].Salary
