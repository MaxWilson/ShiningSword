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
open POC
let company = CQRS.Create({ Name = "Acme"; Employees = Map.empty }, updateCompany)
company.Execute(ChangeName("Acme, Inc."))
company.Execute(ChangeEmployee("Bob", fun _ -> { PersonalInfo = { Name = "Bob Zarf"; Age = 49 }; Salary = 100000; Title = "President" } |> Some ))
let getSalary name (company: Company) = company.Employees[name].Salary
let setSalary name salary =
    ChangeEmployee(name, fun emp ->
        match emp with
        | Some emp -> Some { emp with Salary = salary }
        | None -> None)
let giveRaise name amountInPercent =
    ChangeEmployee(name, fun emp ->
        match emp with
        | Some emp -> Some { emp with Salary = emp.Salary + (amountInPercent * emp.Salary / 100) }
        | None -> None)
company.State.Employees["Bob"].Salary
getSalary "Bob" company.State
let givePercentageRaise name amountInPercent (company:CQRS<_,Company>)=
    let salary = getSalary name company.State
    company.Execute(setSalary name (salary + (amountInPercent * salary / 100)))
givePercentageRaise "Bob" 50 company
getSalary "Bob" company.State
let hireIntern name age =
    ChangeEmployee(name, fun _ -> { PersonalInfo = { Name = name; Age = age }; Salary = 5000; Title = "Intern" } |> Some )
company.Execute (hireIntern "Alice" 22)
company.Execute (hireIntern "Jared" 19)
company |> givePercentageRaise "Jared" -20
getSalary "Jared" company.State
getSalary "Alice" company.State
