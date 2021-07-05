namespace Myriadic

open System
open FSharp.Compiler.SyntaxTree
open FSharp.Compiler.XmlDoc
open FsAst
open Myriad.Core
open FSharp.Compiler.Range

[<RequireQualifiedAccess>]
module Generator =
    /// Instructs to generate lenses for each property of the record
    type LensAttribute(wrapperName : string) =
        inherit Attribute()
        let mutable _wrapperName = wrapperName
        member this.WrapperName = _wrapperName
        new () = LensAttribute(null : string)
        new (``type``: Type) = LensAttribute(``type``.Name)

module internal CreateLenses =
    let r = range0

    let private wrap get set (wrapperName : Option<string>) =
        let name =
            match wrapperName with
            | Some s when not <| String.IsNullOrWhiteSpace(s) -> s
            | _ -> "Optics.lens"
        let lensMaker = SynExpr.CreateLongIdent (false, LongIdentWithDots (Ident.CreateLong name, []), None)
        let inner =
            SynExpr.App (ExprAtomicFlag.NonAtomic, false, lensMaker, SynExpr.CreateParen get, r)
        SynExpr.App (ExprAtomicFlag.NonAtomic, false, inner, SynExpr.CreateParen set, r)

    let private createLensForRecordField (parent: LongIdent) (wrapperName : Option<string>) (field: SynField) =
        let field = field.ToRcd
        let fieldName = match field.Id with None -> failwith "no field name" | Some f -> f

        let recordType =
            LongIdentWithDots.Create (parent |> List.map (fun i -> i.idText))
            |> SynType.CreateLongIdent

        let pattern =
            let name = LongIdentWithDots.CreateString (fieldName.idText + "_")
            SynPatRcd.CreateLongIdent(name, [])

        let expr =
            let srcVarName = "data"
            let srcIdent = Ident.Create srcVarName

            // data.Property
            let getBody = LongIdentWithDots.Create [srcVarName; fieldName.idText]
            let recordArg = SynSimplePat.Typed(SynSimplePat.Id (srcIdent, None, false, false, false, r), recordType, r)
            // (data : Record)
            let getArgs = SynSimplePats.SimplePats ([recordArg], r)
            // fun (data : Record) -> data.Property
            let get = SynExpr.Lambda (false, false, getArgs, SynExpr.CreateLongIdent(false, getBody, None), None, r)

            let valueIdent = Ident.Create "value"
            let valuePattern = SynSimplePat.Typed(SynSimplePat.Id (valueIdent, None, false, false, false, r), field.Type, r)
            // (value : PropertyType)
            let valueArgPatterns = SynSimplePats.SimplePats ([valuePattern], r)
            let copySrc = SynExpr.CreateLongIdent(false, LongIdentWithDots.Create [srcVarName], None)
            let recordToUpdateName : RecordFieldName = (LongIdentWithDots.CreateString fieldName.idText, true)
            // { data with Property = value }
            let recordUpdate =
                SynExpr.CreateRecordUpdate (copySrc, [(recordToUpdateName, SynExpr.Ident valueIdent |> Some, None)])

            // (value : PropertyType) -> { data with Property = value }
            let innerLambdaWithValue =
                SynExpr.Lambda (false, true, getArgs, recordUpdate, None, r)

            // fun (data : Record) (value : PropertyType) -> { data with Property = value }
            let set =
                SynExpr.Lambda (false, true, valueArgPatterns, innerLambdaWithValue, None, r)

            wrap get set wrapperName

        SynModuleDecl.CreateLet [{SynBindingRcd.Let with
                                    Pattern = pattern
                                    Expr = expr }]

    let private createLensForDU (requiresQualifiedAccess : bool) (parent: LongIdent) (wrapperName : Option<string>) (du : SynUnionCase) =
        let duRCD = du.ToRcd
        let singleCase =
            match duRCD.Type with
            | UnionCaseFields ([singleCase]) -> singleCase
            | UnionCaseFields (_ :: _) -> failwith "It is impossible to create a lens for a DU with several cases"
            | _ -> failwithf "Unsupported type"

        let duType =
            LongIdentWithDots.Create (parent |> List.map (fun i -> i.idText))
            |> SynType.CreateLongIdent

        let getterName = Ident("getter", range.Zero)
        let pattern =
            SynPatRcd.CreateLongIdent(LongIdentWithDots.CreateString "lens", [])

        let matchCaseIdentParts =
            if requiresQualifiedAccess then
                (parent |> List.map (fun i -> i.idText)) @ [duRCD.Id.idText]
            else
                [duRCD.Id.idText]

        // The name of the DU case, optionally preceded by the name of the DU itself, if
        // fully qualified access is required
        let fullCaseName = LongIdentWithDots.Create matchCaseIdentParts

        let lensExpression =
            let matchCase =
                let caseVariableName = "x"
                let args = [SynPatRcd.CreateLongIdent (LongIdentWithDots.CreateString caseVariableName, [])]
                let matchCaseIdent = SynPatRcd.CreateLongIdent(fullCaseName, args)

                let rhs = SynExpr.CreateIdent (Ident.Create caseVariableName)
                SynMatchClause.Clause(matchCaseIdent.FromRcd, None, rhs, range.Zero, DebugPointForTarget.No)

            let getterArgName = "x"
            let matchOn =
                let ident = LongIdentWithDots.CreateString getterArgName
                SynExpr.CreateLongIdent(false, ident, None)

            let matchExpression = SynExpr.Match(NoDebugPointAtLetBinding, matchOn, [matchCase], range.Zero)

            let setter =
                let valueIdent = Ident.Create "value"
                let valuePattern = SynSimplePat.Typed(SynSimplePat.Id (valueIdent, None, false, false, false, r), singleCase.ToRcd.Type, r)
                let valueArgPatterns = SynSimplePats.SimplePats ([valuePattern], r)

                let duType =
                    LongIdentWithDots.Create (parent |> List.map (fun i -> i.idText))
                    |> SynType.CreateLongIdent

                let createCase =
                    SynExpr.App (ExprAtomicFlag.NonAtomic, false, SynExpr.LongIdent (false, fullCaseName, None, r), SynExpr.Ident valueIdent, r)
                let recordArg = SynSimplePat.Typed(SynSimplePat.Id (Ident.Create "_", None, false, false, false, r), duType, r)
                let getArgs = SynSimplePats.SimplePats ([recordArg], r)
                let innerLambdaWithValue =
                    SynExpr.Lambda (false, true, getArgs, createCase, None, r)

                SynExpr.Lambda (false, true, valueArgPatterns, innerLambdaWithValue, None, r)

            let get = SynExpr.MatchLambda(false, r, [matchCase], NoDebugPointAtLetBinding, range.Zero)

            wrap get setter wrapperName

        SynModuleDecl.CreateLet [{ SynBindingRcd.Let with
                                      Pattern = pattern
                                      Expr = lensExpression }]
    let private updateLastItem list updater =
        let folder item state =
            match state with
            | [] -> [updater item]
            | l -> item :: l

        List.foldBack folder list []

    let private (|LongIdentLid|) (ident : LongIdentWithDots) =
        ident.Lid

    let private (|SynTypeAppTypeName|_|) (expr : SynType) =
        match expr with
        | SynType.App (name, _, _, _, _, _, _) -> Some name
        | _ -> None

    let createLensModule (namespaceId: LongIdent) (typeDefn: SynTypeDefn, attr: SynAttribute) =
        let (TypeDefn(synComponentInfo, synTypeDefnRepr, _members, _range)) = typeDefn
        let (ComponentInfo(_attributes, _typeParams, _constraints, recordId, _doc, _preferPostfix, _access, _range)) = synComponentInfo

        let moduleIdent = updateLastItem recordId (fun i -> Ident.Create i.idText)

        let wrapperName =
            match attr.ArgExpr with
            | SynExpr.Const (SynConst.Unit, _) -> None
            | SynExpr.Paren(SynExpr.Const ((SynConst.String(s, _)), _), _leftParenRange, _rightParenRange, _range) -> Some s
            | SynExpr.Paren(SynExpr.TypeApp (SynExpr.Ident ident, _, [SynTypeAppTypeName(SynType.LongIdent (LongIdentLid (wrapperIdent :: _)))], _, _, _, _), _, _, _)
                when ident.idText = "typedefof" || ident.idText = "typeof" ->
                Some wrapperIdent.idText
            | _ -> failwithf "Unsupported syntax of specifying the wrapper name for type %A." recordId

        let openParent = SynModuleDecl.CreateOpen (SynOpenDeclTarget.ModuleOrNamespace(namespaceId, r))
        let moduleInfo = SynComponentInfoRcd.Create moduleIdent

        match synTypeDefnRepr with
        | SynTypeDefnRepr.Simple(SynTypeDefnSimpleRepr.Record(_accessibility, recordFields, _recordRange), _range) ->
            let fieldLenses = recordFields |> List.map (createLensForRecordField recordId wrapperName)

            let declarations = [
                yield openParent
                yield! fieldLenses
            ]

            SynModuleDecl.CreateNestedModule(moduleInfo, declarations)
        | SynTypeDefnRepr.Simple(SynTypeDefnSimpleRepr.Union(_accessibility, [singleCase], _recordRange), _range) ->
            let requiresQualifiedAccess = Ast.getAttribute<RequireQualifiedAccessAttribute> typeDefn |> Option.isSome
            let lens = createLensForDU requiresQualifiedAccess recordId wrapperName singleCase

            let declarations = [
                openParent
                lens
            ]

            SynModuleDecl.CreateNestedModule(moduleInfo, declarations)
        | _ -> failwithf "%A is not a record type." recordId

[<MyriadGenerator "lenses1">]
type LensesGenerator() =
    interface IMyriadGenerator with
        member this.ValidInputExtensions: string seq = seq { ".fs"; ".fsx" }
        member __.Generate(ctx) =
            let ast =
                Ast.fromFilename ctx.InputFilename
                |> Async.RunSynchronously
                |> Array.head
                |> fst
            let namespaceAndRecords = Ast.extractRecords ast
            let openDecls = [
                // sorry! This is a domain-specific hack! TODO: derive these properly, i.e. automatically.
                SynModuleDecl.CreateOpen "AutoWizard"
                SynModuleDecl.CreateOpen "Domain.Model"
                SynModuleDecl.CreateOpen "Domain.Model.Character"
                ]
            let recordsModules =
                namespaceAndRecords
                |> List.collect (
                    fun (ns, records) ->
                    records
                    |> List.choose (fun r ->
                        let attr = Ast.getAttribute<Generator.LensAttribute> r
                        Option.map (fun a -> r, a) attr)
                    |> List.map (CreateLenses.createLensModule ns))

            let namespaceAndDUs = Ast.extractDU ast
            let duModules =
                namespaceAndDUs
                |> List.collect (
                    fun (ns, dus) ->
                    dus
                    |> List.choose (fun du ->
                        let attr = Ast.getAttribute<Generator.LensAttribute> du
                        Option.map (fun a -> du, a) attr)
                    |> List.map (CreateLenses.createLensModule ns))

            let namespaceOrModule =
                { SynModuleOrNamespaceRcd.CreateNamespace(Ident.CreateLong "AutoGen")
                    with
                        IsRecursive = true
                        Declarations = openDecls @ recordsModules @ duModules

                        }

            [namespaceOrModule]
