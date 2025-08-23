module DrizzleCarton.HTTPWebService.Entry

open DrizzleCarton.Application.Entry
open DrizzleCarton.Application.EntryRepositoryContract
open DrizzleCarton.Application.User
open DrizzleCarton.Application.UserRepositoryContract
open DrizzleCarton.Model
open DrizzleCarton.Model.Validation

open Giraffe

type EntryDTO = int * string * int option * string * int

type NestedEntriesDTO =
  { Entry: EntryDTO
    SubEntries: NestedEntriesDTO option list option }

let rec buildEntryDTO (entryRepo: IEntryRepository) (entry: Entry) : NestedEntriesDTO option =
  let entryDTO = Entry.toRawTuple entry
  let rawId, _, _, _, _ = entryDTO

  match getSubEntries entryRepo rawId with
  | SubEntriesFound subEntries ->
    { Entry = entryDTO
      SubEntries = subEntries |> List.map (buildEntryDTO entryRepo) |> Some }
    |> Some
  | ZeroSubEntries -> { Entry = entryDTO; SubEntries = None } |> Some
  | GetSubEntriesResult.InvalidIdError _ -> None
  | GetSubEntriesResult.DataRetrievingError _ -> None

let getAllEntriesOfUser (rawUserId: int) : HttpHandler =
  fun next ctx ->
    task {
      let entryRepo = ctx.GetService<IEntryRepository>()
      let userRepo = ctx.GetService<IUserRepository>()

      // validate user id
      let userId =
        match UserId.ofRaw rawUserId with
        | Error(ValidationError msg) -> failwith msg // TODO: send proper failure response
        | Ok id -> id

      match userRepo.FindUserById userId with
      //| Error _ -> return! RequestErrors.NOT_FOUND (sprintf "%i" userId) next ctx
      | Error(ModelValidationError msg) -> failwith msg // TODO: send proper failure response
      | Error(ReadUserFailure.DataAccessError msg) -> failwith msg // TODO: send proper failure response
      | Ok None -> failwith msg // TODO: send proper failure response
      | Ok(Some user) ->
        let _, _, _, userRoot = User.toTuple user
        let userRootId = UserRoot.toRaw userRoot

        match entryRepo.FindEntryById userRootId with
        //| Error e ->
        //  return!
        //    RequestErrors.NOT_FOUND
        //      (sprintf "Root folder for %s(%i) not found: %A" user.Username user.RootFolder e)
        //      next
        //      ctx
        | Error(ReadEntryFailure.ModelValidationError msg) -> failwith msg // TODO: send proper failure response
        | Error(ReadEntryFailure.DataAccessError msg) -> failwith msg // TODO: send proper failure response
        | Ok None -> failwith msg // TODO: send proper failure response
        | Ok(Some root) ->
          let response = buildEntryDTO entryRepo root
          return! json response next ctx
    }
