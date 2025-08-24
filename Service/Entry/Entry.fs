module DrizzleCarton.HTTPWebService.Entry

open DrizzleCarton.Application
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

      match UserId.ofRaw rawUserId with
      | Error(Validation.ValidationError msg) ->
        return! RequestErrors.UNPROCESSABLE_ENTITY $"Provided user id is not valid! %s{msg}" next ctx
      | Ok userId ->
        match User.findById userRepo userId with
        | FindByIdResult.DataRetrievingError msg ->
          return! ServerErrors.INTERNAL_ERROR $"Error: User data could not be retrieved. %s{msg}" next ctx
        | NotFound -> return! RequestErrors.NOT_FOUND "No user found by this id." next ctx
        | Found user ->
          let _, _, _, userRoot = User.toTuple user
          let userRootEntry = UserRoot.toRaw userRoot
          let response = buildEntryDTO entryRepo userRootEntry
          return! json response next ctx
    }
