module DrizzleCarton.HTTPWebService.Entry

open DrizzleCarton.Application
open DrizzleCarton.Application.Entry
open DrizzleCarton.Application.EntryRepositoryContract
open DrizzleCarton.Application.User
open DrizzleCarton.Application.UserRepositoryContract
open DrizzleCarton.Model
open DrizzleCarton.Model.Validation

open Giraffe

open Thoth.Json.Net

type private EntryDTO = int * string * int option * string * int

type private NestedEntriesDTO =
  { Entry: EntryDTO
    SubEntries: NestedEntriesDTO option list option }

let rec private buildEntryDTO (entryRepo: IEntryRepository) (entry: Entry) : NestedEntriesDTO option =
  let entryDTO = Entry.toRawTuple entry

  match getSubEntries entryRepo (Entry.getId entry) with
  | SubEntriesFound subEntries ->
    { Entry = entryDTO
      SubEntries = subEntries |> List.map (buildEntryDTO entryRepo) |> Some }
    |> Some
  | GetSubEntriesResult.ZeroSubEntries -> { Entry = entryDTO; SubEntries = None } |> Some
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
        | FindByIdResult.Found user ->
          let _, _, _, userRoot = User.toTuple user
          let userRootEntry = UserRoot.toRaw userRoot
          let response = buildEntryDTO entryRepo userRootEntry
          return! json response next ctx
    }

type CreateSubFolderRequestDTO = { Name: string; ParentFolderId: int }

let createSubFolder: HttpHandler =
  fun next ctx ->
    task {
      let entryRepo = ctx.GetService<IEntryRepository>()
      let! folderDTO = ctx.BindJsonAsync<CreateSubFolderRequestDTO>()

      match EntryName.ofRaw folderDTO.Name with
      | Error(Validation.ValidationError msg) ->
        return! RequestErrors.UNPROCESSABLE_ENTITY $"Error: Could not create folder, name invalid! %s{msg}" next ctx
      | Ok folderName ->
        match EntryParent.ofRaw (Some folderDTO.ParentFolderId) with
        | Error(Validation.ValidationError msg) ->
          return!
            RequestErrors.UNPROCESSABLE_ENTITY $"rror: Could not create folder, parent id invalid! %s{msg}" next ctx
        | Ok parent ->
          match Entry.createSubFolder entryRepo folderName parent with
          | CreateResult.DataStoringError msg ->
            return! ServerErrors.INTERNAL_ERROR $"Error: Could not store folder! %s{msg}" next ctx
          | InvalidEntryError msg ->
            return!
              RequestErrors.UNPROCESSABLE_ENTITY
                $"Could not create folder, combination of input data invalid! %s{msg}"
                next
                ctx
          | CreateResult.Stored folder -> return! Successful.CREATED (json folder) next ctx
    }
