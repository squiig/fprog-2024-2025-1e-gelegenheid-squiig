module DrizzleCarton.HTTPWebService.Entry

open Giraffe

type EntryDTO = int * string * Option<int> * string * int

type NestedEntriesDTO =
  { Entry: EntryDTO
    Subentries: List<NestedEntriesDTO> }

let rec buildEntryResponse (db: Database) (entry: EntryDTO) : NestedEntriesDTO =
  { Entry = entry
    Subentries = Entry.fetchSubentries db entry.Id |> List.map (buildEntryResponse db) }

let getAllEntries (userId: int) : HttpHandler =
  fun next ctx ->
    task {
      let db = ctx.GetService<Database>()

      match User.fetch db userId with
      | Error _ -> return! RequestErrors.NOT_FOUND (sprintf "%i" userId) next ctx
      | Ok user ->
        match Entry.fetch db user.RootFolder with
        | Error e ->
          return!
            RequestErrors.NOT_FOUND
              (sprintf "Root folder for %s(%i) not found: %A" user.Username user.RootFolder e)
              next
              ctx
        | Ok root ->
          let response = buildEntryResponse db root
          return! json response next ctx
    }
