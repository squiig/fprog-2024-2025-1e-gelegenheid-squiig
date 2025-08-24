module DrizzleCarton.RAMDataAccess.Entry

open DrizzleCarton.Application
open DrizzleCarton.Application.EntryRepositoryContract
open DrizzleCarton.Application.ResultHelper
open DrizzleCarton.Model
open DrizzleCarton.RAMDataAccess

let entryPersistence: IEntryRepository =
  { new IEntryRepository with
      member this.StoreEntry(entryData: EntryData) : Result<EntryId, WriteEntryFailure> =
        match RAMDataAccess.addEntry RAMDataAccess.defaultDb (Entry.EntryData.toRawTuple entryData) with
        | Error _ -> Error(WriteEntryFailure.DataAccessError $"Unknown error while trying to store new Entry!")
        | Ok entry ->
          let rawId, _, _, _, _ = entry

          EntryId.ofRaw rawId
          |> Result.mapError (function
            | Validation.ValidationError msg -> WriteEntryFailure.UnexpectedResultError msg)

      member this.GetSubEntries(id: EntryId) : Result<Entry list, ReadEntryFailure> =
        let rawId = EntryId.toRaw id

        match RAMDataAccess.subEntries RAMDataAccess.defaultDb rawId with
        | Error _ ->
          Error(ReadEntryFailure.DataAccessError $"Unknown error while requesting sub-entries for entry id %d{rawId}")
        | Ok entries ->
          entries
          |> List.map Entry.ofRaw
          |> sequenceResult
          |> Result.mapError (function
            | Validation.ValidationError msg -> ReadEntryFailure.ModelValidationError msg)

      member this.FindEntryById(id: EntryId) : Result<Entry option, ReadEntryFailure> =
        match RAMDataAccess.entry RAMDataAccess.defaultDb (EntryId.toRaw id) with
        | Error(NotFound _) -> Ok None
        | Ok rawEntry ->
          Entry.ofRaw rawEntry
          |> Result.map (fun e -> Some e)
          |> Result.mapError (function
            | Validation.ValidationError msg -> ReadEntryFailure.ModelValidationError msg)

  }
