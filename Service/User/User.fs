module DrizzleCarton.HTTPWebService.User

open DrizzleCarton.Model
open DrizzleCarton.Application
open DrizzleCarton.Application.Common
open DrizzleCarton.Application.Entry
open DrizzleCarton.Application.EntryRepositoryContract
open DrizzleCarton.Application.User
open DrizzleCarton.Application.UserRepositoryContract

open Giraffe

open Thoth.Json.Net

let mapUserId rawId =
  UserId.ofRaw rawId
  |> Result.mapError (function
    | Validation.ValidationError msg -> Message $"Error: Provided user id is not valid! %s{msg}")

let getAllUsers: HttpHandler =
  fun next ctx ->
    let userRepo = ctx.GetService<IUserRepository>()

    match getAll userRepo with
    | GetAllResult.DataRetrievingError msg ->
      ServerErrors.INTERNAL_ERROR $"Error: User data could not be retrieved. %s{msg}" next ctx
    | GetAllResult.Found users -> json users next ctx

let getUser (rawId: int) : HttpHandler =
  fun next ctx ->
    let userRepo = ctx.GetService<IUserRepository>()

    match mapUserId rawId with
    | Error msg -> RequestErrors.UNPROCESSABLE_ENTITY msg next ctx
    | Ok userId ->
      match findById userRepo userId with
      | FindByIdResult.DataRetrievingError msg ->
        ServerErrors.INTERNAL_ERROR $"Error: User data could not be retrieved. %s{msg}" next ctx
      | FindByIdResult.NotFound -> RequestErrors.NOT_FOUND "No user found by this id." next ctx
      | FindByIdResult.Found user -> json user next ctx

let renameUser (rawId: int) : HttpHandler =
  fun next ctx ->
    task {
      let userRepo = ctx.GetService<IUserRepository>()
      let! encodedName = ctx.ReadBodyFromRequestAsync false // TODO: test this, it seems shady

      match mapUserId rawId with
      | Error msg -> return! RequestErrors.UNPROCESSABLE_ENTITY msg next ctx
      | Ok userId ->
        match Decode.fromString Decode.string encodedName with
        | Error e -> return! RequestErrors.BAD_REQUEST (sprintf "%A" e) next ctx
        | Ok decodedName ->
          match UserName.ofRaw decodedName with
          | Error(Validation.ValidationError msg) ->
            return! RequestErrors.UNPROCESSABLE_ENTITY $"Error: Provided name is not valid! %s{msg}" next ctx
          | Ok newName ->
            match User.rename userRepo userId newName with
            | RenameResult.DataRetrievingError msg ->
              return! ServerErrors.INTERNAL_ERROR $"Error: User could not be retrieved. %s{msg}" next ctx
            | RenameResult.DataStoringError msg ->
              return! ServerErrors.INTERNAL_ERROR $"Error: User could not be updated. %s{msg}" next ctx
            | RenameResult.InvalidNameForUserError msg ->
              return!
                RequestErrors.UNPROCESSABLE_ENTITY $"Error: Provided name is not valid for this user! %s{msg}" next ctx
            | RenameResult.UserNotFoundError -> return! RequestErrors.NOT_FOUND "No user found by this id" next ctx
            | RenameResult.UserUpdated updatedUser -> return! Successful.OK (json updatedUser) next ctx
    }

type CreateUserRequestDTO = { Name: string; Quota: int }

let createUser: HttpHandler =
  fun next ctx ->
    task {
      let userRepo = ctx.GetService<IUserRepository>()
      let entryRepo = ctx.GetService<IEntryRepository>()
      let! userDTO = ctx.BindJsonAsync<CreateUserRequestDTO>()

      match UserName.ofRaw userDTO.Name with
      | Error(Validation.ValidationError msg) ->
        return! RequestErrors.UNPROCESSABLE_ENTITY $"Could not create user, username invalid! %s{msg}" next ctx
      | Ok userName ->
        match UserQuota.ofRaw userDTO.Quota with
        | Error(Validation.ValidationError msg) ->
          return! RequestErrors.UNPROCESSABLE_ENTITY $"Could not create user, quota invalid! %s{msg}" next ctx
        | Ok userQuota ->
          match User.add userRepo entryRepo (userName, userQuota) with
          | UserDataStoringError msg ->
            return! ServerErrors.INTERNAL_ERROR $"Error: Could not store user data. %s{msg}" next ctx
          | RootFolderStoringError msg ->
            return! ServerErrors.INTERNAL_ERROR $"Error: Could not store root folder for new user. %s{msg}" next ctx
          | InvalidUserError msg ->
            return! RequestErrors.UNPROCESSABLE_ENTITY $"Could not create user, input data invalid! %s{msg}" next ctx
          | Stored user -> return! Successful.CREATED (json user) next ctx
    }

let getTotalBytesStoredByUser (rawUserId: int) : HttpHandler =
  fun next ctx ->
    let userRepo = ctx.GetService<IUserRepository>()
    let entryRepo = ctx.GetService<IEntryRepository>()

    let formatMsg (userName, count) =
      text $"User %s{userName} has a total of %d{count} bytes stored"

    match UserId.ofRaw rawUserId with
    | Error(Validation.ValidationError msg) ->
      RequestErrors.UNPROCESSABLE_ENTITY $"Error: Provided user id invalid! %s{msg}" next ctx
    | Ok userId ->
      match User.getTotalBytesStoredByUserId userRepo entryRepo userId with
      | GetTotalBytesResult.UserDataRetrievingError msg ->
        ServerErrors.INTERNAL_ERROR $"Error: User data could not be retrieved. %s{msg}" next ctx
      | GetTotalBytesResult.EntryDataRetrievingError msg ->
        ServerErrors.INTERNAL_ERROR $"Error: Entry data could not be retrieved. %s{msg}" next ctx
      | GetTotalBytesResult.UserNotFound -> RequestErrors.NOT_FOUND "No user found by this id" next ctx
      | ZeroEntries user -> Successful.OK (formatMsg (User.rawName user, 0)) next ctx
      | TotalBytesCounted(user, (ByteCount count)) -> Successful.OK (formatMsg (User.rawName user, count)) next ctx
