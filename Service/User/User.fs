module DrizzleCarton.HTTPWebService.User

open DrizzleCarton.Model
open DrizzleCarton.Application
open DrizzleCarton.Application.Entry
open DrizzleCarton.Application.EntryRepositoryContract
open DrizzleCarton.Application.User
open DrizzleCarton.Application.UserRepositoryContract

open Giraffe

open Thoth.Json.Net

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

    match findById userRepo rawId with
    | FindByIdResult.DataRetrievingError msg ->
      ServerErrors.INTERNAL_ERROR $"Error: User data could not be retrieved. %s{msg}" next ctx
    | FindByIdResult.InvalidIdError msg ->
      RequestErrors.UNPROCESSABLE_ENTITY $"Provided user id is not valid! %s{msg}" next ctx
    | FindByIdResult.NotFound -> RequestErrors.NOT_FOUND "No user found by this id." next ctx
    | FindByIdResult.Found user -> json user next ctx

let renameUser (rawId: int) : HttpHandler =
  fun next ctx ->
    task {
      let userRepo = ctx.GetService<IUserRepository>()
      let! encodedName = ctx.ReadBodyFromRequestAsync false // TODO: test this, it seems shady

      match Decode.fromString Decode.string encodedName with
      | Error e -> return! RequestErrors.BAD_REQUEST (sprintf "%A" e) next ctx
      | Ok decodedName ->
        match User.rename userRepo rawId decodedName with
        | RenameResult.DataRetrievingError msg ->
          return! ServerErrors.INTERNAL_ERROR $"Error: User data could not be retrieved. %s{msg}" next ctx
        | RenameResult.DataStoringError msg ->
          return! ServerErrors.INTERNAL_ERROR $"Error: User could not be updated. %s{msg}" next ctx
        | RenameResult.InvalidIdError msg ->
          return! RequestErrors.UNPROCESSABLE_ENTITY $"Provided user id is not valid! %s{msg}" next ctx
        | RenameResult.InvalidNameError msg ->
          return! RequestErrors.UNPROCESSABLE_ENTITY $"Provided name is not valid! %s{msg}" next ctx
        | RenameResult.InvalidNameForUserError msg ->
          return! RequestErrors.UNPROCESSABLE_ENTITY $"Provided name is not valid for this user! %s{msg}" next ctx
        | RenameResult.UserNotFoundError -> return! RequestErrors.NOT_FOUND "No user found by this id" next ctx
        | RenameResult.UserUpdated updatedUser -> return! json updatedUser next ctx
    }

type CreateUserRequestDTO = { Name: string; Quota: int }

let createUser: HttpHandler =
  fun next ctx ->
    task {
      let userRepo = ctx.GetService<IUserRepository>()
      let entryRepo = ctx.GetService<IEntryRepository>()
      let! data = ctx.ReadBodyBufferedFromRequestAsync()

      match Decode.Auto.fromString<CreateUserRequestDTO> data with
      | Error e -> return! RequestErrors.BAD_REQUEST (sprintf "%s" e) next ctx
      | Ok userDTO ->
        match User.add userRepo entryRepo (userDTO.Name, userDTO.Quota) with
        | UserDataStoringError msg ->
          return! ServerErrors.INTERNAL_ERROR $"Error: Could not store user data. %s{msg}" next ctx
        | RootFolderStoringError msg ->
          return! ServerErrors.INTERNAL_ERROR $"Error: Could not store root folder for new user. %s{msg}" next ctx
        | InvalidUserError msg ->
          return! RequestErrors.UNPROCESSABLE_ENTITY $"Could not create user, input data invalid! %s{msg}" next ctx
        | Stored user -> return! json user next ctx
    }
