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
    | GetAllResult.DataFailure msg ->
      ServerErrors.INTERNAL_ERROR
        $"Cannot process this request right now. Data could not be retrieved: %s{msg}"
        next
        ctx
    | GetAllResult.Found users -> json users next ctx

let getUser (rawId: int) : HttpHandler =
  fun next ctx ->
    let userRepo = ctx.GetService<IUserRepository>()

    match UserId.ofRaw rawId with
    | Error(Validation.ValidationError msg) -> RequestErrors.UNPROCESSABLE_ENTITY msg next ctx
    | Ok userId ->
      match findById userRepo userId with
      | FindByIdResult.DataFailure msg ->
        ServerErrors.INTERNAL_ERROR
          $"Cannot process this request right now. Data could not be retrieved: %s{msg}"
          next
          ctx
      | FindByIdResult.NotFound -> RequestErrors.NOT_FOUND "No user found by this id" next ctx
      | FindByIdResult.Found user -> json user next ctx

let renameUser (rawId: int) : HttpHandler =
  fun next ctx ->
    task {
      let userRepo = ctx.GetService<IUserRepository>()

      match UserId.ofRaw rawId with
      | Error(Validation.ValidationError msg) -> return! RequestErrors.UNPROCESSABLE_ENTITY msg next ctx
      | Ok userId ->
        match findById userRepo userId with
        | FindByIdResult.DataFailure msg ->
          return!
            ServerErrors.INTERNAL_ERROR
              $"Cannot process this request right now. Data could not be retrieved: %s{msg}"
              next
              ctx
        | FindByIdResult.NotFound -> return! RequestErrors.NOT_FOUND "No user found by this id" next ctx
        | FindByIdResult.Found oldUser ->
          let! encodedName = ctx.ReadBodyFromRequestAsync false

          match Decode.fromString Decode.string encodedName with
          | Error e -> return! RequestErrors.BAD_REQUEST (sprintf "%A" e) next ctx
          | Ok decodedName ->
            match UserName.ofRaw decodedName with
            | Error(Validation.ValidationError msg) -> return! RequestErrors.UNPROCESSABLE_ENTITY msg next ctx
            | Ok newUsername ->
              let id, _, quota, root = User.toTuple oldUser

              match User.make (id, newUsername, quota, root) with
              | Error(Validation.ValidationError msg) ->
                return!
                  ServerErrors.INTERNAL_ERROR
                    $"Unexpected user validation error when trying to rename user with id %i{rawId} to name %s{decodedName}. Message: %s{msg}"
                    next
                    ctx
              | Ok dirtyUser ->
                match User.update userRepo dirtyUser with
                | Error(DataAccessError msg) ->
                  return!
                    ServerErrors.INTERNAL_ERROR
                      $"Cannot process this request right now. Data access failed. Message: %s{msg}"
                      next
                      ctx
                | Ok updatedUser -> return! json updatedUser next ctx
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
        match Entry.storeNewRootFolder entryRepo with
        | StoreResult.DataFailure msg ->
          return!
            ServerErrors.INTERNAL_ERROR
              $"Aborting user creation! Failed to create root folder for new user. Message: %s{msg}"
              next
              ctx
        | EntryStored rootFolder ->
          let rootFolderId, _, _, _, _ = Entry.toTuple rootFolder

          match User.add userRepo (userDTO.Name, userDTO.Quota, rootFolderId) with
          | AddResult.DataAccessError msg ->
            return!
              ServerErrors.INTERNAL_ERROR
                $"Cannot process this request right now. Data access failed. Message: %s{msg}"
                next
                ctx
          | InvalidFieldError msg ->
            return! RequestErrors.UNPROCESSABLE_ENTITY $"Could not create user, input data invalid: %s{msg}" next ctx
          | Stored user -> return! json user next ctx
    }
