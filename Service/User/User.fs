module DrizzleCarton.HTTPWebService.User

open DrizzleCarton.Model
open DrizzleCarton.Application
open DrizzleCarton.Application.User

open Giraffe

open Thoth.Json.Net

let getAllUsers: HttpHandler =
  fun next ctx ->
    let dataAccess = ctx.GetService<IUserDataAccess>()

    match getAll dataAccess with
    | GetAllResult.DataAccessFailed ->
      ServerErrors.serviceUnavailable
        (text "Cannot process this request right now. Data access failed somehow.")
        next
        ctx
    | GetAllResult.Found users -> json users next ctx

let getUser (id: int) : HttpHandler =
  fun next ctx ->
    let dataAccess = ctx.GetService<IUserDataAccess>()

    match findById dataAccess (UserId.ofRaw id) with
    | GetByIdResult.DataAccessFailed ->
      ServerErrors.serviceUnavailable
        (text "Cannot process this request right now. Data access failed somehow.")
        next
        ctx
    | GetByIdResult.NotFound -> RequestErrors.notFound (text "No user found by this id") next ctx
    | GetByIdResult.Found user -> json user next ctx

let renameUser (id: int) : HttpHandler =
  fun next ctx ->
    task {
      let dataAccess = ctx.GetService<IUserDataAccess>()

      match dataAccess.FindUserById(UserId.ofRaw id) with
      | Error _ -> return! RequestErrors.NOT_FOUND "User not found" next ctx
      | Ok user ->
        let! encodedName = ctx.ReadBodyFromRequestAsync false

        match Decode.fromString Decode.string encodedName with
        | Error e -> return! RequestErrors.BAD_REQUEST (sprintf "%A" e) next ctx
        | Ok name ->
          let newUser: User = { user with Username = name }

          match User.update db id newUser with
          | Ok user -> return! json user next ctx
          | Error _ -> return! RequestErrors.BAD_REQUEST "Could not update user" next ctx
    }

type CreateUserDTO = { Name: string; Quota: int }

let createUser: HttpHandler =
  fun next ctx ->
    task {
      let dataAccess = ctx.GetService<IEntryDataAccess>()
      let! data = ctx.ReadBodyBufferedFromRequestAsync()

      match Serialization.fromJson<CreateUserDTO> data with
      | Error e -> return! RequestErrors.BAD_REQUEST (sprintf "%s" e) next ctx
      | Ok u ->
        match dataAccess.StoreNewRootFolder with
        | Ok entry ->
          match User.add db u.Name u.Quota entry.Id with
          | Ok user -> return! json user next ctx
          | _ -> return! RequestErrors.BAD_REQUEST "failed to create user" next ctx
        | _ -> return! RequestErrors.BAD_REQUEST "failed to create root folder" next ctx
    }
