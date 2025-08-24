module DrizzleCarton.RAMDataAccess.User

open DrizzleCarton.Application
open DrizzleCarton.Application.User
open DrizzleCarton.Application.UserRepositoryContract
open DrizzleCarton.Application.ResultHelper
open DrizzleCarton.Model
open DrizzleCarton.RAMDataAccess

let mapUser db rawUser =
  let id, name, quota, rootId = rawUser

  match RAMDataAccess.entry db rootId with
  | Ok rawRootEntry ->
    match Entry.ofRaw rawRootEntry with
    | Ok rootEntry ->
      User.ofRaw (id, name, quota, rootEntry)
      |> Result.mapError (function
        | Validation.ValidationError msg -> ReadUserFailure.ModelValidationError msg)
    | Error(Validation.ValidationError msg) ->
      Error(ReadUserFailure.ModelValidationError $"User of id %d{id} has an invalid root folder! %s{msg}")
  | Error(RAMDataAccessError.NotFound _) ->
    Error(
      ReadUserFailure.ModelValidationError
        $"User (id %d{id}) found but invalid, the root folder id %d{rootId} does not exist!"
    )

let userPersistence: IUserRepository =
  { new IUserRepository with

      member this.FindUserById(userId: UserId) : Result<User option, ReadUserFailure> =
        let db = RAMDataAccess.defaultDb

        match RAMDataAccess.user db (UserId.toRaw userId) with
        | Error(RAMDataAccessError.NotFound _) -> Ok None
        | Ok rawUser -> mapUser db rawUser |> Result.map (fun u -> Some u)

      member this.GetAllUsers() : Result<User list, ReadUserFailure> =
        let db = RAMDataAccess.defaultDb

        match RAMDataAccess.users db with
        | Error(NotFound _) -> Ok []
        | Ok rawUsers -> rawUsers |> List.map (mapUser db) |> sequenceResult

      member this.StoreUser(userData: UserData) : Result<UserId, WriteUserFailure> =
        raise (System.NotImplementedException())

      member this.UpdateUser(dirtyUser: User) : Result<User, WriteUserFailure> =
        raise (System.NotImplementedException())

  }
