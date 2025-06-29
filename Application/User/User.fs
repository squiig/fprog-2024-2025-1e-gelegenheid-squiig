namespace DrizzleCarton.Application

open DrizzleCarton.Model

// TODO: make all these failure types exhaustive

type DAFindUserByIdFailure = | DataAccessFailed

type DAGetAllUsersFailure = | DataAccessFailed

type DAStoreUserFailure =
  | UserAlreadyStored
  | DataAccessFailed

type DAUpdateUserFailure =
  | UserDoesNotExist
  | DataAccessFailed

/// Defines data access operations for user functionality.
type IUserDataAccess =
  abstract GetAllUsers: unit -> Result<User list, DAGetAllUsersFailure>
  abstract FindUserById: UserId -> Result<User option, DAFindUserByIdFailure>
  abstract StoreUser: User -> Result<unit, DAStoreUserFailure>
  abstract UpdateUser: User -> Result<User, DAUpdateUserFailure>

module User =

  type GetAllResult =
    | DataAccessFailed
    | Found of User list

  let getAll (dataAccess: IUserDataAccess) =
    match dataAccess.GetAllUsers() with
    | Error DAGetAllUsersFailure.DataAccessFailed -> DataAccessFailed
    | Ok users -> Found users

  type GetByIdResult =
    | DataAccessFailed
    | NotFound
    | Found of User

  let findById (dataAccess: IUserDataAccess) (id: UserId) =
    match dataAccess.FindUserById id with
    | Error DAFindUserByIdFailure.DataAccessFailed -> DataAccessFailed
    | Ok None -> NotFound
    | Ok(Some user) -> Found user

  // TODO: fix function
  let add (db: Database) username quota rootFolder =
    Database.addUser db (username, quota, rootFolder) |> Result.map toUser

  // TODO: fix function
  let update (db: Database) id user =
    Database.updateUser db (toTuple user) |> Result.map toUser
