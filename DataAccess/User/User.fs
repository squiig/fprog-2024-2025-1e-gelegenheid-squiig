module DrizzleCarton.DataAccess.User

open DrizzleCarton.Application
open DrizzleCarton.Model

let userPersistence (db: SimulatedDatabase) : IUserDataAccess =
  { new IUserDataAccess with
      member this.FindUserById(id: UserId) : Result<User option, DAFindUserByIdFailure> =
        match SimulatedDatabase.user id db with
        | Error(SimulatedDatabase.NotFound _) -> Ok None
        | Ok user -> Ok(Some(User.toUser user))

      member this.GetAllUsers() : Result<User list, DAGetAllUsersFailure> =
        raise (System.NotImplementedException())

      member this.StoreUser(user: User) : Result<unit, DAStoreUserFailure> =
        raise (System.NotImplementedException())

      member this.UpdateUser(user: User) : Result<User, DAUpdateUserFailure> =
        raise (System.NotImplementedException()) }
