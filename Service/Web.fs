module DrizzleCarton.Web


open Giraffe
open DrizzleCarton.Model
open Thoth.Json.Net

let users : HttpHandler =
    fun next ctx ->
        let db = ctx.GetService<DrizzleCarton.Database> ()
        let users = User.all db
        json users next ctx

let user (id : int) : HttpHandler =
    fun next ctx ->
        let db = ctx.GetService<DrizzleCarton.Database> ()
        let user = User.fetch db id
        json user next ctx

let renameUser (id : int) : HttpHandler =
    fun next ctx -> task {
        let db = ctx.GetService<DrizzleCarton.Database> ()
        match User.fetch db id with
        | Error _ -> return! RequestErrors.NOT_FOUND "User not found" next ctx
        | Ok user ->
            let! encodedName = ctx.ReadBodyFromRequestAsync false
            match Decode.fromString Decode.string encodedName with
            | Error e ->
                return! RequestErrors.BAD_REQUEST (sprintf "%A" e) next ctx
            | Ok name ->
                let newUser: User =
                    { user with Username = name }
                match User.update db id newUser with
                | Ok user -> return! json user next ctx
                | Error _ -> return! RequestErrors.BAD_REQUEST "Could not update user" next ctx
    }

type CreateUser = {
    Name: string
    Quota: int
}

let createUser : HttpHandler =
    fun next ctx -> task {
        let db = ctx.GetService<Database> ()
        let! data = ctx.ReadBodyBufferedFromRequestAsync ()
        match fromJson<CreateUser> data with
        | Error e -> return! RequestErrors.BAD_REQUEST (sprintf "%s" e) next ctx
        | Ok u ->
            match Entry.newRoot db with
            | Ok entry ->
                match User.add db u.Name u.Quota entry.Id with
                | Ok user -> return! json user next ctx
                | _ -> return! RequestErrors.BAD_REQUEST "failed to create user" next ctx
            | _ -> return! RequestErrors.BAD_REQUEST "failed to create root folder" next ctx
    }


type EntriesResponse = {
    Entry: Entry
    Subentries: List<EntriesResponse>
}



let rec buildEntryResponse (db: Database) (entry: Entry): EntriesResponse =
    { Entry = entry
      Subentries =
            Entry.fetchSubentries db entry.Id
            |> List.map (buildEntryResponse db) }

let entries (userId : int) : HttpHandler =
    fun next ctx -> task {
        let db = ctx.GetService<Database> ()
        match User.fetch db userId with
        | Error _ -> return! RequestErrors.NOT_FOUND (sprintf "%i" userId) next ctx
        | Ok user ->
            match Entry.fetch db user.RootFolder with
            | Error e ->
                return! RequestErrors.NOT_FOUND
                    (sprintf "Root folder for %s(%i) not found: %A" user.Username user.RootFolder e)
                    next ctx
            | Ok root ->
                let response = buildEntryResponse db root
                return! json response next ctx
    }


let webApp : HttpHandler =
    choose [
        route "/user"   >=> users

        GET
            >=> routef "/user/%i" user

        PUT
            >=> routef "/user/%i/username" renameUser

        GET
            >=> routef "/user/%i/file" entries
    ]
