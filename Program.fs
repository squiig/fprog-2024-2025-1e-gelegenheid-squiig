open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.DependencyInjection

open Giraffe

open DrizzleCarton.Application
open DrizzleCarton.RAMDataAccess
open DrizzleCarton.HTTPWebService

let report<'T> (result: Result<'T, RAMDataAccess.RAMDataAccessError>) =
  match result with
  | Ok n -> printfn "%A" n
  | Error exn -> printfn "%A" exn

let databaseDemo () =
  let db = RAMDataAccess.connect ()

  match RAMDataAccess.users db with
  | Ok users -> List.iter (fun u -> printfn "%A" u) users
  | Error exn -> printfn "%A" exn

  report <| RAMDataAccess.addUser db ("eleanor", 1024 * 1024 * 1024, 12)

/// Here routes are configured
let webApp: HttpHandler = choose [ Web.webApp ]

let configureApp (app: IApplicationBuilder) = app.UseGiraffe webApp

let configureServices (services: IServiceCollection) =
  // Add Giraffe dependencies
  //services.AddSingleton<RAMDataAccess>(RAMDataAccess.connect ()) |> ignore
  services.AddSingleton<IUserDataAccess>(User.userPersistence) |> ignore
  services.AddSingleton<IEntryDataAccess>(Entry.entryPersistence) |> ignore
  services.AddGiraffe() |> ignore

[<EntryPoint>]
let main _ =
  Host
    .CreateDefaultBuilder()
    .ConfigureWebHostDefaults(fun webHostBuilder ->
      webHostBuilder.Configure(configureApp).ConfigureServices configureServices
      |> ignore)
    .Build()
    .Run()

  0
