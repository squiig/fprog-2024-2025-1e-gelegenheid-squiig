module DrizzleCarton.HTTPWebService.Web

open Giraffe

/// Composes all dispatching of HTTP requests into a single Giraffe HTTP handler.
/// This handler is then used to "run" Giraffe in the main function of the back-end.
let webApp: HttpHandler =
  choose
    [ route "/user" >=> User.getAllUsers

      GET >=> routef "/user/%i" User.getUser

      PUT >=> routef "/user/%i/username" User.renameUser

      GET >=> routef "/user/%i/file" Entry.getAllEntriesOfUser ]
