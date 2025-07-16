module DrizzleCarton.HTTPWebService.Serialization

open Giraffe

open Thoth.Json.Net

/// convert to Json
let json<'T> (x: 'T) : HttpHandler =
  let encoded = Encode.Auto.toString (4, x)
  setHttpHeader "Content-Type" "application/json" >=> setBodyFromString encoded

let fromJson<'T> = Decode.Auto.fromString<'T>
