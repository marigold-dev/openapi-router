(*
Copyright 2021/2022 Johns Hopkins University Applied Physics Laboratory

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

1. Redistributions of source code must retain the above copyright
notice, this list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright
notice, this list of conditions and the following disclaimer in the
documentation and/or other materials provided with the distribution.

3. Neither the name of the copyright holder nor the names of its
contributors may be used to endorse or promote products derived from
this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

open Core;;

module Yojson_conv = Ppx_yojson_conv_lib.Yojson_conv;;

type 'a map = (string * 'a) list [@@deriving show];;

let yojson_of_map f xs : Yojson.Safe.t =
  `Assoc (List.map ~f:(fun (k,v) -> (k, f v)) xs);;

let map_of_yojson (f : Yojson.Safe.t -> 'a) : Yojson.Safe.t ->'a map = function  
  | `Assoc xs -> List.map ~f:(fun (k,v) -> (k, (f v))) xs
  | x -> raise (Yojson_conv.Of_yojson_error (Failure "unexpected input", x))

type any = Yojson.Safe.t;;
let pp_any = Yojson.Safe.pp;;
let any_of_yojson x = x;;
let yojson_of_any x = x;;

type any_map = any map [@@deriving show];;
let any_map_of_yojson = map_of_yojson (fun v -> v);; (* schema_object_of_yojson *);;
let yojson_of_any_map = yojson_of_map (fun v -> v);; (* yojson_of_schema_object *);;


type 'a or_ref = Obj of 'a | Ref of string;;

let yojson_of_or_ref f = function
  | Obj x -> f x
  | Ref r -> `Assoc ["$ref", `String r];;

let or_ref_of_yojson f = function
  | `Assoc ["$ref", `String x] -> Ref x
  | j -> Obj (f j)

let pp_or_ref (f:Format.formatter -> 'a -> unit) pp : ('a or_ref) -> unit = function
    | Obj x -> f pp x
    | Ref r -> Format.fprintf pp "[$ref %s]" r
                   
type json_schema_type =
    | Null
    | Boolean
    | Object
    | Array
    | Number
    | String
    | Integer;;

let string_of_json_schema_type = function
    | Null -> "null"
    | Boolean -> "boolean"
    | Object -> "object"
    | Array -> "array"
    | Number -> "number"
    | String -> "string"
    | Integer -> "integer";;

let json_schema_type_of_yojson : Yojson.Safe.t -> json_schema_type = function
    | `String "null"    -> Null
    | `String "boolean" -> Boolean
    | `String "object"  -> Object
    | `String "array"   -> Array
    | `String "number"  -> Number
    | `String "string"  -> String
    | `String "integer" -> Integer
    | x -> raise (Yojson_conv.Of_yojson_error (Failure "unexpected input", x))
                       
let yojson_of_json_schema_type : json_schema_type -> Yojson.Safe.t  =
  fun x -> `String (string_of_json_schema_type x)
               
let pp_json_schema_type fmt x = Yojson.Safe.pp fmt (yojson_of_json_schema_type x);;

type schema = {
  (* Common fields *)
  schema      : string option [@key "$schema"] [@yojson.option];
  id_         : string option [@key "$id"]  [@yojson.option];
  title       : string option [@yojson.option];
  description : string option [@yojson.option];
  default     : any option [@yojson.option];
  examples    : any list option [@yojson.option];
  read_only   : bool option [@yojson.option];
  write_only  : bool option [@yojson.option];
  comment     : string option [@key "$comment"] [@yojson.option];
  
  typ         : json_schema_type or_ref option [@key "type"] [@yojson.option];
  enum        : any list option [@yojson.option];
  const       : any option [@yojson.option];

  (* numeric type fields *)
  multiple_of      : float option [@key "multipleOf"] [@yojson.option];
  minimum          : float option [@yojson.option];
  exclusiveMinimum : float option [@key "exclusiveMinimum"] [@yojson.option];
  maximum          : float option [@yojson.option];
  exclusiveMaximum : float option [@key "exclusiveMaximum"] [@yojson.option];
  
  (* object type only fields *)
  properties            : schema or_ref map option [@yojson.option];
  pattern_properties    : schema or_ref map option
                          [@key "patternProperties"] [@yojson.option];
  additional_properties : schema or_ref option [@key "additionalProperties"] [@yojson.option];
  required              : string list option [@yojson.option];
  property_names        : schema or_ref option [@key "propertyNames"] [@yojson.option];
  min_properties        : int option [@key "minProperties"] [@yojson.option];
  max_properties        : int option [@key "maxProperties"] [@yojson.option];

  (* array type only fields *)
  min_items   : int option [@key "minItems"] [@yojson.option];
  max_items   : int option [@key "maxItems"] [@yojson.option];
  items       : schema or_ref option [@yojson.option];
  prefix_items : schema or_ref list option [@key "prefixItems"]
                 [@yojson.option];
  (* string type only fields *)
  format      : string option [@yojson.option];
  pattern     : string option [@yojson.option];
  min_length  : int option [@key "minLength"] [@yojson.option];
  max_length  : int option [@key "maxLength"] [@yojson.option];

  (* schema composition *)
  all_of      : schema or_ref list option [@key "allOf"] [@yojson.option];
  any_of      : schema or_ref list option [@key "anyOf"] [@yojson.option];
  one_of      : schema or_ref list option [@key "oneOf"] [@yojson.option]
} [@@deriving make,show,yojson] [@@yojson.allow_extra_fields]
;;

module Helpers = struct
  let obj o = Obj o;;
  let ref r = Ref r;;

  let empty    = make_schema ();;
  let title t s = {s with title = Some t};;
  let description d s = {s with description = Some d};;
  let const d s = {s with const = Some d};;
  let typ t s  = {s with typ = Some t};;
  let format f s = {s with format = Some f};;
  let items is s = {s with items = Some is};;
  let prefix_items pis s = {s with prefix_items = Some pis};;
  let enum xs s  = {s with enum  = Some xs};;
  let any_of xs s = {s with any_of = Some xs};;
  let properties ps s = {s with properties = Some ps};;
  let additional_properties ps s = {s with additional_properties = Some ps};;
  let require p s   = {s with required = Option.value ~default:[] s.required
                                         |> (fun r -> p::r)
                                         |> Option.return}
  let property ?(required = false) k v s =
      {s with properties = Option.value ~default:[] s.properties
                                   |> (fun ps -> ps@[k,v])
                                   |> Option.return;}
      |> if required
      then require k
      else fun s -> s;;
                      
  let null          = empty |> typ (Obj Null);;
  let boolean       = empty |> typ (Obj Boolean);;
  let object_       = empty |> typ (Obj Object);;
  let array         = empty |> typ (Obj Array);;
  let number        = empty |> typ (Obj Number);;
  let string        = empty |> typ (Obj String);;
  let integer       = empty |> typ (Obj Integer);;

  let array_of s = array |> items s;;

  let datetime      = string |> format "date-time";;
  let time          = string |> format "time";;
  let date          = string |> format "date";;
  let email         = string |> format "email";;
  let idn_email     = string |> format "idn-email";;
  let hostname      = string |> format "hostname";;
  let idn_hostname  = string |> format "idn-hostname";;
  let ipv4          = string |> format "ipv4";;
  let ipv6          = string |> format "ipv6";;
  let uri           = string |> format "uri";;
  let uri_reference = string |> format "uri-reference";;
  let iri           = string |> format "iri";;
  let iri_reference = string |> format "iri-reference";;
end;;
