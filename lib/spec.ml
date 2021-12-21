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
open Json_schema;;

type contact_object = {
  name  : string option [@yojson.option];
  url   : string option [@yojson.option];
  email : string option [@yojson.option];
} [@@deriving make,show,yojson];;

type license_object = {
  name : string;
  url  : string option [@yojson.option];
} [@@deriving make,show,yojson];;


type server_variable_object = {
  enum        : string list option [@yojson.option];
  default     : string;
  description : string option [@yojson.option];
} [@@deriving make,show,yojson];;

type external_documentation_object = {
  description : string option [@yojson.option];
  url         : string;
} [@@deriving make,show,yojson];;

type server_object = {
  url         : string;
  description : string option [@yojson.option];
  variables   : server_variable_object map option
                [@yojson.option]
} [@@deriving make,show,yojson];;

type link_object = {
  operation_ref : string option [@key "operationRef"]
                  [@yojson.option];
  operation_id  : string option [@key "operationId"]
                  [@yojson.option];
  parameters    : any map option [@yojson.option];
  request_body  : any  option [@key "requestBody"]
                  [@yojson.option]; 
  description   : string option [@yojson.option];
  server        : server_object option [@yojson.option];
} [@@deriving make,show,yojson];;

type header_object = {
  description       : string option
                      [@yojson.option];
  required          : bool option
                      [@yojson.option];
  deprecated        : bool option
                      [@yojson.option];
  allow_empty_value : bool option [@key "allowEmptyValue"]
                      [@yojson.option];
} [@@deriving make,show,yojson];;

type example_object = {
  summary       : string option
                  [@yojson.option];
  description   : string option
                  [@yojson.option];
  value         : any option
                  [@yojson.option];
  externalValue : string option
                  [@yojson.option];
} [@@deriving make, show, yojson];;

type media_type_object = {
  schema   : schema or_ref option [@yojson.option];
  example  : any option [@yojson.option];
  examples : example_object or_ref map option [@yojson.option];
  encoding : any map option [@yojson.option] (* fixme: should be an encoding_object map *)
} [@@deriving make, show, yojson];;

type response_object = {
  description : string;
  headers     : header_object or_ref map option
                [@yojson.option];
  content     : media_type_object map option
                [@yojson.option];
  links       : link_object or_ref map option
                [@yojson.option];
} [@@deriving make,show,yojson];;

type responses_object = response_object or_ref map [@@deriving show, yojson];;

type parameter_location = Query | Header | Path | Cookie [@@deriving show,yojson];;
let parameter_location_of_yojson = function
  | `String "query"   -> Query
  | `String "header"  -> Header
  | `String "path"    -> Path
  | `String "cookie" -> Cookie
  | x -> let open Ppx_yojson_conv_lib.Yojson_conv in    
    raise (Of_yojson_error
             (Failure (sprintf "%s: unexpected value must be \"query\", \"header\", \"path\", or \"cookie\"" __LOC__), x));;

let yojson_of_parameter_location = function
  | Query  -> `String "query"
  | Header -> `String "header"
  | Path   -> `String "path"
  | Cookie -> `String "cookie"

type parameter_style = Simple | Form [@@deriving show];;
let parameter_style_of_yojson = function
  | `String "simple" -> Simple
  | `String "form"   -> Form
  | x -> let open Ppx_yojson_conv_lib.Yojson_conv in
    raise (Of_yojson_error (Failure (sprintf "%s: unexpected value must be \"simple\" or \"form\"" __LOC__), x));;
;;

let yojson_of_parameter_style = function
  | Simple -> `String "simple"
  | Form   -> `String "form"

type parameter_object = {
  name              : string;
  in_               : parameter_location [@key "in"];
  description       : string option
                      [@yojson.option];
  required          : bool option
                      [@yojson.option];
  deprecated        : bool option
                      [@yojson.option];
  allow_empty_value : bool option [@key "allowEmptyValue"]
                      [@yojson.option];
  style             : parameter_style option [@yojson.option];
  schema            : schema or_ref option [@yojson.option];
  example           : any option [@yojson.option]
} [@@deriving make,show,yojson];;

type request_body_object = {
  description : string option [@yojson.option];
  content     : media_type_object map;
  required    : bool option [@yojson.option]
} [@@deriving make,show,yojson];;

type security_scheme_object = {
  (* Note: these aren't actually all optional, which one is required
     depends on the type field. Should probably use a union type
     with custom json converters *)
  type_              : string [@key "type"];
  description        : string option
                       [@yojson.option];
  name               : string option
                       [@yojson.option];
  in_                : string option
                       [@key"in"] [@yojson.option];
  scheme             : string option
                       [@yojson.option];
  bearer_format      : string option
                       [@key "bearerFormat"] [@yojson.option];
  (* FIXME: actually define Oauth Flows Object Type *)
  flows              : any option
                       [@yojson.option];
  openid_connect_url : string option
                       [@key "openIdConnectUrl"] [@yojson.option]
} [@@deriving make,show,yojson];;

(* FIXME: define an actual type for this *)
type callback_object = any [@@deriving show,yojson];;

type components_object = {
  schemas          : schema or_ref map option
                     [@yojson.option];
  responses        : response_object or_ref map option
                     [@yojson.option];
  parameters       : parameter_object or_ref map option
                     [@yojson.option];
  examples         : example_object or_ref map option
                     [@yojson.option];
  request_bodies   : request_body_object or_ref map option
                     [@key "requestBodies"] [@yojson.option];
  headers          : header_object or_ref map option
                     [@yojson.option];
  security_schemes : security_scheme_object or_ref map option
                     [@key "securitySchemes"] [@yojson.option];
  links            : link_object or_ref map option
                     [@yojson.option];
  callbacks        : callback_object or_ref map option
                     [@yojson.option];
} [@@deriving make,show,yojson];;

type info_object = {
  title            : string;
  description      : string option [@yojson.option];
  terms_of_service : string option [@key "termsOfService"] [@yojson.option];
  contact          : contact_object option [@yojson.option];
  license          : license_object option [@yojson.option];
  version          : string;
} [@@deriving make,show,yojson];;

type operation_object = {
  tags          : string list option
                  [@yojson.option];
  summary       : string option
                  [@yojson.option];
  description   : string option
                  [@yojson.option];
  external_docs : external_documentation_object option
                  [@key "externalDocs"] [@yojson.option];
  operation_id  : string option
                  [@key "operationId"] [@yojson.option];
  parameters    : parameter_object or_ref list option
                  [@yojson.option];
  request_body  : request_body_object or_ref option
                  [@key "requestBody"] [@yojson.option];
  responses     : responses_object;
  callbacks     : callback_object or_ref map option
                  [@yojson.option];
  deprecated    : bool option
                  [@yojson.option];
  (* FIXME: add a type for security_requirement *)
  security      : any option
                  [@yojson.option];
  servers       : server_object list option
                  [@yojson.option];
} [@@deriving make,show,yojson];;

type path_object = {
  summary     : string option [@yojson.option];
  description : string option [@yojson.option];
  get         : operation_object option [@yojson.option];
  put         : operation_object option [@yojson.option];
  post        : operation_object option [@yojson.option];
  delete      : operation_object option [@yojson.option];
  options     : operation_object option [@yojson.option];
  head        : operation_object option [@yojson.option];
  patch       : operation_object option [@yojson.option];
  trace       : operation_object option [@yojson.option];
  servers     : server_object list option [@yojson.option];
  parameters  : parameter_object or_ref list option [@yojson.option]
} [@@deriving make,show,yojson];;

type paths_object = path_object map [@@deriving yojson,show];;

type tag_object = {
  name : string;
  description : string option [@yojson.option];
  external_docs : external_documentation_object option [@key "externalDocs"] [@yojson.option];
} [@@deriving make,show,yojson];;

type t = {
  openapi       : string;
  info          : info_object;
  servers       : server_object list option [@yojson.option];
  paths         : paths_object;
  components    : components_object option [@yojson.option];
  security      : any option [@yojson.option]; (* FIXME: define an actual type 
                                                  for security_requirements_object *)
  tags          : tag_object list option [@yojson.option];
  external_docs : external_documentation_object option [@key "externalDocs"] [@yojson.option]
} [@@deriving make,show,yojson];;
