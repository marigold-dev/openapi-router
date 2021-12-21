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

type t;;
val to_handler : t -> Rock.Handler.t
val empty : t;;

type builder = t -> t;;

(* Specify openapi spec metadata *)
val title            : string -> builder;;
val description      : string -> builder;;
val terms_of_service : string -> builder;;
val contact          : Spec.contact_object -> builder;;
val license          : Spec.license_object -> builder;;
val version          : string -> builder;;

type 'a or_ref = 'a Json_schema.or_ref;;

(* Add a named component to an application spec *)

val schema           : string -> Json_schema.schema or_ref -> builder;;
val response         : string -> Spec.response_object or_ref -> builder;;
val parameter        : string -> Spec.parameter_object or_ref -> builder;;
val example          : string -> Spec.example_object or_ref -> builder;;
val request_body     : string -> Spec.request_body_object or_ref -> builder;;
val header           : string -> Spec.header_object or_ref -> builder;;
val security_scheme  : string -> Spec.security_scheme_object or_ref -> builder;;
val link             : string -> Spec.link_object or_ref -> builder;;
val callback         : string -> Spec.callback_object or_ref -> builder;;


(* Return a JSON reference to a named component of a spec
   (Note: doesn't confirm that the component actually exists)
*)

val schema_ref          : string -> Json_schema.schema or_ref;;
val response_ref        : string -> Spec.response_object or_ref;;
val parameter_ref       : string -> Spec.parameter_object or_ref;;
val example_ref         : string -> Spec.example_object or_ref;;
val request_body_ref    : string -> Spec.request_body_object or_ref;;
val header_ref          : string -> Spec.header_object or_ref;;
val security_scheme_ref : string -> Spec.security_scheme_object or_ref;;
val link_ref            : string -> Spec.link_object or_ref;;
val callback_ref        : string -> Spec.callback_object or_ref;;

(* Wrap standard opium builders *)
val host             : string -> builder;;
val backlog          : int -> builder;;
val port             : int -> builder;;
val jobs             : int -> builder;;
val cmd_name         : string -> builder;;
val not_found        : (Opium.Request.t ->  (Httpaf.Headers.t * Rock.Body.t) Lwt.t) -> builder;;

type route = string -> Rock.Handler.t -> builder;;

type api_route = ?tags:string list ->
    ?summary:string ->
    ?description:string ->
    ?external_docs:Spec.external_documentation_object ->
    ?operation_id:string ->
    ?parameters:Spec.parameter_object Json_schema.or_ref list ->
    ?request_body:Spec.request_body_object Json_schema.or_ref ->
    ?responses:Spec.responses_object ->
    ?callbacks:Json_schema.any Json_schema.or_ref Json_schema.map ->
    ?deprecated:bool ->
    ?security:Json_schema.any ->
    ?servers:Spec.server_object list ->
    route;;

val get : api_route;;
val post : api_route;;
val delete : api_route;;
val put : api_route;;
val options : api_route;;
val head : api_route;;
val patch : api_route;;

val any : Opium.Method.t list -> route;;
val all : route;;
val action : Opium.Method.t -> route;;
val middleware : Rock.Middleware.t -> builder;;

val start : t -> Lwt_io.server Lwt.t;;
val start_multicore : t -> Lwt_io.server Lwt.t;;
val run_command  : t -> unit;;
val run_command' : t -> [> `Ok of unit Lwt.t | `Error | `Not_running ];;
val run_multicore  : t -> unit;;

