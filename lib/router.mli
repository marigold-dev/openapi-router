module type Config_Type = sig
  type app
  type route
  type handler

  val json_path : string
  val doc_path : string
  val json_route : string -> route
  val doc_route : string -> route
  val get : string -> handler -> route
  val post : string -> handler -> route
  val delete : string -> handler -> route
  val put : string -> handler -> route
  val options : string -> handler -> route
  val head : string -> handler -> route
  val patch : string -> handler -> route
  val build_routes : route list -> app
end

module Make : functor (Config : Config_Type) -> sig
  type t = {
    spec : Spec.t;
    routes : Config.route list;
  }

  val empty : t

  val title : string -> t -> t
  (** Specify Openapi title metadata *)

  val description : string -> t -> t
  (** Specify Openapi description metadata *)

  val terms_of_service : string -> t -> t
  (** Specify Openapi terms of service metadata *)

  val contact : Spec.contact_object -> t -> t
  (** Specify Openapi contact metadata *)

  val license : Spec.license_object -> t -> t
  (** Specify Openapi license metadata *)

  val version : string -> t -> t
  (** Specify Openapi version metadata *)

  val schema : string -> Json_schema.schema Json_schema.or_ref -> t -> t
  (** Add a named component to an application spec *)

  val response : string -> Spec.response_object Json_schema.or_ref -> t -> t
  (** Add a named component to an application spec *)

  val parameter : string -> Spec.parameter_object Json_schema.or_ref -> t -> t
  (** Add a named component to an application spec *)

  val example : string -> Spec.example_object Json_schema.or_ref -> t -> t
  (** Add a named component to an application spec *)

  val request_body :
    string -> Spec.request_body_object Json_schema.or_ref -> t -> t
  (** Add a named component to an application spec *)

  val header : string -> Spec.header_object Json_schema.or_ref -> t -> t
  (** Add a named component to an application spec *)

  val security_scheme :
    string -> Spec.security_scheme_object Json_schema.or_ref -> t -> t
  (** Add a named component to an application spec *)

  val link : string -> Spec.link_object Json_schema.or_ref -> t -> t
  (** Add a named component to an application spec *)

  val callback : string -> Spec.callback_object Json_schema.or_ref -> t -> t
  (** Add a named component to an application spec *)

  val schema_ref : string -> 'a Json_schema.or_ref
  (** Return a JSON reference to a named component of a spec (Note: doesn't confirm that the component actually exists)*)

  val response_ref : string -> 'a Json_schema.or_ref
  (** Return a JSON reference to a named component of a spec (Note: doesn't confirm that the component actually exists)*)

  val parameter_ref : string -> 'a Json_schema.or_ref
  (** Return a JSON reference to a named component of a spec (Note: doesn't confirm that the component actually exists)*)

  val example_ref : string -> 'a Json_schema.or_ref
  (** Return a JSON reference to a named component of a spec (Note: doesn't confirm that the component actually exists)*)

  val request_body_ref : string -> 'a Json_schema.or_ref
  (** Return a JSON reference to a named component of a spec (Note: doesn't confirm that the component actually exists)*)

  val header_ref : string -> 'a Json_schema.or_ref
  (** Return a JSON reference to a named component of a spec (Note: doesn't confirm that the component actually exists)*)

  val security_scheme_ref : string -> 'a Json_schema.or_ref
  (** Return a JSON reference to a named component of a spec (Note: doesn't confirm that the component actually exists)*)

  val link_ref : string -> 'a Json_schema.or_ref
  (** Return a JSON reference to a named component of a spec (Note: doesn't confirm that the component actually exists)*)

  val callback_ref : string -> 'a Json_schema.or_ref
  (** Return a JSON reference to a named component of a spec (Note: doesn't confirm that the component actually exists)*)

  val get :
    ?tags:string list ->
    ?summary:string ->
    ?description:string ->
    ?external_docs:Spec.external_documentation_object ->
    ?operation_id:string ->
    ?parameters:Spec.parameter_object Json_schema.or_ref list ->
    ?request_body:Spec.request_body_object Json_schema.or_ref ->
    ?responses:Spec.responses_object ->
    ?callbacks:Spec.callback_object Json_schema.or_ref Json_schema.map ->
    ?deprecated:bool ->
    ?security:Json_schema.any ->
    ?servers:Spec.server_object list ->
    string ->
    Config.handler ->
    t ->
    t

  val default_request_body : Spec.request_body_object Json_schema.or_ref

  val post :
    ?tags:string list ->
    ?summary:string ->
    ?description:string ->
    ?external_docs:Spec.external_documentation_object ->
    ?operation_id:string ->
    ?parameters:Spec.parameter_object Json_schema.or_ref list ->
    ?request_body:Spec.request_body_object Json_schema.or_ref ->
    ?responses:Spec.responses_object ->
    ?callbacks:Spec.callback_object Json_schema.or_ref Json_schema.map ->
    ?deprecated:bool ->
    ?security:Json_schema.any ->
    ?servers:Spec.server_object list ->
    string ->
    Config.handler ->
    t ->
    t

  val delete :
    ?tags:string list ->
    ?summary:string ->
    ?description:string ->
    ?external_docs:Spec.external_documentation_object ->
    ?operation_id:string ->
    ?parameters:Spec.parameter_object Json_schema.or_ref list ->
    ?request_body:Spec.request_body_object Json_schema.or_ref ->
    ?responses:Spec.responses_object ->
    ?callbacks:Spec.callback_object Json_schema.or_ref Json_schema.map ->
    ?deprecated:bool ->
    ?security:Json_schema.any ->
    ?servers:Spec.server_object list ->
    string ->
    Config.handler ->
    t ->
    t

  val put :
    ?tags:string list ->
    ?summary:string ->
    ?description:string ->
    ?external_docs:Spec.external_documentation_object ->
    ?operation_id:string ->
    ?parameters:Spec.parameter_object Json_schema.or_ref list ->
    ?request_body:Spec.request_body_object Json_schema.or_ref ->
    ?responses:Spec.responses_object ->
    ?callbacks:Spec.callback_object Json_schema.or_ref Json_schema.map ->
    ?deprecated:bool ->
    ?security:Json_schema.any ->
    ?servers:Spec.server_object list ->
    string ->
    Config.handler ->
    t ->
    t

  val options :
    ?tags:string list ->
    ?summary:string ->
    ?description:string ->
    ?external_docs:Spec.external_documentation_object ->
    ?operation_id:string ->
    ?parameters:Spec.parameter_object Json_schema.or_ref list ->
    ?request_body:Spec.request_body_object Json_schema.or_ref ->
    ?responses:Spec.responses_object ->
    ?callbacks:Spec.callback_object Json_schema.or_ref Json_schema.map ->
    ?deprecated:bool ->
    ?security:Json_schema.any ->
    ?servers:Spec.server_object list ->
    string ->
    Config.handler ->
    t ->
    t

  val head :
    ?tags:string list ->
    ?summary:string ->
    ?description:string ->
    ?external_docs:Spec.external_documentation_object ->
    ?operation_id:string ->
    ?parameters:Spec.parameter_object Json_schema.or_ref list ->
    ?request_body:Spec.request_body_object Json_schema.or_ref ->
    ?responses:Spec.responses_object ->
    ?callbacks:Spec.callback_object Json_schema.or_ref Json_schema.map ->
    ?deprecated:bool ->
    ?security:Json_schema.any ->
    ?servers:Spec.server_object list ->
    string ->
    Config.handler ->
    t ->
    t

  val patch :
    ?tags:string list ->
    ?summary:string ->
    ?description:string ->
    ?external_docs:Spec.external_documentation_object ->
    ?operation_id:string ->
    ?parameters:Spec.parameter_object Json_schema.or_ref list ->
    ?request_body:Spec.request_body_object Json_schema.or_ref ->
    ?responses:Spec.responses_object ->
    ?callbacks:Spec.callback_object Json_schema.or_ref Json_schema.map ->
    ?deprecated:bool ->
    ?security:Json_schema.any ->
    ?servers:Spec.server_object list ->
    string ->
    Config.handler ->
    t ->
    t

  val build : t -> Config.app
end
