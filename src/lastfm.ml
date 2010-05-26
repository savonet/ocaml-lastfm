(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2007 Savonet team

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details, fully stated in the COPYING
  file at the root of the liquidsoap distribution.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 *****************************************************************************)

(* lastfm protocol API for ocaml *)

(* Records for client *)
type client = { client : string ; version : string }
type login = { user : string ; password : string }

module type Http_t =
sig
  exception Http of string

  val default_timeout : float ref
  val request : ?post:string -> ?timeout:float -> 
                ?headers:((string*string) list) ->
                ?port:int -> host:string -> string ->
                string
end

module Http_ocamlnet =
struct
  exception Http of string

  let default_timeout = ref 5.

  let request ?(post="") ?timeout ?(headers=[]) ?(port=80) ~host req =
    let timeout = 
      match timeout with
        | Some v -> v
        | None   -> !default_timeout
    in
    let call = 
      match post with
        | "" -> new Http_client.get_call
        | _  -> new Http_client.post_call
    in
    let pipeline = new Http_client.pipeline in
    pipeline#set_options 
      { pipeline#get_options with 
          Http_client.connection_timeout = timeout 
      } ;
    let http_headers = call#request_header `Base in
    let body = call#request_body in
    call#set_request_uri (Printf.sprintf "http://%s:%d%s" host port req) ;
    let headers = ("User-agent",
        Printf.sprintf "ocaml-lastfm/%s" Lastfm_constants.version) 
        :: headers
    in
    http_headers#set_fields headers ;
    begin
      match post with
        | "" -> ()
        | _ -> 
          begin
           body#set_value post ; 
           call#set_request_body body ;
           http_headers#update_field 
             "Content-length" 
             (string_of_int (String.length post));
          end
    end ;
    call#set_request_header http_headers ;
    pipeline#add call ;
    try
      pipeline#run () ;
      call#response_body#value
    with
      | Http_client.Http_protocol e 
      | e -> 
         pipeline#reset() ; 
         raise (Http  (Printexc.to_string e))
end

module type Audioscrobbler_t = 
sig
  type source = User | Broadcast | Recommendation | Lastfm | Unknown
  type rating = Love | Ban | Skip
  type action = NowPlaying | Submit
  type song = { artist : string; track: string; time: float option; 
                source : source option; rating : rating option ;
                length : float option ; album : string option ; 
                trackauth : string option ; tracknumber : int option; 
                musicbrainzid : string option }
  type error = Http of string | Banned | Badauth | Badtime
              | Failed of string | UnknownError of string | Success
              | Internal of string | BadData of string

  exception Error of error
  
  val string_of_error : error -> string
  val base_port : int ref
  val base_host : string ref
  val get_song :
      ?time:float ->
      ?source:source ->
      ?rating:rating ->
      ?length:float ->
      ?album:string ->
      ?tracknumber:int ->
      ?musicbrainzid:string ->
      ?trackauth:string ->
      artist:string ->
      track:string ->
      unit ->
      song
  val check_song : song -> action -> unit
  val do_np : ?timeout:float -> ?host:(string*int) -> client -> login -> song -> unit
  val do_submit : ?timeout:float -> ?host:(string*int) -> client -> login -> song list -> (error * song) list
  val handshake : ?timeout:float -> ?host:(string*int) -> client -> login -> string
  val np : ?timeout:float -> string -> song -> unit
  val submit : ?timeout:float -> string -> song list -> (error * song) list
end

module Audioscrobbler_generic(Http:Http_t) =
  struct

    (* See http://www.audioscrobbler.net/development/protocol/
     * For protocol description *)

    (* Data types *)
    type source = User | Broadcast | Recommendation | Lastfm | Unknown
    type rating = Love | Ban | Skip
    type action = NowPlaying | Submit

    (* song submission type *)
    type song = { artist : string; track: string; time: float option; 
                  source : source option; rating : rating option ;
                  length : float option ; album : string option ;
                  trackauth : string option ; tracknumber : int option; 
                  musicbrainzid : string option }

    type error = Http of string | Banned | Badauth | Badtime
                | Failed of string | UnknownError of string | Success
                | Internal of string | BadData of string
    exception Error of error
 
    let string_of_error e = 
      match e with
        | Http s -> Printf.sprintf "http connection failed: %s" s
        | Banned -> "banned client"
        | Badauth -> "wrong login/password"
        | Badtime -> "wrong timestamp, check your clock"
        | Failed s -> Printf.sprintf "failure: %s" s
        | UnknownError s -> Printf.sprintf "unknown error: %s" s
        | Internal s -> Printf.sprintf  "erreur interne: %s" s
        | BadData s -> Printf.sprintf "Wrong song data: %s" s
        | Success -> "success!"

    let error_of_response s = 
      try
        let parse s = 
           let regexp = Pcre.regexp "FAILED\\s([^\\r\\n]*)" in
           let sub = Pcre.exec ~rex:regexp s in
           Failed (Pcre.get_substring sub 1)
        in
	let test (p,e) = 
	  try
	    ignore(Pcre.exec ~pat:p s) ;
	    raise (Error e)
	  with
	    | Not_found -> ()
	in
	let values = [("OK",Success);
	              ("BANNED",Banned);
		      ("BADAUTH",Badauth);
		      ("BADTIME",Badtime)]
        in
	try
	  List.iter test values ;
          parse s
	with 
	  | Error e -> e
      with
        | Not_found -> UnknownError "unrecognized response code"

    (* We wrap Http.request to raise an internal exception. *)
    let request ?post ?timeout ?headers ?port ~host req =
      try
        Http.request ?post ?timeout ?headers ?port ~host req
      with
        | Http.Http s -> raise (Error (Http s))

    (* Protocol constants *)
    let version = "1.2.1"
    let base_port = ref 80
    let base_host = ref "post.audioscrobbler.com"
    let handshake_req = Printf.sprintf "/?hs=true&p=%s&c=%s&v=%s&u=%s&t=%s&a=%s" version
    (* sessions contains (user,pass) => id *)
    let sessions = Hashtbl.create 1
    (* urls contains id => (np_url,submit_url) *)
    let urls = Hashtbl.create 1
    let raise e = raise (Error e)
    let arg_value_string x = 
      match x with 
        | Some e -> e
        | None -> ""
    let arg_value_int x = 
      match x with 
        | Some e -> string_of_int e
        | None -> ""
    let arg_value_float x =
      match x with
        | Some e -> Printf.sprintf "%.0f" e
        | None -> ""

    let clear sessionid =
        let keys = Hashtbl.fold (fun a b r -> if b = sessionid then a::r else r)
                      sessions []
        in
        ignore (List.map (fun x -> Hashtbl.remove sessions x) keys) ;
        Hashtbl.remove urls sessionid

    let handshake ?timeout ?host client login = 
      let client,version,user,pass = 
         client.client,client.version,
         login.user,login.password
      in
      let host,port =
        match host with
          | Some (x,y) -> x,y
          | None -> !base_host,!base_port
      in
      try 
        Hashtbl.find sessions (user,pass,(host,port))
      with
        | Not_found ->
           let timestamp = Printf.sprintf "%.0f" (Unix.time ()) in
           let pass_digest = Digest.string pass in
           let token = Digest.string((Digest.to_hex pass_digest) ^ timestamp) in
           let req = handshake_req client version user timestamp (Digest.to_hex token) in
           let ans = request ?timeout ~host ~port req in
           let state,id,v = 
             try
               let lines = Pcre.split ~pat:"[\r\n]+" ans in
               match lines with
                 | state :: id :: [a; b] -> state,id,(a,b)
                 | _ -> raise (error_of_response ans)
             with
               | Not_found -> raise (error_of_response ans)
           in
           match error_of_response state with
             | Success -> Hashtbl.replace sessions (user,pass,(host,port)) id; 
                          Hashtbl.replace urls id v; id
             | e -> raise e

    let check_song s a = 
      match s.source with
        | None when a = Submit ->
             raise (BadData "source field is required for submit action")
        | _ -> () ;
      match s.length with
        | None when a = Submit && s.source = Some User -> 
            raise (BadData "length required when source is User")
        | _ -> () ;
      match s.time with
        | None when s.source = Some User ->
             raise (BadData "time required when source is User")
        | _ -> () ;
      match s.trackauth with
        | None when s.source = Some Lastfm -> 
             raise (BadData "trackauth required when source is Lastfm")
        | _ -> () ;
      match s.rating with 
        | Some Ban
        | Some Skip when s.source <> Some Lastfm -> 
             raise (BadData "bad rating value (ban and skip are for lastfm sources only)")
        | _ -> ()


    let audioscrobbler_post ?timeout id base_url values = 
      let url = Neturl.parse_url base_url in
      let host = 
        try
          Neturl.url_host url
        with
          (* This should not happend... *)
          | Not_found -> !base_host
      in
      let port = 
        try
          Neturl.url_port url
        with
          | Not_found -> !base_port 
      in
      let req = String.concat "/" (Neturl.url_path url) in
      let args = List.map (fun (a,b) -> 
                             Printf.sprintf "%s=%s" 
                                a (Netencoding.Url.encode b) )
                            values
      in
      let post = String.concat "&" args in
      let headers = [("Content-type","application/x-www-form-urlencoded")] in
      let ans = request ?timeout ~post:post ~headers:headers ~host:host ~port:port req in
      match error_of_response ans with
        | Success -> ()
        | e -> clear id; raise e

    let np ?timeout id song = 
      let url,_ = 
        try 
          Hashtbl.find urls id
        with
          | Not_found -> raise Badauth
      in
      check_song song NowPlaying ;
      let values = [("s",id);("a",song.artist);("t",song.track);
                    ("album",arg_value_string song.album);
                    ("l",arg_value_float song.length);
                    ("n",arg_value_int song.tracknumber);
                    ("m",arg_value_string song.musicbrainzid)] 
      in
      audioscrobbler_post ?timeout id url values

     let submit ?timeout id songs = 
      let _,url = 
        try 
          Hashtbl.find urls id
        with
          | Not_found -> raise (Internal (Printf.sprintf "No session data for session ID %s" id))
      in
      let count = ref 0 in
      let failed = ref [] in
      let args = ref [("s",id)] in
      let get_arg = fun s -> Printf.sprintf "%s[%d]" s !count in
      let add_arg song =
        try 
          check_song song Submit ;
          let ar,tr,t,s,r,l,al,n,m,x =
             song.artist, song.track,song.time,
             song.source, song.rating,
             song.length, song.album,
             song.tracknumber, song.musicbrainzid,
             song.trackauth
          in
          let l = match l with
                    | None -> ""
                    | Some s -> Printf.sprintf "%.0f" s
          in
          let t = match t with
                    | Some t -> Printf.sprintf "%.0f" t
                    | None -> ""
          in
          let x = match x with
                    | Some x -> x
                    | None -> ""
          in
          let s =  match s with 
                     | Some User -> "P"
                     | Some Broadcast -> "R"
                     | Some Recommendation -> "E"
                     | Some Lastfm -> "L"
                     | Some Unknown -> "U"
                     | None -> raise (Internal "source field is required for submit")
          in
          let r =  match r with 
                     | Some Love -> "L"
                     | Some Ban -> "B"
                     | Some Skip -> "S"
                     | None -> ""
          in
          args := [(get_arg "a",ar);(get_arg "t",tr);(get_arg "i",t);
                   (get_arg "o",s ^ x);(get_arg "r",r);(get_arg "l",l);
                   (get_arg "b",arg_value_string al);(get_arg "n",arg_value_int n);
                   (get_arg "m",arg_value_string m)] @ (!args)
       with
         | Error e -> failed := (e,song) :: !failed 
     in
     List.iter add_arg songs ;
     audioscrobbler_post ?timeout id url (!args) ;
     !failed

    let get_song ?time ?source ?rating
                 ?length ?album ?tracknumber 
		 ?musicbrainzid ?trackauth 
		 ~artist ~track () = 
    { artist = artist ; track = track ; time = time  ;
      source = source; rating = rating ; length = length ;
      album = album ; tracknumber = tracknumber ; 
      musicbrainzid = musicbrainzid ; trackauth = trackauth }

    let do_np ?timeout ?host client login song = 
      let id = handshake ?host client login in
      try
        np ?timeout id song
      with
        | Error Badauth -> (* Retry in case of expired session id *)
	                   clear id ;
	                   let id = handshake ?host client login in
		           np id song

    let do_submit ?timeout ?host client login songs = 
      let id = handshake ?host client login in
      try 
        submit ?timeout id songs
      with
        | Error Badauth -> (* Retry in case of expired session id *)
                           clear id ;
                           let id = handshake ?host client login in
                           submit id songs 

  end

module Audioscrobbler = Audioscrobbler_generic(Http_ocamlnet)

module type Radio_t = 
sig
  type track = (string * string) list * string
  type error = Http of string | Auth of string | Adjust of string*string | Playlist | Empty

  exception Error of error
  
  val string_of_error : error -> string
  val base_host : string ref 
  val get : ?timeout:float -> string -> track list
  val parse : string -> login*string*(string option)
  val init : ?timeout:float -> login -> string
  val adjust : ?timeout:float -> string -> string -> (string*string) list
  val playlist : ?timeout:float -> string -> string option -> string
  val tracks : ?timeout:float -> string -> string option -> track list
  val clear : string -> unit
end

module Radio_generic(Http:Http_t) =
  struct

    (* Type for track datas 
     * A track is a list of "field","value" metadatas
     * and an uri *)
    type track = (string * string) list * string

    type session = { login : login; station : ((string*string) list*string) option;
                     base_url : string; playlist_url : string option; 
                     base_path: string }

    type error = Http of string | Auth of string | Adjust of string*string | Playlist | Empty
    exception Error of error
    exception Internal of string

    let string_of_error e = 
      match e with
        | Http s -> Printf.sprintf "http connection failed: %s" s
        | Auth s -> Printf.sprintf "could not open session:\n%s" s
        | Adjust (s,s') -> Printf.sprintf "could not adjust station to %s:\n%s\nIs the URI valid ?" s s'
        | Playlist -> "error while parsing the playlist"
        | Empty -> "no files available"

    (* We wrap Http.request to raise an internal exception. *)
    let request ?post ?timeout ?headers ?port ~host req =
      try
        Http.request ?post ?timeout ?headers ?port ~host req
      with
        | Http.Http s -> raise (Error (Http s))
   
    let _raise = raise
    let raise e = raise (Error e)

    (* Some constant for the protocol *)
    let base_host = ref "ext.last.fm"
    let sessions = Hashtbl.create 10
    let registered_handshake = Printf.sprintf "/radio/handshake.php?username=%s&passwordmd5=%s"
    let station_set base id url = Printf.sprintf "%s/adjust.php?session=%s&url=%s" base id url

    let playlist_req id options = 
      let d = 
        try
         Hashtbl.find sessions id
        with
          | Not_found -> 
              raise (Auth (Printf.sprintf "No session registered for id %s" id))
      in
      let options = 
        match options with
          | Some s -> s
          | None -> ""
      in
      let url = 
        match d.playlist_url with
          | None -> "http://ws.audioscrobbler.com/radio/xspf.php"
          | Some s -> s
      in
      Printf.sprintf "%s?sk=%s&%s" url id options

    let playlist ?timeout id options = 
       let url = playlist_req id options in
       let url = Neturl.parse_url url in
       let host = Neturl.url_host url in
       let port = 
        try
         Neturl.url_port url 
        with Not_found -> 80
       in
       let query = Neturl.url_query url in
       let path = 
         Neturl.join_path 
          (Neturl.url_path url) 
       in
       let req = Printf.sprintf "%s?%s" path query in
       let data = request ?timeout ~port ~host req in
       let data = Netencoding.Base64.decode data in
       Netencoding.Url.decode data
    
    (* Some parsing functions *)

    let parse_args s = 
      let rex = Pcre.regexp "[&\n]" in
      let values = Pcre.split ~rex:rex s in
      let split s l = 
        try
          let sub = Pcre.exec ~pat:"([^=]*)=(.*)" s in
          (Pcre.get_substring sub 1,Pcre.get_substring sub 2) :: l
        with
          | Not_found -> l
      in
      List.fold_right split values []
    
    let parse_handshake s =
      let params = parse_args s in
      try
        let f x = List.assoc x params in
        let g x = try Some (f x) with Not_found -> None in
          f "session",g "playlist_url", 
          f "base_url", f "base_path"
      with
        | Not_found -> raise (Auth s)

    let adjust_pat = "response=OK"
    let check_adjust s =
        Pcre.pmatch ~pat:adjust_pat s
    
    let opt_split_rex = Pcre.regexp "^([^?]+)\\?(.+)$"
    let opt_parse s =
        try
          let sub = Pcre.exec ~rex:opt_split_rex s in
          Pcre.get_substring sub 1, Some (Pcre.get_substring sub 2)
        with
          | Not_found -> s,None

    let auth_split_rex = Pcre.regexp "^lastfm://([^:]+):([^@]+)@(.+)$"
    let parse uri = 
        try
          let sub = Pcre.exec ~rex:auth_split_rex uri in
          let data = Pcre.get_substring sub 3 in
          let station,options = opt_parse data in
          let user,password = Pcre.get_substring sub 1,
                              Pcre.get_substring sub 2
          in
          { user = user ; password = password },
          Printf.sprintf "lastfm://%s" station,options
        with
          | Not_found -> raise (Auth "Could not find login/password.") 
   
    (* Core stuff.. *)
    
    let clear id =
      Hashtbl.remove sessions id
    
    let init ?timeout login = 
     try
      Hashtbl.iter (fun x d -> 
	              if d.login = login then _raise (Internal x))
                       sessions ;
      _raise Not_found
     with
       | Not_found -> 
           let user,password = login.user,login.password in
           let password = Digest.to_hex (Digest.string password) in
           let ret = request ?timeout ~host:!base_host (registered_handshake 
             (Netencoding.Url.encode user) password)
           in
           let id,playlist_url,
             base_url,base_path = 
               parse_handshake ret 
           in
           Hashtbl.replace sessions id {playlist_url=playlist_url;
                                                 base_url=base_url;
                                                 base_path=base_path;
                                                 login=login;
                                                 station=None} ;
           id
       | Internal x -> x
    
    let adjust ?timeout id req = 
      let d = 
        try
          Hashtbl.find sessions id
	with
	  | Not_found -> 
	      raise (Auth (Printf.sprintf "No session registered for id %s" id))
      in
      let base_url,base_path = d.base_url,d.base_path in
        match d.station with
	  | Some (a,s) when s = req -> a
          | _ ->
                let http_req = station_set base_path id 
                   (Netencoding.Url.encode req)
                in
                let ret = request ?timeout ~host:base_url http_req in
                if check_adjust ret then
                 let args = parse_args ret in
                 ( Hashtbl.replace sessions id 
                        {login=d.login;station=Some (args,req);
                         playlist_url=d.playlist_url;
                         base_url=base_url;base_path=base_path} ;
                  args )
                else
                  begin
                    clear id;
                    raise (Adjust (req,ret))
                  end
    
    let tracks ?timeout id options = 
      try
        let playlist = playlist ?timeout id options in
        Xmlplaylist.tracks playlist
      with
        | Xmlplaylist.Error e -> clear id; raise Playlist 
        | Error e -> clear id; raise e
    
    let get ?timeout uri = 
      let login,station,options = parse uri in
      let id = init ?timeout login in
      try
        ignore(adjust ?timeout id station);
        tracks ?timeout id options
      with Error _ -> 
          (* Retry in case of expired session *)
	  clear id;
          let id = init login in
	  ignore(adjust id station);
	  tracks id options

end

module Radio = Radio_generic(Http_ocamlnet)

