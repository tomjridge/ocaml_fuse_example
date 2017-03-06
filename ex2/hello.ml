open Unix
open LargeFile
open Bigarray
open Fuse
(* open Yojson.Safe

type op = Read of string * int * int | Write of string * int * int [@@deriving yojson]

   ocamlfuse can't work with yojson - ocamlfuse has a result module which
   conflicts 

*)

let default_stats = LargeFile.stat "."
let fname = "image"
let name = "/" ^ fname
let backing_file = "/tmp/store"
let size0 = Int64.(of_int 10 |> mul (of_int 1024) |> mul (of_int 1024))
let default_creat_mode = 0o666

let safely f = try f () with e -> Printexc.to_string e |> print_endline; raise e

let fd0 = 
  safely (fun () -> Unix.openfile backing_file Unix.([O_CREAT;O_RDWR]) default_creat_mode)

let do_getattr path = safely (fun () ->
  if path = "/" then default_stats
  else if path = name then 
    { default_stats with 
      st_nlink = 1;
      st_kind = S_REG;
      st_perm = default_creat_mode;
      st_size = size0 }
  else raise (Unix_error (ENOENT,"stat",path)))

let do_readdir path _ = safely (fun () -> 
  if path = "/" then [".";"..";fname]
  else raise (Unix_error (ENOENT,"readdir",path)))

let do_fopen path flags = safely (fun () -> 
  if path = name then None
  else raise (Unix_error (ENOENT,"open",path)))

let do_read path buf ofs _ = safely (fun () -> 
  if path = name then
    try
      (* may need offsets at Int64 *)
      let num_read = ExtUnixAll.LargeFile.BA.single_pread fd0 ofs buf in
      num_read
    with _ -> (print_endline "do_read:!"; raise (Unix_error (ENOENT,"read",path)))
  else raise (Unix_error (ENOENT,"read",path)))

let do_write path buf ofs _ = safely (fun () -> 
  if path = name then
    try 
      let num_read = ExtUnixAll.LargeFile.BA.pwrite fd0 ofs buf in
      num_read
    with _ -> (print_endline "do_write:!"; raise (Unix_error (ENOENT,"write",path)))
  else raise (Unix_error (ENOENT,"write",path)))


(* following apparently required for a block device backed by a file on a fuse fs ? *)
(* let do_fsync path ds hnd = () *)

(* let do_getxattr s1 s2 = "?" *)

let _ =
  main Sys.argv 
    { default_operations with 
      getattr = do_getattr;
	    opendir = (fun path flags -> Unix.close (Unix.openfile path flags 0);None);
      readdir = do_readdir;
      fopen = do_fopen;
      read = do_read;
      write = do_write;
      truncate = (fun _ _ -> print_endline "truncate"; ());
(*      fsync = do_fsync;
      getxattr = do_getxattr; *)
    }
