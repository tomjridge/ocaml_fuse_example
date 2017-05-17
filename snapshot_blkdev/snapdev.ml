open Unix
open LargeFile
open Bigarray
open Fuse

let default_stats = LargeFile.stat "."
let fname = "image"
let name = "/" ^ fname
let backing_file = "./store"
let size0 = Int64.(of_int 10 |> mul (of_int 1024) |> mul (of_int 1024))
let default_creat_mode = 0o666

let safely f = try f () with e -> Printexc.to_string e |> print_endline; raise e

let fd0 = 
  safely (fun () -> Unix.openfile backing_file Unix.([O_CREAT;O_RDWR]) default_creat_mode)

(* btree stuff ---------------------------------------- *)

(* 

we store idx -> block map in a btree on top of a recycling filestore;
btree root pointer is written into block 0, everything else is append-only

snapshots by recording the btree root at some moment in time

sync by copying the tail of the file to the
destination and updating block 0 

*)

open Btree_api
open Block_device
open File_store

module RF = File_store.Recycling_filestore

module MAP = Map_idx_blk.Make(RF)

module RM = MAP.RM

module KV = RM.KV

module FS = Filestore

open KV
open Btree_api

type t = {store: RF.store; page_ref: RF.page_ref}

(* FIXME need LRU cache? *)

(* FIXME following copied from kv_store_small.ml *)

(* initialize *)
let init' = RF.(
    lift (FS.set_free 0) |> Sem.bind (fun () -> 
        alloc_block () |> Sem.bind (fun r ->
            assert (r=0);
            RM.empty () 
          )))

let from_file ~fn ~create ~init = (
  assert (not create || init); (* create --> init *)
  let fd = Blkdev_on_fd.from_file ~fn ~create ~init in
  let fs = Filestore.from_fd ~fd ~init in
  let store = RF.from_filestore fs in
  let t = (
    match init with
    | true -> (
        init' |> Sem.run store 
        |> function (store,Ok page_ref) -> {store;page_ref})
    | false -> (
        (* read from block 0 *)
        RF.read 0 |> Sem.bind (fun blk -> 
            Pickle.Examples.u_int |> Sem.run blk
            |> (fun (_,Ok i) -> Sem.return i))
        |> Sem.run store |> function (store,Ok page_ref) -> {store;page_ref})
  )
  in
  t)

let s0 = (from_file "./store" true true)

let s0' = ref (s0.store,s0.page_ref)

let run x = x |> Sem.run_ref s0'


let block_size = 4096 (* expected *)

module Block = Mk_block(struct let block_size=block_size end)

(* fuse stuff ---------------------------------------- *)


let string_to_fuse_buf s = (
)

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
      assert (Int64.rem ofs (Int64.of_int block_size) = Int64.zero);
      (* we want to return a single block FIXME to begin with *)
      (* may need offsets at Int64 *)
      let i = ((Int64.to_int ofs) / block_size) in
      let blk = RM.find i |> run in
      let blk = 
        match blk with
        | None -> Block.empty ()
        | Some blk -> blk
      in
      assert (Bigarray.Array1.dim buf >= block_size);
      (* copy to buf *)
      for j = 0 to block_size -1 do
        buf.{j} <- String.get blk j
      done;
      block_size
    with _ -> (print_endline "do_read:!"; raise (Unix_error (ENOENT,"read",path)))
  else raise (Unix_error (ENOENT,"read",path)))

let do_write path buf ofs _ = safely (fun () -> 
  if path = name then
    try 
      assert (Int64.rem ofs (Int64.of_int block_size) = Int64.zero);
      let i = ((Int64.to_int ofs) / block_size) in
      let blk = Block.empty () in
      for j = 0 to block_size -1 do
        blk.[j] <- (buf.{j})  (* mutate string! FIXME *)
      done;
      (* insert into tree *)
      let _ = RM.insert i blk |> run in
      block_size 
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
