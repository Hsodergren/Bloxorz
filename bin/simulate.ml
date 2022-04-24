open Bloxorz

let block_to_list = function
  | Block.Standing p -> [ p ]
  | Vertical (p1, p2) | Horizontal (p1, p2) -> [ p1; p2 ]

let min_max l =
  let f (min, max) v =
    if v > max then (min, v) else if v < min then (v, max) else (min, max)
  in
  let fst = List.hd l in
  ListLabels.fold_left ~f ~init:(fst, fst) (List.tl l)

let ( >>| ) o f = Option.map f o
let block_char l p = ListLabels.find_opt ~f:(Pos.equal p) l >>| fun _ -> 'X'

let terrain_char l p =
  List.find_opt (fun (p2, _) -> Pos.equal p p2) l >>| fun (_, m) ->
  TerrainBlock.to_char m

let rec first l def =
  match l with
  | [] -> def
  | hd :: tl -> ( match Lazy.force hd with Some x -> x | None -> first tl def)

let to_chars terrain block =
  let ( let* ) l f = List.concat_map f l in
  let t, b = (Terrain.to_alist terrain, block_to_list block) in
  let minrow, maxrow = min_max @@ List.map (fun (p, _) -> Pos.row p) t in
  let mincol, maxcol = min_max @@ List.map (fun (p, _) -> Pos.col p) t in
  let* row = List.init (maxrow - minrow + 1) Fun.id in
  [
    (let* col = List.init (maxcol - mincol + 1) Fun.id in
     let pos = Pos.make ~row:(row + minrow) ~col:(col + mincol) in
     [ first [ lazy (block_char b pos); lazy (terrain_char t pos) ] ' ' ]);
  ]

let print terrain block =
  let lines = to_chars terrain block in
  let rec print_line = function
    | [] -> Out_channel.(output_char stdout '\n')
    | c :: tl ->
        Out_channel.(output_char stdout c);
        print_line tl
  in
  let rec aux = function
    | [] -> ()
    | line :: tl ->
        print_line line;
        aux tl
  in
  aux lines

let delay = ref 1.

let rec simulate block terrain moves =
  match moves with
  | [] -> ()
  | (move, new_terrain) :: tl ->
      print terrain block;
      Out_channel.output_char stdout '\n';
      Out_channel.flush stdout;
      ignore @@ Unix.sleepf !delay;
      simulate (Block.move block move) new_terrain tl

let argv = Sys.argv
let file = argv.(1)
let () = try delay := Float.of_string argv.(2) with _ -> ()

let () =
  let content =
    In_channel.(
      with_open_text file (fun inc ->
          really_input_string inc (length inc |> Int64.to_int)))
    |> Option.get
  in
  let terrain = Terrain.of_string_exn content in
  match GameDef.solve terrain with
  | None -> print_endline "no solution"
  | Some state ->
      let start = Terrain.start terrain in
      let moves = GameDef.State.moves state in
      simulate (Block.at start) terrain moves;
      Printf.printf "%d moves\n%!" (List.length moves)
