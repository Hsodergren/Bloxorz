module Pos = struct
  type t = {row : int; col: int} [@@deriving ord, make, show, eq]
end

module Move = struct
  type t = [ `Right | `Left | `Up | `Down ]
end

module Block : sig
  type t = private | Standing of Pos.t
                   | Horizontal of Pos.t * Pos.t
                   | Vertical of Pos.t * Pos.t [@@deriving show, eq, ord]

  val move: t -> Move.t -> t
  val right: t -> t
  val left: t -> t
  val up: t -> t
  val down: t -> t

  val at: Pos.t -> t
  val moves: t -> (t * Move.t) list
end = struct
  type t = | Standing of Pos.t
           | Horizontal of Pos.t * Pos.t
           | Vertical of Pos.t * Pos.t [@@deriving show, eq, ord]

  let at pos = Standing pos

  let right = function
    | Standing {row;col} -> Horizontal ({row; col=col+1},{row; col=col+2})
    | Horizontal ({row;_},{col=c2; _}) -> Standing {row;col=c2+1}
    | Vertical ({row=r1;col},{row=r2; _}) ->Vertical ({col=col+1;row=r1}, {col=col+1;row=r2})

  let left = function
    | Standing {row;col} -> Horizontal ({row; col=col-2},{row; col=col-1})
    | Horizontal ({row;col=c1},_) ->Standing {row;col=c1-1}
    | Vertical ({row=r1;col},{row=r2; _}) -> Vertical ({col=col-1;row=r1}, {col=col-1;row=r2})

  let up = function
    | Standing {row;col} -> Vertical ({row=row-2; col},{row=row-1; col})
    | Horizontal ({row;col=c1},{col=c2; _}) -> Horizontal ({row=row-1;col=c1}, {row=row-1;col=c2})
    | Vertical ({row=r1;col},_) -> Standing {col;row=r1-1}

  let down = function
    | Standing {row;col} -> Vertical ({row=row+1; col},{row=row+2; col})
    | Horizontal ({row; col=c1},{col=c2; _}) -> Horizontal ({row=row+1;col=c1}, {row=row+1;col=c2})
    | Vertical (_, {row=r2;col}) -> Standing {col;row=r2+1}

  let move b = function
    | `Right -> right b
    | `Left -> left b
    | `Up -> up b
    | `Down -> down b

  let moves b = [ right b, `Right ; left b, `Left ; up b, `Up ; down b, `Down ]
end

module TerrainBlock = struct
  type t = [ `Empty
           | `Block
           | `Glass
           | `Button of int
           | `Start
           | `Goal ] [@@deriving show, eq, ord]

  let of_char = function
    | 'o' -> `Block
    | 'g' -> `Glass
    | 'S' -> `Start
    | 'G' -> `Goal
    | '0'..'9' as c -> `Button (Char.code c - Char.code '0')
    | _ -> `Empty
end

module Terrain : sig
  type t [@@deriving ord, eq]

  val of_string: string -> (t,string) result
  val of_string_exn: string -> t
  val show: t -> string

  val empty: t

  val start: t -> Pos.t
  val goal: t -> Pos.t

  val push_button: t -> int -> Block.t -> t
  val get: pos:Pos.t -> t -> TerrainBlock.t
end = struct
  module M = Map.Make(Pos)
  module B = Map.Make(Int)
  let enumerate l = List.mapi (fun i v -> i,v) l
  let chars_of_str s = List.of_seq (String.to_seq s)

  type t = {
    terrain: terrain
  ; buttons: button_action B.t
  }
  and terrain = TerrainBlock.t M.t
  and button_action = Block.t -> t -> t

  let empty = {terrain = M.empty; buttons = B.empty}

  let push_button ({buttons;_} as t) id block =
    match B.find_opt id buttons with
    | Some f -> f block t
    | None -> t

  let update ~pos ~block t =
    match block with
    | `Empty -> {t with terrain = M.remove pos t.terrain}
    | block -> {t with terrain = M.add pos block t.terrain}

  let equal {terrain=t1;_} {terrain=t2;_} = M.equal TerrainBlock.equal t1 t2
  let compare {terrain=t1;_} {terrain=t2;_} = M.compare TerrainBlock.compare t1 t2

  let get ~pos {terrain;_} = Option.value (M.find_opt pos terrain) ~default:`Empty

  module Parser = struct
    open Angstrom

    let is_digit = function '0'..'9' -> true | _ -> false

    let int = take_while1 is_digit >>| int_of_string
    let tb = any_char >>| fun c -> if c='t' then `Toggle else TerrainBlock.of_char c

    type req = Standing | All
    let req =
      peek_char_fail >>= (function
      | 'S' -> return Standing
      | 'A' -> return All
      | _ -> fail "requirement must be either 'S' or 'A'") <* advance 1

    let req_fullfilled req block =
      match req, block with
      | Standing, (Block.Standing _) -> true
      | Standing, _ -> false
      | _ -> true

    let pos =
      char '(' *> int <* char ','  >>= fun row ->
      int <* char ')' >>= fun col ->
      return (Pos.make ~row ~col)

    let value =
      (fun pos block -> pos,block) <$> pos <* char '=' <*> tb

    let get_button_fun updates req block terrain =
      let rec add_block terrain (pos,block) =
        match block with
        | `Toggle -> begin
            let new_block = match get terrain ~pos with
              | `Empty -> `Block
              | _ -> `Empty
            in
            add_block terrain (pos,new_block)
        end
        | #TerrainBlock.t as block -> update terrain ~pos ~block
      in
      if req_fullfilled req block
      then List.fold_left add_block terrain updates
      else terrain

    let parse_b =
      int <* char ':' >>= fun id ->
      req <* char ':' >>= fun req ->
      sep_by1 (char ';') value >>= fun updates ->
      return (id,get_button_fun updates req)

    let parse_terrain lines =
      let (let*) l f = List.concat_map f l in
      let blocks =
        let* row,line = enumerate lines in
        let* col,c = enumerate @@ chars_of_str line in
        [(Pos.make ~row ~col, TerrainBlock.of_char c)]
      in
      Ok (M.of_seq (List.to_seq blocks))

    let rec parse_buttons lines =
      match lines with
      | [] -> Ok B.empty
      | hd::tl ->
        match parse_string ~consume:All parse_b hd with
        | Ok (id,f) ->
          Result.map (fun t -> B.add id f t) (parse_buttons tl)
        | Error s -> Error s

    let sep_terrain_button lines =
      let rec aux lines acc =
        match lines with
        | "" :: tl -> List.rev acc,tl
        | hd::tl -> aux tl (hd::acc)
        | [] -> List.rev acc, []
      in
      aux (List.map String.trim lines) []

    let parse_string str =
      let (let*) = Result.bind in
      let ter,but =
        String.trim str
        |> String.split_on_char '\n'
        |> sep_terrain_button
      in
      let* terrain = parse_terrain ter in
      let* buttons = parse_buttons but in
      Ok {terrain;buttons}
  end

  let of_string str = Parser.parse_string str

  let of_string_exn str =
    match of_string str with
    | Ok v -> v
    | Error s -> failwith s

  let find_block block t =
    M.bindings t
    |> List.find (fun (_,b) -> b = block)
    |> fst

  let start {terrain;_} = find_block `Start terrain
  let goal {terrain;_} = find_block `Goal terrain


  let show {terrain=t;_} =
    Seq.map
      (fun (pos,block) -> Printf.sprintf "%s : %s" (Pos.show pos) (TerrainBlock.show block))
      (M.to_seq t)
    |> Seq.fold_left (fun acc str -> acc ^ "\n" ^ str ) ""
end

module GameDef : sig
  module State : sig
    type t

    val moves: t -> Move.t list
  end
  val solve: Terrain.t -> State.t option
end = struct
  module State = struct
    type t = {
      current: Block.t
    ; moves: Move.t list [@compare fun _ _ -> 0] [@equal fun _ _ -> true]
    ; terrain: Terrain.t
    ; goal: Pos.t } [@@deriving ord]

    let initial terrain = { current=Block.at @@ Terrain.start terrain
                          ; terrain
                          ; moves = []
                          ; goal=Terrain.goal terrain }

    let at_goal {current;goal;_} =
      match current with
      | Standing pos -> Pos.equal pos goal
      | _ -> false

    let moves {moves;_} = List.rev moves
  end

  let legal_block block terrain =
    let get pos = Terrain.get ~pos terrain in
    match block with
    | Block.Standing pos -> begin
        match get pos with
        | `Glass | `Empty -> `Illegal
        | `Block | `Start | `Goal -> `Legal terrain
        | `Button i -> `Legal (Terrain.push_button terrain i block)
      end
    | Horizontal (p1,p2) | Vertical (p1,p2) ->
      let valid terrain = function | `Empty -> `Illegal
                                   | `Glass | `Block | `Start | `Goal -> `Legal terrain
                                   | `Button i -> `Legal (Terrain.push_button terrain i block) in
      let bind l f = match l with | `Legal t -> f t | l -> l in
      bind (valid terrain (get p1)) (fun t -> valid t (get p2))

  let legal_moves block terrain =
    let (let*) l f = List.concat_map f l in
    let* (b,m) = Block.moves block in
    match legal_block b terrain with
    | `Legal t -> [(b,m,t)]
    | `Illegal -> []

  let solve terrain =
    let module S = Set.Make(State) in
    let q = Queue.create () in
    let state = State.initial terrain in
    let rec find_solution visited =
      match Queue.take_opt q with
      | None -> None
      | Some state when S.mem state visited -> find_solution visited
      | Some ({State.current;terrain;_} as state) ->
        if State.at_goal state then Some state
        else begin
          let new_states =
            List.map
              (fun (b,m,t) -> {state with current=b; moves=m::state.moves; terrain=t})
              (legal_moves current terrain)
          in
          Queue.add_seq q (List.to_seq new_states);
          find_solution (S.add state visited)
        end
    in
    Queue.push state q;
    find_solution S.empty
end
