// For more information see https://aka.ms/fsharp-console-apps
open Printf
open System
open System.Drawing
open System.Windows.Forms
open System.Threading
open System.Timers


let piece_width = 4
let piece_height = 4

let field_width = 14
let field_height = 24

let field = [| for i in 1 ..field_width -> [| for j in 1 .. field_height -> 0  |] |]
let field_color = [| for i in 1 ..field_width -> [| for j in 1 .. field_height -> Color.Black |] |]
let first_piece = ref [| for i in 1 ..piece_width -> [| for j in 1 .. piece_height -> 0  |] |]
let first_color = ref Color.Transparent

let second_piece = ref [| for i in 1 ..piece_width -> [| for j in 1 .. piece_height -> 0  |] |]
let second_color = ref Color.Transparent

let third_piece = ref [| for i in 1 ..piece_width -> [| for j in 1 .. piece_height -> 0  |] |]
let third_color = ref Color.Transparent


(*
let first_piece = ref [| for i in 1 ..piece_width -> [| for j in 1 .. piece_height -> 0  |] |]
let first_color = ref [| for i in 1 ..piece_width -> [| for j in 1 .. piece_height -> Color.Transparent  |] |]

let second_piece = ref [| for i in 1 ..piece_width -> [| for j in 1 .. piece_height -> 0  |] |]
let second_color = ref [| for i in 1 ..piece_width -> [| for j in 1 .. piece_height -> Color.Transparent  |] |]

let third_piece = ref [| for i in 1 ..piece_width -> [| for j in 1 .. piece_height -> 0  |] |]
let third_color = ref [| for i in 1 ..piece_width -> [| for j in 1 .. piece_height -> Color.Transparent  |] |]
*)
let loc = ref (0,0)

let get_x loc = fst loc.contents
let get_y loc = snd loc.contents

type move_to =
| Left | Right | Down

let r = System.Random()


let create_piece () =
  let piece = [| for i in 1 ..piece_width -> [| for j in 1 .. piece_height -> 0  |] |] in
  let color =  ref Color.Transparent in
  (
  match r.Next(0,7) with
  | 0 -> 
    //printf "%d" 0;
    piece[1][1] <- 1;
    piece[2][1] <- 1;
    piece[1][2] <- 1;
    piece[2][2] <- 1;
    color.contents <- Color.White;
  | 1 -> 
    //printf "%d" 1;
    piece[1][0] <- 1;
    piece[1][1] <- 1;
    piece[1][2] <- 1;
    piece[1][3] <- 1;
    color.contents <- Color.Red;

  | 2 -> 
    //printf "%d" 2;
    piece[1][1] <- 1;
    piece[1][2] <- 1;
    piece[2][2] <- 1;
    piece[1][3] <- 1;
    color.contents <- Color.Orange;

  | 3 -> 
    //printf "%d" 3;
    piece[1][1] <- 1;
    piece[2][1] <- 1;
    piece[1][2] <- 1;
    piece[1][3] <- 1;
    color.contents <- Color.Blue;

  | 4 -> 
    //printf "%d" 4;
    piece[1][1] <- 1;
    piece[2][1] <- 1;
    piece[2][2] <- 1;
    piece[2][3] <- 1;
    color.contents <- Color.Purple;

  | 5 -> 
    //printf "%d" 5;
    piece[2][1] <- 1;
    piece[1][2] <- 1;
    piece[2][2] <- 1;
    piece[1][3] <- 1;
    color.contents <- Color.Green;

  | 6 -> 
    //printf "%d" 6;
    piece[1][1] <- 1;
    piece[1][2] <- 1;
    piece[2][2] <- 1;
    piece[2][3] <- 1;
    color.contents <- Color.Yellow;

  | _ -> failwith "System.Random"
    );
  first_piece.contents <- Array.copy second_piece.contents;
  first_color.contents <- second_color.contents;
  second_piece.contents <- Array.copy third_piece.contents;
  second_color.contents <- third_color.contents;
  third_piece.contents <- Array.copy piece;
  third_color.contents <- color.contents

let get_piece_top () =
  let rec get_top_in_y n (array:int array) =
    if n < piece_height then
      if array[n] = 1 then
        n
      else
        get_top_in_y (n + 1) array
    else
      piece_height - 1
  in
  let rec move_in_x n min = 
    if n < piece_width then
      let y = get_top_in_y 0 first_piece.contents[n] in
      if y < min then
        move_in_x (n + 1) y 
      else
        move_in_x (n + 1) min
    else
      min
  in
  move_in_x 0 (piece_height - 1)

  
let get_piece_bottom () =
  let rec get_bottom_in_y n (array: int array) =
     if 0 < n then
       if array[n] = 1 then
         n
       else
         get_bottom_in_y (n - 1) array
     else
       0
   in
  let rec move_in_x n max = 
     if n < piece_width  then(
       let y = get_bottom_in_y (piece_height - 1) first_piece.contents[n] in
       //printf "%d" n;
       if max < y then
         move_in_x (n + 1) y 
       else
         move_in_x (n + 1) max)
     else
       max
   in
  move_in_x 0 0

let get_piece_left () =
  let rec exists_in_y n (array: int array) =
    if n < piece_height then
      if array[n] = 1 then
        true
      else
        exists_in_y (n + 1) array
    else
      false
  in
  let rec move_in_x n = 
    if n < piece_width then
      if exists_in_y 0 first_piece.contents[n] then
        n
      else
        move_in_x (n + 1)
    else
      piece_width - 1
    in
      move_in_x 0

let get_piece_right () =
  let rec exists_in_y n (array: int array) =
    if n < piece_height then
      if array[n] = 1 then
        true
      else
        exists_in_y (n + 1) array
    else
      false
  in
  let rec move_in_x n = 
    if 0 < n then
      if exists_in_y 0 first_piece.contents[n] then
        n
      else
        move_in_x (n - 1)
    else
      0
    in
      move_in_x (piece_width - 1)



let move_piece move =
  match move with
  | Left ->
    let left = get_piece_left () in
    if get_x loc + left <= 0 then
      false
    else
      let rec exists_in_y x y (array: int array) =
        if y < piece_height then
          if array[y] = 1 && get_x loc + x - 1 >= 0 
            && get_y loc + y >= 0 && field[get_x loc + x - 1][get_y loc + y] = 1 then
            false
          else
            exists_in_y x (y + 1) array
        else
          true
      in
      let rec move_in_x n = 
        if n < piece_width then
          if exists_in_y n 0 first_piece.contents[n] then
            move_in_x (n + 1)
          else
            false
        else
          true
      in
      if move_in_x 0 then(
        loc := (get_x loc - 1, get_y loc);
        true
      )
      else
        false
  | Right ->
    let right = get_piece_right () in
    if get_x loc + right >= field_width - 1 then
      false
    else
      let rec exists_in_y x y (array: int array) =
        if y < piece_height then
          if array[y] = 1 && get_x loc + x + 1 <= field_width
            && get_y loc + y >= 0 && field[get_x loc + x + 1][get_y loc + y] = 1 then
            false
          else
            exists_in_y x (y+ 1) array
        else
          true
      in
      let rec move_in_x n = 
        if n < piece_width then
          if exists_in_y n 0 first_piece.contents[n] then
            move_in_x (n + 1)
          else
            false
        else
          true
      in
      if move_in_x 0 then(
        loc.contents <- (get_x loc + 1, get_y loc);
        true
      )
      else
        false
  | Down ->
    let bottom = get_piece_bottom () in
    if get_y loc + bottom >= field_height - 1 then
      false
    else
      let rec exists_in_y x y (array: int array) =
        if y < piece_height then
          if array[y] = 1 && get_y loc + y + 1 >= 0
            && get_y loc + y + 1 < field_height && field[get_x loc + x][get_y loc + y + 1] = 1 then
            false
          else
            exists_in_y x (y + 1) array
        else
          true
      in
      let rec move_in_x n = 
        if n < piece_width then
          if exists_in_y n 0 first_piece.contents[n] then
            move_in_x (n + 1)
          else
            false
        else
          true
      in
      if move_in_x 0 then(
        loc.contents <- (get_x loc, get_y loc + 1);
        true        
      )
      else
        false

let turn_piece () =
  let piece_turned = [| for i in 1 ..piece_width -> [| for j in 1 .. piece_height -> 0  |] |] in
  let color_turned = [| for i in 1 ..piece_width -> [| for j in 1 .. piece_height -> Color.Black  |] |] in
  let rec move_in_y x y =
    if y < piece_height then(
      piece_turned[(piece_height - 1) - y][x] <- first_piece.contents[x][y];
      color_turned[(piece_height - 1) - y][x] <- first_color.contents;
      move_in_y x (y + 1)
    )
  in
  let rec move_in_x x = 
    if x < piece_width then(
      move_in_y x 0;
      move_in_x (x + 1)      
    )
  in move_in_x 0;

  let rec move_in_y x y =
    if y < piece_height then
      if piece_turned[x][y] = 1 then
        let offset = (get_x loc + x, get_y loc + y) in
        if fst offset < 0 || fst offset >= field_width
          || snd offset >= field_height 
          || (snd offset >= 0 && field[fst offset][snd offset] = 1) then
          false
        else
          move_in_y x (y + 1)
      else
        move_in_y x (y + 1)
    else
      true
  in
  let rec move_in_x x = 
    if x < piece_width then
      if move_in_y x 0 then
        move_in_x (x + 1)    
      else false
    else
      true
  in
  if move_in_x 0 then(
    first_piece.contents <- piece_turned;
    (*first_color.contents <- color_turned;*)
    true
  )
  else
    false

  
let piece_to_field () =
  let rec move_in_y x y =
    if y < piece_height then
      //printf "%d " ((get_y loc) + y)
      //printf "%b" (first_piece.contents[x][y] = 1);
      //printf "%b" ((get_y loc) + y >= 0);
     // printf "%b" ((first_piece.contents[x][y] = 1) && ((get_y loc) + y >= 0));
      if first_piece.contents[x][y] = 1 && (get_y loc) + y >= 0 then(
        field[get_x loc + x][get_y loc + y] <- first_piece.contents[x][y];
        field_color[get_x loc + x][get_y loc + y] <- first_color.contents
        //printf "x:%d, y:%d" x y;
       
      );
      move_in_y x (y + 1)
  in
  let rec move_in_x x =
    //printf "%b" (x < piece_width);
    if x < piece_width then(
      //printf "%d" x;
      move_in_y x 0;
      move_in_x (x + 1)      
    )
  in move_in_x 0;

let delete_line () =
  let y = ref (field_height - 1) in
  let rec move_in_y del_count =
    if y.contents >= 0 then
      let rec move_in_x x line_count =
        if x < field_width then
          let ret = move_in_x (x + 1) line_count
          in
          line_count + field[x][y.contents] + ret
        else
        line_count
      in
      let line_count = move_in_x 0 0
      in
      //printfn "line_count %d" line_count;
      if line_count = 0 then
        del_count
      else if line_count <> field_width then
        y.contents <- y.contents - 1;
        move_in_y  del_count
      else
        let x' = ref 0 in
        let rec go x =
          if x < field_width then
            field[x][y.contents] <- 0;
            //field_color[x][y.contents] <- Color.Black;
            x'.contents <- x;
            go (x + 1)
        in
        go 0;
        //printfn "del %d" x'.contents;
        y.contents <- y.contents - 1;
        move_in_y (del_count + 1)
        //printfn "%A" field;
    else
      del_count
  in
  move_in_y 0
  
let shift_line del_count =
  let y = ref (field_height - 1)
  in
  let rec move_in_y ()=
    if y.contents >= 0 && del_count.contents > 0 then
      let rec move_in_x x line_count =
        if x < field_width then
          let ret = move_in_x (x + 1) line_count
          in
          line_count + field[x][y.contents] + ret
        else
        line_count
      in
      let line_count = move_in_x 0 0
      in
      //printfn "line_count %d" line_count;
      if line_count <> 0 then
        y.contents <- y.contents - 1;
        move_in_y ()
      else
        del_count.contents <- del_count.contents - 1;
        let rec go iy =
          let x' = ref 0 in
          if iy >= 0 then
            let rec go' x =
              if x < field_width then
                x'.contents <- x;
                if iy - 1 >= 0 then
                  //printfn "%d" (field[x][iy - 1]);
                  field[x][iy] <- field[x][iy - 1];
                  field_color[x][iy] <- field_color[x][iy - 1]
                else
                  field[x][0] <- 0;        
                  field_color[x][0] <- Color.Black
                go' (x + 1)
            in go' 0;
            //printfn "shift %d" x'.contents;
            go (iy - 1)
        in
        go y.contents;
        move_in_y ()
        //printfn "%A" field;
  in
  move_in_y ()


let square = 20
let box = new PictureBox()
box.Width <- field_width * square;
box.Height <- field_height * square;
//let canvas = new Bitmap(80,80)//(piece_width*square, piece_height*square)
//let g = Graphics.FromImage(canvas)
let form = new Form()


let draw ((*box:PictureBox*)) =
  let canvas = new Bitmap(field_width * square, field_height * square)
  let g = Graphics.FromImage(canvas)
  let rec move_in_y x y =
    if y < field_height then
      let brush = new SolidBrush(field_color[x][y]) in
      //MessageBox.Show (x.ToString() ^ y.ToString());
      //printf "x:%d, y:%d" x y;
     (* if (get_x loc <= x && x < get_x loc + 4) &&
        (get_y loc <= y && y < get_y loc + 4) then
        let brush = new SolidBrush(Color.Blue);
        g.FillRectangle(brush,x * square, y * square, square, square);
      else
      (*printfn "%d;%d;%A" x y loc.contents;*)
      *)
      g.FillRectangle(brush,x * square, y * square, square, square);
      brush.Dispose ();
      move_in_y x (y + 1)
  in
  let rec move_in_x x =
    if x < field_width then(
      move_in_y x 0;
      move_in_x (x + 1)
    )
  in
  //printfn "%A" field_color;
  move_in_x 0;
  let rec move_in_y x y =
    if y < piece_height then
      if first_piece.contents[x][y] = 1 then
        let brush = new SolidBrush(first_color.contents) in
      //MessageBox.Show ((get_x loc).ToString() ^ (get_y loc).ToString());
      //printf "x:%d, y:%d" x y;
        g.FillRectangle(brush,(get_x loc + x) * square, (get_y loc + y) * square, square, square);
        brush.Dispose ();
      move_in_y x (y + 1)
  in
  let rec move_in_x x =
    if x < piece_width then(
      move_in_y x 0;
      move_in_x (x + 1)
    )
  in
  move_in_x 0;
  box.Image <- canvas;
  g.Dispose()

(*
let draw_piece ((*box:PictureBox*)) =
  let canvas = new Bitmap(field_width * square, field_height * square)
  let g = Graphics.FromImage(canvas)
  let rec move_in_y x y =
    if y < piece_height then
      let brush = new SolidBrush(first_color.contents) in
      //MessageBox.Show ((get_x loc).ToString() ^ (get_y loc).ToString());
      //printf "x:%d, y:%d" x y;
      g.FillRectangle(brush,(get_x loc + x) * square, (get_y loc + y) * square, square, square);
      brush.Dispose ();
      move_in_y x (y + 1)
  in
  let rec move_in_x x =
    if x < piece_width then(
      move_in_y x 0;
      move_in_x (x + 1)
    )
  in
  move_in_x 0;
  box.Image <- canvas;
  g.Dispose()
*)
[<STAThread; EntryPoint>]
let main argv =
 create_piece();
 create_piece();
 create_piece();
 create_piece();

 //piece_to_field ();
 //printf "aaa";
 //field_color[5][6]<-Color.White;
 draw ();
 //draw_piece ();

 form.Controls.Add(box);
 form.Width <- (field_width + 1) * square;
 form.Height <- (field_height + 2) * square;
 form.Text <- "Tetris written in F#";
 
 let timer = new Timer(1500);
 let piece_down () =
   if move_piece Down = false then
     piece_to_field();
     //shift_line (delete_line ());
     let score = delete_line ()
     in
     (*
     if score > 0 then
       Thread.Sleep(1500)*)
     draw ();
     printfn "%d" score;
     timer.Stop();
     Thread.Sleep(1500);
     timer.Start();
     shift_line (ref score);
     loc.contents <- (0,0);
     create_piece()
 
 timer.Elapsed.Add(fun e-> piece_down ();draw(););
 timer.Enabled <- true;
 form.KeyDown.Add(fun e-> if e.KeyCode = Keys.Up then ignore(turn_piece ());draw(););
 form.KeyDown.Add(fun e-> if e.KeyCode = Keys.Down then timer.Stop(); piece_down ();draw();timer.Start());
 form.KeyDown.Add(fun e-> if e.KeyCode = Keys.Left then ignore(move_piece Left);draw(););
 form.KeyDown.Add(fun e-> if e.KeyCode = Keys.Right then ignore(move_piece Right);draw(););
 Application.Run (form);
 0