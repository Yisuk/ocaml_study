let filePath = "./input/aoc2020day3.txt"

type slope = { x : int; y : int }
type grid = OPEN_SQUARE | TREE

let parse input =
  match input with
  | "#" -> Option.Some TREE
  | "." -> Option.Some OPEN_SQUARE
  | _ -> Option.None

let getTrajectory input slope =
  input
  |> List.filteri (fun index _inputRow -> index mod slope.y = 0)
  |> List.mapi (fun index inputRow ->
         String.get inputRow (index * slope.x mod String.length inputRow))
  |> List.map (String.make 1)
  |> List.map parse

let getNumberOfEncounters trajectory =
  trajectory
  |> List.fold_left
       (fun acc value ->
         match value with
         | Option.Some TREE -> acc + 1
         | Option.Some OPEN_SQUARE | Option.None -> acc)
       0

let solvePart1 () =
  let slope = { x = 3; y = 1 } in
  let input = Util.readLine filePath in
  let encounter = slope |> getTrajectory input |> getNumberOfEncounters in
  print_int encounter;
  print_newline ()

let solvePart2 () =
  let slopes =
    [
      { x = 1; y = 1 };
      { x = 3; y = 1 };
      { x = 5; y = 1 };
      { x = 7; y = 1 };
      { x = 1; y = 2 };
    ]
  in
  let input = Util.readLine filePath in
  let encounters =
    slopes
    |> List.map (fun slope -> getTrajectory input slope)
    |> List.map getNumberOfEncounters
    |> List.fold_left (fun acc encounter -> encounter * acc) 1
  in

  print_int encounters;
  print_newline ()
