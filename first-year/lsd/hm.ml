(*Sa se scrie o functie care ia ca parametru un dictionar cu chei si valori nr. intregi, si o 
functie f:ZxZ->Z si returneaza un dictionar in care fiecarei chei k din dictionarul initial ii 
este asociat rezultatul returnat de f(k, v), unde v este valoarea asociata chei k in dictionarul
initial*)

module Int = struct
  type t= int
  let compare=compare
end
module D=Map.Make(Int)

let dictionar = D.empty
  |> D.add 1 2
  |> D.add 5 4
  |> D.add 6 6
  |> D.add 3 7
  |> D.add 8 9
  ;;

let f x y = if x < y then 0 else 1;;

let functie d f= D.fold ( fun k v dict -> D.add k (f k v) dict ) d D.empty 

let b=D.bindings (functie dictionar f);;



(*Sa se scrie o functie care ia ca parametru o lista de perechi de siruri de caractere si returneaza
o pereche de multimi a.i. multimea de pe prima pozitie a perechii contine toate sirurile aflate pe 
prima pozitie a perechilor din lista, iar multimea de pe a doua pozitie a perechii contine sirurile de
pe a doua pozitie a perechilor din lista *)

module M = Set.Make(String);;



let lista = [("a", "c"); ("d", "l"); ("e", "m"); ("s", "o"); ("p", "c")];;

let functie l = List.fold_left ( fun (ms, md) (ls, ld) -> (M.add ls ms, M.add ld md) ) (M.empty, M.empty) l;;

