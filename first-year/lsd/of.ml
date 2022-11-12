let lst= ["a";"b";"a";"d";"a"]

module Int=struct
  type t=int
  let compare=compare
end
module MI = Map.Make(String);;

let functie lista=
  let numaraparitii c lista = List.fold_left ( fun nr el -> if (c=el) then nr+1 else nr) 0 lista in
  let dictionar lista = List.fold_left (fun acc el -> MI.add el (numaraparitii el lista) acc) MI.empty lista
  in dictionar lista;;
  
  let a=MI.bindings(functie lst);;


let listaa=[("a",2);("b",3);("a",4);("c",5);("d",10);("c",1)];;

let functiee lista=
  let valori c lista = List.fold_left ( fun nr (el1,el2) -> if (c=el1) then nr+el2 else nr) 0 lista in
  let dictionar lista = List.fold_left (fun acc (el1,el2) -> MI.add el1 (valori el1 lista) acc) MI.empty lista
  in dictionar lista;;

  let a=MI.bindings(functiee listaa);;

  type bin_tree = Nil | Nod of int * bin_tree * bin_tree

    
  let arbore=
    Nod(1,
    Nod(7,
    Nod(3,Nil,Nil),
    Nod(6,Nil,Nil)),
    Nod(2,
    Nod(5,Nil,Nil),
    Nod(4,Nil,Nil)));;
  
  
      let arbore_ordonat=
        Nod(5,
        Nod(4,
        Nod(2,Nil,Nil),
        Nod(3,Nil,Nil)),
        Nod(8,
        Nod(7,Nil,Nil),
        Nod(10,Nil,Nil)));;
  
  
  
  
  
  
  
  (*a - functie care primeste un arbore binar si returneaza o multime cu frunzele lui*)
  
  let fr arbore =
  let rec frunze arbore set = match arbore with 
  | Nil -> set
  | Nod(r, Nil, Nil) -> IS.add r set
  | Nod(r, s, d) -> IS.union (frunze s set) (frunze d set)
  in frunze arbore IS.empty;;  
  
  let rez=IS.elements(fr arbore);;
  
  
  
  (*b- numarul de noduri al arborelui ORDONAT mai mici decat un n dat*)
  
  
  let rec maimic n arbore= match arbore with
  | Nil-> 0
  | Nod(r,s,d) -> if(r<n) then maimic n s + maimic n d + 1 else maimic n s + maimic n d;;
  
  let rez=maimic 9 arbore_ordonat;;
  let rez2= maimic 5 arbore;; 
  
  
  
  
  
  (*c-dictionar de intregi unde cheia e valoarea nodului iar valoarea e nivelul nodului*)
  
  let rec nivel nod arbore= match arbore with
  |Nil->0
  |Nod(r,s,d) when r=nod-> 1
  |Nod(r,s,d) -> if nod<r then 1+nivel nod s else 1+ nivel nod d;;
  
  let a=nivel 10 arbore_ordonat;;
  
  let dictionar_arbore arbore=
    let rec adaugare arbore1 dictionar = match arbore1 with
    |Nil-> dictionar
    |Nod(r,s,d)-> ( DI.add r (nivel r arbore) (adaugare d (adaugare s dictionar) ) )
    in adaugare arbore DI.empty;;
  
    let dict=DI.bindings(dictionar_arbore arbore_ordonat);;

  