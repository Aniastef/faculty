open Printf;;

(*maximul dintre 3 chestii*)
let functie p m n= max n (max p m);;
let b=functie 12 19 133;;

(*delta*)

let func a b c=
  let delta = a*a-4*b*c 
   in
      if delta>0 then printf "x1=x2" 
       else printf "doamne \n";;


let r=func 2 3 5;;

(*suma cifre numar*)

let rec sum a=
  if(a=0) then 0
  else sum(a/10)+(a mod 10);;

  let n=sum 123020;;

(*pare*)

let rec pare n=
  if (n=0) then 0
  else if n mod 2=0 then pare(n/10)+1
  else pare(n/10);;

  let b=pare 2313237834776;;
  let b=pare 0;;


(*baza 2*)
let rec baza2 nr=
  if(nr=0) then 0
  else baza2(nr/ 2)*10 + nr mod 2;;

  let nr= baza2 14;;

(*orice baza*)

let rec ob nr baza=
  if (nr=0) then 0
  else 10*ob( nr/baza) baza + nr mod baza;;

let f=ob 321 6;;


(*caut orice cifra in descompunerea in orice baza*)
let rec ob nr baza cifra=
  if (nr=0) then 0
  else if (nr mod baza = cifra) then ob (nr/baza) baza cifra +1
  else ob (nr/baza) baza cifra;;

let f=ob 321231 6 0;;


(*factorial*)

let rec facto n=
  if (n=1) then 1
  else n * facto (n-1);;

let rez=facto 4;;


(*aplicare repetata functie*)

let rec ham n f=
if (n=0) then 0
else f(ham (n-1)(f));;

let a=ham 4 (fun x -> x + 2);;


(*lista cifrelor unui numar in ordinea in care apar in numar*)

let diglst n =
  let rec diglst2 res n = 
    if n = 0 then res 
    else diglst2 ((n mod 10)::res) (n/10)
  in diglst2 [n mod 10] (n / 10);;

  let s=diglst 13321;;

  let b=1::[];;
  let a=2::b;;


(*lista cifrelor unui numar invers*)

let rec cif n=
  if(n<10) then [n]
  else (n mod 10)::cif(n/10);;

let m=cif 21231;;

(*daca exista 1 el in lista*)

let func lst = match lst with
|[x]-> true
|h::t ->false
|[]->false;;

let a=func [];;


(*Scrieți o funcție care returnează numărul elementelor pentru o listă cu mai puțin de 3
elemente și -1 dacă lista are mai multe elemente (fără recursivitate*)

let func a= match a with
|[] -> 0
|[x]->1
|[y;z]-> 2
|h::t-> (-1);;

let a=func [1;2];;

(* lista cifrelor care satisfac o conditie anume?? *)

(* let listacifre numar functie=
  let rec lst numar functie lista= match numar with 
  |0->lista
  |n when n<10 -> numar::lista
  |nr when nr>9-> if (functie(numar mod 10))>0 then lst numar/10 functie ((numar mod 10)::lista) else lst (numar/10) functie lista
in lst numar functie [];; *)



let listacifre numar functie=
  let rec lst numar functie lista=
    if (numar=0) then lista
    else if (functie(numar mod 10)) then lst (numar/10) functie ((numar mod 10)::lista) 
    else lst (numar/10) functie lista
    in lst numar functie [];;

  let b=listacifre 23231 (fun x->x mod 2 =1);;



  (*elementul n dintr o lista*)

  let rec lungimelista lista= match lista with
  |[]->0
  |h::t-> lungimelista t +1;;

  let b=lungimelista [2;231;24;4;2;3223];;
  let a=lungimelista [];;

  (* let elm n lista=
    let cazuri= if (n>lungimelista(lista)) then failwith "n mai mare decat lungimea listei"
                else if (n<=0) then failwith "n nu poate fi mai mic sau egal cu 0" in
    let rec el n i lista= match lista with
      |[]->[]
      |h::t-> if (i==n) then el n (i+1) t else h:: (el n (i+1) t)
  in el n 1 lista;;

  let b=lungimelista [23;3;4;5;32;45;52];;
  let a=elm (-1) [23;3;4;5;32;45;52];; *)


(*elimin al n-lea element din lista*)

  let eliminel n lista=
    let rec elmn n i lista= match lista with
    |[]->[]
    |h::t-> if (i mod n)=0 then elmn n (i+1) t else h:: (elmn n (i+1) t)
    in elmn n 1 lista;;
    
    let b=eliminel 1 [1;2;3;4;5;6;7;8;9;10;11;12];;
    

(*numar format din cifrele din lista care respecta o conditie*)

let numar functie lista=List.fold_left(fun numar el-> if functie(el) then numar*10+el else numar) 0 lista;;

let a= numar (fun x -> (x mod 2)=1) [1;2;5;2;3;5;6];; (*asta da 1535*)


let rec num f l= match l with
|[]->0
|h::t-> if f(h) then 10*(num f t)+h else num f t;;

let a= num (fun x -> (x mod 2)=1) [1;2;5;2;3;5;6];; (*asta da 5351*)


let invers f l=
  let rec nm f l numar=match l with
  |[]-> numar
  | h::t-> if f(h) then nm f t (numar*10+h) else nm f t numar
  in nm f l 0;;

  let a= invers (fun x -> (x mod 2)=1) [1;2;5;2;3;5;6];; (*asta da 1535*)


  (*ceva fromto lista numere dintr un interval [a,b] divizibile cu d*)

let fromto a b d=
let rec fromt a b d lista=
  if (b<a) then lista 
  else if (b mod d = 0) then fromt a (b-1) d (b::lista) 
  else fromt a (b-1) d lista
  in fromt a b d [];;

  let a= fromto 2 12 4;; 

  (*primele n elemente dintr o lista data*)

  let primelen n lista=
    let rec prime n i lista= match lista with
    |[]->lista
    |h::t-> if (i<=n) then h::(prime n (i+1) t) else prime n (i+1) t
  in prime n 1 lista;;

  let a=primelen 5 [2;3;4;5;6;7;5;4;3;7;6;5;4;46;7];;

  (*list fold left care returneaza nr de elemente pt care o functie e adevarata*)

  let nradevarat lista f = List.fold_left (fun s el-> if f(el) then s+1 else s ) 0 lista;;

  let test= nradevarat [1;3;4;5;6;4;3;4;7;8] (fun x-> x=4);;

  (*elimina duplicatele consecutive - din 2 elemente lasa 1*)

  let rec duplicate lista=match lista with
    |[]->[]
    |e1::e2::t when e1=e2-> e2::duplicate t
    |h::t-> h::duplicate t;;

    let a= duplicate [1;2;2;2;4;5;64;23;23;34;12];;

    (*funcție care compară două liste după următoarea relație de ordine:
     o listă mai scurtă e "mai mică" decât una mai lungă;
     dacă lungimile sunt egale,
     ordonarea e determinată de prima pereche de elemente diferite.*)

     (*let functie lista1 lista=
      let rec lungimelista lista= match lista with
      |[]->0
      |h::t-> lungimelista t +1 in
      if (lungimelista(lista1) < lungimelista(lista2)) then "lista1 e mai mica decat lista2"
      else if  (lungimelista(lista2) < lungimelista(lista1)) then "lista2 e mai mica decat lista1"
      else if l*)


      (*list partition*)


      (*List.partition-separa o lista in doua liste in functie de f, 
      elementele pentru care f e falsa sunt in prima lista, 
      cele pentru care f e adevarata sunt in a doua lista*)

 let rec partition f lst =match lst with
|[]->([],[]) (*daca lista e goala se returneaza aia*)
|h::t->
        let (l1,l2)=partition f t in 
        if f h then (l1,h::l2) else (h::l1,l2);;

partition (fun x->x mod 3==0) [1;2;3;4;5;6;7;8;9;10];;

(1::[],5::[5;4;3]);; (* devine  ([1], [5; 5; 4; 3]) *)


(*alea de la tema 4*)
let jud_pop = [("Alba", 342376); ("Arad", 430629); ("Arges", 612431); ("Bacau", 616168); ("Bihor", 575398); ("Bistrita-Nasaud", 286225); ("Botosani", 412626); ("Brasov", 549217); ("Braila", 321212); ("Buzau", 451069); ("Caras-Severin", 295579); ("Calarasi", 306691); ("Cluj", 691106); ("Constanta", 684082); ("Covasna", 210177); ("Dambovita", 518745); ("Dolj", 660544); ("Galati", 536167); ("Giurgiu", 281422); ("Gorj", 341594); ("Harghita", 310867); ("Hunedoara", 418565); ("Ialomita", 274148); ("Iasi", 772348); ("Ilfov", 388738); ("Maramures", 478659); ("Mehedinti", 265390); ("Mures", 550846); ("Neamt", 470766); ("Olt", 436400); ("Prahova", 762886); ("Satu Mare", 344360); ("Salaj", 224384); ("Sibiu", 397322); ("Suceava", 634810); ("Teleorman", 380123); ("Timis", 683540); ("Tulcea", 213083); ("Vaslui", 395499); ("Valcea", 371714); ("Vrancea", 340310)];;

let numar_judete lista = List.fold_left(fun acc (el1,el2)-> acc+1) 0 lista;;

let a=numar_judete jud_pop;;

let modifica_populatie nume x lista= List.map (fun (numej,numarp)-> if (numej=nume) then (numej,numarp+x) else (numej,numarp)) lista;;

let a= modifica_populatie "Bacau" 2 jud_pop;;

let f lista=
let numar_judete lista = List.fold_left(fun acc (el1,el2)-> acc+1) 0 lista in 
let media_populatie lista= List.fold_left(fun media (nume,pop)->media+pop) 0 lista in
let a=(float(media_populatie lista))/.(float(numar_judete lista)) in a;;

let medie= f jud_pop;;

(*let statistica lista x=
  let numex lista x=List.fold_left(fun acc (nume,pop)-> if (nume=x) then pop+acc else acc ) 0 lista in
  let populatie lista= List.filter(fun (nume,pop)-> pop>(numex lista x)) lista in
  let lst lista= List.fold_left(fun acc (nume,pop)-> nume::acc) [] populatie(lista) in lst lista;;

let m=statistica jud_pop "Arad";; (*nush dc nu merge*)*)


(*multimi*)


module Int=struct
  type t=int
  let compare=compare
end
module IS=Set.Make(Int)

let s1= IS.add 5 (IS.add 2 ( IS.add 4 (IS.add 12 IS.empty)));;
let s2= IS.add 3 (IS.add 2 ( IS.add 12 (IS.add 5 IS.empty)));;


(*ce e in prima multime si nu e in a 2-a / diferenta*)

let diff s1 s2=IS.fold(fun  e s -> IS.remove e s) s2 s1;;

let a=IS.elements(diff s1 s2);;


module String=struct
  type t=string
  let compare=compare
end
module SS=Set.Make(String)

let sir= SS.add "a" ( SS.add "b" (SS.add "c" SS.empty));;

(*multime din primele elemente din pereche al unei liste de perechi*)

let lista_perechi=[(1,2);(3,4);(12,6)];;
let tiparire lista=List.fold_left(fun acc (e1,e2) -> IS.add e1 acc ) IS.empty lista;;

let a=IS.elements(tiparire lista_perechi);;


module Int=struct
  type t=int
  let compare=compare
end
module IS=Set.Make(Int)

let s1= IS.add 5 (IS.add 2 ( IS.add 4 (IS.add 12 IS.empty)));;

(*fac o multime din elementele din multimea data care indeplinesc conditia f*)

let satisfac f multime = IS.fold (fun el acc -> if f(el) then IS.add el acc else acc)  multime IS.empty;;
let a=IS.elements(satisfac (fun x->x mod 2 =1) s1);;

(*funcția standard partition care ia ca parametri o funcție booleană f și o mulțime s și returnează o pereche de mulțimi, cu elementele din s care satisfac, respectiv nu satisfac funcția f.*)

let partition f set=IS.fold(fun elt (acc1,acc2)->if f elt then (IS.add elt acc1,acc2) else (acc1,IS.add elt acc2))set (IS.empty,IS.empty);; 

(*functie care returneaza multimea cifrelor unui numar -fara nr 0*)

let mn nr=
  let rec mcn nr multime=
    if(nr=0) then multime
    else mcn(nr/10)(IS.add (nr mod 10) multime)
    in mcn nr IS.empty;;

    let b= IS.elements(mn 120345);;

   (*aceeasi functie dar daca numarul e 0 atunci returneaza multimea cu 0, la celalt nu facea asta*)

    let mn nr=
      let rec mcn nr multime contor=
        if(nr=0 && contor=1) then IS.add 0 multime
        else if (nr=0 && contor!=1) then multime
        else mcn(nr/10)(IS.add (nr mod 10) multime) (contor-1)
        in mcn nr IS.empty 1;;
    
        let b= IS.elements(mn 12345);;
        let a=IS.elements(mn 23103);;



        module IntPair = struct
          type t = int * int
          let compare = compare
        end
        module PS = Set.Make(IntPair)

(*nu imi da dar asa se foloseste multime de perechi-numar natural descompus in factori primi -> reprezentat ca o multime de perechi (numar,putere)*)

let nrnat nr=
  let rec ab nr contor putere multime=
    if(nr=0) then multime else 
    if(nr mod contor =0) then ab (nr/contor)(contor)(putere+1) multime
    else ab nr (contor+1) 0 (PS.add (contor,putere) multime)
    in ab nr 2 0 PS.empty;;

    let a=PS.elements(nrnat 40);;



(*Scrieți o funcție care ia o listă de caractere și creează un dicționar în care fiecare
caracter e asociat cu numărul aparițiilor din listă. Pentru găsirea numărului de apariții
în listă, puteți scrie o funcție care primește un caracter și o listă și returnează
numărul de apariții al caracterului în listă. Puteți folosi List.fold_left pentru scrierea
acestei funcții.*)


let lst= ["a";"b";"a";"d";"a"]

module MI = Map.Make(String);;

let functie lista=
  let numaraparitii c lista = List.fold_left ( fun nr el -> if (c==el) then nr+1 else nr) 0 lista in
  let dictionar lista = List.fold_left (fun acc el -> MI.add el (numaraparitii el lista) acc) MI.empty lista
  in dictionar lista;;
  
  let a=functie lst;;

  
  (*funcție care ia o listă de asociere cu perechi de tip (șir, întreg) și creează un dicționar în care fiecare șir e asociat cu suma tuturor valorilor cu care e asociat în listă.*)
  let listaa=[("a",2);("b",3);("a",4);("c",5);("d",10);("c",1)];;

let functiee lista=
  let valori c lista = List.fold_left ( fun nr (el1,el2) -> if (c=el1) then nr+el2 else nr) 0 lista in
  let dictionar lista = List.fold_left (fun acc (el1,el2) -> MI.add el1 (valori el1 lista) acc) MI.empty lista
  in dictionar lista;;

  let a=MI.bindings(functiee listaa);;




    




