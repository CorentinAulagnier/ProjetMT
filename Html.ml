(* Michaël PÉRIN, Verimag / Université Grenoble-Alpes, Février 2017 
 *
 * A library for generating html output	
 *
 * - Required modules: Tricks MyList Graphics -> Color
 *
 * - Compilation:  ocamlc Tricks.cmo MyList.cmo Graphics.cma Color.cmo html.ml
 *)

open Tricks

(* TYPES *)
    
type content = string
type cell = content
type cells = content list
type row = content
type rows = row list      
type table = content
type tables = table list      (* ajout de types *)
type body = content           (* ajout de types *)

type valeur =
  | Int of int
  | Option of string
  | Color of Color.t
	      
type options = (string * valeur) list   

let (concat: content list -> content) = String.concat "\n"
    
let (valeur_to_string: valeur -> string) = fun valeur ->
      match valeur with
      | Int i -> string_of_int i
      | Option s -> s
      | Color c -> Color.color_to_html c


(* OPTIONS:  algin=center  color="red" *)
		
let (process_options: options -> string) = fun options ->
  if options = []
  then ""
  else
    let string =
      options
      >> (List.map (fun (name,valeur) -> name ^ "=" ^ (valeur_to_string valeur)))
      >> (String.concat " ")
    in " " ^ string


(* ENVIRONMENT: <TAG options> content </TAG> *)	       

let (environment: string * string * string -> options -> content -> content) = fun (before_mark,mark,after_mark) options content ->
	String.concat "" [ before_mark ; "<" ^ mark ^ (process_options options) ^ ">" ; after_mark ;
			   content ; after_mark ;
			   before_mark ; "</" ^ mark ^ ">" ; after_mark ]


(* TABLE *)
(* body: <BODY option> tables </BODY>  ou liste de tables*)
let (body: options -> tables -> body) = fun options tables ->
      environment ("Debut de pagE","DIV","Puis fin de pagE") options (concat tables)

	
(* table cell: <TD option> content </TD> *)
	
let (cell: options -> content -> cell) = environment ("  ","TD","")

let (cell_name: options -> content -> cell) = environment ("  ","TD class=\"machine\"","")
    
let (wide_cell: int -> int -> cell) = fun width n ->
      if n>0
      then cell [ ("COLSPAN", Int n) ; ("bgcolor", Option "white") ] ""
      else ""

let (old_wide_cell: int -> int -> cell) = fun width n ->
      if n>0
      then (cell [("width", Int width) ; ("bgcolor", Option "orange") ] "") >> (MyList.make n) >> (String.concat "")
      else ""

(* table: <TABLE option> rows </TABLE> *)
	  
let (table: options -> rows -> table) = fun options rows ->
      environment ("","TABLE","\n") options (concat rows)

(* table row: <TR option> cells </TR> *)
	
let (row: options -> cell list -> row) = fun options cells ->
      environment (" ","TR","\n") options (concat cells)

(* column = a table of one column *)

let (column: options -> cell list -> table) = fun options cells ->
      let
	  (one_cell_row:cell -> row) = fun cell -> row [] [cell] 
      in
	table options (List.map one_cell_row cells)

(* tuple = a table of one row inside a cell *)
	  
let (tuple: options -> cell list -> cell) = fun options cells ->
      cell options
	(table [ ("border", Int 1) ] 
	   [row [] cells]
	)
	
	
(* FONT *)	  
	  
let (font: options -> content -> content)  = environment ("","FONT","")

let (italic: content -> content) = environment ("","I","") []

let (bold: content -> content) = environment ("","B","") [] 

(* où Debut de pagE vaudra à terme*)
(*"
<!DOCTYPE html>
<html>
  <head>
    <meta charset=/"utf-8/" />
    <title>Projet 2017 - </title>
    <link rel=/"stylesheet/" href="reset.css" type=/"text/css/">
    <link rel=/"stylesheet" href=/"mt.css/" type=/"text/css/" title="principal/">
  </head>
  <body>
    <div id=/"wrapper//">
      <header><h1>Demo html</h1></header>
      <div id=/"demo_contenu/">
"*)

(* où Puis fin de pagE*)
(*"
     </div>
      <footer>
          <p>Projet Machine de Turing 2017 - Aulagnier Carriere Riou Thisse</p>
      </footer> 
    </div>
  </body>
</html> 
<!-- Soumettre au validator http://html5.validator.nu -->
<!-- Sinon validator : http://validator.w3.org/ -->
"*)