(* Michaël PÉRIN, Verimag / Université Grenoble-Alpes, Février 2017 
 *
 * Part of the project TURING MACHINES FOR REAL
 *
 * Runs of Turing Machines
 *
 *)


open Symbol
open Alphabet
open Band
open Transition
open Turing_Machine
open Configuration
open Execution

 
let (incr_decr: unit -> Configuration.t) = fun () ->
      let alphabet = Alphabet.make [B;Z;U;D] in
	let band1 = Band.make alphabet [U;U;Z;U] in
	  let tm = Turing_Machine.sequence [ Run Turing_Machine.incr ; Run Turing_Machine.left_most ; Run Turing_Machine.decr ] in
	    let cfg = Configuration.make tm [ band1 ] in
	      Execution.log_run cfg 


let (incr: unit -> Configuration.t) = fun () ->
      let alphabet = Alphabet.make [B;Z;U;D] in
	let band1 = Band.make alphabet [U;U;Z;U] in
	  let cfg = Configuration.make Turing_Machine.incr [ band1 ] in
	    Execution.log_run cfg 
	      

let (decr1: unit -> Configuration.t) = fun () ->
      let alphabet = Alphabet.make [B;U;Z;D] in
	let band1 = Band.make alphabet [Z;Z;Z;U] in
	  let cfg = Configuration.make Turing_Machine.decr [ band1 ] in
	    Execution.log_run cfg 

let (decr2: unit -> Configuration.t) = fun () ->
      let alphabet = Alphabet.make [B;U;Z;D] in
	let band1 = Band.make alphabet [Z;Z;Z;U;U] in
	  let cfg = Configuration.make Turing_Machine.decr [ band1 ] in
	    Execution.log_run cfg 
	      

let (gen_dup: unit -> Configuration.t) = fun () ->
      let alphabet = Alphabet.make [B;Z;U;D] in
	let dup = Turing_Machine.generic_dup alphabet.symbols in
	  let band1 = Band.make alphabet [U;Z;Z;U] in
	    let cfg = Configuration.make dup [ band1 ] in
	      Execution.log_run cfg 

let (gen_copy: unit -> Configuration.t) = fun () ->
      let alphabet = Alphabet.make [B;U;Z] in
	let copy = Turing_Machine.generic_copy alphabet.symbols in
	  let band1 = Band.make alphabet [Z;U;U;Z] 
	  and band2 = Band.make alphabet [] in
	    let cfg = Configuration.make copy [ band1 ; band2 ] in
	      Execution.log_run cfg 

	      
let (gen_reverse: unit -> Configuration.t) = fun () ->
      let alphabet = Alphabet.make [B;U;Z] in
	let reverse = Turing_Machine.generic_reverse alphabet.symbols in	
	  let band1 = Band.make alphabet [U;Z;U;U;Z;Z] 
	  and band2 = Band.make alphabet [] in
	    let cfg = Configuration.make reverse [ band1 ; band2 ] in
	      Execution.log_run cfg 


let (gen_swap: unit -> Configuration.t) = fun () ->
      let alphabet = Alphabet.make [B;U;Z] in
	let swap = Turing_Machine.generic_swap alphabet.symbols in	
	  let band1 = Band.make alphabet [U;Z;U;U;Z;Z] in
	    let cfg = Configuration.make swap [ band1 ] in
	      Execution.log_run cfg 

let (xor: unit -> Configuration.t) = fun () ->
      let alphabet = Alphabet.make [B;Z;U] in
	let band1 = Band.make alphabet [U;Z;U;U;Z;Z] 
	and band2 = Band.make alphabet [U;Z;U;U;Z;Z] in
	  let cfg = Configuration.make Turing_Machine.xor [ band1 ; band2 ] in
	    Execution.log_run cfg
		

let (busy_beaver: Turing_Machine.t -> Configuration.t) = fun bb ->
      let alphabet = Alphabet.binary in
	let band = Band.make alphabet [] in
	  let cfg = Configuration.make bb [ band ] in
	    Execution.log_run cfg 

(*Test Partie2 projet : B-Reduction *)

(* MT à utiliser
 *
 * let question1 = Turing_Machine.sequence [ Run Turing_Machine.go_first_O ; Run Turing_Machine.good_parenthesis ] in
 *
 * let question2 = Turing_Machine.sequence [ Run Turing_Machine.cpy_Bone_Bthree ; Run Turing_Machine.good_remplacement ] in
 *
 * let question3 = Turing_Machine.sequence [ Run Turing_Machine. ; Run Turing_Machine. ] in
 *
 *
 *)	
(*-------------------------------------------------------------------------------------------------------------------------*)	

let (t1Q1: unit -> Configuration.t) = fun () ->
      let alphabet = Alphabet.make [B;O;C;L;X;S] in
	let band1 = Band.make alphabet [O;Z;U;Z;U;Z;U;Z;U;Z;U;Z;U;Z;U;Z;U;Z;C] 
	and band2 = Band.make alphabet [B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B] 
	and band3 = Band.make alphabet [B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B] in
	let question1 = Turing_Machine.sequence [ Run Turing_Machine.go_first_O ; Run Turing_Machine.good_parenthesis ] in
	  let cfg = Configuration.make question1 [ band1 ; band2 ; band3 ] in
	    Execution.log_run cfg	

let (t2Q1: unit -> Configuration.t) = fun () ->
      let alphabet = Alphabet.make [B;O;C;L;X;S] in
	let band1 = Band.make alphabet [O;Z;U;Z;O;Z;U;C;U;O;U;Z;O;Z;C;Z;C;Z;C] 
	and band2 = Band.make alphabet [B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B] 
	and band3 = Band.make alphabet [B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B] in
	let question1 = Turing_Machine.sequence [ Run Turing_Machine.go_first_O ; Run Turing_Machine.good_parenthesis ] in
	  let cfg = Configuration.make question1 [ band1 ; band2 ; band3 ] in
	    Execution.log_run cfg
	  
let (t3Q1: unit -> Configuration.t) = fun () ->
      let alphabet = Alphabet.make [B;O;C;L;X;S] in
	let band1 = Band.make alphabet [U;Z;U;Z;O;Z;U;Z;U;O;U;Z;U;Z;C;Z;C;Z;U] 
	and band2 = Band.make alphabet [B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B] 
	and band3 = Band.make alphabet [B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B] in
	let question1 = Turing_Machine.sequence [ Run Turing_Machine.go_first_O ; Run Turing_Machine.good_parenthesis ] in
	  let cfg = Configuration.make question1 [ band1 ; band2 ; band3 ] in
	    Execution.log_run cfg
	    
let (t4Q1: unit -> Configuration.t) = fun () ->
      let alphabet = Alphabet.make [B;O;C;L;X;S] in
	let band1 = Band.make alphabet [U;Z;U;Z;O;Z;U;Z;U;O;U;Z;U;Z;C;Z;B;B;B] 
	and band2 = Band.make alphabet [B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B] 
	and band3 = Band.make alphabet [B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B] in
	let question1 = Turing_Machine.sequence [ Run Turing_Machine.go_first_O ; Run Turing_Machine.good_parenthesis ] in
	  let cfg = Configuration.make question1 [ band1 ; band2 ; band3 ] in
	    Execution.log_run cfg
	    	
(*-------------------------------------------------------------------------------------------------------------------------*)	
	
let (t1Q2: unit -> Configuration.t) = fun () ->
      let alphabet = Alphabet.make [B;O;C;L;X;S] in
	let band1 = Band.make alphabet [O;Z;U;X;Z;U;Z;U;Z;U;Z;X;U;Z;U;Z;U;Z;C] 
	and band2 = Band.make alphabet [O;Z;U;Z;C;B;B;B;B;B;B;B;B;B;B;B;B;B;B] 
	and band3 = Band.make alphabet [B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B] in
	let question2 = Turing_Machine.sequence [ Run Turing_Machine.cpy_Bone_Bthree ; Run Turing_Machine.good_remplacement ] in
	  let cfg = Configuration.make question2 [ band1 ; band2 ; band3 ] in
	    Execution.log_run cfg	

let (t2Q2: unit -> Configuration.t) = fun () ->
      let alphabet = Alphabet.make [B;O;C;L;X;S] in
	let band1 = Band.make alphabet [X;O;U;Z;U;Z;U;Z;U;Z;U;Z;U;Z;C;U;Z;U;X] 
	and band2 = Band.make alphabet [B;B;B;B;O;Z;U;C;B;B;B;B;B;B;B;B;B;B;B] 
	and band3 = Band.make alphabet [B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B] in
	let question2 = Turing_Machine.sequence [ Run Turing_Machine.cpy_Bone_Bthree ; Run Turing_Machine.good_remplacement ] in
	  let cfg = Configuration.make question2 [ band1 ; band2 ; band3 ] in
	    Execution.log_run cfg
	  
let (t3Q2: unit -> Configuration.t) = fun () ->
      let alphabet = Alphabet.make [B;O;C;L;X;S] in
	let band1 = Band.make alphabet [O;Z;U;Z;U;Z;X;X;U;Z;U;Z;U;Z;U;Z;U;Z;C] 
	and band2 = Band.make alphabet [B;B;B;B;O;Z;U;C;B;B;B;B;B;B;B;B;B;B;B] 
	and band3 = Band.make alphabet [B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B] in
	let question2 = Turing_Machine.sequence [ Run Turing_Machine.cpy_Bone_Bthree ; Run Turing_Machine.good_remplacement ] in
	  let cfg = Configuration.make question2 [ band1 ; band2 ; band3 ] in
	    Execution.log_run cfg
	    	
(*-------------------------------------------------------------------------------------------------------------------------*)	

let (t1Q3: unit -> Configuration.t) = fun () ->
      let alphabet = Alphabet.make [B;O;C;L;X;S] in
	let band1 = Band.make alphabet [O;L;X;S;U;U;X;U;U;X;U;U;X;C;O;Z;Z;Z;C] 
	and band2 = Band.make alphabet [B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B] 
	and band3 = Band.make alphabet [B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B] 
	and band4 = Band.make alphabet [B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B] in
	let question3 = Turing_Machine.sequence [ Run Turing_Machine.go_first_O2 ; Run Turing_Machine.good_parenthesis2 ; Run Turing_Machine.go_first_O3 ; Run Turing_Machine.good_parenthesis3 ; Run Turing_Machine.go_left ; Run Turing_Machine.good_remplacement (*; Run Turing_Machine.sub*) ] in
	  let cfg = Configuration.make question3 [ band1 ; band2 ; band3 ; band4 ] in
	    Execution.log_run cfg	
(*
let (t2Q3: unit -> Configuration.t) = fun () ->
      let alphabet = Alphabet.make [B;O;C;L;X;S] in
	let band1 = Band.make alphabet [X;O;U;Z;U;Z;U;Z;U;Z;U;Z;U;Z;C;U;Z;U;X] 
	and band2 = Band.make alphabet [B;B;B;B;O;Z;U;C;B;B;B;B;B;B;B;B;B;B;B] 
	and band3 = Band.make alphabet [B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B] in

	  let cfg = Configuration.make question3 [ band1 ; band2 ; band3 ] in
	    Execution.log_run cfg
	  
let (t3Q3: unit -> Configuration.t) = fun () ->
      let alphabet = Alphabet.make [B;O;C;L;X;S] in
	let band1 = Band.make alphabet [O;Z;U;Z;U;Z;X;X;U;Z;U;Z;U;Z;U;Z;U;Z;C] 
	and band2 = Band.make alphabet [B;B;B;B;O;Z;U;C;B;B;B;B;B;B;B;B;B;B;B] 
	and band3 = Band.make alphabet [B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B] in

	  let cfg = Configuration.make question3 [ band1 ; band2 ; band3 ] in
	    Execution.log_run cfg
*)	    	
(*-------------------------------------------------------------------------------------------------------------------------*)	
	
	

(* DEMO *)
	        
let (demo: unit -> unit) = fun () ->
      begin
	print_string "\n\n* DEMO * Demo.ml:\n\n" ;
	List.iter (fun _ -> ())
	  [ 
	  (* incr () ;
	   * decr1 () ;
	   * decr2 () ;    
	   * incr_decr () ;
	   * gen_dup () ;
	   * gen_copy () ;    
	   * gen_reverse () ;
	   * gen_swap () ;
	   * xor () ;
	   * gr2 ()
	   * gp Turing_Machine.cpy_Bone_Bthree
	   *)
	   
	  (*My tests *)
	  
	  (*Tests Q1
	   *
	   * t1Q1 () ;
	   * t2Q1 () ;
	   * t3Q1 () 
	   *)

	  (*Tests Q2
	   *On remplace toujours la variable X
	   *
	   * t1Q2 () ;
	   * t2Q2 () ;
	   * t3Q2 ()
	   *)

	  (*Tests Q3
	   *
	   * t1Q3 () ;
	   * t2Q3 () ;
	   * t3Q3 () ;
	   *)


           (* busy_beaver Turing_Machine.bb4
	    * /!\  TERMINATING BUT EXTREMLY LONG COMPUTATIONS ... The sun will be dead before the end of BB6.
	    *
	      busy_beaver Turing_Machine.bb5 ;    
	      busy_beaver Turing_Machine.bb6 ;    
	    *)
	  ]
      end

