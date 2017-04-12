(* Michaïel PïeRIN, Verimag / Universitïe Grenoble-Alpes, Fïevrier 2017
 *
 * Part of the project TURING MACHINES FOR REAL
 *
 * (PROJECT 2019)  1. Multi-Bands Turing Machines working on a an alphabet A can be simulated by a single band Turing Machine using a augmented Alphbet A'
 *
 * (PROJECT 2017)  2. A Turing Machine using an alphabet A can be simulated by a Turing Machine using the binary alphabet {B,D}
 *
 * This module provides means to write Emulator for Problems 1 and 2.
 *
*)



open Tricks
open State
open Action
open Transition
open Band
open Configuration
open Turing_Machine
open Execution
open Bit_Vector


type emulator   = State.t * Action.t * State.t -> Turing_Machine.t
type translator = Band.t list -> Band.t list

type simulator  =
  { name: string ;
    encoder: translator ;
    decoder: translator ;
    emulator: emulator
  }

type simulators = simulator list


module Simulator =
  (struct

    type loggers = Logger.t list

    let (fake_tm_named: string ->  Turing_Machine.t) = fun name ->
      Turing_Machine.finalize name Turing_Machine.nop

    let (show_bands_using: loggers -> string -> Band.t list -> Band.t list) = fun loggers name bands ->
      begin
        (Configuration.make (fake_tm_named name) bands) >> (Configuration.print_using loggers) ;
        bands
      end

    let rec (execute_action_using: simulators * loggers -> (State.t * Action.t * State.t) -> Configuration.t -> Configuration.t) = fun (simulators,loggers) (src,action,tgt) cfg ->
      let cfg = cfg >> (Configuration.show_using loggers)
      in
      let next_bands =
        match simulators with
        | [] -> Action.perform action cfg.bands

        | simulator :: other_simulators
          ->
          let e_tm    = simulator.emulator (src,action,tgt)
          and e_bands = (simulator.encoder  cfg.bands) >> (show_bands_using loggers (String.concat " to " [ "encoding" ; simulator.name ]))
          in let e_cfg = Configuration.make e_tm e_bands
          in
          let e_next_cfg = log_run_using (other_simulators,loggers) e_cfg
          in
          let bands_updated_by_emulation = (simulator.decoder e_next_cfg.bands) >> (show_bands_using loggers (String.concat " " [ simulator.name ; "decoding"]))
          in
          let bands_updated_by_execution = Action.perform action cfg.bands
          in
          if (* FIXME: Band.are_equivalents *) bands_updated_by_execution = bands_updated_by_emulation
          then bands_updated_by_execution
          else failwith
              (String.concat "\n" [ "execute_action_using: simulation errors" ;
                                    Band.to_ascii_many bands_updated_by_emulation ;
                                    "are not equivalent to" ;
                                    Band.to_ascii_many bands_updated_by_execution ;
                                  ])
      in
      { cfg with bands = next_bands }


    and (execute_single_band_instruction_using: simulators * loggers -> (State.t * Instruction.t * State.t) -> Band.t -> Band.t) = fun (simulators,loggers) (src,instruction,tgt) band ->
      let cfg = Configuration.make (fake_tm_named (Instruction.pretty instruction)) [band]
      in let next_cfg = execute_instruction_using (simulators,loggers) (src,instruction,tgt) cfg
      in List.hd next_cfg.bands

    and (execute_instruction_using: simulators * loggers -> (State.t * Instruction.t * State.t) -> Configuration.t -> Configuration.t) = fun (simulators,loggers) (source,instruction,target) cfg ->
      (match instruction with
       | Run tm -> (* FIXME: ajoutez les transitions (source -nop-> init) et (accept -nop-> target) *)
         run_using (simulators,loggers) (Configuration.make tm cfg.bands)

       | Seq [] -> cfg
       | Seq (inst::instructions) ->
         let intermediate_state = State.fresh_from source in
         cfg
         >> (execute_instruction_using (simulators,loggers) (source, inst, intermediate_state))
         >> (execute_instruction_using (simulators,loggers) (intermediate_state, Seq instructions, target))

       | Parallel instructions ->
         let next_bands =
           List.map
             (fun (inst,band) -> execute_single_band_instruction_using (simulators,loggers) (source,inst,target) band)
             (Instruction.zip instructions cfg.bands)
         in { cfg with bands = next_bands }

       | Action action -> execute_action_using (simulators,loggers) (source,action,target) cfg
      )

    and (execute_transition_using: simulators * loggers -> Transition.t -> Configuration.t -> Configuration.t) = fun (simulators,loggers) (source,instruction,target) cfg ->
      let next_cfg = execute_instruction_using (simulators,loggers) (source,instruction,target) cfg
      in { next_cfg with state = target}

    and (run_using: simulators * loggers -> Configuration.t -> Configuration.t) = fun (simulators,loggers) cfg ->
      match Execution.select_enabled_transition cfg.tm cfg with
      | None -> cfg
      | Some transition ->
        let next_cfg = execute_transition_using (simulators,loggers) transition cfg
        in run_using (simulators,loggers) next_cfg

    and (log_run_using: simulators * loggers -> Configuration.t -> Configuration.t) = fun (simulators,loggers) cfg ->
      let loggers = cfg.logger :: loggers
      in
      let final_cfg = (run_using (simulators,loggers) cfg) >> (Configuration.show_using loggers)
      in
      begin
        cfg.logger#close ;
        final_cfg
      end

  end)


open State
open Symbol
open Alphabet
open Pattern
open Action
open Band
open Transition
open Turing_Machine
open Bit_Vector

(* An example of a useless but correct translation that splits the effect of a transition into three steps

   (q) -- l / e : d --> (q')
   ===
   (q) -- l : H --> (q.0) -- ANY / e : H --> (q.00) -- ANY : d --> (q')
*)


module Split =
  (struct

    (* TRANSLATION OF BANDS *)

    let (encode: translator) = fun x -> x

    (* REVERSE TRANSLATION *)

    let (decode: translator) = fun x -> x

    (* EMULATION OF TRANSITIONS *)

    let (just_read: reading -> Action.t) = fun reading ->
      RWM (reading, No_Write, Here)

    let (just_write: writing -> Action.t) = fun writing ->
      match writing with
      | No_Write     -> Nop
      | Write symbol -> RWM (Match(ANY), Write symbol, Here)

    let (just_move: moving -> Action.t) = fun moving ->
      RWM (Match(ANY), No_Write, moving)

    let (synchronize: Action.t list -> Instruction.t) = fun actionS ->
      let rec (rec_synchronize: ('r list * 'w list * 'm list) -> Action.t list -> ('r list * 'w list * 'm list)) = fun (reads,writes,moves) actions ->
        match actions with
        | [] -> (List.rev reads, List.rev writes, List.rev moves)
        | action::actions ->
          (match action with
           | Nop        -> rec_synchronize ( Nop::reads , Nop::writes , Nop::moves) actions
           | RWM(r,w,m) -> rec_synchronize ( (just_read r)::reads , (just_write w)::writes , (just_move m)::moves) actions
           | Simultaneous _ -> failwith "Emulator.Split.synchronize: nested Simultaneous"
          )
      in
      let (reads,writes,moves) = rec_synchronize ([],[],[]) actionS
      in
      Seq[ Action(Simultaneous(reads)) ; Action(Simultaneous(writes)) ; Action(Simultaneous(moves)) ]

    let rec (transitions_emulating: State.t * Action.t * State.t -> Transition.t list) = fun (source,action,target) ->
      (match action with
       | Nop -> [ (source, Action(Nop), target) ]

       | RWM(r,w,m) -> [ (source, Seq[ Action(just_read r) ; Action(just_write w) ; Action(just_move m) ], target) ]

       | Simultaneous actions -> [ (source, synchronize actions, target) ]
      )

    and (emulate_action: emulator) = fun (source,action,target) ->
      let (source,target) =
        if source <> target   (* /!\ loop in the emulated TM if source-target *)
        then (source,target)
        else (State.initial, State.accept)
      in
      let transitions =  transitions_emulating (source,action,target) in
      { Turing_Machine.nop with
        name = String.concat "" [ "Split" ; Pretty.parentheses (Transition.to_ascii (source,Action action, target)) ] ;
        initial = source ;
        accept  = target ;
        transitions = transitions
      }

    (* THE SIMULATOR *)

    let (* USER *) (simulator: simulator) = { name = "Split" ; encoder = encode ;  decoder = decode ; emulator = emulate_action }

  end)




module Binary =
struct

  (* TRANSLATION OF BANDS *)

  (* The modules Bit and Bits are defined in Alphabet.ml *)

  (** NEW 27/03/2107 *)
  type encoding = (Symbol.t * Bits.t) list

  (** Modifie le 06/04/2107 *)

	(*Fonction d'incrementation d'entier*)
	let incr : int -> int
	= fun x -> x+1 
 
	let rec aG : Bit.t -> Bits.t -> Bits.t 
	= fun a ax ->
		match ax with
		| [] -> [a]
		| x::xs -> x::(aG a xs)

	(*Fonction complement de bits a zero pour des codes binaires de meme taille*)
	let rec compl : int -> Bits.t -> Bits.t 
	= fun i bit ->
			match i with
  	| 0 -> bit
  	| i -> compl (i-1) (aG Bit.zero bit)
	
		
	(*Nombre de bits pour ecrire i en binaire*)
	let rec nBits : int -> int 
	= fun i -> 
	match i with
	| 0 -> 1
	| 1 -> 1
	| i -> 1 + nBits (i/2)

	(*Codage d'une liste de symboles d'alphabet, de meme taille que nb en binaire *)
  let rec code : symbols -> int -> int -> encoding
	= fun alphabet i nb ->
						match alphabet with
						| [] -> []
						| [x] -> (x , ( compl ((nBits nb)-(nBits i)) (Bits.int_to_bits i) ) )::(code [] (incr i) nb ) 
  					| x::xs -> (x , ( compl ((nBits nb)-(nBits i)) (Bits.int_to_bits i) ) )::(code xs (incr i) nb )

	(*fonction associant un symbole a un code binaire*)
  let build_encoding : Alphabet.t -> encoding
    = fun alphabet -> 
  				let nb = alphabet.symbol_size_in_bits in
						code alphabet.symbols 0 nb				
	
	(*trouve le coda associe au symbole*)
	let rec associated_symbol : Symbol.t -> encoding -> Bits.t
	=fun symb code ->
		match fst (List.hd code) with
		| v -> if (v = symb) then snd (List.hd code) else associated_symbol symb (List.tl code)
	
		(*change un symbole en sa traduction en bits *)
	let rec bits_in_symbols_list: (Symbol.t list) -> Bits.t ->  (Symbol.t list)
	= fun sym bit ->
  		match bit with
			| [] -> sym
  		| D::next -> (bits_in_symbols_list (D::sym) (List.tl bit )) 
			| B::next -> (bits_in_symbols_list (B::sym) (List.tl bit )) 
	
	(*change une liste de symbole en une liste de leur traduction en bits *)
	let rec replaceBand : Symbol.t list -> encoding -> Bits.t list
	=fun sym code ->
		match sym with
		| [] -> []
		| x::xs -> (associated_symbol x code)::(replaceBand xs code)
	
	(*change une liste de bits en liste de symbols *)
	let rec bitBand_to_Band : Bits.t list -> Symbol.t list
	=fun bits ->
		match bits with
  	| [] -> []
  	| x :: xs -> bits_in_symbols_list (bitBand_to_Band xs) x
	
	(*traduit une bande de symbols normaux en bande de symbols de {B,D} *)
	let encode_band : encoding -> Band.t -> Band.t
	 = fun coding band ->
  	let newband : Band.t =  
  			{
  			color = ( band.color ) ;
  			alphabet = {symbols = [B;D]; symbol_size_in_bits = 1} ;
  			left = ( bitBand_to_Band (replaceBand band.left coding) );
  			head = List.hd ( bits_in_symbols_list [] ( associated_symbol band.head coding ) );
  			right = bits_in_symbols_list ( bitBand_to_Band (replaceBand band.right coding) ) (List.tl (bits_in_symbols_list [] ( associated_symbol band.head coding ))) ;
  		} in
  		newband
	
  (** MODIFIED 06/04/2107 *)
	(*traduit la liste de bandes de symbols normaux en bande de symbols de {B,D} *)
  let rec encode_with : encoding -> Band.t list -> Band.t list
    = fun coding bands ->
				match bands with
				| [] -> []
				| x::xs->  (encode_band coding x )::(encode_with coding xs)
				
      


  (* REVERSE TRANSLATION *)
	
	(*change une liste de bits sous forme de symboles en bits *)
	let rec decode_bits : int -> Symbol.t list -> Bits.t
	=fun nBits decode ->
		match nBits with
		| 0 -> []
		| nBits -> (match decode with
									| B::xs -> aG Bit.zero (decode_bits (nBits-1) xs)  
									| D::xs -> aG Bit.unit (decode_bits (nBits-1) xs)  )
		
		(*decale la liste de symbole du nombre de decalage donnes*)
		let rec decale_list : int -> Symbol.t list -> Symbol.t list
		=fun nb sym ->
			match nb with
			| 0 -> sym
			| nb -> (match sym with
									| [] ->[]
									| x::xs -> decale_list (nb-1) xs)
							
	(*renvoi le symbol associe au bits*)
	let rec associated_Bits :  Bits.t -> encoding -> Symbol.t
	=fun bits -> fun code ->
		match snd (List.hd code) with
		| b -> if (b=bits) then fst (List.hd code) else associated_Bits bits (List.tl code)
	
	(*change une liste de bits sous forme de symbole en une liste de symboles correspondant*)
	let rec symbit_to_bits : int -> encoding -> Symbol.t list -> Symbol.t list
	= fun nBits decode sym ->
		match sym with
		| [] -> []
		| sym -> (associated_Bits (decode_bits nBits sym) decode )::(symbit_to_bits nBits decode (decale_list nBits sym))
	
	(*nombre de bits de bits*)
	let rec nombreMaxBits: Bits.t -> int
	=fun bits ->
		match bits with
		| []-> 0
		| x::xs -> 1 + nombreMaxBits xs
		
	
		let rec symblist : (Symbol.t*Bits.t) -> Symbol.t
		=fun (s,b) -> s
		
	(*decodage d'une bande de bits sous forme de symboles en bande de symbole*)
	let decode_band : encoding -> Band.t -> Band.t
	 = fun coding band ->
		let nBitsMax = nombreMaxBits (snd (List.hd coding))  in (*TODO*)
  	let newband : Band.t =  
  			{
  			color = ( band.color ) ;
  			alphabet = {symbols = (List.map symblist coding) ; symbol_size_in_bits = nBitsMax} ;
  			left = symbit_to_bits nBitsMax coding band.left;
  			head = List.hd (symbit_to_bits nBitsMax coding (band.head::band.right));
  			right =List.tl (symbit_to_bits nBitsMax coding (band.head::band.right));
  		} in
  		newband
			
  (** MODIFIED 27/03/2107 *)
	(*decodage de bandes de bits sous forme de symboles en bandes de symbole*)
  let rec decode_with : encoding -> Band.t list -> Band.t list
  (* PROJET 2017: modifiez ce code -> *)
    = fun coding bands ->
				match bands with
				| [] -> []
				| x::xs->  (decode_band coding x )::(decode_with coding xs)


  (* EMULATION OF TRANSITIONS *)

  let (emulate_action: State.t * Action.t * State.t -> Turing_Machine.t)
  (* PROJET 2017: modifiez ce code -> *)
    = fun (source,action,target) ->
      { Turing_Machine.nop with
        name = String.concat "" [ "Binary" ; Pretty.parentheses (Action.to_ascii action) ] ;
        initial = State.initial ;
        accept  = State.accept ;
        transitions = [(State.initial,Action(action),State.accept)]
      }

  (* THE SIMULATOR *)

  (** MODIFIED 27/03/2107 *)
  let make_simulator : Alphabet.t -> simulator
    = fun alphabet ->
      let encoding = build_encoding alphabet in
      { name = "Binary" ;
        encoder = encode_with encoding ;
        decoder = decode_with encoding ;
        emulator = emulate_action
      }

end


(* DEMO *)

open Alphabet

let (demo: unit -> unit) = fun () ->
  let alphabet = Alphabet.make [B;Z;U] in
  let band = Band.make alphabet [U;U;Z;U] in
  let tm = Turing_Machine.incr in
  let cfg = Configuration.make tm [ band ] in
  let _final_cfg = Simulator.log_run_using
      ([ (* Split.simulator ; *)
        (** MODIFIED 27/03/2107 *) Binary.make_simulator alphabet
      ],[])
      cfg
  in ()
