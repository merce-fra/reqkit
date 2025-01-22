(* Copyright 2025 Mitsubishi Electric R&D Centre Europe
 * Author: François Cellier
 * Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
 * 1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
 * 3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS “AS IS” AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *)

open Src.Input_args

let () = 
  let exit_value = ref 0 in
  let (args, usage) = Src.Input_args.get () in   
  (match (args.input_file, args.input_dir) with 
  (* case the tool is apply on a whole directory in order to extract constructions *)
    |None, Some d ->    
      begin
          try
            let list_req_files =  Array.to_list (Sys.readdir d) in          
            let input_parse_list = List.fold_left (fun acc f ->  (Reqs.Parse.of_file (d^"/"^f))::acc) [] list_req_files in
            let typical_reqs = Src.Util.typical_reqs input_parse_list args.keep_simple in
            let fmt = Format.get_std_formatter () in 
            List.iteri (fun i p ->  ( Format.printf "##################################################################@.Construction #%d@." (i+1); 
                                      Reqs.Parse.pretty_print fmt p;
                                      (match args.output_fmt with
                                      | NuSMV   -> Src.Sup.generate_sup_file fmt p args
                                      | VMT  -> Src.Vmt.generate_vmt_file fmt p args);
                                    ) ) typical_reqs;

            Format.printf "Success@."
          with Reqs.Parse.ParseException msg -> 
            Format.printf "%s@." msg;
            exit_value := -1
      end
    (* case the tool is apply on a single file *)
    |Some f, None when String.ends_with ~suffix:".req" f -> 
      begin
      (* case of .req file *)  
      try 
        let t =  (Reqs.Parse.of_file f) in 
        let fmt = Format.get_std_formatter() in
        (match args.output_fmt with
        | NuSMV  -> Src.Sup.generate_sup_file fmt t args
        | VMT  -> Src.Vmt.generate_vmt_file fmt t args);
      with Reqs.Parse.ParseException msg -> 
        Printf.fprintf stderr "%s\n" msg;
        exit_value := -1
      end
    |Some f, None when (String.ends_with ~suffix:".py" f) || (String.ends_with ~suffix:".sup" f) -> 
      begin
        try
          let t = (Sups.Parse.of_file f) in
          let fmt = Format.get_std_formatter() in
          (match args.output_fmt with
          | VMT -> Src.Vmt.generate_vmt_file_from_sup fmt t args
          | NuSMV -> Printf.fprintf stderr "Error. Only VMT output format supported for SUP input files.\n");
        with Sups.Parse.ParseException msg ->
          Printf.fprintf stderr "%s\n" msg;
          exit_value := -1
      end

    | _,_ -> 
      Printf.fprintf stderr "%s\n" usage;
      exit_value := -1
  );
  exit !exit_value