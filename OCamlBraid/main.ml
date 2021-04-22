let _ =

  Format.printf "p = %s\n" (Big_int.string_of_big_int Lib.p);
  Format.printf "q = %s\n" (Big_int.string_of_big_int Lib.q);  
  
  let wikSigma = Lib.ElGamalWikstrom.coq_WikstromSigma in
  let wikStatment = Lib.ElGamalWikstrom.coq_WikstromStatment in
  
  let rec intToNat = (fun i ->
    match i with
    | 0 -> Lib.O
    | x -> Lib.S (intToNat (x-1))
  ) in 
  
  let rec toVectorNBaseBig = (fun i (x : Big_int.big_int list) ->
    match x with
    | [] -> Lib.Nil
    | z::zs -> Lib.Cons (z, i, (toVectorNBaseBig (Lib.pred i) zs))
  ) in
  
  (* Converts a list of Big_ints to a vector of Big_ints *)
  let toVectorNBig = (fun (x : Big_int.big_int list) ->
    toVectorNBaseBig (intToNat (List.length x)) x
  ) in
  
  let jsonToBigint = (fun x ->
    let xString = Yojson.Basic.to_string x in
    let xStringClean = String.sub xString 1 (String.length xString - 2) in
    let xStringAppend = "0x"^xStringClean in
    Big_int.big_int_of_string xStringAppend
  ) in
  
  let toListOfElements = (fun x ->
    List.map jsonToBigint x
  ) in
  
  let rec toVectorNBaseCiph = (fun i x ->
    match x with
    | [] -> Lib.Nil
    | z::zs ->
    Lib.Cons ((jsonToBigint (fst z), jsonToBigint (snd z)), i, (toVectorNBaseCiph (Lib.pred i) zs))
  ) in
  
  (* Converts a list of strings to a vector of big_ints *)
  let toVectorNCiph = (fun x ->
    toVectorNBaseCiph (intToNat (List.length x)) x
  ) in

  let rec toVectorNBaseJson = (fun i x ->
    match x with
    | [] -> Lib.Nil
    | z::zs ->
    Lib.Cons ((jsonToBigint z), i, (toVectorNBaseJson (Lib.pred i) zs))
  ) in
  
  (* Converts a list of strings to a vector of big_ints *)
  let toVectorNJson = (fun x ->
    toVectorNBaseJson (intToNat (List.length x)) x
  ) in

  (* Transcript *)
  let open Yojson.Basic.Util in

  Format.printf "%s\n" "Loading data";

  if Array.length Sys.argv <> 2 then
    failwith "No transcript file supplied";

  let transcript_name = Sys.argv.(1) in
  (* let transcript = BatFile.open_in "data/transcript_braid.json" in *)
  let transcript = BatFile.open_in transcript_name in
  let transcript_string = BatIO.read_all transcript in
  let transcript_json = Yojson.Basic.from_string transcript_string in
  let group_json = transcript_json |> member "group" in
  let pkjson = transcript_json |> member "pk" in
  let hjson = transcript_json |> member "hs" in
  let ujson = transcript_json |> member "us" in
  let c_json = transcript_json |> member "permutation_commitment" in
  let t_json = transcript_json |> member "proof_commitment" in
  let s_json = transcript_json |> member "proof_reply" in
  let chal_json = transcript_json |> member "challenge" in
  let ciphin_json = transcript_json |> member "ciphers_in" in
  let ciphout_json = transcript_json |> member "ciphers_out" in

  let grouplistjson = Yojson.Basic.Util.to_list group_json in
  let pjson = List.nth grouplistjson 0 in
  let qjson = List.nth grouplistjson 1 in
  let transcript_p = jsonToBigint pjson in
  let transcript_q = jsonToBigint qjson in

  if not (Big.eq Lib.p transcript_p) then
    failwith "p prime mismatch";

  if not (Big.eq Lib.q transcript_q) then
    failwith "q prime mismatch";

  Format.printf "%s\n" "* Loading pk";
  let pklistjson = Yojson.Basic.Util.to_list pkjson in
  let pkgenjson = List.nth pklistjson 0 in
  let pkjson = List.nth pklistjson 1 in
  let pkgen = jsonToBigint pkgenjson in
  let pkval = jsonToBigint pkjson in
  Format.printf "generator = %s\n" (Big_int.string_of_big_int pkgen);
  Format.printf "pk = %s\n" (Big_int.string_of_big_int pkval);
  let pk_ = (pkgen, pkval) in

  Format.printf "%s\n" "* Loading hs";
  let hlistjson = Yojson.Basic.Util.to_list hjson in
  let hjson = List.nth (Yojson.Basic.Util.to_list (List.nth hlistjson 0)) 0 in
  let hsjson = Yojson.Basic.Util.to_list (List.nth hlistjson 1) in
  let hslist = toListOfElements hsjson in
  let hs_ = toVectorNBig hslist in
  let h_ = jsonToBigint hjson in
  Format.printf "h = %s\n" (Big_int.string_of_big_int h_);
  Format.printf "hs size = %d\n" (List.length hslist);

  Format.printf "%s\n" "* Loading us";
  let ulistjson = Yojson.Basic.Util.to_list ujson in
  let ulist = toListOfElements ulistjson in
  let u_ = toVectorNBig ulist in
  Format.printf "us size = %d\n" (List.length ulist);

  Format.printf "%s\n" "* Loading permutation_commitment";
  let c_listjson = Yojson.Basic.Util.to_list c_json in
  let c_list = toListOfElements c_listjson in
  let c_ = toVectorNBig c_list in
  Format.printf "cs size = %d\n" (List.length c_list);

  Format.printf "%s\n" "* Loading proof_commitment";
  let t_listjson = Yojson.Basic.Util.to_list t_json in
  let c_HatJson = List.nth t_listjson 0 in
  let t3_Json = List.nth t_listjson 1 in
  let t_HatJson = List.nth t_listjson 2 in
  let t1_Json = List.nth t_listjson 3 in
  let t2_Json = List.nth t_listjson 4 in
  let t4_Json = List.nth t_listjson 5 in
  let t1_ = jsonToBigint (List.nth (Yojson.Basic.Util.to_list t1_Json) 0) in
  let t2_ = jsonToBigint (List.nth (Yojson.Basic.Util.to_list t2_Json) 0) in
  let t3_ = jsonToBigint (List.nth (Yojson.Basic.Util.to_list t3_Json) 0) in
  let t4_prim = Yojson.Basic.Util.to_list t4_Json in
  (* A,B swap *)
  let t4_ = (jsonToBigint (List.nth t4_prim 1), jsonToBigint (List.nth t4_prim 0)) in
  let tHat_ = toVectorNJson (Yojson.Basic.Util.to_list t_HatJson) in
  let c_Hatlistjson = Yojson.Basic.Util.to_list c_HatJson in
  let c_Hatlist = toListOfElements c_Hatlistjson in
  let cHat_ = toVectorNBig c_Hatlist in
  Format.printf "t1 = %s\n" (Big_int.string_of_big_int t1_);
  Format.printf "t2 = %s\n" (Big_int.string_of_big_int t2_);
  Format.printf "t3 = %s\n" (Big_int.string_of_big_int t3_);
  Format.printf "t4_1 = %s\n" (Big_int.string_of_big_int (jsonToBigint (List.nth t4_prim 0)));
  Format.printf "t4_2 = %s\n" (Big_int.string_of_big_int (jsonToBigint (List.nth t4_prim 1)));
  Format.printf "c_hats size = %d\n" (List.length c_Hatlist);
  Format.printf "t_hats size = %d\n" (List.length (Yojson.Basic.Util.to_list t_HatJson));

  Format.printf "%s\n" "* Loading challenge";
  let challistJson = Yojson.Basic.Util.to_list chal_json in
  let chal_ = jsonToBigint (List.nth challistJson 0) in
  Format.printf "challenge = %s\n" (Big_int.string_of_big_int chal_);

  Format.printf "%s\n" "* Loading proof_reply";
  let s_Listjson = Yojson.Basic.Util.to_list s_json in
  let s3_Json = List.nth s_Listjson 0 in
  let s_HatJson = List.nth s_Listjson 1 in
  let s1_Json = List.nth s_Listjson 2 in
  let s2_Json = List.nth s_Listjson 3 in
  let s_PrimeJson = List.nth s_Listjson 4 in
  let s4_Json = List.nth s_Listjson 5 in
  let s1_ = jsonToBigint (List.nth (Yojson.Basic.Util.to_list s1_Json) 0) in
  let s2_ = jsonToBigint (List.nth (Yojson.Basic.Util.to_list s2_Json) 0) in
  let s3_ = jsonToBigint (List.nth (Yojson.Basic.Util.to_list s3_Json) 0) in
  let s4_ = jsonToBigint (List.nth (Yojson.Basic.Util.to_list s4_Json) 0) in
  let sHat_ = toVectorNJson (Yojson.Basic.Util.to_list s_HatJson) in
  let sPrime_ = toVectorNJson (Yojson.Basic.Util.to_list s_PrimeJson) in
  Format.printf "s1 = %s\n" (Big_int.string_of_big_int s1_);
  Format.printf "s2 = %s\n" (Big_int.string_of_big_int s2_);
  Format.printf "s3 = %s\n" (Big_int.string_of_big_int s3_);
  Format.printf "s4 = %s\n" (Big_int.string_of_big_int s4_);
  Format.printf "s_hats size = %d\n" (List.length (Yojson.Basic.Util.to_list s_HatJson));
  Format.printf "s_primes size = %d\n" (List.length (Yojson.Basic.Util.to_list s_PrimeJson));
  
  Format.printf "%s\n" "* Loading ciphers_in";
  let ciphinlistjson = Yojson.Basic.Util.to_list ciphin_json in
  (* A,B swap *)
  let ciphersinA = Yojson.Basic.Util.to_list (List.nth ciphinlistjson 1) in
  let ciphersinB = Yojson.Basic.Util.to_list (List.nth ciphinlistjson 0) in
  let ciphersin = List.combine ciphersinA ciphersinB in
  Format.printf "input ciphertexts size = %d\n" (List.length ciphersin);
  
  Format.printf "%s\n" "* Loading ciphers_out";
  let ciphoutlistjson = Yojson.Basic.Util.to_list ciphout_json in
  (* A,B swap *)
  let ciphersoutA = Yojson.Basic.Util.to_list (List.nth ciphoutlistjson 1) in
  let ciphersoutB = Yojson.Basic.Util.to_list (List.nth ciphoutlistjson 0) in
  let ciphersout = List.combine ciphersoutA ciphersoutB in
  Format.printf "output ciphertexts size = %d\n" (List.length ciphersout);
  let e_ = toVectorNCiph ciphersin in
  let e_' = toVectorNCiph ciphersout in

  Format.printf "%s\n" "Data loaded";
  Format.print_flush ();

  (* let statment = wikStatment pk h (Lib.hd h hs) hs c cHat u e e' in *)
  let statement = wikStatment pk_ pkgen h_ hs_ c_ cHat_ u_ e_ e_' in
  (* let statement = wikStatment pk_ pkgen (Lib.hd h_ hs_) hs_ c_ cHat_ u_ e_ e_' in *)
  
  Format.printf "%s\n" "Statement Prepared";

  (* let com = Obj.magic ((t1,t2),((t3,t4),tHat)) in *)
  let com = Obj.magic ((t1_,t2_),((t3_,t4_),tHat_)) in
  
  Format.printf "%s\n" "Commitment Prepared";
  Format.print_flush ();
  
  Format.printf "%s\n" "Getting ready to pass response";
  Format.print_flush ();


  
  (* let resp = Obj.magic ((s1, s2), (((sPrime, s3), s4), sHat))  in*)
  let resp = Obj.magic ((s1_, s2_), (((sPrime_, s3_), s4_), sHat_))  in
  
  Format.printf "%s\n" "Transcript Prepared";
  Format.print_flush ();
  
  let result = wikSigma.coq_V1 (((statement, com), chal_), resp) in
  
  Format.printf "<<< Result ok = %b for \'%s'>>>\n" result transcript_name;
  Format.print_flush ();