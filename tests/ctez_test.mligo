#include "../ctez.mligo"

let cfmm_test = 
  let oven_init = Big_map.literal [
    ({ id = 1n ; owner = Test.nth_bootstrap_account 2 ;}),
    ({ 
      address = Test.nth_bootstrap_contract 3n ; 
      ctez_outstanding = 10tez ; 
      fee_index = 1n ; 
      tez_balance = 10tez })
  ] in
  let initial_storage : storage = {
   	ovens = (Big_map.empty : (oven_handle, oven) big_map)  ;
	  target = Bitwise.shift_left 1n 48n ; 
    drift = 0 ;
	  last_update = ("2023-01-07T00:00:00Z" : timestamp) ;
	  ctez_fa12_address = Test.nth_bootstrap_contract 0n ;
	  cfmm_address = Test.nth_bootstrap_account 1 ;
	  fee_index = 281474976710656n ; (*2^48*)
  } in 
  let taddr, _, _ = Test.originate main initial_storage 0tez in
  let _ = Test.set_source initial_storage.cfmm_address in


  let (_, del) = Test.new_account () in
  let del_kh : key_hash = Crypto.hash_key del in
  let create_param : create = {
    id = 1n ;
    delegate = Some del_kh ;
    depositors = Any;
  } in
  let _ = (match (Test.transfer_to_contract (Test.to_contract taddr) (Create create_param) 1tez)  with
    | Success _ -> true
    | Fail err -> 
      (match err with
      | Rejected (msg, _) -> msg = Test.eval "SOMETHING_WRONG"
      | _ -> true )) 
    |> assert in
  
  let set_addresses_param : set_addresses = {
    cfmm_address = Test.nth_bootstrap_contract 2n;
    ctez_fa12_address = Test.nth_bootstrap_contract 3n;
  } in
  let _ = (match (Test.transfer_to_contract (Test.to_contract taddr) (Set_addresses set_addresses_param) 0tez)  with
    | Success _ -> true
    | Fail err -> 
      (match err with
      | Rejected (msg, _) -> msg = Test.eval "Failed"
      | _ -> false )) 
    |> assert in
  let new_storage = Test.get_storage taddr in
  let _ = Test.log ("NEW CFMM ADDRESS", new_storage.cfmm_address) in
  let _ = Test.log ("NEW CTEZ_FA12 ADDRESS", new_storage.cfmm_address) in

  (* params to test cfmm_info  *)
  let price_num = 50436391167323969561508437492287689988593055569116939129294910813529676256982417023809109208854949983583347999455420725447557819810253332684373500411267983243137253376n in
  let price_den = 50436381083975567075556973995521050464584912042645665923189557817931685739190807652550868398155715707739132224614354432349331600084904405901298009140752205223882129408n in
  let cash_pool = 9000102n in 
  let _ = (match (Test.transfer_to_contract (Test.to_contract taddr) (Cfmm_info ((price_num, price_den), cash_pool)) 0tez)  with
    | Success _ -> true
    | Fail err -> 
      (match err with
      | Rejected (msg, _) -> msg = Test.eval "Failed"
      | _ -> false )) in
  (* |> assert in *)
  let new_storage = Test.get_storage taddr in
  let _ = ("NEW DRIFT", Test.log new_storage.drift) in
  let _ = ("OLD DRIFT", Test.log init_storage.drift) in
  let _ = ("NEW TARGET", Test.log new_storage.target) in
  let _ = ("OLD TARGET", Test.log init_storage.target) in
  let _ = ("NEW FEE INDEX", Test.log new_storage.fee_index) in
  let _ = ("OLD FEE INDEX", Test.log init_storage.fee_index) in
  () 