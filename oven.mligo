#include "oven_types.mligo"

(fun (p , s : oven_parameter * oven_storage) -> (
        (* error codes *)
        let error_WITHDRAW_CAN_ONLY_BE_CALLED_FROM_MAIN_CONTRACT = 1n in
        let error_ONLY_OWNER_CAN_DELEGATE = 2n in
        let error_CANNOT_FIND_REGISTER_DEPOSIT_ENTRYPOINT = 3n in
        let error_UNAUTHORIZED_DEPOSITOR = 4n in
        (match p with
        (* Withdraw form the oven, can only be called from the main contract. *)
        | Oven_withdraw x ->
            if Tezos.sender <> s.admin then
            (failwith error_WITHDRAW_CAN_ONLY_BE_CALLED_FROM_MAIN_CONTRACT : oven_result)
            else
            ([Tezos.transaction unit x.0 x.1], s)
        (* Change delegation *)
        | Oven_delegate ko ->
            if Tezos.sender <> s.owner then
            (failwith error_ONLY_OWNER_CAN_DELEGATE : oven_result)
            else ([Tezos.set_delegate ko], s)
        (* Make a deposit. If authorized, this will notify the main contract. *)
        | Oven_deposit ->
            if Tezos.sender = s.owner or Set.mem Tezos.sender s.depositors or Set.mem ("tz1authorizeAnyoneToDeposit3AC7qy8Qf" : address) s.depositors then
            let register = (
                match (Tezos.get_entrypoint_opt "%register_deposit" s.admin : (register_deposit contract) option) with
                | None -> (failwith error_CANNOT_FIND_REGISTER_DEPOSIT_ENTRYPOINT : register_deposit contract)
                | Some register -> register) in
            (([ Tezos.transaction {amount = Tezos.amount ; owner = s.owner} 0mutez register] : operation list), s)
            else
            (failwith error_UNAUTHORIZED_DEPOSITOR : oven_result)
        (* Edit the set of authorized depositors. Insert tz1authorizeAnyoneToDeposit3AC7qy8Qf to authorize anyone. *)
        | Oven_edit_depositor edit ->
            let (depositor, insert) = edit in
            (([] : operation list), {s with depositors = if insert then Set.add depositor s.depositors else Set.remove depositor s.depositors}))))