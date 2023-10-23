
structure ExtractCombine :> EXTRACT_COMBINE =
struct

    
    fun extractcombine (cmp: 'k * 'k -> order, 
    extract: 'a -> ('k * 'v) Seq.seq, 
    combine: 'v * 'v -> 'v, 
    docs: 'a MR.mapreducable
    ) : ('k,'v) Dict.dict =
    MR.mapreduce(
        fn z =>
            let val dicts = Seq.map(fn (k, v) => Dict.insert(cmp, Dict.empty, (k, v)), extract(z))
            in
            Seq.reduce(fn (e, f) => Dict.merge(cmp, fn (v, v') => combine(v, v'), e, f), Dict.empty, dicts)
            end,

        
        Dict.empty,
        (* combine *)
        (fn (a, b) => Dict.merge(cmp, fn (v, v') => combine(v, v'), a, b)),

        docs)
end
(* fn(x, y) => cmp(x,y) *)



