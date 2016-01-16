open List
open Printf
type porownanie = Mniejsze | Rowne | Wieksze

module type PORZADEK_LINIOWY =
sig
    type t
    val porownaj : t ->t ->porownanie
end

module PriQueue = functor (Elem : PORZADEK_LINIOWY) ->
struct
    type elem = Elem.t
    exception EmptyQueue
    type queue = elem list
    let empty = []
    let is_empty q = q = empty
    let rec put x q =
        if q = [] then [x]
        else if Elem.porownaj x (hd q) = Wieksze then x :: q
        else (hd q)::(put x (tl q))
    let getmax q =
        if q = [] then raise EmptyQueue
        else hd q
    let removemax (q:queue) =
        if q = [] then raise EmptyQueue
        else tl q
end

module Order =
struct
    type t = int * int * int
    let porownaj x y =
        let c = compare x y in
        if c > 0 then Mniejsze else
        if c < 0 then Wieksze else Rowne
end

module PQ = PriQueue(Order)

exception Result

let wysokosc ar =
    let n = Array.length ar
    and m = Array.length ar.(0) in
    let mat = Array.init (n+2)
        (fun a -> Array.init (m+2)
            (fun b ->
                if a = 0 || a = n+1 || b = 0 || b = m+1 then max_int
                else ar.(a-1).(b-1)
            )
        )
    in let pq = ref (PQ.put (mat.(1).(1), 1, 1) PQ.empty)
    in let hei = ref (mat.(1).(1))
    in let visited = Array.make_matrix (n+2) (m+2) false
    in let visit x y =
        if not visited.(x).(y)
        then
            pq := PQ.put (mat.(x).(y), x, y) !pq;
    in
        begin
            try
                while not (PQ.is_empty !pq) do
                    let (h, x, y) = PQ.getmax !pq in
                    pq := PQ.removemax !pq;
                    visited.(x).(y) <- true;
                    hei := max !hei h;
                    if (x,y) = (n, m) then raise Result
                    else
                        begin
                            visit (x-1) y;
                            visit (x+1) y;
                            visit x (y+1);
                            visit x (y-1)
                        end
                done
                with Result -> ()
        end;
        !hei

let t =
    [|
        [| 1; 1; 2; 9; 15 |];
        [| 7; 8; 2; 3; 6 |];
        [| 3; 3; 3; 0; 0 |];
        [| 4; 100; 100; 100; 100|];
        [| 4; 5; 5; 2; 6 |]
    |]

let most ar k =
    let div x y =
        if x mod y = 0 then x / y
        else x / y + 1
    in let n = Array.length ar in
    let l = ref 1 and r = ref (n-1) in
    let c =
        let kk = ref max_int in
        for i = 0 to n-1 do
            if ar.(i) < !kk then kk := ar.(i)
        done;
        !kk
    in let good d =
        let g = ref true in
        let pod = ref 0 in (* postawione podpory *)
        let x = ref 0 in (* licznik odleglosci *)
        let m = ref max_int in (* minimum na podprzedziale *)
        for i = 0 to n-1 do
            incr x;
            let s = (div !x d) * c in
            m := min (!m) ar.(i);
            if s > !m then (* trzeba postawić podpore *)
            begin
                if !pod = k then g := false (* nie da sie *)
                else
                begin
                    incr pod;
                    x := 1;
                    m := max_int
                end
            end
        done;
        !g
    in while !l <> !r do
        let mid = (!r + !l)/2 in
        if good mid then r := mid else l := mid + 1
    done;
    !l

let kwadrat mat =
    let min3 x y z = min (min x y) z in
    let n = Array.length mat
    and m = Array.length mat.(0) in
    let wyn = Array.make_matrix n m 0 in
    let nad = Array.make_matrix n m 0 in
    let lewo = Array.make_matrix n m 0 in
    let add i j =
        wyn.(i).(j) <- if mat.(i).(j) then 1 else 0;
        nad.(i).(j) <- if mat.(i).(j) then 1 else 0;
        lewo.(i).(j) <- if mat.(i).(j) then 1 else 0
    in let ret = ref 0 in
        for i = 0 to n-1 do add i 0 done;
        for j = 0 to m-1 do add 0 j done;
        for i = 1 to n-1 do
            for j = 1 to m-1 do
                if mat.(i).(j) then
                begin
                    wyn.(i).(j) <- min3 (wyn.(i-1).(j-1)) (nad.(i-1).(j)) (lewo.(i).(j-1)) + 1;
                    nad.(i).(j) <- nad.(i-1).(j) + 1;
                    lewo.(i).(j) <- lewo.(i).(j-1) + 1;
                    ret := max !ret (wyn.(i).(j))
                end
            done
        done;
        !ret

let t = true
let f = false

let tab =
    [|
        [| f; f; f; f; f; t; t; t; t |];
        [| f; f; f; t; f; t; t; t; t |];
        [| f; f; t; t; t; t; t; t; f |];
        [| f; f; f; f; t; f; f; f; f |];
        [| t; f; f; f; f; f; f; f; f |];
    |]
let _ = printf "%d\n" (kwadrat tab)