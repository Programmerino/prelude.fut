import "option"

type result 'a 'e = 
    #Ok(a)
    | #Error(e)

module type Result = {
    -- | `ok v` is `#Ok v`
    val ok 'a 'e: a -> result a e 

    -- | `error e` is `#Error e`
    val error 'a 'e: e -> result a e

    -- | `value r default` is `v` if `r` is `#Ok v` and `default` otherwise.
    val value 'a 'e: result a e -> a -> a

    -- | `bind r f` is `f v` if `r` is `#Ok v` and `r` if `r` is `#Error_`.
    val bind 'a 'b 'e: result a e -> (a -> result b e) -> result b e

    -- | `join rr` is `r` if `rr` is `#Ok r` and `rr` if `rr` is `#Error _`.
    val join 'a 'b 'e: result (result a e) e -> result a e

    -- | `map f r` s `#Ok (f v)` if `r` is `#Ok v` and `r` if `r` is `#Error _`.
    val map 'a 'b 'e: (a -> b) -> result a e -> result b e

    -- | `map_error f r` is `#Error (f e)` if `r` is `#Error e` and `r` if `r` is `#Ok _`.
    val map_error 'e 'f 'a: (e -> f) -> result a e -> result a f

    -- | `fold ok error r` is `ok v` if `r` is `#Ok v` and `error e` if `r` is `#Error e`.
    val fold 'a 'c 'e: (a -> c) -> (e -> c) -> result a e -> c

    -- | `iter f r` is `f v` if `r` is `#Ok v` and `()` otherwise.
    val iter 'a 'e: (a -> ()) -> result a e -> ()

    -- | `iter_error f r` is `f e` if `r` is `#Error e` and `()` otherwise.
    val iter_error 'e 'a: (e -> ()) -> result a e -> ()

    -- | `is_ok r` is `true` if and only if `r` is `#Ok _`.
    val is_ok 'a 'e: result a e -> bool

    -- | `is_error r` is `true` if and only if `r` is `#Error _`.
    val is_error 'a 'e: result a e -> bool

    -- | `equal ok error r0 r1` tests equality of `r0` and `r1` using `ok` and `error` to respectively compare values wrapped by `#Ok _` and `#Error _`.
    val equal 'a 'e: (a -> a -> bool) -> (e -> e -> bool) -> result a e -> result a e -> bool

    -- | `compare ok error r0 r1` totally orders `r0` and `r1` using `ok` and `error` to respectively compare values wrapped by `#Ok _` and `#Error _`.
    -- `#Ok _` values are smaller than `#Error _` values.
    val compare 'a 'e: (a -> a -> i64) -> (e -> e -> i64) -> result a e -> result a e -> i64

    -- | `to_option r` is `r` as an option, mapping `#Ok v` to `#Some v` and `#Error _` to `#None`.
    val to_option 'a 'e: result a e -> option a

    -- | `to_array r` is `[v]` if `r` is `#Ok v` and `[]` otherwise.
    val to_array 'a 'e: result a e -> []a
}

module Result: Result = {
    def ok 'a 'e v: result a e = #Ok(v)

    def error 'a 'e e: result a e = #Error(e)

    def value 'a 'e (r: result a e) default =
        match r
        case #Ok(v) -> v
        case #Error(_) -> default

    def bind 'a 'b 'e (r: result a e) f: result b e =
        match r
        case #Ok(v) -> f v
        case #Error(e) -> #Error(e)

    def join 'a 'e (rr: result (result a e) e): result a e =
        match rr
        case #Ok(r) -> r
        case #Error(e) -> #Error(e)

    def map 'a 'b 'e f (r: result a e): result b e =
        match r
        case #Ok(v) -> #Ok(f v)
        case #Error(e) -> #Error(e)

    def map_error 'e 'f 'a f (r: result a e): result a f = 
        match r
        case #Ok(v) -> #Ok(v)
        case #Error(e) -> #Error(f e)

    def fold 'a 'c 'e (ok: a -> c) (error: e -> c) (r: result a e): c =
        match r
        case #Ok(v) -> ok v
        case #Error(e) -> error e

    def iter 'a 'e (f: a -> ()) (r: result a e): () =
        match r
        case #Ok(v) -> f v
        case #Error(_) -> ()

    def iter_error 'e 'a (f: e -> ()) (r: result a e): () =
        match r
        case #Ok(_) -> ()
        case #Error(e) -> f e

    def is_ok 'a 'e (r: result a e): bool =
        match r
        case #Ok(_) -> true
        case #Error(_) -> false

    def is_error 'a 'e (r: result a e): bool =
        match r
        case #Ok(_) -> false
        case #Error(_) -> true

    def equal 'a 'e (ok: a -> a -> bool) (error: e -> e -> bool) (r0: result a e) (r1: result a e): bool =
        match (r0, r1)
        case ( #Ok(v0), #Ok(v1) ) -> ok v0 v1
        case ( #Error(e0), #Error(e1) ) -> error e0 e1
        case (_, _) -> false

    def compare 'a 'e (ok: a -> a -> i64) (error: e -> e -> i64) (r0: result a e) (r1: result a e): i64 =
        match (r0, r1)
        case ( #Ok(v0), #Ok(v1) ) -> ok v0 v1
        case ( #Error(e0), #Error(e1) ) -> error e0 e1
        case ( #Ok(_), #Error(_) ) -> -1
        case ( #Error(_), #Ok(_) ) -> 1

    def to_option 'a 'e (r: result a e): option a =
        match r
        case #Ok(v) -> #Some(v)
        case #Error(_) -> #None

    def to_array 'a 'e (r: result a e): []a =
        match r
        case #Ok(v) -> [v]
        case #Error(_) -> []
}