-- | ignore
import "result"
import "string"

-- `ok v` is `#Ok v`
-- val ok 'a 'e: a -> result a e
-- ==
-- entry: test_ok
-- input { 5 } output { true }
entry test_ok (x: i32) =
    (Result.ok x: result i32 ()) == #Ok(5)

-- `error e` is `#Error e`
-- val error 'a 'e: e -> result a e
-- ==
-- entry: test_error
-- input { "error message" } output { true }
entry test_error (msg: string []) =
    (Result.error msg: result () (string[])) == #Error("error message")

-- `value r default` is `v` if `r` is `#Ok v` and `default` otherwise.
-- val value 'a 'e: result a e -> a -> a
-- ==
-- entry: test_value
-- input { 5 } output { 5 }
entry test_value (x: i32) =
    Result.value (Result.ok x) 0

-- `bind r f` is `f v` if `r` is `#Ok v` and `r` if `r` is `#Error_`.
-- val bind 'a 'b 'e: result a e -> (a -> result b e) -> result b e
-- ==
-- entry: test_bind
-- input { 5 } output { true }
entry test_bind (x: i32) =
    Result.bind (Result.ok x) (\x -> Result.ok x: result i32 ()) == #Ok(5)

-- `join rr` is `r` if `rr` is `#Ok r` and `rr` if `rr` is `#Error _`.
-- val join 'a 'b 'e: result (result a e) e -> result a e
-- ==
-- entry: test_join
-- input { 5 } output { true }
entry test_join (x: i32) =
    Result.join (Result.ok (Result.ok x) : result (result i32 ()) ()) == #Ok(5)

-- `map f r` s `#Ok (f v)` if `r` is `#Ok v` and `r` if `r` is `#Error _`.
-- val map 'a 'b 'e: (a -> b) -> result a e -> result b e
-- ==
-- entry: test_map
-- input { 5 } output { true }
entry test_map (x: i32) =
    Result.map (\x -> x + 1) (Result.ok x: result i32 ()) == #Ok(6)

-- `map_error f r` is `#Error (f e)` if `r` is `#Error e` and `r` if `r` is `#Ok _`.
-- val map_error 'e 'f 'a: (e -> f) -> result a e -> result a f
-- ==
-- entry: test_map_error
-- input { "error message" } output { true }
entry test_map_error (msg: string []) =
    Result.map_error id (Result.error msg: result () (string[])) == #Error(msg)

-- `fold ok error r` is `ok v` if `r` is `#Ok v` and `error e` if `r` is `#Error e`.
-- val fold 'a 'c 'e: (a -> c) -> (e -> c) -> result a e -> c
-- ==
-- entry: test_fold
-- input { 5 } output { 6 }
entry test_fold (x: i32) =
    Result.fold (\x -> x + 1) (\x -> x + 1) (Result.ok x)

-- `iter f r` is `f v` if `r` is `#Ok v` and `()` otherwise.
-- val iter 'a 'e: (a -> ()) -> result a e -> ()
-- ==
-- entry: test_iter
-- input { 5 } output { true }
entry test_iter (x: i32) =
    Result.iter (\_ -> ()) (Result.ok x) == ()

-- `iter_error f r` is `f e` if `r` is `#Error e` and `()` otherwise.
-- val iter_error 'e 'a: (e -> ()) -> result a e -> ()
-- ==
-- entry: test_iter_error
-- input { "error message" } output { true }
entry test_iter_error (msg: string []) =
    Result.iter_error (\_ -> ()) (Result.error msg) == ()

-- `is_ok r` is `true` if and only if `r` is `#Ok _`.
-- val is_ok 'a 'e: result a e -> bool
-- ==
-- entry: test_is_ok
-- input { 5 } output { true }
entry test_is_ok (x: i32) =
    Result.is_ok (Result.ok x)

-- `is_error r` is `true` if and only if `r` is `#Error _`.
-- val is_error 'a 'e: result a e -> bool
-- ==
-- entry: test_is_error
-- input { "error message" } output { true }
entry test_is_error (msg: string []) =
    Result.is_error (Result.error msg)

-- `equal ok error r0 r1` tests equality of `r0` and `r1` using `ok` and `error` to respectively compare values wrapped by `#Ok _` and `#Error _`.
-- val equal 'a 'e: (a -> a -> bool) -> (e -> e -> bool) -> result a e -> result a e -> bool
-- ==
-- entry: test_equal
-- input { 5 } output { true }
entry test_equal (x: i32) =
    Result.equal (==) (==) (Result.ok x) (Result.ok x: result i32 ())

-- `compare ok error r0 r1` totally orders `r0` and `r1` using `ok` and `error` to respectively compare values wrapped by `#Ok _` and `#Error _`.
-- `#Ok _` values are smaller than `#Error _` values.
-- val compare 'a 'e: (a -> a -> i64) -> (e -> e -> i64) -> result a e -> result a e -> i64
-- ==
-- entry: test_compare
-- input { 5i64 } output { 10i64 }
entry test_compare (x: i64) =
    Result.compare (+) (+) (Result.ok x) (Result.ok x)

-- `to_option r` is `r` as an option, mapping `#Ok v` to `#Some v` and `#Error _` to `#None`.
-- val to_option 'a 'e: result a e -> option a
-- ==
-- entry: test_to_option
-- input { 5 } output { true }
entry test_to_option (x: i32) =
    Result.to_option (Result.ok x) == #Some(5)

-- `to_array r` is `[v]` if `r` is `#Ok v` and `[]` otherwise.
-- to_array 'a 'e: result a e -> []a
-- ==
-- entry: test_to_array
-- input { 5 } output { [5] }
entry test_to_array (x: i32) =
    Result.to_array (Result.ok x)