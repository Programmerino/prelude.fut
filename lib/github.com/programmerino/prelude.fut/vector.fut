import "option"
import "array"
import "tuple"
import "i64_ext"

local def CREATE_CONST = 6i64

type vector [n] 't = ([n]t, i64)

module type Vector = {
    -- | Return the number of elements in the vector
    val length [n] 't: vector [n] t -> i64

    -- | Is the vector empty?
    val null [n] 't: vector [n] t -> bool

    -- | `get xs idx` gets the element in `xs` at index `idx`. If `xs` has
    -- `len` elements in it, then the valid indexes range from `0` to `len-1`.
    val get [n] 't: vector [n] t -> i64 -> t
    
    -- | `set xs idx x` sets the element in `xs` at index `idx` to `x`. If `xs`
    -- has `len` elements in it, then the valid indexes range from `0` to `len-1`.
    val set [n] 't: *vector [n] t -> i64 -> t -> vector [n] t

    -- | Convert vector to array
    val to_array [n] 't: vector [n] t -> []t

    -- | Convert array to vector
    val of_array [n] 't: [n]t -> vector [n] t

    -- | Empty vector
    val empty 't: vector [0] t

    -- | Push an element to the back of the vector
    val push_back 't [n] : *vector [n] t -> t -> vector [] t

    -- | Concatenate an array with a vector and return a vector
    val concat_array 't [n][m] : *vector [n] t -> [m]t -> vector [] t

    -- | Concatenation with another vector
    val concat 't [n][m] : *vector [n] t -> vector [m] t -> vector [] t
}

module Vector: Vector = {
    def length = snd

    def null = length >-> ((==) 0i64)

    def get 't ((data, size): vector [] t) i =
        assert (i >= 0 && i < size) (#[unsafe] data[i])

    def set 't ((data, size): *vector [] t) i x = assert (i >= 0 && i < size) (data with [i] = x, size)

    def to_array 't ((data, size): vector [] t) = #[unsafe] data[0:size]

    def of_array [n] 't (data: [n]t): vector [n] t =
        (data, n)

    def empty = of_array []

    local def maybe_resize 't [n] zero ((data, size): vector [n] t) m =
        if n == 0 then
            (replicate m zero, m)
        else if (size + m) > n then
            let target = #[unsafe] (2 * n) * ((m + size) `i64_ext.cdiv` (2 * n))
            in
            (data ++ (replicate (target - n) zero), size)
        else
            (data, size)

    local def maybe_resize_one 't [n] zero ((data, size): vector [n] t) =
        if (size + 1) > n then
            (data ++ replicate (i64.max n 1) zero, size)
        else
            (data, size)

    def push_back 't [n] (v: *vector [n] t) (x: t): vector [] t =
        let new_vec = maybe_resize_one (copy x) v
        in
        set (new_vec with 1 = new_vec.1 + 1) n x

    def concat_array 't [n][m] (v: *vector [n] t) (xs: [m]t) =
        if m > 0 then
            let zero = copy xs[0]
            let (data, size) = maybe_resize zero v m
            in (#[unsafe] Array.blit_all xs 0 data n, size + m)
        else
            v

    def concat 't [n][m] (x: *vector [n] t) (y: vector [m] t) =
        concat_array x (to_array y)
}