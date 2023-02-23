import "option"
import "array"
import "i64_ext"

local def CREATE_CONST = 6i64

type~ vector [n] 't = {
    data: []t,
    size_: [0][n]t
}

module Vector = {
    -- | Is the array empty?
    def null [n] 't (_: vector [n] t): bool =
        n == 0

    -- | `get xs idx` gets the element in `xs` at index `idx`. If `xs` has
    -- `len` elements in it, then the valid indexes range from `0` to `len-1`.
    def get [n] 't (xs: vector [n] t) i =
        assert (i >= 0 && i < n) (#[unsafe] xs.data[i])

    def set [n] 't ({data, size_}: *vector [n] t) i x = assert (i >= 0 && i < n) {data = data with [i] = x, size_}

    -- | The first element of the array
    def head [n] 't (xs: vector [n] t) =
        assert (n > 0) (#[unsafe] head xs.data)

    -- | Convert vector to array
    def to_array [n] 't (xs: vector [n] t) =
        #[unsafe] xs.data[0:n]

    -- | Convert vector to array
    def of_array [n] 't (data: [n]t): vector [n] t =
        {data, size_ = []}

    def empty = of_array []

    -- | Applies a function meant to operate on arrays to a vector. The return value is not converted to an array.
    def array_op f xs =
        f (to_array xs)

    -- | Applies a function meant to operate on arrays to a vector. The return value is converted to a vector.
    def array_op_vec f xs =
        of_array (array_op f xs)

    -- | Everything but the first element of the vector (returns array).
    def atail [n] 't (xs: vector [n] t) =
        assert (n > 0) (#[unsafe] tail (to_array xs))

    -- | Everything but the first element of the vector.
    def tail xs =
        xs |> atail |> of_array

    -- | Everything but the last element of the vector (returns array).
    def ainit [n] 't (xs: vector [n] t) =
        assert (n > 0) (#[unsafe] init (to_array xs))

    -- | Everything but the last element of the vector.
    def init xs =
        xs |> ainit |> of_array

    -- | Take some number of elements from the head of the array (returns array).
    def atake i xs =
        take i (to_array xs)

    -- | Take some number of elements from the head of the array.
    def take 't i xs: vector [i] t =
        xs |> atake i |> of_array

    -- | Remove some number of elements from the head of the array (returns array).
    def adrop [n] 't i (xs: vector [n] t) =
        assert (n >= i) (#[unsafe] drop i (to_array xs))

    -- | Remove some number of elements from the head of the array.
    def drop i xs =
        xs |> adrop i |> of_array

    -- | Split an array at a given position (returns array).
    def asplit [n] 't i (xs: vector [n] t) =
        assert (n >= i) (#[unsafe] split i (to_array xs))

    -- | Split an array at a given position.
    def split i xs =
        let (fst, snd) = xs |> asplit i
        in (of_array fst, of_array snd)

    -- | Return the elements of the array in reverse order (returns array).
    def areverse xs =
        reverse (to_array xs)

    -- | Return the elements of the array in reverse order.
    def reverse [n] 't (xs: vector [n] t): vector [n] t =
        xs |> areverse |> of_array

    -- | Concatenate two vectors and return an array
    def aconcat xs ys =
        (to_array xs) ++ (to_array ys)

    def maybe_resize 't [size] zero (xs: vector [size] t) m =
        let n = length xs.data
        in
        if n == 0 then
            replicate m zero
        else if (size + m) > n then
            let target = #[unsafe] (2 * n) * ((m + size) `i64_ext.cdiv` (2 * n))
            in
            xs.data ++ (replicate (target - n) zero)
        else
            xs.data

    def maybe_resize_one 't [size] zero (xs: vector [size] t) =
        let n = length xs.data
        in
        if (size + 1) > n then
            xs.data ++ replicate (i64.max n 1) zero
        else
            xs.data

    -- let push_back 't (v: *vector [] t): *vector[]t = v
    let push_back 't [size] (v: vector [size] t) (x: t): vector[size]t = copy v

    -- | Concatenate an array with a vector and return a vector
    def concat_array 't [size][m] (v: *vector [size] t) (xs: [m]t) =
        if m > 0 then
            let zero = copy xs[0]
            let dst = maybe_resize zero v m
            let nsize = size + m
            in {data = #[unsafe] Array.blit_all xs 0 dst size, size_ = ([] : [][nsize]t)}
        else
            v

    -- | Concatenation with an array where the result has a predetermined size
    def concat_array_to 't [size] (k: i64) ({data, size_}: *vector [size] t) xs =
        concat_array {data, size_} xs :> vector [k] t

    -- | Concatenation with another vector
    def concat 't [n][m] ({data, size_}: *vector [n] t) (ys: vector [m] t) =
        concat_array {data, size_} (ys.data[0:m])

    -- | Concatenation with another vector where the result has a predetermined size. If the provided size is wrong, the function will fail with a run-time error.
    def concat_to 't [n] (k: i64) ({data, size_}: *vector [n] t) ys =
        concat {data, size_} ys :> vector[k]t

    -- | Return the number of elements in the vector
    def length [n] 't (_: vector [n] t) =
        n

    -- | Rotate an array some number of elements to the left. A negative rotation amount is also supported. For example, if b==rotate r a, then b[x] = a[x+r] (returns array).
    def arotate r xs =
        rotate r (to_array xs)

    -- | Rotate an array some number of elements to the left. A negative rotation amount is also supported. For example, if b==rotate r a, then b[x] = a[x+r].
    def rotate [n] 't r (xs: vector [n] t): vector [n] t =
        xs |> arotate r |> of_array

    -- | Construct an array of consecutive integers of the given length, starting at 0.
    def iota n: vector [n] i64 =
        of_array (iota n)

    -- | Construct an array comprising valid indexes into some other array, starting at 0 (returns array).
    def aindices xs =
        indices (to_array xs)

    -- | Construct an array comprising valid indexes into some other array, starting at 0.
    def indices [n] 't (xs: vector [n] t): vector [n] i64 =
        xs |> aindices |> of_array

    -- | Construct an array of the given length containing the given value.
    def replicate 't (n: i64) (x: t): vector [n] t =
        of_array (replicate n x)

    -- | True if all of the input elements are true. Produces true on an empty array.
    def and xs =
        and (to_array xs)

    -- | True if any of the input elements are true. Produces false on an empty array.
    def or xs =
        or (to_array xs)

    -- | Perform a sequential left-fold of an array.
    def foldl f acc bs =
        foldl f acc (to_array bs)

    -- | Perform a sequential right-fold of an array.
    def foldr f acc bs =
        foldr f acc (to_array bs)

    -- | Create a value for each point in a one-dimensional index space.
    def tabulate n f =
        of_array (tabulate n f)

    -- | Apply the given function to each element of an array (returns array).
    def amap f as =
        map f (to_array as)

    -- | Apply the given function to each element of an array.
    def map f as =
        as |> amap f |> of_array

}