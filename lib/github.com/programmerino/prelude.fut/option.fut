-- | The Option monad and helper functions.

type option 'a = #Some a | #None

module type Option = {
    -- | map f inp evaluates to match input case #Some a -> (c a) | #None -> #None
    -- c is the function to apply to the option value
    -- x is the input option
    -- The return value is an option of the input value after applying the mapping
    -- or None if the input is None.
    val map 'a 'b: (a -> b) -> option a -> option b

    -- | Like map, but it flattens the result of the function call rather than
    -- returning nested options
    val flatmap 'a 'b: (a -> option b) -> option a -> option b

    -- | Extracts the value out of an option with an assertion that it is not None.
    -- Example is an example of the type which is necessary for the implementation
    val unwrap 'a: a -> option a -> a

    -- | Returns true if the option is not None
    -- option is the input option
    -- returns true if the option is not None
    val isSome 'a: option a -> bool

    -- | Returns true if the option is None
    -- option is the input option
    -- returns true if the option is None
    val isNone 'a: option a -> bool

    -- | Gets the value if the option is Some, otherwise returns a specified default
    -- value.
    -- default is the specified default value
    -- x is the input option
    -- returns the option if the option is Some, else the default value
    val defaultValue 'a: a -> option a -> a
}

module Option: Option = {
    def map 'a 'b c (x: option a): option b = match x
        case #Some a -> #Some (c a)
        case #None -> #None

    def flatmap 'a 'b c (x: option a): option b = match x
        case #Some a -> c a
        case #None -> #None

    def unwrap 'a example (x: option a) = match x
        case #Some a -> a
        case #None -> assert false example

    def isSome 'a (x: option a) = match x
        case #Some _ -> true
        case #None -> false

    def isNone 'a (x: option a) = match x
        case #Some _ -> false
        case #None -> true

    def defaultValue 'a default (x: option a) = match x
        case #Some a -> a
        case #None -> default

}