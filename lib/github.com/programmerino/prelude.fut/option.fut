type option 'a = #Some a | #None

module Option = {
    -- | map f inp evaluates to match input case #Some a -> (c a) | #None -> #None
    -- c is the function to apply to the option value
    -- x is the input option
    -- The return value is an option of the input value after applying the mapping
    -- or None if the input is None.
    def map 'a 'b (c: a -> b) (x: option a): option b = match x
        case #Some a -> #Some (c a)
        case #None -> #None

    -- | Extracts the value out of an option with an assertion that it is not None.
    -- Example is an example of the type which is necessary for the implementation
    def unwrap 'a (example: a) (x: option a): a = match x
        case #Some a -> a
        case #None -> assert false example

    -- | Returns true if the option is not None
    -- option is the input option
    -- returns true if the option is not None
    def isSome 'a (x: option a): bool = match x
        case #Some _ -> true
        case #None -> false

    -- | Returns true if the option is None
    -- option is the input option
    -- returns true if the option is None
    def isNone 'a (x: option a): bool = match x
        case #Some _ -> false
        case #None -> true

    -- | Gets the value if the option is Some, otherwise returns a specified default
    -- value.
    -- default is the specified default value
    -- x is the input option
    -- returns the option if the option is Some, else the default value
    def defaultValue 'a (default: a) (x: option a): a = match x
        case #Some a -> a
        case #None -> default
}