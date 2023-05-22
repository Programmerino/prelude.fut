-- | A data structure and functions for achieving hashtables in Futhark.
-- A couple pointers -- equality probably doesn't the work the way you think it does since deletions are queued until the next resize, so two hashtables containing the same keys may not be considered equal
import "option"

-- | A performant hashset using open-address hashing
type hashset [a] 't = { slots: [a](bool, bool, t), acc: i64, length: i64, load: i64 }

local def checkHash1 f size acc x =
    let res = f size acc x
    in assert (res >= 0) (assert (res < i64.abs size) res)

local def checkHash2 f size acc x =
    let res = f size acc x
    in assert (res >= 1) (assert (res < i64.abs size) res)

module type HashSet = {
    type t
    -- | Returns an array of unsorted keys stored in the hashset
    val get_keys [n]: hashset [n] t -> *[]t
    -- | Returns the number of active keys in the hashset
    val length [n]: hashset [n] t -> i64
    -- Adds a key to the hashset
    val add [n]: t -> *hashset [n] t -> *hashset [] t
    -- | Adds multiple keys to the hashset
    val multiadd [n][o][p]: [n]t -> *hashset [o] t -> *hashset[]t
    -- | Returns true if the key is present, false otherwise
    val exists [n]: t -> hashset [n] t -> bool
    -- | Deletes a key from the hashset (marked invalid until next resize, then truly deleted)
    val delete [n]: t -> *hashset [n] t -> *hashset [n] t
}

-- | Parameters for the hashset and the necessary information about a type
-- to implement a hashset for it
module type Type = {
    type t
    -- | An example of the type
    val exm: t
    -- | == for the type
    val eq: t -> t -> bool
    -- | What % of the size of the hashtable can be filled before resizing
    -- Note, this is a time-space tradeoff. The default hashsets use 0.25
    val LOAD_FACTOR: f64
    -- | See https://en.wikipedia.org/wiki/Double_hashing
    val hash1: i64 -> i64 -> t -> i64 -- size (2^p), p, key
    -- | See https://en.wikipedia.org/wiki/Double_hashing
    val hash2: i64 -> i64 -> t -> i64
    -- | A function which can generate the hashset
    val make: i64 -> *hashset [] t
    -- | Default size of empty hashset
    val emptySize: i64
    -- | Empty hashset
    val empty: hashset [emptySize] t
    -- | Given current size, current load, load increase, and an i64
    -- accumulator (can store any additional information), return
    -- the new size and a new accumulator value.
    val resize: i64 -> i64 -> i64 -> i64 -> (i64, i64) -- current size, current load, load increase, acc. Return new size and new acc
}

module HashSet (T: Type): HashSet = {
    local open T
    type t = t

    local def hash1 = checkHash1 hash1
    local def hash2 = checkHash2 hash2

    local type find_result = #Occ(i64) | #Empty(i64)

    local def find_slot [a] (hs: hashset [a] t) v =
        let v1 = hash1 a hs.acc v
        let v2 = hash2 a hs.acc v
        let (res, _) = loop (res, x) = (#None : option find_result, v1) while (res == #None) do
            let i = x %% a
            let (occ, prevocc, v_) = hs.slots[i]
            in
            if (not occ) && (not prevocc) then (#Some(#Empty(i)), i)
            else if occ && (v_ `eq` v) && (not prevocc) then (#Some(#Occ(i)), i)
            else (#None, x + v2)
        in Option.unwrap (#Empty(0i64)) res

    local def add_nocheck [a] v ({slots, acc, length, load}: *hashset [a] t) =
        let sloti = find_slot {slots, acc, length, load} v
        let (sloti, length, load) =
            match sloti
            case #Occ(i) -> (i, length, load)
            case #Empty(i) -> (i, length + 1, load + 1)
        in
        {slots = slots with [sloti] = (true, false, v), load, length, acc}

    local def multiadd_nocheck [a] vs ({slots, acc, length, load}: *hashset [a] t) =
        let (slots, length, load) = loop (slots, length, load) for v in vs do
            let {slots, load, length, acc = _} = add_nocheck v {slots, acc, length, load}
            in
            (slots, length, load)
        in  {slots, acc, length, load}

    
    local def get_keys_slots [a] (slots: [a](bool, bool, t)) = map (\x -> x.2) (filter (\(occ, prevocc, _) -> occ && (not prevocc)) slots)

    
    def get_keys [a] (hs: hashset [a] t) = get_keys_slots hs.slots

    
    def length (hs: hashset [] t) = hs.length

    local def maybe_resize [a] ({slots, acc, length, load}: *hashset [a] t) n =
        let af64 = f64.i64 a
        let new_load = f64.i64 (load + n)
        let (slots, acc, length, load) = if LOAD_FACTOR * af64 < new_load then
            let (new_size, acc) = resize a load n acc
            let empty_slots = replicate new_size (false, false, slots[0].2)
            let old_keys = get_keys_slots slots
            let {slots, acc, length, load} = multiadd_nocheck old_keys {slots = empty_slots, acc, length = 0, load = 0}
            in (slots, acc, length, load)
        else
            (slots, acc, length, load)
        in {slots, acc, length, load}
        
    def add [a] v (hs: *hashset [a] t): *hashset [] t =
        let hs = maybe_resize hs 1
        in add_nocheck v hs

    def multiadd [a] [n] (vs: [n]t) (hs: *hashset [a] t) =
        let hs = maybe_resize hs n
        in multiadd_nocheck vs hs
    
    def exists [n] v (hs: hashset [n] t) =
        match find_slot hs v
        case #Occ(i) -> hs.slots[i].0
        case #Empty(_) -> false
        
    
    def delete [n] v ({slots, acc, length, load}: *hashset [n] t) =
        let (slots, length) = match find_slot {slots, acc, length, load} v
        case #Occ(i) -> (slots with [i] = (true, true, v), length - 1)
        case #Empty(_) -> (slots, length)
        in
        {slots, acc, length, load}
}

local def LOAD_FACTOR = 0.25f64

-- | Sample implementation of the hashset using linear probing for i64s
module LPSeti64 = HashSet {
    type t = i64
    def exm = 0i64
    def eq = \(x: i64) y -> x == y
    local def z = 7u64
    local def bits = 64u64
    def hash1 _ d x = i64.u64((u64.i64(x) * z) >> (bits - (u64.i64 d)))
    def hash2 _ _ _ = 1i64
    local def make_pow pow: *hashset [] t =
        let len = 2**pow
        in {
            slots = replicate len (false, false, exm), load = 0, acc = pow, length = 0
        }

    def make n: *hashset [] t =
        let pow = i64.f64 (f64.log2 (f64.i64 n))
        in
        make_pow pow

    def emptySize = 8i64
    def empty: hashset [emptySize] t = make 8 :> hashset [emptySize] t

    def LOAD_FACTOR = LOAD_FACTOR

    def resize a load n (pow: i64) =
        let new_load = f64.i64(load + n)
        let min = i64.f64((new_load / LOAD_FACTOR))
        let (new_size, acc) = iterate_until (\(x,_) -> x >= min) (\(x, p) -> (x * 2, p + 1)) (a, pow)
        let new_size = assert (LOAD_FACTOR * (f64.i64 new_size) >= new_load) new_size
        in (new_size, acc)
}

-- | Default hashset is using the linear probing above
module Seti64 = LPSeti64