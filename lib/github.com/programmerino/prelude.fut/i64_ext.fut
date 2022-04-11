import "string"

module i64_ext = {
    -- | Computes floor(log_10(n)) without relying on a floating point conversion.
    -- This is preferable with large integers and might be slightly faster than
    -- using the floating point conversion.
    def floor_log_10 (n: i64): i64 =
       let (_, accum) = loop (n, accum) = (n, 0) while n > 9 do
                           (n / 10i64, accum + 1)
       in
       accum

    -- | Gets the number of digits in a number (0 counts as 1 digit)
    def digits_in (n: i64): i64 =
        1 + floor_log_10 n
}