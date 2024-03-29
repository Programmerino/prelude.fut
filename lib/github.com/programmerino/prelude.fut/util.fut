-- | ignore

module StringUtils = {
    let concat [n][m] (sep: [n]u8) (str: [][m]u8) =
        let each_size = n + m
        in
        str[0] ++ flatten (map (\x -> #[unsafe] (sep ++ x) :> [each_size]u8) (tail str))

    let string_of_int (i: i64) =
        if i == 0 then "0" else
            let s = []
            let (sign, i) = if i < 0 then (['-'], -i) else ([], i)
            let (_, s) = loop (i, s) while i != 0 do (i / 10, [48 + (i % 10)] ++ s)
            in
            (sign ++ s) |> map (u8.i64)
}

module ArrayUtils = {
    let to_string strFn x =
        x |> map(strFn) |> StringUtils.concat ", "

    let indices_to_values [m] x (is: [m]i64) = is |> map(\i -> x[i])

    let windowed [n] 'a w (xs: [n]a) =
        let w' = assert (w > 0) w
        in
        if w' > n then
            []
        else
            let starts = iota (n - w' + 1)
            in
            starts |> map(\x -> xs[x:x + w'] :> [w]a)

    def blit [n][m] 'a (src: [n]a) src_pos (dst: *[m]a) dst_pos len =
        let src_pos = assert (src_pos >= 0) src_pos
        let dst_pos = assert (dst_pos >= 0) dst_pos
        let len = assert (len >= 0) len
        let len = assert(len <= n - src_pos) len
        let len = assert(len <= m - dst_pos) len
        in
        scatter dst (iota len |> map(\x -> x + dst_pos)) (src[src_pos:src_pos + len] :> [len]a)
}