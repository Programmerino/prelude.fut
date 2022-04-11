module StringUtils = {
    let concat (sep: []u8) (str: [][]u8) =
        let (accum, _) = loop (accum, first) = ("", true) for x in str do
                            if first then
                                (x, false)
                            else
                                (accum ++ sep ++ x, false)
        in
        accum

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

    let indices_to_values [n][m] 'a (zero: a) (x: [n]a) (is: [m]i64): [m]a =
        loop acc = (replicate m zero) for i < m do
            acc with [i] = x[is[i]]

    let windowed [n] 'a (w: i64) (xs: [n]a) : [][w]a =
        let w' = assert (w > 0) w
        in
        if w' > n then
            []
        else
            let starts = iota (n - w' + 1)
            in
            starts |> map(\x -> xs[x:x + w'] :> [w]a)
}

