def HTILE = 256i64
def VTILE = 32i64

def matmult 't [p][r][q] (exm: t) ((+.): t -> t -> t) ((*.): t -> t -> t) (A: [p][r]t) (B: [r][q]t): [p][q]t =
    let A = assert (q % HTILE == 0) (flatten A)
    let B = assert (p % VTILE == 0) (flatten B)
    let clen = p * q
    in
    let C =
        loop C = replicate clen exm for i in (0..VTILE..<p) do
            loop C for j in (0..HTILE..<q) do
                let tile = 
                    loop tile = replicate VTILE (replicate HTILE exm) for k < r do
                        let Aik = #[incremental_flattening(no_intra)] map (\tv -> A[(i + tv) * r + k]) (iota VTILE)
                        let tile =
                            loop tile for tv < VTILE do
                                #[unroll] loop tile for th < HTILE do
                                    let prev = copy tile[tv, th]
                                    in
                                    tile with [tv, th] = prev +. Aik[tv] *. B[k*q + j + th]
                        in tile
                let C =
                      loop C for tv < VTILE do
                        let (is, vs) = iota HTILE |> map(\th -> ((i+tv)*q + j + th, tile[tv, th])) |> unzip
                        in
                        scatter C is vs
                in C
    in (unflatten p q C) :> [p][q]t