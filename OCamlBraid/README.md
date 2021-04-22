Lib.ml is identical to the one present in the Verificatum subfolder, except for prime values p, q.

Main.ml parses all transcript data from json files in the data subfolder. To generate sample braid data run

    cargo test gen_coq_data

from the braid root. Then copy the generated files, for example

    cp *.json ../secure-e-voting-with-coq/OCamlBraid/data