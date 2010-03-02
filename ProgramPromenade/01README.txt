[Haskell]
$ runhugs quine.hs | diff quine.hs -

[Perl]
$ perl quine.pl | diff quine.pl -

[Python]
$ python quine.py | diff quine.py -

[Scheme]
scheme 処理系を起動し quine.l の式を対話的に入力・評価

[OCaml]
$ ocaml quine.ml | diff quine.ml -

[C]
$ gcc -w quine.c; ./a.out | diff quine.c -

[PostScript]
$ gs -q -dBATCH quine.ps | diff quine.ps -
