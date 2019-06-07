makeFourierSeries[{"Closed" | "Open", {{cax_, sax_}, {cay_, say_}}}, t_, n_] := {
  Sum[If[k == 0, 1/2, 1] cax[[k + 1]] Cos[k t] + sax[[k + 1]] Sin[k t], {k, 0, Min[n, Length[cax]]}],
  Sum[If[k == 0, 1/2, 1] cay[[k + 1]] Cos[k t] + say[[k + 1]] Sin[k t], {k, 0, Min[n, Length[cay]]}]
}
