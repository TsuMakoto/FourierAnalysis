(* Fourier coefficients of a single curve *)
fourierComponentData[pointList_, nMax_, op_] := Module[
  {
    epsilon = 10^-3, myu = 2^14, M = 10000,
    s, scale, delta, L,
    nds, sMax, if, xyFunction, X, Y, XFT, YFT, type
  },
  (* スケールを測定 *)
  scale = 1. Mean[
    Table[
      Max[fl /@ pointList] - Min[fl /@ pointList], {fl, {First, Last}}
    ]
  ];

  (* 1ラインのユークリッド弧長を表示 *)
  delta = EuclideanDistance[First[pointList], Last[pointList]];
  L = Which[
    op === "Closed",
    type = "Closed";
    If[First[pointList] === Last[pointList],
      pointList,
      Append[pointList, First[pointList]]
    ],

    op === "Open",
    type = "Open";
    pointList,

    delta == 0.,
    type = "Closed";
    pointList,

    delta/scale < op,
    type = "Closed";
    Append[pointList, First[pointList]],

    True,
    type = "Open";
    Join[pointList, Rest[Reverse[pointList]]]
  ];
  (* B-スプライン曲線 *)
  xyFunction = BSplineFunction[L, SplineDegree -> 4];
  nds = NDSolve[
    {s'[t] == Sqrt[xyFunction'[t].xyFunction'[t]], s[0] == 0},
    s,
    {t, 0, 1},
    MaxSteps -> 10^5,
    PrecisionGoal -> 4
  ];
  (* 曲線の長さ *)
  sMax = s[1] /. nds[[1]];
  if = Interpolation[
    Table[
      {s[rho] /. nds[[1]], rho},
      {rho, 0, 1, 1/M}
    ]
  ];
  X[t_Real] := BSplineFunction[L][Max[Min[1, if[(t + Pi)/(2 Pi) sMax]], 0]][[1]];
  Y[t_Real] := BSplineFunction[L][Max[Min[1, if[(t + Pi)/(2 Pi) sMax]], 0]][[2]];
  (* extract Fourier coefficients *)
  {XFT, YFT} = Fourier[
    Table[
      #[N@t],
      {t, -Pi + epsilon, Pi - epsilon, (2 Pi - 2 epsilon)/myu}
    ]
  ] & /@ {X, Y};
  {type, 2 Pi/
    Sqrt[myu]*(Transpose[
      Table[
        {Re[#], Im[#]} &[Exp[I k Pi] #[[k + 1]]],
        {k, 0, nMax}
      ]
    ] & /@ {XFT, YFT})
  }
]

Options[fourierComponents] = {"MaxOrder" -> 200, "OpenClose" -> 0.025};

fourierComponents[pointlists_, OptionsPattern[]] := Table[
  fourierComponentData[
    hlines[[k]],
    If[Head[#] === List, #[[k]], #] &[OptionValue["MaxOrder"]],
    If[Head[#] === List, #[[k]], #] &[OptionValue["OpenClose"]]
  ],
  {k, 1}
]
