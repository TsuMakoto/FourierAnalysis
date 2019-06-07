(*
  エッジ点からランダムカーブを取得
*)
pointListToLines[pointList_, neighbothoodSize_: 6] := Module[
  {
    (* 重複を削除したものをLへ代入 *)
    L = DeleteDuplicates[pointList],
    NF,
    lambda,
    lineBag,
    counter,
    seenQ,
    sLB,
    nearest,
    nearest1,
    nextPoint,
    couldReverseQ,
    d, n, s
  },
  (* Lに対するnearist関数を定義 *)
  NF = Nearest[L];
  (* 重複無しの長さを取得 *)
  lambda = Length[L];
  Monitor[
    (* データをここに格納 *)
    lineBag = {};
    counter = 0;
    While[counter < lambda,
      (* 未選択のランダムな一点を選択 *)
      sLB = {RandomChoice[DeleteCases[L, _?seenQ]]};
      (* 選択された部分はフラグを立てる *)
      seenQ[sLB[[1]]] = True;
      counter++;
      couldReverseQ = True;
      (* データの生成 *)
      While[
        (
          (* neighbothoodSizeを中心とする点群の集合を作成 *)
          nearest = NF[Last[sLB], {Infinity, neighbothoodSize}];
          (* ユークリッド距離でソート *)
          nearest1 = SortBy[DeleteCases[nearest, _?seenQ], 1. EuclideanDistance[Last[sLB], #] &];
          nearest1 =!= {} || couldReverseQ
        ),
        If[nearest1 === {},
          (* 全ての点を網羅したとき終了 *)
          sLB = Reverse[sLB];
          couldReverseQ = False,
          (* まだ残っているのであれば、次点を作成 *)
          nextPoint = If[Length[sLB] <= 3,
              (* sLBの中身が3以下であれば、一番中心と近い点を抽出 *)
              nearest1[[1]],
              (* 3つ以上あるならば、急カーブとならないよう抽出点を調整 *)
              d = 1. Normalize[(sLB[[-1]] - sLB[[-2]]) + 1/2 (sLB[[-2]] - sLB[[-3]])];
              n = {-1, 1} Reverse[d];
              s = Sort[{Sqrt[(d.(# - sLB[[-1]]))^2 + 2 (n. (# - sLB[[-1]]))^2], #} & /@ nearest1];
              s[[1, 2]]
          ];
          AppendTo[sLB, nextPoint];
          seenQ[nextPoint] = True;
          counter++
        ]
      ];
      AppendTo[lineBag, sLB]
    ];
    (* 線の長さでソートしたものを返す *)
    Reverse[SortBy[Select[lineBag, Length[#] > 12 &], Length]],
  ]
]
