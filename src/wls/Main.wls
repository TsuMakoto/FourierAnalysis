Import["PointListToLines.wls"]
Import["Fouriercomponents.wls"];
Import["MakeFourierSeries.wls"];
(*
  EdgeDetectでエッジを抽出
  エッジの位置を取得
  EdgeDetectでエッジの画像データを作成
*)
edgePosition[path_] := {#2, #1} & @@@ Position[ImageData[EdgeDetect[Import[path]]], 1, {2}]

Main[path_] := Module[
  {
    edgePos = edgePosition[path],
    hlines,
    fCs,
    curves
  },
  hlines = pointListToLines[edgePos];
  (* フーリエ級数変換 *)
  fCs = fourierComponents[hlines];
  (* 数式化 *)
  curves = makeFourierSeries[#, t, 200] & /@ fCs;
  formula = Style[Rationalize[curves, 0.002], 10];
  formula
]
