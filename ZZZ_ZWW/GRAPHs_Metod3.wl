(* ::Package:: *)

 


(* ::Section:: *)
(*Setup*)


SetDirectory[NotebookDirectory[]];
<< "../modules/setup.m"
SetDirectory[NotebookDirectory[]];
Print[Directory[]]

SavePath = "subgraphs/";
SavePDF  = 1;
dir     = "buffer";
pattern = "f4mean_*.csv";


(*constraints on f4 ZZZ*)

constraitF4Z300=1.3/10^4;
constraitF4Z3000=3.7/10^5;
constraitF4ZWW=2/10^4;


(* ::Section:: *)
(*graphs design*)


Clear[base1DOpts, base2DcontOpts, shadowbox, legendLumi, base2DdensOpts];
Clear[shadowbox];


textscale = 20;
imgSize = 600;
fontFam = "Helvetica";
fontWeight = "Plain";
frameThick = 2.2;
lineThick = 2.6;

styleLim300  = Directive[Red,  AbsoluteThickness[2.2]];
styleLim3000 = Directive[Blue, AbsoluteThickness[2.2]];

PointNumber=50;
ContoursNumber=6;
GraphTheme=ColorData["Rainbow"];


shadowbox[legend_] := Framed[legend, Background -> None, FrameStyle -> None, FrameMargins -> 0];

SciLbl[v_, nd_: 1] := Style[
  NumberForm[
    N[v],
    {Infinity, nd},
    ExponentFunction -> (Null &)
  ],
  textscale +2, FontFamily -> fontFam, FontWeight -> fontWeight, Black
];

legendLumi = SwatchLegend[
  {Directive[Red], Directive[Blue]},
  {"300", "4000"},
  LegendLayout -> "Column",
  LegendFunction -> shadowbox,
  LabelStyle -> Directive[textscale, FontFamily -> fontFam, FontWeight -> fontWeight],
  LegendLabel -> Style[
    Row[{"\[Integral] ", Style["L", Italic], Style["d", Italic], Style["t", Italic], " [", Superscript["fb", -1],"]"}],
    textscale
  ]
];


base1DOpts := Sequence[
  Frame      -> True,
  Axes       -> False,
  Joined     -> True,
  PlotMarkers -> None,
  FrameStyle -> Directive[Black, AbsoluteThickness[frameThick]],
  FrameTicksStyle -> Directive[
    textscale + 2, FontFamily -> fontFam, FontWeight -> fontWeight
  ],
  LabelStyle -> Directive[textscale, FontFamily -> fontFam, FontWeight -> fontWeight],
  BaseStyle  -> Directive[textscale, FontFamily -> fontFam, FontWeight -> fontWeight],
  GridLines  -> None,
  PlotRangePadding -> None,
  PlotRangeClipping -> True
];


xLabM2  = Style[
  Row[{Subscript[Style["m", Italic], "2"], " [GeV]"}],
  textscale, FontFamily -> fontFam, FontWeight -> fontWeight
];
yLabF4Log = Style[Row[{Subscript["Log", 10], " |f\!\(\*SuperscriptBox[\(4\), \(\)]\)|"}], textscale];
yLabF4Lin = Style["|f4|", textscale];
yLabZZZ = Style[
  TraditionalForm @ Row[{
    HoldForm @ Abs[Subsuperscript[Style["f", Italic], 4, Style["Z", Italic]]],
    "\[Times]", Superscript[10, stepen]
  }], textscale
];
makePlotLabel[sSel_, mhcSel_] := Style[
  Row[{
    TraditionalForm @ HoldForm[Sqrt[Subscript[s, 0]]],
    " = ", sSel, " [GeV]  ",
    TraditionalForm @ HoldForm[Subscript[m, Superscript[H, "\[PlusMinus]"]]],
    " = ", mhcSel, " [GeV]"
  }],
  textscale, FontFamily -> fontFam, FontWeight -> fontWeight
];


smoothLogPairs[pairs_, n_: 5] := Module[
  {p, x, y, ly, sly},
  
  p = SortBy[pairs, First];
  x = p[[All, 1]];
  y = p[[All, 2]];
  
  (* \:0443\:0431\:0438\:0440\:0430\:0435\:043c \:043d\:0435\:0432\:0430\:043b\:0438\:0434\:043d\:044b\:0435 \:0442\:043e\:0447\:043a\:0438 \:0434\:043b\:044f \:043b\:043e\:0433\:0430\:0440\:0438\:0444\:043c\:0438\:0447\:0435\:0441\:043a\:043e\:0433\:043e \:0433\:0440\:0430\:0444\:0438\:043a\:0430 *)
  p = Select[p, NumericQ[#[[2]]] && #[[2]] > 0 &];
  x = p[[All, 1]];
  y = p[[All, 2]];
  
  ly = Log[y];
  sly = MovingAverage[ly, n];
  
  Transpose[{
    x[[Ceiling[n/2] ;; -Floor[n/2] - 1]],
    Exp[sly]
  }]
]


(* ::Section:: *)
(*Download data*)


(* Data loading *)
ds = SaveToCSVmodule`LoadF4Data[dir, pattern];
rawUU = Import["buffer/f4mean_uuZZZ.csv", "Dataset", "HeaderLines" -> 1];
rawDD = Import["buffer/f4mean_ddZZZ.csv", "Dataset", "HeaderLines" -> 1];
{First  @ rawUU, First  @ rawDD, First @ ds}


Clear[extractPairs, sumPairsLin];
(* \:0418\:0437\:0432\:043b\:0435\:0447\:044c \:043e\:0442\:0441\:043e\:0440\:0442\:0438\:0440\:043e\:0432\:0430\:043d\:043d\:044b\:0435 \:043f\:0430\:0440\:044b {mh2, yKey} \:0438\:0437 \:0434\:0430\:0442\:0430\:0441\:0435\:0442\:0430 *)
extractPairs[d_, sSel_, yKey_:"log10_f4mean"] := Module[{rows},
  rows = Normal @ d[Select[#sqrts0 == sSel &]];
  rows = Select[rows, NumericQ[#["mh2"]] && NumericQ[#[yKey]] &];
  SortBy[{#["mh2"], yNorm * 10.^#[yKey]} & /@ rows, First]
];
extractLog[raw_, sSel_] := Module[{rows},
  rows = Normal @ raw[Select[#sqrts0 == sSel &]];
  rows = Select[rows, NumericQ[#["mh2"]] && NumericQ[#["log10_f4mean"]] &];
  SortBy[{#["mh2"], #["log10_f4mean"]} & /@ rows, First]
];

(* \:0421\:0443\:043c\:043c\:0438\:0440\:043e\:0432\:0430\:0442\:044c \:0434\:0432\:0435 \:0441\:0435\:0440\:0438\:0438 {mh2, y}; yLog10=True \:043e\:0437\:043d\:0430\:0447\:0430\:0435\:0442 \:0434\:0430\:043d\:043d\:044b\:0435 \:0432 log10 *)
sumPairsLin[a_, b_] := Module[{Ha, Hb, H, xs, ys},
  Ha = AssociationThread[a[[All, 1]] -> a[[All, 2]]];
  Hb = AssociationThread[b[[All, 1]] -> b[[All, 2]]];
  H  = Merge[{Ha, Hb}, Total];
  xs = Sort @ Keys[H];
  ys = Lookup[H, xs];
  Transpose[{xs, ys}]
];
eps = 10.^-300;
sumLog[a_, b_] := Module[{Ha, Hb, H, xs, ys},
  Ha = AssociationThread[a[[All, 1]] -> 10.^a[[All, 2]]];
  Hb = AssociationThread[b[[All, 1]] -> 10.^b[[All, 2]]];
  H  = Merge[{Ha, Hb}, Total];
  xs = Sort @ Keys[H];
  ys = Log10 @ Clip[Lookup[H, xs], {eps, Infinity}];
  Transpose[{xs, ys}]
];
(* \:041e\:043f\:0440\:0435\:0434\:0435\:043b\:0438\:0442\:044c yKey \:043f\:043e \:043f\:0435\:0440\:0432\:043e\:043c\:0443 \:0434\:043e\:0441\:0442\:0443\:043f\:043d\:043e\:043c\:0443 \:0441\:0442\:043e\:043b\:0431\:0446\:0443 *)
detectYKey[dataset_] := If[
  Length[dataset] > 0 && MemberQ[Keys @ First @ Normal @ dataset, "f4mean"],
  "f4mean",
  "log10_f4mean"
];


(* ::Section:: *)
(*Graphs*)


(* ::Subsection:: *)
(*ZZZ*)


stepen = 5;
yNorm  = 10.^stepen;
toPairs[logPairs_] := {#[[1]], 10.^(stepen + #[[2]])} & /@ logPairs;
shiftPairs[pairs_] := {#[[1]], #[[2]] + stepen} & /@ pairs;


(* ::Subsubsection:: *)
(*\:0433\:0440\:0430\:0444\:0438\:043a f4 (zzz) from mh2 (fix s0, mhc)*)


sSel1   = 1000;
mhcSel1 = 400;

dsCut1  = ds[Select[#sqrts0 == sSel1 &]];
yKey1   = detectYKey[dsCut1];
useLog1 = (yKey1 === "log10_f4mean");


(* ZZZ *)
pUU1  = shiftPairs @ extractLog[rawUU, sSel1];
pDD1  = shiftPairs @  extractLog[rawDD, sSel1];
pSum1 = shiftPairs @   sumLog[
  extractLog[rawUU, sSel1],
  extractLog[rawDD, sSel1]
];
Print["UU length = ", Length[pUU1]];
Print["DD length = ", Length[pDD1]];
Print["SUM length = ", Length[pSum1]];

Print["UU y range = ", MinMax[pUU1[[All, 2]]]];
Print["DD y range = ", MinMax[pDD1[[All, 2]]]];
Print["SUM y range = ", MinMax[pSum1[[All, 2]]]];

Print["First UU = ", First[pUU1]];
Print["First DD = ", First[pDD1]];
Print["First SUM = ", First[pSum1]];
Print["Constraint 300:  ", (Log10 @ constraitF4Z300) +stepen]
Print["Constraint 3000: ", (Log10 @ constraitF4Z3000) +stepen]





xmin1 = 200;  xmax1 = 450;
ymin1 = -3.4; ymax1 = 0.5;
colorUU1 = Darker[Purple,  0.1];
colorDD1 = Darker[Orange, 0.2];

stylesLines1 = {
  Directive[colorUU1, Dashed, AbsoluteThickness[lineThick]],
  Directive[colorDD1, Dashed, AbsoluteThickness[lineThick]],
  Directive[Black,            AbsoluteThickness[lineThick + 1.0]]
};

legendLines1 = LineLegend[
  stylesLines1,
  {
    Style["ZZZ \!\(\*StyleBox[\"uu\",FontSlant->\"Italic\"]\)", textscale - 2],
    Style["ZZZ \!\(\*StyleBox[\"dd\",FontSlant->\"Italic\"]\)", textscale - 2],
    Style["ZZZ sum", textscale - 2]
  },
  LegendLayout     -> "Column",
  LegendFunction   -> shadowbox,
  LegendMarkerSize -> {32, 2},
  LabelStyle       -> Directive[textscale - 2, FontFamily -> fontFam, FontWeight -> fontWeight]
];



(* Graph ZZZ-1 | Plot *)

limEpilog1 = {
  styleLim300,
  Line[{{xmin1, Log10@((Log10 @ constraitF4Z300) +stepen)},
        {xmax1, Log10@((Log10 @ constraitF4Z300) +stepen)}}],
  styleLim3000,
  Line[{{xmin1, Log10@((Log10 @ constraitF4Z3000) +stepen)},
        {xmax1, Log10@((Log10 @ constraitF4Z3000) +stepen)}}]
};

pltZZZ1 = Show[
  ListLogPlot[pUU1,  PlotStyle -> stylesLines1[[1]], Joined -> True, PlotMarkers -> None],
  ListLogPlot[pDD1,  PlotStyle -> stylesLines1[[2]], Joined -> True, PlotMarkers -> None],
  ListLogPlot[pSum1, PlotStyle -> stylesLines1[[3]], Joined -> True, PlotMarkers -> None],
  Evaluate @ base1DOpts,
  Epilog     -> {limEpilog1, Inset[legendLumi, Scaled[{0.15, 0.2}]]},
  FrameLabel -> {xLabM2, yLabZZZ},
  PlotRange  -> {{xmin1, xmax1}, {ymin1, ymax1}},
  PlotLabel -> None,
  (*Style[Row[{TraditionalForm @ HoldForm[Sqrt[Subscript[s, 0]]]," = ", sSel1, " GeV"}],textscale, FontFamily -> fontFam],*)
  ImageSize -> imgSize
];
finalPlot1 = pltZZZ1


(* ::InheritFromParent:: *)
(**)


If[SavePDF == 1,
  Export[SavePath <> "ZZZ_uudd_sum_s" <> ToString[sSel1] <> "_mhc" <> ToString[mhcSel1] <> ".pdf",
         finalPlot1]
];


(* ::Subsubsection:: *)
(* Graph ZZZ-2|Data:ZZZ sum for s0=1000,10000,13000*)


s0List2 = {320, 1000, 10000, 13000};
mhcSel2 = 400;


xmin2 = 200;  xmax2 = 550;
ymin2 = -2.8; ymax2 = 0.3;
pSumByS0 = Association @ Table[
  s -> shiftPairs @ sumLog[extractLog[rawUU, s], extractLog[rawDD, s]],
  {s, s0List2}
];


cols2 = GraphTheme /@ Rescale[Range[Length[s0List2]], {1, Length[s0List2]}];
styles2 = Directive[#, AbsoluteThickness[lineThick]] & /@ cols2;

sLbl[v_] := Style[
  Row[{
    TraditionalForm @ HoldForm[Sqrt[Subscript[s, 0]]],
    " = ", Style[ToString[Round @ v], Bold], " GeV"
  }],
  textscale,
  FontFamily -> fontFam,
  FontWeight -> fontWeight,
  Black
];



legendLines2 = LineLegend[
  styles2,
  s0List2,
  LegendLayout     -> "Column",
  LegendFunction   -> shadowbox,
  LegendMarkerSize -> {20, 2},
  LabelStyle -> Directive[FontSize -> textscale, FontFamily -> "Helvetica"],
  LegendLabel -> Style[
    Row[{TraditionalForm @ HoldForm[Sqrt[Subscript[s, 0]]]," [GeV]"}],
    textscale,
    FontFamily -> fontFam,
    FontWeight -> fontWeight,
    Black
  ]
];


(*limPlot2 = Plot[
  {limZZZ300, limZZZ3000},
  {x, xmin2, xmax2},
  PlotStyle -> {styleLim300, styleLim3000}
];*)

pltZZZ2 = Show[
  MapThread[
    ListLogPlot[smoothLogPairs[#1, 2], PlotStyle -> #2, Joined -> True, PlotMarkers -> None] &,
    {Values[pSumByS0], styles2}
  ],
(*  limPlot2,*)
  Evaluate @ base1DOpts,
  Epilog -> {
    Black,
    Inset[legendLines2,   Scaled[{0.15, 0.33}]](*,
    Inset[legendLumi, Scaled[{0.89, 0.18}]]*)
  },
  FrameLabel -> {xLabM2, yLabZZZ},
  PlotRange  -> {{xmin2, xmax2}, {ymin2, ymax2}},
  PlotLabel  -> None,
  (*Style[Row[{"ZZZ sum,  ",Subscript[Style["m", Italic], Superscript[Style["H", Italic], "\[PlusMinus]"]]," = ", mhcSel2, " GeV"}],
    textscale,
    FontFamily -> fontFam,
    FontWeight -> fontWeight],*)
  ImageSize -> imgSize
];



finalPlot2 = pltZZZ2


If[SavePDF == 1,
  Export[SavePath <> "ZZZ_sum_diffS0_mhc" <> ToString[mhcSel2] <> ".pdf", finalPlot2]
]


(* ::Subsection:: *)
(*ZWW*)


(* ::Subsubsection:: *)
(*\:0433\:0440\:0430\:0444\:0438\:043a f4 ZWW from mh2 (different mhc)*)


sSel   = 10000;
mhcList = Range[400., 800., 50.];

dsBase  = ds[Select[ #sqrts0 == sSel &]];
yKey   = If[
  Length[dsBase ] > 0 && MemberQ[Keys@First@Normal@dsBase , "f4mean"],
  "f4mean",
  "log10_f4mean"
];

pairs[d_] := Module[{rows},
  rows = Normal @ d[All, (Function[r, {r["mh2"], r[yKey]}])];
  rows = Select[rows, VectorQ[#, NumericQ] &];
  SortBy[rows, First]
];

sumPairsLin[a_, b_, yLog10_: False] := Module[{toAssoc, H, xs, ysLin, eps = 10.^-300},
  toAssoc[list_] := AssociationThread[
    list[[All, 1]] -> If[yLog10, 10.^list[[All, 2]], list[[All, 2]]]
  ];
  H = Merge[{toAssoc[a], toAssoc[b]}, Total];
  xs = Sort @ Keys[H];
  ysLin = Lookup[H, xs];
  Transpose[{xs, If[yLog10, Log10[Clip[ysLin, {eps, Infinity}]], ysLin]}]
];

useLog10 = (yKey === "log10_f4mean");


pZWWsumByMhc = Association @ Table[
  Module[{mhcSel = mhc, dsZWWuu, dsZWWdd, pZWWuu, pZWWdd, pZWWsum},
    dsZWWuu = dsBase[Select[#mhc == mhcSel && #procname == "uuZWW" &]];
    dsZWWdd = dsBase[Select[#mhc == mhcSel && #procname == "ddZWW" &]];
    pZWWuu  = pairs[dsZWWuu];
    pZWWdd  = pairs[dsZWWdd];
    pZWWsum = sumPairsLin[pZWWuu, pZWWdd, useLog10];
    mhcSel -> pZWWsum
  ],
  {mhc, mhcList}
];


allPairs = Values[pZWWsumByMhc];
xmin = Min @ Flatten[First /@ # & /@ allPairs];
xmax = Max @ Flatten[First /@ # & /@ allPairs];

(* \:0441\:0442\:0438\:043b\:0438 \:0438 \:043f\:043e\:0434\:043f\:0438\:0441\:0438 \:0434\:043b\:044f \:0441\:0435\:0440\:0438\:0438 mhc *)
colors = ColorData["Rainbow"] /@ Rescale[Range[Length[mhcList]]];
stylesZWW = (Directive[#, Thick] & /@ colors);   (* \:0438\:043b\:0438 AbsoluteThickness[3] *)
labelsZWW = Row[{Subscript[Style["m", Italic], Superscript[Style["H", Italic], "\[PlusMinus]"]]," = ", #, " GeV"}] & /@ mhcList;

legendZWW = Placed[
  LineLegend[
    Join[stylesZWW, {styleLim}],
    Join[labelsZWW, {"f4 limit"}],
    LegendLayout -> "Column",
    LegendFunction -> (Framed[#, Background -> White,
      FrameStyle -> GrayLevel[.85], FrameMargins -> {{10,10},{8,8}}] &)
  ],
  {Right, Top}
];

(* \:043d\:0430\:0431\:043e\:0440 \:043a\:0440\:0438\:0432\:044b\:0445 *)
curvesZWW = MapThread[
  ListPlot[#1, Joined -> True, PlotStyle -> #2] &,
  {Values[pZWWsumByMhc], stylesZWW}
];

limline = Plot[Log10[constrait], {x, xmin, xmax},
  PlotStyle -> styleLim, Axes -> False, Frame -> False];

pltMainZWW = Show[
  curvesZWW,
  Frame -> True,
  FrameLabel -> {
    "\!\(\*TemplateBox[<|\"boxes\" -> FormBox[SubscriptBox[StyleBox[\"m\", \"TI\"], \"2\"], TraditionalForm], \"errors\" -> {}, \"input\" -> \"m_{2}\", \"state\" -> \"Boxes\"|>,\n\"TeXAssistantTemplate\"]\) GeV",
    If[TrueQ@useLog10,
      "\!\(\*TemplateBox[<|\"boxes\" -> FormBox[SubscriptBox[\"Log\", \"10\"], TraditionalForm], \"errors\" -> {}, \"input\" -> \"\\\\text{Log}_{10}\", \"state\" -> \"Boxes\"|>,\n\"TeXAssistantTemplate\"]\)|f4|",
      "|f4|"
    ]
  },
  GridLines -> None, PlotRange -> All, ImageSize -> 900,
  PlotLabel -> Row[{
    "\!\(\*TemplateBox[<|\"boxes\" -> FormBox[RowBox[{SqrtBox[SubscriptBox[StyleBox[\"s\", \"TI\"], \"0\"]], \"\[LongEqual]\"}], TraditionalForm], \"errors\" -> {}, \"input\" -> \"\\\\sqrt{s_0}=\", \"state\" -> \"Boxes\"|>,\n\"TeXAssistantTemplate\"]\)",
    sSel, " GeV"
  }]
];
pltMainZWWlim = Show[
  pltMainZWW, limline,
  Axes -> False,           (* \:0434\:0443\:0431\:043b\:0438\:0440\:0443\:0435\:043c \:043d\:0430 \:0432\:0441\:044f\:043a\:0438\:0439 \:0441\:043b\:0443\:0447\:0430\:0439 *)
  Frame -> True,
  PlotRange -> All,
  PlotRangeClipping -> True
];


finalPlot2 = Legended[pltMainZWWlim, legendZWW];
finalPlot2


If[SavePDF,
	graphpath = SavePath <> "f4mean_s"<>ToString[sSel]<>"diffMh2.pdf";
  Export[graphpath, finalPlot2]];


(* ::Subsubsection:: *)
(*\:0413\:0440\:0430\:0444\:0438\:043a f4 ZZZ ZWW from mh2 (fix mhc) for different s0*)


mhcSel  = 400;                         (* \:0444\:0438\:043a\:0441\:0438\:0440\:0443\:0435\:043c m_{H^\pm} *)
s0List  = {1000, 10000, 13000};        (* \:043d\:0430\:0431\:043e\:0440 sqrt{s0} *)
yKey    = If[ValueQ[yKey], yKey, "f4mean"];
useLog10 = If[ValueQ[useLog10], useLog10, True];


dsBase = ds[Select[#mhc == mhcSel &]];

pairs[d_] := Module[{rows},
  rows = Normal @ d[All, (Function[r, {r["mh2"], r[yKey]}])];
  rows = Select[rows, VectorQ[#, NumericQ] &];
  SortBy[rows, First]
];

sumPairsLin[a_, b_, yLog10_: False] := Module[{toAssoc, H, xs, ysLin, eps = 10.^-300},
  toAssoc[list_] := AssociationThread[
    list[[All, 1]] -> If[yLog10, 10.^list[[All, 2]], list[[All, 2]]]
  ];
  H = Merge[{toAssoc[a], toAssoc[b]}, Total];
  xs = Sort @ Keys[H];
  ysLin = Lookup[H, xs];
  Transpose[{xs, If[yLog10, Log10[Clip[ysLin, {eps, Infinity}]], ysLin]}]
];

(* \:0432\:044b\:0431\:043e\:0440\:043a\:0430 \:0438 \:0441\:0443\:043c\:043c\:044b \:043f\:043e \:043a\:0430\:0436\:0434\:043e\:043c\:0443 s0 *)
extractForS0[sSel_] := Module[{ds, zzzuu, zzzdd, zwwuu, zwwdd},
  ds     = dsBase[Select[#sqrts0 == sSel &]];       (* \:0435\:0441\:043b\:0438 \:0443 \:0432\:0430\:0441 \:043a\:043b\:044e\:0447 \:043d\:0435 "sqrts0", \:0437\:0430\:043c\:0435\:043d\:0438\:0442\:0435 *)
  zzzuu  = pairs[ds[Select[#procname == "uuZZZ" &]]];
  zzzdd  = pairs[ds[Select[#procname == "ddZZZ" &]]];
  zwwuu  = pairs[ds[Select[#procname == "uuZWW" &]]];
  zwwdd  = pairs[ds[Select[#procname == "ddZWW" &]]];
  <|
    "ZZZsum" -> sumPairsLin[zzzuu, zzzdd, useLog10],
    "ZWWsum" -> sumPairsLin[zwwuu, zwwdd, useLog10]
  |>
];


dataByS0 = Association @ Table[sSel -> extractForS0[sSel], {sSel, s0List}];


(* \:0434\:0438\:0430\:043f\:0430\:0437\:043e\:043d x *)
allPairs = Flatten[Values[dataByS0][[All, All]], 1];
(*xmin = Min @ (First /@ allPairs);
xmax = Max @ (First /@ allPairs);*)
xmin = 200;
xmax = 800;
(* ==== 2) plot ==== *)

sLabel[m_] := Row[{"\[Sqrt]", Subscript[Style["s", Italic], "0"], " = ", m, " GeV"}];

cols = ColorData["Rainbow"] /@ Rescale[Range[Length@s0List]];
stylesZZZ = Directive[#, Thick] & /@ cols;
stylesZWW = Directive[#, Dashed, AbsoluteThickness[2]] & /@ cols;

curvesZZZ = MapThread[
  ListPlot[dataByS0[#1, "ZZZsum"], Joined -> True, PlotStyle -> #2] &,
  {s0List, stylesZZZ}
];
curvesZWW = MapThread[
  ListPlot[dataByS0[#1, "ZWWsum"], Joined -> True, PlotStyle -> #2] &,
  {s0List, stylesZWW}
];

labels = Join[
  (Row[{"ZZZ sum, ", sLabel[#]}] & /@ s0List),
  (Row[{"ZWW sum, ", sLabel[#]}] & /@ s0List)
];

legend = Placed[
  LineLegend[
    Join[stylesZZZ, stylesZWW], labels,
    LegendLayout -> "Column",
    LegendFunction -> (Framed[#, Background -> White,
       FrameStyle -> GrayLevel[.85], FrameMargins -> {{10,10},{8,8}}] &)
  ],
  {Left, Bottom}
];

(* --- \:043e\:0441\:0438 --- *)
xLabel = Style[Row[{Subscript[Style["m", Italic], "2"], " GeV"}], 13];
yLabel = Style[
  If[TrueQ@useLog10, Row[{Subscript["Log", 10], " |f4|"}], "|f4|"], 13];

(* --- \:0437\:0430\:0433\:043e\:043b\:043e\:0432\:043e\:043a --- *)
plotLabel = Row[{
  "\[Sqrt]", Subscript[Style["s", Italic], "0"], " = ", sSel, " GeV,  ",
  Subscript[Style["m", Italic], Superscript[Style["H", Italic], "\[PlusMinus]"]],
  " = ", mhcSel, " GeV"
}];

(* --- \:043f\:0440\:0438\:043c\:0435\:043d\:044f\:0435\:043c \:043a \:0433\:0440\:0430\:0444\:0438\:043a\:0443 --- *)
pltMain = Show[
  Join[curvesZZZ, curvesZWW],
  GridLines -> None, 
  PlotRange -> {{xmin,xmax},{-7.55, -4.95}}, 
  ImageSize -> 900,
  FrameLabel -> {xLabel, yLabel},
  PlotLabel  -> plotLabel,
  Axes -> False,   
  Frame -> True,
  PlotRangeClipping -> True
];

finalPlot3 = Legended[pltMain, legend];
finalPlot3


If[SavePDF,
	graphpath = SavePath <> "f4mean_diffS_mhc"<>ToString[mhcSel]<>".pdf";
  Export[graphpath, finalPlot3]];
