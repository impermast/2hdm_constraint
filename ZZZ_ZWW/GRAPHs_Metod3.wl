(* ::Package:: *)

SetDirectory[NotebookDirectory[]];
<< "../modules/setup.m"


(*Main params of graphs*)
(*Number of points in graphs  N=*)
PointNumber = 200;
ContoursNumber = 10;
GraphTheme = ColorData["Rainbow"];
SavePDF = True;


SavePath = "subgraphs/";
dir     = "buffer";
pattern = "f4mean_*.csv";


(*constraints on f4 ZZZ*)
constraitF4Z300 = 1.9*10^(-4);
constraitF4Z3000 = 2*10^(-5);

constrait = constraitF4Z3000;
styleLim = Directive[GrayLevel[.2], DotDashed, AbsoluteThickness[2]];


(* ::Subsection:: *)
(*\:0433\:0440\:0430\:0444\:0438\:043a f4 (zzz zww) from mh2 (fix s0, mhc)*)


(* ============================================== *)
(* f4 CSV -> \:0442\:0430\:0431\:043b\:0438\:0446\:044b                  *)
(* ============================================== *)
ds  = SaveToCSVmodule`LoadF4Data["buffer","f4mean_*.csv"];
useLog10 = (yKey === "log10_f4mean");
mhcSel = 800;
sSel   = 1000;
dsCut  = ds[Select[ #sqrts0 == sSel &]];
yKey   = If[
  Length[dsCut] > 0 && MemberQ[Keys@First@Normal@dsCut, "f4mean"],
  "f4mean",
  "log10_f4mean"
];
useLog10 = (yKey === "log10_f4mean");


dsZZZuu = dsCut[Select[#procname == "uuZZZ" &]];
dsZZZdd = dsCut[Select[#procname == "ddZZZ" &]];
dsZWWuu = dsCut[Select[#mhc == mhcSel && #procname == "uuZWW" &]];
dsZWWdd = dsCut[Select[#mhc == mhcSel && #procname == "ddZWW" &]];
(* \:0432 \:043f\:0430\:0440\:044b {mh2, f4mean} *)
pairs[d_] := Module[{rows},
  rows = Normal @ d[All, (Function[r, {r["mh2"], r[yKey]}])];
  rows = Select[rows, VectorQ[#, NumericQ] &];
  SortBy[rows, First]
];

pZZZuu = pairs[dsZZZuu];
pZZZdd = pairs[dsZZZdd];
pZWWuu = pairs[dsZWWuu];
pZWWdd = pairs[dsZWWdd];

(* \:0441\:0443\:043c\:043c\:0438\:0440\:043e\:0432\:0430\:043d\:0438\:0435 \:0434\:0432\:0443\:0445 \:043a\:0440\:0438\:0432\:044b\:0445 {mh2, y}; y \:043c\:043e\:0436\:0435\:0442 \:0431\:044b\:0442\:044c \:0432 log10 *)
sumPairsLin[a_, b_, yLog10_: False] := Module[{toAssoc, H, xs, ysLin, eps = 10.^-300},
  toAssoc[list_] := AssociationThread[
    list[[All, 1]] -> If[yLog10, 10.^list[[All, 2]], list[[All, 2]]]
  ];
  H = Merge[{toAssoc[a], toAssoc[b]}, Total];
  xs = Sort @ Keys[H];
  ysLin = Lookup[H, xs];
  Transpose[{xs, If[yLog10, Log10[Clip[ysLin, {eps, Infinity}]], ysLin]}]
];

pZZZsum = sumPairsLin[pZZZuu, pZZZdd, useLog10];
pZWWsum = sumPairsLin[pZWWuu, pZWWdd, useLog10];


(* \:0434\:0438\:0430\:043f\:0430\:0437\:043e\:043d \:043f\:043e \:043e\:0441\:0438 x *)
xmin = Min @ (First /@ pZWWsum);
xmax = Max @ (First /@ pZWWsum);

styles = {
  {Blue, Dotted}, {Blue, Dashed}, Blue,
  {Darker[Green], Dotted}, {Darker[Green], Dashed}, Darker[Green],
  styleLim  (* f4exp *)
};
labels = {
  "ZZZ uu","ZZZ dd","ZZZ sum",
  "ZWW uu","ZWW dd","ZWW sum",
  "f4 limit"
};

legend = Placed[LineLegend[styles, labels, 
		LegendLayout -> "Column", 
		LegendFunction->(Framed[#, Background->White,
                                 FrameStyle->GrayLevel[.85],
                                 FrameMargins->{{10,10},{8,8}}] &)],
       {Left, Bottom}
                             ];

(* \:043e\:0441\:043d\:043e\:0432\:043d\:043e\:0439 \:0433\:0440\:0430\:0444\:0438\:043a *)
pltMain = Show[
  {
    ListPlot[pZZZuu,  Joined->True, PlotStyle->styles[[1]]],
    ListPlot[pZZZdd,  Joined->True, PlotStyle->styles[[2]]],
    ListPlot[pZZZsum, Joined->True, PlotStyle->styles[[3]]],
    ListPlot[pZWWuu,  Joined->True, PlotStyle->styles[[4]]],
    ListPlot[pZWWdd,  Joined->True, PlotStyle->styles[[5]]],
    ListPlot[pZWWsum, Joined->True, PlotStyle->styles[[6]]],
    Plot[Log10[constrait], {x, xmin, xmax}, PlotStyle->styles[[7]]]
  },
  Frame -> True, FrameLabel -> {"\!\(\*TemplateBox[<|\"boxes\" -> FormBox[SubscriptBox[StyleBox[\"m\", \"TI\"], \"2\"], TraditionalForm], \"errors\" -> {}, \"input\" -> \"m_{2}\", \"state\" -> \"Boxes\"|>,\n\"TeXAssistantTemplate\"]\) GeV", "\!\(\*TemplateBox[<|\"boxes\" -> FormBox[SubscriptBox[\"Log\", \"10\"], TraditionalForm], \"errors\" -> {}, \"input\" -> \"\\\\text{Log}_{10}\", \"state\" -> \"Boxes\"|>,\n\"TeXAssistantTemplate\"]\)|f4|"},
  GridLines -> None, 
  PlotRange -> {{xmin,xmax},{-4.49,-7.5}}, 
  ImageSize -> 720, Axes -> False,  Frame -> True, PlotRangeClipping -> True,
  PlotLabel -> Row[{"\!\(\*TemplateBox[<|\"boxes\" -> FormBox[RowBox[{SqrtBox[SubscriptBox[StyleBox[\"s\", \"TI\"], \"0\"]], \"\[LongEqual]\"}], TraditionalForm], \"errors\" -> {}, \"input\" -> \"\\\\sqrt{s_0}=\", \"state\" -> \"Boxes\"|>,\n\"TeXAssistantTemplate\"]\)", sSel, " GeV,  ","\!\(\*TemplateBox[<|\"boxes\" -> FormBox[SubscriptBox[StyleBox[\"m\", \"TI\"], SuperscriptBox[StyleBox[\"H\", \"TI\"], \"\[PlusMinus]\"]], TraditionalForm], \"errors\" -> {}, \"input\" -> \"m_{H^\\\\pm}\", \"state\" -> \"Boxes\"|>,\n\"TeXAssistantTemplate\"]\)= ", mhcSel, " GeV"}]
];


finalPlot1 = Legended[pltMain,legend]


(* ::InheritFromParent:: *)
(**)


If[SavePDF,
	graphpath = SavePath <> "f4mean_s"<>ToString[sSel]<>"mhc"<>ToString[mhcSel]<>".pdf";
  Export[graphpath, finalPlot1]];


(* ::Subsection:: *)
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
First[pZWWsumByMhc]


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


(* ::Subsection:: *)
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
