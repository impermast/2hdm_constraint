(* ::Package:: *)

LogRange1[a_?NumericQ, b_?NumericQ, n_Integer] := 
	Table[10^i,{i,Range[Log10[a],Log10[b],(Log10[b]-Log10[a])/n]}];


(*Main params of graphs*)
(*Number of points in graphs  N=*)
PointNumber = 200;
ContoursNumber = 10;
GraphTheme = ColorData["Rainbow"];
SavePDF = True;


scriptDir = If[StringQ[$InputFileName], DirectoryName[$InputFileName], $InitialDirectory];
SetDirectory[scriptDir];
Get["../modules/ModelParams.wl"];
Get["../modules/SaveToCSVmodule.wl"];
Get["../modules/FunctionalModules.wl"];
SavePath = "subgraphs/";
dir     = "buffer";
pattern = "f4mean_*.csv";


(*constraints on f4 ZZZ*)
constraitF4Z300 = 1.9*10^(-4);
constraitF4Z3000 = 2*10^(-5);


(* ============================================== *)
(* f4 CSV -> \:0442\:0430\:0431\:043b\:0438\:0446\:044b                  *)
(* ============================================== *)
ds  = SaveToCSVmodule`LoadF4Data["buffer","f4mean_*.csv"];
useLog10 = (yKey === "log10_f4mean");
mhcSel = 800;
sSel   = 1000;
dsCut  = ds[Select[#mhc == mhcSel && #sqrts0 == sSel &]];
yKey   = If[
  Length[dsCut] > 0 && MemberQ[Keys@First@Normal@dsCut, "f4mean"],
  "f4mean",
  "log10_f4mean"
];
useLog10 = (yKey === "log10_f4mean");


dsZZZuu = dsCut[Select[#procname == "uuZZZ" &]];
dsZZZdd = dsCut[Select[#procname == "ddZZZ" &]];
dsZWWuu = dsCut[Select[#procname == "uuZWW" &]];
dsZWWdd = dsCut[Select[#procname == "ddZWW" &]];
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
pTOTAL  = sumPairsLin[pZZZsum, pZWWsum, useLog10];


(* \:0434\:0438\:0430\:043f\:0430\:0437\:043e\:043d \:043f\:043e \:043e\:0441\:0438 x *)
xmin = Min @ (First /@ pTOTAL);
xmax = Max @ (First /@ pTOTAL);

styles = {
  {Blue, Dotted}, {Blue, Dashed}, Blue,
  {Darker[Green], Dotted}, {Darker[Green], Dashed}, Darker[Green],
  Black, {Red, DotDashed}  (* f4exp *)
};
labels = {
  "ZZZ uu","ZZZ dd","ZZZ sum",
  "ZWW uu","ZWW dd","ZWW sum",
  "ZZZ+ZWW","f4exp"
};

legend = LineLegend[styles, labels, 
		LegendLayout -> "Column", 
		LegendFunction->(Framed[#, Background->White,
                                 FrameStyle->GrayLevel[.85],
                                 FrameMargins->{{10,10},{8,8}}] &)];

(* \:043e\:0441\:043d\:043e\:0432\:043d\:043e\:0439 \:0433\:0440\:0430\:0444\:0438\:043a *)
pltMain = Show[
  {
    ListPlot[pZZZuu,  Joined->True, PlotStyle->styles[[1]]],
    ListPlot[pZZZdd,  Joined->True, PlotStyle->styles[[2]]],
    ListPlot[pZZZsum, Joined->True, PlotStyle->styles[[3]]],
    ListPlot[pZWWuu,  Joined->True, PlotStyle->styles[[4]]],
    ListPlot[pZWWdd,  Joined->True, PlotStyle->styles[[5]]],
    ListPlot[pZWWsum, Joined->True, PlotStyle->styles[[6]]],
    ListPlot[pTOTAL,  Joined->True, PlotStyle->styles[[7]]],
    Plot[Log10[constraitF4Z3000], {x, xmin, xmax}, PlotStyle->styles[[8]]]
  },
  Frame -> True, FrameLabel -> {"\!\(\*TemplateBox[<|\"boxes\" -> FormBox[SubscriptBox[StyleBox[\"m\", \"TI\"], \"2\"], TraditionalForm], \"errors\" -> {}, \"input\" -> \"m_{2}\", \"state\" -> \"Boxes\"|>,\n\"TeXAssistantTemplate\"]\) GeV", "\!\(\*TemplateBox[<|\"boxes\" -> FormBox[SubscriptBox[\"Log\", \"10\"], TraditionalForm], \"errors\" -> {}, \"input\" -> \"\\\\text{Log}_{10}\", \"state\" -> \"Boxes\"|>,\n\"TeXAssistantTemplate\"]\)|f4|"},
  GridLines -> None, PlotRange -> All, ImageSize -> 720,
  PlotLabel -> Row[{"\!\(\*TemplateBox[<|\"boxes\" -> FormBox[RowBox[{SqrtBox[SubscriptBox[StyleBox[\"s\", \"TI\"], \"0\"]], \"\[LongEqual]\"}], TraditionalForm], \"errors\" -> {}, \"input\" -> \"\\\\sqrt{s_0}=\", \"state\" -> \"Boxes\"|>,\n\"TeXAssistantTemplate\"]\)", sSel, " GeV,  ","\!\(\*TemplateBox[<|\"boxes\" -> FormBox[SubscriptBox[StyleBox[\"m\", \"TI\"], SuperscriptBox[StyleBox[\"H\", \"TI\"], \"\[PlusMinus]\"]], TraditionalForm], \"errors\" -> {}, \"input\" -> \"m_{H^\\\\pm}\", \"state\" -> \"Boxes\"|>,\n\"TeXAssistantTemplate\"]\)= ", mhcSel, " GeV"}]
];
finalPlot = Legended[pltMain,legend]


If[SavePDF,
	graphpath = SavePath <> "f4mean_s"<>ToString[sSel]<>"mhc"<>ToString[mhcSel]<>".pdf";
  Export[graphpath, finalPlot]];
