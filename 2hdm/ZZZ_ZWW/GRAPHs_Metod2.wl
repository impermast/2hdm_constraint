(* ::Package:: *)

(* ::Section:: *)
(* Setup *)


SetDirectory[NotebookDirectory[]];
<< "../modules/setup.m"
SetDirectory[NotebookDirectory[]];
Print[Directory[]]

SavePath = "subgraphs/";
SavePDF  = 0;

dir     = "buffer";
fileUU = "buffer/f4mean_uuZZZ.csv";
fileDD = "buffer/f4mean_ddZZZ.csv";

yColumn = "log10_norma_pb";

(* fixed benchmark selection *)
sSel   = 1000;   (* sqrt(s0) in GeV *)
mhcSel = 400;    (* mH^\[PlusMinus] in GeV *)

sigmaExpCurrentPB   = 1.0*10^-7;
sigmaExpProjectedPB = 3.0*10^-8;


(* ::Section:: *)
(* Graph design: same style logic as Method 3 *)


Clear[base1DOpts, shadowbox];

textscale   = 20;
imgSize     = 600;
fontFam     = "Helvetica";
fontWeight  = "Plain";
frameThick  = 2.2;
lineThick   = 2.6;

styleLimCurrent   = Directive[Red,  Dashed, AbsoluteThickness[2.2]];
styleLimProjected = Directive[Blue, Dashed, AbsoluteThickness[2.2]];

shadowbox[legend_] := Framed[
  legend,
  Background  -> White,
  FrameStyle  -> None,
  FrameMargins -> 0
];

base1DOpts := Sequence[
  Frame      -> True,
  Axes       -> False,
  Joined     -> True,
  PlotMarkers -> None,
  FrameStyle -> Directive[Black, AbsoluteThickness[frameThick]],
  FrameTicksStyle -> Directive[
    textscale - 2,
    FontFamily -> fontFam,
    FontWeight -> fontWeight
  ],
  LabelStyle -> Directive[
    textscale,
    FontFamily -> fontFam,
    FontWeight -> fontWeight
  ],
  BaseStyle  -> Directive[
    textscale,
    FontFamily -> fontFam,
    FontWeight -> fontWeight
  ],
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
    "\[Times]", Superscript[10, -stepen]
  }], textscale
];
makePlotLabel[sSel_, mhcSel_] := Style[
  Row[{
    TraditionalForm @ HoldForm[Sqrt[Subscript[s, 0]]],
    " = ", sSel, " GeV,  ",
    TraditionalForm @ HoldForm[Subscript[m, Superscript[H, "\[PlusMinus]"]]],
    " = ", mhcSel, " GeV"
  }],
  textscale, FontFamily -> fontFam, FontWeight -> fontWeight
];


(* ::Section:: *)
(* Data loading*)


rawUU = Import[fileUU, "Dataset", "HeaderLines" -> 1];
rawDD = Import[fileDD, "Dataset", "HeaderLines" -> 1];

Clear[extractSigmaPairs, sumPairsLin, detectIfLogColumn];

(* detect whether the selected column is already stored in log10 scale *)
detectIfLogColumn[col_] := StringStartsQ[col, "log10_"];

extractSigmaPairs[raw_, s0sel_, mhcsel_, col_] := Module[{rows, vals},
  rows = Normal @ raw[Select[#sqrts0 == s0sel && #mhc == mhcsel &]];
  vals = Select[
    rows,
    NumericQ[#mh2] && KeyExistsQ[#, col] && NumericQ[#[col]] &
  ];
  vals = SortBy[{#mh2, #[col]} & /@ vals, First];
  If[detectIfLogColumn[col],
    {#[[1]], 10.^#[[2]]} & /@ vals,
    vals
  ]
];

sumPairsLin[a_, b_] := Module[{Ha, Hb, H, xs, ys},
  Ha = AssociationThread[a[[All, 1]] -> a[[All, 2]]];
  Hb = AssociationThread[b[[All, 1]] -> b[[All, 2]]];
  H  = Merge[{Ha, Hb}, Total];
  xs = Sort @ Keys[H];
  ys = Lookup[H, xs];
  Transpose[{xs, ys}]
];


pUU  = extractSigmaPairs[rawUU, sSel, mhcSel, yColumn];
pDD  = extractSigmaPairs[rawDD, sSel, mhcSel, yColumn];
pSum = sumPairsLin[pUU, pDD];

If[Length[pUU] == 0 || Length[pDD] == 0,
  Print["No data found for the chosen benchmark: sqrt(s0)=", sSel, ", mhc=", mhcSel, "."];
  Abort[];,
  Print[First[pUU]]
];

xmin = Min[Join[pUU[[All, 1]], pDD[[All, 1]], pSum[[All, 1]]]];
xmax = Max[Join[pUU[[All, 1]], pDD[[All, 1]], pSum[[All, 1]]]];

ymin = 10.^Floor[Log10[Min[Join[pUU[[All, 2]], pDD[[All, 2]], pSum[[All, 2]], {sigmaExpCurrentPB, sigmaExpProjectedPB}]]]];
ymax = 10.^Ceiling[Log10[Max[Join[pUU[[All, 2]], pDD[[All, 2]], pSum[[All, 2]], {sigmaExpCurrentPB, sigmaExpProjectedPB}]]]];


Print[ymin, "   ",ymax]


(* ::Section:: *)
(* Plot for ZZZ only *)


colorUU = Darker[Brown,  0.10];
colorDD = Darker[Yellow, 0.20];

stylesLines = {
  Directive[colorUU, Dashed, AbsoluteThickness[lineThick + 0.2]],
  Directive[colorDD, Dashed, AbsoluteThickness[lineThick + 0.2]],
  Directive[Black,            AbsoluteThickness[lineThick + 0.8]]
};

legendLines = LineLegend[
  Join[stylesLines, {styleLimCurrent, styleLimProjected}],
  {
    Style["uu \[Rule] ZZZ", textscale - 2],
    Style["dd \[Rule] ZZZ", textscale - 2],
    Style["total",         textscale - 2],
    Style["\[Sigma]_exp current",   textscale - 2],
    Style["\[Sigma]_exp projected", textscale - 2]
  },
  LegendLayout     -> "Column",
  LegendFunction   -> shadowbox,
  LegendMarkerSize -> {32, 2},
  LabelStyle       -> Directive[
    textscale - 2,
    FontFamily -> fontFam,
    FontWeight -> fontWeight,
    Black
  ]
];



yminPB = 0.0333;
ymaxPB = 0.038;

limEpilog = {
  styleLimCurrent,
  Line[{{xmin, Log[sigmaExpCurrentPB]}, {xmax, Log[sigmaExpCurrentPB]}}],
  styleLimProjected,
  Line[{{xmin, Log[sigmaExpProjectedPB]}, {xmax, Log[sigmaExpProjectedPB]}}],
  Black,
  Inset[legendLines, Scaled[{0.84, 0.76}]]
};

pltSigmaZZZ = Show[
  (* total \:043b\:0443\:0447\:0448\:0435 \:0440\:0438\:0441\:043e\:0432\:0430\:0442\:044c \:041f\:0415\:0420\:0412\:042b\:041c, \:0447\:0442\:043e\:0431\:044b \:043d\:0435 \:0437\:0430\:043a\:0440\:044b\:0432\:0430\:043b uu \:0438 dd *)
  ListLogPlot[
    pSum,
    PlotStyle   -> stylesLines[[3]],
    Joined      -> True,
    PlotMarkers -> None
  ],
  ListLogPlot[
    pUU,
    PlotStyle   -> stylesLines[[1]],
    Joined      -> True,
    PlotMarkers -> None
  ],
  ListLogPlot[
    pDD,
    PlotStyle   -> stylesLines[[2]],
    Joined      -> True,
    PlotMarkers -> None
  ],
  Evaluate @ base1DOpts,
  Epilog     -> limEpilog,
  FrameLabel -> {xLabM2, yLabSigma},
  PlotRange  -> {{xmin, xmax}, {Log[yminPB], Log[ymaxPB]}},
  PlotLabel  -> plotLabel,
  ImageSize  -> imgSize
];


pltSigmaZZZ


If[SavePDF == 1,
  Export[
    SavePath <> "sigma_ZZZ_uudd_sum_s" <> ToString[sSel] <> "_mhc" <> ToString[mhcSel] <> ".pdf",
    pltSigmaZZZ
  ]
];


(* ::Section::Closed:: *)
(*\:0433\:0440\:0430\:0444\:0438\:043a sigma (zzz) from mh2 (fix s0, mhc)*)


(* ::InheritFromParent:: *)
(**)


dsSigma = SaveToCSVmodule`LoadF4Data[dataDir, pattern];
If[Head[dsSigma] =!= Dataset, dsSigma = Dataset[dsSigma]];
dsSigma[[1]]


sSel = 10000;
mhcSel = 400;

yKey = "log10_norma_pb";

dsCut = Select[
  Normal @ dsSigma,
  Lookup[#, "sqrts0", sSel] == sSel && Lookup[#, "mhc", mhcSel] == mhcSel &
];

dsZZZuu = Select[dsCut, Lookup[#, "procname", ""] == "uuZZZ" &];
dsZZZdd = Select[dsCut, Lookup[#, "procname", ""] == "ddZZZ" &];
dsZWWuu = Select[dsCut, Lookup[#, "procname", ""] == "uuZWW" &];
dsZWWdd = Select[dsCut, Lookup[#, "procname", ""] == "ddZWW" &];

pairs[d_] := SortBy[
  Select[
    ({Lookup[#, "mh2"], Lookup[#, yKey]} & /@ d),
    VectorQ[#, NumericQ] &
  ],
  First
];

pZZZuu = pairs[dsZZZuu];
pZZZdd = pairs[dsZZZdd];
pZWWuu = pairs[dsZWWuu];
pZWWdd = pairs[dsZWWdd];

log10Sum[v_List] := Module[{mx = Max[v]},
  mx + Log10[Total[10.^(v - mx)]]
];

sum[p1_, p2_] := Module[{g},
  g = GatherBy[Join[p1, p2], First];
  SortBy[
    ({First[#[[1]]], log10Sum[#[[All, 2]]]} & /@ g),
    First
  ]
];


pZZZ = sum[pZZZuu, pZZZdd];
pZWW = sum[pZWWuu, pZWWdd];
Print[pZZZuu[[1]],pZZZ[[1]]]


pZZZ


xmin = 175; xmax = 800;
ymin = 10^Min[pZZZuu[[All,2]]]; ymax = 10^Max[pZZZuu[[All,2]]];

xLab = Style["mh2 [GeV]", textscale];
yLab = Style["\!\(\*FormBox[TemplateBox[<|\"boxes\" -> FormBox[\"\[Sigma]\", TraditionalForm], \"errors\" -> {}, \"input\" -> \"\\\\sigma\", \"state\" -> \"Boxes\"|>,\n\"TeXAssistantTemplate\"],
TextForm]\) [pb]",textscale ];
plot1 = ListPlot[MapAt[10^# &, pZZZuu, {All, 2}], base1DOpts];


If[SavePDF,
	graphpath = SavePath <> "s_mh2_ZZZ_"<>"mhc"<>ToString[mhcSel]<>".pdf";
  Export[graphpath, plot1]];
