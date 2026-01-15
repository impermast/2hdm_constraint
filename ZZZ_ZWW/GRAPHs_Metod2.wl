(* ::Package:: *)

SetDirectory[NotebookDirectory[]];
<< "../modules/setup.m"


SavePath = "subgraphs/";
dataDir     = "buffer";
pattern = "f4mean_*.csv";


(* ::Subsubsection:: *)
(*graph settings*)


(*Main params of graphs*)

PointNumber = 200;
ContoursNumber = 10;
GraphTheme = ColorData["Rainbow"];
SavePDF = True;

constraitF4Z300 = 1.9*10^(-4);
constraitF4Z3000 = 2*10^(-5);

constrait = constraitF4Z3000;



shadowbox[legend_] := Framed[
  legend,
  Background   -> White,                (* \:0431\:0435\:043b\:0430\:044f \:043d\:0435\:043f\:0440\:043e\:0437\:0440\:0430\:0447\:043d\:0430\:044f \:043f\:0430\:043d\:0435\:043b\:044c *)
  FrameStyle -> Black,     (* \:0440\:0430\:043c\:043a\:0430 *)
  FrameMargins -> {{8, 8}, {5, 5}}    (* \:043e\:0442\:0441\:0442\:0443\:043f\:044b \:0432\:043d\:0443\:0442\:0440\:0438 \:0440\:0430\:043c\:043a\:0438 *)
];

textscale = 16;

base1DOpts := Sequence[
  Frame        -> True,
  Axes         -> False,
  PlotMarkers -> None,
  Joined -> True,
  FrameStyle   -> Directive[Black, Thickness[0.002]],
  FrameTicksStyle -> Directive[textscale - 2],
  FrameLabel   -> {xLab, yLab},
  LabelStyle   -> Directive[textscale, FontFamily -> "Helvetica"],
  BaseStyle    -> Directive[textscale, FontFamily -> "Helvetica"],
  PlotRange    -> {{xmin, xmax}, {ymin, ymax}},
  GridLines    -> None,
  PlotRangePadding -> Scaled[0.005]
];
base2DcontOpts := Sequence[
  Frame        -> True,
  Axes         -> False,
  FrameStyle   -> Directive[Black, Thickness[0.002]],
  FrameTicksStyle -> Directive[textscale - 2],
  LabelStyle   -> Directive[textscale, FontFamily -> "Helvetica"],
  BaseStyle    -> Directive[textscale, FontFamily -> "Helvetica"],
  FrameLabel   -> {xLab2D, yLab2D},
  PlotRange    -> All
];


legendLumi = SwatchLegend[
  {Directive[Red,  HatchFilling[ 45 Degree,3]],Directive[Blue, HatchFilling[-45 Degree,3]]},
  {"300", "3000"},
  LegendLayout   -> "Column",
  LegendFunction -> shadowbox,
  LabelStyle     -> Directive[FontSize -> textscale, FontFamily -> "Helvetica"],
  LegendLabel    -> Style[Row[{"\[Integral] ",Style["L", Italic],Style["d", Italic],Style["t", Italic],", ", Superscript["fb", -1] }],textscale]
];


(* ::Section:: *)
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


(* ::Section:: *)
(*\:0433\:0440\:0430\:0444\:0438\:043a sigma (zww) from mh2 (fix s0, mhc)*)


xmin = 175; xmax = 800;
ymin = 10^Min[pZWW[[All,2]]]; ymax = 10^Max[pZWW[[All,2]]];

xLab = Style["mh2 [GeV]", textscale];
yLab = Style["\!\(\*FormBox[TemplateBox[<|\"boxes\" -> FormBox[\"\[Sigma]\", TraditionalForm], \"errors\" -> {}, \"input\" -> \"\\\\sigma\", \"state\" -> \"Boxes\"|>,\n\"TeXAssistantTemplate\"],
TextForm]\) [pb]",textscale ];
plot2 = ListPlot[MapAt[10^# &, pZWW, {All, 2}], base1DOpts]


If[SavePDF,
	graphpath = SavePath <> "s_mh2_ZWW_"<>"mhc"<>ToString[mhcSel]<>".pdf";
  Export[graphpath, plot2]];
