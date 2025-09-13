(* ::Package:: *)

(* ::Title:: *)
(*PV-graphs*)


description="Graphs for ZZZ ZWW from txt files of f4Z 2hdm";
If[ $FrontEnd === Null,
	$FeynCalcStartupMessages = False;
	Print[description];
];
If[ $Notebooks === False,
	$FeynCalcStartupMessages = False
];
(*$LoadAddOns={"FeynArts", "FeynHelpers"};*)
$LoadAddOns={"FeynArts"};
<<FeynCalc`
$FAVerbose = 0;

FCCheckVersion[9,3,0];

Install["LoopTools"]
Needs["LoopTools`"]


LogRange1[a_?NumericQ, b_?NumericQ, n_Integer] := 
	Table[10^i,{i,Range[Log10[a],Log10[b],(Log10[b]-Log10[a])/n]}];


(* ::Section:: *)
(*Parameters and Constants*)


(*Main params of graphs*)
(*Number of points in graphs  N=*)
PointNumber = 200;
ContoursNumber = 10;
GraphTheme = ColorData["Rainbow"];
SavePDF = 1; (*1--save, 0--dont*)
ReCalculateTable = 1; (*1--recalculate,  0 -- no*)


SetDirectory[NotebookDirectory[]]
Get["../modules/ModelParams.wl"];
Get["../modules/SaveToCSVmodule.wl"];


SavePath = "subgraphs/";
str    = Import["buffer/F4ZZZ.txt"];
strZWW = Import["buffer/F4ZWW.txt"];


\[Alpha]2max =  0.955317;(* optimal alpha*)
\[Alpha]3max = 0.785398;
\[Alpha]2=0.5
\[Alpha]3=\[Alpha]3max
(*constraints on f4 ZZZ*)
constraitF4Z300 = 1.9*10^(-4);
constraitF4Z3000 = 2*10^(-5);

(*constraints on f4 ZWW*)
constraitF4ZWW = 2*10^(-4);

(*constraints on f2 ZWW*)
constraitF2ZWW = 2*10^(-4);


(* ::Section:: *)
(*Getting F4 funcs from file*)


args = {s,mh1,mh2,mh3,a1,a2,a3};
argsZWW = {s,mh1,mh2,mh3,mhc,a1,a2,a3};
Print["input Funcs:"]
str
strZWW
Activate[Inactive[SetDelayed][ToExpression["FZZZ"]@@(Pattern[#,_]&/@args),ToExpression[str]]];
Activate[Inactive[SetDelayed][ToExpression["FZWW"]@@(Pattern[#,_]&/@argsZWW),ToExpression[strZWW]]];
(*Abs[FZZZ[1,0.3,0.4,0.5,1]]*)
Print["generated funcs"]
FullSimplify[FZZZ[s,mh1,mh2,mh3,a1,a2,a3]//.SmpChanger//.Params]
FullSimplify[FZWW[Q^2,mh1,mh2,mh3,MHC,a1,a2,a3]//.SmpChanger//.Params];


(* ::Section:: *)
(*Prefactor*)


Print["Work with alpha params:"];
\[Alpha]1 = \[Pi]/20;


beta = \[Alpha]1;
x1[a1_,a2_,a3_] := (Cos[a1]*Cos[a2]*Cos[a1] + Sin[a1]*Cos[a2]*Sin[a1]);
x2[a1_,a2_,a3_] :=  (-(Sin[a1]*Cos[a3] + Cos[a1]*Sin[a2]*Sin[a3])*Cos[a1] + (Cos[a1]*Cos[a3] - Sin[a1]*Sin[a2] *Sin[a3])*Sin[a1]);
x3[a1_,a2_,a3_] := ((-Cos[a1] *Sin[a2]* Cos[a3] + Sin[a1] *Sin[a3])*Cos[a1] -(Sin[a1] *Sin[a2] *Cos[a3] + Cos[a1]* Sin[a3])*Sin[a1]);

R11[a1_,a2_,a3_]:=Cos[a1]*Cos[a2];                             R12[a1_,a2_,a3_]:=Sin[a1]*Cos[a2];                              R13[a1_,a2_,a3_]:=Sin[a2];
R21[a1_,a2_,a3_]:=-(Cos[a1]*Sin[a2]*Sin[a3]+Sin[a1]*Cos[a3]);  R22[a1_,a2_,a3_]:=-(Sin[a1]*Sin[a2]*Sin[a3]-Cos[a1]*Cos[a3]);   R23[a1_,a2_,a3_]:=Sin[a3]*Cos[a2];
R31[a1_,a2_,a3_]:=-Cos[a1]*Sin[a2]*Cos[a3]+Sin[a1]*Sin[a3];    R32[a1_,a2_,a3_]:=-(Sin[a1]*Sin[a2]*Cos[a3]+Cos[a1]*Sin[a3]);   R33[a1_,a2_,a3_]:=Cos[a2]*Cos[a3]; 

Y1[a1_,a2_,a3_]:=R12[a1,a2,a3]*Cos[a1]-R11[a1,a2,a3]*Sin[a1];
Y2[a1_,a2_,a3_]:=R22[a1,a2,a3]*Cos[a1]-R21[a1,a2,a3]*Sin[a1];
Y3[a1_,a2_,a3_]:=R32[a1,a2,a3]*Cos[a1]-R31[a1,a2,a3]*Sin[a1];

Print["Prefactor:"]

prefactor[a1_,a2_,a3_] := x1[a1,a2,a3] x2[a1,a2,a3] x3[a1,a2,a3];
FullSimplify[prefactor[a1,a2,a3]]
pref = prefactor[\[Alpha]1,\[Alpha]2,\[Alpha]3]
prefmax=prefactor[\[Alpha]1,\[Alpha]2max,\[Alpha]3max]
pref0 = \!\(TraditionalForm\`
\*FractionBox[
SuperscriptBox[\(g\), \(3\)], \(16 
\*SuperscriptBox[\(\[Pi]\), \(2\)] 
\*SuperscriptBox[\(cw\), \(3\)]\)]\)prefactor[\[Alpha]1,\[Alpha]2,\[Alpha]3];


k=1;
mZ= MZ;
v=vev;
mW=MW;


(* X3 (R1x3+\[ImaginaryI] Y1) (Y2+\[ImaginaryI] R2x3)//.AngleChanger//Simplify
FullSimplify[x3[a1,a2,a3](I*Y1[a1,a2,a3]+R13[a1,a2,a3])(I R23[a1,a2,a3]+Y2[a1,a2,a3])]*)


FplotZZZ[s_,mh2_,mhc_]:=Abs[FZZZ[s,mh1,mh2,Sqrt[mh2^2+v^2],\[Alpha]1,\[Alpha]2max,\[Alpha]3max]]//.SmpChanger//.Params;
FplotZWW[s_,mh2_,mhc_]:=Abs[FZWW[s,mh1,mh2,Sqrt[mh2^2+v^2],mhc,\[Alpha]1,\[Alpha]2max,\[Alpha]3max]]//.SmpChanger//.Params;


FplotZZZ[200^2,100,400]
FplotZZZ[s,mh2,mhc]


(* ::Section:: *)
(*Graphs*)


(* ::Subsection:: *)
(*CSV data*)


FplotZZZ[300^2, 250, mhc] //.SmpChanger//.Params


PointNumber = 500;
xmin = 183.4; xmax = 600;
ymin = 183.4; ymax = 600;

xs = Subdivide[xmin, xmax, PointNumber];
ys = Subdivide[ymin, ymax, PointNumber];
pairs = Tuples[{xs,ys}];

ClearAll[fNum, fNumW];
fNum[x_?NumericQ, y_?NumericQ] := Log10 @ N @ (FplotZZZ[x^2, y, 1] //.SmpChanger//.Params);

fNumW[x_?NumericQ, y_?NumericQ] := Module[{val},
  val = FplotZWW[x^2, y, Sqrt[y^2+v^2]] //. SmpChanger//. Params;
  Log10 @ N[val]
];
Print[First[xs]]


(*\:043f\:043e\:0434\:043a\:043b\:044e\:0447\:0435\:043d\:0438\:0435 \:0437\:0430\:043c\:0435\:043d smpchanger params \:0432 \:043f\:0430\:0440\:0430\:043b\:0435\:043b\:0435\:0437\:0430\:0446\:0438\:044e*)
DistributeDefinitions[FplotZZZ, SmpChanger, Params, mhc, xs, ys,pairs];
ParallelEvaluate[{Head[SmpChanger], Head[Params]}]


vals = Map[({#1, #2, fNum[#1, #2]} & @@ #) &, pairs];
Print["Ready"];
vals[[2]]


Export[FileNameJoin[{Directory[], "backfiles", "f4Z_m2q.csv"}],
       Prepend[vals, {"q","m2","log10_f4ZZZ"}], "CSV"];


 


(*vals1 = Parallelize @ Outer[fNumW, xs, ys];

(* \:0424\:043e\:0440\:043c\:0438\:0440\:0443\:0435\:043c \:043f\:043b\:043e\:0441\:043a\:0443\:044e \:0442\:0430\:0431\:043b\:0438\:0446\:0443 {q, m2, log10_f4ZZZ} *)
table1 = Flatten[
  Table[{xs[[i]], ys[[j]], vals1[[i, j]]}, {i, Length[xs]}, {j, Length[ys]}]
, 1];*)


Print["Ready"];
Export[FileNameJoin[{Directory[], "backfiles", "f4W_m2q.csv"}], table, "CSV"];

(* \:041f\:0440\:043e\:0432\:0435\:0440\:043a\:0430 *)
Print[First[table1]];


(* ::Subsection:: *)
(*F4 ZZZ*)


FplotZZZ[s,mh2,mhc]


b=250;
c= 400;
label0 = Text[Style["m_H=" <> ToString[b] <> " TeV", FontSize -> 14, FontFamily -> "Arial", {1.3, -10}]];
plot1 = Plot[
		{Re[FZZZ[x*MZ^2,mh1,b,c,\[Alpha]1,\[Alpha]2max,\[Alpha]3max]]/.SmpChanger/.Params,
		Im[FZZZ[x*MZ^2,mh1,b,c,\[Alpha]1,\[Alpha]2max,\[Alpha]3max]]/.SmpChanger/.Params}
		,{x,6,40},
  GridLines -> Automatic,
  ImageSize->400,
  PlotRange->{-0.3*10^(-4),0.3*10^(-4)},
  Epilog -> label0,
  AxesLabel -> {"q^2/MZ^2", "f4"}];
Show[plot1]


b=250;
c= 400;
label0 = Text[Style["m_H=" <> ToString[b] <> " TeV", FontSize -> 14, FontFamily -> "Arial", {1.3, -10}]];
plotZWW = Plot[
		{Re[FZWW[x*MW^2,mh1,b,c,c,\[Alpha]1,\[Alpha]2max,\[Alpha]3max]]//.SmpChanger//.Params,
		Im[FZWW[x*MW^2,mh1,b,c,c,\[Alpha]1,\[Alpha]2max,\[Alpha]3max]]//.SmpChanger//.Params}
		,{x,10,100},
  GridLines -> Automatic,
  ImageSize->400,
  PlotRange->{-3*10^(-4),3*10^(-4)},
  Epilog -> label0,
  AxesLabel -> {"q^2/MW^2", "f4_{ZWW}"}];
Show[plotZWW]


(* ::Text:: *)
(*2d graph of prefactor*)


(* ::Text:: *)
(*2 d graph from q m2*)


(*
xmin=0.2;  ymin=0.2;
xmax=3;    ymax=3;

plotZZZm2s = ContourPlot[
  Abs[FZZZ[x,m1,y,Sqrt[y^2+v^2],prefmax]], 
  {x, xmin, xmax}, {y, ymin, ymax},
  ColorFunction -> GraphTheme,
  ScalingFunctions->"Log", 
  Contours -> ContoursNumber,
  PlotPoints->PointNumber,
  LabelStyle -> Directive[FontSize -> 14, FontFamily -> "Helvetica"],
  PlotRange->All,
  FrameLabel -> {"q, TeV", "\!\(\*TemplateBox[<|\"boxes\" -> FormBox[SubscriptBox[StyleBox[\"m\", \"TI\"], \"2\"], TraditionalForm], \"errors\" -> {}, \"input\" -> \"m_2\", \"state\" -> \"Boxes\"|>,\n\"TeXAssistantTemplate\"]\), TeV"},
  PlotLegends -> Automatic
];
    
  conplot = ContourPlot[{Abs[FZZZ[x,m1,y,Sqrt[y^2+v^2],prefmax]] == constraitF4Z300,Abs[FZZZ[x,m1,y,Sqrt[y^2+v^2],prefmax]] == constraitF4Z3000}, {x, xmin, xmax}, {y, ymin, ymax},
  ContourStyle -> {Directive[Black, Thickness[0.005]], Directive[Darker[Blue], Thickness[0.003]]},
  PlotLegends -> SwatchLegend[{Black,Darker[Blue]}, {"300", "3000"}, LegendLayout -> "Column", LegendFunction -> "Panel",LegendLabel->"\!\(\*TemplateBox[<|\"boxes\" -> FormBox[RowBox[{\"\[Integral]\", RowBox[{StyleBox[\"L\", \"TI\"], StyleBox[\"d\", \"TI\"], StyleBox[\"t\", \"TI\"]}], \",\"}], TraditionalForm], \"errors\" -> {}, \"input\" -> \"\\\\int{Ldt},\", \"state\" -> \"Boxes\"|>,\n\"TeXAssistantTemplate\"]\)  pb\!\(\*TemplateBox[<|\"boxes\" -> FormBox[SuperscriptBox[\"\[Null]\", \"-1\"], TraditionalForm], \"errors\" -> {}, \"input\" -> \"^{-1}\", \"state\" -> \"Boxes\"|>,\n\"TeXAssistantTemplate\"]\)"]];
  
  plotZZZm2s=Show[plotZZZm2s,conplot,ImageSize->800]

If[SavePDF == 1,
Export[SavePath <> "ZZZ_f4_m2a3.pdf", plotZZZm2s];
]
*)






(* ::Text:: *)
(*F4ZZZ from m2 for different s*)


p=500
Print["Start graph F4Z(m2) for different q"]
xmin = 408.5*k;     xmax = 409*k;
ymin = 10^(-8); ymax = 10^(-3);
data = Table[{x,FplotZZZ[p^2,x,1]}, {x,LogRange1[xmin,xmax,PointNumber]}]//N;
l = ListLogLogPlot[data,
    PlotMarkers -> None,
    Ticks->Automatic,
    LabelStyle -> Directive[FontSize -> 14, FontFamily -> "Helvetica"],
    BaseStyle -> {FontSize -> 18},
    ImageSize->600,
    Joined->True,
    GridLines -> Automatic,
    PlotRange -> All]



FplotZZZ[p^2,408.82,1]
(*\:0440\:0430\:0437\:0440\:044b\:0432\:0430 \:043d\:0435\:0442, \:0430\:0440\:0442\:0435\:0444\:0430\:043a\:0442 \:0432\:044b\:0447\:0438\:0441\:043b\:0435\:043d\:0438\:044f looptools*)


(* ::Text:: *)
(*Start graph F4Z(m2) for different q*)


xmin = 126*k;     xmax = 500*k;
ymin = 10^(-6); ymax =0.5 10^(-3);
slist = {200,250,300,350,500}*k;
plots = {};
extr={};
hiConstr={};
(*slistextr = LogRange1[0.05, 6, 6];
Do[
     data = Table[{x, N[Abs[FZZZ[slistextr[[i]], m1, x, Sqrt[x^2 + v^2], prefmax]]]}, {x, LogRange1[xmin, xmax, PointNumber]}];
     valY = Max[data[[All, 2]]];
  	For[j = 1, j <= Length[data], j++,
   	If[valY == data[[j, 2]],
     	valX = data[[j, 1]]];];
  	AppendTo[extr, {valX, valY}]
  	, {i, Length[slistextr]}];*)

colors = GraphTheme /@ Rescale[Range[Length[slist]], {1, Length[slist]}];
legend = SwatchLegend[colors, slist, LegendLayout -> "Column", LegendLabel -> "q, GeV", LegendFunction -> Framed];
Do[
     data = Table[{x, FplotZZZ[slist[[i]]^2, x, 1]}, {x, LogRange1[xmin, xmax, PointNumber]}];
     
     level = constraitF4Z3000;
		crossings = Select[
		  Partition[data, 2, 1],
		  (#[[1, 2]] - level) (#[[2, 2]] - level) < 0 &
		];

		getCrossX[{p1_, p2_}] := Module[{x1, x2, y1, y2},
		  {{x1, y1}, {x2, y2}} = {p1, p2};
		  x1 + (level - y1) (x2 - x1)/(y2 - y1)
		];
		roots = getCrossX /@ crossings;
    
      (*first graph creats inveroment*)
     If[i == 1,
      p = ListLogLogPlot[data,
      PlotMarkers -> None,
      Joined -> True,
      
      PlotStyle -> {colors[[i]], Thickness[0.005]},
      PlotLegends -> legend,
      LabelStyle -> Directive[FontSize -> 14, FontFamily -> "Helvetica"],
      BaseStyle -> {FontSize->14},
      AxesLabel -> {Text[Style["\!\(\*TemplateBox[<|\"boxes\" -> FormBox[RowBox[{SubscriptBox[StyleBox[\"m\", \"TI\"], \"2\"], \",\"}], TraditionalForm], \"errors\" -> {}, \"input\" -> \"m_2,\", \"state\" -> \"Boxes\"|>,\n\"TeXAssistantTemplate\"]\) GeV", FontSize -> 16]],
         Text[Style["\!\(\*TemplateBox[<|\"boxes\" -> FormBox[SubsuperscriptBox[StyleBox[\"f\", \"TI\"], \"4\", StyleBox[\"Z\", \"TI\"]], TraditionalForm], \"errors\" -> {}, \"input\" -> \"|f_ 4^Z|\", \"state\" -> \"Boxes\"|>,\n\"TeXAssistantTemplate\"]\)", FontSize -> 16]]},
      
      GridLines -> None,
      PlotRange -> {{xmin, xmax}, {ymin, ymax}}
      ];,
      (*if not first graph; no need for legend*)
      p = ListLogLogPlot[data,
            PlotMarkers -> None,
            Joined -> True,
            PlotStyle -> {colors[[i]],Thickness[0.005]},
            PlotRange -> {{xmin, xmax}, {ymin, ymax}}]];
    AppendTo[plots, p];
    	
	If[Length[roots] == 2,
		{r1, r2} = Sort[roots];
		fillPlot = ListLogLogPlot[
			  {
			    Table[{x, constraitF4Z3000}, {x, LogRange1[r1, r2, 1000]}],
			    Table[{x, ymin}, {x, LogRange1[r1, r2, 1000]}]
			  },
			  PlotStyle->{Dashed,colors[[i]]},
			  Filling -> {1 -> {2}},
			  FillingStyle -> Directive[Opacity[0.025,colors[[i]]], colors[[i]]],
			  PlotRange -> {{xmin, xmax}, {ymin, ymax}}
		];
		AppendTo[plots, fillPlot];
	];
    ,
    {i, Length[slist]}
  ];
legend1 = SwatchLegend[{Red, Blue}, {"300", "3000"}, 
		LegendLayout -> "Column", 
		LegendFunction -> Framed, 
		LabelStyle -> Directive[FontSize -> 14, FontFamily -> "Helvetica"],
		LegendLabel -> Text[Style["\!\(\*TemplateBox[<|\"boxes\" -> FormBox[RowBox[{\"\[Integral]\", StyleBox[\"L\", \"TI\"], StyleBox[\"d\", \"TI\"], StyleBox[\"t\", \"TI\"]}], TraditionalForm], \"errors\" -> {}, \"input\" -> \"\\\\int Ldt\", \"state\" -> \"Boxes\"|>,\n\"TeXAssistantTemplate\"]\), fb\!\(\*TemplateBox[<|\"boxes\" -> FormBox[SuperscriptBox[\"\[Null]\", \"-1\"], TraditionalForm], \"errors\" -> {}, \"input\" -> \"{}^{-1}\", \"state\" -> \"Boxes\"|>,\n\"TeXAssistantTemplate\"]\)", 
		FontSize -> 14]]];

AppendTo[plots,
LogLogPlot[{constraitF4Z300,constraitF4Z3000}, {x, xmin, xmax}, 
	Filling -> Top,
	PlotStyle->{Darker[Red],Darker[Blue]}, 
	FillingStyle -> Directive[Opacity[0.035]],
	PlotLegends->legend1,
	PlotRange -> {{xmin, xmax}, {ymin, ymax}}]
];
Sh = Show[plots ,ImageSize -> 1000]
If[SavePDF == 1,
  Export[SavePath <> "ZZZ_f4_m2.pdf", Sh]]


(*
(* f4ZZZ 2d (a2,m2)*)
p1 = 0.5;
alpha2list = {Pi/50, Pi/20, Pi/10, Pi/4, Pi/3, 0.99*Pi/2}//N;

If[Length[data]==0||ReCalculateTable == 1,
	data={};
	Do[d = Flatten[Table[
      {x, y, Abs[FZZZ[p1, m1, x, Sqrt[x^2+v^2],prefactor[\[Alpha]1,a2,y]]]},
      {x, 0.1, 0.7, 0.6/PointNumber}, {y, 0, \[Pi]/2,\[Pi]/2/PointNumber }],1];
  AppendTo[data, d],
  {a2, alpha2list}];]


plots = {};

data[1,All,{1,2,3}];
Do[
  p = ListContourPlot[data[[i, All, {1, 2, 3}]],
    ColorFunction -> GraphTheme, (* \:0443\:0441\:0442\:0430\:043d\:0430\:0432\:043b\:0438\:0432\:0430\:0435\:043c \:043e\:0434\:0438\:043d\:0430\:043a\:043e\:0432\:0443\:044e \:0446\:0432\:0435\:0442\:043e\:0432\:0443\:044e \:0441\:0445\:0435\:043c\:0443 *)
    Contours -> ContoursNumber,
    ImageSize -> 400,
    FrameLabel -> {"Subscript[m, 2]", "\[Alpha]3"},
    Epilog->Text[Style["\[Alpha]2 = " <> ToString[alpha2list[[i]]],12], {0.55, 1.3},Background->LightRed],
    PlotRange->All,
    PlotLegends -> Automatic];
  AppendTo[plots, p],
  {i, Length[data]}
];

l = Row[plots]
If[SavePDF == 1,
Export[SavePath <> "ZZZ_f4_2Dseries.pdf", l]]*)


(*f4ZZZ from s for optimal m2*)
(*b=0.3;
xmin=0.1;    xmax=0.6;
ymin=10^(-10);ymax=10^(-3);
plots={};
label2 = Text[Style["\!\(\*TemplateBox[<|\"boxes\" -> FormBox[SubscriptBox[StyleBox[\"m\", \"TI\"], \"2\"], TraditionalForm], \"errors\" -> {}, \"input\" -> \"m_ 2\", \"state\" -> \"Boxes\"|>,\n\"TeXAssistantTemplate\"]\)=" <> ToString[b] <> " TeV", FontSize -> 14, FontFamily -> "Arial", FontWeight -> Bold, 
       Background -> LightGray], {0.4, 0.0002}];
data = Table[{x, N[Abs[FZZZ[x, m1, b, Sqrt[b^2+v^2],prefmax]]]},  {x,LogRange1[xmin,xmax,PointNumber]}] // N;
l = ListLogLogPlot[data,
Joined->True,
LabelStyle -> Directive[FontSize -> 14, FontFamily -> "Helvetica"],
PlotMarkers -> None,
Epilog -> label2, 
 AxesLabel -> {Text[Style["\!\(\*TemplateBox[<|\"boxes\" -> FormBox[RowBox[{StyleBox[\"q\", \"TI\"], \",\"}], TraditionalForm], \"errors\" -> {}, \"input\" -> \"q,\", \"state\" -> \"Boxes\"|>,\n\"TeXAssistantTemplate\"]\) TeV", FontSize -> 16]],
         Text[Style["\!\(\*TemplateBox[<|\"boxes\" -> FormBox[TemplateBox[{RowBox[{SubsuperscriptBox[StyleBox[\"f\", \"TI\"], \"4\", StyleBox[\"Z\", \"TI\"]]}]}, \"Abs\"], TraditionalForm], \"errors\" -> {}, \"input\" -> \"|f_ 4^Z|\", \"state\" -> \"Boxes\"|>,\n\"TeXAssistantTemplate\"]\)", FontSize -> 16]]},PlotRange -> {{xmin,xmax},{ymin,ymax}},
PlotStyle->{Darker[Green],Thickness[0.005]},
GridLines -> Automatic];

AppendTo[plots,l];
legend1 = SwatchLegend[{Red, Blue}, {"300", "3000"}, LegendLayout -> "Column", LegendFunction -> "Panel", LegendLabel -> "\!\(\*TemplateBox[<|\"boxes\" -> FormBox[RowBox[{\"\[Integral]\", RowBox[{StyleBox[\"L\", \"TI\"], StyleBox[\"d\", \"TI\"], StyleBox[\"t\", \"TI\"]}], \", \", SuperscriptBox[\"fb\", \"-1\"]}], TraditionalForm], \"errors\" -> {}, \"input\" -> \"\\\\int{Ldt},\\\\,\\\\text{fb}^{-1}\", \"state\" -> \"Boxes\"|>,\n\"TeXAssistantTemplate\"]\)"];
AppendTo[plots,
LogLogPlot[{constraitF4Z300,constraitF4Z3000}, {x, xmin, xmax}, Filling -> Top,PlotStyle->{Red,Blue}, FillingStyle -> Directive[Opacity[0.05]],PlotLegends->legend1,PlotRange -> {{xmin, xmax}, {ymin, ymax}}]
];

Sh=Show[plots, ImageSize->600]

If[SavePDF == 1,
Export[SavePath <> "ZZZ_s_optima.pdf", Sh]]*)


(*F4ZZZ from s for differnet mass*)
(*mlist = {0.1,0.2,0.3,0.5,1,1.5};
plots={};
xmin = 0.1;     xmax = 5;
ymin = 10^(-6); ymax = 10^(-3);
colors = GraphTheme /@ Rescale[Range[Length[mlist]], {1, Length[mlist]}];
legend =SwatchLegend[colors,mlist,LegendLayout->"Column",LegendLabel->"\!\(\*TemplateBox[<|\"boxes\" -> FormBox[SubscriptBox[StyleBox[\"m\", \"TI\"], \"2\"], TraditionalForm], \"errors\" -> {}, \"input\" -> \"m_2\", \"state\" -> \"Boxes\"|>,\n\"TeXAssistantTemplate\"]\), TeV",LegendFunction->"Panel"];
Do[
   data = Table[{x, N[Abs[FZZZ[x, m1, mlist[[i]], Sqrt[mlist[[i]]^2+v^2],prefmax]]]}, {x,LogRange1[xmin,xmax,PointNumber]}];
   If[i ==1,(*\:0443\:0441\:043b\:043e\:0432\:0438\:0435 = \:043a\:043e\:0441\:0442\:044b\:043b\:044c \:0434\:043b\:044f \:0432\:044b\:0432\:043e\:0434\:0430 \:043b\:0435\:0433\:0435\:043d\:0434\:044b*)
   p = ListLogLogPlot[data,
       PlotMarkers -> None,
       Joined->True,
       LabelStyle -> Directive[FontSize -> 14, FontFamily -> "Helvetica"],
       PlotStyle-> {colors[[i]],Thickness[0.005]},
       PlotLegends->legend, 
        AxesLabel -> {Text[Style["\!\(\*TemplateBox[<|\"boxes\" -> FormBox[RowBox[{StyleBox[\"q\", \"TI\"], \",\"}], TraditionalForm], \"errors\" -> {}, \"input\" -> \"q,\", \"state\" -> \"Boxes\"|>,\n\"TeXAssistantTemplate\"]\) TeV", FontSize -> 16]],
         Text[Style["\!\(\*TemplateBox[<|\"boxes\" -> FormBox[TemplateBox[{RowBox[{SubsuperscriptBox[StyleBox[\"f\", \"TI\"], \"4\", StyleBox[\"Z\", \"TI\"]]}]}, \"Abs\"], TraditionalForm], \"errors\" -> {}, \"input\" -> \"|f_ 4^Z|\", \"state\" -> \"Boxes\"|>,\n\"TeXAssistantTemplate\"]\)", FontSize -> 16]]},
         PlotRange -> {{xmin, xmax}, {ymin, ymax}},
       ImageSize -> 400,
       GridLines -> Automatic];,
   p = ListLogLogPlot[data,
       PlotMarkers -> None,
       Joined->True,
       PlotStyle-> {colors[[i]],Thickness[0.005]},
       PlotRange -> {{xmin, xmax}, {ymin, ymax}}]];
  AppendTo[plots, p],
  {i, Length[mlist]}
];

legend1 = SwatchLegend[{Red, Blue}, {"300", "3000"}, LegendLayout -> "Column", LegendFunction -> "Panel", LabelStyle -> Directive[FontSize -> 14, FontFamily -> "Helvetica"],
LegendLabel -> Text[Style["\!\(\*TemplateBox[<|\"boxes\" -> FormBox[RowBox[{\"\[Integral]\", StyleBox[\"L\", \"TI\"], StyleBox[\"d\", \"TI\"], StyleBox[\"t\", \"TI\"]}], TraditionalForm], \"errors\" -> {}, \"input\" -> \"\\\\int Ldt\", \"state\" -> \"Boxes\"|>,\n\"TeXAssistantTemplate\"]\), fb\!\(\*TemplateBox[<|\"boxes\" -> FormBox[SuperscriptBox[\"\[Null]\", \"-1\"], TraditionalForm], \"errors\" -> {}, \"input\" -> \"{}^{-1}\", \"state\" -> \"Boxes\"|>,\n\"TeXAssistantTemplate\"]\)", FontSize -> 14]]];
AppendTo[plots,
LogLogPlot[{constraitF4Z300,constraitF4Z3000}, {x, xmin, xmax}, Filling -> Top, FillingStyle -> Directive[Opacity[0.05]],PlotStyle->{Red,Blue},PlotLegends->legend1,PlotRange -> {{xmin, xmax}, {ymin, ymax}}]
];
Sh=Show[plots, ImageSize->800]

If[SavePDF == 1,
Export[SavePath <> "ZZZ_f4_s.pdf", Sh]]      *)


(* ::Subsection::Closed:: *)
(*ZWW f4*)


mh2 = 250*k;
mhc = 400*k;
f4teorFile  = "buffer/f4_teorZWW.mx";
F4teorZ = Get[f4teorFile];
	F4teorZ = F4teorZ/.{SMP["m_q"]->0, mZ->MZ,SUNFDelta[SUNFIndex[Col1], SUNFIndex[Col2]]->1}//.AngleChanger//.Params//.SmpChanger//.PaveToLooptools/.{FeynCalc`CA->3};
	f4Z[sX_] := F4teorZ/.{s->sX};
	
	
f4teorFile  = "buffer/f4_teorZZZ.mx";
F4teorW = Get[f4teorFile];
	F4teorW = F4teorW/.{SMP["m_q"]->0, mZ->MZ,SUNFDelta[SUNFIndex[Col1], SUNFIndex[Col2]]->1}//.AngleChanger//.Params//.SmpChanger//.PaveToLooptools/.{FeynCalc`CA->3};
	f4W[sX_] := F4teorW/.{s->sX};
	
	
plotF4hat = LogPlot[{Abs[f4Z[x^2]],Abs[f4W[x^2]]},{x,200,1000},
  GridLines -> None,
  ImageSize->600,
  PlotRange -> All,
  PlotLegends->{"z","w"},
  Frame -> True,
  BaseStyle -> {FontSize -> 12}
  ]
  Export["TESTGRAPHf4.png", plotF4hat]


(*ZWW*)
(*
b=0.5
Print["q = ",b,", m1 = ",m1,", m2 = ",m2,", m3 = ",m3,", mhc = ",mc,", alpha1 = ",\[Alpha]1,", alpha2 = ",\[Alpha]2,", alpha3 = ",\[Alpha]3]
plot1 = LogPlot[  FplotZWW[x,m1,m2,mc],{x,0.2,13},
  GridLines -> Automatic,
  ImageSize->600,
  FrameLabel -> {"\!\(\*TemplateBox[<|\"boxes\" -> FormBox[SubscriptBox[StyleBox[\"q\", \"TI\"], \"2\"], TraditionalForm], \"errors\" -> {}, \"input\" -> \"q_2\", \"state\" -> \"Boxes\"|>,\n\"TeXAssistantTemplate\"]\)", "\!\(\*TemplateBox[<|\"boxes\" -> FormBox[SubsuperscriptBox[StyleBox[\"f\", \"TI\"], \"4\", RowBox[{StyleBox[\"Z\", \"TI\"], StyleBox[\"W\", \"TI\"], StyleBox[\"W\", \"TI\"]}]], TraditionalForm], \"errors\" -> {}, \"input\" -> \"f_4^{ZWW}\", \"state\" -> \"Boxes\"|>,\n\"TeXAssistantTemplate\"]\)"},
  PlotRange -> All,
  Frame -> True,
  BaseStyle -> {FontSize -> 12}
]
  
plotF4hat = LogPlot[FZWWbar[b,m1,x,Sqrt[x^2+v^2]],{x,0,4},
  GridLines -> Automatic,
  ImageSize->600,
  PlotRange -> All,
  Frame -> True,
  BaseStyle -> {FontSize -> 12},
  FrameLabel -> {"\!\(\*TemplateBox[<|\"boxes\" -> FormBox[SubscriptBox[StyleBox[\"m\", \"TI\"], \"2\"], TraditionalForm], \"errors\" -> {}, \"input\" -> \"m_2\", \"state\" -> \"Boxes\"|>,\n\"TeXAssistantTemplate\"]\)", "f4ZWW_hat"}]
  

plot3 = LogPlot[  FplotZWW[b,m1,x,Sqrt[x^2+v^2]],{x,0,4},
  GridLines -> Automatic,
  ImageSize->600,
  PlotRange -> All,
  Frame -> True,
  BaseStyle -> {FontSize -> 12},
  FrameLabel -> {"\!\(\*TemplateBox[<|\"boxes\" -> FormBox[SubscriptBox[StyleBox[\"m\", \"TI\"], \"2\"], TraditionalForm], \"errors\" -> {}, \"input\" -> \"m_2\", \"state\" -> \"Boxes\"|>,\n\"TeXAssistantTemplate\"]\)", "f4ZWW"}]
  
If[SavePDF == 1,
Export["./ZZZ/graphs/ZWW_f4_q2.pdf", plot1]
Export["./ZZZ/graphs/ZWW_f4hat_m2.pdf", plotF4hat]
Export["./ZZZ/graphs/ZWW_f4_m2.pdf", plot3]]
*)


(* ::Text:: *)
(*1d: f4zww  from q (m2 -diff colors)*)


xmin = 0;
xmax = 3;
dataQ = Table[
	{x,y, FplotZWW[x,y,Sqrt[y^2+v^2]]},
   {y, {1.5,1.7,2,3,4,5,7.5,10}},{x,LogRange1[xmin,xmax,PointNumber]}
   ];
col = dataQ[[All,1,2]] (*znachenie parametrov v graph*)
ymin = Min[dataQ[[All,All,3]]];
ymax = Max[dataQ[[All,All,3]]];


colors = GraphTheme /@ Rescale[Range[Length[col]], {1, Length[col]}];
legend =SwatchLegend[colors,col,LegendLayout->"Column",LegendLabel->"\!\(\*TemplateBox[<|\"boxes\" -> FormBox[SubscriptBox[StyleBox[\"m\", \"TI\"], \"2\"], TraditionalForm], \"errors\" -> {}, \"input\" -> \"m_2\", \"state\" -> \"Boxes\"|>,\n\"TeXAssistantTemplate\"]\), TeV",LegendFunction->"Panel"];
plots ={};
Do[
	If[i ==1,(*\:0443\:0441\:043b\:043e\:0432\:0438\:0435 = \:043a\:043e\:0441\:0442\:044b\:043b\:044c \:0434\:043b\:044f \:0432\:044b\:0432\:043e\:0434\:0430 \:043b\:0435\:0433\:0435\:043d\:0434\:044b*)
    p = ListLogPlot[dataQ[[i,All,{1,3}]], (*\:0412\:044b\:0432\:043e\:0434\:0438\:043c \:0433\:0440\:0430\:0444\:0438\:043a \:0441 \:0434\:0430\:0442\:043e\:0439 \:0434\:043b\:044f i\:043e\:0433\:043e \:043f\:0430\:0440\:0430\:043c\:0435\:0442\:0440\:0430*)
        LabelStyle -> Directive[FontSize -> 14, FontFamily -> "Helvetica"],
        PlotStyle-> {colors[[i]],Thickness[0.005]},
        FrameLabel -> {Text[Style["q, TeV", FontSize -> 14]],
         Text[Style["\!\(\*TemplateBox[<|\"boxes\" -> FormBox[SubsuperscriptBox[StyleBox[\"f\", \"TI\"], \"4\", StyleBox[\"Z\", \"TI\"]], TraditionalForm], \"errors\" -> {}, \"input\" -> \"f_ 4^Z\", \"state\" -> \"Boxes\"|>,\n\"TeXAssistantTemplate\"]\)", FontSize -> 14]]},
        PlotMarkers -> None,
        PlotLegends->legend,
        PlotRange -> {{xmin, xmax}, {ymin, ymax}},
        Joined->True,
        GridLines -> Automatic,
        PlotRange ->Full ,
        Frame -> True,
        BaseStyle -> {FontSize -> 12}];,
    p = ListLogPlot[dataQ[[i,All,{1,3}]], (*\:0412\:044b\:0432\:043e\:0434\:0438\:043c \:0433\:0440\:0430\:0444\:0438\:043a \:0441 \:0434\:0430\:0442\:043e\:0439 \:0434\:043b\:044f i\:043e\:0433\:043e \:043f\:0430\:0440\:0430\:043c\:0435\:0442\:0440\:0430*)
        PlotStyle-> {colors[[i]],Thickness[0.005]}, (* \:0443\:0441\:0442\:0430\:043d\:0430\:0432\:043b\:0438\:0432\:0430\:0435\:043c \:0446\:0432\:0435\:0442 *)
        Joined->True,
        PlotMarkers -> None,
        PlotRange -> Full];
    ];
  AppendTo[plots, p], (*\:043f\:0440\:0438\:0441\:043e\:0435\:0434\:0438\:043d\:044f\:044e \:043d\:0430\:0431\:043e\:0440 \:0442\:043e\:0447\:0435\:043a \:0432 \:0433\:0440\:0430\:0444\:0438\:043a\:0435 \:043a \:043c\:0430\:0441\:0441\:0438\:0432\:0443 \:0433\:0440\:0430\:0444\:0438\:043a\:043e\:0432*)
  {i, Length[col]}
];

legend1 = SwatchLegend[{Red, Blue}, {"300", "3000"}, LegendLayout -> "Column", LegendFunction -> "Panel", LabelStyle -> Directive[FontSize -> 14, FontFamily -> "Helvetica"],
LegendLabel -> Text[Style["\!\(\*TemplateBox[<|\"boxes\" -> FormBox[RowBox[{\"\[Integral]\", StyleBox[\"L\", \"TI\"], StyleBox[\"d\", \"TI\"], StyleBox[\"t\", \"TI\"]}], TraditionalForm], \"errors\" -> {}, \"input\" -> \"\\\\int Ldt\", \"state\" -> \"Boxes\"|>,\n\"TeXAssistantTemplate\"]\), fb\!\(\*TemplateBox[<|\"boxes\" -> FormBox[SuperscriptBox[\"\[Null]\", \"-1\"], TraditionalForm], \"errors\" -> {}, \"input\" -> \"{}^{-1}\", \"state\" -> \"Boxes\"|>,\n\"TeXAssistantTemplate\"]\)", FontSize -> 14]]];
AppendTo[plots,
LogPlot[{constraitF4Z300,constraitF4Z3000}, {x, xmin, xmax}, Filling -> Top,  FillingStyle -> Directive[Opacity[0.05]],PlotStyle->{Red,Blue},PlotLegends->legend1,PlotRange -> {{xmin, xmax}, {ymin, ymax}}]
];

P = Show[plots, ImageSize -> 800]


If[SavePDF == 1,
Export[SavePath <> "ZWW_f4_m2fromQ2.pdf", P]]


(* ::Text:: *)
(*2d:  f4zww from q m2*)


(*\:0421\:0442\:0430\:0440\:044b\:0435 \:0434\:0432\:0443\:0445\:043f\:0430\:0440\:0430\:043c \:0433\:0440\:0430\:0444\:0438\:043a\:0438*)

p1 = 0.5
b = 0.4
(*plot4 = ContourPlot[
  prefactor[\[Alpha]1,x,y], 
  {x, -\[Pi]/2, \[Pi]/2}, {y, 0, \[Pi]/2},
  ColorFunction -> GraphTheme, 
  Contours -> ContoursNumber,
  PlotPoints->PointNumber,
  LabelStyle -> Directive[FontSize -> 14, FontFamily -> "Helvetica"],
  ImageSize->600,
  FrameLabel -> {Text[Style["\!\(\*TemplateBox[<|\"boxes\" -> FormBox[SubscriptBox[\"\[Alpha]\", \"2\"], TraditionalForm], \"errors\" -> {}, \"input\" -> \"\\\\alpha_2\", \"state\" -> \"Boxes\"|>,\n\"TeXAssistantTemplate\"]\)", FontSize -> 16]],
         Text[Style["\!\(\*TemplateBox[<|\"boxes\" -> FormBox[SubscriptBox[\"\[Alpha]\", \"3\"], TraditionalForm], \"errors\" -> {}, \"input\" -> \"\\\\alpha_3\", \"state\" -> \"Boxes\"|>,\n\"TeXAssistantTemplate\"]\)", FontSize -> 16]]},
  PlotLegends -> Automatic,
  PlotRange -> All (* \:041f\:043e\:043a\:0430\:0436\:0438 \:0432\:0441\:0435 \:0437\:043d\:0430\:0447\:0435\:043d\:0438\:044f \:043d\:0430 \:0433\:0440\:0430\:0444\:0438\:043a\:0435 *)
]
*)
plot5 = ContourPlot[
  FplotZWW[x,m1,y,Sqrt[y^2+v^2]], 
  {x, 0.5, 3}, {y, 0.5, 3},
  ColorFunction -> GraphTheme, 
  LabelStyle -> Directive[FontSize -> 14, FontFamily -> "Helvetica"],
  ScalingFunctions->"Log",
  Contours -> ContoursNumber,
  Frame->True,
  PlotPoints->PointNumber,
  ImageSize->600,
  PlotRange->All,
  FrameLabel -> {Text[Style["\!\(\*TemplateBox[<|\"boxes\" -> FormBox[RowBox[{StyleBox[\"q\", \"TI\"], \",\"}], TraditionalForm], \"errors\" -> {}, \"input\" -> \"q,\", \"state\" -> \"Boxes\"|>,\n\"TeXAssistantTemplate\"]\) TeV", FontSize -> 16]],
         Text[Style["\!\(\*TemplateBox[<|\"boxes\" -> FormBox[RowBox[{SubscriptBox[StyleBox[\"m\", \"TI\"], \"2\"], \",\"}], TraditionalForm], \"errors\" -> {}, \"input\" -> \"m_2,\", \"state\" -> \"Boxes\"|>,\n\"TeXAssistantTemplate\"]\) TeV", FontSize -> 16]]},
  PlotLegends -> Automatic
];
conplot = ContourPlot[{FplotZWW[x,m1,y,Sqrt[y^2+v^2]] == conhat300,FplotZWW[x,m1,y,Sqrt[y^2+v^2]] == conhat3000}, {x, 0.5, 3}, {y, 0.5, 3},
  ContourStyle -> {Directive[Black, Thickness[0.007]], Directive[Darker[Blue], Thickness[0.003]]},
  PlotLegends -> SwatchLegend[{Red, Blue}, {"300", "3000"}, LegendLayout -> "Column", LegendFunction -> "Panel", LabelStyle -> Directive[FontSize -> 14, FontFamily -> "Helvetica"],
LegendLabel -> Text[Style["\!\(\*TemplateBox[<|\"boxes\" -> FormBox[RowBox[{\"\[Integral]\", StyleBox[\"L\", \"TI\"], StyleBox[\"d\", \"TI\"], StyleBox[\"t\", \"TI\"]}], TraditionalForm], \"errors\" -> {}, \"input\" -> \"\\\\int Ldt\", \"state\" -> \"Boxes\"|>,\n\"TeXAssistantTemplate\"]\), fb\!\(\*TemplateBox[<|\"boxes\" -> FormBox[SuperscriptBox[\"\[Null]\", \"-1\"], TraditionalForm], \"errors\" -> {}, \"input\" -> \"{}^{-1}\", \"state\" -> \"Boxes\"|>,\n\"TeXAssistantTemplate\"]\)", FontSize -> 14]]]];  
  Show[plot5,conplot,ImageSize->800]
 
If[SavePDF == 1,
Export[SavePath <> "ZWW_prefactor_a2a3.pdf", plot4];
Export[SavePath <> "ZWW_2D_m2q.pdf", plot5]]


(*(*\:0434\:0432\:0443\:0445\:043f\:0430\:0440\:0430\:043c \:0433\:0440\:0430\:0444\:0438\:043a\:0438 \:043f\:0440\:0438 \:0440\:0430\:0437\:043d\:044b\:0445 mc*)

If[Length[dataA2]==0||ReCalculateTable == 1,
	dataA2=Table[{x,y,a2,FplotZWW[p1,m1,x,y]},
	{x, xmin, xmax, (xmax-xmin)/PointNumber},{y, 0.1, \[Pi]/2, \[Pi]/2/PointNumber},{a2, 0.1, \[Pi]/2, 0.2}
	];
];*)
(*Flatten[dataA2[[All,All,1,{1,2,4}]],{1,2}];*)(*1-3 \:0441\:0442\:043e\:043b\:0431\:0438\:043a - \:0445 \:0443 \:0437\:0435\:0442, \:0447\:0435\:0442\:0432\:0435\:0440\:0442\:044b\:0439 \:0441\:0442\:043e\:043b\:0431\:0438\:043a \:0434\:043e\:0441\:0442\:0443\:043f \:043a \:043a\:043e\:043d\:043a\:0440\:0435\:0442\:043d\:043e\:0439 \:044f\:0447\:0435\:0439\:043a\:0435 \:0441 \:0434\:0430\:043d\:043d\:044b\:043c\:0438.*)(*Flatten \:043f\:043e\:043d\:0438\:0436\:0430\:0435\:0442 \:0440\:0430\:0437\:043c\:0435\:0440\:043d\:043e\:0441\:0442\:044c \:0442\:0430\:0431\:043b\:0438\:0446\:044b, \:043e\:0441\:0442\:0430\:0432\:043b\:044f\:044f \:0437\:0430\:0434\:0430\:043d\:043d\:044b\:0435 \:043e\:0441\:0438*)


(*plots = {};
param = dataA2[[1,1,All,3]]
Do[
    p = ListContourPlot[
    Flatten[dataA2[[All,All,i,{1,2,4}]],{1,2}], (*\:0412\:044b\:0432\:043e\:0434\:0438\:043c \:0433\:0440\:0430\:0444\:0438\:043a \:0441 \:0434\:0430\:0442\:043e\:0439 \:0434\:043b\:044f i\:043e\:0433\:043e \:043f\:0430\:0440\:0430\:043c\:0435\:0442\:0440\:0430*)
    FrameLabel -> {"m2", "\[Alpha]3"},
    Contours -> ContoursNumber,
    ColorFunction -> GraphTheme, (* \:0443\:0441\:0442\:0430\:043d\:0430\:0432\:043b\:0438\:0432\:0430\:0435\:043c \:043e\:0434\:0438\:043d\:0430\:043a\:043e\:0432\:0443\:044e \:0446\:0432\:0435\:0442\:043e\:0432\:0443\:044e \:0441\:0445\:0435\:043c\:0443 *)
    GridLines -> Automatic,
    PlotRange -> All,
    PlotLegends -> Automatic,
    Frame -> True,
    Epilog->Text[Style["\[Alpha]2 = " <> ToString[param[[i]]],14], {3, 1.4},Background->LightYellow],
    ImageSize->400];
  AppendTo[plots, p], (*\:043f\:0440\:0438\:0441\:043e\:0435\:0434\:0438\:043d\:044f\:044e \:043d\:0430\:0431\:043e\:0440 \:0442\:043e\:0447\:0435\:043a \:0432 \:0433\:0440\:0430\:0444\:0438\:043a\:0435 \:043a \:043c\:0430\:0441\:0441\:0438\:0432\:0443 \:0433\:0440\:0430\:0444\:0438\:043a\:043e\:0432*)
  {i, Length[param]}
];
plot2d = Row[plots]

  
If[SavePDF == 1,
Export["./ZZZ/graphs/ZWW_2Dseries_m2a3.pdf", plot2d]]*)


(* ::Subsection::Closed:: *)
(*Graph F4ZWW no angels included*)


(*ZWW*)
b=0.5
conhat300=constraitF4Z300/prefmax;
conhat3000=constraitF4Z3000/prefmax;
Print["q = ",b,", m1 = ",m1,", m2 = ",m2,", m3 = ",m3,", mhc = ",m3,", alpha1 = ",\[Alpha]1,", alpha2 = ",\[Alpha]2,", alpha3 = ",\[Alpha]3]


(*plot1 = LogPlot[  FZWWbar[x,m1,m2,Sqrt[m2^2+v^2]],{x,0.2,3},
  GridLines -> Automatic,
  ImageSize->600,
  FrameLabel -> {Text[Style["q, TeV", FontSize -> 16]],
         Text[Style["\!\(\*TemplateBox[<|\"boxes\" -> FormBox[SubsuperscriptBox[OverscriptBox[StyleBox[\"f\", \"TI\"], \"^\"], \"4\", StyleBox[\"Z\", \"TI\"]], TraditionalForm], \"errors\" -> {}, \"input\" -> \"\\\\hat{f}_4^Z\", \"state\" -> \"Boxes\"|>,\n\"TeXAssistantTemplate\"]\)", FontSize -> 16]]},
  PlotRange -> All,
  Frame -> True,
  LabelStyle -> Directive[FontSize -> 14, FontFamily -> "Helvetica"],
  PlotStyle-> Thickness[0.005],
  BaseStyle -> {FontSize -> 12}
]

plot2 = LogPlot[ FZWWbar[b,m1,m2,x],{x,0.2,3},
  GridLines -> Automatic,
  ImageSize->600,
  PlotRange -> All,
  Frame -> True,
  BaseStyle -> {FontSize -> 12},
  LabelStyle -> Directive[FontSize -> 14, FontFamily -> "Helvetica"],
  PlotStyle-> Thickness[0.005],
  FrameLabel -> {Text[Style["mhc, TeV", FontSize -> 16]],
         Text[Style["\!\(\*TemplateBox[<|\"boxes\" -> FormBox[SubsuperscriptBox[OverscriptBox[StyleBox[\"f\", \"TI\"], \"^\"], \"4\", StyleBox[\"Z\", \"TI\"]], TraditionalForm], \"errors\" -> {}, \"input\" -> \"\\\\hat{f}_4^Z\", \"state\" -> \"Boxes\"|>,\n\"TeXAssistantTemplate\"]\)", FontSize -> 16]]}]
         
plot3 = LogPlot[FZWWbar[b,m1,x,Sqrt[x^2+v^2]],{x,0.2,3},
  GridLines -> Automatic,
  ImageSize->600,
  PlotRange -> All,
  Frame -> True,  
  LabelStyle -> Directive[FontSize -> 14, FontFamily -> "Helvetica"],
  PlotStyle-> Thickness[0.005],
  BaseStyle -> {FontSize -> 12},
  FrameLabel->{Text[Style["\!\(\*TemplateBox[<|\"boxes\" -> FormBox[RowBox[{SubscriptBox[StyleBox[\"m\", \"TI\"], \"2\"], \",\"}], TraditionalForm], \"errors\" -> {}, \"input\" -> \"m_2,\", \"state\" -> \"Boxes\"|>,\n\"TeXAssistantTemplate\"]\) TeV", FontSize -> 16]],
         Text[Style["\!\(\*TemplateBox[<|\"boxes\" -> FormBox[SubsuperscriptBox[OverscriptBox[StyleBox[\"f\", \"TI\"], \"^\"], \"4\", StyleBox[\"Z\", \"TI\"]], TraditionalForm], \"errors\" -> {}, \"input\" -> \"\\\\hat{f}_4^Z\", \"state\" -> \"Boxes\"|>,\n\"TeXAssistantTemplate\"]\)", FontSize -> 16]]}]
  
If[SavePDF == 1,
Export["./ZZZ/graphs/ZWW_f4hat_q2.pdf", plot1]
Export["./ZZZ/graphs/ZWW_f4hat_alpha2.pdf", plot2]
Export["./ZZZ/graphs/ZWW_f4hat_m2.pdf", plot3]]*)


(*\:0420\:0430\:0437\:043d\:044b\:0435 m2 \:0440\:0430\:0437\:043d\:044b\:043c\:0438 \:0446\:0432\:0435\:0442\:0430\:043c\:0438*)
xmin = 0.2;
xmax = 3;
dataQ = Table[
	{x,y, FplotZWW[x,m1,y,Sqrt[y^2+v^2]]},
   {y, LogRange1[0.1,2,6]},{x,LogRange1[xmin,xmax,PointNumber]}
   ];
col = dataQ[[All,1,2]] (*znachenie parametrov v graph*)
ymin = Min[dataQ[[All,All,3]]];
ymax = Max[dataQ[[All,All,3]]];



(*colors = GraphTheme /@ Rescale[Range[Length[col]], {1, Length[col]}];
legend =SwatchLegend[colors,col,LegendLayout->"Column",LegendLabel->"\!\(\*TemplateBox[<|\"boxes\" -> FormBox[SubscriptBox[StyleBox[\"m\", \"TI\"], \"2\"], TraditionalForm], \"errors\" -> {}, \"input\" -> \"m_2\", \"state\" -> \"Boxes\"|>,\n\"TeXAssistantTemplate\"]\), TeV",LegendFunction->"Panel"];
plots ={};
Do[
	If[i ==1,(*\:0443\:0441\:043b\:043e\:0432\:0438\:0435 = \:043a\:043e\:0441\:0442\:044b\:043b\:044c \:0434\:043b\:044f \:0432\:044b\:0432\:043e\:0434\:0430 \:043b\:0435\:0433\:0435\:043d\:0434\:044b*)
    p = ListLogPlot[dataQ[[i,All,{1,3}]], (*\:0412\:044b\:0432\:043e\:0434\:0438\:043c \:0433\:0440\:0430\:0444\:0438\:043a \:0441 \:0434\:0430\:0442\:043e\:0439 \:0434\:043b\:044f i\:043e\:0433\:043e \:043f\:0430\:0440\:0430\:043c\:0435\:0442\:0440\:0430*)
        LabelStyle -> Directive[FontSize -> 14, FontFamily -> "Helvetica"],
        PlotStyle-> {colors[[i]],Thickness[0.005]},
        FrameLabel -> {Text[Style["\!\(\*TemplateBox[<|\"boxes\" -> FormBox[RowBox[{StyleBox[\"q\", \"TI\"], \",\"}], TraditionalForm], \"errors\" -> {}, \"input\" -> \"q,\", \"state\" -> \"Boxes\"|>,\n\"TeXAssistantTemplate\"]\) TeV", FontSize -> 16]],
         Text[Style["\!\(\*TemplateBox[<|\"boxes\" -> FormBox[SubsuperscriptBox[OverscriptBox[StyleBox[\"f\", \"TI\"], \"^\"], \"4\", StyleBox[\"Z\", \"TI\"]], TraditionalForm], \"errors\" -> {}, \"input\" -> \"\\\\hat{f}_4^Z\", \"state\" -> \"Boxes\"|>,\n\"TeXAssistantTemplate\"]\)", FontSize -> 16]]},
        PlotMarkers -> {Automatic, 6},
        PlotLegends->legend,
        PlotRange -> {{xmin, xmax}, {ymin, ymax}},
        Joined->True,
        GridLines -> Automatic,
        PlotRange ->Full ,
        Frame -> True,
        BaseStyle -> {FontSize -> 12}];,
    p = ListLogPlot[dataQ[[i,All,{1,3}]], (*\:0412\:044b\:0432\:043e\:0434\:0438\:043c \:0433\:0440\:0430\:0444\:0438\:043a \:0441 \:0434\:0430\:0442\:043e\:0439 \:0434\:043b\:044f i\:043e\:0433\:043e \:043f\:0430\:0440\:0430\:043c\:0435\:0442\:0440\:0430*)
        PlotStyle-> {colors[[i]],Thickness[0.005]}, (* \:0443\:0441\:0442\:0430\:043d\:0430\:0432\:043b\:0438\:0432\:0430\:0435\:043c \:0446\:0432\:0435\:0442 *)
        Joined->True,
        PlotMarkers -> None,
        PlotRange -> Full];
    ];
  AppendTo[plots, p], (*\:043f\:0440\:0438\:0441\:043e\:0435\:0434\:0438\:043d\:044f\:044e \:043d\:0430\:0431\:043e\:0440 \:0442\:043e\:0447\:0435\:043a \:0432 \:0433\:0440\:0430\:0444\:0438\:043a\:0435 \:043a \:043c\:0430\:0441\:0441\:0438\:0432\:0443 \:0433\:0440\:0430\:0444\:0438\:043a\:043e\:0432*)
  {i, Length[col]}
];
legend1 = SwatchLegend[{Red, Blue}, {"300", "3000"}, LegendLayout -> "Column", LegendFunction -> "Panel", LabelStyle -> Directive[FontSize -> 14, FontFamily -> "Helvetica"],
LegendLabel -> Text[Style["\!\(\*TemplateBox[<|\"boxes\" -> FormBox[RowBox[{\"\[Integral]\", StyleBox[\"L\", \"TI\"], StyleBox[\"d\", \"TI\"], StyleBox[\"t\", \"TI\"]}], TraditionalForm], \"errors\" -> {}, \"input\" -> \"\\\\int Ldt\", \"state\" -> \"Boxes\"|>,\n\"TeXAssistantTemplate\"]\), fb\!\(\*TemplateBox[<|\"boxes\" -> FormBox[SuperscriptBox[\"\[Null]\", \"-1\"], TraditionalForm], \"errors\" -> {}, \"input\" -> \"{}^{-1}\", \"state\" -> \"Boxes\"|>,\n\"TeXAssistantTemplate\"]\)", FontSize -> 14]]];

AppendTo[plots,
p = LogPlot[{conhat300, conhat3000}, {x, xmin, xmax},
  Filling -> Top,
  FillingStyle -> Directive[Opacity[0.05]],
  PlotStyle -> {Red, Blue},
  PlotLegends -> SwatchLegend[{Red, Blue}, {"300", "3000"}, LegendLayout -> "Column", LegendFunction -> "Panel", LabelStyle -> Directive[FontSize -> 14, FontFamily -> "Helvetica"],
LegendLabel -> Text[Style["\!\(\*TemplateBox[<|\"boxes\" -> FormBox[RowBox[{\"\[Integral]\", StyleBox[\"L\", \"TI\"], StyleBox[\"d\", \"TI\"], StyleBox[\"t\", \"TI\"]}], TraditionalForm], \"errors\" -> {}, \"input\" -> \"\\\\int Ldt\", \"state\" -> \"Boxes\"|>,\n\"TeXAssistantTemplate\"]\), fb\!\(\*TemplateBox[<|\"boxes\" -> FormBox[SuperscriptBox[\"\[Null]\", \"-1\"], TraditionalForm], \"errors\" -> {}, \"input\" -> \"{}^{-1}\", \"state\" -> \"Boxes\"|>,\n\"TeXAssistantTemplate\"]\)", FontSize -> 14]]],
  PlotRange -> {{xmin, xmax}, {ymin, ymax}}]];

P = Show[plots, ImageSize -> 800]
If[SavePDF == 1,
Export[SavePath <> "ZWW_f4_m2fromQ2.pdf", P]]
*)


(*2d graph no angels m2 mhc*)
(*
If[Length[dataM2Mhc]==0||ReCalculateTable == 1,
	dataM2Mhc=Table[{x,y,FZWWbar[p1,m1,x,y]},
	{x, 0.13, 3, (3-0.13)/PointNumber},{y, 0.1, 3, 3/PointNumber}
	];
];
(*F4ZWW_hat with log10*)
(*legend = BarLegend[,{ScientificForm[2.4*10^(-3),2],ScientificForm[3.4*10^(-4),2],ScientificForm[4.5*10^(-5),2],ScientificForm[6.1*10^(-6),2],ScientificForm[8.3*10^(-7),2],ScientificForm[1.2*10^(-7),2]}]
LegendMarkerSize\[Rule]400, LegendFunction\[Rule]"Panel", LegendLayout -> "Column",
LegendLabel\[Rule]"f4Z", LabelStyle\[Rule]Directive[Black,FontSize\[Rule]14]]*)

plot1 = ListContourPlot[
	Flatten[dataM2Mhc[[All]],{1,2}], (*\:0442\:0430\:0431\:043b\:0438\:0446\:0443 \:0432 \:0441\:0442\:043e\:043b\:0431\:0438\:043a*)
	FrameLabel -> {Text[Style["\!\(\*TemplateBox[<|\"boxes\" -> FormBox[RowBox[{SubscriptBox[StyleBox[\"m\", \"TI\"], \"2\"], \",\"}], TraditionalForm], \"errors\" -> {}, \"input\" -> \"m_2,\", \"state\" -> \"Boxes\"|>,\n\"TeXAssistantTemplate\"]\) TeV", FontSize -> 16]],
         Text[Style["mhc, TeV", FontSize -> 16]]},
    Contours -> ContoursNumber,
    ScalingFunctions->"Log",
    ColorFunction -> GraphTheme, (* \:0443\:0441\:0442\:0430\:043d\:0430\:0432\:043b\:0438\:0432\:0430\:0435\:043c \:043e\:0434\:0438\:043d\:0430\:043a\:043e\:0432\:0443\:044e \:0446\:0432\:0435\:0442\:043e\:0432\:0443\:044e \:0441\:0445\:0435\:043c\:0443 *)
    GridLines -> None,
    PlotRange -> All,
    LabelStyle -> Directive[FontSize -> 14, FontFamily -> "Helvetica"],
    BaseStyle -> {FontSize -> 14,FontFamily -> "Helvetica"},
    PlotLegends->Automatic,
    Frame -> True
    ];

 conplot = ContourPlot[{FZWWbar[p1, m1, x, y] == conhat300,FZWWbar[p1, m1, x, y] == conhat3000+0.000001}, {x, 0.123, 2.98}, {y, 0.10, 2.98},
ContourStyle -> {Directive[Black, Thickness[0.003]], Directive[Darker[Blue], Thickness[0.003]]},
  PlotLegends -> 
  SwatchLegend[{Red, Blue}, {"300", "3000"}, LegendLayout -> "Column", LegendFunction -> "Panel", 
  LabelStyle -> Directive[FontSize -> 14, FontFamily -> "Helvetica"],
LegendLabel -> Text[Style["\!\(\*TemplateBox[<|\"boxes\" -> FormBox[RowBox[{\"\[Integral]\", StyleBox[\"L\", \"TI\"], StyleBox[\"d\", \"TI\"], StyleBox[\"t\", \"TI\"]}], TraditionalForm], \"errors\" -> {}, \"input\" -> \"\\\\int Ldt\", \"state\" -> \"Boxes\"|>,\n\"TeXAssistantTemplate\"]\), fb\!\(\*TemplateBox[<|\"boxes\" -> FormBox[SuperscriptBox[\"\[Null]\", \"-1\"], TraditionalForm], \"errors\" -> {}, \"input\" -> \"{}^{-1}\", \"state\" -> \"Boxes\"|>,\n\"TeXAssistantTemplate\"]\)", FontSize -> 14]]
]];

endplot= Show[{plot1,conplot},ImageSize->800]

If[SavePDF == 1,
Export[SavePath <> "ZWW_f4hat_m2mhc.pdf", endplot]]
*) 
