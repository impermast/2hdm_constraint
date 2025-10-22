(* ::Package:: *)

(* ::Subsection:: *)
(*Script for scv data generation.*)


description="CSV data generation";
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


(*Main params of graphs*)
(*Number of points in graphs  N=*)
PointNumber = 800;
ContoursNumber = 10;
GraphTheme = ColorData["Rainbow"];

constraitF4Z3000 = 2*10^-5;

Validate = True; (*1--graph plot?,  0 -- no*)


SetDirectory[NotebookDirectory[]]
Get["../modules/ModelParams.wl"];
Get["../modules/FunctionalModules.wl"];
Get["../modules/SaveToCSVmodule.wl"];
SavePath = "subgraphs/";
str    = Import["buffer/F4ZZZ.txt"];
strZWW = Import["buffer/F4ZWW.txt"];


\[Alpha]2max =  0.955317(* optimal alpha*)
\[Alpha]3max = 0.785398
\[Alpha]2=0.5;
\[Alpha]3=\[Alpha]3max;

mZ= MZ;
v=vev;
mW=MW;

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


FplotZZZ[s_,mh2_,mhc_]:=Abs[FZZZ[s,mh1,mh2,Sqrt[mh2^2+v^2],\[Alpha]1,\[Alpha]2max,\[Alpha]3max]]//.SmpChanger//.Params;
FplotZWW[s_,mh2_,mhc_]:=Abs[FZWW[s,mh1,mh2,Sqrt[mh2^2+v^2],mhc,\[Alpha]1,\[Alpha]2max,\[Alpha]3max]]//.SmpChanger//.Params;
FplotZZZ[200^2,100,400]
FplotZZZ[s,mh2,mhc]


(* ::Subsection:: *)
(*For ZZZ*)


If[False,
xmin = 183.4; xmax = 600;
ymin = 183.4; ymax = 600;

PrintTG["Started calculation of csv for ZZZ"];

xs = Subdivide[xmin, xmax, PointNumber];
ys = Subdivide[ymin, ymax, PointNumber];
pairs = Tuples[{xs,ys}];

fNum[x_?NumericQ, y_?NumericQ] := Log10 @ N @ (FplotZZZ[x^2, y, 1] //.SmpChanger//.Params);

Print[First[xs]];

(*\:043f\:043e\:0434\:043a\:043b\:044e\:0447\:0435\:043d\:0438\:0435 \:0437\:0430\:043c\:0435\:043d smpchanger params \:0432 \:043f\:0430\:0440\:0430\:043b\:0435\:043b\:0435\:0437\:0430\:0446\:0438\:044e*)

DistributeDefinitions[FplotZZZ, SmpChanger, Params, mhc, xs, ys,pairs];
ParallelEvaluate[{Head[SmpChanger], Head[Params]}];
vals = Map[({#1, #2, fNum[#1, #2]} & @@ #) &, pairs];
PrintTG["Finished calculation of csv for ZZZ"];
Print[vals[[2]]];
Export[FileNameJoin[{Directory[], "backfiles", "f4Z_m2q.csv"}],
       Prepend[vals, {"q","m2","log10_f4ZZZ"}], "CSV"];
];





(* ::Subsection:: *)
(*For ZWW*)


If[False,
xmin = 183.4; xmax = 600;
ymin = 183.4; ymax = 600;
c[y_] := Sqrt[y^2+v^2];
CSVname = "f4W_m2q_mhc"<>ToString[c[mh2]]<>".csv";

PrintTG["Started calculation of csv for ZWW"];

xs = Subdivide[xmin, xmax, PointNumber];
ys = Subdivide[ymin, ymax, PointNumber];
pairs = Tuples[{xs,ys}];

fNumW[x_?NumericQ, y_?NumericQ] := Module[{val},
  val = FplotZWW[x^2, y, c[y]] //. SmpChanger//. Params;
  Log10 @ N[val]
];
Print[First[xs]];

(*\:043f\:043e\:0434\:043a\:043b\:044e\:0447\:0435\:043d\:0438\:0435 \:0437\:0430\:043c\:0435\:043d smpchanger params \:0432 \:043f\:0430\:0440\:0430\:043b\:0435\:043b\:0435\:0437\:0430\:0446\:0438\:044e*)

DistributeDefinitions[FplotZWW, SmpChanger, Params, mhc, xs, ys,pairs];
ParallelEvaluate[{Head[SmpChanger], Head[Params]}];
valsW = Map[({#1, #2, fNumW[#1, #2]} & @@ #) &, pairs];
Print["Ready"];
Print[valsW[[2]]];

Export[FileNameJoin[{Directory[], "backfiles", CSVname}],
       Prepend[valsW, {"q","m2","log10_f4ZWW"}], "CSV"];
PrintTG["Finished calculation of csv for ZWW"];
];


(* ::Subsubsection:: *)
(*Graph validation*)


If[Validate,

Fname = "f4W_m2q_mhcSqrt.csv";  
filePath = FileNameJoin[{Directory[], "backfiles", Fname}];

raw  = Import[filePath, "CSV"];
headers = First@raw;          (* {"q","m2","log10_f4ZWW"} *)
data = N @ Rest@raw;          (* {q, m2, z} *)

{qmin, qmax} = MinMax[data[[All, 1]]];
{mmin, mmax} = MinMax[data[[All, 2]]];

base = ListContourPlot[
  data,
  InterpolationOrder -> 2,        
  Contours -> 10,                 
  ContourShading -> True,
  ColorFunction -> "SunsetColors",
  ColorFunctionScaling -> True,
  PlotRange -> All,
  Frame -> True,
  FrameLabel -> (Style[#, 14] & /@ {"q, GeV", "m_{h2}, GeV"}),
  PlotLegends -> BarLegend[Automatic,
    LabelStyle -> 12,
    LegendLabel -> Style[TraditionalForm[Log10[f4^{ZWW}]], 12]
  ],
  AspectRatio -> 1,               (* \:043a\:0432\:0430\:0434\:0440\:0430\:0442\:043d\:0430\:044f \:043e\:0431\:043b\:0430\:0441\:0442\:044c *)
  ImagePadding -> 50
];

densplot = ListDensityPlot[
  data,
  InterpolationOrder -> 2,
  ColorFunction -> "SunsetColors",
  ColorFunctionScaling -> True,
  PlotRange -> All,
  FrameLabel -> (Style[#, 14] & /@ {"q, GeV", "m_{h2}, GeV"}),
  PlotLegends -> BarLegend[Automatic,
    LegendLabel -> Style[TraditionalForm[Log10[f4^ZWW]], 12]
  ],
  AspectRatio -> 1
];

c = Log10[constraitF4Z3000];   
line3000 = ListContourPlot[
  data,
  InterpolationOrder -> 2,
  Contours -> {c},
  ContourShading -> False,
  ContourStyle -> Directive[Thick, Black],
  PlotRange -> All,
  Frame -> False, Axes -> False,
  ContourLabels -> (
    Placed[
      Style["f4^{ZWW} = 2\[Times]10^-5", 12, Black, Background -> White],
      Center
    ]
  )
];

];


If[Validate,Show[base]]


If[Validate,Show[densplot, line3000]]
