(* ::Package:: *)

(* ::Title:: *)
(*PV-graphs*)


SetDirectory[NotebookDirectory[]];
<< "../modules/setup.m"


SetDirectory[NotebookDirectory[]];
Print[Directory[]]


(* ::Section:: *)
(*Parameters and Constants*)


(*Main params of graphs*)
(*Number of points in graphs  N=*)
PointNumber = 50;
ContoursNumber = 6;
GraphTheme = ColorData["Rainbow"];
SavePDF = 1; (*1--save, 0--dont*)
ReCalculateTable = 0; (*1--recalculate,  0 -- no*)


SavePath = "subgraphs/";
str    = Import["buffer/F4ZZZ.txt"];
strZWW = Import["buffer/F4ZWW.txt"];


\[Alpha]2max =  0.955317;(* optimal alpha*)
\[Alpha]3max = 0.785398;
\[Alpha]2=0.5;
\[Alpha]3=\[Alpha]3max;
(*constraints on f4 ZZZ*)
constraitF4Z300 = 3*10^(-4);
constraitF4Z3000 = 1.2*10^(-4);

(*constraints on f4 ZWW*)
constraitF4ZWW = 2*10^(-4);

(*constraints on f2 ZWW*)


(* ::Section:: *)
(*Getting F4 funcs from file*)


args = {s,mh1,mh2,mh3,a1,a2,a3};
argsZWW = {s,mh1,mh2,mh3,mhc,a1,a2,a3};
Print["input Funcs:"]
Print[str]
Print[strZWW]
Activate[Inactive[SetDelayed][ToExpression["FZZZ"]@@(Pattern[#,_]&/@args),ToExpression[str]]];
Activate[Inactive[SetDelayed][ToExpression["FZWW"]@@(Pattern[#,_]&/@argsZWW),ToExpression[strZWW]]];
(*Abs[FZZZ[1,0.3,0.4,0.5,1]]*)
Print["generated funcs"]
FullSimplify[FZZZ[s,mh1,mh2,mh3,a1,a2,a3]//.SmpChanger//.Params]
FullSimplify[FZWW[Q^2,mh1,mh2,mh3,MHC,a1,a2,a3]//.SmpChanger//.Params];


(* ::Section:: *)
(*Prefactor*)


k=1;
mZ= MZ;
v=vev;
mW=MW;


(* X3 (R1x3+\[ImaginaryI] Y1) (Y2+\[ImaginaryI] R2x3)//.AngleChanger//Simplify
FullSimplify[x3[a1,a2,a3](I*Y1[a1,a2,a3]+R13[a1,a2,a3])(I R23[a1,a2,a3]+Y2[a1,a2,a3])]*)


FplotZZZ[s_,mh2_,mhc_]:=Abs[FZZZ[s,mh1,mh2,Sqrt[mh2^2+v^2],\[Alpha]1,\[Alpha]2max,\[Alpha]3max]]//.SmpChanger//.Params;
FplotZWW[s_,mh2_,mhc_]:=Abs[FZWW[s,mh1,mh2,Sqrt[mh2^2+v^2],mhc,\[Alpha]1,\[Alpha]2max,\[Alpha]3max]]//.SmpChanger//.Params;



Print[FplotZZZ[400^2,200,400],"\nNext\n",FplotZZZ[s,mh2,mhc]]



(* ::Title:: *)
(*Data and constrains*)


(* ::Section:: *)
(*Graphs settings*)


shadowbox[legend_] := Framed[
  legend,
  Background   -> White,                (* \:0431\:0435\:043b\:0430\:044f \:043d\:0435\:043f\:0440\:043e\:0437\:0440\:0430\:0447\:043d\:0430\:044f \:043f\:0430\:043d\:0435\:043b\:044c *)
  FrameStyle -> None,     (* \:0440\:0430\:043c\:043a\:0430 *)
  FrameMargins -> {{8, 8}, {5, 5}}    (* \:043e\:0442\:0441\:0442\:0443\:043f\:044b \:0432\:043d\:0443\:0442\:0440\:0438 \:0440\:0430\:043c\:043a\:0438 *)
];

textscale = 16;

base1DOpts := Sequence[
  Frame        -> True,
  Axes         -> False,
  PlotMarkers  -> None,
  Joined       -> True,

  (* \:0442\:043e\:043b\:0449\:0435 \:043b\:0438\:043d\:0438\:0438 *)
  PlotStyle    -> Directive[AbsoluteThickness[2.6]],

  (* \:0442\:043e\:043b\:0449\:0435 \:0444\:0440\:0435\:0439\:043c *)
  FrameStyle   -> Directive[Black, AbsoluteThickness[1.6]],

  (* \:0436\:0438\:0440\:043d\:044b\:0435 \:043f\:043e\:0434\:043f\:0438\:0441\:0438/\:0442\:0438\:043a\:0438 *)
  FrameTicksStyle -> Directive[Black, textscale - 2, FontFamily -> "Helvetica", FontWeight -> "Bold"],
  LabelStyle      -> Directive[Black, textscale,     FontFamily -> "Helvetica", FontWeight -> "Bold"],
  BaseStyle       -> Directive[Black, textscale,     FontFamily -> "Helvetica", FontWeight -> "Bold"],

  FrameLabel   -> {xLab, yLab},
  PlotRange    -> {{xmin, xmax}, {ymin, ymax}},
  GridLines    -> None,
  PlotRangePadding -> Scaled[0.005]
];

base2DcontOpts := Sequence[
  Frame        -> True,

  (* \:0436\:0438\:0440\:043d\:044b\:0435 \:043f\:043e\:0434\:043f\:0438\:0441\:0438 *)
  LabelStyle   -> Directive[Black, textscale, FontFamily -> "Helvetica", FontWeight -> "Bold"],
  BaseStyle    -> Directive[Black, textscale, FontFamily -> "Helvetica", FontWeight -> "Bold"],
  FrameLabel   -> {xLab2D, yLab2D},

  (* \:0442\:043e\:043b\:0449\:0435 \:0444\:0440\:0435\:0439\:043c/\:0442\:0438\:043a\:0438 *)
  FrameStyle      -> Directive[Black, AbsoluteThickness[1.6]],
  FrameTicksStyle -> Directive[Black, textscale - 2, FontFamily -> "Helvetica", FontWeight -> "Bold"],

  (* \:0442\:043e\:043b\:0449\:0435 \:043a\:043e\:043d\:0442\:0443\:0440\:043d\:044b\:0435 \:043b\:0438\:043d\:0438\:0438 *)
  ContourStyle -> Directive[AbsoluteThickness[2.2]],

  Contours        -> ContoursNumber,
  ColorFunction   -> GraphTheme,
  PlotPoints      -> PointNumber,
  PlotRange       -> All,

  (* \:0440\:0430\:0441\:0442\:044f\:043d\:0443\:0442\:044c 2D \:043f\:043e \:0432\:044b\:0441\:043e\:0442\:0435 *)
  AspectRatio  -> 1,
  ImageSize    -> {Automatic, 520},

  PlotLegends -> BarLegend[
    Automatic,
    LegendLabel    -> Placed[zLab2D, Top],
    LabelStyle     -> Directive[Black, textscale - 2, FontFamily -> "Helvetica", FontWeight -> "Bold"],
    LegendFunction -> shadowbox
  ]
];



legendLumi = SwatchLegend[
  {
    Directive[Red,  HatchFilling[ 45 Degree, 3]],
    Directive[Blue, HatchFilling[-45 Degree, 3]]
  },
  {"300", "3000"},
  LegendLayout   -> "Column",
  LegendFunction -> shadowbox,

  (* \:0436\:0438\:0440\:043d\:0435\:0435/\:0442\:043e\:043b\:0449\:0435 *)
  LabelStyle     -> Directive[Black, textscale, FontFamily -> "Helvetica", FontWeight -> "Bold"],

  LegendLabel    -> Style[
    Row[{
      "\[Integral] ", Style["L", Italic], Style["d", Italic], Style["t", Italic],
      ", ", Superscript["fb", -1]
    }],
    {textscale, FontFamily -> "Helvetica", FontWeight -> "Bold"}
  ]
];



(* ::Section::Closed:: *)
(*ZZZ*)


(* ::Subsection:: *)
(*roots*)


qList = {300, 400, 500,600,700,800,900,1000,1200};
mhcI = 600;


Clear[FindAllRoots]
FindAllRoots[q_?NumericQ, limit_?NumericQ] :=
  Quiet[
    Check[
      Module[{f, ms, vals, segs, sol, roots},
        f[m_?NumericQ] := N[Abs[FplotZWW[q^2, m, mhcI]] - limit];
        ms = Range[187., 1500., 5.];
        vals = f /@ ms;
        segs = Select[
          Partition[Transpose[{ms, vals}], 2, 1],
          Times @@ (Last /@ #) <= 0 &
        ];
        If[segs === {},
          "No limit",
          sol = Quiet[FindRoot[f[m], {m, #[[1, 1]], #[[2, 1]]}] & /@ segs];
          roots = m /. sol;
          roots = Select[roots, NumericQ];
          roots = Chop[roots, 10^-6];
          If[roots === {}, "No limit", Sort@DeleteDuplicates[roots]]
        ]
      ],
      "No limit"
    ]
  ]


header = {"q [GeV]", "Intersection Roots (300 fb^-1)", 
   "Intersection Roots (3000 fb^-1)"};

data = Table[
   {qVal,
    FindAllRoots[qVal, constraitF4Z300],
    FindAllRoots[qVal, constraitF4Z3000]},
   {qVal, qList}
   ];


formattedData = 
  Map[Function[item, If[ListQ[item], Round[item, 0.1], item]], data, {2}];

OutputForm[
 TableForm[formattedData, TableHeadings -> {None, header}]
]


dataPlots = Table[
   Module[{f},
    f[m_?NumericQ] := N[Abs[FplotZWW[q^2, m, mhcI]]];
    Table[{m, f[m]}, {m, LogRange1[187., 1200., PointNumber]}]
    ],
   {q, qList}
   ];


Show[ListLogPlot[
 dataPlots,
 PlotStyle -> 
  Table[{GraphTheme[i/Length[qList]], Thickness[0.005]}, {i, 
    Length[qList]}],
 PlotMarkers -> None,
 Joined -> True,
 PlotLegends -> 
  Map[StringForm["q = `1` GeV", #] &, qList],
 AxesLabel -> {"m_{h2} [GeV]", "|f_4^Z|"},
 PlotRange -> {{187, 1500}, {10^-6, 3*10^-4}},
 GridLines -> {
   None,
   {
    {constraitF4Z300, 
     Directive[Red, AbsoluteThickness[1.5], Dashed]},
    {constraitF4Z3000, 
     Directive[Blue, AbsoluteThickness[1.5], Dashed]}
    }
   }
]]


(* ::Title:: *)
(*Graphs*)


(* ::Section:: *)
(*F4 ZZZ*)


(* ::Subsection::Closed:: *)
(*Test graphs connection with article*)


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


(* ::Subsection::Closed:: *)
(*Start graph F4Z(m2) for different q with cuts*)


xmin = 200;     xmax = 600;
ymin = 10^(-7); ymax =0.5 10^(-3);
m2list = {200,220,230,250,300};
plots = {};
extr={};
hiConstr={};

colors = GraphTheme /@ Rescale[Range[Length[m2list]], {1, Length[m2list]}];

xLab = xLab = Style["q [GeV]", textscale];
yLab = Style[TraditionalForm @ HoldForm @ 
      Abs @ Subsuperscript[Style["f", "TI"], 4, Style["Z", "TI"]],
  textscale ];
	


	
Do[
     data = N[Table[{x, FplotZZZ[x^2, m2list[[i]], 1]}, {x, LogRange1[xmin, xmax, PointNumber]}]];
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
    
      p = ListLogLogPlot[data,
      base1DOpts,
      PlotStyle -> {colors[[i]], Thickness[0.005]}
      ];
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
			  FillingStyle -> Directive[Opacity[0.025,colors[[i]]], colors[[i]]]
		];
		AppendTo[plots, fillPlot];
	];
    ,
    {i, Length[m2list]}
  ];
  
 AppendTo[plots,
LogLogPlot[{constraitF4Z300,constraitF4Z3000}, {x, xmin, xmax}, 
	Filling -> Top,
	PlotStyle->{Darker[Red],Darker[Blue]}, 
	FillingStyle -> Directive[Opacity[0.035]],
	PlotRange -> {{xmin, xmax}, {ymin, ymax}}]
];



legend  = LineLegend[colors, m2list,
   LegendLayout   -> "Column",
   LegendLabel    -> "\!\(\*FormBox[TemplateBox[<|\"boxes\" -> FormBox[SubscriptBox[StyleBox[\"m\", \"TI\"], \"2\"], TraditionalForm], \"errors\" -> {}, \"input\" -> \"m_ {2}\", \"state\" -> \"Boxes\"|>,\n\"TeXAssistantTemplate\"],
TextForm]\), GeV",
   LabelStyle     -> Directive[FontSize -> textscale, FontFamily -> "Helvetica"],
   LegendFunction -> shadowbox
];



Sh = Legended[
  Show[plots,ImageSize   -> 1000],
  Placed[Column[{legend, legendLumi}, Spacings -> 1],{0.95, 0.75} ]
]


If[SavePDF == 1,
  Export[SavePath <> "ZZZ_f4_s.pdf", Sh]]


(* ::Subsection:: *)
(*2d zzz*)


PrintTG["Starting 2d plot for zzz"];

xmin = 200; xmax = 600;
ymin = 200; ymax = 600;

xLab2D = Style[
  Row[{Style["q", Italic], " [GeV]"}],
  1.2 textscale
];
yLab2D = Style[
  Row[{Subscript[m, 2], " [GeV]"}],
  1.2 textscale
];
zLab2D = Style[TraditionalForm @ HoldForm @ 
      Abs @ Subsuperscript[Style["f", "TI"], 4, Style["Z", "TI"]],
  textscale ];


Func[x_, y_] := FplotZZZ[x^2, y, 100];

plot5 = ContourPlot[
  Func[x, y],
  {x, xmin, xmax}, {y, ymin, ymax},
  ScalingFunctions -> "Log",
  Evaluate @ base2DcontOpts
];


plot5


If[SavePDF == 1,
Export[SavePath <> "ZZZ_f4_2d.pdf", plot5]]


(* ::Subsection:: *)
(*f4Z from q for different m2*)


(*F4ZZZ from s for differnet mass*)
mlist = {200,230,300,500};
plots={};
xmin = 200;     xmax = 800;
ymin = 10^(-7); ymax =6* 10^(-4);
colors = GraphTheme /@ Rescale[Range[Length[mlist]], {1, Length[mlist]}];

xLab = xLab = Style["q, GeV", textscale];
yLab = Style[TraditionalForm @ HoldForm @ 
      Abs @ Subsuperscript[Style["f", "TI"], 4, Style["Z", "TI"]],
  textscale 
];


Do[
   data = Table[{x, FplotZZZ[x^2, mlist[[i]], 1]}, {x,LogRange1[xmin,xmax,PointNumber]}];
   p = ListLogLogPlot[data,
       PlotStyle-> {colors[[i]],Thickness[0.005]},
       base1DOpts];
   AppendTo[plots, p],
   
  {i, Length[mlist]}
];

limplot = LogLogPlot[
	{constraitF4Z300,constraitF4Z3000}, {x, xmin, xmax}, 
	Filling -> Top, 
	FillingStyle -> Directive[Opacity[0.05]],
	PlotStyle->{Red,Blue},
	GridLines -> None,
	PlotRange -> {{xmin, xmax}, {ymin, ymax}}];



legend =LineLegend[colors,mlist,
			LegendLayout->"Column", 
			LabelStyle     -> Directive[FontSize -> textscale, FontFamily -> "Helvetica"],
			LegendFunction->shadowbox,
			LegendLabel -> Style[ Row[{Subscript[m, 2], ", ", Style["GeV", Plain]}],textscale]
			];


fullLegend = Placed[Column[{legendLumi, legend}, Spacings -> 0.05], {Right, Top}];

Sh =  Legended[Show[Append[plots,limplot],ImageSize -> 800], 
  fullLegend
  ]


If[SavePDF == 1,
Export[SavePath <> "ZZZ_f4_s.pdf", Sh]]     


(* ::Subsection:: *)
(*1d graph ZZZ show scaling with alpha*)


(* \:043d\:0430\:0441\:0442\:0440\:043e\:0439\:043a\:0438 *)
mlist = {200, 300, 400};
xmin = 200; xmax = 800;
ymin = 10^-7; ymax = 10^-4;
a2 = 0.5;

colors = ColorData["Rainbow"] /@ Rescale[Range[Length@mlist]];
xLab = Style["q, GeV", textscale];
yLab = Style[TraditionalForm @ HoldForm @ 
      Abs @ Subsuperscript[Style["f", "TI"], 4, Style["Z", "TI"]],
  textscale
];



(* \:043e\:0434\:043d\:0430 \:043c\:0430\:0441\:0441\:0430 -> \:0434\:0432\:0435 \:043b\:0438\:043d\:0438\:0438 \:043e\:0434\:043d\:043e\:0433\:043e \:0446\:0432\:0435\:0442\:0430: \:0441\:043f\:043b\:043e\:0448\:043d\:0430\:044f (\[Alpha]2max) \:0438 \:043f\:0443\:043d\:043a\:0442\:0438\:0440 (\[Alpha]2=0.5) *)
plotForMass[m_, color_] := Module[{dataMax, dataA2},
  dataMax = Table[
    {x, Abs[FZZZ[x^2, mh1, m, Sqrt[m^2 + v^2], \[Alpha]1, \[Alpha]2max, \[Alpha]3max]] //. SmpChanger //. Params},
    {x, LogRange1[xmin, xmax, PointNumber]}
  ];
  dataA2 = Table[
    {x, Abs[FZZZ[x^2, mh1, m, Sqrt[m^2 + v^2], \[Alpha]1, a2, \[Alpha]3max]] //. SmpChanger //. Params},
    {x, LogRange1[xmin, xmax, PointNumber]}
  ];
  ListLogLogPlot[
    {dataMax, dataA2},
    PlotStyle -> {
      Directive[color, Thick],          
      Directive[color, Dashed, Thick]   
    },
    base1DOpts
  ]
];

(* \:0432\:0441\:0435 \:043c\:0430\:0441\:0441\:044b *)
plotsCurves = MapThread[plotForMass, {mlist, colors}];


(* \:043b\:0435\:0433\:0435\:043d\:0434\:0430 *)
legendMass = SwatchLegend[
  colors, mlist,
  LegendLayout -> "Column",
  LegendFunction -> shadowbox,
  LabelStyle     -> Directive[FontSize -> textscale, FontFamily -> "Helvetica"],
  LegendLabel -> Style["\!\(\*SubscriptBox[StyleBox[\"m\",\"TI\"],\"2\"]\), GeV",textscale]
];
legendStyle = LineLegend[
  {Directive[Black, Thick], Directive[Black, Dashed, Thick]},
  {Style["\!\(\*SubscriptBox[\(\[Alpha]\), \(2\)]\) = \!\(\*SubscriptBox[\(\[Alpha]\), \(2\)]\)\!\(\*StyleBox[\"max\", Plain]\)",textscale],
   Style["\!\(\*SubscriptBox[\(\[Alpha]\), \(2\)]\) = 0.5",textscale]},
   LabelStyle     -> Directive[FontSize -> textscale, FontFamily -> "Helvetica"],
  LegendFunction -> shadowbox
];

fullLegend1 = Placed[Column[{legendMass, legendStyle}, Spacings -> 0.05], {Right, Top}];


Sh2 =  Legended[
Show[plotsCurves,ImageSize -> 800], 
  fullLegend1
  ]


(* ::InheritFromParent:: *)
(*Null*)


If[SavePDF == 1,
Export[SavePath <> "ZZZ_f4_rescale.pdf", Sh2]]     


(* ::Section:: *)
(*ZWW f4*)


(* ::Subsection::Closed:: *)
(*test graphs*)


b = 300;
c = 500;
plotF4gen = LogPlot[{FplotZZZ[x^2,b,c],FplotZWW[x^2,b,c]},{x,200,1000},
  GridLines -> None,
  ImageSize->600,
  PlotRange -> All,
  PlotLegends->{"zzz","zww"},
  Frame -> True,
  BaseStyle -> {FontSize -> 12}
  ]


(* ::Subsection::Closed:: *)
(*1d: f4zww  from q (m2 -diff colors)*)


(*F4Zww from s for differnet mass*)
mhc = 500;
mlist = {200,220,230,250,350};
plots={};
xmin = 200;     xmax = 1000;
ymin = 5* 10^(-6); ymax = 3*10^(-4);
colors = GraphTheme /@ Rescale[Range[Length[mlist]], {1, Length[mlist]}];

xLab = Style["q, GeV", textscale];
yLab = Style[TraditionalForm @ HoldForm @ 
      Abs @ Subsuperscript[Style["f", "TI"], 4, Style["Z", "TI"]],
  textscale
];


Do[
   data = Table[{x, FplotZWW[x^2, mlist[[i]], mhc]}, {x,LogRange1[xmin,xmax,PointNumber]}];
   p = ListLogLogPlot[data,
       PlotStyle-> {colors[[i]],Thickness[0.005]},
        base1DOpts];
  AppendTo[plots, p],
  {i, Length[mlist]}
];

AppendTo[plots,
LogLogPlot[{constraitF4Z300,constraitF4Z3000}, {x, xmin, xmax}, 
	Filling -> Top, 
	FillingStyle -> Directive[Opacity[0.05]],
	PlotStyle->{Red,Blue},
	PlotRange -> {{xmin, xmax}, {ymin, ymax}}]
];


legend =LineLegend[colors,mlist,
	LegendLayout->"Column",
	LabelStyle     -> Directive[FontSize -> textscale, FontFamily -> "Helvetica"],
	LegendLabel->Style["\!\(\*TemplateBox[<|\"boxes\" -> FormBox[SubscriptBox[StyleBox[\"m\", \"TI\"], \"2\"], TraditionalForm], \"errors\" -> {}, \"input\" -> \"m_2\", \"state\" -> \"Boxes\"|>,\n\"TeXAssistantTemplate\"]\), GeV",textscale],
	LegendFunction->shadowbox];

mhcLabel =
  Row[{
    Subscript[Style["m", Italic], 
      Superscript["H", "\[PlusMinus]"]],
    " = ",
    NumberForm[mhc, {4, 0}], " GeV"
  }];
epilogTag = Inset[
  shadowbox @ Style[mhcLabel, textscale],
  Scaled[{1, 0.1}],          (* \:043f\:043e\:0437\:0438\:0446\:0438\:044f; \:043f\:043e\:0434\:043f\:0440\:0430\:0432\:044c \:043f\:043e\:0434 \:0441\:0435\:0431\:044f *)
  {Right, Bottom}                (* \:043f\:0440\:0438\:0432\:044f\:0437\:043a\:0430 \:043a \:043f\:0440\:0430\:0432\:043e\:043c\:0443 \:043d\:0438\:0436\:043d\:0435\:043c\:0443 \:0443\:0433\:043b\:0443 \:043f\:0430\:043d\:0435\:043b\:0438 *)
];

fullLegend = Placed[Column[{legendLumi, legend}, Spacings -> 0.05], {Right, Top}];


Sh2 =  Legended[
Show[plots, Epilog -> {epilogTag},ImageSize -> 800], 
  fullLegend
  ]


If[SavePDF == 1,
Export[SavePath <> "ZWW_f4_s.pdf", Sh2]] 


(* ::Subsection::Closed:: *)
(*1 d : f4zww  from mhc (m2 - diff colors)*)


(*F4Zww from s for differnet mass*)
q = 600;
mlist = {300, 400, 500,800,1000};
plots={};
xmin = 200;     xmax = 1000;
ymin = 5* 10^(-7); ymax = 5*10^(-4);
colors = GraphTheme /@ Rescale[Range[Length[mlist]], {1, Length[mlist]}];

xLab = Style["mhc, GeV", textscale];
yLab = Style[TraditionalForm @ HoldForm @ 
      Abs @ Subsuperscript[Style["f", "TI"], 4, Style["Z", "TI"]],
  textscale
];


Do[
   data = Table[{x, FplotZWW[q^2, mlist[[i]], x]}, {x,LogRange1[xmin,xmax,PointNumber]}];
   p = ListLogLogPlot[data,
       PlotStyle-> {colors[[i]],Thickness[0.005]},
        base1DOpts];

  AppendTo[plots, p],
  {i, Length[mlist]}
];

AppendTo[plots,
		LogLogPlot[{constraitF4Z300,constraitF4Z3000}, {x, xmin, xmax}, 
		Filling -> Top, 
		FillingStyle -> Directive[Opacity[0.05]],
		PlotStyle->{Red,Blue},
		PlotRange -> {{xmin, xmax}, {ymin, ymax}}]
];


legend =LineLegend[colors,mlist,
	LegendLayout->"Column",
	LabelStyle     -> Directive[FontSize -> textscale, FontFamily -> "Helvetica"],
	LegendLabel->Style["\!\(\*TemplateBox[<|\"boxes\" -> FormBox[SubscriptBox[StyleBox[\"m\", \"TI\"], \"2\"], TraditionalForm], \"errors\" -> {}, \"input\" -> \"m_2\", \"state\" -> \"Boxes\"|>,\n\"TeXAssistantTemplate\"]\), GeV",textscale],
	LegendFunction->shadowbox];

mhcLabel = Row[{
   Style["q", Italic],
   " = ",
   NumberForm[q, {4, 0}],
   " GeV"
}];

epilogTag = Inset[
  shadowbox @ Style[mhcLabel, textscale],
  Scaled[{0.70, 0.9}],          (* \:043f\:043e\:0437\:0438\:0446\:0438\:044f; \:043f\:043e\:0434\:043f\:0440\:0430\:0432\:044c \:043f\:043e\:0434 \:0441\:0435\:0431\:044f *)
  {Right, Bottom}                (* \:043f\:0440\:0438\:0432\:044f\:0437\:043a\:0430 \:043a \:043f\:0440\:0430\:0432\:043e\:043c\:0443 \:043d\:0438\:0436\:043d\:0435\:043c\:0443 \:0443\:0433\:043b\:0443 \:043f\:0430\:043d\:0435\:043b\:0438 *)
];

fullLegend = Placed[Column[{legendLumi, legend}, Spacings -> 0.05], {Right, Top}];


Sh3 = Legended[
  Show[
    plots,
    Epilog   -> {epilogTag},
    ImageSize -> 800
  ],
  fullLegend
]


If[SavePDF == 1,
Export[SavePath <> "ZWW_f4_mhc.pdf", Sh3]] 


(* ::Subsection:: *)
(*2d:  f4zww from q m2*)


PrintTG["Starting 2d plot for zww"];

xmin = 200; xmax = 600;
ymin = 200; ymax = 600;

xLab2D = Style[
  Row[{Style["q", Italic], ", TeV"}],
  1.2 textscale
];
yLab2D = Style[
  Row[{Subscript[m, 2], ", TeV"}],
  1.2 textscale
];



Func[x_, y_] := FplotZWW[x^2, y, Sqrt[y^2 + v^2]];

plot5 = ContourPlot[
  Func[x, y],
  {x, xmin, xmax}, {y, ymin, ymax},
  ColorFunction   -> GraphTheme,
  ScalingFunctions -> "Log",
  Contours        -> ContoursNumber,
  PlotPoints      -> PointNumber,
  Evaluate @ (Sequence @@ base2DcontOpts),
  PlotLegends -> BarLegend[
    Automatic,
    LabelStyle    -> Directive[textscale, FontFamily -> "Helvetica"],
    LegendFunction -> shadowbox
  ]
];

conplot = ContourPlot[
  {Func[x, y] == constraitF4Z300,
   Func[x, y] == constraitF4Z3000},
  {x, xmin, xmax}, {y, ymin, ymax},
  ContourStyle -> {
    Directive[Red,   Thickness[0.007]],
    Directive[Blue,  Thickness[0.003]]
  }, 
  PlotLegends -> None
];



fullLegend = Placed[legendLumi, {Right, Top}];


ShZWW=Legended[Show[plot5,conplot,ImageSize->800],fullLegend];
ShZWW





(* ::Subsection:: *)
(*\:0433\:0440\:0430\:0444\:0438\:043a 2d \:0442\:043e\:0442 \:0436\:0435 \:043d\:043e densityplot*)


base2DdensOpts := {
  Frame        -> True,
  Axes         -> False,
  FrameStyle   -> Directive[Black, Thickness[0.002]],
  FrameTicksStyle -> Directive[textscale - 2],
  FrameLabel   -> {xLab2D, yLab2D},
  LabelStyle   -> Directive[textscale, FontFamily -> "Helvetica"],
  BaseStyle    -> Directive[textscale, FontFamily -> "Helvetica"],
  PlotRange    -> All
};


Func[x_, y_] := FplotZWW[x^2, y, Sqrt[y^2 + v^2]];

densplot = DensityPlot[
  Func[x, y],
  {x, xmin, xmax}, {y, ymin, ymax},
  ColorFunction    -> GraphTheme,
  ScalingFunctions -> "Log",
  PlotPoints       -> PointNumber,
  Evaluate @ (Sequence @@ base2DdensOpts),
  PlotLegends -> BarLegend[
    Automatic,
    LabelStyle     -> Directive[textscale, FontFamily -> "Helvetica"],
    LegendFunction -> shadowbox
  ]
];

line3000 = ContourPlot[
  Func[x, y] == constraitF4Z3000,
  {x, xmin, xmax}, {y, ymin, ymax},
  ContourStyle -> Directive[Black, Thickness[0.004]],
  Frame     -> False,
  Axes      -> False,
  PlotRange -> All,
  PlotLegends -> None
];


ShZWW = Show[densplot, line3000, ImageSize -> 800]


If[SavePDF == 1,
Export[SavePath <> "ZWW_2D_m2q.pdf", ShZWW ]]
 PrintTG["Finishing 2d plot for zww"];


PrintTG["All plots are finished"];
