#!/usr/bin/env wolframscript
(* ::Package:: *)

 


(* \:041e\:043f\:0446\:0438\:0438 \:0440\:0430\:0431\:043e\:0442\:044b *)
logic = 0; 
(*  0 - \:043f\:0435\:0440\:0435\:0441\:0447\:0438\:0442\:0430\:0442\:044c \:0432\:0441\:0451, 
	1 - \:0437\:0430\:0433\:0440\:0443\:0437\:0438\:0442\:044c \:0432\:0441\:0435 \:0431\:0443\:0444\:0435\:0440\:044b, 
	2 - \:043f\:0435\:0440\:0435\:0441\:0447\:0438\:0442\:0430\:0442\:044c \:0432\:0441\:0435 \:0441 \:0443\:043f\:0440\:043e\:0449\:0435\:043d\:043d\:044b\:043c \:043c\:0430\:0442\:0440\:0438\:0447\:043d\:044b\:043c, 
	3 - \:0437\:0430\:0433\:0440\:0443\:0437\:043a\:0430 WidthCross, \:0440\:0430\:0441\:0447\:0435\:0442 \:0441\:0435\:0447\:0435\:043d\:0438\:044f *)
particleID = 1;
(* for PDF not FeynCalc
*)
ProcName = "uuZZZ";


(* \:0427\:0442\:0435\:043d\:0438\:0435 \:0430\:0440\:0433\:0443\:043c\:0435\:043d\:0442\:043e\:0432 \:043a\:043e\:043c\:0430\:043d\:0434\:043d\:043e\:0439 \:0441\:0442\:0440\:043e\:043a\:0438 *)
If[Length[$ScriptCommandLine] > 1,
    logic = ToExpression[$ScriptCommandLine[[2]]];
];
If[ $FrontEnd === Null, 
    $FeynCalcStartupMessages = False; 
    $LoadFeynCalcMessages = False; 
    Print["tt>Z>ZZ calculation in 2HDM CPV, 1-loop.\n
	Calcultion of cross from PDF and matrix."]; 
  ];
If[ $Notebooks === False, 
	$FeynCalcStartupMessages = False;
	$LoadFeynCalcMessages = False; 
	(*Off[General::shdw];
	Off[C0::"msg"]*)
    ,
    SetDirectory[NotebookDirectory[]]
  ];


(* \:041f\:043e\:0434\:043a\:043b\:044e\:0447\:0435\:043d\:0438\:0435 \:043d\:0435\:043e\:0431\:0445\:043e\:0434\:0438\:043c\:044b\:0445 \:043f\:0430\:043a\:0435\:0442\:043e\:0432 *)

$LoadAddOns = {"FeynArts"};
$PrePrint = InputForm;

Quiet[<<FeynCalc`] 
AppendTo[$ModelPath, "/home/kds/.Mathematica/Applications/FeynArts/Models/"];
Quiet[Install["LoopTools"]];
Quiet[Needs["LoopTools`"]];
$FAVerbose = 0; 

Unprotect[PaVe, Li2, A0, A00, B0, B1, B00, B11, DB0, DB1, C0, D0];
Remove[PaVe, Li2, A0, A00, B0, B1, B00, B11, DB0, DB1, C0, D0];


Get["../modules/ModelParams.wl"];
Get["../modules/FunctionalModules.wl"];

ampFile = StringJoin["buffer/amp_", ProcName, ".mx"];
meFile  = StringJoin["buffer/me_", ProcName, ".mx"];
wcFile  = StringJoin["buffer/wc_", ProcName, ".mx"];
csFile  = StringJoin["buffer/cs_", ProcName, ".mx"];
pvFile  = StringJoin["buffer/pv_", ProcName, ".mx"];


(*model params*)
mh2 = 250;
mh3 = Sqrt[mh2^2+mh1^2];
a2 = 0.1317; 
a3 = 0.7854;
Print["-------
Model parameters in code:
mh2 = ",mh2,
"\nmh3    = ",mh3,
"\nalpha2    = ",a2,
"\nalpha3  = ",a3,
"\n--------"];

(*Integration limits*)
ptmin = MZ+0.1;
s0 = 13000^2;
smin = 4*ptmin^2;  
xmin = Sqrt[smin/s0];
Print["-------\nIntegration limits in code:
ptmin = ",ptmin,
"\nsqrt(s0)    = ",Sqrt[s0],
"\nxmin  = ",xmin//N,"\n--------"];


(*download matrix element*)
If[logic < 2,
	PrintTG["Loading matrix element."];
	MaElread = Get[meFile];
	OnlyPaVe = Get[pvFile];
	MaElread = MaElread//.OnlyPaVe//.SmpChanger//.Params//.AngleChanger;
	MaElreadPTL = MaElread//.PaveToLooptools/.{FeynCalc`CA->3};
	MaEl[sX_,tX_] := MaElreadPTL/.{s->sX,t->tX};
	,
	Print["Using demo matrix element."];
	MaEldemo = Abs[(t^2+s^2)*C0i[cc001,MZ^2,s,MZ^2,mh1^2,mh2^2,mh3^2]/(s-MZ^2)^2];
	MaEl[sX_,tX_] := MaEldemo/.{s->sX,t->tX}//N
]
If[$Notebooks,
	Print["Test of matrix function:\n";
	"Numericaly: ",MaEl[(4*MZ)^2,100]];
];
CheckTime[]


(*download PDFs*)
PrintTG["Loading pdf for run."]
xPDF[x_, Q_, id_] := Module[{result, scriptPath, output},
  scriptPath = FileNameJoin[{NotebookDirectory[],"../pdfcode/", "xPDF.py"}];
  result = RunProcess[{"sh", "-c", 
    "LD_LIBRARY_PATH=/usr/local/lib python3 " <> scriptPath <> " " <> ToString[x] <> " " <> ToString[Q] <> " " <> ToString[id]
  }];
  output = StringTrim[result["StandardOutput"]];
  If[StringMatchQ[output, NumberString], ToExpression[output], Null]
]
L[t_]:=Integrate[(1/x) *xPDF[x,t*s0,-particleID]*xPDF[t/x,t*s0,id],{x,t,particleID}]/Null^2;
Lexp[t_]:=Integrate[xPDF[10^yt,t*s0,particleID] * xPDF[t*10^(-yt),t*s0,-particleID] ,{yt, Log10[t],0}]*Log[10]/Null^2;
CheckTime[];




Print[MaEl[s,t]]


If[ logic == 0 || logic == 2,
PrintTG["Working on WidthCross"];
Pref[x_] := (x-2*MZ^2)/(16 Pi x^2);
WidthCross[s_] := Pref[s] Integrate[
	MaEl[s, t], 
    {t, -s/2 * (1 + Sqrt[1 - (4*ptmin/Sqrt[s0])^2]), -s/2 * (1 - Sqrt[1 - (4*ptmin/Sqrt[s0])^2])}];                                                                               
Put[WidthCross[St],wcFile];
CheckTime[];                 
];

If[ logic == 1 || logic == 3,
Print["Creating WidthCross function from buffer."];
CheckTime[];
WC  =  Get[wcFile];
If[$Notebooks,
Print[WC]
];
WidthCross[s_] := WC/.{St->s};
CheckTime[];
]                

If[$Notebooks,
Num1 = WidthCross[1000^2];
PrintTG[ToString[ScientificForm[Num1]]];
];


If[$Notebooks,
pointArray = Range[0.0005, 0.0015,0.00005];
dataPoints = Table[{x, WidthCross[s0 * x]*Lexp[x]//N}, {x, pointArray}];
Plot1 = ListPlot[
    dataPoints,
    PlotStyle -> {Red, PointSize[Medium]},
    AxesLabel -> {"x", "WidthCross(s0 * x)"},
    PlotLabel -> "Points of integration",
    GridLines -> Automatic,
    Frame -> True
];
Export["supgraphs/integration_points.png",Plot1];
];


If[ logic === 0 || logic == 3,
PrintTG["Integrate crosssection log scale"];
CheckTime[];
iteration = 0;
CrossSvertkaExp = NIntegrate[
		progress = WidthCross[s0*(10^kt)]*Lexp[10^(kt)]
   ,
   {kt, Log10[xmin*xmin], 0},
   Method -> "AdaptiveMonteCarlo",
   EvaluationMonitor :> (
      iteration++;
      If[Mod[iteration,100]===0,
      ShowStep[iteration,  kt,  progress]];
   )
];
PrintTG[StringJoin["\nCrossSection: ", ToString[CrossSvertkaExp*3.9*10^8], " pb\n"]];
Put[CrossSvertkaExp*3.9*10^8,csFile];
CheckTime[];
]


If[ logic === 1,
Print["Getting crosssection from buffer."];
CheckTime[];
CrossSvertkaExp  =  Get[csFile];
Print["CrossSection: ", CrossSvertkaExp," pb"];
CheckTime[];
]


If[ logic === 2,
Print["Integrate DemoCrossSection log scale"];
CheckTime[];
(*Off[General::stop];*)
iteration = 0;
progress = 0;
CrossSvertkaExp = NIntegrate[
   Module[{val},
      val = WidthCross[s0*(10^kt)]*Lexp[10^(kt)]; 
      progress = N[val];
      val
   ],
   {kt, Log10[xmin*xmin], 0},
   EvaluationMonitor :> (
      iteration++;
      If[Mod[iteration,10]===0,
      ShowStep[iteration,  kt,  progress]];
   )
];
Print["CrossSection: ", CrossSvertkaExp*3.9*10^8, " pb"];
Print["Integrate DemoCrossSection lin scale"];

iteration = 0;
progress = 0;
CrossSvertka = NIntegrate[
   Module[{val},
      val = WidthCross[s0*kt]*L[kt]; 
      progress = N[val];
      val
   ],
   {kt, xmin*xmin, 1},
   EvaluationMonitor :> (
      iteration++;
      If[Mod[iteration,10]===0,
      ShowStep[iteration,  kt,  progress]];
   )
];

Print["CrossSection: ", CrossSvertka*3.9*10^8, " pb"];
CheckTime[];
]
