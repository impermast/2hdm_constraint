#!/usr/bin/env wolframscript
(* ::Package:: *)

(* \:041e\:043f\:0446\:0438\:0438 \:0440\:0430\:0431\:043e\:0442\:044b *)
logic = 1; 
NumberOfPoints = 10;
(*  0 - \:043f\:0435\:0440\:0435\:0441\:0447\:0438\:0442\:0430\:0442\:044c \:0432\:0441\:0451, 
	1 - \:0437\:0430\:0433\:0440\:0443\:0437\:0438\:0442\:044c \:0432\:0441\:0435 \:0431\:0443\:0444\:0435\:0440\:044b, 
	2 - \:043f\:0435\:0440\:0435\:0441\:0447\:0438\:0442\:0430\:0442\:044c \:0442\:043e\:043b\:044c\:043a\:043e \:0441\:0435\:0447\:0435\:043d\:0438\:0435, 
	3 - \:043f\:0435\:0440\:0435\:0441\:0447\:0438\:0442\:0430\:0442\:044c \:0441\:0435\:0447\:0435\:043d\:0438\:044f \:0438 \:043f\:0434\:0444 *)

(* \:0427\:0442\:0435\:043d\:0438\:0435 \:0430\:0440\:0433\:0443\:043c\:0435\:043d\:0442\:043e\:0432 \:043a\:043e\:043c\:0430\:043d\:0434\:043d\:043e\:0439 \:0441\:0442\:0440\:043e\:043a\:0438 *)
If[Length[$ScriptCommandLine] > 1,
    logic = ToExpression[$ScriptCommandLine[[2]]];
];
If[ $FrontEnd === Null, 
    $FeynCalcStartupMessages = False; 
    $LoadFeynCalcMessages = False; 
    Print["tt>Z>ZZ calculation in 2HDM CPV, 1-loop.\n",
    "Calcultion of cross from PDF and matrix."]; 
  ];
If[ $Notebooks === False, 
	$FeynCalcStartupMessages = False;
	$LoadFeynCalcMessages = False; 
	Off[General::shdw];
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


Get["modules/ModelParams.wl"];
Get["modules/CheckTime.wl"];

meFile = "buffer/matrixElementZZZ.mx";
csFile = "buffer/crossSectionZZZ.mx";



(*model params*)
mh2 = 500;
mh3 = Sqrt[mh2^2+mh1^2];
a2 = 0.3;
a3 = 0.5;
Print["-------
Model parameters in code:
mh2 = ",mh2,
"\nmh3    = ",mh3,
"\nalpha2    = ",a2,
"\nalpha3  = ",a3,
"\n--------"];

(*Integration limits*)
ptmin = mh1+1;
s0 = 13000^2;
smin = 4*ptmin^2;  
xmin = Sqrt[smin/s0];
Print["-------\nIntegration limits in code:
ptmin = ",ptmin,
"\nsqrt(s0)    = ",Sqrt[s0],
"\nxmin  = ",xmin//N,"\n--------"];


(*download matrix element*)
Print["Loading matrix element."]
MaElread = Get[meFile];
MaElread = MaElread//.SmpChanger//.Params//.AngleChanger;
MaElreadPTL = MaElread//.PaveToLooptools;
MaEl[sX_,tX_] := Abs[MaElreadPTL/.{s->sX,t->tX}];

If[$Notebooks,
	Print["Test of matrix function:\n",
	"Numericaly: ",MaEl[s0,1300],
	"\nAnaliticaly:", MaEl[s,t]//Simplify]
]
CheckTime[]


(*download PDFs*)
Print["Loading pdf for run."]
xPDF[x_, Q_, id_] := Module[{result, scriptPath, output},
  scriptPath = FileNameJoin[{NotebookDirectory[],"../pdfcode/", "xPDF.py"}];
  result = RunProcess[{"sh", "-c", 
    "LD_LIBRARY_PATH=/usr/local/lib python3 " <> scriptPath <> " " <> ToString[x] <> " " <> ToString[Q] <> " " <> ToString[id]
  }];
  output = StringTrim[result["StandardOutput"]];
  If[StringMatchQ[output, NumberString], ToExpression[output], Null]
]
L[t_]:=Integrate[(1/x) *xPDF[x,t*s0,-1]*xPDF[t/x,t*s0,1],{x,t,1}]/Null^2;
Lexp[t_]:=Integrate[xPDF[10^yt,t*s0,1] * xPDF[t*10^(-yt),t*s0,-1] ,{yt, Log10[t],0}]*Log[10]/Null^2;
CheckTime[];




Print["Working on general crosssection"]
Pref[x_] := 1/(64 Pi*x((x/4)-MT^2));
WidthCross[s_] := Integrate[
Pref[s] (MaEl[s, t]*(s - MZ^2)^2  / ((s - MZ^2)^2 + MZ^2 * WZ^2) )/.Params, 
                                 {t, -s/2 * (1 + Sqrt[1 - (4*ptmin/Sqrt[s0])^2]), -s/2 * (1 - Sqrt[1 - (4*ptmin/Sqrt[s0])^2])}];                    
                                 
                          
CheckTime[];                                 


LogRange[a_, b_, n_] := Exp[Range[Log[a], Log[b], (Log[b] - Log[a])/(n - 1)]]//N

If[$Notebooks,
tVal = LogRange[xmin*xmin,1,NumberOfPoints];
texpVal = Range[Log10[xmin*xmin],0,-Log10[xmin*xmin]/(NumberOfPoints - 1)]//N;

crossVal = Table[WidthCross[i*s0]*L[i], {i, tVal}];
crossValExp = Table[WidthCross[10^i*s0]*Lexp[10^i], {i, texpVal}];

dataST = Transpose[{tVal*s0,crossVal}];
dataSTexp = Transpose[{tVal*s0,crossValExp}];


CheckTime[];
];




Print[dataSTexp];


Print[dataST]


If[ logic === 0,
Print["Integrate crosssection log scale"];
CheckTime[];
CrossSvertkaExp  =  AbsoluteTiming[NIntegrate[WidthCross[s0*(10^kt)]*Lexp[10^(kt)], {kt,Log10[xmin*xmin],0}]];
Print["CalcTiming: ", CrossSvertkaExp[[1]]];
Print["Crosssection: ", CrossSvertkaExp[[2]]*3.9*10^8, " pb"];
Put[CrossSvertkaExp[[2]]*3.9*10^8,csFile];
CheckTime[];
]



