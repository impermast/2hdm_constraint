#!/usr/bin/env wolframscript
(* ::Package:: *)

(* \:041e\:043f\:0446\:0438\:0438 \:0440\:0430\:0431\:043e\:0442\:044b *)
logic = 0; 
(*  0 - \:043f\:0435\:0440\:0435\:0441\:0447\:0438\:0442\:0430\:0442\:044c \:0432\:0441\:0451, 
	1 - \:0437\:0430\:0433\:0440\:0443\:0437\:0438\:0442\:044c \:0432\:0441\:0435 \:0431\:0443\:0444\:0435\:0440\:044b, 
	2 - \:043f\:0435\:0440\:0435\:0441\:0447\:0438\:0442\:0430\:0442\:044c \:0432\:0441\:0435 \:0441 \:0443\:043f\:0440\:043e\:0449\:0435\:043d\:043d\:044b\:043c \:043c\:0430\:0442\:0440\:0438\:0447\:043d\:044b\:043c, 
	3 - \:0437\:0430\:0433\:0440\:0443\:0437\:043a\:0430 WidthCross, \:0440\:0430\:0441\:0447\:0435\:0442 \:0441\:0435\:0447\:0435\:043d\:0438\:044f 
	*)
msg = False;
particleID = 2;
(* for PDF not FeynCalc
*)
ProcName = "ddZZZ";


(* \:0427\:0442\:0435\:043d\:0438\:0435 \:0430\:0440\:0433\:0443\:043c\:0435\:043d\:0442\:043e\:0432 \:043a\:043e\:043c\:0430\:043d\:0434\:043d\:043e\:0439 \:0441\:0442\:0440\:043e\:043a\:0438 *)
mh2num = 250;
If[Length[$ScriptCommandLine] > 1,
    logic = ToExpression[$ScriptCommandLine[[2]]];
    mh2num = ToExpression[$ScriptCommandLine[[3]]];
    msg = ToExpression[$ScriptCommandLine[[4]]],
];
If[ $FrontEnd === Null, 
    $FeynCalcStartupMessages = False; 
    $LoadFeynCalcMessages = False; 
    Print["tt>Z>ZZ calculation in 2HDM CPV, 1-loop.\nCalcultion of cross from PDF and matrix."]; 
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
f4teorFile  = "buffer/f4_teor.mx";
F4meanFile  = StringJoin["buffer/f4mean_", ProcName, ".mx"];


If[ logic === 0,
If[FileExistsQ[wcFile],
	WCread = Get[wcFile];
	WidthCross[ST_] := WCread/.{s->ST};
	,
	If[msg === True, PrintTG["File with Widthcross not found. Loading matrix element."]];
	{MaElread,MaElreadExp} = Get[meFile];
	MaElread = MaElread//.AngleChanger//.Params//.SmpChanger//.PaveToLooptools/.{FeynCalc`CA->3}/.{SMP["m_q"]->0};
	MaEl[sX_,tX_] := MaElread/.{s->sX,t->tX};
	If[msg === True, PrintTG["Working on WidthCross"]];
	Pref[x_] := 1/(16 Pi x^2);

	lambda[s_] := s^2 - 4 s MZ^2;
	tmin[s_] := -(s - 2*MZ^2)/2 - Sqrt[lambda[s]]/2;
	tmax[s_] := -(s - 2*MZ^2)/2 + Sqrt[lambda[s]]/2;
	(*-s/2 * (1 + Sqrt[1 - (4*ptmin/Sqrt[s0])^2])   -s/2 * (1 - Sqrt[1 - (4*ptmin/Sqrt[s0])^2])*)
	WidthCross[s_] := Abs[Pref[s] Integrate[MaEl[s, t], 
    {t,  tmin[s], tmax[s]}]];                                                                               
	
	Put[WidthCross[s],wcFile];
	CheckTime[];                 
];
                
If[msg === True,PrintTG["WidthCross calculated."]];
If[$Notebooks,
Print[WidthCross[s]//Simplify];
];
];



(*download PDFs*)
If[msg === True,
PrintTG["Loading pdf for run."]];

xPDF[x_, Q_, id_] := Module[{result, scriptPath, output},
  scriptPath = "../pdfcode/xPDF.py";
  result = RunProcess[{"sh", "-c", 
    "LD_LIBRARY_PATH=/usr/local/lib python3 " <> scriptPath <> " " <> ToString[x] <> " " <> ToString[Q] <> " " <> ToString[id]
  }];
  output = StringTrim[result["StandardOutput"]];
  If[StringMatchQ[output, NumberString], ToExpression[output], Null]
]

F4teor = Get[f4teorFile];
	F4teor = F4teor//.AngleChanger//.Params//.SmpChanger//.PaveToLooptools/.{FeynCalc`CA->3}/.{SMP["m_q"]->0};
	f4[sX_] := F4teor/.{s->sX};
	

L[t_]:=Integrate[(1/x) *xPDF[x,t*s0,-particleID]*xPDF[t/x,t*s0,particleID],{x,t,1}]/Null^2;
Lexp[t_]:=Log[10]*Integrate[xPDF[10^yt,t*s0,particleID] * xPDF[t*10^(-yt),t*s0,-particleID] ,{yt, Log10[t],0}]/Null^2;
CheckTime[]; 


(*model params*)
mh2 = mh2num;
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
f4experimental = 0.001;
s0 = 8000^2;
smin =(2*MZ+0.05)^2;  
xmin = Sqrt[smin/s0];
Print["-------\nIntegration limits in code:",
"\nf4exp    = ",f4experimental,
"\nsqrt(s0)    = ",Sqrt[s0],
"\nxmin  = ",xmin//N,"\n--------"];


If[ logic === 0,
If[msg === True,PrintTG["Integrate crosssection log scale"]];
CheckTime[];
iteration = 0;
Norma = NIntegrate[
		progress = WidthCross[s0*(10^kt)]*Lexp[10^(kt)]* (Log[10])
   ,
   {kt, Log10[xmin*xmin], 0},
   Method -> "AdaptiveMonteCarlo"(*,
   EvaluationMonitor :> (
      iteration++;
      If[Mod[iteration,500]===0,
      ShowStep[iteration,  kt,  progress]];
   )*)];

CrossSvertka = NIntegrate[
		f4[s0*(10^kt)]*WidthCross[s0*(10^kt)]*Lexp[10^(kt)]* (Log[10])
   ,
   {kt, Log10[xmin*xmin], 0},
   Method -> "AdaptiveMonteCarlo"
]; 
F4mean =  Abs[CrossSvertka/Norma];

formatSciPrec[x_] := ScientificForm[x, 3, NumberFormat -> (Row[{#1, "\[Times]10^", #3}] &)];
strOutput = StringJoin[
"params: sqrt(s0) = ",ToString[Sqrt[s0]], ",  f4exp = ",ToString[f4experimental], ",  mh2 = ",ToString[mh2],"\n",
"\n F4mean: ", ToString[formatSciPrec[F4mean]], " \n",
"\n Norma: ", ToString[formatSciPrec[Norma*3.9*10^8]], " pb\n"];
PrintTG[strOutput];
]


If[ logic === 1,
If[msg === True,Print["Getting f4mean from buffer."]];
CheckTime[];
F4mean =  Get[F4meanFile];
Print["CrossSection: ", F4mean];
CheckTime[];
]


If[Length[$ScriptCommandLine] > 1,
output = OpenAppend[F4meanFile];
WriteString[output,  "\n" <> ToString[mh2] <> "\t" <> ToString[formatSciPrec[F4mean]]];
Close[output];
];
