#!/usr/bin/env wolframscript
(* ::Package:: *)

(* \:041e\:043f\:0446\:0438\:0438 \:0440\:0430\:0431\:043e\:0442\:044b *)
logic = 0; 
(*  0 - \:043f\:0435\:0440\:0435\:0441\:0447\:0438\:0442\:0430\:0442\:044c \:0432\:0441\:0451, 
	1 - \:0437\:0430\:0433\:0440\:0443\:0437\:0438\:0442\:044c \:0432\:0441\:0435 \:0431\:0443\:0444\:0435\:0440\:044b, 
	2 - \:043f\:0435\:0440\:0435\:0441\:0447\:0438\:0442\:0430\:0442\:044c \:0432\:0441\:0435 \:0441 \:0443\:043f\:0440\:043e\:0449\:0435\:043d\:043d\:044b\:043c \:043c\:0430\:0442\:0440\:0438\:0447\:043d\:044b\:043c, 
	3 - \:0437\:0430\:0433\:0440\:0443\:0437\:043a\:0430 WidthCross, \:0440\:0430\:0441\:0447\:0435\:0442 \:0441\:0435\:0447\:0435\:043d\:0438\:044f 
	*)


(* \:0427\:0442\:0435\:043d\:0438\:0435 \:0430\:0440\:0433\:0443\:043c\:0435\:043d\:0442\:043e\:0432 \:043a\:043e\:043c\:0430\:043d\:0434\:043d\:043e\:0439 \:0441\:0442\:0440\:043e\:043a\:0438 *)
(* --- Defaults --- *)
ReactionName = "ZWW";
pID = "dd";

(* \:0447\:0438\:0441\:043b\:043e\:0432\:044b\:0435 \:043f\:0430\:0440\:0430\:043c\:0435\:0442\:0440\:044b *)
sqrts0    = 1000;           (* \:0434\:0435\:0444\:043e\:043b\:0442, \:0435\:0441\:043b\:0438 \:043d\:0435 \:043f\:0435\:0440\:0435\:0434\:0430\:043d *)
mh2num = 250.;
mhc    =400.;                

(* --- CLI:  s0  mh2  mhc  pID  ReactionName  [msg] --- *)
args = Rest[$ScriptCommandLine];

toNum[x_] := Quiet@Check[ToExpression[x], $Failed];
toBool[x_] := Which[
  StringMatchQ[ToString[x], "True" | "true"], True,
  StringMatchQ[ToString[x], "False" | "false"], False,
  True, False
];

If[Length[args] >= 1 && toNum[args[[1]]] =!= $Failed, sqrts0     = N@toNum[args[[1]]]];
If[Length[args] >= 2 && toNum[args[[2]]] =!= $Failed, mh2num = N@toNum[args[[2]]]];
If[Length[args] >= 3 && toNum[args[[3]]] =!= $Failed, mhc    = N@toNum[args[[3]]]];
If[Length[args] >= 4, pID          = ToString@args[[4]]];
If[Length[args] >= 5, ReactionName = ToUpperCase@ToString@args[[5]]];
If[Length[args] >= 6, msg          = toBool@args[[6]]];

ProcName = StringJoin[pID, ReactionName];

Off[General::shdw]; Off[C0::"msg"];  (* \:043f\:043e\:0434\:0430\:0432\:0438\:0442\:044c \:043b\:0438\:0448\:043d\:0438\:0435 \:0432\:043e\:0440\:043d\:0438\:043d\:0433\:0438 *)

If[ $FrontEnd === Null, 
    $FeynCalcStartupMessages = False; 
    $LoadFeynCalcMessages = False;  
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


scriptDir = If[StringQ[$InputFileName], DirectoryName[$InputFileName], $InitialDirectory];
SetDirectory[scriptDir];

Get["../modules/SaveToCSVmodule.wl"];
Get["../modules/ModelParams.wl"];
Get["../modules/FunctionalModules.wl"];

ampFile = StringJoin["buffer/amp_", ProcName, ".mx"];
meFile  = StringJoin["buffer/me_", ProcName, ".mx"];
wcFile  = StringJoin["buffer/wc_", ProcName, ".mx"];
f4teorFile  = "buffer/f4_teor"<>ReactionName<>".mx";
f4meanCSV = StringJoin["buffer/f4mean_", ProcName, ".csv"];
f4meanMX  = StringJoin["buffer/f4mean_", ProcName, ".mx"]; 


If[ logic === 0,
If[FileExistsQ[wcFile],
	WCread = Get[wcFile];
	WidthCross[ST_] := WCread[[1]]/.{s->ST,St->ST};
	,
	If[msg === True, PrintTG["File with Widthcross not found. Loading matrix element."]];
	{MaElread,MaElreadExp} = Get[meFile];
	MaElread = MaElread//.AngleChanger//.Params//.SmpChanger//.PaveToLooptools/.{FeynCalc`CA->3}/.{SMP["m_q"]->0};
	MaEl[sX_,tX_] := MaElread/.{St->sX,s->sX,t->tX};
	If[msg === True, PrintTG["Working on WidthCross"]];
	Pref[x_] := 1/(16 Pi x^2);

	lambda[s_] := s^2 - 4 s MZ^2//.Params;
	tmin[s_] := -(s - 2*MZ^2)/2 - Sqrt[lambda[s]]/2//.Params;
	tmax[s_] := -(s - 2*MZ^2)/2 + Sqrt[lambda[s]]/2//.Params;
	(*-s/2 * (1 + Sqrt[1 - (4*ptmin/Sqrt[s0])^2])   -s/2 * (1 - Sqrt[1 - (4*ptmin/Sqrt[s0])^2])*)
	WidthCross[s_] := Abs[Pref[s] Integrate[MaEl[s, t], 
    {t,  tmin[s], tmax[s]}]];                                                                               
	
	Put[WidthCross[s],wcFile];                 
];
                
If[msg === True,PrintTG["WidthCross calculated."]];
If[$Notebooks,
Print[WidthCross[s]//Simplify];
Print[WidthCross[s0]]
];
];



(*download PDFs*)
If[msg === True,
PrintTG["Loading pdf for run."]];
\:0430 \:043f\:043e
xPDF[x_, Q_, id_] := Module[{result, scriptPath, output},
  scriptPath = "../pdfcode/xPDF.py";
  result = RunProcess[{"sh", "-c", 
    "LD_LIBRARY_PATH=/usr/local/lib python3 " <> scriptPath <> " " <> ToString[x] <> " " <> ToString[Q] <> " " <> ToString[id]
  }];
  output = StringTrim[result["StandardOutput"]];
  If[StringMatchQ[output, NumberString], ToExpression[output], Null]
]

F4teor = Get[f4teorFile];
	F4teor = F4teor/.{SMP["m_q"]->0, mZ->MZ, (SUNFDelta[SUNFIndex[Col1], SUNFIndex[Col2]])->1}//.AngleChanger//.Params//.SmpChanger//.PaveToLooptools/.{FeynCalc`CA->3};
	f4[sX_] := F4teor/.{s->sX};
	

L[t_]:=Integrate[(1/x) *xPDF[x,t*s0,-particleID]*xPDF[t/x,t*s0,particleID],{x,t,1}]/Null^2;
Lexp[t_]:=Log[10]*Integrate[xPDF[10^yt,t*s0,particleID] * xPDF[t*10^(-yt),t*s0,-particleID] ,{yt, Log10[t],0}]/Null^2;


(*model params*)
mh2 = mh2num;
mh3 = Sqrt[mh2^2+vev^2]//.Params;
mhc= mhc;
a2 = 0.1317; 
a3 = 0.7854;

(*Integration limits*)
f4experimental = 0.001;
s0 = sqrts0^2;
smin =(2*MZ+0.05)^2//.Params;  
xmin = Sqrt[smin/s0];
If[msg,
	Print["-------\nIntegration limits in code:",
	"\nf4exp    = ",f4experimental,
	"\nsqrt(s0)    = ",Sqrt[s0],
	"\nxmin  = ",xmin//N,"\n--------"];
];





If[ logic === 0,
If[msg === True,PrintTG["Integrate crosssection log scale"]];
 
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
		Abs[f4[s0*(10^kt)]]*WidthCross[s0*(10^kt)]*Lexp[10^(kt)]* (Log[10])
   ,
   {kt, Log10[xmin*xmin], 0},
   Method -> "AdaptiveMonteCarlo"
]; 
F4mean =  Abs[CrossSvertka/Norma];

fmt[x_?NumericQ] := ScientificForm[N@x, 3, NumberFormat -> (Row[{#1, "*10^", #3}] &)];
val[x_] := If[NumericQ[x], ToString@fmt[x], "-"];

msgText = StringJoin[
  ToString[ProcName], ":\n",
  "params: sqrts0 ", val[Sqrt[s0]], " mh2 ", val[mh2], " mhc ", val[mhc], "\n",
  "f4mean: ", val[F4mean], "\n",
  "crosssection: ", val[Norma*3.9*10^8], " pb\n",
  " =========================== \n"
];
PrintTG[msgText];
]


If[ logic === 1,
  If[msg === True, Print["Getting f4mean from buffer."]];
  If[ FileExistsQ[f4meanCSV],
      Module[{hdr,row,posF4,posN, valF4, valN},
        {hdr,row} = SaveToCSVmodule`ReadCSVLastRow[f4meanCSV];
        posF4 = First@Flatten@Position[hdr, "F4mean"];
        posN  = First@Flatten@Position[hdr, "Norma_pb"];
        valF4 = ToExpression[row[[posF4]]];
        valN  = ToExpression[row[[posN]]];
        F4mean = valF4;
        Norma  = valN;       (* \:0445\:0440\:0430\:043d\:044e pb \:0432 CSV; \:0442\:0443\:0442 \:0432\:043e\:0437\:0432\:0440\:0430\:0449\:0430\:044e \:0432 \:043d\:0430\:0442. \:0435\:0434. *)
      ],
      F4mean = Get[f4meanMX];           (* \:0441\:0442\:0430\:0440\:044b\:0439 \:0444\:043e\:0440\:043c\:0430\:0442 *)
  ];
  Print["CrossSection: ", N[10^F4mean]]; 
];



If[Length[$ScriptCommandLine] > 1,
  (* \:0437\:0430\:043f\:0438\:0441\:044c \:0440\:0435\:0437\:0443\:043b\:044c\:0442\:0430\:0442\:0430 \:0443\:0441\:0440\:0435\:0434\:043d\:0435\:043d\:0438\:044f *)
    fmt1[x_?NumericQ] := ToString @ FortranForm @ SetPrecision[N@x, 16];
    safeLog10[x_?NumericQ] := If[x>0, Log10[x], -Infinity];
	colNames = {"timestamp","sqrts0","mh2","mhc","log10_f4mean","log10_norma_pb"};
	row = {
	  DateString[],
	  fmt1[Sqrt[s0]],
	  fmt1[mh2],
	  If[ValueQ[mhc], fmt1[mhc], ""],
	  fmt1[safeLog10[F4mean]],
	  fmt1[safeLog10[Norma*3.9*10^8]]
	};
	SaveToCSVmodule`EnsureCSVHeader[f4meanCSV, colNames];
	SaveToCSVmodule`AppendRowToCSV[f4meanCSV, row];
	If[msg === True, Print[row]];
];

