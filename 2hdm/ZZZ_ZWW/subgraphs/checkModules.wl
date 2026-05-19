(* ::Package:: *)

(* \:041e\:043f\:0446\:0438\:0438 \:0440\:0430\:0431\:043e\:0442\:044b *)
logic = 0; (* 0 - recalc All, 1 - download all from budder, 2 - recalc only some, 3 - \:043frecalc only matrix *)
ReactionName = "ZWW";
ProcName = "uu"<>ReactionName;
particleID = 7;

(* For feynCalc not PDF
7 u, 8 c, 9 t
10d, 11s, 12b  g4
*)
(* \:0427\:0442\:0435\:043d\:0438\:0435 \:0430\:0440\:0433\:0443\:043c\:0435\:043d\:0442\:043e\:0432 \:043a\:043e\:043c\:0430\:043d\:0434\:043d\:043e\:0439 \:0441\:0442\:0440\:043e\:043a\:0438 *)
If[Length[$ScriptCommandLine] > 1,
    logic = ToExpression[$ScriptCommandLine[[2]]];
];
If[ $FrontEnd === Null, 
    $FeynCalcStartupMessages = False; 
    $LoadFeynCalcMessages = False; 
    Print[ProcName," calculation in 2HDM CPV, 1-loop.\n",
    "Calcultion of matrix element from diagrams."]; 
  ];
If[ $Notebooks === False,
	$FeynCalcStartupMessages = False;
	$LoadFeynCalcMessages = False; 
	Off[General::shdw];
    ,
    SetDirectory[NotebookDirectory[]]
  ];

$LoadAddOns = {"FeynArts"};
$PrePrint = InputForm;

Quiet[<<FeynCalc`] 
AppendTo[$ModelPath, "/home/kds/.Mathematica/Applications/FeynArts/Models/"];
Quiet[Install["LoopTools"]];
Quiet[Needs["LoopTools`"]];
$FAVerbose = 0; 
Unprotect[PaVe, Li2, A0, A00, B0, B1, B00, B11, DB0, DB1, C0, D0];
Remove[PaVe, Li2, A0, A00, B0, B1, B00, B11, DB0, DB1, C0, D0];
(* \:0424\:0430\:0439\:043b\:044b \:0434\:043b\:044f \:0431\:0443\:0444\:0435\:0440\:043e\:0432 *)



SetDirectory[NotebookDirectory[]];
Get["../../modules/FunctionalModules.wl"];
Get["../../modules/ModelParams.wl"];
(* \:0424\:043e\:0440\:043c\:0438\:0440\:0443\:0435\:043c \:043f\:0443\:0442\:0438 \:043a \:0444\:0430\:0439\:043b\:0430\:043c *)
Print[Directory[]]



expr = (X1*X2*X3)^2 + FeynCalc`SMP["m_Z"]*X1;
AngleChanger[expr]          (* \:0430\:043d\:0430\:043b\:0438\:0442\:0438\:043a\:0430 *)
NumEvaluate[%]              (* \:0447\:0438\:0441\:043b\:0435\:043d\:043d\:044b\:0435 \:0437\:043d\:0430\:0447\:0435\:043d\:0438\:044f, \:0435\:0441\:043b\:0438 \[Alpha]\:1d62 \:0437\:0430\:0434\:0430\:043d\:044b *)



(* orthonormality of R-columns *)
Print[AngleChanger[X1^2 + X2^2 + X3^2]];          (* \[RightArrow] 1 *)

(* fast shortcuts *)
Print[AngleChanger[X1*X2*X3]  ]                  (* \[RightArrow]  Sin[\[Alpha]2]^2 Sin[\[Alpha]3] Cos[\[Alpha]3] Cos[\[Alpha]2] *)
Print[AngleChanger[(X1*X2*X3)^2]  ]              (* \[RightArrow]  Sin[\[Alpha]2]^4 Sin[\[Alpha]3]^2 Cos[\[Alpha]3]^2 Cos[\[Alpha]2]^2 *)

(* SMP replacements only *)
exprSMP   = FeynCalc`SMP["m_W"]^2 - FeynCalc`SMP["e"]^2;
exprModel = exprSMP /. SmpChanger         (* \[RightArrow] MW^2 - e^2 *)
exprNum   = exprModel /. Params // N      (* numeric result *)




