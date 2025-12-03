(* ::Package:: *)

If[ $FrontEnd === Null,
	$FeynCalcStartupMessages = False;
	Print[description];
];
If[ $Notebooks === False,
	$FeynCalcStartupMessages = False
];

$LoadAddOns = {"FeynArts"};
$PrePrint = InputForm;

Quiet[<<FeynCalc`];
AppendTo[$ModelPath, "/home/kds/progs/math/FeynArts-3.12/Models/"]

$FAVerbose = 0;

(* --- \:041f\:0443\:0442\:0438 --- *)
Install["/home/kds/progs/math/LoopTools-2.16/x86_64-Linux/bin/LoopTools"];
Quiet[Needs["LoopTools`"]];
(* ---------------------------------------------------- *)

(* \:041e\:043f\:0440\:0435\:0434\:0435\:043b\:0435\:043d\:0438\:0435 \:0432\:0441\:043f\:043e\:043c\:043e\:0433\:0430\:0442\:0435\:043b\:044c\:043d\:043e\:0439 \:0444\:0443\:043d\:043a\:0446\:0438\:0438 *)
LogRange1[a_?NumericQ, b_?NumericQ, n_Integer] := 
	Table[10^i,{i,Range[Log10[a],Log10[b],(Log10[b]-Log10[a])/n]}];


setupDirectory = DirectoryName[$InputFileName];
projectRootDirectory = ParentDirectory[setupDirectory];
SetDirectory[projectRootDirectory];

Get["modules/ModelParams.wl"];
Get["modules/SaveToCSVmodule.wl"];
Get["modules/FunctionalModules.wl"];


(*\:0411\:0435\:0437 \:044d\:0442\:043e\:0433\:043e \:043d\:0435 \:0441\:0442\:0440\:043e\:044f\:0442\:0441\:044f \:043d\:043e\:0440\:043c \:0433\:0440\:0430\:0444\:0438\:043a\:0438*)

Unset[$PrePrint];
Unset[$Post];

$FormatType = StandardForm;
SetOptions[EvaluationNotebook[], FormatType -> StandardForm];
