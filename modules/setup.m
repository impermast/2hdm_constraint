(* ::Package:: *)

ClearAll[];
Print["Starting setup"];
$FeynCalcStartupMessages = False;

$LoadAddOns = {"FeynArts"};
$PrePrint = InputForm;


(* PATHes *)
(*
ProjectPath = "/home/kds/sci/2hdm_constraint/";
FeynartsModelsPath1 = "/home/kds/progs/math/FeynArts-3.12/Models/";
FeynartsModelsPath2 = "/home/kds/.Mathematica/Applications/FeynArts/Models/";*)

ProjectPath = "D:\\progs\\2hdm_constraint\\";
FeynartsModelsPath1 = FileNameJoin[{$UserBaseDirectory,"Applications","FeynCalc","FeynArts",
"Models"}];

LooptoolsPath = FileNameJoin[{ProjectPath,"setup\\LoopTools-Cygwin.exe"}];


Check[
	<<"FeynCalc`",
	Import["https://raw.githubusercontent.com/FeynCalc/feyncalc/master/install.m"];
	InstallFeynCalc[]
];


(*FeynRules models from https://feynrules.irmp.ucl.ac.be/wiki/ModelDatabaseMainPage*)
AppendTo[$ModelPath, FeynartsModelsPath1];
Check[LoadModel["THDMCPV"], Print["No 2HDM model found"]];
(*prevent FeynArts from printing info messages*)
$FAVerbose = 0;




Install[LooptoolsPath];
If[Abs[B0[1000,100,100]]== 4.857076168125554,
Print["Correct instalation of Looptools"],
Print["[Error] Looptools failed"]
];


(* \:041e\:043f\:0440\:0435\:0434\:0435\:043b\:0435\:043d\:0438\:0435 \:0432\:0441\:043f\:043e\:043c\:043e\:0433\:0430\:0442\:0435\:043b\:044c\:043d\:043e\:0439 \:0444\:0443\:043d\:043a\:0446\:0438\:0438 *)
LogRange1[a_?NumericQ, b_?NumericQ, n_Integer] := 
	Table[10^i,{i,Range[Log10[a],Log10[b],(Log10[b]-Log10[a])/n]}];


SetDirectory[NotebookDirectory[]];

Get["ModelParams.wl"];
Get["SaveToCSVmodule.wl"];
Get["FunctionalModules.wl"];


(*\:0411\:0435\:0437 \:044d\:0442\:043e\:0433\:043e \:043d\:0435 \:0441\:0442\:0440\:043e\:044f\:0442\:0441\:044f \:043d\:043e\:0440\:043c \:0433\:0440\:0430\:0444\:0438\:043a\:0438*)

$PrePrint =.; 
$Post    =.;

SetOptions[EvaluationNotebook[], DefaultFormatType -> StandardForm];

SetOptions[
  Cells[EvaluationNotebook[], CellStyle -> "Output"],
  FormatType -> StandardForm
];

SetOptions[
  Cells[EvaluationNotebook[], CellStyle -> "Input"],
  FormatType -> StandardForm
];



(* EXAMPLE OF USE
SetDirectory[NotebookDirectory[]];
<< "../modules/setup.m"
SetDirectory[NotebookDirectory[]];
Print[Directory[]]

*)
