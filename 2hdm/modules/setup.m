(* ::Package:: *)

(*============================================================================*)
(*  setup.m \[Dash] Universal initialization for Notebook and Script modes          *)
(*                                                                            *)
(*  Automatically detects execution mode and sets up paths accordingly.       *)
(*============================================================================*)


Print["Starting setup..."];

(* Suppress FeynCalc startup messages *)
$FeynCalcStartupMessages = False;
$LoadFeynCalcMessages = False;
$LoadAddOns = {"FeynArts"};

(* --------------------------------------------------------------------------- *)
(* 1. Determine project root directory   adn pathed                                   *)
(* --------------------------------------------------------------------------- *)


Global`SetupDir = DirectoryName @ ExpandFileName[$InputFileName];
Global`ProjectRoot = ExpandFileName[FileNameJoin[{Global`SetupDir, ".."}]];
Print["Project root: ", Global`ProjectRoot];



Global`ProjectRoot = ExpandFileName @ DirectoryName[Global`SetupDir];
Global`ModulesDir  = ExpandFileName @FileNameJoin[{Global`ProjectRoot, "modules"}];
Global`SetupRootDir = ExpandFileName @FileNameJoin[{Global`ProjectRoot, "setup"}];
Global`xPDFScriptPath = ExpandFileName @FileNameJoin[
    {Global`ModulesDir, "pdfcode", "xPDF.py"}
];
Global`IsWindows = ($OperatingSystem === "Windows");
Global`IsLinux   = ($OperatingSystem === "Unix");

Switch[$OperatingSystem,

	    "Windows",
	(
	    Global`PythonCommand = "python";
	    Global`RunCommand    = "cmd";
	    Global`RunSuffix     = "/c";
	
	    Global`LoopToolsPath = ExpandFileName @FileNameJoin[
	        {Global`SetupRootDir, "LoopTools-Cygwin.exe"}
	    ];
	
	    Global`FeynArtsModelsPath = ExpandFileName @FileNameJoin[
	        {$UserBaseDirectory, "Applications", "FeynCalc", "FeynArts", "Models"}
	    ];
	
	    (* LHAPDF \:0447\:0435\:0440\:0435\:0437 WSL *)
	    (* \:041a\:043e\:043d\:0432\:0435\:0440\:0442\:0430\:0446\:0438\:044f Windows \:043f\:0443\:0442\:0438 \:0432 WSL \:043f\:0443\:0442\:044c: D:\foo\bar -> /mnt/d/foo/bar *)
		Global`xPDFScriptPathWSL = "/mnt/" <>
		    ToLowerCase[StringTake[Global`xPDFScriptPath, 1]] <>
		    StringReplace[StringDrop[Global`xPDFScriptPath, 2], "\\" -> "/"];
	    Global`LHAPDFInstallDir = "/mnt/d/progs/2hdm_constraint/setup/lhapdf_install";
	    Global`VenvPath = "/mnt/d/progs/2hdm_constraint/setup/venv/bin/activate";
	    Global`PythonEnvCommand = 
	        "source " <> Global`VenvPath <>
	        " && export LD_LIBRARY_PATH=" <> Global`LHAPDFInstallDir <> "/lib:$LD_LIBRARY_PATH" <>
	        " && export LHAPDF_DATA_PATH=" <> Global`LHAPDFInstallDir <> "/share/LHAPDF" <>
	        " && python3";
	),

    "Unix",
    (
        Global`PythonCommand = "python3";
        Global`RunCommand    = "bash";
        Global`RunSuffix     = "-c";

        Global`LoopToolsPath = ExpandFileName @FileNameJoin[
            {$UserBaseDirectory, "Applications", "LoopTools", "bin", "LoopTools"}
        ];

        Global`FeynArtsModelsPath = ExpandFileName @FileNameJoin[
            {$UserBaseDirectory, "Applications", "FeynCalc", "FeynArts", "Models"}
        ];
    ),

    _,
    (
        Print["Error: unsupported operating system: ", $OperatingSystem];
        Abort[]
    )
];



xPDF[x_, Q_, id_] := Module[{result, output, wslScript},
    wslScript = Global`PythonEnvCommand <> " " <>
        Global`xPDFScriptPathWSL <>
        " " <> ToString[x] <>
        " " <> ToString[Q] <>
        " " <> ToString[id];
    result = RunProcess[{"wsl", "bash", "-c", wslScript}];
    output = StringTrim[result["StandardOutput"]];
    If[StringMatchQ[output, NumberString], ToExpression[output], Null]
]


(* --------------------------------------------------------------------------- *)
(* 4. Load FeynCalc                                                            *)
(* --------------------------------------------------------------------------- *)
Global`ApplicationsDir = FileNameJoin[{$UserBaseDirectory, "Applications"}];
If[!MemberQ[$Path, Global`ApplicationsDir], PrependTo[$Path, Global`ApplicationsDir]];
$LoadFeynArts = True;
Check[
    <<"FeynCalc`",
    Print["FeynCalc not found. Attempting installation..."];
    Import["https://raw.githubusercontent.com/FeynCalc/feyncalc/master/install.m"];
    InstallFeynCalc[]
];

(* --------------------------------------------------------------------------- *)
(* 5. Load THDM model                                                          *)
(* --------------------------------------------------------------------------- *)
If[!MemberQ[$ModelPath, Global`FeynArtsModelsPath],
   AppendTo[$ModelPath, Global`FeynArtsModelsPath]
];
Check[LoadModel["THDMCPV"], Print["Warning: No 2HDM model found"]];
$FAVerbose = 0;

(* --------------------------------------------------------------------------- *)
(* 6. Install LoopTools                                                        *)
(* --------------------------------------------------------------------------- *)

If[FileExistsQ[Global`LoopToolsPath],
    Install[Global`LoopToolsPath];
    Print["LoopTools installed from: ", Global`LoopToolsPath],
    Print["Warning: LoopTools not found at: ", Global`LoopToolsPath]
];


  
(* --------------------------------------------------------------------------- *)
(* 7. Load project modules                                                     *)
(* --------------------------------------------------------------------------- *)

Get[FileNameJoin[{Global`ProjectRoot, "modules", "ModelParams.wl"}]];
Get[FileNameJoin[{Global`ProjectRoot, "modules", "SaveToCSVmodule.wl"}]];
Get[FileNameJoin[{Global`ProjectRoot, "modules", "FunctionalModules.wl"}]];

(* --------------------------------------------------------------------------- *)
(* 8. Notebook-specific settings (only in Notebook mode)                       *)
(* --------------------------------------------------------------------------- *)
If[$Notebooks,  
    SetOptions[EvaluationNotebook[], DefaultFormatType -> StandardForm];

    SetOptions[
        Cells[EvaluationNotebook[], CellStyle -> "Input"],
        FormatType -> StandardForm
    ];
    
    Print["Notebook settings applied."];
];

(* --------------------------------------------------------------------------- *)
(* 9. Utility functions                                                        *)
(* --------------------------------------------------------------------------- *)
LogRange1[a_?NumericQ, b_?NumericQ, n_Integer] := 
    Table[10^i, {i, Range[Log10[a], Log10[b], (Log10[b] - Log10[a])/n]}];

Print["Setup complete."];

