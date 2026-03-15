(* ::Package:: *)
(*============================================================================*)
(*  setup.m – Universal initialization for Notebook and Script modes          *)
(*                                                                            *)
(*  Automatically detects execution mode and sets up paths accordingly.       *)
(*============================================================================*)

ClearAll[];
Print["Starting setup..."];

(* Suppress FeynCalc startup messages *)
$FeynCalcStartupMessages = False;
$LoadFeynCalcMessages = False;
$LoadAddOns = {"FeynArts"};
$PrePrint = InputForm;

(* --------------------------------------------------------------------------- *)
(* 1. Determine project root directory                                         *)
(* --------------------------------------------------------------------------- *)
ProjectRoot = If[
    $Notebooks,
    (* Notebook mode: go up from notebook directory to project root *)
    (* Assumes notebooks are in subdirectories like ZZZ_ZWW/ *)
    FileNameJoin[{NotebookDirectory[], ".."}],
    (* Script mode: use directory of the calling script *)
    If[StringQ[$InputFileName] && $InputFileName =!= "",
        FileNameJoin[{DirectoryName[$InputFileName], "..", ".."}],  (* Go up 2 levels from ZZZ_ZWW/ *)
        Directory[]  (* Fallback to current directory *)
    ]
];

Print["Project root: ", ProjectRoot];

(* --------------------------------------------------------------------------- *)
(* 2. LoopTools path – OS dependent                                            *)
(* --------------------------------------------------------------------------- *)
LooptoolsPath = If[$OperatingSystem === "Windows",
    FileNameJoin[{ProjectRoot, "setup", "LoopTools-Cygwin.exe"}],
    FileNameJoin[{$UserBaseDirectory, "Applications", "LoopTools", "bin", "LoopTools"}]
];

(* --------------------------------------------------------------------------- *)
(* 3. FeynArts models path                                                     *)
(* --------------------------------------------------------------------------- *)
FeynartsModelsPath = FileNameJoin[{
    $UserBaseDirectory, "Applications", "FeynCalc", "FeynArts", "Models"
}];

(* --------------------------------------------------------------------------- *)
(* 4. Load FeynCalc                                                            *)
(* --------------------------------------------------------------------------- *)
Check[
    <<"FeynCalc`",
    Print["FeynCalc not found. Attempting installation..."];
    Import["https://raw.githubusercontent.com/FeynCalc/feyncalc/master/install.m"];
    InstallFeynCalc[]
];

(* --------------------------------------------------------------------------- *)
(* 5. Load THDM model                                                          *)
(* --------------------------------------------------------------------------- *)
AppendTo[$ModelPath, FeynartsModelsPath];
Check[LoadModel["THDMCPV"], Print["Warning: No 2HDM model found"]];
$FAVerbose = 0;

(* --------------------------------------------------------------------------- *)
(* 6. Install LoopTools                                                        *)
(* --------------------------------------------------------------------------- *)
If[FileExistsQ[LooptoolsPath],
    Install[LooptoolsPath];
    Print["LoopTools installed from: ", LooptoolsPath],
    Print["Warning: LoopTools not found at: ", LooptoolsPath]
];

(* --------------------------------------------------------------------------- *)
(* 7. Load project modules                                                     *)
(* --------------------------------------------------------------------------- *)
SetDirectory[ProjectRoot];
Get[FileNameJoin[{ProjectRoot, "modules", "ModelParams.wl"}]];
Get[FileNameJoin[{ProjectRoot, "modules", "SaveToCSVmodule.wl"}]];
Get[FileNameJoin[{ProjectRoot, "modules", "FunctionalModules.wl"}]];

(* --------------------------------------------------------------------------- *)
(* 8. Notebook-specific settings (only in Notebook mode)                       *)
(* --------------------------------------------------------------------------- *)
If[$Notebooks,
    $PrePrint = .;
    $Post = .;
    
    SetOptions[EvaluationNotebook[], DefaultFormatType -> StandardForm];
    
    SetOptions[
        Cells[EvaluationNotebook[], CellStyle -> "Output"],
        FormatType -> StandardForm
    ];
    
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