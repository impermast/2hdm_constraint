(* ::Package:: *)

(* ::Package:: *)
(**)


BeginPackage["SaveToCSVmodule`"];

SaveTableToCSV::usage = "SaveTableToCSV[names, table, file] writes CSV with header.";
EnsureCSVHeader::usage = "EnsureCSVHeader[file, names] creates file with header if missing.";
AppendRowToCSV::usage = "AppendRowToCSV[file, row] appends one CSV row.";
ReadCSVLastRow::usage = "ReadCSVLastRow[file] returns {header, lastRow}.";

ReadF4CSV::usage  = "ReadF4CSV[file] -> list of <|...|> rows with numeric fields and source";
LoadF4Data::usage = "LoadF4Data[dir:\"buffer\", pattern:\"f4mean_*.csv\"] -> Dataset";

Begin["`Private`"];

SaveTableToCSV[names_List, table_List, file_String] := Module[{s},
  s = OpenWrite[file];
  WriteLine[s, StringRiffle[names, ","]];
  Do[WriteLine[s, StringRiffle[ToString /@ r, ","]], {r, table}];
  Close[s]; file
];

EnsureCSVHeader[file_String, names_List] := Module[{},
  If[!FileExistsQ[file] || FileByteCount[file] == 0, SaveTableToCSV[names, {}, file]];
  file
];

AppendRowToCSV[file_String, row_List] := Module[
  {lockFile, s, acquired, attempts, maxAttempts, sleepMs},

  lockFile    = file <> ".lock";
  maxAttempts = 200;   (* \:0434\:043e 10 \:0441\:0435\:043a\:0443\:043d\:0434 \:043e\:0436\:0438\:0434\:0430\:043d\:0438\:044f *)
  sleepMs     = 50;    (* \:043c\:0441 \:043c\:0435\:0436\:0434\:0443 \:043f\:043e\:043f\:044b\:0442\:043a\:0430\:043c\:0438    *)
  acquired    = False;

  (* \:0416\:0434\:0451\:043c \:043e\:0441\:0432\:043e\:0431\:043e\:0436\:0434\:0435\:043d\:0438\:044f \:043b\:043e\:043a\:0430 *)
  Do[
    If[!FileExistsQ[lockFile],
      (* \:041f\:044b\:0442\:0430\:0435\:043c\:0441\:044f \:0437\:0430\:0445\:0432\:0430\:0442\:0438\:0442\:044c \[LongDash] \:0441\:043e\:0437\:0434\:0430\:0451\:043c \:043b\:043e\:043a-\:0444\:0430\:0439\:043b *)
      Quiet[
        Module[{ls},
          ls = OpenWrite[lockFile];
          WriteLine[ls, ToString[$KernelID] <> " " <> DateString[]];
          Close[ls];
          acquired = True;
        ]
      ];
      Break[];
    ];
    Pause[sleepMs / 1000.],
    maxAttempts
  ];

  (* \:0414\:043e\:043f\:0438\:0441\:044b\:0432\:0430\:0435\:043c \:0441\:0442\:0440\:043e\:043a\:0443 *)
  s = OpenAppend[file];
  WriteLine[s, StringRiffle[ToString /@ row, ","]];
  Close[s];

  (* \:0421\:043d\:0438\:043c\:0430\:0435\:043c \:043b\:043e\:043a *)
  If[acquired && FileExistsQ[lockFile], DeleteFile[lockFile]];

  file
];



ReadCSVLastRow[file_String] := Module[{data, header, rows},
  If[!FileExistsQ[file], Return[$Failed]];
  data = Import[file, "CSV"];
  If[!ListQ[data] || Length[data] < 2, Return[$Failed]];
  header = First[data];
  rows   = Rest[data];
  {header, Last[rows]}
];


(* --- read utils for f4 CSV --- *)

(* \:041a\:043e\:043d\:0432\:0435\:0440\:0442\:0438\:0440\:0443\:0435\:0442 \:0441\:0442\:0440\:043e\:043a\:0443 FortranForm \:0438\:043b\:0438 \:043e\:0431\:044b\:0447\:043d\:043e\:0435 \:0447\:0438\:0441\:043b\:043e \:0432 Mathematica Real *)
parseNum[s_String] := Module[{t},
  (* FortranForm \:043f\:0438\:0448\:0435\:0442 D \:0432\:043c\:0435\:0441\:0442\:043e E \:0432 \:044d\:043a\:0441\:043f\:043e\:043d\:0435\:043d\:0442\:0435 *)
  t = StringReplace[s, {"D+" -> "*^", "D-" -> "*^-", "d+" -> "*^", "d-" -> "*^-"}];
  Quiet @ Check[ToExpression[t], Missing["NotNumeric"]]
];
parseNum[x_] := x;

ReadF4CSV[file_String] := Module[{csv, hdr, rows, assoc, base, parts},
  If[!FileExistsQ[file], Return[{}]];
  csv = Import[file, "CSV"];
  If[!ListQ[csv] || Length[csv] < 2, Return[{}]];
  hdr  = First[csv];
  rows = Rest[csv];

  (* \:0421\:0442\:0440\:043e\:0438\:043c \:0430\:0441\:0441\:043e\:0446\:0438\:0430\:0446\:0438\:0438 \:0441\:0442\:0440\:043e\:043a\:0430 -> \:0437\:043d\:0430\:0447\:0435\:043d\:0438\:0435 *)
  assoc = Association /@ (Thread[hdr -> #] & /@ rows);

  (* \:0427\:0438\:0441\:043b\:043e\:0432\:044b\:0435 \:043f\:043e\:043b\:044f \[LongDash] \:043f\:0430\:0440\:0441\:0438\:043c FortranForm *)
  numFields = {"sqrts0", "mh2", "mhc", "log10_f4mean", "log10_norma_pb",
               "f4mean", "norma_pb"};

  numifyRow[a_Association] := Module[{b = a},
    Do[
      If[KeyExistsQ[b, k],
        b[k] = parseNum[b[k]]
      ],
      {k, numFields}
    ];
    (* mhc: None/\:043f\:0443\:0441\:0442\:0430\:044f \:0441\:0442\:0440\:043e\:043a\:0430 -> None *)
    If[KeyExistsQ[b, "mhc"] &&
       (b["mhc"] === "None" || b["mhc"] === "" || b["mhc"] === Missing[]),
      b["mhc"] = None
    ];
    b
  ];

  assoc = Map[numifyRow, assoc];

  (* \:041c\:0435\:0442\:0430\:0434\:0430\:043d\:043d\:044b\:0435 \:0438\:0437 \:0438\:043c\:0435\:043d\:0438 \:0444\:0430\:0439\:043b\:0430: f4mean_ddZZZ.csv -> pid=dd, proc=ZZZ *)
  base  = StringReplace[FileBaseName[file], "f4mean_" -> ""];
  parts = StringCases[
    base,
    RegularExpression["^([a-z]+)([A-Z].+)$"] :> <|"pid" -> "$1", "proc" -> "$2"|>
  ];
  assoc = Map[
    Join[#,
      <|"source" -> FileNameTake[file], "procname" -> base|>,
      If[parts === {}, <||>, First @ parts]
    ] &,
    assoc
  ];

  assoc
];

LoadF4Data[dir_String : "buffer", pattern_String : "f4mean_*.csv"] :=
  Module[{files},
    files = FileNames[pattern, dir];
    Dataset @ Flatten[ReadF4CSV /@ files]
  ];


End[];
EndPackage[];

