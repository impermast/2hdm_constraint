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

AppendRowToCSV[file_String, row_List] := Module[{s},
  s = OpenAppend[file];
  WriteLine[s, StringRiffle[ToString /@ row, ","]];
  Close[s]; file
];

ReadCSVLastRow[file_String] := Module[{data, header, rows},
  If[!FileExistsQ[file], Return[$Failed]];
  data = Import[file, "CSV"];
  If[!ListQ[data] || Length[data] < 2, Return[$Failed]];
  header = First[data]; rows = Rest[data];
  {header, Last[rows]}
];

(* --- read utils for f4 CSV --- *)

ReadF4CSV[file_String] := Module[{csv, hdr, rows, assoc, base, parts},
  If[!FileExistsQ[file], Return[{}]];
  csv  = Import[file, "CSV"];
  If[!ListQ[csv] || Length[csv] < 2, Return[{}]];
  hdr  = First[csv]; rows = Rest[csv];
  assoc = Association /@ (Thread[hdr -> #] & /@ rows);

  (* \:0447\:0438\:0441\:043b\:0435\:043d\:043d\:044b\:0435 \:043f\:043e\:043b\:044f *)
  toNum[s_] := Quiet@Check[ToExpression[s], s];
  numifyRow[a_] := Module[{b = a},
    Do[If[KeyExistsQ[b,k], b[k] = toNum[b[k]]], {k, {"sqrts0","mh2","mhc","f4mean","norma_pb"}}];
    If[KeyExistsQ[b,"mhc"] && (b["mhc"]==="None" || b["mhc"]==="" || b["mhc"]===Missing[]), b["mhc"]=None];
    b
  ];
  assoc = Map[numifyRow, assoc];

  (* \:043c\:0435\:0442\:0430\:0434\:0430\:043d\:043d\:044b\:0435 \:0438\:0437 \:0438\:043c\:0435\:043d\:0438 \:0444\:0430\:0439\:043b\:0430 *)
  base  = StringReplace[FileBaseName[file], "f4mean_" -> ""];
  parts = StringCases[base, RegularExpression["^([a-z]+)([A-Z].+)$"] :> <|"pid"->$1, "proc"->$2|>];
  assoc = Map[Join[#, <|"source"->FileNameTake[file], "procname"->base|>, If[parts==={}, <||>, First@parts]]&, assoc];

  assoc
];
LoadF4Data[dir_String:"buffer", pattern_String:"f4mean_*.csv"] := Module[{files},
  files = FileNames[pattern, dir];
  Dataset @ Flatten[ReadF4CSV /@ files]
];

End[];
EndPackage[];

