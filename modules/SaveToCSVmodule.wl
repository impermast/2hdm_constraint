(* ::Package:: *)

BeginPackage["SaveToCSVmodule`"];
SaveTableToCSV::usage = "SaveTableToCSV[names, table] saves the table to a CSV file with the given column names.";

SaveTableToCSV[names_List, table_List,file_] := Module[{stream},
  stream = OpenWrite[file];
  WriteLine[stream, StringJoin[Riffle[names, ","]]];
  Do[WriteLine[stream, StringJoin[Riffle[ToString /@ table[[i]], ","]]], {i, Length[table]}];
  Close[stream];
  file
]

EndPackage[];
