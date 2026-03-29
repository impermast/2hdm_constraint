(* ::Package:: *)

(* ::Title:: *)
(*Testing pdf call *)


(* ::Chapter:: *)
(*Loading model and packages*)


LogRange[a_, b_, n_] := Exp[Range[Log[a], Log[b], (Log[b] - Log[a])/(n - 1)]]//N



(* ::Section:: *)
(*PDF Call*)


scriptPath = FileNameJoin[{NotebookDirectory[], "xPDF.py"}];
res = RunProcess[{"sh", "-c", 
    "LD_LIBRARY_PATH=/usr/local/lib python3 " <> scriptPath <> " " <> ToString[0.01] <> " " <> ToString[91] <> " " <> ToString[1]
  }];
output = StringTrim[res["StandardOutput"]]


(* ::Print:: *)
(*Null*)


getPDF[x_, Q_, id_] := Module[{result, scriptPath, output},
  scriptPath = FileNameJoin[{NotebookDirectory[], "xPDF.py"}];
  result = RunProcess[{"sh", "-c", 
    "LD_LIBRARY_PATH=/usr/local/lib python3 " <> scriptPath <> " " <> ToString[x] <> " " <> ToString[Q] <> " " <> ToString[id]
  }];
  output = StringTrim[result["StandardOutput"]];
  If[StringMatchQ[output, NumberString], ToExpression[output], Null]
]



getPDF[0.01, 91, 1]




(* ::Section:: *)
(*\:041f\:0440\:043e\:0432\:0435\:0440\:043a\:0430 \:0441\:043a\:043e\:0440\:043e\:0441\:0442\:0438 \:0433\:0435\:043d\:0435\:0440\:0430\:0446\:0438\:0438*)


numPoints = 100;
qValues = RandomReal[{100, 13000}, numPoints];
xValues = RandomReal[{0, 1}, numPoints];


(* \:0418\:0437\:043c\:0435\:0440\:0435\:043d\:0438\:0435 \:0432\:0440\:0435\:043c\:0435\:043d\:0438 \:0432\:044b\:043f\:043e\:043b\:043d\:0435\:043d\:0438\:044f *)
timing = AbsoluteTiming[
  pdfValues = Table[
  Module[{result = getPDF[xValues[[i]], qValues[[i]], 1]},
    If[result === Null, Print["Null result for x = ", xValues[[i]], ", q = ", qValues[[i]]]];
    result
  ],
  {i, 1, numPoints}
];
];

(* \:0420\:0435\:0437\:0443\:043b\:044c\:0442\:0430\:0442\:044b *)
Print["\:0412\:0440\:0435\:043c\:044f \:0432\:044b\:043f\:043e\:043b\:043d\:0435\:043d\:0438\:044f: ", timing[[1]], " \:0441\:0435\:043a\:0443\:043d\:0434"];
pdfValues



