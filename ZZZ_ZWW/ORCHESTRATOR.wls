#!/usr/bin/env wolframscript
(* ::Package:: *)

Procs = {
  (*<|"pid" -> "dd", "reaction" -> "ZZZ", "particleID" -> 1|>*)
  <|"pid" -> "uu", "reaction" -> "ZZZ", "particleID" -> 2|>
};
(* PDF d\[RightArrow]1,u\[RightArrow]2, d\:02c9\[RightArrow]\[Minus]1,u\:02c9\[RightArrow]\[Minus]2 *)
GridSqrts0 = {13000};
GridMh2    = Range[200, 600, 5];
GridMhc    = {400};

Alpha2Fixed = 0.955317;
Alpha3Fixed = 0.785398;



(* ::Subsection:: *)
(*init*)


Off[General::shdw]; Off[C0::"msg"];
$FeynCalcStartupMessages = False;
$LoadFeynCalcMessages    = False;
$LoadAddOns              = {"FeynArts"};
$PrePrint                = InputForm;

If[$Notebooks,
  $ScriptDir = NotebookDirectory[],
  $ScriptDir = DirectoryName @ ExpandFileName[$InputFileName]
];
SetDirectory[$ScriptDir];
<< "../modules/setup.m";
SetDirectory[$ScriptDir];


fmt1[x_?NumericQ] := ToString[N[x, 16]];
safeLog10[x_?NumericQ] := If[x > 0, Log10[x], -300.];

mZ = MZ/.Params;
colNames = {"timestamp","sqrts0","mh2","mhc","log10_f4mean","log10_norma_pb"};



(* ::Subsection:: *)
(*scan*)


ClearAll[buildWCExpr, ensureWC];

buildWCExpr[meFile_, msg_: True] := Module[
  {MaElread, MaElreadExp, MaEl, Pref, lambda, tmin, tmax, wcExpr},

  If[msg === True, PrintTG["File with WidthCross not found. Loading matrix element."]];
  Clear[mh2, mh3, mhc, s, t, St];
  {MaElread, MaElreadExp} = Get[meFile];
  Print["FreeQ[MaElread, mh2] = ", FreeQ[MaElread, mh2]];
  Print["FreeQ[MaElread, mh3] = ", FreeQ[MaElread, mh3]];
  MaElread = MaElread //. AngleChanger //. Params //. SmpChanger //. PaveToLooptools /. {SMP["m_q"] -> 0};
  
  MaEl[sX_, tX_] := MaElread /. {
    St -> sX,
    s -> sX,
    t -> tX,
    SUNFDelta[__] -> 1,
    FeynCalc`CA -> 3
  };

  If[msg === True, PrintTG["Working on WidthCross"]];

  Pref[x_] := 1/(16 Pi x^2);

  lambda[s_] := s^2 - 4 s MZ^2 //. Params;
  tmin[s_] := (2 MZ^2 - s - Sqrt[lambda[s]])/2 //. Params;
  tmax[s_] := (2 MZ^2 - s + Sqrt[lambda[s]])/2 //. Params;

  wcExpr = Abs[
    Pref[s] Integrate[
      MaEl[s, t],
      {t, tmin[s], tmax[s]}
    ]
  ];
  

  wcExpr
];

ensureWC[meFile_, wcFile_, msg_: True] := Module[{wcExpr},
  If[FileExistsQ[wcFile],
    wcExpr = Get[wcFile],
    wcExpr = buildWCExpr[meFile, msg];
    Put[wcExpr, wcFile];
  ];
  Print["FreeQ[wcExpr, mh2] = ", FreeQ[wcExpr, mh2]];
  Print["FreeQ[wcExpr, mh3] = ", FreeQ[wcExpr, mh3]];
  wcExpr
];


PointKey[s_, m2_, mc_] := {N[s], N[m2], N[mc]};

(* Load computed keys from one CSV file *)
LoadDoneKeysFromFile[csvFile_String] :=
  If[!FileExistsQ[csvFile], <||>,
    With[{rows = Quiet @ Import[csvFile, "CSV"]},
      If[!ListQ[rows] || Length[rows] < 2, <||>,
        Association @ Table[
          Quiet @ Check[
            PointKey[
              ToExpression[r[[2]]],
              ToExpression[r[[3]]],
              ToExpression[r[[4]]]
            ] -> True,
            Nothing
          ],
          {r, Rest[rows]}
        ]
      ]
    ]
  ];


Do[
  (* --- \:0437\:0430\:0433\:0440\:0443\:0437\:043a\:0430 \:043e\:0434\:0438\:043d \:0440\:0430\:0437 \:0434\:043b\:044f \:044d\:0442\:043e\:0433\:043e proc --- *)
  particleID = prc["particleID"];
  ProcName   = prc["pid"] <> prc["reaction"];
  Print["Starting calculations for ", ProcName ];
  
  
  f4meanCSV  = "buffer/f4mean_" <> prc["pid"] <> prc["reaction"] <> ".csv";
  meFile     = "buffer/me_" <> prc["pid"] <> prc["reaction"] <> ".mx";
  wcFile     = "buffer/wc_"     <> prc["pid"] <> prc["reaction"] <> ".mx";
  f4tFile    = "buffer/f4_teor" <> prc["reaction"] <> ".mx";
	  (* --- \:0437\:0430\:0433\:0440\:0443\:0437\:043a\:0430 \:0443\:0436\:0435 \:043f\:043e\:0441\:0447\:0438\:0442\:0430\:043d\:043d\:044b\:0445 \:0442\:043e\:0447\:0435\:043a \:0438\:0437 CSV --- *)
  doneKeys = LoadDoneKeysFromFile[f4meanCSV];
  Print["Loaded ", Length[doneKeys], " already computed points from ", f4meanCSV];

	
  wcExpr = ensureWC[meFile, wcFile, msg];
  WidthCross[ST_?NumericQ] := N[wcExpr /. {s -> ST, St -> ST}];

  F4teor = Get[f4tFile];
  F4teor = F4teor //. SmpChanger //. AngleChanger //. Params /. {
    SUNFDelta[__] -> 1} //. PaveToLooptools;

  a1 = 0.;
  a2 = Alpha2Fixed;
  a3 = Alpha3Fixed;

  Print["Working dir: ", Directory[]];
  Print["wcFile: ", "buffer/wc_" <> prc["pid"] <> prc["reaction"] <> ".mx"];
  Print["Exists: ", FileExistsQ["buffer/wc_" <> prc["pid"] <> prc["reaction"] <> ".mx"]];
	
  (* --- \:0441\:043a\:0430\:043d \:043f\:043e \:043f\:0430\:0440\:0430\:043c\:0435\:0442\:0440\:0430\:043c --- *)
  Do[
  Do[
  Do[

    sqrts0 = sqrts0v;
    mh2num = mh2v;
    mhcnum = mhcv;
    s0     = N[sqrts0^2];
    mh2    = N[mh2num];
    mh3    = N[Sqrt[mh2^2 + vev^2] /. Params];
    mhc    = N[mhcnum];

	(* --- \:043f\:0440\:043e\:043f\:0443\:0441\:043a \:0443\:0436\:0435 \:0432\:044b\:0447\:0438\:0441\:043b\:0435\:043d\:043d\:044b\:0445 \:0442\:043e\:0447\:0435\:043a --- *)
    If[KeyExistsQ[doneKeys, PointKey[sqrts0v, mh2v, mhcv]],
      Print[ProcName <> " SKIP sqrts0=" <> ToString[sqrts0v] <>
            " mh2=" <> ToString[mh2v] <> " mhc=" <> ToString[mhcv] <> " (already done)"];
      Continue[]
    ];

    f4[sVal_?NumericQ] := N @ (F4teor /. {s -> sVal, mh2 -> mh2num, mhc -> mhcnum});

    smin = N[(2*MZ + 0.05)^2 //. Params];
    taumin = N[smin / s0];

    Lexp[t_] := Log[10] * Integrate[
      xPDF[10^yt, t*s0, particleID] *
      xPDF[t*10^(-yt), t*s0, -particleID],
      {yt, Log10[t], 0}] / Null^2;

    Norma = NIntegrate[
      WidthCross[s0*(10^kt)] * Lexp[10^kt] * Log[10],
      {kt, Log10[taumin], 0}, Method -> "AdaptiveMonteCarlo"];

    CrossSvertka = NIntegrate[
      Abs[f4[s0*(10^kt)]] * WidthCross[s0*(10^kt)] * Lexp[10^kt] * Log[10],
      {kt, Log10[taumin], 0}, Method -> "AdaptiveMonteCarlo"];
(*, Method -> "AdaptiveMonteCarlo"*)
    F4mean = Abs[CrossSvertka / Norma];

    row = {DateString[], fmt1[sqrts0v], fmt1[mh2v], fmt1[mhcv],
           fmt1[safeLog10[F4mean]], fmt1[safeLog10[Norma*3.9*10^8]]};
    SaveToCSVmodule`EnsureCSVHeader[f4meanCSV, colNames];
    SaveToCSVmodule`AppendRowToCSV[f4meanCSV, row];
    doneKeys[PointKey[sqrts0v, mh2v, mhcv]] = True;

    Print[ProcName <> " sqrts0=" <> ToString[sqrts0v] <>
          " mh2=" <> ToString[mh2] <> " mh3=" <> ToString[mh3] <>
          " f4mean=" <> ToString[N[F4mean]]];

  , {mhcv, GridMhc}]
  , {mh2v, GridMh2}]
  , {sqrts0v, GridSqrts0}];

, {prc, Procs}];



