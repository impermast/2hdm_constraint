#!/usr/bin/env wolframscript
(* ::Package:: *)

Procs = {
  <|"pid" -> "dd", "reaction" -> "ZZZ", "particleID" -> 1|>,
  <|"pid" -> "uu", "reaction" -> "ZZZ", "particleID" -> 2|>
};

GridSqrts0 = {320, 500, 1000, 10000, 13000};
GridMh2    = Range[200, 250, 1];
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


Do[
  (* --- \:0437\:0430\:0433\:0440\:0443\:0437\:043a\:0430 \:043e\:0434\:0438\:043d \:0440\:0430\:0437 \:0434\:043b\:044f \:044d\:0442\:043e\:0433\:043e proc --- *)
  particleID = prc["particleID"];
  ProcName   = prc["pid"] <> prc["reaction"];
  Print["Starting calculations for ", ProcName ];
  
  
  f4meanCSV  = "buffer/f4mean_" <> prc["pid"] <> prc["reaction"] <> ".csv";
  wcFile     = "buffer/wc_"     <> prc["pid"] <> prc["reaction"] <> ".mx";
  f4tFile    = "buffer/f4_teor" <> prc["reaction"] <> ".mx";

  WCread = Get[wcFile];
  WCexpr = WCread[[1]] /. {mZ -> MZ, SUNFDelta[__] -> 1} //. Params //. PaveToLooptools;
  WidthCross[ST_?NumericQ] := N[WCexpr /. {s -> ST, St -> ST}];

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

    f4[sVal_?NumericQ] := N @ (F4teor /. {s -> sVal, mh2 -> mh2num, mhc -> mhcnum});

    smin = N[(2*MZ + 0.05)^2 //. Params];
    xmin = N[Sqrt[smin / s0]];

    Lexp[t_] := Log[10] * Integrate[
      xPDF[10^yt, t*s0, particleID] *
      xPDF[t*10^(-yt), t*s0, -particleID],
      {yt, Log10[t], 0}] / Null^2;

    Norma = NIntegrate[
      WidthCross[s0*(10^kt)] * Lexp[10^kt] * Log[10],
      {kt, Log10[xmin], 0}, Method -> "AdaptiveMonteCarlo"];

    CrossSvertka = NIntegrate[
      Abs[f4[s0*(10^kt)]] * WidthCross[s0*(10^kt)] * Lexp[10^kt] * Log[10],
      {kt, Log10[xmin], 0}, Method -> "AdaptiveMonteCarlo"];

    F4mean = Abs[CrossSvertka / Norma];

    row = {DateString[], fmt1[sqrts0v], fmt1[mh2v], fmt1[mhcv],
           fmt1[safeLog10[F4mean]], fmt1[safeLog10[Norma*3.9*10^8]]};
    SaveToCSVmodule`EnsureCSVHeader[f4meanCSV, colNames];
    SaveToCSVmodule`AppendRowToCSV[f4meanCSV, row];

    Print[ProcName <> " sqrts0=" <> ToString[sqrts0v] <>
          " mh2=" <> ToString[mh2v] <>
          " f4mean=" <> ToString[N[F4mean]]];

  , {mhcv, GridMhc}]
  , {mh2v, GridMh2}]
  , {sqrts0v, GridSqrts0}];

, {prc, Procs}];




