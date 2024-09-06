(* ::Package:: *)

(* ::Title:: *)
(*Convolution of quark crosssection with PDF checks *)


(* ::Chapter:: *)
(*Loading model and packages*)


(*$LoadAddOns={"FeynArts", "FeynHelpers"};*)
$LoadAddOns={"FeynArts"};
<<FeynCalc`
$FAVerbose = 0;
(*

Needs["CollierLink`"]
Needs["X`"]*)
AppendTo[$ModelPath, "/home/kds/.Mathematica/Applications/FeynArts/Models/"];
Install["LoopTools"]
Needs["LoopTools`"]


LogRange[a_, b_, n_] := Exp[Range[Log[a], Log[b], (Log[b] - Log[a])/(n - 1)]]//N
NumberOfPoints = 20;


(* ::Section:: *)
(*g a -> q ~q. Gain matrix element*)


diags = InsertFields[CreateTopologies[0, 2 -> 2], {F[3, {1}], V[1]} -> 
            {V[5], F[3, {1}]}, InsertionLevel -> {Classes}, Model -> "SMQCD"]; 
 
Paint[diags, ColumnsXRows -> {2, 1}, Numbering -> Simple, 
    SheetHeader -> None, ImageSize -> {256, 128}];


ampQA[0] = FCFAConvert[CreateFeynAmp[diags], IncomingMomenta -> {p1, p2}, 
    OutgoingMomenta -> {k1, k2}, UndoChiralSplittings -> True, ChangeDimension -> 4, 
    TransversePolarizationVectors -> {k1}, List -> False, SMP -> True, 
    Contract -> True, DropSumOver -> True, Prefactor -> 3/2 SMP["e_Q"]]
    
FCClearScalarProducts[];
SetMandelstam[s, t, u, p1, p2, -k1, -k2, SMP["m_u"], qQ, 0, SMP["m_u"]];



ampQA[1] = 1/(SUNN) (ampQA[0] (ComplexConjugate[ampQA[0]])) // 
            FeynAmpDenominatorExplicit // SUNSimplify[#, Explicit -> True, 
            SUNNToCACF -> False] & // FermionSpinSum[#, ExtraFactor -> 1/2] & // 
        DiracSimplify // DoPolarizationSums[#, p2, 0, 
        VirtualBoson -> True] & // DoPolarizationSums[#, k1, p2] & // 
    TrickMandelstam[#, {s, t, u, 2 SMP["m_u"]^2 + qQ^2}] & // Simplify


ampQA[2] = ampQA[1] // ReplaceAll[#, {SMP["m_u"] -> 0}] & // 
    TrickMandelstam[#, {s, t, u, qQ^2}] &
ampQA[3] = 
    Simplify[ampQA[2] /. SUNN -> 3 /. u -> qQ^2 - s - t /. qQ -> I Q]


NampQA[Q_,s_,t_] = ampQA[3]/.{SMP["e"]->0.313,SMP["g_s"]->1.22,SMP["e_Q"]->2/3}/.{Q->Q,s->s,t->t};
NampQA[q,S,T]


(* ::Section:: *)
(*Importing PDF*)


pdfdata = Import["/home/kds/sci/zzz/2hdm_constraint/pdfcode/output125.csv"];
idList={-5,-4,-3,-2,-1,1,2,3, 4,5};
pdfdata = Delete[pdfdata,1];
Tpdfdata= Transpose[pdfdata];

xValues = Tpdfdata[[1]];


PDFALL[x_, id_Integer] := Module[{data, interpolated, index},
  index = Position[idList, id];
  If[Length[index] == 0, (* \:0415\:0441\:043b\:0438 \:0438\:043d\:0434\:0435\:043a\:0441 \:043d\:0435 \:043d\:0430\:0439\:0434\:0435\:043d *)
    Return[Table[0, Length[x]]], (* \:0412\:043e\:0437\:0432\:0440\:0430\:0449\:0430\:0435\:043c \:0441\:043f\:0438\:0441\:043e\:043a \:043d\:0443\:043b\:0435\:0439 \:0442\:0430\:043a\:043e\:0439 \:0436\:0435 \:0434\:043b\:0438\:043d\:044b, \:043a\:0430\:043a \:0438 x *)
    data = Tpdfdata[[index[[1, 1]]+1]]; (* \:041f\:043e\:043b\:0443\:0447\:0430\:0435\:043c \:0441\:043e\:043e\:0442\:0432\:0435\:0442\:0441\:0442\:0432\:0443\:044e\:0449\:0438\:0435 \:0434\:0430\:043d\:043d\:044b\:0435 \:0434\:043b\:044f id *)
    interpolated = Interpolation[Transpose[{xValues, data}]]; (* \:0421\:043e\:0437\:0434\:0430\:0435\:043c \:0438\:043d\:0442\:0435\:0440\:043f\:043e\:043b\:0438\:0440\:043e\:0432\:0430\:043d\:043d\:0443\:044e \:0444\:0443\:043d\:043a\:0446\:0438\:044e *)
    Return[interpolated[x]]; (* \:0412\:043e\:0437\:0432\:0440\:0430\:0449\:0430\:0435\:043c \:0437\:043d\:0430\:0447\:0435\:043d\:0438\:0435 \:0438\:043d\:0442\:0435\:0440\:043f\:043e\:043b\:0438\:0440\:043e\:0432\:0430\:043d\:043d\:043e\:0439 \:0444\:0443\:043d\:043a\:0446\:0438\:0438 \:0434\:043b\:044f \:0437\:0430\:0434\:0430\:043d\:043d\:043e\:0433\:043e x *)
  ]
]


index = 5
place = Position[idList, index][[1, 1]] + 1;

p1 = ListLogLogPlot[
  Transpose[{xValues, Tpdfdata[[place]]}],
  PlotStyle -> {Red, PointSize[Small]},
  GridLines -> Automatic,
  Frame -> True,
  FrameLabel -> {"x", "PDF"},
  PlotLegends -> {"Data Points"}];

p2 = LogLogPlot[
  PDFALL[x, index],
  {x, 0.000001, 1},
  PlotStyle -> Blue,
  PlotLegends -> {"Interpolated Function"}];

Show[p1, p2,PlotLabel->"Check interpolation"]


(* ::Section:: *)
(*kinematic prefactor*)


(* ::Text:: *)
(*dSIgma/dOmega = 1/(64 pi^2 s) M^2  (massless products)*)
(*t = -s/2(1-cos(th))*)


Pref = 1/(64 Pi^2 s);
NampQA[Q,s,t]/.{t->-s/2(1-cos[Th])}
Int = 2 Pi*Integrate[Sin[Th]*(a+b(1-Cos[Th])+c (1-Cos[Th])^2)/(1-Cos[Th]),{Th,0+0.05,Pi-0.05}]
Sigma = Pref*(0.6912808751407405/s^2)*Int/.{a->(2Q^4+Q^2s+s^2), b->-s*Q^2, c->s^2/4}
CrossSectionQA[s] = Sigma/.{Q->0}//Simplify
CrossSectionQA[100]



