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
(*q a -> q a. Gain matrix element*)


diags = InsertFields[CreateTopologies[0, 2 -> 2], {F[3, {1}], V[1]} -> 
            {V[1], F[3, {1}]}, InsertionLevel -> {Classes}, Model -> "SMQCD"]; 
 
Paint[diags, ColumnsXRows -> {2, 1}, Numbering -> Simple, 
    SheetHeader -> None, ImageSize -> {256, 128}];


ampQA[0] = FCFAConvert[CreateFeynAmp[diags], IncomingMomenta -> {p1, p2}, 
    OutgoingMomenta -> {k1, k2}, UndoChiralSplittings -> True, ChangeDimension -> 4, 
    TransversePolarizationVectors -> {k1}, List -> False, SMP -> True, 
    Contract -> True, DropSumOver -> True, Prefactor -> 3/2*SMP["e_Q"]]
    
FCClearScalarProducts[];
SetMandelstam[s, t, u, p1, p2, -k1, -k2, SMP["m_u"], 0, 0, SMP["m_u"]];



ampQA[1] = (ampQA[0] (ComplexConjugate[ampQA[0]])) // 
            FeynAmpDenominatorExplicit // SUNSimplify[#, Explicit -> True, 
            SUNNToCACF -> False] & // FermionSpinSum[#, ExtraFactor -> 1/2^2] & // 
        DiracSimplify // DoPolarizationSums[#, p2, 0, 
        VirtualBoson -> True] & // DoPolarizationSums[#, k1, p2] & // 
    TrickMandelstam[#, {s, t, u, 2 SMP["m_u"]^2}] & // Simplify


ampQA[2] = ampQA[1] // ReplaceAll[#, {SMP["m_u"] -> 0}] & // 
    TrickMandelstam[#, {s, t, u, 0}] &
ampQA[3] = 
    Simplify[ampQA[2] /. SUNN -> 3 /. u -> - s - t]


NampQA[s_,t_] = ampQA[3]/.{SMP["e"]->0.313,SMP["g_s"]->1.22,SMP["e_Q"]->2/3}/.{s->s,t->t};
NampQA[S,T]





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


index = 1
place = Position[idList, index][[1, 1]] + 1;

P1 = ListLogLogPlot[
  Transpose[{xValues, Tpdfdata[[place]]}],
  PlotStyle -> {Red, PointSize[Small]},
  GridLines -> Automatic,
  Frame -> True,
  FrameLabel -> {"x", "PDF"},
  PlotLegends -> {"Data Points"}];

P2 = LogLogPlot[
  PDFALL[x, index],
  {x, 0.000001, 1},
  PlotStyle -> Blue,
  PlotLegends -> {"Interpolated Function"}];

Show[P1, P2,PlotLabel->"Check interpolation"]


(* ::Section:: *)
(*Crosssection prefactor*)


(* ::Text:: *)
(*dSIgma/dOmega = 1/(64 pi^2 s) M^2  (massless products)*)
(*t = -s/2(1-cos(th))*)
(*dSigma/dt = 1/(16 pi s^2) M^2*)


Pref[x_] := 1/(32 Pi*x);
s1= 91^2;
Th1 = 0.07846; Th2 = Pi-0.07846;
t1 = -s1(1-Cos[Th1])/2;
t2 = -s1(1-Cos[Th2])/2;
Matrix[s,Th] = NampQA[s,t]/.{t->-s(1-Cos[Th])/2}
MatrixPrefactor = Simplify[Matrix[s,Th]/((1+ (1/4)(1-2*Cos[Th]+Cos[Th]*Cos[Th]))/(1-Cos[Th]))]
Print["Integrate on theta"]
CrossPB = 4*10^8*Integrate[Sin[Th]*Matrix[s,Th]*Pref[s],{Th,Th1,Th2}]
Print["Integte by theta without params"]
Int = Integrate[Sin[Th]*(1+ (1/4)(1-2*Cos[Th]+Cos[Th]*Cos[Th]))/(1-Cos[Th]),{Th,Th1,Th2}];
Sigma[x_] := Pref[x]*MatrixPrefactor*Int;
Sigma[s]
CrossPB/.{s->s1}
Print["Corection of calcs:"]
Sigma[s1]*4*10^8
SigmaT = Integrate[NampQA[s,t]/(16 Pi (s)^2),{t,t2,t1}];
4*10^8*SigmaT/.{s->s1}


(* ::Section:: *)
(*Correction with MG*)


(* ::Text:: *)
(*MADGRAPH qa>qa, Matrix element = 1.289355 e - 02 GeV^0*)
(*Cross = 4.1 pb*)


MADGRAPHmatrix = 1.289355*10^-2;
mZ = 91;
s0=13000^2;
t0=-mZ^2;
x0=-t0/(s0+t0)//N;
Print["cuts in MG"]
ptj = 10
pta = 10
y1=-5
y2=5

NampQA[s,t]


(* ::Text:: *)
(*\:0422\:0443\:0442 \:0438\:043d\:0442\:0435\:0433\:0440\:0438\:0440\:0443\:0435\:043c \:0441 \:043d\:0435\:0438\:0437\:043c\:0435\:043d\:043d\:044b\:043c\:0438 \:043f\:0440\:0435\:0434\:0435\:043b\:0430\:043c\:0438.*)


IntGev = NIntegrate[Sigma[s0 x] PDFALL[x,1],{x,2 pta / Sqrt[s0],1}]
IntGev *4*10^8


(* ::Text:: *)
(*p_t^2 = ut/s = -t(s+t)/s => pt_max = s/4 => no need of lim*)
(*lims on psevdorapidity*)
(*y = 1/2 ln (s+t/-t); t=0 => y = inf,  t=-s  y=-inf => cut up and down*)
(**)
(*abs(t)=xs/(e^2y -1)*)
(**)
(*(pp->qq )*)
(*S sin^2th=4pt^2; t=-s/2(1-cos th)*)
(*cos th = 1+ 2t/s => sin^2 = 1-cos^2 = t(t/s-2) => pt^2 = s/4(4t^2/s^2-8t/s)   pt^2 = -t(2-t/s) {-s<t<0}*)


th1 = ArcCos[ 1-2*Sqrt[4 pta^2/s0]] // N
th2 = ArcCos[ -1+2*Sqrt[4 pta^2/s0]] // N
Tlim2[u_]:= u*s0/(Exp[2 y1]+1)
Tlim1[u_]:= u*s0/(Exp[2 y2]+1)
Sqrt[Tlim1[1]]//N
Sqrt[Tlim2[1]]//N




SigmaT = Integrate[PDFALL[x,1]*NampQA[x*s0,t]/(16 Pi (x*s0)^2),
	{t,-s0-Sqrt[4 pta^2 *(x*s0)],-Sqrt[4 x*s0 pta^2]},
	{x,2*pta/Sqrt[s0],1}] //N
SigmaTwithY = Integrate[PDFALL[x,1]*NampQA[x*s0,t]/(16 Pi (x*s0)^2),
	{t,-Tlim2[x],-Tlim1[x]},
	{x,0,1}] //N
	
(*SigmaTh = NIntegrate[PDFALL[x,1]*Matrix[s0*x,ArcCos[y]]*Pref[s0*x],
{y,N[1-2*Sqrt[4 pta^2/(x*s0)]],N[-1+2*Sqrt[4 pta^2/(x*s0)]]},
{x,2*pta/Sqrt[s0],1}];*)


IntPbT = SigmaT * 4* 10^8
Print["Cross t, pt cuts: ",IntPbT," pb.        MG cross = 40.4 pb   Y cuts MGcross = 493pb;   Cross = ", SigmaTwithY * 4* 10^8]
Abs[IntPbT-40.4]





DsigmaDx[x_]:=Integrate[PDFALL[x,1]*NampQA[x*s0,t]/(16 Pi (x*s0)^2),
	{t,-Tlim2[x],-Tlim1[x]}]//N
	
LogLogPlot[DsigmaDx[x],{x,0.001,0.1},
	PlotPoints -> 30]
