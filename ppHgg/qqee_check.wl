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


(*FileNames[]
SetDirectory[]*)


LogRange[a_, b_, n_] := Exp[Range[Log[a], Log[b], (Log[b] - Log[a])/(n - 1)]]//N
NumberOfPoints = 20;


(* ::Section:: *)
(*qq -> ee. Gain matrix element*)


diags = InsertFields[CreateTopologies[0, 2 -> 2], {-F[3, {1}], F[3, {1}]} -> 
        {F[2, {1}], -F[2, {1}]}, InsertionLevel -> {Classes}, Model -> "SMQCD", 
        ExcludeParticles -> {S[_],V[1]}]; 
        
          
Paint[diags, ColumnsXRows -> {2, 1}, Numbering -> Simple, 
    SheetHeader -> None, ImageSize -> {256, 128}];


ampQQEE[0] = FCFAConvert[CreateFeynAmp[diags], IncomingMomenta -> {p1, p2}, 
    OutgoingMomenta -> {k1, k2}, UndoChiralSplittings -> True, ChangeDimension -> 4, 
    List -> False, SMP -> True, 
    Contract -> True, DropSumOver -> True, Prefactor -> 3/2*SMP["e_Q"]]
    
FCClearScalarProducts[];
SetMandelstam[s, t, u, p1, p2, -k1, -k2, SMP["m_u"], SMP["m_d"], 0, 0];



ampQQEE[1] = 1/(SUNN^2) (ampQQEE[0] (ComplexConjugate[ampQQEE[0]])) // 
            FeynAmpDenominatorExplicit // SUNSimplify[#, Explicit -> True, 
            SUNNToCACF -> False] & // FermionSpinSum[#, ExtraFactor -> 1/2^2] & // 
        DiracSimplify // TrickMandelstam[#, {s, t, u, SMP["m_u"]^2 + SMP["m_d"]^2}] & // Simplify


ampQQEE[2] = ampQQEE[1] /. SUNN -> 3 // Simplify
Massless_check = ampQQEE[1] /. SUNN -> 3 /. SMP["m_u"]->0/. SMP["m_d"]->0/. SMP["theta"]->tw/.SMP["m_W"]->mW//Simplify


(* Me=0.0005; Mu = 0.0023; *)
 Me=0; Mu=0;
 MZ=91.1;
wZ=2.5;
MW = 81; cosW=0.894; sinW=0.463; e=0.313;
NampQQEE[s_,t_] = ampQQEE[2]/.{SMP["e"]->0.313,SMP["m_e"]->Me,SMP["m_u"]->Mu,SMP["m_d"]->0,SMP["e_Q"]->2/3,SMP["cos_W"]->cosW,SMP["sin_W"]->cosW,SMP["m_Z"]->MZ}/.{s->s,u->-s-t,t->t};
NampQQEE[S,T]//Simplify



(* ::Section:: *)
(*Importing PDF*)


pdfdata = Import["/home/kds/sci/zzz/2hdm_constraint/pdfcode/output125.csv"];
idList={-5,-4,-3,-2,-1,1,2,3,4,5};
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
"PDF/x:"
Integrate[PDFALL[x,index]/x,{x,0.00001,1}]//N 
"PDF:"
Integrate[PDFALL[x,index],{x,0.00001,1}] 
"Conclusion:"
"PDF is x*f(x)"


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
(*dSIgma/dOmega = 1/(64 pi^2 s) M^2 *)
(*p1cm^2=s/4-m1^2*)
(*t = -s/2(1-cos(th))= (E1-E3)^2 - (p1-p3)^2=E1^2+E3^2-2E1E3cos th*)
(*dSigma/dt = 1/(16 pi s^2) M^2*)


Pref[x_] := 1/(64 Pi*x((x/4)-Mu^2));
Pref[s]
NampQQEE[s,t]


(* ::Text:: *)
(*lims on t:*)
(*t0(t1)=0-(sqrt(s/4-m1^2) -+ sqrt(s/4-m3^2))^2*)


s0=13000^2;
ptmin = 10
smin = 800;
xmin = Sqrt[smin/s0];
t0[s_]:= - s/2 * (1-Sqrt[1-4*ptmin^2/s])
t1[s_]:= - s/2 * (1+Sqrt[1-4*ptmin^2/s])
t0[s0] 
t1[s0]



SigmaT[x_] := Integrate[NampQQEE[x,u]*Pref[x],{u,t1[x],t0[x]}]/.{ s-8299.21`-> Sqrt[(s-8299.21)^2+MZ^2*wZ^2]};
SigmaT[s]//Simplify
3.96*10^8*Integrate[SigmaT[x1*x2*s0] *PDFALL[x1,1]*PDFALL[x2,-1]/(x1*x2),{x1,xmin,1},{x2,xmin,1}]


(* ::Section:: *)
(*Correction with MG*)


(* ::Text:: *)
(*MADGRAPH qq>ee, Matrix element = *)
(*Cross = *)


IntGev = Integrate[
			NampQQEE[s0*x1*x2,u]*Pref[s0*x1*x2] PDFALL[x1,1]*PDFALL[x2,-1]/(x1*x2),
				{u,t1[s0*x1*x2],t0[s0*x1*x2]},{x1,xmin,1},{x2,xmin,1}]//N
IntGev *3.96*10^8 "pb"


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


ptmin=10
s1 = 13000^2;
theta = ArcSin[2*ptmin/Sqrt[s1]]//N
y=-Log[Tan[theta/2]]
(*\:041f\:043e\:0447\:0435\:043c\:0443-\:0442\:043e \:043d\:0435 \:0440\:0430\:0431\:043e\:0442\:0430\:0435\:0442 \:0441\:043c\:043e\:0442\:0440\:0435\:0442\:044c QQEE*)


(* ::Section:: *)
(*\:0421\:0432\:0435\:0440\:0442\:043a\:0430*)


WidthCross1[s_] :=FullSimplify[NampQQEE[s, t]/.{(s - MZ^2)->((s - MZ^2)^2 + MZ^2 * wZ^2)^(1/2)}]
WidthCross2[s_] :=FullSimplify[NampQQEE[s, t]*(s - MZ^2)^2  / ((s - MZ^2)^2 + MZ^2 * wZ^2)]
WidthCross1[s]
WidthCross2[s]
WidthCross1[MZ^2]
WidthCross2[MZ^2]


s0 = 13000^2;
MZ=91.1;
wZ=2.5;
E0=Sqrt[s0]/2;

WidthCross[s_] := Integrate[Pref[s] (NampQQEE[s, t]*(s - MZ^2)^2  / ((s - MZ^2)^2 + MZ^2 * wZ^2) ) // Simplify, 
                                 {t, -s/2 * (1 + Sqrt[1 - (ptmin/E0)^2]), -s/2 * (1 - Sqrt[1 - (ptmin/E0)^2])}];
"cross on MZ no PDF"

WidthCross[MZ^2]:=WidthCross[MZ^2-0.00001]
WidthCross[MZ^2]



P2 = LogLogPlot[
  WidthCross[x^2],
  {x, 0, 2*E0},
  PlotStyle -> Blue,
  PlotLegends -> {"Sigma Partonic"}]


smin = 800;
L[t_]:=Integrate[(1/(t*x)) *PDFALL[x,1]*PDFALL[t/x,-1],{x,t,0.1}];


Plot3 = LogLogPlot[
  L[t],
  {t, 0, 1},
  PlotStyle -> Blue,
  PlotLegends -> {"Luminosity from PDF"}]

"\:0421\:0432\:0435\:0440\:0442\:043a\:0430 \:0441 L[t]"
xmin = smin/s0

(*L[xmin]//N*)


NIntegrate[WidthCross[s0*t]*PDFALL[x,1]*PDFALL[t/x,-1]/(t*x)
		,{x,t,0.001},{t,xmin,0.001}]



