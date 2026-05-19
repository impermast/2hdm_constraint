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
NumberOfPoints = 50;


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
 MZ=91.188;
wZ=2.5;

ptmin = 20;
s0 = 13000^2;
smin = 4*ptmin^2;  
xmin = Sqrt[smin/s0];

MW = 81; cosW=0.894; sinW=0.463; e=0.313;
NampQQEE[s_,t_] = ampQQEE[2]/.{SMP["e"]->0.313,SMP["m_e"]->Me,SMP["m_u"]->Mu,SMP["m_d"]->0,SMP["e_Q"]->2/3,SMP["cos_W"]->cosW,SMP["sin_W"]->cosW,SMP["m_Z"]->MZ}/.{s->s,u->-s-t,t->t};
NampQQEE[S,T]//Simplify



(* ::Section:: *)
(*Importing PDF*)


(* ::Subsubsection:: *)
(*\:0432\:044b\:0433\:0440\:0443\:0437\:043a\:0430 \:0438\:0437 csv*)


(*pdfdata = Import["/home/kds/sci/zzz/2hdm_constraint/pdfcode/output125.csv"];
idList={-5,-4,-3,-2,-1,1,2,3,4,5};
pdfdata = Delete[pdfdata,1];
Tpdfdata= Transpose[pdfdata];

xValues = Tpdfdata[[1]];*)


(*PDFALL[x_, id_Integer] := Module[{data, interpolated, index},
  index = Position[idList, id];
  If[Length[index] == 0, (* \:0415\:0441\:043b\:0438 \:0438\:043d\:0434\:0435\:043a\:0441 \:043d\:0435 \:043d\:0430\:0439\:0434\:0435\:043d *)
    Return[Table[0, Length[x]]], (* \:0412\:043e\:0437\:0432\:0440\:0430\:0449\:0430\:0435\:043c \:0441\:043f\:0438\:0441\:043e\:043a \:043d\:0443\:043b\:0435\:0439 \:0442\:0430\:043a\:043e\:0439 \:0436\:0435 \:0434\:043b\:0438\:043d\:044b, \:043a\:0430\:043a \:0438 x *)
    data = Tpdfdata[[index[[1, 1]]+1]]; (* \:041f\:043e\:043b\:0443\:0447\:0430\:0435\:043c \:0441\:043e\:043e\:0442\:0432\:0435\:0442\:0441\:0442\:0432\:0443\:044e\:0449\:0438\:0435 \:0434\:0430\:043d\:043d\:044b\:0435 \:0434\:043b\:044f id *)
    interpolated = Interpolation[Transpose[{xValues, data}]]; (* \:0421\:043e\:0437\:0434\:0430\:0435\:043c \:0438\:043d\:0442\:0435\:0440\:043f\:043e\:043b\:0438\:0440\:043e\:0432\:0430\:043d\:043d\:0443\:044e \:0444\:0443\:043d\:043a\:0446\:0438\:044e *)
    Return[interpolated[x]]; (* \:0412\:043e\:0437\:0432\:0440\:0430\:0449\:0430\:0435\:043c \:0437\:043d\:0430\:0447\:0435\:043d\:0438\:0435 \:0438\:043d\:0442\:0435\:0440\:043f\:043e\:043b\:0438\:0440\:043e\:0432\:0430\:043d\:043d\:043e\:0439 \:0444\:0443\:043d\:043a\:0446\:0438\:0438 \:0434\:043b\:044f \:0437\:0430\:0434\:0430\:043d\:043d\:043e\:0433\:043e x *)
  ]
]*)


(* ::Subsubsection:: *)
(*\:0432\:044b\:0433\:0440\:0443\:0437\:043a\:0430 \:0438\:0437 csv*)


xPDF[x_, Q_, id_] := Module[{result, scriptPath, output},
  scriptPath = FileNameJoin[{NotebookDirectory[],"../pdfcode/", "xPDF.py"}];
  result = RunProcess[{"sh", "-c", 
    "LD_LIBRARY_PATH=/usr/local/lib python3 " <> scriptPath <> " " <> ToString[x] <> " " <> ToString[Q] <> " " <> ToString[id]
  }];
  output = StringTrim[result["StandardOutput"]];
  If[StringMatchQ[output, NumberString], ToExpression[output], Null]
]
testPDF = AbsoluteTiming[xPDF[0.1,125,1]];
Print["\:0412\:0440\:0435\:043c\:044f \:0432\:044b\:043f\:043e\:043b\:043d\:0435\:043d\:0438\:044f: ", testPDF[[1]]]
Abs[testPDF[[2]]]


Print["PDF check"]
index = 1;
Q2=125;
Print["PDF/x:"]
Integrate[xPDF[x,Q2,index]/x,{x,0.0001,1}] 

Print["PDF:"]
Integrate[xPDF[x,Q2,index],{x,0.0001,1}] 

"Conclusion: int xPDF = 1 = int x*PDF"
"PDF is x*f(x)"


NumberOfPoints = 20;
xVal = LogRange[0.0001, 0.9, NumberOfPoints];
xPDFval = Table[xPDF[i, Q2, index], {i, xVal}];
PDFval = Table[xPDF[i, Q2, index]/i, {i, xVal}];


xVal
xPDFval
PDFval


P1 = ListLogLogPlot[
  {PDFval, xVal},
  PlotStyle -> {Red, PointSize[Small],Dashed},
  GridLines -> Automatic,
  Frame -> Automatic,
  FrameLabel -> {"x", "PDF"},
  PlotLegends -> {"PDF"}];
  
P2 = ListLogLogPlot[
  {xPDFval, xVal},
  PlotStyle -> {Blue, PointSize[Small]},
  PlotLegends -> {"xPDF"}];
  
Show[P1, P2]




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


t0[s_]:= - s/2 * (1-Sqrt[1-4*ptmin^2/(s)])
t1[s_]:= - s/2 * (1+Sqrt[1-4*ptmin^2/s])
t0[s0] 
t1[s0]



SigmaT[x_] := Integrate[NampQQEE[x,u]*Pref[x],{u,t1[x],t0[x]}]
SigmaT[s]//Simplify
Print["sqrt(s)=",1300,"  sigma_hat(s)=",SigmaT[1300^2]*3.9*10^8," pb"]
Print["sqrt(s)=",800+800,"  sigma_hat(s)=",SigmaT[(800+800)^2]*3.9*10^8," pb"]
Print["sqrt(s)=",1000+1000,"  sigma_hat(s)=",SigmaT[(1000+1000)^2]*3.9*10^8," pb"]
Print["sqrt(s)=",1500+1500,"  sigma_hat(s)=",SigmaT[(1500+1500)^2]*3.9*10^8," pb"]
Print["sqrt(s)=MZ^2=",45.5+45.5,"  sigma_hat(s)=",SigmaT[91^2]*3.9*10^8," pb"]


(* ::Section:: *)
(*Correction with MG*)


(* ::Text:: *)
(*MADGRAPH qq>ee, Matrix element = *)
(*Cross = *)


Print["\:0438\:043d\:0442\:0435\:0433\:0440\:0438\:0440\:043e\:0432\:0430\:043d\:0438\:0435 \:0432 \:043b\:043e\:0431 \:0434\:0430\:0435\:0442 \:0447\:0443\:0448\:044c("]
IntGev = Integrate[
			NampQQEE[s0*x1*x2,u]*Pref[s0*x1*x2] xPDF[x1,s0*x1*x2,1]*xPDF[x2,s0*x1*x2,-1]/(x1*x2),
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


(* ::Section:: *)
(*\:0421\:0432\:0435\:0440\:0442\:043a\:0430*)


WidthCross[s_] := Integrate[Pref[s] (NampQQEE[s, t]*(s - MZ^2)^2  / ((s - MZ^2)^2 + MZ^2 * wZ^2) ) // Simplify, 
                                 {t, -s/2 * (1 + Sqrt[1 - (4*ptmin/Sqrt[s0])^2]), -s/2 * (1 - Sqrt[1 - (4*ptmin/Sqrt[s0])^2])}];
"cross on MZ no PDF(1300 GeV) = 0.002852"

WidthCross[MZ^2]:=WidthCross[MZ^2-0.00001]
WidthCross[s]
Print["sqrt(s)=",1300,"  sigma_hat(s)=",WidthCross[1300^2]*3.9*10^8," pb"]
Print["sqrt(s)=",800+800,"  sigma_hat(s)=",WidthCross[(800+800)^2]*3.9*10^8," pb"]
Print["sqrt(s)=",1000+1000,"  sigma_hat(s)=",WidthCross[(1000+1000)^2]*3.9*10^8," pb"]
Print["sqrt(s)=",1500+1500,"  sigma_hat(s)=",WidthCross[(1500+1500)^2]*3.9*10^8," pb"]
Print["sqrt(s)=MZ^2=",45.5+45.5,"  sigma_hat(s)=",WidthCross[91^2]*3.9*10^8," pb"]


PlotSigmaHat = LogLogPlot[
  WidthCross[x*s0],
  {x, xmin*xmin, 1},
  PlotStyle -> Blue,
  FrameLabel -> {"x", "sigma"},
  PlotLegends -> {"Sigma Partonic"}]


L[t_]:=Integrate[(1/x) *xPDF[x,t*s0,-1]*xPDF[t/x,t*s0,1],{x,t,1}]/Null^2;
Lexp[t_]:=Integrate[xPDF[10^yt,t*s0,1] * xPDF[t*10^(-yt),t*s0,-1] ,{yt, Log10[t],0}]*Log[10]/Null^2;


"\:0421\:0432\:0435\:0440\:0442\:043a\:0430 \:0441 L[t]"
xPDF[0.001,130,1]xPDF[0.00001,130,-1]
L[xmin] //N

xmin*xmin//N
xmin//N
Log10[xmin*xmin]//N


tVal = LogRange[xmin*xmin,1,NumberOfPoints]
texpVal = Range[Log10[xmin*xmin],0,-Log10[xmin*xmin]/(NumberOfPoints - 1)]//N
lVal = Table[L[i], {i, tVal}]
lexpVal = Table[Lexp[10^i], {i, texpVal}]
crossVal = Table[WidthCross[i*s0]*L[i], {i, tVal}]
crossValExp = Table[WidthCross[10^i*s0]*Lexp[10^i], {i, texpVal}]
dataLT = Transpose[{tVal,lVal}];
dataLexpT = Transpose[{tVal,lexpVal}];
dataST = Transpose[{tVal,crossVal}];
dataSTexp = Transpose[{tVal,crossValExp}];


Plot3 = ListLogLogPlot[
  dataLT,
  PlotStyle -> Blue,
  FrameLabel -> {"t", "L[t]"},
  PlotLegends -> {"Luminosity from x"}]
Plot4 = ListLogLogPlot[
  dataLexpT,
  PlotStyle -> Red,
  FrameLabel -> {"t", "Lexp[t]"},
  PlotLegends -> {"L from y=ln(x)"}]


PlotCross = ListLogLogPlot[
  dataST,
  PlotStyle -> Blue,
  FrameLabel -> {"t", "S[t*s0]*L[t]"},
  PlotLegends -> {"Sigma from x"}]
  
PlotCrossExp = ListLogLogPlot[
  dataSTexp,
  PlotStyle -> Red,
  FrameLabel -> {"t", "S[Exp[y]*s0]*L[Exp[y]]"},
  PlotLegends -> {"Sigma from y"}]



(* ::Text:: *)
(* pp>u ~u> e e+*)
(*\:0435\:0441\:0442\:044c \:0434\:0432\:0430 \:0432\:0430\:0440\:0438\:0430\:043d\:0442\:0430 u \:0438\:0437 p1 ~u \:0438\:0437 p2 \:0438 \:043d\:0430\:043e\:0431\:043e\:0440\:043e\:0442. u \:043d\:0435 \:0442\:043e\:0436\:0434\:0435\:0441\:0442\:0432\:0435\:043d\:043d\:044b, e \:0442\:043e\:0436\:0435. \:0411\:043e\:043b\:044c\:0448\:0435 \:0441\:0438\:043c\:043c\:0435\:0442\:0440\:0438\:0439\:043d\:044b\:0445 \:043c\:043d\:043e\:0436\:0438\:0442\:0435\:043b\:0435\:0439 \:043d\:0435\:0442.*)
(*/2*)


Print["Integrate log scale"]
(*correct: log(xmin*xmin),0*)
Print["s0 = ",s0, ", pt_cut = ",ptmin]
CrossSvertkaExp  =  AbsoluteTiming[NIntegrate[WidthCross[s0*(10^kt)]*Lexp[10^(kt)], {kt,Log10[xmin*xmin],0}]];
Print["\:0412\:0440\:0435\:043c\:044f \:0432\:044b\:043f\:043e\:043b\:043d\:0435\:043d\:0438\:044f: ", CrossSvertkaExp[[1]]]

Abs[CrossSvertkaExp[[2]]]*3.9*10^8 "pb"


Print["Force without log scale"]
CrossSvertka  =  AbsoluteTiming[NIntegrate[WidthCross[s0*t]*L[t]/t,{t,xmin*xmin,1}]];
Print["\:0412\:0440\:0435\:043c\:044f \:0432\:044b\:043f\:043e\:043b\:043d\:0435\:043d\:0438\:044f: ", CrossSvertka[[1]]]
Abs[CrossSvertka[[2]]]*3.9*10^8 "pb"









