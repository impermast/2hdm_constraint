(* ::Package:: *)

(* ::Title:: *)
(*ppHgg in HC*)


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
(*Test vertexes*)


(* ::Subsection:: *)
(*x0aa corection of M^2 and Gamma formula*)


Print["Try 1: x0->aa. Compare with MG and FeynRules."]
d0 = InsertFields[CreateTopologies[0, 1 -> 2],
{S[3]}->{V[1],V[1]}, Model->HC, GenericModel->"HC_FA",  InsertionLevel->{Particles}];
$Model
(*b F12, s3 X0, v1 gamma*)
Paint[d0 , ColumnsXRows -> {10, 1}, Numbering -> Simple,SheetHeader->None,ImageSize->{800,90}];
Print["Amplitude creating for bb-x"]
FCClearScalarProducts[]
ampbbh[0] = FCFAConvert[CreateFeynAmp[d0], 
        IncomingMomenta -> {k}, OutgoingMomenta -> {p1,p2}, ChangeDimension -> 4,
        List -> False, SMP->True,Contract->False,DropSumOver->True,
        UndoChiralSplittings->True,TransversePolarizationVectors -> {p1, p2}
     ];
ampbbh[1]=ampbbh[0]//Contract//DiracSimplify;

(*We average over the spins and the colors of the quarks 1/3^2
Since the final state particles are indistinguishable, we add an extra 1/2.*)
Print["Squared amplitude"]
ScalarProduct[p1,p1]=0;
ScalarProduct[p1,p2]=(ScalarProduct[k,k]-ScalarProduct[p1,p1]-ScalarProduct[p2,p2])/2;
ScalarProduct[p2,p2]=0;
ScalarProduct[k,k]=MX0^2;
widthHiggs = 4.07;
AS =(1/2) (ampbbh[1]*(ComplexConjugate[ampbbh[1]]))//
 FeynAmpDenominatorExplicit // 
  DoPolarizationSums[#,p1,p2] &//DoPolarizationSums[#,p2,p1] &//   SUNSimplify[#, SUNNToCACF ->False] & //
     SUNSimplify[#, SUNNToCACF ->False] & //Simplify;
     
Print["With model parameters"]
(*/.{(MX0^2-s)->((MX0^2-s)^2+(MX0 WX0)^2)^(1/2)}*)
AS = AS/.M$FACouplings//Simplify
ASN = AS/.{SUNN->3}/.M$IntParams/.M$ExtParams/.M$Masses/.M$Widths//Simplify;
Print[ASN, "  GeV"^2]
GammaWidth = ASN/(16 Pi MX0)/.M$Masses;
Print["x0aa Width: ", GammaWidth, " GeV"]
(6.7664581829812416`*^-6)/GammaWidth


(* ::Text:: *)
(*MG and FeynRules6.76646*(10^-6)    GeV*)


(* ::Subsection:: *)
(*Test vertex2. Corection of crossection formula*)


(* ::Text:: *)
(*Sigma = (1/2Ea)(1/2Eb)*|M|^2*PhaseSpace*)


Print["Try 2"]
topologies = CreateTopologies[0, 2 -> 2];
diagrams = InsertFields[topologies, {F[4],-F[4]} -> {V[1], V[1]}, 
Model -> HC, GenericModel->"HC_FA", 
InsertionLevel->{Particles}];
diagramEEAA = DiagramExtract[diagrams,{2,3}];
Paint[diagramEEAA, ColumnsXRows -> {1, 1}, Numbering -> Simple, SheetHeader -> None, ImageSize -> {800, 90}]


Print["Amplitude creating for e~e-aa"]
ampEEAA[0] = FCFAConvert[CreateFeynAmp[diagramEEAA], 
        IncomingMomenta -> {p1, p2}, OutgoingMomenta -> {k1, k2},
        LorentzIndexNames->{\[Mu],\[Nu]}, ChangeDimension -> 4,
        List -> False, SMP->True,Contract->True,DropSumOver->True,
         TransversePolarizationVectors -> {k1, k2},
        UndoChiralSplittings->True]/.M$FACouplings
FCClearScalarProducts[];
SetMandelstam[s,t,u,p1,p2,-k1,-k2,0,0,0,0];
ampEEAA[1]=(ampEEAA[0]*ComplexConjugate[ampEEAA[0]])//
	FeynAmpDenominatorExplicit // 
	FermionSpinSum[#, ExtraFactor -> 1/2^2] &// DiracSimplify//
	DoPolarizationSums[#,k1,k2] &//DoPolarizationSums[#,k2,k1] &//   SUNSimplify[#, SUNNToCACF ->False] & //
	DiracSimplify//
    TrickMandelstam[#, {s, t, u, 2 ME^2}] & //Simplify
 


(* ::Text:: *)
(*dSigma/dOmega=1/(64 Pi^2 s) M^2;   int dOmega = 2pi int sinTheta dTheta *)


ampEEAA[2]=ampEEAA[1]/.{ME->0}
Pref = 1/(64 Pi^2 s);
ampEEAA[2]/.{Th->Th,u->-s/2(1+Cos[Th]),t->-s/2(1-Cos[Th])}//Simplify
CrossSectionEEAA[s_] :=2 Pi Integrate[Sin[Th]*Pref * ampEEAA[2]/.{u->-s/2(1+Cos[Th]),t->-s/2(1-Cos[Th])}//Simplify,{Th,0.05,Pi-0.05}]/.{EL^4 ->4 Pi alpha^2}/.{alpha->1/137}
CrossSectionEEAA[s]


CrossSectionEEAA[s]
mgdata = {{100,15.01},{200,3.756},{300,1.668},{400,0.9382},{500,0.6001}};
calcdata = Table[{x,  4*10^8*CrossSectionEEAA[x^2]}, {x, 100, 500}];
theoryPlot = ListLogPlot[calcdata, Joined -> True, PlotStyle -> Blue];
experimentalPlot = ListLogPlot[mgdata, PlotStyle -> {Red, PointSize[Medium]}, 
                                  PlotMarkers -> Automatic];
Show[theoryPlot, experimentalPlot, PlotRange -> All, 
     LegendLabel -> {"FeynCalc", "madgraph"}]
Abs[15.01-4*10^8*CrossSectionEEAA[100^2]]/15.01


(* ::Section:: *)
(*Obtain the amplitude bb x0 aa*)


Print["Try 3"]
topologies = CreateTopologies[0, 2 -> 2];
diagrams = InsertFields[topologies, {F[12],-F[12]} -> {V[1], V[1]}, LastSelections->{S[3]}, 
Model -> HC, GenericModel->"HC_FA", 
InsertionLevel->{Particles}];
Paint[diagrams, ColumnsXRows -> {1, 1}, Numbering -> Simple, SheetHeader -> None, ImageSize -> {800, 90}];


Print["Amplitude creating for bb-x-aa"]
amp[0] = FCFAConvert[CreateFeynAmp[diagrams], 
        IncomingMomenta -> {p1, p2}, OutgoingMomenta -> {k1, k2},
        LorentzIndexNames->{\[Mu],\[Nu]}, ChangeDimension -> 4,
        List -> False, SMP->True,Contract->False,DropSumOver->True,
         TransversePolarizationVectors -> {k1, k2},
        UndoChiralSplittings->True]


FCClearScalarProducts[];
SetMandelstam[s, t, u, p1, p2, -k1, -k2 , MB,MB, 0, 0];
amp[0]=amp[0]//Contract//DiracSimplify
(*\:0422\:0443\:0442 \:043b\:0435\:0436\:0430\:0442 \:0432\:0441\:0435 \:043a\:0430\:043f\:043b\:0438\:043d\:0433\:0438 \:0447\:0435\:0440\:0435\:0437 \:043f\:0430\:0440\:0430\:043c\:0435\:0442\:0440\:044b \:043c\:043e\:0434\:0435\:043b\:0438*)
(*Print[M$FACouplings]
Print[M$IntParams]
Print[M$ExtParams]
Print[M$Masses]
Print[M$Widths]*)


(*
ScalarProduct[k1,k1]=0;
ScalarProduct[k2,k2]=0;
ScalarProduct[Polarization[k1,-I],k1]=0;
ScalarProduct[Polarization[k2,-I],k2]=0;
ScalarProduct[Polarization[k1,-I],-k1]=0;
ScalarProduct[Polarization[k2,-I],-k2]=0;
ScalarProduct[p1,p1]=MB^2;
ScalarProduct[p2,p2]=MB^2;
ScalarProduct[p1,p2]=(s1-MB^2-MB^2)/2;
ScalarProduct[k1,k2]=s1/2;
*)


(*We average over the spins and the colors of the quarks 1/3^2
Since the final state particles are indistinguishable, we add an extra 1/2.*)
Print["Squared amplitude"]
ScalarProduct[k1,k1]=0;
ScalarProduct[k2,k2]=0;
widthHiggs = 4.07;
ampSquared =(1/2)(1/3)^2 (amp[0]*(ComplexConjugate[amp[0]]))//
 FeynAmpDenominatorExplicit // 
  FermionSpinSum[#, ExtraFactor -> 1/2^2] &// DiracSimplify//
DoPolarizationSums[#,k1,k2] &//DoPolarizationSums[#,k2,k1] &//   SUNSimplify[#, SUNNToCACF ->False] & //
     TrickMandelstam[#, {s, t, u, 2 MB^2}] & //Simplify
     
Print["With model parameters"]

ampSquared = ampSquared/.{t+u->2*MB^2-s}/.{(MX0^2-s)->((MX0^2-s)^2+(MX0 WX0)^2)^(1/2)}/.M$FACouplings//Simplify


ampSquaredNum = ampSquared/.M$IntParams/.M$ExtParams/.M$Masses/.M$Widths//Simplify


M$IntParams
M$ExtParams
M$Masses


(* ::Subsection:: *)
(*Function from Matrix element*)


ampSqAnalytical[x_]:=ampSquared/.{s->x}/.{SUNN->3}
ampSqFunc[x_] := ampSquaredNum/.{s->x}/.{SUNN->3}

Print["Amplitudes^2"]
{s1, ampSqFunc[13000^2]}
{MX0, ampSqFunc[MX0^2]/.M$Masses}



(* ::Text:: *)
(* Matrix element = 1.787573 e - 06 GeV^0  13000 GeV*)
(* Matrix element = 1.549835e-01 GeV^0   125 GeV*)


(* ::Subsection:: *)
(*Check matrix element*)


(* ::Text:: *)
(**)


s1 = 13000^2;


data = Table[{x, ampSqFunc[x^2]}, {x, 1, 200}];
ListLogLogPlot[data, Joined -> True, PlotStyle -> Blue, 
(* PlotTheme -> "Scientific",*)AxesLabel->{Sqrt[(p1+p2)^2],M^2}]
 ampSqFunc[MX0^2]/.M$Masses


(* ::Section:: *)
(*PDF*)


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

P1 = ListLogLogPlot[
  Transpose[{xValues, Tpdfdata[[place]]}],
  PlotStyle -> {Red, PointSize[Small]},
  GridLines -> Automatic,
  Frame -> True,
  FrameLabel -> {"x", "PDF"},
  PlotLegends -> {"Data Points"}];

P2 = LogLogPlot[
  PDFALL[x, index],
  {x, 0.0001, 1},
  PlotStyle -> Blue,
  PlotLegends -> {"Interpolated Function"}];

Show[P1, P2]

 


(* ::Section:: *)
(*Matrix with PDF*)


(* ::Text:: *)
(*\:0421\:0432\:0435\:0440\:0442\:043a\:0430 \:043f\:0434\:0444 \:0441 \:043c\:0430\:0442\:0440\:0438\:0447\:043d\:044b\:043c\:0438*)


(*MatrixElement[s_,x1_,x2_]:=PDFALL[x1,5]*PDFALL[x2,-5]*ampSqFunc[s*x1*x2]

x0 = 125/13000;
CROSS = 4*(10^8)*ampSqFunc[MX0^2]/(16*Pi*MX0^2) /.M$Masses;
Print["No pdf:        ",CROSS, " pb"]
Print["PDF:     ", PDFALL[x0,5]*PDFALL[x0,-5]]
Print["Cross*pdf:    ",CROSS*PDFALL[x0,5]*PDFALL[x0,-5]," pb"]*)





(*\:043f\:0435\:0440\:0435\:0432\:043e\:0434 \:0447\:0435\:0440\:0435\:0437 \:043c\:0433\:0443\:0448\:043d\:044b\:0439 \:043a\:0430\:043b\:044c\:043a\:0443\:043b\:044f\:0442\:043e\:0440 \:0432\:0435\:043b\:0438\:0447\:0438\:043d*)
(*
3.89379292217724 e-4 barn = 1/GeV^2
1.8937 *10^-4 * 10^12 pb = 1/GeV^2
1.8937 *10^8 pb = 1/GeV^2
*)
(*SIGMA*1.8937*10^8*)


(* ::Section:: *)
(*Fix the kinematics*)


(*
p1+p2 ->k1 + j2
dw = A^2 / 2sqrt(s) d3k1 d3k2 /(2E1(2pi)^3 2E2(2pi)^3) 2pi delta(p1+p2-k1-k2)
m1=m2=0: w = (A^2 /2sqrtS) 1/8pi = A^2 / 16pi sqrtS
\:041f\:043e\:0442\:043e\:043c \:0441\:044e\:0434\:0430 \:043d\:0430\:043a\:0440\:0443\:0447\:0438\:0432\:0430\:0435\:043c PDF\:043a\:0443
*)
razmer = "GeV"


stot=13000^2;
mh=125;
sigma[x1_,x2_,id_] := 1/(16 Pi x1*x2*stot) * PDFALL[x1,id]*PDFALL[x2,(-id)]* ampSqFunc[x1*x2*stot];
Print["Test sigma value is ",sigma[mh/Sqrt[stot],mh/Sqrt[stot],-5]," ","GeV"^(-2)]
(*
GeV^-2 -> (2*10^-14)^2 cm^2 -> 4*10^-4 b -> 4*10^8 pb
TeV^-2 -> 10^-6 GeV^-2 ->  4*10^-28 10^-6 cm^2 -> 4*10^-28 10^-6 *10^24 b -> 4* 10^-10 b - > 400 pb
*)  
Print["Test sigma value is ",4*10^8*sigma[mh/Sqrt[stot],mh/Sqrt[stot],-5]," ","pb"]


(* ::Section:: *)
(*Sigma with pdf*)


(* ::Text:: *)
(*\:0420\:0430\:0441\:0447\:0435\:0442 \:043e\:0434\:043d\:043e\:043c\:0435\:0440\:043d\:043e\:0433\:043e \:0441\:0435\:0447\:0435\:043d\:0438\:044f \:043f\:0440\:0438 x1=x2=x*)


CrossSection[x_]:=4*(10^8)*Abs[ampSqFunc[x^2]]*PDFALL[x/Sqrt[s1],5]*PDFALL[x/Sqrt[s1],-5]/(16*Pi*x^2);
matrixdata = Table[{x, CrossSection[x]}, {x, 100, 5000}];
ListLogLogPlot[matrixdata, Joined -> True, PlotStyle -> Blue, AxesLabel->{Sqrt[s],Sigma}]
Print["Onedim numerical integral:"]
NIntegrate[CrossSection[x],{x, 125, 13000}]


(* ::Text:: *)
(*\:043c\:043d\:043e\:0433\:043e\:043c\:0435\:0440\:043d\:044b\:0439 \:0441\:043b\:0443\:0447\:0430\:0439 \:0447\:0438\:0441\:043b\:0435\:043d\:043d\:043e \:0432\:043d\:0443\:0442\:0440\:0435\:043d\:043d\:0438\:043c\:0438 \:043c\:0435\:0442\:043e\:0434\:0430\:043c\:0438 \:0432\:043e\:043b\:044c\:0444\:0440\:0430\:043c\:0430*)


LogRange[a_, b_, n_] := Exp[Range[Log[a], Log[b], (Log[b] - Log[a])/(n - 1)]]//N
xVal = LogRange[0.0001,0.1,NumberOfPoints];
sigmadata=Transpose[{xVal,sigma[xVal,0.05,5]}];
ListLogLogPlot[sigmadata, Joined -> True, PlotStyle -> Blue]

sigmadata3D=Flatten[
   Table[{x, y, sigma[x, y, 4]}, {x, xVal}, {y, xVal}], 
	1];

ListPlot3D[
  sigmadata3D,
  PlotRange -> {Automatic, Automatic, Automatic},
  PlotStyle -> Directive[Opacity[0.7], Specularity[White, 10]],
  ColorFunction -> "Rainbow",
  ScalingFunctions -> {"Log", "Log", "Log"},
  AxesLabel -> {"x1", "x2", "sigma(x1, x2)"},
  Mesh -> None
]


SIGMA = NIntegrate[sigma[x,y,5],{x,0.001,1 }, {y, 0.001,1}];
SIGMA "Gev^-2"
4*10^8*SIGMA "pb"


(* ::Text:: *)
(*\:043c\:043d\:043e\:0433\:043e\:043c\:0435\:0440\:043d\:043e \:0447\:0438\:0441\:043b\:0435\:043d\:043d\:043e \:0440\:0430\:0441\:0447\:0435\:0442 \:0441\:0435\:0447\:0435\:043d\:0438\:044f \:043d\:0430 \:0441\:0435\:0442\:043a\:0435*)


integralb = 0;
bValues = LogRange[0.0001,1,NumberOfPoints];
x1 = bValues;
x2 = bValues;
Print["Number of points is ",Length[x1]]
   Do[
    Do[
    If[Mod[i, 10] == 1 && j == 1, Print["Step: ", i, "  Integral: ", integralb]];
     integralb += sigma[(x1[[i + 1]] + x1[[i]])/2, (x2[[j + 1]] + x2[[j]])/2,5] * (x1[[i + 1]] - x1[[i]]) * (x2[[j + 1]] - x2[[j]]); 
     , {j, Length[x2] - 1}]
    , {i, Length[x1] - 1}];
    
Print[integralb*4*10^8," ","pb"]


(* ::Subsection:: *)
(*strange crosssection dS/dt*)


StrangeCross[s_,x1_,x2_] := ampSqFunc[s*x1*x2]/(32 Pi s^2) * (PDFALL[x1,5]/x1) * (PDFALL[x2,-5]/x2);
StrangeCross[s1,mh/Sqrt[s1],mh/Sqrt[s1]]

xVal = LogRange[0.001,0.9,NumberOfPoints];
sigmadata3D=Flatten[
   Table[{x, y, StrangeCross[s1,x, y]}, {x, xVal}, {y, xVal}], 
	1];
ListPlot3D[
  sigmadata3D,
  PlotRange -> {Automatic, Automatic, Automatic},
  PlotStyle -> Directive[Opacity[0.7], Specularity[White, 10]],
  ColorFunction -> "Rainbow",
  ScalingFunctions -> {"Log", "Log", "Log"},
  AxesLabel -> {"x1", "x2", "sigma(x1, x2)"},
  Mesh -> None
]


Print["Integral with pdf:"]
Loka =NIntegrate[StrangeCross[s,x1,x2],{x1,0.001,0.9},{x2,0.001,0.9},{s,mh^2,s1}]
4*10^8 Loka "pb"


(* ::Section:: *)
(*Save func to file*)


(*Export["/sci/zzz/ppHgg/ppHgg.txt",str,"Text"]*)


(* ::Section:: *)
(*Import example*)


(*example = Import["/sci/zzz/ppHgg.txt"]
args = {s,mh1,mh2,mh3,pref};
Activate[Inactive[SetDelayed][ToExpression["F1Z"]@@(Pattern[#,_]&/@args),ToExpression[str]]]
F1Z[s,mh1,mh2,mh3,pref]
F1Z[0.5^2,0.25,0.4,0.5,1]
*)



