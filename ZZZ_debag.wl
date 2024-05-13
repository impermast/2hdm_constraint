(* ::Package:: *)

(* ::Title:: *)
(*Neutralino-electron scattering*)


description="Mnel El -> Mnel El, MSSM, matrix element squared, tree";
If[ $FrontEnd === Null,
	$FeynCalcStartupMessages = False;
	Print[description];
];
If[ $Notebooks === False,
	$FeynCalcStartupMessages = False
];
(*$LoadAddOns={"FeynArts", "FeynHelpers"};*)
$LoadAddOns={"FeynArts"};
<<FeynCalc`
$FAVerbose = 0;

FCCheckVersion[9,3,0];

Needs["CollierLink`"]
Needs["X`"]
Install["LoopTools"]
Needs["LoopTools`"]


(* ::Section:: *)
(*Generate Feynman diagrams*)


Subscript[m, W]=mW;
Subscript[m, Z]=mZ;
FCGV["MZ"]=mZ;



$ExcludeTopologies[ V4onExt ]=FreeQ[ Cases[#, Propagator[External][__]], Vertex[4] ]&;
$ExcludeTopologies[ La ]=FreeQ[ Cases[#, Propagator[Incoming][__]], Vertex[3] ]&;

(* BLOCK ZZZ hi hj Z *)
CreateTopologies[1, 1 -> 2,ExcludeTopologies->{Tadpoles, Internal, V4onExt, La}];
diagsHHZ = InsertFields[CreateTopologies[1, 1 -> 2,ExcludeTopologies->{Tadpoles, Internal, V4onExt}], {V[2]} -> {V[2], V[2]}, InsertionLevel -> {Classes},
Model -> THDMCPV, ExcludeParticles -> {F[_], V[1], V[3], S[2], S[3], U[_]}, LastSelections -> {S[4|5|6], V[2]}];

(* BLOCK ZZZ hi hj hk *)
CreateTopologies[1, 1 -> 2,ExcludeTopologies->{Tadpoles, Internal, V4onExt, La}];
diagsHHH = InsertFields[CreateTopologies[1, 1 -> 2,ExcludeTopologies->{Tadpoles, Internal, V4onExt}], {V[2]} -> {V[2], V[2]}, InsertionLevel -> {Classes},
Model -> THDMCPV, ExcludeParticles -> {F[_], V[1], V[3], S[2], S[3], U[_]}, LastSelections -> {S[4], S[5], S[6]}];

(* BLOCK ZZZ hi hj G *)
CreateTopologies[1, 1 -> 2,ExcludeTopologies->{Tadpoles, Internal, V4onExt, La}];
diagsHHG = InsertFields[CreateTopologies[1, 1 -> 2,ExcludeTopologies->{Tadpoles, Internal, V4onExt}], {V[2]} -> {V[2], V[2]}, InsertionLevel -> {Classes},
Model -> THDMCPV, ExcludeParticles -> {F[_], V[1], V[3], S[2], S[3], U[_]}, LastSelections -> {S[4|5|6], S[1]}];

diags1 = DiagramExtract[diagsHHZ, {1..6}];
diags2 = DiagramExtract[diagsHHZ, {7..12}];
diags3 = DiagramExtract[diagsHHZ, {13..18}];
diags4 = diagsHHH;
diags5 = diagsHHG;


Print["Diagrams 1:"]
Paint[diags1, ColumnsXRows -> {2, 1}, Numbering -> Simple,SheetHeader->None,ImageSize->{512,256}];
Print["Diagrams 2:"]
Paint[diags2, ColumnsXRows -> {2, 1}, Numbering -> Simple,SheetHeader->None,ImageSize->{512,256}];
Print["Diagrams 3:"]
Paint[diags3, ColumnsXRows -> {2, 1}, Numbering -> Simple,SheetHeader->None,ImageSize->{512,256}];
Print["Diagrams 4:"]
Paint[diags4, ColumnsXRows -> {2, 1}, Numbering -> Simple,SheetHeader->None,ImageSize->{512,256}];
Print["Diagrams 5:"]
Paint[diags5, ColumnsXRows -> {2, 1}, Numbering -> Simple,SheetHeader->None,ImageSize->{512,256}];\:044b



(* ::Section:: *)
(*Obtain the amplitude*)


ampHHZ[0] = FCFAConvert[CreateFeynAmp[diags1,Truncated -> True], 
		IncomingMomenta->{P}, OutgoingMomenta->{p1,p2},LorentzIndexNames->{\[Mu],\[Alpha],\[Beta]},LoopMomenta->{q},
		UndoChiralSplittings->True,ChangeDimension->D,List->False, SMP->False]/.q->q+p1+p2
ampHHZ[0]+= FCFAConvert[CreateFeynAmp[diags2,Truncated -> True], 
		IncomingMomenta->{P}, OutgoingMomenta->{p1,p2},LorentzIndexNames->{\[Mu],\[Alpha],\[Beta]},LoopMomenta->{q},
		UndoChiralSplittings->True,ChangeDimension->D,List->False, SMP->False]/.q->q+p1+p2/.p2->P-p1	
ampHHZ[0]+= FCFAConvert[CreateFeynAmp[diags3,Truncated -> True], 
		IncomingMomenta->{P}, OutgoingMomenta->{p1,p2},LorentzIndexNames->{\[Mu],\[Alpha],\[Beta]},LoopMomenta->{q},
		UndoChiralSplittings->True,ChangeDimension->D,List->False, SMP->False]/.q->q+p1+p2/.p1->P-p2
ampHHH[0]= FCFAConvert[CreateFeynAmp[diags4,Truncated -> True], 
		IncomingMomenta->{P}, OutgoingMomenta->{p1,p2},LorentzIndexNames->{\[Mu],\[Alpha],\[Beta]},LoopMomenta->{q},
		UndoChiralSplittings->True,ChangeDimension->D,List->False, SMP->False]/.q->q+p1+p2/.{p2->P-p1};
ampHHG[0]= FCFAConvert[CreateFeynAmp[diags5,Truncated -> True], 
		IncomingMomenta->{P}, OutgoingMomenta->{p1,p2},LorentzIndexNames->{\[Mu],\[Alpha],\[Beta]},LoopMomenta->{q},
		UndoChiralSplittings->True,ChangeDimension->D,List->False, SMP->False]/.q->q+p1+p2/.{p2->P-p1};

ampHHZ[1] = ampHHZ[0]//ReplaceAll[#,
	Pair[Momentum[Polarization[___],___],___]:>1]&//Contract
ampHHH[1] = ampHHH[0]//ReplaceAll[#,
	Pair[Momentum[Polarization[___],___],___]:>1]&//Contract
ampHHG[1] = ampHHG[0]//ReplaceAll[#,
	Pair[Momentum[Polarization[___],___],___]:>1]&//Contract


FCClearScalarProducts[];

(* BLOCK ZZZ *)
(*PLEASE TAKE CARE ABOUT THE SIGH WHEN SWITCHING ALL MOMENTA TO BE INCOMING*)
ScalarProduct[p1,p1]=mZ^2;
ScalarProduct[p2,p2]=mZ^2;
ScalarProduct[P,p1]=ScalarProduct[P,P]/2;
ScalarProduct[P,p2]=ScalarProduct[P,P]/2;
ScalarProduct[p1,p2]=ScalarProduct[P,P]/2- mZ^2;
ScalarProduct[P,P]=s;

MW=mW;
MZ=mZ;


(* ::Section:: *)
(*Fix the kinematics*)


ampHHZ[2]=ampHHZ[1]//DiracSimplify//TID[#(*/.{p2->P-p1, p1->P-p2}*),q,ToPaVe->True,UsePaVeBasis->True]&;
ampHHZ[2] =  ampHHZ[2]/.{Momentum[P,\[Mu]]->0, Momentum[p1,\[Mu]]->0, Momentum[p2,\[Mu]]->0};
(*\:0422\:043e\:0447\:043a\:0430 \:0441\:043b\:0435\:0448 \:044d\:0442\:043e \:043f\:043e\:0434\:0441\:0442\:0430\:043d\:043e\:0432\:043a\:0430 \:043c\:043e\:043c\:0435\:043d\:0442\:043e\:0432? \:0415\:0441\:043b\:0438 \:0442\:0443\:0442 \:0443\:0441\:0442\:0440\:0435\:043c\:043b\:044f\:0442\:044c \:043d\:0435 \:043a \:043d\:0443\:043b\:044e,\:0430 \:043a \:043a\:043e\:0440\:043d\:044e \:0438\:0437 \:0441?*)
ampHHH[2]=ampHHH[1]//DiracSimplify//TID[#(*/.{p2->P-p1, p1->P-p2}*),q,ToPaVe->True,UsePaVeBasis->True]&;
ampHHH[2] =  ampHHH[2]/.{Momentum[P,\[Mu]]->0, Momentum[p1,\[Mu]]->0, Momentum[p2,\[Mu]]->0};
ampHHG[2]=ampHHG[1]//DiracSimplify//TID[#(*/.{p2->P-p1, p1->P-p2}*),q,ToPaVe->True,UsePaVeBasis->True]&;
ampHHG[2] =  ampHHG[2]/.{Momentum[P,\[Mu]]->0, Momentum[p1,\[Mu]]->0, Momentum[p2,\[Mu]]->0};


Print["HHZ:"];
(*\:0427\:0422\:043e \:043f\:0440\:043e\:0438\:0441\:0445\:043e\:0434\:0438\:0442 \:043d\:0438\:0436\:0435, \:043f\:043e\:0441\:043b\:0435 \:0443\:043f\:0440\:0430\:0449\:0435\:043d\:0438\:044f \:043f\:0440\:043e\:0432\:043e\:0434\:0438\:0442\:0441\:044f \:0441\:0432\:0435\:0440\:0442\:043a\:0430 \:0438\:043d\:0434\:0435\:043a\:0441\:043e\:0432?*)
f4ZHHZ[s_,mh1_,mh2_,mh3_]:= FullSimplify[ Coefficient[ ampHHZ[2], FCI[FVD[P,\[Beta]] MTD[\[Mu],\[Alpha]]] + FCI[FVD[P,\[Alpha]] MTD[\[Mu],\[Beta]]]]];
FullSimplify[f4ZHHZ[s,mh1,mh2,mh3]]
(* 
FVD[P,beta] -- D lorenz vector p^beta|
MTD[mu,alpha] -- metric with mu alpha indexes
*)
Print["HHH:"];
f4ZHHH[s_,mh1_,mh2_,mh3_]:= FullSimplify[ Coefficient[ ampHHH[2], FCI[FVD[P,\[Beta]] MTD[\[Mu],\[Alpha]]] + FCI[FVD[P,\[Alpha]] MTD[\[Mu],\[Beta]]]]];
FullSimplify[f4ZHHH[s,mh1,mh2,mh3]]
Print["HHG:"];
f4ZHHG[s_,mh1_,mh2_,mh3_]:= FullSimplify[ Coefficient[ ampHHG[2], FCI[FVD[P,\[Beta]] MTD[\[Mu],\[Alpha]]] + FCI[FVD[P,\[Alpha]] MTD[\[Mu],\[Beta]]]]];
FullSimplify[f4ZHHG[s,mh1,mh2,mh3]]
Print["Summ = "]
f4Z[s_,mh1_,mh2_,mh3_,pref_]:=f4ZHHZ[s,mh1,mh2,mh3]+f4ZHHH[s,mh1,mh2,mh3]+f4ZHHG[s,mh1,mh2,mh3];
FullSimplify[f4Z[s,mh1,mh2,mh3,pref]]


\[Xi]=1;

(*NOT IN USE PART OF CODE*)

(*\:0422\:0443\:0442 \:043c\:0430\:0442\:0440\:0438\:0446\:0430 \:0441\:043c\:0435\:0448\:0438\:0432\:0430\:043d\:0438\:044f \:0445\:0438\:0433\:0433\:0441\:043e\:0432, \:043d\:043e \:043f\:043e\:0447\:0435\:043c\:0443 \:0437\:0430\:0434\:0430\:0435\:0442\:0441\:044f \:0442\:0430\:043a? *)
R1x1=c1 c2;                R1x2=s1 c2;                R1x3=s2;
R2x1=-(s1 c3 + c1 s2 s3); R2x2=c1 c3 - s1 s2 s3;     R2x3=c2 s3;
R3x1=-c1 s2 c3 + s1 s3;   R3x2=-(s1 s2 c3 + c1 s3);  R3x3=c2 c3;
c1 = Cos[\[Alpha]1]; s1 = Sin[\[Alpha]1];
c2 = Cos[\[Alpha]2]; s2 = Sin[\[Alpha]2];
c3 = Cos[\[Alpha]3]; s3 = Sin[\[Alpha]3];



(* ::Section:: *)
(*Parameters and Constants*)


(*mhc=300*10^(-3);*)
mhc=mh2;
gw = 1/sw;
g1 = 1/cw;
g=g1;
gc22 = -gw*sw; gc24 =  gw*sw; gc26 = -gw*sw;
gc29 =  cw*gw; gc31 = -cw*gw; gc33 =  gw*sw;
gc36 = -cw*gw; gc38 =  cw*gw; gc40 = -cw*gw;
gc42 =  cw*gw;

k = 10^(-3);

mZ=91.187*k;
m_Z=mZ;
SMP["m_Z"]=mZ;
mW=80.385*k;
m_W=mW;
m1=125.5*k;
m2=500*k;
m3=Sqrt[m2^2+v^2];
v=246.22*k;
vev=v;
cw=mW/mZ;
sw=Sqrt[1-cw^2];
(*g=2*mW/v;
q=g*sw;*)
SP[P,P]=s;


(* ::Section:: *)
(*Parsing to LoopTools*)


args = {s,mh1,mh2,mh3,pref};
str=f4Z[s,mh1,mh2,mh3,pref];
Print["Before changes:"]
str=ToString[str, InputForm];
(*
str=StringReplace[str, {"PaVe[1, {" -> "PaVe[0, 1, 0, {"}];
str=StringReplace[str, {"PaVe[2, {" -> "PaVe[0, 0, 1, {"}];
str=StringReplace[str, {"A0[" -> "PaVe[0, "}];
str=StringReplace[str, {"B0[" -> "PaVe[0, 0, "}];
str=StringReplace[str, {"C0[" -> "PaVe[0, 0, 0, "}];
str=StringReplace[str, {"C1[" -> "PaVe[0, 1, 0, "}];
str=StringReplace[str, {"C2[" -> "PaVe[0, 0, 1, "}];
str=StringReplace[str, {"PaVe[0, 0, {" -> "PaVe[1, 0, 0, {"}];
str=StringReplace[str, {"PaVe[0, 0, 1, {" -> "PaVe[1, 1, 0, {"}];
str=StringReplace[str, {"PaVe[0, 0, 2, {" -> "PaVe[1, 0, 1, {"}];
str=StringReplace[str, {", PaVeAutoOrder -> True, PaVeAutoReduce -> True" -> "", "{" -> "", "}" -> "", "PaVe" -> "PVX"}];
str=StringReplace[str, {"SMP[\"m_W\"]" -> "mW"}];
*)
str=StringReplace[str, {"PaVe[1, {" -> "C0i[cc1, {"}];
str=StringReplace[str, {"PaVe[2, {" -> "PaVe[0, 0, 1, {"}];
str=StringReplace[str, {"C1[" -> "C0i[cc1, "}];
str=StringReplace[str, {"C2[" -> "C0i[cc2, "}];
str=StringReplace[str, {"PaVe[0, 0, {" -> "PaVe[1, 0, 0, {"}];
str=StringReplace[str, {"PaVe[0, 0, 1, {" -> "C0i[cc001, {"}];
str=StringReplace[str, {"PaVe[0, 0, 2, {" -> "PaVe[1, 0, 1, {"}];
str=StringReplace[str, {"FeynCalc`" -> "LoopTools`"}];
str=StringReplace[str, {"X1*X2*X3" -> "pref"}];
str=StringReplace[str, {", PaVeAutoOrder -> True, PaVeAutoReduce -> True" -> "", "{" -> "", "}" -> ""}];
str=StringReplace[str, {"SMP[\"m_W\"]" -> "mW"}];


Print["After changes:"]
str;
Activate[Inactive[SetDelayed][ToExpression["F1Z"]@@(Pattern[#,_]&/@args),ToExpression[str]]]
H11[mh2_,mh3_]:=ToExpression[str1];
H12[mh2_,mh3_]:=ToExpression[str2];

body = ToExpression[str1];
funcName = "H14";


Print["Work with alpha params:"];

\[Alpha]2 = \[Pi]/5; (* Lightest Higgs is a pure scalar*)
\[Alpha]1 = \[Pi]/3;
\[Alpha]3 = \[Pi]/7;

FullSimplify[Abs[F1Z[s,mh1,mh2,mh3,pref1]]];
beta = \[Alpha]1;
x1[a1_,a2_,a3_] := (Cos[a1]*Cos[a2]*Cos[a1] + Sin[a1]*Cos[a2]*Sin[a1]);
x2[a1_,a2_,a3_] :=  (-(Sin[a1]*Cos[a3] + Cos[a1]*Sin[a2]*Sin[a3])*Cos[a1] + (Cos[a1]*Cos[a3] - Sin[a1]*Sin[a2] *Sin[a3])*Sin[a1]);
x3[a1_,a2_,a3_] := ((-Cos[a1] *Sin[a2]* Cos[a3] + Sin[a1] *Sin[a3])*Cos[a1] -(Sin[a1] *Sin[a2] *Cos[a3] + Cos[a1]* Sin[a3])*Sin[a1]);
Y1 = (R1x2*Cos[beta] - R1x1*Sin[beta]);
Y2 = (R2x2*Cos[beta] - R2x1*Sin[beta]);
Y3 = (R3x2*Cos[beta] - R3x1*Sin[beta]);


Print["Prefactor:"]
prefactor[a1_,a2_,a3_] := x1[a1,a2,a3] x2[a1,a2,a3] x3[a1,a2,a3];
N[prefactor[\[Alpha]1,\[Alpha]2,\[Alpha]3]]
pref0 =mZ^2/(b^2-mZ^2) \!\(TraditionalForm\`
\*FractionBox[
SuperscriptBox[\(g\), \(3\)], \(16 
\*SuperscriptBox[\(\[Pi]\), \(2\)] 
\*SuperscriptBox[\(cw\), \(3\)]\)]\)prefactor[\[Alpha]1,\[Alpha]2,\[Alpha]3]
pref1 =\[Pi]^2 g^3/cw^3 prefactor[\[Alpha]1,\[Alpha]2,\[Alpha]3]


(* ::Section:: *)
(*Graphs*)


b=0.5
label0 = Text[Style["m_H=" <> ToString[b] <> " TeV", FontSize -> 14, FontFamily -> "Arial", 
       Background -> LightGray, Frame -> True, FrameStyle -> Directive[Thick, Black]], {1.3, -10}];
plot1 = LogPlot[N[Abs[F1Z[x^2,m1,b,Sqrt[b^2+v^2],prefactor[\[Alpha]1,\[Alpha]2,\[Alpha]3]]/pref1]],{x,0.2,1.0},
  GridLines -> Automatic,
  ImageSize->400,
  PlotRange->{10^(-5),1},
  Epilog -> label0,
  AxesLabel -> {"q, TeV", "f4_hat"}];
  (*Pic logic*)
 If[b === 0.5, img1 = Import["/home/kds/ZZZ/plot500.png"];,
 If[b === 1, img1 = Import["/home/kds/ZZZ/plot1000.png"];, 
 img1 = Import["/home/kds/ZZZ/plot2000.png"];]]
Row[{plot1, Graphics[{Inset[img1, Scaled[{0.5, 0.5}], Automatic, Scaled[1]]}, ImageSize -> 400]}]


p = 0.5
img2 = Import["/home/kds/ZZZ/plot2.png"]
label1 = Text[Style["p=" <> ToString[p] <> " TeV", FontSize -> 14, FontFamily -> "Arial", FontWeight -> Bold, 
       Background -> LightGray], {0, 0}];
data = Table[{x, N[Abs[F1Z[p^2, m1, x, Sqrt[x^2+v^2],prefactor[\[Alpha]1,\[Alpha]2,\[Alpha]3]]*pref0/pref1]]}, {x, 0.1, 10,0.01}] // N;
l = ListLogLogPlot[data,
PlotMarkers -> Automatic,
Epilog -> label1, 
AxesLabel -> {"m_H, TeV", "Abs[f4]"}, 
PlotRange -> {10^-8,10^-4},
GridLines -> Automatic];

Show[l]



p1 = 0.5
plotex = ContourPlot[prefactor[\[Alpha]1,x,y], {x, 0.65, 1.}, {y,  0, \[Pi]/2},
  ColorFunction->GrayLevel, PlotLegends -> Automatic, 
  PlotRange -> All,  ImageSize->{200,200}];
plot1 = ContourPlot[
  N[Abs[F1Z[p1^2,m1,x,Sqrt[x^2+v^2],prefactor[\[Alpha]1,\[Alpha]2,y]]]], 
  {x, 0.65, 1.6}, {y, 0, \[Pi]/2},
  ColorFunction -> "TemperatureMap", 
  Contours -> 10,
  PlotPoints->20,
  FrameLabel -> {"\[Alpha]1", "\[Alpha]2"},
  PlotLegends -> Automatic,
  ImageSize->{200,200}\:044b
];
Row[{plot1, plotex}]






