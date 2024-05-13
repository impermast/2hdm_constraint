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


$ExcludeTopologies[ V4onExt ]=FreeQ[ Cases[#, Propagator[External][__]], Vertex[4] ]&;
$ExcludeTopologies[ La ]=FreeQ[ Cases[#, Propagator[Incoming][__]], Vertex[3] ]&;

(* BLOCK Hgg H g g *)
(*CreateTopologies[1, 1 -> 2,ExcludeTopologies->{Tadpoles, Internal, V4onExt, La}];
diagsHHZ = InsertFields[CreateTopologies[1, 1 -> 2,ExcludeTopologies->{Tadpoles, Internal, V4onExt}],
{V[2]} -> {V[2], V[2]}, InsertionLevel -> {Classes},
Model -> SM, ExcludeParticles -> {F[_], V[1], V[3], S[2], S[3], U[_]}, LastSelections -> {S[4|5|6], V[2]}];

(* BLOCK ZZZ hi hj hk *)
CreateTopologies[1, 1 -> 2,ExcludeTopologies->{Tadpoles, Internal, V4onExt, La}];
diagsHHH = InsertFields[CreateTopologies[1, 1 -> 2,ExcludeTopologies->{Tadpoles, Internal, V4onExt}], {V[2]} -> {V[2], V[2]}, InsertionLevel -> {Classes},
Model -> SM, ExcludeParticles -> {F[_], V[1], V[3], S[2], S[3], U[_]}, LastSelections -> {S[4], S[5], S[6]}];

(* BLOCK ZZZ hi hj G *)
CreateTopologies[1, 1 -> 2,ExcludeTopologies->{Tadpoles, Internal, V4onExt, La}];
diagsHHG = InsertFields[CreateTopologies[1, 1 -> 2,ExcludeTopologies->{Tadpoles, Internal, V4onExt}], {V[2]} -> {V[2], V[2]}, InsertionLevel -> {Classes},
Model -> SM, ExcludeParticles -> {F[_], V[1], V[3], S[2], S[3], U[_]}, LastSelections -> {S[4|5|6], S[1]}];

diags1 = DiagramExtract[diagsHHZ, {1..6}];
diags2 = DiagramExtract[diagsHHZ, {7..12}];
diags3 = DiagramExtract[diagsHHZ, {13..18}];
diags4 = diagsHHH;*)
(*diags5 = diagsHHG;*)


(* ::Section:: *)
(*Test vertex*)


diagscheck = InsertFields[CreateTopologies[1, 1 -> 2,ExcludeTopologies->{}],
{S[1]} -> {V[1], V[1]},Model -> SM, InsertionLevel -> {Particles }];

Print["Test diag"]
d =  DiagramExtract[diagscheck];
d1 = DiagramExtract[diagscheck, {38}];
d2 = DiagramExtract[diagscheck, {9,12,40}];
Paint[diagscheck, ColumnsXRows -> {10, 1}, Numbering -> Simple,SheetHeader->None,ImageSize->{800,64}];
Paint[d1, ColumnsXRows -> {10, 1}, Numbering -> Simple,SheetHeader->None,ImageSize->{800,64}];
Paint[d2, ColumnsXRows -> {10, 1}, Numbering -> Simple,SheetHeader->None,ImageSize->{800,64}];
(*amp[0] = FCFAConvert[CreateFeynAmp[diags, Truncated -> True, GaugeRules -> {}, 
    PreFactor -> 1], IncomingMomenta -> {p}, OutgoingMomenta -> {p}, LoopMomenta -> {q}, 
    LorentzIndexNames -> {mu, nu}, UndoChiralSplittings -> True, 
    ChangeDimension -> D, List -> True, SMP -> True, DropSumOver -> True, 
    Contract -> True, FinalSubstitutions -> {SMP["m_u"] -> SMP["m_q"]}]
    
Subscript[m, W]=mW;
Subscript[m, Z]=mZ;
FCGV["MZ"]=MZ;
FCGV["MW"]=MW;
FCGV["SW"]=MW;
FCGV["MH"]=MH;
FCGV["EL"]=ME;
*)

Print["Amplitude creating"]
ampd1[0] = FCFAConvert[CreateFeynAmp[d1,PreFactor -> 2], 
		IncomingMomenta->{P}, OutgoingMomenta->{k1,k2},LoopMomenta->{q},LorentzIndexNames -> {\[Mu],\!\(TraditionalForm\`\[Nu]\)}, UndoChiralSplittings -> True,
		ChangeDimension->D,List->False, SMP->True,DropSumOver->True]
Print["Momenta pairing"]
ampd1[1] = ampd1[0]//FCTraceFactor//DiracSimplify


ampd2[0] = FCFAConvert[CreateFeynAmp[d2,PreFactor -> 1], 
		IncomingMomenta->{P}, OutgoingMomenta->{k1,k2},LoopMomenta->{q},LorentzIndexNames -> {\[Mu],\!\(TraditionalForm\`\[Nu]\)}, UndoChiralSplittings -> True,
		ChangeDimension->D,List->False, SMP->True, DropSumOver->True];
Print["Momenta pairing"]
ampd2[1] = ampd2[0]//FCTraceFactor//DiracSimplify
(*ampd2[4] = TID[ampd2[0], q, ToPaVe -> True]*)




	
FCClearScalarProducts[]
(* BLOCK HGG *)
(*PLEASE TAKE CARE ABOUT THE SIGH WHEN SWITCHING ALL MOMENTA TO BE INCOMING
\:0426-\:0441\:0438\:0441\:0442\:0435\:043c\:0430
P=p1+p2
Pp1=0+p2p1 = Pp2 = p1p2
P(p1+p2)=2p1p2=PP=> p1p2=PP/2
\:041b-\:0441\:0438\:0441\:0442\:0435\:043c\:0430
P=p1+p2
P^2=s
p1^2=p2^2=0
2p1p2=s=2E1(E-E1)(cos\theta_12)
\:043f\:043e\:043b\:043e\:0447\:043a\:0430 \:043f\:043e \:044d\:043d\:0435\:0440\:0433\:0438\:0438. \:0430\:043c\:043f\:043b\:0438\:0442\:0443\:0434\:0430 \:043c\:043e\:0436\:0435\:0442 \:0438 \:043d\:0435 \:0437\:0430\:0432\:0438\:0441\:0438\:0442 \:043e\:0442 \:0443\:0433\:043b\:043e\:0432, \:043d\:043e \:0438\:043d\:0442\:0435\:0433\:0440\:0438\:0440\:043e\:0432\:0430\:043d\:0438\:0435 \:0431\:0443\:0434\:0435\:0442 \:0438\:0434\:0442\:0438 \:043f\:043e \:043e\:0433\:0440\:0430\:043d\:0438\:0447\:0435\:043d\:043d\:043e\:0439 \:043e\:0431\:043b\:0430\:0441\:0442\:0438
*)
ScalarProduct[k1,k1]=0;
ScalarProduct[k2,k2]=0;
ScalarProduct[Polarization[k1,-I],k1]=0;
ScalarProduct[Polarization[k2,-I],k2]=0;

ScalarProduct[P,P]=s;

ScalarProduct[k1,k2]=s/2;
ScalarProduct[P,k2]=s/2;
ScalarProduct[k1,P]=s/2;
Print["Obtein amplitude"]
ampd2[2] = ampd2[1] // TID[#, q, ToPaVe -> True]/.D->4 &//Simplify;
ampd1[2] = ampd1[1] // TID[#, q, ToPaVe -> True]/.D->4 &//Simplify;
amp = ampd1[2]+ampd2[2]/.s->SMP["m_H"]^2//Simplify
Print["Squared amplitude"]
ampSq2 = (amp (ComplexConjugate[amp])) //SUNSimplify[#, SUNNToCACF -> False] & // 
        DoPolarizationSums[#, k1, k2] & // DoPolarizationSums[#, k2, k1] & // Simplify



str=amp
Print["Before changes:"]
str=ToString[str, InputForm]

str=StringReplace[str, {"MQU[Index[Generation, 4]]" -> "mt"}];
str=StringReplace[str, {"SMP[\"m_W\"]" -> "mW"}];
str=StringReplace[str, {"SMP[\"sin_W\"]" -> "sinW"}];
str=StringReplace[str, {"SMP[\"m_H\"]" -> "mH"}];
str=StringReplace[str, {"B0[" -> "B0["}];
str=StringReplace[str, {"C0[" -> "C0[ "}];
str=StringReplace[str, {"PaVe[0, 0," -> "C0i[cc00, "}];
str=StringReplace[str, {"FeynCalc`" -> "LoopTools`"}];
str=StringReplace[str, {", PaVeAutoOrder -> True, PaVeAutoReduce -> True" -> "", "{" -> "", "}" -> ""}];
Print["After changes:"]
args={s,mW,mH,mt,sinW};
str1=str;
Activate[Inactive[SetDelayed][ToExpression["F1Z"]@@(Pattern[#,_]&/@args),ToExpression[str1]]]
F1Z[s,mW,mH,mt,sinW]
F1Z[0.100,0.125,0.120,0.23]


(*ampHHZ[2]=ampHHZ[1]//DiracSimplify//TID[#(*/.{p2->P-p1, p1->P-p2}*),q,ToPaVe->True,UsePaVeBasis->True]&;
ampHHZ[2] =  ampHHZ[2]/.{Momentum[P,\[Mu]]->0, Momentum[p1,\[Mu]]->0, Momentum[p2,\[Mu]]->0};*)





(* ::Section:: *)
(*Obtain the amplitude*)


(* ::Section:: *)
(*Fix the kinematics*)


(*
Pa ->p1 + p2
dw = A^2 / 2Ea d3p1 d3p2 /(2E1(2pi)^3 2E2(2pi)^3) 2pi delta(Pa-p1-p2)
ma=m2=0: w = (A^2 /2Ea) 1/8pi = A^2 / 16pi Ea 
\:041f\:043e\:0442\:043e\:043c \:0441\:044e\:0434\:0430 \:043d\:0430\:043a\:0440\:0443\:0447\:0438\:0432\:0430\:0435\:043c PDF\:043a\:0443
*)


phaseSpacePrefactor[Ea_] := 1/(16 Pi Ea);
sigma[Eh_] := phaseSpacePrefactor[Eh] * PDF[Eh]* F1Z[s,mH,mt,sinW];


(* ::Section:: *)
(*Parsing to LoopTools*)


(*args = {s,mh1,mh2,mh3,pref};
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
str=StringReplace[str, {"SMP[\"m_W\"]" -> "mW"}];*)





(* ::Section:: *)
(*Save func to file*)


(*Export["ZZZ/F1Z.txt",str,"Text"]*)


(* ::Section:: *)
(*Import example*)


(*example = Import["ZZZ/F1Z.txt"]
args = {s,mh1,mh2,mh3,pref};
Activate[Inactive[SetDelayed][ToExpression["F1Z"]@@(Pattern[#,_]&/@args),ToExpression[str]]]
F1Z[s,mh1,mh2,mh3,pref]
F1Z[0.5^2,0.25,0.4,0.5,1]
*)
