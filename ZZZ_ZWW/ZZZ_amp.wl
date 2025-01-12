(* ::Package:: *)

(* ::Title:: *)
(*ZZZ in 2hdm*)


(*$LoadAddOns={"FeynArts", "FeynHelpers"};*)
$LoadAddOns={"FeynArts"};
<<FeynCalc`
$FAVerbose = 0;

FCCheckVersion[9,3,0];
(*
Needs["CollierLink`"]
Needs["X`"]*)
AppendTo[$ModelPath, "/home/kds/.Mathematica/Applications/FeynArts/Models/"];
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
diagsHHZ = InsertFields[CreateTopologies[1, 1 -> 2,ExcludeTopologies->{Tadpoles, Internal, V4onExt}],
{V[2]} -> {V[2], V[2]}, InsertionLevel -> {Classes},
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


(* ::Section:: *)
(*Test vertex*)


diagscheck = InsertFields[CreateTopologies[0, 1 -> 2,ExcludeTopologies->{Tadpoles, Internal, V4onExt}],
{S[4]} -> {V[2], V[2]},Model -> THDMCPV, InsertionLevel -> {Classes}];

Print["Test diag"]
Paint[diagscheck, ColumnsXRows -> {2, 1}, Numbering -> Simple,SheetHeader->None,ImageSize->{512,256}]
d1 = diagscheck;
ampd1[0] = FCFAConvert[CreateFeynAmp[d1,Truncated -> True], 
		IncomingMomenta->{P}, OutgoingMomenta->{p1,p2},LorentzIndexNames->{\[Mu],\[Alpha],\[Beta]},
		UndoChiralSplittings->True,ChangeDimension->D,List->False, SMP->False]/.p->p1+p2;
ampd1[1] = ampd1[0]//ReplaceAll[#,
	Pair[Momentum[Polarization[___],___],___]:>1]&//Contract
	



im_size = {512/2,256/2}
Print["Diagrams 1:"]
Paint[diags1, ColumnsXRows -> {6, 1}, Numbering -> Simple,SheetHeader->None,ImageSize->{512,256}];
Print["Diagrams 2:"]
Paint[diags2, ColumnsXRows -> {6, 1}, Numbering -> Simple,SheetHeader->None,ImageSize->{512/2,256/2}];
Print["Diagrams 3:"]
Paint[diags3, ColumnsXRows -> {6, 1}, Numbering -> Simple,SheetHeader->None,ImageSize->{512/2,256/2}];
Print["Diagrams 4:"]
Paint[diags4, ColumnsXRows -> {6, 1}, Numbering -> Simple,SheetHeader->None,ImageSize->{512/2,256/2}];
Print["Diagrams 5:"]
Paint[diags5, ColumnsXRows -> {6, 1}, Numbering -> Simple,SheetHeader->None,ImageSize->{512/2,256/2}];


(* ::Section:: *)
(*Obtain the amplitude*)


ampHHZ[0] = FCFAConvert[CreateFeynAmp[diags1,Truncated -> True], 
		IncomingMomenta->{P}, OutgoingMomenta->{p1,p2},LorentzIndexNames->{\[Mu],\[Alpha],\[Beta]},LoopMomenta->{q},
		UndoChiralSplittings->True,ChangeDimension->D,List->False, SMP->False, Contract-> True, DropSumOver->True]/.q->q+p1+p2;
ampHHZ[0]+= FCFAConvert[CreateFeynAmp[diags2,Truncated -> True], 
		IncomingMomenta->{P}, OutgoingMomenta->{p1,p2},LorentzIndexNames->{\[Mu],\[Alpha],\[Beta]},LoopMomenta->{q},
		UndoChiralSplittings->True,ChangeDimension->D,List->False, SMP->False, Contract-> True, DropSumOver->True]/.q->q+p1+p2/.p2->P-p1;
ampHHZ[0]+= FCFAConvert[CreateFeynAmp[diags3,Truncated -> True], 
		IncomingMomenta->{P}, OutgoingMomenta->{p1,p2},LorentzIndexNames->{\[Mu],\[Alpha],\[Beta]},LoopMomenta->{q},
		UndoChiralSplittings->True,ChangeDimension->D,List->False, SMP->False, Contract-> True, DropSumOver->True]/.q->q+p1+p2/.p1->P-p2;
ampHHH[0]= FCFAConvert[CreateFeynAmp[diags4,Truncated -> True], 
		IncomingMomenta->{P}, OutgoingMomenta->{p1,p2},LorentzIndexNames->{\[Mu],\[Alpha],\[Beta]},LoopMomenta->{q},
		UndoChiralSplittings->True,ChangeDimension->D,List->False, SMP->False, Contract-> True, DropSumOver->True]/.q->q+p1+p2/.{p2->P-p1};
ampHHG[0]= FCFAConvert[CreateFeynAmp[diags5,Truncated -> True], 
		IncomingMomenta->{P}, OutgoingMomenta->{p1,p2},LorentzIndexNames->{\[Mu],\[Alpha],\[Beta]},LoopMomenta->{q},
		UndoChiralSplittings->True,ChangeDimension->D,List->False, SMP->False, Contract-> True, DropSumOver->True]/.q->q+p1+p2/.{p2->P-p1};
		
(* \:0421 \:043f\:0440\:0435\:0444\:0430\:043a\:0442\:043e\:0440\:043e\:043c =1
ampHHZ[0] = FCFAConvert[CreateFeynAmp[diags1,Truncated -> True,PreFactor->1], 
		IncomingMomenta->{P}, OutgoingMomenta->{p1,p2},LorentzIndexNames->{\[Mu],\[Alpha],\[Beta]},LoopMomenta->{q},
		UndoChiralSplittings->True,ChangeDimension->D,List->False, SMP->False]/.q->q+p1+p2
ampHHZ[0]+= FCFAConvert[CreateFeynAmp[diags2,Truncated -> True,PreFactor->1], 
		IncomingMomenta->{P}, OutgoingMomenta->{p1,p2},LorentzIndexNames->{\[Mu],\[Alpha],\[Beta]},LoopMomenta->{q},
		UndoChiralSplittings->True,ChangeDimension->D,List->False, SMP->False]/.q->q+p1+p2/.p2->P-p1	
ampHHZ[0]+= FCFAConvert[CreateFeynAmp[diags3,Truncated -> True,PreFactor->1], 
		IncomingMomenta->{P}, OutgoingMomenta->{p1,p2},LorentzIndexNames->{\[Mu],\[Alpha],\[Beta]},LoopMomenta->{q},
		UndoChiralSplittings->True,ChangeDimension->D,List->False, SMP->False]/.q->q+p1+p2/.p1->P-p2
ampHHH[0]= FCFAConvert[CreateFeynAmp[diags4,Truncated -> True,PreFactor->1], 
		IncomingMomenta->{P}, OutgoingMomenta->{p1,p2},LorentzIndexNames->{\[Mu],\[Alpha],\[Beta]},LoopMomenta->{q},
		UndoChiralSplittings->True,ChangeDimension->D,List->False, SMP->False]/.q->q+p1+p2/.{p2->P-p1};
ampHHG[0]= FCFAConvert[CreateFeynAmp[diags5,Truncated -> True,PreFactor->1], 
		IncomingMomenta->{P}, OutgoingMomenta->{p1,p2},LorentzIndexNames->{\[Mu],\[Alpha],\[Beta]},LoopMomenta->{q},
		UndoChiralSplittings->True,ChangeDimension->D,List->False, SMP->False]/.q->q+p1+p2/.{p2->P-p1};
*)
ampHHZ[1] = ampHHZ[0]//ReplaceAll[#,
	Pair[Momentum[Polarization[___],___],___]:>1]&//Contract;
ampHHH[1] = ampHHH[0]//ReplaceAll[#,
	Pair[Momentum[Polarization[___],___],___]:>1]&//Contract
ampHHG[1] = ampHHG[0]//ReplaceAll[#,
	Pair[Momentum[Polarization[___],___],___]:>1]&//Contract;





(* ::Section:: *)
(*Fix the kinematics*)


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


ampHHZ[2]=ampHHZ[1]//DiracSimplify//TID[#(*/.{p2->P-p1, p1->P-p2}*),q,ToPaVe->True,UsePaVeBasis->True]& //Simplify;
(*ampHHZ[2] =  ampHHZ[2]/.{Momentum[P,\[Mu]]->0, Momentum[p1,\[Mu]]->0, Momentum[p2,\[Mu]]->0};*)
ampHHH[2]=ampHHH[1]//DiracSimplify//TID[#(*/.{p2->P-p1, p1->P-p2}*),q,ToPaVe->True,UsePaVeBasis->True]&//Simplify
(*ampHHH[2] =  ampHHH[2]/.{Momentum[P,\[Mu]]->0, Momentum[p1,\[Mu]]->0, Momentum[p2,\[Mu]]->0}*)
ampHHG[2]=ampHHG[1]//DiracSimplify//TID[#(*/.{p2->P-p1, p1->P-p2}*),q,ToPaVe->True,UsePaVeBasis->True]& //Simplify;
(*ampHHG[2] =  ampHHG[2]/.{Momentum[P,\[Mu]]->0, Momentum[p1,\[Mu]]->0, Momentum[p2,\[Mu]]->0};*)


(* ::Subsubsection:: *)
(*cross-section*)


ampHHH[3] = ampHHH[2]// FCReplaceD[#, D -> 4 - 2 Epsilon] & //
    Series[#, {Epsilon, 0, 0}] & // Normal


ampSqHHH = 1/2 (ampHHH[3]  (ComplexConjugate[ampHHH[3] ]))//Contract //
        FeynAmpDenominatorExplicit  // Simplify

phaseSpacePrefactor = 1/(16 Pi mZ);
TotalDecay = phaseSpacePrefactor * ampSqHHH


(* ::Subsubsection:: *)
(*coeficients*)


Print["HHZ:"];
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
f4Z[s_,mh1_,mh2_,mh3_,pref_]:=pref*f4ZHHZ[s,mh1,mh2,mh3]+f4ZHHH[s,mh1,mh2,mh3]+f4ZHHG[s,mh1,mh2,mh3];
FullSimplify[f4Z[s,mh1,mh2,mh3,pref]]


(* ::Subsubsection:: *)
(*cross only for correct VertexFunction*)


Amp[0]=I*(FCI[FVD[P,\[Beta]] MTD[\[Mu],\[Alpha]]] + FCI[FVD[P,\[Alpha]] MTD[\[Mu],\[Beta]]])f4Z[s,m1,m2,m3,1]


(FCI[FVD[P,\[Beta]] MTD[\[Mu],\[Alpha]]] + FCI[FVD[P,\[Alpha]] MTD[\[Mu],\[Beta]]])*(FCI[FVD[P,\[Beta]] MTD[\[Mu],\[Alpha]]] + FCI[FVD[P,\[Alpha]] MTD[\[Mu],\[Beta]]])//Contract 


(* ::Text:: *)
(*1/2 \:0438\:0437-\:0437\:0430 \:0442\:043e\:0436\:0434\:0435\:0441\:0442\:0432\:0435\:043d\:043d\:043e\:0441\:0442\:0438 \:043a\:043e\:043d\:0435\:0447\:043d\:044b\:0445 \:0441\:043e\:0441\:0442\:043e\:044f\:043d\:0438\:0439*)


SqAmp = 1/2 Amp[0]*(ComplexConjugate[Amp[0]])//Contract //Simplify
DecayRate = phaseSpacePrefactor* SqAmp/.D->4//Simplify


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
str


(* ::Section:: *)
(*Save func to file*)


(*Export["/home/kds/sci/zzz/2hdm_constraint/F1Z.txt",str,"Text"]*)


(* ::Section:: *)
(*Import example*)


(*example = Import["ZZZ/F1Z.txt"]
args = {s,mh1,mh2,mh3,pref};
Activate[Inactive[SetDelayed][ToExpression["F1Z"]@@(Pattern[#,_]&/@args),ToExpression[str]]]
F1Z[s,mh1,mh2,mh3,pref]
F1Z[0.5^2,0.25,0.4,0.5,1]
*)
