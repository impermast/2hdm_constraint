(* ::Package:: *)

(* ::Title:: *)
(*Neutralino-electron scattering*)


(* ::Section:: *)
(*Load FeynCalc and the necessary add-ons or other packages*)


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
Install["LoopTools"]
Needs["LoopTools`"]


(* ::Section:: *)
(*Generate Feynman diagrams*)


(* ::Text:: *)
(*Nicer typesetting*)


Subscript[m, W]=mW;
Subscript[m, Z]=mZ;
mZ=SMP["m_Z"];


$ExcludeTopologies[ V4onExt ]=FreeQ[ Cases[#, Propagator[External][__]], Vertex[4] ]&;
$ExcludeTopologies[ La ]=FreeQ[ Cases[#, Propagator[Incoming][__]], Vertex[3] ]&;

(* BLOCK ZWW hi hj Gc *)
CreateTopologies[1, 1 -> 2,ExcludeTopologies->{Tadpoles, Internal, V4onExt, La}];
diags1 = InsertFields[CreateTopologies[1, 1 -> 2,ExcludeTopologies->{Tadpoles, Internal, V4onExt}], {V[2]} -> {V[3], -V[3]}, InsertionLevel -> {Classes},
Model -> THDMCPV, ExcludeParticles -> {F[_], S[3], V[3], V[2]}, LastSelections -> {S[4|5|6], !S[1]}];


(* BLOCK ZWW hi hj Hc *)
CreateTopologies[1, 1 -> 2,ExcludeTopologies->{Tadpoles, Internal, V4onExt, La}];
diags2 = InsertFields[CreateTopologies[1, 1 -> 2,ExcludeTopologies->{Tadpoles, Internal, V4onExt}], {V[2]} -> {V[3], -V[3]}, InsertionLevel -> {Classes},
Model -> THDMCPV, ExcludeParticles -> {F[_], S[2], V[3], V[2]}, LastSelections -> {S[4|5|6], !S[1]}];

(* BLOCK ZWW Z Z Hi *)
CreateTopologies[1, 1 -> 2,ExcludeTopologies->{Tadpoles, Internal, V4onExt, La}];
diags3 = InsertFields[CreateTopologies[1, 1 -> 2,ExcludeTopologies->{Tadpoles, Internal, V4onExt}], {V[2]} -> {V[3], -V[3]}, InsertionLevel -> {Classes},
Model -> THDMCPV, ExcludeParticles -> {F[_], S[3], S[2], V[2]}, LastSelections -> {S[4|5|6], !S[1]}];

(* BLOCK ZWW GC GC hi*)
CreateTopologies[1, 1 -> 2,ExcludeTopologies->{Tadpoles, Internal, V4onExt, La}];
diags4 = InsertFields[CreateTopologies[1, 1 -> 2,ExcludeTopologies->{Tadpoles, Internal, V4onExt}], {V[2]} -> {V[3], -V[3]}, InsertionLevel -> {Classes},
Model -> THDMCPV, ExcludeParticles -> {F[_], S[3], V[3], V[2]}, LastSelections -> {S[4|5|6], !S[1]}];

(* BLOCK ZWW hi Hc Hc *)
CreateTopologies[1, 1 -> 2,ExcludeTopologies->{Tadpoles, Internal, V4onExt, La}];
diags5 = InsertFields[CreateTopologies[1, 1 -> 2,ExcludeTopologies->{Tadpoles, Internal, V4onExt}], {V[2]} -> {V[3], -V[3]}, InsertionLevel -> {Classes},
Model -> THDMCPV, ExcludeParticles -> {F[_], S[2], V[3], V[2]}, LastSelections -> {S[4|5|6], !S[1]}];


diags1 = DiagramExtract[diags1, {3..4, 6..9}];
diags2 = DiagramExtract[diags2, {3..4, 6..9}];
diags3 = DiagramExtract[diags3, {7..9}];
diags4 = DiagramExtract[diags4, {1..2,5}];
diags5 = DiagramExtract[diags5, {1,2,5}];

Print["Diagrams hi hj Gc:"]
Paint[diags1, ColumnsXRows -> {3, 1}, Numbering -> Simple,SheetHeader->None,ImageSize->{512,256}];
Print["Diagrams hi hj Hc:"]
Paint[diags2, ColumnsXRows -> {3, 1}, Numbering -> Simple,SheetHeader->None,ImageSize->{512,256}];
Print["Diagrams Z Z hi:"]
Paint[diags3, ColumnsXRows -> {3, 1}, Numbering -> Simple,SheetHeader->None,ImageSize->{512,256}];
Print["Diagrams GC GC hi:"]
Paint[diags4, ColumnsXRows -> {3, 1}, Numbering -> Simple,SheetHeader->None,ImageSize->{512,256}];
Print["Diagrams HC HC hi:"]
Paint[diags5, ColumnsXRows -> {3, 1}, Numbering -> Simple,SheetHeader->None,ImageSize->{512,256}];




(* ::Section:: *)
(*Obtain the amplitude*)


ampHHG[0] = FCFAConvert[CreateFeynAmp[diags1,Truncated -> True], 
		IncomingMomenta->{P}, OutgoingMomenta->{p1,p2},LorentzIndexNames->{\[Mu],\[Alpha],\[Beta]},LoopMomenta->{q},
		UndoChiralSplittings->True,ChangeDimension->D,List->False, SMP->True]/.q->q+p1+p2/.{p2->P-p1};
ampHHHc[0] = FCFAConvert[CreateFeynAmp[diags2,Truncated -> True], 
		IncomingMomenta->{P}, OutgoingMomenta->{p1,p2},LorentzIndexNames->{\[Mu],\[Alpha],\[Beta]},LoopMomenta->{q},
		UndoChiralSplittings->True,ChangeDimension->D,List->False, SMP->True]/.q->q+p1+p2/.{p2->P-p1};	
ampWWH[0] = FCFAConvert[CreateFeynAmp[diags3,Truncated -> True], 
		IncomingMomenta->{P}, OutgoingMomenta->{p1,p2},LorentzIndexNames->{\[Mu],\[Alpha],\[Beta]},LoopMomenta->{q},
		UndoChiralSplittings->True,ChangeDimension->D,List->False, SMP->True]/.q->q+p1+p2/.{p2->P-p1};
ampGGH[0] = FCFAConvert[CreateFeynAmp[diags4,Truncated -> True], 
		IncomingMomenta->{P}, OutgoingMomenta->{p1,p2},LorentzIndexNames->{\[Mu],\[Alpha],\[Beta]},LoopMomenta->{q},
		UndoChiralSplittings->True,ChangeDimension->D,List->False, SMP->True]/.q->q+p1+p2/.{p2->P-p1};	
ampHcHcH[0] = FCFAConvert[CreateFeynAmp[diags5,Truncated -> True], 
		IncomingMomenta->{P}, OutgoingMomenta->{p1,p2},LorentzIndexNames->{\[Mu],\[Alpha],\[Beta]},LoopMomenta->{q},
		UndoChiralSplittings->True,ChangeDimension->D,List->False, SMP->True]/.q->q+p1+p2/.{p2->P-p1};

ampHHG[1] = ampHHG[0]//ReplaceAll[#,
	Pair[Momentum[Polarization[___],___],___]:>1]&//Contract;
ampHHHc[1] = ampHHHc[0]//ReplaceAll[#,
	Pair[Momentum[Polarization[___],___],___]:>1]&//Contract;
ampWWH[1] = ampWWH[0]//ReplaceAll[#,
	Pair[Momentum[Polarization[___],___],___]:>1]&//Contract;
ampGGH[1] = ampGGH[0]//ReplaceAll[#,
	Pair[Momentum[Polarization[___],___],___]:>1]&//Contract;
ampHcHcH[1] = ampHcHcH[0]//ReplaceAll[#,
	Pair[Momentum[Polarization[___],___],___]:>1]&//Contract;


FCClearScalarProducts[];

(* BLOCK ZWW *)
ScalarProduct[p1,p1]=mW^2;
ScalarProduct[p2,p2]=mW^2;
ScalarProduct[P,p1]=ScalarProduct[P,P]/2;
ScalarProduct[P,p2]=ScalarProduct[P,P]/2;
ScalarProduct[p1,p2]=ScalarProduct[P,P]/2- mW^2;
ScalarProduct[P,P]=s;

MW=mW;
MZ=mZ;


(* ::Section:: *)
(*Fix the kinematics*)


ampHHG[2]=ampHHG[1]//DiracSimplify//TID[#,q,ToPaVe->True,UsePaVeBasis->True]&;
ampHHG[2]=ampHHG[2]/.{Momentum[P,\[Mu]]->0, Momentum[p1,\[Mu]]->0, Momentum[p2,\[Mu]]->0};

ampHHHc[2]=ampHHHc[1]//DiracSimplify//TID[#,q,ToPaVe->True,UsePaVeBasis->True]&;
ampHHHc[2]=ampHHHc[2]/.{Momentum[P,\[Mu]]->0, Momentum[p1,\[Mu]]->0, Momentum[p2,\[Mu]]->0};

ampWWH[2]=ampWWH[1]//DiracSimplify//TID[#,q,ToPaVe->True,UsePaVeBasis->True]&;
ampWWH[2]=ampWWH[2]/.{Momentum[P,\[Mu]]->0, Momentum[p1,\[Mu]]->0, Momentum[p2,\[Mu]]->0};

ampGGH[2]=ampGGH[1]//DiracSimplify//TID[#,q,ToPaVe->True,UsePaVeBasis->True]&;
ampGGH[2]=ampGGH[2]/.{Momentum[P,\[Mu]]->0, Momentum[p1,\[Mu]]->0, Momentum[p2,\[Mu]]->0};

ampHcHcH[2]=ampHcHcH[1]//DiracSimplify//TID[#,q,ToPaVe->True,UsePaVeBasis->True]&;
ampHcHcH[2]=ampHcHcH[2]/.{Momentum[P,\[Mu]]->0, Momentum[p1,\[Mu]]->0, Momentum[p2,\[Mu]]->0};

Print["Summ of amplitudes:"]
ampALL = ampHHG[2]+ampHHHc[2]


Print["HHG:"]
f4ZHHG[s_,mh1_,mh2_,mh3_]:= FullSimplify[ Coefficient[ ampHHG[2], FCI[FVD[P,\[Alpha]] MTD[\[Mu],\[Beta]]]]];
FullSimplify[f4ZHHG[s,mh1,mh2,mh3]]

Print["HHHc:"]
f4ZHHHc[s_,mh1_,mh2_,mh3_,mhc_]:= FullSimplify[ Coefficient[ ampHHHc[2], FCI[FVD[P,\[Alpha]] MTD[\[Mu],\[Beta]]]]];
FullSimplify[f4ZHHHc[s,mh1,mh2,mh3,mhc]]

Print["WWH:"]
f4WWH[s_,mh1_,mh2_,mh3_]:= FullSimplify[ Coefficient[ ampWWH[2], FCI[FVD[P,\[Alpha]] MTD[\[Mu],\[Beta]]]]];
FullSimplify[f4WWH[s,mh1,mh2,mh3]]

Print["GGH:"]
f4GGH[s_,mh1_,mh2_,mh3_]:= FullSimplify[ Coefficient[ ampGGH[2], FCI[FVD[P,\[Alpha]] MTD[\[Mu],\[Beta]]]]];
FullSimplify[f4GGH[s,mh1,mh2,mh3]]

Print["HcHcH:"]
f4HcHcH[s_,mh1_,mh2_,mh3_]:= FullSimplify[ Coefficient[ ampHcHcH[2], FCI[FVD[P,\[Alpha]] MTD[\[Mu],\[Beta]]]]];
FullSimplify[f4HcHcH[s,mh1,mh2,mh3]]

Print["Summ:"]
f4Z[s_,mh1_,mh2_,mh3_,mhc_]:=f4ZHHG[s,mh1,mh2,mh3]+f4ZHHHc[s,mh1,mh2,mh3,mhc];
FullSimplify[f4Z[s,mh1,mh2,mh3,mhc]]


(* ::Section:: *)
(*Convert to looptools*)


args = {s,mh1,mh2,mh3,mhc};
str=f4Z[s,mh1,mh2,mh3,mhc];
Print["Before changes:"]
str=ToString[str, InputForm]

str=StringReplace[str, {"PaVe[1, {" -> "C0i[cc1, {"}];
str=StringReplace[str, {"PaVe[2, {" -> "PaVe[0, 0, 1, {"}];
str=StringReplace[str, {"C1[" -> "C0i[cc1, "}];
str=StringReplace[str, {"C2[" -> "C0i[cc2, "}];
str=StringReplace[str, {"PaVe[0, 0, {" -> "C0i[cc00, {"}];
str=StringReplace[str, {"PaVe[0, 0, 1, {" -> "C0i[cc001, {"}];
str=StringReplace[str, {"PaVe[0, 0, 2, {" -> "C0i[cc002, {"}];
str=StringReplace[str, {"FeynCalc`" -> "LoopTools`"}];
str=StringReplace[str, {", PaVeAutoOrder -> True, PaVeAutoReduce -> True" -> "", "{" -> "", "}" -> ""}];
str=StringReplace[str, {"SMP[\"m_W\"]" -> "mW"}]

(*\:0417\:0430\:043c\:0435\:043d\:0430 \:0430\:043b\:044c\:0444*)
str=StringReplace[str, {"R1x1" -> "R11[a1,a2,a3]"}];   str=StringReplace[str, {"R1x2" -> "R11[a1,a2,a3]"}];  str=StringReplace[str, {"R1x3" -> "R13[a1,a2,a3]"}];
str=StringReplace[str, {"R2x1" -> "R21[a1,a2,a3]"}];   str=StringReplace[str, {"R2x2" -> "R22[a1,a2,a3]"}];  str=StringReplace[str, {"R2x3" -> "R23[a1,a2,a3]"}];
str=StringReplace[str, {"R3x1" -> "R31[a1,a2,a3]"}];   str=StringReplace[str, {"R3x2" -> "R32[a1,a2,a3]"}];  str=StringReplace[str, {"R3x3" -> "R33[a1,a2,a3]"}];

str=StringReplace[str, {"Y1" -> "Y1[a1,a2,a3]"}];   str=StringReplace[str, {"Y2" -> "Y2[a1,a2,a3]"}];  str=StringReplace[str, {"Y3" -> "Y3[a1,a2,a3]"}]; 
str=StringReplace[str, {"X1" -> "x1[a1,a2,a3]"}];   str=StringReplace[str, {"X2" -> "x2[a1,a2,a3]"}];  str=StringReplace[str, {"X3" -> "x3[a1,a2,a3]"}]; 


Print["After changes:"]
str


(* ::Section:: *)
(*Saving amplitude to file*)


Export["ZZZ/FZWW.txt",str,"Text"]


(* ::Section:: *)
(*Import example*)


(*
example = Import["ZZZ/FZWW.txt"];
args = {s,mh1,mh2,mh3,mhc,a1,a2,a3};
Activate[Inactive[SetDelayed][ToExpression["F1Z"]@@(Pattern[#,_]&/@args),ToExpression[example]]];
F1Z[s,mh1,mh2,mh3,mhc,a1,a2,a3]
Feps[q_,mi_,mj_,mhc_,a1_,a2_,a3_]:=g^3/(16*Pi^2*cw)*x1[a1,a2,a3]*x2[a1,a2,a3]*x3[a1,a2,a3]*(C0i[cc001,q^2,mW^2,mW^2,mi^2,mj^2,mW^2]-C0i[cc001,q^2,mW^2,mW^2,mi^2,mj^2,mhc^2]);
F4table[q_,m1_,m2_,m3_,mhc_,a1_,a2_,a3_]:=Feps[q,m1,m2,mhc,a1,a2,a3]+Feps[q,m2,m3,mhc,a1,a2,a3]+Feps[q,m3,m1,mhc,a1,a2,a3]-Feps[q,m2,m1,mhc,a1,a2,a3]-Feps[q,m3,m2,mhc,a1,a2,a3]-Feps[q,m1,m3,mhc,a1,a2,a3];
F4table[q,mh1,mh2,mh3,mhc,a1,a2,a3]


Print["Work with alpha params:"];
\[Alpha]1 = \[Pi]/20;
\[Alpha]2 =  0.955317;(* Lightest Higgs is a pure scalar*)
\[Alpha]3 = 0.785398;

beta = \[Alpha]1;
x1[a1_,a2_,a3_] := (Cos[a1]*Cos[a2]*Cos[a1] + Sin[a1]*Cos[a2]*Sin[a1]);
x2[a1_,a2_,a3_] :=  (-(Sin[a1]*Cos[a3] + Cos[a1]*Sin[a2]*Sin[a3])*Cos[a1] + (Cos[a1]*Cos[a3] - Sin[a1]*Sin[a2] *Sin[a3])*Sin[a1]);
x3[a1_,a2_,a3_] := ((-Cos[a1] *Sin[a2]* Cos[a3] + Sin[a1] *Sin[a3])*Cos[a1] -(Sin[a1] *Sin[a2] *Cos[a3] + Cos[a1]* Sin[a3])*Sin[a1]);

R11[a1_,a2_,a3_]:=Cos[a1]*Cos[a2];                             R12[a1_,a2_,a3_]:=Sin[a1]*Cos[a2];                              R13[a1_,a2_,a3_]:=Sin[a2];
R21[a1_,a2_,a3_]:=-(Cos[a1]*Sin[a2]*Sin[a3]+Sin[a1]*Cos[a3]);  R22[a1_,a2_,a3_]:=-(Sin[a1]*Sin[a2]*Sin[a3]-Cos[a1]*Cos[a3]);   R23[a1_,a2_,a3_]:=Sin[a3]*Cos[a2];
R31[a1_,a2_,a3_]:=-Cos[a1]*Sin[a2]*Cos[a3]+Sin[a1]*Sin[a3];    R32[a1_,a2_,a3_]:=-(Sin[a1]*Sin[a2]*Cos[a3]+Cos[a1]*Sin[a3]);   R33[a1_,a2_,a3_]:=Cos[a2]*Cos[a3]; 

Y1[a1_,a2_,a3_]:=R12[a1,a2,a3]*Cos[a1]-R11[a1,a2,a3]*Sin[a1];
Y2[a1_,a2_,a3_]:=R22[a1,a2,a3]*Cos[a1]-R21[a1,a2,a3]*Sin[a1];
Y3[a1_,a2_,a3_]:=R32[a1,a2,a3]*Cos[a1]-R31[a1,a2,a3]*Sin[a1];



Print["Numerical:"]
cw=1; g=1;
mW = 0.1;
mZ = 0.11;
Abs[Re[F1Z[0.5^2,0.25,0.4,0.5,0.5,1,1,1]]]-Abs[Re[F4table[0.5,0.25,0.4,0.5,0.5,1,1,1]]]
*)




