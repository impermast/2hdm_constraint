(* ::Package:: *)

(* ::Title:: *)
(*Neutralino-electron scattering*)


(* ::Section:: *)
(*Load FeynCalc and the necessary add-ons or other packages*)


SetDirectory[NotebookDirectory[]];
<< "../modules/setup.m"
SetDirectory[NotebookDirectory[]];
Print[Directory[]]


LogicSave = False;
AmpFileName = "F6ZWW.txt";
AmpPath = "buffer/"<> AmpFileName;


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
		UndoChiralSplittings->True,ChangeDimension->D,List->False, SMP->True](*/.q->q+p1+p2/.{p2->P-p1}*);
ampHHHc[0] = FCFAConvert[CreateFeynAmp[diags2,Truncated -> True], 
		IncomingMomenta->{P}, OutgoingMomenta->{p1,p2},LorentzIndexNames->{\[Mu],\[Alpha],\[Beta]},LoopMomenta->{q},
		UndoChiralSplittings->True,ChangeDimension->D,List->False, SMP->True](*/.q->q+p1+p2/.{p2->P-p1}*);	
ampWWH[0] = FCFAConvert[CreateFeynAmp[diags3,Truncated -> True], 
		IncomingMomenta->{P}, OutgoingMomenta->{p1,p2},LorentzIndexNames->{\[Mu],\[Alpha],\[Beta]},LoopMomenta->{q},
		UndoChiralSplittings->True,ChangeDimension->D,List->False, SMP->True]/.q->q+p1+p2/.{p2->P-p1};
ampGGH[0] = FCFAConvert[CreateFeynAmp[diags4,Truncated -> True], 
		IncomingMomenta->{P}, OutgoingMomenta->{p1,p2},LorentzIndexNames->{\[Mu],\[Alpha],\[Beta]},LoopMomenta->{q},
		UndoChiralSplittings->True,ChangeDimension->D,List->False, SMP->True](*/.q->q+p1+p2*)/.{p2->P-p1};
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
ampALL = (*ampHHG[2]+*)ampHHHc[2]


Print["HHG:"]
f4ZHHG[s_,mh1_,mh2_,mh3_]:= FullSimplify[
Coefficient[ampHHG[2],Eps[\[Mu],\[Alpha],\[Beta],p1]]
]/4;
FullSimplify[f4ZHHG[s,mh1,mh2,mh3]]


Print["HHHc:"]
f4ZHHHc[s_,mh1_,mh2_,mh3_,mhc_]:= FullSimplify[
Coefficient[ampHHHc[2],FCI[FVD[p1,\[Alpha]] MTD[\[Mu],\[Beta]]]]+
Coefficient[ampHHHc[2],FCI[FVD[p2,\[Alpha]] MTD[\[Mu],\[Beta]]]]+
Coefficient[ampHHHc[2],FCI[MTD[\[Alpha],\[Mu]]FVD[p2,\[Beta]]]]+
Coefficient[ampHHHc[2],FCI[MTD[\[Alpha],\[Mu]]FVD[p1,\[Beta]]]]
]/4;
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
f4Z[s_, mh1_, mh2_, mh3_, mhc_] := 
    (f4ZHHG[s, mh1, mh2, mh3] + f4ZHHHc[s, mh1, mh2, mh3, mhc]) //. AngleChanger//.PaveToLooptools;


f4Z[s,m1,m2,m3,m4]//Simplify


(* ::Section:: *)
(*Saving amplitude to file*)


If[LogicSave,
Export[AmpPath,f4Z[s,mh1,mh2,mh3,mhc]//Simplify,"Text"]];


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


Abs[Re[F1Z[0.5^2,0.25,0.4,0.5,0.5,1,1,1]]]-Abs[Re[F4table[0.5,0.25,0.4,0.5,0.5,1,1,1]]]
*)




