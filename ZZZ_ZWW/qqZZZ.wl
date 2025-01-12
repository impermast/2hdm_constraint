(* ::Package:: *)

logic = 1; (*1 autoload, 0 calculate*)


(* ::Title:: *)
(*ZZZ in 2hdm*)


(*$LoadAddOns={"FeynArts", "FeynHelpers"};*)
startTime = AbsoluteTime[];
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


SecToMin[seconds_] := Module[
    {hours, minutes, secs},
    secs = Round[Mod[seconds, 60]];
    hours = Floor[secs / 3600];
    minutes = Floor[Mod[secs, 3600] / 60];
    Print[ToString[hours] <> " \:0447\:0430\:0441\:043e\:0432 " <> 
          ToString[minutes] <> " \:043c\:0438\:043d\:0443\:0442 " <> 
          ToString[secs] <> " \:0441\:0435\:043a\:0443\:043d\:0434"];
];



(* ::Section:: *)
(*Generate Feynman diagrams*)


Subscript[m, W]=mW;
Subscript[m, Z]=mZ;
FCGV["MZ"]=mZ;



$ExcludeTopologies[ V4onExt ]=FreeQ[ Cases[#, Propagator[External][__]], Vertex[4] ]&;
$ExcludeTopologies[ La ]=FreeQ[ Cases[#, Propagator[Incoming][__]], Vertex[3] ]&;
diagsQQZZ = InsertFields[CreateTopologies[1, 2 -> 2,ExcludeTopologies->{Tadpoles}],
{F[9], -F[9]} -> {V[2],V[2]}, InsertionLevel -> {Classes},
Model -> THDMCPV, ExcludeParticles -> {F[_], V[1], V[3], S[2], S[3], U[_]}, LastSelections -> {V[2]}];
(*(* BLOCK ZZZ hi hj G *)
CreateTopologies[1, 1 -> 2,ExcludeTopologies->{Tadpoles, Internal, V4onExt, La}];
diagsHHG = InsertFields[CreateTopologies[1, 1 -> 2,ExcludeTopologies->{Tadpoles, Internal, V4onExt}], {V[2]} -> {V[2], V[2]}, InsertionLevel -> {Classes},
Model -> THDMCPV, ExcludeParticles -> {F[_], V[1], V[3], S[2], S[3], U[_]}, LastSelections -> {S[4|5|6], S[1]}];
*)
diagsAll = DiagramExtract[diagsQQZZ, {100..141}];
diagscheckUp = DiagramExtract[diagsQQZZ, {95..99,142..145}];
(*diags2 = DiagramExtract[diagsHHZ, {7..12}];*)
Print["Diagrams 1:"]
Paint[diagsAll, ColumnsXRows -> {11, 4}, Numbering -> Simple,SheetHeader->None,ImageSize->{1024,412}];
Print["Check"]
Paint[diagscheckUp, ColumnsXRows -> {10, 2}, Numbering -> Simple,SheetHeader->None,ImageSize->{1200,256}];


(*HHH*)
diagHHH = DiagramExtract[diagsAll, {19..24}];
diagHHG = DiagramExtract[diagsAll, {1..18}];
diagHHZ = DiagramExtract[diagsAll, {25..42}];
Print["Diagrams HHH:"]
Paint[diagHHH, ColumnsXRows -> {6, 1}, Numbering -> Simple,SheetHeader->None,ImageSize->{600,100}];
Print["Diagrams HHG:"]
Paint[diagHHG, ColumnsXRows -> {6, 3}, Numbering -> Simple,SheetHeader->None,ImageSize->{600,300}];
Print["Diagrams HHZ:"]
Paint[diagHHZ, ColumnsXRows -> {6, 3}, Numbering -> Simple,SheetHeader->None,ImageSize->{600,300}];


SecToMin[AbsoluteTime[] - startTime]


(* ::Section::Closed:: *)
(*Test vertex*)


diagscheck = InsertFields[CreateTopologies[0, 1 -> 2,ExcludeTopologies->{Tadpoles, Internal, V4onExt}],
{S[4]} -> {F[9], -F[9]},Model -> THDMCPV, InsertionLevel -> {Classes}];

Print["Test diag"]
Paint[diagscheck, ColumnsXRows -> {2, 1}, Numbering -> Simple,SheetHeader->None,ImageSize->{512,256}];
d1 = diagscheck;
ampd1[0] = FCFAConvert[CreateFeynAmp[d1,Truncated -> True], 
		IncomingMomenta->{P}, OutgoingMomenta->{p1,p2},LorentzIndexNames->{\[Mu],\[Alpha],\[Beta]},
		UndoChiralSplittings->True,ChangeDimension->D,List->False, SMP->False]/.p->p1+p2;
ampd1[1] = ampd1[0]//ReplaceAll[#,
	Pair[Momentum[Polarization[___],___],___]:>1]&//Contract
	



(* ::Section:: *)
(*Obtain the amplitude*)


ampHHZ[0] = FCFAConvert[CreateFeynAmp[diagHHZ,Truncated -> True], 
		IncomingMomenta->{Q1,Q2}, OutgoingMomenta->{p1,p2},LorentzIndexNames->{\[Alpha]1,\[Beta]1,\[Mu]1},LoopMomenta->{q},
		UndoChiralSplittings->True,ChangeDimension->D,List->False, SMP->False, Contract-> False, DropSumOver->True]/.Q1+Q2->P;
ampHHH[0]= FCFAConvert[CreateFeynAmp[diagHHH,Truncated -> True], 
		IncomingMomenta->{Q1,Q2}, OutgoingMomenta->{p1,p2},LorentzIndexNames->{\[Alpha]1,\[Beta]1,\[Mu]1},LoopMomenta->{q},
		UndoChiralSplittings->True,ChangeDimension->D,List->False, SMP->False, Contract-> False, DropSumOver->True]/.Q1+Q2->P;
ampHHG[0]= FCFAConvert[CreateFeynAmp[diagHHG,Truncated -> True], 
		IncomingMomenta->{Q1,Q2}, OutgoingMomenta->{p1,p2},LorentzIndexNames->{\[Alpha]1,\[Beta]1,\[Mu]1},LoopMomenta->{q},
		UndoChiralSplittings->True,ChangeDimension->D,List->False, SMP->False, Contract-> False, DropSumOver->True]/.Q1+Q2->P;
		
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



SetDirectory[NotebookDirectory[]]
Put[ampHHH[1],"BuffAmpHHH1.mx"]
Get["BuffAmpHHH1.mx"]


SecToMin[AbsoluteTime[] - startTime]


(* ::Section:: *)
(*Fix the kinematics*)


FCClearScalarProducts[];

(*PLEASE TAKE CARE ABOUT THE SIGH WHEN SWITCHING ALL MOMENTA TO BE INCOMING*)
ScalarProduct[p1,p1]=mZ^2;
ScalarProduct[p2,p2]=mZ^2;
ScalarProduct[Q1,Q1]=mq^2;
ScalarProduct[Q2,Q2]=mq^2;
ScalarProduct[p1,p2]= s/2 - mZ^2;
ScalarProduct[Q1,Q2]=(s-2*mq^2)/2;

MW=mW;
MZ=mZ;



params = {
   (* \:041c\:0430\:0441\:0441\:044b \:043a\:0432\:0430\:0440\:043a\:043e\:0432 (GeV) *)
   mt -> 172.76, (* \:041c\:0430\:0441\:0441\:0430 \:0442\:043e\:043f-\:043a\:0432\:0430\:0440\:043a\:0430 *)
   mb -> 4.18,   (* \:041c\:0430\:0441\:0441\:0430 \:0431\:043e\:0442\:0442\:043e\:043c-\:043a\:0432\:0430\:0440\:043a\:0430 *)  
   (* \:041c\:0430\:0441\:0441\:044b \:043a\:0430\:043b\:0438\:0431\:0440\:043e\:0432\:043e\:0447\:043d\:044b\:0445 \:0431\:043e\:0437\:043e\:043d\:043e\:0432 (GeV) *)
   mW -> 80.4, (* \:041c\:0430\:0441\:0441\:0430 W-\:0431\:043e\:0437\:043e\:043d\:0430 *)
   mZ -> 91.2,(* \:041c\:0430\:0441\:0441\:0430 Z-\:0431\:043e\:0437\:043e\:043d\:0430 *)  
   mh1 -> 125.1 (* \:041c\:0430\:0441\:0441\:0430 \:0425\:0438\:0433\:0433\:0441\:0430 *)
   (* \:0421\:0442\:0430\:043d\:0434\:0430\:0440\:0442\:043d\:0430\:044f \:043c\:043e\:0434\:0435\:043b\:044c: \:0443\:0433\:043b\:044b \:0441\:043c\:0435\:0448\:0438\:0432\:0430\:043d\:0438\:044f *)
   sw -> Sqrt[1 - (mW/mZ)^2], (* sin(theta_W) *)
   cw -> Sqrt[(mW/mZ)^2],      (* cos(theta_W) *)
   (* \:041a\:0430\:043b\:0438\:0431\:0440\:043e\:0432\:043e\:0447\:043d\:044b\:0435 \:043a\:043e\:043d\:0441\:0442\:0430\:043d\:0442\:044b *)
   gw -> 0.653, (* \:041a\:0430\:043b\:0438\:0431\:0440\:043e\:0432\:043e\:0447\:043d\:0430\:044f \:043a\:043e\:043d\:0441\:0442\:0430\:043d\:0442\:0430 SU(2)_L *)
   g1 -> 0.357, (* \:041a\:0430\:043b\:0438\:0431\:0440\:043e\:0432\:043e\:0447\:043d\:0430\:044f \:043a\:043e\:043d\:0441\:0442\:0430\:043d\:0442\:0430 U(1)_Y *)
   (* \:0414\:0440\:0443\:0433\:0438\:0435 \:043f\:0430\:0440\:0430\:043c\:0435\:0442\:0440\:044b *)
   v -> 246.22, (* \:0412\:0430\:043a\:0443\:0443\:043c\:043d\:043e\:0435 \:0441\:0440\:0435\:0434\:043d\:0435\:0435 \:0437\:043d\:0430\:0447\:0435\:043d\:0438\:0435 \:0425\:0438\:0433\:0433\:0441\:0430 (GeV) *)
   e -> Sqrt[4 \[Pi] \[Alpha]e], (* \:042d\:043b\:0435\:043c\:0435\:043d\:0442\:0430\:0440\:043d\:044b\:0439 \:0437\:0430\:0440\:044f\:0434 \:0447\:0435\:0440\:0435\:0437 \[Alpha]e *)
   \[Alpha]e -> 1/137, (* \:042d\:043b\:0435\:043a\:0442\:0440\:043e\:043c\:0430\:0433\:043d\:0438\:0442\:043d\:0430\:044f \:043a\:043e\:043d\:0441\:0442\:0430\:043d\:0442\:0430 *)
   g -> Sqrt[4 \[Pi] \[Alpha]e]/sw
};



If[logic === 1,
	Print["Loading ampHHH from file."];
    ampHHH[3] = Get["BuffAmpHHH2.mx"],
    
    Print["Recomputing ampHHH and saving to file."];
	ampHHZ[2]=ampHHZ[1]/.M$FACouplings/.params//DiracSimplify//TID[#(*/.{p2->P-p1, p1->P-p2}*),q,ToPaVe->True,UsePaVeBasis->True]& //Simplify;
	(*ampHHZ[2] =  ampHHZ[2]/.{Momentum[P,\[Mu]]->0, Momentum[p1,\[Mu]]->0, Momentum[p2,\[Mu]]->0};*)
	ampHHH[2]=ampHHH[1]/.M$FACouplings/.params//DiracSimplify//TID[#(*/.{p2->P-p1, p1->P-p2}*),q,ToPaVe->True,UsePaVeBasis->True]& //Simplify;
	(*ampHHH[2] =  ampHHH[2]/.{Momentum[P,\[Mu]]->0, Momentum[p1,\[Mu]]->0, Momentum[p2,\[Mu]]->0}*)
	ampHHG[2]=ampHHG[1]/.M$FACouplings/.params//DiracSimplify//TID[#(*/.{p2->P-p1, p1->P-p2}*),q,ToPaVe->True,UsePaVeBasis->True]& //Simplify;
	(*ampHHG[2] =  ampHHG[2]/.{Momentum[P,\[Mu]]->0, Momentum[p1,\[Mu]]->0, Momentum[p2,\[Mu]]->0};*)
	Put[ampHHH[2],"BuffAmpHHH2.mx"]
	Print["Done."]
]


SecToMin[AbsoluteTime[] - startTime]


(* ::Section:: *)
(*Coeficients of amp*)


(* ::Text:: *)
(*\:0412\:044b\:0447\:043b\:0435\:043d\:044f\:0435\:043c \:0438\:0437 \:0430\:043c\:043f\:043b\:0438\:0442\:0443\:0434\:044b \:0442\:043e\:043b\:044c\:043a\:043e \:0441\:043b\:0430\:0433\:0430\:0435\:043c\:044b\:0435 \:0441\:043e\:043e\:0442\:0432\:0435\:0442\:0441\:0432\:0443\:044e\:0449\:0438\:0435 f4 \:0432\:0435\:0440\:0448\:0438\:043d\:043d\:044b\:043c \:0444\:0443\:043d\:043a\:0446\:0438\:044f\:043c \:0441 \:043f\:043e\:043c\:043e\:0449\:044c\:044e \:0444\:0443\:043d\:043a\:0446\:0438\:0438 \:043a\:043e\:044d\:0444.*)


(* ::Subsection:: *)
(*Coeficients*)


CoefficientList[ampHHH[2],C0i]


(* ::Subsection:: *)
(*cross-section*)


If[logic === 1,
    Print["Loading ampHHH from file."];
    ampHHH[3] = Get["ampHHH3.mx"],
    
    Print["Recomputing ampHHH and saving to file."];
    ampHHH[3] = ampHHH[2]// FCReplaceD[#, D -> 4 - 2 Epsilon] & //
    Series[#, {Epsilon, 0, 0}] & // Normal;
    Amp = ampHHH[3];
    Put[ampHHH[3],"ampHHH3.mx"];
    Print["Done."];
];


If[logic === 1,
    Print["Loading ampSqHHH from file."];
    ampSqHHH = Get["ampSqHHH.mx"],
    (* \:041f\:0435\:0440\:0435\:0441\:0447\:0435\:0442 \:0430\:043c\:043f\:043b\:0438\:0442\:0443\:0434\:044b, \:0435\:0441\:043b\:0438 logic = 0 *)
    Print["Recomputing ampSqHHH and saving to file."];
    ampSqHHH = 1/2 (ampHHH[3] (ComplexConjugate[ampHHH[3]])) //
        FeynAmpDenominatorExplicit // 
        FermionSpinSum[#, ExtraFactor -> 1/2^2] & // 
        DiracSimplify // 
        DoPolarizationSums[#, p1, NumberOfPolarizations -> 3] & // 
        DoPolarizationSums[#, p2, NumberOfPolarizations -> 3] & // 
        Simplify;
    Put[ ampSqHHH, "ampSqHHH.mx"];
    Print["Done."] 
];

(* \:0412\:044b\:0447\:0438\:0441\:043b\:0435\:043d\:0438\:0435 \:043f\:043e\:043b\:043d\:043e\:0433\:043e \:0440\:0430\:0441\:043f\:0430\:0434\:0430 *)
Pref[x_] := 1/(16 Pi x^2);
TotalDecay = Pref[s] * ampSqHHH


SecToMin[AbsoluteTime[] - startTime]


(* ::Subsubsection:: *)
(*coeficients*)


Print["HHZ:"];
P = p1+p2
f4ZHHZ[s_,mh1_,mh2_,mh3_]:= FullSimplify[ Coefficient[ ampHHZ[2], FCI[FVD[P,\[Mu]] MTD[\[Beta],\[Alpha]]] + FCI[FVD[P,\[Alpha]] MTD[\[Mu],\[Beta]]]]];
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
(*sigma = pref M^2 => M = f_exp *( P^mu g_ab - P^a g_mu b)*)


Amp[0]=I*(FCI[FVD[P,\[Beta]] MTD[\[Mu],\[Alpha]]] + FCI[FVD[P,\[Alpha]] MTD[\[Mu],\[Beta]]])* 10^-6


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
(*Timing*)


endTime = AbsoluteTime[];
executionTime = endTime - startTime
SecToMin[executionTime]


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
