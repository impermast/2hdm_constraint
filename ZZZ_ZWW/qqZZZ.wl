(* ::Package:: *)

logic = 0; (*1 autoload, 0 calculate*)
SetDirectory[NotebookDirectory[]] 
startTime = AbsoluteTime[];


(* ::Title:: *)
(*ZZZ in 2hdm*)


(*$LoadAddOns={"FeynArts", "FeynHelpers"};*)

$LoadAddOns={"FeynArts"};
<<FeynCalc`
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
(*Test vertex*)


diagscheck = InsertFields[CreateTopologies[1, 2 -> 2,ExcludeTopologies->{Tadpoles, Internal}],
 {F[9], -F[9]}->{V[2],V[2]} ,Model -> THDMCPV, InsertionLevel -> {Classes}];
Print["Test diag"]
(*Paint[diagscheck, ColumnsXRows -> {20, 10}, Numbering -> Simple,SheetHeader->None,ImageSize->{1024,512}];
*)
d1 = DiagramExtract[diagscheck, {3}];
Paint[d1, ColumnsXRows -> {1, 1}, Numbering -> Simple,SheetHeader->None,ImageSize->{125,125}];
ampd1[0] = FCFAConvert[CreateFeynAmp[d1], IncomingMomenta -> {t1,t2}, 
    OutgoingMomenta -> {Z1,Z2}, List -> False, ChangeDimension -> D, 
    DropSumOver -> True, SMP -> True, Contract -> True, UndoChiralSplittings -> True];
ampd1[1] = ampd1[0]/.M$FACouplings//ChangeDimension[#,4]&
ampd1[2] = ampd1[0]/.M$FACouplings


FCClearScalarProducts[];
SP[t1, t1] = SMP["m_q"]^2;
SP[t2, t2] = SMP["m_q"]^2;
SP[t1, t2] = (s - 2 SMP["m_q"]^2)/2;

SP[Z1, Z1] = SMP["m_Z"]^2;
SP[Z2, Z2] = SMP["m_Z"]^2;
SP[Z1, Z2] = (s - 2 SMP["m_Z"]^2)/2;

SP[t2, Z1] = (SMP["m_Z"]^2 + SMP["m_q"]^2-u)/2;
SP[t2, Z2] = (SMP["m_Z"]^2 + SMP["m_q"]^2-t)/2;

SP[t1, Z1] = (SMP["m_Z"]^2 + SMP["m_q"]^2-t)/2;
SP[t1, Z2] = (SMP["m_Z"]^2 + SMP["m_q"]^2-u)/2;

u = 2*(SMP["m_Z"]^2 + SMP["m_q"]^2)-s-t;


(*\:041c\:0430\:0442\:0440\:0438\:0447\:043d\:044b\:0439 \:0431\:0435\:0437 PV \:043f\:0440\:0435\:043e\:0431\:0440\:0430\:0437\:043e\:0432\:0430\:043d\:0438\:0439*)
ampdSquared[0] = ampd1[1]*ComplexConjugate[ampd1[1]] //
		DiracSimplify // 
        DoPolarizationSums[#, Z1, NumberOfPolarizations -> 3] & // 
        DoPolarizationSums[#, Z2, NumberOfPolarizations -> 3] & // 
        FermionSpinSum // DiracSimplify // SUNSimplify


(*\:041f\:0435\:0440\:0435\:0432\:043e\:0434\:0438\:043c \:0432 PV \:0438 \:043f\:043e\:0442\:043e\:043c \:0441\:0447\:0438\:0442\:0430\:0435\:043c \:043c\:0430\:0442\:0440\:0438\:0447\:043d\:044b\:0439*)
ampd1[3]=TID[ampd1[2], \!\(TraditionalForm\`\(TraditionalForm\`\(TraditionalForm\`LoopMom1\)\)\), ToPaVe->True,UsePaVeBasis->True] //ChangeDimension[#,4] &
ampdSquared[1] = ampd1[3]*ComplexConjugate[ampd1[3]] //
		DiracSimplify // 
        DoPolarizationSums[#, Z1, NumberOfPolarizations -> 3] & // 
        DoPolarizationSums[#, Z2, NumberOfPolarizations -> 3] & // 
        FermionSpinSum // DiracSimplify // SUNSimplify


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
(*diagHHH = DiagramExtract[diagsAll, {19..24}];*)
diagHHH = DiagramExtract[diagsAll, {19}];
diagHHG = DiagramExtract[diagsAll, {1..18}];
diagHHZ = DiagramExtract[diagsAll, {25..42}];
Print["Diagrams HHH:"]
Paint[diagHHH, ColumnsXRows -> {6, 1}, Numbering -> Simple,SheetHeader->None,ImageSize->{600,100}];
Print["Diagrams HHG:"]
Paint[diagHHG, ColumnsXRows -> {6, 3}, Numbering -> Simple,SheetHeader->None,ImageSize->{600,300}];
Print["Diagrams HHZ:"]
Paint[diagHHZ, ColumnsXRows -> {6, 3}, Numbering -> Simple,SheetHeader->None,ImageSize->{600,300}];


SecToMin[AbsoluteTime[] - startTime]


(* ::Section:: *)
(*Obtain the amplitude*)


ampHHZ[0] = FCFAConvert[CreateFeynAmp[diagHHZ], IncomingMomenta -> {p1,p2}, 
    OutgoingMomenta -> {k1,k2}, List -> False, ChangeDimension -> D, 
    DropSumOver -> True, SMP -> True, Contract -> True, UndoChiralSplittings -> True];
		
ampHHH[0] = FCFAConvert[CreateFeynAmp[diagHHH], IncomingMomenta -> {p1,p2}, 
    OutgoingMomenta -> {k1,k2}, List -> False, ChangeDimension -> D, 
    DropSumOver -> True, SMP -> True, Contract -> True, UndoChiralSplittings -> True];
		 
ampHHG[0] =  FCFAConvert[CreateFeynAmp[diagHHG], IncomingMomenta -> {p1,p2}, 
    OutgoingMomenta -> {k1,k2}, List -> False, ChangeDimension -> D, 
    DropSumOver -> True, SMP -> True, Contract -> True, UndoChiralSplittings -> True];
		
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

(*ampHHZ[1] = ampHHZ[1]//ReplaceAll[#,
	Pair[Momentum[Polarization[___],___],___]:>1]&;
ampHHH[1] = ampHHH[1]//ReplaceAll[#,
	Pair[Momentum[Polarization[___],___],___]:>1]&
ampHHG[1] = ampHHG[1]//ReplaceAll[#,
	Pair[Momentum[Polarization[___],___],___]:>1]&;*)

ampHHH[1]=ampHHH[0]/.M$FACouplings//.{(SequenceForm[FeynCalc`Pair[FeynCalc`Momentum[k1 + k2, D], FeynCalc`Momentum[k1 + k2, D]], "-", MZ^2]) -> (s-MZ^2)}


SecToMin[AbsoluteTime[] - startTime]


(* ::Section:: *)
(*Fix the kinematics*)


FCClearScalarProducts[];

(*PLEASE TAKE CARE ABOUT THE SIGH WHEN SWITCHING ALL MOMENTA TO BE INCOMING*)

SP[p1, p1] = SMP["m_q"]^2;
SP[p2, p2] = SMP["m_q"]^2;
SP[p1, p2] = (s - 2 SMP["m_q"]^2)/2;

(*s=(p1+p2)^2 *)
SP[k1, k1] = SMP["m_Z"]^2;
SP[k2, k2] = SMP["m_Z"]^2;
SP[k1, k2] = (s - 2 SMP["m_Z"]^2)/2;
SP[k1+k2,k1+k2] = s;

(*u=(p2-k1)^2 t=(p2-k2)^2*)
SP[p2, k1] = (SMP["m_Z"]^2 + SMP["m_q"]^2-u)/2;
SP[p2, k2] = (SMP["m_Z"]^2 + SMP["m_q"]^2-t)/2;

(*t=(p1-k1)^2 u=(p1-k2)^2*)
SP[p1, k1] = (SMP["m_Z"]^2 + SMP["m_q"]^2-t)/2;
SP[p1, k2] = (SMP["m_Z"]^2 + SMP["m_q"]^2-u)/2;

(*mass^2 sum=s+t+u*)

u = 2*(SMP["m_Z"]^2 + SMP["m_q"]^2)-s-t;
(*DONT WRITE Loop kinematic, PV funcs create own*)


(*Mandelstam var*)
(*FCClearScalarProducts[];
SetMandelstam[s, t, u, k1, k2, -p1, -p2 , SMP["m_t"], SMP["m_t"], SMP["m_Z"] , SMP["m_Z"]];*)



params = {
   (* \:041c\:0430\:0441\:0441\:044b \:043a\:0432\:0430\:0440\:043a\:043e\:0432 (GeV) *)
   MT -> 172.76, (* \:041c\:0430\:0441\:0441\:0430 \:0442\:043e\:043f-\:043a\:0432\:0430\:0440\:043a\:0430 *)
   MB -> 4.18,   (* \:041c\:0430\:0441\:0441\:0430 \:0431\:043e\:0442\:0442\:043e\:043c-\:043a\:0432\:0430\:0440\:043a\:0430 *)  
   (* \:041c\:0430\:0441\:0441\:044b \:043a\:0430\:043b\:0438\:0431\:0440\:043e\:0432\:043e\:0447\:043d\:044b\:0445 \:0431\:043e\:0437\:043e\:043d\:043e\:0432 (GeV) *)
   MW -> 80.4, (* \:041c\:0430\:0441\:0441\:0430 W-\:0431\:043e\:0437\:043e\:043d\:0430 *)
   MZ -> 91.2,(* \:041c\:0430\:0441\:0441\:0430 Z-\:0431\:043e\:0437\:043e\:043d\:0430 *)  
   mh1 -> 125.1, (* \:041c\:0430\:0441\:0441\:0430 \:0425\:0438\:0433\:0433\:0441\:0430 *)
   (* \:0421\:0442\:0430\:043d\:0434\:0430\:0440\:0442\:043d\:0430\:044f \:043c\:043e\:0434\:0435\:043b\:044c: \:0443\:0433\:043b\:044b \:0441\:043c\:0435\:0448\:0438\:0432\:0430\:043d\:0438\:044f *)
   sw -> Sqrt[1 - (80.4/91.2)^2], (* sin(theta_W) *)
   cw -> Sqrt[(80.4/91.2)^2],      (* cos(theta_W) *)
   (* \:041a\:0430\:043b\:0438\:0431\:0440\:043e\:0432\:043e\:0447\:043d\:044b\:0435 \:043a\:043e\:043d\:0441\:0442\:0430\:043d\:0442\:044b *)
   gw -> 0.653, (* \:041a\:0430\:043b\:0438\:0431\:0440\:043e\:0432\:043e\:0447\:043d\:0430\:044f \:043a\:043e\:043d\:0441\:0442\:0430\:043d\:0442\:0430 SU(2)_L *)
   g1 -> 0.357, (* \:041a\:0430\:043b\:0438\:0431\:0440\:043e\:0432\:043e\:0447\:043d\:0430\:044f \:043a\:043e\:043d\:0441\:0442\:0430\:043d\:0442\:0430 U(1)_Y *)
   (* \:0414\:0440\:0443\:0433\:0438\:0435 \:043f\:0430\:0440\:0430\:043c\:0435\:0442\:0440\:044b *)
   v -> 246.22, (* \:0412\:0430\:043a\:0443\:0443\:043c\:043d\:043e\:0435 \:0441\:0440\:0435\:0434\:043d\:0435\:0435 \:0437\:043d\:0430\:0447\:0435\:043d\:0438\:0435 \:0425\:0438\:0433\:0433\:0441\:0430 (GeV) *)
   e -> Sqrt[4 \[Pi] /137], (* \:042d\:043b\:0435\:043c\:0435\:043d\:0442\:0430\:0440\:043d\:044b\:0439 \:0437\:0430\:0440\:044f\:0434 \:0447\:0435\:0440\:0435\:0437 \[Alpha]e *)
   alpha -> 1/137, (* \:042d\:043b\:0435\:043a\:0442\:0440\:043e\:043c\:0430\:0433\:043d\:0438\:0442\:043d\:0430\:044f \:043a\:043e\:043d\:0441\:0442\:0430\:043d\:0442\:0430 *)
   g -> Sqrt[4 \[Pi] 1/137]/Sqrt[1 - (80.4/91.2)^2]
};



(* ::Section:: *)
(*Matrix*)


(* ::Subsubsection:: *)
(*PV-reduction*)


If[logic === 1,
	Print["Loading ampHHH from file."];
    ampHHH[2] = Get["BuffAmpHHH2.mx"],
    
    Print["Recomputing ampHHH and saving to file."];
	(*ampHHZ[2]=ampHHZ[1]/.M$FACouplings/.params//DiracSimplify//TID[#/.{p2->P-p1, p1->P-p2},q,ToPaVe->True,UsePaVeBasis->True]& //Simplify;*)
	(*ampHHZ[2] =  ampHHZ[2]/.{Momentum[P,\[Mu]]->0, Momentum[p1,\[Mu]]->0, Momentum[p2,\[Mu]]->0};*)
	ampHHH[2] = TID[ampHHH[1], LoopMom1, ToPaVe->True,UsePaVeBasis->True ]//ChangeDimension[#,4] &;
	(*ampHHG[2]=ampHHG[1]/.M$FACouplings/.params//DiracSimplify//TID[#/.{p2->P-p1, p1->P-p2},q,ToPaVe->True,UsePaVeBasis->True]& //Simplify;*)
	Put[ampHHH[2],"BuffAmpHHH2.mx"];
	Print["Done."]
]
Print["\tCPU Time used: ", Round[N[TimeUsed[]], 0.001], " s."];


ampHHH[2] 


(* ::Subsection:: *)
(*Matrix calcs*)


        
If[logic === 1,
	Print["Loading ampHHH from file."];
    ampSquared[0] = Get["BuffAmpSqHHH.mx"],
    
    Print["Recomputing ampHHH and saving to file."];
	ampSquared[0] = 1/2 (ampHHH[2] (ComplexConjugate[ampHHH[2]])) //  
		DiracSimplify // 
        DoPolarizationSums[#, k1, NumberOfPolarizations -> 3] & // 
        DoPolarizationSums[#, k2, NumberOfPolarizations -> 3] & //
        FermionSpinSum // DiracSimplify // SUNSimplify;
	Put[ampSquared[0],"BuffAmpSqHHH.mx"];
	Print["Done."]
]
Print["\tCPU Time used: ", N[TimeUsed[]], " s."];


ampSquared[0]/. PropagatorDenominator[Plus[Momentum[k1], Momentum[k2]], MZ] -> 1/(s - MZ^2)


(* ::Section:: *)
(*Coeficients of amp*)


(* ::Text:: *)
(*\:0412\:044b\:0447\:043b\:0435\:043d\:044f\:0435\:043c \:0438\:0437 \:0430\:043c\:043f\:043b\:0438\:0442\:0443\:0434\:044b \:0442\:043e\:043b\:044c\:043a\:043e \:0441\:043b\:0430\:0433\:0430\:0435\:043c\:044b\:0435 \:0441\:043e\:043e\:0442\:0432\:0435\:0442\:0441\:0442\:0432\:0443\:044e\:0449\:0438\:0435 f4 \:0432\:0435\:0440\:0448\:0438\:043d\:043d\:044b\:043c \:0444\:0443\:043d\:043a\:0446\:0438\:044f\:043c \:0441 \:043f\:043e\:043c\:043e\:0449\:044c\:044e \:0444\:0443\:043d\:043a\:0446\:0438\:0438 \:043a\:043e\:044d\:0444.*)


(* ::Subsection:: *)
(*Coeficients*)


Print["PV-func"];
FeynCalc`PaVe[0,0,1, {mZ^2, s, mZ^2}, 
	{(125.1*Sqrt[1 - mW^2/mZ^2])^2, mh2^2, mh3^2}, PaVeAutoOrder -> True, PaVeAutoReduce -> True]/.params


expr = ampHHH[2];
commonFactor = X1 * X2 * X3/SequenceForm[FeynCalc`Pair[FeynCalc`Momentum[k1, 4] + FeynCalc`Momentum[k2, 4], FeynCalc`Momentum[k1, 4] + FeynCalc`Momentum[k2, 4]], "-",MZ^2];
(* \:0418\:0437\:0432\:043b\:0435\:0447\:0435\:043d\:0438\:0435 \:043a\:043e\:044d\:0444\:0444\:0438\:0446\:0438\:0435\:043d\:0442\:043e\:0432 \:043f\:0435\:0440\:0435\:0434 FeynCalc`PaVe *)
expr = expr / commonFactor;
expr = ExpandAll[expr];


Print["Coef HHH:"];
terms = List @@ expr;
Length[terms]

C001terms = Select[terms, 
 !FreeQ[#, FeynCalc`PaVe[0,0,1, {__, __, __}, {__, __, __}, 
      PaVeAutoOrder -> True, PaVeAutoReduce -> True]] &
];
Length[C001terms]


SubjAmpHHH = commonFactor*Total[C001terms]//Simplify


(* ::Subsection:: *)
(*cross-section*)



        
If[logic === 1,
	Print["Loading Matrix from file."];
    ampSqHHH = Get["BuffAmpSqCoef.mx"],
    
    Print["Recomputing Matrix and saving to file."];
	ampSqHHH = 1/2 *1/(SUNN^2)(SubjAmpHHH (ComplexConjugate[SubjAmpHHH])) // 
        DiracSimplify // 
        DoPolarizationSums[#, k1, NumberOfPolarizations -> 3] & // 
        DoPolarizationSums[#, k2, NumberOfPolarizations -> 3] & // 
        FermionSpinSum//DiracSimplify// SUNSimplify
	Put[ampSqHHH,"BuffAmpSqCoef.mx"];
	Print["Done."]
]
Print["\tCPU Time used: ", N[TimeUsed[]], " s."];






(* \:0412\:044b\:0447\:0438\:0441\:043b\:0435\:043d\:0438\:0435 \:043f\:043e\:043b\:043d\:043e\:0433\:043e \:0440\:0430\:0441\:043f\:0430\:0434\:0430 *)
Pref[x_] := 1/(16 Pi x^2);
TotalDecay = Pref[s] * ampSqHHH



(* ::Subsubsection:: *)
(*coeficients*)


(* ::Subsubsection::Closed:: *)
(*cross only for correct VertexFunction*)
(*sigma = pref M^2 => M = f_exp *( P^mu g_ab - P^a g_mu b)*)


Amp[0]=I*(FCI[FVD[P,\[Beta]] MTD[\[Mu],\[Alpha]]] + FCI[FVD[P,\[Alpha]] MTD[\[Mu],\[Beta]]])* 10^-6


(FCI[FVD[P,\[Beta]] MTD[\[Mu],\[Alpha]]] + FCI[FVD[P,\[Alpha]] MTD[\[Mu],\[Beta]]])*(FCI[FVD[P,\[Beta]] MTD[\[Mu],\[Alpha]]] + FCI[FVD[P,\[Alpha]] MTD[\[Mu],\[Beta]]])//Contract 


(* ::Text:: *)
(*1/2 \:0438\:0437-\:0437\:0430 \:0442\:043e\:0436\:0434\:0435\:0441\:0442\:0432\:0435\:043d\:043d\:043e\:0441\:0442\:0438 \:043a\:043e\:043d\:0435\:0447\:043d\:044b\:0445 \:0441\:043e\:0441\:0442\:043e\:044f\:043d\:0438\:0439*)


SqAmp = 1/2 Amp[0]*(ComplexConjugate[Amp[0]])//Contract //Simplify
DecayRate = phaseSpacePrefactor* SqAmp/.D->4//Simplify


(* ::Section::Closed:: *)
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
