(* ::Package:: *)

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



Subscript[m, W]=mW;
Subscript[m, Z]=mZ;
FCGV["MZ"]=mZ;


$ExcludeTopologies[ V4onExt ]=FreeQ[ Cases[#, Propagator[External][__]], Vertex[4] ]&;
$ExcludeTopologies[ La ]=FreeQ[ Cases[#, Propagator[Incoming][__]], Vertex[3] ]&;

(* BLOCK ZZZ hi hj Z *)
CreateTopologies[1, 1 -> 2,ExcludeTopologies->{Tadpoles, Internal, V4onExt, La}];
diagsHHZ = InsertFields[CreateTopologies[1, 1 -> 2,ExcludeTopologies->{Internal, V4onExt}], {V[2]} -> {S[4], S[5]}, InsertionLevel -> {Classes},
Model -> THDMCPV, ExcludeParticles -> {F[_], V[1], V[3], S[2], S[3], U[_]}];
diags1 = DiagramExtract[diagsHHZ, {1..6}];
diags2 = DiagramExtract[diagsHHZ, {7..12}];
diags3 = DiagramExtract[diagsHHZ, {13..18}];

Print["Diagrams 1:"]
Paint[diags1, ColumnsXRows -> {2, 1}, Numbering -> Simple,SheetHeader->None,ImageSize->{512,256}];
Print["Diagrams 2:"]
Paint[diags2, ColumnsXRows -> {2, 1}, Numbering -> Simple,SheetHeader->None,ImageSize->{512,256}];
Print["Diagrams 3:"]
Paint[diags3, ColumnsXRows -> {2, 1}, Numbering -> Simple,SheetHeader->None,ImageSize->{512,256}];*)


ampHHZ[0] = FCFAConvert[CreateFeynAmp[diags1,Truncated -> True,PreFactor->1], 
		IncomingMomenta->{P}, OutgoingMomenta->{p1,p2},LorentzIndexNames->{\[Mu],\[Alpha],\[Beta]},LoopMomenta->{q},
		UndoChiralSplittings->True,ChangeDimension->D,List->False, SMP->False]/.q->q+p1+p2
		
ampHHZ[1] = ampHHZ[0]//ReplaceAll[#,
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


ampHHZ[2]=ampHHZ[1]//DiracSimplify//TID[#(*/.{p2->P-p1, p1->P-p2}*),q,ToPaVe->True,UsePaVeBasis->True]&;
ampHHZ[2] =  ampHHZ[2]/.{Momentum[P,\[Mu]]->0, Momentum[p1,\[Mu]]->0, Momentum[p2,\[Mu]]->0};

Print["HHZ:"];
(*\:0427\:0422\:043e \:043f\:0440\:043e\:0438\:0441\:0445\:043e\:0434\:0438\:0442 \:043d\:0438\:0436\:0435, \:043f\:043e\:0441\:043b\:0435 \:0443\:043f\:0440\:0430\:0449\:0435\:043d\:0438\:044f \:043f\:0440\:043e\:0432\:043e\:0434\:0438\:0442\:0441\:044f \:0441\:0432\:0435\:0440\:0442\:043a\:0430 \:0438\:043d\:0434\:0435\:043a\:0441\:043e\:0432?*)
f4ZHHZ[s_,mh1_,mh2_,mh3_]:= FullSimplify[ Coefficient[ ampHHZ[2], FCI[FVD[P,\[Beta]] MTD[\[Mu],\[Alpha]]] + FCI[FVD[P,\[Alpha]] MTD[\[Mu],\[Beta]]]]];
FullSimplify[f4ZHHZ[s,mh1,mh2,mh3]]




