#!/usr/bin/env wolframscript
(* ::Package:: *)

(* \:041e\:043f\:0446\:0438\:0438 \:0440\:0430\:0431\:043e\:0442\:044b *)
logic = 0; (* 0 - \:043f\:0435\:0440\:0435\:0441\:0447\:0438\:0442\:0430\:0442\:044c \:0432\:0441\:0451, 1 - \:0437\:0430\:0433\:0440\:0443\:0437\:0438\:0442\:044c \:0431\:0443\:0444\:0435\:0440\:044b, 2 - \:043f\:0435\:0440\:0435\:0441\:0447\:0438\:0442\:0430\:0442\:044c \:0442\:043e\:043b\:044c\:043a\:043e \:0441\:0435\:0447\:0435\:043d\:0438\:0435, 3 - \:043f\:0435\:0440\:0435\:0441\:0447\:0438\:0442\:0430\:0442\:044c \:0442\:043e\:043b\:044c\:043a\:043e \:043a\:0432\:0430\:0434\:0440\:0430\:0442 \:043c\:0430\:0442\:0440\:0438\:0447\:043d\:043e\:0433\:043e \:044d\:043b\:0435\:043c\:0435\:043d\:0442\:0430 *)

(* \:0427\:0442\:0435\:043d\:0438\:0435 \:0430\:0440\:0433\:0443\:043c\:0435\:043d\:0442\:043e\:0432 \:043a\:043e\:043c\:0430\:043d\:0434\:043d\:043e\:0439 \:0441\:0442\:0440\:043e\:043a\:0438 *)
If[Length[$ScriptCommandLine] > 1,
    logic = ToExpression[$ScriptCommandLine[[2]]];
];
If[ $FrontEnd === Null, 
    $FeynCalcStartupMessages = False; 
    $LoadFeynCalcMessages = False; 
    Print["tt>Z>ZZ calculation in 2HDM CPV, 1-loop"]; 
  ];
If[ $Notebooks === False, 
	$FeynCalcStartupMessages = False;
	$LoadFeynCalcMessages = False; 
	Off[General::shdw];
    ,
    SetDirectory[NotebookDirectory[]]
  ];


(* \:041f\:043e\:0434\:043a\:043b\:044e\:0447\:0435\:043d\:0438\:0435 \:043d\:0435\:043e\:0431\:0445\:043e\:0434\:0438\:043c\:044b\:0445 \:043f\:0430\:043a\:0435\:0442\:043e\:0432 *)
$LoadAddOns = {"FeynArts"};
$PrePrint = InputForm;

Quiet[<<FeynCalc`] 
AppendTo[$ModelPath, "/home/kds/.Mathematica/Applications/FeynArts/Models/"];
Quiet[Install["LoopTools"]];
Quiet[Needs["LoopTools`"]];
$FAVerbose = 0; 

Unprotect[PaVe, Li2, A0, A00, B0, B1, B00, B11, DB0, DB1, C0, D0];
Remove[PaVe, Li2, A0, A00, B0, B1, B00, B11, DB0, DB1, C0, D0];



(* \:0424\:0430\:0439\:043b\:044b \:0434\:043b\:044f \:0431\:0443\:0444\:0435\:0440\:043e\:0432 *)

ampFile = "ampZZZ.mx";

meFile = "matrixElementZZZ.mx";

csFile = "crossSectionZZZ.mx";




(*Diags*)
$ExcludeTopologies[ V4onExt ]=FreeQ[ Cases[#, Propagator[External][__]], Vertex[4] ]&;
	$ExcludeTopologies[ La ]=FreeQ[ Cases[#, Propagator[Incoming][__]], Vertex[3] ]&;
	diags = InsertFields[
				CreateTopologies[1, 2 -> 2,ExcludeTopologies->{Tadpoles}],
				{F[9], -F[9]} -> {V[2],V[2]}, InsertionLevel -> {Classes},
				Model -> THDMCPV, ExcludeParticles -> {F[_], V[1], V[3], S[2], S[3], U[_]}, LastSelections -> {V[2]}];
diagsAll = DiagramExtract[diags, {100..141}];

Print["Diagrams:"]
diagHHH = DiagramExtract[diagsAll, {19..24}];
diagHHG = DiagramExtract[diagsAll, {1..18}];
diagHHZ = DiagramExtract[diagsAll, {25..42}];

If[$Notebooks,
   Print["Diagrams HHH:"];
   Paint[diagHHH, ColumnsXRows -> {6, 1}, Numbering -> Simple, SheetHeader -> None, ImageSize -> {600, 100}];

   Print["Diagrams HHG:"];
   Paint[diagHHG, ColumnsXRows -> {6, 3}, Numbering -> Simple, SheetHeader -> None, ImageSize -> {600, 300}];

   Print["Diagrams HHZ:"];
   Paint[diagHHZ, ColumnsXRows -> {6, 3}, Numbering -> Simple, SheetHeader -> None, ImageSize -> {600, 300}];
];





(* \:0420\:0430\:0441\:0447\:0435\:0442 \:0430\:043c\:043f\:043b\:0438\:0442\:0443\:0434\:044b *)
If[logic >= 1 && FileExistsQ[ampFile],
    Print["Download amp from buffer"];
    amp = Get[ampFile];
    ,
    Print["Calculation of amplitude.","\tCPU Time used: ", Round[N[TimeUsed[]], 0.001], " s."];
    ampHHH = FCFAConvert[CreateFeynAmp[diagHHH], IncomingMomenta -> {p1,p2}, 
    OutgoingMomenta -> {k1,k2}, LoopMomenta->{Q}, List -> False, ChangeDimension -> D, 
    DropSumOver -> True, SMP -> False, Contract -> True, UndoChiralSplittings -> True];
    ampHHG = FCFAConvert[CreateFeynAmp[diagHHG], IncomingMomenta -> {p1,p2}, 
    OutgoingMomenta -> {k1,k2}, LoopMomenta->{Q}, List -> False, ChangeDimension -> D, 
    DropSumOver -> True, SMP -> False, Contract -> True, UndoChiralSplittings -> True];
    ampHHZ = FCFAConvert[CreateFeynAmp[diagHHZ], IncomingMomenta -> {p1,p2}, 
    OutgoingMomenta -> {k1,k2}, LoopMomenta->{Q}, List -> False, ChangeDimension -> D, 
    DropSumOver -> True, SMP -> False, Contract -> True, UndoChiralSplittings -> True];
    
    ampHHH=ampHHH/.M$FACouplings;
	ampHHG=ampHHG/.M$FACouplings;
	ampHHZ=ampHHZ/.M$FACouplings;
	
	(*\:041a\:0438\:043d\:0435\:043c\:0430\:0442\:0438\:043a\:0430*)
	FCClearScalarProducts[];
SP[p1, p1] = SMP["m_t"]^2;
SP[p2, p2] = SMP["m_t"]^2;
SP[p1, p2] = (s - 2 SMP["m_t"]^2)/2;
(*s=(p1+p2)^2 *)
SP[k1, k1] = SMP["m_Z"]^2;
SP[k2, k2] = SMP["m_Z"]^2;
SP[k1, k2] = (s - 2 SMP["m_Z"]^2)/2;
SP[k1 + k2, k1 + k2] = s;
SP[K0,K0] = s;
(*u=(p2-k1)^2 t=(p2-k2)^2*)
SP[p2, k1] = (SMP["m_Z"]^2 + SMP["m_t"]^2-u)/2;
SP[p2, k2] = (SMP["m_Z"]^2 + SMP["m_t"]^2-t)/2;
(*t=(p1-k1)^2 u=(p1-k2)^2*)
SP[p1, k1] = (SMP["m_Z"]^2 + SMP["m_t"]^2-t)/2;
SP[p1, k2] = (SMP["m_Z"]^2 + SMP["m_t"]^2-u)/2;
(*mass^2 sum=s+t+u*)
u = 2*(SMP["m_Z"]^2 + SMP["m_t"]^2)-s-t;
	Print["PV-reduction in process.","\tCPU Time used: ", Round[N[TimeUsed[]], 0.001], " s."];
	ampHHZ = TID[ampHHZ, Q, ToPaVe->True,UsePaVeBasis->True ]//ChangeDimension[#,4] &;		
    ampHHG = TID[ampHHG, Q, ToPaVe->True,UsePaVeBasis->True ]//ChangeDimension[#,4] &;		
    ampHHH = TID[ampHHH, Q, ToPaVe->True,UsePaVeBasis->True ]//ChangeDimension[#,4] &;		
    
    ampHHH = ampHHH//Simplify;
    amp = ampHHH+ampHHZ+ampHHG;
    Put[amp,ampFile];
    Print["PV-reduction done. Amplitude saved in buffer. ","\tCPU Time used: ", Round[N[TimeUsed[]], 0.001], " s."];
];
If[$Notebooks,
Print[amp]];



(*\:0412\:044b\:0431\:0438\:0440\:0430\:0435\:043c \:0441\:043b\:0430\:0433\:0430\:0435\:043c\:044b\:0435 \:0441\:043e\:043e\:0442\:0432\:0435\:0442\:0441\:0442\:0432\:0443\:044e\:0449\:0438\:0435 \:0432\:0435\:0440\:0448\:0438\:043d\:043d\:043e\:0439 \:0444\:0443\:043d\:043a\:0446\:0438\:0438 f4_Z*)
Denom = SequenceForm[FeynCalc`Pair[FeynCalc`Momentum[k1, 4] + FeynCalc`Momentum[k2, 4], FeynCalc`Momentum[k1, 4] + FeynCalc`Momentum[k2, 4]], "-",MZ^2];
expr = amp;
expr = ExpandAll[expr];
terms = List @@ expr;
C001terms = Select[terms, 
 !FreeQ[#, FeynCalc`PaVe[0,0,1, {__, __, __}, {__, __, __}, 
      PaVeAutoOrder -> True, PaVeAutoReduce -> True]] &
];
C1terms = Select[terms, 
 !FreeQ[#, FeynCalc`PaVe[1, {__, __, __}, {__, __, __}, 
      PaVeAutoOrder -> True, PaVeAutoReduce -> True]] &
];
Print["Get terms from amp:\tGood/All:  ",Length[C001terms],"/",Length[terms],
"\nCPU Time used: ", Round[N[TimeUsed[]], 0.001], " s."
];

SubjAmp = Total[C001terms] //Simplify;
If[$Notebooks,
Print[FullForm[SubjAmp]]];


(* \:0417\:0430\:0433\:0440\:0443\:0437\:043a\:0430 \:0438\:043b\:0438 \:0440\:0430\:0441\:0447\:0451\:0442 \:043a\:0432\:0430\:0434\:0440\:0430\:0442\:0430 \:043c\:0430\:0442\:0440\:0438\:0447\:043d\:043e\:0433\:043e \:044d\:043b\:0435\:043c\:0435\:043d\:0442\:0430 *)

If[logic >= 1 && logic != 3 && FileExistsQ[meFile],
    Print["Loading ampHHH from file."];
    ampSquared =  Get[meFile];
    ,
    Print["Recomputing ampHHH and saving to file.","\tCPU Time used: ", Round[N[TimeUsed[]], 0.001], " s."];
        ampSquared = 1/2 (SubjAmp (ComplexConjugate[SubjAmp])) //  
		DiracSimplify // 
        DoPolarizationSums[#, k1, NumberOfPolarizations -> 3] & // 
        DoPolarizationSums[#, k2, NumberOfPolarizations -> 3] & //
        FermionSpinSum // DiracSimplify // SUNSimplify;
        ampSquared = ampSquared/.{FeynAmpDenominator[PropagatorDenominator[Plus[Momentum[k1], Momentum[k2]], MZ]] -> 1/(s - SMP["m_Z"]^2)}// Simplify;
    Put[ampSquared,meFile];
    Print["Matrix element saved in buffer.","\tCPU Time used: ", Round[N[TimeUsed[]], 0.001], " s."];
];


If[$Notebooks,
Print[ampSquared]];


(*(* \:0417\:0430\:0433\:0440\:0443\:0437\:043a\:0430 \:0438\:043b\:0438 \:0440\:0430\:0441\:0447\:0451\:0442 \:0441\:0435\:0447\:0435\:043d\:0438\:044f *)
If[logic >= 1 && logic != 2 && FileExistsQ[csFile],
    Print["\:0417\:0430\:0433\:0440\:0443\:0437\:043a\:0430 \:0441\:0435\:0447\:0435\:043d\:0438\:044f \:0438\:0437 \:0431\:0443\:0444\:0435\:0440\:0430..."];
    crossSection = Get[csFile];
    ,
    Print["\:0412\:044b\:043f\:043e\:043b\:043d\:0435\:043d\:0438\:0435 \:0440\:0430\:0441\:0447\:0451\:0442\:0430 \:0441\:0435\:0447\:0435\:043d\:0438\:044f..."];
    crossSection =
        Module[{phaseSpace},    Save[, ];
            phaseSpace = PhaseSpaceIntegrate[ampSquared, {s, mZ, mZ, mZ
                }];
            Simplify[phaseSpace]
        ];
    Put[crossSection,csFile];
    Print["CrossSection saved in buffer.","\tCPU Time used: ", Round[N[TimeUsed[]], 0.001], " s."];
];
*)


(* \:0418\:0442\:043e\:0433\:043e\:0432\:044b\:0439 \:0432\:044b\:0432\:043e\:0434 *)

Print["\:041a\:0432\:0430\:0434\:0440\:0430\:0442 \:043c\:0430\:0442\:0440\:0438\:0447\:043d\:043e\:0433\:043e \:044d\:043b\:0435\:043c\:0435\:043d\:0442\:0430: ", ampSquared//Simplify];

(*Print["\:0421\:0435\:0447\:0435\:043d\:0438\:0435 \:043f\:0440\:043e\:0446\:0435\:0441\:0441\:0430: ", crossSection];*)





