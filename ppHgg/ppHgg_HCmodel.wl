(* ::Package:: *)

(* ::Title:: *)
(*ppHgg in HC*)


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
(*Test vertex*)


Print["Try 1"]
d0 = InsertFields[CreateTopologies[0, 1 -> 2],
{S[3]} -> {V[1], V[1]},Model->HC,InsertionLevel->{Particles}];
$Model
(*b F12, s3 X0, v1 gamma*)
Paint[d0 , ColumnsXRows -> {10, 1}, Numbering -> Simple,SheetHeader->None,ImageSize->{800,90}];


Print["Try 2"]
topologies = CreateTopologies[0, 2 -> 2];
diagrams = InsertFields[topologies, {F[12],-F[12]} -> {V[1], V[1]}, LastSelections->{S[3]}, 
Model -> HC, GenericModel->"HC", 
InsertionLevel->{Particles}];
Paint[diagrams, ColumnsXRows -> {1, 1}, Numbering -> Simple, SheetHeader -> None, ImageSize -> {800, 90}]
(*Print[M$ClassesDescription]*)
 (* Print[M$CouplingMatrices]
*)


(*ampHHZ[2]=ampHHZ[1]//DiracSimplify//TID[#(*/.{p2->P-p1, p1->P-p2}*),q,ToPaVe->True,UsePaVeBasis->True]&;
ampHHZ[2] =  ampHHZ[2]/.{Momentum[P,\[Mu]]->0, Momentum[p1,\[Mu]]->0, Momentum[p2,\[Mu]]->0};*)


(*diagsHHH = InsertFields[CreateTopologies[1, 1 -> 2,ExcludeTopologies->{Tadpoles, Internal, V4onExt}], {V[2]} -> {V[2], V[2]}, InsertionLevel -> {Classes},
Model -> THDMCPV, ExcludeParticles -> {F[_], V[1], V[3], S[2], S[3], U[_]}, LastSelections -> {S[4], S[5], S[6]}];
(*$Model
Print[M$ClassesDescription]*)
Paint[diagsHHH , ColumnsXRows -> {10, 1}, Numbering -> Simple,SheetHeader->None,ImageSize->{800,90}];*)


(* ::Section:: *)
(*Obtain the amplitude*)


Print["Amplitude creating for bb-x-aa"]
amp[0] = FCFAConvert[CreateFeynAmp[diagrams], 
        IncomingMomenta -> {p1, p2}, OutgoingMomenta -> {k1, k2},
        LorentzIndexNames->{\[Mu],\[Nu],\[Alpha],\[Beta]}, ChangeDimension -> 4,
        List -> False, SMP->True,Contract->True,DropSumOver->True,
        TransversePolarizationVectors -> {k1, k2}, 
        UndoChiralSplittings->True
        ]


FCClearScalarProducts[]
SetMandelstam[s, t, u, p1, p2, -k1, -k2 , SMP["m_B"],SMP["m_B"], 0, 0];
amp[1]=amp[0]//DiracSimplify


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


(*ScalarProduct[k1,k1]=0;
ScalarProduct[k2,k2]=0;
We average over the spins and the colors of the quarks 1/3^2
Since the final state particles are indistinguishable, we add an extra 1/2.
*)
Print["Squared amplitude"]
ScalarProduct[k1,k1]=0;
ScalarProduct[k2,k2]=0;
ampSquared =(1/2)(1/3)^2 (amp[1] (ComplexConjugate[amp[1]]))//
 FeynAmpDenominatorExplicit // DiracSimplify//
 FermionSpinSum[#, ExtraFactor -> 1/2^2] & // DiracSimplify//
 DoPolarizationSums[#, k1,0] & // DoPolarizationSums[#, k2,0] & //
    SUNSimplify[#, SUNNToCACF ->False] & // TrickMandelstam[#, {s, t, u, 2 SMP["m_B"]^2}] & //Simplify
   





str=Abs[ampSq1]
Print["Before changes:"]
str=ToString[str, InputForm]

str=StringReplace[str, {"MQU[Index[Generation, 4]]" -> "mt"}];
str=StringReplace[str, {"SMP[\"m_W\"]" -> "mW"}];
str=StringReplace[str, {"SMP[\"sin_W\"]" -> "sinW"}];
str=StringReplace[str, {"SMP[\"m_H\"]" -> "mH"}];
str=StringReplace[str, {"SMP[\"m_c\"]" -> "MC"}];
str=StringReplace[str, {"SMP[\"e\"]" -> "e"}];
str=StringReplace[str, {"SMP[\"m_t\"]" -> "mt"}];
str=StringReplace[str, {"SMP[\"m_u\"]" -> "mu"}];
str=StringReplace[str, {"SUNN" -> "Norma"}];
str=StringReplace[str, {"B0[" -> "B0["}];
str=StringReplace[str, {"C0[" -> "C0[ "}];
str=StringReplace[str, {"PaVe[0, 0," -> "C0i[cc00, "}];
str=StringReplace[str, {"FeynCalc`" -> "LoopTools`"}];
str=StringReplace[str, {", PaVeAutoOrder -> True, PaVeAutoReduce -> True" -> "", "{" -> "", "}" -> ""}];
Print["After changes:"]
args={s1,MC};
str1=str
Activate[Inactive[SetDelayed][ToExpression["F1Z"]@@(Pattern[#,_]&/@args),ToExpression[str1]]]
F1Z[s1,MC]


(* ::Section:: *)
(*Fix the kinematics*)


(*
p1+p2 ->k1 + j2
dw = A^2 / 2sqrt(s) d3k1 d3k2 /(2E1(2pi)^3 2E2(2pi)^3) 2pi delta(p1+p2-k1-k2)
m1=m2=0: w = (A^2 /2sqrtS) 1/8pi = A^2 / 16pi sqrtS
\:041f\:043e\:0442\:043e\:043c \:0441\:044e\:0434\:0430 \:043d\:0430\:043a\:0440\:0443\:0447\:0438\:0432\:0430\:0435\:043c PDF\:043a\:0443
*)


phaseSpacePrefactor[s_] := 1/(16 Pi );
s=13^2;
sigma[x1_,x2_,id_,mq_] :=1/(2*s*x1) * 1/(2*s*x2) phaseSpacePrefactor[x1*x2*s] * PDF[x1,id]*PDF[x2,-id]* F1Z[x1*x2*s,mq];


(* ::Section:: *)
(*with masses*)


R=1


mc = 0.00127;
mb = 0.0042;
mt = 0.173;
mW = 0.080;

alpha = 1/137;
e = Sqrt[alpha/(4 Pi)];
g=e/sinW;

mH= R*0.125;
mt = R*0.175;
mW = R*0.08;
mZ = R*0.09;
mu = R*0.15;
sinW = Sqrt[1-mW*mW/(mZ*mZ)];
e=1; Norma=1;
F1Z[s,mc];
data = Table[{x, F1Z[x, mc]}, {x, 0.001, 10, 0.001}];
ListLogLogPlot[data, Joined -> True, PlotStyle -> Blue, AxesLabel -> {"x", "F1Z"}, 
 PlotTheme -> "Scientific"]



(* ::Section:: *)
(*Save func to file*)


Export["/sci/zzz/ppHgg.txt",str,"Text"]


(* ::Section:: *)
(*Import example*)


(*example = Import["/sci/zzz/ppHgg.txt"]
args = {s,mh1,mh2,mh3,pref};
Activate[Inactive[SetDelayed][ToExpression["F1Z"]@@(Pattern[#,_]&/@args),ToExpression[str]]]
F1Z[s,mh1,mh2,mh3,pref]
F1Z[0.5^2,0.25,0.4,0.5,1]
*)
