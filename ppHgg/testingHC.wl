(* ::Package:: *)

$LoadAddOns={"FeynArts"};
<<FeynCalc`
$FAVerbose = 0;

FCCheckVersion[9,3,0];
$FeynRulesPath = SetDirectory["~/.Mathematica/Applications/FeynCalc/AddOns/FeynRules"];
<<FeynRules`
AppendTo[$ModelPath, "/home/kds/.Mathematica/Applications/FeynArts/Models/"];
$ModelPath


SetDirectory["/home/kds/.Mathematica/Applications/FeynCalc/AddOns/FeynRules/Models/HC"]
LoadModel["SM_HC.fr","HC.fr"]
LoadRestriction["Cabibbo.rst", "Massless.rst"];


vertHC = FeynmanRules[ LagHC,Contains->{b},MaxParticles->3]
vertX0AA = FeynmanRules[ LagHC,SelectParticles->{{X0,A,A}}];


AbbX = vertHC[[1,2]]
AaaX = vertX0AA[[1,2]]

Amp[0] = AbbX*AaaX*1/(q^2-MX0^2+ I MX0 WX0);


(*Amp[1] = Amp[0]//ReplaceAll[#,
	Pair[Momentum[Polarization[___],___],___]:>1]&//Contract
FCClearScalarProducts[];
ScalarProduct[p1,p1]=0;
ScalarProduct[p2,p2]=0;
ScalarProduct[p1,p2]=s;
Print["Obtein amplitude for Hgg"]
(*ampd1[2]=ampd1[1]//DiracSimplify//TID[#(*/.{k2->p1+p2-k1, k1->p1+p2-k2}*),q,ToPaVe->True,UsePaVeBasis->True]&;
ampd1[2] = ampd1[1] // TID[#, q, ToPaVe -> True]/.D->4 &//Simplify
*)
Amp[2] = Amp[1]//DiracSimplify//TID[#(*/.{p2->q-p1, p1->q-p2}*),q,ToPaVe->True,UsePaVeBasis->True]&
(*Amp[2] =  Amp[2]/.{Momentum[q,\[Mu]]->0, Momentum[p1,\[Mu]]->0, Momentum[p2,\[Mu]]->0};
Print[Amp[2]]*)*)


$ModelPath


(*WriteFeynArtsOutput[ LagHC]*)



(*\:0412\:044b\:0447\:0438\:0441\:043b\:0435\:043d\:0438\:0435 \:0448\:0438\:0440\:0438\:043d*)
Print["\:0428\:0438\:0440\:0438\:043d\:0430"]
Waa = ComputeWidths[vertX0AA][[1,2]] // Simplify
Wbb = ComputeWidths[vertHC][[1,2]] // Simplify


Params = {MX0->125,aEWM1->127.9`,aS->0.1184`,ca->1,cabi->0.227736`,Gf->0.0000116637`,ka->1,kAaa->1,kAAgg->1,kAbb->1,kAgg->1,kAll->1,kAtt->1,kAww->0,kAza->1,kAzz->0,kg->1,kHaa->1,kHbb->1,kHda->0,kHdwI->0,kHdwR->0,kHdz->0,kHgg->1,kHHgg->1,kHll->1,kHtt->1,kHww->0,kHza->1,kHzz->0,kl->1,kla->1,klb->1,kq->1,kq3->1,kqa->1,kqb->1,kSM->1,kw->1,kw1->1,kw2->1,kw3->0,kw4->0,kw5->0,kz->1,kz1->0,kz3->1,kz5->0,kza->0,Lambda->1000,ymb->4.7`,ymt->172,ymtau->1.777`,CKM1x1->0.974180040319821`,CKM1x2->0.2257725604285693`,CKM2x1->-0.2257725604285693`,CKM2x2->0.974180040319821`,sa->0.`,aEW->0.007818608287724784`,G->1.219777963704922`,kHdw->0.` +0.` I,MW->79.82435974619784`,ee->0.31345100004952897`,sw2->0.23369913342182447`,cw->0.8753861242778386`,sw->0.483424382320363`,ad->-0.12747442148678403`,al->-0.01207402701388773`,an->0.18517461872323218`,au->0.06977422425033589`,bd->-0.18517461872323218`,bl->-0.18517461872323218`,bn->0.18517461872323218`,bu->0.18517461872323218`,gwwz->-0.5675978831835842`,g1->0.35807170271074895`,gw->0.6483971671950268`,vev->246.22056907348593`,gAaa->0.000013477023594718654`,gAAgg->3.108301747361443`*^-7,gAgg->0.00007653278250874452`,gAza->4.499980218804759`*^-6,gHaa->0.00002639250453965737`,gHgg->-0.00005102185500582968`,gHHgg->2.072201164907629`*^-7,gHza->0.000039166441035016915`,lam->0.1288668963082114`,yb->0.02699532280412272`,yt->0.9879139409168314`,ytau->0.010206529494239587`,muH->88.38834764831844`}
Waa/.Params
Wbb/.Params


(* ::Text:: *)
(*\[CapitalGamma] = [\[Lambda] (m^2, m^2 _ 1, m^2 _ 2)]^(1/2) | M | ^2/(16 \[Pi] S | m | ^3)*)


Maa = Waa *(16 Pi Abs[MX0]^3)/MX0^2
Mbb = Wbb *(16 Pi Abs[MX0]^3)/Sqrt[MX0^4-4 MB^2*MX0^2]


Amp2= Maa*Mbb*1/(WX0^2 MX0^2)
