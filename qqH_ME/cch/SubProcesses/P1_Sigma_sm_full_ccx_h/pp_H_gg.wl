(* ::Package:: *)

(* ::Title:: *)
(*PP->H->gg*)


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
(*Amplitude of H->gg*)


FCClearScalarProducts;
ScalarProduct[k1,k1]=0;
ScalarProduct[k2,k2]=0;
ScalarProduct[Polarization[k1,-I],k1]=0;
ScalarProduct[Polarization[k2,-I],k2]=0;

(*ScalarProduct[p,p]=x;
ScalarProduct[k1,k2]=x/2;
ScalarProduct[p,k1]=x/2;
ScalarProduct[p,k2]=x/2;*)


(*https://arxiv.org/pdf/1109.5304.pdf*)
Fw[s_]:= (s+6*mW^2 - (6*mW^2)*(s-2*mW^2)* C0[0,0,s,mW^2,mW^2,mW^2])/s; 
Ff[s_,x_]:= (-2)*(4*x^2/s)*(1-s*C0[0,0,s,x^2,x^2,x^2]*(1-4*x^2/s)/2);
F[x_]:= Fw[x]+3*Ff[x,mc];
M[s1_]:= (alpha*g/(4 Pi)mW)F[s1]*(Sqrt[s1/2]*ScalarProduct[Polarization[k1,I],Polarization[k2,I]]-SP[Polarization[k1,I],k2]*SP[Polarization[k2,I],k1])
ampSq[x_]:= (M[x] (ComplexConjugate[M[x]])) //SUNSimplify[#, SUNNToCACF -> False] & // 
        FermionSpinSum[#] & //
        DoPolarizationSums[#, k1, k2] & // DoPolarizationSums[#, k2, k1] & // FeynAmpDenominatorExplicit// Simplify


M[qH^2]//Simplify        
ampSq[qH^2]//Simplify


(* ::Section:: *)
(*Amp qq->H*)



(* \:041e\:043f\:0440\:0435\:0434\:0435\:043b\:0435\:043d\:0438\:0435 \:0444\:0443\:043d\:043a\:0446\:0438\:0438 \:0434\:043b\:044f \:0430\:043f\:043f\:0440\:043e\:043a\:0441\:0438\:043c\:0430\:0446\:0438\:0438 \:0434\:0430\:043d\:043d\:044b\:0445 *)
fitFunction[x_, a_] := a x^2;
(* \:0410\:043f\:043f\:0440\:043e\:043a\:0441\:0438\:043c\:0430\:0446\:0438\:044f \:0434\:0430\:043d\:043d\:044b\:0445 \:0438 \:043d\:0430\:0445\:043e\:0436\:0434\:0435\:043d\:0438\:0435 \:043d\:0430\:0438\:043b\:0443\:0447\:0448\:0438\:0445 \:043f\:0430\:0440\:0430\:043c\:0435\:0442\:0440\:043e\:0432 *)
fit = FindFit[qqhData, fitFunction[x, a], a, x]
(* \:0420\:0430\:0441\:0447\:0435\:0442 \:043e\:0448\:0438\:0431\:043a\:0438 RMSE *)
predictedValues = fitFunction[qqhenergy, a] /. fit;
rmse = Sqrt[Mean[(qqhresult - predictedValues)^2]]

(*\:0410\:043f\:0440\:043e\:043a\:0441\:0438\:043c\:0438\:0440\:043e\:0432\:0430\:043d\:043d\:0430\:044f \:0444\:0443\:043d\:043a\:0446\:0438\:044f*)
(*x -- \:0434\:043e\:043b\:0436\:0435\:043d \:0431\:044b\:0442\:044c x1*x2*s \:0430 \:0441\:0435\:0439\:0447\:0430\:0441 \:044d\:0442\:043e \:044d\:043d\:0435\:0440\:0433\:0438\:044f \:0447\:0430\:0441\:0442\:0438\:0446 \:0434\:0430 \:0435\:0449\:0435 \:0438 \:0432 \:0413\:044d\:0432, \:0430 \:043d\:0435 \:0422\:044d\:0432
s=4E^2 => E= sqrt(s)/2
  *)
qqhGeV[x_]=(fitFunction[x,a]/. fit);
qqh[x_]=qqhGeV[Sqrt[x]/2*1000]/10^6;
(* \:041f\:043e\:0441\:0442\:0440\:043e\:0435\:043d\:0438\:0435 \:0433\:0440\:0430\:0444\:0438\:043a\:0430 *)
Show[ListPlot[qqhData, PlotStyle -> PointSize[Medium]], 
 Plot[qqhGeV[x], {x, Min[qqhenergy], Max[qqhenergy]}, PlotStyle -> Red], 
 PlotLabel -> "\:0410\:043f\:043f\:0440\:043e\:043a\:0441\:0438\:043c\:0430\:0446\:0438\:044f \:0434\:0430\:043d\:043d\:044b\:0445", FrameLabel -> {"GeV", "1/GeV^-2"}]
 
 (**)


(* ::Section:: *)
(*Obtain the PDF*)


(* ::Text:: *)
(*filename: pdf_{id}_{q}*)
(*id: 1 - d, 2 - u, 3 - s, 4 - c, 5 - b, 6 - t*)
(**)
(*here [q] = GeV:*)
(* ## Basic all-flavour PDF querying at x=0.01, Q=M_Z*)
(*    	 print(p.xfxQ(pid, 0.01, 91.2))   here is a reason*)


pdfdata = Import["/home/kds/ZZZ/pdfcode/pdf_all_13000.csv"];
idList={-5,-4,-3,-2,-1,1,2,3, 4,5};
Numbers={2, 3, 4, 5, 6,7,8,9,10,11};
pdfdata = Delete[pdfdata,1];
Tpdfdata= Transpose[pdfdata];
xValues = Tpdfdata[[1]]

PDFALL[x_, id_Integer] := Module[{data, interpolated,index},
  index = Position[idList, id][[1, 1]];
  data = Tpdfdata[[index]]; (* \:041f\:043e\:043b\:0443\:0447\:0430\:0435\:043c \:0441\:043e\:043e\:0442\:0432\:0435\:0442\:0441\:0442\:0432\:0443\:044e\:0449\:0438\:0435 \:0434\:0430\:043d\:043d\:044b\:0435 \:0434\:043b\:044f id *)
  interpolated = Interpolation[Transpose[{xValues, data}]]; (* \:0421\:043e\:0437\:0434\:0430\:0435\:043c \:0438\:043d\:0442\:0435\:0440\:043f\:043e\:043b\:0438\:0440\:043e\:0432\:0430\:043d\:043d\:0443\:044e \:0444\:0443\:043d\:043a\:0446\:0438\:044e *)
  Return[interpolated[x]]; (* \:0412\:043e\:0437\:0432\:0440\:0430\:0449\:0430\:0435\:043c \:0437\:043d\:0430\:0447\:0435\:043d\:0438\:0435 \:0438\:043d\:0442\:0435\:0440\:043f\:043e\:043b\:0438\:0440\:043e\:0432\:0430\:043d\:043d\:043e\:0439 \:0444\:0443\:043d\:043a\:0446\:0438\:0438 \:0434\:043b\:044f \:0437\:0430\:0434\:0430\:043d\:043d\:043e\:0433\:043e x *)
]
PDFALL[0.1,3]


(* ::Section:: *)
(*Fix the kinematics*)


(*
p1+p2 ->k1 + j2
dw = A^2 / 2sqrt(s) d3k1 d3k2 /(2E1(2pi)^3 2E2(2pi)^3) 2pi delta(p1+p2-k1-k2)
m1=m2=0: w = (A^2 /2sqrtS) 1/8pi = A^2 / 16pi sqrtS
\:041f\:043e\:0442\:043e\:043c \:0441\:044e\:0434\:0430 \:043d\:0430\:043a\:0440\:0443\:0447\:0438\:0432\:0430\:0435\:043c PDF\:043a\:0443
*)
phaseSpacePrefactor[s_] := 1/(16 Pi Sqrt[s]);



(*
s=4E^2
(E,0,0,E)  (E,0,0,-E)
x1,x2 part transversed energy
(x1E,0,0,x1E)  (x2E,0,0,-x2E)
s1=(p1+p2)^2=4x1x2E^2=x1x2s
mH^2=s1 -- virtual Higgs boson
*)
s=13^2;
mc=0.00127;
mW = 0.080;
mt = 0.173;
alpha = 1/137;
e = Sqrt[alpha/(4 Pi)];
sinW = Sqrt[0.222];
g=e/sinW;
Sigma[x1_,id1_,x2_,id2_]:=phaseSpacePrefactor[x1*x2*s]*PDFALL[x1,id1]*PDFALL[x2,id2]*qqh[x1*x2*s]*ampSq[x1*x2*s]




x1 = xValues;
x2 = xValues
 integral = 0
   Do[
    Do[
     integral += Sigma[(x1[[i + 1]] - x1[[i]])/2,3, (x2[[j + 1]] - x2[[j]])/2,-3] * (x1[[i + 1]] - x1[[i]]) * (x2[[j + 1]] - x2[[j]]); 
     , {j, Length[x2] - 1}]
    , {i, Length[x1] - 1}];
    
    integral
Print["We work in TeV system, so this must be 1/TeV^2"]


(* ::Section:: *)
(*Const*)


(* ::Section:: *)
(*Save func to file*)


(* ::Section:: *)
(*Import example*)


(*example = Import["ZZZ/F1Z.txt"]
args = {s,mh1,mh2,mh3,pref};
Activate[Inactive[SetDelayed][ToExpression["F1Z"]@@(Pattern[#,_]&/@args),ToExpression[str]]]
F1Z[s,mh1,mh2,mh3,pref]
F1Z[0.5^2,0.25,0.4,0.5,1]
*)
