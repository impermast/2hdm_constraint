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
LogRange[a_, b_, n_] := Exp[Range[Log[a], Log[b], (Log[b] - Log[a])/(n - 1)]]//N

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
(*
ScalarProduct[p,p]=s;
ScalarProduct[k1,k2]=s/2;
ScalarProduct[p,k1]=s/2;
ScalarProduct[p,k2]=s/2;*)


(*https://arxiv.org/pdf/1109.5304.pdf*)
Fw[s_]:= (s+6*mW^2 - (6*mW^2)*(s-2*mW^2)* C0[0,0,s,mW^2,mW^2,mW^2])/s; 
Ff[s_,x_]:= (-2)*(4*x^2/s)*(1-s*C0[0,0,s,x^2,x^2,x^2]*(1-4*x^2/s)/2);
F[x_,y_]:= Fw[x]+3*Ff[x,y];
M[s1_,y_]:= (e^2*g/(4 Pi)^2 / mW)F[s1,y]*(s1/2*ScalarProduct[Polarization[k1,I],Polarization[k2,I]]-SP[Polarization[k1,I],k2]*SP[Polarization[k2,I],k1])
ampSq1[x_,y_]:= (M[x,y] (ComplexConjugate[M[x,y]])) //SUNSimplify[#, SUNNToCACF -> False] & // 
        FermionSpinSum[#] & //
        DoPolarizationSums[#, k1, k2] & // DoPolarizationSums[#, k2, k1] & // FeynAmpDenominatorExplicit//Simplify       
        
ampSq[x_,y_]:= Abs[ampSq1[x,y]]/.SP[p,p]->(x)/. SP[p,k1]->(x/2)/. SP[p,k2]->(x/2)/. SP[k1,k2]->(x/2)//Simplify


ampSq[s,m]


(* ::Section:: *)
(*Amp qq->H*)


qqhData = Import["/home/kds/ZZZ/pdfcode/qqh.txt", "Table"];
qqhenergy = qqhData[[All, 1]];
qqhresult = qqhData[[All, 2]];

(* \:0418\:043d\:0442\:0435\:0440\:043f\:043e\:043b\:044f\:0446\:0438\:044f \:0434\:0430\:043d\:043d\:044b\:0445 *)
fitFunction = Interpolation[qqhData];

(* \:041f\:043e\:0441\:0442\:0440\:043e\:0435\:043d\:0438\:0435 \:0433\:0440\:0430\:0444\:0438\:043a\:0430 \:0441 \:043b\:043e\:0433\:0430\:0440\:0438\:0444\:043c\:0438\:0447\:0435\:0441\:043a\:0438\:043c\:0438 \:0448\:043a\:0430\:043b\:0430\:043c\:0438 *)
Show[ListLogLogPlot[qqhData, PlotStyle -> PointSize[Medium], Joined -> False], 
 LogLogPlot[fitFunction[x], {x, Min[qqhenergy], Max[qqhenergy]}, PlotStyle -> Red], 
 PlotLabel -> "\:0418\:043d\:0442\:0435\:0440\:043f\:043e\:043b\:044f\:0446\:0438\:044f \:0434\:0430\:043d\:043d\:044b\:0445", FrameLabel -> {"GeV", "1/GeV^2"}]

(* GeV^2 = 10^-6 tev^2
x -- \:0434\:043e\:043b\:0436\:0435\:043d \:0431\:044b\:0442\:044c x1*x2*s \:0430 \:0441\:0435\:0439\:0447\:0430\:0441 \:044d\:0442\:043e \:044d\:043d\:0435\:0440\:0433\:0438\:044f \:0447\:0430\:0441\:0442\:0438\:0446 \:0434\:0430 \:0435\:0449\:0435 \:0438 \:0432 \:0413\:044d\:0432, \:0430 \:043d\:0435 \:0422\:044d\:0432
1/gev = 1000/tev => 1/gev^2 = 10^6/tev^2
s=4E^2 => E= sqrt(s)/2
*)
cch[x_]=fitFunction[x];


tthData = Import["/home/kds/ZZZ/pdfcode/tth.txt", "Table"];
tthenergy = tthData[[All, 1]];
tthresult = tthData[[All, 2]];

(* \:0418\:043d\:0442\:0435\:0440\:043f\:043e\:043b\:044f\:0446\:0438\:044f \:0434\:0430\:043d\:043d\:044b\:0445 *)
fitFunction = Interpolation[tthData];

(* \:041f\:043e\:0441\:0442\:0440\:043e\:0435\:043d\:0438\:0435 \:0433\:0440\:0430\:0444\:0438\:043a\:0430 \:0441 \:043b\:043e\:0433\:0430\:0440\:0438\:0444\:043c\:0438\:0447\:0435\:0441\:043a\:0438\:043c\:0438 \:0448\:043a\:0430\:043b\:0430\:043c\:0438 *)
Show[ListLogLogPlot[tthData, PlotStyle -> PointSize[Medium], Joined -> False], 
 LogLogPlot[fitFunction[x], {x, Min[tthenergy], Max[tthenergy]}, PlotStyle -> Red], 
 PlotLabel -> "\:0418\:043d\:0442\:0435\:0440\:043f\:043e\:043b\:044f\:0446\:0438\:044f \:0434\:0430\:043d\:043d\:044b\:0445", FrameLabel -> {"GeV", "GeV^2"}]

(*
GeV^2 = M^2 -> tev^2
x -- \:0434\:043e\:043b\:0436\:0435\:043d \:0431\:044b\:0442\:044c x1*x2*s \:0430 \:0441\:0435\:0439\:0447\:0430\:0441 \:044d\:0442\:043e \:044d\:043d\:0435\:0440\:0433\:0438\:044f \:0447\:0430\:0441\:0442\:0438\:0446 \:0434\:0430 \:0435\:0449\:0435 \:0438 \:0432 \:0413\:044d\:0432, \:0430 \:043d\:0435 \:0422\:044d\:0432
1000gev = tev => GeV^2 = 10^-6 tev^2
s=4E^2 => E= sqrt(s)/2
*)
tth[x_]=fitFunction[x];


bbhData = Import["/home/kds/ZZZ/pdfcode/bbh.txt", "Table"];
bbhenergy = bbhData[[All, 1]];
bbhresult = bbhData[[All, 2]];

(* \:0418\:043d\:0442\:0435\:0440\:043f\:043e\:043b\:044f\:0446\:0438\:044f \:0434\:0430\:043d\:043d\:044b\:0445 *)
fitFunction = Interpolation[bbhData]

(* \:041f\:043e\:0441\:0442\:0440\:043e\:0435\:043d\:0438\:0435 \:0433\:0440\:0430\:0444\:0438\:043a\:0430 \:0441 \:043b\:043e\:0433\:0430\:0440\:0438\:0444\:043c\:0438\:0447\:0435\:0441\:043a\:0438\:043c\:0438 \:0448\:043a\:0430\:043b\:0430\:043c\:0438 *)
Show[ListLogLogPlot[bbhData, PlotStyle -> PointSize[Medium], Joined -> False], 
 LogLogPlot[fitFunction[x], {x, Min[bbhenergy], Max[bbhenergy]}, PlotStyle -> Red], 
 PlotLabel -> "\:0418\:043d\:0442\:0435\:0440\:043f\:043e\:043b\:044f\:0446\:0438\:044f bbh", AxesLabel -> {"GeV", "GeV^2"}]

(*
GeV^2 = M^2 -> tev^2
x -- \:0434\:043e\:043b\:0436\:0435\:043d \:0431\:044b\:0442\:044c x1*x2*s \:0430 \:0441\:0435\:0439\:0447\:0430\:0441 \:044d\:0442\:043e \:044d\:043d\:0435\:0440\:0433\:0438\:044f \:0447\:0430\:0441\:0442\:0438\:0446 \:0434\:0430 \:0435\:0449\:0435 \:0438 \:0432 \:0413\:044d\:0432, \:0430 \:043d\:0435 \:0422\:044d\:0432
1000gev = tev => GeV^2 = 10^-6 tev^2
s=4E^2 => E= sqrt(s)/2
*)
bbh[x_]=fitFunction[x];




qqhGeV[x_, id_] := Which[
    id == 4, cch[x],
    id == 5, bbh[x],
    id == 6, tth[x],
    True, "Invalid id"
]

qqh[100,6]
tth[10000]


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

xValues = Tpdfdata[[1]];


PDFALL[x_, id_Integer] := Module[{data, interpolated, index},
  index = Position[idList, id];
  If[Length[index] == 0, (* \:0415\:0441\:043b\:0438 \:0438\:043d\:0434\:0435\:043a\:0441 \:043d\:0435 \:043d\:0430\:0439\:0434\:0435\:043d *)
    Return[Table[0, Length[x]]], (* \:0412\:043e\:0437\:0432\:0440\:0430\:0449\:0430\:0435\:043c \:0441\:043f\:0438\:0441\:043e\:043a \:043d\:0443\:043b\:0435\:0439 \:0442\:0430\:043a\:043e\:0439 \:0436\:0435 \:0434\:043b\:0438\:043d\:044b, \:043a\:0430\:043a \:0438 x *)
    data = Tpdfdata[[index[[1, 1]]]]; (* \:041f\:043e\:043b\:0443\:0447\:0430\:0435\:043c \:0441\:043e\:043e\:0442\:0432\:0435\:0442\:0441\:0442\:0432\:0443\:044e\:0449\:0438\:0435 \:0434\:0430\:043d\:043d\:044b\:0435 \:0434\:043b\:044f id *)
    interpolated = Interpolation[Transpose[{xValues, data}]]; (* \:0421\:043e\:0437\:0434\:0430\:0435\:043c \:0438\:043d\:0442\:0435\:0440\:043f\:043e\:043b\:0438\:0440\:043e\:0432\:0430\:043d\:043d\:0443\:044e \:0444\:0443\:043d\:043a\:0446\:0438\:044e *)
    Return[interpolated[x]]; (* \:0412\:043e\:0437\:0432\:0440\:0430\:0449\:0430\:0435\:043c \:0437\:043d\:0430\:0447\:0435\:043d\:0438\:0435 \:0438\:043d\:0442\:0435\:0440\:043f\:043e\:043b\:0438\:0440\:043e\:0432\:0430\:043d\:043d\:043e\:0439 \:0444\:0443\:043d\:043a\:0446\:0438\:0438 \:0434\:043b\:044f \:0437\:0430\:0434\:0430\:043d\:043d\:043e\:0433\:043e x *)
  ]
]

PDFALL[0.161,3]


(* ::Section:: *)
(*Fix the kinematics*)


(*
p1+p2 ->  k1 + j2
dw = A^2 / 2sqrt(s) d3k1 d3k2 /(2E1(2pi)^3 2E2(2pi)^3) 2pi delta(p1+p2-k1-k2)
m1=m2=0: w = (A^2 /2sqrtS) 1/8pi = A^2 / 16pi sqrtS
\:041f\:043e\:0442\:043e\:043c \:0441\:044e\:0434\:0430 \:043d\:0430\:043a\:0440\:0443\:0447\:0438\:0432\:0430\:0435\:043c PDF\:043a\:0443
*)
phaseSpacePrefactor[s_] := 1/(8 Pi);



(*
s=4E^2
(E,0,0,E)  (E,0,0,-E)
x1,x2 part transversed energy
(x1E,0,0,x1E)  (x2E,0,0,-x2E)
s1=(p1+p2)^2=4x1x2E^2=x1x2s
mH^2=s1 -- virtual Higgs boson
*)
R=1000;
If[R == 1,
    razmer = "TeV";
    qqh[x_,id_]=qqhGeV[Sqrt[x]*1000,id]/10^6;,
    If[R == 1000,
        razmer = "GeV";
        qqh[x_,id_]=qqhGeV[Sqrt[x],id];]]
        
        
s  = (13*R)^2;
mH = 0.125*R;
mc = 0.00127*R;
mb = 0.0042*R;
mt = 0.173*R;
mW = 0.080*R;

alpha = 1/137;
e = Sqrt[alpha/(4 Pi)];
sinW = Sqrt[0.222];
g=e/sinW;




Sigma[x1_,x2_,id_,mid_]:=1/(2 *x1 *Sqrt[s])* 1/(2 *x2 *Sqrt[s]) * phaseSpacePrefactor[1]*qqh[x1*x2*s,id]*ampSq[x1*x2*s,mt]*PDFALL[x1,-id]*PDFALL[x2,id]/(x1*x2*s-mH^2)^2
Print["Test sigma value is ",Sigma[0.1,0.02,4,mc]," ",razmer^(-2)]

SigmaHgg[x_]:=1/(2 x) * phaseSpacePrefactor[1] * ampSq[x^2,mt];

(*If[!FileExistsQ["/home/kds/ZZZ/pdfcode/sigval.txt"],
    (* \:0415\:0441\:043b\:0438 \:0444\:0430\:0439\:043b \:043d\:0435 \:0441\:0443\:0449\:0435\:0441\:0442\:0432\:0443\:0435\:0442, \:0432\:044b\:0447\:0438\:0441\:043b\:044f\:0435\:043c sigmaValues \:0438 \:0437\:0430\:043f\:0438\:0441\:044b\:0432\:0430\:0435\:043c \:0435\:0433\:043e \:0432 \:0444\:0430\:0439\:043b *)
    Print["Creating file of function values in xValues points. Length of xValues = ", Length[xValues]]
    sigmaValues = Table[Sigma[x y, 4,mc], {x, xValues}, {y, xValues}];
    Export["/home/kds/ZZZ/pdfcode/sigval.txt", sigmaValues, "Table"],
    (* \:0415\:0441\:043b\:0438 \:0444\:0430\:0439\:043b \:0441\:0443\:0449\:0435\:0441\:0442\:0432\:0443\:0435\:0442, \:0441\:0447\:0438\:0442\:044b\:0432\:0430\:0435\:043c sigmaValues \:0438\:0437 \:0444\:0430\:0439\:043b\:0430 *)
    Print["Importing values of function in xValues for file. Length of xValues = ", Length[xValues]]
    sigmaValues = Import["/home/kds/ZZZ/pdfcode/sigval.txt", "Table"];
]*)


(*\:0427\:0438\:0441\:043b\:0435\:043d\:043d\:043e\:0435 \:0438\:043d\:0442\:0435\:0433\:0440\:0438\:0440\:043e\:0432\:0430\:043d\:0438\:0435 2\:0414 \:043e\:0431\:043b\:0430\:0441\:0442\:0438 \:043f\:0440\:044f\:043c\:043e\:0443\:0433\:043e\:043b\:044c\:043d\:0438\:043a\:043e\:043c. 
\:0441\:0432\:043e\:0431\:043e\:0434\:043d\:044b\:0439 \:043f\:0430\:0440\:0430\:043c\:0435\:0442\:0440 s -- \:044d\:043d\:0435\:0440\:0433\:0438\:044f \:0443\:0441\:043e\:043a\:0440\:0438\:0442\:0435\:043b\:044f \:0432 \:0422\:044d\:0412*)
xmin = 0.05*R; xmax = 13*R; step = 0.005*R;
Sdata = Table[{x, SigmaHgg[x]}, {x, xmin, xmax, step}];
ListLogLogPlot[Sdata, Joined -> True, PlotStyle -> Blue, AxesLabel -> {"E", "sigma~E^2"}, 
 PlotTheme -> "Scientific"]
 
yHgg = Transpose[Sdata][[2]];
xHgg = Transpose[Sdata][[1]];

Print["Number of points is ",Length[xHgg]]
 integHgg = 0;
    Do[
    If[Mod[i, 100] == 1, Print["Step: ", i, "  Integral: ", integHgg]];
     integHgg += (yHgg[[i+1]]+yHgg[[i]])/2 *(xHgg[[i+1]]-xHgg[[i]]);
     , {i, Length[xHgg] - 1}]
    
(*
GeV^-2 -> (2*10^-14)^2 cm^2 -> 4*10^-4 b -> 4*10^8 pb
TeV^-2 -> 10^-6 GeV^-2 ->  4*10^-28 10^-6 cm^2 -> 4*10^-28 10^-6 *10^24 b -> 4* 10^-10 b - > 400 pb
*)    

(*Print["On Higgs mass S = ", SigmaHgg[mH]," ",razmer]*)
Print[integHgg//Simplify, " " ,razmer]





(*\:0441\:0440\:0435\:0437 \:043f\:043e \:0434\:0430\:043d\:043d\:044b\:043c, \:0447\:0442\:043e\:0431\:044b \:043c\:0435\:043d\:044c\:0448\:0435 \:0441\:0447\:0438\:0442\:0430\:0442\:044c
x1,x2 \:043c\:0430\:0441\:0441\:0438\:0432\:044b \:0442\:043e\:0447\:0435\:043a \:043f\:043e \:043a\:043e\:0442\:043e\:0440\:044b\:043c \:0438\:0434\:0435\:0442 \:0438\:043d\:0442\:0435\:0433\:0440\:0438\:0440\:043e\:0432\:0430\:043d\:0438\:0435
*)
cValues = LogRange[0.01,1,100];
x1 = cValues;
x2 = cValues;

Print["Number of points is ",Length[x1]]
 integralc = 0;
   Do[
    Do[
    If[Mod[i, 20] == 1 && j == 1, Print["Step: ", i, "  Integral: ", integralc]];
     integralc += Sigma[(x1[[i + 1]] + x1[[i]])/2, (x2[[j + 1]] + x2[[j]])/2,4,mc] * (x1[[i + 1]] - x1[[i]]) * (x2[[j + 1]] - x2[[j]]); 
     , {j, Length[x2] - 1}]
    , {i, Length[x1] - 1}];
    
integralc
razmer


 integralb = 0;
bValues = LogRange[0.01,1,100];
x1 = bValues;
x2 = bValues;
Print["Number of points is ",Length[x1]]
   Do[
    Do[
    If[Mod[i, 10] == 1 && j == 1, Print["Step: ", i, "  Integral: ", integralb]];
     integralb += Sigma[(x1[[i + 1]] + x1[[i]])/2, (x2[[j + 1]] + x2[[j]])/2,5,mb] * (x1[[i + 1]] - x1[[i]]) * (x2[[j + 1]] - x2[[j]]); 
     , {j, Length[x2] - 1}]
    , {i, Length[x1] - 1}];
    
integralb
razmer



 (*integralt = 0;
tValues = LogRange[5/130,1,10]
x1 = tValues;
x2 = tValues;
Print["Number of points is ",Length[x1]]
   Do[
    Do[
    If[Mod[i, 10] == 1 && j == 1, Print["Step: ", i, "  Integral: ", integralt]];
     integralt += Sigma[(x1[[i + 1]] + x1[[i]])/2, (x2[[j + 1]] + x2[[j]])/2,6,mt] * (x1[[i + 1]] - x1[[i]]) * (x2[[j + 1]] - x2[[j]]); 
     , {j, Length[x2] - 1}]
    , {i, Length[x1] - 1}];
    
integralt
*)


IntAll = Simplify[integralc+integralb];

(*15 \:043f\:0438\:043a\:0431\:0430\:0440\:043d h-gg*)

(*
1/GeV =2*10^-14 cm
1000/TeV= 1/GeV= 2*10^-14 cm
10^6/TeV^2= 4*10^-28 cm^2 => 1/TeV^2= 4 * 10^-34cm^2 =
barn = 10^-24 sm^2  => 1/TeV^2 =  4 * 10^-34 *10^24 = 4 * 10^-10 barn = 4 * 10^-10* 10^12 pikabarn
1/TeV^2 400pb

1/GeV =2*10^-14 cm
1/GeV^2 =4*10^-28 cm^2 = 4*10^-28 *10^36 pb = 4*10^8 pb
*)

Print["Integral is equal to ",  IntAll ];
Print["We work in TeV system, so this must be ",razmer,"^-2"]

IntSm = IntAll * 4 *10^(8);
Print["Integral = ",  IntSm," pikabarn" ];


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
