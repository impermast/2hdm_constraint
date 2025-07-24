(* ::Package:: *)

(* ::Package:: *)
(*============================================================================*)
(*  ModelParams` \[Dash] 2HDM parameters & analytic replacements                     *)
(*                                                                             *)
(*  \[Bullet] \[Beta] is IDENTIFIED with \[Alpha]\:2081  (\[Beta] \[Congruent] \[Alpha]\:2081)                                        *)
(*  \[Bullet] Provides:                                                                *)
(*      AngleRules   \[Dash] X\:1d62,Y\:1d62,R_{ij} \[RightArrow] \[Alpha]\:2081,\[Alpha]\:2082,\[Alpha]\:2083  (with fast X\:2081X\:2082X\:2083 shortcuts)   *)
(*      AngleChanger \[Dash] wrapper   expr /. AngleRules   (+ optional Simplify)    *)
(*      SmpChanger   \[Dash] FeynCalc SMP \[RightArrow] Model symbols                            *)
(*      Params       \[Dash] numeric SM / 2HDM constants                             *)
(*      NumEvaluate  \[Dash] AngleRules & SmpChanger & Params (numeric output)       *)
(*============================================================================*)


BeginPackage["ModelParams`"];

(* \[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine] user\:2011visible symbols \[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine] *)
Params::usage       = "Numeric parameter replacement rules. Apply with /. Params.";
AngleRules::usage   = "Analytic replacements X_i, Y_i, R_ij \[RightArrow] \[Alpha]\:2081,\[Alpha]\:2082,\[Alpha]\:2083 (\[Beta]\[Congruent]\[Alpha]\:2081).";
AngleChanger::usage = "AngleChanger[expr, simplify:True] substitutes AngleRules (+ Simplify).";
SmpChanger::usage   = "Rules converting FeynCalc`SMP[\[Ellipsis]] symbols to ModelParams variables.";
PaveToLooptools::usage = "Change Pave p-v function notation to Looptools";


pMQ   = 0.;            (* common light\:2011quark mass  *)
pMW   = 80.377;        (* GeV                     *)
pMZ   = 91.1876;       (* GeV                     *)
pmh1  = 125;
pvev  = 246;           (* GeV                     *)
pca   = 3;             (* colour\:2011factor           *)

(* derived *)
pe  = Sqrt[4 Pi palpha];
psw = Sqrt[1 - (pMW/pMZ)^2];
pcw = Sqrt[(pMW/pMZ)^2];
pgw = pe/psw;
pg1 = pe/pcw;
palpha = 1/137;
pg = Sqrt[4 \[Pi] 1/137]/Sqrt[1 - (80.4/91.2)^2];

Begin["`Private`"];

(* --------------------------------------------------------------------------- *)
(* 1.  Mixing\:2011angle helpers                                                     *)
(*      \[Beta] is NOT independent \[DoubleRightArrow]  c\[Beta]=cos \[Alpha]\:2081,  s\[Beta]=sin \[Alpha]\:2081                          *)
(* --------------------------------------------------------------------------- *)
pTrigRules = {
  pS1 -> Sin[Global`a1],  pC1 -> Cos[Global`a1],
  pS2 -> Sin[Global`a2],  pC2 -> Cos[Global`a2],
  pS3 -> Sin[Global`a3],  pC3 -> Cos[Global`a3],
  pSb -> Sin[Global`a1],  pCb -> Cos[Global`a1]
};

(* --------------------------------------------------------------------------- *)
(* 2.  Analytic replacements  (El\:202fKaffas\[Dash]Osland\[Dash]\[CapitalOSlash]greid 2007)                    *)
(* --------------------------------------------------------------------------- *)

ClearAll[AngleRules];
AngleRules = Join[
  {
   
   (* ---- 3rd column of R --------------------------------------------------- *)
    Global`R1x1 | ModelParams`R1x1 :>  pC1 pC2,                      Global`R1x2 | ModelParams`R1x2 :> pS1 pC2                  ,     Global`R1x3 | ModelParams`R1x3 :>  pS2,
    Global`R2x1 | ModelParams`R2x1 :>  -(pC1 pS2 pS3 + pS1 pC3),     Global`R2x2 | ModelParams`R2x2 :>  -(pS1 pS2 pS3 - pC1 pC3),     Global`R2x3 | ModelParams`R2x3 :>  pS3 pC2,     
    Global`R3x1 | ModelParams`R3x1 :>  -pC1 pS2 pC3 + pS1 pS3  ,     Global`R3x2 | ModelParams`R3x2 :>  -(pS1 pS2 pC3 + pC1 pS3),     Global`R3x3 | ModelParams`R3x3 :>  pC2 pC3,                   

    (* ---- first two columns ------------------------------------------------- *)
    X1 | ModelParams`X1 :>  pC2 (  pCb pC1 + pSb pS1 ),
    Y1 | ModelParams`Y1 :>  R1x2 pCb - R1x1 pSb,

    X2 | ModelParams`X2 :> -pCb ( pC1 pS2 pS3 + pS1 pC3 ) + pSb ( pC1 pC3 - pS1 pS2 pS3 ),
    Y2 | ModelParams`Y2 :>  R2x2 pCb - R2x1 pSb,

    X3 | ModelParams`X3 :> -pCb ( pC1 pS2 pC3 - pS1 pS3 ) - pSb ( pC1 pS3 + pS1 pS2 pC3 ),
    Y3 | ModelParams`Y3 :>  R3x2 pCb - R3x1 pSb,

    (* ---- orthonormality shortcuts ----------------------------------------- *)
    (X1|ModelParams`X1)^2 + (X2|ModelParams`X2)^2 + (X3|ModelParams`X3)^2 :> 1,
    (Y1|ModelParams`Y1)^2 + (Y2|ModelParams`Y2)^2 + (Y3|ModelParams`Y3)^2 :> 1,
    (X1|ModelParams`X1)(Y1|ModelParams`Y1) + (X2|ModelParams`X2)(Y2|ModelParams`Y2) + (X3|ModelParams`X3)(Y3|ModelParams`Y3) :> 0
  },
  {
    (* ---- fast user\:2011donated shortcuts  -------------------------------------- *)
    (FeynCalc`X1|ModelParams`X1)*(FeynCalc`X2|ModelParams`X2)*(FeynCalc`X3|ModelParams`X3)                 :>  pS2^2 pS3 pC3 pC2,
    (FeynCalc`X1|X1|ModelParams`X1)^2*(FeynCalc`X2|ModelParams`X2)^2*(FeynCalc`X3|ModelParams`X3)^2        :>  pS2^4 pS3^2 pC3^2 pC2^2
  }
];

(* --------------------------------------------------------------------------- *)
(* 3.  Numeric constants  \[Dash]\[NonBreakingSpace]edit here if needed                                 *)
(* --------------------------------------------------------------------------- *)
(* \:0441\:0432\:044f\:0437\:044b\:0432\:0430\:0435\:043c \:0433\:043b\:043e\:0431\:0430\:043b\:044c\:043d\:044b\:0435 \:043f\:0435\:0440\:0435\:043c\:0435\:043d\:043d\:044b\:0435 \:0441\:043e \:0437\:043d\:0430\:0447\:0435\:043d\:0438\:044f\:043c\:0438 \:0438\:0437 p-\:0432\:0435\:0440\:0441\:0438\:0439 *)

Params = {
  Global`MQ -> ModelParams`pMQ,
  Global`mh1 -> ModelParams`pmh1,
  Global`MW -> ModelParams`pMW,
  Global`MZ -> ModelParams`pMZ,
  Global`sw -> ModelParams`psw,
  Global`cw -> ModelParams`pcw,
  Global`gw -> ModelParams`pgw,
  Global`g1 -> ModelParams`pg1,
  Global`g -> ModelParams`pg,
  Global`alpha -> ModelParams`palpha,
  Global`e  -> ModelParams`pe,
  Global`vev -> ModelParams`pvev,
  Global`ca -> ModelParams`pca
};

(* --------------------------------------------------------------------------- *)
(* 4.  SMP\[RightArrow]Model translation (FeynCalc)                                        *)
(* --------------------------------------------------------------------------- *)
SmpChanger = {
  FeynCalc`SMP["m_Z"] -> ModelParams`pMZ,
  FeynCalc`SMP["m_W"] -> ModelParams`pMW,
  FeynCalc`SMP["m_t"] -> ModelParams`pMQ,
  FeynCalc`SMP["m_b"] -> ModelParams`pMQ,
  FeynCalc`SMP["m_u"] -> ModelParams`pMQ,
  FeynCalc`SMP["m_d"] -> ModelParams`pMQ,
  FeynCalc`SMP["m_q"] -> ModelParams`pMQ,
  FeynCalc`SMP["e"]   -> ModelParams`pe,
  FeynCalc`CA          -> ModelParams`pca
};

PaveToLooptools = {
   FeynCalc`PaVe[0, 0, 1, {pa_, pb_, pc_}, {pd_, pE_, pf_}, ___] -> LoopTools`C0i[LoopTools`cc001, pb, pa, pc, pd, pE, pf],
   FeynCalc`PaVe[0, 0, 2, {pa_, pb_, pc_}, {pd_, pE_, pf_}, ___] -> LoopTools`C0i[LoopTools`cc002, pb, pa, pc, pd, pE, pf],
   FeynCalc`PaVe[1,  {pa_, pb_, pc_}, {pd_, pE_, pf_}, ___] -> LoopTools`C0i[LoopTools`cc1, pb, pa, pc, pd, pE, pf],
   FeynCalc`PaVe[2,  {pa_, pb_, pc_}, {pd_, pE_, pf_}, ___] -> LoopTools`C0i[LoopTools`cc2, pb, pa, pc, pd, pE, pf],
   FeynCalc`PaVeAutoOrder -> Sequence[],
   FeynCalc`PaVeAutoReduce -> Sequence[]
};

(* --------------------------------------------------------------------------- *)
(* 5.  Convenience wrappers                                                    *)
(* --------------------------------------------------------------------------- *)
ClearAll[AngleChanger];
AngleChanger = Join[AngleRules, pTrigRules];

End[];  (* `Private` *)
EndPackage[];



(*BeginPackage["ModelParams`"];

Params::usage = "params \:0441\:043e\:0434\:0435\:0440\:0436\:0438\:0442 \:043e\:0441\:043d\:043e\:0432\:043d\:044b\:0435 \:043f\:0430\:0440\:0430\:043c\:0435\:0442\:0440\:044b \:043c\:043e\:0434\:0435\:043b\:0438 \:0434\:043b\:044f \:0440\:0430\:0441\:0447\:0435\:0442\:043e\:0432 \:0432 \:0413\:044d\:0412.";
AngleChanger::usage = "\:0417\:0430\:043c\:0435\:043d\:0430 \:0443\:0433\:043b\:043e\:0432 \:043f\:043e\:0432\:043e\:0440\:043e\:0442\:0430 Y X R \:043c\:0430\:0442\:0440\:0438\:0446 \:043d\:0430 a1 a2 a3";

(* \:041e\:043f\:0440\:0435\:0434\:0435\:043b\:044f\:0435\:043c \:043f\:0430\:0440\:0430\:043c\:0435\:0442\:0440\:044b \:043c\:043e\:0434\:0435\:043b\:0438 \:0432 **\:0433\:043b\:043e\:0431\:0430\:043b\:044c\:043d\:043e\:043c \:043a\:043e\:043d\:0442\:0435\:043a\:0441\:0442\:0435** *)
MQ = 0;
WZ = 2;
ca = 3;
MW = 80.4;
MZ = 91.2;
mh1 = 125.1;
sw = Sqrt[1 - (80.4/91.2)^2];
cw = Sqrt[(80.4/91.2)^2];
gw = 0.653;
g1 = 0.357;
v = 246.22;
e = Sqrt[4 \[Pi] /137];
alpha = 1/137;
g = Sqrt[4 \[Pi] 1/137]/Sqrt[1 - (80.4/91.2)^2];


SmpChanger = {
	FeynCalc`SMP["m_Z"] -> MZ,	
	FeynCalc`SMP["m_t"] -> MQ,
	FeynCalc`SMP["m_b"] -> MQ,
	FeynCalc`SMP["m_u"] -> MQ,
	FeynCalc`SMP["m_d"] -> MQ,
	FeynCalc`SMP["m_q"] -> MQ,
	FeynCalc`SMP["e"] -> e,
	FeynCalc`SMP["m_W"] -> MW,
	FeynCalc`CA -> ca
};

Params = {
   (* \:041c\:0430\:0441\:0441\:044b \:043a\:0432\:0430\:0440\:043a\:043e\:0432 (GeV) *)
   MQ -> 0,  
   (* \:041c\:0430\:0441\:0441\:044b \:043a\:0430\:043b\:0438\:0431\:0440\:043e\:0432\:043e\:0447\:043d\:044b\:0445 \:0431\:043e\:0437\:043e\:043d\:043e\:0432 (GeV) *)
   MW -> 80.4, (* \:041c\:0430\:0441\:0441\:0430 W-\:0431\:043e\:0437\:043e\:043d\:0430 *)
   WZ -> 2, (* \:0428\:0418\:0420\:0418\:041d\:0410 Z-\:0431\:043e\:0437\:043e\:043d\:0430 *)
   MZ -> 91.2,
   mZ -> 91.2,(* \:041c\:0430\:0441\:0441\:0430 Z-\:0431\:043e\:0437\:043e\:043d\:0430 *)  
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
   g -> Sqrt[4 \[Pi] 1/137]/Sqrt[1 - (80.4/91.2)^2],
   ca -> 3
};
PaveToLooptools = {
   FeynCalc`PaVe[0, 0, 1, {a_, b_, c_}, {d_, e_, f_}, ___] -> LoopTools`C0i[LoopTools`cc001, a, b, c, d, e, f],
   FeynCalc`PaVe[0, 0, 2, {a_, b_, c_}, {d_, e_, f_}, ___] -> LoopTools`C0i[LoopTools`cc002, a, b, c, d, e, f],
   FeynCalc`PaVe[1, {a_, b_, c_}, {d_, e_, f_}, ___] -> LoopTools`C0i[LoopTools`cc1, a, b, c, d, e, f],
   FeynCalc`PaVe[2, {a_, b_, c_}, {d_, e_, f_}, ___] -> LoopTools`C0i[LoopTools`cc2, a, b, c, d, e, f],
   FeynCalc`PaVeAutoOrder -> Sequence[],
   FeynCalc`PaVeAutoReduce -> Sequence[]
};
EndPackage[];*)
