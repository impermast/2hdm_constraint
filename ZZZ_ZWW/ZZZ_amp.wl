(* ::Package:: *)

(* ::Title:: *)
(*ZZZ in 2hdm*)


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
(*Generate Feynman diagrams*)


Subscript[m, W]=mW;
Subscript[m, Z]=mZ;
FCGV["MZ"]=mZ;



$ExcludeTopologies[ V4onExt ]=FreeQ[ Cases[#, Propagator[External][__]], Vertex[4] ]&;
$ExcludeTopologies[ La ]=FreeQ[ Cases[#, Propagator[Incoming][__]], Vertex[3] ]&;

(* BLOCK ZZZ hi hj Z *)
CreateTopologies[1, 1 -> 2,ExcludeTopologies->{Tadpoles, Internal, V4onExt, La}];
diagsHHZ = InsertFields[CreateTopologies[1, 1 -> 2,ExcludeTopologies->{Tadpoles, Internal, V4onExt}],
{V[2]} -> {V[2], V[2]}, InsertionLevel -> {Classes},
Model -> THDMCPV, ExcludeParticles -> {F[_], V[1], V[3], S[2], S[3], U[_]}, LastSelections -> {S[4|5|6], V[2]}];

(* BLOCK ZZZ hi hj hk *)
CreateTopologies[1, 1 -> 2,ExcludeTopologies->{Tadpoles, Internal, V4onExt, La}];
diagsHHH = InsertFields[CreateTopologies[1, 1 -> 2,ExcludeTopologies->{Tadpoles, Internal, V4onExt}], {V[2]} -> {V[2], V[2]}, InsertionLevel -> {Classes},
Model -> THDMCPV, ExcludeParticles -> {F[_], V[1], V[3], S[2], S[3], U[_]}, LastSelections -> {S[4], S[5], S[6]}];

(* BLOCK ZZZ hi hj G *)
CreateTopologies[1, 1 -> 2,ExcludeTopologies->{Tadpoles, Internal, V4onExt, La}];
diagsHHG = InsertFields[CreateTopologies[1, 1 -> 2,ExcludeTopologies->{Tadpoles, Internal, V4onExt}], {V[2]} -> {V[2], V[2]}, InsertionLevel -> {Classes},
Model -> THDMCPV, ExcludeParticles -> {F[_], V[1], V[3], S[2], S[3], U[_]}, LastSelections -> {S[4|5|6], S[1]}];

diags1 = DiagramExtract[diagsHHZ, {1..6}];
diags2 = DiagramExtract[diagsHHZ, {7..12}];
diags3 = DiagramExtract[diagsHHZ, {13..18}];
diags4 = diagsHHH;
diags5 = diagsHHG;


(* ::Section:: *)
(*Test vertex*)


diagscheck = InsertFields[CreateTopologies[0, 1 -> 2,ExcludeTopologies->{Tadpoles, Internal, V4onExt}],
{S[4]} -> {V[2], V[2]},Model -> THDMCPV, InsertionLevel -> {Classes}];

Print["Test diag"]2
Paint[diagscheck, ColumnsXRows -> {2, 1}, Numbering -> Simple,SheetHeader->None,ImageSize->{512,256}]
d1 = diagscheck;
ampd1[0] = FCFAConvert[CreateFeynAmp[d1,Truncated -> True], 
		IncomingMomenta->{P}, OutgoingMomenta->{p1,p2},LorentzIndexNames->{\[Mu],\[Alpha],\[Beta]},
		UndoChiralSplittings->True,ChangeDimension->D,List->False, SMP->False]/.p->p1+p2;
ampd1[1] = ampd1[0]//ReplaceAll[#,
	Pair[Momentum[Polarization[___],___],___]:>1]&//Contract
	



imsize = {600, 100}
Print["Diagrams 1:"]
Paint[diags1, ColumnsXRows -> {6, 1}, Numbering -> Simple, SheetHeader -> None, ImageSize -> imsize ];
Print["Diagrams 2:"]
Paint[diags2, ColumnsXRows -> {6, 1}, Numbering -> Simple, SheetHeader -> None, ImageSize -> imsize ];
Print["Diagrams 3:"]
Paint[diags3, ColumnsXRows -> {6, 1}, Numbering -> Simple, SheetHeader -> None, ImageSize -> imsize ];
Print["Diagrams 4:"]
Paint[diags4, ColumnsXRows -> {6, 1}, Numbering -> Simple, SheetHeader -> None, ImageSize -> imsize ];
Print["Diagrams 5:"]
Paint[diags5, ColumnsXRows -> {6, 1}, Numbering -> Simple, SheetHeader -> None, ImageSize -> imsize ];


(* ::Print:: *)
(*Graphics[{Inset[Graphics[Annotation[Tooltip[{Thickness[0.005], Line[CompressedData["*)
(*1:eJw9lAlIVEEYx8daU9u01TzWyq431WIZ1nNFUpivU+mQLASrNUrxobWh3VvZ*)
(*IYXpFpZZ1tuygwqtoGM90k4pS6UnlhZuF2nRYVm5ZW6t5msK5g0Mww+Gmfn+*)
(*3/8/o5PSF6b0QwhNp/Pfavwp0xEI6P/A8GTLiYmFIuUMx4vNwRjMsVPsE3nK*)
(*daPLxyzAkJTfXFgjaQEt180qN2J4HxQckyhQrjQP6MrEsLN5kpdDDgCktYPb*)
(*Dsr6kY/zRcoJe3db12L4LsYmhvCUIWz30sUYYjepV9VL/oBspj1tegxx2aaS*)
(*FIFy3WHPOYMw6BweNS6IckaP77w2DpxRizYWiX6ANAEzaqs4yKv+lRvBU9by*)
(*GeEWDrI63rQ2Sb6ArnicDsnmYNFK/UajQNnWMeH7Dg6KPJ0GV0RZ19IYkcOB*)
(*uuXVjSJxCKCjSeHeJziQllQ+0POUE4y8yx0OTvU+rWiQfAC5Z1+c94GD647A*)
(*eykC5UeLnSt8MHTGqQL7ZG9AB+7lLYvEMGdoZkuhSLmzvPR4Ioavxd3qUJ7y*)
(*o/K1Uzdg8E+NtNZLGkAxzplmqs+fI+cqkgXKCcnS8q0YWo6/6uqRBwMqWV0T*)
(*nobheXWBNZSnXKlaOGwuhkbvl8tSBC9a73ZiwhgMn+Nvi6In1Uf4fOk3B6Xx*)
(*6RcapEGAWjPHio0cdFuj/V0QZc2X5N4LHJiKpw4O49WAFoREy/s5eBw0okoQ*)
(*BtL67COvZnHgdahttih6KGyafL/zoeSu7HeOrWzvk92U86J2HYifwrsp9xV8*)
(*OT9NEAYo7xlVy522iK7Ke/20xvmNkkqpRxs1Rq1CKqXenyVlByP4/ooed91z*)
(*jxmFfopevrk+H0+KLoqehrNucU0SUvTW6B0N/RFS+uFoOvIjVegjrF+Jd4px*)
(*vdRLWD+vzB7xTsf3ENbv0vgcTa74mzA/pG0N2PdJdhDmlwL34PGxQjdhfnop*)
(*r8ssk7oI89vZy8NtQfwPwvxoS7eOM4t2wvwKXpdf2+VvhPk5bHLkzSapgzC/*)
(*x5w/U1wmthOWh9BDZr/DwnvC8lJ9zXXTGv4tYXm6WZuTF41aCcubrt2wbYLl*)
(*GWF5TLh6/2AF30xYXk/dKn22wlJPWJ5RmsG23lJFWN7/fwMfs26x/+AvHtmh*)
(*cw==*)
(*"]], Inset[$CellContext`Z, {3.25, 9.0548}, ImageScaled[{Rational[1, 2], 1}]], Line[CompressedData["*)
(*1:eJwt1A1Qk3UcB/AH9PKlQMSrC7gIYggN9iKzbWDkgy+Iim5sqPMQ2nEg66Yw*)
(*QRA4sRkSEyoQGkOOK9CKDXyZkxXWIVzAKJgNkJfVHbvhCzHDOnJtxIv0farn*)
(*bvve5777/V92uwWn54gyPQmC2IMXlf+90Uni3yeSJMhxqSgGlnRby8YiSCKP*)
(*Rn8/D7anFm24Dfcbw6tbqV584fDn8Nv9XtM2WFi4z/gZvLjkI/JBsp25eQak*)
(*zZL/2nak3nv7JSvywOO7y3KqN0z7vYT9Kofz3RpYEmv+Yzccrti2tReWRbbO*)
(*lcC9o/aROVgYXrn3Gpz40Tt3gpGqiKs3B5Dc33g5yUi7edPJMeSOQNvaCmSH*)
(*h8s5iJwq4YV8j+z+Zc2iHllcxnIuUvM/HFlNfa75dKCcy8B5MqylabDL99ox*)
(*BSyZLWjhwjd3LRu0sLDXb40/zMiSN9hgBadX7g3T3qq9upGJ+w1FT70ChzLI*)
(*sTh4qOocnwNXFInacmBV4vENUrgr4J5nI5xwbClFA9uydrj6YUWxOHMYDpqU*)
(*6J2wsM7g8QL2Uad+uy2EhfU3O6YZcHcQXZIMC6/oxnfCP0l1KSq4iagt2A27*)
(*J5ppXbBPAO0WBz5iFLw+Dyskz6t84aSRPqYfmySkH1aoZ7DfV9HVaVy4fqVP*)
(*3wlLk8kXxTA/Jd11OZJUFt+N+DobJkvviy+gP9O+ef1FuDD4wanz6K0ZfEcz*)
(*zP7E4KxBzx48ceoObH8Syv8G/Ypu2W2BZR+fqXXAPbyGxcfU/OxWvxAGqTz3*)
(*u6fPErV+S2t7Cs53vjPO5LsF528aL6hET1eI1HRYxts72QZnLi+ZdsIzjUdt*)
(*HXBApUueBnff+KDqFubl/lFHi2FiNnh/PfqzSRamBu6ILhPkop+ovZjVDusH*)
(*p6bj4YG0iOxhylv+jgmEj6+rMT6F6/U9qwnMx+ZmWNZGYb2zcQf/xPnrhkdL*)
(*3oDDy7RF87jvRlZNZww8379gxPerLHcKTEmwkpXvjqXu477nIYNlD73zT6Nf*)
(*0I4slMAK8XjdDfSlv47GfwrPrLcwH6HXjDw92QYntPz1oxd+B7yuudEeuEkQ*)
(*+TCUSSo5Ldzrk7DqZ53pTfQFii9m56l5u9DDH257cEjwMgfnS121bwHrFa4q*)
(*T4+CEybqZ4Zgf6G3RgALta/e/hL7e6mzJCfg6gDbgBLuetZAU8Eqc+OB92Dr*)
(*nrDmKzA/jMXOwPwTskb3HSwJezc3D/1UYvyh+7A2u9ysRu/oMz9zwEGXDj4y*)
(*wc9X8HD+/39hkv8As5asQQ==*)
(*"]], Inset[$CellContext`Z, {16.280796897832168`, 15.429621715174807`}, ImageScaled[{Rational[1, 2], 0}]], Line[CompressedData["*)
(*1:eJwl1AtMU1cYB/ALLcjAYJ1t7RRsx3OhgK0wjGws1wd0jlm7IRWRSgttFzRi*)
(*bfcw05BiNDKRUaAMCU6pCxAQtJ2ZTHwMp2MdStbSVZhkusgjFCmtDl9Bcf+z*)
(*neTem1++x/lycu99s2jPx5pAiqIkuMjz/1sCTf23eDQV/amWlwjLor61Vy2l*)
(*KYnWkLsNpv8KbxTCCWUjnRWwVXp1bIJLU5k625UeWCfdobTDlbz+Cz6Y1bim*)
(*aRCecIs2xwtpSrD6cn0A6u8WjjqVcLNMrM2G33j+ML0JNp0VOy1wR/oWlRuW*)
(*9QSbZ+F4fndceCJNOa5ZAlMx33e22pYNMOt8TlUOXFseId8H06fy+rLh8fyY*)
(*mHbYJH1QtxxmiOX6IVLPOamwo1/Y2fxQZhJN+Vs7kxQwa7H6xUrY0Rc5NYl5*)
(*57hKQwFMFU7N7IddlpFDR0l+9eNdsfB4gZx5CZaFfvSPn0NT1ctXq2ZI/rOD*)
(*J8bgBO+IOSYZ86iOvEYh37iuPVoBK4sfffYuXDA8LWogbrg+UAXPF6hOO2Gj*)
(*IfTUGJxxs2lT2ErsN7T+GxHmc3lef2s9bFQE8YthYVCwfh/sWHP+8l74Kn+7*)
(*u4O4tLZnKzx/6IDhDiyiu7xc+MDilz6GCPXS3JJu9M+ya5jLYP9wyaosmJIH*)
(*hohgxzJfnAvz30yt+zqL2J2tK+XQRlHTr9wdsDJC44lF/KvsPx9/TurX3RgN*)
(*RNy49o+NNaR/cFtbCNx+5qfZTlh34cZ0Cpzft8fyGyx6VanaT+JJ6c4JuHch*)
(*R+iEt8Xm+Rlimvrb+oJPzpfJPrdbAAvS3i/6BDZvFg6+A9MBcd6jcItvypUL*)
(*N2/I6TrCpY1vO6OKSmHlpsGB7fCuGlfbYRJP1DAWIp976y7zBOk/29p/Gvvx*)
(*9Y3zNtj047PuZDjtujzBLibv660VvWya0nsKOfdIPf+lTguXatKSn5L+FsPP*)
(*SWzaqP+w/3vWKvTTVgxFwF2K8lYh3FyREp6K/MNP2p0S2Drsle6GL/6i61fD*)
(*Srt1wRXkSzImgw7CrJpzIWyc56K16pmTsPGHRU9UmGfSb5rrgXWMaU8DXBKq*)
(*trvh3sb4Ox3Id19URvtgwdaswnq4rb6auyCFfE/cujz4oeT+eCTsyNsyOof9*)
(*05ZmMlNh65fH71XCxVWCgA9gpXoghQdzeZl7lbAssizKtoSmysqf3v6CxCfH*)
(*J9RLcJ4Rz4+ZSH+/eWcGLGh50N1B+rWGdL0HX/PupPvg3kuPVmhR7zH/fvs+*)
(*yTfaws7Ar8iCWeT/wqb/BeeBmPU=*)
(*"]], Inset[$CellContext`Z, {16.280796897832168`, 4.570378284825192}, ImageScaled[{Rational[1, 2], 1}]], Line[CompressedData["*)
(*1:eJwt1A9Qk3UYB/AXtJMRLCgkgiFQMpmrWDFhh0CvCAoI5/QWAle2JhY2QErS*)
(*pRysRKDyZAeIoPyZAweCuBcjpMWfpc4yOEQ6EAlhnvwZhkU4byPmXd9f13v3*)
(*vu997nl+7+/P89wbIDu4e78jRVFJuMmbciKPjTT13/U6TaklpcM/8miK6Wht*)
(*cYElTadWjsIZPa3zL8EREZ++R8PSUvcX3OEx9rjdFRY3MoNkvPSTwuvTQTRV*)
(*pa5rf8BHvv675BvwYnNBfAc8YT4W1AoL2Gc5x+CFRyFtlXDGc/KkcDgvur7j*)
(*OImHj7CsWJci89y9bFis+GGkE1bVsJYlMMXpv5UP3xjS1YfAXYHsB7the4+T*)
(*lAWXxMqzNsFxnNjVoxtoKiUq6yYXNrGaTp+FnWZiIwPhodWholS460jvnjfg*)
(*dNu1KQ/4F5dVCeFwzni+eYhLU6L4+rptsM0af14FL9YwTxPJuXFG7ybDKeLA*)
(*n+Lgxv7PDwfCiuVt06GwpXHNlmeBmCex298T5mzh6+/DSoFX9BzOTTl5QGiE*)
(*VQmUYxusblp2aSfeq2+Wwycj3Z3q4GZF8JEgeOAC71oJHCd89KcZ+4xp0XGz*)
(*YWYlwv0KvCCf+30HyV9Y0hXDkg/fOe0PVzHFc3K4+9BtemE9zmGdcGQvrJ74*)
(*lcUQzz8uIzaJS4OzYf+q2S8+JnWo+aOTBzNjOZpcuIK7j5p9DfvWDZQqyfyP*)
(*Z3ZpYa+o1MUTsK2fn54FVxWZ9pA4rZCtioQZe8F4JizsEA2thUU8+2cJpC80*)
(*D3nWV2nKEFbA9SZ1m/aLmYKpDyZNk6hD4Rn+R/1wyqF4di2cJ/ZJ6CL5AelU*)
(*Gmx/mF+qgQWR/4T4wKoi7VvFsGLjgGoGdaj5LfXvfXBGal9PN+zl+01DGBlf*)
(*YVFr4Ezj5jRH2JQd43sGbp5ylRkDsI+yzZ5V8KWfOz2Ow+b85/Xn4YhnuqIo*)
(*mHm6a42OfH9Z4rPij/VVHsjTw4bqMHY3bONvH+uFcytnHQphpkw5fBWu0Dc0*)
(*SGDlyZ3tZH4DtyXnTZje6nwxHxa+73rBDTYoHGyJJM64ty35IS606N24pF94*)
(*B/thkzdbOUj6SMt90giLUtOavoXVT+K+VMDU/gJtEuxUWf02TfIvnoh/BTa/*)
(*qOmzrkNfjw47L5G6l+/wrIXd+pKr78FeFplGAKdMVdTehsXS0ajvfeFb54x3*)
(*4Li77XeCYfPR7Z4TJH/6a7WWg3MrZw7Pw6LywVx/2NTXIbPAiw5rLzf4ID+U*)
(*f8kKl3x1X7YJLhElC/6Cu25KL49501SQrDdjDFYnGbeegg1+2utXYIOw/l0J*)
(*bH555WoB6UuRxwgXTrE4l0XDbuS/5f3//2s9/S+cWbuk*)
(*"]], Inset[$CellContext`Z, {9.332600380984282, 13.056274380900543`}, ImageScaled[{1, 0}]], {Dashing[{0.030000000000000002`, 0.030000000000000002`}], Line[{{6.500000000001819, 9.999999999996362}, {12.999999999998181`, 6.}}]}, Inset[$CellContext`h1, {9.463624737400366, 7.156640198275594}, ImageScaled[{1, 1}]], {Dashing[{0.030000000000000002`, 0.030000000000000002`}], Line[{{13., 13.999999999996009`}, {13., 5.99999999999511}}]}, Inset[$CellContext`h2, {13.8993, 10.}, ImageScaled[{0, Rational[1, 2]}]], {PointSize[0.04], Point[{6.5, 10.}], Point[{13., 14.}], Point[{13., 6.}]}, Inset[1, {10., -0.5}, ImageScaled[{Rational[1, 2], 0}]]}, "ad/becf/dedfef.m", TooltipStyle -> "TextStyling"], "ad/becf/dedfef.m", "Tooltip"], AspectRatio -> 1, PlotRange -> {{-1, 21}, {-1, 21}}], {0, 0}, {0, 0}, {22, 22}], Inset[Graphics[Annotation[Tooltip[{Thickness[0.005], Line[CompressedData["*)
(*1:eJw9lAlIVEEYx8daU9u01TzWyq431WIZ1nNFUpivU+mQLASrNUrxobWh3VvZ*)
(*IYXpFpZZ1tuygwqtoGM90k4pS6UnlhZuF2nRYVm5ZW6t5msK5g0Mww+Gmfn+*)
(*3/8/o5PSF6b0QwhNp/Pfavwp0xEI6P/A8GTLiYmFIuUMx4vNwRjMsVPsE3nK*)
(*daPLxyzAkJTfXFgjaQEt180qN2J4HxQckyhQrjQP6MrEsLN5kpdDDgCktYPb*)
(*Dsr6kY/zRcoJe3db12L4LsYmhvCUIWz30sUYYjepV9VL/oBspj1tegxx2aaS*)
(*FIFy3WHPOYMw6BweNS6IckaP77w2DpxRizYWiX6ANAEzaqs4yKv+lRvBU9by*)
(*GeEWDrI63rQ2Sb6ArnicDsnmYNFK/UajQNnWMeH7Dg6KPJ0GV0RZ19IYkcOB*)
(*uuXVjSJxCKCjSeHeJziQllQ+0POUE4y8yx0OTvU+rWiQfAC5Z1+c94GD647A*)
(*eykC5UeLnSt8MHTGqQL7ZG9AB+7lLYvEMGdoZkuhSLmzvPR4Ioavxd3qUJ7y*)
(*o/K1Uzdg8E+NtNZLGkAxzplmqs+fI+cqkgXKCcnS8q0YWo6/6uqRBwMqWV0T*)
(*nobheXWBNZSnXKlaOGwuhkbvl8tSBC9a73ZiwhgMn+Nvi6In1Uf4fOk3B6Xx*)
(*6RcapEGAWjPHio0cdFuj/V0QZc2X5N4LHJiKpw4O49WAFoREy/s5eBw0okoQ*)
(*BtL67COvZnHgdahttih6KGyafL/zoeSu7HeOrWzvk92U86J2HYifwrsp9xV8*)
(*OT9NEAYo7xlVy522iK7Ke/20xvmNkkqpRxs1Rq1CKqXenyVlByP4/ooed91z*)
(*jxmFfopevrk+H0+KLoqehrNucU0SUvTW6B0N/RFS+uFoOvIjVegjrF+Jd4px*)
(*vdRLWD+vzB7xTsf3ENbv0vgcTa74mzA/pG0N2PdJdhDmlwL34PGxQjdhfnop*)
(*r8ssk7oI89vZy8NtQfwPwvxoS7eOM4t2wvwKXpdf2+VvhPk5bHLkzSapgzC/*)
(*x5w/U1wmthOWh9BDZr/DwnvC8lJ9zXXTGv4tYXm6WZuTF41aCcubrt2wbYLl*)
(*GWF5TLh6/2AF30xYXk/dKn22wlJPWJ5RmsG23lJFWN7/fwMfs26x/+AvHtmh*)
(*cw==*)
(*"]], Inset[$CellContext`Z, {3.25, 9.0548}, ImageScaled[{Rational[1, 2], 1}]], Line[CompressedData["*)
(*1:eJwt1A1Qk3UcB/AH9PKlQMSrC7gIYggN9iKzbWDkgy+Iim5sqPMQ2nEg66Yw*)
(*QRA4sRkSEyoQGkOOK9CKDXyZkxXWIVzAKJgNkJfVHbvhCzHDOnJtxIv0farn*)
(*bvve5777/V92uwWn54gyPQmC2IMXlf+90Uni3yeSJMhxqSgGlnRby8YiSCKP*)
(*Rn8/D7anFm24Dfcbw6tbqV584fDn8Nv9XtM2WFi4z/gZvLjkI/JBsp25eQak*)
(*zZL/2nak3nv7JSvywOO7y3KqN0z7vYT9Kofz3RpYEmv+Yzccrti2tReWRbbO*)
(*lcC9o/aROVgYXrn3Gpz40Tt3gpGqiKs3B5Dc33g5yUi7edPJMeSOQNvaCmSH*)
(*h8s5iJwq4YV8j+z+Zc2iHllcxnIuUvM/HFlNfa75dKCcy8B5MqylabDL99ox*)
(*BSyZLWjhwjd3LRu0sLDXb40/zMiSN9hgBadX7g3T3qq9upGJ+w1FT70ChzLI*)
(*sTh4qOocnwNXFInacmBV4vENUrgr4J5nI5xwbClFA9uydrj6YUWxOHMYDpqU*)
(*6J2wsM7g8QL2Uad+uy2EhfU3O6YZcHcQXZIMC6/oxnfCP0l1KSq4iagt2A27*)
(*J5ppXbBPAO0WBz5iFLw+Dyskz6t84aSRPqYfmySkH1aoZ7DfV9HVaVy4fqVP*)
(*3wlLk8kXxTA/Jd11OZJUFt+N+DobJkvviy+gP9O+ef1FuDD4wanz6K0ZfEcz*)
(*zP7E4KxBzx48ceoObH8Syv8G/Ypu2W2BZR+fqXXAPbyGxcfU/OxWvxAGqTz3*)
(*u6fPErV+S2t7Cs53vjPO5LsF528aL6hET1eI1HRYxts72QZnLi+ZdsIzjUdt*)
(*HXBApUueBnff+KDqFubl/lFHi2FiNnh/PfqzSRamBu6ILhPkop+ovZjVDusH*)
(*p6bj4YG0iOxhylv+jgmEj6+rMT6F6/U9qwnMx+ZmWNZGYb2zcQf/xPnrhkdL*)
(*3oDDy7RF87jvRlZNZww8379gxPerLHcKTEmwkpXvjqXu477nIYNlD73zT6Nf*)
(*0I4slMAK8XjdDfSlv47GfwrPrLcwH6HXjDw92QYntPz1oxd+B7yuudEeuEkQ*)
(*+TCUSSo5Ldzrk7DqZ53pTfQFii9m56l5u9DDH257cEjwMgfnS121bwHrFa4q*)
(*T4+CEybqZ4Zgf6G3RgALta/e/hL7e6mzJCfg6gDbgBLuetZAU8Eqc+OB92Dr*)
(*nrDmKzA/jMXOwPwTskb3HSwJezc3D/1UYvyh+7A2u9ysRu/oMz9zwEGXDj4y*)
(*wc9X8HD+/39hkv8As5asQQ==*)
(*"]], Inset[$CellContext`Z, {16.280796897832168`, 15.429621715174807`}, ImageScaled[{Rational[1, 2], 0}]], Line[CompressedData["*)
(*1:eJwl1AtMU1cYB/ALLcjAYJ1t7RRsx3OhgK0wjGws1wd0jlm7IRWRSgttFzRi*)
(*bfcw05BiNDKRUaAMCU6pCxAQtJ2ZTHwMp2MdStbSVZhkusgjFCmtDl9Bcf+z*)
(*neTem1++x/lycu99s2jPx5pAiqIkuMjz/1sCTf23eDQV/amWlwjLor61Vy2l*)
(*KYnWkLsNpv8KbxTCCWUjnRWwVXp1bIJLU5k625UeWCfdobTDlbz+Cz6Y1bim*)
(*aRCecIs2xwtpSrD6cn0A6u8WjjqVcLNMrM2G33j+ML0JNp0VOy1wR/oWlRuW*)
(*9QSbZ+F4fndceCJNOa5ZAlMx33e22pYNMOt8TlUOXFseId8H06fy+rLh8fyY*)
(*mHbYJH1QtxxmiOX6IVLPOamwo1/Y2fxQZhJN+Vs7kxQwa7H6xUrY0Rc5NYl5*)
(*57hKQwFMFU7N7IddlpFDR0l+9eNdsfB4gZx5CZaFfvSPn0NT1ctXq2ZI/rOD*)
(*J8bgBO+IOSYZ86iOvEYh37iuPVoBK4sfffYuXDA8LWogbrg+UAXPF6hOO2Gj*)
(*IfTUGJxxs2lT2ErsN7T+GxHmc3lef2s9bFQE8YthYVCwfh/sWHP+8l74Kn+7*)
(*u4O4tLZnKzx/6IDhDiyiu7xc+MDilz6GCPXS3JJu9M+ya5jLYP9wyaosmJIH*)
(*hohgxzJfnAvz30yt+zqL2J2tK+XQRlHTr9wdsDJC44lF/KvsPx9/TurX3RgN*)
(*RNy49o+NNaR/cFtbCNx+5qfZTlh34cZ0Cpzft8fyGyx6VanaT+JJ6c4JuHch*)
(*R+iEt8Xm+Rlimvrb+oJPzpfJPrdbAAvS3i/6BDZvFg6+A9MBcd6jcItvypUL*)
(*N2/I6TrCpY1vO6OKSmHlpsGB7fCuGlfbYRJP1DAWIp976y7zBOk/29p/Gvvx*)
(*9Y3zNtj047PuZDjtujzBLibv660VvWya0nsKOfdIPf+lTguXatKSn5L+FsPP*)
(*SWzaqP+w/3vWKvTTVgxFwF2K8lYh3FyREp6K/MNP2p0S2Drsle6GL/6i61fD*)
(*Srt1wRXkSzImgw7CrJpzIWyc56K16pmTsPGHRU9UmGfSb5rrgXWMaU8DXBKq*)
(*trvh3sb4Ox3Id19URvtgwdaswnq4rb6auyCFfE/cujz4oeT+eCTsyNsyOof9*)
(*05ZmMlNh65fH71XCxVWCgA9gpXoghQdzeZl7lbAssizKtoSmysqf3v6CxCfH*)
(*J9RLcJ4Rz4+ZSH+/eWcGLGh50N1B+rWGdL0HX/PupPvg3kuPVmhR7zH/fvs+*)
(*yTfaws7Ar8iCWeT/wqb/BeeBmPU=*)
(*"]], Inset[$CellContext`Z, {16.280796897832168`, 4.570378284825192}, ImageScaled[{Rational[1, 2], 1}]], Line[CompressedData["*)
(*1:eJwt1A9Qk3UYB/AXtJMRLCgkgiFQMpmrWDFhh0CvCAoI5/QWAle2JhY2QErS*)
(*pRysRKDyZAeIoPyZAweCuBcjpMWfpc4yOEQ6EAlhnvwZhkU4byPmXd9f13v3*)
(*vu997nl+7+/P89wbIDu4e78jRVFJuMmbciKPjTT13/U6TaklpcM/8miK6Wht*)
(*cYElTadWjsIZPa3zL8EREZ++R8PSUvcX3OEx9rjdFRY3MoNkvPSTwuvTQTRV*)
(*pa5rf8BHvv675BvwYnNBfAc8YT4W1AoL2Gc5x+CFRyFtlXDGc/KkcDgvur7j*)
(*OImHj7CsWJci89y9bFis+GGkE1bVsJYlMMXpv5UP3xjS1YfAXYHsB7the4+T*)
(*lAWXxMqzNsFxnNjVoxtoKiUq6yYXNrGaTp+FnWZiIwPhodWholS460jvnjfg*)
(*dNu1KQ/4F5dVCeFwzni+eYhLU6L4+rptsM0af14FL9YwTxPJuXFG7ybDKeLA*)
(*n+Lgxv7PDwfCiuVt06GwpXHNlmeBmCex298T5mzh6+/DSoFX9BzOTTl5QGiE*)
(*VQmUYxusblp2aSfeq2+Wwycj3Z3q4GZF8JEgeOAC71oJHCd89KcZ+4xp0XGz*)
(*YWYlwv0KvCCf+30HyV9Y0hXDkg/fOe0PVzHFc3K4+9BtemE9zmGdcGQvrJ74*)
(*lcUQzz8uIzaJS4OzYf+q2S8+JnWo+aOTBzNjOZpcuIK7j5p9DfvWDZQqyfyP*)
(*Z3ZpYa+o1MUTsK2fn54FVxWZ9pA4rZCtioQZe8F4JizsEA2thUU8+2cJpC80*)
(*D3nWV2nKEFbA9SZ1m/aLmYKpDyZNk6hD4Rn+R/1wyqF4di2cJ/ZJ6CL5AelU*)
(*Gmx/mF+qgQWR/4T4wKoi7VvFsGLjgGoGdaj5LfXvfXBGal9PN+zl+01DGBlf*)
(*YVFr4Ezj5jRH2JQd43sGbp5ylRkDsI+yzZ5V8KWfOz2Ow+b85/Xn4YhnuqIo*)
(*mHm6a42OfH9Z4rPij/VVHsjTw4bqMHY3bONvH+uFcytnHQphpkw5fBWu0Dc0*)
(*SGDlyZ3tZH4DtyXnTZje6nwxHxa+73rBDTYoHGyJJM64ty35IS606N24pF94*)
(*B/thkzdbOUj6SMt90giLUtOavoXVT+K+VMDU/gJtEuxUWf02TfIvnoh/BTa/*)
(*qOmzrkNfjw47L5G6l+/wrIXd+pKr78FeFplGAKdMVdTehsXS0ajvfeFb54x3*)
(*4Li77XeCYfPR7Z4TJH/6a7WWg3MrZw7Pw6LywVx/2NTXIbPAiw5rLzf4ID+U*)
(*f8kKl3x1X7YJLhElC/6Cu25KL49501SQrDdjDFYnGbeegg1+2utXYIOw/l0J*)
(*bH555WoB6UuRxwgXTrE4l0XDbuS/5f3//2s9/S+cWbuk*)
(*"]], Inset[$CellContext`Z, {9.332600380984282, 13.056274380900543`}, ImageScaled[{1, 0}]], {Dashing[{0.030000000000000002`, 0.030000000000000002`}], Line[{{6.500000000001819, 9.999999999996362}, {12.999999999998181`, 6.}}]}, Inset[$CellContext`h2, {9.463624737400366, 7.156640198275594}, ImageScaled[{1, 1}]], {Dashing[{0.030000000000000002`, 0.030000000000000002`}], Line[{{13., 13.999999999996009`}, {13., 5.99999999999511}}]}, Inset[$CellContext`h1, {13.8993, 10.}, ImageScaled[{0, Rational[1, 2]}]], {PointSize[0.04], Point[{6.5, 10.}], Point[{13., 14.}], Point[{13., 6.}]}, Inset[2, {10., -0.5}, ImageScaled[{Rational[1, 2], 0}]]}, "ad/becf/dedfef.m", TooltipStyle -> "TextStyling"], "ad/becf/dedfef.m", "Tooltip"], AspectRatio -> 1, PlotRange -> {{-1, 21}, {-1, 21}}], {22, 0}, {0, 0}, {22, 22}], Inset[Graphics[Annotation[Tooltip[{Thickness[0.005], Line[CompressedData["*)
(*1:eJw9lAlIVEEYx8daU9u01TzWyq431WIZ1nNFUpivU+mQLASrNUrxobWh3VvZ*)
(*IYXpFpZZ1tuygwqtoGM90k4pS6UnlhZuF2nRYVm5ZW6t5msK5g0Mww+Gmfn+*)
(*3/8/o5PSF6b0QwhNp/Pfavwp0xEI6P/A8GTLiYmFIuUMx4vNwRjMsVPsE3nK*)
(*daPLxyzAkJTfXFgjaQEt180qN2J4HxQckyhQrjQP6MrEsLN5kpdDDgCktYPb*)
(*Dsr6kY/zRcoJe3db12L4LsYmhvCUIWz30sUYYjepV9VL/oBspj1tegxx2aaS*)
(*FIFy3WHPOYMw6BweNS6IckaP77w2DpxRizYWiX6ANAEzaqs4yKv+lRvBU9by*)
(*GeEWDrI63rQ2Sb6ArnicDsnmYNFK/UajQNnWMeH7Dg6KPJ0GV0RZ19IYkcOB*)
(*uuXVjSJxCKCjSeHeJziQllQ+0POUE4y8yx0OTvU+rWiQfAC5Z1+c94GD647A*)
(*eykC5UeLnSt8MHTGqQL7ZG9AB+7lLYvEMGdoZkuhSLmzvPR4Ioavxd3qUJ7y*)
(*o/K1Uzdg8E+NtNZLGkAxzplmqs+fI+cqkgXKCcnS8q0YWo6/6uqRBwMqWV0T*)
(*nobheXWBNZSnXKlaOGwuhkbvl8tSBC9a73ZiwhgMn+Nvi6In1Uf4fOk3B6Xx*)
(*6RcapEGAWjPHio0cdFuj/V0QZc2X5N4LHJiKpw4O49WAFoREy/s5eBw0okoQ*)
(*BtL67COvZnHgdahttih6KGyafL/zoeSu7HeOrWzvk92U86J2HYifwrsp9xV8*)
(*OT9NEAYo7xlVy522iK7Ke/20xvmNkkqpRxs1Rq1CKqXenyVlByP4/ooed91z*)
(*jxmFfopevrk+H0+KLoqehrNucU0SUvTW6B0N/RFS+uFoOvIjVegjrF+Jd4px*)
(*vdRLWD+vzB7xTsf3ENbv0vgcTa74mzA/pG0N2PdJdhDmlwL34PGxQjdhfnop*)
(*r8ssk7oI89vZy8NtQfwPwvxoS7eOM4t2wvwKXpdf2+VvhPk5bHLkzSapgzC/*)
(*x5w/U1wmthOWh9BDZr/DwnvC8lJ9zXXTGv4tYXm6WZuTF41aCcubrt2wbYLl*)
(*GWF5TLh6/2AF30xYXk/dKn22wlJPWJ5RmsG23lJFWN7/fwMfs26x/+AvHtmh*)
(*cw==*)
(*"]], Inset[$CellContext`Z, {3.25, 9.0548}, ImageScaled[{Rational[1, 2], 1}]], Line[CompressedData["*)
(*1:eJwt1A1Qk3UcB/AH9PKlQMSrC7gIYggN9iKzbWDkgy+Iim5sqPMQ2nEg66Yw*)
(*QRA4sRkSEyoQGkOOK9CKDXyZkxXWIVzAKJgNkJfVHbvhCzHDOnJtxIv0farn*)
(*bvve5777/V92uwWn54gyPQmC2IMXlf+90Uni3yeSJMhxqSgGlnRby8YiSCKP*)
(*Rn8/D7anFm24Dfcbw6tbqV584fDn8Nv9XtM2WFi4z/gZvLjkI/JBsp25eQak*)
(*zZL/2nak3nv7JSvywOO7y3KqN0z7vYT9Kofz3RpYEmv+Yzccrti2tReWRbbO*)
(*lcC9o/aROVgYXrn3Gpz40Tt3gpGqiKs3B5Dc33g5yUi7edPJMeSOQNvaCmSH*)
(*h8s5iJwq4YV8j+z+Zc2iHllcxnIuUvM/HFlNfa75dKCcy8B5MqylabDL99ox*)
(*BSyZLWjhwjd3LRu0sLDXb40/zMiSN9hgBadX7g3T3qq9upGJ+w1FT70ChzLI*)
(*sTh4qOocnwNXFInacmBV4vENUrgr4J5nI5xwbClFA9uydrj6YUWxOHMYDpqU*)
(*6J2wsM7g8QL2Uad+uy2EhfU3O6YZcHcQXZIMC6/oxnfCP0l1KSq4iagt2A27*)
(*J5ppXbBPAO0WBz5iFLw+Dyskz6t84aSRPqYfmySkH1aoZ7DfV9HVaVy4fqVP*)
(*3wlLk8kXxTA/Jd11OZJUFt+N+DobJkvviy+gP9O+ef1FuDD4wanz6K0ZfEcz*)
(*zP7E4KxBzx48ceoObH8Syv8G/Ypu2W2BZR+fqXXAPbyGxcfU/OxWvxAGqTz3*)
(*u6fPErV+S2t7Cs53vjPO5LsF528aL6hET1eI1HRYxts72QZnLi+ZdsIzjUdt*)
(*HXBApUueBnff+KDqFubl/lFHi2FiNnh/PfqzSRamBu6ILhPkop+ovZjVDusH*)
(*p6bj4YG0iOxhylv+jgmEj6+rMT6F6/U9qwnMx+ZmWNZGYb2zcQf/xPnrhkdL*)
(*3oDDy7RF87jvRlZNZww8379gxPerLHcKTEmwkpXvjqXu477nIYNlD73zT6Nf*)
(*0I4slMAK8XjdDfSlv47GfwrPrLcwH6HXjDw92QYntPz1oxd+B7yuudEeuEkQ*)
(*+TCUSSo5Ldzrk7DqZ53pTfQFii9m56l5u9DDH257cEjwMgfnS121bwHrFa4q*)
(*T4+CEybqZ4Zgf6G3RgALta/e/hL7e6mzJCfg6gDbgBLuetZAU8Eqc+OB92Dr*)
(*nrDmKzA/jMXOwPwTskb3HSwJezc3D/1UYvyh+7A2u9ysRu/oMz9zwEGXDj4y*)
(*wc9X8HD+/39hkv8As5asQQ==*)
(*"]], Inset[$CellContext`Z, {16.280796897832168`, 15.429621715174807`}, ImageScaled[{Rational[1, 2], 0}]], Line[CompressedData["*)
(*1:eJwl1AtMU1cYB/ALLcjAYJ1t7RRsx3OhgK0wjGws1wd0jlm7IRWRSgttFzRi*)
(*bfcw05BiNDKRUaAMCU6pCxAQtJ2ZTHwMp2MdStbSVZhkusgjFCmtDl9Bcf+z*)
(*neTem1++x/lycu99s2jPx5pAiqIkuMjz/1sCTf23eDQV/amWlwjLor61Vy2l*)
(*KYnWkLsNpv8KbxTCCWUjnRWwVXp1bIJLU5k625UeWCfdobTDlbz+Cz6Y1bim*)
(*aRCecIs2xwtpSrD6cn0A6u8WjjqVcLNMrM2G33j+ML0JNp0VOy1wR/oWlRuW*)
(*9QSbZ+F4fndceCJNOa5ZAlMx33e22pYNMOt8TlUOXFseId8H06fy+rLh8fyY*)
(*mHbYJH1QtxxmiOX6IVLPOamwo1/Y2fxQZhJN+Vs7kxQwa7H6xUrY0Rc5NYl5*)
(*57hKQwFMFU7N7IddlpFDR0l+9eNdsfB4gZx5CZaFfvSPn0NT1ctXq2ZI/rOD*)
(*J8bgBO+IOSYZ86iOvEYh37iuPVoBK4sfffYuXDA8LWogbrg+UAXPF6hOO2Gj*)
(*IfTUGJxxs2lT2ErsN7T+GxHmc3lef2s9bFQE8YthYVCwfh/sWHP+8l74Kn+7*)
(*u4O4tLZnKzx/6IDhDiyiu7xc+MDilz6GCPXS3JJu9M+ya5jLYP9wyaosmJIH*)
(*hohgxzJfnAvz30yt+zqL2J2tK+XQRlHTr9wdsDJC44lF/KvsPx9/TurX3RgN*)
(*RNy49o+NNaR/cFtbCNx+5qfZTlh34cZ0Cpzft8fyGyx6VanaT+JJ6c4JuHch*)
(*R+iEt8Xm+Rlimvrb+oJPzpfJPrdbAAvS3i/6BDZvFg6+A9MBcd6jcItvypUL*)
(*N2/I6TrCpY1vO6OKSmHlpsGB7fCuGlfbYRJP1DAWIp976y7zBOk/29p/Gvvx*)
(*9Y3zNtj047PuZDjtujzBLibv660VvWya0nsKOfdIPf+lTguXatKSn5L+FsPP*)
(*SWzaqP+w/3vWKvTTVgxFwF2K8lYh3FyREp6K/MNP2p0S2Drsle6GL/6i61fD*)
(*Srt1wRXkSzImgw7CrJpzIWyc56K16pmTsPGHRU9UmGfSb5rrgXWMaU8DXBKq*)
(*trvh3sb4Ox3Id19URvtgwdaswnq4rb6auyCFfE/cujz4oeT+eCTsyNsyOof9*)
(*05ZmMlNh65fH71XCxVWCgA9gpXoghQdzeZl7lbAssizKtoSmysqf3v6CxCfH*)
(*J9RLcJ4Rz4+ZSH+/eWcGLGh50N1B+rWGdL0HX/PupPvg3kuPVmhR7zH/fvs+*)
(*yTfaws7Ar8iCWeT/wqb/BeeBmPU=*)
(*"]], Inset[$CellContext`Z, {16.280796897832168`, 4.570378284825192}, ImageScaled[{Rational[1, 2], 1}]], Line[CompressedData["*)
(*1:eJwt1A9Qk3UYB/AXtJMRLCgkgiFQMpmrWDFhh0CvCAoI5/QWAle2JhY2QErS*)
(*pRysRKDyZAeIoPyZAweCuBcjpMWfpc4yOEQ6EAlhnvwZhkU4byPmXd9f13v3*)
(*vu997nl+7+/P89wbIDu4e78jRVFJuMmbciKPjTT13/U6TaklpcM/8miK6Wht*)
(*cYElTadWjsIZPa3zL8EREZ++R8PSUvcX3OEx9rjdFRY3MoNkvPSTwuvTQTRV*)
(*pa5rf8BHvv675BvwYnNBfAc8YT4W1AoL2Gc5x+CFRyFtlXDGc/KkcDgvur7j*)
(*OImHj7CsWJci89y9bFis+GGkE1bVsJYlMMXpv5UP3xjS1YfAXYHsB7the4+T*)
(*lAWXxMqzNsFxnNjVoxtoKiUq6yYXNrGaTp+FnWZiIwPhodWholS460jvnjfg*)
(*dNu1KQ/4F5dVCeFwzni+eYhLU6L4+rptsM0af14FL9YwTxPJuXFG7ybDKeLA*)
(*n+Lgxv7PDwfCiuVt06GwpXHNlmeBmCex298T5mzh6+/DSoFX9BzOTTl5QGiE*)
(*VQmUYxusblp2aSfeq2+Wwycj3Z3q4GZF8JEgeOAC71oJHCd89KcZ+4xp0XGz*)
(*YWYlwv0KvCCf+30HyV9Y0hXDkg/fOe0PVzHFc3K4+9BtemE9zmGdcGQvrJ74*)
(*lcUQzz8uIzaJS4OzYf+q2S8+JnWo+aOTBzNjOZpcuIK7j5p9DfvWDZQqyfyP*)
(*Z3ZpYa+o1MUTsK2fn54FVxWZ9pA4rZCtioQZe8F4JizsEA2thUU8+2cJpC80*)
(*D3nWV2nKEFbA9SZ1m/aLmYKpDyZNk6hD4Rn+R/1wyqF4di2cJ/ZJ6CL5AelU*)
(*Gmx/mF+qgQWR/4T4wKoi7VvFsGLjgGoGdaj5LfXvfXBGal9PN+zl+01DGBlf*)
(*YVFr4Ezj5jRH2JQd43sGbp5ylRkDsI+yzZ5V8KWfOz2Ow+b85/Xn4YhnuqIo*)
(*mHm6a42OfH9Z4rPij/VVHsjTw4bqMHY3bONvH+uFcytnHQphpkw5fBWu0Dc0*)
(*SGDlyZ3tZH4DtyXnTZje6nwxHxa+73rBDTYoHGyJJM64ty35IS606N24pF94*)
(*B/thkzdbOUj6SMt90giLUtOavoXVT+K+VMDU/gJtEuxUWf02TfIvnoh/BTa/*)
(*qOmzrkNfjw47L5G6l+/wrIXd+pKr78FeFplGAKdMVdTehsXS0ajvfeFb54x3*)
(*4Li77XeCYfPR7Z4TJH/6a7WWg3MrZw7Pw6LywVx/2NTXIbPAiw5rLzf4ID+U*)
(*f8kKl3x1X7YJLhElC/6Cu25KL49501SQrDdjDFYnGbeegg1+2utXYIOw/l0J*)
(*bH555WoB6UuRxwgXTrE4l0XDbuS/5f3//2s9/S+cWbuk*)
(*"]], Inset[$CellContext`Z, {9.332600380984282, 13.056274380900543`}, ImageScaled[{1, 0}]], {Dashing[{0.030000000000000002`, 0.030000000000000002`}], Line[{{6.500000000001819, 9.999999999996362}, {12.999999999998181`, 6.}}]}, Inset[$CellContext`h1, {9.463624737400366, 7.156640198275594}, ImageScaled[{1, 1}]], {Dashing[{0.030000000000000002`, 0.030000000000000002`}], Line[{{13., 13.999999999996009`}, {13., 5.99999999999511}}]}, Inset[$CellContext`h3, {13.8993, 10.}, ImageScaled[{0, Rational[1, 2]}]], {PointSize[0.04], Point[{6.5, 10.}], Point[{13., 14.}], Point[{13., 6.}]}, Inset[3, {10., -0.5}, ImageScaled[{Rational[1, 2], 0}]]}, "ad/becf/dedfef.m", TooltipStyle -> "TextStyling"], "ad/becf/dedfef.m", "Tooltip"], AspectRatio -> 1, PlotRange -> {{-1, 21}, {-1, 21}}], {44, 0}, {0, 0}, {22, 22}], Inset[Graphics[Annotation[Tooltip[{Thickness[0.005], Line[CompressedData["*)
(*1:eJw9lAlIVEEYx8daU9u01TzWyq431WIZ1nNFUpivU+mQLASrNUrxobWh3VvZ*)
(*IYXpFpZZ1tuygwqtoGM90k4pS6UnlhZuF2nRYVm5ZW6t5msK5g0Mww+Gmfn+*)
(*3/8/o5PSF6b0QwhNp/Pfavwp0xEI6P/A8GTLiYmFIuUMx4vNwRjMsVPsE3nK*)
(*daPLxyzAkJTfXFgjaQEt180qN2J4HxQckyhQrjQP6MrEsLN5kpdDDgCktYPb*)
(*Dsr6kY/zRcoJe3db12L4LsYmhvCUIWz30sUYYjepV9VL/oBspj1tegxx2aaS*)
(*FIFy3WHPOYMw6BweNS6IckaP77w2DpxRizYWiX6ANAEzaqs4yKv+lRvBU9by*)
(*GeEWDrI63rQ2Sb6ArnicDsnmYNFK/UajQNnWMeH7Dg6KPJ0GV0RZ19IYkcOB*)
(*uuXVjSJxCKCjSeHeJziQllQ+0POUE4y8yx0OTvU+rWiQfAC5Z1+c94GD647A*)
(*eykC5UeLnSt8MHTGqQL7ZG9AB+7lLYvEMGdoZkuhSLmzvPR4Ioavxd3qUJ7y*)
(*o/K1Uzdg8E+NtNZLGkAxzplmqs+fI+cqkgXKCcnS8q0YWo6/6uqRBwMqWV0T*)
(*nobheXWBNZSnXKlaOGwuhkbvl8tSBC9a73ZiwhgMn+Nvi6In1Uf4fOk3B6Xx*)
(*6RcapEGAWjPHio0cdFuj/V0QZc2X5N4LHJiKpw4O49WAFoREy/s5eBw0okoQ*)
(*BtL67COvZnHgdahttih6KGyafL/zoeSu7HeOrWzvk92U86J2HYifwrsp9xV8*)
(*OT9NEAYo7xlVy522iK7Ke/20xvmNkkqpRxs1Rq1CKqXenyVlByP4/ooed91z*)
(*jxmFfopevrk+H0+KLoqehrNucU0SUvTW6B0N/RFS+uFoOvIjVegjrF+Jd4px*)
(*vdRLWD+vzB7xTsf3ENbv0vgcTa74mzA/pG0N2PdJdhDmlwL34PGxQjdhfnop*)
(*r8ssk7oI89vZy8NtQfwPwvxoS7eOM4t2wvwKXpdf2+VvhPk5bHLkzSapgzC/*)
(*x5w/U1wmthOWh9BDZr/DwnvC8lJ9zXXTGv4tYXm6WZuTF41aCcubrt2wbYLl*)
(*GWF5TLh6/2AF30xYXk/dKn22wlJPWJ5RmsG23lJFWN7/fwMfs26x/+AvHtmh*)
(*cw==*)
(*"]], Inset[$CellContext`Z, {3.25, 9.0548}, ImageScaled[{Rational[1, 2], 1}]], Line[CompressedData["*)
(*1:eJwt1A1Qk3UcB/AH9PKlQMSrC7gIYggN9iKzbWDkgy+Iim5sqPMQ2nEg66Yw*)
(*QRA4sRkSEyoQGkOOK9CKDXyZkxXWIVzAKJgNkJfVHbvhCzHDOnJtxIv0farn*)
(*bvve5777/V92uwWn54gyPQmC2IMXlf+90Uni3yeSJMhxqSgGlnRby8YiSCKP*)
(*Rn8/D7anFm24Dfcbw6tbqV584fDn8Nv9XtM2WFi4z/gZvLjkI/JBsp25eQak*)
(*zZL/2nak3nv7JSvywOO7y3KqN0z7vYT9Kofz3RpYEmv+Yzccrti2tReWRbbO*)
(*lcC9o/aROVgYXrn3Gpz40Tt3gpGqiKs3B5Dc33g5yUi7edPJMeSOQNvaCmSH*)
(*h8s5iJwq4YV8j+z+Zc2iHllcxnIuUvM/HFlNfa75dKCcy8B5MqylabDL99ox*)
(*BSyZLWjhwjd3LRu0sLDXb40/zMiSN9hgBadX7g3T3qq9upGJ+w1FT70ChzLI*)
(*sTh4qOocnwNXFInacmBV4vENUrgr4J5nI5xwbClFA9uydrj6YUWxOHMYDpqU*)
(*6J2wsM7g8QL2Uad+uy2EhfU3O6YZcHcQXZIMC6/oxnfCP0l1KSq4iagt2A27*)
(*J5ppXbBPAO0WBz5iFLw+Dyskz6t84aSRPqYfmySkH1aoZ7DfV9HVaVy4fqVP*)
(*3wlLk8kXxTA/Jd11OZJUFt+N+DobJkvviy+gP9O+ef1FuDD4wanz6K0ZfEcz*)
(*zP7E4KxBzx48ceoObH8Syv8G/Ypu2W2BZR+fqXXAPbyGxcfU/OxWvxAGqTz3*)
(*u6fPErV+S2t7Cs53vjPO5LsF528aL6hET1eI1HRYxts72QZnLi+ZdsIzjUdt*)
(*HXBApUueBnff+KDqFubl/lFHi2FiNnh/PfqzSRamBu6ILhPkop+ovZjVDusH*)
(*p6bj4YG0iOxhylv+jgmEj6+rMT6F6/U9qwnMx+ZmWNZGYb2zcQf/xPnrhkdL*)
(*3oDDy7RF87jvRlZNZww8379gxPerLHcKTEmwkpXvjqXu477nIYNlD73zT6Nf*)
(*0I4slMAK8XjdDfSlv47GfwrPrLcwH6HXjDw92QYntPz1oxd+B7yuudEeuEkQ*)
(*+TCUSSo5Ldzrk7DqZ53pTfQFii9m56l5u9DDH257cEjwMgfnS121bwHrFa4q*)
(*T4+CEybqZ4Zgf6G3RgALta/e/hL7e6mzJCfg6gDbgBLuetZAU8Eqc+OB92Dr*)
(*nrDmKzA/jMXOwPwTskb3HSwJezc3D/1UYvyh+7A2u9ysRu/oMz9zwEGXDj4y*)
(*wc9X8HD+/39hkv8As5asQQ==*)
(*"]], Inset[$CellContext`Z, {16.280796897832168`, 15.429621715174807`}, ImageScaled[{Rational[1, 2], 0}]], Line[CompressedData["*)
(*1:eJwl1AtMU1cYB/ALLcjAYJ1t7RRsx3OhgK0wjGws1wd0jlm7IRWRSgttFzRi*)
(*bfcw05BiNDKRUaAMCU6pCxAQtJ2ZTHwMp2MdStbSVZhkusgjFCmtDl9Bcf+z*)
(*neTem1++x/lycu99s2jPx5pAiqIkuMjz/1sCTf23eDQV/amWlwjLor61Vy2l*)
(*KYnWkLsNpv8KbxTCCWUjnRWwVXp1bIJLU5k625UeWCfdobTDlbz+Cz6Y1bim*)
(*aRCecIs2xwtpSrD6cn0A6u8WjjqVcLNMrM2G33j+ML0JNp0VOy1wR/oWlRuW*)
(*9QSbZ+F4fndceCJNOa5ZAlMx33e22pYNMOt8TlUOXFseId8H06fy+rLh8fyY*)
(*mHbYJH1QtxxmiOX6IVLPOamwo1/Y2fxQZhJN+Vs7kxQwa7H6xUrY0Rc5NYl5*)
(*57hKQwFMFU7N7IddlpFDR0l+9eNdsfB4gZx5CZaFfvSPn0NT1ctXq2ZI/rOD*)
(*J8bgBO+IOSYZ86iOvEYh37iuPVoBK4sfffYuXDA8LWogbrg+UAXPF6hOO2Gj*)
(*IfTUGJxxs2lT2ErsN7T+GxHmc3lef2s9bFQE8YthYVCwfh/sWHP+8l74Kn+7*)
(*u4O4tLZnKzx/6IDhDiyiu7xc+MDilz6GCPXS3JJu9M+ya5jLYP9wyaosmJIH*)
(*hohgxzJfnAvz30yt+zqL2J2tK+XQRlHTr9wdsDJC44lF/KvsPx9/TurX3RgN*)
(*RNy49o+NNaR/cFtbCNx+5qfZTlh34cZ0Cpzft8fyGyx6VanaT+JJ6c4JuHch*)
(*R+iEt8Xm+Rlimvrb+oJPzpfJPrdbAAvS3i/6BDZvFg6+A9MBcd6jcItvypUL*)
(*N2/I6TrCpY1vO6OKSmHlpsGB7fCuGlfbYRJP1DAWIp976y7zBOk/29p/Gvvx*)
(*9Y3zNtj047PuZDjtujzBLibv660VvWya0nsKOfdIPf+lTguXatKSn5L+FsPP*)
(*SWzaqP+w/3vWKvTTVgxFwF2K8lYh3FyREp6K/MNP2p0S2Drsle6GL/6i61fD*)
(*Srt1wRXkSzImgw7CrJpzIWyc56K16pmTsPGHRU9UmGfSb5rrgXWMaU8DXBKq*)
(*trvh3sb4Ox3Id19URvtgwdaswnq4rb6auyCFfE/cujz4oeT+eCTsyNsyOof9*)
(*05ZmMlNh65fH71XCxVWCgA9gpXoghQdzeZl7lbAssizKtoSmysqf3v6CxCfH*)
(*J9RLcJ4Rz4+ZSH+/eWcGLGh50N1B+rWGdL0HX/PupPvg3kuPVmhR7zH/fvs+*)
(*yTfaws7Ar8iCWeT/wqb/BeeBmPU=*)
(*"]], Inset[$CellContext`Z, {16.280796897832168`, 4.570378284825192}, ImageScaled[{Rational[1, 2], 1}]], Line[CompressedData["*)
(*1:eJwt1A9Qk3UYB/AXtJMRLCgkgiFQMpmrWDFhh0CvCAoI5/QWAle2JhY2QErS*)
(*pRysRKDyZAeIoPyZAweCuBcjpMWfpc4yOEQ6EAlhnvwZhkU4byPmXd9f13v3*)
(*vu997nl+7+/P89wbIDu4e78jRVFJuMmbciKPjTT13/U6TaklpcM/8miK6Wht*)
(*cYElTadWjsIZPa3zL8EREZ++R8PSUvcX3OEx9rjdFRY3MoNkvPSTwuvTQTRV*)
(*pa5rf8BHvv675BvwYnNBfAc8YT4W1AoL2Gc5x+CFRyFtlXDGc/KkcDgvur7j*)
(*OImHj7CsWJci89y9bFis+GGkE1bVsJYlMMXpv5UP3xjS1YfAXYHsB7the4+T*)
(*lAWXxMqzNsFxnNjVoxtoKiUq6yYXNrGaTp+FnWZiIwPhodWholS460jvnjfg*)
(*dNu1KQ/4F5dVCeFwzni+eYhLU6L4+rptsM0af14FL9YwTxPJuXFG7ybDKeLA*)
(*n+Lgxv7PDwfCiuVt06GwpXHNlmeBmCex298T5mzh6+/DSoFX9BzOTTl5QGiE*)
(*VQmUYxusblp2aSfeq2+Wwycj3Z3q4GZF8JEgeOAC71oJHCd89KcZ+4xp0XGz*)
(*YWYlwv0KvCCf+30HyV9Y0hXDkg/fOe0PVzHFc3K4+9BtemE9zmGdcGQvrJ74*)
(*lcUQzz8uIzaJS4OzYf+q2S8+JnWo+aOTBzNjOZpcuIK7j5p9DfvWDZQqyfyP*)
(*Z3ZpYa+o1MUTsK2fn54FVxWZ9pA4rZCtioQZe8F4JizsEA2thUU8+2cJpC80*)
(*D3nWV2nKEFbA9SZ1m/aLmYKpDyZNk6hD4Rn+R/1wyqF4di2cJ/ZJ6CL5AelU*)
(*Gmx/mF+qgQWR/4T4wKoi7VvFsGLjgGoGdaj5LfXvfXBGal9PN+zl+01DGBlf*)
(*YVFr4Ezj5jRH2JQd43sGbp5ylRkDsI+yzZ5V8KWfOz2Ow+b85/Xn4YhnuqIo*)
(*mHm6a42OfH9Z4rPij/VVHsjTw4bqMHY3bONvH+uFcytnHQphpkw5fBWu0Dc0*)
(*SGDlyZ3tZH4DtyXnTZje6nwxHxa+73rBDTYoHGyJJM64ty35IS606N24pF94*)
(*B/thkzdbOUj6SMt90giLUtOavoXVT+K+VMDU/gJtEuxUWf02TfIvnoh/BTa/*)
(*qOmzrkNfjw47L5G6l+/wrIXd+pKr78FeFplGAKdMVdTehsXS0ajvfeFb54x3*)
(*4Li77XeCYfPR7Z4TJH/6a7WWg3MrZw7Pw6LywVx/2NTXIbPAiw5rLzf4ID+U*)
(*f8kKl3x1X7YJLhElC/6Cu25KL49501SQrDdjDFYnGbeegg1+2utXYIOw/l0J*)
(*bH555WoB6UuRxwgXTrE4l0XDbuS/5f3//2s9/S+cWbuk*)
(*"]], Inset[$CellContext`Z, {9.332600380984282, 13.056274380900543`}, ImageScaled[{1, 0}]], {Dashing[{0.030000000000000002`, 0.030000000000000002`}], Line[{{6.500000000001819, 9.999999999996362}, {12.999999999998181`, 6.}}]}, Inset[$CellContext`h3, {9.463624737400366, 7.156640198275594}, ImageScaled[{1, 1}]], {Dashing[{0.030000000000000002`, 0.030000000000000002`}], Line[{{13., 13.999999999996009`}, {13., 5.99999999999511}}]}, Inset[$CellContext`h1, {13.8993, 10.}, ImageScaled[{0, Rational[1, 2]}]], {PointSize[0.04], Point[{6.5, 10.}], Point[{13., 14.}], Point[{13., 6.}]}, Inset[4, {10., -0.5}, ImageScaled[{Rational[1, 2], 0}]]}, "ad/becf/dedfef.m", TooltipStyle -> "TextStyling"], "ad/becf/dedfef.m", "Tooltip"], AspectRatio -> 1, PlotRange -> {{-1, 21}, {-1, 21}}], {66, 0}, {0, 0}, {22, 22}], Inset[Graphics[Annotation[Tooltip[{Thickness[0.005], Line[CompressedData["*)
(*1:eJw9lAlIVEEYx8daU9u01TzWyq431WIZ1nNFUpivU+mQLASrNUrxobWh3VvZ*)
(*IYXpFpZZ1tuygwqtoGM90k4pS6UnlhZuF2nRYVm5ZW6t5msK5g0Mww+Gmfn+*)
(*3/8/o5PSF6b0QwhNp/Pfavwp0xEI6P/A8GTLiYmFIuUMx4vNwRjMsVPsE3nK*)
(*daPLxyzAkJTfXFgjaQEt180qN2J4HxQckyhQrjQP6MrEsLN5kpdDDgCktYPb*)
(*Dsr6kY/zRcoJe3db12L4LsYmhvCUIWz30sUYYjepV9VL/oBspj1tegxx2aaS*)
(*FIFy3WHPOYMw6BweNS6IckaP77w2DpxRizYWiX6ANAEzaqs4yKv+lRvBU9by*)
(*GeEWDrI63rQ2Sb6ArnicDsnmYNFK/UajQNnWMeH7Dg6KPJ0GV0RZ19IYkcOB*)
(*uuXVjSJxCKCjSeHeJziQllQ+0POUE4y8yx0OTvU+rWiQfAC5Z1+c94GD647A*)
(*eykC5UeLnSt8MHTGqQL7ZG9AB+7lLYvEMGdoZkuhSLmzvPR4Ioavxd3qUJ7y*)
(*o/K1Uzdg8E+NtNZLGkAxzplmqs+fI+cqkgXKCcnS8q0YWo6/6uqRBwMqWV0T*)
(*nobheXWBNZSnXKlaOGwuhkbvl8tSBC9a73ZiwhgMn+Nvi6In1Uf4fOk3B6Xx*)
(*6RcapEGAWjPHio0cdFuj/V0QZc2X5N4LHJiKpw4O49WAFoREy/s5eBw0okoQ*)
(*BtL67COvZnHgdahttih6KGyafL/zoeSu7HeOrWzvk92U86J2HYifwrsp9xV8*)
(*OT9NEAYo7xlVy522iK7Ke/20xvmNkkqpRxs1Rq1CKqXenyVlByP4/ooed91z*)
(*jxmFfopevrk+H0+KLoqehrNucU0SUvTW6B0N/RFS+uFoOvIjVegjrF+Jd4px*)
(*vdRLWD+vzB7xTsf3ENbv0vgcTa74mzA/pG0N2PdJdhDmlwL34PGxQjdhfnop*)
(*r8ssk7oI89vZy8NtQfwPwvxoS7eOM4t2wvwKXpdf2+VvhPk5bHLkzSapgzC/*)
(*x5w/U1wmthOWh9BDZr/DwnvC8lJ9zXXTGv4tYXm6WZuTF41aCcubrt2wbYLl*)
(*GWF5TLh6/2AF30xYXk/dKn22wlJPWJ5RmsG23lJFWN7/fwMfs26x/+AvHtmh*)
(*cw==*)
(*"]], Inset[$CellContext`Z, {3.25, 9.0548}, ImageScaled[{Rational[1, 2], 1}]], Line[CompressedData["*)
(*1:eJwt1A1Qk3UcB/AH9PKlQMSrC7gIYggN9iKzbWDkgy+Iim5sqPMQ2nEg66Yw*)
(*QRA4sRkSEyoQGkOOK9CKDXyZkxXWIVzAKJgNkJfVHbvhCzHDOnJtxIv0farn*)
(*bvve5777/V92uwWn54gyPQmC2IMXlf+90Uni3yeSJMhxqSgGlnRby8YiSCKP*)
(*Rn8/D7anFm24Dfcbw6tbqV584fDn8Nv9XtM2WFi4z/gZvLjkI/JBsp25eQak*)
(*zZL/2nak3nv7JSvywOO7y3KqN0z7vYT9Kofz3RpYEmv+Yzccrti2tReWRbbO*)
(*lcC9o/aROVgYXrn3Gpz40Tt3gpGqiKs3B5Dc33g5yUi7edPJMeSOQNvaCmSH*)
(*h8s5iJwq4YV8j+z+Zc2iHllcxnIuUvM/HFlNfa75dKCcy8B5MqylabDL99ox*)
(*BSyZLWjhwjd3LRu0sLDXb40/zMiSN9hgBadX7g3T3qq9upGJ+w1FT70ChzLI*)
(*sTh4qOocnwNXFInacmBV4vENUrgr4J5nI5xwbClFA9uydrj6YUWxOHMYDpqU*)
(*6J2wsM7g8QL2Uad+uy2EhfU3O6YZcHcQXZIMC6/oxnfCP0l1KSq4iagt2A27*)
(*J5ppXbBPAO0WBz5iFLw+Dyskz6t84aSRPqYfmySkH1aoZ7DfV9HVaVy4fqVP*)
(*3wlLk8kXxTA/Jd11OZJUFt+N+DobJkvviy+gP9O+ef1FuDD4wanz6K0ZfEcz*)
(*zP7E4KxBzx48ceoObH8Syv8G/Ypu2W2BZR+fqXXAPbyGxcfU/OxWvxAGqTz3*)
(*u6fPErV+S2t7Cs53vjPO5LsF528aL6hET1eI1HRYxts72QZnLi+ZdsIzjUdt*)
(*HXBApUueBnff+KDqFubl/lFHi2FiNnh/PfqzSRamBu6ILhPkop+ovZjVDusH*)
(*p6bj4YG0iOxhylv+jgmEj6+rMT6F6/U9qwnMx+ZmWNZGYb2zcQf/xPnrhkdL*)
(*3oDDy7RF87jvRlZNZww8379gxPerLHcKTEmwkpXvjqXu477nIYNlD73zT6Nf*)
(*0I4slMAK8XjdDfSlv47GfwrPrLcwH6HXjDw92QYntPz1oxd+B7yuudEeuEkQ*)
(*+TCUSSo5Ldzrk7DqZ53pTfQFii9m56l5u9DDH257cEjwMgfnS121bwHrFa4q*)
(*T4+CEybqZ4Zgf6G3RgALta/e/hL7e6mzJCfg6gDbgBLuetZAU8Eqc+OB92Dr*)
(*nrDmKzA/jMXOwPwTskb3HSwJezc3D/1UYvyh+7A2u9ysRu/oMz9zwEGXDj4y*)
(*wc9X8HD+/39hkv8As5asQQ==*)
(*"]], Inset[$CellContext`Z, {16.280796897832168`, 15.429621715174807`}, ImageScaled[{Rational[1, 2], 0}]], Line[CompressedData["*)
(*1:eJwl1AtMU1cYB/ALLcjAYJ1t7RRsx3OhgK0wjGws1wd0jlm7IRWRSgttFzRi*)
(*bfcw05BiNDKRUaAMCU6pCxAQtJ2ZTHwMp2MdStbSVZhkusgjFCmtDl9Bcf+z*)
(*neTem1++x/lycu99s2jPx5pAiqIkuMjz/1sCTf23eDQV/amWlwjLor61Vy2l*)
(*KYnWkLsNpv8KbxTCCWUjnRWwVXp1bIJLU5k625UeWCfdobTDlbz+Cz6Y1bim*)
(*aRCecIs2xwtpSrD6cn0A6u8WjjqVcLNMrM2G33j+ML0JNp0VOy1wR/oWlRuW*)
(*9QSbZ+F4fndceCJNOa5ZAlMx33e22pYNMOt8TlUOXFseId8H06fy+rLh8fyY*)
(*mHbYJH1QtxxmiOX6IVLPOamwo1/Y2fxQZhJN+Vs7kxQwa7H6xUrY0Rc5NYl5*)
(*57hKQwFMFU7N7IddlpFDR0l+9eNdsfB4gZx5CZaFfvSPn0NT1ctXq2ZI/rOD*)
(*J8bgBO+IOSYZ86iOvEYh37iuPVoBK4sfffYuXDA8LWogbrg+UAXPF6hOO2Gj*)
(*IfTUGJxxs2lT2ErsN7T+GxHmc3lef2s9bFQE8YthYVCwfh/sWHP+8l74Kn+7*)
(*u4O4tLZnKzx/6IDhDiyiu7xc+MDilz6GCPXS3JJu9M+ya5jLYP9wyaosmJIH*)
(*hohgxzJfnAvz30yt+zqL2J2tK+XQRlHTr9wdsDJC44lF/KvsPx9/TurX3RgN*)
(*RNy49o+NNaR/cFtbCNx+5qfZTlh34cZ0Cpzft8fyGyx6VanaT+JJ6c4JuHch*)
(*R+iEt8Xm+Rlimvrb+oJPzpfJPrdbAAvS3i/6BDZvFg6+A9MBcd6jcItvypUL*)
(*N2/I6TrCpY1vO6OKSmHlpsGB7fCuGlfbYRJP1DAWIp976y7zBOk/29p/Gvvx*)
(*9Y3zNtj047PuZDjtujzBLibv660VvWya0nsKOfdIPf+lTguXatKSn5L+FsPP*)
(*SWzaqP+w/3vWKvTTVgxFwF2K8lYh3FyREp6K/MNP2p0S2Drsle6GL/6i61fD*)
(*Srt1wRXkSzImgw7CrJpzIWyc56K16pmTsPGHRU9UmGfSb5rrgXWMaU8DXBKq*)
(*trvh3sb4Ox3Id19URvtgwdaswnq4rb6auyCFfE/cujz4oeT+eCTsyNsyOof9*)
(*05ZmMlNh65fH71XCxVWCgA9gpXoghQdzeZl7lbAssizKtoSmysqf3v6CxCfH*)
(*J9RLcJ4Rz4+ZSH+/eWcGLGh50N1B+rWGdL0HX/PupPvg3kuPVmhR7zH/fvs+*)
(*yTfaws7Ar8iCWeT/wqb/BeeBmPU=*)
(*"]], Inset[$CellContext`Z, {16.280796897832168`, 4.570378284825192}, ImageScaled[{Rational[1, 2], 1}]], Line[CompressedData["*)
(*1:eJwt1A9Qk3UYB/AXtJMRLCgkgiFQMpmrWDFhh0CvCAoI5/QWAle2JhY2QErS*)
(*pRysRKDyZAeIoPyZAweCuBcjpMWfpc4yOEQ6EAlhnvwZhkU4byPmXd9f13v3*)
(*vu997nl+7+/P89wbIDu4e78jRVFJuMmbciKPjTT13/U6TaklpcM/8miK6Wht*)
(*cYElTadWjsIZPa3zL8EREZ++R8PSUvcX3OEx9rjdFRY3MoNkvPSTwuvTQTRV*)
(*pa5rf8BHvv675BvwYnNBfAc8YT4W1AoL2Gc5x+CFRyFtlXDGc/KkcDgvur7j*)
(*OImHj7CsWJci89y9bFis+GGkE1bVsJYlMMXpv5UP3xjS1YfAXYHsB7the4+T*)
(*lAWXxMqzNsFxnNjVoxtoKiUq6yYXNrGaTp+FnWZiIwPhodWholS460jvnjfg*)
(*dNu1KQ/4F5dVCeFwzni+eYhLU6L4+rptsM0af14FL9YwTxPJuXFG7ybDKeLA*)
(*n+Lgxv7PDwfCiuVt06GwpXHNlmeBmCex298T5mzh6+/DSoFX9BzOTTl5QGiE*)
(*VQmUYxusblp2aSfeq2+Wwycj3Z3q4GZF8JEgeOAC71oJHCd89KcZ+4xp0XGz*)
(*YWYlwv0KvCCf+30HyV9Y0hXDkg/fOe0PVzHFc3K4+9BtemE9zmGdcGQvrJ74*)
(*lcUQzz8uIzaJS4OzYf+q2S8+JnWo+aOTBzNjOZpcuIK7j5p9DfvWDZQqyfyP*)
(*Z3ZpYa+o1MUTsK2fn54FVxWZ9pA4rZCtioQZe8F4JizsEA2thUU8+2cJpC80*)
(*D3nWV2nKEFbA9SZ1m/aLmYKpDyZNk6hD4Rn+R/1wyqF4di2cJ/ZJ6CL5AelU*)
(*Gmx/mF+qgQWR/4T4wKoi7VvFsGLjgGoGdaj5LfXvfXBGal9PN+zl+01DGBlf*)
(*YVFr4Ezj5jRH2JQd43sGbp5ylRkDsI+yzZ5V8KWfOz2Ow+b85/Xn4YhnuqIo*)
(*mHm6a42OfH9Z4rPij/VVHsjTw4bqMHY3bONvH+uFcytnHQphpkw5fBWu0Dc0*)
(*SGDlyZ3tZH4DtyXnTZje6nwxHxa+73rBDTYoHGyJJM64ty35IS606N24pF94*)
(*B/thkzdbOUj6SMt90giLUtOavoXVT+K+VMDU/gJtEuxUWf02TfIvnoh/BTa/*)
(*qOmzrkNfjw47L5G6l+/wrIXd+pKr78FeFplGAKdMVdTehsXS0ajvfeFb54x3*)
(*4Li77XeCYfPR7Z4TJH/6a7WWg3MrZw7Pw6LywVx/2NTXIbPAiw5rLzf4ID+U*)
(*f8kKl3x1X7YJLhElC/6Cu25KL49501SQrDdjDFYnGbeegg1+2utXYIOw/l0J*)
(*bH555WoB6UuRxwgXTrE4l0XDbuS/5f3//2s9/S+cWbuk*)
(*"]], Inset[$CellContext`Z, {9.332600380984282, 13.056274380900543`}, ImageScaled[{1, 0}]], {Dashing[{0.030000000000000002`, 0.030000000000000002`}], Line[{{6.500000000001819, 9.999999999996362}, {12.999999999998181`, 6.}}]}, Inset[$CellContext`h2, {9.463624737400366, 7.156640198275594}, ImageScaled[{1, 1}]], {Dashing[{0.030000000000000002`, 0.030000000000000002`}], Line[{{13., 13.999999999996009`}, {13., 5.99999999999511}}]}, Inset[$CellContext`h3, {13.8993, 10.}, ImageScaled[{0, Rational[1, 2]}]], {PointSize[0.04], Point[{6.5, 10.}], Point[{13., 14.}], Point[{13., 6.}]}, Inset[5, {10., -0.5}, ImageScaled[{Rational[1, 2], 0}]]}, "ad/becf/dedfef.m", TooltipStyle -> "TextStyling"], "ad/becf/dedfef.m", "Tooltip"], AspectRatio -> 1, PlotRange -> {{-1, 21}, {-1, 21}}], {88, 0}, {0, 0}, {22, 22}], Inset[Graphics[Annotation[Tooltip[{Thickness[0.005], Line[CompressedData["*)
(*1:eJw9lAlIVEEYx8daU9u01TzWyq431WIZ1nNFUpivU+mQLASrNUrxobWh3VvZ*)
(*IYXpFpZZ1tuygwqtoGM90k4pS6UnlhZuF2nRYVm5ZW6t5msK5g0Mww+Gmfn+*)
(*3/8/o5PSF6b0QwhNp/Pfavwp0xEI6P/A8GTLiYmFIuUMx4vNwRjMsVPsE3nK*)
(*daPLxyzAkJTfXFgjaQEt180qN2J4HxQckyhQrjQP6MrEsLN5kpdDDgCktYPb*)
(*Dsr6kY/zRcoJe3db12L4LsYmhvCUIWz30sUYYjepV9VL/oBspj1tegxx2aaS*)
(*FIFy3WHPOYMw6BweNS6IckaP77w2DpxRizYWiX6ANAEzaqs4yKv+lRvBU9by*)
(*GeEWDrI63rQ2Sb6ArnicDsnmYNFK/UajQNnWMeH7Dg6KPJ0GV0RZ19IYkcOB*)
(*uuXVjSJxCKCjSeHeJziQllQ+0POUE4y8yx0OTvU+rWiQfAC5Z1+c94GD647A*)
(*eykC5UeLnSt8MHTGqQL7ZG9AB+7lLYvEMGdoZkuhSLmzvPR4Ioavxd3qUJ7y*)
(*o/K1Uzdg8E+NtNZLGkAxzplmqs+fI+cqkgXKCcnS8q0YWo6/6uqRBwMqWV0T*)
(*nobheXWBNZSnXKlaOGwuhkbvl8tSBC9a73ZiwhgMn+Nvi6In1Uf4fOk3B6Xx*)
(*6RcapEGAWjPHio0cdFuj/V0QZc2X5N4LHJiKpw4O49WAFoREy/s5eBw0okoQ*)
(*BtL67COvZnHgdahttih6KGyafL/zoeSu7HeOrWzvk92U86J2HYifwrsp9xV8*)
(*OT9NEAYo7xlVy522iK7Ke/20xvmNkkqpRxs1Rq1CKqXenyVlByP4/ooed91z*)
(*jxmFfopevrk+H0+KLoqehrNucU0SUvTW6B0N/RFS+uFoOvIjVegjrF+Jd4px*)
(*vdRLWD+vzB7xTsf3ENbv0vgcTa74mzA/pG0N2PdJdhDmlwL34PGxQjdhfnop*)
(*r8ssk7oI89vZy8NtQfwPwvxoS7eOM4t2wvwKXpdf2+VvhPk5bHLkzSapgzC/*)
(*x5w/U1wmthOWh9BDZr/DwnvC8lJ9zXXTGv4tYXm6WZuTF41aCcubrt2wbYLl*)
(*GWF5TLh6/2AF30xYXk/dKn22wlJPWJ5RmsG23lJFWN7/fwMfs26x/+AvHtmh*)
(*cw==*)
(*"]], Inset[$CellContext`Z, {3.25, 9.0548}, ImageScaled[{Rational[1, 2], 1}]], Line[CompressedData["*)
(*1:eJwt1A1Qk3UcB/AH9PKlQMSrC7gIYggN9iKzbWDkgy+Iim5sqPMQ2nEg66Yw*)
(*QRA4sRkSEyoQGkOOK9CKDXyZkxXWIVzAKJgNkJfVHbvhCzHDOnJtxIv0farn*)
(*bvve5777/V92uwWn54gyPQmC2IMXlf+90Uni3yeSJMhxqSgGlnRby8YiSCKP*)
(*Rn8/D7anFm24Dfcbw6tbqV584fDn8Nv9XtM2WFi4z/gZvLjkI/JBsp25eQak*)
(*zZL/2nak3nv7JSvywOO7y3KqN0z7vYT9Kofz3RpYEmv+Yzccrti2tReWRbbO*)
(*lcC9o/aROVgYXrn3Gpz40Tt3gpGqiKs3B5Dc33g5yUi7edPJMeSOQNvaCmSH*)
(*h8s5iJwq4YV8j+z+Zc2iHllcxnIuUvM/HFlNfa75dKCcy8B5MqylabDL99ox*)
(*BSyZLWjhwjd3LRu0sLDXb40/zMiSN9hgBadX7g3T3qq9upGJ+w1FT70ChzLI*)
(*sTh4qOocnwNXFInacmBV4vENUrgr4J5nI5xwbClFA9uydrj6YUWxOHMYDpqU*)
(*6J2wsM7g8QL2Uad+uy2EhfU3O6YZcHcQXZIMC6/oxnfCP0l1KSq4iagt2A27*)
(*J5ppXbBPAO0WBz5iFLw+Dyskz6t84aSRPqYfmySkH1aoZ7DfV9HVaVy4fqVP*)
(*3wlLk8kXxTA/Jd11OZJUFt+N+DobJkvviy+gP9O+ef1FuDD4wanz6K0ZfEcz*)
(*zP7E4KxBzx48ceoObH8Syv8G/Ypu2W2BZR+fqXXAPbyGxcfU/OxWvxAGqTz3*)
(*u6fPErV+S2t7Cs53vjPO5LsF528aL6hET1eI1HRYxts72QZnLi+ZdsIzjUdt*)
(*HXBApUueBnff+KDqFubl/lFHi2FiNnh/PfqzSRamBu6ILhPkop+ovZjVDusH*)
(*p6bj4YG0iOxhylv+jgmEj6+rMT6F6/U9qwnMx+ZmWNZGYb2zcQf/xPnrhkdL*)
(*3oDDy7RF87jvRlZNZww8379gxPerLHcKTEmwkpXvjqXu477nIYNlD73zT6Nf*)
(*0I4slMAK8XjdDfSlv47GfwrPrLcwH6HXjDw92QYntPz1oxd+B7yuudEeuEkQ*)
(*+TCUSSo5Ldzrk7DqZ53pTfQFii9m56l5u9DDH257cEjwMgfnS121bwHrFa4q*)
(*T4+CEybqZ4Zgf6G3RgALta/e/hL7e6mzJCfg6gDbgBLuetZAU8Eqc+OB92Dr*)
(*nrDmKzA/jMXOwPwTskb3HSwJezc3D/1UYvyh+7A2u9ysRu/oMz9zwEGXDj4y*)
(*wc9X8HD+/39hkv8As5asQQ==*)
(*"]], Inset[$CellContext`Z, {16.280796897832168`, 15.429621715174807`}, ImageScaled[{Rational[1, 2], 0}]], Line[CompressedData["*)
(*1:eJwl1AtMU1cYB/ALLcjAYJ1t7RRsx3OhgK0wjGws1wd0jlm7IRWRSgttFzRi*)
(*bfcw05BiNDKRUaAMCU6pCxAQtJ2ZTHwMp2MdStbSVZhkusgjFCmtDl9Bcf+z*)
(*neTem1++x/lycu99s2jPx5pAiqIkuMjz/1sCTf23eDQV/amWlwjLor61Vy2l*)
(*KYnWkLsNpv8KbxTCCWUjnRWwVXp1bIJLU5k625UeWCfdobTDlbz+Cz6Y1bim*)
(*aRCecIs2xwtpSrD6cn0A6u8WjjqVcLNMrM2G33j+ML0JNp0VOy1wR/oWlRuW*)
(*9QSbZ+F4fndceCJNOa5ZAlMx33e22pYNMOt8TlUOXFseId8H06fy+rLh8fyY*)
(*mHbYJH1QtxxmiOX6IVLPOamwo1/Y2fxQZhJN+Vs7kxQwa7H6xUrY0Rc5NYl5*)
(*57hKQwFMFU7N7IddlpFDR0l+9eNdsfB4gZx5CZaFfvSPn0NT1ctXq2ZI/rOD*)
(*J8bgBO+IOSYZ86iOvEYh37iuPVoBK4sfffYuXDA8LWogbrg+UAXPF6hOO2Gj*)
(*IfTUGJxxs2lT2ErsN7T+GxHmc3lef2s9bFQE8YthYVCwfh/sWHP+8l74Kn+7*)
(*u4O4tLZnKzx/6IDhDiyiu7xc+MDilz6GCPXS3JJu9M+ya5jLYP9wyaosmJIH*)
(*hohgxzJfnAvz30yt+zqL2J2tK+XQRlHTr9wdsDJC44lF/KvsPx9/TurX3RgN*)
(*RNy49o+NNaR/cFtbCNx+5qfZTlh34cZ0Cpzft8fyGyx6VanaT+JJ6c4JuHch*)
(*R+iEt8Xm+Rlimvrb+oJPzpfJPrdbAAvS3i/6BDZvFg6+A9MBcd6jcItvypUL*)
(*N2/I6TrCpY1vO6OKSmHlpsGB7fCuGlfbYRJP1DAWIp976y7zBOk/29p/Gvvx*)
(*9Y3zNtj047PuZDjtujzBLibv660VvWya0nsKOfdIPf+lTguXatKSn5L+FsPP*)
(*SWzaqP+w/3vWKvTTVgxFwF2K8lYh3FyREp6K/MNP2p0S2Drsle6GL/6i61fD*)
(*Srt1wRXkSzImgw7CrJpzIWyc56K16pmTsPGHRU9UmGfSb5rrgXWMaU8DXBKq*)
(*trvh3sb4Ox3Id19URvtgwdaswnq4rb6auyCFfE/cujz4oeT+eCTsyNsyOof9*)
(*05ZmMlNh65fH71XCxVWCgA9gpXoghQdzeZl7lbAssizKtoSmysqf3v6CxCfH*)
(*J9RLcJ4Rz4+ZSH+/eWcGLGh50N1B+rWGdL0HX/PupPvg3kuPVmhR7zH/fvs+*)
(*yTfaws7Ar8iCWeT/wqb/BeeBmPU=*)
(*"]], Inset[$CellContext`Z, {16.280796897832168`, 4.570378284825192}, ImageScaled[{Rational[1, 2], 1}]], Line[CompressedData["*)
(*1:eJwt1A9Qk3UYB/AXtJMRLCgkgiFQMpmrWDFhh0CvCAoI5/QWAle2JhY2QErS*)
(*pRysRKDyZAeIoPyZAweCuBcjpMWfpc4yOEQ6EAlhnvwZhkU4byPmXd9f13v3*)
(*vu997nl+7+/P89wbIDu4e78jRVFJuMmbciKPjTT13/U6TaklpcM/8miK6Wht*)
(*cYElTadWjsIZPa3zL8EREZ++R8PSUvcX3OEx9rjdFRY3MoNkvPSTwuvTQTRV*)
(*pa5rf8BHvv675BvwYnNBfAc8YT4W1AoL2Gc5x+CFRyFtlXDGc/KkcDgvur7j*)
(*OImHj7CsWJci89y9bFis+GGkE1bVsJYlMMXpv5UP3xjS1YfAXYHsB7the4+T*)
(*lAWXxMqzNsFxnNjVoxtoKiUq6yYXNrGaTp+FnWZiIwPhodWholS460jvnjfg*)
(*dNu1KQ/4F5dVCeFwzni+eYhLU6L4+rptsM0af14FL9YwTxPJuXFG7ybDKeLA*)
(*n+Lgxv7PDwfCiuVt06GwpXHNlmeBmCex298T5mzh6+/DSoFX9BzOTTl5QGiE*)
(*VQmUYxusblp2aSfeq2+Wwycj3Z3q4GZF8JEgeOAC71oJHCd89KcZ+4xp0XGz*)
(*YWYlwv0KvCCf+30HyV9Y0hXDkg/fOe0PVzHFc3K4+9BtemE9zmGdcGQvrJ74*)
(*lcUQzz8uIzaJS4OzYf+q2S8+JnWo+aOTBzNjOZpcuIK7j5p9DfvWDZQqyfyP*)
(*Z3ZpYa+o1MUTsK2fn54FVxWZ9pA4rZCtioQZe8F4JizsEA2thUU8+2cJpC80*)
(*D3nWV2nKEFbA9SZ1m/aLmYKpDyZNk6hD4Rn+R/1wyqF4di2cJ/ZJ6CL5AelU*)
(*Gmx/mF+qgQWR/4T4wKoi7VvFsGLjgGoGdaj5LfXvfXBGal9PN+zl+01DGBlf*)
(*YVFr4Ezj5jRH2JQd43sGbp5ylRkDsI+yzZ5V8KWfOz2Ow+b85/Xn4YhnuqIo*)
(*mHm6a42OfH9Z4rPij/VVHsjTw4bqMHY3bONvH+uFcytnHQphpkw5fBWu0Dc0*)
(*SGDlyZ3tZH4DtyXnTZje6nwxHxa+73rBDTYoHGyJJM64ty35IS606N24pF94*)
(*B/thkzdbOUj6SMt90giLUtOavoXVT+K+VMDU/gJtEuxUWf02TfIvnoh/BTa/*)
(*qOmzrkNfjw47L5G6l+/wrIXd+pKr78FeFplGAKdMVdTehsXS0ajvfeFb54x3*)
(*4Li77XeCYfPR7Z4TJH/6a7WWg3MrZw7Pw6LywVx/2NTXIbPAiw5rLzf4ID+U*)
(*f8kKl3x1X7YJLhElC/6Cu25KL49501SQrDdjDFYnGbeegg1+2utXYIOw/l0J*)
(*bH555WoB6UuRxwgXTrE4l0XDbuS/5f3//2s9/S+cWbuk*)
(*"]], Inset[$CellContext`Z, {9.332600380984282, 13.056274380900543`}, ImageScaled[{1, 0}]], {Dashing[{0.030000000000000002`, 0.030000000000000002`}], Line[{{6.500000000001819, 9.999999999996362}, {12.999999999998181`, 6.}}]}, Inset[$CellContext`h3, {9.463624737400366, 7.156640198275594}, ImageScaled[{1, 1}]], {Dashing[{0.030000000000000002`, 0.030000000000000002`}], Line[{{13., 13.999999999996009`}, {13., 5.99999999999511}}]}, Inset[$CellContext`h2, {13.8993, 10.}, ImageScaled[{0, Rational[1, 2]}]], {PointSize[0.04], Point[{6.5, 10.}], Point[{13., 14.}], Point[{13., 6.}]}, Inset[6, {10., -0.5}, ImageScaled[{Rational[1, 2], 0}]]}, "ad/becf/dedfef.m", TooltipStyle -> "TextStyling"], "ad/becf/dedfef.m", "Tooltip"], AspectRatio -> 1, PlotRange -> {{-1, 21}, {-1, 21}}], {110, 0}, {0, 0}, {22, 22}]}, AspectRatio -> Rational[1, 6], ImageSize -> {392.5, 194.}, PlotRange -> {{0, 132}, {0, 22}}] H*)


(* ::Section:: *)
(*Obtain the amplitude*)


ampHHZ[0] = FCFAConvert[CreateFeynAmp[diags1,Truncated -> True], 
		IncomingMomenta->{P}, OutgoingMomenta->{p1,p2},LorentzIndexNames->{\[Mu],\[Alpha],\[Beta]},LoopMomenta->{q},
		UndoChiralSplittings->True,ChangeDimension->D,List->False, SMP->False, Contract-> True, DropSumOver->True]/.q->q+P
ampHHZ[0]+= FCFAConvert[CreateFeynAmp[diags2,Truncated -> True], 
		IncomingMomenta->{P}, OutgoingMomenta->{p1,p2},LorentzIndexNames->{\[Mu],\[Alpha],\[Beta]},LoopMomenta->{q},
		UndoChiralSplittings->True,ChangeDimension->D,List->False, SMP->False, Contract-> True, DropSumOver->True]/.q->q+P/.p2->P-p1
ampHHZ[0]+= FCFAConvert[CreateFeynAmp[diags3,Truncated -> True], 
		IncomingMomenta->{P}, OutgoingMomenta->{p1,p2},LorentzIndexNames->{\[Mu],\[Alpha],\[Beta]},LoopMomenta->{q},
		UndoChiralSplittings->True,ChangeDimension->D,List->False, SMP->False, Contract-> True, DropSumOver->True]/.q->q+P/.p1->P-p2
ampHHH[0]= FCFAConvert[CreateFeynAmp[diags4,Truncated -> True], 
		IncomingMomenta->{P}, OutgoingMomenta->{p1,p2},LorentzIndexNames->{\[Mu],\[Alpha],\[Beta]},LoopMomenta->{q},
		UndoChiralSplittings->True,ChangeDimension->D,List->False, SMP->False, Contract-> True, DropSumOver->True]/.q->q+P/.p1->P-p2/.p2->P-p1;
ampHHG[0]= FCFAConvert[CreateFeynAmp[diags5,Truncated -> True], 
		IncomingMomenta->{P}, OutgoingMomenta->{p1,p2},LorentzIndexNames->{\[Mu],\[Alpha],\[Beta]},LoopMomenta->{q},
		UndoChiralSplittings->True,ChangeDimension->D,List->False, SMP->False, Contract-> True, DropSumOver->True]/.q->q+P/.p1->P-p2/.p2->P-p1;
		
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





(* ::Section:: *)
(*Fix the kinematics*)


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


Print["HHZ"]
ampHHZ[2]=ampHHZ[1]//DiracSimplify//TID[#(*/.{p2->P-p1, p1->P-p2}*),q,ToPaVe->True,UsePaVeBasis->True]&//Simplify
(*ampHHZ[2] =  ampHHZ[2]/.{Momentum[P,\[Mu]]->0, Momentum[p1,\[Mu]]->0, Momentum[p2,\[Mu]]->0};*)
Print["HHH"]
ampHHH[2]=ampHHH[1]//DiracSimplify//TID[#,q,ToPaVe->True,UsePaVeBasis->True]&//Simplify
(*ampHHH[2] =  ampHHH[2]/.{Momentum[P,\[Mu]]->0, Momentum[p1,\[Mu]]->0, Momentum[p2,\[Mu]]->0}*)
Print["HHG"]
ampHHG[2]=ampHHG[1]//DiracSimplify//TID[#(*/.{p2->P-p1, p1->P-p2}*),q,ToPaVe->True,UsePaVeBasis->True]& //Simplify;
(*ampHHG[2] =  ampHHG[2]/.{Momentum[P,\[Mu]]->0, Momentum[p1,\[Mu]]->0, Momentum[p2,\[Mu]]->0};*)





(* ::Subsubsection:: *)
(*coeficients*)


Print["HHZ:"];
HHZcheck = Coefficient[ampHHZ[2],{FCI[FVD[P,\[Alpha]]MTD[\[Beta],\[Mu]]],FCI[FVD[P,\[Beta]]MTD[\[Alpha],\[Mu]]]}];
If[HHZcheck[[1]]===HHZcheck[[2]],
f4ZHHZ[s_,mh1_,mh2_,mh3_]:= FullSimplify[ HHZcheck[[1]]];
];
FullSimplify[f4ZHHZ[s,mh1,mh2,mh3]]
(* 
FVD[P,beta] -- D lorenz vector p^beta|
MTD[mu,alpha] -- metric with mu alpha indexes
*)
Print["HHH:"];
HHHcheck = Coefficient[ampHHH[2],{FCI[FVD[P,\[Alpha]]MTD[\[Beta],\[Mu]]],FCI[FVD[P,\[Beta]]MTD[\[Alpha],\[Mu]]]}];
If[HHHcheck[[1]]===HHHcheck[[2]],
f4ZHHH[s_,mh1_,mh2_,mh3_]:= FullSimplify[ HHHcheck[[1]]];
];
FullSimplify[f4ZHHH[s,mh1,mh2,mh3]]


Print["HHG:"];
HHGcheck = Coefficient[ampHHG[2],{FCI[FVD[P,\[Alpha]]MTD[\[Beta],\[Mu]]],FCI[FVD[P,\[Beta]]MTD[\[Alpha],\[Mu]]]}];
If[HHGcheck[[1]]===HHGcheck[[2]],
f4ZHHG[s_,mh1_,mh2_,mh3_]:= FullSimplify[ HHGcheck[[1]]],
f4ZHHG[s_,mh1_,mh2_,mh3_]:=0;
];
FullSimplify[f4ZHHG[s,mh1,mh2,mh3]]
Print["Summ = "]
f4Z[s_,mh1_,mh2_,mh3_,pref_]:=pref*f4ZHHZ[s,mh1,mh2,mh3]+f4ZHHH[s,mh1,mh2,mh3]+f4ZHHG[s,mh1,mh2,mh3];
FullSimplify[f4Z[s,mh1,mh2,mh3,pref]]


(* ::Subsubsection:: *)
(*cross only for correct VertexFunction*)


Amp[0]=I*(FCI[FVD[P,\[Beta]] MTD[\[Mu],\[Alpha]]] + FCI[FVD[P,\[Alpha]] MTD[\[Mu],\[Beta]]])f4Z[s,m1,m2,m3,1]


(FCI[FVD[P,\[Beta]] MTD[\[Mu],\[Alpha]]] + FCI[FVD[P,\[Alpha]] MTD[\[Mu],\[Beta]]])*(FCI[FVD[P,\[Beta]] MTD[\[Mu],\[Alpha]]] + FCI[FVD[P,\[Alpha]] MTD[\[Mu],\[Beta]]]) // Contract 


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
