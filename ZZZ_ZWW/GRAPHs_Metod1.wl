(* ::Package:: *)

(* ::Title:: *)
(*PV-graphs*)


SetDirectory[NotebookDirectory[]];
<<"../modules/setup.m"


SetDirectory[NotebookDirectory[]];
Print[Directory[]]


(* ::Section:: *)
(*Parameters and Constants*)



SavePDF=1;
ReCalculateTable=0;


SavePath="subgraphs/";
str=Import["buffer/F4ZZZ.txt"];
strZWW=Import["buffer/F4ZWW.txt"];


\[Alpha]2max=0.955317;
\[Alpha]3max=0.785398;
\[Alpha]2=0.5;
\[Alpha]3=\[Alpha]3max;
constraitF4Z300=1.3/10^4;
constraitF4Z3000=3.7/10^5;
constraitF4ZWW=2/10^4;


(* ::Section:: *)
(*Getting F4 funcs from file*)


args={s,mh1,mh2,mh3,a1,a2,a3};
argsZWW={s,mh1,mh2,mh3,mhc,a1,a2,a3};
Print["input Funcs:"]
Print[str]
Print[strZWW]
Activate[ToExpression["FZZZ"]@@(Pattern[#1,_]&)/@args\!\(\*
TagBox[":=",
"InactiveToken",
BaseStyle->"Inactive",
SyntaxForm->":="]\)ToExpression[str]];
Activate[ToExpression["FZWW"]@@(Pattern[#1,_]&)/@argsZWW\!\(\*
TagBox[":=",
"InactiveToken",
BaseStyle->"Inactive",
SyntaxForm->":="]\)ToExpression[strZWW]];
Print["generated funcs"]
FullSimplify[FZZZ[s,mh1,mh2,mh3,a1,a2,a3]//. SmpChanger//. Params]
FullSimplify[FZWW[Q^2,mh1,mh2,mh3,MHC,a1,a2,a3]//. SmpChanger//. Params];


(* ::Section:: *)
(*Prefactor*)


k=1;
mZ=MZ;
v=vev;
mW=MW;


Null


FplotZZZ[s_,mh2_,mhc_]:=Abs[FZZZ[s,mh1,mh2,Sqrt[mh2^2+v^2],\[Alpha]1,\[Alpha]2max,\[Alpha]3max]]//. SmpChanger//. Params;
FplotZWW[s_,mh2_,mhc_]:=Abs[FZWW[s,mh1,mh2,Sqrt[mh2^2+v^2],mhc,\[Alpha]1,\[Alpha]2max,\[Alpha]3max]]//. SmpChanger//. Params;


Print[FplotZZZ[400^2,200,400],"\nNext\n",FplotZZZ[s,mh2,mhc]]


(* ::Title:: *)
(*Data and constrains*)


(* ::Section:: *)
(*Graphs settings*)


Clear[base1DOpts, base2DcontOpts, shadowbox, legendLumi];
shadowbox[legend_] := Framed[legend, Background -> White, FrameStyle -> Black, FrameMargins -> {{8, 8}, {5, 5}}];
textscale = 16;
fontFam = "Helvetica";
fontWeight = "Plain";
frameThick = 2.2;
lineThick = 2.6;
legendPos1D = Scaled[{0.78, 0.82}];
PointNumber=50;
ContoursNumber=6;
GraphTheme=ColorData["Rainbow"];

base1DOpts := Sequence[
  Frame -> True, Axes -> False, PlotMarkers -> None, Joined -> True,
  FrameStyle -> Directive[Black, AbsoluteThickness[frameThick]],
  FrameTicksStyle -> Directive[textscale - 2, FontFamily -> fontFam, FontWeight -> fontWeight],
  FrameLabel -> {xLab, yLab},
  LabelStyle -> Directive[textscale, FontFamily -> fontFam, FontWeight -> fontWeight],
  BaseStyle -> Directive[textscale, FontFamily -> fontFam, FontWeight -> fontWeight],
  PlotRange -> {{xmin, xmax}, {ymin, ymax}},
  GridLines -> None,
  PlotRangePadding -> None
];

base2DcontOpts := Sequence[
  Frame -> True,
  LabelStyle -> Directive[textscale, FontFamily -> fontFam, FontWeight -> fontWeight],
  BaseStyle -> Directive[textscale, FontFamily -> fontFam, FontWeight -> fontWeight],
  FrameLabel -> {xLab2D, yLab2D},
  FrameStyle -> Directive[Black, AbsoluteThickness[frameThick]],
  FrameTicksStyle -> Directive[textscale - 2, FontFamily -> fontFam, FontWeight -> fontWeight],
  Contours -> ContoursNumber,
  ColorFunction -> GraphTheme,
  PlotPoints -> PointNumber,
  PlotRange -> All,
  PlotLegends -> BarLegend[
    Automatic,
    LegendLabel -> Placed[zLab2D, Top],
    LabelStyle -> Directive[textscale - 2, FontFamily -> fontFam, FontWeight -> fontWeight],
    LegendFunction -> shadowbox
  ]
];

legendLumi = SwatchLegend[
  {Directive[Red, HatchFilling[45*Degree, 3]], Directive[Blue, HatchFilling[-45*Degree, 3]]},
  {"300", "4000"},
  LegendLayout -> "Column",
  LegendFunction -> shadowbox,
  LabelStyle -> Directive[textscale, FontFamily -> fontFam, FontWeight -> fontWeight],
  LegendLabel -> Style[
    Row[{"\[Integral] ", Style["L", Italic], Style["d", Italic], Style["t", Italic], ", ", Superscript["fb", -1]}],
    textscale
  ]
];


(* ::Section:: *)
(*ZZZ*)


(* ::Subsection:: *)
(*roots*)


qList={190,200,210,220,230};
mhcI=800;


Clear[FindAllRoots]
FindAllRoots[q_?NumericQ,limit_?NumericQ]:=Quiet[Check[Module[{f,ms,vals,segs,sol,roots},f[m_?NumericQ]:=N[Abs[FplotZZZ[q^2,m,mhcI]]-limit];ms=Range[187.,1500.,5.];vals=f/@ms;segs=Select[Partition[Transpose[{ms,vals}],2,1],Times@@Last/@#1<=0&];If[segs==={},"No limit",sol=Quiet[(FindRoot[f[m],{m,#1[[1,1]],#1[[2,1]]}]&)/@segs];roots=m/. sol;roots=Select[roots,NumericQ];roots=Chop[roots,1/10^6];If[roots==={},"No limit",Sort[DeleteDuplicates[roots]]]]],"No limit"]]


header={"q [GeV]","Intersection Roots (300 fb^-1)","Intersection Roots (3000 fb^-1)"};
data=Table[{qVal,FindAllRoots[qVal,constraitF4Z300],FindAllRoots[qVal,constraitF4Z3000]},{qVal,qList}];


formattedData=Map[Function[item,If[ListQ[item],Round[item,0.1],item]],data,{2}];
OutputForm[TableForm[formattedData,TableHeadings->{None,header}]]


dataPlots=Table[Module[{f},f[m_?NumericQ]:=N[Abs[FplotZZZ[q^2,m,mhcI]]];Table[{m,f[m]},{m,LogRange1[187.,700.,PointNumber]}]],{q,qList}];


Show[ListLogPlot[dataPlots,PlotStyle->Table[{GraphTheme[i/Length[qList]],Thickness[0.005]},{i,Length[qList]}],PlotMarkers->None,Joined->True,PlotLegends->(StringForm["q = `1` GeV", #]&)/@qList,AxesLabel->{"m_{h2} [GeV]","|f_4^Z|"},PlotRange->{{Min[qList],Max[qList]},{1/10^6,3/10^4}},GridLines->{None,{{constraitF4Z300,Directive[Red,AbsoluteThickness[1.5],Dashed]},{constraitF4Z3000,Directive[Blue,AbsoluteThickness[1.5],Dashed]}}}]]


(* ::Title:: *)
(*Graphs*)


(* ::Section:: *)
(*F4 ZZZ*)


(* ::Subsection:: *)
(*Test graphs connection with article*)


FplotZZZ[s,mh2,mhc]


b=250;
c=400;
label0=Text[Style["m_H="<>ToString[b]<>" TeV",FontSize->14,FontFamily->"Arial",{1.3,-10}]];
plot1=Plot[{Re[FZZZ[x MZ^2,mh1,b,c,\[Alpha]1,\[Alpha]2max,\[Alpha]3max]]/. SmpChanger/. Params,Im[FZZZ[x MZ^2,mh1,b,c,\[Alpha]1,\[Alpha]2max,\[Alpha]3max]]/. SmpChanger/. Params},{x,6,40},GridLines->Automatic,ImageSize->400,PlotRange->{-(0.3/10^4),0.3/10^4},Epilog->label0,AxesLabel->{"q^2/MZ^2","f4"}];
Show[plot1]


b=250;
c=400;
label0=Text[Style["m_H="<>ToString[b]<>" TeV",FontSize->14,FontFamily->"Arial",{1.3,-10}]];
plotZWW=Plot[{Re[FZWW[x MW^2,mh1,b,c,c,\[Alpha]1,\[Alpha]2max,\[Alpha]3max]]//. SmpChanger//. Params,Im[FZWW[x MW^2,mh1,b,c,c,\[Alpha]1,\[Alpha]2max,\[Alpha]3max]]//. SmpChanger//. Params},{x,10,100},GridLines->Automatic,ImageSize->400,PlotRange->{-(3/10^4),3/10^4},Epilog->label0,AxesLabel->{"q^2/MW^2","f4_{ZWW}"}];
Show[plotZWW]


p=500
Print["Start graph F4Z(m2) for different q"]
xmin=408.5 k;xmax=409 k;
ymin=1/10^8;ymax=1/10^3;
data=N[Table[{x,FplotZZZ[p^2,x,1]},{x,LogRange1[xmin,xmax,PointNumber]}]];
l=ListLogLogPlot[data,PlotMarkers->None,Ticks->Automatic,LabelStyle->Directive[FontSize->14,FontFamily->"Helvetica"],BaseStyle->{FontSize->18},ImageSize->600,Joined->True,GridLines->Automatic,PlotRange->All]


FplotZZZ[p^2,408.82,1]


(* ::Subsection:: *)
(*Start graph F4Z m2 for different q with cuts*)


xmin = 200; xmax = 600; 
ymin = 1/10^7; ymax = 0.5/10^3; 
m2list = {200, 250, 300, 350,400, 450}; 
plots = {}; 
extr = {}; 
hiConstr = {}; 
colors = GraphTheme /@ Rescale[Range[Length[m2list]], {1, Length[m2list]}];
Print[colors]
xLab = xLab = Style["q [GeV]", textscale]; 
yLab = Style[TraditionalForm[HoldForm[Abs[Subsuperscript[Style["f", "TI"], 4, 
        Style["Z", "TI"]]]]], textscale]; 


Do[
	data = N[Table[{x, FplotZZZ[x^2, m2list[[i]], 600]}, {x, LogRange1[xmin, xmax, PointNumber]}]];
    p = ListLogLogPlot[data, 
        Evaluate@base1DOpts,
		PlotStyle -> Directive[colors[[i]]]
      ];
    AppendTo[plots, p]; 
    
	
    level = constraitF4Z3000; 
    crossings = Select[Partition[data, 2, 1], (#1[[1,2]] - level)*(#1[[2,2]] - level) < 0 & ]; 
    getCrossX[{p1_, p2_}] := Module[{x1, x2, y1, y2}, 
      {{x1, y1}, {x2, y2}} = {p1, p2}; x1 + ((level - y1)*(x2 - x1))/(y2 - y1)]; 
    roots = getCrossX /@ crossings; 

    If[Length[roots] == 2, 
      {r1, r2} = Sort[roots]; 
      Print[{r1, r2}];
      fillPlot = ListLogLogPlot[{Table[{x, constraitF4Z3000}, {x, LogRange1[r1, r2, 1000]}],
		    Table[{x, ymin},            {x, LogRange1[r1, r2, 1000]}]},
		    Evaluate@base1DOpts,
		  PlotStyle -> Directive[colors[[i]]],
		  Filling -> {1 -> {2}},
		  FillingStyle -> Opacity[0.025, colors[[i]]],
		  PlotRange -> {{xmin, xmax}, {ymin, ymax}}
		];];  
    , 
   {i, Length[m2list]}]; 
   


AppendTo[plots, LogLogPlot[{constraitF4Z300, constraitF4Z3000}, {x, xmin, xmax}, 
    Filling -> Top, PlotStyle -> {Darker[Red], Darker[Blue]}, 
    FillingStyle -> Directive[Opacity[0.035]], 
    PlotRange -> {{xmin, xmax}, {ymin, ymax}}]]; 


legend = LineLegend[colors, m2list, LegendLayout -> "Column", 
    LegendLabel -> "\!\(\*FormBox[TemplateBox[<|\"boxes\" -> \
FormBox[SubscriptBox[StyleBox[\"m\", \"TI\"], \"2\"], TraditionalForm], \"errors\" \
-> {}, \"input\" -> \"m_ {2}\", \"state\" -> \
\"Boxes\"|>,\n\"TeXAssistantTemplate\"],\nTextForm]\), GeV", 
    LabelStyle -> Directive[FontSize -> textscale, FontFamily -> "Helvetica"], 
    LegendFunction -> shadowbox]; 


Sh = Legended[Show[plots, ImageSize -> 1000], 
   Placed[Column[{legend, legendLumi}, Spacings -> 1], {0.95, 0.75}]]


If[SavePDF==1,Export[SavePath<>"ZZZ_f4_s.pdf",Sh]]


(* ::Subsection:: *)
(*2 d zzz*)


PrintTG["Starting 2d plot for zzz"];
xmin=200;xmax=600;
ymin=200;ymax=600;
xLab2D=Style[Row[{Style["q",Italic]," [GeV]"}],1.2 textscale];
yLab2D=Style[Row[{Subscript[m, 2]," [GeV]"}],1.2 textscale];
zLab2D=Style[TraditionalForm[HoldForm[Abs[Subsuperscript[Style["f","TI"],4,Style["Z","TI"]]]]],textscale];


Func[x_,y_]:=FplotZZZ[x^2,y,100];
plot5=ContourPlot[Func[x,y],{x,xmin,xmax},{y,ymin,ymax},ScalingFunctions->"Log",Evaluate[base2DcontOpts]];


plot5


If[SavePDF==1,Export[SavePath<>"ZZZ_f4_2d.pdf",plot5]]


(* ::Subsection:: *)
(*f4Z from q for different m2*)


mlist={200,230,300,500};
plots={};
xmin=200;xmax=800;
ymin=1/10^7;ymax=6/10^4;
colors=GraphTheme/@Rescale[Range[Length[mlist]],{1,Length[mlist]}];
xLab=xLab=Style["q, GeV",textscale];
yLab=Style[TraditionalForm[HoldForm[Abs[Subsuperscript[Style["f","TI"],4,Style["Z","TI"]]]]],textscale];


Do[data=Table[{x,FplotZZZ[x^2,mlist[[i]],1]},{x,LogRange1[xmin,xmax,PointNumber]}];p=ListLogLogPlot[data,PlotStyle->{colors[[i]],Thickness[0.005]},base1DOpts];AppendTo[plots,p],{i,Length[mlist]}];
limplot=LogLogPlot[{constraitF4Z300,constraitF4Z3000},{x,xmin,xmax},Filling->Top,FillingStyle->Directive[Opacity[0.05]],PlotStyle->{Red,Blue},GridLines->None,PlotRange->{{xmin,xmax},{ymin,ymax}}];


legend=LineLegend[colors,mlist,LegendLayout->"Column",LabelStyle->Directive[FontSize->textscale,FontFamily->"Helvetica"],LegendFunction->shadowbox,LegendLabel->Style[Row[{Subscript[m, 2],", ",Style["GeV",Plain]}],textscale]];


fullLegend=Placed[Column[{legendLumi,legend},Spacings->0.05],{Right,Top}];
Sh=Legended[Show[Append[plots,limplot],ImageSize->800],fullLegend]


If[SavePDF==1,Export[SavePath<>"ZZZ_f4_s.pdf",Sh]]


(* ::Subsection:: *)
(*d graph ZZZ show scaling with alpha*)


mlist={200,300,400};
xmin=200;xmax=800;
ymin=1/10^7;ymax=1/10^4;
a2=0.5;
colors=ColorData["Rainbow"]/@Rescale[Range[Length[mlist]]];
xLab=Style["q, GeV",textscale];
yLab=Style[TraditionalForm[HoldForm[Abs[Subsuperscript[Style["f","TI"],4,Style["Z","TI"]]]]],textscale];


plotForMass[m_,color_]:=Module[{dataMax,dataA2},dataMax=Table[{x,Abs[FZZZ[x^2,mh1,m,Sqrt[m^2+v^2],\[Alpha]1,\[Alpha]2max,\[Alpha]3max]]//. SmpChanger//. Params},{x,LogRange1[xmin,xmax,PointNumber]}];dataA2=Table[{x,Abs[FZZZ[x^2,mh1,m,Sqrt[m^2+v^2],\[Alpha]1,a2,\[Alpha]3max]]//. SmpChanger//. Params},{x,LogRange1[xmin,xmax,PointNumber]}];ListLogLogPlot[{dataMax,dataA2},PlotStyle->{Directive[color,Thick],Directive[color,Dashed,Thick]},base1DOpts]];
plotsCurves=MapThread[plotForMass,{mlist,colors}];


legendMass=SwatchLegend[colors,mlist,LegendLayout->"Column",LegendFunction->shadowbox,LabelStyle->Directive[FontSize->textscale,FontFamily->"Helvetica"],LegendLabel->Style["\!\(\*SubscriptBox[StyleBox[\"m\",\"TI\"],\"2\"]\), GeV",textscale]];
legendStyle=LineLegend[{Directive[Black,Thick],Directive[Black,Dashed,Thick]},{Style["\!\(\*SubscriptBox[\(\[Alpha]\), \(2\)]\) = \!\(\*SubscriptBox[\(\[Alpha]\), \(2\)]\)\!\(\*StyleBox[\"max\", Plain]\)",textscale],Style["\!\(\*SubscriptBox[\(\[Alpha]\), \(2\)]\) = 0.5",textscale]},LabelStyle->Directive[FontSize->textscale,FontFamily->"Helvetica"],LegendFunction->shadowbox];
fullLegend1=Placed[Column[{legendMass,legendStyle},Spacings->0.05],{Right,Top}];


Sh2=Legended[Show[plotsCurves,ImageSize->800],fullLegend1]


(* ::InheritFromParent:: *)
(*Null*)


If[SavePDF==1,Export[SavePath<>"ZZZ_f4_rescale.pdf",Sh2]]


(* ::Section:: *)
(*ZWW f4*)


(* ::Subsection:: *)
(*test graphs*)


b=300;
c=500;
plotF4gen=LogPlot[{FplotZZZ[x^2,b,c],FplotZWW[x^2,b,c]},{x,200,1000},GridLines->None,ImageSize->600,PlotRange->All,PlotLegends->{"zzz","zww"},Frame->True,BaseStyle->{FontSize->12}]


(* ::Subsection:: *)
(*1 (d:f4zww from q (m2-diff colors))*)


mhc=500;
mlist={200,220,230,250,350};
plots={};
xmin=200;xmax=1000;
ymin=5/10^6;ymax=3/10^4;
colors=GraphTheme/@Rescale[Range[Length[mlist]],{1,Length[mlist]}];
xLab=Style["q, GeV",textscale];
yLab=Style[TraditionalForm[HoldForm[Abs[Subsuperscript[Style["f","TI"],4,Style["Z","TI"]]]]],textscale];


Do[data=Table[{x,FplotZWW[x^2,mlist[[i]],mhc]},{x,LogRange1[xmin,xmax,PointNumber]}];p=ListLogLogPlot[data,PlotStyle->{colors[[i]],Thickness[0.005]},base1DOpts];AppendTo[plots,p],{i,Length[mlist]}];
AppendTo[plots,LogLogPlot[{constraitF4Z300,constraitF4Z3000},{x,xmin,xmax},Filling->Top,FillingStyle->Directive[Opacity[0.05]],PlotStyle->{Red,Blue},PlotRange->{{xmin,xmax},{ymin,ymax}}]];


legend=LineLegend[colors,mlist,LegendLayout->"Column",LabelStyle->Directive[FontSize->textscale,FontFamily->"Helvetica"],LegendLabel->Style["\!\(\*TemplateBox[<|\"boxes\" -> FormBox[SubscriptBox[StyleBox[\"m\", \"TI\"], \"2\"], TraditionalForm], \"errors\" -> {}, \"input\" -> \"m_2\", \"state\" -> \"Boxes\"|>,\n\"TeXAssistantTemplate\"]\), GeV",textscale],LegendFunction->shadowbox];
mhcLabel=Row[{\!\(\*SubscriptBox[\(Style["\<m\>", Italic]\), 
TemplateBox[{"\"H\"", "\"\[PlusMinus]\""},
"Superscript"]]\)," = ",NumberForm[mhc,{4,0}]," GeV"}];
epilogTag=Inset[shadowbox[Style[mhcLabel,textscale]],Scaled[{1,0.1}],{Right,Bottom}];
fullLegend=Placed[Column[{legendLumi,legend},Spacings->0.05],{Right,Top}];


Sh2=Legended[Show[plots,Epilog->{epilogTag},ImageSize->800],fullLegend]


If[SavePDF==1,Export[SavePath<>"ZWW_f4_s.pdf",Sh2]]


(* ::Subsection:: *)
(*1 (d:f4zww from mhc (m2-diff colors))*)


q=600;
mlist={300,400,500,800,1000};
plots={};
xmin=200;xmax=1000;
ymin=5/10^7;ymax=5/10^4;
colors=GraphTheme/@Rescale[Range[Length[mlist]],{1,Length[mlist]}];
xLab=Style["mhc, GeV",textscale];
yLab=Style[TraditionalForm[HoldForm[Abs[Subsuperscript[Style["f","TI"],4,Style["Z","TI"]]]]],textscale];


Do[data=Table[{x,FplotZWW[q^2,mlist[[i]],x]},{x,LogRange1[xmin,xmax,PointNumber]}];p=ListLogLogPlot[data,PlotStyle->{colors[[i]],Thickness[0.005]},base1DOpts];AppendTo[plots,p],{i,Length[mlist]}];
AppendTo[plots,LogLogPlot[{constraitF4Z300,constraitF4Z3000},{x,xmin,xmax},Filling->Top,FillingStyle->Directive[Opacity[0.05]],PlotStyle->{Red,Blue},PlotRange->{{xmin,xmax},{ymin,ymax}}]];


legend=LineLegend[colors,mlist,LegendLayout->"Column",LabelStyle->Directive[FontSize->textscale,FontFamily->"Helvetica"],LegendLabel->Style["\!\(\*TemplateBox[<|\"boxes\" -> FormBox[SubscriptBox[StyleBox[\"m\", \"TI\"], \"2\"], TraditionalForm], \"errors\" -> {}, \"input\" -> \"m_2\", \"state\" -> \"Boxes\"|>,\n\"TeXAssistantTemplate\"]\), GeV",textscale],LegendFunction->shadowbox];
mhcLabel=Row[{Style["q",Italic]," = ",NumberForm[q,{4,0}]," GeV"}];
epilogTag=Inset[shadowbox[Style[mhcLabel,textscale]],Scaled[{0.70,0.9}],{Right,Bottom}];
fullLegend=Placed[Column[{legendLumi,legend},Spacings->0.05],{Right,Top}];


Sh3=Legended[Show[plots,Epilog->{epilogTag},ImageSize->800],fullLegend]


If[SavePDF==1,Export[SavePath<>"ZWW_f4_mhc.pdf",Sh3]]


(* ::Subsection:: *)
(*2 (d:f4zww from q m2)*)


PrintTG["Starting 2d plot for zww"];
xmin=200;xmax=600;
ymin=200;ymax=600;
xLab2D=Style[Row[{Style["q",Italic],", TeV"}],1.2 textscale];
yLab2D=Style[Row[{Subscript[m, 2],", TeV"}],1.2 textscale];


Func[x_,y_]:=FplotZWW[x^2,y,Sqrt[y^2+v^2]];
plot5=ContourPlot[Func[x,y],{x,xmin,xmax},{y,ymin,ymax},ColorFunction->GraphTheme,ScalingFunctions->"Log",Contours->ContoursNumber,PlotPoints->PointNumber,Evaluate[Sequence@@base2DcontOpts],PlotLegends->BarLegend[Automatic,LabelStyle->Directive[textscale,FontFamily->"Helvetica"],LegendFunction->shadowbox]];
conplot=ContourPlot[{Func[x,y]==constraitF4Z300,Func[x,y]==constraitF4Z3000},{x,xmin,xmax},{y,ymin,ymax},ContourStyle->{Directive[Red,Thickness[0.007]],Directive[Blue,Thickness[0.003]]},PlotLegends->None];


fullLegend=Placed[legendLumi,{Right,Top}];


ShZWW=Legended[Show[plot5,conplot,ImageSize->800],fullLegend];
ShZWW


(* ::Subsection:: *)
(*\:0433\:0440\:0430\:0444\:0438\:043a 2 d \:0442\:043e\:0442 \:0436\:0435 \:043d\:043e densityplot*)


base2DdensOpts:={Frame->True,Axes->False,FrameStyle->Directive[Black,Thickness[0.002]],FrameTicksStyle->Directive[textscale-2],FrameLabel->{xLab2D,yLab2D},LabelStyle->Directive[textscale,FontFamily->"Helvetica"],BaseStyle->Directive[textscale,FontFamily->"Helvetica"],PlotRange->All};


Func[x_,y_]:=FplotZWW[x^2,y,Sqrt[y^2+v^2]];
densplot=DensityPlot[Func[x,y],{x,xmin,xmax},{y,ymin,ymax},ColorFunction->GraphTheme,ScalingFunctions->"Log",PlotPoints->PointNumber,Evaluate[Sequence@@base2DdensOpts],PlotLegends->BarLegend[Automatic,LabelStyle->Directive[textscale,FontFamily->"Helvetica"],LegendFunction->shadowbox]];
line3000=ContourPlot[Func[x,y]==constraitF4Z3000,{x,xmin,xmax},{y,ymin,ymax},ContourStyle->Directive[Black,Thickness[0.004]],Frame->False,Axes->False,PlotRange->All,PlotLegends->None];


ShZWW=Show[densplot,line3000,ImageSize->800]


If[SavePDF==1,Export[SavePath<>"ZWW_2D_m2q.pdf",ShZWW]]
PrintTG["Finishing 2d plot for zww"];


PrintTG["All plots are finished"];






