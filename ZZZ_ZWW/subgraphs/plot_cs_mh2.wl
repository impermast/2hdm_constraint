(* ::Package:: *)

(* Function to plot cross-section graph *)
plotCrossSectionGraph[]:= Module[{dataD,dataU,mh2ValuesD,mh2ValuesU,crossSectionsD,crossSectionsU,interpolatedD,interpolatedU,totalXS,graph},

	SetDirectory[NotebookDirectory[]];
	Print[Directory[]];
	errorRelative = 0.05; 
	csFileU = "../buffer/cs_uuZZZ.mx"; 
	csFileD = "../buffer/cs_ddZZZ.mx"; 
	crossGraphAdress = "crossSectionMh2.png";

    dataU = Import[csFileU, "Table"];
    dataD = Import[csFileD, "Table"];
    dataU = Rest[dataU];
    dataD = Rest[dataD];
    mh2ValuesU = dataU[[All, 1]];
    mh2ValuesD = dataD[[All, 1]];
    crossSectionsU = dataU[[All, 2]];
    crossSectionsD = dataD[[All, 2]];
    crossSectionsU = ToExpression[StringReplace[crossSectionsU, "x10^" -> "*10^"]];
    crossSectionsD = ToExpression[StringReplace[crossSectionsD, "x10^" -> "*10^"]];
    
    interpolatedU = Interpolation[Transpose@{mh2ValuesU, crossSectionsU}];
	interpolatedD = Interpolation[Transpose@{mh2ValuesD, crossSectionsD}];
	totalXS = Table[{x, interpolatedU[x] + interpolatedD[x]}, {x, mh2ValuesU}];
    
    graph = ListPlot[
		  {
		    Transpose@{mh2ValuesU, crossSectionsU},
		    Transpose@{mh2ValuesD, crossSectionsD},
		    totalXS
		  },
		  Joined -> True,
		  PlotMarkers -> {None, None, None},
		  PlotStyle -> {
		    Directive[Red, Thick],
		    Directive[Blue, Thick],
		    Directive[Black, Dashed, Thick]
		  },
		  PlotLegends-> Placed[
		    {
		      Style["uu>Z>ZZ", Red, 12],
		      Style["dd>Z>ZZ", Blue, 12],
		      Style["Total", Black, Italic, 12]
		    },
		    Automatic
		  ],
		  PlotTheme->"Scientific",
		  Frame -> True,
		    FrameLabel -> {
			    Style[StringJoin["\!\(\*TemplateBox[<|\"boxes\" -> FormBox[SubscriptBox[StyleBox[\"m\", \"TI\"], RowBox[{StyleBox[\"h\", \"TI\"], \"2\"}]], TraditionalForm], \"errors\" -> {}, \"input\" -> \"m_{h2}\", \"state\" -> \"Boxes\"|>,\n\"TeXAssistantTemplate\"]\)",", GeV"], 14],
			    Style["\[Sigma], fb", 14]
			  },
		  LabelStyle -> Directive[FontFamily -> "Helvetica", FontSize -> 12],
		  ImageSize -> 600,
		  GridLines -> None
		];
    Export[crossGraphAdress, graph];
    Print["Graph saved"];
];

(* Function to plot f4 graph *)
plotF4Graph[]:= Module[{dataD,dataU,mh2ValuesD,mh2ValuesU,f4ValuesD,f4ValuesU,interpolatedD,interpolatedU,totalF4mean,graph},
	SetDirectory[NotebookDirectory[]];
	f4FileU = "../buffer/f4mean_uuZZZ.mx";
    f4FileD = "../buffer/f4mean_ddZZZ.mx";
    f4meanGraphAdress= "f4meanMh2.png";

    dataD = Import[f4FileD, "Table"];
    dataD = Rest[dataD];
    dataU = Import[f4FileU, "Table"];
    dataU = Rest[dataU];
    
    mh2ValuesU = dataU[[All, 1]];
    f4ValuesU = dataU[[All, 2]];
    
    mh2ValuesD = dataD[[All, 1]];
    f4ValuesD = dataD[[All, 2]];    
    f4ValuesD = ToExpression[StringReplace[f4ValuesD, "x10^" -> "*10^"]];
    f4ValuesU = ToExpression[StringReplace[f4ValuesU, "x10^" -> "*10^"]];
    interpolatedU = Interpolation[Transpose@{mh2ValuesU, f4ValuesU}];
	interpolatedD = Interpolation[Transpose@{mh2ValuesD, f4ValuesD}];
	totalF4mean = Table[{x, interpolatedU[x] + interpolatedD[x]}, {x, mh2ValuesU}];
    
    f4constrVal = 1*10^-5; 
	f4constr = Table[
	  {x, f4constrVal},
	  {x, Min[mh2ValuesU], Max[mh2ValuesU], 5}
	];
    
    
   graph = ListLogPlot[
		  {
		    Transpose@{mh2ValuesU, f4ValuesU},
		    Transpose@{mh2ValuesD, f4ValuesD},
		    totalF4mean,
		    f4constr
		  },
		  Joined -> True,
		  PlotMarkers -> {None, None, None, None},
		  PlotStyle -> {
		    Directive[Darker[Red], Thick],
		    Directive[Darker[Blue], Thick],
		    Directive[Black,  Thick],
		    Directive[Black, Dashed,Thick]
		  },
		  PlotLegends->Placed[
		    {
		      Style["f4mean from uu", Darker[Red], 12],
		      Style["f4mean from dd", Darker[Blue], 12],
		      Style["f4mean total", Black, Italic, 12],
		      Style["f4mean from constraint", Black, Italic, 12]
		    }, 
		    Automatic],
		  Frame -> True,
		    FrameLabel -> {
			    Style[StringJoin["\!\(\*TemplateBox[<|\"boxes\" -> FormBox[SubscriptBox[StyleBox[\"m\", \"TI\"], RowBox[{StyleBox[\"h\", \"TI\"], \"2\"}]], TraditionalForm], \"errors\" -> {}, \"input\" -> \"m_{h2}\", \"state\" -> \"Boxes\"|>,\n\"TeXAssistantTemplate\"]\)",", GeV"], 14],
			    Style["\!\(\*TemplateBox[<|\"boxes\" -> FormBox[SubsuperscriptBox[StyleBox[\"f\", \"TI\"], \"4\", StyleBox[\"Z\", \"TI\"]], TraditionalForm], \"errors\" -> {}, \"input\" -> \"f_4^Z\", \"state\" -> \"Boxes\"|>,\n\"TeXAssistantTemplate\"]\)", 14]
			  },
		  LabelStyle -> Directive[FontFamily -> "Helvetica", FontSize -> 12],
		  ImageSize -> 600,
		  GridLines -> None
		];
    Export[f4meanGraphAdress, graph];
    Print["Graph saved"];
];

(* \:041e\:0441\:043d\:043e\:0432\:043d\:0430\:044f \:0444\:0443\:043d\:043a\:0446\:0438\:044f *)
main[] := (
  plotCrossSectionGraph[];
  plotF4Graph[];
)

(* \:0412\:044b\:0437\:043e\:0432 \:043e\:0441\:043d\:043e\:0432\:043d\:043e\:0439 \:0444\:0443\:043d\:043a\:0446\:0438\:0438 *)
main[];



