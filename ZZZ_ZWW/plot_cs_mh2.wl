(* ::Package:: *)

errorRelative = 0.05; 
csFileU = "buffer/cs_uuZZZ.mx"; 
csFileD = "buffer/cs_ddZZZ.mx"; 


If[FileExistsQ[csFileU],
    dataU = Import[csFileU, "Table"];
    dataD = Import[csFileD, "Table"];
    dataU = Rest[dataU];
    dataD = Rest[dataD];
    mh2ValuesU = dataU[[All, 1]];
    mh2ValuesD = dataD[[All, 1]];
    crossSectionsU = dataU[[All, 2]];
    crossSectionsD = dataD[[All, 2]];
    crossSectionsNumU = ToExpression[StringReplace[crossSectionsU, "x10^" -> "*10^"]];
    
     crossSectionsNumD = ToExpression[StringReplace[crossSectionsD, "x10^" -> "*10^"]];
    graph = ListPlot[
        {Transpose[{mh2ValuesU, crossSectionsNumU}],Transpose[{mh2ValuesD, crossSectionsNumD}]}, 
        Joined -> True, 
        PlotMarkers -> Automatic,
        PlotLabels->Placed[{"uuCross","ddCross"},Above]
    ];

    Print[graph];
    Export["subgraphs/crossSectionMh2.png", graph];

    Print["Graph saved as cross_section_plot.png"];

    ,
    Print["Error: File ", csFileU, " not found!"]
]






