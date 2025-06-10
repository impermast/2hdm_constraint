(* ::Package:: *)

errorRelative = 0.05; 
csFileU = "../buffer/cs_uuZZZ.mx"; 
csFileD = "../buffer/cs_ddZZZ.mx"; 

f4FileD = "../buffer/f4mean_ddZZZ.mx"; 

crossGraphAdress = "crossSectionMh2.png"
f4GraphAdress = "f4_from_Mh2.png"


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
    Export[crossGraphAdress, graph];

    Print["Graph saved"];

    ,
    Print["Error: File ", csFileU, " not found!"];
];


If[FileExistsQ[csFileU],
    dataD = Import[f4FileD, "Table"];
    dataD = Rest[dataD];
    mh2ValuesD = dataD[[All, 1]];
    f4ValuesD = dataD[[All, 2]];
    f4ValuesD = ToExpression[StringReplace[f4ValuesD, "x10^" -> "*10^"]];
    graph = ListPlot[
        Transpose[{mh2ValuesD, f4ValuesD}], 
        Joined -> True, 
        PlotMarkers -> Automatic
    ];

    Print[graph];
    Export[f4GraphAdress, graph];

    Print["Graph saved"];

    ,
    Print["Error: File ", csFileU, " not found!"];
];

