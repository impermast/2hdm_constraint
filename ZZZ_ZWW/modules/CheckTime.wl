BeginPackage["CheckTimePackage`"];

(* Проверка инициализации времени *)
If[!ValueQ[startTime], startTime = AbsoluteTime[]];

(* Объявление функции *)
CheckTime::usage = "CheckTime[] выводит прошедшее время с момента запуска.";

Begin["`Private`"];

CheckTime[] := Module[{elapsed, hours, minutes, seconds},
    elapsed = AbsoluteTime[] - startTime;
    hours = Floor[elapsed / 3600];
    minutes = Floor[Mod[elapsed, 3600] / 60];
    seconds = Round[Mod[elapsed, 60]];
    Print["Time: ",  
          StringPadLeft[ToString[hours], 2, "0"], "h:", 
          StringPadLeft[ToString[minutes], 2, "0"], "m:", 
          StringPadLeft[ToString[seconds], 2, "0"], "s"]
];

End[];
EndPackage[];
