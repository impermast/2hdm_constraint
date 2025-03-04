(* ::Package:: *)

BeginPackage["FunctionalPackage`"];

(* \:041f\:0440\:043e\:0432\:0435\:0440\:043a\:0430 \:0438\:043d\:0438\:0446\:0438\:0430\:043b\:0438\:0437\:0430\:0446\:0438\:0438 \:0432\:0440\:0435\:043c\:0435\:043d\:0438 *)
If[!ValueQ[startTime], startTime = AbsoluteTime[]];

(* \:041e\:043f\:0438\:0441\:0430\:043d\:0438\:0435 \:0444\:0443\:043d\:043a\:0446\:0438\:0439 *)
CheckTime::usage = "CheckTime[] \:0432\:044b\:0432\:043e\:0434\:0438\:0442 \:043f\:0440\:043e\:0448\:0435\:0434\:0448\:0435\:0435 \:0432\:0440\:0435\:043c\:044f \:0441 \:043c\:043e\:043c\:0435\:043d\:0442\:0430 \:0437\:0430\:043f\:0443\:0441\:043a\:0430.";
ShowStep::usage = "ShowStep[it, kt, progress] \:043a\:0440\:0430\:0441\:0438\:0432\:043e \:0444\:043e\:0440\:043c\:0430\:0442\:0438\:0440\:0443\:0435\:0442 \:0432\:044b\:0432\:043e\:0434 \:0438\:0442\:0435\:0440\:0430\:0446\:0438\:0438.";
PrintTG::usage = "PrintTG[msg] \:043e\:0442\:043f\:0440\:0430\:0432\:043b\:044f\:0435\:0442 \:0441\:043e\:043e\:0431\:0449\:0435\:043d\:0438\:0435 \:0432 Telegram \:0447\:0435\:0440\:0435\:0437 notifier_console.py.";

Begin["`Private`"];

(* \:0424\:0443\:043d\:043a\:0446\:0438\:044f \:0444\:043e\:0440\:043c\:0430\:0442\:0438\:0440\:043e\:0432\:0430\:043d\:043d\:043e\:0433\:043e \:0432\:044b\:0432\:043e\:0434\:0430 *)
ShowStep[it_, kt_, progress_] := Module[{},
   Print[
      "\t|itt = ", PaddedForm[it, 4], 
      "    kt = ", NumberForm[kt, {4, 2}], 
      "    val = ", ScientificForm[progress, 2] 
   ];
];

(* \:0424\:0443\:043d\:043a\:0446\:0438\:044f \:043e\:0442\:0441\:0447\:0451\:0442\:0430 \:0432\:0440\:0435\:043c\:0435\:043d\:0438 *)
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

(* \:0424\:0443\:043d\:043a\:0446\:0438\:044f \:0434\:043b\:044f \:0441\:0431\:0440\:043e\:0441\:0430 \:0442\:0430\:0439\:043c\:0435\:0440\:0430 *)
ResetCheckTime[] := (startTime = AbsoluteTime[]; Print["Timer reset."]);

(* \:0424\:0443\:043d\:043a\:0446\:0438\:044f \:0434\:043b\:044f \:043e\:0442\:043f\:0440\:0430\:0432\:043a\:0438 \:0441\:043e\:043e\:0431\:0449\:0435\:043d\:0438\:044f \:0432 Telegram *)
PrintTG[msg_String] := Module[{command, result},
  Print[msg];
  command = StringJoin["python3 ../tg/notifier_console.py \"", msg, "\""];
  result = RunProcess[{"bash", "-c", command}];
  result["StandardOutput"]
];

End[];
EndPackage[];
