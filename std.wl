(* ::Package:: *)

BeginPackage["Std`"];
(*
 * using below command to import this package
SetDirectory@NotebookDirectory[];
Get@FileNameJoin[{ParentDirectory[],"std.wl"}];
 *)
bomFreeQ::usage="bomFreeQ[s], utf8 detect only";
unixStypeQ::usage="unixStypeQ[s],unix newline stype is \n and windows stype is \r\n";
unixStype::usage="unixStype[s], convert to unix newline style";
checkString::usage="checkString[s], ensure a string is BOM-free, and newline is unix style";
compressNewline::usage="compressNewline[s],successive \n compress to single \n";
dropLeft::usage="dropLeft[ls,fQ], drop a element from list repeatly, when fQ return false.";


Begin["`Private`"]


utf8BOM[] := Module[{bom,r},
	bom={"EF","BB","BF"};
	r=FromDigits[#,16]&/@bom//FromCharacterCode
]
bomFreeQ[s_]:=Not@StringMatchQ[s,utf8BOM[]~~___]


unixStypeQ[s_]:=FreeQ[s,"\r\n"]
unixStype[s_]:=StringReplace[s,"\r\n"..->"\n"]


compressNewline[s_]:=StringReplace[s,"\n"..->"\n"]


checkString[s_]:=Module[{r},
	On[Assert];
	r=And[bomFreeQ[s],unixStypeQ];
	Assert[r,"### bom or unix newline test fail!"];
	r
]


dropLeft[ls_,fQ_]:=If[fQ[First[ls]],dropLeft[Rest[ls],fQ],ls]


End[ ];


EndPackage[ ]
