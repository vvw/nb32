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
compressNewline::usage="successive \n compress to single \n";


Begin["`Private`"]


utf8BOM[] := Module[{bom,r},
	bom={"EF","BB","BF"};
	r=FromDigits[#,16]&/@bom//FromCharacterCode
]
bomFreeQ[s_]:=Not@StringMatchQ[s,utf8BOM[]~~___]


unixStypeQ[s_]:=FreeQ[s,"\r\n"]
unixStype[s_]:=StringReplace[s,"\r\n"..->"\n"]


compressNewline[s_]:=StringReplace[s,"\n"..->"\n"]


End[ ];


EndPackage[ ]
