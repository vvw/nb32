(* ::Package:: *)

BeginPackage["Segment2`","Skew`"];


Segment2::usage = "Segment2 is a package, intended to be use as Chinese and English Character Segment tools.";
segmentRow::usage = "segmentColumn[img], \:5206\:5272\:884c";
segmentColumn::usage = "segmentColumn[img], \:5206\:5272\:5217";


Begin["`Private`"];


segmentRow[i_] := i//correctSkew//Binarize//ImageData//SplitBy[#,MatchQ[#,{0..}]&]/.{{0..}..}->Sequence[]& //Image[#,"Bit",Magnification->1]&/@#&
segmentColumn[i_]:=i//ImageData//Transpose//SplitBy[#,MatchQ[#,{0..}]&]&//#/.{{0..}..}->Sequence[]&// Transpose/@#&//Image[#,"Bit",Magnification->1]&/@#&


End[ ];


EndPackage[ ]
