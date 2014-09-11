(* ::Package:: *)

BeginPackage["Segment`","Skew`"];


Segment::usage = "Segment is a package, intended to be use as Chinese and English Character Segment tools.";
segmentByHorizon::usage = "segmentByHorizon[img], segment a Image to many lines";
segmentByVertical::usage = "segmentByVertical[mt], mt is [0,1] matrix";


Begin["`Private`"];


segmentByHorizon[i_] := i//correctSkew//Binarize//ImageData//SplitBy[#,MatchQ[#,{0..}]&]/.{{0..}..}->Sequence[]& //Image[#,"Bit",Magnification->1]&/@#&


(*label rows desired preserve*)
labelRowsPreserve[m_] :=SplitBy[m\[Transpose],MatchQ[#,{0..}]&]/.{{x:Repeated[{0..},{1,1}]}:>({x}/.{0->2})}// Flatten[#,1]&
(*delete blank rows*)
delRowsBlank[m_]:=SplitBy[#,MatchQ[#,{0..}]&]/.{{0..}..}->Sequence[]& @m
segmentByVertical[m_] :=m//labelRowsPreserve // delRowsBlank//#/.{x:2..}:>{x}/.{2->0}&//Transpose/@#&//ArrayPlot[#, ColorRules->{0->Black, 1->White, 2->Red}, Mesh->True] &/@#&


End[ ];


EndPackage[ ]
