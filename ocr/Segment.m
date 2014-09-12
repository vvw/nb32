(* ::Package:: *)

BeginPackage["Segment`","Skew`"];


Segment::usage = "Segment is a package, intended to be use as Chinese and English Character Segment tools.";
segmentByHorizon::usage = "segmentByHorizon[img], segment a Image to many lines";
segmentByVertical::usage = "segmentByVertical[mt], mt is [0,1] matrix";
showSegmentByVertical::usage = "showSegmentByVertical[m_], like segmentByVertical but show more details";
segmentZhs::usage = "segmentZhs[i], segment Chinese and English Character or something else to little images.";


Begin["`Private`"];


segmentByHorizon[i_] := i//correctSkew//Binarize//ImageData//SplitBy[#,MatchQ[#,{0..}]&]/.{{0..}..}->Sequence[]& //Image[#,"Bit",Magnification->1]&/@#&


(*label rows desired preserve*)
labelRowsPreserve[m_] :=SplitBy[m\[Transpose],MatchQ[#,{0..}]&]/.{{x:Repeated[{0..},{1,1}]}:>({x}/.{0->2})}// Flatten[#,1]&
(*delete blank rows*)
delRowsBlank[m_]:=SplitBy[#,MatchQ[#,{0..}]&]/.{{0..}..}->Sequence[]& @m
showSegmentByVertical[m_] :=m//labelRowsPreserve // delRowsBlank//#/.{x:2..}:>{x}/.{2->0}&//Transpose/@#&//ArrayPlot[#, ColorRules->{0->Black, 1->White, 2->Red}, Mesh->True] &/@#&
segmentByVertical[m_] :=m//labelRowsPreserve // delRowsBlank//#/.{x:2..}:>{x}/.{2->0}&//Transpose/@#&//Image[#,"Bit",Magnification->1]&/@#&


segmentZhs[i_]:=i//segmentByHorizon//(#//ImageData//segmentByVertical)&~ParallelMap~#&


End[ ];


EndPackage[ ]
