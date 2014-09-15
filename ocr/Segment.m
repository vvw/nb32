(* ::Package:: *)

BeginPackage["Segment`","Skew`"];


Segment::usage = "Segment is a package, intended to be use as Chinese and English Character Segment tools.";
segmentByHorizon::usage = "segmentByHorizon[img], segment a Image to many lines";
segmentByVertical::usage = "segmentByVertical[mt], mt is [0,1] matrix";
showSegmentByVertical::usage = "showSegmentByVertical[m_], like segmentByVertical but show more details";
segmentZhs::usage = "segmentZhs[i], segment Chinese and English Character or something else to little images.";
segmentByVertical2::usage = "\:5207\:5206\:5217\:ff0cm \:77e9\:9635\:ff0ctreshold,{min,max} \:5982\:679c\:6709min\:5230max\:4e2a\:8fde\:7eed\:7684\:7a7a\:767d\:50cf\:7d20\:503c\:5c31\:4f1a\:88ab\:5207\:5f00";


Begin["`Private`"];


segmentByHorizon[i_] := i//correctSkew//Binarize//ImageData//SplitBy[#,MatchQ[#,{0..}]&]/.{{0..}..}->Sequence[]& //Image[#,"Bit",Magnification->1]&/@#&


(*label rows desired preserve*)
labelRowsPreserve[m_] :=SplitBy[m\[Transpose],MatchQ[#,{0..}]&]/.{{x:Repeated[{0..},{1,1}]}:>({x}/.{0->2})}// Flatten[#,1]&
(*delete blank rows*)
delRowsBlank[m_]:=SplitBy[#,MatchQ[#,{0..}]&]/.{{0..}..}->Sequence[]& @m
showSegmentByVertical[m_] :=m//labelRowsPreserve // delRowsBlank//#/.{x:2..}:>{x}/.{2->0}&//Transpose/@#&//ArrayPlot[#, ColorRules->{0->Black, 1->White, 2->Red}, Mesh->True] &/@#&
segmentByVertical[m_] :=m//labelRowsPreserve // delRowsBlank//#/.{x:2..}:>{x}/.{2->0}&//Transpose/@#&//Image[#,"Bit",Magnification->1]&/@#&


(*\:5207\:5206\:5217\:ff0cm \:77e9\:9635\:ff0ctreshold,{min,max} \:5982\:679c\:6709min\:5230max\:4e2a\:8fde\:7eed\:7684\:7a7a\:767d\:50cf\:7d20\:503c\:5c31\:4f1a\:88ab\:5207\:5f00*)
(*segmentByVerticalAndLevel[m_,treshold_] :=m//SplitBy[m\[Transpose],MatchQ[#,{0..}]&]/.{{x:Repeated[{0..},treshold]}:>({x}/.{0->2})}// Flatten[#,1]& // delRowsBlank//#/.{x:2..}:>{x}/.{2->0}&//Transpose/@#&//Image[#,"Bit",Magnification->1]&/@#&*)
labelRowsPreserve2[m_,treshold_] :=SplitBy[m\[Transpose],MatchQ[#,{0..}]&]/.{{x:Repeated[{0..},treshold]}:>({x}/.{0->2})}// Flatten[#,1]&
segmentByVertical2[m_,treshold_] :=labelRowsPreserve2[m,treshold] // delRowsBlank//#/.{x:2..}:>{x}/.{2->0}&//Transpose/@#&//Image[#,"Bit",Magnification->1]&/@#&


segmentZhs[i_]:=i//segmentByHorizon//(#//ImageData//segmentByVertical)&~ParallelMap~#&


End[ ];


EndPackage[ ]
