(* ::Package:: *)

BeginPackage["Segment`","Skew`"];


Segment::usage = "Segment is a package, intended to be use as Chinese and English Character Segment tools.";
segmentByHorizon::usage = "segmentByHorizon[img], segment a Image to many lines";
segmentByVertical::usage = "segmentByVertical[mt], mt is [0,1] matrix";
showSegmentByVertical::usage = "showSegmentByVertical[m_], like segmentByVertical but show more details";
segmentZhs::usage = "segmentZhs[i], segment Chinese and English Character or something else to little images.";
zhWidthThreshold::usage="width threshold of chinese character";
labelWhite::usage="\:6807\:8bb0\:7a7a\:767d\:5217\:ff0c\:6807\:8bb0\:4e3a2\:7684\:8981\:5c0f\:5fc3\:5904\:7406\:ff0c\:6807\:8bb0\:4e3a3\:7684\:53ef\:4ee5\:653e\:5fc3\:5206\:5272";
segmentGreenRed::usage="2\:7ea2\:ff0c3\:7eff\:ff0c\:7ea2\:7684\:8981\:7279\:522b\:5904\:7406\:ff0c\:68c0\:67e5\:524d\:540e\:5b57\:7b26\:77e9\:9635\:662f\:5426\:5b8c\:6574\:ff0c\:5b8c\:6574\:5219\:8f6c\:6210\:7eff\:8272\:ff0c\:4e0d\:5b8c\:6574\:5219\:53d6\:6d88\:8fd9\:4e2a\:6807\:8bb0";
completeQFactory::usage="not you bussines";


Begin["`Private`"];


segmentByHorizon[i_] := i//correctSkew//Binarize//ImageData//SplitBy[#,MatchQ[#,{0..}]&]/.{{0..}..}->Sequence[]& //Image[#,"Bit",Magnification->1]&/@#&


(*label rows desired preserve*)
labelRowsPreserve[m_] :=SplitBy[m\[Transpose],MatchQ[#,{0..}]&]/.{{x:Repeated[{0..},{1,1}]}:>({x}/.{0->2})}// Flatten[#,1]&
(*delete blank rows*)
delRowsBlank[m_]:=SplitBy[#,MatchQ[#,{0..}]&]/.{{0..}..}->Sequence[]& @m
showSegmentByVertical[m_] :=m//labelRowsPreserve // delRowsBlank//#/.{x:2..}:>{x}/.{2->0}&//Transpose/@#&//ArrayPlot[#, ColorRules->{0->Black, 1->White, 2->Red}, Mesh->True] &/@#&
segmentByVertical[m_] :=m//labelRowsPreserve // delRowsBlank//#/.{x:2..}:>{x}/.{2->0}&//Transpose/@#&//Image[#,"Bit",Magnification->1]&/@#&


segmentZhs[i_]:=i//segmentByHorizon//(#//ImageData//segmentByVertical)&~ParallelMap~#&


(*\:4e2d\:6587\:5b57\:7b26\:5bbd\:5ea6\:7684\:9600\:503c*)
zhWidthThreshold[i_]:=With[{zh=i//segmentZhs},
Module[{wd=zh//Flatten//ImageData/@#&//Dimensions/@#&//(#[[2]]&)/@#&},
wd->(zh//Flatten)//FindClusters//Sort[#, Length[#1]>Length[#2]&]&//First//ImageData/@#&//Dimensions/@#&//
#[[2]]&/@#&//{Min[#],Max[#]}&]
]


(*\:6807\:8bb0\:4e3a2\:7684\:8981\:5c0f\:5fc3\:5904\:7406\:ff0c\:6807\:8bb0\:4e3a3\:7684\:53ef\:4ee5\:653e\:5fc3\:5206\:5272*)
labelWhite[i_]:=i//Binarize//ImageData//Transpose//SplitBy[#,MatchQ[#,{0..}]&]&//#/.{{x:Repeated[{0..},{1,2}]}:>({x}/.{0->2})}&//
 #/.{{x:{0..}..}:>({x}/.{0->3})}&//Flatten[#,1]&//Transpose
(*\:538b\:7f29\:6807\:8bb0\:4e3a3\:7684\:5217*)
compressLabel[m_]:=m//Transpose//SplitBy[#,MatchQ[#,{3..}]&]&//#/.{x:{3..}..}:>{First[{x}]}&//Flatten[#,1]&//Transpose
(*2\:7ea2\:ff0c3\:7eff\:ff0c\:7ea2\:7684\:8981\:7279\:522b\:5904\:7406\:ff0c\:68c0\:67e5\:524d\:540e\:5b57\:7b26\:77e9\:9635\:662f\:5426\:5b8c\:6574\:ff0c\:5b8c\:6574\:5219\:8f6c\:6210\:7eff\:8272\:ff0c\:4e0d\:5b8c\:6574\:5219\:53d6\:6d88\:8fd9\:4e2a\:6807\:8bb0*)
segmentGreenRed[i_]:=With[{grouping:=(#//Transpose//SplitBy[#,MatchQ[#,{2..}]&]&//SplitBy[#,MatchQ[#,{3..}]&]&/@#&//Flatten[#,1]&//Transpose/@#&)&},
	i//segmentByHorizon//labelWhite/@#&//compressLabel/@#&//grouping/@#&]


(*\:51fd\:6570\:5de5\:5382*)
completeQFactory[i_]:=With[{minmax=i//zhWidthThreshold},
minmax[[1]]<=#<=minmax[[2]]&
]



End[ ];


EndPackage[ ]
