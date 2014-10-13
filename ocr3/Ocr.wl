(* ::Package:: *)

BeginPackage["Ocr`",{"Segment`"}];

ocr::uage="ocr[i]"
ocrChinese::usage="ocrChinese[i]"
imageScaled::usage="imageScaled[i, ntimes]"
imageLines::usage="imageLines[i], split a image to some small lines"


Begin["`Private`"]


ocrChinese[i_]:=TextRecognize[i,Language->"Chinese","SegmentationMode"->7];
imageScaled[i_, ntimes_]:=ImageResize[i, Scaled[ntimes]];
imageLines[i_]:=i//segment//(#//splitByGreen//turnBlack/@#&//mergeSplitBy//Image[#,"Bit",Magnification->1]&//ColorNegate)&/@#&;


End[ ];


EndPackage[ ]
