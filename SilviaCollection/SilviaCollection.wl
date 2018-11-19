(* ::Package:: *)

Package["SilviaCollection`"]


PackageExport["branch"]
PackageExport["levelIndentFunc"]
PackageExport["horizontalTreeForm"]
PackageExport["GraphEdgeSleek"]
PackageExport["toRegularArray"]
PackageExport["inputHistoryViewer"]
PackageExport["StringSplitNested"]
PackageExport["colorFromHex"]
PackageExport["colorToHex"]


ClearAll[branch]
branch=Through@*{##}&;


ClearAll[levelIndentFunc]
levelIndentFunc[lst_] :=
 MapIndexed[
    {ConstantArray["\t", Length[#2] - 1], #1, "\n"} &,
    lst /. e_HoldForm :> StringTake[ToString[e, InputForm], {10, -2}],
    {-1}] // Flatten // StringJoin
(*

USAGE:

traceRes = Trace[Reduce[x^2 == -1, x], TraceInternal -> True, TraceDepth -> 3];
Export["tracePrintTest.txt", levelIndentFunc@traceRes, "String"]
*)

(*On["Packing"]*)
(*Needs["Spelunking`"]*)


ClearAll[horizontalTreeForm]
Options[horizontalTreeForm]={"Transform"->RotationTransform[\[Pi]/2],AspectRatio->1.5};
horizontalTreeForm[OptionsPattern[]]:=
TreeForm/*ToBoxes/*(Block[{
GraphicsBox=Inactive[GraphicsBox],TagBox=Inactive[TagBox],GraphicsComplexBox=Inactive[GraphicsComplexBox]
,TooltipBox=Inactive[TooltipBox],InsetBox=Inactive[InsetBox],FormBox=Inactive[FormBox],StyleBox=Inactive[StyleBox],RowBox=Inactive[RowBox]
,FrameBox=Inactive[FrameBox]
},#]&)/*
ReplaceRepeated[Inactive[TooltipBox|TagBox][e_,l__]:>e]/*
ReplaceAll[Inactive[GraphicsComplexBox][pts_,others__]:>Inactive[GraphicsComplexBox][pts//OptionValue["Transform"]/*ScalingTransform[{1,OptionValue[AspectRatio]}],others]]/*
ReplaceAll[(FontSize->_):>(FontSize->10)]/*
ReplaceAll[{(AspectRatio->_):>(AspectRatio->Automatic),(PlotRangePadding->_):>Sequence[]}]/*
Activate/*RawBoxes


ClearAll[GraphEdgeSleek]
GraphEdgeSleek[graph_]:=graph//ToBoxes//#/.BezierCurveBox->BSplineCurveBox&//MakeExpression//ReleaseHold


Clear[colorFromHex, colorToHex]
colorFromHex[hexcolor_String, colorspace_: RGBColor] := 
 Characters[hexcolor] // Partition[#, 2] & // 
   ToExpression[StringJoin[{"16^^", ##}]] & @@@ # & // 
      colorspace @@ (#/255) &

colorToHex[color_?ColorQ] := 
 ColorConvert[color, "RGB"] // List @@ # & // Round[255 #] & // 
    IntegerString[#, 16, 2] & // StringJoin // ToUpperCase


ClearAll[toRegularArray]
toRegularArray[raggedLst_] :=
                              Module[{walk, temp, pos, lvl, dim},

                                     SetAttributes[walk, HoldAllComplete];
                                     walk[lst_List] :=
                                                       If[
                                                          And[lst =!= {}, And @@ (Head[#] === List & /@ lst)],
                                                          walk /@ lst,
                                                          "walk"[Length@lst]
                                                        ];
                                     walk[expr_] := expr;

                                     dim = FixedPoint[
                                                       (
                                                         temp = walk[#];
                                                         pos = Join[#, {1}] & /@ Position[temp, "walk"[_]];
                                                         lvl = Min[Length /@ pos];
                                                         Sow[
                                                              Select[pos, Length[#] == lvl &] // Extract[temp, #] & // Max
                                                            ];
                                                         temp
                                                       ) &,
                                                       raggedLst
                                                     ] // Reap // #[[2, 1, ;; -2]] & // Reverse;

                                     Remove[walk];
                                     PadRight[raggedLst, dim, Missing["Nonexistent"]]
                                     
                                    ]

(* SetOptions[$FrontEndSession, TranslationOptions -> {"Language" -> "CustomAnnotation"}] *)


ClearAll[inputHistoryViewer]
inputHistoryViewer[range:{__Integer}]:=Column[InString/@range//ToExpression//RawBoxes/@#&//Style[#,"Input",ShowStringCharacters->True]&/@#&,Frame->All,ItemSize->Full]


ClearAll[StringSplitNested]
StringSplitNested[str_String,delimiterLst:{__}]:=Fold[Function[{expr,delimiter},Map[StringSplit[#,delimiter]&,expr,{-1}]],str,delimiterLst]
StringSplitNested[str_String,deliStr_String]:=StringSplitNested[str,Characters@deliStr]