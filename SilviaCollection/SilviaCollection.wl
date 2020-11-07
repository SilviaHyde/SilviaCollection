(* ::Package:: *)

Package["SilviaCollection`"]


PackageExport["nativeSize"]
PackageExport["checkerboard"]
PackageExport["gammaDecode"]
PackageExport["gammaEncode"]
PackageExport["imgGammaDecode"]
PackageExport["imgGammaEncode"]
PackageExport["FindDivisionsExact"]
PackageExport["pipe"]
PackageExport["branch"]
PackageExport["branchSeq"]
PackageExport["levelIndentFunc"]
PackageExport["horizontalTreeForm"]
PackageExport["GraphEdgeSleek"]
PackageExport["toRegularArray"]
PackageExport["inputHistoryViewer"]
PackageExport["StringSplitNested"]
PackageExport["colorFromHex"]
PackageExport["colorToHex"]
PackageExport["DatasetGrid"]

ClearAll[FindDivisionsExact]
FindDivisionsExact[range : {_, _}, n_Integer?(# >= 2 &)] := Rescale[Range[n] // N // Rescale, {0, 1}, range]
FindDivisionsExact[n_Integer?(#>=2&)]:=FindDivisionsExact[{0,1},n]

ClearAll[pipe,branch,branchSeq]
pipe=RightComposition;
branch=Through@*{##}&;
branchSeq=pipe[branch@##,Apply@Sequence]&;


ClearAll[nativeSize]
nativeSize[magF_:1]:=
	Module[{
		nativeResolution=120,Sstd=72,nbMgfy=AbsoluteCurrentValue[EvaluationNotebook[],Magnification],$width=ImageDimensions[#][[1]]
	}
	, Image[#,ImageSize->magF Sstd/nativeResolution $width/nbMgfy]
	]&


ClearAll[checkerboard]
checkerboard[width_Integer,height_Integer,step:_Integer:100,gap:{_Integer,_Integer}:{15,5}]:=
ParallelTable[
	(UnitStep[Mod[i,step]/step-1/2]+UnitStep[Mod[j,step]/step-1/2])//Plus[#,(-1)^UnitStep[1-#] 1/3 UnitStep[Mod[i+j,gap[[1]]]-gap[[2]]]]&
	,{i,height},{j,width}
	]//Rescale//Image


ClearAll[gammaDecode,gammaEncode]
gammaDecode=Compile[{{c,_Real}}
	,With[{z=Abs[c]},Sign[c]If[z<=.04045,z/12.92,((z+0.055)/1.055)^2.4]]
	,RuntimeAttributes->{Listable},Parallelization->True
	,"RuntimeOptions"->"Speed"
	,CompilationTarget->"C"
	];
gammaEncode=Compile[{{c,_Real}}
	,With[{z=Abs[c]},Sign[c]If[z<=.04045/12.92,12.92z,1.055z^(1/2.4)-0.055]]
	,RuntimeAttributes->{Listable},Parallelization->True
	,"RuntimeOptions"->"Speed"
	,CompilationTarget->"C"
	];


ClearAll[imgGammaDecode,imgGammaEncode]
imgGammaDecode=pipe[
	branchSeq[
		pipe[ImageData[#,"Real"]&,gammaDecode]
		,Information[#,"ColorSpace"]&
	]
	,Image[#1,ColorSpace->#2]&
];
imgGammaEncode=pipe[
	branchSeq[
		pipe[ImageData[#,"Real"]&,gammaEncode]
		,Information[#,"ColorSpace"]&
	]
	,Image[#1,ColorSpace->#2]&
];


ClearAll[levelIndentFunc]
SilviaCollection`Private`levelIndentFuncMsg =
  {
    RowBox[{RowBox[{"traceRes", "=", RowBox[{"Trace", "[", RowBox[{RowBox[{"Reduce", "[", RowBox[{RowBox[{SuperscriptBox["x", "2"], "\[Equal]", RowBox[{"-", "1"}]}], ",", "x"}], "]"}], ",", RowBox[{"TraceInternal", "\[Rule]", "True"}], ",", RowBox[{"TraceDepth", "\[Rule]", "3"}]}], "]"}]}], ";"}]
    , RowBox[{"Export", "[", RowBox[{"\"tracePrintTest.txt\"", ",", RowBox[{"levelIndentFunc", "[", "traceRes", "]"}], ",", "\"String\""}], "]"}]
    } // pipe[
      Map@RawBoxes
      , Column
      , Grid[{
            {Style["Usage Example: ", Bold, 14], SpanFromLeft}
            , {"\t", Style[#,ShowStringCharacters->True]}
          }, Alignment -> Left, Frame -> False
        ] &
    ];
levelIndentFunc::usage=ToString[ SilviaCollection`Private`levelIndentFuncMsg, StandardForm ]
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


ClearAll[horizontalTreeForm]
Options[horizontalTreeForm]={"Transform"->RotationTransform[\[Pi]/2],AspectRatio->1.5};
horizontalTreeForm[OptionsPattern[]]:=
  pipe[
   TreeForm
   , ToBoxes
   , Block[{
      GraphicsBox = Inactive[GraphicsBox]
      , TagBox = Inactive[TagBox]
      , GraphicsComplexBox = Inactive[GraphicsComplexBox]
      , TooltipBox = Inactive[TooltipBox]
      , InsetBox = Inactive[InsetBox]
      , FormBox = Inactive[FormBox]
      , StyleBox = Inactive[StyleBox]
      , RowBox = Inactive[RowBox]
      , FrameBox = Inactive[FrameBox]
      }, #] &
   , ReplaceRepeated[Inactive[TooltipBox | TagBox][e_, l__] :> e]
   , ReplaceAll[Inactive[GraphicsComplexBox][pts_, others__] :>
      Inactive[GraphicsComplexBox][
       pts // pipe[
         OptionValue["Transform"]
         , ScalingTransform[{1, OptionValue[AspectRatio]}]
         ]
       , others]
     ]
   , ReplaceAll[(FontSize -> _) :> (FontSize -> 10)]
   , ReplaceAll[{
      (AspectRatio -> _) :> (AspectRatio -> Automatic)
      , (PlotRangePadding -> _) :> Sequence[]
     }]
   , Activate
   , RawBoxes
   ]


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


ClearAll[inputHistoryViewer]
inputHistoryViewer[range:{__Integer}]:=Column[InString/@range//ToExpression//RawBoxes/@#&//Style[#,"Input",ShowStringCharacters->True]&/@#&,Frame->All,ItemSize->Full]


ClearAll[StringSplitNested]
StringSplitNested[str_String,delimiterLst:{__}]:=Fold[Function[{expr,delimiter},Map[StringSplit[#,delimiter]&,expr,{-1}]],str,delimiterLst]
StringSplitNested[str_String,deliStr_String]:=StringSplitNested[str,Characters@deliStr]


ClearAll[DatasetGrid]
DatasetGrid = RightComposition[
   ToBoxes
   , Cases[#, Style[Grid[__], __], \[Infinity]][[1]] &
   , ReplaceRepeated[{
        Mouseover[e_, ___] :> e, MouseAppearance[e_, ___] :> e, 
        EventHandler[e_, ___] :> e, 
        Annotation[e_, ___] :> e, (ContextMenu -> _) :> Sequence[]
     }]
   ];

