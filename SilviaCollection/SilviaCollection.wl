(* ::Package:: *)

Package["SilviaCollection`"]


PackageExport["pipe"]
PackageExport["pipeList"]
PackageExport["branch"]
PackageExport["branchSeq"]
PackageExport["deCurry"]

PackageExport["nativeSize"]
PackageExport["checkerboard"]
PackageExport["gammaDecode"]
PackageExport["gammaEncode"]
PackageExport["imgGammaDecode"]
PackageExport["imgGammaEncode"]
PackageExport["lsRGB2okLab"]
PackageExport["okLab2lsRGB"]

PackageExport["levelIndentFunc"]
PackageExport["horizontalTreeForm"]
PackageExport["GraphEdgeSleek"]
PackageExport["toRegularArray"]
PackageExport["inputHistoryViewer"]
PackageExport["StringSplitNested"]
PackageExport["colorFromHex"]
PackageExport["colorToHex"]
PackageExport["DatasetGrid"]
PackageExport["multiDimGrid"]
PackageExport["randomIntegerPartitions"]


ClearAll[pipe,pipeList,branch,branchSeq]

pipe=RightComposition;
pipeList=pipe[Fold[branchSeq[Identity,pipe[#2,#1]]&,Identity,Reverse@{##}],List]&;

branch=Through@*{##}&;
branchSeq=pipe[branch@##,Apply@Sequence]&;

deCurry = pipe[
                    branchSeq[ Head, Apply@List ]
                   ,CurryApplied[3][Fold][Construct]
                   ];
(*
In[]:= f[a, b, c, d] // reverseCurry
Out[]= f[a][b][c][d]
*)


randomIntegerPartitions[n_Integer,parts_Integer,group_Integer]:=
	RandomVariate[DirichletDistribution[ConstantArray[1,parts]],group]//
		pipe[
			Round[(n-parts)#]\[Transpose]&
			,Append[#,n-parts-Total[#]]\[Transpose]+1&
		]
(*
In[]:= randomIntegerPartitions[10,5,3] // pipe[Transpose,Join[#,Thread[{" \[Sum] \[DoubleLongRightArrow] ",Total@#}]\[Transpose]]&,Transpose,Grid]
Out[]=
		2	2	1	2	3	 \[Sum] \[DoubleLongRightArrow] 	10
		3	3	1	2	1	 \[Sum] \[DoubleLongRightArrow] 	10
		2	1	1	4	2	 \[Sum] \[DoubleLongRightArrow] 	10
*)


ClearAll[nativeSize]
(*
nativeSize[magF_:1]:=
	Module[{
		nativeResolution=120,Sstd=72,nbMgfy=AbsoluteCurrentValue[EvaluationNotebook[],Magnification],$width=ImageDimensions[#][[1]]
	}
	, Image[#,ImageSize->magF Sstd/nativeResolution $width/nbMgfy]
	]&
*)
nativeSize[magF_:1]:=
	Module[{
		nativeScale=SystemInformation["Devices","ConnectedDisplays"]//Lookup["Scale"]//First
		(* feMgfy does NOT work like nbMgfy. It won't affect image size as counting in pixels.
		,feMgfy=AbsoluteCurrentValue[$FrontEndSession,Magnification]*)
		,nbMgfy=AbsoluteCurrentValue[EvaluationNotebook[],Magnification]
		,$width=ImageDimensions[#][[1]]
	}
	, Image[#,ImageSize->(magF*$width)/(nativeScale*(*feMgfy**)nbMgfy)]
	]&


multiDimGrid[data_?ArrayQ] :=
 Module[{array = data, oddQ, fullDim, levels, gridDepth, rowDim, colDim, rowAccu, colAccu
         , render = Function[{content, bg}, Item[Style[content, 6], Background -> bg, FrameStyle -> Directive[GrayLevel[1], AbsoluteThickness[.5]], Alignment -> {Center, Center}]]
        }
        , oddQ = OddQ@ArrayDepth@array
        ; If[oddQ, array = {array}]
        ; fullDim = Dimensions@array
        ; levels = 2 Range[Length[fullDim]/2] - 2
        ; gridDepth = Length@levels
        ; {rowDim, colDim} = fullDim // {#[[1 ;; -1 ;; 2]], #[[2 ;; -1 ;; 2]]} &
        ; rowAccu = FoldList[#2 (#1 + 1) &, rowDim // Rest // Append[0] // Reverse] // Reverse
        ; colAccu = FoldList[#2 (#1 + 1) &, colDim // Rest // Append[0] // Reverse] // Reverse
        ; array // pipe[
                        Map[If[Head[#] === Item, #, Style[#, 8]] &, #, {Length[fullDim]}] &
                        , CurryApplied[Fold, {1, 3, 2}][
                              Function[{grid, lvl, dim, gap, styleF}
                                       , Map[pipe[ArrayFlatten,
                                                  {styleF /@ Range[dim[[2]]], ConstantArray[SpanFromLeft, {dim[[2]], gap[[2]]}]} // pipe[Apply@Riffle, Flatten, Prepend]
                                                  , Transpose,
                                                  {styleF /@ Range[dim[[1]]], ConstantArray[SpanFromAbove, {dim[[1]], gap[[1]]}]} // pipe[Apply@Riffle, Flatten, Prepend[styleF@""], Prepend]
                                                  , Transpose]
                                             , grid, {lvl}
                                            ]
                                      ][#1, Sequence @@ #2] &
                              , {
                                  levels // Reverse
                                  , Map[Reverse, {rowDim, colDim}]//Transpose
                                  , Map[Reverse, {rowAccu, colAccu}]//Transpose
                                  , If[Length[levels] == 1
                                        , {CurryApplied[render, {2, 1}]@GrayLevel[0.7]}
                                        , N[Subdivide[.3, 1, Length[levels] - 1]] // pipe[Reverse, pipe[ColorData["GrayYellowTones"], Lighter[#, .2] &] /@ # &, CurryApplied[render, {2, 1}] /@ # &]
                                      ]
                                }//Transpose
                         ]
                        , Grid[#, Frame -> None, Alignment -> {".", Center}] &
                        , If[oddQ, MapAt[Rest, {1, ;;}], Identity]
                        , Append[Spacings->{Automatic,-0.1}]
                       ]
  ]


ClearAll[checkerboard]
checkerboard[width_Integer,height_Integer,step:_Integer:100,gap:{_Integer,_Integer}:{15,5}]:=
ParallelTable[
	(UnitStep[Mod[i,step]/step-1/2]+UnitStep[Mod[j,step]/step-1/2])//Plus[#,(-1)^UnitStep[1-#] 1/3 UnitStep[Mod[i+j,gap[[1]]]-gap[[2]]]]&
	,{i,height},{j,width}
	]//Rescale//Image


$gammaDClib = FindLibrary["gammaDecode"];
$gammaEClib = FindLibrary["gammaEncode"];
ClearAll[gammaDecode,gammaEncode]
gammaDecode=
	CompiledFunction[{11,12.2,5848},{_Real},{{3,0,0},{3,0,5}},{{0.04045,{3,0,2}},{0.055,{3,0,6}},{2.4,{3,0,9}},{0.9478672985781991,{3,0,8}},{0.,{3,0,3}},{0.07739938080495357,{3,0,4}}},{1,1,11,0,0}
		,{{40,38,3,0,0,3,0,1},{40,44,3,0,0,2,0,0},{27,4,3,1,2,0},{2,0,4},{16,1,4,5},{7,5,7},{3,5},{13,1,6,7},{16,7,8,7},{41,263,3,0,7,3,0,9,3,0,10},{7,10,7},{10,0,5},{16,5,7,5},{1}}
		,Function[{c},With[{z=Abs[c]},Sign[c] If[z<=0.04045,z 0.07739938080495357,((z+0.055) 0.9478672985781991)^2.4]],Listable],Evaluate
		,LibraryFunction[$gammaDClib,"compiledFunction2",{{Real,0,"Constant"}},Real]
	];
gammaEncode=
	CompiledFunction[{11,12.2,5848},{_Real},{{3,0,0},{3,0,5}},{{-0.055,{3,0,10}},{0.0031308049535603718,{3,0,2}},{1.055,{3,0,6}},{12.92,{3,0,4}},{0.4166666666666667,{3,0,7}},{0.,{3,0,3}}},{1,1,11,0,0}
		,{{40,38,3,0,0,3,0,1},{40,44,3,0,0,2,0,0},{27,4,3,1,2,0},{2,0,4},{16,4,1,5},{7,5,8},{3,5},{41,263,3,0,1,3,0,7,3,0,8},{16,6,8,9},{13,9,10,9},{7,9,8},{10,0,5},{16,5,8,5},{1}}
		,Function[{c},With[{z=Abs[c]},Sign[c] If[z<=0.0031308049535603718,12.92 z,1.055 z^0.4166666666666667-0.055]],Listable],Evaluate
		,LibraryFunction[$gammaEClib,"compiledFunction3",{{Real,0,"Constant"}},Real]
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


lsrgb2lms={{0.4122214708`,0.5363325363`,0.0514459929`},{0.2119034982`,0.6806995451`,0.1073969566`},{0.0883024619`,0.2817188376`,0.6299787005`}}\[Transpose];
lsrgb2lmsInv=Inverse@lsrgb2lms;
lms2oklab={{0.2104542553`,0.793617785`,-0.0040720468`},{1.9779984951`,-2.428592205`,0.4505937099`},{0.0259040371`,0.7827717662`,-0.808675766`}}\[Transpose];
lms2oklabInv=Inverse@lms2oklab;


lsRGB2okLab[rgb_]/;MatchQ[Dimensions[rgb],{3}|{_,3}]:=CubeRoot[rgb . lsrgb2lms] . lms2oklab
okLab2lsRGB[lab_]/;MatchQ[Dimensions[lab],{3}|{_,3}]:=Power[lab . lms2oklabInv,3] . lsrgb2lmsInv


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
    lst /. e_HoldForm :> StringReplace[StringTake[ToString[e, InputForm], {10, -2}],"\n"->"\\n"],
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
GraphEdgeSleek=pipe[
			ToBoxes,ToString[#,InputForm]&,ToExpression
			,Insert[BaseStyle->{BezierCurveBoxOptions->{Method->{"SplinePoints"->20}}},{2,-1}]
			,MakeExpression,ReleaseHold
		];


Clear[colorFromHex, colorToHex]
colorFromHex[hexcolor_String, colorspace_: RGBColor] := 
 Characters[hexcolor] // Partition[#, 2] & // 
   ToExpression[StringJoin[{"16^^", ##}]] & @@@ # & // 
      colorspace @@ (#/255) &

colorToHex[color_?ColorQ] := 
 ColorConvert[color, "RGB"] // List @@ # & // Round[255 #] & // 
    IntegerString[#, 16, 2] & // StringJoin // ToUpperCase


ClearAll[toRegularArray]
toRegularArray[raggedLst_, missing_] :=
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
                                     PadRight[raggedLst, dim, missing]
                                     
                                    ]


ClearAll[inputHistoryViewer]
inputHistoryViewer[range:{__Integer}]:=Column[InString/@range//ToExpression//RawBoxes/@#&//Style[#,"Input",ShowStringCharacters->True]&/@#&,Frame->All,ItemSize->Full]


ClearAll[StringSplitNested]
StringSplitNested[str_String,delimiterLst:{__}]:=Fold[Function[{expr,delimiter},Map[StringSplit[#,delimiter]&,expr,{-1}]],str,delimiterLst]
StringSplitNested[str_String,deliStr_String]:=StringSplitNested[str,Characters@deliStr]


ClearAll[DatasetGrid]
DatasetGrid = pipe[
   ToBoxes
   , Cases[#, Style[Grid[__], __], \[Infinity]][[1]] &
   , ReplaceRepeated[{
        Mouseover[e_, ___] :> e, MouseAppearance[e_, ___] :> e, 
        EventHandler[e_, ___] :> e, 
        Annotation[e_, ___] :> e, (ContextMenu -> _) :> Sequence[]
     }]
   , ReplaceAll[Button[lbl_, ___] :> lbl]
   ];

