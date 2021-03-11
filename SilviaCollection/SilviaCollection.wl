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

(* Use built-in Subdivide instead: *)
ClearAll[FindDivisionsExact]
FindDivisionsExact::deprec="FindDivisionsExact is deprecated, use built-in Subdivide instead.";
Message[FindDivisionsExact::deprec]
FindDivisionsExact[range : {_, _}, n_Integer?(# >= 2 &)] := Rescale[Range[n] // N // Rescale, {0, 1}, range]
FindDivisionsExact[n_Integer?(#>=2&)]:=FindDivisionsExact[{0,1},n]

ClearAll[pipe,branch,branchSeq]
pipe=RightComposition;
branch=Through@*{##}&;
branchSeq=pipe[branch@##,Apply@Sequence]&;


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
DatasetGrid = pipe[
   ToBoxes
   , Cases[#, Style[Grid[__], __], \[Infinity]][[1]] &
   , ReplaceRepeated[{
        Mouseover[e_, ___] :> e, MouseAppearance[e_, ___] :> e, 
        EventHandler[e_, ___] :> e, 
        Annotation[e_, ___] :> e, (ContextMenu -> _) :> Sequence[]
     }]
   ];

