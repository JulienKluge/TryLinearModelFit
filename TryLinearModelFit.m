(* ::Package:: *)

$TryLinearModelFitVersion="0"


Options[TryLinearModelFit]=DeleteDuplicatesBy[Options[LinearModelFit]~Join~Options[NonlinearModelFit],First];

(*Check for valid arguments. Pass to internal evaluation if valid.*)
TryLinearModelFit[data_,form_,parameters_,variables_,opts:OptionsPattern[]]:=iTryLinearModelFit[data,form,parameters,variables,opts]/;And[
	MatchQ[data,       List[(_?NumericQ)..]|List[List[_?NumericQ,_?NumericQ]..]],
	MatchQ[parameters, List[(_Symbol)..]],
	MatchQ[variables,  List[(_Symbol)..]|_Symbol]
]

(*if we have constraints in the model definiton*)
TryLinearModelFit::nocons="TryLinearModelFit can not incorporate constraints for linear model casting."
TryLinearModelFit[data_,{form_,cons_},parameters_,variables_,opts:OptionsPattern[]]:=If[cons=!=True,
	Message[TryLinearModelFit::nocons]; (*we can't support constraints for linear models*)
	TryLinearModelFit[data,form,parameters,variables,opts]
,
	TryLinearModelFit[data,form,parameters,variables,opts]
]


TryLinearModelFit::forbiddenopt="TryLinearModelFit can not incorporate `1` as an option."
TryLinearModelFit::nullopt="TryLinearModelFit does not need `1` specified as an option."

(*we warn the user about unnessecary specified options as well as forbidden [voided effect] options.*)
checkOptions[{}]:=Null
checkOptions[opts_List]:=Module[{
forbiddenOptions={LinearOffsetFunction,NominalVariables},
unnessecaryOptions={AccuracyGoal,EvaluationMonitor,Gradient,MaxIterations,Method,PrecisionGoal,StepMonitor,IncludeConstantBasis}
},
	If[MemberQ[forbiddenOptions,#],
		Message[TryLinearModelFit::forbiddenopt,#];
	,
		If[MemberQ[unnessecaryOptions,#],
			Message[TryLinearModelFit::nullopt,#]
		]
	]&/@(opts[[All,1]]);
]


(*this function splits the current top level expression into leaves in four categories:
free of variables and parameter
free of variables but contains parameters
contains variables but free of parameters
contains variables and parameters*)
splitHead[head_,varPattern_,paramPattern_]:=Module[{leaves},
	leaves=List@@head;
]


(*
This function decomposes an expression into a list of base functions and their associated parameters. It also returns a list of linear offset
functions [free of variables and parameters]. The return has the form:
{{param1,baseFunction1},{param2,basefunction2},linearOffsetFunction}
If there is no linearOffsetFunction found, it will be set to 0
*)

(*there is a special case to be made for polynomials*)
decomposeExpression[expr_?PolynomialQ,varPattern_,paramPattern_]:=Null

(**)
decomposeExpression[expr_Plus,varPattern_,paramPattern_]:=decomposeExpression[#,varPattern,paramPattern]&/@(List@@expr)


Options[iTryLinearModelFit]=Options[TryLinearModelFit];
iTryLinearModelFit[pdata_,pform_,pparameters_,pvariables_,opts:OptionsPattern[]]:=Module[
{varPattern,paramPattern},

	(*this pattern is used for testing if an expression contains the model variables*)
	varPattern=If[Head[pvariables]===Symbol,pvariables,Alternatives@@pvariables];
	
	(*this pattern is used for testing if an expression contains any parameters*)
	paramPattern=Alternatives@@pparameters;
	
	(*check unnessecary and voided options*)
	checkOptions[{opts}];
	
]
