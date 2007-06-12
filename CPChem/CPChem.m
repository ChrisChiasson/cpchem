(* Mathematica Package *)

(* Created by the Wolfram Workbench Jun 12, 2007 *)

BeginPackage["CPChem`"]
(* Exported symbols added here with SymbolName::usage *) 


CPChem`Private`elements={{Chlorine,"Cl"},{Silver,"Ag"},{Strontium,"Sn"},
	{Hydrogen,"H"},{Zinc,"Zn"}}


Molecule::usage="Molecule[elem1@n1,elem2@n2,...,chrg] represents a molecule \
containing n1 atoms of element elem1, n2 atoms of element elem2, up to however \
many elements are desired, and having an overall electric charge of chrg."


ReactionAtomBalance::usage="ReactionAtomBalance[rxn] sums up the elements on \
the left and right sides of a reaction and sets them Equal to each other."


Begin["`Private`"]
(* Implementation of the package *)


MakeBoxes[chemPlus,_]="+"

MakeBoxes[chemMinus,_]="-"


MakeBoxes[chemSuperscript@-1,form_]:=MakeBoxes[chemMinus,form]

MakeBoxes[chemSuperscript@1,form_]:=MakeBoxes[chemPlus,form]

MakeBoxes[chemSuperscript@z_/;NumberQ@Unevaluated@z&&
			Element[Unevaluated@z,Reals]&&
			Unevaluated@z=!=0,
		form_]:=
	With[{absZ=Abs@Unevaluated@z,signZ=Sign@Unevaluated@z},
		StyleBox[
			RowBox[{MakeBoxes[absZ,form],
				MakeBoxes[chemSuperscript@signZ,form]}],
			AutoSpacing->False
			]
		]

MakeBoxes[chemSuperscript@z_,form_]:=MakeBoxes[z,form]


MakeBoxes[chemSubscript@y_,form_]:=MakeBoxes[y,form]


setElementFormat@x_:=(
	MakeBoxes[x@1|x[1,0],form_]:=MakeBoxes[x,form];
	MakeBoxes[x@y_|x[y_,0],form_]:=
		SubscriptBox[MakeBoxes[x,form],MakeBoxes[chemSubscript@y,form]];
	MakeBoxes[x[1,z_],form_]:=
		SuperscriptBox[MakeBoxes[x,form],MakeBoxes[chemSuperscript@z,form]];
	MakeBoxes[x[y_,z_],form_]:=
		SubsuperscriptBox[MakeBoxes[x,form],MakeBoxes[chemSubscript@y,form],
			MakeBoxes[chemSuperscript@z,form]]
	)

setElementFormat[x_,abbrev_]:=(setElementFormat@x;MakeBoxes[x,_]=abbrev;x)


formatChargedElements[HoldComplete[chargedElement_],form_]:=
	MakeBoxes[chargedElement,form]

formatChargedElements[HoldComplete[chargedElements__],form_]:=
	StyleBox[MakeBoxes[Times[chargedElements],form],ZeroWidthTimes->True]

MakeBoxes[Molecule[elements__/;!Extract[HoldComplete[elements],{-1},AtomQ],z_],
	form_]:=
	formatChargedElements[Insert[HoldComplete@elements,z,{-1,2}],form]


Through[{Unprotect,Update}[Times]]

formatMolecules[HoldComplete[newArgs__],form_]:=MakeBoxes[Times[newArgs],form]

MakeBoxes[Times[args__/;MemberQ[Unevaluated@{args},_Molecule]],form_]:=
	formatMolecules[Replace[HoldComplete[args],molecule_Molecule:>
		PrecedenceForm[molecule,1000],{1}],form]

Through[{Update,Protect,Update}[Times]]


setElementFormat@@@elements


ReactionAtomBalance[_[sides__]]:=
	Equal[sides]/.Molecule[args__,_]:>Plus@args/.
		(elementHead:Alternatives@@CPChem`Private`elements[[All,1]])[
			elementNumber_]:>elementNumber*elementHead


End[]

EndPackage[]