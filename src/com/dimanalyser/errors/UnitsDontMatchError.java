package com.dimanalyser.errors;

import com.dimanalyser.interpreter.StackElement;
import com.dimanalyser.variablemanager.PhysicalUnit;

public class UnitsDontMatchError extends InterpretationError {




	/**
	 * 
	 */
	private static final long serialVersionUID = -8451744034443520259L;


	public UnitsDontMatchError(StackElement lhs, StackElement rhs,
			StackElement s) {
		super(String.format("Units don't match at %s operation. [%s]=%s should be [%s]=%s ",s.getExpression(),lhs.getExpression(),lhs.getUnit().toString(),rhs.getExpression(),rhs.getUnit().toString()));
	}


	public UnitsDontMatchError(int i, StackElement par, PhysicalUnit expected, StackElement s) {
		super(String.format("Parameter Units of parameter %d don't match in function call %s. [%s]=%s should be %s ",i,s.getExpression(),par.getExpression(),par.getUnit().toString(),expected.toString()));
	}
}
