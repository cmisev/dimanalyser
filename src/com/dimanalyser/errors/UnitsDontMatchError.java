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
		super(String.format("Units don't match at %s operation. [%s]=%s should be [%s]=%s ",s.getExpression(),lhs.getExpression(),lhs.getUnit().toString(),lhs.getExpression(),rhs.getUnit().toString()));
	}
}
