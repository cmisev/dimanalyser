package com.dimanalyser.errors;

import com.dimanalyser.variablemanager.PhysicalUnit;

public class UnitsDontMatchError extends InterpretationError {




	/**
	 * 
	 */
	private static final long serialVersionUID = -8451744034443520259L;

	public UnitsDontMatchError(PhysicalUnit lhs, PhysicalUnit rhs, String operator) {
		super(String.format("Units don't match at %s operation. %s should be %s ",operator,lhs.toString(),rhs.toString()));
	}
	
	public UnitsDontMatchError(PhysicalUnit lhs, PhysicalUnit rhs, String slhs, String srhs, String operator) {
		super(String.format("Units don't match at %s operation. [%s]=%s should be [%s]=%s ",operator,slhs,lhs.toString(),srhs,rhs.toString()));
	}
}
