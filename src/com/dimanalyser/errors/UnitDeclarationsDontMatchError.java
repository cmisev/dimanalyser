package com.dimanalyser.errors;

public class UnitDeclarationsDontMatchError extends InterpretationError {



	/**
	 * 
	 */
	private static final long serialVersionUID = 6270446242125793870L;

	public UnitDeclarationsDontMatchError() {
		super("Number of Unit declarations don't match");
	}
}
