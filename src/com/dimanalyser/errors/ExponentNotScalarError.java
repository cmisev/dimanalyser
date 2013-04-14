package com.dimanalyser.errors;

public class ExponentNotScalarError extends InterpretationError {


	/**
	 * 
	 */
	private static final long serialVersionUID = 7073000237145281096L;

	public ExponentNotScalarError() {
		super("Exponents can't have a physical unit");
	}
}
