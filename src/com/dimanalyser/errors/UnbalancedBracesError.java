package com.dimanalyser.errors;

public class UnbalancedBracesError extends InterpretationError {


	/**
	 * 
	 */
	private static final long serialVersionUID = 4639086983251752861L;

	public UnbalancedBracesError() {
		super("Braces are unbalanced");
	}
}
