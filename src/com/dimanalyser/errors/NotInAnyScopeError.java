package com.dimanalyser.errors;

public class NotInAnyScopeError extends InterpretationError {



	/**
	 * 
	 */
	private static final long serialVersionUID = 6812788000927988093L;

	public NotInAnyScopeError() {
		super("Can't leave scope because not in any scope");
	}
}
