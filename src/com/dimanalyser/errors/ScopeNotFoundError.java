package com.dimanalyser.errors;


public class ScopeNotFoundError extends InterpretationError {
	
	/**
	 * 
	 */
	private static final long serialVersionUID = 5906508565383644827L;

	public ScopeNotFoundError(String name) {
		super(String.format("Scope %s was not found",name));
	}
}
