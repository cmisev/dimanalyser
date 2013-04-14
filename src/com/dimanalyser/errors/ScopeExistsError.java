package com.dimanalyser.errors;


public class ScopeExistsError extends InterpretationError {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1156667799329253719L;

	public ScopeExistsError(String name) {
		super(String.format("Scope %s already exists in current scope",name));
	}
}
