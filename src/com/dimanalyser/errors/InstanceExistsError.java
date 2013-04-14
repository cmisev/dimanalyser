package com.dimanalyser.errors;


public class InstanceExistsError extends InterpretationError {

	/**
	 * 
	 */
	private static final long serialVersionUID = -462724666466854980L;

	public InstanceExistsError(String name) {
		super(String.format("Instance %s already exists in current scope",name));
	}
}
