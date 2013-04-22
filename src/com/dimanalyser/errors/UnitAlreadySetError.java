package com.dimanalyser.errors;


public class UnitAlreadySetError extends InterpretationError {
	

	/**
	 * 
	 */
	private static final long serialVersionUID = 8865873067121639178L;

	public UnitAlreadySetError(String name) {
		super(String.format("Unit on %s instance was already set",name));
	}
}
