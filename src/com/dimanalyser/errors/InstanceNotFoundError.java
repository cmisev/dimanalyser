package com.dimanalyser.errors;


public class InstanceNotFoundError extends InterpretationError {
	

	/**
	 * 
	 */
	private static final long serialVersionUID = -6086043716684911573L;

	public InstanceNotFoundError(String name) {
		super(String.format("Instance %s was not found",name));
	}
}
