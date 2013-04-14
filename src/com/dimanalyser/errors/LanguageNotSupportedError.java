package com.dimanalyser.errors;


public class LanguageNotSupportedError extends InterpretationError {

	/**
	 * 
	 */
	private static final long serialVersionUID = 4659586835908480687L;

	public LanguageNotSupportedError(String name) {
		super(String.format("Language %s is not yet supported",name));
	}
}
