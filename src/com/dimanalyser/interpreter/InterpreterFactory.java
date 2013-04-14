package com.dimanalyser.interpreter;

import com.dimanalyser.errors.LanguageNotSupportedError;

public class InterpreterFactory {

	
	public static Interpreter getInterpreter(String language) throws LanguageNotSupportedError {
		if (language.equals("fortran")) {
			return new FortranInterpreter();
		} else {
			throw new LanguageNotSupportedError(language);
		}
	}
	
}
