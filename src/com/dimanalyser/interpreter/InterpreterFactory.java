/*
 *  Copyright © 2013 Cyril Misev
 * 
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package com.dimanalyser.interpreter;

import com.dimanalyser.errors.LanguageNotSupportedError;

/**
 * Factory for concrete language interpreters
 * 
 * @author Cyril Misev <c.misev@gmail.com>
 *
 */
public class InterpreterFactory {

	/**
	 * Return an instance of an interpreter of a given language
	 * 
	 * @param language the language of the interpreter
	 * @return the interpreter instance
	 * @throws LanguageNotSupportedError
	 */
	public static Interpreter getInterpreter(String language) throws LanguageNotSupportedError {
		// TODO add identification by file extension
		if (language.equalsIgnoreCase("fortran")) {
			return new FortranInterpreter();
		} else {
			throw new LanguageNotSupportedError(language);
		}
	}
	
}
