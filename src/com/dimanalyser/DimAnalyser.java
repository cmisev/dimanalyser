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

package com.dimanalyser;

import com.dimanalyser.common.Globals;
import com.dimanalyser.errors.InterpretationError;
import com.dimanalyser.errors.LanguageNotSupportedError;
import com.dimanalyser.interpreter.Interpreter;
import com.dimanalyser.interpreter.InterpreterFactory;

public class DimAnalyser {

	/**
	 * @param args
	 */
	
	
	
	public static void main(String[] args) {
		if (args.length==1) {
			Globals.getInstance().openFile(args[0]);

			try {
				Interpreter interpreter;
				interpreter = InterpreterFactory.getInterpreter("fortran");
			
				while(!Globals.getInstance().fileRead()) {
					try {
						interpreter.interpretStatements();
					} catch (Exception e) {
						Globals.getInstance().errorMessage(e.getMessage());
						if (!(e instanceof InterpretationError)) {
							e.printStackTrace();
						}
					}
				}
			} catch (LanguageNotSupportedError e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}	
		} else {
			System.out.println("Usage: dimanalyser [file name]");
		}
				
	}

}
