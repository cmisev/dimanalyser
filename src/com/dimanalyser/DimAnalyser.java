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

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

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
		
		List<String> lines = new ArrayList<String>();
		File file = new File(Globals.fileName);
		String line;
		
		try {
			BufferedReader reader = new BufferedReader(new FileReader(file));
			while((line = reader.readLine())!=null) {
				lines.add(line);
			}
			reader.close();
		} catch (FileNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		int linenumber = 0;
		
		try {
			Interpreter interpreter;
			interpreter = InterpreterFactory.getInterpreter("fortran");
		
			while(linenumber<lines.size()) {
				try {
					linenumber = interpreter.interpretStatements(linenumber, lines);
				} catch (InterpretationError e) {
					System.err.println(String.format("Error in %s at line %d: %s",Globals.fileName,linenumber+1,e.getMessage()));
					linenumber++;
				}
			}
		} catch (LanguageNotSupportedError e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		
	}

}
