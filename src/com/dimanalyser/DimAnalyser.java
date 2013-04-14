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
					System.err.println(String.format("Error in %s at line %i: %s",Globals.fileName,linenumber+1,e.getMessage()));
				}
			}
		} catch (LanguageNotSupportedError e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		
	}

}
