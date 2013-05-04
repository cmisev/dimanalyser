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
package com.dimanalyser.test;

import static org.junit.Assert.*;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileDescriptor;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.util.Scanner;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import com.dimanalyser.common.Globals;
import com.dimanalyser.errors.InterpretationError;
import com.dimanalyser.errors.LanguageNotSupportedError;
import com.dimanalyser.interpreter.Interpreter;
import com.dimanalyser.interpreter.InterpreterFactory;

public class InterpreterTest {

	@Before
	public void setUp() throws Exception {
	}

	@After
	public void tearDown() throws Exception {
	}

	@Test
	public void testInstanitateInterpreter() {

		assertNotNull(new InterpreterFactory());
		
		try {
			assertNotNull(InterpreterFactory.getInterpreter("fortran"));
		} catch (LanguageNotSupportedError e) {
			fail("LanguageNotSupportedError raised while it shouldn't");
		}
		
		boolean exceptionThrown = false;
		try {
			InterpreterFactory.getInterpreter("alienlanguage");
		} catch (LanguageNotSupportedError e) {
			exceptionThrown = true;
		}
		
		assertTrue(exceptionThrown);
		
	}
	
	@Test
	public void testFortranInterpreter() {
		
		Globals.getInstance().openFile("res/test/fortran/testfortran.f90");

		ByteArrayOutputStream outContent = new ByteArrayOutputStream();
		ByteArrayOutputStream errContent = new ByteArrayOutputStream();
		
		System.setErr(new PrintStream(errContent));
		System.setOut(new PrintStream(outContent));
		
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
		
		try {
			assertEquals(readFile("res/test/fortran/testfortran.output"), outContent.toString());
			assertEquals(readFile("res/test/fortran/testfortran.err"), errContent.toString());
		} catch (IOException e) {
			fail("IOException raised while it shouldn't");
		}
		
		System.setOut(new PrintStream(new FileOutputStream(FileDescriptor.out)));
		System.setErr(new PrintStream(new FileOutputStream(FileDescriptor.err)));

	}
	
	
	private String readFile(String pathname) throws IOException {

	    File file = new File(pathname);
	    StringBuilder fileContents = new StringBuilder((int)file.length());
	    Scanner scanner = new Scanner(file);
	    String lineSeparator = System.getProperty("line.separator");

	    try {
	        while(scanner.hasNextLine()) {        
	            fileContents.append(scanner.nextLine() + lineSeparator);
	        }
	        return fileContents.toString();
	    } finally {
	        scanner.close();
	    }
	}

}
