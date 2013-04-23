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

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import com.dimanalyser.common.Globals;
import com.dimanalyser.errors.ExponentNotScalarError;
import com.dimanalyser.errors.InterpretationError;
import com.dimanalyser.errors.LanguageNotSupportedError;
import com.dimanalyser.errors.UnbalancedBracesError;
import com.dimanalyser.interpreter.Interpreter;
import com.dimanalyser.interpreter.InterpreterFactory;
import com.dimanalyser.variablemanager.PhysicalUnit;

public class InterpreterTest {

	@Before
	public void setUp() throws Exception {
		Globals.initUnits();
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
	public void testParseUnitDeclarationsFromComment() {
		try {

			Interpreter ip = InterpreterFactory.getInterpreter("fortran");
			List<PhysicalUnit> actualList = ip.parseUnitDeclarationsFromComment("U((kg*m)/(s^2))  () U(N*m)");
			
			assertEquals(2,actualList.size());
			assertTrue(Globals.units.get("N").equals(actualList.get(0)));
			assertTrue(Globals.units.get("J").equals(actualList.get(1)));
			
		} catch (UnbalancedBracesError e) {
			fail("UnbalancedBracesError raised while it shouldn't");
		} catch (ExponentNotScalarError e) {
			fail("ExponentNotScalarError raised while it shouldn't");
		} catch (LanguageNotSupportedError e) {
			fail("LanguageNotSupportedError raised while it shouldn't");
		}
		
	}
	
	@Test
	public void testFortranInterpreter() {
		List<String> lines = new ArrayList<String>();
		File file = new File("testfortran.f90");
		String line;
		
		try {
			BufferedReader reader = new BufferedReader(new FileReader(file));
			while((line = reader.readLine())!=null) {
				lines.add(line);
			}
			reader.close();
		} catch (FileNotFoundException e) {
			fail("FileNotFoundException raised while it shouldn't");
		} catch (IOException e) {
			fail("IOException raised while it shouldn't");
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
			fail("LanguageNotSupportedError raised while it shouldn't");
		}
	}

}
