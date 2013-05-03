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

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import com.dimanalyser.common.Globals;
import com.dimanalyser.errors.ExponentNotScalarError;
import com.dimanalyser.variablemanager.PhysicalUnit;

public class PhysicalUnitTest {

	@Before
	public void setUp() throws Exception {
	}

	@After
	public void tearDown() throws Exception {
	}

	@Test
	public void testPhysicalUnit() {
		assertNotNull(new PhysicalUnit(-1.54, new double[]{1.5,2.0,1/3.}));
	}

	@Test
	public void testOperators() {
		
		try {
			assertTrue(Globals.getInstance().getUnit("J").equals(
					PhysicalUnit.fraction(
							PhysicalUnit.product(
									Globals.getInstance().getUnit("kg"),
									PhysicalUnit.power(
											Globals.getInstance().getUnit("m"),
											PhysicalUnit.product(Globals.getInstance().getUnitless(), 2.0)
									)),
							PhysicalUnit.power(
									Globals.getInstance().getUnit("s"),
									PhysicalUnit.product(Globals.getInstance().getUnitless(), 2.0)
							)
					)));
		} catch (ExponentNotScalarError e) {
			fail("Thrown ExponentNotScalarError while it shouldn't");
		}

		boolean thrownError = false;
		try {
			PhysicalUnit.power(
					Globals.getInstance().getUnitless(),
					Globals.getInstance().getUnit("J"));
			
		} catch (ExponentNotScalarError e) {
			thrownError = true;
		}
		assertTrue(thrownError);
		
		
		assertFalse(Globals.getInstance().getUnit("N").equals(Globals.getInstance().getUnitless()));
		assertTrue(Globals.getInstance().getUnitless().equals(PhysicalUnit.product(Globals.getInstance().getUnitless(), 1.24)));
		
	}


	@Test
	public void testToString() {
		assertEquals("m",Globals.getInstance().getUnit("m").toString());
		assertEquals("kg",Globals.getInstance().getUnit("kg").toString());
		assertEquals("s",Globals.getInstance().getUnit("s").toString());
		
		assertEquals("m^2 kg s^-2",Globals.getInstance().getUnit("J").toString());
		assertEquals("m kg s^-2",Globals.getInstance().getUnit("N").toString());
		try {
			assertEquals("m^4 kg^2 s^-4",PhysicalUnit.power(Globals.getInstance().getUnit("J"),PhysicalUnit.product(Globals.getInstance().getUnitless(), 2.0)).toString());
		} catch (ExponentNotScalarError e) {
			fail("Thrown ExponentNotScalarError while it shouldn't");
		}

		
		
	}

}
