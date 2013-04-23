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
		Globals.initUnits();
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
			assertTrue(Globals.units.get("J").equals(
					PhysicalUnit.fraction(
							PhysicalUnit.product(
									Globals.units.get("kg"),
									PhysicalUnit.power(
											Globals.units.get("m"),
											PhysicalUnit.product(Globals.UNIT_UNITLESS, 2.0)
									)),
							PhysicalUnit.power(
									Globals.units.get("s"),
									PhysicalUnit.product(Globals.UNIT_UNITLESS, 2.0)
							)
					)));
		} catch (ExponentNotScalarError e) {
			fail("Thrown ExponentNotScalarError while it shouldn't");
		}

		boolean thrownError = false;
		try {
			PhysicalUnit.power(
					Globals.UNIT_UNITLESS,
					Globals.units.get("J"));
			
		} catch (ExponentNotScalarError e) {
			thrownError = true;
		}
		assertTrue(thrownError);
		
		
		assertFalse(Globals.units.get("N").equals(Globals.UNIT_UNITLESS));
		assertTrue(Globals.UNIT_UNITLESS.equals(PhysicalUnit.product(Globals.UNIT_UNITLESS, 1.24)));
		
	}


	@Test
	public void testToString() {
		assertEquals("m",Globals.units.get("m").toString());
		assertEquals("kg",Globals.units.get("kg").toString());
		assertEquals("s",Globals.units.get("s").toString());
		
		assertEquals("m^2 kg s^-2",Globals.units.get("J").toString());
		assertEquals("m kg s^-2",Globals.units.get("N").toString());
		try {
			assertEquals("m^4 kg^2 s^-4",PhysicalUnit.power(Globals.units.get("J"),PhysicalUnit.product(Globals.UNIT_UNITLESS, 2.0)).toString());
		} catch (ExponentNotScalarError e) {
			fail("Thrown ExponentNotScalarError while it shouldn't");
		}

		
		
	}

}
