package com.dimanalyser.common;

import java.util.HashMap;

import com.dimanalyser.variablemanager.PhysicalUnit;

public class Globals {

	public static final int NUM_BASEUNITS=3;
	public static final PhysicalUnit UNIT_UNITLESS = new PhysicalUnit(1.0, 0.0,0.0,0.0 );
	public static final String COMMENT_START = "!";
	public static String fileName = "testfortran.f90";
	
	public static HashMap<String,PhysicalUnit> units;
	
	public static void debug(String message, int depth) {
		System.out.println(new String(new char[4*depth]).replace("\0", " ") + message);
	}

	public static void initUnits() {
		units = new HashMap<String, PhysicalUnit>();
		units.put("m", new PhysicalUnit(1.0, 1.0,0.0,0.0 ));
		units.put("kg",new PhysicalUnit(1.0, 0.0,1.0,0.0 ));
		units.put("s", new PhysicalUnit(1.0, 0.0,0.0,1.0 ));
		units.put("N", new PhysicalUnit(1.0, 1.0,1.0,-2.0 ));
		units.put("J", new PhysicalUnit(1.0, 2.0,1.0,-2.0 ));
	}
}
