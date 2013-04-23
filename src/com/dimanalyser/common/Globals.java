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


package com.dimanalyser.common;

import java.util.HashMap;

import com.dimanalyser.variablemanager.PhysicalUnit;


/**
 * 
 * @author Cyril Misev <c.misev@gmail.com>
 *
 */
public class Globals {

	public static final int NUM_BASEUNITS=3;
	public static final PhysicalUnit UNIT_UNITLESS = new PhysicalUnit(new double[]{0.0,0.0,0.0});
	public static final String COMMENT_START = "!";
	public static String fileName = "testfortran.f90";
	
	public static HashMap<String,PhysicalUnit> units;
	
	public static void debug(String message, int depth) {
		System.out.println(new String(new char[4*depth]).replace("\0", " ") + message);
	}

	public static void initUnits() {
		units = new HashMap<String, PhysicalUnit>();
		units.put("m", new PhysicalUnit(new double[]{1.0,0.0,0.0}));
		units.put("kg",new PhysicalUnit(new double[]{0.0,1.0,0.0}));
		units.put("s", new PhysicalUnit(new double[]{0.0,0.0,1.0}));
		units.put("N", new PhysicalUnit(new double[]{1.0,1.0,-2.0}));
		units.put("J", new PhysicalUnit(new double[]{2.0,1.0,-2.0}));
	}
}
