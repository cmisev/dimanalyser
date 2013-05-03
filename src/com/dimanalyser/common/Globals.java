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

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import com.dimanalyser.variablemanager.PhysicalUnit;

/**
 * This is a singleton class holding global variables such as the filename, configuration settings
 * and commandline parameters.
 * 
 * @author Cyril Misev <c.misev@gmail.com>
 *
 */
public final class Globals {

	/**
	 * private static variable holding the singleton instance
	 */
	private static Globals mInstance = new Globals();
	
	/**
	 * Number of base units
	 */
	private int mBaseUnitsCount=3;
	
	/**
	 * Basic unitless unit
	 */
	private PhysicalUnit mUnitless;
	

	/**
	 * Name of currently processed file
	 */
	private String mFileName = "testfortran.f90";
	
	/**
	 * Line number of currently processed file
	 */
	private int mLineNumber;
	
	/**
	 * HashMap holding all known units, with their label as HashMap key
	 */
	private HashMap<String,PhysicalUnit> mUnits;

	/**
	 * Array holding all read lines of the file
	 */
	private List<String> mLines;	
	
	/**
	 * debug depth
	 */
	private int mDebugDepth = 0;

	/**
	 * private constructor 
	 */
	private Globals() {
	}
	
	
	/**
	 * Static method printing debug messages where wanted
	 * @param message the message to be printed
	 * @param depth a positive integer allowing for the creation of a hierarchy of the debug messages
	 */
	public static void debug(String message) {
		System.out.println(String.format("%4d ",mInstance.getLineNumber()) + new String(new char[4*mInstance.mDebugDepth]).replace("\0", " ") + message);
	}
	
	/**
	 * Enter a new Debug level
	 * @return the debug depth
	 */
	public static void enterDebugLevel() {
		mInstance.mDebugDepth++;
	}
	
	/**
	 * Leave debug level
	 */
	public static void leaveDebugLevel() {
		mInstance.mDebugDepth--;
		if (mInstance.mDebugDepth==-1) {
			mInstance.mDebugDepth=0;
		}
	}

	/**
	 * Get the singleton instance
	 * 
	 * @return the singleton instance
	 */
	public static Globals getInstance() {
		if (mInstance == null) {
			mInstance = new Globals();
		}
		return mInstance;
	}
	
	/**
	 * Get an unit definition
	 * @param identifier the string identifier of the unit to get
	 * @return the unit
	 */
	public PhysicalUnit getUnit(String identifier) {
		if (mUnits == null) {
			mUnits = new HashMap<String, PhysicalUnit>();
			mUnits.put("m", new PhysicalUnit(new double[]{1.0,0.0,0.0}));
			mUnits.put("kg",new PhysicalUnit(new double[]{0.0,1.0,0.0}));
			mUnits.put("s", new PhysicalUnit(new double[]{0.0,0.0,1.0}));
			mUnits.put("N", new PhysicalUnit(new double[]{1.0,1.0,-2.0}));
			mUnits.put("J", new PhysicalUnit(new double[]{2.0,1.0,-2.0}));
		}
		return mUnits.get(identifier);
	}
	
	/**
	 * Get the unitless unit definition
	 * @return the unitless unit
	 */
	public PhysicalUnit getUnitless() {
		if (mUnitless == null) {
			mUnitless = new PhysicalUnit(new double[]{0.0,0.0,0.0});
		}
		return mUnitless;
	}
	
	/**
	 * Get the number of base units
	 * @return the number of base units
	 */
	public int getBaseUnitsCount() {
		return mBaseUnitsCount;
	}
	
	/**
	 * Get the file name of the file being currently processed
	 * @return the file name
	 */
	public String getCurrentFilename() {
		return mFileName;
	}

	/**
	 * Open a filename and set the line number to the beginning of the file
	 * @param filename the file name to open
	 */
	public void openFile(String filename) {
		mFileName = filename;
		mLineNumber = 0;
		
		mLines = new ArrayList<String>();
		File file = new File(mFileName);
		String line;
		try {
			BufferedReader reader = new BufferedReader(new FileReader(file));
			while((line = reader.readLine())!=null) {
				mLines.add(line);
			}
			reader.close();
		} catch (FileNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}


	/**
	 * Determine whether the file is fully read or not
	 */
	public boolean fileRead() {
		return (mLineNumber==mLines.size());
	}
	
	/**
	 * Get the current line of the file
	 * @return the current line of the file
	 */
	public String getNextLine() {
		return mLines.get(mLineNumber++);
	}
	
	/**
	 * Get the current line number
	 * @return the current line number
	 */
	public int getLineNumber() {
		return mLineNumber;
	}
	
	
}
