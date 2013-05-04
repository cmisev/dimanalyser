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


package com.dimanalyser.variablemanager;

import com.dimanalyser.common.Globals;
import com.dimanalyser.errors.ExponentNotScalarError;

/**
 * An object holding all information needed to identify a numerical value with a physical unit.
 * 
 * @author Cyril Misev <c.misev@gmail.com>
 *
 */
public class PhysicalUnit {

	/**
	 * Array of base units exponents
	 */
	double[] mBaseUnits;
	
	/**
	 * The scaling of the unit. e.g. in a mks base unit system, a gram differs from a kg 
	 * in the sense that it holds a scaling of 1e-3 where as kg holds a scaling of 1.
	 */
	double mScaling;
	
	/**
	 * The value-part of the numerical value, especially used in exponents.
	 */
	double mValue;
	
	/**
	 * Main constructor
	 * 
	 * @param value The value-part of the numerical value
	 * @param scaling The scaling of the unit
	 * @param baseunits Array of base units exponents
	 */
	public PhysicalUnit(double value,double scaling,double[] baseunits) {
		mBaseUnits = new double[Globals.getInstance().getBaseUnitsCount()];
		
		for(int k=0; k<Globals.getInstance().getBaseUnitsCount(); k++) {
			mBaseUnits[k] = baseunits[k];
		}
		mScaling = scaling;
		mValue = value;
	}
	
	/**
	 * Construct a physical unit where the value is 1
	 * 
	 * @param scaling the scaling of the unit
	 * @param basunits the array of base units exponents
	 */
	public PhysicalUnit(double scaling, double[] basunits) {
		this(1.0,scaling,basunits);
	}
	
	/**
	 * Construct a physical unit where both the value and the scaling are 1
	 * 
	 * @param basunits the array of base units exponents
	 */
	public PhysicalUnit( double[] basunits) {
		this(1.0,1.0,basunits);
	}


	/**
	 * Calculate the product of two numerical values holding a physical unit.
	 * 
	 * @param lhs the left-hand side operand
	 * @param rhs the right-hand side operand
	 * @return the product
	 */
	public static PhysicalUnit product(PhysicalUnit lhs, PhysicalUnit rhs) {
		double[] baseUnits = new double[Globals.getInstance().getBaseUnitsCount()];
		
		
		for(int k=0; k<Globals.getInstance().getBaseUnitsCount(); k++) {
			baseUnits[k] = lhs.getBaseUnits()[k] + rhs.getBaseUnits()[k];
		}
		return new PhysicalUnit(lhs.getValue()*rhs.getValue(),lhs.getScaling()*rhs.getScaling(), baseUnits);
	}

	/**
	 * Calculate the product of a numerical value holding a physical unit and a scalar.
	 * 
	 * @param lhs the left-hand side operand
	 * @param rhs the right-hand side operand
	 * @return the product
	 */
	public static PhysicalUnit product(PhysicalUnit lhs,
			double rhs) {
		
		return new PhysicalUnit(rhs*lhs.getValue(),lhs.getScaling(),lhs.getBaseUnits());
	}

	/**
	 * Calculate the fraction of two numerical values holding a physical unit.
	 * 
	 * @param lhs the left-hand side operand
	 * @param rhs the right-hand side operand
	 * @return the fraction
	 */
	public static PhysicalUnit fraction(PhysicalUnit lhs, PhysicalUnit rhs) {
		double[] baseUnits = new double[Globals.getInstance().getBaseUnitsCount()];
		
		for(int k=0; k<Globals.getInstance().getBaseUnitsCount(); k++) {
			baseUnits[k] = lhs.getBaseUnits()[k] - rhs.getBaseUnits()[k];
		}
		
		return new PhysicalUnit(lhs.getValue()/rhs.getValue(),lhs.getScaling()/rhs.getScaling(), baseUnits);
	}
	
	/**
	 * Calculate the power of a numerical value holding an unit to a physical unit which must be pure scalar.
	 * 
	 * @param lhs the base
	 * @param rhs the exponent
	 * @return the power
	 */
	public static PhysicalUnit power(PhysicalUnit lhs, PhysicalUnit rhs) throws ExponentNotScalarError {
		double[] baseUnits = new double[Globals.getInstance().getBaseUnitsCount()];
		
		
		if (rhs.getScaling()!=1.0) throw new ExponentNotScalarError();
		
		for(int k=0; k<Globals.getInstance().getBaseUnitsCount(); k++) {
			if (rhs.getBaseUnits()[k]!=0) {
				throw new ExponentNotScalarError();
			}
			baseUnits[k] = lhs.getBaseUnits()[k]*rhs.getValue();
			
		}
		
		return new PhysicalUnit(Math.pow(lhs.getValue(),rhs.getValue()),Math.pow(lhs.getScaling(),rhs.getValue()), baseUnits);
	}
	
	/**
	 * <pre>toString()</pre> implementation for identification/debugging/display purposes
	 */
	public String toString() {
		String retval="";
		if (mScaling!=1.0) {
			retval = retval + String.format("%g", mScaling);
		}
		
		if (mBaseUnits[0]==1.0) {
			retval = retval + " m";
		} else if (mBaseUnits[0]!=0.0) {
			retval = retval + String.format(" m^%.0g", mBaseUnits[0]);
		}
		
		if (mBaseUnits[1]==1.0) {
			retval = retval + " kg";
		} else if (mBaseUnits[1]!=0.0) {
			retval = retval + String.format(" kg^%.0g", mBaseUnits[1]);
		}
		
		if (mBaseUnits[2]==1.0) {
			retval = retval + " s";
		} else if (mBaseUnits[2]!=0.0) {
			retval = retval + String.format(" s^%.0g", mBaseUnits[2]);
		}
		if (retval.trim().equals("")) {
			return "1";
		}
		return retval.trim();
	}
	
	/**
	 * <pre>equals()</pre> implementation for identification purposes
	 * 
	 * @param other the physical unit to compare the current physical unit to
	 * @return <pre>true</pre> if the numerical values hold the same physical unit (ignoring the value itself) 
	 */
	public boolean equals(PhysicalUnit other) {
		
		if (other.getScaling()!=mScaling) return false;
		for(int k=0; k<Globals.getInstance().getBaseUnitsCount(); k++) {
			if (mBaseUnits[k]!=other.getBaseUnits()[k]) return false;
		}
		return true;
	}
	
	/**
	 * Get a hash code for usage in a HashMap object
	 */
	public int hashCode() {
		int retval = 0;
		for(int k=0; k<Globals.getInstance().getBaseUnitsCount(); k++) {
			retval += retval*7+((int) mBaseUnits[k])+3;
		}
		return retval;
	}

	/**
	 * Get a purely scalar numerical value as a physical unit object
	 * 
	 * @param f the scalar
	 * @return the physical unit object
	 */
	public static PhysicalUnit getUnitless(double f) {
		return product(Globals.getInstance().getUnitless(), f);
	}
	
	/**
	 * Get the value
	 * 
	 * @return the value
	 */
	public double getValue() {
		return mValue;
	}

	/**
	 * Get the base units exponents
	 * 
	 * @return the base units exponents
	 */
	private double[] getBaseUnits() {
		return mBaseUnits;
	}
	
	/**
	 * Get the base units scaling
	 * 
	 * @return the base units scaling
	 */
	private double getScaling() {
		return mScaling;
	}
}
