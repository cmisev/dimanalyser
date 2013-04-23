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

public class PhysicalUnit {

	double[] mBaseUnits;
	double mScaling;
	double mValue;
	
	public PhysicalUnit(double value,double scaling,double[] baseunits) {
		mBaseUnits = new double[Globals.NUM_BASEUNITS];
		
		for(int k=0; k<Globals.NUM_BASEUNITS; k++) {
			mBaseUnits[k] = baseunits[k];
		}
		mScaling = scaling;
		mValue = value;
	}
	
	

	public PhysicalUnit(double scaling, double[] basunits) {
		this(1.0,scaling,basunits);
	}
	
	public PhysicalUnit( double[] basunits) {
		this(1.0,1.0,basunits);
	}



	public static PhysicalUnit product(PhysicalUnit lhs, PhysicalUnit rhs) {
		double[] baseUnits = new double[Globals.NUM_BASEUNITS];
		
		for(int k=0; k<Globals.NUM_BASEUNITS; k++) {
			baseUnits[k] = lhs.getBaseUnits()[k] + rhs.getBaseUnits()[k];
		}
		return new PhysicalUnit(lhs.getValue()*rhs.getValue(),lhs.getScaling()*rhs.getScaling(), baseUnits);
	}
	
	public double getValue() {
		return mValue;
	}



	public static PhysicalUnit fraction(PhysicalUnit lhs, PhysicalUnit rhs) {
		double[] baseUnits = new double[Globals.NUM_BASEUNITS];
		
		for(int k=0; k<Globals.NUM_BASEUNITS; k++) {
			baseUnits[k] = lhs.getBaseUnits()[k] - rhs.getBaseUnits()[k];
		}
		
		return new PhysicalUnit(lhs.getScaling()/rhs.getScaling(), baseUnits);
	}
	
	public static PhysicalUnit power(PhysicalUnit lhs, PhysicalUnit rhs) throws ExponentNotScalarError {
		double[] baseUnits = new double[Globals.NUM_BASEUNITS];
		
		if (rhs.getScaling()!=1.0) throw new ExponentNotScalarError();
		
		for(int k=0; k<Globals.NUM_BASEUNITS; k++) {
			if (rhs.getBaseUnits()[k]!=0) {
				throw new ExponentNotScalarError();
			}
			baseUnits[k] = lhs.getBaseUnits()[k]*rhs.getValue();
			
		}
		
		return new PhysicalUnit(Math.pow(lhs.getValue(),rhs.getValue()),Math.pow(lhs.getScaling(),rhs.getValue()), baseUnits);
	}
	
	
	private double[] getBaseUnits() {
		return mBaseUnits;
	}

	private double getScaling() {
		return mScaling;
	}

	public static PhysicalUnit product(PhysicalUnit lhs,
			double rhs) {
		return new PhysicalUnit(rhs,lhs.getScaling(),lhs.getBaseUnits());
	}
	
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
	
	public boolean equals(PhysicalUnit other) {
		
		if (other.getScaling()!=mScaling) return false;
		for(int k=0; k<Globals.NUM_BASEUNITS; k++) {
			if (mBaseUnits[k]!=other.getBaseUnits()[k]) return false;
		}
		return true;
	}
	
	public int hashCode() {
		int retval = 0;
		for(int k=0; k<Globals.NUM_BASEUNITS; k++) {
			retval += retval*7+((int) mBaseUnits[k])+3;
		}
		return retval;
	}

	public static PhysicalUnit getUnitless(double f) {
		return product(Globals.UNIT_UNITLESS, f);
	}
}
