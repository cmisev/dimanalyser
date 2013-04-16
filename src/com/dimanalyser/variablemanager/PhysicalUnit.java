package com.dimanalyser.variablemanager;

import com.dimanalyser.common.Globals;
import com.dimanalyser.errors.ExponentNotScalarError;

public class PhysicalUnit {

	double[] mBaseUnits;
	double mScaling;
	
	public PhysicalUnit(double scaling,double... baseunits) {
		mBaseUnits = new double[Globals.NUM_BASEUNITS];
		
		for(int k=0; k<Globals.NUM_BASEUNITS; k++) {
			mBaseUnits[k] = baseunits[k];
		}
		mScaling = scaling;
	}

	public static PhysicalUnit product(PhysicalUnit lhs, PhysicalUnit rhs) {
		double[] baseUnits = new double[Globals.NUM_BASEUNITS];
		
		for(int k=0; k<Globals.NUM_BASEUNITS; k++) {
			baseUnits[k] = lhs.getBaseUnits()[k] + rhs.getBaseUnits()[k];
		}
		
		return new PhysicalUnit(lhs.getScaling()*rhs.getScaling(), baseUnits);
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
				
		for(int k=0; k<Globals.NUM_BASEUNITS; k++) {
			if (rhs.getBaseUnits()[k]!=0) {
				throw new ExponentNotScalarError();
			}
			baseUnits[k] = lhs.getBaseUnits()[k]*rhs.getScaling();
			
		}
		
		return new PhysicalUnit(Math.pow(lhs.getScaling(),rhs.getScaling()), baseUnits);
	}
	
	
	private double[] getBaseUnits() {
		return mBaseUnits;
	}

	private double getScaling() {
		return mScaling;
	}

	public static PhysicalUnit product(PhysicalUnit lhs,
			double rhs) {
		return new PhysicalUnit(lhs.getScaling()*rhs,lhs.getBaseUnits());
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
}
