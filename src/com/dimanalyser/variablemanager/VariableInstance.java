package com.dimanalyser.variablemanager;

import com.dimanalyser.errors.UnitAlreadySetError;

public class VariableInstance extends Instance {

	PhysicalUnit mUnit;
	
	public VariableInstance(String name, int accessLevel, PhysicalUnit unit) {
		super(name, accessLevel);
		mUnit = unit;
	}

	public VariableInstance(String name, int accessLevel) {
		super(name, accessLevel);
		mUnit = null;
	}

	public String toString() {
		if (mUnit!=null) {			
			return String.format("variable instance %s [%s]", mName, mUnit.toString());
		} else {
			return String.format("variable instance %s, unit not yet determined", getName());
		}
	}
	
	@Override
	public void setUnit(PhysicalUnit unit) throws UnitAlreadySetError {
		if (mUnit==null) {
			mUnit = unit;
		} else {
			throw new UnitAlreadySetError(mName);
		}
	}
	
	@Override
	public PhysicalUnit getUnit() {
		return mUnit;
	}
}
