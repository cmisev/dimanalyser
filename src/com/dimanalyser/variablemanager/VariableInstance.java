package com.dimanalyser.variablemanager;

public class VariableInstance extends Instance {

	PhysicalUnit mUnit;
	
	public VariableInstance(String name, int accessLevel, PhysicalUnit unit) {
		super(name, accessLevel);
		mUnit = unit;
	}

	public String toString() {
		if (mUnit!=null) {			
			return String.format("variable instance %s [%s]", mName, mUnit.toString());
		} else {
			return String.format("variable instance %s, unit not yet determined", getName());
		}
	}
}
