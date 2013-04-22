package com.dimanalyser.variablemanager;

import com.dimanalyser.common.Globals;
import com.dimanalyser.errors.UnitAlreadySetError;

public class Instance {

	protected String mName;
	int mAccessLevel;
	
	public Instance(String name, int accessLevel) {
		mName = name;
		mAccessLevel = accessLevel;
	}
	
	public String getName() {
		return mName;
	}
	
	public int getAccessLevel(){
		return mAccessLevel;
	}

	public PhysicalUnit getUnit() {
		return Globals.UNIT_UNITLESS;
	}

	public void setUnit(PhysicalUnit lhs) throws UnitAlreadySetError {
		throw new UnitAlreadySetError(mName);
	}

}
