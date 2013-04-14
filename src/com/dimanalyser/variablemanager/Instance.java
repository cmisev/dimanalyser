package com.dimanalyser.variablemanager;

import com.dimanalyser.common.Globals;

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

}
