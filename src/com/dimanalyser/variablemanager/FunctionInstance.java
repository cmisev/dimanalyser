package com.dimanalyser.variablemanager;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import com.dimanalyser.common.Globals;
import com.dimanalyser.errors.UnitAlreadySetError;

public class FunctionInstance extends Instance {

	PhysicalUnit mReturnUnit;
	HashMap<String, VariableInstance> mParameters;
	
	public FunctionInstance(String name, int accessLevel, PhysicalUnit returnUnit) {
		super(name, accessLevel);
		mReturnUnit = returnUnit;
		mParameters = new HashMap<String,VariableInstance>();
	}

	public FunctionInstance(String name, int accessLevel) {
		this(name,accessLevel,null);
	}

	public String toString() {
		if (mReturnUnit!=null) {			
			return String.format("function instance %s [%s]", mName, mReturnUnit.toString());
		} else {
			return String.format("function instance %s, return unit not yet determined", getName());
		}
	}
	
	public void addParameter(String name, PhysicalUnit unit) {
		mParameters.put(name,new VariableInstance(name, InheritanceLevel.SCOPE_PROTECTED, unit));
	}
	
	public void addParameter(String name) {
		addParameter(name,null);
	}
	
	public VariableInstance getParameter(String name) {
		return mParameters.get(name);
	}
	
	public boolean hasParameter(String name) {
		return mParameters.containsKey(name);
	}
	
	
	@Override
	public void setUnit(PhysicalUnit unit) throws UnitAlreadySetError {
		if (mReturnUnit==null) {
			mReturnUnit = unit;
		} else {
			throw new UnitAlreadySetError(mName);
		}
	}
}
