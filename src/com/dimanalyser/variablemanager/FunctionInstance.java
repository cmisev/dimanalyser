package com.dimanalyser.variablemanager;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.dimanalyser.common.Globals;
import com.dimanalyser.errors.InstanceNotFoundError;
import com.dimanalyser.errors.UnitAlreadySetError;

public class FunctionInstance extends Instance {

	PhysicalUnit mReturnUnit;
	List<VariableInstance> mParameters;
	
	public FunctionInstance(String name, int accessLevel, PhysicalUnit returnUnit) {
		super(name, accessLevel);
		mReturnUnit = returnUnit;
		mParameters = new ArrayList<VariableInstance>();
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
		mParameters.add(new VariableInstance(name, InheritanceLevel.SCOPE_PROTECTED, unit));
	}
	
	public void addParameter(String name) {
		addParameter(name,null);
	}
	
	public VariableInstance getParameter(String name) throws InstanceNotFoundError {
		
		for (VariableInstance instance : mParameters) {
			if (instance.getName().equals(name)) {
				return instance;
			}
		}
		throw new InstanceNotFoundError(name);
	}
	
	public boolean hasParameter(String name) {	
		for (VariableInstance instance : mParameters) {
			if (instance.getName().equals(name)) {
				return true;
			}
		}
		return false;
	}
	
	
	@Override
	public void setUnit(PhysicalUnit unit) throws UnitAlreadySetError {
		if (mReturnUnit==null) {
			mReturnUnit = unit;
		} else {
			throw new UnitAlreadySetError(mName);
		}
	}

	public void addParameter(PhysicalUnit unit) {
		addParameter("",unit);
	}
	
	public void setParameterName(int i, String name) {
		if (i<mParameters.size()) {
			VariableInstance vi = mParameters.get(i);
			mParameters.remove(i);
			mParameters.add(i,new VariableInstance(name,InheritanceLevel.SCOPE_PROTECTED,vi.getUnit()));
		} else {
			addParameter(name);
		}
	}

	public Instance getParameter(int i) {
		return mParameters.get(i);
	}
}
