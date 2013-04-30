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

import java.util.ArrayList;
import java.util.List;

import com.dimanalyser.errors.InstanceNotFoundError;
import com.dimanalyser.errors.UnitAlreadySetError;

/**
 * Instance class holding a function-type object, behaving itself like a {@link VariableInstance VariableInstance}
 * but depending on several other {@link VariableInstance VariableInstances}, like parameters
 * 
 * @author Cyril Misev <c.misev@gmail.com>
 *
 */
public class FunctionInstance extends Instance {

	/**
	 * Unit of the object itself
	 */
	PhysicalUnit mReturnUnit;
	
	/**
	 * List of VariableInstances holding the instances the object is depending on
	 */
	List<VariableInstance> mParameters;
	
	/**
	 * Main constructor, creating a FunctionInstance with a known unit
	 * 
	 * @param name Name of the function instance
	 * @param accessLevel Access level of the function instance (private/public/protected, see {@link InheritanceLevel InheritanceLevel})
	 * @param returnUnit the unit of the object itself (typically the unit of the return value)
	 */
	public FunctionInstance(String name, int accessLevel, PhysicalUnit returnUnit) {
		super(name, accessLevel);
		mReturnUnit = returnUnit;
		mParameters = new ArrayList<VariableInstance>();
	}

	/**
	 * Constructor for cases where the instance has not yet a known unit
	 * 
	 * @param name Name of the function instance
	 * @param accessLevel Access level of the function instance (private/public/protected, see {@link InheritanceLevel InheritanceLevel})
	 */
	public FunctionInstance(String name, int accessLevel) {
		this(name,accessLevel,null);
	}

	/**
	 * <pre>toString()</pre> implementation for identification/debugging purposes
	 */
	public String toString() {
		// TODO Better function instance toString implementation
		if (mReturnUnit!=null) {			
			return String.format("function instance %s [%s]", mName, mReturnUnit.toString());
		} else {
			return String.format("function instance %s, return unit not yet determined", getName());
		}
	}

	// TODO identify parameter variable instance with local variable in function body
	
	/**
	 * Add a parameter with a known physical unit and a known name
	 * @param name the name of the parameter
	 * @param unit the unit of the parameter
	 */
	public void addParameter(String name, PhysicalUnit unit) {
		mParameters.add(new VariableInstance(name, InheritanceLevel.SCOPE_PROTECTED, unit));
	}
	

	/**
	 * Add a parameter with a known name
	 * @param name the name of the parameter
	 */
	public void addParameter(String name) {
		addParameter(name,null);
	}
	
	/**
	 * Add a parameter where only the unit is known (typically at the call of the object when it is not yet declared)
	 * @param unit the unit of the parameter
	 */
	public void addParameter(PhysicalUnit unit) {
		addParameter("",unit);
	}
	
	/**
	 * Set the name of the parameter when it is finally known 
	 * 
	 * @param i  the index of the parameter
	 * @param name the name to be set
	 */
	public void setParameterName(int i, String name) {
		if (i<mParameters.size()) {
			VariableInstance vi = mParameters.get(i);
			mParameters.remove(i);
			mParameters.add(i,new VariableInstance(name,InheritanceLevel.SCOPE_PROTECTED,vi.getUnit()));
		} else {
			addParameter(name);
		}
	}
	
	/**
	 * Determine whether a parameter of a given name is present
	 * 
	 * @param name the name of the parameter to check
	 * @return <pre>true</pre> if the parameter is present
	 */
	public boolean hasParameter(String name) {	
		for (VariableInstance instance : mParameters) {
			if (instance.getName().equals(name)) {
				return true;
			}
		}
		return false;
	}
	
	/**
	 * Get a parameter of a given name
	 * 
	 * @param name the name of the parameter
	 * @return the corresponding variable instance representing the parameter
	 * @throws InstanceNotFoundError
	 */
	public VariableInstance getParameter(String name) throws InstanceNotFoundError {
		
		for (VariableInstance instance : mParameters) {
			if (instance.getName().equals(name)) {
				return instance;
			}
		}
		throw new InstanceNotFoundError(name);
	}
	
	/**
	 * Get a parameter by its index
	 * 
	 * @param i the index of the parameter to get
	 * @return the corresponding variable instance representing the parameter
	 */
	public VariableInstance getParameter(int i) {
		return mParameters.get(i);
	}
	

	/**
	 * Set the unit of the object (typically the return value)
	 * 
	 */
	@Override
	public void setUnit(PhysicalUnit unit) throws UnitAlreadySetError {
		if (mReturnUnit==null) {
			mReturnUnit = unit;
		} else {
			throw new UnitAlreadySetError(mName);
		}
	}

}
