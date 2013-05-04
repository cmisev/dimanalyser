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

/**
 * Instance holding information about a typical one-value variable.
 * 
 * @author Cyril Misev <c.misev@gmail.com>
 *
 */
public class ParameterInstance extends VariableInstance {

	/**
	 * The number of the parameter
	 */
	private int mParameterNumber;
	
	/**
	 * Main constructor in case the unit of the variable is known at the creation of the variable instance (explicit unit declaration)
	 * 
	 * @param name the name of the variable instance
	 * @param accessLevel Access level of the instance (private/public/protected, see {@link InheritanceLevel InheritanceLevel})
	 * @param unit the unit of the variable.
	 * @param parameterNumber the number of the parameter
	 */
	public ParameterInstance(String name, int accessLevel, PhysicalUnit unit, int parameterNumber) {
		super(name, accessLevel,unit);
		mParameterNumber = parameterNumber;
	}

	/**
	 * Constructor in case the unit is not known yet
	 * 
	 * @param name the name of the variable instance
	 * @param accessLevel Access level of the instance (private/public/protected, see {@link InheritanceLevel InheritanceLevel})
	 * @param parameterNumber the number of the parameter
	 */
	public ParameterInstance(String name, int accessLevel, int parameterNumber) {
		super(name, accessLevel);
		mParameterNumber = parameterNumber;
	}

	/**
	 * <pre>toString()</pre> implementation for identification/debugging purposes
	 */
	@Override
	public String toString() {
		if (mUnit!=null) {			
			return String.format("parameter %d [%s]=%s", mParameterNumber, mName, mUnit.toString());
		} else {
			return String.format("parameter %d (%s), unit not yet determined", mParameterNumber, getName());
		}
	}
	
	/**
	 * Set the name of the variable instance.
	 * 
	 * @param name the name to set
	 */
	public void setName(String name) {
		mName = name;
	}

}
