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
import com.dimanalyser.errors.UnitAlreadySetError;

/**
 * Instance holding information about a typical one-value variable.
 * 
 * @author Cyril Misev <c.misev@gmail.com>
 *
 */
public class VariableInstance extends Instance {

	/**
	 * The unit of the variable, may be not yet defined, in this case this field is <pre>null</pre>. It may be implicitly defined at
	 * a later stage when during an operation with a known unit the operation expects the units to be equal. 
	 */
	PhysicalUnit mUnit;
	
	/**
	 * Main constructor in case the unit of the variable is known at the creation of the variable instance (explicit unit declaration)
	 * 
	 * @param name the name of the variable instance
	 * @param accessLevel Access level of the instance (private/public/protected, see {@link InheritanceLevel InheritanceLevel})
	 * @param unit the unit of the variable.
	 */
	public VariableInstance(String name, int accessLevel, PhysicalUnit unit) {
		super(name, accessLevel);
		if (unit == null) {
			mUnit = null;
		} else {
			mUnitDefinedAtLineNumber = Globals.getInstance().getLineNumber();
			mUnitDefinedInFileName = Globals.getInstance().getCurrentFilename();
			mUnit = unit;
		}
	}

	/**
	 * Constructor in case the unit is not known yet
	 * 
	 * @param name the name of the variable instance
	 * @param accessLevel Access level of the instance (private/public/protected, see {@link InheritanceLevel InheritanceLevel})
	 */
	public VariableInstance(String name, int accessLevel) {
		this(name, accessLevel, null);
	}

	/**
	 * <pre>toString()</pre> implementation for identification/debugging purposes
	 */
	public String toString() {
		if (mUnit!=null) {			
			return String.format("variable instance %s [%s]", mName, mUnit.toString());
		} else {
			return String.format("variable instance %s, unit not yet determined", getName());
		}
	}
	
	/**
	 * Set the physical unit of the instance.
	 * 
	 * @param unit the unit to be set.
	 * @throws UnitAlreadySetError
	 */
	@Override
	public void setUnit(PhysicalUnit unit) throws UnitAlreadySetError {
		if (mUnit==null) {
			mUnit = unit;
			if (unit!=null) {
				Globals.debug(String.format("Unit of variable instance %s set to %s",mName,mUnit.toString()));
			}
		} else if (unit!=null) {
			if (!mUnit.equals(unit)) {
				throw new UnitAlreadySetError(this);
			}
		}
	}

	
	/**
	 * Get the physical unit of the instance.
	 * 
	 * @return the physical unit of the instance
	 */
	@Override
	public PhysicalUnit getUnit() {
		return mUnit;
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
