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
 * Class representing objects in mathematical expression that are not numerical constants (derived types, functions, variables).
 * May or may not resolve to a given physical unit.
 * 
 * @author Cyril Misev <c.misev@gmail.com>
 *
 */
abstract public class Instance extends UnitsMatchable {

	/**
	 * The name of the object
	 */
	protected String mName;
	
	/**
	 * Access level of the object (private/public/protected, see {@link InheritanceLevel InheritanceLevel})
	 */
	protected int mAccessLevel;
	
	/**
	 * Constructor, give the object the two main attributes that are common to all instances, a name and an access level.
	 * 
	 * @param name Name of the instance
	 * @param accessLevel Access level of the instance (private/public/protected, see {@link InheritanceLevel InheritanceLevel})
	 */
	public Instance(String name, int accessLevel) {
		super();
		mName = name;
		mAccessLevel = accessLevel;
	}
	
	/**
	 * Get the name of the instance
	 * 
	 * @return the name of the instance
	 */
	public String getName() {
		return mName;
	}
		
	/**
	 * Get the access level
	 * 
	 * @return the access level
	 */
	public int getAccessLevel(){
		return mAccessLevel;
	}

	
	/**
	 * Set the physical unit of the instance.
	 * 
	 * @param unit the unit to be set.
	 * @throws UnitAlreadySetError
	 */
	@Override
	public void setUnit(PhysicalUnit unit) throws UnitAlreadySetError {
		if (!unit.equals(Globals.getInstance().getUnitless()))
			throw new UnitAlreadySetError(this);
	}

	/**
	 * Get the physical unit of the instance. By default unitless.
	 * 
	 * @return the physical unit of the instance
	 */
	@Override
	public PhysicalUnit getUnit() {
		return Globals.getInstance().getUnitless();
	}
	
	@Override
	public String getExpression() {
		return toString();
	}


}
