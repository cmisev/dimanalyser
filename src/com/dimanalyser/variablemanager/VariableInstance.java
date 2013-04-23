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
			if (!mUnit.equals(unit)) {
				throw new UnitAlreadySetError(mName);
			}
		}
	}
	
	@Override
	public PhysicalUnit getUnit() {
		return mUnit;
	}

}
