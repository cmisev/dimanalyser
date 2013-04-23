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
