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
import com.dimanalyser.errors.UnableToMatchUnitsError;
import com.dimanalyser.errors.UnitAlreadySetError;

/**
 * @author Cyril Misev <c.misev@gmail.com>
 *
 */
abstract public class UnitsMatchable {

	/**
	 * The line number where the unit of the instance was determined. 0 if not yet determined
	 */
	protected int mUnitDefinedAtLineNumber=0;
	
	/**
	 * The file name where the unit of the instance was determined.
	 */
	protected String mUnitDefinedInFileName = "";
	
	/**
	 * The instance which lead to the implicit definition of the unit of this instance, if applicable
	 */
	protected UnitsMatchable mUnitDefinedBy = null;
	
	
	/**
	 * 
	 */
	public UnitsMatchable() {
		// TODO Auto-generated constructor stub
	}
	
	/**
	 * Set the physical unit of the instance.
	 * 
	 * @param unit the unit to be set.
	 * @throws UnitAlreadySetError
	 * @throws UnableToMatchUnitsError 
	 */
	public abstract void setUnit(PhysicalUnit unit) throws UnitAlreadySetError, UnableToMatchUnitsError;

	/**
	 * Set the physical unit of the instance and keep track of the origin of the implicit unit definition.
	 * 
	 * @param unit the unit to be set.
	 * @param origin the instance leading to the implicit unit definition, may be null
	 * @throws UnitAlreadySetError
	 * @throws UnableToMatchUnitsError 
	 */
	public void setUnit(PhysicalUnit unit, UnitsMatchable origin) throws UnitAlreadySetError, UnableToMatchUnitsError {
		setUnit(unit);
		mUnitDefinedAtLineNumber = Globals.getInstance().getLineNumber();
		mUnitDefinedInFileName = Globals.getInstance().getCurrentFilename();
		mUnitDefinedBy = origin;
	}
	
	public String definitionOriginTree(int depth) {
		if (depth>0) {
			if (mUnitDefinedBy != null) {
				return String.format("\n    @ %s: %d by %s%s", mUnitDefinedInFileName, mUnitDefinedAtLineNumber, mUnitDefinedBy.getExpression(), mUnitDefinedBy.definitionOriginTree(depth-1));
			} else {
				return String.format("\n    @ %s: %d", mUnitDefinedInFileName, mUnitDefinedAtLineNumber);
			}
		}
		return "";
	}

	/**
	 * Get the Expression for display purposes 
	 * 
	 * @return the expression
	 */
	abstract public String getExpression();

	/**
	 * Get the unit of the expression
	 * 
	 * @return the physical unit of the expression
	 */
	abstract public PhysicalUnit getUnit();
	
	/**
	 * Get the expression and the unit for display purposes
	 */
	abstract public String toString();
}
