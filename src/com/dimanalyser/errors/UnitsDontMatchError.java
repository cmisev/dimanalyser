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

package com.dimanalyser.errors;

import com.dimanalyser.interpreter.StackElement;
import com.dimanalyser.variablemanager.PhysicalUnit;

public class UnitsDontMatchError extends InterpretationError {




	/**
	 * 
	 */
	private static final long serialVersionUID = -8451744034443520259L;


	public UnitsDontMatchError(StackElement lhs, StackElement rhs,
			StackElement s) {
		super(String.format("Units don't match at %s operation. [%s]=%s should be [%s]=%s ",s.getExpression(),lhs.getExpression(),lhs.getUnit().toString(),rhs.getExpression(),rhs.getUnit().toString()));
	}


	public UnitsDontMatchError(int i, StackElement par, PhysicalUnit expected, StackElement s) {
		super(String.format("Parameter Units of parameter %d don't match in function call %s. [%s]=%s should be %s ",i,s.getExpression(),par.getExpression(),par.getUnit().toString(),expected.toString()));
	}
}
