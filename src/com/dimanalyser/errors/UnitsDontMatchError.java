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

import com.dimanalyser.variablemanager.UnitsMatchable;

/**
 * Error thrown if the units don't match.
 * @author Cyril Misev <c.misev@gmail.com>
 *
 */
public class UnitsDontMatchError extends InterpretationError {




	private static final long serialVersionUID = -8451744034443520259L;


	public UnitsDontMatchError(UnitsMatchable lhs, UnitsMatchable rhs,
			String s) {
		super(String.format("Units don't match at %s operation. %s should be %s\n  %s Definition trace:%s\n\n  %s Definition trace:%s\n",s,lhs.toString(),rhs.toString(),lhs.getExpression().trim(),lhs.definitionOriginTree(20),rhs.getExpression().trim(),rhs.definitionOriginTree(20)));
	}

}
