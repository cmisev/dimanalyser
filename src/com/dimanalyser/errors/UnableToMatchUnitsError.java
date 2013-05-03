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

/**
 * Error thrown if the units can't be matched (i.e.) if attempting to set an unit of a mixed expression.
 * @author Cyril Misev <c.misev@gmail.com>
 *
 */
public class UnableToMatchUnitsError extends InterpretationError {





	/**
	 * 
	 */
	private static final long serialVersionUID = 6291268521918092983L;


	public UnableToMatchUnitsError(StackElement lhs, StackElement rhs) {
		super(String.format("Units of expressions %s and %s can't be matched",lhs.getExpression(),rhs.getExpression()));
	}

}
