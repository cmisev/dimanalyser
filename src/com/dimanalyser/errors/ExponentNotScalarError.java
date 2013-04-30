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

/**
 * Error thrown in case a exponentiation with a non-scalar exponent occurred (e.g. 1 to the power of kg)
 * 
 * @author Cyril Misev <c.misev@gmail.com>
 *
 */
public class ExponentNotScalarError extends InterpretationError {


	private static final long serialVersionUID = 7073000237145281096L;

	public ExponentNotScalarError() {
		super("Exponents can't have a physical unit");
	}
}
