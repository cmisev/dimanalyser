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
 * Error thrown if the number of unit declarations in the comment doesn't match the number of instances where the unit may be declared. 
 * @author Cyril Misev <c.misev@gmail.com>
 *
 */
public class UnitDeclarationsDontMatchError extends InterpretationError {



	private static final long serialVersionUID = 6270446242125793870L;

	public UnitDeclarationsDontMatchError() {
		super("Number of Unit declarations don't match");
	}
}
