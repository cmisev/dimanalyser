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

import com.dimanalyser.variablemanager.Instance;

/**
 * Error thrown if, while attempting to implicitly set the unit of an {@link com.dimanalyser.variablemanager.Instance Instance}
 * the unit was already set previously. 
 * 
 * @author Cyril Misev <c.misev@gmail.com>
 *
 */
public class UnitAlreadySetError extends InterpretationError {
	
	private static final long serialVersionUID = 8865873067121639178L;

	public UnitAlreadySetError(Instance instance) {
		super(String.format("Unit on  instance \"%s\" was already set%s",instance.getName(),instance.definitionOriginTree(5)));
	}
}
