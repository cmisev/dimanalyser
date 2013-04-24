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
 * Error thrown by {@link com.dimanalyser.variablemanager.VariableManager VariableManager} if it attempts to create
 * an already existing scope.
 * @author Cyril Misev <c.misev@gmail.com>
 *
 */
public class ScopeExistsError extends InterpretationError {

	private static final long serialVersionUID = 1156667799329253719L;

	public ScopeExistsError(String name) {
		super(String.format("Scope %s already exists in current scope",name));
	}
}
