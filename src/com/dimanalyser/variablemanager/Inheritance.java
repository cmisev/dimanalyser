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

/**
 * class holding information of the nature of an inheritance.
 * 
 * @author Cyril Misev <c.misev@gmail.com>
 *
 */
public class Inheritance {

	/**
	 * The level of inheritance (private/public/protected, see {@link InheritanceLevel InheritanceLevel})
	 */
	private int mInheritanceLevel;
	
	/**
	 * The scope inherited by this inheritance
	 */
	private Scope mScope;
	
	/**
	 * Constructor
	 * 
	 * @param scope The scope inherited by the inheritance
	 * @param inheritanceLevel The level of inheritance (private/public/protected, see {@link InheritanceLevel InheritanceLevel})
	 */
	public Inheritance(Scope scope, int inheritanceLevel) {
		mScope = scope;
		mInheritanceLevel = inheritanceLevel;
	}
	
	/**
	 * Getter for the scope inherited by this inheritance
	 * 
	 * @return the scope inherited by this inheritance
	 */
	public Scope getScope() {
		return mScope;
	}
	
	
	/**
	 * Getter for the inheritance level
	 * 
	 * @return the inheritance level
	 */
	public int getInheritanceLevel() {
		return mInheritanceLevel;
	}
}
