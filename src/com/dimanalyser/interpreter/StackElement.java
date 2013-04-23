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

package com.dimanalyser.interpreter;

import com.dimanalyser.variablemanager.PhysicalUnit;

public class StackElement {

	private String mExpression;
	private PhysicalUnit mUnit;
	private int mOperandsCount;
	
	
	public StackElement(String expression, PhysicalUnit unit, int operandsCount) {
		mExpression = expression;
		mUnit = unit;
		mOperandsCount = operandsCount;
	}
	
	public StackElement(String expression, int operandsCount) {
		this(expression,null,operandsCount);
	}
	
	public StackElement(String expression) {
		this(expression,0);
	}

	public StackElement(String expression, PhysicalUnit unit) {
		this(expression,unit,0);
	}

	public void setExpression(String expression) {
		mExpression = expression;
	}
	
	public void setUnit(PhysicalUnit unit) {
		mUnit = unit;
	}
	
	public String getExpression() {
		return mExpression;
	}
	
	public PhysicalUnit getUnit() {
		return mUnit;
	}
	
	public int getOperandsCount() {
		return mOperandsCount;
	}
	
	public String toString() {
		return mExpression;
	}
	
	public boolean equals(String expr) {
		return mExpression.equals(expr);
	}
	
	public boolean equals(PhysicalUnit unit) {
		return mUnit.equals(unit);
	}

}
