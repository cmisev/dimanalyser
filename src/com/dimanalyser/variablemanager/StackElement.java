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
 * Expression stack element. Contains a chunk of an expression, either raw or already interpreted,
 * having a phyiscal unit assigned to it.
 * 
 * @author Cyril Misev <c.misev@gmail.com>
 *
 */
public class StackElement extends UnitsMatchable {

	/**
	 * Human-readable string of the expression
	 */
	private String mExpression;
	
	/**
	 * Unit attached to the expression may or may not be defined
	 */
	private PhysicalUnit mUnit;
	
	/**
	 * If 0, the stack element is an operand and should be pushed onto the stack.
	 * If a positive integer, it is an operator or an atom otherwise assembling several
	 * sub-expressions acting on the <pre>mOperandsCount</pre> top-most elements of the stack.
	 */
	private int mOperandsCount;

	
	/**
	 * Callback to call when the unit of the expression is being set. Useful to propagate unit through the whole expression
	 * to implicitly define units of instances.
	 */
	private ISetUnitCallback mUnitSetCallback;
	
	
	/**
	 * General Constructor, allows the setting of a unit definition callback
	 * @param expression the human-readable string of the expression
	 * @param operandsCount the number of operands the stack expression element is acting on (0 if operand)
	 * @param unit resulting unit of the expression
	 */
	public StackElement(String expression, PhysicalUnit unit, int operandsCount, ISetUnitCallback callback) {
		mExpression = expression;
		mUnit = unit;
		mOperandsCount = operandsCount;
		mUnitSetCallback = callback;
		
		if (unit!=null) {
			mUnit = unit;
			mUnitDefinedAtLineNumber = Globals.getInstance().getLineNumber();
			mUnitDefinedInFileName = Globals.getInstance().getCurrentFilename();
		}
	}
	
	/**
	 * General Constructor
	 * @param expression the human-readable string of the expression
	 * @param operandsCount the number of operands the stack expression element is acting on (0 if operand)
	 * @param unit resulting unit of the expression
	 */
	public StackElement(String expression, PhysicalUnit unit, int operandsCount) {
		this(expression,unit,operandsCount,null);
	}
	
	/**
	 * Constructor initializing the stack element without the unit being defined
	 * @param expression the human-readable string of the expression
	 * @param operandsCount the number of operands the stack expression element is acting on (0 if operand)
	 */
	public StackElement(String expression, int operandsCount) {
		this(expression,null,operandsCount);
	}
	
	/**
	 * Constructor initializing the stack element without the unit being defined
	 * @param expression the human-readable string of the expression
	 * @param callback the callback to call upon setting the unit
	 */
	public StackElement(String expression, ISetUnitCallback callback) {
		this(expression,null,2,callback);
	}

	
	/**
	 * Constructor initializing the stack element as an operand without the unit being defined
	 * @param expression the human-readable string of the expression
	 */
	public StackElement(String expression) {
		this(expression,0);
	}

	/**
	 * Constructor initializing the stack element as an operand
	 * @param expression the human-readable string of the expression
	 * @param unit the unit of the operand
	 */
	public StackElement(String expression, PhysicalUnit unit) {
		this(expression,unit,0);
	}

	/**
	 * Set the human-readable string of the expression
	 * @param expression the new human-readable string of the expression
	 */
	public void setExpression(String expression) {
		mExpression = expression;
	}
	
	/**
	 * Set the physical unit of the instance and keep track of the origin of the implicit unit definition.
	 * 
	 * @param unit the unit to be set.
	 * @param origin the instance leading to the implicit unit definition, may be null
	 * @throws UnitAlreadySetError
	 * @throws UnableToMatchUnitsError 
	 */
	@Override
	public void setUnit(PhysicalUnit unit, UnitsMatchable origin) throws UnableToMatchUnitsError, UnitAlreadySetError {
		mUnit = unit;
		mUnitDefinedAtLineNumber = Globals.getInstance().getLineNumber();
		mUnitDefinedInFileName = Globals.getInstance().getCurrentFilename();
		mUnitDefinedBy = origin;
		if (mUnitSetCallback!=null) mUnitSetCallback.setUnit(unit,origin);
	}
	
	/**
	 * Set the unit resulting from the expression
	 * @param unit the unit resulting from the expression
	 * @throws UnableToMatchUnitsError 
	 * @throws UnitAlreadySetError 
	 */
	@Override
	public void setUnit(PhysicalUnit unit) throws UnitAlreadySetError,
			UnableToMatchUnitsError {
		setUnit(unit, null);
	}
	
	/**
	 * Set the unit callback
	 * @param callback callback function to call if the unit is being set on this stack element
	 * @throws UnableToMatchUnitsError 
	 */
	public void setUnitSetCallback(ISetUnitCallback callback) throws UnableToMatchUnitsError {
		mUnitSetCallback = callback;
	}
	
	/**
	 * Get the human-readable string of the expression
	 * @return the human-readable string of the expression
	 */
	public String getExpression() {
		return mExpression;
	}
	
	/**
	 * Get the unit resulting from the expression
	 * @return the unit resulting from the expression
	 */
	public PhysicalUnit getUnit() {
		return mUnit;
	}
	
	/**
	 * Get the number of operands the chunk is acting on (0 if operand)
	 * @return the number of operands the chunk is acting on (0 if operand)
	 */
	public int getOperandsCount() {
		return mOperandsCount;
	}
	
	/**
	 * <pre>toString()</pre> implementation for purposes of debugging and identification
	 */
	public String toString() {
		if (mUnit==null) {
			return String.format("expression %s, unit not yet defined", mExpression.trim());
		} else {
			return String.format("expression [%s]=%s", mExpression.trim(), mUnit.toString());
		}
			
	}
	
	/**
	 * Test if the human readable-expression matches a string
	 * @param expr the string to be tested
	 * @return <pre>true</pre> if the expressions match
	 */
	public boolean equals(String expr) {
		return mExpression.equals(expr);
	}
	
	/**
	 * Test if a certain unit equals the unit of the expression
	 * @param unit
	 * @return <pre>true</pre> if the units match
	 */
	public boolean equals(PhysicalUnit unit) {
		return mUnit.equals(unit);
	}


}
