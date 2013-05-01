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

import java.util.ArrayList;
import java.util.List;
import java.util.Stack;

import com.dimanalyser.common.Globals;
import com.dimanalyser.errors.ExponentNotScalarError;
import com.dimanalyser.errors.InterpretationError;
import com.dimanalyser.errors.UnbalancedBracesError;
import com.dimanalyser.errors.UnitDeclarationsDontMatchError;
import com.dimanalyser.variablemanager.PhysicalUnit;
import com.dimanalyser.variablemanager.VariableManager;

/**
 * Abstract interpreter class the aim of an interpreter is to interpret the syntax of a specific
 * programming language, to identify mathematical expressions and control sequences.
 * 
 * @author Cyril Misev <c.misev@gmail.com>
 *
 */
public abstract class Interpreter {
	
	/**
	 * The {@link VariableManager VariableManager} holding all objects and scopes identified so far.
	 */
	protected VariableManager mVariableManager;
	
	/**
	 * An {@link ExpressionParser ExpressionParser} used to interpret unit declarations in comments
	 * with a syntax common to all programming language.
	 */
	private ExpressionParser mUnitDeclarationsParser;
	
	/**
	 * Buffer to store the unit declarations on the current line
	 */
	private List<PhysicalUnit> mUnitsBuffer;
	
	/**
	 * Current position in unit buffer
	 */
	private int mUnitIndex;
	
	
	/**
	 * Constructor. Initializes units dictionary and the unit declarations parser.
	 */
	protected Interpreter() {
		Globals.initUnits();
		mVariableManager = new VariableManager();
		mUnitDeclarationsParser = new ExpressionParser();
		mUnitDeclarationsParser.addBraces("(",")");
		mUnitDeclarationsParser.addBinaryOperatorInHierarchy(new String[]{
				"*","/"
		});
		mUnitDeclarationsParser.addBinaryOperatorInHierarchy(new String[]{
				"^"
		});
	}
	
	/**
	 * The core method of the interpreter. Called by the main program loop to interpret one or more lines (depending on whether the
	 * statement is a one-line statement or extended to several lines, as decided by the concrete method itself).
	 * 
	 * @param linenumber the index in <pre>lines</pre> of the line to be interpreted.
	 * @param lines all lines of the current file.
	 * @return the next line index in <pre>lines</pre> to be read.
	 * @throws InterpretationError
	 */
	public abstract int interpretStatements(int linenumber, List<String> lines) throws InterpretationError;
	
	/**
	 * Gathers and interprets all <pre>U(...)</pre> unit declarations in a comment.
	 * 
	 * @param string the comment to be interpreted
	 * @throws UnbalancedBracesError
	 * @throws ExponentNotScalarError
	 */
	protected void parseUnitDeclarationsFromComment(String string) throws UnbalancedBracesError, ExponentNotScalarError {
		mUnitsBuffer = new ArrayList<PhysicalUnit>();
		
		int start = 0;
		int end = 0;
		
		while((start=mUnitDeclarationsParser.getInterpretedIndex(string, "U(", end))<string.length()) {
			end = mUnitDeclarationsParser.getInterpretedIndex(string, ")",start+2);
			mUnitsBuffer.add(parseUnitDeclaration(string.substring(start+2,end).replace(" ","")));
			end = end + 1;
		}
		mUnitIndex = 0;
	}
	
	/**
	 * Gets the next unit declaration from the unit declarations buffer
	 * 
	 * @return the next unit declaration
	 * @throws UnitDeclarationsDontMatchError 
	 */
	protected PhysicalUnit getUnitFromBuffer() throws UnitDeclarationsDontMatchError {
		switch (mUnitsBuffer.size()) {
		case 0:
			return null;
		case 1:
			return mUnitsBuffer.get(0);
		default:
			try {
				return mUnitsBuffer.get(mUnitIndex++);
			} catch (Exception e) {
				
				throw new UnitDeclarationsDontMatchError();
			}
		}
	}
	
	/**
	 * Like {@link Interpreter#getUnitFromBuffer() getUnitFromBuffer()}, additionally checks if the number of expected units match
	 * 
	 * @return the next unit declaration
	 * @throws UnitDeclarationsDontMatchError 
	 */
	protected PhysicalUnit getUnitFromBuffer(int size) throws UnitDeclarationsDontMatchError {
		if (mUnitsBuffer.size()==0 || mUnitsBuffer.size()==1 || mUnitsBuffer.size()==size) {
			return getUnitFromBuffer();
		} else {
			throw new UnitDeclarationsDontMatchError();
		}
	}
	
	/**
	 * Private method to parse a single <pre>U(...)</pre> content
	 * 
	 * @param string the <pre>U(...)</pre> content
	 * @return the corresponding physical unit
	 * @throws UnbalancedBracesError
	 * @throws ExponentNotScalarError
	 */
	private PhysicalUnit parseUnitDeclaration(String string) throws UnbalancedBracesError, ExponentNotScalarError {
		List<StackElement> expr = mUnitDeclarationsParser.parseExpression(string);
		
		Stack<StackElement> stack = new Stack<StackElement>();
		for(StackElement s : expr) {
			if (s.equals("*")) {
				StackElement rhs = stack.pop();
				StackElement lhs = stack.pop();
				stack.push(new StackElement(String.format("%s*%s", lhs.getExpression(), rhs.getExpression()), PhysicalUnit.product(lhs.getUnit(), rhs.getUnit())));
			} else if (s.equals("/")) {
				StackElement rhs = stack.pop();
				StackElement lhs = stack.pop();
				stack.push(new StackElement(String.format("%s/%s", lhs.getExpression(), rhs.getExpression()), PhysicalUnit.fraction(lhs.getUnit(), rhs.getUnit())));
			} else if (s.equals("^")) {
				StackElement rhs = stack.pop();
				StackElement lhs = stack.pop();
				stack.push(new StackElement(String.format("%s^%s", lhs.getExpression(), rhs.getExpression()), PhysicalUnit.power(lhs.getUnit(), rhs.getUnit())));
			} else if (s.equals("(")) {
			} else {
				try {
					double f = Float.parseFloat(s.getExpression());
					s.setUnit(PhysicalUnit.getUnitless(f));
				} catch (NumberFormatException nfe) {
					s.setUnit(Globals.units.get(s.getExpression()));
				}
				stack.push(s);
			}
		}
		
		return stack.pop().getUnit();
	}
}
