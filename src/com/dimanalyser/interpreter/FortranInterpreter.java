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

import java.util.List;
import java.util.Stack;


import com.dimanalyser.common.Globals;
import com.dimanalyser.errors.ExponentNotScalarError;
import com.dimanalyser.errors.InstanceExistsError;
import com.dimanalyser.errors.InstanceNotFoundError;
import com.dimanalyser.errors.InterpretationError;
import com.dimanalyser.errors.NotInAnyScopeError;
import com.dimanalyser.errors.ScopeExistsError;
import com.dimanalyser.errors.UnableToMatchUnitsError;
import com.dimanalyser.errors.UnbalancedBracesError;
import com.dimanalyser.errors.UnitAlreadySetError;
import com.dimanalyser.errors.UnitDeclarationsDontMatchError;
import com.dimanalyser.errors.UnitsDontMatchError;
import com.dimanalyser.variablemanager.FunctionInstance;
import com.dimanalyser.variablemanager.InheritanceLevel;
import com.dimanalyser.variablemanager.Instance;
import com.dimanalyser.variablemanager.PhysicalUnit;
import com.dimanalyser.variablemanager.VariableInstance;

public class FortranInterpreter extends Interpreter {

	/**
	 * The expression parser parsing expressions in Fortran-specific syntax.
	 */
	private ExpressionParser mFortranParser;
	
	/**
	 * Subroutines and function may be defined on several instruction lines. 
	 * The function instance is kept in this temporary field until the 
	 * subroutine/function parameters/return types are completely defined
	 */
	// TODO better way of doing it?
	private FunctionInstance mCurrentFunctionInstance;
	
	/**
	 * A list of keywords that can be ignored for unit checking purposes
	 */
	private String[] mIgnoreKeywords;
	
	
	
	/**
	 * Constructor. Initialize the Fortran expression parser.
	 */
	public FortranInterpreter() {
		super();
		
		mCurrentFunctionInstance = null;
		mFortranParser = new ExpressionParser();

		mFortranParser.addBraces("(", ")");
		mFortranParser.addBraces("(/", "/)");
		mFortranParser.addQuotes('"', '\\');
		mFortranParser.addBinaryOperatorInHierarchy(new String[]{
				"="
		});
		mFortranParser.addListSeparatorInHierarchy(new String[]{
				","
		});
		mFortranParser.addBinaryOperatorInHierarchy(new String[]{
				".EQV.",".NEQV."
		});
		mFortranParser.addBinaryOperatorInHierarchy(new String[]{
				".OR."
		});
		mFortranParser.addBinaryOperatorInHierarchy(new String[]{
				".AND."
		});
		mFortranParser.addUnaryOperatorInHierarchy(".NOT.");
		mFortranParser.addBinaryOperatorInHierarchy(new String[]{
				".EQ.",".NE.",".LT.",".GT.",".GE.",".LE.","<",">","<=",">=","==","/="
		});
		mFortranParser.addBinaryOperatorInHierarchy(new String[]{
				"//"
		});
		mFortranParser.addBinaryOperatorInHierarchy(new String[]{
				"+","-"
		});
		mFortranParser.addBinaryOperatorInHierarchy(new String[]{
				"*","/"
		});
		mFortranParser.addBinaryOperatorInHierarchy(new String[]{
				"**"
		});
		mFortranParser.addUnaryOperatorInHierarchy("-");
		
		mIgnoreKeywords = new String[]{
				// F77
				"ASSIGN","BACKSPACE","CLOSE","ENDFILE","FORMAT","GOTO","INQUIRE","OPEN","PAUSE","REWIND","STOP", 
				// F90
				"ALLOCATE","CYCLE","DEALLOCATE","EXIT","NAMELIST","NULLIFY","SEQUENCE",
				// F2003
				"FLUSH","WAIT",
				// F2008
				"ERROR STOP","SYNC","LOCK","UNLOCK"
		};
		
	}

	/**
	 * The core method of the interpreter. Called by the main program loop to interpret one or more lines (depending on whether the
	 * statement is a one-line statement or extended to several lines, as decided by the concrete method itself).
	 * 
	 * @param linenumber the index in <pre>lines</pre> of the line to be interpreted.
	 * @param lines all lines of the current file.
	 * @return the next line index in <pre>lines</pre> to be read.
	 * @throws UnableToMatchUnitsError 
	 * @throws InterpretationError
	 */
	@Override
	public void interpretStatements() 
			throws UnbalancedBracesError, ScopeExistsError, UnitDeclarationsDontMatchError, 
				InstanceExistsError, ExponentNotScalarError, NotInAnyScopeError, UnitAlreadySetError, 
				InstanceNotFoundError, UnitsDontMatchError, UnableToMatchUnitsError {
		
		
		String comment = "";
		String instructions = "&";
		
		
		while(instructions.endsWith("&")) {
			String line = Globals.getInstance().getNextLine();
			int k = mFortranParser.getInterpretedIndex(line,"!");
			
			if (k < line.length()) {
				comment = comment.concat(line.substring(k+1));
				line = line.substring(0,k);
			}
			
			instructions = instructions.substring(0, instructions.length()-1).concat(line.trim());
		}
		
		instructions = instructions.toUpperCase();
		parseUnitDeclarationsFromComment(comment);
		
		interpredInstructions(instructions);
	}
	
	/**
	 * Recursive helper function to interpret instructions embedded to each other, i.e. <pre>IF (...) CALL ...</pre>
	 * 
	 * @param instructions the instructions to interpret
	 * @throws ScopeExistsError 
	 * @throws UnbalancedBracesError 
	 * @throws InstanceNotFoundError 
	 * @throws ExponentNotScalarError 
	 * @throws UnitAlreadySetError 
	 * @throws InstanceExistsError 
	 * @throws UnitDeclarationsDontMatchError 
	 * @throws UnitsDontMatchError 
	 * @throws NotInAnyScopeError 
	 * @throws UnableToMatchUnitsError 
	 */
	private void interpredInstructions(String instructions) 
			throws ScopeExistsError, UnbalancedBracesError, InstanceNotFoundError, ExponentNotScalarError, 
				InstanceExistsError, UnitAlreadySetError, UnitDeclarationsDontMatchError, UnitsDontMatchError, NotInAnyScopeError, UnableToMatchUnitsError {
		
		instructions = instructions.trim();
		
		// TODO Fortran: Implement all possible ways of doing comments
		// TODO Fortran: Implement all possible data types
		
		
		// Fortran keyword, see http://fortranwiki.org/fortran/show/Keywords
		// DONE Fortran 77: Interpretation of ignore keywords: 
		//   	assign, backspace, close, endfile, format, goto, inquire, open, pause, rewind, stop
		// TODO Fortran 77: Interpretation and testing of block data keyword
		// TODO Fortran 77: Interpretation and testing of call keyword
		// TODO Fortran 77: Interpretation and testing of common keyword
		// TODO Fortran 77: Interpretation and testing of continue keyword
		// TODO Fortran 77: Interpretation and testing of data keyword
		// TODO Fortran 77: Interpretation and testing of dimension keyword
		// TODO Fortran 77: Interpretation and testing of do keyword
		// TODO Fortran 77: Interpretation and testing of else keyword
		// TODO Fortran 77: Interpretation and testing of else if keyword
		// TODO Fortran 77: Interpretation and testing of end keyword
		// TODO Fortran 77: Interpretation and testing of endif keyword
		// TODO Fortran 77: Interpretation and testing of entry keyword
		// TODO Fortran 77: Interpretation and testing of equivalence keyword
		// TODO Fortran 77: Interpretation and testing of external keyword
		// TODO Fortran 77: Interpretation and testing of function keyword
		// TODO Fortran 77: Interpretation and testing of if keyword
		// TODO Fortran 77: Interpretation and testing of implicit keyword
		// TODO Fortran 77: Interpretation and testing of inquire keyword
		// TODO Fortran 77: Interpretation and testing of intrinsic keyword
		// TODO Fortran 77: Interpretation and testing of open keyword
		// TODO Fortran 77: Interpretation and testing of parameter keyword
		// TODO Fortran 77: Interpretation and testing of print keyword
		// TODO Fortran 77: Interpretation and testing of program keyword
		// TODO Fortran 77: Interpretation and testing of read keyword
		// TODO Fortran 77: Interpretation and testing of return keyword
		// TODO Fortran 77: Interpretation and testing of rewrite keyword
		// TODO Fortran 77: Interpretation and testing of save keyword
		// TODO Fortran 77: Interpretation and testing of subroutine keyword
		// TODO Fortran 77: Interpretation and testing of then keyword
		// TODO Fortran 77: Interpretation and testing of write keyword

		// DONE Fortran 90: Interpretation of ignore keywords: 
		//   	allocate, cycle, deallocate, exit, namelist, nullify, sequence
		// TODO Fortran 90: Interpretation and testing of allocatable keyword
		// TODO Fortran 90: Interpretation and testing of case keyword
		// TODO Fortran 90: Interpretation and testing of contains keyword
		// TODO Fortran 90: Interpretation and testing of elsewhere keyword
		// TODO Fortran 90: Interpretation and testing of include keyword
		// TODO Fortran 90: Interpretation and testing of interface keyword
		// TODO Fortran 90: Interpretation and testing of intent keyword
		// TODO Fortran 90: Interpretation and testing of module keyword
		// TODO Fortran 90: Interpretation and testing of only keyword
		// TODO Fortran 90: Interpretation and testing of operator keyword
		// TODO Fortran 90: Interpretation and testing of optional keyword
		// TODO Fortran 90: Interpretation and testing of pointer keyword
		// TODO Fortran 90: Interpretation and testing of private keyword
		// TODO Fortran 90: Interpretation and testing of procedure keyword
		// TODO Fortran 90: Interpretation and testing of public keyword
		// TODO Fortran 90: Interpretation and testing of result keyword
		// TODO Fortran 90: Interpretation and testing of recursive keyword
		// TODO Fortran 90: Interpretation and testing of select keyword
		// TODO Fortran 90: Interpretation and testing of target keyword
		// TODO Fortran 90: Interpretation and testing of use keyword
		// TODO Fortran 90: Interpretation and testing of while keyword
		// TODO Fortran 90: Interpretation and testing of where keyword

		// TODO Fortran 95: Interpretation and testing of elemental keyword
		// TODO Fortran 95: Interpretation and testing of forall keyword
		// TODO Fortran 95: Interpretation and testing of pure keyword 
		

		// DONE Fortran 2003: Interpretation of ignore keywords: 
		//   	flush,wait
		// TODO Fortran 2003: Interpretation and testing of abstract keyword
		// TODO Fortran 2003: Interpretation and testing of associate keyword
		// TODO Fortran 2003: Interpretation and testing of asynchronous keyword
		// TODO Fortran 2003: Interpretation and testing of bind keyword
		// TODO Fortran 2003: Interpretation and testing of class keyword
		// TODO Fortran 2003: Interpretation and testing of deferred keyword
		// TODO Fortran 2003: Interpretation and testing of enum keyword
		// TODO Fortran 2003: Interpretation and testing of enumerator keyword
		// TODO Fortran 2003: Interpretation and testing of extends keyword
		// TODO Fortran 2003: Interpretation and testing of final keyword
		// TODO Fortran 2003: Interpretation and testing of generic keyword
		// TODO Fortran 2003: Interpretation and testing of import keyword
		// TODO Fortran 2003: Interpretation and testing of non_overridable keyword
		// TODO Fortran 2003: Interpretation and testing of nopass keyword
		// TODO Fortran 2003: Interpretation and testing of pass keyword
		// TODO Fortran 2003: Interpretation and testing of protected keyword
		// TODO Fortran 2003: Interpretation and testing of value keyword
		// TODO Fortran 2003: Interpretation and testing of volatile keyword
		
		// DONE Fortran 2008: Interpretation of ignore keywords: 
		//   	error stop, sync, lock, unlock
		// TODO Fortran 2008: Interpretation and testing of block keyword
		// TODO Fortran 2008: Interpretation and testing of codimension keyword
		// TODO Fortran 2008: Interpretation and testing of do concurrent keyword
		// TODO Fortran 2008: Interpretation and testing of contiguous keyword
		// TODO Fortran 2008: Interpretation and testing of critical keyword
		// TODO Fortran 2008: Interpretation and testing of submodule keyword
				
		if (instructions.startsWith("PROGRAM ") || instructions.equals("PROGRAM")) {
			mVariableManager.enterScope("PROGRAM", InheritanceLevel.SCOPE_PRIVATE);
		
		} else if (instructions.startsWith("BLOCK ") || instructions.equals("BLOCK")) {
			mVariableManager.enterScope(String.format("BLOCK_%s:%d",Globals.getInstance().getCurrentFilename(),Globals.getInstance().getLineNumber()), InheritanceLevel.SCOPE_PRIVATE);
		} else if (instructions.startsWith("SUBROUTINE ")) {
			List<String> parameterList = mFortranParser.getParametersList(instructions.substring(11));
			
			try {
				mCurrentFunctionInstance = (FunctionInstance) mVariableManager.getInstance(parameterList.get(0).trim());
						
			} catch(InstanceNotFoundError ie) {
				mCurrentFunctionInstance = new FunctionInstance(parameterList.get(0).trim(), InheritanceLevel.SCOPE_PRIVATE, Globals.getInstance().getUnitless());
				mVariableManager.addInstance(mCurrentFunctionInstance);
			}
			for(int i=1; i<parameterList.size(); i++) {
				mCurrentFunctionInstance.setParameterName(i-1,(parameterList.get(i)));
			}
			mVariableManager.enterScope(parameterList.get(0), InheritanceLevel.SCOPE_PRIVATE);
			
		} else if (instructions.startsWith("REAL")) {
			List<String> variableList = 
					mFortranParser.getVariableList(
							instructions.substring(mFortranParser.getInterpretedIndex(instructions, "::")+2),true);
			
			
			for (int i=0; i<variableList.size(); i++) {
				declareInstance(variableList.get(i),InheritanceLevel.SCOPE_PUBLIC,getUnitFromBuffer(variableList.size()));
			}
		} else if (instructions.startsWith("CALL ")) {
			checkUnits(instructions.substring(5));
		} else if (instructions.startsWith("DO ")) {
			mVariableManager.enterScope(String.format("DO_%s:%d",Globals.getInstance().getCurrentFilename(),Globals.getInstance().getLineNumber()), InheritanceLevel.SCOPE_PUBLIC);
			if (instructions.startsWith("DO WHILE ") || instructions.startsWith("DO WHILE(")) {
				interpredInstructions(instructions.substring(8));
			} else {
				interpredInstructions(instructions.substring(3));
			}
		} else if (instructions.startsWith("IF(") || instructions.startsWith("IF ")) {
			if (instructions.endsWith("THEN")) {
				mVariableManager.enterScope(String.format("IF_%s:%d",Globals.getInstance().getCurrentFilename(),Globals.getInstance().getLineNumber()), InheritanceLevel.SCOPE_PUBLIC);
				checkUnits(instructions.substring(2,instructions.length()-4));
			} else {
				int start = mFortranParser.getInterpretedIndex(instructions, "(");
				int end = mFortranParser.getInterpretedIndex(instructions, ")",start+1);
				checkUnits(instructions.substring(start+1,end));
				interpredInstructions(instructions.substring(end+1));
			}
		} else if (instructions.startsWith("ELSE")) {
			mVariableManager.leaveScope();
			if (instructions.equals("ELSE")) {
				mVariableManager.enterScope(String.format("ELSE_%s:%d",Globals.getInstance().getCurrentFilename(),Globals.getInstance().getLineNumber()), InheritanceLevel.SCOPE_PUBLIC);
			} else {
				interpredInstructions(instructions.substring(5));
			}
		} else if (instructions.equals("END") || instructions.startsWith("END ") || instructions.equals("CONTINUE")) {
			mVariableManager.leaveScope();
			mCurrentFunctionInstance = null;
		} else if (instructions.startsWith("CONTAINS")) {
			mCurrentFunctionInstance = null;
		} else if (!(instructions.equals("") || instructionInIgnoreKeyWords(instructions))) {
			if ("0123456789".contains(instructions.substring(0,1))) {
				// TODO Better way of doing it
				int k=1;
				while(k<instructions.length() && "0123456789:".contains(instructions.substring(k,k+1))) {
					k++;
				}
				if (k<instructions.length()) {
					interpredInstructions(instructions.substring(k));
				}
			} else {
				checkUnits(instructions);
			}
		}
	}
	
	/**
	 * Helper method. Check if instruction starts with/is a keyword in the list of instructions that are save to ignore.
	 *  
	 * @param instructions the instruction line
	 * @return <pre>true<pre> if the instruction is save to ignore
	 */
	private boolean instructionInIgnoreKeyWords(String instructions) {
		for (String ikw : mIgnoreKeywords) {
			if (instructions.equals(ikw) || instructions.startsWith(ikw + " ") || instructions.startsWith(ikw + "(")) return true;
		}
		return false;
	}
	

	/**
	 * Helper method. Declare the unit of a certain instance. Distinguish between a parameter of the
	 * subroutine/function currently being defined and a local variable.
	 * 
	 * @param name name of instance to be declared
	 * @param accessLevel access level of instance to be declared
	 * @param unit unit of instance to be declared
	 * @throws InstanceExistsError
	 * @throws UnitAlreadySetError
	 * @throws InstanceNotFoundError
	 */
	private void declareInstance(String name, int accessLevel, PhysicalUnit unit) 
			throws InstanceExistsError, UnitAlreadySetError, InstanceNotFoundError {
		if (mCurrentFunctionInstance!=null && mCurrentFunctionInstance.hasParameter(name)) {
			mCurrentFunctionInstance.getParameter(name).setUnit(unit);
		}
		
		if (mCurrentFunctionInstance!=null && mCurrentFunctionInstance.getName().equals(name)) {
			mCurrentFunctionInstance.setUnit(unit);
		} else {
			mVariableManager.addInstance(new VariableInstance(name, accessLevel, unit));
		}
	}

	/**
	 * Parse an expression and check if the units match. If possible, implicitly set the units of an instance that was
	 * not yet declared.
	 * 
	 * @param expression the expression to check
	 * @throws UnbalancedBracesError
	 * @throws ExponentNotScalarError
	 * @throws InstanceNotFoundError
	 * @throws UnitsDontMatchError
	 * @throws UnitAlreadySetError
	 * @throws InstanceExistsError
	 * @throws UnitDeclarationsDontMatchError 
	 * @throws UnableToMatchUnitsError 
	 */
	private void checkUnits(String expression) throws UnbalancedBracesError, ExponentNotScalarError, InstanceNotFoundError, UnitsDontMatchError, UnitAlreadySetError, InstanceExistsError, UnitDeclarationsDontMatchError, UnableToMatchUnitsError {
		// TODO maybe move to ExpressionParser, handle operations with interfaces?
		List<StackElement> expr = mFortranParser.parseExpression(expression.trim());
		Stack<StackElement> stack = new Stack<StackElement>();
		
		for(int k =0; k<expr.size(); k++) {
			StackElement s = expr.get(k);
			if (s.equals("*")) {
				StackElement rhs = stack.pop();
				StackElement lhs = stack.pop();
				stack.push(new StackElement(String.format("%s*%s", lhs.getExpression(), rhs.getExpression()), PhysicalUnit.product(lhs.getUnit(), rhs.getUnit())));
			} else if (s.equals("/")) {
				StackElement rhs = stack.pop();
				StackElement lhs = stack.pop();
				stack.push(new StackElement(String.format("%s/%s", lhs.getExpression(), rhs.getExpression()), PhysicalUnit.fraction(lhs.getUnit(), rhs.getUnit())));
			} else if (s.equals("**")) {
				StackElement rhs = stack.pop();
				StackElement lhs = stack.pop();
				stack.push(new StackElement(String.format("%s**%s", lhs.getExpression(), rhs.getExpression()), PhysicalUnit.power(lhs.getUnit(), rhs.getUnit())));
			} else if (s.equals("+") || s.equals("-") || 
					s.equals(".LE.") || s.equals(".LT.") || 
					s.equals(".GT.") || s.equals(".GE.") || 
					s.equals(".EQ.") || s.equals("=")) {
				
				if (stack.lastElement().equals(",")) {
					int elementcount = stack.pop().getOperandsCount();
					StackElement reference = stack.pop();
					String listexpression = reference.getExpression();
					
					for (int j=1; j<elementcount; j++) {
						StackElement current = stack.pop();
						setEqualUnits(reference, current, elementcount-j);
						listexpression = current.getExpression() + "," + listexpression;
					}
					stack.push(new StackElement(listexpression,reference.getUnit()));
				}
				
				StackElement rhs = stack.pop();
				StackElement lhs = stack.pop();
				
				setEqualUnits(lhs, rhs, s);
			
				stack.push(new StackElement(String.format("%s%s%s", lhs.getExpression(), s.getExpression(), rhs.getExpression()),lhs.getUnit()));
			} else if (s.equals("(")) {
				stack.lastElement().setExpression((String.format("(%s)", stack.lastElement().getExpression())));
			} else if (s.equals(",")) {
				stack.push(s);
			} else {
				try {
					double f = Float.parseFloat(s.getExpression());
					s.setUnit(PhysicalUnit.getUnitless(f));
					
					PhysicalUnit declaredUnit = getUnitFromBuffer();
					if (declaredUnit!=null) {
						s.setUnit(PhysicalUnit.product(s.getUnit(), declaredUnit));
					}
					
				} catch (NumberFormatException nfe) {
					
					if (stack.size()>0 && stack.lastElement().getExpression().charAt(0)=='(') {
						int parameterCount = 1;
						if (stack.lastElement().equals("(,)")) {
							parameterCount = stack.pop().getOperandsCount();
						} else if (stack.lastElement().equals("()")) {
							parameterCount = 0;
							stack.pop();
						}
						try {
							Instance instance = mVariableManager.getInstance(s.getExpression().trim());
							s.setUnit(instance.getUnit());
							if (instance instanceof FunctionInstance) {
								FunctionInstance fi = (FunctionInstance) instance;
								for (int i=0; i<parameterCount; i++) {
									if (!fi.getParameter(i).getUnit().equals(stack.get(stack.size()-parameterCount+i).getUnit())){
										throw new UnitsDontMatchError(i+1, stack.get(stack.size()-parameterCount+i), fi.getParameter(i).getUnit(), s);
									}
								}
							}
						} catch (InstanceNotFoundError inf) {
							FunctionInstance fi = new FunctionInstance(s.getExpression().trim(), InheritanceLevel.SCOPE_PUBLIC);
							mVariableManager.addInstance(fi);
							for (int i=0; i<parameterCount; i++) {
								fi.addParameter(stack.get(stack.size()-parameterCount+i).getUnit());
							}
						}
						
						
						if(parameterCount==0) {
							s.setExpression(s.getExpression()+"()");
						} else {
							String parameters = stack.pop().getExpression();
							for (int i=1; i<parameterCount; i++) {
								parameters+=","+stack.pop().getExpression();
							}
							s.setExpression(s.getExpression()+"("+parameters+")");
						}
					} else {
						Instance instance = mVariableManager.getInstance(s.getExpression().trim());
						s.setUnit(instance.getUnit());
					}
				}
				stack.push(s);
			}
		}
	}
}
