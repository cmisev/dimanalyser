package com.dimanalyser.interpreter;

import java.util.ArrayList;
import java.util.List;
import java.util.Stack;

import com.dimanalyser.common.Globals;
import com.dimanalyser.errors.ExponentNotScalarError;
import com.dimanalyser.errors.InterpretationError;
import com.dimanalyser.errors.UnbalancedBracesError;
import com.dimanalyser.variablemanager.PhysicalUnit;
import com.dimanalyser.variablemanager.VariableManager;

public abstract class Interpreter {
	
	protected VariableManager mVariableManager;
	private ExpressionParser mUnitDeclarationsParser;
	
	protected Interpreter() {
		Globals.initUnits();
		mVariableManager = new VariableManager();
		mUnitDeclarationsParser = new ExpressionParser();
		mUnitDeclarationsParser.addBraces("(",")");
		mUnitDeclarationsParser.addBinaryOperatorHierarchy(new String[]{
				"*","/"
		});
		mUnitDeclarationsParser.addBinaryOperatorHierarchy(new String[]{
				"^"
		});
	}
	
	public abstract int interpretStatements(int linenumber, List<String> lines) throws InterpretationError;
	
	
	public List<PhysicalUnit> parseUnitDeclarationsFromComment(String string) throws UnbalancedBracesError, ExponentNotScalarError {
		List<PhysicalUnit> retval = new ArrayList<PhysicalUnit>();
		
		int start = 0;
		int end = 0;
		
		while((start=mUnitDeclarationsParser.getInterpretedIndex(string, "U(", end))<string.length()) {
			end = mUnitDeclarationsParser.getInterpretedIndex(string, ")",start+2);
			retval.add(parseUnitDeclaration(string.substring(start+2,end).replace(" ","")));
			end = end + 1;
		}
		
		return retval;
	}
	
	private PhysicalUnit parseUnitDeclaration(String string) throws UnbalancedBracesError, ExponentNotScalarError {
		List<String> expr = mUnitDeclarationsParser.parseExpression(string);
		Stack<PhysicalUnit> stack = new Stack<PhysicalUnit>();
		for(String s : expr) {
			if (s.equals("*")) {
				PhysicalUnit rhs = stack.pop();
				PhysicalUnit lhs = stack.pop();
				stack.push(PhysicalUnit.product(lhs, rhs));
			} else if (s.equals("/")) {
				PhysicalUnit rhs = stack.pop();
				PhysicalUnit lhs = stack.pop();
				stack.push(PhysicalUnit.fraction(lhs, rhs));
			} else if (s.equals("^")) {
				PhysicalUnit rhs = stack.pop();
				PhysicalUnit lhs = stack.pop();
				stack.push(PhysicalUnit.power(lhs, rhs));
			} else if (s.equals("(")) {
			} else {
				try {
					double f = Float.parseFloat(s);
					stack.push(PhysicalUnit.product(Globals.UNIT_UNITLESS, f));
				} catch (NumberFormatException nfe) {
					stack.push(Globals.units.get(s));
				}
			}
		}
		
		return stack.pop();
	}
}
