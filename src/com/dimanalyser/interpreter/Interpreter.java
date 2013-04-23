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
		mUnitDeclarationsParser.addBinaryOperatorInHierarchy(new String[]{
				"*","/"
		});
		mUnitDeclarationsParser.addBinaryOperatorInHierarchy(new String[]{
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
