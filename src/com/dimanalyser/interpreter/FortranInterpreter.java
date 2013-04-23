package com.dimanalyser.interpreter;

import java.util.List;
import java.util.Stack;

import sun.reflect.generics.reflectiveObjects.NotImplementedException;

import com.dimanalyser.common.Globals;
import com.dimanalyser.errors.ExponentNotScalarError;
import com.dimanalyser.errors.InstanceExistsError;
import com.dimanalyser.errors.InstanceNotFoundError;
import com.dimanalyser.errors.NotInAnyScopeError;
import com.dimanalyser.errors.ScopeExistsError;
import com.dimanalyser.errors.UnbalancedBracesError;
import com.dimanalyser.errors.UnitAlreadySetError;
import com.dimanalyser.errors.UnitDeclarationsDontMatchError;
import com.dimanalyser.errors.UnitsDontMatchError;
import com.dimanalyser.variablemanager.FunctionInstance;
import com.dimanalyser.variablemanager.InheritanceLevel;
import com.dimanalyser.variablemanager.PhysicalUnit;
import com.dimanalyser.variablemanager.VariableInstance;

public class FortranInterpreter extends Interpreter {

	private ExpressionParser mFortranParser;
	
	public FortranInterpreter() {
		super();
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
				".and.",".or."
		});
		mFortranParser.addBinaryOperatorInHierarchy(new String[]{
				".eq.",".ne."
		});
		mFortranParser.addBinaryOperatorInHierarchy(new String[]{
				".lt.",".gt.",".ge.",".le."
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
		
	}

	@Override
	public int interpretStatements(int linenumber, List<String> lines) 
			throws UnbalancedBracesError, ScopeExistsError, UnitDeclarationsDontMatchError, 
				InstanceExistsError, ExponentNotScalarError, NotInAnyScopeError, UnitAlreadySetError, 
				InstanceNotFoundError, UnitsDontMatchError {
		
		
		String comment = "";
		String instructions = "&";
		int linesinterpreted = 0;
		FunctionInstance currentFunctionInstance = null;
		
		
		while(instructions.endsWith("&")) {
			String line = lines.get(linenumber+linesinterpreted);
			int k = mFortranParser.getInterpretedIndex(line,Globals.COMMENT_START);
			
			if (k < line.length()) {
				comment = comment.concat(line.substring(k+1));
				line = line.substring(0,k);
			}
			
			instructions = instructions.substring(0, instructions.length()-1).concat(line.trim());
			linesinterpreted++;
		}
		
		
		
		if (instructions.startsWith("PROGRAM ")) {
			mVariableManager.enterScope("PROGRAM", InheritanceLevel.SCOPE_PRIVATE);
		} else if (instructions.startsWith("SUBROUTINE ")) {
			List<String> parameterList = mFortranParser.getParametersList(instructions.substring(11));
			mVariableManager.enterScope(parameterList.get(0), InheritanceLevel.SCOPE_PRIVATE);
			currentFunctionInstance = new FunctionInstance(parameterList.get(0), InheritanceLevel.SCOPE_PRIVATE, Globals.UNIT_UNITLESS);
			mVariableManager.addInstance(currentFunctionInstance);
			for(int i=1; i<parameterList.size(); i++) {
				currentFunctionInstance.addParameter(parameterList.get(i));
			}
			
		} else if (instructions.startsWith("REAL")) {
			List<String> variableList = 
					mFortranParser.getVariableList(
							instructions.substring(mFortranParser.getInterpretedIndex(instructions, "::")+2),true);
			
			List<PhysicalUnit> units = parseUnitDeclarationsFromComment(comment);
			
			if (units.size()==variableList.size()) {
				for (int i=0; i<variableList.size(); i++) {
					declareInstance(currentFunctionInstance,variableList.get(i),InheritanceLevel.SCOPE_PUBLIC,units.get(i));
				}
			} else if (units.size()==1) {
				for (int i=0; i<variableList.size(); i++) {
					declareInstance(currentFunctionInstance,variableList.get(i),InheritanceLevel.SCOPE_PUBLIC,units.get(0));
				}
			} else if (units.size()==0) {
				for (int i=0; i<variableList.size(); i++) {
					declareInstance(currentFunctionInstance,variableList.get(i),InheritanceLevel.SCOPE_PUBLIC,null);
				}
			} else {
				throw new UnitDeclarationsDontMatchError();
			}
			
		} else if (instructions.startsWith("END PROGRAM") || instructions.startsWith("END SUBROUTINE")) {
			mVariableManager.leaveScope();
			currentFunctionInstance = null;
		} else if (instructions.startsWith("CONTAINS")) {
			currentFunctionInstance = null;
		} else if (!instructions.trim().equals("")) {
			checkUnits(instructions);
		}
		
		
		return linenumber+linesinterpreted;
	}
	
	
	private void declareInstance(FunctionInstance currentFunctionInstance,
			String name, int accessLevel, PhysicalUnit unit) throws InstanceExistsError, UnitAlreadySetError {
		if (currentFunctionInstance!=null && currentFunctionInstance.hasParameter(name)) {
			currentFunctionInstance.getParameter(name).setUnit(unit);
		}
		
		if (currentFunctionInstance!=null && currentFunctionInstance.getName().equals(name)) {
			currentFunctionInstance.setUnit(unit);
		} else {
			mVariableManager.addInstance(new VariableInstance(name, accessLevel, unit));
		}
	}

	private void checkUnits(String expression) throws UnbalancedBracesError, ExponentNotScalarError, InstanceNotFoundError, UnitsDontMatchError, UnitAlreadySetError {
		List<StackElement> expr = mFortranParser.parseExpression(expression);
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
			} else if (s.equals("**")) {
				StackElement rhs = stack.pop();
				StackElement lhs = stack.pop();
				stack.push(new StackElement(String.format("%s**%s", lhs.getExpression(), rhs.getExpression()), PhysicalUnit.power(lhs.getUnit(), rhs.getUnit())));
			} else if (s.equals("+") || s.equals("-") || 
					s.equals(".le.") || s.equals(".lt.") || 
					s.equals(".gt.") || s.equals(".ge.") || 
					s.equals(".eq.") || s.equals("=")) {
				StackElement rhs = stack.pop();
				StackElement lhs = stack.pop();
				
				if (rhs.getUnit()==null && lhs.getUnit()==null) {
					throw new NotImplementedException();
				} else if (rhs.getUnit()==null) {
					mVariableManager.getInstance(rhs.getExpression().trim()).setUnit(lhs.getUnit());
					rhs.setUnit(lhs.getUnit());
				} else if (lhs.getUnit()==null) {
					mVariableManager.getInstance(lhs.getExpression().trim()).setUnit(rhs.getUnit());
					lhs.setUnit(rhs.getUnit());
				} else {
					if (!lhs.getUnit().equals(rhs.getUnit())) {
						throw new UnitsDontMatchError(lhs, rhs, s);
					}
				}
				stack.push(new StackElement(String.format("%s%s%s", lhs.getExpression(), s.getExpression(), rhs.getExpression()),lhs.getUnit()));
			} else if (s.equals("(")) {
				stack.lastElement().setExpression((String.format("(%s)", stack.lastElement().getExpression())));
			} else {
				try {
					double f = Float.parseFloat(s.getExpression());
					s.setUnit(PhysicalUnit.getUnitless(f));
				} catch (NumberFormatException nfe) {
					s.setUnit(mVariableManager.getInstance(s.getExpression().trim()).getUnit());
				}
				stack.push(s);
			}
		}
	}
}
