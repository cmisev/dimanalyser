package com.dimanalyser.interpreter;

import java.util.ArrayList;
import java.util.List;

import com.dimanalyser.common.Globals;
import com.dimanalyser.errors.ExponentNotScalarError;
import com.dimanalyser.errors.InterpretationError;
import com.dimanalyser.errors.UnbalancedBracesError;
import com.dimanalyser.variablemanager.PhysicalUnit;
import com.dimanalyser.variablemanager.VariableManager;

public abstract class Interpreter {
	
	protected VariableManager mVariableManager;
	
	protected Interpreter() {
		Globals.initUnits();
		mVariableManager = new VariableManager();
	}
	
	public abstract int interpretStatements(int linenumber, List<String> lines) throws InterpretationError;
	
	
	public List<PhysicalUnit> parseUnitDeclarationsFromComment(String string) throws UnbalancedBracesError, ExponentNotScalarError {
		List<PhysicalUnit> retval = new ArrayList<PhysicalUnit>();
		
		int start = 0;
		int end = 0;
		
		while((start=ExpressionParser.getInterpretedIndex(string, "U(", end))<string.length()) {
			end = ExpressionParser.getInterpretedIndex(string, ")",start+2);
			retval.add(parseUnitDeclaration(string.substring(start+2,end).replace(" ","")));
			end = end + 1;
		}
		
		return retval;
	}
	
	private PhysicalUnit parseUnitDeclaration(String string) throws UnbalancedBracesError, ExponentNotScalarError {
		int k=Math.max(ExpressionParser.getInterpretedIndexReverse(string,"*"),
				ExpressionParser.getInterpretedIndexReverse(string,"/"));
		
		if (k!=-1) {
			PhysicalUnit lhs = parseUnitDeclaration(string.substring(0, k));
			PhysicalUnit rhs = parseUnitDeclaration(string.substring(k+1));
			if (string.substring(k, k+1).equals("*")) {
				return PhysicalUnit.product(lhs,rhs);
			} else {
				return PhysicalUnit.fraction(lhs,rhs);
			}
		}
		
		
		k = ExpressionParser.getInterpretedIndexReverse(string,"^");
		
		if (k!=-1) {
			PhysicalUnit lhs = parseUnitDeclaration(string.substring(0, k));
			PhysicalUnit rhs = parseUnitDeclaration(string.substring(k+1));
			return PhysicalUnit.power(lhs,rhs);
		}
		
		if (string.startsWith("(")) {
			return parseUnitDeclaration(string.substring(1, string.length()-1));
		}
		
		try {
			return PhysicalUnit.product(Globals.UNIT_UNITLESS, Double.parseDouble(string));
		} catch(NumberFormatException nfe) {
			return Globals.units.get(string);
		}
	}
}
