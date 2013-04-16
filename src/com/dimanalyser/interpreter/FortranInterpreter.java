package com.dimanalyser.interpreter;

import java.util.List;

import com.dimanalyser.common.Globals;
import com.dimanalyser.errors.ExponentNotScalarError;
import com.dimanalyser.errors.InstanceExistsError;
import com.dimanalyser.errors.NotInAnyScopeError;
import com.dimanalyser.errors.ScopeExistsError;
import com.dimanalyser.errors.UnbalancedBracesError;
import com.dimanalyser.errors.UnitDeclarationsDontMatchError;
import com.dimanalyser.variablemanager.InheritanceLevel;
import com.dimanalyser.variablemanager.PhysicalUnit;
import com.dimanalyser.variablemanager.VariableInstance;

public class FortranInterpreter extends Interpreter {

	public FortranInterpreter() {
		super();
	}

	@Override
	public int interpretStatements(int linenumber, List<String> lines) 
			throws UnbalancedBracesError, ScopeExistsError, UnitDeclarationsDontMatchError, InstanceExistsError, ExponentNotScalarError, NotInAnyScopeError {
		
		
		String comment = "";
		String instructions = "&";
		int linesinterpreted = 0;
		
		while(instructions.endsWith("&")) {
			String line = lines.get(linenumber+linesinterpreted);
			int k = ParserHelpers.getInterpretedIndex(line,Globals.COMMENT_START);
			
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
			List<String> parameterList = ParserHelpers.getParametersList(instructions);
			mVariableManager.enterScope(parameterList.get(0), InheritanceLevel.SCOPE_PRIVATE);
				
			
		} else if (instructions.startsWith("REAL")) {
			List<String> variableList = 
					ParserHelpers.getVariableList(
							instructions.substring(ParserHelpers.getInterpretedIndex(instructions, "::")+2),true);
			
			List<PhysicalUnit> units = parseUnitDeclarationsFromComment(comment);
			
			if (units.size()==variableList.size()) {
				for (int i=0; i<variableList.size(); i++) {
					mVariableManager.addInstance(new VariableInstance(variableList.get(i),InheritanceLevel.SCOPE_PUBLIC,units.get(i)));
				}
			} else if (units.size()==1) {
				for (int i=0; i<variableList.size(); i++) {
					mVariableManager.addInstance(new VariableInstance(variableList.get(i),InheritanceLevel.SCOPE_PUBLIC,units.get(0)));
				}
			} else if (units.size()==0) {
				for (int i=0; i<variableList.size(); i++) {
					mVariableManager.addInstance(new VariableInstance(variableList.get(i),InheritanceLevel.SCOPE_PUBLIC,null));
				}
			} else {
				throw new UnitDeclarationsDontMatchError();
			}
			
		} else if (instructions.startsWith("END PROGRAM") || instructions.startsWith("END SUBROUTINE")) {
			mVariableManager.leaveScope();
		}
		
		
		return linenumber+linesinterpreted;
	}
	
}
