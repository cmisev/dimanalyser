package com.dimanalyser.interpreter;

import java.util.List;

import com.dimanalyser.common.Globals;
import com.dimanalyser.errors.ExponentNotScalarError;
import com.dimanalyser.errors.InstanceExistsError;
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
			throws UnbalancedBracesError, ScopeExistsError, UnitDeclarationsDontMatchError, InstanceExistsError, ExponentNotScalarError {
		
		
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
/*
if line.startswith("PROGRAM"):
    self._vm.enterScope("PROGRAM",Globals.SCOPE_PRIVATE)

if line.startswith("SUBROUTINE "):
    line = line.lstrip("SUBROUTINE").lstrip().rstrip(":")
    k = 0
    while k<len(line) and not line[k]=='(':
        k = k + 1
    
    self._vm.enterScope(line[0:k],Globals.SCOPE_PRIVATE)

if line.startswith("MODULE "):
    line = line.lstrip("MODULE").lstrip().rstrip(":")
    k = 0
    while k<len(line) and not line[k]=='(':
        k = k + 1
    
    self._vm.enterScope(line[0:k],Globals.SCOPE_PRIVATE)


if line.startswith("REAL"):
    if Globals.unitdeclarator_start in comment:
        unit = self.interpretUnitDeclaration(comment)
        
        line = line[line.find("::")+2:]
        names = self.stripBraces(line).split(",")
        
        for name in names:
            self._vm.addInstance(ValueInstance(name.strip(), Globals.SCOPE_PRIVATE, unit))
    else:
        Globals.warn("REAL variable without unit declaration. Handled as unitless")


if line.startswith("END PROGRAM") or line.startswith("END SUBROUTINE") or line.startswith("END FUNCTION"):   
    self._vm.leaveScope()


return linenumber + 1

*/