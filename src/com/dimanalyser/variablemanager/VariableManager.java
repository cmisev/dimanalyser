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

import java.util.HashMap;
import java.util.Stack;

import com.dimanalyser.common.Globals;
import com.dimanalyser.errors.InstanceExistsError;
import com.dimanalyser.errors.InstanceNotFoundError;
import com.dimanalyser.errors.NotInAnyScopeError;
import com.dimanalyser.errors.ScopeExistsError;
import com.dimanalyser.errors.ScopeNotFoundError;

public class VariableManager {
	    
		private HashMap<String,Scope> mScopes;
		private Stack<Scope> mScopeWalk;
		private Scope mCurrentScope;
	
		public VariableManager() {
	    	mScopes = new HashMap<String, Scope>();
	    	mScopeWalk = new Stack<Scope>();
	    	mCurrentScope = new Scope("__GLOBAL__");
			mScopes.put("__GLOBAL__",mCurrentScope);
	    }
		
		public void enterScope(String name,int inheritanceLevel) throws ScopeExistsError {
			Globals.debug(String.format("Entering scope %s", name),mScopeWalk.size());
			
			if (mScopes.containsKey(name)) {
				throw new ScopeExistsError(name);
			} else {
				mScopeWalk.push(mCurrentScope);
				Scope scope = new Scope(name);
				scope.addInheritance(new Inheritance(mCurrentScope, inheritanceLevel));
				mCurrentScope = scope;
				mScopes.put(name,mCurrentScope);
			}
		}
		
		public void includeScope(String name, int inheritanceLevel) throws ScopeNotFoundError {
			if (mScopes.containsKey(name)) {
				mCurrentScope.addInheritance(new Inheritance(mScopes.get(name),inheritanceLevel));
			} else {
				throw new ScopeNotFoundError(name);
			}
		}
		
		public void leaveScope() throws NotInAnyScopeError {
			try {
				Globals.debug(String.format("Leaving scope %s", mCurrentScope.getName()),mScopeWalk.size()-1);
				mCurrentScope = mScopeWalk.pop();
			} catch(Exception e) {
				throw new NotInAnyScopeError();
			}
		}
		
		public void addInstance(Instance instance) throws InstanceExistsError {
			Globals.debug(String.format("Adding %s", instance.toString()),mScopeWalk.size());
			mCurrentScope.addInstance(instance);
		}
		
		
		public Instance getInstance(String name) throws InstanceNotFoundError {
			return mCurrentScope.getInstance(name);
		}
		    
}
