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

/**
 * Manage a repository of known instances and scopes.
 * 
 * @author Cyril Misev <c.misev@gmail.com>
 *
 */
public class VariableManager {
	    
		/**
		 * A list of all available scopes
		 */
		private HashMap<String,Scope> mScopes;
		
		/**
		 * A stack used for a sequential walk-through of the file, keeps track of the lower level scopes the currently interpreted
		 * scope is embedded in.
		 */
		private Stack<Scope> mScopeWalk;
		
		/**
		 * The currently interpreted scope.
		 */
		private Scope mCurrentScope;
	
		/**
		 * Constructor. Initialize the internal repository of scopes.
		 */
		public VariableManager() {
	    	mScopes = new HashMap<String, Scope>();
	    	mScopeWalk = new Stack<Scope>();
	    	mCurrentScope = new Scope("__GLOBAL__");
			mScopes.put("__GLOBAL__",mCurrentScope);
	    }
		
		/**
		 * Function called when entering the definition body of a new scope.
		 * 
		 * @param name name given to identify the scope
		 * @param inheritanceLevel inheritance level of the scope (private/public/protected, see {@link InheritanceLevel InheritanceLevel})
		 * @throws ScopeExistsError
		 */
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
		
		/**
		 * Include a known scope to the current scope as an inheritance.
		 * 
		 * @param name the name of the scope to include
		 * @param inheritanceLevel inheritance level of the scope (private/public/protected, see {@link InheritanceLevel InheritanceLevel})
		 * @throws ScopeNotFoundError
		 */
		public void includeScope(String name, int inheritanceLevel) throws ScopeNotFoundError {
			// TODO load external scopes
			if (mScopes.containsKey(name)) {
				mCurrentScope.addInheritance(new Inheritance(mScopes.get(name),inheritanceLevel));
			} else {
				throw new ScopeNotFoundError(name);
			}
		}
		
		/**
		 * Leave the definition body of the current scope
		 * @throws NotInAnyScopeError
		 */
		public void leaveScope() throws NotInAnyScopeError {
			// TODO store scope to file
			try {
				Globals.debug(String.format("Leaving scope %s", mCurrentScope.getName()),mScopeWalk.size()-1);
				mCurrentScope = mScopeWalk.pop();
			} catch(Exception e) {
				throw new NotInAnyScopeError();
			}
		}
		
		/**
		 * Add an instance to the currently interpreted scope
		 * 
		 * @param instance the instance to add to the scope
		 * @throws InstanceExistsError
		 */
		public void addInstance(Instance instance) throws InstanceExistsError {
			Globals.debug(String.format("Adding %s", instance.toString()),mScopeWalk.size());
			mCurrentScope.addInstance(instance);
		}
		
		/**
		 * Get an instance by its name as accessible and/or present in the scope.
		 * 
		 * @param name the name of the instance
		 * @return the instance
		 * @throws InstanceNotFoundError
		 */
		public Instance getInstance(String name) throws InstanceNotFoundError {
			return mCurrentScope.getInstance(name);
		}
		    
}
