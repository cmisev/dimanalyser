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
