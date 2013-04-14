package com.dimanalyser.variablemanager;

public class Inheritance {

	private int mInheritanceLevel;
	private Scope mScope;
	
	public Inheritance(Scope scope, int inheritanceLevel) {
		mScope = scope;
		mInheritanceLevel = inheritanceLevel;
	}
	
	public Scope getScope() {
		return mScope;
	}
	
	public int getInheritanceLevel() {
		return mInheritanceLevel;
	}
}
