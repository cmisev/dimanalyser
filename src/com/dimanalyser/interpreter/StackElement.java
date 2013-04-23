package com.dimanalyser.interpreter;

import com.dimanalyser.variablemanager.PhysicalUnit;

public class StackElement {

	private String mExpression;
	private PhysicalUnit mUnit;
	private int mOperandsCount;
	
	
	public StackElement(String expression, PhysicalUnit unit, int operandsCount) {
		mExpression = expression;
		mUnit = unit;
		mOperandsCount = operandsCount;
	}
	
	public StackElement(String expression, int operandsCount) {
		this(expression,null,operandsCount);
	}
	
	public StackElement(String expression) {
		this(expression,0);
	}

	public StackElement(String expression, PhysicalUnit unit) {
		this(expression,unit,0);
	}

	public void setExpression(String expression) {
		mExpression = expression;
	}
	
	public void setUnit(PhysicalUnit unit) {
		mUnit = unit;
	}
	
	public String getExpression() {
		return mExpression;
	}
	
	public PhysicalUnit getUnit() {
		return mUnit;
	}
	
	public int getOperandsCount() {
		return mOperandsCount;
	}
	
	public String toString() {
		return mExpression;
	}
	
	public boolean equals(String expr) {
		return mExpression.equals(expr);
	}
	
	public boolean equals(PhysicalUnit unit) {
		return mUnit.equals(unit);
	}

}
