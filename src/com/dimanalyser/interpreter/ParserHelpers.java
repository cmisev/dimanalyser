package com.dimanalyser.interpreter;

import java.util.ArrayList;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.Stack;

import com.dimanalyser.errors.UnbalancedBracesError;

public class ParserHelpers {

	public static List<String> getParametersList(String string) throws UnbalancedBracesError {
		List<String> retval = new ArrayList<String>();
 		int k = string.indexOf("(");
		if (k>0) {
			retval.add(string.substring(0,k));
			string = string.substring(k+1);
			k = getInterpretedIndex(string, ")");
			string = string.substring(0, k);
			while(string.length()>0) {
				k = getInterpretedIndex(string, ",");
				String parameter = string.substring(0,k);
				if (parameter.length()>0) {
					retval.add(parameter);
				}
				if (k<string.length()) {
					string = string.substring(k+1);
				} else {
					string ="";
				}
			}
		} else {
			retval.add(string);
		}
		return retval;
	}
	
	public static int getInterpretedIndex(String string, String needle, int start) throws UnbalancedBracesError {
		int k = start;
		Stack<String> bstack = new Stack<String>();
		boolean inliteral = false;

		while(k!=string.length()) {
			if (!inliteral) {
				
				if (bstack.empty() && string.substring(k).startsWith(needle)) {
					return k;
				}
				try {
					
					switch (string.charAt(k)) {
					case '"':
					case '\'':
						inliteral = true;
					case '(':
					case '[':
						bstack.push(string.substring(k, k+1));
						break;
					case ')':
						if(bstack.lastElement().equals("(")) {
							bstack.pop();
						} else {
							throw new UnbalancedBracesError();
						}
						break;
					case ']':
						if(bstack.lastElement().equals("[")) {
							bstack.pop();
						} else {
							throw new UnbalancedBracesError();
						}
						break;
					}
				} catch(NoSuchElementException e) {
					throw new UnbalancedBracesError();
				}
			} else {
				if ((string.charAt(k)=='"' || string.charAt(k)=='\'') &&
					(string.substring(k, k+1).equals(bstack.lastElement())) &&
					(k==0 || string.charAt(k-1)!='\\')) {
					inliteral = false;
					bstack.pop();
				}
			}
			
			k++;
		}
		return k;
	}
	
	public static int getInterpretedIndex(String string, String needle) throws UnbalancedBracesError {
		return getInterpretedIndex(string, needle, 0);
	}

	public static int getInterpretedIndexReverse(String string, String needle, int start) throws UnbalancedBracesError {
		int k = start;
		Stack<String> bstack = new Stack<String>();
		boolean inliteral = false;

		while(k!=-1) {
			if (!inliteral) {
				
				if (bstack.empty() && string.substring(k).startsWith(needle)) {
					return k;
				}
				try {
					
					switch (string.charAt(k)) {
					case '"':
					case '\'':
						inliteral = true;
					case ')':
					case ']':
						bstack.push(string.substring(k, k+1));
						break;
					case '(':
						if(bstack.lastElement().equals(")")) {
							bstack.pop();
						} else {
							throw new UnbalancedBracesError();
						}
						break;
					case '[':
						if(bstack.lastElement().equals("]")) {
							bstack.pop();
						} else {
							throw new UnbalancedBracesError();
						}
						break;
					}
				} catch(NoSuchElementException e) {
					throw new UnbalancedBracesError();
				}
			} else {
				if ((string.charAt(k)=='"' || string.charAt(k)=='\'') &&
					(string.substring(k, k+1).equals(bstack.lastElement())) &&
					(k==0 || string.charAt(k-1)!='\\')) {
					inliteral = false;
					bstack.pop();
				}
			}
			
			k--;
		}
		return k;
	}
	
	public static int getInterpretedIndexReverse(String string, String needle) throws UnbalancedBracesError {
		return getInterpretedIndexReverse(string, needle, string.length()-1);
	}

	
	public static List<String> getVariableList(String string, boolean stripBraces) throws UnbalancedBracesError {
		List<String> retval = new ArrayList<String>();
		
		int k = 0;
		
		while(string.length()>0) {
			k = getInterpretedIndex(string, ",");
			String variable = string.substring(0,k);
			if (stripBraces) {
				int j = variable.indexOf("(");
				if (j>0) {
					variable = variable.substring(0,j);
				}
			}
			
			if (variable.length()>0) {
				retval.add(variable.trim());
			}
			if (k<string.length()) {
				string = string.substring(k+1);
			} else {
				string ="";
			}
		}
		return retval;
	}



	

}
