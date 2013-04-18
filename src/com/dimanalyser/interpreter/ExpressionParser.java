package com.dimanalyser.interpreter;

import java.util.ArrayList;
import java.util.EmptyStackException;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.Stack;

import com.dimanalyser.errors.UnbalancedBracesError;

public class ExpressionParser {

	List<String> mAtomsList;
	List<String> mBinaryOperatorList;
	List<String> mUnaryOperatorList;
	List<List<String>> mOperatorHierarchy;
	List<String> mBracesOpening;
	List<String> mBracesClosing;
	List<Character> mQuotes;
	List<Character> mQuotesEscape;
	
	
	public ExpressionParser() {
		mOperatorHierarchy = new ArrayList<List<String>>();
		mAtomsList = new ArrayList<String>();
		mBracesOpening = new ArrayList<String>();
		mBracesClosing = new ArrayList<String>();
		mQuotes = new ArrayList<Character>();
		mQuotesEscape = new ArrayList<Character>();
		mBinaryOperatorList = new ArrayList<String>();
		mUnaryOperatorList = new ArrayList<String>();
		
	}

	public void addBinaryOperatorHierarchy(String[] operatorlist) {
		mOperatorHierarchy.add(new ArrayList<String>());
		for(String operator : operatorlist) {
			mOperatorHierarchy.get(mOperatorHierarchy.size()-1).add(operator);
			mAtomsList.add(operator);
			mBinaryOperatorList.add(operator);
		}
	}
	
	public void addUnaryOperator(String operator) {
		mAtomsList.add(operator);
		mUnaryOperatorList.add(operator);
	}
	
	public void addBraces(String opening, String closing) {
		mAtomsList.add(opening);
		mAtomsList.add(closing);
		mBracesOpening.add(opening);
		mBracesClosing.add(closing);
	}
	
	public void addQuotes(Character quote, Character escape) {
		mQuotes.add(quote);
		mQuotesEscape.add(escape);
	}
	
	public Stack<String> parseExpression(String expression) throws UnbalancedBracesError {
		
		Stack<String> stack = new Stack<String>(); 
		List<String> atoms = splitExpression(expression);
		parseAtomsRecursive(stack,atoms,0,atoms.size()-1,0);
		
		return stack;
	}
	

	
	private void parseAtomsRecursive(Stack<String> stack, List<String> atoms,
			int start, int end, int operatorLevel) throws UnbalancedBracesError {
		
		int k = end;
		int i=0;
		Stack<Integer> bracesStack = new Stack<Integer>();
		
		if (start==end) {
			stack.push(atoms.get(start));
			return;
		}
		
		if (start>end) {
			stack.push("");
			return;
		}
		
		if (mQuotes.contains(atoms.get(start).charAt(0)) && start+1==end) {
			stack.push(atoms.get(start+1));
			stack.push(atoms.get(start));
			return;
		}
		
		while(k>=start) {
			if ((i=mBracesClosing.indexOf(atoms.get(k)))>=0) {
				bracesStack.push(i);
			}
			
			if ((i=mBracesOpening.indexOf(atoms.get(k)))>=0) {
				try {
					if (i==bracesStack.lastElement()) {
						bracesStack.pop();
					} else {
						throw new UnbalancedBracesError();
					}
				} catch(NoSuchElementException e) {
					throw new UnbalancedBracesError();
					
				}
			}
			
			if (bracesStack.size()==0) {
				if(operatorLevel < mOperatorHierarchy.size() && mOperatorHierarchy.get(operatorLevel).contains(atoms.get(k)) &&
				   !(k>start && mUnaryOperatorList.contains(atoms.get(k)) && mBinaryOperatorList.contains(atoms.get(k-1))) || 
				   (k==start && mUnaryOperatorList.contains(atoms.get(k))) && operatorLevel==mOperatorHierarchy.size() || 
				   (k==end && mUnaryOperatorList.contains(atoms.get(k))) && operatorLevel==mOperatorHierarchy.size()) {
						parseAtomsRecursive(stack,atoms,start,k-1,operatorLevel);
						parseAtomsRecursive(stack,atoms,k+1,end,operatorLevel);
						stack.push(atoms.get(k));
						return;
				}
			}
			k--;
		}
		if (mBracesClosing.contains(atoms.get(end))) {
			if (mBracesOpening.indexOf(atoms.get(start)) == mBracesClosing.indexOf(atoms.get(end))) {
				parseAtomsRecursive(stack,atoms,start+1,end-1,0);
				stack.push(atoms.get(start));
			} else if (mBracesOpening.indexOf(atoms.get(start+1)) == mBracesClosing.indexOf(atoms.get(end))) {
				parseAtomsRecursive(stack,atoms,start+2,end-1,0);
				stack.push(atoms.get(start+1));
				stack.push(atoms.get(start));
			} else {
				parseAtomsRecursive(stack,atoms,start,end,operatorLevel+1);
			}
		} else {
			parseAtomsRecursive(stack,atoms,start,end,operatorLevel+1);
		}
	}

	public List<String> splitExpression(String expression) {
		List<String> retval = new ArrayList<String>();
		
		boolean inliteral = false;
		Character quote = '"';
		
		int k = 0;
		int lastAtomEnd = 0;
		int quoteStart = 0;
		while (k < expression.length()) {
			if (inliteral) {
				
				int qi = mQuotes.indexOf(expression.charAt(k));
				
				if ((qi >= 0) && (expression.charAt(k)==quote)) {
					int kescapes=k-1;
					while(kescapes > 0 && expression.charAt(kescapes)==mQuotesEscape.get(qi)) {
						kescapes--;
					}
					if (((k-kescapes) & 1) == 1) {
						inliteral = false;
						if (lastAtomEnd != quoteStart) {
							retval.add(expression.substring(lastAtomEnd, quoteStart));
						}
						retval.add(Character.toString(quote));
						retval.add(expression.substring(quoteStart+1,k));
						lastAtomEnd = k+1;
					}
				}
				k++;
				
			} else {
				
				if (mQuotes.contains(expression.charAt(k))) {
					quoteStart = k;
					inliteral = true;
					quote = expression.charAt(k);
					k++;
				} else {					
					String currentAtom = "";
					for (String atom : mAtomsList) {
						if (currentAtom.length()<atom.length() &&
								k+atom.length()<=expression.length() &&
								expression.substring(k, k+atom.length()).equals(atom)) {
							currentAtom = atom;
						}
					}
					
					if (currentAtom.length()==0) {
						k++;
					} else {
						if (lastAtomEnd != k) {
							retval.add(expression.substring(lastAtomEnd, k));
						}
						retval.add(currentAtom);
						k+=currentAtom.length();
						lastAtomEnd = k;
					}
				}
				
			}
		}
		
		if (lastAtomEnd != expression.length()) {
			retval.add(expression.substring(lastAtomEnd));
		}
		
		return retval;
	}
	
	
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
				parameter = parameter.trim();
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
			try {
				if (!inliteral) {
					
					if (bstack.empty() && string.substring(k).startsWith(needle)) {
						return k;
					}
					
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
				} else {
					if ((string.charAt(k)=='"' || string.charAt(k)=='\'') &&
						(string.substring(k, k+1).equals(bstack.lastElement()))) {
						int kescapes=k-1;
						while(kescapes > 0 && string.charAt(kescapes)=='\\') {
							kescapes--;
						}
						if (((k-kescapes) & 1) == 1) {
							inliteral = false;
							bstack.pop();
						}
					}
				}
			} catch(NoSuchElementException e) {
				throw new UnbalancedBracesError();
				
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
			try {
				if (!inliteral) {
					
					if (bstack.empty() && string.substring(k).startsWith(needle)) {
						return k;
					}
						
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
				} else {
					if ((string.charAt(k)=='"' || string.charAt(k)=='\'') &&
						(string.substring(k, k+1).equals(bstack.lastElement()))) {
						
						int kescapes=k-1;
						while(kescapes > 0 && string.charAt(kescapes)=='\\') {
							kescapes--;
						}
						if (((k-kescapes) & 1) == 1) {
							inliteral = false;
							bstack.pop();
						}
						k = kescapes+1;
					}
				}
			} catch(NoSuchElementException e) {
				throw new UnbalancedBracesError();
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
			variable = variable.trim();
			
			if (variable.length()>0) {
				retval.add(variable);
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
