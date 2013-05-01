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

package com.dimanalyser.interpreter;

import java.util.ArrayList;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.Stack;

import com.dimanalyser.errors.UnbalancedBracesError;

/**
 * A generic Parser of mathematical expressions used by the interpreter. Each programming language
 * has a characteristic set of indivisible strings, called atoms in this expression parser, representing
 * operators, braces, and quotes. The class allows these strings to be added to an internal inventory.
 * 
 * the {@link ExpressionParser#parseExpression(String) parseExpression} method
 * is the core of the Parser and parses the expression into a list of {@link StackElement StackElement} 
 * instances, that can be easily interpreted using a stack. While iterating over the StackElement list,
 * StackElements containing operands are pushed onto the list, while operators pop the last pushed elements,
 * act on them and push the result back on the stack.
 * 
 * @author Cyril Misev <c.misev@gmail.com>
 *
 */
public class ExpressionParser {

	/**
	 * Full list of atoms
	 */
	List<String> mAtomsList;
	
	/**
	 * Atoms list containing all binary operators
	 */
	List<String> mBinaryOperatorList;
	
	/**
	 * Atoms list containing all unary operators. Unary operators can be appended either to the left or to the right of an expression
	 */
	List<String> mUnaryOperatorList;
	
	/**
	 * Atoms list containing all list separators.
	 */
	List<String> mListSeparatorList;
	
	/**
	 * Hierarchical structure containing all binary operators and list separators. Each hierarchy level can contain several atoms.
	 * The hierarchy is necessary to identify in which order the operators/list separators have to be interpreted, e.g. typically
	 * the operations addition and subtraction are interpreted before (lower index in the hierarchy) the operations multiplication
	 * and division, while products and fractions keep being interpreted as a whole expression.
	 */
	List<List<String>> mOperatorHierarchy;
	
	/**
	 * Atoms list containing all opening braces. Braces can have more than one character (e.g. "(/ ... /)" in Fortran. 
	 * The matching closing brace has the same index as the opening brace.
	 */
	List<String> mBracesOpening;
	
	/**
	 * Atoms list containing all closing braces.
	 * The matching opening brace has the same index as the closing brace.
	 */
	List<String> mBracesClosing;
	
	/**
	 * List containing characters starting and ending literal quotes.
	 */
	List<Character> mQuotes;
	
	/**
	 * List containing the escape characters belonging to the quote characters (same index). 
	 */
	List<Character> mQuotesEscape;
	
	
	/**
	 * Constructor. Initializes the internal atoms lists.
	 */
	public ExpressionParser() {
		mOperatorHierarchy = new ArrayList<List<String>>();
		mAtomsList = new ArrayList<String>();
		mBracesOpening = new ArrayList<String>();
		mBracesClosing = new ArrayList<String>();
		mQuotes = new ArrayList<Character>();
		mQuotesEscape = new ArrayList<Character>();
		mBinaryOperatorList = new ArrayList<String>();
		mUnaryOperatorList = new ArrayList<String>();
		mListSeparatorList = new ArrayList<String>();
	}

	/**
	 * Add a list of binary operators to the list of known binary operators. E.g. addition and subtraction operators
	 * are interpreted on a lower level than multiplication operators. Therefore, these operators must be added using
	 * separate calls to this method, first adding the addition/subtraction operators followed by the multiplication/division
	 * operators.
	 * 
	 * @param operatorlist a list of operators to be added to the next hierarchy.
	 */
	public void addBinaryOperatorInHierarchy(String[] operatorlist) {
		mOperatorHierarchy.add(new ArrayList<String>());
		for(String operator : operatorlist) {
			mOperatorHierarchy.get(mOperatorHierarchy.size()-1).add(operator);
			mAtomsList.add(operator);
			mBinaryOperatorList.add(operator);
		}
	}
	
	
	/**
	 * Add list separators to the list of known list separators in the same hierarchy as binary operators.
	 * Typically, list separators are added in a separate call before adding arithmetic operators.
	 * 
	 * @param operatorlist a list of list separators to be added to the next hierarchy.
	 */
	public void addListSeparatorInHierarchy(String[] operatorlist) {
		mOperatorHierarchy.add(new ArrayList<String>());
		for(String operator : operatorlist) {
			mOperatorHierarchy.get(mOperatorHierarchy.size()-1).add(operator);
			mAtomsList.add(operator);
			mListSeparatorList.add(operator);
		}
	}
	
	/**
	 * Add an unary operator to the list of known unary operators. The same operator can be included in the list of binary operators
	 * as well, and must be included if it may act both as an unary and a binary operator.
	 * 
	 * @param operator the operator to be added
	 */
	public void addUnaryOperator(String operator) {
		mAtomsList.add(operator);
		mUnaryOperatorList.add(operator);
	}
	
	/**
	 * Add a opening/closing brace pair to the list of known braces. 
	 * 
	 * @param opening the opening brace
	 * @param closing the closing brace
	 */
	public void addBraces(String opening, String closing) {
		mAtomsList.add(opening);
		mAtomsList.add(closing);
		mBracesOpening.add(opening);
		mBracesClosing.add(closing);
	}
	
	/**
	 * Add quote characters and its escape character to the list of known quote characters. The literal quote isn't 
	 * considered terminated when it is lead by an non-escaped escape character.
	 * 
	 * @param quote the quote character
	 * @param escape the corresponding escape character
	 */
	public void addQuotes(Character quote, Character escape) {
		mQuotes.add(quote);
		mQuotesEscape.add(escape);
	}
	
	
	/**
	 * The core method of the expression parser. Returns a list of {@link StackElement StackElement} instances.
	 * The expression can be easily interpreted by iterating over the list using a stack, where:
	 * 
	 * <ul>
	 * 		<li>operands get pushed onto the stack</li>
	 * 		<li>
	 * 			operators pop the last two elements of the stack (right hand side operand is the last element, 
	 * 			left hand side operand is the second-last element), perform the operation and push the result onto 
	 * 			the stack. For unary operators acting on the left/right hand side of the expression, the expression corresponds
	 * 			to the last/second last element respectively.
	 * 		</li>
	 * 		<li>braces are given by their opening string</li>
	 * 		<li>strings preceding braces (like function names) are following the element containing the brace</li>
	 * 		<li>
	 * 			the interpretation of a list is triggered by the appearance of a stack element in the returned list 
	 * 			containing the list separator string. The number of list elements is given by the 
	 * 			{@link StackElement#getOperandsCount() getOperandsCount()} getter of this element containing the list separator.
	 * 			The list elements correspond to the last pushed elements of the stack.
	 * 		</li>
	 * 		<li>quotes are given by two list elements the first containing the enclosed literal, the second containing the quote character.</li>
	 * </ul>
	 * 
	 * @param expression The expression to be parsed
	 * @return The list of stack elements interpreted according to the list of known operators, list separators and quote characters.
	 * @throws UnbalancedBracesError
	 */
	public List<StackElement> parseExpression(String expression) throws UnbalancedBracesError {
		List<StackElement> stack = new ArrayList<StackElement>(); 
		List<String> atoms = splitExpression(expression);
		parseAtomsRecursive(stack,atoms,0,atoms.size()-1,0);
		
		return stack;
	}
	
	/**
	 * An internal recursive method to parse a sub-expression of the preprocessed, split expression string
	 * 
	 * @param exp the eventual list of stack elements returned by {@link ExpressionParser#parseExpression(String) parseExpression}
	 * @param atoms the full list of expression chunks preprocessed by {@link ExpressionParser#splitExpression(String) splitExpression}
	 * @param start the start index of the expression to be interpreted in the atoms array (inclusive)
	 * @param end the end index of the expression to be interpreted in the atoms array (inclusive)
	 * @param operatorLevel the level of the operators in the operator hierarchy to interpret in this call.
	 * @throws UnbalancedBracesError
	 */
	private void parseAtomsRecursive(List<StackElement> exp, List<String> atoms,
			int start, int end, int operatorLevel) throws UnbalancedBracesError {
		
		int k = end;
		int i=0;
		Stack<Integer> bracesStack = new Stack<Integer>();
		
		if (start==end) {
			exp.add(new StackElement(atoms.get(start)));
			return;
		}
		
		if (start>end) {
			exp.add(new StackElement(""));
			return;
		}
		
		if (mQuotes.contains(atoms.get(start).charAt(0)) && start+1==end) {
			exp.add(new StackElement(atoms.get(start+1)));
			exp.add(new StackElement(atoms.get(start),1));
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
				if (operatorLevel < mOperatorHierarchy.size() && mOperatorHierarchy.get(operatorLevel).contains(atoms.get(k)) && 
						mListSeparatorList.contains(atoms.get(k))) {
					parseAtomsRecursive(exp, atoms, start, k-1, operatorLevel);
					StackElement lelm = exp.get(exp.size()-1);
					if (lelm.getExpression().equals(atoms.get(k))) {
						exp.remove(exp.size()-1);
						parseAtomsRecursive(exp, atoms, k+1, end, operatorLevel);
						exp.add(new StackElement(atoms.get(k),lelm.getOperandsCount()+1));
					} else {
						parseAtomsRecursive(exp, atoms, k+1, end, operatorLevel);
						exp.add(new StackElement(atoms.get(k),2));
					}
					return;
				}
				
				if(operatorLevel < mOperatorHierarchy.size() && mOperatorHierarchy.get(operatorLevel).contains(atoms.get(k)) &&
				   !(k>start && mUnaryOperatorList.contains(atoms.get(k)) && mBinaryOperatorList.contains(atoms.get(k-1))) || 
				   (k==start && mUnaryOperatorList.contains(atoms.get(k))) && operatorLevel==mOperatorHierarchy.size() || 
				   (k==end && mUnaryOperatorList.contains(atoms.get(k))) && operatorLevel==mOperatorHierarchy.size()) {
						parseAtomsRecursive(exp,atoms,start,k-1,operatorLevel);
						parseAtomsRecursive(exp,atoms,k+1,end,operatorLevel);
						exp.add(new StackElement(atoms.get(k),2));
						return;
				}
				

			}
			k--;
		}
		if (mBracesClosing.contains(atoms.get(end))) {
			if (mBracesOpening.indexOf(atoms.get(start)) == mBracesClosing.indexOf(atoms.get(end))) {
				parseAtomsRecursive(exp,atoms,start+1,end-1,0);
				exp.add(new StackElement(atoms.get(start),1));
			} else if (mBracesOpening.indexOf(atoms.get(start+1)) == mBracesClosing.indexOf(atoms.get(end))) {
				parseAtomsRecursive(exp,atoms,start+2,end-1,0);
				exp.add(new StackElement(atoms.get(start+1),1));
				exp.add(new StackElement(atoms.get(start),1));
			} else {
				parseAtomsRecursive(exp,atoms,start,end,operatorLevel+1);
			}
		} else {
			parseAtomsRecursive(exp,atoms,start,end,operatorLevel+1);
		}
	}

	/**
	 * Split the expression string into chunks of either atoms or not yet interpreted expressions (variable names, numerical constants)
	 * 
	 * @param expression the expression to be split
	 * @return an array of split chunks of the expression
	 */
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
					
					
					if (k>1 && (expression.charAt(k)=='-' || expression.charAt(k)=='+')
						&& ("eEdD".contains(expression.substring(k-1,k)))
						&& ("0123456789.".contains(expression.substring(k-2,k-1)))) {
						k++;
					} else {
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
		}
		
		if (lastAtomEnd != expression.length()) {
			retval.add(expression.substring(lastAtomEnd));
		}
		
		return retval;
	}
	

	
	/**
	 * Get a list of strings where in an expression <pre>some_string(one,two,three)</pre> the first element of the list (index 0)
	 * is "some_string", the index 1 points to "one", the index 2 points to "two" and the index 3 points to "three"
	 * 
	 * @param string the expression to be parsed
	 * @return the list of strings
	 * @throws UnbalancedBracesError
	 */
	public List<String> getParametersList(String string) throws UnbalancedBracesError {
		// TODO better integration in language-specific parser configuration
		List<String> retval = new ArrayList<String>();
 		int k = getInterpretedIndex(string, "(");
		if (k<string.length()) {
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
	
	/**
	 * Looks up and returns the position of the first occurrence from the left of <pre>needle</pre> in <pre>expression</pre>, 
	 * while ignoring all occurrences surrounded by braces or by quotes.
	 *  
	 * @param expression the expression to be examined
	 * @param needle the string to be found in the expression
	 * @param start the starting index of the lookup. all occurrences before <pre>start</pre> are ignored
	 * @return index of first occurrence in the expression from the left after <pre>start</pre>. <pre>expression.length()</pre> if <pre>needle</pre> is not found.
	 * @throws UnbalancedBracesError
	 */
	public int getInterpretedIndex(String expression, String needle, int start) throws UnbalancedBracesError {
		boolean inliteral = false;
		Character quote = '"';
		int k = start;
		Stack<Integer> bstack = new Stack<Integer>();

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
					}
				}
				k++;
				
			} else {
				
				if (mQuotes.contains(expression.charAt(k))) {
					inliteral = true;
					quote = expression.charAt(k);
					k++;
				} else {
					if (bstack.empty() && expression.substring(k).startsWith(needle)) {
						return k;
					}
					
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
						int bi = 0;
						if ((bi=mBracesClosing.indexOf(currentAtom))>=0) {
							try {
								if (bi==bstack.lastElement()) {
									bstack.pop();
								} else {
									throw new UnbalancedBracesError();
								}
							} catch(NoSuchElementException e) {
								throw new UnbalancedBracesError();
								
							}
						} else if ((bi=mBracesOpening.indexOf(currentAtom))>=0) {
							bstack.push(bi);
						}
						k+=currentAtom.length();
					}
				}
				
			}
		}
		
		return k;
	}
	
	
	/**
	 * Looks up and returns the position of the first occurrence from the left of <pre>needle</pre> in <pre>expression</pre>, 
	 * while ignoring all occurrences surrounded by braces or by quotes.
	 *  
	 * @param expression the expression to be examined
	 * @param needle the string to be found in the expression
	 * @return index of first occurrence in the expression from the left. <pre>expression.length()</pre> if <pre>needle</pre> is not found.
	 * @throws UnbalancedBracesError
	 */
	public  int getInterpretedIndex(String string, String needle) throws UnbalancedBracesError {
		return getInterpretedIndex(string, needle, 0);
	}


	/**
	 * Looks up and returns the position of the first occurrence from the right of <pre>needle</pre> in <pre>expression</pre>, 
	 * while ignoring all occurrences surrounded by braces or by quotes.
	 *  
	 * @param expression the expression to be examined
	 * @param needle the string to be found in the expression
	 * @param start the starting index of the lookup. all occurrences after <pre>start</pre> are ignored
	 * @return index of first occurrence in the expression from the right before <pre>start</pre>. <pre>-1</pre> if <pre>needle</pre> is not found.
	 * @throws UnbalancedBracesError
	 */
	public int getInterpretedIndexReverse(String expression, String needle, int start) throws UnbalancedBracesError {
		boolean inliteral = false;
		Character quote = '"';
		int k = start;
		Stack<Integer> bstack = new Stack<Integer>();

		while (k > -1) {
			if (inliteral) {
				
				int qi = mQuotes.indexOf(expression.charAt(k));
				
				if ((qi >= 0) && (expression.charAt(k)==quote)) {
					int kescapes=k-1;
					while(kescapes > 0 && expression.charAt(kescapes)==mQuotesEscape.get(qi)) {
						kescapes--;
					}
					if (((k-kescapes) & 1) == 1) {
						inliteral = false;
					}
				}
				k--;
				
			} else {
				
				if (mQuotes.contains(expression.charAt(k))) {
					inliteral = true;
					quote = expression.charAt(k);
					k--;
				} else {
					if (bstack.empty() && expression.substring(k).startsWith(needle)) {
						return k;
					}
					
					String currentAtom = "";
					for (String atom : mAtomsList) {
						if (currentAtom.length()<atom.length() &&
								k-atom.length()>=-1 &&
								expression.substring(k-atom.length()+1, k+1).equals(atom)) {
							currentAtom = atom;
						}
					}

					if (currentAtom.length()==0) {
						k--;
					} else {
						int bi = 0;
						if ((bi=mBracesOpening.indexOf(currentAtom))>=0) {
							try {
								if (bi==bstack.lastElement()) {
									bstack.pop();
								} else {
									throw new UnbalancedBracesError();
								}
							} catch(NoSuchElementException e) {
								throw new UnbalancedBracesError();
								
							}
						} else if ((bi=mBracesClosing.indexOf(currentAtom))>=0) {
							bstack.push(bi);
						}

						k-=currentAtom.length();
					}
				}
				
			}
		}
		
		return k;
	}
	
	/**
	 * Looks up and returns the position of the first occurrence from the right of <pre>needle</pre> in <pre>expression</pre>, 
	 * while ignoring all occurrences surrounded by braces or by quotes.
	 *  
	 * @param expression the expression to be examined
	 * @param needle the string to be found in the expression
	 * @return index of first occurrence in the expression from the right <pre>-1</pre> if <pre>needle</pre> is not found.
	 * @throws UnbalancedBracesError
	 */
	public int getInterpretedIndexReverse(String string, String needle) throws UnbalancedBracesError {
		return getInterpretedIndexReverse(string, needle, string.length()-1);
	}

	/**
	 * Interprets a comma-separated list of variables into a list of strings
	 * 
	 * @param string string containing the comma-separated variables
	 * @param stripBraces strip braces
	 * @return the list of comma-separated variables
	 * @throws UnbalancedBracesError
	 */
	public List<String> getVariableList(String string, boolean stripBraces) throws UnbalancedBracesError {
		// TODO better integration in language-specific parser configuration
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
