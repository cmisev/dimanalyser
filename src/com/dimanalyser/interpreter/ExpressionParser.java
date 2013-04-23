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

public class ExpressionParser {

	List<String> mAtomsList;
	List<String> mBinaryOperatorList;
	List<String> mUnaryOperatorList;
	List<String> mListSeparatorList;
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
		mListSeparatorList = new ArrayList<String>();
	}

	public void addBinaryOperatorInHierarchy(String[] operatorlist) {
		mOperatorHierarchy.add(new ArrayList<String>());
		for(String operator : operatorlist) {
			mOperatorHierarchy.get(mOperatorHierarchy.size()-1).add(operator);
			mAtomsList.add(operator);
			mBinaryOperatorList.add(operator);
		}
	}
	
	public void addListSeparatorInHierarchy(String[] operatorlist) {
		mOperatorHierarchy.add(new ArrayList<String>());
		for(String operator : operatorlist) {
			mOperatorHierarchy.get(mOperatorHierarchy.size()-1).add(operator);
			mAtomsList.add(operator);
			mListSeparatorList.add(operator);
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
	
	public List<StackElement> parseExpression(String expression) throws UnbalancedBracesError {
		
		List<StackElement> stack = new Stack<StackElement>(); 
		List<String> atoms = splitExpression(expression);
		parseAtomsRecursive(stack,atoms,0,atoms.size()-1,0);
		
		return stack;
	}
	

	
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
	

	
	public List<String> getParametersList(String string) throws UnbalancedBracesError {
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
	
	public  int getInterpretedIndex(String string, String needle) throws UnbalancedBracesError {
		return getInterpretedIndex(string, needle, 0);
	}

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
	
	public int getInterpretedIndexReverse(String string, String needle) throws UnbalancedBracesError {
		return getInterpretedIndexReverse(string, needle, string.length()-1);
	}

	
	public List<String> getVariableList(String string, boolean stripBraces) throws UnbalancedBracesError {
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
