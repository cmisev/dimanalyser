package com.dimanalyser.test;

import static org.junit.Assert.*;

import java.util.List;
import java.util.Stack;


import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import com.dimanalyser.errors.UnbalancedBracesError;
import com.dimanalyser.interpreter.ExpressionParser;

public class ExpressionParserTest {

	@Before
	public void setUp() throws Exception {
	}

	@After
	public void tearDown() throws Exception {
	}
	
	
	@Test
	public void testExpressionParser() {
		ExpressionParser ep = new ExpressionParser();
		
		ep.addBraces("(", ")");
		ep.addBraces("(/", "/)");
		ep.addQuotes('"', '\\');
		ep.addBinaryOperatorHierarchy(new String[]{
				"="
		});
		ep.addBinaryOperatorHierarchy(new String[]{
				","
		});
		ep.addBinaryOperatorHierarchy(new String[]{
				".and.",".or."
		});
		ep.addBinaryOperatorHierarchy(new String[]{
				".eq.",".ne."
		});
		ep.addBinaryOperatorHierarchy(new String[]{
				".lt.",".gt.",".ge.",".le."
		});
		ep.addBinaryOperatorHierarchy(new String[]{
				"+","-"
		});
		ep.addBinaryOperatorHierarchy(new String[]{
				"*","/"
		});
		ep.addBinaryOperatorHierarchy(new String[]{
				"**"
		});
		ep.addBinaryOperatorHierarchy(new String[]{
				"e-","e+","E-","E+"
		});
		
		ep.addUnaryOperator("-");
		ep.addUnaryOperator("++");
		
		
//		List<String> actual = ep.splitExpression("forces = (/ 1.0, -3.0, (mass1+mass2-mass3)*distance/time**2, -force2+force3 /)".replace(" ",""));
//		
//		for (String elem : actual) {
//			System.out.println("["+elem+"]");
//		}
		//  calculate_force(\"coriolis\",2), calculate_force(), calculate_force(2.0)
		try {
			Stack<String> actual = ep.parseExpression("forces = (/ 1.0, -3.0, (mass1+mass2-mass3)*distance/time**2, -force2+force3 /)".replace(" ",""));
			assertEquals("=", actual.pop());
			assertEquals("(/", actual.pop());
			assertEquals(",", actual.pop());
			assertEquals("+", actual.pop());
			assertEquals("force3", actual.pop());
			assertEquals("-", actual.pop());
			assertEquals("force2", actual.pop());
			assertEquals("", actual.pop());
			assertEquals(",", actual.pop());
			assertEquals("/", actual.pop());
			assertEquals("**", actual.pop());
			assertEquals("2", actual.pop());
			assertEquals("time", actual.pop());
			assertEquals("*", actual.pop());
			assertEquals("distance", actual.pop());
			assertEquals("(", actual.pop());
			assertEquals("-", actual.pop());
			assertEquals("mass3", actual.pop());
			assertEquals("+", actual.pop());
			assertEquals("mass2", actual.pop());
			assertEquals("mass1", actual.pop());
			assertEquals(",", actual.pop());
			assertEquals("-", actual.pop());
			assertEquals("3.0", actual.pop());
			assertEquals("", actual.pop());
			assertEquals("1.0", actual.pop());
			assertEquals("forces", actual.pop());
			assertTrue(actual.empty());
			
			actual = ep.parseExpression("(val.lt.1e-5.and.val.gt.-tol)");
			assertEquals("(", actual.pop());
			assertEquals(".and.", actual.pop());
			assertEquals(".gt.", actual.pop());
			assertEquals("-", actual.pop());
			assertEquals("tol", actual.pop());
			assertEquals("", actual.pop());
			assertEquals("val", actual.pop());
			assertEquals(".lt.", actual.pop());
			assertEquals("e-", actual.pop());
			assertEquals("5", actual.pop());
			assertEquals("1", actual.pop());
			assertEquals("val", actual.pop());
			assertTrue(actual.empty());
			
			
			actual = ep.parseExpression("(\"\",\"\\\"\",\"\")");
			assertEquals("(", actual.pop());
			assertEquals(",", actual.pop());
			assertEquals("\"", actual.pop());
			assertEquals("", actual.pop());
			assertEquals(",", actual.pop());
			assertEquals("\"", actual.pop());
			assertEquals("\\\"", actual.pop());
			assertEquals("\"", actual.pop());
			assertEquals("", actual.pop());
			assertTrue(actual.empty());
			
			actual=ep.parseExpression("function()");
			assertEquals("function", actual.pop());
			assertEquals("(", actual.pop());
			assertEquals("", actual.pop());
			assertTrue(actual.empty());
			
			actual=ep.parseExpression("function(a,b++*-5,-c(:,5))");
			assertEquals("function", actual.pop());
			assertEquals("(", actual.pop());
			assertEquals(",", actual.pop());
			assertEquals("-", actual.pop());
			assertEquals("c", actual.pop());
			assertEquals("(", actual.pop());
			assertEquals(",", actual.pop());
			assertEquals("5", actual.pop());
			assertEquals(":", actual.pop());
			assertEquals("", actual.pop());
			assertEquals(",", actual.pop());
			assertEquals("*", actual.pop());
			assertEquals("-", actual.pop());
			assertEquals("5", actual.pop());
			assertEquals("", actual.pop());
			assertEquals("++", actual.pop());
			assertEquals("", actual.pop());
			assertEquals("b", actual.pop());
			assertEquals("a", actual.pop());
			assertTrue(actual.empty());
			
			
			
		} catch (UnbalancedBracesError e) {
			fail("UnbalancedBracesError raised while it shouldn't");
		}
		
		boolean exceptionThrown = false;
		ep.addBraces("[", "]");
		
		try {
			ep.parseExpression("test([a)])");
		} catch (UnbalancedBracesError e) {
			exceptionThrown = true;
		} 
		assertTrue(exceptionThrown);
		
		
		exceptionThrown = false;
		try {
			ep.parseExpression("(test([a])");
		} catch (UnbalancedBracesError e) {
			exceptionThrown = true;
		} 
		assertTrue(exceptionThrown);
	}

	@Test
	public void testGetParametersList() {
		try {
			List<String> actualList = ExpressionParser.getParametersList("integrate_function( distance,force(:) , xmin,xmax, \"spline\")");
			assertEquals(6, actualList.size());
			assertEquals("integrate_function", actualList.get(0));
			assertEquals("distance", actualList.get(1));
			assertEquals("force(:)", actualList.get(2));
			assertEquals("xmin", actualList.get(3));
			assertEquals("xmax", actualList.get(4));
			assertEquals("\"spline\"", actualList.get(5));
			
			actualList = ExpressionParser.getParametersList("integrate_function");
			assertEquals(1, actualList.size());
			assertEquals("integrate_function", actualList.get(0));
			
			actualList = ExpressionParser.getParametersList("integrate_function( )");
			assertEquals(1, actualList.size());
			assertEquals("integrate_function", actualList.get(0));
			
		} catch (UnbalancedBracesError e) {
			fail("UnbalancedBracesError raised while it shouldn't");
		}
		
	}

	@Test
	public void testGetInterpretedIndex() {
		boolean exceptionThrown = false;
		
		try {
			assertEquals(16, ExpressionParser.getInterpretedIndex("()(())([])[()()])", ")"));
		} catch (UnbalancedBracesError e) {
			fail("UnbalancedBracesError raised while it shouldn't");
		} 
		
		try {
			ExpressionParser.getInterpretedIndex("()(())([])[()()]),", ",");
		} catch (UnbalancedBracesError e) {
			exceptionThrown = true;
		} 
		
		assertTrue(exceptionThrown);
		exceptionThrown = false;
		
		try {
			ExpressionParser.getInterpretedIndex("()(())([])[(()]),", ",");
		} catch (UnbalancedBracesError e) {
			exceptionThrown = true;
		} 
		
		assertTrue(exceptionThrown);
		
		exceptionThrown = false;
		
		try {
			ExpressionParser.getInterpretedIndex("()(())([)[()]),", ",");
		} catch (UnbalancedBracesError e) {
			exceptionThrown = true;
		} 
		
		assertTrue(exceptionThrown);

		try {
			assertEquals(16, ExpressionParser.getInterpretedIndex("()(())([')[()']),", ","));
		} catch (UnbalancedBracesError e) {
			fail("UnbalancedBracesError raised while it shouldn't");
		} 
		
		try {
			assertEquals(17, ExpressionParser.getInterpretedIndex("()(())([')[()']),", ";"));
		} catch (UnbalancedBracesError e) {
			fail("UnbalancedBracesError raised while it shouldn't");
		} 

		try {
			assertEquals(21, ExpressionParser.getInterpretedIndex("()(())([\"')\\\"[()'\"]),", ";"));
		} catch (UnbalancedBracesError e) {
			fail("UnbalancedBracesError raised while it shouldn't");
		} 
		
		try {
			assertEquals(8, ExpressionParser.getInterpretedIndex(";\\\\\\\\\\\"\",", ","));
		} catch (UnbalancedBracesError e) {
			fail("UnbalancedBracesError raised while it shouldn't");
		} 
	}
	
	@Test
	public void testGetInterpretedIndexReverse() {
		boolean exceptionThrown = false;
		
		try {
			assertEquals(2, ExpressionParser.getInterpretedIndexReverse("()((())([])[()()]", "("));
		} catch (UnbalancedBracesError e) {
			fail("UnbalancedBracesError raised while it shouldn't");
		} 
		
		try {
			ExpressionParser.getInterpretedIndexReverse(",(()(())([])[()()]", ",");
		} catch (UnbalancedBracesError e) {
			exceptionThrown = true;
		} 
		
		assertTrue(exceptionThrown);
		exceptionThrown = false;
		
		try {
			ExpressionParser.getInterpretedIndexReverse(",()(())([])[(()])", ",");
		} catch (UnbalancedBracesError e) {
			exceptionThrown = true;
		} 
		
		assertTrue(exceptionThrown);
		
		exceptionThrown = false;
		
		try {
			ExpressionParser.getInterpretedIndexReverse("()(()),([)[()])", ",");
		} catch (UnbalancedBracesError e) {
			exceptionThrown = true;
		} 
		
		assertTrue(exceptionThrown);

		try {
			assertEquals(6, ExpressionParser.getInterpretedIndexReverse("()(()),([')[()'])", ","));
		} catch (UnbalancedBracesError e) {
			fail("UnbalancedBracesError raised while it shouldn't");
		} 
		
		try {
			assertEquals(-1, ExpressionParser.getInterpretedIndexReverse("()(())([')[()']),", ";"));
		} catch (UnbalancedBracesError e) {
			fail("UnbalancedBracesError raised while it shouldn't");
		} 
		
		try {
			assertEquals(-1, ExpressionParser.getInterpretedIndexReverse("\"()((\\\"))([')[()'])\",", ";"));
		} catch (UnbalancedBracesError e) {
			fail("UnbalancedBracesError raised while it shouldn't");
		} 
		
		try {
			assertEquals(0, ExpressionParser.getInterpretedIndexReverse(";\"\\\\\\\\\\\"\",", ";"));
		} catch (UnbalancedBracesError e) {
			fail("UnbalancedBracesError raised while it shouldn't");
		} 
		
	}
	
	


	@Test
	public void testGetVariableList() {
		try {
			List<String> actualList = ExpressionParser.getVariableList("mass, acceleration(1,2,3),c,  ,velocity , energy", true);
			
			assertEquals(5, actualList.size());
			assertEquals("mass", actualList.get(0));
			assertEquals("acceleration", actualList.get(1));
			assertEquals("c", actualList.get(2));
			assertEquals("velocity", actualList.get(3));
			assertEquals("energy", actualList.get(4));

			actualList = ExpressionParser.getVariableList("mass, acceleration(1,2,3) ,c  ,velocity , energy", false);
			
			assertEquals(5, actualList.size());
			assertEquals("mass", actualList.get(0));
			assertEquals("acceleration(1,2,3)", actualList.get(1));
			assertEquals("c", actualList.get(2));
			assertEquals("velocity", actualList.get(3));
			assertEquals("energy", actualList.get(4));
			
			
		} catch (UnbalancedBracesError e) {
			fail("UnbalancedBracesError raised while it shouldn't");
		}
		
	}

}
