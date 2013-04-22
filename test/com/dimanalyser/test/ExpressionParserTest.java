package com.dimanalyser.test;

import static org.junit.Assert.*;

import java.util.List;


import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import com.dimanalyser.errors.UnbalancedBracesError;
import com.dimanalyser.interpreter.ExpressionParser;

public class ExpressionParserTest {

	private ExpressionParser ep;
	
	@Before
	public void setUp() throws Exception {
		ep = new ExpressionParser();
		ep.addBraces("(", ")");
		ep.addBraces("(/", "/)");
		ep.addBraces("[", "]");
		ep.addQuotes('"', '\\');
		ep.addQuotes('\'', '\\');
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
		
		ep.addUnaryOperator("-");
		ep.addUnaryOperator("++");
		
		
//		List<String> actual = ep.splitExpression("forces = (/ 1.0, -3.0, (mass1+mass2-mass3)*distance/time**2, -force2+force3 /)".replace(" ",""));
//		
//		for (String elem : actual) {
//			System.out.println("["+elem+"]");
//		}
		//  calculate_force(\"coriolis\",2), calculate_force(), calculate_force(2.0)
		try {
			List<String> actual = ep.parseExpression("forces = (/ 1.0, -3.0, (mass1+mass2-mass3)*distance/time**2, -force2+force3 /)".replace(" ",""));
			assertEquals("=", actual.get(26));
			assertEquals("(/", actual.get(25));
			assertEquals(",", actual.get(24));
			assertEquals("+", actual.get(23));
			assertEquals("force3", actual.get(22));
			assertEquals("-", actual.get(21));
			assertEquals("force2", actual.get(20));
			assertEquals("", actual.get(19));
			assertEquals(",", actual.get(18));
			assertEquals("/", actual.get(17));
			assertEquals("**", actual.get(16));
			assertEquals("2", actual.get(15));
			assertEquals("time", actual.get(14));
			assertEquals("*", actual.get(13));
			assertEquals("distance", actual.get(12));
			assertEquals("(", actual.get(11));
			assertEquals("-", actual.get(10));
			assertEquals("mass3", actual.get(9));
			assertEquals("+", actual.get(8));
			assertEquals("mass2", actual.get(7));
			assertEquals("mass1", actual.get(6));
			assertEquals(",", actual.get(5));
			assertEquals("-", actual.get(4));
			assertEquals("3.0", actual.get(3));
			assertEquals("", actual.get(2));
			assertEquals("1.0", actual.get(1));
			assertEquals("forces", actual.get(0));
			assertEquals(27, actual.size());
			
			actual = ep.parseExpression("(val.lt.1e-5.and.val.gt.-tol)");
			assertEquals("(", actual.get(9));
			assertEquals(".and.", actual.get(8));
			assertEquals(".gt.", actual.get(7));
			assertEquals("-", actual.get(6));
			assertEquals("tol", actual.get(5));
			assertEquals("", actual.get(4));
			assertEquals("val", actual.get(3));
			assertEquals(".lt.", actual.get(2));
			assertEquals("1e-5", actual.get(1));
			assertEquals("val", actual.get(0));
			assertEquals(10, actual.size());
			
			
			actual = ep.parseExpression("(\"\",\"\\\"\",\"\")");
			assertEquals("(", actual.get(8));
			assertEquals(",", actual.get(7));
			assertEquals("\"", actual.get(6));
			assertEquals("", actual.get(5));
			assertEquals(",", actual.get(4));
			assertEquals("\"", actual.get(3));
			assertEquals("\\\"", actual.get(2));
			assertEquals("\"", actual.get(1));
			assertEquals("", actual.get(0));
			assertEquals(9, actual.size());
			
			actual=ep.parseExpression("function()");
			assertEquals("function", actual.get(2));
			assertEquals("(", actual.get(1));
			assertEquals("", actual.get(0));
			assertEquals(3, actual.size());
			
			actual=ep.parseExpression("function(a,b++*-5,-c(:,5))");
			assertEquals("function", actual.get(18));
			assertEquals("(", actual.get(17));
			assertEquals(",", actual.get(16));
			assertEquals("-", actual.get(15));
			assertEquals("c", actual.get(14));
			assertEquals("(", actual.get(13));
			assertEquals(",", actual.get(12));
			assertEquals("5", actual.get(11));
			assertEquals(":", actual.get(10));
			assertEquals("", actual.get(9));
			assertEquals(",", actual.get(8));
			assertEquals("*", actual.get(7));
			assertEquals("-", actual.get(6));
			assertEquals("5", actual.get(5));
			assertEquals("", actual.get(4));
			assertEquals("++", actual.get(3));
			assertEquals("", actual.get(2));
			assertEquals("b", actual.get(1));
			assertEquals("a", actual.get(0));
			assertEquals(19, actual.size());
			
			actual=ep.parseExpression("1e-20,5.0e5,483.E-3,e-5,force-4.0e+5");
			assertEquals(",", actual.get(12));
			assertEquals("-", actual.get(11));
			assertEquals("4.0e+5", actual.get(10));
			assertEquals("force", actual.get(9));
			assertEquals(",", actual.get(8));
			assertEquals("-", actual.get(7));
			assertEquals("5", actual.get(6));
			assertEquals("e", actual.get(5));
			assertEquals(",", actual.get(4));
			assertEquals("483.E-3", actual.get(3));
			assertEquals(",", actual.get(2));
			assertEquals("5.0e5", actual.get(1));
			assertEquals("1e-20", actual.get(0));
			assertEquals(13, actual.size());
			
			
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
			List<String> actualList = ep.getParametersList("integrate_function( distance,force(:) , xmin,xmax, \"spline\")");
			System.out.println(actualList.toString());
			assertEquals(6, actualList.size());
			assertEquals("integrate_function", actualList.get(0));
			assertEquals("distance", actualList.get(1));
			assertEquals("force(:)", actualList.get(2));
			assertEquals("xmin", actualList.get(3));
			assertEquals("xmax", actualList.get(4));
			assertEquals("\"spline\"", actualList.get(5));
			
			actualList = ep.getParametersList("integrate_function");
			assertEquals(1, actualList.size());
			assertEquals("integrate_function", actualList.get(0));
			
			actualList = ep.getParametersList("integrate_function( )");
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
			assertEquals(16, ep.getInterpretedIndex("()(())([])[()()])", ")"));
		} catch (UnbalancedBracesError e) {
			fail("UnbalancedBracesError raised while it shouldn't");
		} 
		
		try {
			ep.getInterpretedIndex("()(())([])[()()]),", ",");
		} catch (UnbalancedBracesError e) {
			exceptionThrown = true;
		} 
		
		assertTrue(exceptionThrown);
		exceptionThrown = false;
		
		try {
			ep.getInterpretedIndex("()(())([])[(()]),", ",");
		} catch (UnbalancedBracesError e) {
			exceptionThrown = true;
		} 
		
		assertTrue(exceptionThrown);
		
		exceptionThrown = false;
		
		try {
			ep.getInterpretedIndex("()(())([)[()]),", ",");
		} catch (UnbalancedBracesError e) {
			exceptionThrown = true;
		} 
		
		assertTrue(exceptionThrown);

		try {
			assertEquals(16, ep.getInterpretedIndex("()(())([')[()']),", ","));
		} catch (UnbalancedBracesError e) {
			fail("UnbalancedBracesError raised while it shouldn't");
		} 
		
		try {
			assertEquals(17, ep.getInterpretedIndex("()(())([')[()']),", ";"));
		} catch (UnbalancedBracesError e) {
			fail("UnbalancedBracesError raised while it shouldn't");
		} 

		try {
			assertEquals(21, ep.getInterpretedIndex("()(())([\"')\\\"[()'\"]),", ";"));
		} catch (UnbalancedBracesError e) {
			fail("UnbalancedBracesError raised while it shouldn't");
		} 
		
		try {
			assertEquals(8, ep.getInterpretedIndex(";\\\\\\\\\\\"\",", ","));
		} catch (UnbalancedBracesError e) {
			fail("UnbalancedBracesError raised while it shouldn't");
		} 
	}
	
	@Test
	public void testGetInterpretedIndexReverse() {
		boolean exceptionThrown = false;
		
		try {
			assertEquals(2, ep.getInterpretedIndexReverse("()((())([])[()()]", "("));
		} catch (UnbalancedBracesError e) {
			fail("UnbalancedBracesError raised while it shouldn't");
		} 
		
		try {
			ep.getInterpretedIndexReverse(",(()(())([])[()()]", ",");
		} catch (UnbalancedBracesError e) {
			exceptionThrown = true;
		} 
		
		assertTrue(exceptionThrown);
		exceptionThrown = false;
		
		try {
			ep.getInterpretedIndexReverse(",()(())([])[(()])", ",");
		} catch (UnbalancedBracesError e) {
			exceptionThrown = true;
		} 
		
		assertTrue(exceptionThrown);
		
		exceptionThrown = false;
		
		try {
			ep.getInterpretedIndexReverse("()(()),([)[()])", ",");
		} catch (UnbalancedBracesError e) {
			exceptionThrown = true;
		} 
		
		assertTrue(exceptionThrown);

		try {
			assertEquals(6, ep.getInterpretedIndexReverse("()(()),([')[()'])", ","));
		} catch (UnbalancedBracesError e) {
			fail("UnbalancedBracesError raised while it shouldn't");
		} 
		
		try {
			assertEquals(-1, ep.getInterpretedIndexReverse("()(())([')[()']),", ";"));
		} catch (UnbalancedBracesError e) {
			fail("UnbalancedBracesError raised while it shouldn't");
		} 
		
		try {
			assertEquals(-1, ep.getInterpretedIndexReverse("\"()((\\\"))([')[()'])\",", ";"));
		} catch (UnbalancedBracesError e) {
			fail("UnbalancedBracesError raised while it shouldn't");
		} 
		
		try {
			assertEquals(0, ep.getInterpretedIndexReverse(";\"\\\\\\\\\\\"\",", ";"));
		} catch (UnbalancedBracesError e) {
			fail("UnbalancedBracesError raised while it shouldn't");
		} 
		
	}
	
	


	@Test
	public void testGetVariableList() {
		try {
			List<String> actualList = ep.getVariableList("mass, acceleration(1,2,3),c,  ,velocity , energy", true);
			
			assertEquals(5, actualList.size());
			assertEquals("mass", actualList.get(0));
			assertEquals("acceleration", actualList.get(1));
			assertEquals("c", actualList.get(2));
			assertEquals("velocity", actualList.get(3));
			assertEquals("energy", actualList.get(4));

			actualList = ep.getVariableList("mass, acceleration(1,2,3) ,c  ,velocity , energy", false);
			
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
