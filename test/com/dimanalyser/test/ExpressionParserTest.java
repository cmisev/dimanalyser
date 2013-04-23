package com.dimanalyser.test;

import static org.junit.Assert.*;

import java.util.List;


import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import com.dimanalyser.errors.UnbalancedBracesError;
import com.dimanalyser.interpreter.ExpressionParser;
import com.dimanalyser.interpreter.StackElement;

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
		ep.addBinaryOperatorInHierarchy(new String[]{
				"="
		});
		ep.addListSeparatorInHierarchy(new String[]{
				","
		});
		ep.addBinaryOperatorInHierarchy(new String[]{
				".and.",".or."
		});
		ep.addBinaryOperatorInHierarchy(new String[]{
				".eq.",".ne."
		});
		ep.addBinaryOperatorInHierarchy(new String[]{
				".lt.",".gt.",".ge.",".le."
		});
		ep.addBinaryOperatorInHierarchy(new String[]{
				"+","-"
		});
		ep.addBinaryOperatorInHierarchy(new String[]{
				"*","/"
		});
		ep.addBinaryOperatorInHierarchy(new String[]{
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
		ep.addBinaryOperatorInHierarchy(new String[]{
				"="
		});
		ep.addListSeparatorInHierarchy(new String[]{
				","
		});
		ep.addBinaryOperatorInHierarchy(new String[]{
				".and.",".or."
		});
		ep.addBinaryOperatorInHierarchy(new String[]{
				".eq.",".ne."
		});
		ep.addBinaryOperatorInHierarchy(new String[]{
				".lt.",".gt.",".ge.",".le."
		});
		ep.addBinaryOperatorInHierarchy(new String[]{
				"+","-"
		});
		ep.addBinaryOperatorInHierarchy(new String[]{
				"*","/"
		});
		ep.addBinaryOperatorInHierarchy(new String[]{
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
			List<StackElement> actual = ep.parseExpression("forces = (/ 1.0, -3.0, (mass1+mass2-mass3)*distance/time**2, -force2+force3 /)".replace(" ",""));
			
			assertEquals("=", actual.get(24).toString());
			assertEquals("(/", actual.get(23).toString());
			assertEquals(",", actual.get(22).toString());
			assertEquals(4, actual.get(22).getOperandsCount());
			assertEquals("+", actual.get(21).toString());
			assertEquals("force3", actual.get(20).toString());
			assertEquals("-", actual.get(19).toString());
			assertEquals("force2", actual.get(18).toString());
			assertEquals("", actual.get(17).toString());
			assertEquals("/", actual.get(16).toString());
			assertEquals("**", actual.get(15).toString());
			assertEquals("2", actual.get(14).toString());
			assertEquals("time", actual.get(13).toString());
			assertEquals("*", actual.get(12).toString());
			assertEquals("distance", actual.get(11).toString());
			assertEquals("(", actual.get(10).toString());
			assertEquals("-", actual.get(9).toString());
			assertEquals("mass3", actual.get(8).toString());
			assertEquals("+", actual.get(7).toString());
			assertEquals("mass2", actual.get(6).toString());
			assertEquals("mass1", actual.get(5).toString());
			assertEquals("-", actual.get(4).toString());
			assertEquals("3.0", actual.get(3).toString());
			assertEquals("", actual.get(2).toString());
			assertEquals("1.0", actual.get(1).toString());
			assertEquals("forces", actual.get(0).toString());
			assertEquals(25, actual.size());
			
			actual = ep.parseExpression("(val.lt.1e-5.and.val.gt.-tol)");
			assertEquals("(", actual.get(9).toString());
			assertEquals(".and.", actual.get(8).toString());
			assertEquals(".gt.", actual.get(7).toString());
			assertEquals("-", actual.get(6).toString());
			assertEquals("tol", actual.get(5).toString());
			assertEquals("", actual.get(4).toString());
			assertEquals("val", actual.get(3).toString());
			assertEquals(".lt.", actual.get(2).toString());
			assertEquals("1e-5", actual.get(1).toString());
			assertEquals("val", actual.get(0).toString());
			assertEquals(10, actual.size());
			
			
			actual = ep.parseExpression("(\"\",\"\\\"\",\"\")");
			assertEquals("(", actual.get(7).toString());
			assertEquals(",", actual.get(6).toString());
			assertEquals(3, actual.get(6).getOperandsCount());
			assertEquals("\"", actual.get(5).toString());
			assertEquals("", actual.get(4).toString());
			assertEquals("\"", actual.get(3).toString());
			assertEquals("\\\"", actual.get(2).toString());
			assertEquals("\"", actual.get(1).toString());
			assertEquals("", actual.get(0).toString());
			assertEquals(8, actual.size());
			
			actual=ep.parseExpression("function()");
			assertEquals("function", actual.get(2).toString());
			assertEquals("(", actual.get(1).toString());
			assertEquals("", actual.get(0).toString());
			assertEquals(3, actual.size());
			
			actual=ep.parseExpression("function(a,b++*-5,-c(:,5,6))");
			System.out.println(actual.toString());
			assertEquals("function", actual.get(18).toString());
			assertEquals("(", actual.get(17).toString());
			assertEquals(",", actual.get(16).toString());
			assertEquals(3, actual.get(16).getOperandsCount());
			assertEquals("-", actual.get(15).toString());
			assertEquals("c", actual.get(14).toString());
			assertEquals("(", actual.get(13).toString());
			assertEquals(",", actual.get(12).toString());
			assertEquals(3, actual.get(12).getOperandsCount());
			assertEquals("6", actual.get(11).toString());
			assertEquals("5", actual.get(10).toString());
			assertEquals(":", actual.get(9).toString());
			assertEquals("", actual.get(8).toString());
			assertEquals("*", actual.get(7).toString());
			assertEquals("-", actual.get(6).toString());
			assertEquals("5", actual.get(5).toString());
			assertEquals("", actual.get(4).toString());
			assertEquals("++", actual.get(3).toString());
			assertEquals("", actual.get(2).toString());
			assertEquals("b", actual.get(1).toString());
			assertEquals("a", actual.get(0).toString());
			assertEquals(19, actual.size());
			
			actual=ep.parseExpression("1e-20,5.0e5,483.E-3,e-5,force-4.0e+5");
			assertEquals(",", actual.get(9).toString());
			assertEquals(5, actual.get(9).getOperandsCount());
			assertEquals("-", actual.get(8).toString());
			assertEquals("4.0e+5", actual.get(7).toString());
			assertEquals("force", actual.get(6).toString());
			assertEquals("-", actual.get(5).toString());
			assertEquals("5", actual.get(4).toString());
			assertEquals("e", actual.get(3).toString());
			assertEquals("483.E-3", actual.get(2).toString());
			assertEquals("5.0e5", actual.get(1).toString());
			assertEquals("1e-20", actual.get(0).toString());
			assertEquals(10, actual.size());
			
			
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
