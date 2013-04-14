package com.dimanalyser.test;

import static org.junit.Assert.*;

import java.util.List;


import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import com.dimanalyser.errors.UnbalancedBracesError;
import com.dimanalyser.interpreter.ParserHelpers;

public class ParserHelpersTest {

	@Before
	public void setUp() throws Exception {
	}

	@After
	public void tearDown() throws Exception {
	}

	@Test
	public void testGetParametersList() {
		try {
			List<String> actualList = ParserHelpers.getParametersList("integrate_function( distance,force(:) , xmin,xmax, \"spline\")");
			assertEquals(6, actualList.size());
			assertEquals("integrate_function", actualList.get(0));
			assertEquals("distance", actualList.get(1));
			assertEquals("force(:)", actualList.get(2));
			assertEquals("xmin", actualList.get(3));
			assertEquals("xmax", actualList.get(4));
			assertEquals("\"spline\"", actualList.get(5));
			
			actualList = ParserHelpers.getParametersList("integrate_function");
			assertEquals(1, actualList.size());
			assertEquals("integrate_function", actualList.get(0));
			
			actualList = ParserHelpers.getParametersList("integrate_function( )");
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
			assertEquals(16, ParserHelpers.getInterpretedIndex("()(())([])[()()])", ")"));
		} catch (UnbalancedBracesError e) {
			fail("UnbalancedBracesError raised while it shouldn't");
		} 
		
		try {
			ParserHelpers.getInterpretedIndex("()(())([])[()()]),", ",");
		} catch (UnbalancedBracesError e) {
			exceptionThrown = true;
		} 
		
		assertTrue(exceptionThrown);
		exceptionThrown = false;
		
		try {
			ParserHelpers.getInterpretedIndex("()(())([])[(()]),", ",");
		} catch (UnbalancedBracesError e) {
			exceptionThrown = true;
		} 
		
		assertTrue(exceptionThrown);
		
		exceptionThrown = false;
		
		try {
			ParserHelpers.getInterpretedIndex("()(())([)[()]),", ",");
		} catch (UnbalancedBracesError e) {
			exceptionThrown = true;
		} 
		
		assertTrue(exceptionThrown);

		try {
			assertEquals(16, ParserHelpers.getInterpretedIndex("()(())([')[()']),", ","));
		} catch (UnbalancedBracesError e) {
			fail("UnbalancedBracesError raised while it shouldn't");
		} 
		
		try {
			assertEquals(17, ParserHelpers.getInterpretedIndex("()(())([')[()']),", ";"));
		} catch (UnbalancedBracesError e) {
			fail("UnbalancedBracesError raised while it shouldn't");
		} 

		try {
			assertEquals(21, ParserHelpers.getInterpretedIndex("()(())([\"')\\\"[()'\"]),", ";"));
		} catch (UnbalancedBracesError e) {
			fail("UnbalancedBracesError raised while it shouldn't");
		} 
	}
	
	@Test
	public void testGetInterpretedIndexReverse() {
		boolean exceptionThrown = false;
		
		try {
			assertEquals(2, ParserHelpers.getInterpretedIndexReverse("()((())([])[()()]", "("));
		} catch (UnbalancedBracesError e) {
			fail("UnbalancedBracesError raised while it shouldn't");
		} 
		
		try {
			ParserHelpers.getInterpretedIndexReverse(",(()(())([])[()()]", ",");
		} catch (UnbalancedBracesError e) {
			exceptionThrown = true;
		} 
		
		assertTrue(exceptionThrown);
		exceptionThrown = false;
		
		try {
			ParserHelpers.getInterpretedIndexReverse(",()(())([])[(()])", ",");
		} catch (UnbalancedBracesError e) {
			exceptionThrown = true;
		} 
		
		assertTrue(exceptionThrown);
		
		exceptionThrown = false;
		
		try {
			ParserHelpers.getInterpretedIndexReverse("()(()),([)[()])", ",");
		} catch (UnbalancedBracesError e) {
			exceptionThrown = true;
		} 
		
		assertTrue(exceptionThrown);

		try {
			assertEquals(6, ParserHelpers.getInterpretedIndexReverse("()(()),([')[()'])", ","));
		} catch (UnbalancedBracesError e) {
			fail("UnbalancedBracesError raised while it shouldn't");
		} 
		
		try {
			assertEquals(-1, ParserHelpers.getInterpretedIndexReverse("()(())([')[()']),", ";"));
		} catch (UnbalancedBracesError e) {
			fail("UnbalancedBracesError raised while it shouldn't");
		} 
		
		try {
			assertEquals(-1, ParserHelpers.getInterpretedIndexReverse("\"()((\\\"))([')[()'])\",", ";"));
		} catch (UnbalancedBracesError e) {
			fail("UnbalancedBracesError raised while it shouldn't");
		} 
		
	}
	
	


	@Test
	public void testGetVariableList() {
		try {
			List<String> actualList = ParserHelpers.getVariableList("mass, acceleration(1,2,3),c,  ,velocity , energy", true);
			
			assertEquals(5, actualList.size());
			assertEquals("mass", actualList.get(0));
			assertEquals("acceleration", actualList.get(1));
			assertEquals("c", actualList.get(2));
			assertEquals("velocity", actualList.get(3));
			assertEquals("energy", actualList.get(4));

			actualList = ParserHelpers.getVariableList("mass, acceleration(1,2,3) ,c  ,velocity , energy", false);
			
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
