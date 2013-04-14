package com.dimanalyser.test;

import static org.junit.Assert.*;

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
		fail("Not yet implemented");
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
		fail("Not yet implemented");
	}

}
