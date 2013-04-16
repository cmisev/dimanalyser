package com.dimanalyser.test;

import static org.junit.Assert.*;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import com.dimanalyser.common.Globals;
import com.dimanalyser.errors.InstanceExistsError;
import com.dimanalyser.errors.InstanceNotFoundError;
import com.dimanalyser.errors.NotInAnyScopeError;
import com.dimanalyser.errors.ScopeExistsError;
import com.dimanalyser.errors.ScopeNotFoundError;
import com.dimanalyser.variablemanager.InheritanceLevel;
import com.dimanalyser.variablemanager.VariableInstance;
import com.dimanalyser.variablemanager.VariableManager;

public class VariableManagerTest {

	private VariableManager vm;
	
	@Before
	public void setUp() throws Exception {
		vm = new VariableManager();
		Globals.initUnits();
	}

	@After
	public void tearDown() throws Exception {
		vm = null;
	}


	@Test
	public void testEnterScope() {
		
		boolean errorThrown = false;
		
		try {
			vm.enterScope("PROGRAM", InheritanceLevel.SCOPE_PRIVATE);
		} catch (ScopeExistsError e) {
			fail("Thrown ScopeExistsError while it shouldn't");
		}
		
		try {
			vm.enterScope("PROGRAM", InheritanceLevel.SCOPE_PRIVATE);
		} catch (ScopeExistsError e) {
			errorThrown = true;
		}
		
		assertTrue(errorThrown);

		
	}

	@Test
	public void testIncludeScope() {
		
		boolean errorThrown = false;
		
		try {
			vm.enterScope("FIELDS", InheritanceLevel.SCOPE_PUBLIC);
			vm.addInstance(new VariableInstance("grad_B",InheritanceLevel.SCOPE_PUBLIC));
			vm.addInstance(new VariableInstance("metric",InheritanceLevel.SCOPE_PRIVATE));
			vm.leaveScope();
			
			vm.enterScope("PROGRAM", InheritanceLevel.SCOPE_PUBLIC);
			vm.includeScope("FIELDS", InheritanceLevel.SCOPE_PUBLIC);
			assertEquals("variable instance grad_B, unit not yet determined",vm.getInstance("grad_B").toString());

			vm.addInstance(new VariableInstance("grad_B",InheritanceLevel.SCOPE_PUBLIC,Globals.units.get("N")));
			assertEquals("variable instance grad_B [m kg s^-2]",vm.getInstance("grad_B").toString());
			

			
			try {
				vm.includeScope("NOTPRESENT", InheritanceLevel.SCOPE_PUBLIC);
			} catch (ScopeNotFoundError e) {
				errorThrown = true;
			}
			assertTrue(errorThrown);
			
			errorThrown = false;
			try {
				vm.getInstance("metric");
			} catch (InstanceNotFoundError e) {
				errorThrown = true;
			}
			assertTrue(errorThrown);
			
			errorThrown = false;
			try {
				vm.addInstance(new VariableInstance("grad_B",InheritanceLevel.SCOPE_PUBLIC,Globals.units.get("N")));
			} catch (InstanceExistsError e) {
				errorThrown = true;
			}
			assertTrue(errorThrown);
			
			vm.leaveScope();
		} catch (ScopeExistsError e) {
			fail("Thrown ScopeExistsError while it shouldn't");
		} catch (NotInAnyScopeError e) {
			fail("Thrown NotInAnyScopeError while it shouldn't");
		} catch (InstanceExistsError e) {
			fail("Thrown InstanceExistsError while it shouldn't");
		} catch (ScopeNotFoundError e) {
			fail("Thrown ScopeNotFoundError while it shouldn't");
		} catch (InstanceNotFoundError e) {
			fail("Thrown InstanceNotFoundError while it shouldn't");
		}
	}

	@Test
	public void testLeaveScope() {

		boolean errorThrown = false;
		
		try {
			vm.enterScope("PROGRAM", InheritanceLevel.SCOPE_PRIVATE);
			vm.leaveScope();
		} catch (ScopeExistsError e) {
			fail("Thrown ScopeExistsError while it shouldn't");
		} catch (NotInAnyScopeError e) {
			fail("Thrown NotInAnyScopeError while it shouldn't");
		}
		
		try {
			vm.leaveScope();
		} catch (NotInAnyScopeError e) {
			errorThrown = true;
		}
		assertTrue(errorThrown);
	}




}
