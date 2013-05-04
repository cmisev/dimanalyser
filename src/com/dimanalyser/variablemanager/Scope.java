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

package com.dimanalyser.variablemanager;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.dimanalyser.errors.InstanceExistsError;
import com.dimanalyser.errors.InstanceNotFoundError;

/**
 * Class defining a validity scope of a group of {@link Instance Instances}. A scope may inherit several other scopes.
 * 
 * instances/inherited scopes having a access/inheritance level (see {@link InheritanceLevel InheritanceLevel}) of 
 * 
 * <ul>
 * 		<li>"public" may be accessed from any other scope</li>
 * 		<li>"protected" may be accessed only from other scopes inheriting the scope containing the instance/inheritance</li>
 * 		<li>"private" may only be accessed by the scope containing the instance/inheritance</li>
 * </ul>
 * 
 * scopes may contain an instance of a given name even if inheritances contain accessible instances of the same name. An access to
 * the instance of the given name yields the closest instance in the inheritance tree.
 * 
 * @author Cyril Misev <c.misev@gmail.com>
 *
 */
public class Scope {

	/**
	 * the name of the scope
	 */
	private String mName;
	
	/**
	 * a list of inheritances the scope is inheriting from
	 */
	private List<Inheritance> mInheritances;
	
	/**
	 * a list of instances the scope is defining
	 */
	private Map<String,Instance> mInstances;
	
	/**
	 * Constructor
	 * 
	 * @param name the name of the scope
	 */
	public Scope(String name) {
		mName = name;
		mInheritances = new ArrayList<Inheritance>();
		mInstances = new HashMap<String, Instance>();
	}

	/**
	 * Add an inheritance to the scope
	 * 
	 * @param inheritance an inheritance object defining the inheritance level and the inherited scope
	 */
	public void addInheritance(Inheritance inheritance) {
		mInheritances.add(inheritance);
	}

	/**
	 * Add an instance to the current scope
	 * 
	 * @param instance the instance to add to the scope
	 * @throws InstanceExistsError
	 */
	public void addInstance(Instance instance) throws InstanceExistsError {
		if (mInstances.containsKey(instance.getName())) {
			throw new InstanceExistsError(instance.getName());
		} else {
			mInstances.put(instance.getName(), instance);
		}
	}

	/**
	 * Get the name of the scope
	 * 
	 * @return the name of the scope
	 */
	public String getName() {
		return mName;
	}

	/**
	 * Get the instance identified with a given name walk down the inheritance tree if the current scope doesn't contain the
	 * instance of the given name. Only return the instance if it has a more open access level than indicated.
	 * 
	 * @param name the name of the instance to get
	 * @param accessLevel the maximum access level
	 * @return the instance if found and accessible
	 * @throws InstanceNotFoundError
	 */
	public Instance getInstance(String name, int accessLevel) throws InstanceNotFoundError {
		if (mInstances.containsKey(name) && mInstances.get(name).getAccessLevel()<=accessLevel ) {
			return mInstances.get(name);
		}
		
		for(Inheritance inh : mInheritances) {
			try {
				return inh.getScope().getInstance(name, Math.max(accessLevel,inh.getInheritanceLevel()));
			} catch(InstanceNotFoundError nf) {
				
			}
		}
		
		throw new InstanceNotFoundError(name);
	}
	
	/**
	 * Get the instance identified with a given name walk down the inheritance tree if the current scope doesn't contain the
	 * instance of the given name.
	 * 
	 * @param name the name of the instance to get
	 * @return the instance if found and accessible
	 * @throws InstanceNotFoundError
	 */
	public Instance getInstance(String name) throws InstanceNotFoundError {
		return getInstance(name, InheritanceLevel.SCOPE_PUBLIC);
	}
	
	public Collection<Instance> getInstances() {
		return mInstances.values();
	}


}