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
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.dimanalyser.common.Globals;
import com.dimanalyser.errors.InstanceExistsError;
import com.dimanalyser.errors.InstanceNotFoundError;

public class Scope {

	private String mName;
	private List<Inheritance> mInheritances;
	private Map<String,Instance> mInstances;
	
	
	public Scope(String name) {
		mName = name;
		mInheritances = new ArrayList<Inheritance>();
		mInstances = new HashMap<String, Instance>();
	}

	public void addInheritance(Inheritance inheritance) {
		mInheritances.add(inheritance);
	}

	public void addInstance(Instance instance) throws InstanceExistsError {
		if (mInstances.containsKey(instance.getName())) {
			throw new InstanceExistsError(instance.getName());
		} else {
			mInstances.put(instance.getName(), instance);
		}
	}



	public String getName() {
		return mName;
	}

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
	
	public Instance getInstance(String name) throws InstanceNotFoundError {
		return getInstance(name, InheritanceLevel.SCOPE_PUBLIC);
	}


}