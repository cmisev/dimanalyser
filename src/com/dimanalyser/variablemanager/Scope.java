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