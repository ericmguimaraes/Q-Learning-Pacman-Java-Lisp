package eguimaraes.qlearning.pacman;

import org.armedbear.lisp.Function;
import org.armedbear.lisp.Interpreter;
import org.armedbear.lisp.Packages;
import org.armedbear.lisp.Symbol;
//provide a stable connection to lisp
public class LispConnection {
	
	private static LispConnection instance;
	
	private Interpreter interpreter;
	private org.armedbear.lisp.Package lispPackage;
	
	private LispConnection() {
		interpreter = Interpreter.createInstance();
		interpreter.eval("(load \"lispfunctions.lisp\")");
		lispPackage = Packages.findPackage("CL-USER");
		interpreter.eval("( require 'java-collections )");
	}
	
	public static LispConnection getInstance(){
		if(instance==null){
			instance = new LispConnection();
		}
		return instance;
	}

	public Interpreter getInterpreter() {
		return interpreter;
	}

	public org.armedbear.lisp.Package getLispPackage() {
		return lispPackage;
	}
	
	public Function getFunction(String name){
		return (Function) getLispPackage().findAccessibleSymbol(name).getSymbolFunction();
	}

}
