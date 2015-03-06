package eguimaraes.qlearning.pacman;

import org.armedbear.lisp.JavaObject;
import org.armedbear.lisp.LispObject;

public class LispFunction {

	private static LispFunction instance;

	private LispConnection connection;

	private LispFunction() {
		connection = LispConnection.getInstance();
		connection.getFunction("INIT").execute(new JavaObject(this));
	}

	public static LispFunction getInstance() {
		if (instance == null) {
			instance = new LispFunction();
		}
		return instance;
	}

	public int requestRandomMove(Pacman pac) {
		LispObject result = connection.getFunction("RAN").execute(
				new JavaObject(pac.realDir));
		int n = result.intValue();
		return n;
	}
	
	public String intArrayToString(int[] a){
		String str="";
		for (int i = 0; i < a.length; i++) {
			str = str+Integer.toString(a[i])+" ";
		}
		str = str.trim();
		return str;
	}

	public void test(String str) {
		System.out.println(str);
	}
	
	public void calltest() {
		connection.getFunction("test").execute();
	}
}
