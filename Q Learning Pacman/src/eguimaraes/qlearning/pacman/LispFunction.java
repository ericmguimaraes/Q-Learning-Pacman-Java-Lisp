package eguimaraes.qlearning.pacman;

import org.armedbear.lisp.JavaObject;
import org.armedbear.lisp.LispObject;

public class LispFunction {

	private static LispFunction instance;

	private LispConnection connection;
	
	private GamePlayState state;

	private LispFunction(GamePlayState state) {
		connection = LispConnection.getInstance();
		connection.getFunction("INIT").execute(new JavaObject(this));
		this.state = state;
	}

	public static LispFunction getInstance(GamePlayState state) {
		if (instance == null) {
			instance = new LispFunction(state);
		}
		return instance;
	}

	public int requestRandomMove(Pacman pac) {
		LispObject result = connection.getFunction("RAN").execute(
				new JavaObject(pac.realDir));
		int n = result.intValue();
		return n;
	}
	
	public String arrayToString(int[] a){
		String str="";
		for (int i = 0; i < a.length; i++) {
			str = str+Integer.toString(a[i])+" ";
		}
		str = str.trim();
		return str;
	}
	
	public int[] stringToArray(String str){
		String[] astr = str.split(",");
		int[] r = new int[astr.length];
		for (int i : r) {
			r[i]=Integer.parseInt(astr[i]);
		}
		return r;
	}

	public void test(String str) {
		System.out.println(stringToArray(str).toString());
	}
	 
	public void calltest() {
		connection.getFunction("test").execute();
	}
}
