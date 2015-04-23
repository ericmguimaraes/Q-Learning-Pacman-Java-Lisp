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
		LispObject result = connection.getFunction("get-random-action").execute();
		int n = result.intValue();
		return n;
	}
	
	public void test(String str) {
		System.out.println(connection.stringToArray(str).toString());
	}
	 
	public void calltest() {
		connection.getFunction("test").execute();
	}
	
	public int requestQLearningMove() {
		LispObject result = connection.getFunction("get-action").execute();
		return result.intValue();
	}
	
	public void update(Features lastState, int lastAction, Features stateResult, int reward) {
		connection.getFunction("update").execute(new JavaObject(lastState.toString1000()), new JavaObject(lastAction),
				new JavaObject(stateResult.toString1000()), new JavaObject(reward));
	}
	
	public String getFeatures(int action){
		return state.featuresExtractor.getFeatures(state.pac.iX, state.pac.iY, action).toString();
	}
	
	public String getFeatures(){
		return state.featuresExtractor.getFeatures(state.pac.iX, state.pac.iY).toString();
	}
	
	public int getLastDir(){
		return state.pac.realDir;
	}
	
	public String getActions(){
		return state.pac.getValidActions();
	}
	
	public void saveData(String str){
		connection.getFunction("save-data").execute(new JavaObject(str));
	}
	
}
