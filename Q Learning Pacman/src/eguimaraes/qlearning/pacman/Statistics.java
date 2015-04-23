package eguimaraes.qlearning.pacman;

import eguimaraes.qlearning.pacman.GamePlayState.GameDifficulty;
import eguimaraes.qlearning.pacman.GamePlayState.GameMode;

public class Statistics {
	
	private GameMode mode;
	private int score;
	private int level;
	private int triesCounter;
	
	public GameMode getMode() {
		return mode;
	}
	public void setMode(GameMode mode) {
		this.mode = mode;
	}
	public int getScore() {
		return score;
	}
	public void setScore(int score) {
		this.score = score;
	}
	public int getLevel() {
		return level;
	}
	public void setLevel(int level) {
		this.level = level;
	}
	public int getTriesCounter() {
		return triesCounter;
	}
	public void setTriesCounter(int triesCounter) {
		this.triesCounter = triesCounter;
	}
	
	@Override
	public String toString() {
		int mode;
		switch(this.mode){
			case HUMAN: mode = 0;
			break;
			case RANDOM: mode = 1;
			break;
			default: mode = 2;
		}
		return mode+" "+triesCounter+" "+level+" "+score;
	}
}
