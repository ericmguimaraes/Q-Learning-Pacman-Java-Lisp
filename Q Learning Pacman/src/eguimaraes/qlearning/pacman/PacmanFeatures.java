package eguimaraes.qlearning.pacman;

public class PacmanFeatures extends Features {
	
	private float closestFoodDistance;
	private int numGhost1stepAway;
	private int eatDot;
	private int eatPowerDot;
	private int eatGhost;
	private float closestGhostToEatDistance;
	private int beEaten;
	
	public float getClosestFoodDistance() {
		return closestFoodDistance;
	}
	public void setClosestFoodDistance(float f) {
		this.closestFoodDistance = f;
	}
	public int getNumGhost1stepAway() {
		return numGhost1stepAway;
	}
	public void setNumGhost1stepAway(int numGhost1stepAway) {
		this.numGhost1stepAway = numGhost1stepAway;
	}
	public int getEatDot() {
		return eatDot;
	}
	public void setEatDot(int eatDot) {
		this.eatDot = eatDot;
	}
	public int getEatPowerDot() {
		return eatPowerDot;
	}
	public void setEatPowerDot(int eatPowerDot) {
		this.eatPowerDot = eatPowerDot;
	}
	public int getEatGhost() {
		return eatGhost;
	}
	public void setEatGhost(int eatGhost) {
		this.eatGhost = eatGhost;
	}
	public float getClosestGhostToEatDistance() {
		return closestGhostToEatDistance;
	}
	public void setClosestGhostToEatDistance(float f) {
		this.closestGhostToEatDistance = f;
	}

	public int getBeEaten() {
		return beEaten;
	}
	public void setBeEaten(int beEaten) {
		this.beEaten = beEaten;
	}
	@Override
	public String toString() {
//		return "closestFoodDistance: "+ closestFoodDistance+" "+
//				"numGhost1stepAway: "+ numGhost1stepAway+" "+
//				"eatDot: "+ eatDot+" "+
//				"eatPowerDot: "+ eatPowerDot+" "+
//				"eatGhost: "+ eatGhost+" "+
//				"closestGhostToEatDistance: "+ closestGhostToEatDistance;
		
		
		return (int) closestFoodDistance+" "+
		numGhost1stepAway+" "+
		eatDot+" "+
		eatPowerDot+" "+
		eatGhost+" "+
		(int) closestGhostToEatDistance+" "+
		beEaten;
		//(int) (1/(closestGhostToEatDistance^2))*100;
	}
	
	
	public String toString1000() {
//		return "closestFoodDistance: "+ closestFoodDistance+" "+
//				"numGhost1stepAway: "+ numGhost1stepAway+" "+
//				"eatDot: "+ eatDot+" "+
//				"eatPowerDot: "+ eatPowerDot+" "+
//				"eatGhost: "+ eatGhost+" "+
//				"closestGhostToEatDistance: "+ closestGhostToEatDistance;
		
		int n = 100000;
		return (int) (closestFoodDistance*n)+" "+
		numGhost1stepAway*n+" "+
		eatDot*n+" "+
		eatPowerDot*n+" "+
		eatGhost*n+" "+
		(int) closestGhostToEatDistance*n+" "+
		beEaten;
		//(int) (1/(closestGhostToEatDistance^2))*100;
	}
}
