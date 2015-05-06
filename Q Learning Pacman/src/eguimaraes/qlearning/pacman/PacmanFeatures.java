package eguimaraes.qlearning.pacman;

public class PacmanFeatures extends Features {

	private float distDot;
	private float numGhost1stepAway;
	//private int eatDot;
	//private int eatPowerDot;
	//private int eatGhost;
	private float distGhostBlind;
	//private float closestGhostToEatDistance;
	private float distGhostActive;
	
	public float getClosestGhostToEatDistanceDividedByOne() {
		return distGhostBlind;
	}

	public void setClosestGhostToEatDistanceDividedByOne(
			float closestGhostToEatDistanceDividedByOne) {
		this.distGhostBlind = closestGhostToEatDistanceDividedByOne;
	}

	private int beEaten;

	public float getDistDot() {
		return distDot;
	}

	public void setDistDot(float f) {
		this.distDot = f;
	}

	public float getNumGhost1stepAway() {
		return numGhost1stepAway;
	}

	public void setNumGhost1stepAway(float numGhost1stepAway) {
		this.numGhost1stepAway = numGhost1stepAway;
	}

//	public int getEatDot() {
//		return eatDot;
//	}
//
//	public void setEatDot(int eatDot) {
//		this.eatDot = eatDot;
//	}
//
//	public int getEatPowerDot() {
//		return eatPowerDot;
//	}
//
//	public void setEatPowerDot(int eatPowerDot) {
//		this.eatPowerDot = eatPowerDot;
//	}
//
//	public int getEatGhost() {
//		return eatGhost;
//	}
//
//	public void setEatGhost(int eatGhost) {
//		this.eatGhost = eatGhost;
//	}

	public float getDistGhostBlind() {
		return this.distGhostBlind;
	}

	public void setDistGhostBlind(float f) {
		this.distGhostBlind = f;
	}

	public int getBeEaten() {
		return beEaten;
	}

	public void setBeEaten(int beEaten) {
		this.beEaten = beEaten;
	}

	@Override
	public String toString() {
		// return "closestFoodDistance: "+ closestFoodDistance+" "+
		// "numGhost1stepAway: "+ numGhost1stepAway+" "+
		// "eatDot: "+ eatDot+" "+
		// "eatPowerDot: "+ eatPowerDot+" "+
		// "eatGhost: "+ eatGhost+" "+
		// "closestGhostToEatDistance: "+ closestGhostToEatDistance;

		//
		// return (int) closestFoodDistance+" "+
		// numGhost1stepAway+" "+
		// eatDot+" "+
		// eatPowerDot+" "+
		// eatGhost+" "+
		// (int) closestGhostToEatDistance+" "+
		// beEaten;
		// (int) (1/(closestGhostToEatDistance^2))*100;
		return toString1000();
	}

	//(setf feat '(1/dotDist dot PowerDot 1/GhostDist eatGhost numGhost beEaten))
	private String toString1000() {
		int n = 100000;
		return (int) (distDot * n) + " " + 
		//eatDot * n + " " +
		//eatPowerDot * n + " " +
		(int) (distGhostBlind * n) + " " +
		//eatGhost * n + " " + 
		(int) (numGhost1stepAway * n) + " " +
		beEaten * n;// + " "+
		//(int) (distGhostActive * n);
				//+ (int) (closestPowerDotDistance * n);
	}

	public float getDistGhostActive() {
		return distGhostActive;
	}

	public void setDistGhostActive(float distGhostActive) {
		this.distGhostActive = distGhostActive;
	}

}
