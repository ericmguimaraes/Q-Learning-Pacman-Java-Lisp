package eguimaraes.qlearning.pacman;

public class PacmanFeatures extends Features {
	
	private int closestFoodDistance;
	private int numGhost1stepAway;
	private int eatDot;
	private int eatPowerDot;
	private int eatGhost;
	private int closestGhostToEatDistance;
	
	public int getClosestFoodDistance() {
		return closestFoodDistance;
	}
	public void setClosestFoodDistance(int closestFoodDistance) {
		this.closestFoodDistance = closestFoodDistance;
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
	public int getClosestGhostToEatDistance() {
		return closestGhostToEatDistance;
	}
	public void setClosestGhostToEatDistance(int closestGhostToEatDistance) {
		this.closestGhostToEatDistance = closestGhostToEatDistance;
	}

	@Override
	public String toString() {
//		return "closestFoodDistance: "+ closestFoodDistance+" "+
//				"numGhost1stepAway: "+ numGhost1stepAway+" "+
//				"eatDot: "+ eatDot+" "+
//				"eatPowerDot: "+ eatPowerDot+" "+
//				"eatGhost: "+ eatGhost+" "+
//				"closestGhostToEatDistance: "+ closestGhostToEatDistance;
		
		
		return closestFoodDistance+" "+
		numGhost1stepAway+" "+
		eatDot+" "+
		eatPowerDot+" "+
		eatGhost+" "+
		closestGhostToEatDistance;
	}
}
