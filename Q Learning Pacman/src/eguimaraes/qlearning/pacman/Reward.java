package eguimaraes.qlearning.pacman;

public class Reward {
	
	private int value;
	
	private RewardType rewardType;
	
	public static enum RewardType {
		DOT, POWER_DOT, EAT_GHOST, DIE, WALK;
	}
	
	public Reward(RewardType type) {
		rewardType = type;
		switch(type){
			case DOT: value = 2;
			break;
			case POWER_DOT: value = 4;
			break;
			case EAT_GHOST: value = 400;
			break;
			case DIE: value = -600;
			break;
			case WALK: value = -1;
			break;
		}
	}
	
	public int getValue() {
		return value;
	}

	public RewardType getRewardType() {
		return rewardType;
	}

	@Override
	public String toString() {
		return Integer.toString(value);
	}
}
