package eguimaraes.qlearning.pacman;

public class FeaturesExtraction {
		
		private static FeaturesExtraction instance;
		
		private GamePlayState state;
		
		private FeaturesExtraction(GamePlayState state){
			this.state = state;
		}
		
		public FeaturesExtraction getInstance(GamePlayState state){
			if(instance==null) 
				instance = new FeaturesExtraction(state);
			return instance;
		}
		
		private int getClosestFoodDistance(){
			int result = 0;
			
			return result;
		}
		
		private int getNumGhost1stepAway(){
			int result = 0;
			
			return result;
		}
		
		private int getEatDot(){
			int result = 0;
			
			return result;
		}
		
		private int getEatPowerDot(){
			int result = 0;
			
			return result;
		}
		
		private int getEatGhost(){
			int result = 0;
			
			return result;
		}
		
		private int getClosestGhostToEatDistance(){
			int result = 0;
			
			return result;
		}
		
		public int[] getFeatures(){
			int[] result = new int[6];
			result[0]=getClosestFoodDistance();
			result[1]=getNumGhost1stepAway();
			result[2]=getEatDot();
			result[3]=getEatPowerDot();
			result[4]=getEatGhost();
			result[5]=getClosestGhostToEatDistance();
			return result;
		}
		
}
