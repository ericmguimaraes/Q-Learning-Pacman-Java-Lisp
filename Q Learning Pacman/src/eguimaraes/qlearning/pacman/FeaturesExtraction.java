package eguimaraes.qlearning.pacman;

public class FeaturesExtraction {

	private static FeaturesExtraction instance;

	private GamePlayState game;

	private FeaturesExtraction(GamePlayState game) {
		this.game = game;
	}

	public static FeaturesExtraction getInstance(GamePlayState state) {
		if (instance == null)
			instance = new FeaturesExtraction(state);
		return instance;
	}

	public int getManhatanDistance(int x1, int y1, int x2, int y2){
		return (Math.abs(x1-x2)+Math.abs(y1-y2));
	}
	
	public int[] getNewPosition(int x, int y, int action){
		//I had to invert the order of x and y because the first developer used this pattern
		//which is completely out of the worldly math pattern to coordenates
		//and I just saw it after write everything with the normal pattern 
		int aux = x;
		x=y;
		y=aux;
		
	int[] result = new int[2]; 
		switch(action){
			case 0: 
				if(game.maze.iMaze[x+1][y]!=Maze.WALL && game.maze.iMaze[x+1][y]!=Maze.DOOR){
					result[0] = x+1;
				}else{
					result[0] = x;
				}
				result[1] = y;
				break;
			case 1: 
				if(game.maze.iMaze[x][y+1]!=Maze.WALL && game.maze.iMaze[x][y+1]!=Maze.DOOR){
					result[1] = y+1;
				}else{
					result[1] = y;
				}
				result[0] = x;
				break;
			case 2: 
				if(game.maze.iMaze[x-1][y]!=Maze.WALL && game.maze.iMaze[x-1][y]!=Maze.DOOR){
					result[0] = x-1;
				}else{
					result[0] = x;
				}
				result[1] = y;
				break;
			case 3: 
				if(game.maze.iMaze[x][y-1]!=Maze.WALL && game.maze.iMaze[x][y-1]!=Maze.DOOR){
					result[1] = y-1;
				}else{
					result[1] = y;
				}
				result[0] = x;
				break;
		}
		aux = result[0];
		result[0]=result[1];
		result[1]=aux;
		return result;
	}

	public int getClosestFoodDistance(int x, int y) {
		int distance = Maze.HEIGHT*Maze.WIDTH;
		for (int h = 0; h < Maze.HEIGHT; h++) {
			for (int w = 0; w < Maze.WIDTH; w++) {
				if(getManhatanDistance(x, y, w, h)<distance && game.maze.iMaze[h][w]==Maze.DOT)
					distance=getManhatanDistance(x, y, w, h);
			}
		}
		return distance;
	}

	public int getNumGhost1stepAway(int x, int y) {
		int countGhost = 0;
		for (int i = 0; i < game.ghosts.length; i++) {
			if(toHouseSize(game.ghosts[i].iX)==x+1 && toHouseSize(game.ghosts[i].iY)==y)
				countGhost=countGhost+1;
			if(toHouseSize(game.ghosts[i].iX)==x-1 && toHouseSize(game.ghosts[i].iY)==y)
				countGhost=countGhost+1;
			if(toHouseSize(game.ghosts[i].iX)==x && toHouseSize(game.ghosts[i].iY)==y+1)
				countGhost=countGhost+1;
			if(toHouseSize(game.ghosts[i].iX)==x && toHouseSize(game.ghosts[i].iY)==y-1)
				countGhost=countGhost+1;
		}
		return countGhost;
	}

	public int getEatDot(int x, int y) {
		if(game.maze.iMaze[y][x]==Maze.DOT){
			return 1;
		}else{
			return 0;			
		}
	}

	public int getEatPowerDot(int x, int y) {
		if(game.maze.iMaze[y][x]==Maze.POWER_DOT){
			return 1;
		}else{
			return 0;			
		}
	}


	public int getEatGhost(int x, int y) {
		for (int i = 0; i < game.ghosts.length; i++) {
			if(game.ghosts[i].iStatus==Ghost.BLIND && (toHouseSize(game.ghosts[i].iX)==x && toHouseSize(game.ghosts[i].iY)==y))
				return 1;
		}
		return 0;
	}

	public int getClosestGhostToEatDistance(int x, int y) {
		int distance = Maze.HEIGHT*Maze.WIDTH;
		for (int i = 0; i < game.ghosts.length; i++) {
			if(game.ghosts[i].iStatus==Ghost.BLIND)
				if(getManhatanDistance(x, y, toHouseSize(game.ghosts[i].iX), toHouseSize(game.ghosts[i].iY))<distance)
					distance=getManhatanDistance(x, y, toHouseSize(game.ghosts[i].iX), ((int) game.ghosts[i].iY/GamePlayState.houseSize));
		}
		return distance;
	}
	
	private int toHouseSize(int x){
		return (int) x/ GamePlayState.houseSize;
	}

	public PacmanFeatures getFeatures(int x, int y, int action) {
		x = toHouseSize(x); y=toHouseSize(y);
		int newx = getNewPosition(x, y, action)[0], newy= getNewPosition(x, y, action)[1];
		//System.out.println(newx+" "+newy+" "+action);
		PacmanFeatures result = new PacmanFeatures();
		result.setClosestFoodDistance(getClosestFoodDistance(newx, newy));
		result.setClosestGhostToEatDistance(getClosestGhostToEatDistance(newx, newy));
		result.setEatDot(getEatDot(newx, newy));
		result.setEatGhost(getEatGhost(newx, newy));
		result.setEatPowerDot(getEatPowerDot(newx, newy));
		result.setNumGhost1stepAway(getNumGhost1stepAway(newx, newy));
		return result;
	}


}
