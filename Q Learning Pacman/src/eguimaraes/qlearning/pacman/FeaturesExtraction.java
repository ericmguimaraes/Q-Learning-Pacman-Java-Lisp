package eguimaraes.qlearning.pacman;

public class FeaturesExtraction {

	private static FeaturesExtraction instance;

	private GamePlayState game;
	
	private Dijkstra dijkstra;

	private FeaturesExtraction(GamePlayState game) {
		this.game = game;
		dijkstra = new Dijkstra();
	}

	public static FeaturesExtraction getInstance(GamePlayState state) {
		if (instance == null)
			instance = new FeaturesExtraction(state);
		return instance;
	}

	public int getManhatanDistance(int x1, int y1, int x2, int y2) {
		return (Math.abs(x1 - x2) + Math.abs(y1 - y2));
	}

	public int[] getNewPosition(int x, int y, int action) {

		int[] result = new int[2];
		switch (action) {
		case 0:
			if (!game.maze.isBlocked(x + 1, y)) {
				result[0] = x + 1;
			} else {
				result[0] = x;
			}
			result[1] = y;
			break;
		case 1:
			if (!game.maze.isBlocked(x, y-1)) {
				result[1] = y - 1;
			} else {
				result[1] = y;
			}
			result[0] = x;
			break;
		case 2:
			if (!game.maze.isBlocked(x - 1, y)) {
				result[0] = x - 1;
			} else {
				result[0] = x;
			}
			result[1] = y;
			break;
		case 3:
			if (!game.maze.isBlocked(x , y+1)) {
				result[1] = y + 1;
			} else {
				result[1] = y;
			}
			result[0] = x;
			break;
		}
		return result;
	}
	
	public int[] getNewPositionNOTHOUSESIZE(int x, int y, int action) {
		int[] result = new int[2];
		switch (action) {
		case 0:
			if (!game.maze.isBlocked(toHouseSize(x + 16), toHouseSize(y))) {
				result[0] = x + 16;
			} else {
				result[0] = x;
			}
			result[1] = y;
			break;
		case 1:
			if (!game.maze.isBlocked(toHouseSize(x), toHouseSize(y-16))) {
				result[1] = y - 16;
			} else {
				result[1] = y;
			}
			result[0] = x;
			break;
		case 2:
			if (!game.maze.isBlocked(toHouseSize(x - 16), toHouseSize(y))) {
				result[0] = x - 16;
			} else {
				result[0] = x;
			}
			result[1] = y;
			break;
		case 3:
			if (!game.maze.isBlocked(toHouseSize(x) , toHouseSize(y+16))) {
				result[1] = y + 16;
			} else {
				result[1] = y;
			}
			result[0] = x;
			break;
		}
		return result;
	}

	public float getClosestFoodDistance(int x, int y) {
		/*int distance = Maze.HEIGHT + Maze.WIDTH;
		for (int h = 0; h < Maze.HEIGHT; h++) {
			for (int w = 0; w < Maze.WIDTH; w++) {
				if (getManhatanDistance(x, y, w, h) < distance
						&& game.maze.iMaze[h][w] == Maze.DOT)
					distance = getManhatanDistance(x, y, w, h);
			}
		}
		*/
		int distance = dijkstra.getDistanceToTheClosestDot(x, y, game.maze);
		if(distance<0)distance=0;
		//System.err.println("DISTANCE: "+distance);

		//float n = ((float) distance / 40);
		float n;
//		if(distance==0)
//			n=1;
//		else 
			n = (float) 1/(distance+1);
		//System.err.println(n);
		//System.out.println("Distance: "+n);
		return n; 
	}

	

	public int getEatDot(int x, int y) {
		if (game.maze.iMaze[y][x] == Maze.DOT) {
			return 1;
		} else {
			return 0;
		}
	}

	public int getEatPowerDot(int x, int y) {
		if (game.maze.iMaze[y][x] == Maze.POWER_DOT) {
			return 1;
		} else {
			return 0;
		}
	}

	public int isSomeGhostHere(int x, int y, int ghostStatus) {
		for (int i = 0; i < game.ghosts.length; i++) {
			if (game.ghosts[i].iStatus == ghostStatus
					&& isCollision(x, y, game.ghosts[i].iX, game.ghosts[i].iY,
							8))
				return 1;
		}
		return 0;
	}
	
	public float getNumGhost1stepAway(int x, int y) {
		float countGhost = 0;
		for (int i = 0; i < game.ghosts.length; i++) {
			if (game.ghosts[i].iStatus == Ghost.BLIND)
				continue;
			if (isCollision(x, y, game.ghosts[i].iX, game.ghosts[i].iY, 20))
				countGhost++;
		}
		float n = ((float) countGhost/4);
		return n;
	}

	public boolean isCollision(int iPacX, int iPacY, int xG, int yG, int pixels) {
		if (xG <= iPacX + pixels && xG >= iPacX - pixels
				&& yG <= iPacY + pixels && yG >= iPacY - pixels)
			return true;
		return false;
	}

	public int getEatGhost(int x, int y) {
		return isSomeGhostHere(x, y, Ghost.BLIND);
	}

	public int getBeEaten(int x, int y) {
		return isSomeGhostHere(x, y, Ghost.OUT);
	}

	public float getClosestGhostToEatDistance(int x, int y) {
//		int distance = Maze.HEIGHT*16 + Maze.WIDTH*16, count = 0;
//
//		for (int i = 0; i < game.ghosts.length; i++) {
//			if (game.ghosts[i].iStatus == Ghost.BLIND) {
//				int newdist = getManhatanDistance(x, y, game.ghosts[i].iX, game.ghosts[i].iY);
//				if (newdist < distance)
//					distance = newdist;
//				count++;
//			}
//		}
		//if(count != 0)System.out.println(distance);
		int distance = dijkstra.getDistanceToTheClosestEatableGhost(x, y, game.ghosts, game.maze);
		//float n = ((float) distance /  50);
//		int c = 0;
//		for (Ghost g : game.ghosts) {
//			if(g.iStatus == Ghost.BLIND) c++;
//		}
//		float n;
//		if(c==0)
//			n=0;
//		else
//			n = (float) 1/(distance+1);
//		
		return (float) distance/40;//count != 0 ? n : 0;
	}

	public int getisTurningBack(int newdir) {
		switch (game.lastAction) {
		case 0:
			if (newdir == 2)
				return 1;
		case 1:
			if (newdir == 3)
				return 1;
		case 2:
			if (newdir == 0)
				return 1;
		case 3:
			if (newdir == 1)
				return 1;
		}
		return 0;
	}

	public static int toHouseSize(int x) {
		return (int) x / GamePlayState.houseSize;
	}

	public PacmanFeatures getFeatures(int x, int y, int action) {
		int newx = toHouseSize(x);
		int newy = toHouseSize(y);
		newx = getNewPosition(newx, newy, action)[0];
		newy = getNewPosition(newx, newy, action)[1];
		x = newx * GamePlayState.houseSize + x % GamePlayState.houseSize;
		y = newy * GamePlayState.houseSize + y % GamePlayState.houseSize;
		return getFeaturesAUX(x, y, newx, newy);
	}

	public PacmanFeatures getFeatures(int x, int y) {
		int newx = toHouseSize(x), newy = toHouseSize(y);
		return getFeaturesAUX(x, y, newx, newy);
	}
	
	public PacmanFeatures getFeaturesFromHouseSize(int x, int y) {
		return getFeaturesAUX(x*16, y*16, x, y);
	}

	private PacmanFeatures getFeaturesAUX(int x, int y, int newx, int newy) {
		PacmanFeatures result = new PacmanFeatures();
		boolean follower = getBeEaten(x, y)==1 || getNumGhost1stepAway(x, y)>0;
		result.setClosestFoodDistance(follower?0:getClosestFoodDistance(newx, newy));
		result.setClosestGhostToEatDistance(follower?0:getClosestGhostToEatDistance(newx, newy));
		result.setEatDot(follower?0:getEatDot(newx, newy));
		result.setEatGhost(follower?0:getEatGhost(x, y));
		result.setEatPowerDot(follower?0:getEatPowerDot(newx, newy));
		result.setNumGhost1stepAway(getNumGhost1stepAway(x, y));
		result.setBeEaten(getBeEaten(x, y));
		return result;
	}

}
