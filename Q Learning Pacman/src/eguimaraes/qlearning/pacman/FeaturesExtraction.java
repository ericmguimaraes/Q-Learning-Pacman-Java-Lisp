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

	public int getManhatanDistance(int x1, int y1, int x2, int y2) {
		return (Math.abs(x1 - x2) + Math.abs(y1 - y2));
	}

	public int[] getNewPosition(int x, int y, int action) {

		int[] result = new int[2];
		switch (action) {
		case 0:
			if (game.maze.iMaze[y][x + 1] != Maze.WALL
					&& game.maze.iMaze[y][x + 1] != Maze.DOOR) {
				result[0] = x + 1;
			} else {
				result[0] = x;
			}
			result[1] = y;
			break;
		case 1:
			if (game.maze.iMaze[y - 1][x] != Maze.WALL
					&& game.maze.iMaze[y - 1][x] != Maze.DOOR) {
				result[1] = y - 1;
			} else {
				result[1] = y;
			}
			result[0] = x;
			break;
		case 2:
			if (game.maze.iMaze[y][x - 1] != Maze.WALL
					&& game.maze.iMaze[y][x - 1] != Maze.DOOR) {
				result[0] = x - 1;
			} else {
				result[0] = x;
			}
			result[1] = y;
			break;
		case 3:
			if (game.maze.iMaze[y + 1][x] != Maze.WALL
					&& game.maze.iMaze[y + 1][x] != Maze.DOOR) {
				result[1] = y + 1;
			} else {
				result[1] = y;
			}
			result[0] = x;
			break;
		}

		return result;
	}

	public float getClosestFoodDistance(int x, int y) {
		int distance = Maze.HEIGHT * Maze.WIDTH;
		for (int h = 0; h < Maze.HEIGHT; h++) {
			for (int w = 0; w < Maze.WIDTH; w++) {
				if (getManhatanDistance(x, y, w, h) < distance
						&& game.maze.iMaze[h][w] == Maze.DOT)
					distance = getManhatanDistance(x, y, w, h);
			}
		}
		float n = ((float) distance / 30);
		// System.out.println("Distance orig: "+n);
		return n;
	}

	public int getNumGhost1stepAway(int x, int y) {
		int countGhost = 0;
		for (int i = 0; i < game.ghosts.length; i++) {
			if (game.ghosts[i].iStatus == Ghost.BLIND)
				continue;
			int xg = toHouseSize(game.ghosts[i].iX), yg = toHouseSize(game.ghosts[i].iY);
			if ((xg == x && yg == y) || (xg == x + 1 && yg == y)
					|| (xg == x + 1 && yg == y + 1)
					|| (xg == x + 1 && yg == y - 1) || (xg == x - 1 && yg == y)
					|| (xg == x - 1 && yg == y + 1)
					|| (xg == x - 1 && yg == y - 1) || (yg == y + 1 && xg == x)
					|| (yg == y + 1 && xg == x + 1)
					|| (yg == y + 1 && xg == x - 1) || (yg == y - 1 && xg == x)
					|| (yg == y - 1 && xg == x + 1)
					|| (yg == y - 1 && xg == x - 1))
				countGhost = countGhost + 1;
			// if ((xg == x + 2 && yg == y)
			// || (xg == x && yg == y - 2)
			// || (xg == x - 2 && yg == y)
			// || (xg == x && yg == y + 2)
			// || (xg == x + 2 && yg == y - 1)
			// || (xg == x + 2 && yg == y - 2)
			// || (xg == x + 2 && yg == y + 1)
			// || (xg == x + 2 && yg == y + 2)
			// || (xg == x - 2 && yg == y - 1)
			// || (xg == x - 2 && yg == y - 2)
			// || (xg == x - 2 && yg == y + 1)
			// || (xg == x - 2 && yg == y + 2)
			// || (yg == y + 2 && xg == x - 1)
			// || (yg == y + 2 && xg == x - 2)
			// || (yg == y + 2 && xg == x + 1)
			// || (yg == y + 2 && xg == x + 2)
			// || (yg == y - 2 && xg == x - 1)
			// || (yg == y - 2 && xg == x - 2)
			// || (yg == y - 2 && xg == x + 1)
			// || (yg == y - 2 && xg == x + 2))
			// countGhost = countGhost + 1;
		}

		return countGhost;
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

	public int getEatGhost(int x, int y) {
		for (int i = 0; i < game.ghosts.length; i++) {
			if (game.ghosts[i].iStatus == Ghost.BLIND
					&& (toHouseSize(game.ghosts[i].iX) == x && toHouseSize(game.ghosts[i].iY) == y))
				return 1;
		}
		return 0;
	}

	public float getClosestGhostToEatDistance(int x, int y) {
		int distance = 0;// Maze.HEIGHT*Maze.WIDTH;
		for (int i = 0; i < game.ghosts.length; i++) {
			if (game.ghosts[i].iStatus == Ghost.BLIND)
				if (getManhatanDistance(x, y, toHouseSize(game.ghosts[i].iX),
						toHouseSize(game.ghosts[i].iY)) < distance)
					distance = getManhatanDistance(x, y,
							toHouseSize(game.ghosts[i].iX),
							((int) game.ghosts[i].iY / GamePlayState.houseSize));
		}
		float n = ((float) distance / 30);
		return n;
	}

	private int getBeEaten(int x, int y) {
		for (int i = 0; i < game.ghosts.length; i++) {
			if (game.ghosts[i].iStatus == Ghost.BLIND)
				continue;
			int pic = 16;
			if ((x-pic < game.ghosts[i].iX) && (game.ghosts[i].iX < x+pic) &&
					(y-pic < game.ghosts[i].iY) && (game.ghosts[i].iY < y+pic)) {
				return 1;
			}
		}
		return 0;
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

	public int toHouseSize(int x) {
		return (int) x / GamePlayState.houseSize;
	}

	public PacmanFeatures getFeatures(int x, int y, int action) {
		x = toHouseSize(x);
		y = toHouseSize(y);
		int newx = getNewPosition(x, y, action)[0], newy = getNewPosition(x, y,
				action)[1];
		return getFeaturesAUX(newx, newy);
	}

	public PacmanFeatures getFeatures(int x, int y) {
		int newx = toHouseSize(x), newy = toHouseSize(y);
		return getFeaturesAUX(newx, newy);
		// TODO get coming back in direction feature
	}

	private PacmanFeatures getFeaturesAUX(int newx, int newy) {
		PacmanFeatures result = new PacmanFeatures();
		boolean follower = getNumGhost1stepAway(newx, newy) > 0;
		result.setClosestFoodDistance(follower ? 0 : getClosestFoodDistance(
				newx, newy));
		result.setClosestGhostToEatDistance(follower ? 0
				: getClosestGhostToEatDistance(newx, newy));
		result.setEatDot(follower ? 0 : getEatDot(newx, newy));
		result.setEatGhost(follower ? 0 : getEatGhost(newx, newy));
		result.setEatPowerDot(follower ? 0 : getEatPowerDot(newx, newy));
		result.setNumGhost1stepAway(getNumGhost1stepAway(newx, newy));
		result.setBeEaten(getBeEaten(newx, newy));
		return result;
	}

}
