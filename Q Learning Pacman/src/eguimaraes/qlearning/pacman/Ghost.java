/**
 * Copyright (C) 1997-2010 Junyang Gu <mikejyg@gmail.com>
 * 
 * This file is part of javaiPacman.
 *
 * javaiPacman is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * javaiPacman is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with javaiPacman.  If not, see <http://www.gnu.org/licenses/>.
 */

package eguimaraes.qlearning.pacman;

import java.lang.Error;
import java.awt.*;

public class Ghost {
	final int IN = 0;
	final int OUT = 1;
	final int BLIND = 2;
	final int EYE = 3;

	final int[] steps = { 7, 7, 1, 1 };
	final int[] frames = { 8, 8, 2, 1 };

	final int INIT_BLIND_COUNT = 600; // remain blind for ??? frames
	int blindCount;

	GhostSpeed speed = new GhostSpeed();

	int iX, iY, iDir, iStatus;
	int iBlink, iBlindCount;

	// random calculation factors
	final int DIR_FACTOR = 2;
	final int POS_FACTOR = 10;

	// the applet this object is associated to
	Window applet;
	Graphics graphics;

	// the maze the ghosts knows
	Maze maze;

	// the ghost image
	Image imageGhost;
	Image imageBlind;
	Image imageEye;

	Ghost(Window a, Graphics g, Maze m, Color color) {
		applet = a;
		graphics = g;
		maze = m;

		imageGhost = applet.createImage(18, 18);
		ImagePac.drawGhost(imageGhost, 0, color);

		imageBlind = applet.createImage(18, 18);
		ImagePac.drawGhost(imageBlind, 1, Color.white);

		imageEye = applet.createImage(18, 18);
		ImagePac.drawGhost(imageEye, 2, Color.lightGray);
	}

	public void start(int initialPosition, int round) {
		if (initialPosition >= 2)
			initialPosition++;
		iX = (8 + initialPosition) * 16;
		iY = 8 * 16;
		iDir = 3;
		iStatus = IN;

		blindCount = INIT_BLIND_COUNT / ((round + 1) / 2);

		speed.start(steps[iStatus], frames[iStatus]);
	}

	public void draw() {
		maze.DrawDot(iX / 16, iY / 16);
		maze.DrawDot(iX / 16 + (iX % 16 > 0 ? 1 : 0), iY / 16
				+ (iY % 16 > 0 ? 1 : 0));

		if (iStatus == BLIND && iBlink == 1 && iBlindCount % 32 < 16)
			graphics.drawImage(imageGhost, iX - 1, iY - 1, applet);
		else if (iStatus == OUT || iStatus == IN)
			graphics.drawImage(imageGhost, iX - 1, iY - 1, applet);
		else if (iStatus == BLIND)
			graphics.drawImage(imageBlind, iX - 1, iY - 1, applet);
		else
			graphics.drawImage(imageEye, iX - 1, iY - 1, applet);
	}

	public void move(int iPacX, int iPacY, int iPacDir) {
		if (iStatus == BLIND) {
			iBlindCount--;
			if (iBlindCount < blindCount / 3)
				iBlink = 1;
			if (iBlindCount == 0)
				iStatus = OUT;
			if (iBlindCount % 2 == 1) // blind moves at 1/2 speed
				return;
		}

		if (speed.isMove() == 0)
			// no move
			return;

		if (iX % 16 == 0 && iY % 16 == 0)
		// determine direction
		{
			switch (iStatus) {
			case IN:
				iDir = INSelect();
				break;
			case OUT:
				iDir = OUTSelect(iPacX, iPacY, iPacDir);
				break;
			case BLIND:
				iDir = BLINDSelect(iPacX, iPacY, iPacDir);
				break;
			case EYE:
				iDir = EYESelect();
			}
		}

		if (iStatus != EYE) {
			iX += Tables.iXDirection[iDir];
			iY += Tables.iYDirection[iDir];
		} else {
			iX += 2 * Tables.iXDirection[iDir];
			iY += 2 * Tables.iYDirection[iDir];
		}

	}

	public int INSelect()
	// count available directions
			throws Error {
		int iM, i, iRand;
		int iDirTotal = 0;

		for (i = 0; i < 4; i++) {
			iM = maze.iMaze[iY / 16 + Tables.iYDirection[i]][iX / 16
					+ Tables.iXDirection[i]];
			if (iM != Maze.WALL && i != Tables.iBack[iDir]) {
				iDirTotal++;
			}
		}
		// randomly select a direction
		if (iDirTotal != 0) {
			iRand = Utility.RandSelect(iDirTotal);
			if (iRand >= iDirTotal)
				throw new Error("iRand out of range");
			// exit(2);
			for (i = 0; i < 4; i++) {
				iM = maze.iMaze[iY / 16 + Tables.iYDirection[i]][iX / 16
						+ Tables.iXDirection[i]];
				if (iM != Maze.WALL && i != Tables.iBack[iDir]) {
					iRand--;
					if (iRand < 0)
					// the right selection
					{
						if (iM == Maze.DOOR)
							iStatus = OUT;
						iDir = i;
						break;
					}
				}
			}
		}
		return (iDir);
	}

	public int OUTSelect(int iPacX, int iPacY, int iPacDir)
	// count available directions
			throws Error {
		int iM, i, iRand;
		int iDirTotal = 0;
		int[] iDirCount = new int[4];

		for (i = 0; i < 4; i++) {
			iDirCount[i] = 0;
			iM = maze.iMaze[iY / 16 + Tables.iYDirection[i]][iX / 16
					+ Tables.iXDirection[i]];
			if (iM != Maze.WALL && i != Tables.iBack[iDir] && iM != Maze.DOOR)
			// door is not accessible for OUT
			{
				iDirCount[i]++;
				iDirCount[i] += iDir == iPacDir ? DIR_FACTOR : 0;
				switch (i) {
				case 0: // right
					iDirCount[i] += iPacX > iX ? POS_FACTOR : 0;
					break;
				case 1: // up
					iDirCount[i] += iPacY < iY ? POS_FACTOR : 0;
					break;
				case 2: // left
					iDirCount[i] += iPacX < iX ? POS_FACTOR : 0;
					break;
				case 3: // down
					iDirCount[i] += iPacY > iY ? POS_FACTOR : 0;
					break;
				}
				iDirTotal += iDirCount[i];
			}
		}
		// randomly select a direction
		if (iDirTotal != 0) {
			iRand = Utility.RandSelect(iDirTotal);
			if (iRand >= iDirTotal)
				throw new Error("iRand out of range");
			// exit(2);
			for (i = 0; i < 4; i++) {
				iM = maze.iMaze[iY / 16 + Tables.iYDirection[i]][iX / 16
						+ Tables.iXDirection[i]];
				if (iM != Maze.WALL && i != Tables.iBack[iDir]
						&& iM != Maze.DOOR) {
					iRand -= iDirCount[i];
					if (iRand < 0)
					// the right selection
					{
						iDir = i;
						break;
					}
				}
			}
		} else
			throw new Error("iDirTotal out of range");
		// exit(1);
		return (iDir);
	}

	public void blind() {
		if (iStatus == BLIND || iStatus == OUT) {
			iStatus = BLIND;
			iBlindCount = blindCount;
			iBlink = 0;
			// reverse
			if (iX % 16 != 0 || iY % 16 != 0) {
				iDir = Tables.iBack[iDir];
				// a special condition:
				// when ghost is leaving home, it can not go back
				// while becoming blind
				int iM;
				iM = maze.iMaze[iY / 16 + Tables.iYDirection[iDir]][iX / 16
						+ Tables.iXDirection[iDir]];
				if (iM == Maze.DOOR)
					iDir = Tables.iBack[iDir];
			}
		}
	}

	public int EYESelect()
	// count available directions
			throws Error {
		int iM, i, iRand;
		int iDirTotal = 0;
		int[] iDirCount = new int[4];

		for (i = 0; i < 4; i++) {
			iDirCount[i] = 0;
			iM = maze.iMaze[iY / 16 + Tables.iYDirection[i]][iX / 16
					+ Tables.iXDirection[i]];
			if (iM != Maze.WALL && i != Tables.iBack[iDir]) {
				iDirCount[i]++;
				switch (i) {
				// door position 10,6
				case 0: // right
					iDirCount[i] += 160 > iX ? POS_FACTOR : 0;
					break;
				case 1: // up
					iDirCount[i] += 96 < iY ? POS_FACTOR : 0;
					break;
				case 2: // left
					iDirCount[i] += 160 < iX ? POS_FACTOR : 0;
					break;
				case 3: // down
					iDirCount[i] += 96 > iY ? POS_FACTOR : 0;
					break;
				}
				iDirTotal += iDirCount[i];
			}
		}
		// randomly select a direction
		if (iDirTotal != 0) {
			iRand = Utility.RandSelect(iDirTotal);
			if (iRand >= iDirTotal)
				throw new Error("RandSelect out of range");
			// exit(2);
			for (i = 0; i < 4; i++) {
				iM = maze.iMaze[iY / 16 + Tables.iYDirection[i]][iX / 16
						+ Tables.iXDirection[i]];
				if (iM != Maze.WALL && i != Tables.iBack[iDir]) {
					iRand -= iDirCount[i];
					if (iRand < 0)
					// the right selection
					{
						if (iM == Maze.DOOR)
							iStatus = IN;
						iDir = i;
						break;
					}
				}
			}
		} else
			throw new Error("iDirTotal out of range");
		return (iDir);
	}

	public int BLINDSelect(int iPacX, int iPacY, int iPacDir)
	// count available directions
			throws Error {
		int iM, i, iRand;
		int iDirTotal = 0;
		int[] iDirCount = new int[4];

		for (i = 0; i < 4; i++) {
			iDirCount[i] = 0;
			iM = maze.iMaze[iY / 16 + Tables.iYDirection[i]][iX / 16
					+ Tables.iXDirection[i]];
			if (iM != Maze.WALL && i != Tables.iBack[iDir] && iM != Maze.DOOR)
			// door is not accessible for OUT
			{
				iDirCount[i]++;
				iDirCount[i] += iDir == iPacDir ? DIR_FACTOR : 0;
				switch (i) {
				case 0: // right
					iDirCount[i] += iPacX < iX ? POS_FACTOR : 0;
					break;
				case 1: // up
					iDirCount[i] += iPacY > iY ? POS_FACTOR : 0;
					break;
				case 2: // left
					iDirCount[i] += iPacX > iX ? POS_FACTOR : 0;
					break;
				case 3: // down
					iDirCount[i] += iPacY < iY ? POS_FACTOR : 0;
					break;
				}
				iDirTotal += iDirCount[i];
			}
		}
		// randomly select a direction
		if (iDirTotal != 0) {
			iRand = Utility.RandSelect(iDirTotal);
			if (iRand >= iDirTotal)
				throw new Error("RandSelect out of range");
			// exit(2);
			for (i = 0; i < 4; i++) {
				iM = maze.iMaze[iY / 16 + Tables.iYDirection[i]][iX / 16
						+ Tables.iXDirection[i]];
				if (iM != Maze.WALL && i != Tables.iBack[iDir]) {
					iRand -= iDirCount[i];
					if (iRand < 0)
					// the right selection
					{
						iDir = i;
						break;
					}
				}
			}
		} else
			throw new Error("iDirTotal out of range");
		return (iDir);
	}

	// return 1 if caught the pac!
	// return 2 if being caught by pac
	int testCollision(int iPacX, int iPacY) {
		if (iX <= iPacX + 2 && iX >= iPacX - 2 && iY <= iPacY + 2
				&& iY >= iPacY - 2) {
			switch (iStatus) {
			case OUT:
				return (1);
			case BLIND:
				iStatus = EYE;
				iX = iX / 4 * 4;
				iY = iY / 4 * 4;
				return (2);
			}
		}
		// nothing
		return (0);
	}
}
