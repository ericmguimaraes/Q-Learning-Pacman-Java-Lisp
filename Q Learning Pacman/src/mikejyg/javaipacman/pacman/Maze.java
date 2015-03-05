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

package mikejyg.javaipacman.pacman;

import java.awt.*;

/* define the maze */
public class Maze {
	// constant definitions
	static final int BLANK = 0;
	static final int WALL = 1;
	static final int DOOR = 2;
	static final int DOT = 4;
	static final int POWER_DOT = 8;

	static final int HEIGHT = 16;
	static final int WIDTH = 21;

	static final int iHeight = HEIGHT * 16;
	static final int iWidth = WIDTH * 16;

	// the applet the object associate with
	Window applet;
	// the graphics it will be using
	Graphics graphics;

	// the maze image
	Image imageMaze;

	// the dot image
	Image imageDot;

	// total dots left
	int iTotalDotcount;

	// the status of maze
	int[][] iMaze;

	// initialize the maze
	Maze(Window a, Graphics g) {
		// setup associations
		applet = a;
		graphics = g;

		imageMaze = applet.createImage(iWidth, iHeight);
		imageDot = applet.createImage(2, 2);

		// create data
		iMaze = new int[HEIGHT][WIDTH];
	}

	public void start() {
		int i, j, k;
		iTotalDotcount = 0;
		for (i = 0; i < HEIGHT; i++)
			for (j = 0; j < WIDTH; j++) {
				switch (Tables.MazeDefine[i].charAt(j)) {
				case ' ':
					k = BLANK;
					break;
				case 'X':
					k = WALL;
					break;
				case '.':
					k = DOT;
					iTotalDotcount++;
					break;
				case 'O':
					k = POWER_DOT;
					break;
				case '-':
					k = DOOR;
					break;
				default:
					k = DOT;
					iTotalDotcount++;
					break;
				}
				iMaze[i][j] = k;
			}
		// create initial maze image
		createImage();
	}

	public void draw() {
		graphics.drawImage(imageMaze, 0, 0, applet);
		drawDots();
	}

	void drawDots() // on the offscreen
	{
		int i, j;

		for (i = 0; i < HEIGHT; i++)
			for (j = 0; j < WIDTH; j++) {
				if (iMaze[i][j] == DOT)
					graphics.drawImage(imageDot, j * 16 + 7, i * 16 + 7, applet);
			}
	}

	void createImage() {
		// create the image of a dot
		ImagePac.drawDot(imageDot);

		// create the image of the maze
		Graphics gmaze = imageMaze.getGraphics();

		// background
		gmaze.setColor(Color.black);
		gmaze.fillRect(0, 0, iWidth, iHeight);

		DrawWall(gmaze);
	}

	public void DrawDot(int icol, int iRow) {
		if (iMaze[iRow][icol] == DOT)
			graphics.drawImage(imageDot, icol * 16 + 7, iRow * 16 + 7, applet);
	}

	void DrawWall(Graphics g) {
		int i, j;
		int iDir;

		g.setColor(Color.blue);

		for (i = 0; i < HEIGHT; i++) {
			for (j = 0; j < WIDTH; j++) {
				for (iDir = Tables.RIGHT; iDir <= Tables.DOWN; iDir++) {
					if (iMaze[i][j] == DOOR) {
						g.drawLine(j * 16, i * 16 + 8, j * 16 + 16, i * 16 + 8);
						continue;
					}
					if (iMaze[i][j] != WALL)
						continue;
					switch (iDir) {
					case Tables.UP:
						if (i == 0)
							break;
						if (iMaze[i - 1][j] == WALL)
							break;
						DrawBoundary(g, j, i - 1, Tables.DOWN);
						break;
					case Tables.RIGHT:
						if (j == WIDTH - 1)
							break;
						if (iMaze[i][j + 1] == WALL)
							break;
						DrawBoundary(g, j + 1, i, Tables.LEFT);
						break;
					case Tables.DOWN:
						if (i == HEIGHT - 1)
							break;
						if (iMaze[i + 1][j] == WALL)
							break;
						DrawBoundary(g, j, i + 1, Tables.UP);
						break;
					case Tables.LEFT:
						if (j == 0)
							break;
						if (iMaze[i][j - 1] == WALL)
							break;
						DrawBoundary(g, j - 1, i, Tables.RIGHT);
						break;
					default:
					}
				}
			}
		}
	}

	void DrawBoundary(Graphics g, int col, int row, int iDir) {
		int x, y;

		x = col * 16;
		y = row * 16;

		switch (iDir) {
		case Tables.LEFT:
			// draw lower half segment
			if (iMaze[row + 1][col] != WALL)
				// down empty
				if (iMaze[row + 1][col - 1] != WALL)
				// left-down empty
				{
					// arc(x-8,y+8,270,0,6);
					g.drawArc(x - 8 - 6, y + 8 - 6, 12, 12, 270, 100);
				} else {
					g.drawLine(x - 2, y + 8, x - 2, y + 16);
				}
			else {
				g.drawLine(x - 2, y + 8, x - 2, y + 17);
				g.drawLine(x - 2, y + 17, x + 7, y + 17);
			}

			// Draw upper half segment
			if (iMaze[row - 1][col] != WALL)
				// upper empty
				if (iMaze[row - 1][col - 1] != WALL)
				// upper-left empty
				{
					// arc(x-8,y+7,0,90,6);
					g.drawArc(x - 8 - 6, y + 7 - 6, 12, 12, 0, 100);
				} else {
					g.drawLine(x - 2, y, x - 2, y + 7);
				}
			else {
				g.drawLine(x - 2, y - 2, x - 2, y + 7);
				g.drawLine(x - 2, y - 2, x + 7, y - 2);
			}
			break;

		case Tables.RIGHT:
			// draw lower half segment
			if (iMaze[row + 1][col] != WALL)
				// down empty
				if (iMaze[row + 1][col + 1] != WALL)
				// down-right empty
				{
					// arc(x+16+7,y+8,180,270,6);
					g.drawArc(x + 16 + 7 - 6, y + 8 - 6, 12, 12, 180, 100);
				} else {
					g.drawLine(x + 17, y + 8, x + 17, y + 15);
				}
			else {
				g.drawLine(x + 8, y + 17, x + 17, y + 17);
				g.drawLine(x + 17, y + 8, x + 17, y + 17);
			}
			// Draw upper half segment
			if (iMaze[row - 1][col] != WALL)
				// upper empty
				if (iMaze[row - 1][col + 1] != WALL)
				// upper-right empty
				{
					// arc(x+16+7,y+7,90,180,6);
					g.drawArc(x + 16 + 7 - 6, y + 7 - 6, 12, 12, 90, 100);
				} else {
					g.drawLine(x + 17, y, x + 17, y + 7);
				}
			else {
				g.drawLine(x + 8, y - 2, x + 17, y - 2);
				g.drawLine(x + 17, y - 2, x + 17, y + 7);
			}
			break;

		case Tables.UP:
			// draw left half segment
			if (iMaze[row][col - 1] != WALL)
				// left empty
				if (iMaze[row - 1][col - 1] != WALL)
				// left-upper empty
				{
					// arc(x+7,y-8,180,270,6);
					g.drawArc(x + 7 - 6, y - 8 - 6, 12, 12, 180, 100);
				} else {
					g.drawLine(x, y - 2, x + 7, y - 2);
				}

			// Draw right half segment
			if (iMaze[row][col + 1] != WALL)
				// right empty
				if (iMaze[row - 1][col + 1] != WALL)
				// right-upper empty
				{
					// arc(x+8,y-8,270,0,6);
					g.drawArc(x + 8 - 6, y - 8 - 6, 12, 12, 270, 100);
				} else {
					g.drawLine(x + 8, y - 2, x + 16, y - 2);
				}
			break;

		case Tables.DOWN:
			// draw left half segment
			if (iMaze[row][col - 1] != WALL)
				// left empty
				if (iMaze[row + 1][col - 1] != WALL)
				// left-down empty
				{
					// arc(x+7,y+16+7,90,180,6);
					g.drawArc(x + 7 - 6, y + 16 + 7 - 6, 12, 12, 90, 100);
				} else {
					g.drawLine(x, y + 17, x + 7, y + 17);
				}

			// Draw right half segment
			if (iMaze[row][col + 1] != WALL)
				// right empty
				if (iMaze[row + 1][col + 1] != WALL)
				// right-down empty
				{
					// arc(x+8,y+16+7,0,90,6);
					g.drawArc(x + 8 - 6, y + 16 + 7 - 6, 12, 12, 0, 100);
				} else {
					g.drawLine(x + 8, y + 17, x + 15, y + 17);
				}
			break;
		}
	}

}
