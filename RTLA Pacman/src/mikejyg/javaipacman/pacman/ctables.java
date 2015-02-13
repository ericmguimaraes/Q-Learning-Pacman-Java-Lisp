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

/**
 * the tables are used to speed up computation
 */
public class ctables
{
	// for direction computation
	public static final int[] iXDirection={1,0,-1,0};
	public static final int[] iYDirection={0,-1,0,1};
	public static final int[] iDirection=
	{
		-1,	// 0:
		1,	// 1: x=0, y=-1
		-1,	// 2:
		-1,	// 3:
		2,	// 4: x=-1, y=0
		-1,	// 5:
		0,	// 6: x=1, y=0
		-1,	// 7
		-1,     // 8
		3     	// 9: x=0, y=1
	};

	// backward direction
	public static final int[] iBack={2,3,0,1};

	// direction code
	public static final int RIGHT=0;
	public static final int UP=1;
	public static final int LEFT=2;
	public static final int DOWN=3;

	// the maze difinition string
	public static final String[] MazeDefine=
	{
		"XXXXXXXXXXXXXXXXXXXXX",	// 1
		"X.........X.........X",	// 2
		"XOXXX.XXX.X.XXX.XXXOX",	// 3
		"X......X..X.........X",	// 4
		"XXX.XX.X.XXX.XX.X.X.X",	// 5
		"X....X..........X.X.X",	// 6
		"X.XX.X.XXX-XXX.XX.X.X",	// 7
		"X.XX.X.X     X......X",	// 8
		"X.XX...X     X.XXXX.X",	// 9
		"X.XX.X.XXXXXXX.XXXX.X",	// 10
		"X....X.... .........X",	// 11
		"XXX.XX.XXXXXXX.X.X.XX",	// 12
		"X.........X....X....X",	// 13
		"XOXXXXXXX.X.XXXXXXXOX",	// 14
		"X...................X",	// 15
		"XXXXXXXXXXXXXXXXXXXXX",	// 16
	};


}

