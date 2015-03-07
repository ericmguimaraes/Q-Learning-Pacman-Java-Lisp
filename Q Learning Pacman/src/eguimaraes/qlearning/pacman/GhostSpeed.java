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

/**
 * speed control
 * 
 * use init(s,f) to set the frame/step ratio NOTE: s must <= f call start() to
 * reset counters call isMove per frame to see if step are to be taken
 */
public class GhostSpeed {
	// move steps per frames
	int steps;
	int frames;

	int frameCount;
	int stepCount;

	float frameStepRatio;

	GhostSpeed() {
			start(1, 1);
	}

	public void start(int s, int f) throws Error {
		if (f < s)
			throw new Error("Cspeed.init(...): frame must >= step");

		steps = s;
		frames = f;
		frameStepRatio = (float) frames / (float) steps;

		stepCount = steps;
		frameCount = frames;
	}

	// return 1 if move, 0 not move
	public int isMove() {
		frameCount--;

		float ratio = (float) frameCount / (float) stepCount;

		if (frameCount == 0)
			frameCount = frames;

		if (ratio < frameStepRatio) {
			stepCount--;
			if (stepCount == 0)
				stepCount = steps;
			return (1);
		}
		return (0);
	}
}
