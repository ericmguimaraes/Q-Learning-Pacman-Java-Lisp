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

package mikejyg.javaipacman.wrapper;

import java.applet.Applet;
import java.awt.*;
import java.awt.event.*;

import mikejyg.javaipacman.pacman.Pacman;

/**
 * a java applet class for pacman
 */
public class applet extends Applet
implements ActionListener
{
	private static final long serialVersionUID = -749993332452315528L;

	static Pacman pacMan=null;

	public void init()
	{
		setSize(50,50);
		// create button
		setLayout(new FlowLayout(FlowLayout.CENTER));
		Button play=new Button("PLAY");
		add(play);

		play.addActionListener(this);

		//      newGame();
	}

	void newGame()
	{
		pacMan=new Pacman();
	}

	/////////////////////////////////////////////////
	// handles button event
	/////////////////////////////////////////////////
	public void actionPerformed(ActionEvent e)
	{
		if ( pacMan != null && ! pacMan.isFinalized() )
			// another is running
			return;
		newGame();
	}

}
