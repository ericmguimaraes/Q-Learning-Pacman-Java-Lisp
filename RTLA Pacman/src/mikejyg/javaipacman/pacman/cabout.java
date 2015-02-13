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
import java.awt.event.*;

// to display about information
class cabout extends Window
implements MouseListener
{
	private static final long serialVersionUID = -6444989674095739037L;

	final String[] about = {
			"",
			"javaiPacman",
			"",
			"  - Copyright 1997-2010 Junyang Gu <mikejyg@gmail.com>",
			"",
			"an intelligent pacman game implmented in Java",
			""
	};

	cabout(Frame parent)
	{
		super(parent);

		setSize(420, 280);
		setLocation(100, 100);
		show();

		addMouseListener(this);
	}

	public void paint(Graphics g)
	{
		g.setColor(Color.black);
		g.setFont(new Font("Helvetica", Font.BOLD, 12));
		for (int i=0; i<about.length; i++)
			g.drawString(about[i], 6, (i+1)*18);
	}

	public void mouseClicked(MouseEvent e)
	{
		dispose();
		// e.consume();
	}

	public void mousePressed(MouseEvent e) 
	{}

	public void mouseReleased(MouseEvent e) 
	{}
	public void mouseEntered(MouseEvent e) {}
	public void mouseExited(MouseEvent e) {}

}



