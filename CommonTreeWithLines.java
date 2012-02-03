/*
    Copyright (C) 2012 Fabian 'fabs' Yamaguchi <fabs@phenoelit.de>
    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

import org.antlr.runtime.tree.CommonTree;

public class CommonTreeWithLines extends CommonTree
{
		
	 public CommonTreeWithLines(org.antlr.runtime.Token token) {
		// TODO Auto-generated constructor stub
		 super(token);
		 if(token == null) return;
		 this.lastColumn = token.getCharPositionInLine();
		 if(token.getText() != null) this.lastColumn += token.getText().length();
		 
	}
	 
     public int lastColumn;
}
