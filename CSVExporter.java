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

import org.antlr.runtime.ANTLRFileStream;
import org.antlr.runtime.CommonTokenStream;
import org.antlr.runtime.RecognitionException;
import org.antlr.runtime.Token;
import org.antlr.runtime.TokenStream;
import org.antlr.runtime.tree.CommonTreeAdaptor;

import java.io.*;

public class CSVExporter {
	
    static final String separator = "\t";
    
    public static void main(String[] args) throws IOException
	{
	    CodeTree codeTree = new CodeTree();
	    
	    try{
		
		String inputFilename = parseCommandLine(args);
		CPPGrammarParser.code_return ast = parseFile(inputFilename);
		codeTree.initializeFromAST(ast.tree);
		codeTree.print();
	    } catch (Exception e) {
		e.printStackTrace();
	    }
	}
	    
    private static String parseCommandLine(String[] args) throws Exception
	{
	    if(args.length != 1){
		throw new Exception("filename required.");
	    }
	    return args[0];
	}
    
    private static CPPGrammarParser.code_return parseFile(String inputFilename)
	throws IOException, RecognitionException{
	
	ANTLRFileStream antlrFileStream = new ANTLRFileStream(inputFilename) ;
	CPPGrammarLexer lexer = new CPPGrammarLexer(antlrFileStream);
	TokenStream tokenStream = new CommonTokenStream(lexer);
	CPPGrammarParser parser = new CPPGrammarParser(tokenStream );
	parser.setTreeAdaptor(new CommonTreeAdaptor()
	    {
		public Object create(Token payload) { return new CommonTreeWithLines(payload);}
	    });
	return parser.code();
    }
    
}


