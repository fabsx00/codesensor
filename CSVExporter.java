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
		try{
			String inputFilename = parseCommandLine(args);
			CPPGrammarParser.code_return ast = parseFile(inputFilename);
			traverse((CommonTreeWithLines)ast.tree, 0);
			
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
			throws IOException, RecognitionException
	{
		
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

	
	private static void traverse(CommonTreeWithLines node, int level)	
	{
		switch(node.getType()){
		case CPPGrammarParser.FUNCTION_DEF:
			handleFunctionDef(node, level);
			break;
		case CPPGrammarParser.SELECTION:
			handleSelection(node, level);
			break;
		case CPPGrammarParser.ITERATION:
			handleIteration(node, level);
			break;
		case CPPGrammarParser.JUMP_STATEMENT:
			handleJump(node, level);
			break;
		case CPPGrammarParser.FUNCTION_CALL:
			handleFunctionCall(node, level);
			break;
		case CPPGrammarParser.LABEL:
			handleLabel(node, level);
			break;
		case CPPGrammarParser.SIMPLE_DECL:
			handleSimpleDecl(node, level);
			break;
		default:
			traverseChildren(node, level);
		};	
	}
	
	private static void handleSimpleDecl(CommonTreeWithLines node, int level)
	{
		CommonTreeWithLines typedefNode = (CommonTreeWithLines) node.getChild(0);
		CommonTreeWithLines templateNode = (CommonTreeWithLines) node.getChild(1);
		CommonTreeWithLines typeNode = (CommonTreeWithLines) node.getChild(2);
		CommonTreeWithLines classDefNode = (CommonTreeWithLines) node.getChild(3);
		CommonTreeWithLines initDeclNode = (CommonTreeWithLines) node.getChild(4);
		CommonTreeWithLines terminatorNode = (CommonTreeWithLines) node.getChild(5);
		CommonTreeWithLines startNode = typeNode;

		String typeStr = "";
		if(typeNode.getChildCount() > 0)
			typeStr = children2String(typeNode, true);
		
		if(classDefNode.getChildCount() > 0){
			typeStr = handleClassDef(classDefNode, level);
			startNode = classDefNode;
		}
		
		if(typedefNode.getChildCount() > 0){
			handleTypeDef(node, typeStr, level);
			return;
		}
		
		for(int i = 0; i < initDeclNode.getChildCount(); i++){
			CommonTreeWithLines declName = (CommonTreeWithLines) initDeclNode.getChild(i);
			
			if(declName.getType() != CPPGrammarParser.INIT_DECL_NAME){
				traverseChildren(declName, level);
				continue;
			}
			
			String declNameStr = children2String(declName, false);
			
			String startPos = startNode.getLine() + ":" + startNode.getCharPositionInLine();
			String endPos = terminatorNode.getLine() + ":" + terminatorNode.getCharPositionInLine();
			
			String csvLine = "decl" + separator + startPos;
			csvLine += separator + endPos;
			csvLine += separator + level;
			csvLine += separator + typeStr;
			csvLine += separator + declNameStr;
			System.out.println(csvLine);
		}
		
	}

	private static void handleTypeDef(CommonTreeWithLines node, String typeStr,
			int level)
	{
		CommonTreeWithLines typedefNode = (CommonTreeWithLines) node.getChild(0);
		CommonTreeWithLines initDeclNode = (CommonTreeWithLines) node.getChild(4);
		CommonTreeWithLines terminatorNode = (CommonTreeWithLines) node.getChild(5);
		
		String startPos = typedefNode.getLine() + ":" + typedefNode.getCharPositionInLine();
		String endPos = terminatorNode.getLine() + ":" + terminatorNode.getCharPositionInLine();
		
		String csvLine = "typedef" + separator + startPos;
		csvLine += separator + endPos;
		csvLine += separator + level;
		csvLine += separator + typeStr;
		csvLine += separator + children2String(initDeclNode, false);
		System.out.println(csvLine);
	}

	private static String handleClassDef(CommonTreeWithLines node, int level)
	{
		CommonTreeWithLines classKeyNode = (CommonTreeWithLines) node.getChild(0);
		CommonTreeWithLines classNameNode = (CommonTreeWithLines) node.getChild(1);
		CommonTreeWithLines baseClassesNode = (CommonTreeWithLines) node.getChild(2);
		CommonTreeWithLines classContentNode = (CommonTreeWithLines) node.getChild(4);
		CommonTreeWithLines terminatorNode = (CommonTreeWithLines) node.getChild(5);
		
		String startPos = classKeyNode.getLine() + ":" + classKeyNode.getCharPositionInLine();
		String endPos = terminatorNode.getLine() + ":" + terminatorNode.getCharPositionInLine();
		
		String className;
		if(classNameNode.getChildCount() > 0 )
			className = children2String(classNameNode, false);
		else
			className = "<anonymous_" + startPos + ">";
		
		String csvLine = classKeyNode.toString() + separator + startPos;
		csvLine += separator + endPos;
		csvLine += separator + level;
		csvLine += separator + className;
		System.out.println(csvLine);

		traverseChildren(classContentNode, level + 1);
		return className;
	}

	private static void handleFunctionDef(CommonTreeWithLines node, int level)
	{
		CommonTreeWithLines returnType = (CommonTreeWithLines) node.getChild(1);
		CommonTreeWithLines name = (CommonTreeWithLines) node.getChild(2);
		CommonTreeWithLines parameterList = (CommonTreeWithLines) node.getChild(3);
		CommonTreeWithLines functionContent = (CommonTreeWithLines) node.getChild(6);
		CommonTreeWithLines closingTag = (CommonTreeWithLines) node.getChild(7);
		
		String returnTypeStr = children2String(returnType, true);
		String nameStr = children2String(name, false);
		
		String startPos;
		if(!returnTypeStr.equals(""))
			startPos = returnType.getLine() + ":" + returnType.getCharPositionInLine();
		else
			startPos = name.getLine() + ":" + name.getCharPositionInLine();
		
		String csvLine = "func" + separator + startPos;
		csvLine += separator + closingTag.getLine() + ":" + closingTag.getCharPositionInLine();
		csvLine += separator + level;
		csvLine += separator + returnTypeStr + separator + nameStr;
		System.out.println(csvLine);

		handleParameterList(parameterList, nameStr, startPos, level + 1);
		
		traverseChildren(functionContent, level + 1);		
	}
	
	private static void handleParameterList(CommonTreeWithLines parameterList,
						String name, String startPos, int level)
	{
		int nChildren = parameterList.getChildCount();
		if(nChildren <= 2)
			return;
		for(int i = 1; i < nChildren; i+=2){
			CommonTreeWithLines paramDecl = (CommonTreeWithLines) parameterList.getChild(i);
			CommonTreeWithLines terminator = (CommonTreeWithLines) parameterList.getChild(i+1);
			CommonTreeWithLines paramType = (CommonTreeWithLines) paramDecl.getChild(0);
			CommonTreeWithLines paramName = (CommonTreeWithLines) paramDecl.getChild(1);
			
			String csvLine = "param" + separator + paramType.getLine() + ":" + paramType.getCharPositionInLine();
			csvLine += separator + terminator.getLine() + ":" + terminator.getCharPositionInLine();
			csvLine += separator + level;
			csvLine += separator + startPos + separator + name;
			csvLine += separator + children2String(paramType, true);
			if(paramName != null)
				 csvLine += separator + children2String(paramName, false);
			System.out.println(csvLine);
		}
	}

	private static void handleSelection(CommonTreeWithLines node, int level)
	{
		CommonTreeWithLines keyword = (CommonTreeWithLines) node.getChild(0);
		
		String keywordStr = children2String(keyword, false);
		
		if(keywordStr.equals("if") || keywordStr.equals("switch")){
			CommonTreeWithLines statements = (CommonTreeWithLines) node.getChild(2);
			handleIfOrWhile(node, level, keywordStr);		
			traverseChildren(statements, level + 1);
		}else if(keywordStr.equals("else")){
			CommonTreeWithLines statements = (CommonTreeWithLines) node.getChild(1);
			handleElse(node, level);
			traverseChildren(statements, level + 1);
		}
		
	}

	private static void handleIteration(CommonTreeWithLines node, int level)
	{
		CommonTreeWithLines keyword = (CommonTreeWithLines) node.getChild(0);
		
		String keywordStr = children2String(keyword, false);
		
		if(keywordStr.equals("while") || keywordStr.equals("do")){
			CommonTreeWithLines statements = (CommonTreeWithLines) node.getChild(2);
			handleIfOrWhile(node, level, keywordStr);		
			traverseChildren(statements, level + 1);
		}else if(keywordStr.equals("for")){
			CommonTreeWithLines statements = (CommonTreeWithLines) node.getChild(5);
			handleFor(node, level);		
			traverseChildren(statements, level + 1);
		}
		
	}
	
	
	private static void handleFor(CommonTreeWithLines node, int level)
	{
		CommonTreeWithLines keywordToken = (CommonTreeWithLines) node.getChild(0).getChild(0);
		CommonTreeWithLines forInitStatement = (CommonTreeWithLines) node.getChild(1);
		CommonTreeWithLines condition = (CommonTreeWithLines) node.getChild(2);
		CommonTreeWithLines expr = (CommonTreeWithLines) node.getChild(4);
		CommonTreeWithLines statements = (CommonTreeWithLines) node.getChild(5);
		CommonTreeWithLines terminator = getTerminatorNode(statements);
		String csvLine = "";
		csvLine += "for" + separator +  keywordToken.getLine() + ":" + keywordToken.getCharPositionInLine();
		csvLine += separator + terminator.getLine() + ":" + terminator.getCharPositionInLine();
		csvLine += separator + level;
		csvLine += separator + "(" + children2String(forInitStatement, true);
		csvLine += children2String(condition, true) + ";";
		csvLine += children2String(expr, true);
		csvLine +=  ")";
		System.out.println(csvLine);
	
		traverseChildren(forInitStatement, level);
		traverseChildren(condition, level);
		traverseChildren(expr, level);
		
	
	}

	private static void handleIfOrWhile(CommonTreeWithLines node, int level, String keywordStr)
	{
		CommonTreeWithLines keywordToken = (CommonTreeWithLines) node.getChild(0).getChild(0);
		CommonTreeWithLines condition = (CommonTreeWithLines) node.getChild(1);
		CommonTreeWithLines statements = (CommonTreeWithLines) node.getChild(2); 
		CommonTreeWithLines terminator = getTerminatorNode(statements);
		String csvLine = "";
		csvLine += keywordStr + separator +  keywordToken.getLine() + ":" + keywordToken.getCharPositionInLine();
		csvLine += separator + terminator.getLine() + ":" + terminator.getCharPositionInLine();
		csvLine += separator + level;
		csvLine += separator + "(" + children2String(condition, true) + ")";
		System.out.println(csvLine);
	
		traverseChildren(condition, level);
	}
	
	private static void handleElse(CommonTreeWithLines node, int level)
	{
		CommonTreeWithLines keywordToken = (CommonTreeWithLines) node.getChild(0).getChild(0);
		CommonTreeWithLines statements = (CommonTreeWithLines) node.getChild(1); 
		CommonTreeWithLines terminator = getTerminatorNode(statements);
		String csvLine = "";
		csvLine += "else" + separator +  keywordToken.getLine() + ":" + keywordToken.getCharPositionInLine();
		csvLine += separator + terminator.getLine() + ":" + terminator.getCharPositionInLine();
		csvLine += separator + level;
		System.out.println(csvLine);
	}
	
	private static void handleJump(CommonTreeWithLines node, int level)
	{
		CommonTreeWithLines keywordToken = (CommonTreeWithLines) node.getChild(0);
		String keywordStr = children2String(keywordToken, false);
		CommonTreeWithLines destination = (CommonTreeWithLines) node.getChild(1);
		CommonTreeWithLines terminator = (CommonTreeWithLines) node.getChild(2);
		
		String csvLine = "";
		csvLine += keywordStr + separator +  keywordToken.getLine() + ":" + keywordToken.getCharPositionInLine();
		csvLine += separator + terminator.getLine() + ":" + terminator.getCharPositionInLine();
		csvLine += separator + level;
		csvLine += separator + children2String(destination, false);
		System.out.println(csvLine);
		
		traverseChildren(destination, level);
	}

	private static void handleFunctionCall(CommonTreeWithLines node, int level)
	{
		CommonTreeWithLines callee = (CommonTreeWithLines) node.getChild(0);
		CommonTreeWithLines argumentList = (CommonTreeWithLines) node.getChild(2);
		CommonTreeWithLines terminator = (CommonTreeWithLines) getClosingBracketNode(argumentList);
		
		String calleeStr = children2String(callee, false);
		String calleePos = callee.getLine() + ":" + callee.getCharPositionInLine();
		String csvLine = "";
		csvLine += "call" + separator +  calleePos;
		csvLine += separator + terminator.getLine() + ":" + terminator.getCharPositionInLine();
		csvLine += separator + level;
		csvLine += separator + calleeStr;
		csvLine += separator + "(" + children2String(argumentList, false) + ")";
		System.out.println(csvLine);
		
		handleArgumentList(argumentList, calleeStr, calleePos, level);
		// parse expressions contained in arguments
		traverseChildren(argumentList, level);
	}
	
	
	
	private static void handleArgumentList(CommonTreeWithLines argumentList,
									String name, String startPos, int level)
	{
		int nChildren = argumentList.getChildCount();
		if(nChildren <= 2)
			return;
		for(int i = 1; i < nChildren; i+=2){
			CommonTreeWithLines argument = (CommonTreeWithLines) argumentList.getChild(i);
			CommonTreeWithLines terminator = (CommonTreeWithLines)argumentList.getChild(i+1);
			
			String csvLine = "arg" + separator + argument.getLine() + ":" + argument.getCharPositionInLine();
			csvLine += separator + terminator.getLine() + ":" + terminator.getCharPositionInLine();
			csvLine += separator + level;
			csvLine += separator + startPos + separator + name;
			csvLine += separator + children2String(argument, false);
			System.out.println(csvLine);
		
			traverseChildren(argument, level);
		}
	}

	private static void handleLabel(CommonTreeWithLines node, int level)
	{
		String labelStr = children2String(node, true);
		CommonTreeWithLines closingNode = (CommonTreeWithLines) node.getChild(node.getChildCount() -1);
		
		String startPos = node.getLine() + ":" + node.getCharPositionInLine();
		String endPos = closingNode.getLine() + ":" + closingNode.getCharPositionInLine();
		
		String csvLine = "label" + separator + startPos;
		csvLine += separator + endPos + separator + labelStr;
		System.out.println(csvLine);
	}	
	
	private static CommonTreeWithLines getTerminatorNode(CommonTreeWithLines node)
	{
		if(isLeaf(node)) return null;
		
		CommonTreeWithLines lastChild = (CommonTreeWithLines)node.getChild(node.getChildCount() - 1);
		String lastChildText = lastChild.toString();
		if(lastChildText.equals(";")) return lastChild;
		if(lastChildText.equals("}")) return lastChild;
		
		return getTerminatorNode(lastChild);
	}
	
	private static CommonTreeWithLines getClosingBracketNode(CommonTreeWithLines node)
	{
		if(isLeaf(node)) return null;
		
		CommonTreeWithLines lastChild = (CommonTreeWithLines)node.getChild(node.getChildCount() - 1);
		String lastChildText = lastChild.toString();
		if(lastChildText.equals(")")) return lastChild;
			
		return getTerminatorNode(lastChild);
	}
	
	private static String children2String(CommonTreeWithLines node, boolean addSpace) {
		int nChildren = node.getChildCount();
		if(nChildren == 0) return "";
		
		CommonTreeWithLines firstChild = (CommonTreeWithLines) node.getChild(0);
		String retval = "";
		if(isLeaf(firstChild)){
			if(shouldPrintChild(firstChild.toString()))
				retval = node.getChild(0).toString();
		}else
			retval = children2String(firstChild, addSpace);
		
		for(int i = 1; i < node.getChildCount(); i++){
			CommonTreeWithLines child = (CommonTreeWithLines)node.getChild(i);
			if(isLeaf(child)){
				if(addSpace) retval += " ";
				
				if(shouldPrintChild(child.toString()))
				    retval += child.toString();
			}
			else
				retval += children2String(child, addSpace);
		}
		return retval;
	}

	private static boolean shouldPrintChild(String s)
        {
		if(s.equals("CALL_TEMPLATE_LIST"))
			return false;
		if(s.equals("TYPE_DEF"))
			return false;
		if(s.equals("TEMPLATE_DECL_SPECIFIER"))
			return false;
		if(s.equals("CLASS_DEF"))
			return false;
		
		return true;
	}

	private static void traverseChildren(CommonTreeWithLines node, int level)
	{
			
		for(int i = 0; i < node.getChildCount(); i++){
			CommonTreeWithLines child = (CommonTreeWithLines)node.getChild(i);
				traverse(child, level);
			}
	}
	
	private static boolean isLeaf(CommonTreeWithLines node)
	{
		return (node.getChildCount() == 0);
	}
	
	
}


