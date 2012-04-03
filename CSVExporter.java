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

import java.util.HashMap;
import java.io.*;


public class CSVExporter {
	
	static final String separator = "\t";
	static HashMap<String,Integer> operatorMap;
	
	public static void main(String[] args) throws IOException{		
		try{
			initializeOperatorMap();
			String inputFilename = parseCommandLine(args);
			CPPGrammarParser.code_return ast = parseFile(inputFilename);
			traverse((CommonTreeWithLines)ast.tree, 0);
			
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	static void initializeOperatorMap() throws Exception{		
		operatorMap = new HashMap<String,Integer>();
		operatorMap.put("+", 1); operatorMap.put("-", 1); operatorMap.put("*", 1);
		operatorMap.put("/", 1); operatorMap.put("%", 1);			
		operatorMap.put("++", 1); operatorMap.put("--", 1); operatorMap.put("=", 1);
		operatorMap.put("+=", 1); operatorMap.put("-=", 1); operatorMap.put("%=", 1);
		operatorMap.put("*=", 1); operatorMap.put("/=", 1); operatorMap.put("&=", 1);		
		operatorMap.put("<<=", 1); operatorMap.put(">>=", 1); operatorMap.put("^=", 1);
		operatorMap.put("|=", 1);						
		operatorMap.put("==", 1); operatorMap.put("!=", 1); operatorMap.put("<=", 1);
		operatorMap.put(">=", 1); operatorMap.put("<", 1); operatorMap.put(">", 1);
		operatorMap.put("&&", 1); operatorMap.put("||", 1); operatorMap.put("!", 1);
		
		operatorMap.put("&", 1); operatorMap.put("|", 1); operatorMap.put("^", 1);
		operatorMap.put("~", 1); operatorMap.put("<<", 1); operatorMap.put(">>", 1);

		operatorMap.put("*", 1); operatorMap.put(".", 1); operatorMap.put("->", 1);
		operatorMap.put("::", 1); operatorMap.put(".*", 1); operatorMap.put("->*", 1);
		operatorMap.put("&", 1); operatorMap.put("sizeof", 1); operatorMap.put("new", 1);
		operatorMap.put("delete", 1); operatorMap.put("[", 1); operatorMap.put("]", 1);
		
	}
    
	static boolean isOperator(String s){
		return operatorMap.containsKey(s);
	}

	private static String parseCommandLine(String[] args) throws Exception{
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

	
	private static void traverse(CommonTreeWithLines node, int level){
	    
	    if(isLeaf(node)){
		outputLeafNode(node, level);
		return;
	    }		
		
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
		case CPPGrammarParser.SW:
			handleStatementWater(node, level);
			break;	       
		default:
			traverseChildren(node, level);
		};	
	}

	private static void handleSelection(CommonTreeWithLines node, int level)
	{
		CommonTreeWithLines keyword = (CommonTreeWithLines) node.getChild(0);
		
		String keywordStr = children2String(keyword, false);
		
		if(keywordStr.equals("if") || keywordStr.equals("switch")){
			CommonTreeWithLines statements = (CommonTreeWithLines) node.getChild(2);
			handleIfOrWhile(node, level, keywordStr);		
			outputPseudoNode("stmts", statements, "", level + 1);
			
		}else if(keywordStr.equals("else")){
			CommonTreeWithLines statements = (CommonTreeWithLines) node.getChild(1);
			handleElse(node, level);
			outputPseudoNode("stmts", statements, "", level + 1);			
		}
		
	}

	private static void handleIteration(CommonTreeWithLines node, int level)
	{
		CommonTreeWithLines keyword = (CommonTreeWithLines) node.getChild(0);		
		String keywordStr = children2String(keyword, false);
		
		if(keywordStr.equals("while") || keywordStr.equals("do")){
			CommonTreeWithLines statements = (CommonTreeWithLines) node.getChild(2);
			handleIfOrWhile(node, level, keywordStr);		
			outputPseudoNode("stmts", statements, "", level + 1);						
		}else if(keywordStr.equals("for")){
			CommonTreeWithLines statements = (CommonTreeWithLines) node.getChild(5);
			handleFor(node, level);		
			outputPseudoNode("stmts", statements, "", level + 1);			
		}
		
	}
		
	static void outputCSVRow(String rowType, String startPos, String endPos, String level, String content){
		String csvLine = rowType;
		
		csvLine += separator + startPos;
		csvLine += separator + endPos;
		csvLine += separator + level;
		csvLine += separator + content;
		System.out.println(csvLine);		
	}
	
	
	private static void handleStatementWater(CommonTreeWithLines node, int level){
		// type, start position, end position, level, text
		CommonTreeWithLines swContentNode = (CommonTreeWithLines) node.getChild(0);				
		outputLeafNode(swContentNode, level);
	}
	
	static void outputLeafNode(CommonTreeWithLines node, int level){
		
		if(!shouldPrintChild(node.toString())) return;
		
		String startPos = node.getLine() + ":" + node.getCharPositionInLine();
		String endPos = "0:0";
		String content = node.toString();						
		String rowType;
		if(isOperator(content)) rowType = "op";
		else rowType = "water";
		content = content.replaceAll("\n", "_n");
		outputCSVRow(rowType, startPos, endPos, Integer.toString(level), content);
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
		
		// INIT_DECL_LIST
		for(int i = 0; i < initDeclNode.getChildCount(); i++){
			CommonTreeWithLines declName = (CommonTreeWithLines) initDeclNode.getChild(i);
			
			if(declName.getType() != CPPGrammarParser.INIT_DECL_NAME){
				
				if(isLeaf(declName))
					outputLeafNode(declName, level);
				else				
				    // traverseChildren(declName, level + 1);
				    traverse(declName, level + 1);

				continue;
			}			

			String declNameStr = children2String(declName, false);			
			String startPos = startNode.getLine() + ":" + startNode.getCharPositionInLine();
			String endPos = terminatorNode.getLine() + ":" + terminatorNode.getCharPositionInLine();
			String content = typeStr + separator + declNameStr;
			
			outputCSVRow("decl", startPos, endPos, Integer.toString(level), content);												
		}
		
	}

	private static void handleTypeDef(CommonTreeWithLines node, String typeStr,
					  int level){
		CommonTreeWithLines typedefNode = (CommonTreeWithLines) node.getChild(0);
		CommonTreeWithLines initDeclNode = (CommonTreeWithLines) node.getChild(4);
		CommonTreeWithLines terminatorNode = (CommonTreeWithLines) node.getChild(5);
		
		String startPos = typedefNode.getLine() + ":" + typedefNode.getCharPositionInLine();
		String endPos = terminatorNode.getLine() + ":" + terminatorNode.getCharPositionInLine();				
		String content = typeStr + separator + children2String(initDeclNode, false);
		
		outputCSVRow("typedef", startPos, endPos, Integer.toString(level), content);		
	}

	private static String handleClassDef(CommonTreeWithLines node, int level){
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
		
		String classKey = classKeyNode.toString();
		String content = className;

		outputCSVRow(classKey, startPos, endPos, Integer.toString(level), content);
		
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
		
		String endPos = closingTag.getLine() + ":" + closingTag.getCharPositionInLine();
		String content = returnTypeStr + separator + nameStr;
		
		outputCSVRow("func", startPos, endPos, Integer.toString(level), content);
		handleParameterList(parameterList, level + 1);
		outputPseudoNode("stmnts", functionContent, "", level + 1);		
	}
	
	
	private static void handleParameterList(CommonTreeWithLines parameterList,
						int level)
	{
		int nChildren = parameterList.getChildCount();
		if(nChildren <= 2)
			return;
		
		String csvLine = "params" + separator + parameterList.getLine()  + ":" + parameterList.getCharPositionInLine();
		csvLine += separator + "0" + ":" + "0";
		csvLine += separator + level;
		csvLine += separator + "";
		System.out.println(csvLine);		
		level++;
		
		for(int i = 1; i < nChildren; i+=2){
			CommonTreeWithLines paramDecl = (CommonTreeWithLines) parameterList.getChild(i);
			CommonTreeWithLines terminator = (CommonTreeWithLines) parameterList.getChild(i+1);
			CommonTreeWithLines paramType = (CommonTreeWithLines) paramDecl.getChild(0);
			CommonTreeWithLines paramName = (CommonTreeWithLines) paramDecl.getChild(1);
			
			String startPos = paramType.getLine() + ":" + paramType.getCharPositionInLine();
			String endPos = terminator.getLine() + ":" + terminator.getCharPositionInLine();
			String content = children2String(paramType, true);
			
			if(paramName != null)
				content += separator + children2String(paramName, false);
						
			outputCSVRow("param", startPos, endPos, Integer.toString(level), content);						
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
		
		String conditionStr = children2String(condition, true);
		String forInitStr = children2String(forInitStatement, true);
		String exprStr = children2String(expr, true);
		
		String csvLine = "";
		csvLine += "for" + separator +  keywordToken.getLine() + ":" + keywordToken.getCharPositionInLine();
		csvLine += separator + terminator.getLine() + ":" + terminator.getCharPositionInLine();
		csvLine += separator + level;
		csvLine += separator + '(' + forInitStr;
		csvLine += conditionStr + ";";
		csvLine += exprStr;
		csvLine +=  ")";
		System.out.println(csvLine);
		
		
		outputPseudoNode("forinit", forInitStatement, forInitStr, level + 1);
		outputPseudoNode("cond", condition, conditionStr, level + 1);
		outputPseudoNode("forexpr", expr, exprStr, level + 1);		
		
	}

	private static void handleIfOrWhile(CommonTreeWithLines node, int level, String keywordStr)
	{
		CommonTreeWithLines keywordToken = (CommonTreeWithLines) node.getChild(0).getChild(0);
		CommonTreeWithLines condition = (CommonTreeWithLines) node.getChild(1);
		CommonTreeWithLines statements = (CommonTreeWithLines) node.getChild(2); 
		CommonTreeWithLines terminator = getTerminatorNode(statements);
		
		String conditionStr = children2String(condition, true);
		
		String csvLine = "";
		csvLine += keywordStr + separator +  keywordToken.getLine() + ":" + keywordToken.getCharPositionInLine();
		csvLine += separator + terminator.getLine() + ":" + terminator.getCharPositionInLine();
		csvLine += separator + level;
		csvLine += separator + "(" + conditionStr + ")";
		System.out.println(csvLine);
		
		outputPseudoNode("cond", condition, conditionStr, level + 1);
		
	}
	
	private static void outputPseudoNode(String type, CommonTreeWithLines condition, String conditionStr, int level)
		{
		CommonTreeWithLines conditionTerminator = getTerminatorNode(condition);
		String csvLine;
		csvLine = type + separator + condition.getLine()  + ":" + condition.getCharPositionInLine();
		csvLine += separator + conditionTerminator.getLine() + ":" + conditionTerminator.getCharPositionInLine();
		csvLine += separator + level;
		csvLine += separator + conditionStr;
		System.out.println(csvLine);

		traverseChildren(condition, level + 1);

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
		
		traverseChildren(destination, level + 1);
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
		// csvLine += separator + "(" + children2String(argumentList, false) + ")";
		System.out.println(csvLine);
		
		handleArgumentList(argumentList, level + 1);
		// parse expressions contained in arguments
		
	}
	
		
	private static void handleArgumentList(CommonTreeWithLines argumentList, int level)
	{
		int nChildren = argumentList.getChildCount();
		if(nChildren <= 2)
			return;
		for(int i = 1; i < nChildren; i+=2){
			CommonTreeWithLines argument = (CommonTreeWithLines) argumentList.getChild(i);
			CommonTreeWithLines terminator = (CommonTreeWithLines)argumentList.getChild(i+1);
			String content;
					

			String csvLine = "arg" + separator + argument.getLine() + ":" + argument.getCharPositionInLine();
			csvLine += separator + terminator.getLine() + ":" + terminator.getCharPositionInLine();
			csvLine += separator + level;
			
			content = children2String(argument, false);
			content = content.replaceAll("\n", "_n");
			csvLine += separator + content;
			System.out.println(csvLine);
		
			traverseChildren(argument, level + 1);
		}
	}

	private static void handleLabel(CommonTreeWithLines node, int level)
	{
		String labelStr = children2String(node, true);
		CommonTreeWithLines closingNode = (CommonTreeWithLines) node.getChild(node.getChildCount() -1);
		
		String startPos = node.getLine() + ":" + node.getCharPositionInLine();
		String endPos = closingNode.getLine() + ":" + closingNode.getCharPositionInLine();
		
		String csvLine = "label" + separator + startPos;
		csvLine += separator + endPos;
		csvLine += separator + level;
		csvLine += separator + labelStr;
		System.out.println(csvLine);
	}	
	
	private static CommonTreeWithLines getTerminatorNode(CommonTreeWithLines node)
	{
		if(isLeaf(node)) return node;
		
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


