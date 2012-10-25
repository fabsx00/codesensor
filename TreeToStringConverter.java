
class TreeToStringConverter
{
    public static String buildConditionString(CommonTreeWithLines condition)
    {
	CommonTreeWithLines andNode = (CommonTreeWithLines) condition.getChild(0);		
	return buildAndString(andNode);			
    }
    
    public static String buildAndString(CommonTreeWithLines node)
    {
	String retval = "";
	for(int i = 0; i < node.getChildCount(); i++)
	    {
		if(i > 0) retval += " && ";
		CommonTreeWithLines childNode = (CommonTreeWithLines) node.getChild(i);
		retval += buildOrString(childNode);
	    }	
	return retval;
    }
    
    public static String buildOrString(CommonTreeWithLines node)
    {
	String retval = "";
	for(int i = 0; i < node.getChildCount(); i++)
	    {
		if(i > 0) retval += " || ";
		CommonTreeWithLines childNode = (CommonTreeWithLines) node.getChild(i);
		retval += buildExprElemString(childNode);
	    }	
	return retval;
    }
    
    public static String buildExprElemString(CommonTreeWithLines node)
    {
	String retval = "";
	for(int i = 0; i < node.getChildCount(); i++)
	    {
		if(i > 0) retval += " ";
		CommonTreeWithLines childNode = (CommonTreeWithLines) node.getChild(i);
		retval += buildExprElemChildString(childNode);
	    }	
	return retval;
    }

    public static String buildExprElemChildString(CommonTreeWithLines node)
    {						
	switch(node.getType())
	    {
	    case CPPGrammarParser.BRACKETS:				
		return "(" + buildConditionString(node) + ')';				
	    case CPPGrammarParser.CURLIES:
		return "{" + buildConditionString(node) + '}';				
	    case CPPGrammarParser.SQUARES:
		return "[" + buildConditionString(node) + ']';
	    case CPPGrammarParser.FUNCTION_CALL:
		return buildFunctionCallString(node);			
	    }	
	
	if(shouldPrintLeaf(node.toString()))
	    return node.toString();
	return "";
    }
    
    
    public static String buildFunctionCallString(CommonTreeWithLines node)
    {
	String retval = "";			
	CommonTreeWithLines callee = (CommonTreeWithLines) node.getChild(0);
	CommonTreeWithLines argumentList = (CommonTreeWithLines) node.getChild(2);       		
	
	String calleeStr = leaves2String(callee, false);
	retval += calleeStr;
	retval += buildArgumentListString(argumentList);
	return retval;
    }
    
    public static String buildArgumentListString(CommonTreeWithLines node)
    {
	return leaves2String(node, false);
    }
    
    
    /*
      calls toString() on all leaves and concatenates
      these substrings.
    */
    
    public static String leaves2String(CommonTreeWithLines node, boolean addSpace)
	{
	    String retval = "";	    
	    int nChildren = node.getChildCount();
	    if(nChildren == 0) return "";
	    
	    for(int i = 0; i < node.getChildCount(); i++){
		CommonTreeWithLines child = (CommonTreeWithLines)node.getChild(i);
						
		if(isLeaf(child)){
		    if(addSpace && i >0) retval += " ";
		    
		    if(shouldPrintLeaf(child.toString()))
			retval += child.toString();
		}
		else
		    retval += leaves2String(child, addSpace);
	    }	    
	    return retval;
	}
    
    public static boolean shouldPrintLeaf(String s)
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
    
    private static boolean isLeaf(CommonTreeWithLines node)
    {
	return (node.getChildCount() == 0);
    }
    
}
