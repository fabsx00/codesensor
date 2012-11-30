grammar CPPGrammar;

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

options {
 output = AST;
 ASTLabelType = CommonTree;
 language=Java;
}

tokens{
  SOURCE_FILE;
  
  FUNCTION_DEF; FUNCTION_NAME; RETURN_TYPE;
  CTOR_LIST; STATEMENTS; INITIALIZER_ID;
  CTOR_INITIALIZER;
  
  NAME; PARAMETER_LIST; PARAMETER_DECL;
  CALLEE;
  ARGUMENT_LIST; ARGUMENT; CALL_TEMPLATE_LIST;
  INIT; VAR_DECL; POINTER; TYPE_SUFFIX;

  SIMPLE_DECL; NAMESPACE_DEF; USING_DIRECTIVE;
  INCLUDE_DIRECTIVE; TEMPLATE_DECL_SPECIFIER;
  
  SELECTION; ITERATION;
  KEYWORD; SWITCH; FOR_INIT;
  FOR_EXPR; JUMP_STATEMENT; DESTINATION;
  CONDITION; LABEL; EXPR_STATEMENT;

  CTOR_EXPR; FUNCTION_CALL; CLASS_DEF;
  CLASS_NAME; TYPE_DEF; BASE_CLASSES;
  CLASS_CONTENT; TYPE_NAME; TYPE; INIT_DECL_LIST;
  NON_FUNC_CALL;

  BIT_OR; BIT_OR_ELEM;

  BRACKETS; CURLIES; SQUARES;
  AND; OR; COND_EXPR; OR_ELEM; AND_ELEM;
  ASSIGN; ASSIGN_OP; LVAL; RVAL;
}

code  : part* -> ^(SOURCE_FILE part*);

part: (declaration)=> declaration
     | water!
     ;

declaration: (simple_decl) => simple_decl
    | function_def
    | namespace_def
    | using_directive
    | include_directive
  ;

using_directive_: 'using' 'namespace' identifier ';';
include_directive_: '#include' ('"' (ALPHA_NUMERIC | '.')+  '"' | '<' (ALPHA_NUMERIC | '.')+ '>');

// Declarations

simple_decl_:
    'typedef' template_var_decl ';' -> ^(TYPE_DEF 'typedef') template_var_decl ';'
    | template_var_decl ';'         -> template_var_decl ';'
    | var_decl ';'                  -> var_decl ';'
    ;

template_var_decl: template_declaration_start var_decl
        -> ^(TEMPLATE_DECL_SPECIFIER template_declaration_start) var_decl;

var_decl
scope{
    CommonTree typeSpec;
}
    :
    (t0=type_name {$var_decl::typeSpec=(CommonTree)t0.getTree();} )
       init_declarator_list
            -> init_declarator_list
    | (t1=class_def {$var_decl::typeSpec=(CommonTree)t1.getTree();}
            -> ^(CLASS_DEF class_def)) (init_declarator_list -> init_declarator_list)?
        ;

template_declaration_start: 'template' '<' template_param_list '>' ;

class_def: class_def_ -> ^(CLASS_DEF class_def_);
class_def_: class_key class_name base_classes '{' class_content '}';
class_name: identifier? -> ^(CLASS_NAME identifier?);

class_content: class_content_ -> ^(CLASS_CONTENT class_content_?);
class_content_: class_content_elem*;

class_content_elem:  (simple_decl) => simple_decl
  | (function_def) => function_def
  | (label) => label
  | '{' class_content_ '}'
  | no_curlies
  ;

base_classes: base_classes_? -> ^(BASE_CLASSES base_classes_?); 
base_classes_: ':' base_class (',' base_class)*;
base_class: 'virtual'? access_specifier? identifier;

init_declarator_list:
  (init_declarator -> init_declarator)
        (',' init_declarator_list -> ^(INIT_DECL_LIST init_declarator ',' init_declarator_list))?;

init_declarator: init_declarator_ -> ^(VAR_DECL init_declarator_);

init_declarator_
scope{
    CommonTree pointer;
    CommonTree type_suffix;
}
: ((init_decl_name -> init_decl_name) 
        (
            ( ('(' expr? ')')
                    -> init_decl_name STRING[";"] ^(INIT ^(ASSIGN ^(LVAL init_decl_name) ^(ASSIGN_OP init_decl_name) ^(RVAL '(' expr? ')')) ))
        |   ( ('=' assign_expr)
                    -> init_decl_name STRING[";"] ^(INIT ^(ASSIGN ^(LVAL init_decl_name) ^(ASSIGN_OP '=') ^(RVAL assign_expr))) )
        
        )?) -> {$init_declarator_::pointer != null && $init_declarator_::type_suffix == null}?
                    ^(TYPE {$var_decl::typeSpec} ^(POINTER {$init_declarator_::pointer})) $init_declarator_
            -> {$init_declarator_::pointer != null && $init_declarator_::type_suffix != null}?
                    ^(TYPE {$var_decl::typeSpec} ^(POINTER {$init_declarator_::pointer}) ^(TYPE_SUFFIX {$init_declarator_::type_suffix})) $init_declarator_
            -> {$init_declarator_::pointer == null && $init_declarator_::type_suffix != null}?
                    ^(TYPE {$var_decl::typeSpec} ^(TYPE_SUFFIX {$init_declarator_::type_suffix})) $init_declarator_
            -> ^(TYPE {$var_decl::typeSpec}) $init_declarator_
    ;

init_decl_name_: ( p=ptr_operator* identifier) s=type_suffix?
        {
            if(p != null) $init_declarator_::pointer = (CommonTree) p.getTree();
            if(s != null) $init_declarator_::type_suffix = (CommonTree) s.getTree();
        }
        -> identifier
;

type_suffix: ('[' constant_expr? ']');

// Function Definitions
     
function_def_ : (function_start function_param_list)
        ( ctor_list compound_statement -> function_start function_param_list ^(CTOR_LIST ctor_list) compound_statement
        | compound_statement -> function_start function_param_list compound_statement);

function_start: template_declaration_start return_type function_name -> ^(TEMPLATE_DECL_SPECIFIER template_declaration_start) return_type function_name
    | return_type function_name -> return_type function_name
    ;
        
return_type: return_type_ -> ^(RETURN_TYPE return_type_?);
return_type_: (function_decl_specifiers* type_name)? function_decl_specifiers* ptr_operator*;

function_name_: '(' function_name_ ')' | identifier | operator_function_id;
function_param_list : o='(' parameter_declaration_clause? c=')' cv_qualifier* exception_specification? ->
                    ^( PARAMETER_LIST $o parameter_declaration_clause? $c);

exception_specification : 'throw' '(' type_id_list ')';

ctor_list: ':'  ctor_initializer (',' ctor_initializer)* -> ctor_initializer+;
ctor_initializer:  '::'? identifier '(' expr? ')' -> ^(CTOR_INITIALIZER ^(INITIALIZER_ID '::'? identifier) ^(CTOR_EXPR expr?) );

parameter_declaration_clause: parameter_decl (',' parameter_decl)*;

parameter_decl_
scope{ CommonTree type_suffix;}
:
param_decl_specifiers
        ( (ptr_operator+ parameter_name -> {$parameter_decl_::type_suffix == null}?  ^(TYPE param_decl_specifiers ^(POINTER ptr_operator+)) parameter_name
                                        -> ^(TYPE param_decl_specifiers ^(POINTER ptr_operator+) ^(TYPE_SUFFIX {$parameter_decl_::type_suffix})) parameter_name)
        | (parameter_name -> {$parameter_decl_::type_suffix == null}? ^(TYPE param_decl_specifiers) parameter_name
                          -> ^(TYPE param_decl_specifiers ^(TYPE_SUFFIX {$parameter_decl_::type_suffix})) parameter_name))
        | identifier; // common case: some macro. Also catches void. 


parameter_name_: x=parameter_name_start s=type_suffix?
{if(s != null) $parameter_decl_::type_suffix = (CommonTree) s.getTree();} -> $x
;

parameter_name_start: ('(' parameter_name_ ')' | identifier);
param_decl_specifiers: ('auto' | 'register')? type_name;

// statement

statement: compound_statement
         | non_compound_statement
;

compound_statement: '{' statement* '}' -> '{' ^(STATEMENTS statement*) '}';

non_compound_statement:  (non_expr_statement) => non_expr_statement
    | (expr_statement) => expr_statement
    | statement_water
;

non_expr_statement: selection_statement | iteration_statement
    | jump_statement | try_block | catch_block | simple_decl | label
;

statement_water: identifier | ~(ALPHA_NUMERIC | '::' | '{' | '}');

expr_statement: expr_statement_ -> ^(EXPR_STATEMENT expr_statement_);
expr_statement_: {!(input.LT(1).getText().equals("{"))}?=> expr ';';

selection_statement: selection_statement_ -> ^(SELECTION selection_statement_);
selection_statement_: if_statement | else_statement | switch_statement;

iteration_statement: iteration_statement_ -> ^(ITERATION iteration_statement_);
iteration_statement_: for_statement | while_statement | do_statement;

jump_statement_: ( break_or_continue | return_statement |goto_statement ) ';';
break_or_continue: (k='break' | k='continue') -> ^(KEYWORD $k) ^(DESTINATION);
return_statement: k='return' expr? -> ^(KEYWORD $k) ^(DESTINATION expr?);
goto_statement:  k='goto' ( d=identifier) -> ^(KEYWORD $k) ^(DESTINATION $d?);

try_block: 'try' compound_statement;
catch_block: 'catch' '('param_decl_specifiers parameter_name? ')' compound_statement;

if_statement: k='if' '(' condition ')'  statement -> ^(KEYWORD $k) '(' condition ')' ^(STATEMENTS statement?) ;
else_statement: k='else' statement -> ^(KEYWORD $k) ^(STATEMENTS statement?);
switch_statement: k='switch' '(' condition ')' statement -> ^(KEYWORD $k ) '(' condition ')' ^(STATEMENTS statement?);

for_statement: k='for' '(' for_init_statement condition ';'  expr? ')' statement
            -> ^(KEYWORD $k) '(' ^(FOR_INIT for_init_statement?) condition ';' ^(FOR_EXPR expr?) ')' ^(STATEMENTS statement?);
while_statement: k='while' '(' condition ')' statement -> ^(KEYWORD $k) '(' condition ')' ^(STATEMENTS statement?);
do_statement: k='do' statement 'while' '(' expr ')' -> ^(KEYWORD $k) '(' ^(CONDITION expr) ')' ^(STATEMENTS statement?) ;


for_init_statement : (simple_decl) => simple_decl | expr? ';';

label_: (('case'? (identifier | number) ) | access_specifier) ':' ;

type_id_list: no_brackets* ('(' type_id_list ')' no_brackets*)*;
namespace_def_: 'namespace' identifier? '{' namespace_content '}';
namespace_content: namespace_content_elem* ('{' namespace_content '}' namespace_content_elem*)*;
namespace_content_elem: (simple_decl) => simple_decl
  | (function_def) => function_def
  | no_curlies;
  
type_name: type_name_ -> ^(TYPE_NAME type_name_);

type_name_: cv_qualifier* class_key? ('unsigned' | 'signed')? ALPHA_NUMERIC ('<' template_param_list '>' )? ('::' ALPHA_NUMERIC ('<' template_param_list '>' )?)* ;
template_param_list : template_param_list_elem*;
template_param_list_elem:  ('<' template_param_list '>')
         | ('(' template_param_list ')')
         | no_angle_brackets_or_brackets
;

// Expressions

condition_: expr;
expr: assign_expr (',' assign_expr)?;

assign_expr: (conditional_expression -> conditional_expression )
        (assignment_operator assign_expr -> ^(ASSIGN ^(LVAL conditional_expression) ^(ASSIGN_OP assignment_operator) ^(RVAL assign_expr)))?;

constant_expr: conditional_expression;
conditional_expression: (or_expression -> or_expression)
        ('?' condition ':' conditional_expression -> ^(COND_EXPR condition '?' or_expression ':' conditional_expression))?;

or_expression : (and_expression  -> and_expression)
        ('||' or_expression -> ^(OR ^(OR_ELEM and_expression) '||' ^(OR_ELEM or_expression)))? ; 

and_expression : (inclusive_or_expression -> inclusive_or_expression)
        ('&&' and_expression -> ^(AND ^(AND_ELEM inclusive_or_expression) '&&' ^(AND_ELEM and_expression)))?;


inclusive_or_expression: (exclusive_or_expression -> exclusive_or_expression) ('|' inclusive_or_expression
            -> ^(BIT_OR ^(BIT_OR_ELEM exclusive_or_expression) '|' ^(BIT_OR_ELEM inclusive_or_expression)))?;


exclusive_or_expression: bit_and_expression ('^' exclusive_or_expression)?;


bit_and_expression: equality_expression ('&' bit_and_expression)?;

equality_expression: relational_expression (('=='| '!=') equality_expression)?;

relational_expression: shift_expression
        (('<'|'>'|'<='|'>=') relational_expression)?;

shift_expression: additive_expression ( ('<<'|'>>') shift_expression)?;

additive_expression: multiplicative_expression ('+' additive_expression)?;

multiplicative_expression: cast_expression ( ('*'| '/'| '%') cast_expression)?;

cast_expression: (('(' type_name ptr_operator* ')') => '(' type_name ptr_operator* ')' cast_expression)
    | unary_expression
    ;

call_template_list: ('<' template_param_list '>' );
function_argument_list: function_argument_list_ -> ^(ARGUMENT_LIST function_argument_list_?);
function_argument_list_: '(' ( function_argument (',' function_argument)* )? ')';
function_argument: assign_expr -> ^(ARGUMENT assign_expr);

unary_expression:  unary_operator* postfix_expression ;

postfix_expression
scope{
    CommonTree callTail;
}
: ( func_called=callee ((function_call_tail)=> x=function_call_tail {$postfix_expression::callTail = (CommonTree) x.getTree();} tail=postfix_tail?)?)
    -> {$postfix_expression::callTail != null}? ^(FUNCTION_CALL ^(CALLEE $func_called) $x) $tail?
    -> ^(NON_FUNC_CALL $func_called $tail?)?
;

callee: (primary_expression postfix*);
postfix_tail: (('.'|'->') primary_expression);

postfix: ('.' identifier
       	 | '->' identifier
       	 | '[' expr ']');


function_call_tail: call_template_list function_argument_list
                  | function_argument_list
                  ;

primary_expression: ('(' expr ')' | identifier | constant);

unary_operator
	: '&'
	| '*'
	| '+'
	| '-'
	| '~'
	| '!'
	;

constant
    :   HEX_LITERAL
    |   OCTAL_LITERAL
    |   DECIMAL_LITERAL
	|	STRING
    |   FLOATING_POINT_LITERAL
    ;

// water

no_brackets: ~('(' | ')');
no_brackets_curlies_or_squares: ~('(' | ')' | '{' | '}' | '[' | ']');
no_brackets_or_semicolon: ~('(' | ')' | ';');
no_angle_brackets_or_brackets : ~('<' | '>' | '(' | ')');
no_curlies: ~('{' | '}');
no_squares_or_semicolon: ~('[' | ']' | ';');

// keywords & operators

cv_qualifier :  'const' | 'volatile';
function_decl_specifiers: ('inline' | 'virtual' | 'explicit' | 'friend' | 'static');
class_key: ('struct' | 'class' | 'union' | 'enum');
ptr_operator: ('*' | '&');

access_specifier: ('public' | 'private' | 'protected');
operator_function_id: 'operator' operator;

operator: (('new' | 'delete' ) ('[' ']')?)
  | '+' | '-' | '*' | '/' | '%' |'^' | '&' | '|' | '~'
  | '!' | '=' | '<' | '>' | '+=' | '-=' | '*='
  | '/=' | '%=' | '^=' | '&=' | '|=' | '>>'
  |'<<'| '>>=' | '<<=' | '==' | '!='
  | '<=' | '>=' | '&&' | '||' | '++' | '--'
  | ',' | '->*' | '->' | '(' ')' | '[' ']'
  ;

assignment_operator: '=' | '*=' | '/=' | '%=' | '+=' | '-=' | '<<=' | '>>=' | '&=' | '^=' | '|='; 

identifier : ALPHA_NUMERIC ('::' ALPHA_NUMERIC)*;

water : ~OTHER;

// proxy rules
function_def: function_def_ -> ^(FUNCTION_DEF function_def_);
function_name: function_name_ -> ^(FUNCTION_NAME function_name_);
parameter_decl: parameter_decl_ -> ^(PARAMETER_DECL parameter_decl_);
parameter_name: parameter_name_ -> ^(NAME parameter_name_);

condition: condition_? -> ^(CONDITION condition_?);

namespace_def: namespace_def_ -> ^(NAMESPACE_DEF namespace_def_);
using_directive: using_directive_ -> ^(USING_DIRECTIVE using_directive_);
simple_decl: simple_decl_ -> ^(SIMPLE_DECL simple_decl_);
jump_statement: jump_statement_ -> ^(JUMP_STATEMENT jump_statement_);

init_decl_name: init_decl_name_ -> ^(NAME init_decl_name_);

include_directive: include_directive_ -> ^(INCLUDE_DIRECTIVE include_directive_);
label: label_ -> ^(LABEL label_);

number: HEX_LITERAL | DECIMAL_LITERAL | OCTAL_LITERAL;

// Lexer: 
// List valid characters not yet used in rules

DOT: '.'; // SIZEOF: 'sizeof'; 
QMARK: '?'; COLON: ':';

ALPHA_NUMERIC : ('a' .. 'z'| 'A' .. 'Z' | '_' | '~')('a' .. 'z'| 'A' .. 'Z' | '_' | '0' .. '9')*;
// DIGITS  : ('0' .. '9')+;

// stuff we want to discard, no matter what.

CPPCOMMENT 
    : '//' ( options {greedy=false; }: .)* '\n' {$channel = HIDDEN; }
    ;

COMMENT :       '/*'  ( options {greedy=false;} : .)* '*/'
        { $channel = HIDDEN; }
    ;

STRING: ('\'' ( ('\\' . ) | ~('\\' | '\'') )* '\'' ) // { $channel = HIDDEN; };
        |('"'  ( ('\\' . ) | ~('\\' | '"') )* '"') ; // { $channel = HIDDEN; };

PREPROC
    : '#'  ( options {greedy=false; }: .)* ~('\\') '\n' {$channel = HIDDEN; }
    ;

HEX_LITERAL : '0' ('x'|'X') HexDigit+ IntegerTypeSuffix? ;
DECIMAL_LITERAL : ('0' | '1'..'9' '0'..'9'*) IntegerTypeSuffix? ;
OCTAL_LITERAL : '0' ('0'..'7')+ IntegerTypeSuffix? ;

fragment
HexDigit : ('0'..'9'|'a'..'f'|'A'..'F') ;

fragment
IntegerTypeSuffix
	:	('u'|'U')? ('l'|'L')
	|	('u'|'U')  ('l'|'L')?
	;

FLOATING_POINT_LITERAL
    :   ('0'..'9')+ '.' ('0'..'9')* Exponent? FloatTypeSuffix?
    |   '.' ('0'..'9')+ Exponent? FloatTypeSuffix?
    |   ('0'..'9')+ Exponent FloatTypeSuffix?
    |   ('0'..'9')+ Exponent? FloatTypeSuffix
	;

fragment
Exponent : ('e'|'E') ('+'|'-')? ('0'..'9')+ ;

fragment
FloatTypeSuffix : ('f'|'F'|'d'|'D') ;

WHITESPACE: (' ' | '\t' | '\n' | '\r')+ {$channel = HIDDEN; };
OTHER : .  {$channel = HIDDEN;};
