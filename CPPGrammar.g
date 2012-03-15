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
  
  INIT_DECL_NAME; PARAMETER_LIST; PARAMETER_DECL;
  PARAMETER_TYPE; PARAMETER_NAME; CALLEE;
  ARGUMENT_LIST; ARGUMENT; CALL_TEMPLATE_LIST;
  
  SIMPLE_DECL;  NAMESPACE_DEF; USING_DIRECTIVE;
  INCLUDE_DIRECTIVE; TEMPLATE_DECL_SPECIFIER;
  
  SW; SELECTION; ITERATION;
  KEYWORD; SWITCH; FOR_INIT;
  FOR_EXPR; JUMP_STATEMENT; DESTINATION;
  CONDITION; LABEL;

  CTOR_EXPR; FUNCTION_CALL; CLASS_DEF;
  CLASS_NAME; TYPE_DEF; BASE_CLASSES;
  CLASS_CONTENT; TYPE_SPECIFIER; INIT_DECL_LIST;
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

simple_decl_:  t='typedef'? template_declaration_start?
               (  type_specifier init_declarator_list | class_def init_declarator_list? ) c= ';' 
                -> ^(TYPE_DEF $t?) ^(TEMPLATE_DECL_SPECIFIER template_declaration_start?)
                    ^(TYPE_SPECIFIER  type_specifier? ) ^(CLASS_DEF class_def?)
                    ^(INIT_DECL_LIST init_declarator_list?) $c;

template_declaration_start: 'template' '<' template_param_list '>' ;

class_def: class_key class_name base_classes '{' class_content '}';
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

init_declarator_list: init_declarator (',' init_declarator)*;
init_declarator: init_decl_name initializer?;
init_decl_name_: ( ptr_operator* identifier) ('[' constant_expr ']')?;
initializer: ('(' expr? ')') | '=' assignment_expr;

// Function Definitions
     
function_def_ :  function_start function_param_list ctor_list? compound_statement ->
                 function_start function_param_list ^(CTOR_LIST ctor_list?) compound_statement;

function_start: template_declaration_start? return_type function_name ->
                ^(TEMPLATE_DECL_SPECIFIER template_declaration_start?) return_type function_name;

return_type: return_type_ -> ^(RETURN_TYPE return_type_?);
return_type_: (function_decl_specifiers* type_specifier)? function_decl_specifiers* ptr_operator*;

function_name_: '(' function_name_ ')' | identifier | operator_function_id;
function_param_list : o='(' parameter_declaration_clause? c=')' cv_qualifier* exception_specification? ->
                    ^( PARAMETER_LIST $o parameter_declaration_clause? $c);

exception_specification : 'throw' '(' type_id_list ')';

ctor_list: ':'  ctor_initializer (',' ctor_initializer)* -> ctor_initializer+;
ctor_initializer:  '::'? identifier '(' expr? ')' -> ^(CTOR_INITIALIZER ^(INITIALIZER_ID '::'? identifier) ^(CTOR_EXPR expr?) );

parameter_declaration_clause: parameter_decl (',' parameter_decl)*;
parameter_decl_: param_decl_specifiers ptr_operator* parameter_name -> ^(PARAMETER_TYPE param_decl_specifiers ptr_operator*) parameter_name
  | identifier -> ^(SW identifier); // common case: some macro. Also catches void. 

parameter_name_: ('(' parameter_name_ ')' | identifier) ('[' constant_expr ']')?;
param_decl_specifiers: ('auto' | 'register')? type_specifier;

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


statement_water: no_curlies ->^(SW no_curlies);

expr_statement: expr_statement_start expr_statement_elem* ';';

expr_statement_start: (recognized_expr) => recognized_expr
    | expr_statement_water;

expr_statement_elem:  (recognized_expr) => recognized_expr
    | '{' expr_statement_l2 '}'                   
    | expr_statement_water;


expr_statement_l2: expr_statement_l2_elem*;

expr_statement_l2_elem: (recognized_expr) => recognized_expr
|'{' expr_statement_l2 '}'
| expr_statement_l2_water
;                   

expr_statement_water: expr_statement_water_ ->^(SW expr_statement_water_);
expr_statement_l2_water: expr_statement_l2_water_ ->^(SW expr_statement_l2_water_);

expr_statement_water_: ~('{' | '}' | ';');
expr_statement_l2_water_: no_curlies;

selection_statement: selection_statement_ -> ^(SELECTION selection_statement_);
selection_statement_: if_statement | else_statement | switch_statement;

iteration_statement: iteration_statement_ -> ^(ITERATION iteration_statement_);
iteration_statement_: for_statement | while_statement | do_statement;

jump_statement_: ( break_or_continue | return_statement |goto_statement ) ';';
break_or_continue: (k='break' | k='continue') -> ^(KEYWORD $k) ^(DESTINATION);
return_statement: k='return' expr? -> ^(KEYWORD $k) ^(DESTINATION expr?);
goto_statement:  k='goto' ( d=ALPHA_NUMERIC|  d=DIGITS) -> ^(KEYWORD $k) ^(DESTINATION $d?);

try_block: 'try' compound_statement;
catch_block: 'catch' '('param_decl_specifiers parameter_name? ')' compound_statement;

if_statement: k='if' '(' condition ')'  statement -> ^(KEYWORD $k) condition ^(STATEMENTS statement?) ;
else_statement: k='else' statement -> ^(KEYWORD $k) ^(STATEMENTS statement?);
switch_statement: k='switch' '(' condition ')' statement -> ^(KEYWORD $k ) condition ^(STATEMENTS statement?);

for_statement: k='for' '(' for_init_statement condition ';'  expr? ')' statement
            -> ^(KEYWORD $k) ^(FOR_INIT for_init_statement?) condition ';' ^(FOR_EXPR expr?) ^(STATEMENTS statement?);
while_statement: k='while' '(' condition ')' statement -> ^(KEYWORD $k) condition ^(STATEMENTS statement?);
do_statement: k='do' statement 'while' '(' expr ')' -> ^(KEYWORD $k) ^(CONDITION expr) ^(STATEMENTS statement?) ;

condition_: expr;
for_init_statement : (simple_decl) => simple_decl | expr? ';';

label_: (('case'? (ALPHA_NUMERIC | DIGITS | '::')+ ) | access_specifier) ':' ;

type_id_list: no_brackets* ('(' type_id_list ')' no_brackets*)*;
namespace_def_: 'namespace' identifier? '{' namespace_content '}';
namespace_content: namespace_content_elem* ('{' namespace_content '}' namespace_content_elem*)*;
namespace_content_elem: (simple_decl) => simple_decl
  | (function_def) => function_def
  | no_curlies;
  
type_specifier: cv_qualifier* class_key? ('unsigned' | 'signed')? ALPHA_NUMERIC ('<' template_param_list '>' )? ('::' ALPHA_NUMERIC ('<' template_param_list '>' )?)* ;
template_param_list : template_param_list_elem*;
template_param_list_elem:  ('<' template_param_list '>')
         | ('(' template_param_list ')')
         | no_angle_brackets_or_brackets
;

// Expressions

expr:  expr_elem+;
expr_elem:  (recognized_expr) => recognized_expr
                | '(' expr ')'
                | no_brackets_or_semicolon;

recognized_expr: function_call;
function_call_:  called_function call_template_list function_argument_list
              -> called_function ^(CALL_TEMPLATE_LIST call_template_list?) function_argument_list;
call_template_list: ('<' template_param_list '>' )?;
function_argument_list: function_argument_list_ -> ^(ARGUMENT_LIST function_argument_list_?);
function_argument_list_: '(' ( function_argument (',' function_argument)* )? ')';
function_argument: assignment_expr -> ^(ARGUMENT assignment_expr);
called_function: called_function_ -> ^(CALLEE called_function_);
called_function_:  ( '(' expr ')' )=> '(' expr ')' | b_ident;
b_ident:  ptr_operator* ('('  b_ident+ ')'  | identifier) (('.' | '->') b_ident)?;

constant_expr: constant_expr_elem*;

constant_expr_elem: '[' constant_expr ']'
                    | no_squares_or_semicolon;

assignment_expr: assignment_expr_elem+;

assignment_expr_elem: (recognized_expr) => recognized_expr
   | ( '(' assignment_expr_l2 ')'
   | ('{' assignment_expr_l2 '}')
   | ('[' assignment_expr_l2 ']'))
   | ~(',' | ';' | '(' | ')' | '{' | '}' | '[' | ']');
    
assignment_expr_l2: assignment_expr_l2_elem*;

assignment_expr_l2_elem: (recognized_expr) => recognized_expr
  | ('(' assignment_expr_l2 ')' | '{' assignment_expr_l2 '}' | ('[' assignment_expr_l2 ']'))
  | no_brackets_curlies_or_squares;

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

identifier  : ALPHA_NUMERIC ('::' ALPHA_NUMERIC)*;
water : ~OTHER;

// proxy rules
function_def: function_def_ -> ^(FUNCTION_DEF function_def_);
function_name: function_name_ -> ^(FUNCTION_NAME function_name_);
parameter_decl: parameter_decl_ -> ^(PARAMETER_DECL parameter_decl_);
parameter_name: parameter_name_ -> ^(PARAMETER_NAME parameter_name_);

namespace_def: namespace_def_ -> ^(NAMESPACE_DEF namespace_def_);
using_directive: using_directive_ -> ^(USING_DIRECTIVE using_directive_);
simple_decl: simple_decl_ -> ^(SIMPLE_DECL simple_decl_);
jump_statement: jump_statement_ -> ^(JUMP_STATEMENT jump_statement_);
condition: condition_? -> ^(CONDITION condition_?);

init_decl_name: init_decl_name_ -> ^(INIT_DECL_NAME init_decl_name_);

include_directive: include_directive_ -> ^(INCLUDE_DIRECTIVE include_directive_);
label: label_ -> ^(LABEL label_);
function_call: function_call_ -> ^(FUNCTION_CALL function_call_);

// Lexer: 
// List valid characters not yet used in rules
DOT: '.'; SIZEOF: 'sizeof'; 
Q_MARK: '?';

ALPHA_NUMERIC : ('a' .. 'z'| 'A' .. 'Z' | '_' | '~')('a' .. 'z'| 'A' .. 'Z' | '_' | '0' .. '9')*;
DIGITS  : ('0' .. '9')+;

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

WHITESPACE: (' ' | '\t' | '\n' | '\r')+ {$channel = HIDDEN; };
OTHER : .  {$channel = HIDDEN;};
