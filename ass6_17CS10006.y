%{
	#include "ass6_17CS10006_translator.h"
	void yyerror(const char*);
	extern int yylex(void);
	using namespace std;
%}


%union{
	int intval;
	float floatval;
	string *strval;
	char charval;
	idStruct idAttr;
	SymbolTable *ST;
	expStruct expAttr;
	list *nextlist;
	decStruct decAttr;
	arglistStruct argsAttr;
	int instr;
}

%token AUTO
%token BREAK
%token CASE
%token CHAR
%token CONST
%token CONTINUE
%token DEFAULT
%token DO
%token DOUBLE
%token ELSE
%token ENUM
%token EXTERN
%token FLOAT
%token FOR
%token GOTO
%token IF
%token INLINE
%token INT
%token LONG
%token REGISTER
%token RESTRICT
%token RETURN
%token SHORT
%token SIGNED
%token SIZEOF
%token STATIC
%token STRUCT
%token SWITCH
%token TYPEDEF
%token UNION
%token UNSIGNED
%token VOID
%token VOLATILE
%token WHILE
%token BOOL
%token COMPLEX
%token IMAGINARY

%token <idAttr> IDENTIFIER
%token <intval> INTEGER_CONSTANT
%token <floatval> FLOATING_CONSTANT
%token <strval> ENUMERATION_CONSTANT
%token <charval> CHAR_CONST
%token <strval> STRING_LITERAL

%token POINTER
%token INCREMENT
%token DECREMENT
%token LEFT_SHIFT
%token RIGHT_SHIFT
%token LESS_EQUALS
%token GREATER_EQUALS
%token EQUALS
%token NOT_EQUALS
%token AND
%token OR
%token ELLIPSIS
%token MULTIPLY_ASSIGN
%token DIVIDE_ASSIGN
%token MODULO_ASSIGN
%token ADD_ASSIGN
%token SUBTRACT_ASSIGN
%token LEFT_SHIFT_ASSIGN
%token RIGHT_SHIFT_ASSIGN
%token AND_ASSIGN
%token XOR_ASSIGN
%token OR_ASSIGN
%token SINGLE_LINE_COMMENT
%token MULTI_LINE_COMMENT

// All these non-terminals are assigned to expression structure. Explaination for each of them is given in the individual blocks
%type <expAttr> primary_expression
%type <expAttr> postfix_expression
%type <expAttr> unary_expression
%type <expAttr> cast_expression
%type <expAttr> multiplicative_expression
%type <expAttr> additive_expression
%type <expAttr> shift_expression
%type <expAttr> relational_expression
%type <expAttr> equality_expression
%type <expAttr> AND_expression
%type <expAttr> exclusive_OR_expression
%type <expAttr> inclusive_OR_expression
%type <expAttr> logical_AND_expression
%type <expAttr> logical_OR_expression
%type <expAttr> conditional_expression
%type <expAttr> assignment_expression_opt
%type <expAttr> assignment_expression
%type <expAttr> constant_expression
%type <expAttr> expression
%type <expAttr> expression_statement
%type <expAttr> expression_opt
%type <expAttr> declarator
%type <expAttr> direct_declarator
%type <expAttr> initializer
%type <expAttr> identifier_opt
%type <expAttr> declaration
%type <expAttr> init_declarator_list
%type <expAttr> init_declarator_list_opt
%type <expAttr> init_declarator

// These non-terminals are given nextlist as type as all of them will have to support a list of dangling gotos
%type <nextlist> block_item_list
%type <nextlist> block_item
%type <nextlist> statement
%type <nextlist> labeled_statement
%type <nextlist> compound_statement
%type <nextlist> selection_statement
%type <nextlist> iteration_statement
%type <nextlist> jump_statement
%type <nextlist> block_item_list_opt

%type <argsAttr> argument_expression_list
%type <argsAttr> argument_expression_list_opt

%type <decAttr> type_specifier
%type <decAttr> declaration_specifiers
%type <decAttr> specifier_qualifier_list
%type <decAttr> type_name
%type <decAttr> pointer
%type <decAttr> pointer_opt

%type <instr> M
%type <nextlist> N
%type <charval> unary_operator

%start translation_unit

%left '+' '-'
%left '*' '/' '%'
%nonassoc UNARY

%%

translation_unit:	external_declaration
				|	translation_unit external_declaration
				;

N: {$$ = makelist(nextinstr); quad.emit(o_GOTO, -1);};

primary_expression:				IDENTIFIER {
									/*Check if function name*/
									symtabelem * check_func = globalST->search(*$1.name);
									if(check_func == NULL)
									{
										$$.loc 	=  currST->lookup(*$1.name);
										if($$.loc->type != NULL && $$.loc->type->baseType == t_ARRAY)
										{
											/*If array then store the expStruct.array and send a temporary variable with zero initial value*/
											$$.array = $$.loc;
											$$.loc = currST->gentemp(new typeNode(t_INT));
											$$.loc->initialValue.ival = 0;
											$$.loc->isInitialized = true;
											quad.emit(o_COPY,0,$$.loc->name);
											$$.type = $$.array->type;
										}
										else
										{
											// if not an array then a local variable
											$$.type = $$.loc->type;
											$$.array = NULL;
											$$.isPointer = false;
										}
									}
									else
									{
										// It is a function
										$$.loc = check_func;
										$$.type = check_func->type;
										$$.array = NULL;
										$$.isPointer = false;
									}
								} |
								INTEGER_CONSTANT {
									// Initialize the value of the temporary variable with the integer
									$$.loc 	= currST->gentemp(new typeNode(t_INT));
									$$.type = $$.loc->type;
									$$.loc->initialValue.ival = $1;
									$$.loc->isInitialized = true;
									$$.array = NULL;
									quad.emit(o_COPY, $1, $$.loc->name);
								} |
								FLOATING_CONSTANT {
									// Initialize the value of the temporary variable with the floatval
									$$.loc 	= currST->gentemp(new typeNode(t_DOUBLE));
									$$.type = $$.loc->type;
									$$.loc->initialValue.dval = $1;
									$$.loc->isInitialized = true;
									$$.array = NULL;
									quad.emit(o_COPY, $1, $$.loc->name);
								} |
								CHAR_CONST {
									// Initialize the value of the temporary variable with the character
									$$.loc 	= currST->gentemp(new typeNode(t_CHAR));
									$$.type = $$.loc->type;
									$$.loc->initialValue.cval = $1;
									$$.loc->isInitialized = true;
									$$.array = NULL;
								quad.emit(o_COPY, $1, $$.loc->name);
								} |
								STRING_LITERAL {
									strs.push_back(*$1);
									$$.loc = NULL;
									$$.isString = true;
									$$.strid = strs.size()-1;
									$$.array = NULL;
									$$.isPointer = false;
								} |
								'(' expression ')' {
									// Simply copy the value
									$$ = $2;
								};

M: {$$ = nextinstr;};

enumeration_constant:			IDENTIFIER {};

postfix_expression :			primary_expression {
									$$ = $1;
								} |
								postfix_expression '[' expression ']' {
									/*Array Logic
									$$.array will store the base pointer
									$$.type will store the base type of the array
									$$.array->type has the full type of the array which will be used for size calculations*/
									$$.loc = currST->gentemp(new typeNode(t_INT));
									symtabelem* temporary = currST->gentemp(new typeNode(t_INT));
									char temp[10];
									sprintf(temp,"%ld",$1.type->next->getSize());
									quad.emit(o_MULT,$3.loc->name,temp,temporary->name);
									quad.emit(o_PLUS,$1.loc->name,temporary->name,$$.loc->name);
									// the new size will be calculated and the temporary variable storing the size will be passed on a $$.loc
									$$.array = $1.array;
									$$.type = $1.type->next;
								} |
								postfix_expression '(' argument_expression_list_opt ')' {
									/*Function Call Logic*/
									$$.loc = currST->gentemp(CopyType($1.type));
									// Funciton call value will be stored in a temporary variable
									char str[10];
									if($3.arguments == NULL)
									{
										/*No function Parameters directly call the function*/
										sprintf(str,"0");
										quad.emit(o_CALL,$1.loc->name,str,$$.loc->name);
									}
									else
									{
										if((*$3.arguments)[0]->isString)
										{
											str[0] = '_';
											sprintf(str+1,"%d",(*$3.arguments)[0]->strid);
											quad.emit(o_PARAM,str);
											quad.emit(o_CALL,$1.loc->name,"1",$$.loc->name);
										}
										else
										{
											for(int i=0;i<$3.arguments->size();i++)
											{
												// Print all the parameters one by one
												if((*$3.arguments)[i]->poss_array != NULL && $1.loc->name != "printi")
													quad.emit(o_PARAM,(*$3.arguments)[i]->poss_array->name);
												else
													quad.emit(o_PARAM,(*$3.arguments)[i]->loc->name);
												// Here parameter typechecking is not performed to reduce the complexity and also the input is considered to be accurate
											}
											sprintf(str,"%ld",$3.arguments->size());
											quad.emit(o_CALL,$1.loc->name,str,$$.loc->name);
										}
									}
									$$.array = NULL;
									$$.type = $$.loc->type;
								} |
								postfix_expression '.' IDENTIFIER {/*Struct Logic to be Skipped*/}|
								postfix_expression POINTER IDENTIFIER {
									/*Pointer Dereference Logic*/
								} |
								postfix_expression INCREMENT {
									$$.loc = currST->gentemp(CopyType($1.type));
									if($1.array != NULL)
									{
										// Array element post increment logic
										symtabelem * temp_elem = currST->gentemp(CopyType($1.type));
										quad.emit(o_RINDEX,$1.array->name,$1.loc->name,$$.loc->name);
										quad.emit(o_RINDEX,$1.array->name,$1.loc->name,temp_elem->name);
										quad.emit(o_PLUS,temp_elem->name,"1",temp_elem->name);
										quad.emit(o_LINDEX,$1.loc->name,temp_elem->name,$1.array->name);
										$$.array = NULL;
									}
									else
									{
										// Simple post increment
										quad.emit(o_COPY,$1.loc->name,$$.loc->name);
										quad.emit(o_PLUS,$1.loc->name,"1",$1.loc->name);	
									}
									$$.type = $$.loc->type;									
								} |
								postfix_expression DECREMENT {
									$$.loc = currST->gentemp(CopyType($1.type));
									if($1.array != NULL)
									{
										// Array element post decrement logic
										symtabelem * temp_elem = currST->gentemp(CopyType($1.type));
										quad.emit(o_RINDEX,$1.array->name,$1.loc->name,$$.loc->name);
										quad.emit(o_RINDEX,$1.array->name,$1.loc->name,temp_elem->name);
										quad.emit(o_MINUS,temp_elem->name,"1",temp_elem->name);
										quad.emit(o_LINDEX,$1.loc->name,temp_elem->name,$1.array->name);
										$$.array = NULL;
									}
									else
									{
										// Simple post decrement
										quad.emit(o_COPY,$1.loc->name,$$.loc->name);
										quad.emit(o_MINUS,$1.loc->name,"1",$1.loc->name);
									}
									$$.type = $$.loc->type;
								} |
								'(' type_name ')' '{' initializer_list '}' {
									/*Type Conversion Logic*/
								}|
								'(' type_name ')' '{' initializer_list ',' '}' {
									/*Type Conversion Logic*/
								};

argument_expression_list:		assignment_expression {
									/*Arguments are to be handled in function calling*/
									$$.arguments = new vector<expStruct*>;
									expStruct * tex = new expStruct($1);
									$$.arguments->push_back(tex);
								}|
								argument_expression_list ',' assignment_expression {
									/*List is to be handled in Function*/
									expStruct * tex = new expStruct($3);
									$$.arguments->push_back(tex);
								};

argument_expression_list_opt:	argument_expression_list {
									/*To be handled later*/
									$$ = $1;
								}|
								/*Epslion*/ {
									/*Default values to be set here*/
									$$.arguments = NULL;
								};

unary_expression:				postfix_expression {
									$$ = $1;
								}|
								INCREMENT unary_expression {
									$$.loc = currST->gentemp($2.type);
									if($2.array != NULL)
									{
										// Array element pre increment logic
										symtabelem * temp_elem = currST->gentemp(CopyType($2.type));
										quad.emit(o_RINDEX,$2.array->name,$2.loc->name,temp_elem->name);
										quad.emit(o_PLUS,temp_elem->name,"1",temp_elem->name);
										quad.emit(o_LINDEX,$2.loc->name,temp_elem->name,$2.array->name);
										quad.emit(o_RINDEX,$2.array->name,$2.loc->name,$$.loc->name);
										$$.array = NULL;
									}
									else
									{
										// Simple expression pre increment
										quad.emit(o_PLUS,$2.loc->name,"1",$2.loc->name);
										quad.emit(o_COPY,$2.loc->name,$$.loc->name);
									}
									$$.type = $$.loc->type;
								}|
								DECREMENT unary_expression {
									$$.loc = currST->gentemp(CopyType($2.type));
									if($2.array != NULL)
									{
										// Array Element pre decrement logica
										symtabelem * temp_elem = currST->gentemp(CopyType($2.type));
										quad.emit(o_RINDEX,$2.array->name,$2.loc->name,temp_elem->name);
										quad.emit(o_MINUS,temp_elem->name,"1",temp_elem->name);
										quad.emit(o_LINDEX,$2.loc->name,temp_elem->name,$2.array->name);
										quad.emit(o_RINDEX,$2.array->name,$2.loc->name,$$.loc->name);
										$$.array = NULL;
									}
									else
									{
										// Simple pre decrement
										quad.emit(o_MINUS,$2.loc->name,"1",$2.loc->name);
										quad.emit(o_COPY,$2.loc->name,$$.loc->name);
									}
									$$.type = $$.loc->type;
								}|
								unary_operator cast_expression
								{
									typeNode * temp_type;
									switch($1)
									{
										case '&':
											/*copy the address in the temporary and append ptr to its type*/
											temp_type = new typeNode(t_PTR,1,$2.type);
											$$.loc = currST->gentemp(CopyType(temp_type));
											$$.type = $$.loc->type;
											quad.emit(o_ADDR,$2.loc->name,$$.loc->name);
											$$.array = NULL;
											break;
										case '*':
											/*mark the output with isPointer=true*/
											$$.isPointer = true;
											$$.type = $2.loc->type->next;
											$$.loc = $2.loc;
											$$.array = NULL;
											break;
										case '+':
											$$.loc = currST->gentemp(CopyType($2.type));
											$$.type = $$.loc->type;
											quad.emit(o_COPY,$2.loc->name,$$.loc->name);
											break;
										case '-':
											$$.loc = currST->gentemp(CopyType($2.type));
											$$.type = $$.loc->type;
											quad.emit(o_UMINUS,$2.loc->name,$$.loc->name);
											break;
										case '~':
											/*Bitwise Not to be implemented Later on*/
											$$.loc = currST->gentemp(CopyType($2.type));
											$$.type = $$.loc->type;
											quad.emit(o_BNOT,$2.loc->name,$$.loc->name);
											break;
										case '!':
											$$.loc = currST->gentemp(CopyType($2.type));
											$$.type = $$.loc->type;
											$$.truelist = $2.falselist;
											$$.falselist = $2.truelist;
											break;
										default:
											/*Error condition*/
											break;
									}
								}|
								SIZEOF unary_expression {}|
								SIZEOF '(' type_name ')' {};

unary_operator	:				'&' {
									$$ = '&';
								}|
								'*'	{
									$$ = '*';
								}|
								'+' {
									$$ = '+';
								}|
								'-' {
									$$ = '-';
								}|
								'~' {
									$$ = '~';
								}|
								'!' {
									$$ = '!';
								};

cast_expression : 				unary_expression {
									if($1.array != NULL && $1.array->type != NULL)
									{
										/*RINDEX operation*/
										/*If the unary expression is moving to cast expression and still if it is array then is is surely a R-type indexing for the array element*/
										$$.loc = currST->gentemp(new typeNode($1.array->type->next->baseType));
										quad.emit(o_RINDEX,$1.array->name,$1.loc->name,$$.loc->name);
										$$.array = NULL;
										$$.type = $$.loc->type;
										$$.poss_array = $1.array;
									}
									else if($1.isPointer == true)
									{
										/*If it is pointer after reaching cast expression then surely it is R-type dereferecing*/
										$$.loc = currST->gentemp(CopyType($1.type));
										$$.isPointer = false;
										quad.emit(o_RDEREF,$1.loc->name,$$.loc->name);
									}
									else
										$$ = $1;
								}|
								'(' type_name ')' cast_expression{
									/*Cast Expression to be handled later*/
								};
/*For each type of calulative expression from here, expressions on both sides are first typechecked an accodingly a quad is generated with generation of a temporary*/
multiplicative_expression:		cast_expression {
									$$ = $1;
								}|
								multiplicative_expression '*' cast_expression {
									typecheck(&$1,&$3);
									$$.loc = currST->gentemp($1.type);
									$$.type = $$.loc->type;
									quad.emit(o_MULT,$1.loc->name,$3.loc->name,$$.loc->name);
								}|
								multiplicative_expression '/' cast_expression {
									typecheck(&$1,&$3);
									$$.loc = currST->gentemp($1.type);
									$$.type = $$.loc->type;
									quad.emit(o_DIV,$1.loc->name,$3.loc->name,$$.loc->name);
								}|
								multiplicative_expression '%' cast_expression{
									typecheck(&$1,&$3);
									$$.loc = currST->gentemp($1.type);
									$$.type = $$.loc->type;
									quad.emit(o_MOD,$1.loc->name,$3.loc->name,$$.loc->name);
								};

additive_expression:			multiplicative_expression {
									$$ = $1;
								}|
								additive_expression '+' multiplicative_expression {
									typecheck(&$1,&$3);
									$$.loc = currST->gentemp($1.type);
									$$.type = $$.loc->type;
									quad.emit(o_PLUS,$1.loc->name,$3.loc->name,$$.loc->name);
								}|
								additive_expression '-' multiplicative_expression {
									typecheck(&$1,&$3);
									$$.loc = currST->gentemp($1.type);
									$$.type = $$.loc->type;
									quad.emit(o_MINUS,$1.loc->name,$3.loc->name,$$.loc->name);
								};

/*In Shift operation both left and right expression should be integer*/
shift_expression:				additive_expression {
									$$ = $1;
								}|
								shift_expression LEFT_SHIFT additive_expression {
									$$.loc = currST->gentemp($1.type);
									$$.type = $$.loc->type;
									quad.emit(o_SHL,$1.loc->name,$3.loc->name,$$.loc->name);
								}|
								shift_expression RIGHT_SHIFT additive_expression{
									$$.loc = currST->gentemp($1.type);
									$$.type = $$.loc->type;
									quad.emit(o_SHR,$1.loc->name,$3.loc->name,$$.loc->name);
								};

/*Relational expression change/sets the type of the input expressin to boolean*/
relational_expression:			shift_expression {
									$$ = $1;
								}|
								relational_expression '<' shift_expression {
									typecheck(&$1,&$3);
									$$.type = new typeNode(t_BOOL);
									$$.truelist = makelist(nextinstr);
									$$.falselist = makelist(nextinstr+1);
									quad.emit(o_JLT,$1.loc->name,$3.loc->name,"-1");
									quad.emit(o_GOTO,"-1");
								}|
								relational_expression '>' shift_expression {
									typecheck(&$1,&$3);
									$$.type = new typeNode(t_BOOL);
									$$.truelist = makelist(nextinstr);
									$$.falselist = makelist(nextinstr+1);
									quad.emit(o_JGT,$1.loc->name,$3.loc->name,"-1");
									quad.emit(o_GOTO,"-1");
								}|
								relational_expression LESS_EQUALS shift_expression {
									typecheck(&$1,&$3);
									$$.type = new typeNode(t_BOOL);
									$$.truelist = makelist(nextinstr);
									$$.falselist = makelist(nextinstr+1);
									quad.emit(o_JLE,$1.loc->name,$3.loc->name,"-1");
									quad.emit(o_GOTO,"-1");
								}|
								relational_expression GREATER_EQUALS shift_expression {
									typecheck(&$1,&$3);
									$$.type = new typeNode(t_BOOL);
									$$.truelist = makelist(nextinstr);
									$$.falselist = makelist(nextinstr+1);
									quad.emit(o_JGE,$1.loc->name,$3.loc->name,"-1");
									quad.emit(o_GOTO,"-1");
								};

equality_expression:			relational_expression {
									$$ = $1;
								}|
								equality_expression EQUALS relational_expression {
									typecheck(&$1,&$3);
									$$.type = new typeNode(t_BOOL);
									$$.truelist = makelist(nextinstr);
									$$.falselist = makelist(nextinstr+1);
									quad.emit(o_JE,$1.loc->name,$3.loc->name,"-1");
									quad.emit(o_GOTO,"-1");
								}|
								equality_expression NOT_EQUALS relational_expression {
									typecheck(&$1,&$3);
									$$.type = new typeNode(t_BOOL);
									$$.truelist = makelist(nextinstr);
									$$.falselist = makelist(nextinstr+1);
									quad.emit(o_JNE,$1.loc->name,$3.loc->name,"-1");
									quad.emit(o_GOTO,"-1");
								};

/*Bitwise And operation is assumed to be operated on integers*/
AND_expression :				equality_expression {
									$$ = $1;
								}|
								AND_expression '&' equality_expression {
									$$.loc = currST->gentemp($1.type);
									$$.type = $$.loc->type;
									quad.emit(o_BAND,$1.loc->name,$3.loc->name,$$.loc->name);
								};

exclusive_OR_expression:		AND_expression {
									$$ = $1;
								}|
								exclusive_OR_expression '^' AND_expression {
									$$.loc = currST->gentemp($1.type);
									$$.type = $$.loc->type;
									quad.emit(o_BXOR,$1.loc->name,$3.loc->name,$$.loc->name);
								};

inclusive_OR_expression:		exclusive_OR_expression {
									$$ = $1;
								}|
								inclusive_OR_expression '|' exclusive_OR_expression	{
									$$.loc = currST->gentemp($1.type);
									$$.type = $$.loc->type;
									quad.emit(o_BOR,$1.loc->name,$3.loc->name,$$.loc->name);
								};

logical_AND_expression:			inclusive_OR_expression {
									$$ = $1;
								}|
								logical_AND_expression AND M inclusive_OR_expression {
									if($1.type->baseType != t_BOOL)
										conv2Bool(&$1);
									if($4.type->baseType != t_BOOL)
										conv2Bool(&$4);
									backpatch($1.truelist,$3);
									$$.type = new typeNode(t_BOOL);
									$$.falselist = merge($1.falselist,$4.falselist);
									$$.truelist = $4.truelist;
								};

logical_OR_expression:			logical_AND_expression {
									$$ = $1;
								}|
								logical_OR_expression OR M logical_AND_expression	{
									if($1.type->baseType != t_BOOL)
										conv2Bool(&$1);
									if($4.type->baseType != t_BOOL)
										conv2Bool(&$4);	
									backpatch($1.falselist,$3);
									$$.type = new typeNode(t_BOOL);
									$$.truelist = merge($1.truelist,$4.truelist);
									$$.falselist = $4.falselist;
								};

/*It is assumed that type of expression and conditional expression are same*/
conditional_expression: 		logical_OR_expression {
									$$ = $1;
								}|
								logical_OR_expression N '?' M expression N ':' M conditional_expression {
									$$.loc = currST->gentemp($5.type);
									$$.type = $$.loc->type;
									quad.emit(o_COPY,$9.loc->name,$$.loc->name);
									list* I = makelist(nextinstr);
									quad.emit(o_GOTO,"-1");
									backpatch($6,nextinstr);
									quad.emit(o_COPY,$5.loc->name,$$.loc->name);
									I = merge(I,makelist(nextinstr));
									quad.emit(o_GOTO,"-1");
									backpatch($2,nextinstr);
									conv2Bool(&$1);
									backpatch($1.truelist,$4);
									backpatch($1.falselist,$8);
									backpatch(I,nextinstr);
								};

/*We assume that only Equal to operator will emerge out of this non-terminal*/
assignment_operator:			'=' 													|
								MULTIPLY_ASSIGN 										|
								DIVIDE_ASSIGN 											|
								MODULO_ASSIGN 											|
								ADD_ASSIGN 												|
								SUBTRACT_ASSIGN 										|
								LEFT_SHIFT_ASSIGN 										|
								RIGHT_SHIFT_ASSIGN 										|
								AND_ASSIGN 												|
								XOR_ASSIGN 												|
								OR_ASSIGN												;

assignment_expression:			conditional_expression {
									$$ = $1;
								}|
								unary_expression assignment_operator assignment_expression {
									/*If an unary_expression upto this point is an array or a pointer then surely it is a left indexing or left dereference respectively*/
									if($1.isPointer)
									{
										quad.emit(o_LDEREF,$3.loc->name,$1.loc->name);
									}
									quad.print();
									currST->print();
									printf("%s %s\n",$1.loc->name.c_str(),$3.loc->name.c_str());
									typecheck(&$1,&$3,true);
									if($1.array != NULL)
									{
										/*To be assigned in an array*/
										quad.emit(o_LINDEX,$1.loc->name,$3.loc->name,$1.array->name);
									}
									else if(!$1.isPointer)
										quad.emit(o_COPY,$3.loc->name,$1.loc->name);
									$$.loc = currST->gentemp($3.type);
									$$.type = $$.loc->type;
									quad.emit(o_COPY,$3.loc->name,$$.loc->name);
								};

/*A constant value of this expression exists*/
constant_expression:			conditional_expression {
									$$ = $1;
								};

expression :					assignment_expression {
									$$ = $1;
								}|
								expression ',' assignment_expression {
									$$ = $3;
								};

/*Declarations*/ 

declaration:					declaration_specifiers init_declarator_list_opt ';' {
									/*If the declaration here is a function then we can surely say that it was a function prototype and not full function definition. Thus, we will clear the current symbol table for future function prototype/definition*/
									/*Assign Types to all the entities of init_declarator_list*/
									if($2.loc != NULL && $2.type != NULL && $2.type->baseType == t_FUNC)
									{
										/*Delete currST*/
										currST = new SymbolTable();
									}
								};

init_declarator_list_opt: 		init_declarator_list {
									if($1.type != NULL && $1.type->baseType == t_FUNC)
									{
										$$ = $1;
									}
								}|
								/*Epslion*/	{
									$$.loc = NULL;
								};

declaration_specifiers:			storage_class_specifier declaration_specifiers_opt {}|
								type_specifier declaration_specifiers_opt 				|
								type_qualifier declaration_specifiers_opt {}|
								function_specifier declaration_specifiers_opt {};

declaration_specifiers_opt: 	declaration_specifiers 									|
								/*Epslion*/												;

init_declarator_list:			init_declarator {
									/*Expecting only function declaration*/
									$$ = $1;
								}|
								init_declarator_list ',' init_declarator				;

init_declarator:				declarator {
									/*Nothing to be done here*/
									if($1.type != NULL && $1.type->baseType == t_FUNC)
									{
										$$ = $1;
									}
								}|
								declarator '=' initializer {
									/*Declarator is initialized with the proper value*/
									typecheck(&$1,&$3,true);
									quad.emit(o_COPY,$3.loc->name,$1.loc->name);
								};

storage_class_specifier:		EXTERN {}|
								STATIC {}|
								AUTO {}|
								REGISTER {};

type_specifier:					VOID {
									t = new typeNode(t_VOID);
								}|
								CHAR {
									t = new typeNode(t_CHAR);
								}|
								SHORT {}|
								INT	{
									t = new typeNode(t_INT);
								}|
								LONG {}|
								FLOAT {}|
								DOUBLE {
									t = new typeNode(t_DOUBLE);
								}|
								SIGNED {}|
								UNSIGNED {}|
								BOOL {}|
								COMPLEX {}|
								IMAGINARY {}|
								enum_specifier {};

specifier_qualifier_list: 		type_specifier specifier_qualifier_list_opt {
									/*Nothing to be done here as the type is already initialized globally*/
								}|
								type_qualifier specifier_qualifier_list_opt {};	

specifier_qualifier_list_opt: 	specifier_qualifier_list {}|
								/*Epslion*/	{};

enum_specifier:					ENUM identifier_opt '{' enumerator_list '}' {}|
								ENUM identifier_opt '{' enumerator_list ',' '}' {}|
								ENUM IDENTIFIER	{};

identifier_opt:					IDENTIFIER {
									$$.loc  = currST->lookup(*$1.name);
									$$.type = new typeNode(t->baseType);
								}|
								/*Epslion*/	{};

enumerator_list:				enumerator {}|
								enumerator_list ',' enumerator {};

enumerator:						enumeration_constant {}|
								enumeration_constant '=' constant_expression {};

type_qualifier:					CONST {}|
								RESTRICT {}|
								VOLATILE {};

function_specifier:				INLINE {};

declarator :					pointer_opt direct_declarator {
									/*Append Pointers to the type*/
									if($1.type == NULL)
									{
										/*No Pointer for this direct_declarator*/
									}
									else
									{
										/*Append pointers to this direct_declarator*/
										if($2.loc->type->baseType != t_PTR)
										{
											typeNode * test = $1.type;
											while(test->next != NULL)
											{
												test = test->next;
											}
											test->next = $2.loc->type;
											$2.loc->type = $1.type;
										}
									}

									if($2.type != NULL && $2.type->baseType == t_FUNC)
									{
										/*Function type*/

										$$ = $2;
										/*Adjust Offset*/
									}
									else
									{
										/*Not a function*/
										/*Update the size and offset*/
										$2.loc->size = $2.loc->type->getSize();
										$2.loc->offset = currST->offset;
										currST->offset += $2.loc->size;
										$$ = $2;
										$$.type = $$.loc->type;
									}
								};

pointer_opt:					pointer {
									$$ = $1;
								}|
								/*Epslion*/	{
									/*No pointer present*/
									$$.type = NULL;
								};

direct_declarator:				IDENTIFIER {
									/*Identifier will be added to current symbol table here*/
										$$.loc = currST->lookup(*$1.name);
										if($$.loc->var_type == "")
										{
											$$.loc->var_type = "local";
											/*Initialize type here*/
											$$.loc->type = new typeNode(t->baseType);
										}
										else if($$.loc->var_type == "return")
										{
											/*Already declared function change the symbol table*/

										}
										$$.type = $$.loc->type;
									
									/*Type of this variable is set here but the pointers are not appended*/
								}|
								'(' declarator ')' {
									$$ = $2;
								}|
								direct_declarator '[' type_qualifier_list_opt assignment_expression_opt ']' {
									/*Append in type array and adjust the size*/
									if($1.type->baseType == t_ARRAY)
									{
										/*if type is already an array*/
										typeNode * typ1 = $1.type,*typ = $1.type;
										typ1 = typ1->next;
										while(typ1->next != NULL)
										{
											typ1 = typ1->next;
											typ = typ->next;
										}
										typ->next = new typeNode(t_ARRAY,$4.loc->initialValue.ival,typ1);
									}
									else
									{
										/*Append array type*/
										if($4.loc == NULL)
											$1.type = new typeNode(t_ARRAY,-1,$1.type);
										else
											$1.type = new typeNode(t_ARRAY,$4.loc->initialValue.ival,$1.type);
									}
									$$ = $1;
									$$.loc->type = $$.type;
								}|
								direct_declarator '[' STATIC type_qualifier_list_opt assignment_expression ']' {}|
								direct_declarator '[' type_qualifier_list STATIC assignment_expression ']' {}|
								direct_declarator '[' type_qualifier_list_opt '*' ']' {/*Not sure but mostly we don't have to implement this*/}|
								direct_declarator '(' parameter_type_list ')' {
									/*Function Declarator*/
									/*First Element of this table is Our Function and the rest of the elements are Params*/
									/*add a new entry in global symbol table*/
									symtabelem * new_func = globalST->search(currST->table[0]->name);
									if(new_func == NULL)
									{
										new_func = globalST->lookup(currST->table[0]->name);
										$$.loc = currST->table[0];
										if(new_func->var_type == "")
										{
											/*Function is being declared here for the first time*/
											new_func->type = CopyType(currST->table[0]->type);
											new_func->var_type = "func";
											new_func->isInitialized = false;
											new_func->nestedTable = currST;
											/*Change the first element to retval and change the rest to param*/
											currST->name = currST->table[0]->name;
											//currST->table[0]->name = "retVal";
											currST->table[0]->var_type = "return";
											currST->table[0]->size = currST->table[0]->type->getSize();
											currST->table[0]->offset = 0;
											currST->offset = currST->table[0]->size;
											for(int i=1;i<currST->table.size();i++)
											{
												currST->table[i]->var_type = "param";
												currST->table[i]->offset += currST->table[0]->size;
												currST->offset += currST->table[i]->size;
											}
											/*After Function Declaration new Symbol Table is created*/
										}
									}
									else
									{
										// Already declared function. Therefore drop the new table and connect current symbol table pointer to the previously created funciton symbol table
										currST = new_func->nestedTable;
									}
									currST->start_quad = nextinstr;
									$$.loc = new_func;
									$$.type = new typeNode(t_FUNC);
								}|
								direct_declarator '(' identifier_list_opt ')' {
									/*No Parameters for this function*/
									symtabelem * new_func = globalST->search(currST->table[0]->name);
									if(new_func == NULL)
									{
										new_func = globalST->lookup(currST->table[0]->name);
										$$.loc = currST->table[0];
										if(new_func->var_type == "")
										{
											/*Function is being declared here for the first time*/
											new_func->type = CopyType(currST->table[0]->type);
											new_func->var_type = "func";
											new_func->isInitialized = false;
											new_func->nestedTable = currST;
											/*Change the first element to retval and change the rest to param*/
											currST->name = currST->table[0]->name;
											//currST->table[0]->name = "retVal";
											currST->table[0]->var_type = "return";
											currST->table[0]->size = currST->table[0]->type->getSize();
											currST->table[0]->offset = 0;
											currST->offset = currST->table[0]->size;
										}
									}
									else
									{
										// Already declared function. Therefore drop the new table and connect current symbol table pointer to the previously created funciton symbol table
										currST = new_func->nestedTable;
									}
									currST->start_quad = nextinstr;
									$$.loc = new_func;
									$$.type = new typeNode(t_FUNC);
								};

type_qualifier_list_opt:		type_qualifier_list {}|
								/*Epslion*/	{};

assignment_expression_opt:		assignment_expression {
									$$ = $1;
								}|
								/*Epslion*/ {
									$$.loc = NULL;
								};

identifier_list_opt:			identifier_list 										|
								/*Epslion*/												;

pointer:						'*' type_qualifier_list_opt {
									$$.type = new typeNode(t_PTR);
								}|
								'*' type_qualifier_list_opt pointer {
									$$.type = new typeNode(t_PTR,1,$3.type);
								};

type_qualifier_list:			type_qualifier {}|
								type_qualifier_list type_qualifier {};

parameter_type_list:			parameter_list {
									/*No need to store the List Explicitly since all the parameters are stored in the current symbol table by default*/
								}|
								parameter_list ',' ELLIPSIS	{};

parameter_list:					parameter_declaration {
									/*First Parameter Added here*/
								}|
								parameter_list ',' parameter_declaration {
									/*More Parameters are already added here*/
								};

parameter_declaration:			declaration_specifiers declarator {
									/*The parameter is already added to the current Symbol Table*/
								}|
								declaration_specifiers {};

identifier_list :				IDENTIFIER 												|
								identifier_list ',' IDENTIFIER							;

type_name:						specifier_qualifier_list								;

initializer:					assignment_expression {
									$$ = $1;
								}|
								'{' initializer_list '}' {}|
								'{' initializer_list ',' '}' {};

initializer_list:				designation_opt initializer 							|
								initializer_list ',' designation_opt initializer		;																															

designation_opt:				designation 											|
								/*Epslion*/												;

designation:					designator_list '='										;

designator_list:				designator 												|
								designator_list designator								;

designator:						'[' constant_expression ']' 							|
								'.' IDENTIFIER {};

/*Statements*/
statement:						labeled_statement {/*Switch Case*/}|
								compound_statement {
									$$ = $1;
								}|
								expression_statement {
									$$ = NULL;
								}|
								selection_statement	{
									$$ = $1;
								}|
								iteration_statement {
									$$ = $1;
								}|
								jump_statement {
									$$ = $1;
								};

labeled_statement:				IDENTIFIER ':' statement {}|
								CASE constant_expression ':' statement {}|
								DEFAULT ':' statement {};

compound_statement:				'{' block_item_list_opt '}'	{
									$$ = $2;
								};

block_item_list_opt:			block_item_list {
									$$ = $1;
								}|	
								/*Epslion*/	{
									$$ = NULL;
								};

block_item_list:				block_item {
									$$ = $1;
								}|
								block_item_list M block_item {
									backpatch($1,$2);
									$$ = $3;
								};

block_item:						declaration {
									$$ = NULL;
								}|
								statement {
									$$ = $1;
								};

expression_statement:			expression_opt ';'{
									$$ = $1;
								};

expression_opt:					expression {
									$$ = $1;
								}|
								/*Epslion*/	{
									/*Initialize Expression to NULL*/
									$$.loc = NULL;
								};

selection_statement:			IF '(' expression N ')' M statement N ELSE M statement {
									/* A dangling goto is present after expression just in case if the type of the expression is not boolean
									then a type conversion will be performed. But the code of the type conversion will be placed at the end.
									Therefore, this dangling pointer needs to be pointing the newly created if which checks whether the expression is zero or not.
									Another dangling goto after the statement of IF condition. That is also just for safety. It will be used if the nested statement will also have some conditional statements
									There are Ms in between to store the location / quad index of first list of statements and second list of statements
									*/
									$7 = merge($7,$8);
									$11 = merge($11,makelist(nextinstr));
									quad.emit(o_GOTO,"-1");
									backpatch($4,nextinstr);

									conv2Bool(&$3);

									backpatch($3.truelist,$6);
									backpatch($3.falselist,$10);
									$$ = merge($7,$11);
								}|
								IF '(' expression N ')' M statement {
									/* Very similar to IF and ELSE part. Here the else part is missing so we do not specifically need dangling goto for the statements list. We will however need dangling goto for expression (if it is not boolean)*/
									$7 = merge($7,makelist(nextinstr));
									quad.emit(o_GOTO,"-1");
									backpatch($4,nextinstr);
									conv2Bool(&$3);
									backpatch($3.truelist,$6);
									$$ = merge($7,$3.falselist);
								}|
								SWITCH '(' expression ')' statement	{};

iteration_statement:			WHILE '(' M expression N ')' M statement {
									/*The has to be goto which takes again to the beginning of the loop where we check the conditions. For storing that location we have a M in front of the expression. Also if the expression returns success then we have to jump to the statements list. Therefore, we again need to store the location of the beginning of the statement list. Thus, we have one more M in front of the statment list.
									*/
									quad.emit(o_GOTO,$3);
									backpatch($8,$3); 			/*S.nextlist to M1.instr*/
									backpatch($5,nextinstr); 	/*N1.nextlist to nextinstr*/
									conv2Bool(&$4);
									backpatch($4.truelist,$7);
									$$ = $4.falselist;
								}|
								DO M statement  WHILE '(' M expression N ')' ';' {
									/*Here we don't have to by default jump to the start. The output of the condtion of the while will decide whether we have to jump to the beginning of the loop or to the next instruction.
									Thus, we have M in front of the statements to store its locaitons. We also have M in front of the expression because there can be conditional statements in statement block. So there must be a pointer which stores the location of the start of expression. Again we have a dangling goto after expression (if it is not boolean)
									*/
									backpatch($8,nextinstr);
									backpatch($3,$6);			/*S1.nextlist to M2.instr*/
									conv2Bool(&$7);
									backpatch($7.truelist,$2);	/*B.truelist to M1.instr*/
									$$ = $7.falselist;
								}|
								FOR '(' expression_opt ';' M expression_opt N ';' M expression_opt N ')' M statement {
									/*After the loop end there has to be a goto to 3rd expression. Thus, we have a M before the third expression. After the execution of the third expression we move to the middle condition. Dangling pointer is there if the expression is not boolean. If the condition is successful then we have to go to the statements list and therefore, we have a M before the statements list.*/
									backpatch($11,$5);			/*N2.nextlist to M1.instr*/
									backpatch($14,$9);			/*S.nextlist to M2.instr*/
									quad.emit(o_GOTO,$9);
									backpatch($7,nextinstr);	/*N1.nextlist to nextinstr*/
									conv2Bool(&$6);
									backpatch($6.truelist,$13);
									$$ = $6.falselist;
								}|
								FOR '(' declaration expression_opt ';' expression_opt ')' statement {};

jump_statement:					GOTO IDENTIFIER ';' {}|
								CONTINUE ';' {}|
								BREAK ';' {}|
								RETURN expression_opt ';' {
									if($2.loc == NULL)
										quad.emit(o_RET);
									else
									{
										expStruct * dummy = new expStruct();
										dummy->loc = currST->table[0];
										dummy->type = dummy->loc->type;
										typecheck(dummy,&$2,true);
										delete dummy;
										quad.emit(o_RET,$2.loc->name);
									}
									$$ = NULL;
								};

/*External Definitions*/
translation_unit:				external_declaration									|
								translation_unit external_declaration					;

external_declaration:			function_definition										|
								declaration												;

function_definition:	declaration_specifiers declarator declaration_list_opt compound_statement {
									symtabelem * func = globalST->lookup($2.loc->name);
									/*The first entry of the current symbol table is the fucntion name. It has to be changes to retVal. Define the function and reinitialize the current symbol table so that it is ready to take new inputs of functions*/
									func->nestedTable->table[0]->type = CopyType(func->type);
									func->nestedTable->table[0]->name = "retVal";
									func->nestedTable->table[0]->offset = 0;
									/*Adjust offset and append current Symbol table to funciton symbol table*/
									/*If return type is pointer then change the offset*/
									if(func->nestedTable->table[0]->type->baseType == t_PTR)
									{
										int diff = size_of_ptr - func->nestedTable->table[0]->size;
										func->nestedTable->table[0]->size = size_of_ptr;
										for(int i=1;i<func->nestedTable->table.size();i++)
										{
											func->nestedTable->table[i]->offset += diff;
										}
									}
									int offset_size = 0;
									for(int i=0;i<func->nestedTable->table.size();i++)
									{
										offset_size += func->nestedTable->table[i]->size;
									}
									/*Getting the last quad index of the function*/
									printf("function name = %s\n",func->nestedTable->name.c_str());
									func->nestedTable->end_quad = nextinstr-1;
									
									/*Function Definition Ended so Create a new Current Symbol Table*/
									currST = new SymbolTable();
								};

declaration_list_opt:			declaration_list 										|
								/*Epslion*/												;

declaration_list:				declaration 											|
								declaration_list declaration							;

%%
void yyerror(const char*s)
{
	printf("%s",s);
}
