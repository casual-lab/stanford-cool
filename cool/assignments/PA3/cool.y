/*
*  cool.y
*              Parser definition for the COOL language.
*
*/
%{
  #include <iostream>
  #include "cool-tree.h"
  #include "stringtab.h"
  #include "utilities.h"
  
  extern char *curr_filename;
  
  
  /* Locations */
  #define YYLTYPE int              /* the type of locations */
  #define cool_yylloc curr_lineno  /* use the curr_lineno from the lexer
  for the location of tokens */
    
    extern int node_lineno;          /* set before constructing a tree node
    to whatever you want the line number
    for the tree node to be */
      
      
      #define YYLLOC_DEFAULT(Current, Rhs, N)         \
      Current = Rhs[1];                             \
      node_lineno = Current;
    
    
    #define SET_NODELOC(Current)  \
    node_lineno = Current;
    
    /* IMPORTANT NOTE ON LINE NUMBERS
    *********************************
    * The above definitions and macros cause every terminal in your grammar to 
    * have the line number supplied by the lexer. The only task you have to
    * implement for line numbers to work correctly, is to use SET_NODELOC()
    * before constructing any constructs from non-terminals in your grammar.
    * Example: Consider you are matching on the following very restrictive 
    * (fictional) construct that matches a plus between two integer constants. 
    * (SUCH A RULE SHOULD NOT BE  PART OF YOUR PARSER):
    
    plus_consts	: INT_CONST '+' INT_CONST 
    
    * where INT_CONST is a terminal for an integer constant. Now, a correct
    * action for this rule that attaches the correct line number to plus_const
    * would look like the following:
    
    plus_consts	: INT_CONST '+' INT_CONST 
    {
      // Set the line number of the current non-terminal:
      // ***********************************************
      // You can access the line numbers of the i'th item with @i, just
      // like you acess the value of the i'th exporession with $i.
      //
      // Here, we choose the line number of the last INT_CONST (@3) as the
      // line number of the resulting expression (@$). You are free to pick
      // any reasonable line as the line number of non-terminals. If you 
      // omit the statement @$=..., bison has default rules for deciding which 
      // line number to use. Check the manual for details if you are interested.
      @$ = @3;
      
      
      // Observe that we call SET_NODELOC(@3); this will set the global variable
      // node_lineno to @3. Since the constructor call "plus" uses the value of 
      // this global, the plus node will now have the correct line number.
      SET_NODELOC(@3);
      
      // construct the result node:
      $$ = plus(int_const($1), int_const($3));
    }
    
    */
    
    
    
    void yyerror(char *s);        /*  defined below; called for each parse error */
    extern int yylex();           /*  the entry point to the lexer  */
    
    /************************************************************************/
    /*                DONT CHANGE ANYTHING IN THIS SECTION                  */
    
    Program ast_root;	      /* the result of the parse  */
    Classes parse_results;        /* for use in semantic analysis */
    int omerrs = 0;               /* number of errors in lexing and parsing */
    %}
    
    /* A union of all the types that can be the result of parsing actions. */
    %union {
      Boolean boolean;
      Symbol symbol;
      Program program;
      Class_ class_;
      Classes classes;
      Feature feature;
      Features features;
      Formal formal;
      Formals formals;
      Case case_;
      Cases cases;
      Expression expression;
      Expressions expressions;
      char *error_msg;
    }
    
    /* 
    Declare the terminals; a few have types for associated lexemes.
    The token ERROR is never used in the parser; thus, it is a parse
    error when the lexer returns it.
    
    The integer following token declaration is the numeric constant used
    to represent that token internally.  Typically, Bison generates these
    on its own, but we give explicit numbers to prevent version parity
    problems (bison 1.25 and earlier start at 258, later versions -- at
    257)
    */
    %token CLASS 258 ELSE 259 FI 260 IF 261 IN 262 
    %token INHERITS 263 LET 264 LOOP 265 POOL 266 THEN 267 WHILE 268
    %token CASE 269 ESAC 270 OF 271 DARROW 272 NEW 273 ISVOID 274
    %token <symbol>  STR_CONST 275 INT_CONST 276 
    %token <boolean> BOOL_CONST 277
    %token <symbol>  TYPEID 278 OBJECTID 279 
    %token ASSIGN 280 NOT 281 LE 282 ERROR 283
    
    /*  DON'T CHANGE ANYTHING ABOVE THIS LINE, OR YOUR PARSER WONT WORK       */
    /**************************************************************************/
    
    /* Complete the nonterminal list below, giving a type for the semantic
    value of each non terminal. (See section 3.6 in the bison 
    documentation for details). */
    
    /* Declare types for the grammar's non-terminals. */
    %type <program> program
    %type <classes> class_list
    %type <class_> class
    
    /* You will want to change the following line. */
    %type <features> nullable_feature_list
    %type <feature> one_method one_attr one_feature
    %type <formals> formal_list
    %type <formal> one_formal
    %type <expressions> expr_params expr_stmts 
    %type <expression> one_expr let_main dispatch_expr
    %type <case_> one_case
    %type <cases> case_list
    
    /* Precedence declarations go here. */
    %right ASSIGN
    %left NOT
    %nonassoc LE '<' '='
    %left '+' '-'
    %left '*' '/'
    %left ISVOID
    %left '~'
    %left '@'
    %left '.'
    
    %%
    /* 
    Save the root of the abstract syntax tree in a global variable.
    */
    program	: class_list	{ @$ = @1; ast_root = program($1); }
    ;
    
    class_list
    : class ';'			/* single class */
    { @$ = @1;SET_NODELOC(@1);$$ = single_Classes($1);parse_results = $$; }
    | class_list class ';'	/* several classes */
    { @$ = @1;SET_NODELOC(@1);$$ = append_Classes($1,single_Classes($2));parse_results = $$; }
    | error ';'
    { @$ = @1;SET_NODELOC(@1);$$ = nil_Classes();parse_results = $$; }
    | class_list error ';'
    { @$ = @1;SET_NODELOC(@1);$$ = $1;parse_results = $$; }
    ; 
    
    /* If no parent is specified, the class inherits from the Object class. */
    class	: CLASS TYPEID '{' nullable_feature_list '}' 
    { @$ = @1;SET_NODELOC(@1);
      $$ = class_($2,idtable.add_string("Object"),$4,
                  stringtable.add_string(curr_filename)); }
    | CLASS TYPEID INHERITS TYPEID '{' nullable_feature_list '}' 
    { @$ = @1;SET_NODELOC(@1);
      $$ = class_($2,$4,$6,stringtable.add_string(curr_filename)); }
    | CLASS TYPEID INHERITS TYPEID '{' error '}' 
    { @$ = @1;SET_NODELOC(@1);
      $$ = class_($2,$4,nil_Features(),stringtable.add_string(curr_filename)); }
    | CLASS TYPEID '{' error '}' 
    { @$ = @1;SET_NODELOC(@1);
      $$ = class_($2,idtable.add_string("Object"), nil_Features(),
                  stringtable.add_string(curr_filename)); }
    ;
    
    /* Feature list may be empty, but no empty features in list. */
    nullable_feature_list
    : 
    { $$ = nil_Features(); }
    | one_feature ';'
    { @$ = @1;SET_NODELOC(@1);$$ = single_Features($1); }
    | nullable_feature_list one_feature ';'
    { @$ = @1;SET_NODELOC(@1);$$ = append_Features($1, single_Features($2)); }
    | nullable_feature_list error ';'
    { @$ = @1;SET_NODELOC(@1);$$ = $1; }
    | error ';'
    { @$ = @1;SET_NODELOC(@1);$$ = nil_Features(); }
    ;
    
    one_feature
    : one_attr {@$ = @1;SET_NODELOC(@1);$$ = $1;} 
    | one_method {@$ = @1;SET_NODELOC(@1);$$ = $1;} 
    ;

    one_method
    : OBJECTID '(' ')' ':' TYPEID '{' one_expr '}'
    { @$ = @1;SET_NODELOC(@1);$$ = method($1, nil_Formals(), $5, $7); }
    | OBJECTID '(' formal_list ')' ':' TYPEID '{' one_expr '}'
    { @$ = @1;SET_NODELOC(@1);$$ = method($1, $3, $6, $8); }
    ;
    
    one_attr
    : OBJECTID ':' TYPEID
    { @$ = @1;SET_NODELOC(@1);$$ = attr($1, $3, no_expr()); }
    | OBJECTID ':' TYPEID ASSIGN one_expr
    { @$ = @1;SET_NODELOC(@1);$$ = attr($1, $3, assign($1, $5)); }
    ;

    formal_list
    : one_formal
    { @$ = @1;SET_NODELOC(@1);$$ = single_Formals($1); }
    | formal_list ',' one_formal
    { @$ = @1;SET_NODELOC(@1);$$ = append_Formals($1, single_Formals($3)); }
    ;

    one_formal
    : OBJECTID ':' TYPEID
    { @$ = @1;SET_NODELOC(@1);$$ = formal($1, $3); }
    ;

    one_expr
    : OBJECTID ASSIGN one_expr
    { @$ = @1;SET_NODELOC(@1);$$ = assign($1, $3); }
    | dispatch_expr
    { @$ = @1;SET_NODELOC(@1);$$ = $1; }
    | IF one_expr THEN one_expr ELSE one_expr FI
    { @$ = @1;SET_NODELOC(@1);$$ = cond($2, $4, $6); }
    | WHILE one_expr LOOP one_expr POOL
    { @$ = @1;SET_NODELOC(@1);$$ = loop($2, $4); }
    | '{' expr_stmts '}'
    { @$ = @1;SET_NODELOC(@1);$$ = block($2); }
    | LET let_main
    { @$ = @1;SET_NODELOC(@1);$$ = $2; }
    | CASE one_expr OF case_list ESAC
    { @$ = @1;SET_NODELOC(@1);$$ = typcase($2, $4); }
    | NEW TYPEID
    { @$ = @1;SET_NODELOC(@1);$$ = new_($2); }
    | ISVOID one_expr
    { @$ = @1;SET_NODELOC(@1);$$ = isvoid($2); }
    | one_expr '+' one_expr
    { @$ = @1;SET_NODELOC(@1);$$ = plus($1, $3); }
    | one_expr '-' one_expr
    { @$ = @1;SET_NODELOC(@1);$$ = sub($1, $3); }
    | one_expr '*' one_expr
    { @$ = @1;SET_NODELOC(@1);$$ = mul($1, $3); }
    | one_expr '/' one_expr
    { @$ = @1;SET_NODELOC(@1);$$ = divide($1, $3); }
    | '~' one_expr
    { @$ = @1;SET_NODELOC(@1);$$ = neg($2); }
    | one_expr '<' one_expr
    { @$ = @1;SET_NODELOC(@1);$$ = lt($1, $3); }
    | one_expr LE one_expr
    { @$ = @1;SET_NODELOC(@1);$$ = leq($1, $3); }
    | one_expr '=' one_expr
    { @$ = @1;SET_NODELOC(@1);$$ = eq($1, $3); }
    | NOT one_expr
    { @$ = @1;SET_NODELOC(@1);$$ = comp($2); }
    | '(' one_expr ')'
    { @$ = @1;SET_NODELOC(@1);$$ = $2; }
    | OBJECTID
    { @$ = @1;SET_NODELOC(@1);$$ = object($1); }
    | INT_CONST
    { @$ = @1;SET_NODELOC(@1);$$ = int_const($1); }
    | STR_CONST
    { @$ = @1;SET_NODELOC(@1);$$ = string_const($1); }
    | BOOL_CONST
    { @$ = @1;SET_NODELOC(@1);$$ = bool_const($1); }
    ;

    expr_stmts
    : one_expr ';'
    { @$ = @1;SET_NODELOC(@1);$$ = single_Expressions($1); }
    | expr_stmts one_expr ';'
    { @$ = @1;SET_NODELOC(@1);$$ = append_Expressions($1, single_Expressions($2)); }
    | error ';'
    { @$ = @1;SET_NODELOC(@1);$$ = nil_Expressions(); }
    | expr_stmts error ';'
    { @$ = @1;SET_NODELOC(@1);$$ = $1; }
    ;

    dispatch_expr
    : one_expr '.' OBJECTID '(' expr_params ')'
    { @$ = @1;SET_NODELOC(@1);$$ = dispatch($1, $3, $5); }
    | one_expr '@' TYPEID '.' OBJECTID '(' expr_params ')'
    { @$ = @1;SET_NODELOC(@1);$$ = static_dispatch($1, $3, $5, $7); }
    | OBJECTID '(' expr_params ')'
    { @$ = @1;SET_NODELOC(@1);$$ = dispatch(object(idtable.add_string("self")), $1, $3); }
    ;

    expr_params
    : 
    { $$ = nil_Expressions(); }
    | one_expr
    { @$ = @1;SET_NODELOC(@1);$$ = single_Expressions($1); }
    | expr_params ',' one_expr
    { @$ = @1;SET_NODELOC(@1);$$ = append_Expressions($1, single_Expressions($3)); }
    ;

    let_main
    : OBJECTID ':' TYPEID ASSIGN one_expr IN one_expr
    { @$ = @1;SET_NODELOC(@1);$$ = let($1, $3, $5, $7); }
    | OBJECTID ':' TYPEID IN one_expr
    { @$ = @1;SET_NODELOC(@1);$$ = let($1, $3, no_expr(), $5); }
    | OBJECTID ':' TYPEID ASSIGN one_expr ',' let_main
    { @$ = @1;SET_NODELOC(@1);$$ = let($1, $3, $5, $7); }
    | OBJECTID ':' TYPEID ',' let_main
    { @$ = @1;SET_NODELOC(@1);$$ = let($1, $3, no_expr(), $5); }
    | error ',' let_main
    { @$ = @1;SET_NODELOC(@1);$$ = $3; }
    ;

    case_list
    : one_case
    { @$ = @1;SET_NODELOC(@1);$$ = single_Cases($1); }
    | case_list one_case
    { @$ = @1;SET_NODELOC(@1);$$ = append_Cases($1, single_Cases($2)); }
    ;

    one_case
    : OBJECTID ':' TYPEID DARROW one_expr ';'
    { @$ = @1;SET_NODELOC(@1);$$ = branch($1, $3, $5); }
    ;

    /* end of grammar */
    %%
    
    /* This function is called automatically when Bison detects a parse error. */
    void yyerror(char *s)
    {
      extern int curr_lineno;
      
      cerr << "\"" << curr_filename << "\", line " << curr_lineno << ": " \
      << s << " at or near ";
      print_cool_token(yychar);
      cerr << endl;
      omerrs++;
      
      if(omerrs>50) {fprintf(stdout, "More than 50 errors\n"); exit(1);}
    }
    
    