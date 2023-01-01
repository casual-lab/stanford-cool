/*
 *  The scanner definition for COOL.
 */

/*
 *  Stuff enclosed in %{ %} in the first section is copied verbatim to the
 *  output, so headers and global definitions are placed here to be visible
 * to the code in the file.  Don't remove anything that was here initially
 */
%{
#include <cool-parse.h>
#include <stringtab.h>
#include <utilities.h>

/* The compiler assumes these identifiers. */
#define yylval cool_yylval
#define yylex  cool_yylex

/* Max size of string constants */
#define MAX_STR_CONST 1025
#define YY_NO_UNPUT   /* keep g++ happy */

extern FILE *fin; /* we read from this file */

/* define YY_INPUT so we read from the FILE fin:
 * This change makes it possible to use this scanner in
 * the Cool compiler.
 */
#undef YY_INPUT
#define YY_INPUT(buf,result,max_size) \
	if ( (result = fread( (char*)buf, sizeof(char), max_size, fin)) < 0) \
		YY_FATAL_ERROR( "read() in flex scanner failed");

char string_buf[MAX_STR_CONST]; /* to assemble string constants */
char *string_buf_ptr;

extern int curr_lineno;
extern int verbose_flag;

extern YYSTYPE cool_yylval;

/*
 *  Add Your own definitions here
 */
# define cool_yyinput(c)  { c = yyinput(); if (c == '\n') curr_lineno++; }
# define take_char(cc)    { if(string_buf_ptr==(string_buf+(MAX_STR_CONST))) cool_yylval.error_msg = "String constant too long"; \
                            else if (string_buf_ptr < (string_buf+(MAX_STR_CONST))) *string_buf_ptr = cc; \
                            string_buf_ptr++;}

static int comment_level = 0;
%}
/*
 * Define names for regular expressions here.
 */
%option noyywrap
%x      COMMENT
%x      STR

SINGLE_LETTER_OPT ("."|"@"|"~"|"+"|"-"|"*"|"/"|"="|"("|")"|"{"|"}"|";"|","|":"|"<")

%%

 /*
  * Comments
  */
"(*"          { comment_level = 1; BEGIN(COMMENT);}
"*)"          {yylval.error_msg = "Unmatched *)"; return ERROR;}
<COMMENT>{
  [^*\n(]*         /* eat anything that's not a '*' */
  "("
  \n              curr_lineno++;
  "*"+[^*)\n]*  /* eat up '*'s not followed by ')'s */
  "(*"            comment_level++;
  "*"+")"         {comment_level--; if(comment_level == 0) BEGIN(INITIAL);}
  <<EOF>>         {yylval.error_msg = "EOF in comment"; BEGIN(INITIAL);return ERROR;}
}
"--"[^\n]*          /* eat up a one-line comment */


 /*
  *  The multiple-character operators.
  */
"=>"		{ return (DARROW); }
"<-"    { return (ASSIGN); }
"<="    { return (LE); }
{SINGLE_LETTER_OPT}         { return int(yytext[0]); }

 /*
  * Keywords are case-insensitive except for the values true and false,
  * which must begin with a lower-case letter.
  */
(?i:class)                              { return CLASS; }
(?i:else)                               { return ELSE; }
(?i:fi)                                 { return FI; }
(?i:if)                                 { return IF; }
(?i:in)                                 { return IN; }
(?i:inherits)                           { return INHERITS; }
(?i:isvoid)                             { return ISVOID; }
(?i:let)                                { return LET; }
(?i:loop)                               { return LOOP; }
(?i:pool)                               { return POOL; }
(?i:then)                               { return THEN; }
(?i:while)                              { return WHILE; }
(?i:case)                               { return CASE; }
(?i:esac)                               { return ESAC; }
(?i:new)                                { return NEW; }
(?i:of)                                 { return OF; }
(?i:not)                                { return NOT; }
t(?i:rue)                               { yylval.boolean = 1; return (BOOL_CONST); }
f(?i:alse)                              { yylval.boolean = 0; return (BOOL_CONST); }

 /*
  * Identifiers 
  */
[A-Z][a-zA-Z0-9_]*   { yylval.symbol = idtable.add_string(yytext); return (TYPEID); }
[a-z][a-zA-Z0-9_]*   { yylval.symbol = idtable.add_string(yytext); return (OBJECTID); }

 /*
  * Integers Constants
  */
[0-9]+               { yylval.symbol = inttable.add_string(yytext); return (INT_CONST); }

 /*
  *  String constants (C syntax)
  *  Escape sequence \c is accepted for all characters c. Except for 
  *  \n \t \b \f, the result is c.
  *
  */
\"      {string_buf_ptr = string_buf; BEGIN(STR);}
<STR>{
  \"        { /* saw closing quote - all done */
              BEGIN(INITIAL);
              if (string_buf_ptr >= string_buf+(MAX_STR_CONST)){
                  yylval.error_msg = "String constant too long";
                  return ERROR;
              }
              *string_buf_ptr = '\0';
              yylval.symbol = stringtable.add_string(string_buf);
              return STR_CONST;
            }

  \n        { curr_lineno++; yylval.error_msg = "Unterminated string constant"; 
              BEGIN(INITIAL); return ERROR; }
  \0        { 
              int tmpc;
              while((tmpc = yyinput()) != '\n' && tmpc != '\"');
              yylval.error_msg = "String contains null character";
              BEGIN(INITIAL);
              return ERROR;}
  <<EOF>>   { yylval.error_msg = "EOF in string constant"; BEGIN(INITIAL);return ERROR;}

  \\[^ntbf\0]    if(yytext[1] == '\n') curr_lineno++; take_char(yytext[1]) 


  \\n  take_char('\n')
  \\t  take_char('\t')
  /* \\r  take_char('\r') */    /* not allowed as in COOL specification */
  \\b  take_char('\b')
  \\f  take_char('\f')

  ([^\\\n\"\0]+)|\\        {                      /* anything else of a string */
          char *yptr = yytext;
          while ( *yptr ){
              if (string_buf_ptr >= string_buf+(MAX_STR_CONST)){
                yylval.error_msg = "String constant too long";
                break;
              }
              *string_buf_ptr++ = *yptr++;
          }
  }
}

 /*
  * whitespaces
  */
[ \f\r\t\v]+
\n            curr_lineno++;
[^\n\" \f\r\t\v0-9a-zA-Z]   {
    yylval.error_msg = yytext;
    return ERROR;
}
%%
