//
// The following include files must come first.

#ifndef COOL_TREE_HANDCODE_H
#define COOL_TREE_HANDCODE_H

#include <iostream>
#include <vector>
#include <map>
#include "tree.h"
#include "cool.h"
#include "stringtab.h"
#define yylineno curr_lineno;
extern int yylineno;

inline Boolean copy_Boolean(Boolean b) {return b; }
inline void assert_Boolean(Boolean) {}
inline void dump_Boolean(ostream& stream, int padding, Boolean b)
	{ stream << pad(padding) << (int) b << "\n"; }

void dump_Symbol(ostream& stream, int padding, Symbol b);
void assert_Symbol(Symbol b);
Symbol copy_Symbol(Symbol b);

class Program_class;
typedef Program_class *Program;
class Class__class;
typedef Class__class *Class_;
class Feature_class;
typedef Feature_class *Feature;
class Formal_class;
typedef Formal_class *Formal;
class Expression_class;
typedef Expression_class *Expression;
class Case_class;
typedef Case_class *Case;

typedef list_node<Class_> Classes_class;
typedef Classes_class *Classes;
typedef list_node<Feature> Features_class;
typedef Features_class *Features;
typedef list_node<Formal> Formals_class;
typedef Formals_class *Formals;
typedef list_node<Expression> Expressions_class;
typedef Expressions_class *Expressions;
typedef list_node<Case> Cases_class;
typedef Cases_class *Cases;

class ClassTable;
typedef ClassTable *ClassTableP;
typedef std::vector<Symbol> MethodSignature;
typedef std::map<Symbol, MethodSignature> MethodSignatures;



#define Program_EXTRAS                          \
virtual void semant() = 0;			\
virtual void dump_with_types(ostream&,int) = 0; 		\
virtual void semant_walk(ClassTableP ctp) = 0; 

#define program_EXTRAS                          \
void semant();     				\
void semant_walk(ClassTableP ctp); \
void dump_with_types(ostream&, int);            

#define Class__EXTRAS                   \
virtual void dump_with_types(ostream&,int) = 0; 		\
virtual void resolve_inherited_attr(ClassTableP ctp) = 0; \
virtual void resolve_inherited_methods(ClassTableP ctp) = 0;	\
virtual void semant_walk(ClassTableP ctp) = 0; \
virtual void set_parent(Symbol p) = 0;	\
virtual Symbol get_filename() = 0;      \
virtual Symbol get_name() = 0;		\
virtual Symbol get_parent() = 0;	\
virtual Features get_features() = 0;	\


#define class__EXTRAS                                 \
void semant_walk(ClassTableP ctp); \
void dump_with_types(ostream&,int);                    \
void resolve_inherited_attr(ClassTableP ctp);	\
void resolve_inherited_methods(ClassTableP ctp);	\
void set_parent(Symbol p) { parent = p; }		\
Symbol get_filename() { return filename; }             \
Symbol get_name() { return name; }				\
Symbol get_parent() { return parent; }			\
Features get_features() { return features; }	


#define Feature_EXTRAS                                        \
virtual void dump_with_types(ostream&,int) = 0; 		\
virtual void semant_walk(ClassTableP ctp) = 0; \
virtual bool is_method() = 0;		\
virtual Symbol get_name() = 0;		\
virtual Symbol get_type() = 0;				\
virtual Expression get_expr() = 0;


#define Feature_SHARED_EXTRAS                                       \
void semant_walk(ClassTableP ctp); \
void dump_with_types(ostream&,int);    \
Symbol get_name(){ return name; }	


#define method_EXTRAS \
void resolve_method(MethodSignature* m); \
bool is_method(){ return true; }	\
Formals get_formals(){ return formals; }	\
Symbol get_return_type(){ return return_type; }	\
Symbol get_type(){ return return_type; }		\
Expression get_expr(){ return expr; }


#define attr_EXTRAS 	\
bool is_method(){ return false; } \
Symbol get_type(){ return type_decl; }	\
Expression get_expr(){ return init; }


#define Formal_EXTRAS                              \
virtual void dump_with_types(ostream&,int) = 0; 		\
virtual void semant_walk(ClassTableP ctp) = 0;	\
virtual Symbol get_type_decl() = 0;	\
virtual Symbol get_name() = 0;


#define formal_EXTRAS                           \
void semant_walk(ClassTableP ctp); \
void dump_with_types(ostream&,int);	\
Symbol get_type_decl(){ return type_decl; } 	\
Symbol get_name(){ return name; }


#define Case_EXTRAS                             \
virtual void dump_with_types(ostream&,int) = 0; 		\
virtual void semant_walk(ClassTableP ctp) = 0;	\
virtual Symbol get_ret_type() = 0;	\
virtual Symbol get_type_decl() = 0;


#define branch_EXTRAS                                   \
void semant_walk(ClassTableP ctp); \
void dump_with_types(ostream& ,int);	\
Symbol get_ret_type(){ return expr->type; }	\
Symbol get_type_decl(){ return type_decl; }


#define Expression_EXTRAS                    \
Symbol type;                                 \
Symbol get_type() { return type; }           \
Expression set_type(Symbol s) { type = s; return this; } \
virtual void dump_with_types(ostream&,int) = 0; 		\
virtual void semant_walk(ClassTableP ctp) = 0;  \
void dump_type(ostream&, int);               \
Expression_class() { type = (Symbol) NULL; }	\
virtual bool is_no_expr(){ return false; }	\
virtual char* get_expr_constructor() = 0;


#define Expression_SHARED_EXTRAS           \
void semant_walk(ClassTableP ctp); \
void dump_with_types(ostream&,int); 

#define assign_EXTRAS           \
char* get_expr_constructor(){ return "assign"; }         \
bool is_no_expr(){ return false; }


#define static_dispatch_EXTRAS          \
char* get_expr_constructor(){ return "static_dispatch"; }                \
bool is_no_expr(){ return false; }


#define dispatch_EXTRAS         \
char* get_expr_constructor(){ return "dispatch"; }               \
bool is_no_expr(){ return false; }


#define cond_EXTRAS             \
char* get_expr_constructor(){ return "cond"; }           \
bool is_no_expr(){ return false; }


#define loop_EXTRAS             \
char* get_expr_constructor(){ return "loop"; }           \
bool is_no_expr(){ return false; }


#define typcase_EXTRAS          \
char* get_expr_constructor(){ return "typcase"; }                \
bool is_no_expr(){ return false; }


#define block_EXTRAS            \
char* get_expr_constructor(){ return "block"; }          \
bool is_no_expr(){ return false; }


#define let_EXTRAS              \
char* get_expr_constructor(){ return "let"; }            \
bool is_no_expr(){ return false; }


#define plus_EXTRAS             \
char* get_expr_constructor(){ return "plus"; }           \
bool is_no_expr(){ return false; }


#define sub_EXTRAS              \
char* get_expr_constructor(){ return "sub"; }            \
bool is_no_expr(){ return false; }


#define mul_EXTRAS              \
char* get_expr_constructor(){ return "mul"; }            \
bool is_no_expr(){ return false; }


#define divide_EXTRAS           \
char* get_expr_constructor(){ return "divide"; }         \
bool is_no_expr(){ return false; }


#define neg_EXTRAS              \
char* get_expr_constructor(){ return "neg"; }            \
bool is_no_expr(){ return false; }


#define lt_EXTRAS               \
char* get_expr_constructor(){ return "lt"; }             \
bool is_no_expr(){ return false; }


#define eq_EXTRAS               \
char* get_expr_constructor(){ return "eq"; }             \
bool is_no_expr(){ return false; }


#define leq_EXTRAS              \
char* get_expr_constructor(){ return "leq"; }            \
bool is_no_expr(){ return false; }


#define comp_EXTRAS             \
char* get_expr_constructor(){ return "comp"; }           \
bool is_no_expr(){ return false; }


#define int_const_EXTRAS        \
char* get_expr_constructor(){ return "int_const"; }              \
bool is_no_expr(){ return false; }


#define bool_const_EXTRAS               \
char* get_expr_constructor(){ return "bool_const"; }             \
bool is_no_expr(){ return false; }


#define string_const_EXTRAS             \
char* get_expr_constructor(){ return "string_const"; }           \
bool is_no_expr(){ return false; }


#define new__EXTRAS             \
char* get_expr_constructor(){ return "new_"; }           \
bool is_no_expr(){ return false; }


#define isvoid_EXTRAS           \
char* get_expr_constructor(){ return "isvoid"; }         \
bool is_no_expr(){ return false; }


#define no_expr_EXTRAS          \
char* get_expr_constructor(){ return "no_expr"; }                \
bool is_no_expr(){ return true; }


#define object_EXTRAS           \
char* get_expr_constructor(){ return "object"; }         \
bool is_no_expr(){ return false; }						\
Symbol get_name(){ return name; }

#endif
