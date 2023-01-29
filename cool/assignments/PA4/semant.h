#ifndef SEMANT_H_
#define SEMANT_H_

#include <assert.h>
#include <iostream>
#include <map>
#include <vector>
#include <set>
#include <symtab.h>
#include "cool-tree.h"
#include "stringtab.h"
#include "symtab.h"
#include "list.h"

#define TRUE 1
#define FALSE 0


struct InheritElem
{
	Symbol parent;
	int in_degrees;
	InheritElem(Symbol p) : parent(p), in_degrees(0) {}
	InheritElem(Symbol par, int in_degr):parent(par), in_degrees(in_degr){}
};
class SymbolInfo{
public:
	Symbol static_type;
	SymbolInfo(Symbol s): static_type(s){}
};

// This is a structure that may be used to contain the semantic
// information such as the inheritance graph.  You may use it or not as
// you like: it is only here to provide a container for the supplied
// methods.

class ClassTable {
private:
	int semant_errors;
	ostream &error_stream;

	void install_basic_classes();
	void build_inherit_graph(Classes &classes);
    void verify_inheritance(Class_ cls);
    void verify_acyclic();

public:
	Class_ cur_cls = NULL;
	SymbolTable<Symbol, SymbolInfo> obj_env;
	std::map<Symbol, MethodSignatures> method_env;
	std::map<Symbol, Class_> type_nodes;
	std::map<Symbol, InheritElem> inherit_graph;

	ClassTable(Classes);
	int errors() { return semant_errors; }
	ostream &semant_error();
	ostream &semant_error(Class_ c);
	ostream &semant_error(Symbol filename, tree_node *t);
	ostream &cur_cls_semant_err();
	ostream &cur_node_semant_err(tree_node* t);

    void print_context();
	void print_obj_env();
	void print_method_env();
	void print_type_nodes();
	void print_inherit_graph();
};


#endif
