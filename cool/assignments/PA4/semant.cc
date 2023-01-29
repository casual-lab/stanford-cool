#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <set>
#include <map>
#include <stack>
#include "semant.h"
#include "utilities.h"
#include "cool-tree.h"

extern int semant_debug;
extern char *curr_filename;

//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
static Symbol 
    arg,
    arg2,
    Bool,
    concat,
    cool_abort,
    copy,
    Int,
    in_int,
    in_string,
    IO,
    length,
    Main,
    main_meth,
    No_class,
    No_type,
    Object,
    out_int,
    out_string,
    prim_slot,
    self,
    SELF_TYPE,
    Str,
    str_field,
    substr,
    type_name,
    val;
//
// Initializing the predefined symbols.
//
static void initialize_constants(void)
{
    arg         = idtable.add_string("arg");
    arg2        = idtable.add_string("arg2");
    Bool        = idtable.add_string("Bool");
    concat      = idtable.add_string("concat");
    cool_abort  = idtable.add_string("abort");
    copy        = idtable.add_string("copy");
    Int         = idtable.add_string("Int");
    in_int      = idtable.add_string("in_int");
    in_string   = idtable.add_string("in_string");
    IO          = idtable.add_string("IO");
    length      = idtable.add_string("length");
    Main        = idtable.add_string("Main");
    main_meth   = idtable.add_string("main");
    //   _no_class is a symbol that can't be the name of any 
    //   user-defined class.
    No_class    = idtable.add_string("_no_class");
    No_type     = idtable.add_string("_no_type");
    Object      = idtable.add_string("Object");
    out_int     = idtable.add_string("out_int");
    out_string  = idtable.add_string("out_string");
    prim_slot   = idtable.add_string("_prim_slot");
    self        = idtable.add_string("self");
    SELF_TYPE   = idtable.add_string("SELF_TYPE");
    Str         = idtable.add_string("String");
    str_field   = idtable.add_string("_str_field");
    substr      = idtable.add_string("substr");
    type_name   = idtable.add_string("type_name");
    val         = idtable.add_string("_val");
}

//////////////////////////////////////////////////////////////////
//  Printer Function
//////////////////////////////////////////////////////////////////



void ClassTable::print_inherit_graph(){
    cout<<"inherit_graph: "<<endl;
    if(inherit_graph.size() == 0) cout<<"\tempty"<<endl;
    for(auto p:inherit_graph){
        cout<<"\t"<<p.first->get_string()<<"->{ "<< \
        "in_degree: "<<p.second.in_degrees<< ", "<< \
        "parent: "<<p.second.parent->get_string()<<" }"<<endl;
    }
}
void ClassTable::print_type_nodes(){
    cout<<"type_nodes: "<<endl;
    if(type_nodes.size() == 0) cout<<"\tempty"<<endl;
    else cout<<"\t[";
    for(auto p:type_nodes){
        cout<<p.first->get_string()<<": "<<p.second->get_name()->get_string()<<", ";
    }
    cout<<"]"<<endl;
}
void ClassTable::print_method_env(){
    cout<<"method_env: "<<endl;
    if(method_env.size() == 0) cout<<"\tempty"<<endl;
    for(auto obj:method_env){
        cout<<"\t"<<obj.first->get_string()<<"->["<<endl;
        for(auto method: obj.second){
            cout<<"\t\t"<<method.first->get_string()<<"(";
            for(auto t:method.second){
                cout<<t->get_string()<<",";
            }
            cout<<"),"<<endl;
        }
        cout<<"\t],"<<endl;
    }
}
void ClassTable::print_obj_env(){
    cout<<"obj_env: "<<endl;
    obj_env.dump();
}
void ClassTable::print_context(){
    print_obj_env();
    print_inherit_graph();
    print_type_nodes();
    print_method_env();
}

///////////////////////////////////////////////////////////////
//  ClassTable Constructor
///////////////////////////////////////////////////////////////

void ClassTable::verify_inheritance(Class_ cls){
    static std::set<Symbol> not_inheritable{Int, Str, Bool, SELF_TYPE};

    if(not_inheritable.find(cls->get_parent()) != not_inheritable.end()){
        semant_error(cls)<<cls->get_parent()->get_string()<<" cannot be inherited."<<endl;
        cls->set_parent(Object);
    }
    if(type_nodes.find(cls->get_parent()) == type_nodes.end()){
        semant_error(cls)<<cls->get_parent()->get_string()<<" have never declared."<<endl;
        cls->set_parent(Object);
    }
    inherit_graph.at(cls->get_parent()).in_degrees++;
    inherit_graph.at(cls->get_name()).parent = cls->get_parent();
}

void ClassTable::build_inherit_graph(Classes &classes){
    // 1) Ensure: type_nodes only contains unique classes.
    for (int i = classes->first(); classes->more(i); i = classes->next(i)){
        Class_ cls = classes->nth(i);
        if(cls->get_name() == SELF_TYPE){
            semant_error(cls)<<"SELF_TYPE cannot be redefined."<<endl;
            continue;
        }
        bool inserted = type_nodes.emplace(cls->get_name(), cls).second;
        if(! inserted){
            semant_error(cls)<<cls->get_name()->get_string()<<" cannot be redefined."<<endl;
        }
        inherit_graph.emplace(cls->get_name(), *(new InheritElem(Object)));
    }
    // 2) try to establish inheritance relationship. 
    //    Undefined parents will be replaced with 'Object'
    for (auto p : type_nodes){
        if(p.second->get_name() == Object) continue;
        Class_ cls = p.second;
        InheritElem elm = inherit_graph.at(cls->get_name());

        verify_inheritance(cls);
    }
}

void ClassTable::verify_acyclic(){
    std::map<Symbol, InheritElem> inherit_graph_cp = inherit_graph;
    std::stack<Symbol> stk;
    int n_nodes = inherit_graph_cp.size();
    for(auto itr = inherit_graph_cp.begin(); itr != inherit_graph_cp.end(); itr++){
        if(0 == itr->second.in_degrees){ 
            stk.push(itr->first); 
            n_nodes --;
        }
    }
    while(stk.size() != 0){
        Symbol curve_from = stk.top();
        stk.pop();
        Symbol curve_to = inherit_graph_cp.at(curve_from).parent;
        if (curve_to && inherit_graph_cp.find(curve_to) != inherit_graph_cp.end() 
            && 0 == --(inherit_graph_cp.at(curve_to).in_degrees)){
            stk.push(curve_to);
            n_nodes --;
        }
    }
    if(n_nodes != 0){
        semant_error()<<"There exist cyclic inheritance."<<endl;
    }
}

ClassTable::ClassTable(Classes classes) : semant_errors(0), error_stream(cerr){
    install_basic_classes();
    build_inherit_graph(classes);
    if(semant_debug) {
        cout<<"# After build inherit graph."<<endl;
        print_inherit_graph();
        print_type_nodes();
    }
    verify_acyclic();
}

void ClassTable::install_basic_classes() {
    // The tree package uses these globals to annotate the classes built below.
   // curr_lineno  = 0;
    Symbol filename = stringtable.add_string("<basic class>");
    
    // The following demonstrates how to create dummy parse trees to
    // refer to basic Cool classes.  There's no need for method
    // bodies -- these are already built into the runtime system.
    
    // IMPORTANT: The results of the following expressions are
    // stored in local variables.  You will want to do something
    // with those variables at the end of this method to make this
    // code meaningful.

    // 
    // The Object class has no parent class. Its methods are
    //        abort() : Object    aborts the program
    //        type_name() : Str   returns a string representation of class name
    //        copy() : SELF_TYPE  returns a copy of the object
    //
    // There is no need for method bodies in the basic classes---these
    // are already built in to the runtime system.

    Class_ Object_class =
	class_(Object, 
	       No_class,
	       append_Features(
			       append_Features(
					       single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
					       single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
			       single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
	       filename);
    InheritElem *obj_elm = new InheritElem(No_class);
    inherit_graph.emplace(Object, *obj_elm);
    type_nodes.emplace(Object, Object_class);

    // 
    // The IO class inherits from Object. Its methods are
    //        out_string(Str) : SELF_TYPE       writes a string to the output
    //        out_int(Int) : SELF_TYPE            "    an int    "  "     "
    //        in_string() : Str                 reads a string from the input
    //        in_int() : Int                      "   an int     "  "     "
    //
    Class_ IO_class = 
	class_(IO, 
	       Object,
	       append_Features(
			       append_Features(
					       append_Features(
							       single_Features(method(out_string, single_Formals(formal(arg, Str)),
										      SELF_TYPE, no_expr())),
							       single_Features(method(out_int, single_Formals(formal(arg, Int)),
										      SELF_TYPE, no_expr()))),
					       single_Features(method(in_string, nil_Formals(), Str, no_expr()))),
			       single_Features(method(in_int, nil_Formals(), Int, no_expr()))),
	       filename);
    InheritElem *io_elm = new InheritElem(Object);
    inherit_graph.emplace(IO, *io_elm);
    type_nodes.emplace(IO, IO_class);

    //
    // The Int class has no methods and only a single attribute, the
    // "val" for the integer. 
    //
    Class_ Int_class =
	class_(Int, 
	       Object,
	       single_Features(attr(val, prim_slot, no_expr())),
	       filename);
    InheritElem *int_elm = new InheritElem(Object);
    inherit_graph.emplace(Int, *int_elm);
    type_nodes.emplace(Int, Int_class);

    //
    // Bool also has only the "val" slot.
    //
    Class_ Bool_class =
	class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename);
    InheritElem *bool_elm = new InheritElem(Object);
    inherit_graph.emplace(Bool, *bool_elm); 
    type_nodes.emplace(Bool, Bool_class);

    //
    // The class Str has a number of slots and operations:
    //       val                                  the length of the string
    //       str_field                            the string itself
    //       length() : Int                       returns length of the string
    //       concat(arg: Str) : Str               performs string concatenation
    //       substr(arg: Int, arg2: Int): Str     substring selection
    //       
    Class_ Str_class =
	class_(Str, 
	       Object,
	       append_Features(
			       append_Features(
					       append_Features(
							       append_Features(
									       single_Features(attr(val, Int, no_expr())),
									       single_Features(attr(str_field, prim_slot, no_expr()))),
							       single_Features(method(length, nil_Formals(), Int, no_expr()))),
					       single_Features(method(concat, 
								      single_Formals(formal(arg, Str)),
								      Str, 
								      no_expr()))),
			       single_Features(method(substr, 
						      append_Formals(single_Formals(formal(arg, Int)), 
								     single_Formals(formal(arg2, Int))),
						      Str, 
						      no_expr()))),
	       filename);
    InheritElem *str_elm = new InheritElem(Object);
    inherit_graph.emplace(Str, *str_elm);
    type_nodes.emplace(Str, Str_class);
}

////////////////////////////////////////////////////////////////////
//
// semant_error is an overloaded function for reporting errors
// during semantic analysis.  There are three versions:
//
//    ostream& ClassTable::semant_error()                
//
//    ostream& ClassTable::semant_error(Class_ c)
//       print line number and filename for `c'
//
//    ostream& ClassTable::semant_error(Symbol filename, tree_node *t)  
//       print a line number and filename
//
///////////////////////////////////////////////////////////////////

ostream& ClassTable::semant_error(Class_ c)
{                                                             
    return semant_error(c->get_filename(),c);
}    

ostream& ClassTable::semant_error(Symbol filename, tree_node *t)
{
    error_stream << filename << ":" << t->get_line_number() << ": ";
    return semant_error();
}

ostream& ClassTable::semant_error()                  
{                                                 
    semant_errors++;                            
    return error_stream;
} 

ostream& ClassTable::cur_cls_semant_err(){
    return semant_error(cur_cls);
}

ostream& ClassTable::cur_node_semant_err(tree_node* t){
    return semant_error(cur_cls->get_filename(), t);
}

////////////////////////////////////////////////////////////
//  Helper functions for type checking
////////////////////////////////////////////////////////////

bool type_le(Symbol a, Symbol b, 
             const std::map<Symbol, InheritElem> &inherit_graph, 
             const Class_ cur_cls){
    if (a == b) return true;
    if (b == SELF_TYPE) return false;
    if (a == SELF_TYPE){
        Symbol cur = cur_cls->get_name();
        return type_le(cur, b, inherit_graph, cur_cls);
    }
    
    while(inherit_graph.at(a).parent != No_class){
        if(inherit_graph.at(a).parent == b) return true;
        a = inherit_graph.at(a).parent;
    }
    return false;
}

bool type_le(Symbol a, Symbol b, ClassTableP ctp){
    return type_le(a, b, ctp->inherit_graph, ctp->cur_cls);
}

bool is_inherited_methods_conflicted(MethodSignatures* existing_sigs, 
                                     method_class* mm, 
                                     MethodSignature *sig){
    if(existing_sigs->find(mm->get_name()) == existing_sigs->end()) 
        return false;
    if(existing_sigs->at(mm->get_name()).size() != sig->size()) 
        return true;
    for(unsigned j = 0; j<sig->size(); j++){
        if (existing_sigs->at(mm->get_name())[j] != sig->at(j)) return true;
    }
    return false;
}

void do_dispatch_semant_walk(ClassTableP ctp, Expression expr, Symbol name, Symbol type_name,
                             Expressions actual, tree_node* cur_node, Symbol &type){
    expr->semant_walk(ctp);
    Symbol t0 = expr->type;
    MethodSignature sig;
    Symbol target_c;

    if(type_name == NULL){
        target_c = t0 == SELF_TYPE? ctp->cur_cls->get_name() : t0;
    }else{
        if(type_name == SELF_TYPE || ! type_le(t0, type_name, ctp)){
            ctp->cur_node_semant_err(cur_node)<<"Static-dispatch Type Error. "<<endl;
            type = Object;
            return;
        }
        target_c = type_name;
    }
    if(ctp->method_env.find(target_c) == ctp->method_env.end()){
        ctp->cur_node_semant_err(cur_node)<<"Class used in the dispatch is not found."<<endl;
        type = Object; return;
    }
    if(ctp->method_env.at(target_c).find(name) == ctp->method_env.at(target_c).end()){
        ctp->cur_node_semant_err(cur_node)<<"Method not found."<<endl;
        type = Object; return;
    }
    sig = ctp->method_env.at(target_c).at(name);
    if(sig.size()-1 != (unsigned)actual->len()){
        ctp->cur_node_semant_err(cur_node)<< \
        "The number of parameters is inconsistent with the method signature."<< \
        name->get_string()<<endl;
    }
    for(unsigned i = 0, j = actual->first(); 
        i < sig.size()-1 && actual->more(j); 
        i++, j=actual->next(j))
    {
        actual->nth(j)->semant_walk(ctp);
        Symbol ti_ = sig[i], ti = actual->nth(j)->type;
        if(! type_le(ti, ti_, ctp)){
            ctp->cur_node_semant_err(cur_node)<< \
            "Type Error of the parameter at the position "<<i<<"."<<endl;
        }
    }
    if(sig.back() == SELF_TYPE){
        type = t0;
    }else{
        type = sig.back();
    }
}

int inherit_len(Symbol a, const std::map<Symbol, InheritElem> &inherit_graph){
    int l = 0;
    while(inherit_graph.at(a).parent != No_class){
        l++;
        a = inherit_graph.at(a).parent;
    }
    return l;
}

Symbol type_lub(Symbol a, Symbol b, Symbol cur_cls, 
              const std::map<Symbol, InheritElem> &inherit_graph){
    if(a == b) return a;
    if(a == SELF_TYPE) return type_lub(cur_cls, b, cur_cls, inherit_graph);
    if(b == SELF_TYPE) return type_lub(a, cur_cls, cur_cls, inherit_graph);

    int al = inherit_len(a, inherit_graph), bl = inherit_len(b, inherit_graph);
    if(al < bl) {
        int tl = al; al = bl; bl = tl;
        Symbol t = a; a = b; b = t;
    }
    
    while(al > bl){
        a = inherit_graph.at(a).parent; al--;
    }
    while(a != b && a != No_class && b != No_class){
        a = inherit_graph.at(a).parent;
        b = inherit_graph.at(b).parent;
    }
    assert(a == b && a != No_class && b != No_class);
    return a;
}

Symbol type_lub(Symbol a, Symbol b, ClassTableP ctp){
    return type_lub(a, b, ctp->cur_cls->get_name(), ctp->inherit_graph);
}

void arith_semant_walk(ClassTableP ctp, Expression e1, Expression e2, Symbol &type, tree_node* node){
    e1->semant_walk(ctp);
    e2->semant_walk(ctp);
    if(e1->type != Int ||e2->type != Int){
        ctp->cur_node_semant_err(node)<<"Only integers can participate in arithmatic operations."<<endl;
    }
    type = Int;
}

////////////////////////////////////////////////////////////////
//  recursive function for walking on AST
////////////////////////////////////////////////////////////////

void no_expr_class::semant_walk(ClassTableP ctp){
    type = No_type;
}

void eq_class::semant_walk(ClassTableP ctp){
    static const std::set<Symbol> spec{Int, Bool, Str};

    e1->semant_walk(ctp);
    e2->semant_walk(ctp);
    
    if((spec.find(e1->type) != spec.end() || spec.find(e2->type) != spec.end()) 
       && e1->type != e2->type){
        ctp->cur_node_semant_err(this)<< \
        "If ether side of \"=\" has type Int, Str, Bool, both sides must have the same type."<<endl;
    }
    type = Bool;
}

void divide_class::semant_walk(ClassTableP ctp){
    arith_semant_walk(ctp, e1, e2, type, this);
}

void mul_class::semant_walk(ClassTableP ctp){
    arith_semant_walk(ctp, e1, e2, type, this);
}

void sub_class::semant_walk(ClassTableP ctp){
    arith_semant_walk(ctp, e1, e2, type, this);
}

void plus_class::semant_walk(ClassTableP ctp){
    arith_semant_walk(ctp, e1, e2, type, this);
}

void neg_class::semant_walk(ClassTableP ctp){
    e1->semant_walk(ctp);
    if( e1->type != Int){
        ctp->cur_node_semant_err(this)<<"The expression after \"~\" must have type Int."<<endl;
    }
    type = Int;
}

void leq_class::semant_walk(ClassTableP ctp){
    e1->semant_walk(ctp);
    e2->semant_walk(ctp);
    if(e1->type != Int || e2->type != Int){
        ctp->cur_node_semant_err(this)<<"Only variables of Int are comparable."<<endl;
    }
    type = Bool;
}

void lt_class::semant_walk(ClassTableP ctp){
    e1->semant_walk(ctp);
    e2->semant_walk(ctp);
    if(e1->type != Int || e2->type != Int){
        ctp->cur_node_semant_err(this)<<"Only variables of Int are comparable."<<endl;
    }
    type = Bool;
}

void comp_class::semant_walk(ClassTableP ctp){
    e1->semant_walk(ctp);
    if( e1->type != Bool){
        ctp->cur_node_semant_err(this)<<"The expression after \"NOT\" must have type Bool."<<endl;
    }
    type = Bool;
}

void isvoid_class::semant_walk(ClassTableP ctp){
    e1->semant_walk(ctp);
    type = Bool;
}

void loop_class::semant_walk(ClassTableP ctp){
    pred->semant_walk(ctp);
    if(pred->type != Bool){
        ctp->cur_node_semant_err(this)<<"The predicate of a loop must have type Bool."<<endl;
    }
    body->semant_walk(ctp);
    type = Object;
}

void branch_class::semant_walk(ClassTableP ctp){
    ctp->obj_env.enterscope();
    ctp->obj_env.addid(name, new SymbolInfo(type_decl));
    expr->semant_walk(ctp);
    ctp->obj_env.exitscope();
}

void typcase_class::semant_walk(ClassTableP ctp){
    expr->semant_walk(ctp);
    if(expr->type == No_type){
        ctp->cur_node_semant_err(this)<<"Case expression cannot receive void."<<endl;
    }

    std::set<Symbol> existing_type;
    for(int i = cases->first(); cases->more(i); i = cases->next(i)){
        Case branch = cases->nth(i);
        if(! existing_type.emplace(branch->get_type_decl()).second){
            ctp->cur_node_semant_err(this)<<"Duplicate branch " \
            <<branch->get_type_decl()->get_string()<<" in case statement."<<endl;
        }
        branch->semant_walk(ctp);
        type = (i == cases->first())? \
            (branch->get_ret_type()):type_lub(type, branch->get_ret_type(), ctp);
    }
}

void let_class::semant_walk(ClassTableP ctp){
    ctp->obj_env.enterscope();
    
    init->semant_walk(ctp);

    if(! init->is_no_expr() && ! type_le(init->type, type_decl, ctp)){
        ctp->cur_node_semant_err(this)<<"Type Error on variable "<<identifier->get_string()<<"." \
        <<type_decl->get_string()<<" is required but "<<init->type->get_string()<<" is found."<<endl;
    }
    if(identifier == self){
        ctp->cur_node_semant_err(this)<<"\"self\" cannot be redefined."<<endl;
    }
    ctp->obj_env.addid(identifier, new SymbolInfo(type_decl));
    body->semant_walk(ctp);
    ctp->obj_env.exitscope();
    type = body->type;
}

void block_class::semant_walk(ClassTableP ctp){
    ctp->obj_env.enterscope();
    type = Object;
    for(
        int i = body->first(); 
        (bool)(body->more(i)); 
        i = body->next(i))
        {
        body->nth(i)->semant_walk(ctp);
        type = body->nth(i)->type;
    }
    ctp->obj_env.exitscope();
}

void cond_class::semant_walk(ClassTableP ctp){
    pred->semant_walk(ctp);
    then_exp->semant_walk(ctp);
    else_exp->semant_walk(ctp);

    if(pred->type != Bool){
        ctp->cur_node_semant_err(this)<<"There must be a expression of Bool type."<<endl;
    }
    type = type_lub(then_exp->type, else_exp->type, ctp);
}

void static_dispatch_class::semant_walk(ClassTableP ctp){
    do_dispatch_semant_walk(ctp, expr, name, type_name, actual, this, type);
}

void dispatch_class::semant_walk(ClassTableP ctp){
    do_dispatch_semant_walk(ctp, expr, name, NULL, actual, this, type);
}

void new__class::semant_walk(ClassTableP ctp){
    type = type_name;
}

void string_const_class::semant_walk(ClassTableP ctp){
    type = Str;
}

void int_const_class::semant_walk(ClassTableP ctp){
    type = Int;
}

void bool_const_class::semant_walk(ClassTableP ctp){
    type = Bool;
}

void assign_class::semant_walk(ClassTableP ctp){
    expr->semant_walk(ctp);
    if(name == self){
        ctp->cur_node_semant_err(this)<<"Self cannot be assigned."<<endl;
        type = expr->type;
        return;
    }
    if(!ctp->obj_env.lookup(name)){
        ctp->cur_node_semant_err(this)<<name->get_string()<< \
        " must be declared before being used."<<endl;
        type = expr->type;
        return;
    }
    Symbol id_type = ctp->obj_env.lookup(name)->static_type;
    if(! type_le(expr->type, id_type, ctp)){
        type = id_type;
        ctp->cur_node_semant_err(this)<< \
        "The right value of assignment must be of a subclass inheriting from that of the identifier."<<endl;
        return;
    }
    type = expr->type;
}

void object_class::semant_walk(ClassTableP ctp){
    if(!ctp->obj_env.lookup(name)){
        ctp->cur_node_semant_err(this)<<name->get_string()<<" must be declared before being used."<<endl;
        type = Object;
        return;
    }
    type = ctp->obj_env.lookup(name)->static_type;
}

void formal_class::semant_walk(ClassTableP ctp){
    SymbolInfo *info = new SymbolInfo(get_type_decl());
    if(type_decl == SELF_TYPE){
        ctp->cur_node_semant_err(this)<<"SELF_TYPE cannot be declaring type in a formal."<<endl;
        info->static_type = Object;
    }
    if(ctp->obj_env.probe(get_name())){
        ctp->cur_node_semant_err(this)<<"Formal parameter "<<get_name()<<" is multiply defined."<<endl;
    }
    ctp->obj_env.addid(get_name(), info);
}

void method_class::semant_walk(ClassTableP ctp){
    if(semant_debug){
        printf("# Line %-4d Entering method: %s.%s:%s\n", line_number, 
                ctp->cur_cls->get_name()->get_string(), 
                name->get_string(), 
                return_type->get_string());
    }

    ctp->obj_env.enterscope();
    ctp->obj_env.addid(self, new SymbolInfo(SELF_TYPE));
    for (int i = formals->first(); formals->more(i); i = formals->next(i)){
        formals->nth(i)->semant_walk(ctp);
    }
    expr->semant_walk(ctp);
    if(return_type != SELF_TYPE && ctp->type_nodes.find(return_type) == ctp->type_nodes.end()){
        ctp->cur_node_semant_err(this)<<"Return type is not defined."<<endl;
    }
    if(!type_le(expr->type, return_type, ctp)){
        ctp->cur_node_semant_err(this)<<"Return type must conform to the signature. " \
        <<return_type->get_string()<<" is required but "<<expr->type->get_string()<<" is found."<<endl;
    }
    ctp->obj_env.exitscope();
}

void attr_class::semant_walk(ClassTableP ctp){
    SymbolInfo *info = new SymbolInfo(get_type());

    if(semant_debug){
        printf("# Line %-4d: Entering attribute: %s: %s\n", line_number, name->get_string(), type_decl->get_string());
    }
    
    ctp->obj_env.enterscope();
    ctp->obj_env.addid(self, new SymbolInfo(SELF_TYPE));
    get_expr()->semant_walk(ctp);
    ctp->obj_env.exitscope();

    if(! get_expr()->is_no_expr() 
       && ! type_le(get_expr()->type, info->static_type, ctp)){
        ctp->cur_node_semant_err(this)<<info->static_type->get_string()<< \
        " is required but the initial value is of "<<get_expr()->type->get_string()<<endl;
    }
}

/*
    Check conflict between inherited attributes.

    Attributes cannot be reefined.
    Attributes are declared from the root (ancestors) to leaf (subclass).
    Type cheching of initial values.
 */
void class__class::resolve_inherited_attr(ClassTableP ctp){
    if(parent != No_class && ctp->type_nodes.find(parent) != ctp->type_nodes.end()){
        ctp->type_nodes.at(parent)->resolve_inherited_attr(ctp);
    }
    for(int i = features->first(); features->more(i); i = features->next(i)){
        Feature ftr = features->nth(i);
        if(ftr->is_method()){
            continue;
        }else if(ctp->obj_env.probe(ftr->get_name())){
            ctp->cur_node_semant_err(ftr)<<"Attributes cannot be redefined."<<endl;
        }else if(ftr->get_name() == self){
            ctp->cur_node_semant_err(ftr)<<"Self object cannot be redefined."<<endl;
        }else{
            ctp->obj_env.addid(ftr->get_name(), new SymbolInfo(((attr_class*)ftr)->get_type()));
        }
    }
    for(int i = features->first(); features->more(i); i = features->next(i)){
        Feature ftr = features->nth(i);
        if(! ftr->is_method()){
            ftr->semant_walk(ctp);
        }
    }
}

/*
    Check conflict between inherited methods.

    Methods can be redefined (overrided), but must have the same type signature.
    Methods with the same name and different signatures are not allowed.
    Subclasses' overrided methods have higher priority than their parents.
 */
void class__class::resolve_inherited_methods(ClassTableP ctp){
    ctp->cur_cls = this;
    MethodSignatures *sigs = new MethodSignatures();
    for(Symbol cur = name; cur != No_class; cur = ctp->type_nodes[cur]->get_parent()){
        Features ftrs = ctp->type_nodes[cur]->get_features();
        for(int i = ftrs->first(); ftrs->more(i); i = ftrs->next(i)){
            if(! ftrs->nth(i)->is_method()) continue;
            
            method_class *mm = (method_class*)ftrs->nth(i);
            MethodSignature *sig = new MethodSignature();
            mm->resolve_method(sig);
            if (is_inherited_methods_conflicted(sigs, mm, sig)) {
                ctp->semant_error(ctp->type_nodes[cur]->get_filename(), mm)<<"Methods cannot be overrided with different signatures."<<endl;
            }
            sigs->emplace(mm->get_name(), *sig);
        }
    }
    ctp->method_env.emplace(name, *sigs);
}

void class__class::semant_walk(ClassTableP ctp){
    ctp->cur_cls = this;

    this->resolve_inherited_attr(ctp);
    
    if(semant_debug){
        cout<<"# After resolving attributes of "<<name->get_string()<<"."<<endl;
        ctp->print_obj_env();
    }

    for(int i = features->first(); features->more(i); i = features->next(i)){
        if(features->nth(i)->is_method()){
            features->nth(i)->semant_walk(ctp);
        }
    }
}

void method_class::resolve_method(MethodSignature* sig){
    for(int j = formals->first(); formals->more(j); j = formals->next(j)){
        sig->push_back(formals->nth(j)->get_type_decl());
    }
    sig->push_back(return_type);
}

void find_entry_main(ClassTableP ctp, tree_node* node){
    if(ctp->type_nodes.find(Main) == ctp->type_nodes.end()){
        ctp->semant_error()<<"Class Main is not defined."<<endl;
        return;
    }
    Features main_ftrs = ctp->type_nodes.at(Main)->get_features();
    for(int i = main_ftrs->first(); main_ftrs->more(i); i = main_ftrs->next(i)){
        Feature ftr = main_ftrs->nth(i);
        if(ftr->is_method() && ftr->get_name() == main_meth){
            return;
        }
    }
    ctp->semant_error()<< \
    "main() is not found in Main class, which should not be inherited from other classes."<<endl;
}

void program_class::semant_walk(ClassTableP ctp){
    for (auto p:ctp->type_nodes){
        p.second->resolve_inherited_methods(ctp);
    }
    find_entry_main(ctp, this);
    if(semant_debug){
        cout<<"\n# After resolving methods:"<<endl;
        ctp->print_method_env();
    }
    for(int i = classes->first(); classes->more(i); i = classes->next(i)){
        ctp->obj_env.enterscope();
        classes->nth(i)->semant_walk(ctp);
        ctp->obj_env.exitscope();
    }
}



/*   This is the entry point to the semantic checker.

     Your checker should do the following two things:

     1) Check that the program is semantically correct
     2) Decorate the abstract syntax tree with type information
        by setting the `type' field in each Expression node.
        (see `tree.h')

     You are free to first do 1), make sure you catch all semantic
     errors. Part 2) can be done in a second stage, when you want
     to build mycoolc.
 */
void program_class::semant()
{
    initialize_constants();

    /* ClassTable constructor may do some semantic analysis */
    ClassTableP ctp = new ClassTable(classes);

    if(semant_debug) {
        cout<<"\n# After build ClassTable"<<endl;
        ctp->print_type_nodes();
        ctp->print_inherit_graph();
    }
    /* some semantic analysis code may go here */
    semant_walk(ctp);

    if (ctp->errors()) {
	cerr << "Compilation halted due to static semantic errors." << endl;
	exit(1);
    }
}
