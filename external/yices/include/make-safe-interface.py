#!/usr/bin/python
import re
import sys
import os


class TypeMap:
    def __init__(self, constructor, deconstructor):
        self.constructor = constructor
        self.deconstructor = deconstructor

def make_type_constructor(ty):
    def constructor(value):
        return "(%s %s)" % (ty, value)

    return constructor

def make_type_deconstructor(ty):
    def deconstructor(value):
        return """(match %s with
  %s _%s -> _%s
| _ -> raise NoCoercion)""" % (value, ty, value, value)

    return deconstructor

# pmr: we don't yet use this, stub for consistency
def construct_lbool(value):
    pass

def deconstruct_lbool(value):
    return """(match (enum_to_int `lbool %s) with
   C_int _%s -> _%s
 | _ -> raise NoCoercion)""" % (value, value, value)

swig_tycon_map = {"int":"C_int", "unsigned":"C_int32", "long long":"C_int64", "char *":"C_string", "double":"C_double"}
abstract_tycon_map = {"yices_ast":"Yices_ast", "yices_expr":"Yices_expr", "yices_type":"Yices_type", "yices_var_decl":"Yices_var_decl", "yices_context":"Yices_context", "yices_model":"Yices_model", "yices_var_decl_iterator":"Yices_var_decl_iterator"}

swig_convertors = [(k, TypeMap(make_type_constructor(swig_tycon_map[k]), make_type_deconstructor(swig_tycon_map[k]))) for k in swig_tycon_map.keys()]
abstract_convertors = [(k, TypeMap(make_type_deconstructor(abstract_tycon_map[k]), make_type_constructor(abstract_tycon_map[k]))) for k in abstract_tycon_map.keys()]
type_convertors = dict(swig_convertors + abstract_convertors)
type_convertors["lbool"] = TypeMap(construct_lbool, deconstruct_lbool)

ocaml_type_map = {"int":"int", "unsigned":"int32", "long long":"int64", "char *":"string", "void":"unit", "double":"float", "lbool":"int", "yices_ast":"yices_ast", "yices_expr":"yices_expr", "yices_type":"yices_type", "yices_var_decl":"yices_var_decl", "yices_context":"yices_context", "yices_model":"yices_model", "yices_var_decl_iterator":"yices_var_decl_iterator"}


prototype_re = re.compile("extern (\w+( \*)?) (\w+)\((.*)\);")


class WeirdType(Exception):
    def __init__(self, value):
        self.value = value

    def __str__(self):
        "Weird type: %s" % (repr(self.value))


def map_type(ty):
    try:
        return ocaml_type_map[ty]
    except KeyError:
        raise WeirdType(ty)

def type_arg(arg):
    (name, ty) = arg

    if ty.endswith("[]"):
        return map_type(ty[0:-2]) + " array"
    else:
        return map_type(ty)

def type_function(name, retty, arglist):
    argtypes = [type_arg(a) for a in arglist]
    retty = map_type(retty)

    if argtypes != []:
        ty = " -> ".join(argtypes)
    else:
        ty = "unit"

    return "val %s: %s -> %s" % (name, ty, retty)

def type_cast(ty, value, value_fn, array_fn):
    array = ty.endswith("[]")
    if not array:
        convty = ty
    else:
        convty = ty[0:-2]
        
    try:
        convertor = value_fn(type_convertors[convty], value)
    except KeyError:
        raise WeirdType(convty)

    if not array:
        return convertor
    else:
        return array_fn(value, convertor)

def construct_type(type_convertor, value):
    return type_convertor.constructor(value)

def deconstruct_type(type_convertor, value):
    return type_convertor.deconstructor(value)
    
def construct_array(value, convertor):
    return "(C_array (Array.map (fun %s -> %s) %s))" % (value, convertor, value)

# pmr: not needed atm, stub for consistency
def deconstruct_array(value, convertor):
    pass

def wrap_function(name, retty, arglist):
    args = [type_cast(argtype, argname, construct_type, construct_array) for argname, argtype in arglist]
    params = [argname for argname, argtype in arglist]

    if retty != "void":
        ret = type_cast(retty, "__ret", deconstruct_type, deconstruct_array)
    else:
        ret = "()"

    if params == []:
        params = ["_"]

    return """let %s %s =
    let __ret = _%s (C_list [%s]) in
        %s""" % (name, " ".join(params), name, "; ".join(args), ret)

def split_formal_arg(arg):
    argparts = arg.split(" ")
    ty = " ".join(argparts[0:-1])
    name = argparts[-1]

    if name.endswith("[]"):
        name = name[0:-2]
        ty += "[]"

    return (name, ty)

def prototype_components(match):
    name = m.group(3)
    retty = m.group(1)
    raw_arglist = m.group(4).split(",")
    arglist = [split_formal_arg(a.strip()) for a in raw_arglist if a != ""]

    def is_redundant_len_arg(i):
        try:
            return arglist[i - 1][1].endswith("[]") and arglist[i][1] == "unsigned"
        except IndexError:
            return False

    arglist = [arglist[i] for i in range(0, len(arglist)) if not is_redundant_len_arg(i)]

    return (name, retty, arglist)


f = open(sys.argv[1])
matches = [prototype_re.match(line) for line in f.readlines()]
prototypes = [prototype_components(m) for m in matches if m]

fun_types = []
fun_wrappers = []
# pmr: uglyugly but there are no good ways to handle exceptions in list comps
for name, retty, arglist in prototypes:
    try:
        fun_type = type_function(name, retty, arglist)
        fun_wrapper = wrap_function(name, retty, arglist)
        
        fun_types.append(fun_type)
        fun_wrappers.append(fun_wrapper)
    except WeirdType:
        print "%s has weird type; skipping wrapper" % (name)

(base, ext) = os.path.splitext(os.path.basename(sys.argv[1]))
to_wrap = sys.argv[2]
new_base = sys.argv[3]

abstract_types = ["yices_ast", "yices_expr", "yices_type", "yices_var_decl", "yices_context", "yices_model", "yices_var_decl_iterator"]

common_header = """open Swig
open %s
open List

%s

exception NoCoercion

""" % (to_wrap.capitalize(), "\n".join(["type %s = %s of c_obj" % (t, t.capitalize()) for t in abstract_types]))

interface = open("%s.mli" % (new_base), "w")
interface.write(common_header)
interface.write("\n".join(fun_types))
interface.close()

implementation = open("%s.ml" % (new_base), "w")
implementation.write(common_header)
implementation.write("\n\n".join(fun_wrappers))
implementation.close()
