extern "C" {
#include <stdint.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/custom.h>
}
#include "core/SolverTypes.h"
#include "core/Solver.h"

//#define MYDEBUG
//#define AND_INVERTER
#define AND_OR
#define REPRODUCIBLE
#define Solver_val(v) (*((Solver **) Data_custom_val(v)))

using namespace Minisat;

extern "C" void minisat2_dealloc_solver (value s);

static struct custom_operations minisat2_solver_ops = {
  (char *)"minisat2.solver",
  // custom_finalize_default,
  minisat2_dealloc_solver,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default
};

static void convert_literals (value l, vec<Lit> &r) {
  const int size = Wosize_val (l);
  for (int i = 0; i < size; i++) {
    r.push (toLit (Int_val (Field (l, i))));
  }
}

static void print_clause(vec<Lit> &c) {
  int i;

  for (i = 0; i < c.size(); i++) {
    printf ("%s%d ", sign(c[i]) ? "-" : "", var (c[i]));
  }
}

static value alloc_solver (Solver *s) {
  CAMLparam0 ();
  CAMLlocal1 (v);
  v = alloc_custom(&minisat2_solver_ops, sizeof(Solver*), 0, 1);
  Solver_val(v) = s;
  CAMLreturn (v);
}

extern "C" void minisat2_dealloc_solver (value s) {
  Solver* solver = Solver_val (s);
  delete solver;
}

extern "C" value minisat2_new_solver (value unit) {
  CAMLparam1(unit);
  CAMLlocal1(v);
  Solver *solver = new Solver ();
  v = alloc_solver(solver);
  assert (Solver_val (v)->nVars () == 0);
  CAMLreturn(v);
}

extern "C" value minisat2_new_var (value s) {
  CAMLparam1(s);
  Solver* solver = Solver_val(s);
  Var var = solver->newVar();
  CAMLreturn(Val_int(var));
}

extern "C" value minisat2_new_inferred_var (value s) {
  CAMLparam1(s);
  Solver* solver = Solver_val(s);
  Var var = solver->newVar(true, false);
  CAMLreturn(Val_int(var));
}

extern "C" value minisat2_pos_lit (value s, value v) {
  CAMLparam2(s, v);
  Var var = Int_val(v);
  Lit lit = mkLit (var, false);
  CAMLreturn(Val_int(toInt(lit)));
}

extern "C" value minisat2_neg_lit (value s, value v) {
  CAMLparam2(s, v);
  Var var = Int_val(v);
  Lit lit = mkLit (var, true);
  CAMLreturn(Val_int(toInt(lit)));
}

extern "C" value minisat2_add_clause (value s, value c) {
  CAMLparam2(s, c);
  Solver* solver = Solver_val(s);
  vec<Lit> clause;
  convert_literals(c, clause);
  solver->addClause (clause);
  CAMLreturn(Val_unit);
}

extern "C" value minisat2_simplify (value s) {
  CAMLparam1(s);
  Solver* solver = Solver_val(s);
  solver->simplify();
  CAMLreturn(Val_unit);
}

extern "C" value minisat2_solve (value s) {
  CAMLparam1(s);
  CAMLlocal1(r);
  Solver* solver = Solver_val(s);

  r = solver->solve () ? Val_true : Val_false;
  CAMLreturn(r);
}

extern "C" value minisat2_solve_with_assumption (value s, value a) {
  CAMLparam2(s, a);
  CAMLlocal1(r);

  Solver* solver = Solver_val(s);
  vec<Lit> assumption;
  convert_literals(a, assumption);

  r = solver->solve (assumption) ? Val_true : Val_false;
  CAMLreturn(r);
}



extern "C" value minisat2_value_of (value s, value v) {
  CAMLparam2(s, v);
  CAMLlocal1(r);
  
  Solver* solver = Solver_val(s);
  Var var = Int_val(v);
  lbool val = solver->model[var];

  if(val == l_False) {
    r = Val_int(0);
  } else if(val == l_True) {
    r = Val_int(1);
  } else if (val == l_Undef) {
    r = Val_int(2);
  } else {
    assert(0);
  }

  CAMLreturn(r);
}

extern "C" value minisat2_nvars (value s) {
  CAMLparam1(s);
  Solver* solver = Solver_val(s);
  int n = solver->nVars();
  CAMLreturn(Val_int(n));
}

extern "C" value minisat2_okay (value s) {
  CAMLparam1(s);
  Solver* solver = Solver_val(s);
  bool ret = solver->okay();
  CAMLreturn(Val_bool(ret));
}
