#ifndef _SIMULA_INIT_
#define _SIMULA_INIT_

#include "global.h"
#include "reader.h"
#include "molecule.h"
#include "substrate.h"

namespace {
  using namespace simula;

  /////////////////////////////////////////////////////////////////////////////
  // initialize molecules
  //
  void init_molecule (const Molecule_Type& t) 
  {
    // build all the moleucles
    for (simI1 i = 0; i < t.amount(); ++i) {
      gen_molecule(t);
    }
  }

  /////////////////////////////////////////////////////////////////////////////
  // initialize all molecule types
  //
  void init_molecule_type (reader::node* n) 
  {
    using namespace reader;

#ifndef NDEBUG
    cout << ">>> new molecule <<<\n\n";
#endif

    // parsing molecule information
    simI1 id = gen_molecule_type();
    Molecule_Type& tp = get_molecule_type(id);
    tp.set_data_id (id + 1);
    tp.set_type_id (parse_attr<simI1>    (n,"id"    ));
    tp.set_amount  (parse_attr<simI1>    (n,"amount"));
    tp.set_name    (parse_attr<simString>(n,"name"  ));
    // define lambda function for parse molecule shape
    simVI2 rpos;
    simVI1 ridx;
    auto shape_func = [&] (node* n) {
      simI2 v = parse_str <simI2>(n->value());
      simI1 i = parse_attr<simI1>(n,"id");
      rpos.push_back(v);
      ridx.push_back(i);
    };
    // parsing shape information
    for_each_node(n->first_node("shape"), "rpos", shape_func);
    tp.set_rpos(rpos, ridx);
      
#ifndef NDEBUG
    get_molecule_type(id).debug();
#endif

  }

};

namespace simula
{
  /////////////////////////////////////////////////////////////////////////////
  // overall initialization
  //
  void init(const simChar* input) 
  {
    using namespace reader;
    // initialize molecule type
    for_each_node(parse(input), "molecule", init_molecule_type);
    // initialize molecules
    for (simI1 i = 0; i < get_molecule_type_size(); ++i) {
      init_molecule(get_molecule_type(i));
    }
    // initialize substrate
    initRandSeed();
  }

};


#endif // _SIMULA_INIT_
