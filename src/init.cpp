#include "init.h"
#include "substrate.h" // Substrate, Molecule
#include "reader.h"    // reader functions

using namespace simula;

namespace {
	/**
	 * @brief initialize molecules
	 */
	void init_molecule(const Molecule_Type& t)
	{
		for (simI1 i = 0; i < t.amount(); ++i)
			gen_molecule(t);
	}

	/**
	 * @brief initialize all molecule types
	 */
	void init_molecule_type(const reader::node_t* n)
	{
		using namespace reader;
#ifndef NDEBUG
		cout << " >>> new molecule <<<\n\n";
#endif
		/** @todo parsing molecule information */
		///////////////////////////////////////////////////////////////////////////
		simI1 id = gen_molecule_type();
		Molecule_Type& tp = get_molecule_type(id);
		///////////////////////////////////////////////////////////////////////////
		// molecule: user defined index
		tp.set_usr_id(parse_attr<simI1>(n, "id"));
		///////////////////////////////////////////////////////////////////////////
		// molecule: total amount
		tp.set_amount(parse_attr<simI1>(n, "amount"));
		///////////////////////////////////////////////////////////////////////////
		// molecule: user defined name
		tp.set_name(parse_attr<simString>(n, "name"));
		///////////////////////////////////////////////////////////////////////////
		// molecule: geometry
		// using lambda function for parse molecule shape
		simVI2 rpos; // relative dots positions
		simVI1 ridx; // relative dots indices
		auto rpos_func = [&](node_t* sn /* node 'rpos' */) {
			simI2 v = parse_str <simI2>(sn->value());
			simI1 i = parse_attr<simI1>(sn, "id");
			rpos.push_back(v);
			ridx.push_back(i);
		};
		for_each_node(n->first_node("shape"), "rpos", rpos_func);
		// assign and clean up
		tp.set_rpos(rpos, ridx);
		rpos.clear();
		ridx.clear();
		///////////////////////////////////////////////////////////////////////////
		// molecule: bonding
		simBonds bonds;
		auto bond_func = [&](node_t* sn /* node 'bond' */) {
			bonds.emplace_back();
			simOneBond& new_bond = bonds.back();
			new_bond.energy = parse_attr<simF1>(sn, "energy");
			new_bond.target = parse_attr<simI1>(sn, "target");
			// get all bond rpos
			auto bond_pos_func = [&](node_t* nrpos /* node 'rpos' */) {
				simI2 v = parse_str <simI2>(nrpos->value());
				simI1 i = parse_attr<simI1>(nrpos, "id");
				new_bond.rpos.emplace_back();
				new_bond.rpos.back().x = v.x;
				new_bond.rpos.back().y = v.y;
				new_bond.rpos.back().z = i;
			};
			for_each_node(sn, "rpos", bond_pos_func);
		};
		for_each_node(n, "bond", bond_func);
		// assign and clean up
		tp.set_bond(bonds);
		bonds.clear();
		///////////////////////////////////////////////////////////////////////////
	}

	/**
	* @brief initialize substrate
	*/
	void init_substrate(const reader::node_t* n)
	{
		simI2 v = reader::parse_str<simI2>(n->value());
#ifndef NDEBUG
		cout << " initialize substrate: ";
		cout << "(" << v.x << "," << v.y << ")\n\n";
#endif
		gen_sub(v.x, v.y);
	}

};

void simula::init(const simChar* input)
{
	using namespace reader;
	/** @brief we cannot initialize a document with ptr, so open file here */
	doc_t doc; 
	xml_t xml(input);
	doc.parse<0>(xml.data());

	/** @brief initialize user defined constants */
	for_each_node(doc.first_node(), "molecule", init_molecule_type);
	for_each_node(doc.first_node(), "substrate", init_substrate, 1);

#ifndef NDEBUG
	for (simI1 i = 0; i < get_molecule_type_size(); ++i)
	{
		get_molecule_type(i).debug();
	}
#endif

	/** @brief initialize the simulation system */
	// initialize molecules
	for (simI1 i = 0; i < get_molecule_type_size(); ++i)
	{
		init_molecule(get_molecule_type(i));
	}

	// initialize random seed
	initRandSeed();
}
