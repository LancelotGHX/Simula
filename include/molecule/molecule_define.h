#ifndef _SIMULA_MOLECULE_DEFINE_
#define _SIMULA_MOLECULE_DEFINE_

#include "molecule_type_define.h"

///////////////////////////////////////////////////////////////////////////////
// private variable namespace
namespace _molecule_ {
	extern simula::simI1 _max_rpos_idx_;
};

///////////////////////////////////////////////////////////////////////////////
// project namespace
namespace simula {

	/////////////////////////////////////////////////////////////////////////////
	// convert land id to molecule self id
	inline simI1 lid2sid(simI1 v)
	{ 
		return static_cast<simI1>(v / _molecule_::_max_rpos_idx_); 
	}
	/////////////////////////////////////////////////////////////////////////////
	// @brief convert land id to molecule rpos id
	inline simI1 lid2rid(simI1 v)
	{ 
		return static_cast<simI1>(v % _molecule_::_max_rpos_idx_); 
	}
	/////////////////////////////////////////////////////////////////////////////
	// Molecule class defining every simulation object
	//  var: type
	//  var: type_id
	//	var: self_id
	//	var: x
	//	var: y
	//	var: d
	//	var: size
  //	var: rpos
	class Molecule {
	private:
		const MoleculeType * _tp_ = NULL; // pointer to its molecule type
		simI1                _id_;        // molecule instance index
		// changable values
		simI1 _d_; // molecular direction
		simI1 _x_; // molecular x-position
		simI1 _y_; // molecular y-position
	public:

		/**
		 * @brief Constructor
		 * @param pointer to molecule type
		 * @param molecule index
		 */
		Molecule(const MoleculeType * const tp, const simI1 id)
			: _tp_(tp), _id_(id) {}

		/**
		 * @defgroup Getters
		 * @{
		 */
		const simI1 land_id(simI1 i) const;
		inline const simI1 type_id() const { return _tp_->gen_id(); }
		inline const simI1 self_id() const { return _id_; }
		inline const simI1 x() const { return _x_; }
		inline const simI1 y() const { return _y_; }
		inline const simI1 d() const { return _d_; }
		inline const MoleculeType* type() const { return _tp_; }
		inline const simSize size() const { return _tp_->size(); }
		inline const simVI2& rpos() const { return this->_tp_->rpos(); }
		inline const simVI1& ridx() const { return this->_tp_->ridx(); }
		/** @} */

		/**
		 * @defgroup Setters
		 * @{
		 */
		inline void set_x(const simI1 xpos) { _x_ = xpos; }
		inline void set_y(const simI1 ypos) { _y_ = ypos; }
		inline void set_d(const simI1 dir) { _d_ = dir; }
		/** @} */

#ifndef NDEBUG
		/** @brief debug function */
		void debug();
#endif

	};
};

#endif // _SIMULA_MOLECULE_DEFINE_
