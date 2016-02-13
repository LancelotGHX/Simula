#ifndef _SIMULA_MOLECULE_DEFINE_
#define _SIMULA_MOLECULE_DEFINE_

#include "molecule_type_define.h"

namespace simula {
	/**
	 * @brief Molecule class defining every simulation object
	 * @var type
	 * @var type_id
	 * @var self_id
	 * @var x
	 * @var y
	 * @var d
	 * @var size
	 * @var rpos
	 */
	class Molecule {
	private:
		const Molecule_Type * _tp_; // pointer to its molecule type
		simI1                 _id_; // molecule instance index
		//--- changable values
		simI1 _d_; // molecular direction
		simI1 _x_; // molecular x-position
		simI1 _y_; // molecular y-position
	public:

		/**
		 * @brief Constructor
		 * @param pointer to molecule type
		 * @param molecule index
		 */
		Molecule(const Molecule_Type * const tp, const simI1 id)
			: _tp_(tp), _id_(id) {}

		/**
		 * @defgroup Setters
		 * @{
		 */
		inline const Molecule_Type* type() const { return _tp_; }
		inline const simI1 type_id() const { return _tp_->data_id(); }
		inline const simI1 self_id() const { return _id_; }
		inline const simI1 x() const { return _x_; }
		inline const simI1 y() const { return _y_; }
		inline const simI1 d() const { return _d_; }
		inline const simI1   size() const { return _tp_->size(); }
		inline const simVI2& rpos() const { return this->_tp_->rpos(); }
		/** @} */

		/**
		 * @defgroup Getters
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
