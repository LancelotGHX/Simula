#ifndef _SIMULA_READER_XML_
#define _SIMULA_READER_XML_

#include <boost/lexical_cast.hpp>
#include <rapidxml/rapidxml_utils.hpp> // read file
#include <rapidxml/rapidxml.hpp>       //
#include <iostream>
#include <vector>
#include "global.h"

namespace {
  rapidxml::xml_document<> _doc_;
};

namespace simula {

  namespace reader {

    typedef rapidxml::xml_node<>      node;
    typedef rapidxml::xml_attribute<> attr;

    /**
     * @brief template string parser
     */
    template<typename T>
    T parse_str(simString str) // auto type conversion
    {
      boost::trim_if(str, boost::is_any_of(" \n\t"));
      return boost::lexical_cast<T>(str);
    }
    template<>
    simString parse_str<simString>(simString str)
    {
      return boost::trim_copy_if(str, boost::is_any_of(" \n\t"));
    }
    template<>
    simI2 parse_str<simI2>(simString str)
    {
      using boost::lexical_cast;
      using boost::is_any_of;
      using boost::token_compress_on;
      simStrVec strvec;
      simI2     result;
      // split string into string vector
      split(strvec, str, is_any_of(","), token_compress_on);
      // trim each string
      trim_if(strvec[0], is_any_of(" \n\t"));
      trim_if(strvec[1], is_any_of(" \n\t"));
      // convert string into numerical data
      result.x = lexical_cast<simI1>(strvec[0]) ;
      result.y = lexical_cast<simI1>(strvec[1]) ;
      return result;
    }

    /**
     * @brief get attribute/node value 
     */
    inline const simChar* attr_value(const node* n, const simChar* name)
    {
      return n->first_attribute(name)->value();
    }
    inline const simChar* node_value(const node* n, const simChar* name)
    {
      return n->first_node(name)->value();
    }

    /**
     * @brief read xml input file
     */
    inline node* parse(const simChar* name)
    {
      rapidxml::file<> xmlFile(name);
      _doc_.parse<0>(xmlFile.data());
      return _doc_.first_node();
    }

    /**
     * @brief convention function to parse node attribute
     */
    template<typename T>
    T parse_attr(const node* n, const simChar* name)
    {
      return parse_str<T>(attr_value(n,name));
    }

    /** 
     * @brief apply function to each node
     */
    template<typename Func>
    void for_each_node(const node* r, const simChar* name, Func func)
    {
      for (node* n = r->first_node(name); n; n = n->next_sibling(name)) {
	func(n);
      }
    }

  };
};

#endif // _SIMULA_READER_XML_
