#ifndef _SIMULA_READER_XML_
#define _SIMULA_READER_XML_

#include "global.h"
#include <rapidxml/rapidxml_utils.hpp>
#include <rapidxml/rapidxml.hpp>
#include <boost/lexical_cast.hpp>
#include <iostream>
#include <vector>

namespace
{
  rapidxml::xml_document<> _doc_;
};

namespace simula {
  namespace reader {

    typedef rapidxml::xml_node<>      node;
    typedef rapidxml::xml_attribute<> attr;

    ///////////////////////////////////////////////////////////////////////////
    // template string parser
    //
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
      using namespace boost;
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
    ///////////////////////////////////////////////////////////////////////////
    // get attribute/node value 
    //
    const simChar* attr_value(const node* n, const simChar* name)
    {
      return n->first_attribute(name)->value();
    }
    const simChar* node_value(const node* n, const simChar* name)
    {
      return n->first_node(name)->value();
    }

    //////////////////////////////////////////////////
    // read xml input file
    //
    node* parse(const simChar* name)
    {
      rapidxml::file<> xmlFile(name);
      _doc_.parse<0>(xmlFile.data());
      return _doc_.first_node();
    }

    template<typename T>
    T parse_attr(const node* n, const simChar* name)
    {
      return parse_str<T>(attr_value(n,name));
    }

    ///////////////////////////////////////////////////////////////////////////
    // apply function to each node
    //
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
