#ifndef _SIMULA_READER_XML_
#define _SIMULA_READER_XML_

#include "global.h"
#include <rapidxml/rapidxml_utils.hpp>
#include <rapidxml/rapidxml.hpp>
#include <boost/lexical_cast.hpp>
#include <iostream>
#include <vector>

namespace simula {
  
  namespace reader {

    typedef rapidxml::xml_node<>      node;
    typedef rapidxml::xml_attribute<> attr;
    
    
    void allattr(rapidxml::xml_node<>* Node) {
      using namespace rapidxml;
      using namespace std;
      for (xml_attribute<>* attr = Node->first_attribute();
	   attr; attr = attr->next_attribute()) 
	{
	  cout << "Attribute: " << attr->name() << "\n";
	  cout << "---> Value: " << attr->value() << "\n";
	}
    }

    
    ///////////////////////////////////////////////////////////////////////////
    // read data for molecule type list
    //   pre-condition: read the first correct node/attribute if only one 
    //   value is needed. read all matched values if multiple values are
    //   needed.
    //
    void read_molecule(node* r)
    {
      using namespace boost;
      using namespace std;

      node* n = NULL;
      for (n = r->first_node("molecule"); n; n = n->next_sibling("molecule")) {

	cout << "new >>> " << n->name() << " <<<\n";

	attr* id_attr = n->first_attribute("id");
	cout << "  molecule id = ";
	cout << lexical_cast<simI1>(id_attr->value());
	cout << "\n";
	
	// Interate over the sub-nodes
	// name
	cout << "  name: " << n->first_node("name")->value() << "\n";
	// amount
	cout << "  amount: ";
	cout << lexical_cast<simI1>(n->first_node("amount")->value());
	cout << "\n";
	// geometry
	cout << "  geometry: ";
	simStrVec strvec;
	simString geostr = n->first_node("geometry")->value();
	split(strvec, geostr, is_any_of(","), token_compress_on);
	for (simI1 i = 0; i < strvec.size(); ++i) {
	  trim_if(strvec[i], is_any_of(" \n\t"));
	  cout << lexical_cast<simI1>(strvec[i]) << "\n";
	}

      }
    }

    void parse(const simChar * fname)
    {
      //////////////////////////////////////////////////
      // read xml input file
      rapidxml::file<> xmlFile(fname);
      rapidxml::xml_document<> doc;
      doc.parse<0>(xmlFile.data());

      //////////////////////////////////////////////////
      // read input file
      read_molecule(doc.first_node());
    }

  };
};

#endif // _SIMULA_READER_XML_
