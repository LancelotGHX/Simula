#ifndef _SIMULA_READER_XML_
#define _SIMULA_READER_XML_

#include "global.h"
#include "rapidxml/rapidxml_utils.hpp"
#include "rapidxml/rapidxml.hpp"
#include <iostream>

namespace simula { 
  namespace reader {
    using namespace rapidxml;

    using namespace std;

    void allattr(xml_node<>* Node) {
      for (xml_attribute<>* attr = Node->first_attribute();
	   attr; attr = attr->next_attribute()) 
	{
	  cout << "Attribute: " << attr->name() << "\n";
	  cout << "---> Value: " << attr->value() << "\n";
	}
    }

    void parse_input(const simChar * filename)
    {
      // read xml input file
      file<> xmlFile(filename);
      xml_document<> doc;
      doc.parse<0>(xmlFile.data());
      xml_node<>* root = doc.first_node();
	
      std::cout << root->name() << "\n";

      // read input file
      for (xml_node<>* iNode = root->first_node(); 
	 iNode; iNode = iNode->next_sibling()) 
      {

	// show all attribute
	allattr(iNode);
	cout <<  "Node value: " << iNode->value() << " ";
	cout <<  "Node Name: " << iNode->name() << "\n";
	
	// Interate over the sub nodes
	for(xml_node<> * jNode = iNode->first_node(); 
	    jNode; jNode = jNode->next_sibling())
	  {

	    allattr(jNode);
	    cout <<  "Node value: " << jNode->value() << " ";
	    cout <<  "Node name: " << jNode->name() << "\n";
	  }
      
      }    

    }
  };
};

#endif // _SIMULA_READER_XML_
