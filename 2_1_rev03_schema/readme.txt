ONIX FOR BOOKS RELEASE 2.1

XML SCHEMA MINOR REVISION, MARCH 2005

NOTES FOR IMPLEMENTERS

COPYRIGHT (c) EDItEUR 2005



1. Non-deterministic content models

A number of software application frameworks, including Microsoft .NET, cannot use the ONIX Release 2.1 Schema in its original form released in November 2004, due to the inability of some XML Schema Parsers to handle non-deterministic content models.

In the ONIX Release 2.1 DTD, from which the original XML Schema was derived without alteration, there are three instances of the composite element <Title> at the same level within the content model of the element <Product>. Two of these instances are a legacy of ONIX before Release 2.0 and should have been removed at that time. These have been removed in this revision. See Release Notes in the Schema for further details.

A minority of XML Parsers consider the way that the <SalesRights> composite was included in the content model of <Product> to be non-deterministic. Although this is arguably not the case, there is nevertheless a better way of expressing the same structure in an XML Schema that avoids the difficulty entirely, and the opportunity has been taken to revise the structure accordingly. See Release Notes in the Schema for further details.


2. Target namespace for the XHTML Subset

The original XML Schema for ONIX Release 2.1 uses the so-called "Chameleon Include" method of including the XHTML Subset module, i.e. the module does not have its own namespace but assumes the namespace of the including schema. It has emerged that the Microsoft .NET Framework 1.0/1.1 does not support Chameleon Includes. For this reason the download package includes two alternative versions of the XHTML Subset module in which target namespaces for the reference and short versions of the schema respectively have been added.

Until such time as Microsoft fix the acknowledged bug in the .NET Framework (see Microsoft Knowledge Base document number 317611), systems developed using it will need to validate ONIX records and messages using locally-installed copies of the schema that include whichever alternative version of the XHTML Subset module is appropriate.

Francis Cave
ONIX Support Team
March 2005
