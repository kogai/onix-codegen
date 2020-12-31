ONIX FOR BOOKS RELEASE 2.1

XML SCHEMA MINOR CORRECTION, May 2013

NOTES FOR IMPLEMENTERS

COPYRIGHT (c) EDItEUR 2005–2013



1. Deletion of obsolete 'Level 1' ONIX compatibility elements

A number of XML data elements relating to obsolete 'Level 1' ONIX were inadvertently left in the ONIX 2.1 DTD and XSD schemas. They have now been removed from the XSD.

These Level 1 elements were present in ONIX 1.x, when 'Level 1' and 'Level 2' were distinct 'flavors' of implementation. They were inherited and deliberately retained in ONIX 2.0, in the expectation that Level 1 and Level 2 flavors would also be defined. In the event, Level 1 of ONIX 2.0 was never developed: ALL ONIX since version 2.0 has in effect been Level 2, and the distinction is no longer meaningful. However, the Level 1 elements inherited into 2.0 were never removed, and should not have been inherited into version 2.1. They should have been removed in 2003 at the latest.

The effect is that when the DTD or the original XSD are used for XML validation of an ONIX 2.1 message file, various elements that would normally be used inside <Header>, <Series> and <Set> appear to be valid OUTSIDE of the <Header>, <Series> and <Set> composites. The documentation for versions 2.0 and 2.1 make it clear that this is not in fact the case - they are clearly invalid. However, the DTD and original XSD do not invalidate them. The corrected XSD rejects these elements as invalid unless they are used inside <Header>, <Series> or <Set>.

Note that the DTD still contains these Level 1 elements, as removing them at this stage (May 2013, just 18 months before the sunset date for ONIX 2.1 support) could do more harm than good - it may unexpectedly prevent *apparently* valid ONIX from working. The XSD has been modified as it is used by fewer organisations, who are most concerned with message validity. A modified version of the DTD is available on request from EDItEUR.



Graham Bell
ONIX Support Team
May 2013
