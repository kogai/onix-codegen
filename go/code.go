package onix

import (
	"encoding/xml"
	"fmt"
)


// ProductIDType Product identifier type code, List 5
type ProductIDType string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *ProductIDType) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {
  // For example, a publisher’s or wholesaler’s product number. Note that <IDTypeName> is required with proprietary identifiers
  case "01":
		*c = "Proprietary"
  // International Standard Book Number, pre-2007, unhyphenated (10 characters) – now DEPRECATED in ONIX for Books, except where providing historical information for compatibility with legacy systems. It should only be used in relation to products published before 2007 – when ISBN-13 superseded it – and should never be used as the ONLY identifier (it should always be accompanied by the correct GTIN-13 / ISBN-13) For example, a publisher’s or wholesaler’s product number. Note that <IDTypeName> is required with proprietary identifiers
  case "02":
		*c = "ISBN-10"
  // GS1 Global Trade Item Number, formerly known as EAN article number (13 digits)
  case "03":
		*c = "GTIN-13"
  // UPC product number (12 digits)
  case "04":
		*c = "UPC"
  // International Standard Music Number (M plus nine digits). Pre-2008 – now DEPRECATED in ONIX for Books, except where providing historical information for compatibility with legacy systems. It should only be used in relation to products published before 2008 – when ISMN-13 superseded it – and should never be used as the ONLY identifier (it should always be accompanied by the correct ISMN-13)
  case "05":
		*c = "ISMN-10"
  // Digital Object Identifier (variable length and character set)
  case "06":
		*c = "DOI"
  // Library of Congress Control Number (12 characters, alphanumeric)
  case "13":
		*c = "LCCN"
  // GS1 Global Trade Item Number (14 digits)
  case "14":
		*c = "GTIN-14"
  // International Standard Book Number, from 2007, unhyphenated (13 digits starting 978 or 9791–9799)
  case "15":
		*c = "ISBN-13"
  // The number assigned to a publication as part of a national legal deposit process
  case "17":
		*c = "Legal deposit number"
  // Uniform Resource Name: note that in trade applications an ISBN must be sent as a GTIN-13 and, where required, as an ISBN-13 – it should not be sent as a URN
  case "22":
		*c = "URN"
  // A unique number assigned to a bibliographic item by OCLC
  case "23":
		*c = "OCLC number"
  // An ISBN-13 assigned by a co-publisher. The ‘main’ ISBN sent with ID type code 03 and/or 15 should always be the ISBN that is used for ordering from the supplier identified in Supply Detail. However, ISBN rules allow a co-published title to carry more than one ISBN. The co-publisher should be identified in an instance of the <Publisher> composite, with the applicable <PublishingRole> code
  case "24":
		*c = "Co-publisher’s ISBN-13"
  // International Standard Music Number, from 2008 (13-digit number starting 9790)
  case "25":
		*c = "ISMN-13"
  // Actionable ISBN, in fact a special DOI incorporating the ISBN-13 within the DOI syntax. Begins ‘10.978.’ or ‘10.979.’ and includes a / character between the registrant element (publisher prefix) and publication element of the ISBN, eg 10.978.000/1234567. Note the ISBN-A should always be accompanied by the ISBN itself, using codes 03 and/or 15
  case "26":
		*c = "ISBN-A"
  // E-publication identifier controlled by JPOIID’s Committee for Research and Management of Electronic Publishing Codes
  case "27":
		*c = "JP e-code"
  // Unique number assigned by the Chinese Online Library Cataloging Center (see http://olcc.nlc.gov.cn)
  case "28":
		*c = "OLCC number"
  // Japanese magazine identifier, similar in scope to ISSN but identifying a specific issue of a serial publication. Five digits to identify the periodical, plus a hyphen and two digits to identify the issue
  case "29":
		*c = "JP Magazine ID"
  // Used only with comic books and other products which use the UPC extension to identify individual issues or products. Do not use where the UPC12 itself identifies the specific product, irrespective of any 5-digit extension – use code 04 instead
  case "30":
		*c = "UPC12+5"
  // Numéro de la notice bibliographique BNF
  case "31":
		*c = "BNF Control number"
  // Archival Resource Key, as a URL (including the address of the ARK resolver provided by eg a national library
  case "35":
		*c = "ARK"
	default:
		return fmt.Errorf("undefined code for ProductIDType has been passed, got [%s]", v)
	}
	return nil
}

// DiscountCodeType Discount code type, List 100
type DiscountCodeType string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *DiscountCodeType) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {
  // UK publisher’s or distributor’s discount group code in a format specified by BIC to ensure uniqueness
  case "01":
		*c = "BIC discount group code"
  // A publisher’s or supplier’s own code which identifies a trade discount category, as specified in <DiscountCodeTypeName>. The actual discount for each code is set by trading partner agreement (applies to goods supplied on standard trade discounting terms) 
  case "02":
		*c = "Proprietary discount code"
  // Terms code used in the Netherlands book trade
  case "03":
		*c = "Boeksoort"
  // Terms code used in German ONIX applications
  case "04":
		*c = "German terms code"
  // A publisher’s or supplier’s own code which identifies a commission rate category, as specified in <DiscountCodeTypeName>. The actual commission rate for each code is set by trading partner agreement (applies to goods supplied on agency terms) 
  case "05":
		*c = "Proprietary commission code"
  // UK publisher’s or distributor’s commission group code in format specified by BIC to ensure uniqueness. Format is identical to BIC discount group code, but indicates a commission rather than a discount (applies to goods supplied on agency terms) 
  case "06":
		*c = "BIC commission group code"
	default:
		return fmt.Errorf("undefined code for DiscountCodeType has been passed, got [%s]", v)
	}
	return nil
}
