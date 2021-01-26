package onix

import (
	"encoding/xml"
	"fmt"
	"strings"
)


// Character 
type Character string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *Character) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {
	default:
		return fmt.Errorf("undefined code for Character has been passed, got [%s]", v)
	}
}

// Charset 
type Charset string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *Charset) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {
	default:
		return fmt.Errorf("undefined code for Charset has been passed, got [%s]", v)
	}
}

// Coords 
type Coords string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *Coords) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {
	default:
		return fmt.Errorf("undefined code for Coords has been passed, got [%s]", v)
	}
}

// DtDotCountryCodeList 
type DtDotCountryCodeList string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *DtDotCountryCodeList) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {
	default:
		return fmt.Errorf("undefined code for DtDotCountryCodeList has been passed, got [%s]", v)
	}
}

// DtDotDecimal Datatype for any real number
type DtDotDecimal string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *DtDotDecimal) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {
	default:
		return fmt.Errorf("undefined code for DtDotDecimal has been passed, got [%s]", v)
	}
}

// DtDotEmailString Datatype for plausible e-mail address
type DtDotEmailString string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *DtDotEmailString) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {
	default:
		return fmt.Errorf("undefined code for DtDotEmailString has been passed, got [%s]", v)
	}
}

// DtDotInteger Datatype for any integer
type DtDotInteger string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *DtDotInteger) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {
	default:
		return fmt.Errorf("undefined code for DtDotInteger has been passed, got [%s]", v)
	}
}

// DtDotMultiLevelNumber Datatype for string of dot-separated numbers, eg 3.12.8
type DtDotMultiLevelNumber string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *DtDotMultiLevelNumber) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {
	default:
		return fmt.Errorf("undefined code for DtDotMultiLevelNumber has been passed, got [%s]", v)
	}
}

// DtDotMultiLevelNumberOrHyphen Datatype for string of dot-separated numbers where hyphen can replace a number, eg 3.-.8
type DtDotMultiLevelNumberOrHyphen string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *DtDotMultiLevelNumberOrHyphen) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {
	default:
		return fmt.Errorf("undefined code for DtDotMultiLevelNumberOrHyphen has been passed, got [%s]", v)
	}
}

// DtDotNonEmptyString Datatype for non-empty string without leading or trailing white space
type DtDotNonEmptyString string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *DtDotNonEmptyString) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {
	default:
		return fmt.Errorf("undefined code for DtDotNonEmptyString has been passed, got [%s]", v)
	}
}

// DtDotNonEmptyURI Datatype for URI without leading or trailing white space
type DtDotNonEmptyURI string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *DtDotNonEmptyURI) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {
	default:
		return fmt.Errorf("undefined code for DtDotNonEmptyURI has been passed, got [%s]", v)
	}
}

// DtDotPercentDecimal Datatype for real number 0–100
type DtDotPercentDecimal string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *DtDotPercentDecimal) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {
	default:
		return fmt.Errorf("undefined code for DtDotPercentDecimal has been passed, got [%s]", v)
	}
}

// DtDotPositiveDecimal Datatype for zero or any positive real number
type DtDotPositiveDecimal string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *DtDotPositiveDecimal) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {
	default:
		return fmt.Errorf("undefined code for DtDotPositiveDecimal has been passed, got [%s]", v)
	}
}

// DtDotPositiveInteger Datatype for zero or any positive integer
type DtDotPositiveInteger string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *DtDotPositiveInteger) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {
	default:
		return fmt.Errorf("undefined code for DtDotPositiveInteger has been passed, got [%s]", v)
	}
}

// DtDotRegionCodeList 
type DtDotRegionCodeList string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *DtDotRegionCodeList) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {
	default:
		return fmt.Errorf("undefined code for DtDotRegionCodeList has been passed, got [%s]", v)
	}
}

// DtDotRomanNumeralString Datatype for Roman numerals (upper or lower case)
type DtDotRomanNumeralString string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *DtDotRomanNumeralString) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {
	default:
		return fmt.Errorf("undefined code for DtDotRomanNumeralString has been passed, got [%s]", v)
	}
}

// DtDotStrictPositiveDecimal Datatype for any positive real number (not including zero)
type DtDotStrictPositiveDecimal string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *DtDotStrictPositiveDecimal) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {
	default:
		return fmt.Errorf("undefined code for DtDotStrictPositiveDecimal has been passed, got [%s]", v)
	}
}

// DtDotStrictPositiveInteger Datatype for any positive integer (not including zero)
type DtDotStrictPositiveInteger string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *DtDotStrictPositiveInteger) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {
	default:
		return fmt.Errorf("undefined code for DtDotStrictPositiveInteger has been passed, got [%s]", v)
	}
}

// DtDotTimeOrDuration 
type DtDotTimeOrDuration string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *DtDotTimeOrDuration) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {
	default:
		return fmt.Errorf("undefined code for DtDotTimeOrDuration has been passed, got [%s]", v)
	}
}

// DtDotYear Datatype for year 1000 to 2999
type DtDotYear string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *DtDotYear) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {
	default:
		return fmt.Errorf("undefined code for DtDotYear has been passed, got [%s]", v)
	}
}

// DtDotYearOrYearRange Datatype for year or range of years 1000-2099
type DtDotYearOrYearRange string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *DtDotYearOrYearRange) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {
	default:
		return fmt.Errorf("undefined code for DtDotYearOrYearRange has been passed, got [%s]", v)
	}
}

// Length 
type Length string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *Length) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {
	default:
		return fmt.Errorf("undefined code for Length has been passed, got [%s]", v)
	}
}

// LinkTypes 
type LinkTypes string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *LinkTypes) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {
	default:
		return fmt.Errorf("undefined code for LinkTypes has been passed, got [%s]", v)
	}
}

// MultiLength 
type MultiLength string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *MultiLength) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {
	default:
		return fmt.Errorf("undefined code for MultiLength has been passed, got [%s]", v)
	}
}

// Pixels 
type Pixels string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *Pixels) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {
	default:
		return fmt.Errorf("undefined code for Pixels has been passed, got [%s]", v)
	}
}

// Scope 
type Scope string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *Scope) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

  // 
  case "row":
		*c = ``

  // 
  case "col":
		*c = ``

  // 
  case "rowgroup":
		*c = ``

  // 
  case "colgroup":
		*c = ``
	default:
		return fmt.Errorf("undefined code for Scope has been passed, got [%s]", v)
	}
	return nil
}

// Script 
type Script string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *Script) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {
	default:
		return fmt.Errorf("undefined code for Script has been passed, got [%s]", v)
	}
}

// Shape 
type Shape string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *Shape) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

  // 
  case "rect":
		*c = ``

  // 
  case "circle":
		*c = ``

  // 
  case "poly":
		*c = ``

  // 
  case "default":
		*c = ``
	default:
		return fmt.Errorf("undefined code for Shape has been passed, got [%s]", v)
	}
	return nil
}

// SourceTypeCode 
type SourceTypeCode string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *SourceTypeCode) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {
	default:
		return fmt.Errorf("undefined code for SourceTypeCode has been passed, got [%s]", v)
	}
}

// StyleSheet 
type StyleSheet string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *StyleSheet) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {
	default:
		return fmt.Errorf("undefined code for StyleSheet has been passed, got [%s]", v)
	}
}

// TFrame 
type TFrame string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *TFrame) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

  // 
  case "void":
		*c = ``

  // 
  case "above":
		*c = ``

  // 
  case "below":
		*c = ``

  // 
  case "hsides":
		*c = ``

  // 
  case "lhs":
		*c = ``

  // 
  case "rhs":
		*c = ``

  // 
  case "vsides":
		*c = ``

  // 
  case "box":
		*c = ``

  // 
  case "border":
		*c = ``
	default:
		return fmt.Errorf("undefined code for TFrame has been passed, got [%s]", v)
	}
	return nil
}

// TRules 
type TRules string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *TRules) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

  // 
  case "none":
		*c = ``

  // 
  case "groups":
		*c = ``

  // 
  case "rows":
		*c = ``

  // 
  case "cols":
		*c = ``

  // 
  case "all":
		*c = ``
	default:
		return fmt.Errorf("undefined code for TRules has been passed, got [%s]", v)
	}
	return nil
}

// TextCaseCode 
type TextCaseCode string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *TextCaseCode) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {
	default:
		return fmt.Errorf("undefined code for TextCaseCode has been passed, got [%s]", v)
	}
}

// TextFormatCode 
type TextFormatCode string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *TextFormatCode) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {
	default:
		return fmt.Errorf("undefined code for TextFormatCode has been passed, got [%s]", v)
	}
}

// URI 
type URI string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *URI) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {
	default:
		return fmt.Errorf("undefined code for URI has been passed, got [%s]", v)
	}
}

// UriList 
type UriList string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *UriList) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {
	default:
		return fmt.Errorf("undefined code for UriList has been passed, got [%s]", v)
	}
}

// XHTMLContentType 
type XHTMLContentType string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *XHTMLContentType) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {
	default:
		return fmt.Errorf("undefined code for XHTMLContentType has been passed, got [%s]", v)
	}
}

// XHTMLLanguageCode 
type XHTMLLanguageCode string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *XHTMLLanguageCode) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {
	default:
		return fmt.Errorf("undefined code for XHTMLLanguageCode has been passed, got [%s]", v)
	}
}

// XHTMLNumber 
type XHTMLNumber string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *XHTMLNumber) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {
	default:
		return fmt.Errorf("undefined code for XHTMLNumber has been passed, got [%s]", v)
	}
}

// XHTMLText 
type XHTMLText string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *XHTMLText) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {
	default:
		return fmt.Errorf("undefined code for XHTMLText has been passed, got [%s]", v)
	}
}

// AVItemIDType AV Item Identifier type
type AVItemIDType struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *AVItemIDType) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // For example, a publisher’s own identifier. Note that <IDTypeName> is required with proprietary identifiers
  case "01":
		c.Body = `Proprietary`

  // Formerly known as the EAN-13 (unhyphenated)
  case "03":
		c.Body = `GTIN-13`

  // Digital Object Identifier (variable length and character set beginning ‘10.’, and without https://doi.org/ or the older http://dx.doi.org/)
  case "06":
		c.Body = `DOI`

  // Motion picture work identifier from the International Movie Database
  case "12":
		c.Body = `IMDB`

  // International Standard Recording Code, 5 alphanumeric characters plus 7 digits
  case "18":
		c.Body = `ISRC`

  // International Standard Audiovisual Number (17 or 26 characters – 16 or 24 hexadecimal digits, plus one or two alphanumeric check characters, and without spaces or hyphens)
  case "19":
		c.Body = `ISAN`

  // Entertainment Identifier Registry DOI
  case "31":
		c.Body = `EIDR DOI`
	default:
		return fmt.Errorf("undefined code for AVItemIDType has been passed, got [%s]", v)
	}
	return nil
}

// AVItemType AV Item type code
type AVItemType struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *AVItemType) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // A complete audiovisual work which is published as a content item in a product which carries two or more such works, eg when two or three AV works are published in a single omnibus package
  case "01":
		c.Body = `Audiovisual work`

  // Audiovisual components such as a scene index or introduction which appear before the main content of the product
  case "02":
		c.Body = `Front matter`

  // Audiovisual components such as scenes or ‘chapters’ which appear as part of the main body of the AV material in the product
  case "03":
		c.Body = `Body matter`

  // Audiovisual components such as advertising which appear after the main content of the product
  case "04":
		c.Body = `End matter`
	default:
		return fmt.Errorf("undefined code for AVItemType has been passed, got [%s]", v)
	}
	return nil
}

// AddresseeIDType Name identifier type
type AddresseeIDType struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *AddresseeIDType) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // Note that <IDTypeName> is required with proprietary identifiers
  case "01":
		c.Body = `Proprietary`

  // DEPRECATED – use 01
  case "02":
		c.Body = `Proprietary`

  // Deutsche Nationalbibliothek publisher identifier
  case "03":
		c.Body = `DNB publisher identifier`

  // Börsenverein Verkehrsnummer
  case "04":
		c.Body = `Börsenverein Verkehrsnummer`

  // German ISBN Agency publisher identifier
  case "05":
		c.Body = `German ISBN Agency publisher identifier`

  // GS1 global location number (formerly EAN location number)
  case "06":
		c.Body = `GLN`

  // Book trade Standard Address Number – US, UK etc
  case "07":
		c.Body = `SAN`

  // MARC code list for organizations – see http://www.loc.gov/marc/organizations/orgshome.html
  case "08":
		c.Body = `MARC organization code`

  // Trading party identifier used in the Netherlands
  case "10":
		c.Body = `Centraal Boekhuis Relatie ID`

  // Flemish publisher code
  case "13":
		c.Body = `Fondscode Boekenbank`

  // Business Identity Code (Finland). See http://www.ytj.fi/ (in Finnish)
  case "15":
		c.Body = `Y-tunnus`

  // International Standard Name Identifier. A sixteen digit number. Usually presented with spaces or hyphens dividing the number into four groups of four digits, but in ONIX the spaces or hyphens should be omitted. See http://www.isni.org/
  case "16":
		c.Body = `ISNI`

  // Personennamendatei – person name authority file used by Deutsche Nationalbibliothek and in other German-speaking countries. See http://www.dnb.de/standardisierung/normdateien/pnd.htm (German) or http://www.dnb.de/eng/standardisierung/normdateien/pnd.htm (English). DEPRECATED in favor of the GND
  case "17":
		c.Body = `PND`

  // A control number assigned to a Library of Congress Control Number (LCCN) Name Authority / NACO record
  case "18":
		c.Body = `NACO`

  // Publisher identifier administered by Japanese ISBN Agency
  case "19":
		c.Body = `Japanese Publisher identifier`

  // Gemeinsame Körperschaftsdatei – Corporate Body Authority File in the German-speaking countries. See http://www.dnb.de/standardisierung/normdateien/gkd.htm (German) or http://www.dnb.de/eng/standardisierung/normdateien/gkd.htm (English). DEPRECATED in favor of the GND
  case "20":
		c.Body = `GKD`

  // Open Researcher and Contributor ID. A sixteen digit number. Usually presented with hyphens dividing the number into four groups of four digits, but in ONIX the hyphens should be omitted. See http://www.orcid.org/
  case "21":
		c.Body = `ORCID`

  // Publisher identifier maintained by the Chinese ISBN Agency (GAPP)
  case "22":
		c.Body = `GAPP Publisher Identifier`

  // Identifier for a business organization for VAT purposes, eg within the EU’s VIES system. See http://ec.europa.eu/taxation_customs/vies/faqvies.do for EU VAT ID formats, which vary from country to country. Generally these consist of a two-letter country code followed by the 8–12 digits of the national VAT ID. Some countries include one or two letters within their VAT ID. See http://en.wikipedia.org/wiki/VAT_identification_number for non-EU countries that maintain similar identifiers. Spaces, dashes etc should be omitted
  case "23":
		c.Body = `VAT Identity Number`

  // 4-digit business organization identifier controlled by the Japanese Publication Wholesalers Association
  case "24":
		c.Body = `JP Distribution Identifier`

  // Gemeinsame Normdatei – Joint Authority File in the German-speaking countries. See http://www.dnb.de/EN/gnd (English). Combines the PND, SWD and GKD into a single authority file, and should be used in preference
  case "25":
		c.Body = `GND`

  // Dunn and Bradstreet Universal Numbering System, see http://www.dnb.co.uk/dandb-duns-number
  case "26":
		c.Body = `DUNS`

  // Ringgold organizational identifier, see http://www.ringgold.com/identify.html
  case "27":
		c.Body = `Ringgold ID`

  // French Electre publisher identifier
  case "28":
		c.Body = `Identifiant Editeur Electre`

  // DOI used in EIDR party registry, for example ‘10.5237/C9F6-F41F’ (Sam Raimi). See http://eidr.org
  case "29":
		c.Body = `EIDR Party DOI`

  // French Electre imprint Identifier
  case "30":
		c.Body = `Identifiant Marque Electre`

  // Virtual Internet Authority File. <IDValue> should be a number. The URI form of the identifier can be created by prefixing the number with ‘https://viaf.org/viaf/’. See https://viaf.org
  case "31":
		c.Body = `VIAF ID`

  // DOI used in CrossRef’s Open Funder Registry list of academic research funding bodies, for example ‘10.13039/100004440’ (Wellcome Trust). See http://www.crossref.org/fundingdata/registry.html
  case "32":
		c.Body = `FundRef DOI`

  // Control number assigned to a Name Authority record by the Biblioteca Nacional de España
  case "33":
		c.Body = `BNE CN`

  // Numéro de la notice de personne BNF
  case "34":
		c.Body = `BNF Control Number`

  // Archival Resource Key, as a URL (including the address of the ARK resolver provided by eg a national library)
  case "35":
		c.Body = `ARK`

  // Nasjonalt autoritetsregister for navn – Norwegian national authority file for personal and corporate names. For use in ONIX 3.0 only
  case "36":
		c.Body = `Nasjonalt autoritetsregister`

  // Global Research Identifier Database ID (see https://www.grid.ac). For use in ONIX 3.0 only
  case "37":
		c.Body = `GRID`

  // Party ID from Identifiers and Standards for Higher Education and Research (fr: Identifiants et Référentiels pour l’enseignement supérieur et la recherche). For use on ONIX 3.0 only. See https://www.idref.fr
  case "38":
		c.Body = `IDRef`
	default:
		return fmt.Errorf("undefined code for AddresseeIDType has been passed, got [%s]", v)
	}
	return nil
}

// AgentIDType Supplier identifier type
type AgentIDType struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *AgentIDType) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // Note that <IDTypeName> is required with proprietary identifiers
  case "01":
		c.Body = `Proprietary`

  // DEPRECATED – use 01
  case "02":
		c.Body = `Proprietary`

  // Börsenverein Verkehrsnummer
  case "04":
		c.Body = `Börsenverein Verkehrsnummer`

  // German ISBN Agency publisher identifier
  case "05":
		c.Body = `German ISBN Agency publisher identifier`

  // GS1 global location number (formerly EAN location number)
  case "06":
		c.Body = `GLN`

  // Book trade Standard Address Number – US, UK etc
  case "07":
		c.Body = `SAN`

  // Flemish supplier code
  case "12":
		c.Body = `Distributeurscode Boekenbank`

  // Flemish publisher code
  case "13":
		c.Body = `Fondscode Boekenbank`

  // Identifier for a business organization for VAT purposes, eg within the EU’s VIES system. See http://ec.europa.eu/taxation_customs/vies/faqvies.do for EU VAT ID formats, which vary from country to country. Generally these consist of a two-letter country code followed by the 8–12 digits of the national VAT ID. Some countries include one or two letters within their VAT ID. See http://en.wikipedia.org/wiki/VAT_identification_number for non-EU countries that maintain similar identifiers. Spaces, dashes etc should be omitted
  case "23":
		c.Body = `VAT Identity Number`
	default:
		return fmt.Errorf("undefined code for AgentIDType has been passed, got [%s]", v)
	}
	return nil
}

// AgentRole Agent role
type AgentRole struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *AgentRole) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // Publisher’s exclusive sales agent in a specified territory
  case "05":
		c.Body = `Exclusive sales agent`

  // Publisher’s non-exclusive sales agent in a specified territory
  case "06":
		c.Body = `Non-exclusive sales agent`

  // Publisher for a specified territory
  case "07":
		c.Body = `Local publisher`

  // Publisher’s sales agent in a specific territory. Use only where exclusive / non-exclusive status is not known. Prefer 05 or 06 as appropriate, where possible
  case "08":
		c.Body = `Sales agent`
	default:
		return fmt.Errorf("undefined code for AgentRole has been passed, got [%s]", v)
	}
	return nil
}

// AncillaryContentType Illustration and other content type
type AncillaryContentType struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *AncillaryContentType) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // See description in the <IllustrationTypeDescription> element
  case "00":
		c.Body = `Unspecified, see description`

  // Illustrations, black and white
  case "01":
		c.Body = `Illustrations, black and white`

  // Illustrations, color
  case "02":
		c.Body = `Illustrations, color`

  // Including black and white photographs
  case "03":
		c.Body = `Halftones, black and white`

  // Including color photographs
  case "04":
		c.Body = `Halftones, color`

  // Line drawings, black and white
  case "05":
		c.Body = `Line drawings, black and white`

  // Line drawings, color
  case "06":
		c.Body = `Line drawings, color`

  // Tables, black and white
  case "07":
		c.Body = `Tables, black and white`

  // Tables, color
  case "08":
		c.Body = `Tables, color`

  // Illustrations, unspecified
  case "09":
		c.Body = `Illustrations, unspecified`

  // Including photographs
  case "10":
		c.Body = `Halftones, unspecified`

  // Tables, unspecified
  case "11":
		c.Body = `Tables, unspecified`

  // Line drawings, unspecified
  case "12":
		c.Body = `Line drawings, unspecified`

  // Halftones, duotone
  case "13":
		c.Body = `Halftones, duotone`

  // Maps
  case "14":
		c.Body = `Maps`

  // Frontispiece
  case "15":
		c.Body = `Frontispiece`

  // Diagrams
  case "16":
		c.Body = `Diagrams`

  // Figures
  case "17":
		c.Body = `Figures`

  // Charts
  case "18":
		c.Body = `Charts`

  // Recorded music extracts or examples, or complete recorded work(s), accompanying textual or other content
  case "19":
		c.Body = `Recorded music items`

  // Printed music extracts or examples, or complete music score(s), accompanying textual or other content
  case "20":
		c.Body = `Printed music items`

  // To be used in the mathematical sense of a diagram that represents numerical values plotted against an origin and axes, cf codes 16 and 18
  case "21":
		c.Body = `Graphs`

  // ‘Plates’ means illustrations that are on separate pages bound into the body of a book
  case "22":
		c.Body = `Plates, unspecified`

  // ‘Plates’ means illustrations that are on separate pages bound into the body of a book
  case "23":
		c.Body = `Plates, black and white`

  // ‘Plates’ means illustrations that are on separate pages bound into the body of a book
  case "24":
		c.Body = `Plates, color`

  // Index
  case "25":
		c.Body = `Index`

  // Bibliography
  case "26":
		c.Body = `Bibliography`

  // Larger-scale inset maps of places or features of interest included in a map product
  case "27":
		c.Body = `Inset maps`

  // GPS grids included in a map product
  case "28":
		c.Body = `GPS grids`

  // Glossary
  case "29":
		c.Body = `Glossary`

  // For use in ONIX 3.0 only
  case "30":
		c.Body = `Table of contents`
	default:
		return fmt.Errorf("undefined code for AncillaryContentType has been passed, got [%s]", v)
	}
	return nil
}

// AudienceCode Audience type
type AudienceCode struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *AudienceCode) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // For a non-specialist adult audience. Consider also adding an ONIX Adult audience rating
  case "01":
		c.Body = `General/trade`

  // For a juvenile audience, not specifically for any educational purpose. An audience range should also be included
  case "02":
		c.Body = `Children/juvenile`

  // For a teenage audience, not specifically for any educational purpose. An audience range should also be included
  case "03":
		c.Body = `Young adult`

  // Kindergarten, pre-school, primary/elementary or secondary/high school education. An audience range should also be included
  case "04":
		c.Body = `Primary and secondary/elementary and high school`

  // For tertiary education – universities and colleges of higher education
  case "05":
		c.Body = `College/higher education`

  // For an expert adult audience, including professional development and academic research
  case "06":
		c.Body = `Professional and scholarly`

  // Intended for use in teaching English as a second, non-native or additional language. Indication of the language level (eg CEFR) should be included where possible. An audience range should also be included if the product is (also) suitable for use in primary and secondary education
  case "07":
		c.Body = `ELT/ESL`

  // For an adult audience in a formal or semi-formal learning setting, eg vocational training, apprenticeships, or academic or recreational learning for adults
  case "08":
		c.Body = `Adult education`

  // Intended for use in teaching second, non-native or additional languages, for example teaching German to Spanish speakers. Indication of the language level (eg CEFR) should be included where possible. An audience range should also be included if the product is (also) suitable for use in primary and secondary education. Prefer code 07 for products specific to teaching English
  case "09":
		c.Body = `Second language teaching`
	default:
		return fmt.Errorf("undefined code for AudienceCode has been passed, got [%s]", v)
	}
	return nil
}

// AudienceCodeType Audience code type
type AudienceCodeType struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *AudienceCodeType) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // Using a code from List 28
  case "01":
		c.Body = `ONIX audience codes`

  // As specified in <AudienceCodeTypeName>
  case "02":
		c.Body = `Proprietary`

  // Motion Picture Association of America rating applied to movies
  case "03":
		c.Body = `MPAA rating`

  // British Board of Film Classification rating applied to movies
  case "04":
		c.Body = `BBFC rating`

  // German FSK (Freiwillige Selbstkontrolle der Filmwirtschaft) rating applied to movies
  case "05":
		c.Body = `FSK rating`

  // French Canadian audience code list, used by BTLF for Memento
  case "06":
		c.Body = `BTLF audience code`

  // Audience code used by Electre (France)
  case "07":
		c.Body = `Electre audience code`

  // Spain: educational audience and material type code of the Asociación Nacional de Editores de Libros y Material de Enseñanza
  case "08":
		c.Body = `ANELE Tipo`

  // Code list used to specify reading levels for children’s books, used in Flanders, and formerly in the Netherlands – see also code 18
  case "09":
		c.Body = `AVI`

  // German USK (Unterhaltungssoftware Selbstkontrolle) rating applied to video or computer games
  case "10":
		c.Body = `USK rating`

  // Audience code used in Flanders
  case "11":
		c.Body = `AWS`

  // Type of school: codelist maintained by VdS Bildungsmedien eV, the German association of educational media publishers. See http://www.bildungsmedien.de/service/onixlisten/schulform_onix_codelist29_value12_0408.pdf
  case "12":
		c.Body = `Schulform`

  // School region: codelist maintained by VdS Bildungsmedien eV, the German association of educational media publishers, indicating where products are licensed to be used in schools. See http://www.bildungsmedien.de/service/onixlisten/bundesland_onix_codelist29_value13_0408.pdf
  case "13":
		c.Body = `Bundesland`

  // Occupation: codelist for vocational training materials, maintained by VdS Bildungsmedien eV, the German association of educational media publishers. See http://www.bildungsmedien.de/service/onixlisten/ausbildungsberufe_onix_codelist29_value14_0408.pdf
  case "14":
		c.Body = `Ausbildungsberuf`

  // Finnish school or college level
  case "15":
		c.Body = `Suomalainen kouluasteluokitus`

  // UK Publishers Association, Children’s Book Group, coded indication of intended reader age, carried on book covers
  case "16":
		c.Body = `CBG age guidance`

  // Audience code used in Nielsen Book Services
  case "17":
		c.Body = `Nielsen Book audience code`

  // Code list used to specify reading levels for children’s books, used in the Netherlands – see also code 09
  case "18":
		c.Body = `AVI (revised)`

  // Lexile measure (the Lexile measure in <AudienceCodeValue> may optionally be prefixed by the Lexile code). Examples might be ‘880L’, ‘AD0L’ or ‘HL600L’. Deprecated – use <Complexity> instead
  case "19":
		c.Body = `Lexile measure`

  // Fry readability metric based on number of sentences and syllables per 100 words. Expressed as a number from 1 to 15 in <AudienceCodeValue>. Deprecated – use <Complexity> instead
  case "20":
		c.Body = `Fry Readability score`

  // Children’s audience code (対象読者), two-digit encoding of intended target readership from 0–2 years up to High School level
  case "21":
		c.Body = `Japanese Children’s audience code`

  // Publisher’s rating indicating suitability for a particular adult audience, using a code from List 203. Should only be used when the ONIX Audience code indicates a general adult audience (code 01 from List 28)
  case "22":
		c.Body = `ONIX Adult audience rating`

  // Codes A1 to C2 indicating standardised level of language learning or teaching material, from beginner to advanced, defined by the Council of Europe (see http://www.coe.int/lang-CEFR)
  case "23":
		c.Body = `Common European Framework of Reference for Language Learning (CEFR)`

  // Rating used in Korea to control selling of books and e-books to minors. Current values are 0 (suitable for all) and 19 (only for sale to ages 19+). See http://www.kpec.or.kr/english/
  case "24":
		c.Body = `Korean Publication Ethics Commission rating`

  // UK Institute of Education Book Bands for Guided Reading scheme (see http://www.ioe.ac.uk/research/4664.html). <AudienceCodeValue> is a color, eg ‘Pink A’ or ‘Copper’. Deprecated – use <Complexity> instead
  case "25":
		c.Body = `IoE Book Band`

  // Used for German videos/DVDs with educational or informative content; value for <AudienceCodeValue> must be either ‘Infoprogramm gemäß § 14 JuSchG’ or ‘Lehrprogramm gemäß § 14 JuSchG’
  case "26":
		c.Body = `FSK Lehr-/Infoprogramm`

  // Where this is different from the language of the text of the book recorded in <Language>. <AudienceCodeValue> should be a value from List 74
  case "27":
		c.Body = `Intended audience language`

  // Pan European Game Information rating used primarily for video games
  case "28":
		c.Body = `PEGI rating`

  // Code indicating the intended curriculum (eg Naturvetenskapsprogrammet, Estetica programmet) in Swedish higher secondary education
  case "29":
		c.Body = `Gymnasieprogram`
	default:
		return fmt.Errorf("undefined code for AudienceCodeType has been passed, got [%s]", v)
	}
	return nil
}

// AudienceRangePrecision Audience range precision
type AudienceRangePrecision struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *AudienceRangePrecision) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // Exact
  case "01":
		c.Body = `Exact`

  // From
  case "03":
		c.Body = `From`

  // To
  case "04":
		c.Body = `To`
	default:
		return fmt.Errorf("undefined code for AudienceRangePrecision has been passed, got [%s]", v)
	}
	return nil
}

// AudienceRangeQualifier Audience range qualifier
type AudienceRangeQualifier struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *AudienceRangeQualifier) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // Values for <AudienceRangeValue> are specified in List 77
  case "11":
		c.Body = `US school grade range`

  // Values are defined by BIC for England and Wales, Scotland and N Ireland
  case "12":
		c.Body = `UK school grade`

  // Values in <AudienceRangeValue> must be integers
  case "15":
		c.Body = `Reading speed, words per minute`

  // For use up to 36 months only: values in <AudienceRangeValue> must be integers
  case "16":
		c.Body = `Interest age, months`

  // Values in <AudienceRangeValue> must be integers
  case "17":
		c.Body = `Interest age, years`

  // Values in <AudienceRangeValue> must be integers
  case "18":
		c.Body = `Reading age, years`

  // Spain: combined grade and region code, maintained by the Ministerio de Educación
  case "19":
		c.Body = `Spanish school grade`

  // Norwegian educational level for primary and secondary education
  case "20":
		c.Body = `Skoletrinn`

  // Swedish educational qualifier (code)
  case "21":
		c.Body = `Nivå`

  // Italian school grade
  case "22":
		c.Body = `Italian school grade`

  // DEPRECATED – assigned in error: see List 29
  case "23":
		c.Body = `Schulform`

  // DEPRECATED – assigned in error: see List 29
  case "24":
		c.Body = `Bundesland`

  // DEPRECATED – assigned in error: see List 29
  case "25":
		c.Body = `Ausbildungsberuf`

  // Values for <AudienceRangeValue> are specified in List 77
  case "26":
		c.Body = `Canadian school grade range`

  // Finnish school grade range
  case "27":
		c.Body = `Finnish school grade range`

  // Lukion kurssi
  case "28":
		c.Body = `Finnish Upper secondary school course`

  // Values are P, K, 1–17 (including college-level audiences), see List 227
  case "29":
		c.Body = `Chinese School Grade range`

  // French educational level classification, URI http://data.education.fr/voc/scolomfr/scolomfr-voc-022
  case "30":
		c.Body = `Nomenclature niveaux éducatif détaillé`

  // Nível de Educação do Brasil, see List 238. For use in ONIX 3.0 only
  case "31":
		c.Body = `Brazil Education level`

  // French educational level classification, URI http://data.education.fr/voc/scolomfr/scolomfr-voc-012. For use in ONIX 3.0 only
  case "32":
		c.Body = `Nomenclature niveaux éducatif de base`

  // For use in ONIX 3.0 only
  case "33":
		c.Body = `Finnish Upper secondary school course (2021+)`
	default:
		return fmt.Errorf("undefined code for AudienceRangeQualifier has been passed, got [%s]", v)
	}
	return nil
}

// BarcodeType Barcode indicator
type BarcodeType struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *BarcodeType) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // Not barcoded
  case "00":
		c.Body = `Not barcoded`

  // Barcoded, scheme unspecified
  case "01":
		c.Body = `Barcoded, scheme unspecified`

  // GTIN-13
  case "02":
		c.Body = `GTIN-13`

  // GTIN-13+5 (US dollar price encoded)
  case "03":
		c.Body = `GTIN-13+5 (US dollar price encoded)`

  // GTIN-13+5 (CAN dollar price encoded)
  case "04":
		c.Body = `GTIN-13+5 (CAN dollar price encoded)`

  // GTIN-13+5 (no price encoded)
  case "05":
		c.Body = `GTIN-13+5 (no price encoded)`

  // AKA item/price
  case "06":
		c.Body = `UPC-12 (item-specific)`

  // AKA item/price
  case "07":
		c.Body = `UPC-12+5 (item-specific)`

  // AKA price/item
  case "08":
		c.Body = `UPC-12 (price-point)`

  // AKA price/item
  case "09":
		c.Body = `UPC-12+5 (price-point)`
	default:
		return fmt.Errorf("undefined code for BarcodeType has been passed, got [%s]", v)
	}
	return nil
}

// BibleContents Bible contents
type BibleContents struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *BibleContents) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // The seven portions of the Apocrypha added to the Catholic canon at the Council of Trent in 1546: Tobit; Judith; Wisdom of Solomon; Sirach (Ecclesiasticus); Baruch, including the Letter of Jeremiah; I and II Maccabees; Extra portions of Esther and Daniel (Additions to Esther; the Prayer of Azariah; Song of the Three Jews; Susannah; Bel and the Dragon). These are not generally included in the Protestant canon
  case "AP":
		c.Body = `Apocrypha (Catholic canon)`

  // A collection of Apocryphal texts, canon not specified
  case "AQ":
		c.Body = `Apocrypha (canon unspecified)`

  // I Esdras; Prayer of Manasseh; Psalm 151; III Maccabees
  case "AX":
		c.Body = `Additional Apocryphal texts: Greek Orthodox canon`

  // I and II Esdras; Prayer of Manasseh; Psalm 151; III and IV Maccabees
  case "AY":
		c.Body = `Additional Apocryphal texts: Slavonic Orthodox canon`

  // Additional Apocryphal texts included in some Bible versions: I and II Esdras; Prayer of Manasseh
  case "AZ":
		c.Body = `Additional Apocryphal texts`

  // The 66 books included in the Protestant, Catholic and Orthodox canons, together with the seven portions of the Apocrypha included in the Catholic canon. (Equivalent to OT plus NT plus AP)
  case "GA":
		c.Body = `General canon with Apocrypha (Catholic canon)`

  // The 66 books included in the Protestant, Catholic and Orthodox canons, together with Apocryphal texts, canon not specified. (Equivalent to OT plus NT plus AQ)
  case "GC":
		c.Body = `General canon with Apocryphal texts (canon unspecified)`

  // The 66 books included in the Protestant, Catholic and Orthodox canons, 39 from the Old Testament and 27 from the New Testament. The sequence of books may differ in different canons. (Equivalent to OT plus NT)
  case "GE":
		c.Body = `General canon`

  // The books of Matthew, Mark, Luke and John
  case "GS":
		c.Body = `Gospels`

  // Those 39 books which were included in the Jewish canon by the rabbinical academy established at Jamma in 90 CE. Also known as the Jewish or Hebrew scriptures
  case "OT":
		c.Body = `Old Testament`

  // The 27 books included in the Christian canon through the Easter Letter of Athanasius, Bishop of Alexandria and also by a general council of the Christian church held near the end of the 4th century CE
  case "NT":
		c.Body = `New Testament`

  // Includes the 27 books of the New Testament plus Psalms and Proverbs from the Old Testament. Equivalent to NT plus PP)
  case "NP":
		c.Body = `New Testament with Psalms and Proverbs`

  // The books containing the letters of Paul to the various early Christian churches
  case "PE":
		c.Body = `Paul’s Epistles`

  // The book of Psalms and the book of Proverbs combined
  case "PP":
		c.Body = `Psalms and Proverbs`

  // The book of Psalms
  case "PS":
		c.Body = `Psalms`

  // The first five books of the Bible: Genesis, Exodus, Numbers, Leviticus, Deuteronomy. Also applied to the Torah
  case "PT":
		c.Body = `Pentateuch`

  // Selected books of either the OT or NT not otherwise noted
  case "ZZ":
		c.Body = `Other portions`
	default:
		return fmt.Errorf("undefined code for BibleContents has been passed, got [%s]", v)
	}
	return nil
}

// BiblePurpose Bible purpose
type BiblePurpose struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *BiblePurpose) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // A Bible (or selected Biblical text) designed for presentation from a religious organization
  case "AW":
		c.Body = `Award`

  // A Bible (or selected Biblical text) designed to be a gift to commemorate a child’s birth
  case "BB":
		c.Body = `Baby`

  // A special gift Bible (or selected Biblical text) designed for the bride on her wedding day. Usually white
  case "BR":
		c.Body = `Bride`

  // A Bible (or selected Biblical text) designed to be used in the confirmation reading or as a gift to a confirmand
  case "CF":
		c.Body = `Confirmation`

  // A text Bible (or selected Biblical text) designed in presentation and readability for a child
  case "CH":
		c.Body = `Children’s`

  // A small Bible (or selected Biblical text) with a trim height of five inches or less
  case "CM":
		c.Body = `Compact`

  // A Bible (or selected Biblical text) which includes text conveying cross-references to related scripture passages
  case "CR":
		c.Body = `Cross-reference`

  // A Bible (or selected Biblical text) laid out to provide readings for each day of the year
  case "DR":
		c.Body = `Daily readings`

  // A Bible (or selected Biblical text) containing devotional content together with the scripture
  case "DV":
		c.Body = `Devotional`

  // A Bible (or selected Biblical text) containing family record pages and/or additional study material for family devotion
  case "FM":
		c.Body = `Family`

  // A standard Bible (or selected Biblical text) of any version with no distinguishing characteristics beyond the canonical text
  case "GT":
		c.Body = `General/Text`

  // A Bible (or selected Biblical text) designed for gift or presentation, often including a presentation page
  case "GF":
		c.Body = `Gift`

  // A large Bible (or selected Biblical text) with large print designed for use in reading scriptures in public worship from either the pulpit or lectern
  case "LP":
		c.Body = `Lectern/Pulpit`

  // A Bible (or selected Biblical text) especially designed with helps and study guides oriented to the adult male
  case "MN":
		c.Body = `Men’s`

  // A Bible (or selected Biblical text) designed for use in primary school
  case "PS":
		c.Body = `Primary school`

  // Usually inexpensive but sturdy, a Bible (or selected Biblical text) designed for use in church pews
  case "PW":
		c.Body = `Pew`

  // A Bible (or selected Biblical text) including texts in Greek and/or Hebrew and designed for scholarly study
  case "SC":
		c.Body = `Scholarly`

  // Slimline
  case "SL":
		c.Body = `Slimline`

  // A Bible (or selected Biblical text) with study articles and helps especially for use in the classroom
  case "ST":
		c.Body = `Student`

  // A Bible (or selected Biblical text) with many extra features, e.g. book introductions, dictionary, concordance, references, maps, etc., to help readers better understand the scripture
  case "SU":
		c.Body = `Study`

  // A special gift Bible (or selected Biblical text) designed as a gift to the couple on their wedding day
  case "WG":
		c.Body = `Wedding gift`

  // A devotional or study Bible (or selected Biblical text) with helps targeted at the adult woman
  case "WM":
		c.Body = `Women’s`

  // A Bible (or selected Biblical text) containing special study and devotional helps designed specifically for the needs of teenagers
  case "YT":
		c.Body = `Youth`
	default:
		return fmt.Errorf("undefined code for BiblePurpose has been passed, got [%s]", v)
	}
	return nil
}

// BibleReferenceLocation Bible reference location
type BibleReferenceLocation struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *BibleReferenceLocation) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // References are printed in a narrow column in the center of the page between two columns of text
  case "CCL":
		c.Body = `Center column`

  // References are printed at the foot of the page
  case "PGE":
		c.Body = `Page end`

  // References are printed in a column to the side of the scripture
  case "SID":
		c.Body = `Side column`

  // References are printed at the end of the applicable verse
  case "VER":
		c.Body = `Verse end`

  // The person creating the ONIX record does not know where the references are located
  case "UNK":
		c.Body = `Unknown`

  // Other locations not otherwise identified
  case "ZZZ":
		c.Body = `Other`
	default:
		return fmt.Errorf("undefined code for BibleReferenceLocation has been passed, got [%s]", v)
	}
	return nil
}

// BibleTextFeature Bible text feature
type BibleTextFeature struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *BibleTextFeature) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // Words spoken by Christ are printed in red
  case "RL":
		c.Body = `Red letter`
	default:
		return fmt.Errorf("undefined code for BibleTextFeature has been passed, got [%s]", v)
	}
	return nil
}

// BibleTextOrganization Bible text organization
type BibleTextOrganization struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *BibleTextOrganization) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // A Bible with the text organized in the order in which events are believed to have happened
  case "CHR":
		c.Body = `Chronological`

  // A Bible which explores keywords or themes by referring text to preceding or following text
  case "CHA":
		c.Body = `Chain reference`

  // A Bible or other text in which different versions are printed one line above the other, so that the variations can easily be detected
  case "INT":
		c.Body = `Interlinear`

  // A Bible with two or more versions printed side by side
  case "PAR":
		c.Body = `Parallel`

  // A Bible in which the text is presented in the traditional order
  case "STN":
		c.Body = `Standard`
	default:
		return fmt.Errorf("undefined code for BibleTextOrganization has been passed, got [%s]", v)
	}
	return nil
}

// BibleVersion Bible version
type BibleVersion struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *BibleVersion) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // Alberto Vaccari – Pontificio Istituto Biblico
  case "ALV":
		c.Body = `Alberto Vaccari`

  // A translation based on the American Standard Version and showing multiple options for the translation of ancient text. Published in full in 1965. Sponsored by the Lockman Foundation
  case "AMP":
		c.Body = `Amplified`

  // Most popular Catholic Bible translation in Italian prior to the CEI translation in 1971
  case "ANM":
		c.Body = `Antonio Martini`

  // A 1901 translation using verbal equivalence techniques with the purpose of Americanizing the REV
  case "ASV":
		c.Body = `American Standard`

  // 2011 contemporary English translation of the Bible sponsored by the US-based Christian Resources Development Corporation. The translation includes Old Testament, Apocrypha and New Testament, and is aimed to be accessible to most English readers (minimum 7th grade reading age)
  case "CEB":
		c.Body = `Common English Bible`

  // Italian Episcopal Conference 1971 translation suitable for Italian Catholic liturgy. (Includes minor 1974 revision)
  case "CEI":
		c.Body = `Conferenza Episcopale Italiana`

  // New translation of the C.E.I. first published in 2008 – the version most widely used by the Italian Catholic Church
  case "CEN":
		c.Body = `Conferenza Episcopale Italiana 2008`

  // A translation completed in 1995 and sponsored by the American Bible Society under the leadership of Barclay Newman
  case "CEV":
		c.Body = `Contemporary English`

  // 1968 Interfaith version promoted by the Italian Bible Society. Has a Catholic ‘imprimateur’, but its ecumenical approach has Jewish, Protestant and Christian Orthodox approval
  case "CNC":
		c.Body = `Concordata`

  // Version based on original documents, edited by Giovanni Diodati in 1607, revised by Diodati in 1641 and again in 1894. It is the reference version for many Italian Protestants
  case "DDI":
		c.Body = `Diodati`

  // Revision of the Diodati Bible dating to the 1990s, aiming at highest fidelity to original ancient Greek (New Testament) and Hebrew (Old Testament) texts
  case "DDN":
		c.Body = `Nuova Diodati`

  // An early (1580-1609) English translation from the Latin Vulgate designed for Catholics and performed by George Martin
  case "DOU":
		c.Body = `Douay-Rheims`

  // A German translation of the Bible for use in Roman Catholic churches
  case "EIN":
		c.Body = `Einheitsübersetzung`

  // An update of the Revised Standard Version that makes ‘modest’ use of gender-free terminology
  case "ESV":
		c.Body = `English Standard`

  // Finnish Bible translation
  case "FBB":
		c.Body = `Biblia (1776)`

  // Finnish Bible translation
  case "FRA":
		c.Body = `Raamattu (1933/1938)`

  // Finnish Bible translation
  case "FRK":
		c.Body = `Raamattu kansalle`

  // Finnish Bible translation
  case "FRM":
		c.Body = `Raamattu (1992)`

  // A 1995 translation by the World Bible Publishing Company using the English language in a manner to communicate to the late 20th century American
  case "GDW":
		c.Body = `God’s Word`

  // An early (1560) English version of the Bible translated by William Whittingham with strong Protestant leanings
  case "GEN":
		c.Body = `Geneva`

  // A translation sponsored by the American Bible Society. The New Testament was first published (as ‘Today’s English Version’ TEV) in 1966. The Old Testament was completed in 1976, and the whole was published as the ‘Good News Bible’
  case "GNB":
		c.Body = `Good News`

  // Version edited by E. Galbiati, A. Penna and P. Rossano, and published by UTET. This version, based on original texts, is rich in notes and has been used as the basis for CEI translation
  case "GPR":
		c.Body = `Galbiati, Penna, Rossano – UTET`

  // New Testament text in an original Greek version
  case "GRK":
		c.Body = `Original Greek`

  // Richly annotated 1963 Version edited by S. Garofano and S. Rinaldi, and published by Marietti
  case "GRM":
		c.Body = `Garofano, Rinaldi – Marietti`

  // Old Testament text in an original Hebrew version
  case "HBR":
		c.Body = `Original Hebrew`

  // Published by Broadman and Holman this translation rejects all forms of gender-neutral wording and is written with strong influences from the Southern Baptist perspective of biblical scholarship
  case "HCS":
		c.Body = `Holman Christian Standard`

  // A translation completed in 1986 targeting readability at the US third grade level
  case "ICB":
		c.Body = `International Children’s`

  // Interconfessional translation resulting from 1985 effort by Catholic and Protestant scholars, aimed at delivering an easy-to-understand message
  case "ILC":
		c.Body = `Traduzione Interconfessionale in Lingua Corrente`

  // A translation designed for English speaking Catholics based on the original languages. It is based on French as well as ancient texts and was first published in 1966
  case "JER":
		c.Body = `Jerusalem`

  // A translation commissioned by King James I of England and first published in 1611
  case "KJV":
		c.Body = `King James`

  // A verbal translation led by William Prindele. Published in 1994, it was designed to modernize the language of the King James Version based on Webster’s New International Dictionary, 2nd edition, unabridged
  case "KJT":
		c.Body = `21st Century King James`

  // A paraphrase translation led by Kenneth N Taylor and first published in 1972
  case "LVB":
		c.Body = `Living Bible`

  // 1924 translation by Giovanni Luzzi, Professor at the Waldensian Faculty of Theology in Rome, who revised the 17th Century Diodati version
  case "LZZ":
		c.Body = `Luzzi`

  // A paraphrase translation of the New Testament by Eugene Peterson first published in 1993
  case "MSG":
		c.Body = `Message Bible`

  // A translation aimed at Catholic readers first published in its entirety in 1970. A revised New Testament was issued in 1986 as the 2nd Edition. The 3rd Edtion was published in 1991 with a revision to Psalms. The 4th Edition (also known as the New American Bible Revised Edition) was published in 2011, incorporating revisions to the Old Testament
  case "NAB":
		c.Body = `New American`

  // A translation commissioned by the Lockman Foundation. The New Testament was published in 1960 followed by the entire Bible in 1971
  case "NAS":
		c.Body = `New American Standard`

  // A 1995 translation using more modern language than the NASB
  case "NAU":
		c.Body = `New American Standard, Updated`

  // Norwegian Bible translation
  case "NBA":
		c.Body = `Bibelen 1895`

  // Norwegian Bible translation
  case "NBB":
		c.Body = `Bibelen 1930`

  // Norwegian Bible translation
  case "NBC":
		c.Body = `Bibelen 1938`

  // Norwegian Bible translation
  case "NBD":
		c.Body = `Bibelen 1978-85`

  // Norwegian Bible translation
  case "NBE":
		c.Body = `Bibelen 1978`

  // Norwegian Bible translation
  case "NBF":
		c.Body = `Bibelen 1985`

  // Norwegian Bible translation
  case "NBG":
		c.Body = `Bibelen 1988`

  // Norwegian Bible translation
  case "NBH":
		c.Body = `Bibelen 1978-85/rev. 2005`

  // Norwegian Bible translation
  case "NBI":
		c.Body = `Bibelen 2011`

  // A translation inspired by the International Children’s version. First published by World Publishing in 1991
  case "NCV":
		c.Body = `New Century`

  // A translation first issued in 1961 (New Testament) and 1970 (complete Bible) as a result of a proposal at the 1946 General Assembly of the Church of Scotland
  case "NEB":
		c.Body = `New English`

  // Norwegian Bible translation
  case "NGO":
		c.Body = `Bibelen Guds ord`

  // A translation underwritten by Biblica (formerly the International Bible Society, and previously the New York Bible Society). The New Testament was published in 1973 followed by the entire Bible in 1978. The NIV text was revised in 1984 and again in 2011
  case "NIV":
		c.Body = `New International`

  // A 1996 translation designed for people with limited literacy in English and based on the NIV
  case "NIR":
		c.Body = `New International Reader’s`

  // A revision of the Jerusalem Bible. First published in 1986
  case "NJB":
		c.Body = `New Jerusalem`

  // A version issued by Thomas Nelson Publishers in 1982-83 designed to update the language of the King James Version while maintaining the phrasing and rhythm and using the same sources as its predecessor
  case "NKJ":
		c.Body = `New King James`

  // Norwegian ‘nynorsk’ Bible translation
  case "NNK":
		c.Body = `Bibelen, nynorsk`

  // A translation sponsored by Tyndale House and first released in 1996. It is considered a revision and updating of the Living Bible
  case "NLV":
		c.Body = `New Living`

  // A revision of the Revised Standard based on ancient texts but updating language to American usage of the 1980s
  case "NRS":
		c.Body = `New Revised Standard`

  // A Spanish translation from the original Greek and Hebrew, sponsored by Tyndale House
  case "NTV":
		c.Body = `Nueva Traduccion Vivienta`

  // Nuovissima version – a Catholic-oriented translation in modern Italian, edited by a group including Carlo Martini, Gianfranco Ravasi and Ugo Vanni and first published (in 48 volumes, 1967-1980) by Edizioni San Paolo
  case "NVB":
		c.Body = `Novissima Versione della Bibbia`

  // A Spanish translation from the original Greek and Hebrew, sponsored by the International Bible Society/Sociedad Bíblica Internacional
  case "NVD":
		c.Body = `Nueva Biblia al Dia`

  // A Spanish translation underwritten by the International Bible Society
  case "NVI":
		c.Body = `Nueva Version Internacional`

  // An idiomatic translation by J B Phillips, first completed in 1966
  case "PHP":
		c.Body = `New Testament in Modern English (Phillips)`

  // A 1989 revision of the NEB. A significant effort was made to reduce the British flavor present in the NEB
  case "REB":
		c.Body = `Revised English`

  // The first major revision of the King James Version, the Revised Version incorporates insights from early manuscripts discovered between 1611 and 1870, and corrects readings in the KJV which nineteenth-century scholarship deemed mistaken. The New Testament was published in 1881, the Old Testament in 1885, and the Apocrypha in 1895
  case "REV":
		c.Body = `Revised Version`

  // A translation authorized by the National Council of Churches of Christ in the USA. The New Testament was published in 1946 followed by a complete Protestant canon in 1951
  case "RSV":
		c.Body = `Revised Standard`

  // A Spanish translation based on the original texts
  case "RVL":
		c.Body = `Reina Valera`

  // Swedish Bible translation
  case "SBB":
		c.Body = `Bibel 2000`

  // Norwegian ‘samisk’ Bible translation
  case "SMK":
		c.Body = `Bibelen, samisk`

  // A translation of the New Testament sponsored by the American Bible Society and first published in 1966. It was incorporated into the ‘Good News Bible’ (GNB) in 1976
  case "TEV":
		c.Body = `Today’s English`

  // An updating of the New International Version. The New Testament was published in 2002, and the entire Bible in 2005. Superseded by the 2011 NIV update
  case "TNI":
		c.Body = `Today’s New International`

  // Other translations not otherwise noted
  case "ZZZ":
		c.Body = `Other`
	default:
		return fmt.Errorf("undefined code for BibleVersion has been passed, got [%s]", v)
	}
	return nil
}

// CitedContentType Cited content type
type CitedContentType struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *CitedContentType) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // The full text of a review in a third-party publication in any medium
  case "01":
		c.Body = `Review`

  // Bestseller list
  case "02":
		c.Body = `Bestseller list`

  // Other than a review
  case "03":
		c.Body = `Media mention`

  // (North America) Inclusion in a program such as ‘Chicago Reads’, ‘Seattle Reads’
  case "04":
		c.Body = `‘One locality, one book’ program`

  // For example a ‘best books of the year’ or ‘25 books you should have read’ list, without regard to their bestseller status
  case "05":
		c.Body = `Curated list`
	default:
		return fmt.Errorf("undefined code for CitedContentType has been passed, got [%s]", v)
	}
	return nil
}

// CollectionIDType Series identifier type
type CollectionIDType struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *CollectionIDType) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // For example, publisher’s own series ID. Note that <IDTypeName> is required with proprietary identifiers
  case "01":
		c.Body = `Proprietary`

  // International Standard Serial Number, unhyphenated, 8 digits
  case "02":
		c.Body = `ISSN`

  // Maintained by the Deutsche Nationalbibliothek
  case "03":
		c.Body = `German National Bibliography series ID`

  // Maintained by VLB
  case "04":
		c.Body = `German Books in Print series ID`

  // Maintained by Electre Information, France
  case "05":
		c.Body = `Electre series ID`

  // Digital Object Identifier (variable length and character set)
  case "06":
		c.Body = `DOI`

  // Use only where the collection (series or set) is available as a single product
  case "15":
		c.Body = `ISBN-13`

  // Uniform Resource Name
  case "22":
		c.Body = `URN`

  // French National Bibliography series ID. Identifiant des publications en série maintenu par la Bibliothèque Nationale de France
  case "29":
		c.Body = `BNF Control number`

  // Archival Resource Key, as a URL (including the address of the ARK resolver provided by eg a national library)
  case "35":
		c.Body = `ARK`

  // International Standard Serial Number ‘linking ISSN’, used when distinct from the serial ISSN. Unhyphenated, 8 digits. For use in ONIX 3.0 only
  case "38":
		c.Body = `ISSN-L`
	default:
		return fmt.Errorf("undefined code for CollectionIDType has been passed, got [%s]", v)
	}
	return nil
}

// CollectionSequenceType Collection sequence type
type CollectionSequenceType struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *CollectionSequenceType) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // A short explanatory label for the sequence should be provided in <CollectionSequenceTypeName>
  case "01":
		c.Body = `Proprietary`

  // Order as specified by the title, eg by volume or part number sequence, provided for confirmation
  case "02":
		c.Body = `Title order`

  // Order of publication of products within the collection
  case "03":
		c.Body = `Publication order`

  // Order defined by a continuing narrative or temporal sequence within products in the collection. Applicable to either fiction or to non-fiction (eg within a collection of history textbooks)
  case "04":
		c.Body = `Temporal/narrative order`

  // Original publication order, for a republished collection or collected works originally published outside a collection
  case "05":
		c.Body = `Original publication order`

  // Where it is different from the title order, publication order, narrative order etc
  case "06":
		c.Body = `Suggested reading order`

  // Where it is different from the title order, publication order, narrative order, reading order etc
  case "07":
		c.Body = `Suggested display order`
	default:
		return fmt.Errorf("undefined code for CollectionSequenceType has been passed, got [%s]", v)
	}
	return nil
}

// CollectionType Collection type
type CollectionType struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *CollectionType) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // Collection type is not determined
  case "00":
		c.Body = `Unspecified (default)`

  // The collection is a bibliographic collection (eg a series or set (Fr. série)) defined and identified by a publisher, either on the product itself or in product information supplied by the publisher. The books in the collection generally share a subject, narrative, design style or authorship. They may may have a specific order, or the collection may be unordered
  case "10":
		c.Body = `Publisher collection`

  // The collection is a bibliographic collection defined and identified by a publisher, either on the product itself or in product information supplied by the publisher, where the books in the collection have no specific order, shared subject, narrative, style or shared authorship, and are grouped by the publisher largely for marketing purposes. The collection has many of the characteristics of an imprint or marque. Used primarily in French book publishing, to distinguish between ‘série’ (using the normal code 10) and ‘collection’ (code 11), and where the collection éditoriale is not an imprint
  case "11":
		c.Body = `Collection éditoriale`

  // The collection has been defined and identified by a party in the metadata supply chain other than the publisher, typically an aggregator.
  case "20":
		c.Body = `Ascribed collection`
	default:
		return fmt.Errorf("undefined code for CollectionType has been passed, got [%s]", v)
	}
	return nil
}

// ComplexitySchemeIdentifier Complexity scheme identifier
type ComplexitySchemeIdentifier struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *ComplexitySchemeIdentifier) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // For example AD or HL. DEPRECATED in ONIX 3 – use code 06 instead
  case "01":
		c.Body = `Lexile code`

  // For example 880L. DEPRECATED in ONIX 3 – use code 06 instead
  case "02":
		c.Body = `Lexile number`

  // Fry readability metric based on number of sentences and syllables per 100 words. Expressed as an integer from 1 to 15 in <ComplexityCode>
  case "03":
		c.Body = `Fry Readability score`

  // UK Institute of Education Book Bands for Guided Reading scheme (see https://www.ucl.ac.uk/reading-recovery-europe/ilc/publications/which-book-why). <ComplexityCode> is a color, eg ‘Pink A’ or ‘Copper’
  case "04":
		c.Body = `IoE Book Band`

  // <ComplexityCode> is a code from ‘A’ to Z+’. See http://www.fountasandpinnellleveledbooks.com/aboutLeveledTexts.aspx
  case "05":
		c.Body = `Fountas &amp; Pinnell Text Level Gradient`

  // The Lexile measure in <ComplexityCode> combines the Lexile number (for example 620L or 880L) and optionally the Lexile code (for example AD or HL). Examples might be ‘880L’, ‘AD0L’ or ‘HL600L’. See https://lexile.com/about-lexile/lexile-overview/
  case "06":
		c.Body = `Lexile measure`

  // Advantage-TASA Open Standard book readability score, used for example within the Renaissance Learning Accelerated Reader scheme. <ComplexityCode> is the ‘Book Level’, a real number between 0 and 17. See http://www.renaissance.com/products/accelerated-reader/atos-analyzer
  case "07":
		c.Body = `ATOS for Books`

  // Flesch-Kincaid Grade Level Formula, a standard readability measure based on the weighted number of syllables per word and words per sentence. <ComplexityCode> is a real number typically between about -1 and 20
  case "08":
		c.Body = `Flesch-Kincaid Grade Level`

  // Use this code for books levelled by the publisher or a third party using the Fountas and Pinnell Guided Reading methodology
  case "09":
		c.Body = `Guided Reading Level`

  // Used for books aimed at K-2 literacy intervention. <ComplexityCode> is an integer between 1 and 20
  case "10":
		c.Body = `Reading Recovery Level`
	default:
		return fmt.Errorf("undefined code for ComplexitySchemeIdentifier has been passed, got [%s]", v)
	}
	return nil
}

// ConferenceRole Event role
type ConferenceRole struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *ConferenceRole) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // For example an academic, professional or political conference
  case "01":
		c.Body = `Publication linked to conference`

  // Complete proceedings of conference
  case "02":
		c.Body = `Complete proceedings of conference`

  // Selected papers from conference
  case "03":
		c.Body = `Selected papers from conference`

  // For example a competitive match, fixture series or championship
  case "11":
		c.Body = `Publication linked to sporting event`

  // Programme or guide for sporting event
  case "12":
		c.Body = `Programme or guide for sporting event`

  // For example a theatrical or musical event or performance, a season of events or performances, or an exhibition of art
  case "21":
		c.Body = `Publication linked to artistic event`

  // Programme or guide for artistic event
  case "22":
		c.Body = `Programme or guide for artistic event`

  // For example a commercial exposition
  case "31":
		c.Body = `Publication linked to exposition`

  // Programme or guide for exposition
  case "32":
		c.Body = `Programme or guide for exposition`
	default:
		return fmt.Errorf("undefined code for ConferenceRole has been passed, got [%s]", v)
	}
	return nil
}

// ConferenceSponsorIDType Name identifier type
type ConferenceSponsorIDType struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *ConferenceSponsorIDType) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // Note that <IDTypeName> is required with proprietary identifiers
  case "01":
		c.Body = `Proprietary`

  // DEPRECATED – use 01
  case "02":
		c.Body = `Proprietary`

  // Deutsche Nationalbibliothek publisher identifier
  case "03":
		c.Body = `DNB publisher identifier`

  // Börsenverein Verkehrsnummer
  case "04":
		c.Body = `Börsenverein Verkehrsnummer`

  // German ISBN Agency publisher identifier
  case "05":
		c.Body = `German ISBN Agency publisher identifier`

  // GS1 global location number (formerly EAN location number)
  case "06":
		c.Body = `GLN`

  // Book trade Standard Address Number – US, UK etc
  case "07":
		c.Body = `SAN`

  // MARC code list for organizations – see http://www.loc.gov/marc/organizations/orgshome.html
  case "08":
		c.Body = `MARC organization code`

  // Trading party identifier used in the Netherlands
  case "10":
		c.Body = `Centraal Boekhuis Relatie ID`

  // Flemish publisher code
  case "13":
		c.Body = `Fondscode Boekenbank`

  // Business Identity Code (Finland). See http://www.ytj.fi/ (in Finnish)
  case "15":
		c.Body = `Y-tunnus`

  // International Standard Name Identifier. A sixteen digit number. Usually presented with spaces or hyphens dividing the number into four groups of four digits, but in ONIX the spaces or hyphens should be omitted. See http://www.isni.org/
  case "16":
		c.Body = `ISNI`

  // Personennamendatei – person name authority file used by Deutsche Nationalbibliothek and in other German-speaking countries. See http://www.dnb.de/standardisierung/normdateien/pnd.htm (German) or http://www.dnb.de/eng/standardisierung/normdateien/pnd.htm (English). DEPRECATED in favor of the GND
  case "17":
		c.Body = `PND`

  // A control number assigned to a Library of Congress Control Number (LCCN) Name Authority / NACO record
  case "18":
		c.Body = `NACO`

  // Publisher identifier administered by Japanese ISBN Agency
  case "19":
		c.Body = `Japanese Publisher identifier`

  // Gemeinsame Körperschaftsdatei – Corporate Body Authority File in the German-speaking countries. See http://www.dnb.de/standardisierung/normdateien/gkd.htm (German) or http://www.dnb.de/eng/standardisierung/normdateien/gkd.htm (English). DEPRECATED in favor of the GND
  case "20":
		c.Body = `GKD`

  // Open Researcher and Contributor ID. A sixteen digit number. Usually presented with hyphens dividing the number into four groups of four digits, but in ONIX the hyphens should be omitted. See http://www.orcid.org/
  case "21":
		c.Body = `ORCID`

  // Publisher identifier maintained by the Chinese ISBN Agency (GAPP)
  case "22":
		c.Body = `GAPP Publisher Identifier`

  // Identifier for a business organization for VAT purposes, eg within the EU’s VIES system. See http://ec.europa.eu/taxation_customs/vies/faqvies.do for EU VAT ID formats, which vary from country to country. Generally these consist of a two-letter country code followed by the 8–12 digits of the national VAT ID. Some countries include one or two letters within their VAT ID. See http://en.wikipedia.org/wiki/VAT_identification_number for non-EU countries that maintain similar identifiers. Spaces, dashes etc should be omitted
  case "23":
		c.Body = `VAT Identity Number`

  // 4-digit business organization identifier controlled by the Japanese Publication Wholesalers Association
  case "24":
		c.Body = `JP Distribution Identifier`

  // Gemeinsame Normdatei – Joint Authority File in the German-speaking countries. See http://www.dnb.de/EN/gnd (English). Combines the PND, SWD and GKD into a single authority file, and should be used in preference
  case "25":
		c.Body = `GND`

  // Dunn and Bradstreet Universal Numbering System, see http://www.dnb.co.uk/dandb-duns-number
  case "26":
		c.Body = `DUNS`

  // Ringgold organizational identifier, see http://www.ringgold.com/identify.html
  case "27":
		c.Body = `Ringgold ID`

  // French Electre publisher identifier
  case "28":
		c.Body = `Identifiant Editeur Electre`

  // DOI used in EIDR party registry, for example ‘10.5237/C9F6-F41F’ (Sam Raimi). See http://eidr.org
  case "29":
		c.Body = `EIDR Party DOI`

  // French Electre imprint Identifier
  case "30":
		c.Body = `Identifiant Marque Electre`

  // Virtual Internet Authority File. <IDValue> should be a number. The URI form of the identifier can be created by prefixing the number with ‘https://viaf.org/viaf/’. See https://viaf.org
  case "31":
		c.Body = `VIAF ID`

  // DOI used in CrossRef’s Open Funder Registry list of academic research funding bodies, for example ‘10.13039/100004440’ (Wellcome Trust). See http://www.crossref.org/fundingdata/registry.html
  case "32":
		c.Body = `FundRef DOI`

  // Control number assigned to a Name Authority record by the Biblioteca Nacional de España
  case "33":
		c.Body = `BNE CN`

  // Numéro de la notice de personne BNF
  case "34":
		c.Body = `BNF Control Number`

  // Archival Resource Key, as a URL (including the address of the ARK resolver provided by eg a national library)
  case "35":
		c.Body = `ARK`

  // Nasjonalt autoritetsregister for navn – Norwegian national authority file for personal and corporate names. For use in ONIX 3.0 only
  case "36":
		c.Body = `Nasjonalt autoritetsregister`

  // Global Research Identifier Database ID (see https://www.grid.ac). For use in ONIX 3.0 only
  case "37":
		c.Body = `GRID`

  // Party ID from Identifiers and Standards for Higher Education and Research (fr: Identifiants et Référentiels pour l’enseignement supérieur et la recherche). For use on ONIX 3.0 only. See https://www.idref.fr
  case "38":
		c.Body = `IDRef`
	default:
		return fmt.Errorf("undefined code for ConferenceSponsorIDType has been passed, got [%s]", v)
	}
	return nil
}

// ContentAudience Content audience
type ContentAudience struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *ContentAudience) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // Any audience
  case "00":
		c.Body = `Unrestricted`

  // Distribution by agreement between the parties to the ONIX exchange (this value is provided to cover applications where ONIX content includes material which is not for general distribution)
  case "01":
		c.Body = `Restricted`

  // Distributors, bookstores, publisher’s own staff etc
  case "02":
		c.Body = `Booktrade`

  // End-customers
  case "03":
		c.Body = `End-customers`

  // Librarians
  case "04":
		c.Body = `Librarians`

  // Teachers
  case "05":
		c.Body = `Teachers`

  // Students
  case "06":
		c.Body = `Students`

  // Press or other media
  case "07":
		c.Body = `Press`

  // Where a specially formatted description is required for this audience
  case "08":
		c.Body = `Shopping comparison service`

  // Text not intended for display, but may be used (in addition to any less restricted text) for indexing and search
  case "09":
		c.Body = `Search engine index`

  // (Including vloggers, influencers etc) Where this is distinct from end customers or the Press
  case "10":
		c.Body = `Bloggers`
	default:
		return fmt.Errorf("undefined code for ContentAudience has been passed, got [%s]", v)
	}
	return nil
}

// ContentDateRole Content date role
type ContentDateRole struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *ContentDateRole) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // Nominal date of publication (of the content item or supporting resource)
  case "01":
		c.Body = `Publication date`

  // Date when a TV or radio program was / will be broadcast
  case "04":
		c.Body = `Broadcast date`

  // Date from which a content item or supporting resource may be referenced or used. The content is embargoed until this date
  case "14":
		c.Body = `From date`

  // Date until which a content item or supporting resource may be referenced or used
  case "15":
		c.Body = `Until date`

  // Date when a resource was last changed or updated
  case "17":
		c.Body = `Last updated`

  // Combines From date and Until date to define a period (both dates are inclusive). Use with for example dateformat 06
  case "24":
		c.Body = `From… until date`

  // Date from which a supporting resource is available for download. Note that this date also implies that it can be immediately displayed to the intended audience, unless a From date (code 14) is also supplied and is later than the Available from date
  case "27":
		c.Body = `Available from`

  // Date until which a supporting resource is available for download. Note that this date does not imply it must be removed from display to the intended audience on this date – for this, use Until date (code 15)
  case "28":
		c.Body = `Available until`

  // Start date referenced by the supporting resource, for example, the ‘earliest exam date’ for an official recommendation
  case "31":
		c.Body = `Associated start date`

  // End date referenced by the supporting resource, for example, the ‘latest exam date’ for an official recommendation
  case "32":
		c.Body = `Associated end date`
	default:
		return fmt.Errorf("undefined code for ContentDateRole has been passed, got [%s]", v)
	}
	return nil
}

// ContributorDateRole Person / organization date role
type ContributorDateRole struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *ContributorDateRole) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // Date of birth
  case "50":
		c.Body = `Date of birth`

  // Date of death
  case "51":
		c.Body = `Date of death`

  // (‘Floruit’). To date the height of or most productive period during a career
  case "56":
		c.Body = `Flourished around`
	default:
		return fmt.Errorf("undefined code for ContributorDateRole has been passed, got [%s]", v)
	}
	return nil
}

// ContributorPlaceRelator Contributor place relator
type ContributorPlaceRelator struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *ContributorPlaceRelator) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // To express unknown relationship types (for use when expressing legacy ONIX 2.1 data in ONIX 3.0)
  case "00":
		c.Body = `Associated with`

  // Born in
  case "01":
		c.Body = `Born in`

  // Died in
  case "02":
		c.Body = `Died in`

  // Formerly resided in
  case "03":
		c.Body = `Formerly resided in`

  // Currently resides in
  case "04":
		c.Body = `Currently resides in`

  // Educated in
  case "05":
		c.Body = `Educated in`

  // Worked in
  case "06":
		c.Body = `Worked in`

  // (‘Floruit’)
  case "07":
		c.Body = `Flourished in`

  // Or nationality. For use with country codes only
  case "08":
		c.Body = `Citizen of`

  // The place of legal registration of an organisation
  case "09":
		c.Body = `Registered in`

  // The place an organisation or part of an organisation is based or operates from
  case "10":
		c.Body = `Operating from`
	default:
		return fmt.Errorf("undefined code for ContributorPlaceRelator has been passed, got [%s]", v)
	}
	return nil
}

// ContributorRole Contributor role code
type ContributorRole struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *ContributorRole) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // Author of a textual work
  case "A01":
		c.Body = `By (author)`

  // With or as told to: ‘ghost’ or secondary author of a literary work (for clarity, should not be used for true ‘ghost’ authors who are not credited on the book and whose existence is secret)
  case "A02":
		c.Body = `With`

  // Writer of screenplay or script (film or video)
  case "A03":
		c.Body = `Screenplay by`

  // Writer of libretto (opera): see also A31
  case "A04":
		c.Body = `Libretto by`

  // Author of lyrics (song): see also A31
  case "A05":
		c.Body = `Lyrics by`

  // Composer of music
  case "A06":
		c.Body = `By (composer)`

  // Visual artist when named as the primary creator of, eg, a book of reproductions of artworks
  case "A07":
		c.Body = `By (artist)`

  // Photographer when named as the primary creator of, eg, a book of photographs
  case "A08":
		c.Body = `By (photographer)`

  // For example of editorial concept, of board game, etc
  case "A09":
		c.Body = `Created by`

  // From an idea by
  case "A10":
		c.Body = `From an idea by`

  // Designed by
  case "A11":
		c.Body = `Designed by`

  // Artist when named as the creator of artwork which illustrates a text, or the originator (sometimes ‘penciller’ for collaborative art) of the artwork of a graphic novel or comic book
  case "A12":
		c.Body = `Illustrated by`

  // Photographer when named as the creator of photographs which illustrate a text
  case "A13":
		c.Body = `Photographs by`

  // Author of text which accompanies art reproductions or photographs, or which is part of a graphic novel or comic book
  case "A14":
		c.Body = `Text by`

  // Author of preface
  case "A15":
		c.Body = `Preface by`

  // Author of prologue
  case "A16":
		c.Body = `Prologue by`

  // Author of summary
  case "A17":
		c.Body = `Summary by`

  // Author of supplement
  case "A18":
		c.Body = `Supplement by`

  // Author of afterword
  case "A19":
		c.Body = `Afterword by`

  // Author of notes or annotations: see also A29
  case "A20":
		c.Body = `Notes by`

  // Author of commentaries on the main text
  case "A21":
		c.Body = `Commentaries by`

  // Author of epilogue
  case "A22":
		c.Body = `Epilogue by`

  // Author of foreword
  case "A23":
		c.Body = `Foreword by`

  // Author of introduction: see also A29
  case "A24":
		c.Body = `Introduction by`

  // Author/compiler of footnotes
  case "A25":
		c.Body = `Footnotes by`

  // Author of memoir accompanying main text
  case "A26":
		c.Body = `Memoir by`

  // Person who carried out experiments reported in the text
  case "A27":
		c.Body = `Experiments by`

  // Author of introduction and notes: see also A20 and A24
  case "A29":
		c.Body = `Introduction and notes by`

  // Writer of computer programs ancillary to the text
  case "A30":
		c.Body = `Software written by`

  // Author of the textual content of a musical drama: see also A04 and A05
  case "A31":
		c.Body = `Book and lyrics by`

  // Author of additional contributions to the text
  case "A32":
		c.Body = `Contributions by`

  // Author of appendix
  case "A33":
		c.Body = `Appendix by`

  // Compiler of index
  case "A34":
		c.Body = `Index by`

  // Drawings by
  case "A35":
		c.Body = `Drawings by`

  // Use also for the cover artist of a graphic novel or comic book if named separately
  case "A36":
		c.Body = `Cover design or artwork by`

  // Responsible for preliminary work on which the work is based
  case "A37":
		c.Body = `Preliminary work by`

  // Author of the first edition (usually of a standard work) who is not an author of the current edition
  case "A38":
		c.Body = `Original author`

  // Maps drawn or otherwise contributed by
  case "A39":
		c.Body = `Maps by`

  // Use for secondary creators when separate persons are named as having respectively drawn and inked/colored/finished artwork, eg for a graphic novel or comic book. Use with A12 for ‘drawn by’. Use A40 for ‘finished by’, but prefer more specific codes A46 to A48 instead of A40 unless the more specific secondary roles are inappropriate, unclear or unavailable
  case "A40":
		c.Body = `Inked or colored by`

  // Designer or paper engineer of die-cuts, press-outs or of pop-ups in a pop-up book, who may be different from the illustrator
  case "A41":
		c.Body = `Paper engineering by`

  // Use where a standard work is being continued by somebody other than the original author
  case "A42":
		c.Body = `Continued by`

  // Interviewer
  case "A43":
		c.Body = `Interviewer`

  // Interviewee
  case "A44":
		c.Body = `Interviewee`

  // Writer of dialogue, captions in a comic book (following an outline by the primary writer)
  case "A45":
		c.Body = `Comic script by`

  // Renders final comic book line art based on work of the illustrator or penciller. Preferred to code A40
  case "A46":
		c.Body = `Inker`

  // Provides comic book color art and effects. Preferred to code A40
  case "A47":
		c.Body = `Colorist`

  // Creates comic book text balloons and other text elements (where this is a distinct role from script writer and/or illustrator)
  case "A48":
		c.Body = `Letterer`

  // Person or organization responsible for performing research on which the work is based. For use in ONIX 3.0 only
  case "A51":
		c.Body = `Research by`

  // Other type of primary creator not specified above
  case "A99":
		c.Body = `Other primary creator`

  // Edited by
  case "B01":
		c.Body = `Edited by`

  // Revised by
  case "B02":
		c.Body = `Revised by`

  // Retold by
  case "B03":
		c.Body = `Retold by`

  // Abridged by
  case "B04":
		c.Body = `Abridged by`

  // Adapted by
  case "B05":
		c.Body = `Adapted by`

  // Translated by
  case "B06":
		c.Body = `Translated by`

  // As told by
  case "B07":
		c.Body = `As told by`

  // This code applies where a translator has provided a commentary on issues relating to the translation. If the translator has also provided a commentary on the work itself, codes B06 and A21 should be used
  case "B08":
		c.Body = `Translated with commentary by`

  // Name of a series editor when the product belongs to a series
  case "B09":
		c.Body = `Series edited by`

  // Edited and translated by
  case "B10":
		c.Body = `Edited and translated by`

  // Editor-in-chief
  case "B11":
		c.Body = `Editor-in-chief`

  // Guest editor
  case "B12":
		c.Body = `Guest editor`

  // Volume editor
  case "B13":
		c.Body = `Volume editor`

  // Editorial board member
  case "B14":
		c.Body = `Editorial board member`

  // Editorial coordination by
  case "B15":
		c.Body = `Editorial coordination by`

  // Managing editor
  case "B16":
		c.Body = `Managing editor`

  // Usually the founder editor of a serial publication: Begruendet von
  case "B17":
		c.Body = `Founded by`

  // Prepared for publication by
  case "B18":
		c.Body = `Prepared for publication by`

  // Associate editor
  case "B19":
		c.Body = `Associate editor`

  // Use also for ‘advisory editor’, ‘series advisor’, ‘editorial consultant’ etc
  case "B20":
		c.Body = `Consultant editor`

  // General editor
  case "B21":
		c.Body = `General editor`

  // Dramatized by
  case "B22":
		c.Body = `Dramatized by`

  // In Europe, an expert editor who takes responsibility for the legal content of a collaborative law volume
  case "B23":
		c.Body = `General rapporteur`

  // An editor who is responsible for establishing the text used in an edition of a literary work, where this is recognised as a distinctive role (in Spain, ‘editor literario’)
  case "B24":
		c.Body = `Literary editor`

  // Arranged by (music)
  case "B25":
		c.Body = `Arranged by (music)`

  // Responsible for the technical accuracy and language, may also be involved in coordinating and preparing technical material for publication
  case "B26":
		c.Body = `Technical editor`

  // Thesis advisor or supervisor
  case "B27":
		c.Body = `Thesis advisor or supervisor`

  // Thesis examiner
  case "B28":
		c.Body = `Thesis examiner`

  // Responsible overall for the scientific content of the publication
  case "B29":
		c.Body = `Scientific editor`

  // For use in ONIX 3.0 only
  case "B30":
		c.Body = `Historical advisor`

  // Editor of the first edition (usually of a standard work) who is not an editor of the current edition. For use in ONIX 3.0 only
  case "B31":
		c.Body = `Original editor`

  // Other type of adaptation or editing not specified above
  case "B99":
		c.Body = `Other adaptation by`

  // For puzzles, directories, statistics, etc
  case "C01":
		c.Body = `Compiled by`

  // For textual material (eg for an anthology)
  case "C02":
		c.Body = `Selected by`

  // Eg for a collection of photographs etc
  case "C03":
		c.Body = `Non-text material selected by`

  // Eg for an exhibition
  case "C04":
		c.Body = `Curated by`

  // Other type of compilation not specified above
  case "C99":
		c.Body = `Other compilation by`

  // Producer
  case "D01":
		c.Body = `Producer`

  // Director
  case "D02":
		c.Body = `Director`

  // Conductor of a musical performance
  case "D03":
		c.Body = `Conductor`

  // Of a dance performance. For use in ONIX 3.0 only
  case "D04":
		c.Body = `Choreographer`

  // Other type of direction not specified above
  case "D99":
		c.Body = `Other direction by`

  // Performer in a dramatized production (including a voice actor in an audio production)
  case "E01":
		c.Body = `Actor`

  // Dancer
  case "E02":
		c.Body = `Dancer`

  // Where the narrator is a character in a dramatized production (including a voice actor in an audio production). For the ‘narrator’ of a non-dramatized audiobook, see code E07
  case "E03":
		c.Body = `Narrator`

  // Commentator
  case "E04":
		c.Body = `Commentator`

  // Singer etc
  case "E05":
		c.Body = `Vocal soloist`

  // Instrumental soloist
  case "E06":
		c.Body = `Instrumental soloist`

  // Reader of recorded text, as in an audiobook
  case "E07":
		c.Body = `Read by`

  // Name of a musical group in a performing role
  case "E08":
		c.Body = `Performed by (orchestra, band, ensemble)`

  // Of a speech, lecture etc
  case "E09":
		c.Body = `Speaker`

  // Introduces and links other contributors and material, eg within a documentary
  case "E10":
		c.Body = `Presenter`

  // Other type of performer not specified above: use for a recorded performance which does not fit a category above, eg a performance by a stand-up comedian
  case "E99":
		c.Body = `Performed by`

  // Cinematographer, etc
  case "F01":
		c.Body = `Filmed/photographed by`

  // Editor (film or video)
  case "F02":
		c.Body = `Editor (film or video)`

  // Other type of recording not specified above
  case "F99":
		c.Body = `Other recording by`

  // May be associated with any contributor role, and placement should therefore be controlled by contributor sequence numbering
  case "Z01":
		c.Body = `Assisted by`

  // Honored/dedicated to
  case "Z02":
		c.Body = `Honored/dedicated to`

  // For publication of laws, regulations, rulings etc. For use in ONIX 3.0 only
  case "Z03":
		c.Body = `Enacting jurisdiction`

  // Use with <UnnamedPersons> code 02 as a ‘flag’ to indicate the publication is anonymously peer-reviewed. For use in ONIX 3.0 only
  case "Z04":
		c.Body = `Peer reviewed`

  // For use ONLY with ‘et al’ or ‘Various’ within <UnnamedPersons>, where the roles of the multiple contributors vary
  case "Z98":
		c.Body = `(Various roles)`

  // Other creative responsibility not falling within A to F above
  case "Z99":
		c.Body = `Other`
	default:
		return fmt.Errorf("undefined code for ContributorRole has been passed, got [%s]", v)
	}
	return nil
}

// CopyrightOwnerIDType Name identifier type
type CopyrightOwnerIDType struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *CopyrightOwnerIDType) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // Note that <IDTypeName> is required with proprietary identifiers
  case "01":
		c.Body = `Proprietary`

  // DEPRECATED – use 01
  case "02":
		c.Body = `Proprietary`

  // Deutsche Nationalbibliothek publisher identifier
  case "03":
		c.Body = `DNB publisher identifier`

  // Börsenverein Verkehrsnummer
  case "04":
		c.Body = `Börsenverein Verkehrsnummer`

  // German ISBN Agency publisher identifier
  case "05":
		c.Body = `German ISBN Agency publisher identifier`

  // GS1 global location number (formerly EAN location number)
  case "06":
		c.Body = `GLN`

  // Book trade Standard Address Number – US, UK etc
  case "07":
		c.Body = `SAN`

  // MARC code list for organizations – see http://www.loc.gov/marc/organizations/orgshome.html
  case "08":
		c.Body = `MARC organization code`

  // Trading party identifier used in the Netherlands
  case "10":
		c.Body = `Centraal Boekhuis Relatie ID`

  // Flemish publisher code
  case "13":
		c.Body = `Fondscode Boekenbank`

  // Business Identity Code (Finland). See http://www.ytj.fi/ (in Finnish)
  case "15":
		c.Body = `Y-tunnus`

  // International Standard Name Identifier. A sixteen digit number. Usually presented with spaces or hyphens dividing the number into four groups of four digits, but in ONIX the spaces or hyphens should be omitted. See http://www.isni.org/
  case "16":
		c.Body = `ISNI`

  // Personennamendatei – person name authority file used by Deutsche Nationalbibliothek and in other German-speaking countries. See http://www.dnb.de/standardisierung/normdateien/pnd.htm (German) or http://www.dnb.de/eng/standardisierung/normdateien/pnd.htm (English). DEPRECATED in favor of the GND
  case "17":
		c.Body = `PND`

  // A control number assigned to a Library of Congress Control Number (LCCN) Name Authority / NACO record
  case "18":
		c.Body = `NACO`

  // Publisher identifier administered by Japanese ISBN Agency
  case "19":
		c.Body = `Japanese Publisher identifier`

  // Gemeinsame Körperschaftsdatei – Corporate Body Authority File in the German-speaking countries. See http://www.dnb.de/standardisierung/normdateien/gkd.htm (German) or http://www.dnb.de/eng/standardisierung/normdateien/gkd.htm (English). DEPRECATED in favor of the GND
  case "20":
		c.Body = `GKD`

  // Open Researcher and Contributor ID. A sixteen digit number. Usually presented with hyphens dividing the number into four groups of four digits, but in ONIX the hyphens should be omitted. See http://www.orcid.org/
  case "21":
		c.Body = `ORCID`

  // Publisher identifier maintained by the Chinese ISBN Agency (GAPP)
  case "22":
		c.Body = `GAPP Publisher Identifier`

  // Identifier for a business organization for VAT purposes, eg within the EU’s VIES system. See http://ec.europa.eu/taxation_customs/vies/faqvies.do for EU VAT ID formats, which vary from country to country. Generally these consist of a two-letter country code followed by the 8–12 digits of the national VAT ID. Some countries include one or two letters within their VAT ID. See http://en.wikipedia.org/wiki/VAT_identification_number for non-EU countries that maintain similar identifiers. Spaces, dashes etc should be omitted
  case "23":
		c.Body = `VAT Identity Number`

  // 4-digit business organization identifier controlled by the Japanese Publication Wholesalers Association
  case "24":
		c.Body = `JP Distribution Identifier`

  // Gemeinsame Normdatei – Joint Authority File in the German-speaking countries. See http://www.dnb.de/EN/gnd (English). Combines the PND, SWD and GKD into a single authority file, and should be used in preference
  case "25":
		c.Body = `GND`

  // Dunn and Bradstreet Universal Numbering System, see http://www.dnb.co.uk/dandb-duns-number
  case "26":
		c.Body = `DUNS`

  // Ringgold organizational identifier, see http://www.ringgold.com/identify.html
  case "27":
		c.Body = `Ringgold ID`

  // French Electre publisher identifier
  case "28":
		c.Body = `Identifiant Editeur Electre`

  // DOI used in EIDR party registry, for example ‘10.5237/C9F6-F41F’ (Sam Raimi). See http://eidr.org
  case "29":
		c.Body = `EIDR Party DOI`

  // French Electre imprint Identifier
  case "30":
		c.Body = `Identifiant Marque Electre`

  // Virtual Internet Authority File. <IDValue> should be a number. The URI form of the identifier can be created by prefixing the number with ‘https://viaf.org/viaf/’. See https://viaf.org
  case "31":
		c.Body = `VIAF ID`

  // DOI used in CrossRef’s Open Funder Registry list of academic research funding bodies, for example ‘10.13039/100004440’ (Wellcome Trust). See http://www.crossref.org/fundingdata/registry.html
  case "32":
		c.Body = `FundRef DOI`

  // Control number assigned to a Name Authority record by the Biblioteca Nacional de España
  case "33":
		c.Body = `BNE CN`

  // Numéro de la notice de personne BNF
  case "34":
		c.Body = `BNF Control Number`

  // Archival Resource Key, as a URL (including the address of the ARK resolver provided by eg a national library)
  case "35":
		c.Body = `ARK`

  // Nasjonalt autoritetsregister for navn – Norwegian national authority file for personal and corporate names. For use in ONIX 3.0 only
  case "36":
		c.Body = `Nasjonalt autoritetsregister`

  // Global Research Identifier Database ID (see https://www.grid.ac). For use in ONIX 3.0 only
  case "37":
		c.Body = `GRID`

  // Party ID from Identifiers and Standards for Higher Education and Research (fr: Identifiants et Référentiels pour l’enseignement supérieur et la recherche). For use on ONIX 3.0 only. See https://www.idref.fr
  case "38":
		c.Body = `IDRef`
	default:
		return fmt.Errorf("undefined code for CopyrightOwnerIDType has been passed, got [%s]", v)
	}
	return nil
}

// CopyrightType Rights type
type CopyrightType struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *CopyrightType) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // Text or image copyright (normally indicated by the © symbol). The default if no <CopyrightType> is specified
  case "C":
		c.Body = `Copyright`

  // Phonogram copyright or neighbouring right (normally indicated by the ℗ symbol)
  case "P":
		c.Body = `Phonogram right`

  // Sui generis database right
  case "D":
		c.Body = `Database right`
	default:
		return fmt.Errorf("undefined code for CopyrightType has been passed, got [%s]", v)
	}
	return nil
}

// CountryCode Country – based on ISO 3166-1
type CountryCode struct {
	Body []string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *CountryCode) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	codes := strings.Split(v, " ")
	tmpeCodes := []string{}
	for _, code := range codes {
		switch code {

		// Andorra
		case "AD":
			tmpeCodes = append(tmpeCodes, `Andorra`)

		// United Arab Emirates
		case "AE":
			tmpeCodes = append(tmpeCodes, `United Arab Emirates`)

		// Afghanistan
		case "AF":
			tmpeCodes = append(tmpeCodes, `Afghanistan`)

		// Antigua and Barbuda
		case "AG":
			tmpeCodes = append(tmpeCodes, `Antigua and Barbuda`)

		// Anguilla
		case "AI":
			tmpeCodes = append(tmpeCodes, `Anguilla`)

		// Albania
		case "AL":
			tmpeCodes = append(tmpeCodes, `Albania`)

		// Armenia
		case "AM":
			tmpeCodes = append(tmpeCodes, `Armenia`)

		// Deprecated – use BQ, CW and SX as appropriate
		case "AN":
			tmpeCodes = append(tmpeCodes, `Netherlands Antilles`)

		// Angola
		case "AO":
			tmpeCodes = append(tmpeCodes, `Angola`)

		// Antarctica
		case "AQ":
			tmpeCodes = append(tmpeCodes, `Antarctica`)

		// Argentina
		case "AR":
			tmpeCodes = append(tmpeCodes, `Argentina`)

		// American Samoa
		case "AS":
			tmpeCodes = append(tmpeCodes, `American Samoa`)

		// Austria
		case "AT":
			tmpeCodes = append(tmpeCodes, `Austria`)

		// Australia
		case "AU":
			tmpeCodes = append(tmpeCodes, `Australia`)

		// Aruba
		case "AW":
			tmpeCodes = append(tmpeCodes, `Aruba`)

		// Åland Islands
		case "AX":
			tmpeCodes = append(tmpeCodes, `Åland Islands`)

		// Azerbaijan
		case "AZ":
			tmpeCodes = append(tmpeCodes, `Azerbaijan`)

		// Bosnia and Herzegovina
		case "BA":
			tmpeCodes = append(tmpeCodes, `Bosnia and Herzegovina`)

		// Barbados
		case "BB":
			tmpeCodes = append(tmpeCodes, `Barbados`)

		// Bangladesh
		case "BD":
			tmpeCodes = append(tmpeCodes, `Bangladesh`)

		// Belgium
		case "BE":
			tmpeCodes = append(tmpeCodes, `Belgium`)

		// Burkina Faso
		case "BF":
			tmpeCodes = append(tmpeCodes, `Burkina Faso`)

		// Bulgaria
		case "BG":
			tmpeCodes = append(tmpeCodes, `Bulgaria`)

		// Bahrain
		case "BH":
			tmpeCodes = append(tmpeCodes, `Bahrain`)

		// Burundi
		case "BI":
			tmpeCodes = append(tmpeCodes, `Burundi`)

		// Benin
		case "BJ":
			tmpeCodes = append(tmpeCodes, `Benin`)

		// Saint Barthélemy
		case "BL":
			tmpeCodes = append(tmpeCodes, `Saint Barthélemy`)

		// Bermuda
		case "BM":
			tmpeCodes = append(tmpeCodes, `Bermuda`)

		// Brunei Darussalam
		case "BN":
			tmpeCodes = append(tmpeCodes, `Brunei Darussalam`)

		// Bolivia, Plurinational State of
		case "BO":
			tmpeCodes = append(tmpeCodes, `Bolivia, Plurinational State of`)

		// Bonaire, Sint Eustatius and Saba
		case "BQ":
			tmpeCodes = append(tmpeCodes, `Bonaire, Sint Eustatius and Saba`)

		// Brazil
		case "BR":
			tmpeCodes = append(tmpeCodes, `Brazil`)

		// Bahamas
		case "BS":
			tmpeCodes = append(tmpeCodes, `Bahamas`)

		// Bhutan
		case "BT":
			tmpeCodes = append(tmpeCodes, `Bhutan`)

		// Bouvet Island
		case "BV":
			tmpeCodes = append(tmpeCodes, `Bouvet Island`)

		// Botswana
		case "BW":
			tmpeCodes = append(tmpeCodes, `Botswana`)

		// Belarus
		case "BY":
			tmpeCodes = append(tmpeCodes, `Belarus`)

		// Belize
		case "BZ":
			tmpeCodes = append(tmpeCodes, `Belize`)

		// Canada
		case "CA":
			tmpeCodes = append(tmpeCodes, `Canada`)

		// Cocos (Keeling) Islands
		case "CC":
			tmpeCodes = append(tmpeCodes, `Cocos (Keeling) Islands`)

		// Congo, Democratic Republic of the
		case "CD":
			tmpeCodes = append(tmpeCodes, `Congo, Democratic Republic of the`)

		// Central African Republic
		case "CF":
			tmpeCodes = append(tmpeCodes, `Central African Republic`)

		// Congo
		case "CG":
			tmpeCodes = append(tmpeCodes, `Congo`)

		// Switzerland
		case "CH":
			tmpeCodes = append(tmpeCodes, `Switzerland`)

		// Cote d’Ivoire
		case "CI":
			tmpeCodes = append(tmpeCodes, `Cote d’Ivoire`)

		// Cook Islands
		case "CK":
			tmpeCodes = append(tmpeCodes, `Cook Islands`)

		// Chile
		case "CL":
			tmpeCodes = append(tmpeCodes, `Chile`)

		// Cameroon
		case "CM":
			tmpeCodes = append(tmpeCodes, `Cameroon`)

		// China
		case "CN":
			tmpeCodes = append(tmpeCodes, `China`)

		// Colombia
		case "CO":
			tmpeCodes = append(tmpeCodes, `Colombia`)

		// Costa Rica
		case "CR":
			tmpeCodes = append(tmpeCodes, `Costa Rica`)

		// DEPRECATED, replaced by ME – Montenegro and RS – Serbia
		case "CS":
			tmpeCodes = append(tmpeCodes, `Serbia and Montenegro`)

		// Cuba
		case "CU":
			tmpeCodes = append(tmpeCodes, `Cuba`)

		// Cabo Verde
		case "CV":
			tmpeCodes = append(tmpeCodes, `Cabo Verde`)

		// Curaçao
		case "CW":
			tmpeCodes = append(tmpeCodes, `Curaçao`)

		// Christmas Island
		case "CX":
			tmpeCodes = append(tmpeCodes, `Christmas Island`)

		// Cyprus
		case "CY":
			tmpeCodes = append(tmpeCodes, `Cyprus`)

		// Formerly Czech Republic
		case "CZ":
			tmpeCodes = append(tmpeCodes, `Czechia`)

		// Germany
		case "DE":
			tmpeCodes = append(tmpeCodes, `Germany`)

		// Djibouti
		case "DJ":
			tmpeCodes = append(tmpeCodes, `Djibouti`)

		// Denmark
		case "DK":
			tmpeCodes = append(tmpeCodes, `Denmark`)

		// Dominica
		case "DM":
			tmpeCodes = append(tmpeCodes, `Dominica`)

		// Dominican Republic
		case "DO":
			tmpeCodes = append(tmpeCodes, `Dominican Republic`)

		// Algeria
		case "DZ":
			tmpeCodes = append(tmpeCodes, `Algeria`)

		// Ecuador
		case "EC":
			tmpeCodes = append(tmpeCodes, `Ecuador`)

		// Estonia
		case "EE":
			tmpeCodes = append(tmpeCodes, `Estonia`)

		// Egypt
		case "EG":
			tmpeCodes = append(tmpeCodes, `Egypt`)

		// Western Sahara
		case "EH":
			tmpeCodes = append(tmpeCodes, `Western Sahara`)

		// Eritrea
		case "ER":
			tmpeCodes = append(tmpeCodes, `Eritrea`)

		// Spain
		case "ES":
			tmpeCodes = append(tmpeCodes, `Spain`)

		// Ethiopia
		case "ET":
			tmpeCodes = append(tmpeCodes, `Ethiopia`)

		// Finland
		case "FI":
			tmpeCodes = append(tmpeCodes, `Finland`)

		// Fiji
		case "FJ":
			tmpeCodes = append(tmpeCodes, `Fiji`)

		// Falkland Islands (Malvinas)
		case "FK":
			tmpeCodes = append(tmpeCodes, `Falkland Islands (Malvinas)`)

		// Micronesia, Federated States of
		case "FM":
			tmpeCodes = append(tmpeCodes, `Micronesia, Federated States of`)

		// Faroe Islands
		case "FO":
			tmpeCodes = append(tmpeCodes, `Faroe Islands`)

		// France
		case "FR":
			tmpeCodes = append(tmpeCodes, `France`)

		// Gabon
		case "GA":
			tmpeCodes = append(tmpeCodes, `Gabon`)

		// United Kingdom
		case "GB":
			tmpeCodes = append(tmpeCodes, `United Kingdom`)

		// Grenada
		case "GD":
			tmpeCodes = append(tmpeCodes, `Grenada`)

		// Georgia
		case "GE":
			tmpeCodes = append(tmpeCodes, `Georgia`)

		// French Guiana
		case "GF":
			tmpeCodes = append(tmpeCodes, `French Guiana`)

		// Guernsey
		case "GG":
			tmpeCodes = append(tmpeCodes, `Guernsey`)

		// Ghana
		case "GH":
			tmpeCodes = append(tmpeCodes, `Ghana`)

		// Gibraltar
		case "GI":
			tmpeCodes = append(tmpeCodes, `Gibraltar`)

		// Greenland
		case "GL":
			tmpeCodes = append(tmpeCodes, `Greenland`)

		// Gambia
		case "GM":
			tmpeCodes = append(tmpeCodes, `Gambia`)

		// Guinea
		case "GN":
			tmpeCodes = append(tmpeCodes, `Guinea`)

		// Guadeloupe
		case "GP":
			tmpeCodes = append(tmpeCodes, `Guadeloupe`)

		// Equatorial Guinea
		case "GQ":
			tmpeCodes = append(tmpeCodes, `Equatorial Guinea`)

		// Greece
		case "GR":
			tmpeCodes = append(tmpeCodes, `Greece`)

		// South Georgia and the South Sandwich Islands
		case "GS":
			tmpeCodes = append(tmpeCodes, `South Georgia and the South Sandwich Islands`)

		// Guatemala
		case "GT":
			tmpeCodes = append(tmpeCodes, `Guatemala`)

		// Guam
		case "GU":
			tmpeCodes = append(tmpeCodes, `Guam`)

		// Guinea-Bissau
		case "GW":
			tmpeCodes = append(tmpeCodes, `Guinea-Bissau`)

		// Guyana
		case "GY":
			tmpeCodes = append(tmpeCodes, `Guyana`)

		// Hong Kong
		case "HK":
			tmpeCodes = append(tmpeCodes, `Hong Kong`)

		// Heard Island and McDonald Islands
		case "HM":
			tmpeCodes = append(tmpeCodes, `Heard Island and McDonald Islands`)

		// Honduras
		case "HN":
			tmpeCodes = append(tmpeCodes, `Honduras`)

		// Croatia
		case "HR":
			tmpeCodes = append(tmpeCodes, `Croatia`)

		// Haiti
		case "HT":
			tmpeCodes = append(tmpeCodes, `Haiti`)

		// Hungary
		case "HU":
			tmpeCodes = append(tmpeCodes, `Hungary`)

		// Indonesia
		case "ID":
			tmpeCodes = append(tmpeCodes, `Indonesia`)

		// Ireland
		case "IE":
			tmpeCodes = append(tmpeCodes, `Ireland`)

		// Israel
		case "IL":
			tmpeCodes = append(tmpeCodes, `Israel`)

		// Isle of Man
		case "IM":
			tmpeCodes = append(tmpeCodes, `Isle of Man`)

		// India
		case "IN":
			tmpeCodes = append(tmpeCodes, `India`)

		// British Indian Ocean Territory
		case "IO":
			tmpeCodes = append(tmpeCodes, `British Indian Ocean Territory`)

		// Iraq
		case "IQ":
			tmpeCodes = append(tmpeCodes, `Iraq`)

		// Iran, Islamic Republic of
		case "IR":
			tmpeCodes = append(tmpeCodes, `Iran, Islamic Republic of`)

		// Iceland
		case "IS":
			tmpeCodes = append(tmpeCodes, `Iceland`)

		// Italy
		case "IT":
			tmpeCodes = append(tmpeCodes, `Italy`)

		// Jersey
		case "JE":
			tmpeCodes = append(tmpeCodes, `Jersey`)

		// Jamaica
		case "JM":
			tmpeCodes = append(tmpeCodes, `Jamaica`)

		// Jordan
		case "JO":
			tmpeCodes = append(tmpeCodes, `Jordan`)

		// Japan
		case "JP":
			tmpeCodes = append(tmpeCodes, `Japan`)

		// Kenya
		case "KE":
			tmpeCodes = append(tmpeCodes, `Kenya`)

		// Kyrgyzstan
		case "KG":
			tmpeCodes = append(tmpeCodes, `Kyrgyzstan`)

		// Cambodia
		case "KH":
			tmpeCodes = append(tmpeCodes, `Cambodia`)

		// Kiribati
		case "KI":
			tmpeCodes = append(tmpeCodes, `Kiribati`)

		// Comoros
		case "KM":
			tmpeCodes = append(tmpeCodes, `Comoros`)

		// Saint Kitts and Nevis
		case "KN":
			tmpeCodes = append(tmpeCodes, `Saint Kitts and Nevis`)

		// Korea, Democratic People’s Republic of
		case "KP":
			tmpeCodes = append(tmpeCodes, `Korea, Democratic People’s Republic of`)

		// Korea, Republic of
		case "KR":
			tmpeCodes = append(tmpeCodes, `Korea, Republic of`)

		// Kuwait
		case "KW":
			tmpeCodes = append(tmpeCodes, `Kuwait`)

		// Cayman Islands
		case "KY":
			tmpeCodes = append(tmpeCodes, `Cayman Islands`)

		// Kazakhstan
		case "KZ":
			tmpeCodes = append(tmpeCodes, `Kazakhstan`)

		// Lao People’s Democratic Republic
		case "LA":
			tmpeCodes = append(tmpeCodes, `Lao People’s Democratic Republic`)

		// Lebanon
		case "LB":
			tmpeCodes = append(tmpeCodes, `Lebanon`)

		// Saint Lucia
		case "LC":
			tmpeCodes = append(tmpeCodes, `Saint Lucia`)

		// Liechtenstein
		case "LI":
			tmpeCodes = append(tmpeCodes, `Liechtenstein`)

		// Sri Lanka
		case "LK":
			tmpeCodes = append(tmpeCodes, `Sri Lanka`)

		// Liberia
		case "LR":
			tmpeCodes = append(tmpeCodes, `Liberia`)

		// Lesotho
		case "LS":
			tmpeCodes = append(tmpeCodes, `Lesotho`)

		// Lithuania
		case "LT":
			tmpeCodes = append(tmpeCodes, `Lithuania`)

		// Luxembourg
		case "LU":
			tmpeCodes = append(tmpeCodes, `Luxembourg`)

		// Latvia
		case "LV":
			tmpeCodes = append(tmpeCodes, `Latvia`)

		// Libya
		case "LY":
			tmpeCodes = append(tmpeCodes, `Libya`)

		// Morocco
		case "MA":
			tmpeCodes = append(tmpeCodes, `Morocco`)

		// Monaco
		case "MC":
			tmpeCodes = append(tmpeCodes, `Monaco`)

		// Moldova, Republic of
		case "MD":
			tmpeCodes = append(tmpeCodes, `Moldova, Republic of`)

		// Montenegro
		case "ME":
			tmpeCodes = append(tmpeCodes, `Montenegro`)

		// Saint Martin (French part)
		case "MF":
			tmpeCodes = append(tmpeCodes, `Saint Martin (French part)`)

		// Madagascar
		case "MG":
			tmpeCodes = append(tmpeCodes, `Madagascar`)

		// Marshall Islands
		case "MH":
			tmpeCodes = append(tmpeCodes, `Marshall Islands`)

		// Formerly FYR Macedonia
		case "MK":
			tmpeCodes = append(tmpeCodes, `North Macedonia`)

		// Mali
		case "ML":
			tmpeCodes = append(tmpeCodes, `Mali`)

		// Myanmar
		case "MM":
			tmpeCodes = append(tmpeCodes, `Myanmar`)

		// Mongolia
		case "MN":
			tmpeCodes = append(tmpeCodes, `Mongolia`)

		// Macao
		case "MO":
			tmpeCodes = append(tmpeCodes, `Macao`)

		// Northern Mariana Islands
		case "MP":
			tmpeCodes = append(tmpeCodes, `Northern Mariana Islands`)

		// Martinique
		case "MQ":
			tmpeCodes = append(tmpeCodes, `Martinique`)

		// Mauritania
		case "MR":
			tmpeCodes = append(tmpeCodes, `Mauritania`)

		// Montserrat
		case "MS":
			tmpeCodes = append(tmpeCodes, `Montserrat`)

		// Malta
		case "MT":
			tmpeCodes = append(tmpeCodes, `Malta`)

		// Mauritius
		case "MU":
			tmpeCodes = append(tmpeCodes, `Mauritius`)

		// Maldives
		case "MV":
			tmpeCodes = append(tmpeCodes, `Maldives`)

		// Malawi
		case "MW":
			tmpeCodes = append(tmpeCodes, `Malawi`)

		// Mexico
		case "MX":
			tmpeCodes = append(tmpeCodes, `Mexico`)

		// Malaysia
		case "MY":
			tmpeCodes = append(tmpeCodes, `Malaysia`)

		// Mozambique
		case "MZ":
			tmpeCodes = append(tmpeCodes, `Mozambique`)

		// Namibia
		case "NA":
			tmpeCodes = append(tmpeCodes, `Namibia`)

		// New Caledonia
		case "NC":
			tmpeCodes = append(tmpeCodes, `New Caledonia`)

		// Niger
		case "NE":
			tmpeCodes = append(tmpeCodes, `Niger`)

		// Norfolk Island
		case "NF":
			tmpeCodes = append(tmpeCodes, `Norfolk Island`)

		// Nigeria
		case "NG":
			tmpeCodes = append(tmpeCodes, `Nigeria`)

		// Nicaragua
		case "NI":
			tmpeCodes = append(tmpeCodes, `Nicaragua`)

		// Netherlands
		case "NL":
			tmpeCodes = append(tmpeCodes, `Netherlands`)

		// Norway
		case "NO":
			tmpeCodes = append(tmpeCodes, `Norway`)

		// Nepal
		case "NP":
			tmpeCodes = append(tmpeCodes, `Nepal`)

		// Nauru
		case "NR":
			tmpeCodes = append(tmpeCodes, `Nauru`)

		// Niue
		case "NU":
			tmpeCodes = append(tmpeCodes, `Niue`)

		// New Zealand
		case "NZ":
			tmpeCodes = append(tmpeCodes, `New Zealand`)

		// Oman
		case "OM":
			tmpeCodes = append(tmpeCodes, `Oman`)

		// Panama
		case "PA":
			tmpeCodes = append(tmpeCodes, `Panama`)

		// Peru
		case "PE":
			tmpeCodes = append(tmpeCodes, `Peru`)

		// French Polynesia
		case "PF":
			tmpeCodes = append(tmpeCodes, `French Polynesia`)

		// Papua New Guinea
		case "PG":
			tmpeCodes = append(tmpeCodes, `Papua New Guinea`)

		// Philippines
		case "PH":
			tmpeCodes = append(tmpeCodes, `Philippines`)

		// Pakistan
		case "PK":
			tmpeCodes = append(tmpeCodes, `Pakistan`)

		// Poland
		case "PL":
			tmpeCodes = append(tmpeCodes, `Poland`)

		// Saint Pierre and Miquelon
		case "PM":
			tmpeCodes = append(tmpeCodes, `Saint Pierre and Miquelon`)

		// Pitcairn
		case "PN":
			tmpeCodes = append(tmpeCodes, `Pitcairn`)

		// Puerto Rico
		case "PR":
			tmpeCodes = append(tmpeCodes, `Puerto Rico`)

		// Palestine, State of
		case "PS":
			tmpeCodes = append(tmpeCodes, `Palestine, State of`)

		// Portugal
		case "PT":
			tmpeCodes = append(tmpeCodes, `Portugal`)

		// Palau
		case "PW":
			tmpeCodes = append(tmpeCodes, `Palau`)

		// Paraguay
		case "PY":
			tmpeCodes = append(tmpeCodes, `Paraguay`)

		// Qatar
		case "QA":
			tmpeCodes = append(tmpeCodes, `Qatar`)

		// Réunion
		case "RE":
			tmpeCodes = append(tmpeCodes, `Réunion`)

		// Romania
		case "RO":
			tmpeCodes = append(tmpeCodes, `Romania`)

		// Serbia
		case "RS":
			tmpeCodes = append(tmpeCodes, `Serbia`)

		// Russian Federation
		case "RU":
			tmpeCodes = append(tmpeCodes, `Russian Federation`)

		// Rwanda
		case "RW":
			tmpeCodes = append(tmpeCodes, `Rwanda`)

		// Saudi Arabia
		case "SA":
			tmpeCodes = append(tmpeCodes, `Saudi Arabia`)

		// Solomon Islands
		case "SB":
			tmpeCodes = append(tmpeCodes, `Solomon Islands`)

		// Seychelles
		case "SC":
			tmpeCodes = append(tmpeCodes, `Seychelles`)

		// Sudan
		case "SD":
			tmpeCodes = append(tmpeCodes, `Sudan`)

		// Sweden
		case "SE":
			tmpeCodes = append(tmpeCodes, `Sweden`)

		// Singapore
		case "SG":
			tmpeCodes = append(tmpeCodes, `Singapore`)

		// Saint Helena, Ascension and Tristan da Cunha
		case "SH":
			tmpeCodes = append(tmpeCodes, `Saint Helena, Ascension and Tristan da Cunha`)

		// Slovenia
		case "SI":
			tmpeCodes = append(tmpeCodes, `Slovenia`)

		// Svalbard and Jan Mayen
		case "SJ":
			tmpeCodes = append(tmpeCodes, `Svalbard and Jan Mayen`)

		// Slovakia
		case "SK":
			tmpeCodes = append(tmpeCodes, `Slovakia`)

		// Sierra Leone
		case "SL":
			tmpeCodes = append(tmpeCodes, `Sierra Leone`)

		// San Marino
		case "SM":
			tmpeCodes = append(tmpeCodes, `San Marino`)

		// Senegal
		case "SN":
			tmpeCodes = append(tmpeCodes, `Senegal`)

		// Somalia
		case "SO":
			tmpeCodes = append(tmpeCodes, `Somalia`)

		// Suriname
		case "SR":
			tmpeCodes = append(tmpeCodes, `Suriname`)

		// South Sudan
		case "SS":
			tmpeCodes = append(tmpeCodes, `South Sudan`)

		// Sao Tome and Principe
		case "ST":
			tmpeCodes = append(tmpeCodes, `Sao Tome and Principe`)

		// El Salvador
		case "SV":
			tmpeCodes = append(tmpeCodes, `El Salvador`)

		// Sint Maarten (Dutch part)
		case "SX":
			tmpeCodes = append(tmpeCodes, `Sint Maarten (Dutch part)`)

		// Syrian Arab Republic
		case "SY":
			tmpeCodes = append(tmpeCodes, `Syrian Arab Republic`)

		// Formerly known as Swaziland
		case "SZ":
			tmpeCodes = append(tmpeCodes, `Eswatini`)

		// Turks and Caicos Islands
		case "TC":
			tmpeCodes = append(tmpeCodes, `Turks and Caicos Islands`)

		// Chad
		case "TD":
			tmpeCodes = append(tmpeCodes, `Chad`)

		// French Southern Territories
		case "TF":
			tmpeCodes = append(tmpeCodes, `French Southern Territories`)

		// Togo
		case "TG":
			tmpeCodes = append(tmpeCodes, `Togo`)

		// Thailand
		case "TH":
			tmpeCodes = append(tmpeCodes, `Thailand`)

		// Tajikistan
		case "TJ":
			tmpeCodes = append(tmpeCodes, `Tajikistan`)

		// Tokelau
		case "TK":
			tmpeCodes = append(tmpeCodes, `Tokelau`)

		// Timor-Leste
		case "TL":
			tmpeCodes = append(tmpeCodes, `Timor-Leste`)

		// Turkmenistan
		case "TM":
			tmpeCodes = append(tmpeCodes, `Turkmenistan`)

		// Tunisia
		case "TN":
			tmpeCodes = append(tmpeCodes, `Tunisia`)

		// Tonga
		case "TO":
			tmpeCodes = append(tmpeCodes, `Tonga`)

		// Turkey
		case "TR":
			tmpeCodes = append(tmpeCodes, `Turkey`)

		// Trinidad and Tobago
		case "TT":
			tmpeCodes = append(tmpeCodes, `Trinidad and Tobago`)

		// Tuvalu
		case "TV":
			tmpeCodes = append(tmpeCodes, `Tuvalu`)

		// Taiwan, Province of China
		case "TW":
			tmpeCodes = append(tmpeCodes, `Taiwan, Province of China`)

		// Tanzania, United Republic of
		case "TZ":
			tmpeCodes = append(tmpeCodes, `Tanzania, United Republic of`)

		// Ukraine
		case "UA":
			tmpeCodes = append(tmpeCodes, `Ukraine`)

		// Uganda
		case "UG":
			tmpeCodes = append(tmpeCodes, `Uganda`)

		// United States Minor Outlying Islands
		case "UM":
			tmpeCodes = append(tmpeCodes, `United States Minor Outlying Islands`)

		// United States
		case "US":
			tmpeCodes = append(tmpeCodes, `United States`)

		// Uruguay
		case "UY":
			tmpeCodes = append(tmpeCodes, `Uruguay`)

		// Uzbekistan
		case "UZ":
			tmpeCodes = append(tmpeCodes, `Uzbekistan`)

		// Holy See (Vatican City State)
		case "VA":
			tmpeCodes = append(tmpeCodes, `Holy See (Vatican City State)`)

		// Saint Vincent and the Grenadines
		case "VC":
			tmpeCodes = append(tmpeCodes, `Saint Vincent and the Grenadines`)

		// Venezuela, Bolivarian Republic of
		case "VE":
			tmpeCodes = append(tmpeCodes, `Venezuela, Bolivarian Republic of`)

		// Virgin Islands, British
		case "VG":
			tmpeCodes = append(tmpeCodes, `Virgin Islands, British`)

		// Virgin Islands, US
		case "VI":
			tmpeCodes = append(tmpeCodes, `Virgin Islands, US`)

		// Viet Nam
		case "VN":
			tmpeCodes = append(tmpeCodes, `Viet Nam`)

		// Vanuatu
		case "VU":
			tmpeCodes = append(tmpeCodes, `Vanuatu`)

		// Wallis and Futuna
		case "WF":
			tmpeCodes = append(tmpeCodes, `Wallis and Futuna`)

		// Samoa
		case "WS":
			tmpeCodes = append(tmpeCodes, `Samoa`)

		// Yemen
		case "YE":
			tmpeCodes = append(tmpeCodes, `Yemen`)

		// Mayotte
		case "YT":
			tmpeCodes = append(tmpeCodes, `Mayotte`)

		// DEPRECATED, replaced by ME – Montenegro and RS – Serbia
		case "YU":
			tmpeCodes = append(tmpeCodes, `Yugoslavia`)

		// South Africa
		case "ZA":
			tmpeCodes = append(tmpeCodes, `South Africa`)

		// Zambia
		case "ZM":
			tmpeCodes = append(tmpeCodes, `Zambia`)

		// Zimbabwe
		case "ZW":
			tmpeCodes = append(tmpeCodes, `Zimbabwe`)
		default:
			return fmt.Errorf("undefined code for CountryCode has been passed, got [%s]", v)
		}
	}
	c.Body = tmpeCodes
	return nil
}

// CountryOfManufacture Country – based on ISO 3166-1
type CountryOfManufacture struct {
	Body []string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *CountryOfManufacture) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	codes := strings.Split(v, " ")
	tmpeCodes := []string{}
	for _, code := range codes {
		switch code {

		// Andorra
		case "AD":
			tmpeCodes = append(tmpeCodes, `Andorra`)

		// United Arab Emirates
		case "AE":
			tmpeCodes = append(tmpeCodes, `United Arab Emirates`)

		// Afghanistan
		case "AF":
			tmpeCodes = append(tmpeCodes, `Afghanistan`)

		// Antigua and Barbuda
		case "AG":
			tmpeCodes = append(tmpeCodes, `Antigua and Barbuda`)

		// Anguilla
		case "AI":
			tmpeCodes = append(tmpeCodes, `Anguilla`)

		// Albania
		case "AL":
			tmpeCodes = append(tmpeCodes, `Albania`)

		// Armenia
		case "AM":
			tmpeCodes = append(tmpeCodes, `Armenia`)

		// Deprecated – use BQ, CW and SX as appropriate
		case "AN":
			tmpeCodes = append(tmpeCodes, `Netherlands Antilles`)

		// Angola
		case "AO":
			tmpeCodes = append(tmpeCodes, `Angola`)

		// Antarctica
		case "AQ":
			tmpeCodes = append(tmpeCodes, `Antarctica`)

		// Argentina
		case "AR":
			tmpeCodes = append(tmpeCodes, `Argentina`)

		// American Samoa
		case "AS":
			tmpeCodes = append(tmpeCodes, `American Samoa`)

		// Austria
		case "AT":
			tmpeCodes = append(tmpeCodes, `Austria`)

		// Australia
		case "AU":
			tmpeCodes = append(tmpeCodes, `Australia`)

		// Aruba
		case "AW":
			tmpeCodes = append(tmpeCodes, `Aruba`)

		// Åland Islands
		case "AX":
			tmpeCodes = append(tmpeCodes, `Åland Islands`)

		// Azerbaijan
		case "AZ":
			tmpeCodes = append(tmpeCodes, `Azerbaijan`)

		// Bosnia and Herzegovina
		case "BA":
			tmpeCodes = append(tmpeCodes, `Bosnia and Herzegovina`)

		// Barbados
		case "BB":
			tmpeCodes = append(tmpeCodes, `Barbados`)

		// Bangladesh
		case "BD":
			tmpeCodes = append(tmpeCodes, `Bangladesh`)

		// Belgium
		case "BE":
			tmpeCodes = append(tmpeCodes, `Belgium`)

		// Burkina Faso
		case "BF":
			tmpeCodes = append(tmpeCodes, `Burkina Faso`)

		// Bulgaria
		case "BG":
			tmpeCodes = append(tmpeCodes, `Bulgaria`)

		// Bahrain
		case "BH":
			tmpeCodes = append(tmpeCodes, `Bahrain`)

		// Burundi
		case "BI":
			tmpeCodes = append(tmpeCodes, `Burundi`)

		// Benin
		case "BJ":
			tmpeCodes = append(tmpeCodes, `Benin`)

		// Saint Barthélemy
		case "BL":
			tmpeCodes = append(tmpeCodes, `Saint Barthélemy`)

		// Bermuda
		case "BM":
			tmpeCodes = append(tmpeCodes, `Bermuda`)

		// Brunei Darussalam
		case "BN":
			tmpeCodes = append(tmpeCodes, `Brunei Darussalam`)

		// Bolivia, Plurinational State of
		case "BO":
			tmpeCodes = append(tmpeCodes, `Bolivia, Plurinational State of`)

		// Bonaire, Sint Eustatius and Saba
		case "BQ":
			tmpeCodes = append(tmpeCodes, `Bonaire, Sint Eustatius and Saba`)

		// Brazil
		case "BR":
			tmpeCodes = append(tmpeCodes, `Brazil`)

		// Bahamas
		case "BS":
			tmpeCodes = append(tmpeCodes, `Bahamas`)

		// Bhutan
		case "BT":
			tmpeCodes = append(tmpeCodes, `Bhutan`)

		// Bouvet Island
		case "BV":
			tmpeCodes = append(tmpeCodes, `Bouvet Island`)

		// Botswana
		case "BW":
			tmpeCodes = append(tmpeCodes, `Botswana`)

		// Belarus
		case "BY":
			tmpeCodes = append(tmpeCodes, `Belarus`)

		// Belize
		case "BZ":
			tmpeCodes = append(tmpeCodes, `Belize`)

		// Canada
		case "CA":
			tmpeCodes = append(tmpeCodes, `Canada`)

		// Cocos (Keeling) Islands
		case "CC":
			tmpeCodes = append(tmpeCodes, `Cocos (Keeling) Islands`)

		// Congo, Democratic Republic of the
		case "CD":
			tmpeCodes = append(tmpeCodes, `Congo, Democratic Republic of the`)

		// Central African Republic
		case "CF":
			tmpeCodes = append(tmpeCodes, `Central African Republic`)

		// Congo
		case "CG":
			tmpeCodes = append(tmpeCodes, `Congo`)

		// Switzerland
		case "CH":
			tmpeCodes = append(tmpeCodes, `Switzerland`)

		// Cote d’Ivoire
		case "CI":
			tmpeCodes = append(tmpeCodes, `Cote d’Ivoire`)

		// Cook Islands
		case "CK":
			tmpeCodes = append(tmpeCodes, `Cook Islands`)

		// Chile
		case "CL":
			tmpeCodes = append(tmpeCodes, `Chile`)

		// Cameroon
		case "CM":
			tmpeCodes = append(tmpeCodes, `Cameroon`)

		// China
		case "CN":
			tmpeCodes = append(tmpeCodes, `China`)

		// Colombia
		case "CO":
			tmpeCodes = append(tmpeCodes, `Colombia`)

		// Costa Rica
		case "CR":
			tmpeCodes = append(tmpeCodes, `Costa Rica`)

		// DEPRECATED, replaced by ME – Montenegro and RS – Serbia
		case "CS":
			tmpeCodes = append(tmpeCodes, `Serbia and Montenegro`)

		// Cuba
		case "CU":
			tmpeCodes = append(tmpeCodes, `Cuba`)

		// Cabo Verde
		case "CV":
			tmpeCodes = append(tmpeCodes, `Cabo Verde`)

		// Curaçao
		case "CW":
			tmpeCodes = append(tmpeCodes, `Curaçao`)

		// Christmas Island
		case "CX":
			tmpeCodes = append(tmpeCodes, `Christmas Island`)

		// Cyprus
		case "CY":
			tmpeCodes = append(tmpeCodes, `Cyprus`)

		// Formerly Czech Republic
		case "CZ":
			tmpeCodes = append(tmpeCodes, `Czechia`)

		// Germany
		case "DE":
			tmpeCodes = append(tmpeCodes, `Germany`)

		// Djibouti
		case "DJ":
			tmpeCodes = append(tmpeCodes, `Djibouti`)

		// Denmark
		case "DK":
			tmpeCodes = append(tmpeCodes, `Denmark`)

		// Dominica
		case "DM":
			tmpeCodes = append(tmpeCodes, `Dominica`)

		// Dominican Republic
		case "DO":
			tmpeCodes = append(tmpeCodes, `Dominican Republic`)

		// Algeria
		case "DZ":
			tmpeCodes = append(tmpeCodes, `Algeria`)

		// Ecuador
		case "EC":
			tmpeCodes = append(tmpeCodes, `Ecuador`)

		// Estonia
		case "EE":
			tmpeCodes = append(tmpeCodes, `Estonia`)

		// Egypt
		case "EG":
			tmpeCodes = append(tmpeCodes, `Egypt`)

		// Western Sahara
		case "EH":
			tmpeCodes = append(tmpeCodes, `Western Sahara`)

		// Eritrea
		case "ER":
			tmpeCodes = append(tmpeCodes, `Eritrea`)

		// Spain
		case "ES":
			tmpeCodes = append(tmpeCodes, `Spain`)

		// Ethiopia
		case "ET":
			tmpeCodes = append(tmpeCodes, `Ethiopia`)

		// Finland
		case "FI":
			tmpeCodes = append(tmpeCodes, `Finland`)

		// Fiji
		case "FJ":
			tmpeCodes = append(tmpeCodes, `Fiji`)

		// Falkland Islands (Malvinas)
		case "FK":
			tmpeCodes = append(tmpeCodes, `Falkland Islands (Malvinas)`)

		// Micronesia, Federated States of
		case "FM":
			tmpeCodes = append(tmpeCodes, `Micronesia, Federated States of`)

		// Faroe Islands
		case "FO":
			tmpeCodes = append(tmpeCodes, `Faroe Islands`)

		// France
		case "FR":
			tmpeCodes = append(tmpeCodes, `France`)

		// Gabon
		case "GA":
			tmpeCodes = append(tmpeCodes, `Gabon`)

		// United Kingdom
		case "GB":
			tmpeCodes = append(tmpeCodes, `United Kingdom`)

		// Grenada
		case "GD":
			tmpeCodes = append(tmpeCodes, `Grenada`)

		// Georgia
		case "GE":
			tmpeCodes = append(tmpeCodes, `Georgia`)

		// French Guiana
		case "GF":
			tmpeCodes = append(tmpeCodes, `French Guiana`)

		// Guernsey
		case "GG":
			tmpeCodes = append(tmpeCodes, `Guernsey`)

		// Ghana
		case "GH":
			tmpeCodes = append(tmpeCodes, `Ghana`)

		// Gibraltar
		case "GI":
			tmpeCodes = append(tmpeCodes, `Gibraltar`)

		// Greenland
		case "GL":
			tmpeCodes = append(tmpeCodes, `Greenland`)

		// Gambia
		case "GM":
			tmpeCodes = append(tmpeCodes, `Gambia`)

		// Guinea
		case "GN":
			tmpeCodes = append(tmpeCodes, `Guinea`)

		// Guadeloupe
		case "GP":
			tmpeCodes = append(tmpeCodes, `Guadeloupe`)

		// Equatorial Guinea
		case "GQ":
			tmpeCodes = append(tmpeCodes, `Equatorial Guinea`)

		// Greece
		case "GR":
			tmpeCodes = append(tmpeCodes, `Greece`)

		// South Georgia and the South Sandwich Islands
		case "GS":
			tmpeCodes = append(tmpeCodes, `South Georgia and the South Sandwich Islands`)

		// Guatemala
		case "GT":
			tmpeCodes = append(tmpeCodes, `Guatemala`)

		// Guam
		case "GU":
			tmpeCodes = append(tmpeCodes, `Guam`)

		// Guinea-Bissau
		case "GW":
			tmpeCodes = append(tmpeCodes, `Guinea-Bissau`)

		// Guyana
		case "GY":
			tmpeCodes = append(tmpeCodes, `Guyana`)

		// Hong Kong
		case "HK":
			tmpeCodes = append(tmpeCodes, `Hong Kong`)

		// Heard Island and McDonald Islands
		case "HM":
			tmpeCodes = append(tmpeCodes, `Heard Island and McDonald Islands`)

		// Honduras
		case "HN":
			tmpeCodes = append(tmpeCodes, `Honduras`)

		// Croatia
		case "HR":
			tmpeCodes = append(tmpeCodes, `Croatia`)

		// Haiti
		case "HT":
			tmpeCodes = append(tmpeCodes, `Haiti`)

		// Hungary
		case "HU":
			tmpeCodes = append(tmpeCodes, `Hungary`)

		// Indonesia
		case "ID":
			tmpeCodes = append(tmpeCodes, `Indonesia`)

		// Ireland
		case "IE":
			tmpeCodes = append(tmpeCodes, `Ireland`)

		// Israel
		case "IL":
			tmpeCodes = append(tmpeCodes, `Israel`)

		// Isle of Man
		case "IM":
			tmpeCodes = append(tmpeCodes, `Isle of Man`)

		// India
		case "IN":
			tmpeCodes = append(tmpeCodes, `India`)

		// British Indian Ocean Territory
		case "IO":
			tmpeCodes = append(tmpeCodes, `British Indian Ocean Territory`)

		// Iraq
		case "IQ":
			tmpeCodes = append(tmpeCodes, `Iraq`)

		// Iran, Islamic Republic of
		case "IR":
			tmpeCodes = append(tmpeCodes, `Iran, Islamic Republic of`)

		// Iceland
		case "IS":
			tmpeCodes = append(tmpeCodes, `Iceland`)

		// Italy
		case "IT":
			tmpeCodes = append(tmpeCodes, `Italy`)

		// Jersey
		case "JE":
			tmpeCodes = append(tmpeCodes, `Jersey`)

		// Jamaica
		case "JM":
			tmpeCodes = append(tmpeCodes, `Jamaica`)

		// Jordan
		case "JO":
			tmpeCodes = append(tmpeCodes, `Jordan`)

		// Japan
		case "JP":
			tmpeCodes = append(tmpeCodes, `Japan`)

		// Kenya
		case "KE":
			tmpeCodes = append(tmpeCodes, `Kenya`)

		// Kyrgyzstan
		case "KG":
			tmpeCodes = append(tmpeCodes, `Kyrgyzstan`)

		// Cambodia
		case "KH":
			tmpeCodes = append(tmpeCodes, `Cambodia`)

		// Kiribati
		case "KI":
			tmpeCodes = append(tmpeCodes, `Kiribati`)

		// Comoros
		case "KM":
			tmpeCodes = append(tmpeCodes, `Comoros`)

		// Saint Kitts and Nevis
		case "KN":
			tmpeCodes = append(tmpeCodes, `Saint Kitts and Nevis`)

		// Korea, Democratic People’s Republic of
		case "KP":
			tmpeCodes = append(tmpeCodes, `Korea, Democratic People’s Republic of`)

		// Korea, Republic of
		case "KR":
			tmpeCodes = append(tmpeCodes, `Korea, Republic of`)

		// Kuwait
		case "KW":
			tmpeCodes = append(tmpeCodes, `Kuwait`)

		// Cayman Islands
		case "KY":
			tmpeCodes = append(tmpeCodes, `Cayman Islands`)

		// Kazakhstan
		case "KZ":
			tmpeCodes = append(tmpeCodes, `Kazakhstan`)

		// Lao People’s Democratic Republic
		case "LA":
			tmpeCodes = append(tmpeCodes, `Lao People’s Democratic Republic`)

		// Lebanon
		case "LB":
			tmpeCodes = append(tmpeCodes, `Lebanon`)

		// Saint Lucia
		case "LC":
			tmpeCodes = append(tmpeCodes, `Saint Lucia`)

		// Liechtenstein
		case "LI":
			tmpeCodes = append(tmpeCodes, `Liechtenstein`)

		// Sri Lanka
		case "LK":
			tmpeCodes = append(tmpeCodes, `Sri Lanka`)

		// Liberia
		case "LR":
			tmpeCodes = append(tmpeCodes, `Liberia`)

		// Lesotho
		case "LS":
			tmpeCodes = append(tmpeCodes, `Lesotho`)

		// Lithuania
		case "LT":
			tmpeCodes = append(tmpeCodes, `Lithuania`)

		// Luxembourg
		case "LU":
			tmpeCodes = append(tmpeCodes, `Luxembourg`)

		// Latvia
		case "LV":
			tmpeCodes = append(tmpeCodes, `Latvia`)

		// Libya
		case "LY":
			tmpeCodes = append(tmpeCodes, `Libya`)

		// Morocco
		case "MA":
			tmpeCodes = append(tmpeCodes, `Morocco`)

		// Monaco
		case "MC":
			tmpeCodes = append(tmpeCodes, `Monaco`)

		// Moldova, Republic of
		case "MD":
			tmpeCodes = append(tmpeCodes, `Moldova, Republic of`)

		// Montenegro
		case "ME":
			tmpeCodes = append(tmpeCodes, `Montenegro`)

		// Saint Martin (French part)
		case "MF":
			tmpeCodes = append(tmpeCodes, `Saint Martin (French part)`)

		// Madagascar
		case "MG":
			tmpeCodes = append(tmpeCodes, `Madagascar`)

		// Marshall Islands
		case "MH":
			tmpeCodes = append(tmpeCodes, `Marshall Islands`)

		// Formerly FYR Macedonia
		case "MK":
			tmpeCodes = append(tmpeCodes, `North Macedonia`)

		// Mali
		case "ML":
			tmpeCodes = append(tmpeCodes, `Mali`)

		// Myanmar
		case "MM":
			tmpeCodes = append(tmpeCodes, `Myanmar`)

		// Mongolia
		case "MN":
			tmpeCodes = append(tmpeCodes, `Mongolia`)

		// Macao
		case "MO":
			tmpeCodes = append(tmpeCodes, `Macao`)

		// Northern Mariana Islands
		case "MP":
			tmpeCodes = append(tmpeCodes, `Northern Mariana Islands`)

		// Martinique
		case "MQ":
			tmpeCodes = append(tmpeCodes, `Martinique`)

		// Mauritania
		case "MR":
			tmpeCodes = append(tmpeCodes, `Mauritania`)

		// Montserrat
		case "MS":
			tmpeCodes = append(tmpeCodes, `Montserrat`)

		// Malta
		case "MT":
			tmpeCodes = append(tmpeCodes, `Malta`)

		// Mauritius
		case "MU":
			tmpeCodes = append(tmpeCodes, `Mauritius`)

		// Maldives
		case "MV":
			tmpeCodes = append(tmpeCodes, `Maldives`)

		// Malawi
		case "MW":
			tmpeCodes = append(tmpeCodes, `Malawi`)

		// Mexico
		case "MX":
			tmpeCodes = append(tmpeCodes, `Mexico`)

		// Malaysia
		case "MY":
			tmpeCodes = append(tmpeCodes, `Malaysia`)

		// Mozambique
		case "MZ":
			tmpeCodes = append(tmpeCodes, `Mozambique`)

		// Namibia
		case "NA":
			tmpeCodes = append(tmpeCodes, `Namibia`)

		// New Caledonia
		case "NC":
			tmpeCodes = append(tmpeCodes, `New Caledonia`)

		// Niger
		case "NE":
			tmpeCodes = append(tmpeCodes, `Niger`)

		// Norfolk Island
		case "NF":
			tmpeCodes = append(tmpeCodes, `Norfolk Island`)

		// Nigeria
		case "NG":
			tmpeCodes = append(tmpeCodes, `Nigeria`)

		// Nicaragua
		case "NI":
			tmpeCodes = append(tmpeCodes, `Nicaragua`)

		// Netherlands
		case "NL":
			tmpeCodes = append(tmpeCodes, `Netherlands`)

		// Norway
		case "NO":
			tmpeCodes = append(tmpeCodes, `Norway`)

		// Nepal
		case "NP":
			tmpeCodes = append(tmpeCodes, `Nepal`)

		// Nauru
		case "NR":
			tmpeCodes = append(tmpeCodes, `Nauru`)

		// Niue
		case "NU":
			tmpeCodes = append(tmpeCodes, `Niue`)

		// New Zealand
		case "NZ":
			tmpeCodes = append(tmpeCodes, `New Zealand`)

		// Oman
		case "OM":
			tmpeCodes = append(tmpeCodes, `Oman`)

		// Panama
		case "PA":
			tmpeCodes = append(tmpeCodes, `Panama`)

		// Peru
		case "PE":
			tmpeCodes = append(tmpeCodes, `Peru`)

		// French Polynesia
		case "PF":
			tmpeCodes = append(tmpeCodes, `French Polynesia`)

		// Papua New Guinea
		case "PG":
			tmpeCodes = append(tmpeCodes, `Papua New Guinea`)

		// Philippines
		case "PH":
			tmpeCodes = append(tmpeCodes, `Philippines`)

		// Pakistan
		case "PK":
			tmpeCodes = append(tmpeCodes, `Pakistan`)

		// Poland
		case "PL":
			tmpeCodes = append(tmpeCodes, `Poland`)

		// Saint Pierre and Miquelon
		case "PM":
			tmpeCodes = append(tmpeCodes, `Saint Pierre and Miquelon`)

		// Pitcairn
		case "PN":
			tmpeCodes = append(tmpeCodes, `Pitcairn`)

		// Puerto Rico
		case "PR":
			tmpeCodes = append(tmpeCodes, `Puerto Rico`)

		// Palestine, State of
		case "PS":
			tmpeCodes = append(tmpeCodes, `Palestine, State of`)

		// Portugal
		case "PT":
			tmpeCodes = append(tmpeCodes, `Portugal`)

		// Palau
		case "PW":
			tmpeCodes = append(tmpeCodes, `Palau`)

		// Paraguay
		case "PY":
			tmpeCodes = append(tmpeCodes, `Paraguay`)

		// Qatar
		case "QA":
			tmpeCodes = append(tmpeCodes, `Qatar`)

		// Réunion
		case "RE":
			tmpeCodes = append(tmpeCodes, `Réunion`)

		// Romania
		case "RO":
			tmpeCodes = append(tmpeCodes, `Romania`)

		// Serbia
		case "RS":
			tmpeCodes = append(tmpeCodes, `Serbia`)

		// Russian Federation
		case "RU":
			tmpeCodes = append(tmpeCodes, `Russian Federation`)

		// Rwanda
		case "RW":
			tmpeCodes = append(tmpeCodes, `Rwanda`)

		// Saudi Arabia
		case "SA":
			tmpeCodes = append(tmpeCodes, `Saudi Arabia`)

		// Solomon Islands
		case "SB":
			tmpeCodes = append(tmpeCodes, `Solomon Islands`)

		// Seychelles
		case "SC":
			tmpeCodes = append(tmpeCodes, `Seychelles`)

		// Sudan
		case "SD":
			tmpeCodes = append(tmpeCodes, `Sudan`)

		// Sweden
		case "SE":
			tmpeCodes = append(tmpeCodes, `Sweden`)

		// Singapore
		case "SG":
			tmpeCodes = append(tmpeCodes, `Singapore`)

		// Saint Helena, Ascension and Tristan da Cunha
		case "SH":
			tmpeCodes = append(tmpeCodes, `Saint Helena, Ascension and Tristan da Cunha`)

		// Slovenia
		case "SI":
			tmpeCodes = append(tmpeCodes, `Slovenia`)

		// Svalbard and Jan Mayen
		case "SJ":
			tmpeCodes = append(tmpeCodes, `Svalbard and Jan Mayen`)

		// Slovakia
		case "SK":
			tmpeCodes = append(tmpeCodes, `Slovakia`)

		// Sierra Leone
		case "SL":
			tmpeCodes = append(tmpeCodes, `Sierra Leone`)

		// San Marino
		case "SM":
			tmpeCodes = append(tmpeCodes, `San Marino`)

		// Senegal
		case "SN":
			tmpeCodes = append(tmpeCodes, `Senegal`)

		// Somalia
		case "SO":
			tmpeCodes = append(tmpeCodes, `Somalia`)

		// Suriname
		case "SR":
			tmpeCodes = append(tmpeCodes, `Suriname`)

		// South Sudan
		case "SS":
			tmpeCodes = append(tmpeCodes, `South Sudan`)

		// Sao Tome and Principe
		case "ST":
			tmpeCodes = append(tmpeCodes, `Sao Tome and Principe`)

		// El Salvador
		case "SV":
			tmpeCodes = append(tmpeCodes, `El Salvador`)

		// Sint Maarten (Dutch part)
		case "SX":
			tmpeCodes = append(tmpeCodes, `Sint Maarten (Dutch part)`)

		// Syrian Arab Republic
		case "SY":
			tmpeCodes = append(tmpeCodes, `Syrian Arab Republic`)

		// Formerly known as Swaziland
		case "SZ":
			tmpeCodes = append(tmpeCodes, `Eswatini`)

		// Turks and Caicos Islands
		case "TC":
			tmpeCodes = append(tmpeCodes, `Turks and Caicos Islands`)

		// Chad
		case "TD":
			tmpeCodes = append(tmpeCodes, `Chad`)

		// French Southern Territories
		case "TF":
			tmpeCodes = append(tmpeCodes, `French Southern Territories`)

		// Togo
		case "TG":
			tmpeCodes = append(tmpeCodes, `Togo`)

		// Thailand
		case "TH":
			tmpeCodes = append(tmpeCodes, `Thailand`)

		// Tajikistan
		case "TJ":
			tmpeCodes = append(tmpeCodes, `Tajikistan`)

		// Tokelau
		case "TK":
			tmpeCodes = append(tmpeCodes, `Tokelau`)

		// Timor-Leste
		case "TL":
			tmpeCodes = append(tmpeCodes, `Timor-Leste`)

		// Turkmenistan
		case "TM":
			tmpeCodes = append(tmpeCodes, `Turkmenistan`)

		// Tunisia
		case "TN":
			tmpeCodes = append(tmpeCodes, `Tunisia`)

		// Tonga
		case "TO":
			tmpeCodes = append(tmpeCodes, `Tonga`)

		// Turkey
		case "TR":
			tmpeCodes = append(tmpeCodes, `Turkey`)

		// Trinidad and Tobago
		case "TT":
			tmpeCodes = append(tmpeCodes, `Trinidad and Tobago`)

		// Tuvalu
		case "TV":
			tmpeCodes = append(tmpeCodes, `Tuvalu`)

		// Taiwan, Province of China
		case "TW":
			tmpeCodes = append(tmpeCodes, `Taiwan, Province of China`)

		// Tanzania, United Republic of
		case "TZ":
			tmpeCodes = append(tmpeCodes, `Tanzania, United Republic of`)

		// Ukraine
		case "UA":
			tmpeCodes = append(tmpeCodes, `Ukraine`)

		// Uganda
		case "UG":
			tmpeCodes = append(tmpeCodes, `Uganda`)

		// United States Minor Outlying Islands
		case "UM":
			tmpeCodes = append(tmpeCodes, `United States Minor Outlying Islands`)

		// United States
		case "US":
			tmpeCodes = append(tmpeCodes, `United States`)

		// Uruguay
		case "UY":
			tmpeCodes = append(tmpeCodes, `Uruguay`)

		// Uzbekistan
		case "UZ":
			tmpeCodes = append(tmpeCodes, `Uzbekistan`)

		// Holy See (Vatican City State)
		case "VA":
			tmpeCodes = append(tmpeCodes, `Holy See (Vatican City State)`)

		// Saint Vincent and the Grenadines
		case "VC":
			tmpeCodes = append(tmpeCodes, `Saint Vincent and the Grenadines`)

		// Venezuela, Bolivarian Republic of
		case "VE":
			tmpeCodes = append(tmpeCodes, `Venezuela, Bolivarian Republic of`)

		// Virgin Islands, British
		case "VG":
			tmpeCodes = append(tmpeCodes, `Virgin Islands, British`)

		// Virgin Islands, US
		case "VI":
			tmpeCodes = append(tmpeCodes, `Virgin Islands, US`)

		// Viet Nam
		case "VN":
			tmpeCodes = append(tmpeCodes, `Viet Nam`)

		// Vanuatu
		case "VU":
			tmpeCodes = append(tmpeCodes, `Vanuatu`)

		// Wallis and Futuna
		case "WF":
			tmpeCodes = append(tmpeCodes, `Wallis and Futuna`)

		// Samoa
		case "WS":
			tmpeCodes = append(tmpeCodes, `Samoa`)

		// Yemen
		case "YE":
			tmpeCodes = append(tmpeCodes, `Yemen`)

		// Mayotte
		case "YT":
			tmpeCodes = append(tmpeCodes, `Mayotte`)

		// DEPRECATED, replaced by ME – Montenegro and RS – Serbia
		case "YU":
			tmpeCodes = append(tmpeCodes, `Yugoslavia`)

		// South Africa
		case "ZA":
			tmpeCodes = append(tmpeCodes, `South Africa`)

		// Zambia
		case "ZM":
			tmpeCodes = append(tmpeCodes, `Zambia`)

		// Zimbabwe
		case "ZW":
			tmpeCodes = append(tmpeCodes, `Zimbabwe`)
		default:
			return fmt.Errorf("undefined code for CountryOfManufacture has been passed, got [%s]", v)
		}
	}
	c.Body = tmpeCodes
	return nil
}

// CountryOfPublication Country – based on ISO 3166-1
type CountryOfPublication struct {
	Body []string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *CountryOfPublication) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	codes := strings.Split(v, " ")
	tmpeCodes := []string{}
	for _, code := range codes {
		switch code {

		// Andorra
		case "AD":
			tmpeCodes = append(tmpeCodes, `Andorra`)

		// United Arab Emirates
		case "AE":
			tmpeCodes = append(tmpeCodes, `United Arab Emirates`)

		// Afghanistan
		case "AF":
			tmpeCodes = append(tmpeCodes, `Afghanistan`)

		// Antigua and Barbuda
		case "AG":
			tmpeCodes = append(tmpeCodes, `Antigua and Barbuda`)

		// Anguilla
		case "AI":
			tmpeCodes = append(tmpeCodes, `Anguilla`)

		// Albania
		case "AL":
			tmpeCodes = append(tmpeCodes, `Albania`)

		// Armenia
		case "AM":
			tmpeCodes = append(tmpeCodes, `Armenia`)

		// Deprecated – use BQ, CW and SX as appropriate
		case "AN":
			tmpeCodes = append(tmpeCodes, `Netherlands Antilles`)

		// Angola
		case "AO":
			tmpeCodes = append(tmpeCodes, `Angola`)

		// Antarctica
		case "AQ":
			tmpeCodes = append(tmpeCodes, `Antarctica`)

		// Argentina
		case "AR":
			tmpeCodes = append(tmpeCodes, `Argentina`)

		// American Samoa
		case "AS":
			tmpeCodes = append(tmpeCodes, `American Samoa`)

		// Austria
		case "AT":
			tmpeCodes = append(tmpeCodes, `Austria`)

		// Australia
		case "AU":
			tmpeCodes = append(tmpeCodes, `Australia`)

		// Aruba
		case "AW":
			tmpeCodes = append(tmpeCodes, `Aruba`)

		// Åland Islands
		case "AX":
			tmpeCodes = append(tmpeCodes, `Åland Islands`)

		// Azerbaijan
		case "AZ":
			tmpeCodes = append(tmpeCodes, `Azerbaijan`)

		// Bosnia and Herzegovina
		case "BA":
			tmpeCodes = append(tmpeCodes, `Bosnia and Herzegovina`)

		// Barbados
		case "BB":
			tmpeCodes = append(tmpeCodes, `Barbados`)

		// Bangladesh
		case "BD":
			tmpeCodes = append(tmpeCodes, `Bangladesh`)

		// Belgium
		case "BE":
			tmpeCodes = append(tmpeCodes, `Belgium`)

		// Burkina Faso
		case "BF":
			tmpeCodes = append(tmpeCodes, `Burkina Faso`)

		// Bulgaria
		case "BG":
			tmpeCodes = append(tmpeCodes, `Bulgaria`)

		// Bahrain
		case "BH":
			tmpeCodes = append(tmpeCodes, `Bahrain`)

		// Burundi
		case "BI":
			tmpeCodes = append(tmpeCodes, `Burundi`)

		// Benin
		case "BJ":
			tmpeCodes = append(tmpeCodes, `Benin`)

		// Saint Barthélemy
		case "BL":
			tmpeCodes = append(tmpeCodes, `Saint Barthélemy`)

		// Bermuda
		case "BM":
			tmpeCodes = append(tmpeCodes, `Bermuda`)

		// Brunei Darussalam
		case "BN":
			tmpeCodes = append(tmpeCodes, `Brunei Darussalam`)

		// Bolivia, Plurinational State of
		case "BO":
			tmpeCodes = append(tmpeCodes, `Bolivia, Plurinational State of`)

		// Bonaire, Sint Eustatius and Saba
		case "BQ":
			tmpeCodes = append(tmpeCodes, `Bonaire, Sint Eustatius and Saba`)

		// Brazil
		case "BR":
			tmpeCodes = append(tmpeCodes, `Brazil`)

		// Bahamas
		case "BS":
			tmpeCodes = append(tmpeCodes, `Bahamas`)

		// Bhutan
		case "BT":
			tmpeCodes = append(tmpeCodes, `Bhutan`)

		// Bouvet Island
		case "BV":
			tmpeCodes = append(tmpeCodes, `Bouvet Island`)

		// Botswana
		case "BW":
			tmpeCodes = append(tmpeCodes, `Botswana`)

		// Belarus
		case "BY":
			tmpeCodes = append(tmpeCodes, `Belarus`)

		// Belize
		case "BZ":
			tmpeCodes = append(tmpeCodes, `Belize`)

		// Canada
		case "CA":
			tmpeCodes = append(tmpeCodes, `Canada`)

		// Cocos (Keeling) Islands
		case "CC":
			tmpeCodes = append(tmpeCodes, `Cocos (Keeling) Islands`)

		// Congo, Democratic Republic of the
		case "CD":
			tmpeCodes = append(tmpeCodes, `Congo, Democratic Republic of the`)

		// Central African Republic
		case "CF":
			tmpeCodes = append(tmpeCodes, `Central African Republic`)

		// Congo
		case "CG":
			tmpeCodes = append(tmpeCodes, `Congo`)

		// Switzerland
		case "CH":
			tmpeCodes = append(tmpeCodes, `Switzerland`)

		// Cote d’Ivoire
		case "CI":
			tmpeCodes = append(tmpeCodes, `Cote d’Ivoire`)

		// Cook Islands
		case "CK":
			tmpeCodes = append(tmpeCodes, `Cook Islands`)

		// Chile
		case "CL":
			tmpeCodes = append(tmpeCodes, `Chile`)

		// Cameroon
		case "CM":
			tmpeCodes = append(tmpeCodes, `Cameroon`)

		// China
		case "CN":
			tmpeCodes = append(tmpeCodes, `China`)

		// Colombia
		case "CO":
			tmpeCodes = append(tmpeCodes, `Colombia`)

		// Costa Rica
		case "CR":
			tmpeCodes = append(tmpeCodes, `Costa Rica`)

		// DEPRECATED, replaced by ME – Montenegro and RS – Serbia
		case "CS":
			tmpeCodes = append(tmpeCodes, `Serbia and Montenegro`)

		// Cuba
		case "CU":
			tmpeCodes = append(tmpeCodes, `Cuba`)

		// Cabo Verde
		case "CV":
			tmpeCodes = append(tmpeCodes, `Cabo Verde`)

		// Curaçao
		case "CW":
			tmpeCodes = append(tmpeCodes, `Curaçao`)

		// Christmas Island
		case "CX":
			tmpeCodes = append(tmpeCodes, `Christmas Island`)

		// Cyprus
		case "CY":
			tmpeCodes = append(tmpeCodes, `Cyprus`)

		// Formerly Czech Republic
		case "CZ":
			tmpeCodes = append(tmpeCodes, `Czechia`)

		// Germany
		case "DE":
			tmpeCodes = append(tmpeCodes, `Germany`)

		// Djibouti
		case "DJ":
			tmpeCodes = append(tmpeCodes, `Djibouti`)

		// Denmark
		case "DK":
			tmpeCodes = append(tmpeCodes, `Denmark`)

		// Dominica
		case "DM":
			tmpeCodes = append(tmpeCodes, `Dominica`)

		// Dominican Republic
		case "DO":
			tmpeCodes = append(tmpeCodes, `Dominican Republic`)

		// Algeria
		case "DZ":
			tmpeCodes = append(tmpeCodes, `Algeria`)

		// Ecuador
		case "EC":
			tmpeCodes = append(tmpeCodes, `Ecuador`)

		// Estonia
		case "EE":
			tmpeCodes = append(tmpeCodes, `Estonia`)

		// Egypt
		case "EG":
			tmpeCodes = append(tmpeCodes, `Egypt`)

		// Western Sahara
		case "EH":
			tmpeCodes = append(tmpeCodes, `Western Sahara`)

		// Eritrea
		case "ER":
			tmpeCodes = append(tmpeCodes, `Eritrea`)

		// Spain
		case "ES":
			tmpeCodes = append(tmpeCodes, `Spain`)

		// Ethiopia
		case "ET":
			tmpeCodes = append(tmpeCodes, `Ethiopia`)

		// Finland
		case "FI":
			tmpeCodes = append(tmpeCodes, `Finland`)

		// Fiji
		case "FJ":
			tmpeCodes = append(tmpeCodes, `Fiji`)

		// Falkland Islands (Malvinas)
		case "FK":
			tmpeCodes = append(tmpeCodes, `Falkland Islands (Malvinas)`)

		// Micronesia, Federated States of
		case "FM":
			tmpeCodes = append(tmpeCodes, `Micronesia, Federated States of`)

		// Faroe Islands
		case "FO":
			tmpeCodes = append(tmpeCodes, `Faroe Islands`)

		// France
		case "FR":
			tmpeCodes = append(tmpeCodes, `France`)

		// Gabon
		case "GA":
			tmpeCodes = append(tmpeCodes, `Gabon`)

		// United Kingdom
		case "GB":
			tmpeCodes = append(tmpeCodes, `United Kingdom`)

		// Grenada
		case "GD":
			tmpeCodes = append(tmpeCodes, `Grenada`)

		// Georgia
		case "GE":
			tmpeCodes = append(tmpeCodes, `Georgia`)

		// French Guiana
		case "GF":
			tmpeCodes = append(tmpeCodes, `French Guiana`)

		// Guernsey
		case "GG":
			tmpeCodes = append(tmpeCodes, `Guernsey`)

		// Ghana
		case "GH":
			tmpeCodes = append(tmpeCodes, `Ghana`)

		// Gibraltar
		case "GI":
			tmpeCodes = append(tmpeCodes, `Gibraltar`)

		// Greenland
		case "GL":
			tmpeCodes = append(tmpeCodes, `Greenland`)

		// Gambia
		case "GM":
			tmpeCodes = append(tmpeCodes, `Gambia`)

		// Guinea
		case "GN":
			tmpeCodes = append(tmpeCodes, `Guinea`)

		// Guadeloupe
		case "GP":
			tmpeCodes = append(tmpeCodes, `Guadeloupe`)

		// Equatorial Guinea
		case "GQ":
			tmpeCodes = append(tmpeCodes, `Equatorial Guinea`)

		// Greece
		case "GR":
			tmpeCodes = append(tmpeCodes, `Greece`)

		// South Georgia and the South Sandwich Islands
		case "GS":
			tmpeCodes = append(tmpeCodes, `South Georgia and the South Sandwich Islands`)

		// Guatemala
		case "GT":
			tmpeCodes = append(tmpeCodes, `Guatemala`)

		// Guam
		case "GU":
			tmpeCodes = append(tmpeCodes, `Guam`)

		// Guinea-Bissau
		case "GW":
			tmpeCodes = append(tmpeCodes, `Guinea-Bissau`)

		// Guyana
		case "GY":
			tmpeCodes = append(tmpeCodes, `Guyana`)

		// Hong Kong
		case "HK":
			tmpeCodes = append(tmpeCodes, `Hong Kong`)

		// Heard Island and McDonald Islands
		case "HM":
			tmpeCodes = append(tmpeCodes, `Heard Island and McDonald Islands`)

		// Honduras
		case "HN":
			tmpeCodes = append(tmpeCodes, `Honduras`)

		// Croatia
		case "HR":
			tmpeCodes = append(tmpeCodes, `Croatia`)

		// Haiti
		case "HT":
			tmpeCodes = append(tmpeCodes, `Haiti`)

		// Hungary
		case "HU":
			tmpeCodes = append(tmpeCodes, `Hungary`)

		// Indonesia
		case "ID":
			tmpeCodes = append(tmpeCodes, `Indonesia`)

		// Ireland
		case "IE":
			tmpeCodes = append(tmpeCodes, `Ireland`)

		// Israel
		case "IL":
			tmpeCodes = append(tmpeCodes, `Israel`)

		// Isle of Man
		case "IM":
			tmpeCodes = append(tmpeCodes, `Isle of Man`)

		// India
		case "IN":
			tmpeCodes = append(tmpeCodes, `India`)

		// British Indian Ocean Territory
		case "IO":
			tmpeCodes = append(tmpeCodes, `British Indian Ocean Territory`)

		// Iraq
		case "IQ":
			tmpeCodes = append(tmpeCodes, `Iraq`)

		// Iran, Islamic Republic of
		case "IR":
			tmpeCodes = append(tmpeCodes, `Iran, Islamic Republic of`)

		// Iceland
		case "IS":
			tmpeCodes = append(tmpeCodes, `Iceland`)

		// Italy
		case "IT":
			tmpeCodes = append(tmpeCodes, `Italy`)

		// Jersey
		case "JE":
			tmpeCodes = append(tmpeCodes, `Jersey`)

		// Jamaica
		case "JM":
			tmpeCodes = append(tmpeCodes, `Jamaica`)

		// Jordan
		case "JO":
			tmpeCodes = append(tmpeCodes, `Jordan`)

		// Japan
		case "JP":
			tmpeCodes = append(tmpeCodes, `Japan`)

		// Kenya
		case "KE":
			tmpeCodes = append(tmpeCodes, `Kenya`)

		// Kyrgyzstan
		case "KG":
			tmpeCodes = append(tmpeCodes, `Kyrgyzstan`)

		// Cambodia
		case "KH":
			tmpeCodes = append(tmpeCodes, `Cambodia`)

		// Kiribati
		case "KI":
			tmpeCodes = append(tmpeCodes, `Kiribati`)

		// Comoros
		case "KM":
			tmpeCodes = append(tmpeCodes, `Comoros`)

		// Saint Kitts and Nevis
		case "KN":
			tmpeCodes = append(tmpeCodes, `Saint Kitts and Nevis`)

		// Korea, Democratic People’s Republic of
		case "KP":
			tmpeCodes = append(tmpeCodes, `Korea, Democratic People’s Republic of`)

		// Korea, Republic of
		case "KR":
			tmpeCodes = append(tmpeCodes, `Korea, Republic of`)

		// Kuwait
		case "KW":
			tmpeCodes = append(tmpeCodes, `Kuwait`)

		// Cayman Islands
		case "KY":
			tmpeCodes = append(tmpeCodes, `Cayman Islands`)

		// Kazakhstan
		case "KZ":
			tmpeCodes = append(tmpeCodes, `Kazakhstan`)

		// Lao People’s Democratic Republic
		case "LA":
			tmpeCodes = append(tmpeCodes, `Lao People’s Democratic Republic`)

		// Lebanon
		case "LB":
			tmpeCodes = append(tmpeCodes, `Lebanon`)

		// Saint Lucia
		case "LC":
			tmpeCodes = append(tmpeCodes, `Saint Lucia`)

		// Liechtenstein
		case "LI":
			tmpeCodes = append(tmpeCodes, `Liechtenstein`)

		// Sri Lanka
		case "LK":
			tmpeCodes = append(tmpeCodes, `Sri Lanka`)

		// Liberia
		case "LR":
			tmpeCodes = append(tmpeCodes, `Liberia`)

		// Lesotho
		case "LS":
			tmpeCodes = append(tmpeCodes, `Lesotho`)

		// Lithuania
		case "LT":
			tmpeCodes = append(tmpeCodes, `Lithuania`)

		// Luxembourg
		case "LU":
			tmpeCodes = append(tmpeCodes, `Luxembourg`)

		// Latvia
		case "LV":
			tmpeCodes = append(tmpeCodes, `Latvia`)

		// Libya
		case "LY":
			tmpeCodes = append(tmpeCodes, `Libya`)

		// Morocco
		case "MA":
			tmpeCodes = append(tmpeCodes, `Morocco`)

		// Monaco
		case "MC":
			tmpeCodes = append(tmpeCodes, `Monaco`)

		// Moldova, Republic of
		case "MD":
			tmpeCodes = append(tmpeCodes, `Moldova, Republic of`)

		// Montenegro
		case "ME":
			tmpeCodes = append(tmpeCodes, `Montenegro`)

		// Saint Martin (French part)
		case "MF":
			tmpeCodes = append(tmpeCodes, `Saint Martin (French part)`)

		// Madagascar
		case "MG":
			tmpeCodes = append(tmpeCodes, `Madagascar`)

		// Marshall Islands
		case "MH":
			tmpeCodes = append(tmpeCodes, `Marshall Islands`)

		// Formerly FYR Macedonia
		case "MK":
			tmpeCodes = append(tmpeCodes, `North Macedonia`)

		// Mali
		case "ML":
			tmpeCodes = append(tmpeCodes, `Mali`)

		// Myanmar
		case "MM":
			tmpeCodes = append(tmpeCodes, `Myanmar`)

		// Mongolia
		case "MN":
			tmpeCodes = append(tmpeCodes, `Mongolia`)

		// Macao
		case "MO":
			tmpeCodes = append(tmpeCodes, `Macao`)

		// Northern Mariana Islands
		case "MP":
			tmpeCodes = append(tmpeCodes, `Northern Mariana Islands`)

		// Martinique
		case "MQ":
			tmpeCodes = append(tmpeCodes, `Martinique`)

		// Mauritania
		case "MR":
			tmpeCodes = append(tmpeCodes, `Mauritania`)

		// Montserrat
		case "MS":
			tmpeCodes = append(tmpeCodes, `Montserrat`)

		// Malta
		case "MT":
			tmpeCodes = append(tmpeCodes, `Malta`)

		// Mauritius
		case "MU":
			tmpeCodes = append(tmpeCodes, `Mauritius`)

		// Maldives
		case "MV":
			tmpeCodes = append(tmpeCodes, `Maldives`)

		// Malawi
		case "MW":
			tmpeCodes = append(tmpeCodes, `Malawi`)

		// Mexico
		case "MX":
			tmpeCodes = append(tmpeCodes, `Mexico`)

		// Malaysia
		case "MY":
			tmpeCodes = append(tmpeCodes, `Malaysia`)

		// Mozambique
		case "MZ":
			tmpeCodes = append(tmpeCodes, `Mozambique`)

		// Namibia
		case "NA":
			tmpeCodes = append(tmpeCodes, `Namibia`)

		// New Caledonia
		case "NC":
			tmpeCodes = append(tmpeCodes, `New Caledonia`)

		// Niger
		case "NE":
			tmpeCodes = append(tmpeCodes, `Niger`)

		// Norfolk Island
		case "NF":
			tmpeCodes = append(tmpeCodes, `Norfolk Island`)

		// Nigeria
		case "NG":
			tmpeCodes = append(tmpeCodes, `Nigeria`)

		// Nicaragua
		case "NI":
			tmpeCodes = append(tmpeCodes, `Nicaragua`)

		// Netherlands
		case "NL":
			tmpeCodes = append(tmpeCodes, `Netherlands`)

		// Norway
		case "NO":
			tmpeCodes = append(tmpeCodes, `Norway`)

		// Nepal
		case "NP":
			tmpeCodes = append(tmpeCodes, `Nepal`)

		// Nauru
		case "NR":
			tmpeCodes = append(tmpeCodes, `Nauru`)

		// Niue
		case "NU":
			tmpeCodes = append(tmpeCodes, `Niue`)

		// New Zealand
		case "NZ":
			tmpeCodes = append(tmpeCodes, `New Zealand`)

		// Oman
		case "OM":
			tmpeCodes = append(tmpeCodes, `Oman`)

		// Panama
		case "PA":
			tmpeCodes = append(tmpeCodes, `Panama`)

		// Peru
		case "PE":
			tmpeCodes = append(tmpeCodes, `Peru`)

		// French Polynesia
		case "PF":
			tmpeCodes = append(tmpeCodes, `French Polynesia`)

		// Papua New Guinea
		case "PG":
			tmpeCodes = append(tmpeCodes, `Papua New Guinea`)

		// Philippines
		case "PH":
			tmpeCodes = append(tmpeCodes, `Philippines`)

		// Pakistan
		case "PK":
			tmpeCodes = append(tmpeCodes, `Pakistan`)

		// Poland
		case "PL":
			tmpeCodes = append(tmpeCodes, `Poland`)

		// Saint Pierre and Miquelon
		case "PM":
			tmpeCodes = append(tmpeCodes, `Saint Pierre and Miquelon`)

		// Pitcairn
		case "PN":
			tmpeCodes = append(tmpeCodes, `Pitcairn`)

		// Puerto Rico
		case "PR":
			tmpeCodes = append(tmpeCodes, `Puerto Rico`)

		// Palestine, State of
		case "PS":
			tmpeCodes = append(tmpeCodes, `Palestine, State of`)

		// Portugal
		case "PT":
			tmpeCodes = append(tmpeCodes, `Portugal`)

		// Palau
		case "PW":
			tmpeCodes = append(tmpeCodes, `Palau`)

		// Paraguay
		case "PY":
			tmpeCodes = append(tmpeCodes, `Paraguay`)

		// Qatar
		case "QA":
			tmpeCodes = append(tmpeCodes, `Qatar`)

		// Réunion
		case "RE":
			tmpeCodes = append(tmpeCodes, `Réunion`)

		// Romania
		case "RO":
			tmpeCodes = append(tmpeCodes, `Romania`)

		// Serbia
		case "RS":
			tmpeCodes = append(tmpeCodes, `Serbia`)

		// Russian Federation
		case "RU":
			tmpeCodes = append(tmpeCodes, `Russian Federation`)

		// Rwanda
		case "RW":
			tmpeCodes = append(tmpeCodes, `Rwanda`)

		// Saudi Arabia
		case "SA":
			tmpeCodes = append(tmpeCodes, `Saudi Arabia`)

		// Solomon Islands
		case "SB":
			tmpeCodes = append(tmpeCodes, `Solomon Islands`)

		// Seychelles
		case "SC":
			tmpeCodes = append(tmpeCodes, `Seychelles`)

		// Sudan
		case "SD":
			tmpeCodes = append(tmpeCodes, `Sudan`)

		// Sweden
		case "SE":
			tmpeCodes = append(tmpeCodes, `Sweden`)

		// Singapore
		case "SG":
			tmpeCodes = append(tmpeCodes, `Singapore`)

		// Saint Helena, Ascension and Tristan da Cunha
		case "SH":
			tmpeCodes = append(tmpeCodes, `Saint Helena, Ascension and Tristan da Cunha`)

		// Slovenia
		case "SI":
			tmpeCodes = append(tmpeCodes, `Slovenia`)

		// Svalbard and Jan Mayen
		case "SJ":
			tmpeCodes = append(tmpeCodes, `Svalbard and Jan Mayen`)

		// Slovakia
		case "SK":
			tmpeCodes = append(tmpeCodes, `Slovakia`)

		// Sierra Leone
		case "SL":
			tmpeCodes = append(tmpeCodes, `Sierra Leone`)

		// San Marino
		case "SM":
			tmpeCodes = append(tmpeCodes, `San Marino`)

		// Senegal
		case "SN":
			tmpeCodes = append(tmpeCodes, `Senegal`)

		// Somalia
		case "SO":
			tmpeCodes = append(tmpeCodes, `Somalia`)

		// Suriname
		case "SR":
			tmpeCodes = append(tmpeCodes, `Suriname`)

		// South Sudan
		case "SS":
			tmpeCodes = append(tmpeCodes, `South Sudan`)

		// Sao Tome and Principe
		case "ST":
			tmpeCodes = append(tmpeCodes, `Sao Tome and Principe`)

		// El Salvador
		case "SV":
			tmpeCodes = append(tmpeCodes, `El Salvador`)

		// Sint Maarten (Dutch part)
		case "SX":
			tmpeCodes = append(tmpeCodes, `Sint Maarten (Dutch part)`)

		// Syrian Arab Republic
		case "SY":
			tmpeCodes = append(tmpeCodes, `Syrian Arab Republic`)

		// Formerly known as Swaziland
		case "SZ":
			tmpeCodes = append(tmpeCodes, `Eswatini`)

		// Turks and Caicos Islands
		case "TC":
			tmpeCodes = append(tmpeCodes, `Turks and Caicos Islands`)

		// Chad
		case "TD":
			tmpeCodes = append(tmpeCodes, `Chad`)

		// French Southern Territories
		case "TF":
			tmpeCodes = append(tmpeCodes, `French Southern Territories`)

		// Togo
		case "TG":
			tmpeCodes = append(tmpeCodes, `Togo`)

		// Thailand
		case "TH":
			tmpeCodes = append(tmpeCodes, `Thailand`)

		// Tajikistan
		case "TJ":
			tmpeCodes = append(tmpeCodes, `Tajikistan`)

		// Tokelau
		case "TK":
			tmpeCodes = append(tmpeCodes, `Tokelau`)

		// Timor-Leste
		case "TL":
			tmpeCodes = append(tmpeCodes, `Timor-Leste`)

		// Turkmenistan
		case "TM":
			tmpeCodes = append(tmpeCodes, `Turkmenistan`)

		// Tunisia
		case "TN":
			tmpeCodes = append(tmpeCodes, `Tunisia`)

		// Tonga
		case "TO":
			tmpeCodes = append(tmpeCodes, `Tonga`)

		// Turkey
		case "TR":
			tmpeCodes = append(tmpeCodes, `Turkey`)

		// Trinidad and Tobago
		case "TT":
			tmpeCodes = append(tmpeCodes, `Trinidad and Tobago`)

		// Tuvalu
		case "TV":
			tmpeCodes = append(tmpeCodes, `Tuvalu`)

		// Taiwan, Province of China
		case "TW":
			tmpeCodes = append(tmpeCodes, `Taiwan, Province of China`)

		// Tanzania, United Republic of
		case "TZ":
			tmpeCodes = append(tmpeCodes, `Tanzania, United Republic of`)

		// Ukraine
		case "UA":
			tmpeCodes = append(tmpeCodes, `Ukraine`)

		// Uganda
		case "UG":
			tmpeCodes = append(tmpeCodes, `Uganda`)

		// United States Minor Outlying Islands
		case "UM":
			tmpeCodes = append(tmpeCodes, `United States Minor Outlying Islands`)

		// United States
		case "US":
			tmpeCodes = append(tmpeCodes, `United States`)

		// Uruguay
		case "UY":
			tmpeCodes = append(tmpeCodes, `Uruguay`)

		// Uzbekistan
		case "UZ":
			tmpeCodes = append(tmpeCodes, `Uzbekistan`)

		// Holy See (Vatican City State)
		case "VA":
			tmpeCodes = append(tmpeCodes, `Holy See (Vatican City State)`)

		// Saint Vincent and the Grenadines
		case "VC":
			tmpeCodes = append(tmpeCodes, `Saint Vincent and the Grenadines`)

		// Venezuela, Bolivarian Republic of
		case "VE":
			tmpeCodes = append(tmpeCodes, `Venezuela, Bolivarian Republic of`)

		// Virgin Islands, British
		case "VG":
			tmpeCodes = append(tmpeCodes, `Virgin Islands, British`)

		// Virgin Islands, US
		case "VI":
			tmpeCodes = append(tmpeCodes, `Virgin Islands, US`)

		// Viet Nam
		case "VN":
			tmpeCodes = append(tmpeCodes, `Viet Nam`)

		// Vanuatu
		case "VU":
			tmpeCodes = append(tmpeCodes, `Vanuatu`)

		// Wallis and Futuna
		case "WF":
			tmpeCodes = append(tmpeCodes, `Wallis and Futuna`)

		// Samoa
		case "WS":
			tmpeCodes = append(tmpeCodes, `Samoa`)

		// Yemen
		case "YE":
			tmpeCodes = append(tmpeCodes, `Yemen`)

		// Mayotte
		case "YT":
			tmpeCodes = append(tmpeCodes, `Mayotte`)

		// DEPRECATED, replaced by ME – Montenegro and RS – Serbia
		case "YU":
			tmpeCodes = append(tmpeCodes, `Yugoslavia`)

		// South Africa
		case "ZA":
			tmpeCodes = append(tmpeCodes, `South Africa`)

		// Zambia
		case "ZM":
			tmpeCodes = append(tmpeCodes, `Zambia`)

		// Zimbabwe
		case "ZW":
			tmpeCodes = append(tmpeCodes, `Zimbabwe`)
		default:
			return fmt.Errorf("undefined code for CountryOfPublication has been passed, got [%s]", v)
		}
	}
	c.Body = tmpeCodes
	return nil
}

// CurrencyCode Currency code – based on ISO 4217
type CurrencyCode struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *CurrencyCode) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // United Arab Emirates
  case "AED":
		c.Body = `UAE Dirham`

  // Afghanistan. DEPRECATED, replaced by AFN
  case "AFA":
		c.Body = `Afghani`

  // Afghanistan (prices normally quoted as integers)
  case "AFN":
		c.Body = `Afghani`

  // Albania (prices normally quoted as integers)
  case "ALL":
		c.Body = `Lek`

  // Armenia (prices normally quoted as integers)
  case "AMD":
		c.Body = `Armenian Dram`

  // Curaçao, Sint Maarten
  case "ANG":
		c.Body = `Netherlands Antillian Guilder`

  // Angola
  case "AOA":
		c.Body = `Kwanza`

  // Argentina
  case "ARS":
		c.Body = `Argentine Peso`

  // Austria. Now replaced by the Euro (EUR). Deprecated – use only for historical prices that pre-date the introduction of the Euro
  case "ATS":
		c.Body = `Schilling`

  // Australia, Christmas Island, Cocos (Keeling) Islands, Heard Island and McDonald Islands, Kiribati, Nauru, Norfolk Island, Tuvalu
  case "AUD":
		c.Body = `Australian Dollar`

  // Aruba
  case "AWG":
		c.Body = `Aruban Florin`

  // Azerbaijan
  case "AZN":
		c.Body = `Azerbaijan Manat`

  // Bosnia and Herzegovina
  case "BAM":
		c.Body = `Convertible Marks`

  // Barbados
  case "BBD":
		c.Body = `Barbados Dollar`

  // Bangladesh
  case "BDT":
		c.Body = `Taka`

  // Belgium. Now replaced by the Euro (EUR). Deprecated – use only for historical prices that pre-date the introduction of the Euro
  case "BEF":
		c.Body = `Belgian Franc`

  // DEPRECATED, replaced by BGN
  case "BGL":
		c.Body = `Bulgarian Lev`

  // Bulgaria
  case "BGN":
		c.Body = `Bulgarian Lev`

  // Bahrain (prices normally quoted with 3 decimal places)
  case "BHD":
		c.Body = `Bahraini Dinar`

  // Burundi (prices normally quoted as integers)
  case "BIF":
		c.Body = `Burundi Franc`

  // Bermuda
  case "BMD":
		c.Body = `Bermudian Dollar`

  // Brunei Darussalam
  case "BND":
		c.Body = `Brunei Dollar`

  // Bolivia
  case "BOB":
		c.Body = `Boliviano`

  // Brazil
  case "BRL":
		c.Body = `Brazilian Real`

  // Bahamas
  case "BSD":
		c.Body = `Bahamian Dollar`

  // Bhutan
  case "BTN":
		c.Body = `Ngultrun`

  // Botswana
  case "BWP":
		c.Body = `Pula`

  // Belarus (prices normally quoted as integers). Deprecated – now replaced by new Belarussian Ruble (BYN): use only for historical prices that pre-date the introduction of the new Belarussian Ruble
  case "BYR":
		c.Body = `(Old) Belarussian Ruble`

  // Belarus
  case "BYN":
		c.Body = `Belarussian Ruble`

  // Belize
  case "BZD":
		c.Body = `Belize Dollar`

  // Canada
  case "CAD":
		c.Body = `Canadian Dollar`

  // Congo (Democratic Republic of the)
  case "CDF":
		c.Body = `Franc Congolais`

  // Switzerland, Liechtenstein
  case "CHF":
		c.Body = `Swiss Franc`

  // Chile (prices normally quoted as integers)
  case "CLP":
		c.Body = `Chilean Peso`

  // China
  case "CNY":
		c.Body = `Yuan Renminbi`

  // Colombia (prices normally quoted as integers)
  case "COP":
		c.Body = `Colombian Peso`

  // Costa Rica (prices normally quoted as integers)
  case "CRC":
		c.Body = `Costa Rican Colon`

  // Deprecated, replaced by RSD
  case "CSD":
		c.Body = `Serbian Dinar`

  // Cuba (alternative currency)
  case "CUC":
		c.Body = `Cuban Convertible Peso`

  // Cuba
  case "CUP":
		c.Body = `Cuban Peso`

  // Cabo Verde (prices normally quoted as integers)
  case "CVE":
		c.Body = `Cabo Verde Escudo`

  // Cyprus. Now replaced by the Euro (EUR). Deprecated – use only for historical prices that pre-date the introduction of the Euro
  case "CYP":
		c.Body = `Cyprus Pound`

  // Czechia
  case "CZK":
		c.Body = `Czech Koruna`

  // Germany. Now replaced by the Euro (EUR). Deprecated – use only for historical prices that pre-date the introduction of the Euro
  case "DEM":
		c.Body = `Mark`

  // Djibouti (prices normally quoted as integers)
  case "DJF":
		c.Body = `Djibouti Franc`

  // Denmark, Faroe Islands, Greenland
  case "DKK":
		c.Body = `Danish Krone`

  // Dominican Republic
  case "DOP":
		c.Body = `Dominican Peso`

  // Algeria
  case "DZD":
		c.Body = `Algerian Dinar`

  // Estonia.Now replaced by the Euro (EUR). Deprecated – use only for historical prices that pre-date the introduction of the Euro
  case "EEK":
		c.Body = `Kroon`

  // Egypt
  case "EGP":
		c.Body = `Egyptian Pound`

  // Eritrea
  case "ERN":
		c.Body = `Nakfa`

  // Spain. Now replaced by the Euro (EUR). Deprecated – use only for historical prices that pre-date the introduction of the Euro (prices normally quoted as integers)
  case "ESP":
		c.Body = `Peseta`

  // Ethiopia
  case "ETB":
		c.Body = `Ethiopian Birr`

  // Eurozone: Andorra, Austria, Belgium, Cyprus, Estonia, Finland, France, Fr Guiana, Fr S Territories, Germany, Greece, Guadeloupe, Holy See (Vatican City), Ireland, Italy, Latvia, Lithuania, Luxembourg, Martinique, Malta, Mayotte, Monaco, Montenegro, Netherlands, Portugal, Réunion, St Barthelemy, St Martin, St Pierre and Miquelon, San Marino, Slovakia, Slovenia, Spain
  case "EUR":
		c.Body = `Euro`

  // Finland. Now replaced by the Euro (EUR). Deprecated – use only for historical prices that pre-date the introduction of the Euro
  case "FIM":
		c.Body = `Markka`

  // Fiji
  case "FJD":
		c.Body = `Fiji Dollar`

  // Falkland Islands (Malvinas)
  case "FKP":
		c.Body = `Falkland Islands Pound`

  // France. Now replaced by the Euro (EUR). Deprecated – use only for historical prices that pre-date the introduction of the Euro
  case "FRF":
		c.Body = `Franc`

  // United Kingdom, Isle of Man, Channel Islands, South Georgia, South Sandwich Islands, British Indian Ocean Territory (de jure)
  case "GBP":
		c.Body = `Pound Sterling`

  // Georgia
  case "GEL":
		c.Body = `Lari`

  // Deprecated, replaced by GHS
  case "GHC":
		c.Body = `Ghana Cedi`

  // Ghana
  case "GHS":
		c.Body = `Ghana Cedi`

  // Gibraltar
  case "GIP":
		c.Body = `Gibraltar Pound`

  // Gambia
  case "GMD":
		c.Body = `Dalasi`

  // Guinea (prices normally quoted as integers)
  case "GNF":
		c.Body = `Guinean Franc`

  // Greece. Now replaced by the Euro (EUR). Deprecated – use only for historical prices that pre-date the introduction of the Euro
  case "GRD":
		c.Body = `Drachma`

  // Guatemala
  case "GTQ":
		c.Body = `Quetzal`

  // Now replaced by the CFA Franc BCEAO XOF use only for historical prices that pre-date use of the CFA Franc
  case "GWP":
		c.Body = `Guinea-Bissau Peso`

  // Guyana (prices normally quoted as integers)
  case "GYD":
		c.Body = `Guyana Dollar`

  // Hong Kong
  case "HKD":
		c.Body = `Hong Kong Dollar`

  // Honduras
  case "HNL":
		c.Body = `Lempira`

  // Croatia
  case "HRK":
		c.Body = `Kuna`

  // Haiti
  case "HTG":
		c.Body = `Gourde`

  // Hungary (prices normally quoted as integers)
  case "HUF":
		c.Body = `Forint`

  // Indonesia (prices normally quoted as integers)
  case "IDR":
		c.Body = `Rupiah`

  // Ireland. Now replaced by the Euro (EUR). Deprecated – use only for historical prices that pre-date the introduction of the Euro
  case "IEP":
		c.Body = `Punt`

  // Israel
  case "ILS":
		c.Body = `New Israeli Sheqel`

  // India, Bhutan (prices normally quoted as integers)
  case "INR":
		c.Body = `Indian Rupee`

  // Iraq (prices normally quoted as integers)
  case "IQD":
		c.Body = `Iraqi Dinar`

  // Iran (Islamic Republic of) (prices normally quoted as integers)
  case "IRR":
		c.Body = `Iranian Rial`

  // Iceland (prices normally quoted as integers)
  case "ISK":
		c.Body = `Iceland Krona`

  // Italy. Now replaced by the Euro (EUR). Deprecated – use only for historical prices that pre-date the introduction of the Euro (prices normally quoted as integers)
  case "ITL":
		c.Body = `Lira`

  // Jamaica
  case "JMD":
		c.Body = `Jamaican Dollar`

  // Jordan (prices normally quoted with 3 decimal places)
  case "JOD":
		c.Body = `Jordanian Dinar`

  // Japan (prices normally quoted as integers)
  case "JPY":
		c.Body = `Yen`

  // Kenya
  case "KES":
		c.Body = `Kenyan Shilling`

  // Kyrgyzstan
  case "KGS":
		c.Body = `Som`

  // Cambodia
  case "KHR":
		c.Body = `Riel`

  // Comoros (prices normally quoted as integers)
  case "KMF":
		c.Body = `Comorian Franc`

  // Korea (Democratic People’s Republic of) (prices normally quoted as integers)
  case "KPW":
		c.Body = `North Korean Won`

  // Korea (Republic of) (prices normally quoted as integers)
  case "KRW":
		c.Body = `Won`

  // Kuwait (prices normally quoted with 3 decimal places)
  case "KWD":
		c.Body = `Kuwaiti Dinar`

  // Cayman Islands
  case "KYD":
		c.Body = `Cayman Islands Dollar`

  // Kazakstan
  case "KZT":
		c.Body = `Tenge`

  // Lao People’s Democratic Republic (prices normally quoted as integers)
  case "LAK":
		c.Body = `Lao Kip`

  // Lebanon (prices normally quoted as integers)
  case "LBP":
		c.Body = `Lebanese Pound`

  // Sri Lanka
  case "LKR":
		c.Body = `Sri Lanka Rupee`

  // Liberia
  case "LRD":
		c.Body = `Liberian Dollar`

  // Lesotho
  case "LSL":
		c.Body = `Loti`

  // Lithuania. Now replaced by the Euro (EUR). Deprecated – use only for historical prices that pre-date the introduction of the Euro
  case "LTL":
		c.Body = `Litus`

  // Luxembourg. Now replaced by the Euro (EUR). Deprecated – use only for historical prices that pre-date the introduction of the Euro (prices normally quoted as integers)
  case "LUF":
		c.Body = `Luxembourg Franc`

  // Latvia. Now replaced by the Euro (EUR). Deprecated – use only for historical prices that pre-date the introduction of the Euro
  case "LVL":
		c.Body = `Latvian Lats`

  // Libyan Arab Jamahiriya (prices normally quoted with 3 decimal places)
  case "LYD":
		c.Body = `Libyan Dinar`

  // Morocco, Western Sahara
  case "MAD":
		c.Body = `Moroccan Dirham`

  // Moldova, Republic of
  case "MDL":
		c.Body = `Moldovan Leu`

  // Madagascar (prices normally quoted with 0 or 1 decimal place – 1 iraimbilanja = Ar0.2)
  case "MGA":
		c.Body = `Malagasy Ariary`

  // Now replaced by the Ariary (MGA) (prices normally quoted as integers)
  case "MGF":
		c.Body = `Malagasy Franc`

  // North Macedonia (formerly FYR Macedonia)
  case "MKD":
		c.Body = `Denar`

  // Myanmar (prices normally quoted as integers)
  case "MMK":
		c.Body = `Kyat`

  // Mongolia (prices normally quoted as integers)
  case "MNT":
		c.Body = `Tugrik`

  // Macau
  case "MOP":
		c.Body = `Pataca`

  // Mauritania (prices normally quoted with 0 or 1 decimal place – 1 khoums = UM0.2). Was interchangeable with MRU (New) Ouguiya at rate of 10:1 until June 2018. DEPRECATED, use MRU instead
  case "MRO":
		c.Body = `(Old) Ouguiya`

  // Mauritania (prices normally quoted with 0 or 1 decimal place – 1 khoums = UM0.2). Replaced MRO (old) Ouguiya at rate of 10:1 in June 2018. For use in ONIX 3.0 only
  case "MRU":
		c.Body = `Ouguiya`

  // Malta. Now replaced by the Euro (EUR). Deprecated – use only for historical prices that pre-date the introduction of the Euro
  case "MTL":
		c.Body = `Maltese Lira`

  // Mauritius (prices normally quoted as integers)
  case "MUR":
		c.Body = `Mauritius Rupee`

  // Maldives
  case "MVR":
		c.Body = `Rufiyaa`

  // Malawi
  case "MWK":
		c.Body = `Malawi Kwacha`

  // Mexico
  case "MXN":
		c.Body = `Mexican Peso`

  // Malaysia
  case "MYR":
		c.Body = `Malaysian Ringgit`

  // Mozambique
  case "MZN":
		c.Body = `Mozambique Metical`

  // Namibia
  case "NAD":
		c.Body = `Namibia Dollar`

  // Nigeria
  case "NGN":
		c.Body = `Naira`

  // Nicaragua
  case "NIO":
		c.Body = `Cordoba Oro`

  // Netherlands. Now replaced by the Euro (EUR). Deprecated – use only for historical prices that pre-date the introduction of the Euro
  case "NLG":
		c.Body = `Guilder`

  // Norway, Bouvet Island, Svalbard and Jan Mayen
  case "NOK":
		c.Body = `Norwegian Krone`

  // Nepal
  case "NPR":
		c.Body = `Nepalese Rupee`

  // New Zealand, Cook Islands, Niue, Pitcairn, Tokelau
  case "NZD":
		c.Body = `New Zealand Dollar`

  // Oman (prices normally quoted with 3 decimal places)
  case "OMR":
		c.Body = `Rial Omani`

  // Panama
  case "PAB":
		c.Body = `Balboa`

  // Peru (formerly Nuevo Sol)
  case "PEN":
		c.Body = `Sol`

  // Papua New Guinea
  case "PGK":
		c.Body = `Kina`

  // Philippines
  case "PHP":
		c.Body = `Philippine Peso`

  // Pakistan (prices normally quoted as integers)
  case "PKR":
		c.Body = `Pakistan Rupee`

  // Poland
  case "PLN":
		c.Body = `Złoty`

  // Portugal. Now replaced by the Euro (EUR). Deprecated – use only for historical prices that pre-date the introduction of the Euro
  case "PTE":
		c.Body = `Escudo`

  // Paraguay (prices normally quoted as integers)
  case "PYG":
		c.Body = `Guarani`

  // Qatar
  case "QAR":
		c.Body = `Qatari Rial`

  // Deprecated, replaced by RON
  case "ROL":
		c.Body = `Romanian Old Leu`

  // Romania
  case "RON":
		c.Body = `Romanian Leu`

  // Serbia (prices normally quoted as integers)
  case "RSD":
		c.Body = `Serbian Dinar`

  // Russian Federation
  case "RUB":
		c.Body = `Russian Ruble`

  // DEPRECATED, replaced by RUB
  case "RUR":
		c.Body = `Russian Ruble`

  // Rwanda (prices normally quoted as integers)
  case "RWF":
		c.Body = `Rwanda Franc`

  // Saudi Arabia
  case "SAR":
		c.Body = `Saudi Riyal`

  // Solomon Islands
  case "SBD":
		c.Body = `Solomon Islands Dollar`

  // Seychelles
  case "SCR":
		c.Body = `Seychelles Rupee`

  // Now replaced by the Sudanese Pound (SDG)
  case "SDD":
		c.Body = `Sudanese Dinar`

  // Sudan
  case "SDG":
		c.Body = `Sudanese Pound`

  // Sweden
  case "SEK":
		c.Body = `Swedish Krona`

  // Singapore
  case "SGD":
		c.Body = `Singapore Dollar`

  // Saint Helena
  case "SHP":
		c.Body = `Saint Helena Pound`

  // Slovenia. Now replaced by the Euro (EUR). Deprecated – use only for historical prices that pre-date the introduction of the Euro
  case "SIT":
		c.Body = `Tolar`

  // Slovakia. Now replaced by the Euro (EUR). Deprecated – use only for historical prices that pre-date the introduction of the Euro
  case "SKK":
		c.Body = `Slovak Koruna`

  // Sierra Leone (prices normally quoted as integers)
  case "SLL":
		c.Body = `Leone`

  // Somalia (prices normally quoted as integers)
  case "SOS":
		c.Body = `Somali Shilling`

  // Suriname
  case "SRD":
		c.Body = `Surinam Dollar`

  // DEPRECATED, replaced by SRD
  case "SRG":
		c.Body = `Suriname Guilder`

  // São Tome and Principe (prices normally quoted as integers). Was interchangeable with STN (New) Dobra at rate of 1000:1 until June 2018. DEPRECATED, use STN instead
  case "STD":
		c.Body = `(Old) Dobra`

  // São Tome and Principe. Replaced STD (old) Dobra at rate of 1000:1 in June 2018. For use in ONIX 3.0 only
  case "STN":
		c.Body = `Dobra`

  // El Salvador
  case "SVC":
		c.Body = `El Salvador Colon`

  // Syrian Arab Republic (prices normally quoted as integers)
  case "SYP":
		c.Body = `Syrian Pound`

  // Eswatini (formerly known as Swaziland)
  case "SZL":
		c.Body = `Lilangeni`

  // Thailand
  case "THB":
		c.Body = `Baht`

  // Tajikistan
  case "TJS":
		c.Body = `Somoni`

  // Deprecated, replaced by TMT (prices normally quoted as integers)
  case "TMM":
		c.Body = `Turkmenistan Manat`

  // Turkmenistan
  case "TMT":
		c.Body = `Turkmenistan New Manat`

  // Tunisia (prices normally quoted with 3 decimal places)
  case "TND":
		c.Body = `Tunisian Dinar`

  // Tonga
  case "TOP":
		c.Body = `Pa’anga`

  // Deprecated. Timor-Leste now uses the US Dollar
  case "TPE":
		c.Body = `Timor Escudo`

  // Deprecated, replaced by TRY (prices normally quoted as integers)
  case "TRL":
		c.Body = `Turkish Lira (old)`

  // Turkey, from 1 January 2005
  case "TRY":
		c.Body = `Turkish Lira`

  // Trinidad and Tobago
  case "TTD":
		c.Body = `Trinidad and Tobago Dollar`

  // Taiwan (Province of China)
  case "TWD":
		c.Body = `New Taiwan Dollar`

  // Tanzania (United Republic of) (prices normally quoted as integers)
  case "TZS":
		c.Body = `Tanzanian Shilling`

  // Ukraine
  case "UAH":
		c.Body = `Hryvnia`

  // Uganda (prices normally quoted as integers)
  case "UGX":
		c.Body = `Uganda Shilling`

  // United States, American Samoa, Bonaire, Sint Eustatius and Saba, British Indian Ocean Territory, Ecuador, El Salvador, Guam, Haiti, Marshall Is, Micronesia (Federated States of), Northern Mariana Is, Palau, Panama, Puerto Rico, Timor-Leste, Turks and Caicos Is, US Minor Outlying Is, Virgin Is (British), Virgin Is (US)
  case "USD":
		c.Body = `US Dollar`

  // Uruguay
  case "UYU":
		c.Body = `Peso Uruguayo`

  // Uzbekistan (prices normally quoted as integers)
  case "UZS":
		c.Body = `Uzbekistan Sum`

  // Deprecated, replaced by VEF
  case "VEB":
		c.Body = `Bolívar`

  // Venezuela (formerly Bolívar fuerte). Deprecated, replaced by VES
  case "VEF":
		c.Body = `Bolívar`

  // Venezuela (replaced VEF from August 2018 at rate of 100,000:1). For use in ONIX 3.0 only
  case "VES":
		c.Body = `Bolívar Soberano`

  // Viet Nam (prices normally quoted as integers)
  case "VND":
		c.Body = `Dong`

  // Vanuatu (prices normally quoted as integers)
  case "VUV":
		c.Body = `Vatu`

  // Samoa
  case "WST":
		c.Body = `Tala`

  // Cameroon, Central African Republic, Chad, Congo, Equatorial Guinea, Gabon (prices normally quoted as integers)
  case "XAF":
		c.Body = `CFA Franc BEAC`

  // Anguilla, Antigua and Barbuda, Dominica, Grenada, Montserrat, Saint Kitts and Nevis, Saint Lucia, Saint Vincent and the Grenadines
  case "XCD":
		c.Body = `East Caribbean Dollar`

  // Benin, Burkina Faso, Côte D’Ivoire, Guinea-Bissau, Mali, Niger, Senegal, Togo (prices normally quoted as integers)
  case "XOF":
		c.Body = `CFA Franc BCEAO`

  // French Polynesia, New Caledonia, Wallis and Futuna (prices normally quoted as integers)
  case "XPF":
		c.Body = `CFP Franc`

  // Yemen (prices normally quoted as integers)
  case "YER":
		c.Body = `Yemeni Rial`

  // DEPRECATED, replaced by CSD
  case "YUM":
		c.Body = `Yugoslavian Dinar`

  // South Africa, Namibia, Lesotho
  case "ZAR":
		c.Body = `Rand`

  // Zambia. Deprecated, replaced with ZMW (prices normally quoted as integers)
  case "ZMK":
		c.Body = `Kwacha`

  // Zambia
  case "ZMW":
		c.Body = `Zambian Kwacha`

  // Deprecated, replaced with ZWL (prices normally quoted as integers)
  case "ZWD":
		c.Body = `Zimbabwe Dollar`

  // Zimbabwe
  case "ZWL":
		c.Body = `Zimbabwe Dollar`
	default:
		return fmt.Errorf("undefined code for CurrencyCode has been passed, got [%s]", v)
	}
	return nil
}

// CurrencyZone Currency zone
type CurrencyZone struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *CurrencyZone) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // Countries that at the time being have the Euro as their national currency. Deprecated
  case "EUR":
		c.Body = `Eurozone`
	default:
		return fmt.Errorf("undefined code for CurrencyZone has been passed, got [%s]", v)
	}
	return nil
}

// DateFormat Date format
type DateFormat struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *DateFormat) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // Common Era year, month and day (default for most dates)
  case "00":
		c.Body = `YYYYMMDD`

  // Year and month
  case "01":
		c.Body = `YYYYMM`

  // Year and week number
  case "02":
		c.Body = `YYYYWW`

  // Year and quarter (Q = 1, 2, 3, 4, with 1 = Jan to Mar)
  case "03":
		c.Body = `YYYYQ`

  // Year and season (S = 1, 2, 3, 4, with 1 = ‘Spring’)
  case "04":
		c.Body = `YYYYS`

  // Year (default for some dates)
  case "05":
		c.Body = `YYYY`

  // Spread of exact dates
  case "06":
		c.Body = `YYYYMMDDYYYYMMDD`

  // Spread of months
  case "07":
		c.Body = `YYYYMMYYYYMM`

  // Spread of week numbers
  case "08":
		c.Body = `YYYYWWYYYYWW`

  // Spread of quarters
  case "09":
		c.Body = `YYYYQYYYYQ`

  // Spread of seasons
  case "10":
		c.Body = `YYYYSYYYYS`

  // Spread of years
  case "11":
		c.Body = `YYYYYYYY`

  // For complex, approximate or uncertain dates, or dates BCE
  case "12":
		c.Body = `Text string`

  // Exact time. Use ONLY when exact times with hour/minute precision are relevant. By default, time is local. Alternatively, the time may be suffixed with an optional ‘Z’ for UTC times, or with ‘+’ or ‘-’ and an hhmm timezone offset from UTC. Times without a timezone are ‘rolling’ local times, times qualified with a timezone (using Z, + or -) specify a particular instant in time
  case "13":
		c.Body = `YYYYMMDDThhmm`

  // Exact time. Use ONLY when exact times with second precision are relevant. By default, time is local. Alternatively, the time may be suffixed with an optional ‘Z’ for UTC times, or with ‘+’ or ‘-’ and an hhmm timezone offset from UTC. Times without a timezone are ‘rolling’ local times, times qualified with a timezone (using Z, + or -) specify a particular instant in time
  case "14":
		c.Body = `YYYYMMDDThhmmss`

  // Year month day (Hijri calendar)
  case "20":
		c.Body = `YYYYMMDD (H)`

  // Year and month (Hijri calendar)
  case "21":
		c.Body = `YYYYMM (H)`

  // Year (Hijri calendar)
  case "25":
		c.Body = `YYYY (H)`

  // For complex, approximate or uncertain dates (Hijri calendar), text would usually be in Arabic script
  case "32":
		c.Body = `Text string (H)`
	default:
		return fmt.Errorf("undefined code for DateFormat has been passed, got [%s]", v)
	}
	return nil
}

// DefaultCurrencyCode Currency code – based on ISO 4217
type DefaultCurrencyCode struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *DefaultCurrencyCode) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // United Arab Emirates
  case "AED":
		c.Body = `UAE Dirham`

  // Afghanistan. DEPRECATED, replaced by AFN
  case "AFA":
		c.Body = `Afghani`

  // Afghanistan (prices normally quoted as integers)
  case "AFN":
		c.Body = `Afghani`

  // Albania (prices normally quoted as integers)
  case "ALL":
		c.Body = `Lek`

  // Armenia (prices normally quoted as integers)
  case "AMD":
		c.Body = `Armenian Dram`

  // Curaçao, Sint Maarten
  case "ANG":
		c.Body = `Netherlands Antillian Guilder`

  // Angola
  case "AOA":
		c.Body = `Kwanza`

  // Argentina
  case "ARS":
		c.Body = `Argentine Peso`

  // Austria. Now replaced by the Euro (EUR). Deprecated – use only for historical prices that pre-date the introduction of the Euro
  case "ATS":
		c.Body = `Schilling`

  // Australia, Christmas Island, Cocos (Keeling) Islands, Heard Island and McDonald Islands, Kiribati, Nauru, Norfolk Island, Tuvalu
  case "AUD":
		c.Body = `Australian Dollar`

  // Aruba
  case "AWG":
		c.Body = `Aruban Florin`

  // Azerbaijan
  case "AZN":
		c.Body = `Azerbaijan Manat`

  // Bosnia and Herzegovina
  case "BAM":
		c.Body = `Convertible Marks`

  // Barbados
  case "BBD":
		c.Body = `Barbados Dollar`

  // Bangladesh
  case "BDT":
		c.Body = `Taka`

  // Belgium. Now replaced by the Euro (EUR). Deprecated – use only for historical prices that pre-date the introduction of the Euro
  case "BEF":
		c.Body = `Belgian Franc`

  // DEPRECATED, replaced by BGN
  case "BGL":
		c.Body = `Bulgarian Lev`

  // Bulgaria
  case "BGN":
		c.Body = `Bulgarian Lev`

  // Bahrain (prices normally quoted with 3 decimal places)
  case "BHD":
		c.Body = `Bahraini Dinar`

  // Burundi (prices normally quoted as integers)
  case "BIF":
		c.Body = `Burundi Franc`

  // Bermuda
  case "BMD":
		c.Body = `Bermudian Dollar`

  // Brunei Darussalam
  case "BND":
		c.Body = `Brunei Dollar`

  // Bolivia
  case "BOB":
		c.Body = `Boliviano`

  // Brazil
  case "BRL":
		c.Body = `Brazilian Real`

  // Bahamas
  case "BSD":
		c.Body = `Bahamian Dollar`

  // Bhutan
  case "BTN":
		c.Body = `Ngultrun`

  // Botswana
  case "BWP":
		c.Body = `Pula`

  // Belarus (prices normally quoted as integers). Deprecated – now replaced by new Belarussian Ruble (BYN): use only for historical prices that pre-date the introduction of the new Belarussian Ruble
  case "BYR":
		c.Body = `(Old) Belarussian Ruble`

  // Belarus
  case "BYN":
		c.Body = `Belarussian Ruble`

  // Belize
  case "BZD":
		c.Body = `Belize Dollar`

  // Canada
  case "CAD":
		c.Body = `Canadian Dollar`

  // Congo (Democratic Republic of the)
  case "CDF":
		c.Body = `Franc Congolais`

  // Switzerland, Liechtenstein
  case "CHF":
		c.Body = `Swiss Franc`

  // Chile (prices normally quoted as integers)
  case "CLP":
		c.Body = `Chilean Peso`

  // China
  case "CNY":
		c.Body = `Yuan Renminbi`

  // Colombia (prices normally quoted as integers)
  case "COP":
		c.Body = `Colombian Peso`

  // Costa Rica (prices normally quoted as integers)
  case "CRC":
		c.Body = `Costa Rican Colon`

  // Deprecated, replaced by RSD
  case "CSD":
		c.Body = `Serbian Dinar`

  // Cuba (alternative currency)
  case "CUC":
		c.Body = `Cuban Convertible Peso`

  // Cuba
  case "CUP":
		c.Body = `Cuban Peso`

  // Cabo Verde (prices normally quoted as integers)
  case "CVE":
		c.Body = `Cabo Verde Escudo`

  // Cyprus. Now replaced by the Euro (EUR). Deprecated – use only for historical prices that pre-date the introduction of the Euro
  case "CYP":
		c.Body = `Cyprus Pound`

  // Czechia
  case "CZK":
		c.Body = `Czech Koruna`

  // Germany. Now replaced by the Euro (EUR). Deprecated – use only for historical prices that pre-date the introduction of the Euro
  case "DEM":
		c.Body = `Mark`

  // Djibouti (prices normally quoted as integers)
  case "DJF":
		c.Body = `Djibouti Franc`

  // Denmark, Faroe Islands, Greenland
  case "DKK":
		c.Body = `Danish Krone`

  // Dominican Republic
  case "DOP":
		c.Body = `Dominican Peso`

  // Algeria
  case "DZD":
		c.Body = `Algerian Dinar`

  // Estonia.Now replaced by the Euro (EUR). Deprecated – use only for historical prices that pre-date the introduction of the Euro
  case "EEK":
		c.Body = `Kroon`

  // Egypt
  case "EGP":
		c.Body = `Egyptian Pound`

  // Eritrea
  case "ERN":
		c.Body = `Nakfa`

  // Spain. Now replaced by the Euro (EUR). Deprecated – use only for historical prices that pre-date the introduction of the Euro (prices normally quoted as integers)
  case "ESP":
		c.Body = `Peseta`

  // Ethiopia
  case "ETB":
		c.Body = `Ethiopian Birr`

  // Eurozone: Andorra, Austria, Belgium, Cyprus, Estonia, Finland, France, Fr Guiana, Fr S Territories, Germany, Greece, Guadeloupe, Holy See (Vatican City), Ireland, Italy, Latvia, Lithuania, Luxembourg, Martinique, Malta, Mayotte, Monaco, Montenegro, Netherlands, Portugal, Réunion, St Barthelemy, St Martin, St Pierre and Miquelon, San Marino, Slovakia, Slovenia, Spain
  case "EUR":
		c.Body = `Euro`

  // Finland. Now replaced by the Euro (EUR). Deprecated – use only for historical prices that pre-date the introduction of the Euro
  case "FIM":
		c.Body = `Markka`

  // Fiji
  case "FJD":
		c.Body = `Fiji Dollar`

  // Falkland Islands (Malvinas)
  case "FKP":
		c.Body = `Falkland Islands Pound`

  // France. Now replaced by the Euro (EUR). Deprecated – use only for historical prices that pre-date the introduction of the Euro
  case "FRF":
		c.Body = `Franc`

  // United Kingdom, Isle of Man, Channel Islands, South Georgia, South Sandwich Islands, British Indian Ocean Territory (de jure)
  case "GBP":
		c.Body = `Pound Sterling`

  // Georgia
  case "GEL":
		c.Body = `Lari`

  // Deprecated, replaced by GHS
  case "GHC":
		c.Body = `Ghana Cedi`

  // Ghana
  case "GHS":
		c.Body = `Ghana Cedi`

  // Gibraltar
  case "GIP":
		c.Body = `Gibraltar Pound`

  // Gambia
  case "GMD":
		c.Body = `Dalasi`

  // Guinea (prices normally quoted as integers)
  case "GNF":
		c.Body = `Guinean Franc`

  // Greece. Now replaced by the Euro (EUR). Deprecated – use only for historical prices that pre-date the introduction of the Euro
  case "GRD":
		c.Body = `Drachma`

  // Guatemala
  case "GTQ":
		c.Body = `Quetzal`

  // Now replaced by the CFA Franc BCEAO XOF use only for historical prices that pre-date use of the CFA Franc
  case "GWP":
		c.Body = `Guinea-Bissau Peso`

  // Guyana (prices normally quoted as integers)
  case "GYD":
		c.Body = `Guyana Dollar`

  // Hong Kong
  case "HKD":
		c.Body = `Hong Kong Dollar`

  // Honduras
  case "HNL":
		c.Body = `Lempira`

  // Croatia
  case "HRK":
		c.Body = `Kuna`

  // Haiti
  case "HTG":
		c.Body = `Gourde`

  // Hungary (prices normally quoted as integers)
  case "HUF":
		c.Body = `Forint`

  // Indonesia (prices normally quoted as integers)
  case "IDR":
		c.Body = `Rupiah`

  // Ireland. Now replaced by the Euro (EUR). Deprecated – use only for historical prices that pre-date the introduction of the Euro
  case "IEP":
		c.Body = `Punt`

  // Israel
  case "ILS":
		c.Body = `New Israeli Sheqel`

  // India, Bhutan (prices normally quoted as integers)
  case "INR":
		c.Body = `Indian Rupee`

  // Iraq (prices normally quoted as integers)
  case "IQD":
		c.Body = `Iraqi Dinar`

  // Iran (Islamic Republic of) (prices normally quoted as integers)
  case "IRR":
		c.Body = `Iranian Rial`

  // Iceland (prices normally quoted as integers)
  case "ISK":
		c.Body = `Iceland Krona`

  // Italy. Now replaced by the Euro (EUR). Deprecated – use only for historical prices that pre-date the introduction of the Euro (prices normally quoted as integers)
  case "ITL":
		c.Body = `Lira`

  // Jamaica
  case "JMD":
		c.Body = `Jamaican Dollar`

  // Jordan (prices normally quoted with 3 decimal places)
  case "JOD":
		c.Body = `Jordanian Dinar`

  // Japan (prices normally quoted as integers)
  case "JPY":
		c.Body = `Yen`

  // Kenya
  case "KES":
		c.Body = `Kenyan Shilling`

  // Kyrgyzstan
  case "KGS":
		c.Body = `Som`

  // Cambodia
  case "KHR":
		c.Body = `Riel`

  // Comoros (prices normally quoted as integers)
  case "KMF":
		c.Body = `Comorian Franc`

  // Korea (Democratic People’s Republic of) (prices normally quoted as integers)
  case "KPW":
		c.Body = `North Korean Won`

  // Korea (Republic of) (prices normally quoted as integers)
  case "KRW":
		c.Body = `Won`

  // Kuwait (prices normally quoted with 3 decimal places)
  case "KWD":
		c.Body = `Kuwaiti Dinar`

  // Cayman Islands
  case "KYD":
		c.Body = `Cayman Islands Dollar`

  // Kazakstan
  case "KZT":
		c.Body = `Tenge`

  // Lao People’s Democratic Republic (prices normally quoted as integers)
  case "LAK":
		c.Body = `Lao Kip`

  // Lebanon (prices normally quoted as integers)
  case "LBP":
		c.Body = `Lebanese Pound`

  // Sri Lanka
  case "LKR":
		c.Body = `Sri Lanka Rupee`

  // Liberia
  case "LRD":
		c.Body = `Liberian Dollar`

  // Lesotho
  case "LSL":
		c.Body = `Loti`

  // Lithuania. Now replaced by the Euro (EUR). Deprecated – use only for historical prices that pre-date the introduction of the Euro
  case "LTL":
		c.Body = `Litus`

  // Luxembourg. Now replaced by the Euro (EUR). Deprecated – use only for historical prices that pre-date the introduction of the Euro (prices normally quoted as integers)
  case "LUF":
		c.Body = `Luxembourg Franc`

  // Latvia. Now replaced by the Euro (EUR). Deprecated – use only for historical prices that pre-date the introduction of the Euro
  case "LVL":
		c.Body = `Latvian Lats`

  // Libyan Arab Jamahiriya (prices normally quoted with 3 decimal places)
  case "LYD":
		c.Body = `Libyan Dinar`

  // Morocco, Western Sahara
  case "MAD":
		c.Body = `Moroccan Dirham`

  // Moldova, Republic of
  case "MDL":
		c.Body = `Moldovan Leu`

  // Madagascar (prices normally quoted with 0 or 1 decimal place – 1 iraimbilanja = Ar0.2)
  case "MGA":
		c.Body = `Malagasy Ariary`

  // Now replaced by the Ariary (MGA) (prices normally quoted as integers)
  case "MGF":
		c.Body = `Malagasy Franc`

  // North Macedonia (formerly FYR Macedonia)
  case "MKD":
		c.Body = `Denar`

  // Myanmar (prices normally quoted as integers)
  case "MMK":
		c.Body = `Kyat`

  // Mongolia (prices normally quoted as integers)
  case "MNT":
		c.Body = `Tugrik`

  // Macau
  case "MOP":
		c.Body = `Pataca`

  // Mauritania (prices normally quoted with 0 or 1 decimal place – 1 khoums = UM0.2). Was interchangeable with MRU (New) Ouguiya at rate of 10:1 until June 2018. DEPRECATED, use MRU instead
  case "MRO":
		c.Body = `(Old) Ouguiya`

  // Mauritania (prices normally quoted with 0 or 1 decimal place – 1 khoums = UM0.2). Replaced MRO (old) Ouguiya at rate of 10:1 in June 2018. For use in ONIX 3.0 only
  case "MRU":
		c.Body = `Ouguiya`

  // Malta. Now replaced by the Euro (EUR). Deprecated – use only for historical prices that pre-date the introduction of the Euro
  case "MTL":
		c.Body = `Maltese Lira`

  // Mauritius (prices normally quoted as integers)
  case "MUR":
		c.Body = `Mauritius Rupee`

  // Maldives
  case "MVR":
		c.Body = `Rufiyaa`

  // Malawi
  case "MWK":
		c.Body = `Malawi Kwacha`

  // Mexico
  case "MXN":
		c.Body = `Mexican Peso`

  // Malaysia
  case "MYR":
		c.Body = `Malaysian Ringgit`

  // Mozambique
  case "MZN":
		c.Body = `Mozambique Metical`

  // Namibia
  case "NAD":
		c.Body = `Namibia Dollar`

  // Nigeria
  case "NGN":
		c.Body = `Naira`

  // Nicaragua
  case "NIO":
		c.Body = `Cordoba Oro`

  // Netherlands. Now replaced by the Euro (EUR). Deprecated – use only for historical prices that pre-date the introduction of the Euro
  case "NLG":
		c.Body = `Guilder`

  // Norway, Bouvet Island, Svalbard and Jan Mayen
  case "NOK":
		c.Body = `Norwegian Krone`

  // Nepal
  case "NPR":
		c.Body = `Nepalese Rupee`

  // New Zealand, Cook Islands, Niue, Pitcairn, Tokelau
  case "NZD":
		c.Body = `New Zealand Dollar`

  // Oman (prices normally quoted with 3 decimal places)
  case "OMR":
		c.Body = `Rial Omani`

  // Panama
  case "PAB":
		c.Body = `Balboa`

  // Peru (formerly Nuevo Sol)
  case "PEN":
		c.Body = `Sol`

  // Papua New Guinea
  case "PGK":
		c.Body = `Kina`

  // Philippines
  case "PHP":
		c.Body = `Philippine Peso`

  // Pakistan (prices normally quoted as integers)
  case "PKR":
		c.Body = `Pakistan Rupee`

  // Poland
  case "PLN":
		c.Body = `Złoty`

  // Portugal. Now replaced by the Euro (EUR). Deprecated – use only for historical prices that pre-date the introduction of the Euro
  case "PTE":
		c.Body = `Escudo`

  // Paraguay (prices normally quoted as integers)
  case "PYG":
		c.Body = `Guarani`

  // Qatar
  case "QAR":
		c.Body = `Qatari Rial`

  // Deprecated, replaced by RON
  case "ROL":
		c.Body = `Romanian Old Leu`

  // Romania
  case "RON":
		c.Body = `Romanian Leu`

  // Serbia (prices normally quoted as integers)
  case "RSD":
		c.Body = `Serbian Dinar`

  // Russian Federation
  case "RUB":
		c.Body = `Russian Ruble`

  // DEPRECATED, replaced by RUB
  case "RUR":
		c.Body = `Russian Ruble`

  // Rwanda (prices normally quoted as integers)
  case "RWF":
		c.Body = `Rwanda Franc`

  // Saudi Arabia
  case "SAR":
		c.Body = `Saudi Riyal`

  // Solomon Islands
  case "SBD":
		c.Body = `Solomon Islands Dollar`

  // Seychelles
  case "SCR":
		c.Body = `Seychelles Rupee`

  // Now replaced by the Sudanese Pound (SDG)
  case "SDD":
		c.Body = `Sudanese Dinar`

  // Sudan
  case "SDG":
		c.Body = `Sudanese Pound`

  // Sweden
  case "SEK":
		c.Body = `Swedish Krona`

  // Singapore
  case "SGD":
		c.Body = `Singapore Dollar`

  // Saint Helena
  case "SHP":
		c.Body = `Saint Helena Pound`

  // Slovenia. Now replaced by the Euro (EUR). Deprecated – use only for historical prices that pre-date the introduction of the Euro
  case "SIT":
		c.Body = `Tolar`

  // Slovakia. Now replaced by the Euro (EUR). Deprecated – use only for historical prices that pre-date the introduction of the Euro
  case "SKK":
		c.Body = `Slovak Koruna`

  // Sierra Leone (prices normally quoted as integers)
  case "SLL":
		c.Body = `Leone`

  // Somalia (prices normally quoted as integers)
  case "SOS":
		c.Body = `Somali Shilling`

  // Suriname
  case "SRD":
		c.Body = `Surinam Dollar`

  // DEPRECATED, replaced by SRD
  case "SRG":
		c.Body = `Suriname Guilder`

  // São Tome and Principe (prices normally quoted as integers). Was interchangeable with STN (New) Dobra at rate of 1000:1 until June 2018. DEPRECATED, use STN instead
  case "STD":
		c.Body = `(Old) Dobra`

  // São Tome and Principe. Replaced STD (old) Dobra at rate of 1000:1 in June 2018. For use in ONIX 3.0 only
  case "STN":
		c.Body = `Dobra`

  // El Salvador
  case "SVC":
		c.Body = `El Salvador Colon`

  // Syrian Arab Republic (prices normally quoted as integers)
  case "SYP":
		c.Body = `Syrian Pound`

  // Eswatini (formerly known as Swaziland)
  case "SZL":
		c.Body = `Lilangeni`

  // Thailand
  case "THB":
		c.Body = `Baht`

  // Tajikistan
  case "TJS":
		c.Body = `Somoni`

  // Deprecated, replaced by TMT (prices normally quoted as integers)
  case "TMM":
		c.Body = `Turkmenistan Manat`

  // Turkmenistan
  case "TMT":
		c.Body = `Turkmenistan New Manat`

  // Tunisia (prices normally quoted with 3 decimal places)
  case "TND":
		c.Body = `Tunisian Dinar`

  // Tonga
  case "TOP":
		c.Body = `Pa’anga`

  // Deprecated. Timor-Leste now uses the US Dollar
  case "TPE":
		c.Body = `Timor Escudo`

  // Deprecated, replaced by TRY (prices normally quoted as integers)
  case "TRL":
		c.Body = `Turkish Lira (old)`

  // Turkey, from 1 January 2005
  case "TRY":
		c.Body = `Turkish Lira`

  // Trinidad and Tobago
  case "TTD":
		c.Body = `Trinidad and Tobago Dollar`

  // Taiwan (Province of China)
  case "TWD":
		c.Body = `New Taiwan Dollar`

  // Tanzania (United Republic of) (prices normally quoted as integers)
  case "TZS":
		c.Body = `Tanzanian Shilling`

  // Ukraine
  case "UAH":
		c.Body = `Hryvnia`

  // Uganda (prices normally quoted as integers)
  case "UGX":
		c.Body = `Uganda Shilling`

  // United States, American Samoa, Bonaire, Sint Eustatius and Saba, British Indian Ocean Territory, Ecuador, El Salvador, Guam, Haiti, Marshall Is, Micronesia (Federated States of), Northern Mariana Is, Palau, Panama, Puerto Rico, Timor-Leste, Turks and Caicos Is, US Minor Outlying Is, Virgin Is (British), Virgin Is (US)
  case "USD":
		c.Body = `US Dollar`

  // Uruguay
  case "UYU":
		c.Body = `Peso Uruguayo`

  // Uzbekistan (prices normally quoted as integers)
  case "UZS":
		c.Body = `Uzbekistan Sum`

  // Deprecated, replaced by VEF
  case "VEB":
		c.Body = `Bolívar`

  // Venezuela (formerly Bolívar fuerte). Deprecated, replaced by VES
  case "VEF":
		c.Body = `Bolívar`

  // Venezuela (replaced VEF from August 2018 at rate of 100,000:1). For use in ONIX 3.0 only
  case "VES":
		c.Body = `Bolívar Soberano`

  // Viet Nam (prices normally quoted as integers)
  case "VND":
		c.Body = `Dong`

  // Vanuatu (prices normally quoted as integers)
  case "VUV":
		c.Body = `Vatu`

  // Samoa
  case "WST":
		c.Body = `Tala`

  // Cameroon, Central African Republic, Chad, Congo, Equatorial Guinea, Gabon (prices normally quoted as integers)
  case "XAF":
		c.Body = `CFA Franc BEAC`

  // Anguilla, Antigua and Barbuda, Dominica, Grenada, Montserrat, Saint Kitts and Nevis, Saint Lucia, Saint Vincent and the Grenadines
  case "XCD":
		c.Body = `East Caribbean Dollar`

  // Benin, Burkina Faso, Côte D’Ivoire, Guinea-Bissau, Mali, Niger, Senegal, Togo (prices normally quoted as integers)
  case "XOF":
		c.Body = `CFA Franc BCEAO`

  // French Polynesia, New Caledonia, Wallis and Futuna (prices normally quoted as integers)
  case "XPF":
		c.Body = `CFP Franc`

  // Yemen (prices normally quoted as integers)
  case "YER":
		c.Body = `Yemeni Rial`

  // DEPRECATED, replaced by CSD
  case "YUM":
		c.Body = `Yugoslavian Dinar`

  // South Africa, Namibia, Lesotho
  case "ZAR":
		c.Body = `Rand`

  // Zambia. Deprecated, replaced with ZMW (prices normally quoted as integers)
  case "ZMK":
		c.Body = `Kwacha`

  // Zambia
  case "ZMW":
		c.Body = `Zambian Kwacha`

  // Deprecated, replaced with ZWL (prices normally quoted as integers)
  case "ZWD":
		c.Body = `Zimbabwe Dollar`

  // Zimbabwe
  case "ZWL":
		c.Body = `Zimbabwe Dollar`
	default:
		return fmt.Errorf("undefined code for DefaultCurrencyCode has been passed, got [%s]", v)
	}
	return nil
}

// DefaultLanguageOfText Language – based on ISO 639-2/B
type DefaultLanguageOfText struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *DefaultLanguageOfText) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // Afar
  case "aar":
		c.Body = `Afar`

  // Abkhaz
  case "abk":
		c.Body = `Abkhaz`

  // Achinese
  case "ace":
		c.Body = `Achinese`

  // Acoli
  case "ach":
		c.Body = `Acoli`

  // Adangme
  case "ada":
		c.Body = `Adangme`

  // Adygei
  case "ady":
		c.Body = `Adygei`

  // Collective name
  case "afa":
		c.Body = `Afro-Asiatic languages`

  // Artificial language
  case "afh":
		c.Body = `Afrihili`

  // Afrikaans
  case "afr":
		c.Body = `Afrikaans`

  // Ainu
  case "ain":
		c.Body = `Ainu`

  // Macrolanguage
  case "aka":
		c.Body = `Akan`

  // Akkadian
  case "akk":
		c.Body = `Akkadian`

  // Macrolanguage
  case "alb":
		c.Body = `Albanian`

  // Aleut
  case "ale":
		c.Body = `Aleut`

  // Collective name
  case "alg":
		c.Body = `Algonquian languages`

  // Southern Altai
  case "alt":
		c.Body = `Southern Altai`

  // Amharic
  case "amh":
		c.Body = `Amharic`

  // English, Old (ca. 450-1100)
  case "ang":
		c.Body = `English, Old (ca. 450-1100)`

  // Angika
  case "anp":
		c.Body = `Angika`

  // Collective name
  case "apa":
		c.Body = `Apache languages`

  // Macrolanguage
  case "ara":
		c.Body = `Arabic`

  // Official Aramaic; Imperial Aramaic (700-300 BCE)
  case "arc":
		c.Body = `Official Aramaic; Imperial Aramaic (700-300 BCE)`

  // Aragonese
  case "arg":
		c.Body = `Aragonese`

  // Armenian
  case "arm":
		c.Body = `Armenian`

  // Mapudungun; Mapuche
  case "arn":
		c.Body = `Mapudungun; Mapuche`

  // Arapaho
  case "arp":
		c.Body = `Arapaho`

  // Collective name
  case "art":
		c.Body = `Artificial languages`

  // Arawak
  case "arw":
		c.Body = `Arawak`

  // Assamese
  case "asm":
		c.Body = `Assamese`

  // Asturian; Bable; Leonese; Asturleonese
  case "ast":
		c.Body = `Asturian; Bable; Leonese; Asturleonese`

  // Collective name
  case "ath":
		c.Body = `Athapascan languages`

  // Collective name
  case "aus":
		c.Body = `Australian languages`

  // Avaric
  case "ava":
		c.Body = `Avaric`

  // Avestan
  case "ave":
		c.Body = `Avestan`

  // Awadhi
  case "awa":
		c.Body = `Awadhi`

  // Macrolanguage
  case "aym":
		c.Body = `Aymara`

  // Macrolanguage
  case "aze":
		c.Body = `Azerbaijani`

  // Collective name
  case "bad":
		c.Body = `Banda languages`

  // Collective name
  case "bai":
		c.Body = `Bamileke languages`

  // Bashkir
  case "bak":
		c.Body = `Bashkir`

  // Macrolanguage
  case "bal":
		c.Body = `Baluchi`

  // Bambara
  case "bam":
		c.Body = `Bambara`

  // Balinese
  case "ban":
		c.Body = `Balinese`

  // Basque
  case "baq":
		c.Body = `Basque`

  // Basa
  case "bas":
		c.Body = `Basa`

  // Collective name
  case "bat":
		c.Body = `Baltic languages`

  // Beja; Bedawiyet
  case "bej":
		c.Body = `Beja; Bedawiyet`

  // Belarusian
  case "bel":
		c.Body = `Belarusian`

  // Bemba
  case "bem":
		c.Body = `Bemba`

  // Bengali
  case "ben":
		c.Body = `Bengali`

  // Collective name
  case "ber":
		c.Body = `Berber languages`

  // Bhojpuri
  case "bho":
		c.Body = `Bhojpuri`

  // Collective name
  case "bih":
		c.Body = `Bihari languages`

  // Macrolanguage
  case "bik":
		c.Body = `Bikol`

  // Bini; Edo
  case "bin":
		c.Body = `Bini; Edo`

  // Bislama
  case "bis":
		c.Body = `Bislama`

  // Siksika
  case "bla":
		c.Body = `Siksika`

  // Collective name
  case "bnt":
		c.Body = `Bantu languages`

  // Bosnian
  case "bos":
		c.Body = `Bosnian`

  // Braj
  case "bra":
		c.Body = `Braj`

  // Breton
  case "bre":
		c.Body = `Breton`

  // Collective name
  case "btk":
		c.Body = `Batak languages`

  // Macrolanguage
  case "bua":
		c.Body = `Buriat`

  // Buginese
  case "bug":
		c.Body = `Buginese`

  // Bulgarian
  case "bul":
		c.Body = `Bulgarian`

  // Burmese
  case "bur":
		c.Body = `Burmese`

  // Blin; Bilin
  case "byn":
		c.Body = `Blin; Bilin`

  // Caddo
  case "cad":
		c.Body = `Caddo`

  // Collective name
  case "cai":
		c.Body = `Central American Indian languages`

  // Galibi Carib
  case "car":
		c.Body = `Galibi Carib`

  // Catalan
  case "cat":
		c.Body = `Catalan`

  // Collective name
  case "cau":
		c.Body = `Caucasian languages`

  // Cebuano
  case "ceb":
		c.Body = `Cebuano`

  // Collective name
  case "cel":
		c.Body = `Celtic languages`

  // Chamorro
  case "cha":
		c.Body = `Chamorro`

  // Chibcha
  case "chb":
		c.Body = `Chibcha`

  // Chechen
  case "che":
		c.Body = `Chechen`

  // Chagatai
  case "chg":
		c.Body = `Chagatai`

  // Macrolanguage
  case "chi":
		c.Body = `Chinese`

  // Chuukese (Truk)
  case "chk":
		c.Body = `Chuukese (Truk)`

  // Macrolanguage
  case "chm":
		c.Body = `Mari`

  // Chinook jargon
  case "chn":
		c.Body = `Chinook jargon`

  // Choctaw
  case "cho":
		c.Body = `Choctaw`

  // Chipewyan; Dene Suline
  case "chp":
		c.Body = `Chipewyan; Dene Suline`

  // Cherokee
  case "chr":
		c.Body = `Cherokee`

  // Church Slavic; Old Slavonic; Church Slavonic; Old Bulgarian; Old Church Slavonic
  case "chu":
		c.Body = `Church Slavic; Old Slavonic; Church Slavonic; Old Bulgarian; Old Church Slavonic`

  // Chuvash
  case "chv":
		c.Body = `Chuvash`

  // Cheyenne
  case "chy":
		c.Body = `Cheyenne`

  // ONIX local code, equivalent to ckb in ISO 639-3. For use in ONIX 3.0 only
  case "ckb":
		c.Body = `Central Kurdish (Sorani)`

  // Collective name
  case "cmc":
		c.Body = `Chamic languages`

  // ONIX local code, equivalent to cmn in ISO 639-3
  case "cmn":
		c.Body = `Mandarin`

  // For use in ONIX 3.0 only
  case "cnr":
		c.Body = `Montenegrin`

  // Coptic
  case "cop":
		c.Body = `Coptic`

  // Cornish
  case "cor":
		c.Body = `Cornish`

  // Corsican
  case "cos":
		c.Body = `Corsican`

  // Collective name
  case "cpe":
		c.Body = `Creoles and pidgins, English-based`

  // Collective name
  case "cpf":
		c.Body = `Creoles and pidgins, French-based`

  // Collective name
  case "cpp":
		c.Body = `Creoles and pidgins, Portuguese-based`

  // Macrolanguage
  case "cre":
		c.Body = `Cree`

  // Crimean Turkish; Crimean Tatar
  case "crh":
		c.Body = `Crimean Turkish; Crimean Tatar`

  // Collective name
  case "crp":
		c.Body = `Creoles and pidgins`

  // Kashubian
  case "csb":
		c.Body = `Kashubian`

  // Collective name
  case "cus":
		c.Body = `Cushitic languages`

  // Czech
  case "cze":
		c.Body = `Czech`

  // Dakota
  case "dak":
		c.Body = `Dakota`

  // Danish
  case "dan":
		c.Body = `Danish`

  // Dargwa
  case "dar":
		c.Body = `Dargwa`

  // Collective name
  case "day":
		c.Body = `Land Dayak languages`

  // Macrolanguage
  case "del":
		c.Body = `Delaware`

  // Macrolanguage
  case "den":
		c.Body = `Slave (Athapascan)`

  // Dogrib
  case "dgr":
		c.Body = `Dogrib`

  // Macrolanguage
  case "din":
		c.Body = `Dinka`

  // Divehi; Dhivehi; Maldivian
  case "div":
		c.Body = `Divehi; Dhivehi; Maldivian`

  // Macrolanguage
  case "doi":
		c.Body = `Dogri`

  // Collective name
  case "dra":
		c.Body = `Dravidian languages`

  // Lower Sorbian
  case "dsb":
		c.Body = `Lower Sorbian`

  // Duala
  case "dua":
		c.Body = `Duala`

  // Dutch, Middle (ca. 1050-1350)
  case "dum":
		c.Body = `Dutch, Middle (ca. 1050-1350)`

  // Dutch; Flemish
  case "dut":
		c.Body = `Dutch; Flemish`

  // Dyula
  case "dyu":
		c.Body = `Dyula`

  // Dzongkha
  case "dzo":
		c.Body = `Dzongkha`

  // Efik
  case "efi":
		c.Body = `Efik`

  // ONIX local code for Italian dialect, equivalent to egl in ISO 639-3. For use in ONIX 3.0 only
  case "egl":
		c.Body = `Emilian`

  // Egyptian (Ancient)
  case "egy":
		c.Body = `Egyptian (Ancient)`

  // Ekajuk
  case "eka":
		c.Body = `Ekajuk`

  // Elamite
  case "elx":
		c.Body = `Elamite`

  // English
  case "eng":
		c.Body = `English`

  // English, Middle (1100-1500)
  case "enm":
		c.Body = `English, Middle (1100-1500)`

  // Artificial language
  case "epo":
		c.Body = `Esperanto`

  // Macrolanguage
  case "est":
		c.Body = `Estonian`

  // Ewe
  case "ewe":
		c.Body = `Ewe`

  // Ewondo
  case "ewo":
		c.Body = `Ewondo`

  // Fang
  case "fan":
		c.Body = `Fang`

  // Faroese
  case "fao":
		c.Body = `Faroese`

  // Fanti
  case "fat":
		c.Body = `Fanti`

  // Fijian
  case "fij":
		c.Body = `Fijian`

  // Filipino; Pilipino
  case "fil":
		c.Body = `Filipino; Pilipino`

  // Finnish
  case "fin":
		c.Body = `Finnish`

  // ONIX local code, equivalent to fit in ISO 639-3
  case "fit":
		c.Body = `Meänkieli / Tornedalen Finnish`

  // Collective name
  case "fiu":
		c.Body = `Finno-Ugrian languages`

  // ONIX local code, equivalent to fkv in ISO 639-3
  case "fkv":
		c.Body = `Kvensk`

  // Fon
  case "fon":
		c.Body = `Fon`

  // French
  case "fre":
		c.Body = `French`

  // French, Middle (ca. 1400-1600)
  case "frm":
		c.Body = `French, Middle (ca. 1400-1600)`

  // French, Old (ca. 842-1400)
  case "fro":
		c.Body = `French, Old (ca. 842-1400)`

  // Northern Frisian
  case "frr":
		c.Body = `Northern Frisian`

  // Eastern Frisian
  case "frs":
		c.Body = `Eastern Frisian`

  // Western Frisian
  case "fry":
		c.Body = `Western Frisian`

  // Fulah
  case "ful":
		c.Body = `Fulah`

  // Friulian
  case "fur":
		c.Body = `Friulian`

  // Gã
  case "gaa":
		c.Body = `Gã`

  // Gayo
  case "gay":
		c.Body = `Gayo`

  // Macrolanguage
  case "gba":
		c.Body = `Gbaya`

  // Collective name
  case "gem":
		c.Body = `Germanic languages`

  // Georgian
  case "geo":
		c.Body = `Georgian`

  // German
  case "ger":
		c.Body = `German`

  // Ethiopic (Ge’ez)
  case "gez":
		c.Body = `Ethiopic (Ge’ez)`

  // Gilbertese
  case "gil":
		c.Body = `Gilbertese`

  // Scottish Gaelic
  case "gla":
		c.Body = `Scottish Gaelic`

  // Irish
  case "gle":
		c.Body = `Irish`

  // Galician
  case "glg":
		c.Body = `Galician`

  // Manx
  case "glv":
		c.Body = `Manx`

  // German, Middle High (ca. 1050-1500)
  case "gmh":
		c.Body = `German, Middle High (ca. 1050-1500)`

  // German, Old High (ca. 750-1050)
  case "goh":
		c.Body = `German, Old High (ca. 750-1050)`

  // Macrolanguage
  case "gon":
		c.Body = `Gondi`

  // Gorontalo
  case "gor":
		c.Body = `Gorontalo`

  // Gothic
  case "got":
		c.Body = `Gothic`

  // Macrolanguage
  case "grb":
		c.Body = `Grebo`

  // Greek, Ancient (to 1453)
  case "grc":
		c.Body = `Greek, Ancient (to 1453)`

  // Greek, Modern (1453-)
  case "gre":
		c.Body = `Greek, Modern (1453-)`

  // Macrolanguage
  case "grn":
		c.Body = `Guarani`

  // ONIX local code, equivalent to grt in ISO 639-3
  case "grt":
		c.Body = `Garo`

  // Swiss German; Alemannic
  case "gsw":
		c.Body = `Swiss German; Alemannic`

  // Gujarati
  case "guj":
		c.Body = `Gujarati`

  // Gwich’in
  case "gwi":
		c.Body = `Gwich’in`

  // Macrolanguage
  case "hai":
		c.Body = `Haida`

  // Haitian French Creole
  case "hat":
		c.Body = `Haitian French Creole`

  // Hausa
  case "hau":
		c.Body = `Hausa`

  // Hawaiian
  case "haw":
		c.Body = `Hawaiian`

  // Hebrew
  case "heb":
		c.Body = `Hebrew`

  // Herero
  case "her":
		c.Body = `Herero`

  // Hiligaynon
  case "hil":
		c.Body = `Hiligaynon`

  // Collective name
  case "him":
		c.Body = `Himachali languages; Western Pahari languages`

  // Hindi
  case "hin":
		c.Body = `Hindi`

  // Hittite
  case "hit":
		c.Body = `Hittite`

  // Macrolanguage
  case "hmn":
		c.Body = `Hmong; Mong`

  // Hiri Motu
  case "hmo":
		c.Body = `Hiri Motu`

  // Croatian
  case "hrv":
		c.Body = `Croatian`

  // Upper Sorbian
  case "hsb":
		c.Body = `Upper Sorbian`

  // Hungarian
  case "hun":
		c.Body = `Hungarian`

  // Hupa
  case "hup":
		c.Body = `Hupa`

  // Iban
  case "iba":
		c.Body = `Iban`

  // Igbo
  case "ibo":
		c.Body = `Igbo`

  // Icelandic
  case "ice":
		c.Body = `Icelandic`

  // Artificial language
  case "ido":
		c.Body = `Ido`

  // Sichuan Yi; Nuosu
  case "iii":
		c.Body = `Sichuan Yi; Nuosu`

  // Collective name
  case "ijo":
		c.Body = `Ijo languages`

  // Macrolanguage
  case "iku":
		c.Body = `Inuktitut`

  // Artificial language
  case "ile":
		c.Body = `Interlingue; Occidental`

  // Iloko
  case "ilo":
		c.Body = `Iloko`

  // Artificial language
  case "ina":
		c.Body = `Interlingua (International Auxiliary Language Association)`

  // Collective name
  case "inc":
		c.Body = `Indic languages`

  // Indonesian
  case "ind":
		c.Body = `Indonesian`

  // Collective name
  case "ine":
		c.Body = `Indo-European languages`

  // Ingush
  case "inh":
		c.Body = `Ingush`

  // Macrolanguage
  case "ipk":
		c.Body = `Inupiaq`

  // Collective name
  case "ira":
		c.Body = `Iranian languages`

  // Collective name
  case "iro":
		c.Body = `Iroquoian languages`

  // Italian
  case "ita":
		c.Body = `Italian`

  // Javanese
  case "jav":
		c.Body = `Javanese`

  // Lojban
  case "jbo":
		c.Body = `Lojban`

  // Japanese
  case "jpn":
		c.Body = `Japanese`

  // Judeo-Persian
  case "jpr":
		c.Body = `Judeo-Persian`

  // Macrolanguage
  case "jrb":
		c.Body = `Judeo-Arabic`

  // Kara-Kalpak
  case "kaa":
		c.Body = `Kara-Kalpak`

  // Kabyle
  case "kab":
		c.Body = `Kabyle`

  // Kachin; Jingpho
  case "kac":
		c.Body = `Kachin; Jingpho`

  // Kalâtdlisut; Greenlandic
  case "kal":
		c.Body = `Kalâtdlisut; Greenlandic`

  // Kamba
  case "kam":
		c.Body = `Kamba`

  // Kannada
  case "kan":
		c.Body = `Kannada`

  // Collective name
  case "kar":
		c.Body = `Karen languages`

  // Kashmiri
  case "kas":
		c.Body = `Kashmiri`

  // Macrolanguage
  case "kau":
		c.Body = `Kanuri`

  // Kawi
  case "kaw":
		c.Body = `Kawi`

  // Kazakh
  case "kaz":
		c.Body = `Kazakh`

  // Kabardian (Circassian)
  case "kbd":
		c.Body = `Kabardian (Circassian)`

  // ONIX local code, equivalent to kdr in ISO 639-3
  case "kdr":
		c.Body = `Karaim`

  // Khasi
  case "kha":
		c.Body = `Khasi`

  // Collective name
  case "khi":
		c.Body = `Khoisan languages`

  // Central Khmer
  case "khm":
		c.Body = `Central Khmer`

  // Khotanese; Sakan
  case "kho":
		c.Body = `Khotanese; Sakan`

  // Kikuyu; Gikuyu
  case "kik":
		c.Body = `Kikuyu; Gikuyu`

  // Kinyarwanda
  case "kin":
		c.Body = `Kinyarwanda`

  // Kirghiz; Kyrgyz
  case "kir":
		c.Body = `Kirghiz; Kyrgyz`

  // Kimbundu
  case "kmb":
		c.Body = `Kimbundu`

  // Macrolanguage
  case "kok":
		c.Body = `Konkani`

  // Macrolanguage
  case "kom":
		c.Body = `Komi`

  // Macrolanguage
  case "kon":
		c.Body = `Kongo`

  // Korean
  case "kor":
		c.Body = `Korean`

  // Kusaiean (Caroline Islands)
  case "kos":
		c.Body = `Kusaiean (Caroline Islands)`

  // Macrolanguage
  case "kpe":
		c.Body = `Kpelle`

  // Karachay-Balkar
  case "krc":
		c.Body = `Karachay-Balkar`

  // Karelian
  case "krl":
		c.Body = `Karelian`

  // Collective name
  case "kro":
		c.Body = `Kru languages`

  // Kurukh
  case "kru":
		c.Body = `Kurukh`

  // Kuanyama
  case "kua":
		c.Body = `Kuanyama`

  // Kumyk
  case "kum":
		c.Body = `Kumyk`

  // Macrolanguage
  case "kur":
		c.Body = `Kurdish`

  // Kutenai
  case "kut":
		c.Body = `Kutenai`

  // Ladino
  case "lad":
		c.Body = `Ladino`

  // Macrolanguage
  case "lah":
		c.Body = `Lahnda`

  // Lamba
  case "lam":
		c.Body = `Lamba`

  // Lao
  case "lao":
		c.Body = `Lao`

  // Latin
  case "lat":
		c.Body = `Latin`

  // Macrolanguage
  case "lav":
		c.Body = `Latvian`

  // Lezgian
  case "lez":
		c.Body = `Lezgian`

  // ONIX local code for Italian dialect, equivalent to lij in ISO 639-3. For use in ONIX 3.0 only
  case "lij":
		c.Body = `Ligurian`

  // Limburgish
  case "lim":
		c.Body = `Limburgish`

  // Lingala
  case "lin":
		c.Body = `Lingala`

  // Lithuanian
  case "lit":
		c.Body = `Lithuanian`

  // ONIX local code for Italian dialect, equivalent to lmo in ISO 639-3. For use in ONIX 3.0 only
  case "lmo":
		c.Body = `Lombard`

  // Mongo-Nkundu
  case "lol":
		c.Body = `Mongo-Nkundu`

  // Lozi
  case "loz":
		c.Body = `Lozi`

  // Luxembourgish; Letzeburgesch
  case "ltz":
		c.Body = `Luxembourgish; Letzeburgesch`

  // Luba-Lulua
  case "lua":
		c.Body = `Luba-Lulua`

  // Luba-Katanga
  case "lub":
		c.Body = `Luba-Katanga`

  // Ganda
  case "lug":
		c.Body = `Ganda`

  // Luiseño
  case "lui":
		c.Body = `Luiseño`

  // Lunda
  case "lun":
		c.Body = `Lunda`

  // Luo (Kenya and Tanzania)
  case "luo":
		c.Body = `Luo (Kenya and Tanzania)`

  // Lushai
  case "lus":
		c.Body = `Lushai`

  // Macedonian
  case "mac":
		c.Body = `Macedonian`

  // Madurese
  case "mad":
		c.Body = `Madurese`

  // Magahi
  case "mag":
		c.Body = `Magahi`

  // Marshallese
  case "mah":
		c.Body = `Marshallese`

  // Maithili
  case "mai":
		c.Body = `Maithili`

  // Makasar
  case "mak":
		c.Body = `Makasar`

  // Malayalam
  case "mal":
		c.Body = `Malayalam`

  // Macrolanguage
  case "man":
		c.Body = `Mandingo`

  // Maori
  case "mao":
		c.Body = `Maori`

  // Collective name
  case "map":
		c.Body = `Austronesian languages`

  // Marathi
  case "mar":
		c.Body = `Marathi`

  // Masai
  case "mas":
		c.Body = `Masai`

  // Macrolanguage
  case "may":
		c.Body = `Malay`

  // Moksha
  case "mdf":
		c.Body = `Moksha`

  // Mandar
  case "mdr":
		c.Body = `Mandar`

  // Mende
  case "men":
		c.Body = `Mende`

  // Irish, Middle (ca. 1100-1550)
  case "mga":
		c.Body = `Irish, Middle (ca. 1100-1550)`

  // Mi’kmaq; Micmac
  case "mic":
		c.Body = `Mi’kmaq; Micmac`

  // Minangkabau
  case "min":
		c.Body = `Minangkabau`

  // Use where no suitable code is available
  case "mis":
		c.Body = `Uncoded languages`

  // Collective name
  case "mkh":
		c.Body = `Mon-Khmer languages`

  // Macrolanguage
  case "mlg":
		c.Body = `Malagasy`

  // Maltese
  case "mlt":
		c.Body = `Maltese`

  // Manchu
  case "mnc":
		c.Body = `Manchu`

  // Manipuri
  case "mni":
		c.Body = `Manipuri`

  // Collective name
  case "mno":
		c.Body = `Manobo languages`

  // Mohawk
  case "moh":
		c.Body = `Mohawk`

  // DEPRECATED – use rum
  case "mol":
		c.Body = `Moldavian; Moldovan`

  // Macrolanguage
  case "mon":
		c.Body = `Mongolian`

  // Mooré; Mossi
  case "mos":
		c.Body = `Mooré; Mossi`

  // Multiple languages
  case "mul":
		c.Body = `Multiple languages`

  // Collective name
  case "mun":
		c.Body = `Munda languages`

  // Creek
  case "mus":
		c.Body = `Creek`

  // ONIX local code, equivalent to mwf in ISO 639-3. For use in ONIX 3.0 only
  case "mwf":
		c.Body = `Murrinh-Patha`

  // Mirandese
  case "mwl":
		c.Body = `Mirandese`

  // Macrolanguage
  case "mwr":
		c.Body = `Marwari`

  // Collective name
  case "myn":
		c.Body = `Mayan languages`

  // Erzya
  case "myv":
		c.Body = `Erzya`

  // Collective name
  case "nah":
		c.Body = `Nahuatl languages`

  // Collective name
  case "nai":
		c.Body = `North American Indian languages`

  // Neapolitan
  case "nap":
		c.Body = `Neapolitan`

  // Nauruan
  case "nau":
		c.Body = `Nauruan`

  // Navajo
  case "nav":
		c.Body = `Navajo`

  // Ndebele, South
  case "nbl":
		c.Body = `Ndebele, South`

  // Ndebele, North
  case "nde":
		c.Body = `Ndebele, North`

  // Ndonga
  case "ndo":
		c.Body = `Ndonga`

  // Low German; Low Saxon
  case "nds":
		c.Body = `Low German; Low Saxon`

  // Macrolanguage
  case "nep":
		c.Body = `Nepali`

  // Newari; Nepal Bhasa
  case "new":
		c.Body = `Newari; Nepal Bhasa`

  // Nias
  case "nia":
		c.Body = `Nias`

  // Collective name
  case "nic":
		c.Body = `Niger-Kordofanian languages`

  // Niuean
  case "niu":
		c.Body = `Niuean`

  // Norwegian Nynorsk
  case "nno":
		c.Body = `Norwegian Nynorsk`

  // Norwegian Bokmål
  case "nob":
		c.Body = `Norwegian Bokmål`

  // Nogai
  case "nog":
		c.Body = `Nogai`

  // Old Norse
  case "non":
		c.Body = `Old Norse`

  // Macrolanguage
  case "nor":
		c.Body = `Norwegian`

  // N’Ko
  case "nqo":
		c.Body = `N’Ko`

  // ONIX local code, equivalent to nrf in ISO 639-3. For use in ONIX 3.0 only
  case "nrf":
		c.Body = `Guernésiais, Jèrriais`

  // Pedi; Sepedi; Northern Sotho
  case "nso":
		c.Body = `Pedi; Sepedi; Northern Sotho`

  // Collective name
  case "nub":
		c.Body = `Nubian languages`

  // Classical Newari; Old Newari; Classical Nepal Bhasa
  case "nwc":
		c.Body = `Classical Newari; Old Newari; Classical Nepal Bhasa`

  // Chichewa; Chewa; Nyanja
  case "nya":
		c.Body = `Chichewa; Chewa; Nyanja`

  // Nyamwezi
  case "nym":
		c.Body = `Nyamwezi`

  // Nyankole
  case "nyn":
		c.Body = `Nyankole`

  // Nyoro
  case "nyo":
		c.Body = `Nyoro`

  // Nzima
  case "nzi":
		c.Body = `Nzima`

  // Occitan (post 1500)
  case "oci":
		c.Body = `Occitan (post 1500)`

  // ONIX local code, equivalent to odt in ISO 639-3
  case "odt":
		c.Body = `Old Dutch / Old Low Franconian (ca. 400–1050)`

  // Macrolanguage
  case "oji":
		c.Body = `Ojibwa`

  // ONIX local code, equivalent to omq in ISO 639-5. Collective name
  case "omq":
		c.Body = `Oto-Manguean languages`

  // Macrolanguage
  case "ori":
		c.Body = `Oriya`

  // Macrolanguage
  case "orm":
		c.Body = `Oromo`

  // Osage
  case "osa":
		c.Body = `Osage`

  // Ossetian; Ossetic
  case "oss":
		c.Body = `Ossetian; Ossetic`

  // Turkish, Ottoman
  case "ota":
		c.Body = `Turkish, Ottoman`

  // Collective name
  case "oto":
		c.Body = `Otomian languages`

  // Collective name
  case "paa":
		c.Body = `Papuan languages`

  // Pangasinan
  case "pag":
		c.Body = `Pangasinan`

  // Pahlavi
  case "pal":
		c.Body = `Pahlavi`

  // Pampanga; Kapampangan
  case "pam":
		c.Body = `Pampanga; Kapampangan`

  // Panjabi
  case "pan":
		c.Body = `Panjabi`

  // Papiamento
  case "pap":
		c.Body = `Papiamento`

  // Palauan
  case "pau":
		c.Body = `Palauan`

  // Old Persian (ca. 600-400 B.C.)
  case "peo":
		c.Body = `Old Persian (ca. 600-400 B.C.)`

  // Macrolanguage
  case "per":
		c.Body = `Persian; Farsi`

  // ONIX local code, equivalent to pes in ISO 639-3. For use in ONIX 3.0 only
  case "pes":
		c.Body = `Iranian Persian; Parsi`

  // Collective name
  case "phi":
		c.Body = `Philippine languages`

  // Phoenician
  case "phn":
		c.Body = `Phoenician`

  // Pali
  case "pli":
		c.Body = `Pali`

  // ONIX local code for Italian dialect, equivalent to pms in ISO 639-3. For use in ONIX 3.0 only
  case "pms":
		c.Body = `Piedmontese`

  // Polish
  case "pol":
		c.Body = `Polish`

  // Ponapeian
  case "pon":
		c.Body = `Ponapeian`

  // Portuguese
  case "por":
		c.Body = `Portuguese`

  // Collective name
  case "pra":
		c.Body = `Prakrit languages`

  // Provençal, Old (to 1500); Occitan, Old (to 1500)
  case "pro":
		c.Body = `Provençal, Old (to 1500); Occitan, Old (to 1500)`

  // ONIX local code, equivalent to prs in ISO 639-3. For use in ONIX 3.0 only
  case "prs":
		c.Body = `Dari; Afghan Persian`

  // Macrolanguage
  case "pus":
		c.Body = `Pushto; Pashto`

  // ONIX local code, distinct dialect of Occitan (not distinguished from oci by ISO 639-3)
  case "qar":
		c.Body = `Aranés`

  // ONIX local code, distinct dialect of Catalan (not distinguished from cat by ISO 639-3)
  case "qav":
		c.Body = `Valencian`

  // ONIX local code, distinct variant of langue d’oïl (old northern French) (not distinguished from fro, or from frm, fre, nrf by ISO 639-3). For use in ONIX 3.0 only
  case "qgl":
		c.Body = `Gallo`

  // ONIX local code, distinct dialect of of Rusyn (not distinguished from rue by ISO 639-3). For use in ONIX 3.0 only
  case "qlk":
		c.Body = `Lemko`

  // ONIX local code, distinct and exclusively spoken variation of Spanish, not distinguished from spa (Spanish, Castilian) by ISO 639-3. Neutral Latin American Spanish should be considered a ‘shorthand’ for spa plus a ‘country code’ for Latin America – but prefer spa plus the relevant country code for specifically Mexican Spanish, Argentine (Rioplatense) Spanish, Puerto Rican Spanish etc. Neutral Latin American Spanish must only be used with audio material (including the audio tracks of TV, video and film) to indicate use of accent, vocabulary and construction suitable for broad use across Latin America. For use in ONIX 3.0 only
  case "qls":
		c.Body = `Neutral Latin American Spanish`

  // Macrolanguage
  case "que":
		c.Body = `Quechua`

  // Macrolanguage
  case "raj":
		c.Body = `Rajasthani`

  // Rapanui
  case "rap":
		c.Body = `Rapanui`

  // Rarotongan; Cook Islands Maori
  case "rar":
		c.Body = `Rarotongan; Cook Islands Maori`

  // ONIX local code, equivalent to rcf in ISO 639-3. For use in ONIX 3.0 only
  case "rcf":
		c.Body = `Réunion Creole French`

  // ONIX local code for Italian dialect, equivalent to rgl in ISO 639-3. For use in ONIX 3.0 only
  case "rgn":
		c.Body = `Romagnol`

  // Collective name
  case "roa":
		c.Body = `Romance languages`

  // Romansh
  case "roh":
		c.Body = `Romansh`

  // Macrolanguage
  case "rom":
		c.Body = `Romany`

  // Romanian
  case "rum":
		c.Body = `Romanian`

  // Rundi
  case "run":
		c.Body = `Rundi`

  // Aromanian; Arumanian; Macedo-Romanian
  case "rup":
		c.Body = `Aromanian; Arumanian; Macedo-Romanian`

  // Russian
  case "rus":
		c.Body = `Russian`

  // Sandawe
  case "sad":
		c.Body = `Sandawe`

  // Sango
  case "sag":
		c.Body = `Sango`

  // Yakut
  case "sah":
		c.Body = `Yakut`

  // Collective name
  case "sai":
		c.Body = `South American Indian languages`

  // Collective name
  case "sal":
		c.Body = `Salishan languages`

  // Samaritan Aramaic
  case "sam":
		c.Body = `Samaritan Aramaic`

  // Sanskrit
  case "san":
		c.Body = `Sanskrit`

  // Sasak
  case "sas":
		c.Body = `Sasak`

  // Santali
  case "sat":
		c.Body = `Santali`

  // DEPRECATED – use srp
  case "scc":
		c.Body = `Serbian`

  // Sicilian
  case "scn":
		c.Body = `Sicilian`

  // Scots
  case "sco":
		c.Body = `Scots`

  // DEPRECATED – use hrv
  case "scr":
		c.Body = `Croatian`

  // ONIX local code for Sardinian dialect, equivalent to sdc in ISO 639-3. For use in ONIX 3.0 only
  case "sdc":
		c.Body = `Sassarese`

  // ONIX local code for Sardinian dialect, equivalent to sdn in ISO 639-3. For use in ONIX 3.0 only
  case "sdn":
		c.Body = `Gallurese`

  // Selkup
  case "sel":
		c.Body = `Selkup`

  // Collective name
  case "sem":
		c.Body = `Semitic languages`

  // Irish, Old (to 1100)
  case "sga":
		c.Body = `Irish, Old (to 1100)`

  // Collective name
  case "sgn":
		c.Body = `Sign languages`

  // Shan
  case "shn":
		c.Body = `Shan`

  // Sidamo
  case "sid":
		c.Body = `Sidamo`

  // Sinhala; Sinhalese
  case "sin":
		c.Body = `Sinhala; Sinhalese`

  // Collective name
  case "sio":
		c.Body = `Siouan languages`

  // Collective name
  case "sit":
		c.Body = `Sino-Tibetan languages`

  // Collective name
  case "sla":
		c.Body = `Slavic languages`

  // Slovak
  case "slo":
		c.Body = `Slovak`

  // Slovenian
  case "slv":
		c.Body = `Slovenian`

  // Southern Sami
  case "sma":
		c.Body = `Southern Sami`

  // Northern Sami
  case "sme":
		c.Body = `Northern Sami`

  // Collective name
  case "smi":
		c.Body = `Sami languages`

  // Lule Sami
  case "smj":
		c.Body = `Lule Sami`

  // Inari Sami
  case "smn":
		c.Body = `Inari Sami`

  // Samoan
  case "smo":
		c.Body = `Samoan`

  // Skolt Sami
  case "sms":
		c.Body = `Skolt Sami`

  // Shona
  case "sna":
		c.Body = `Shona`

  // Sindhi
  case "snd":
		c.Body = `Sindhi`

  // Soninke
  case "snk":
		c.Body = `Soninke`

  // Sogdian
  case "sog":
		c.Body = `Sogdian`

  // Somali
  case "som":
		c.Body = `Somali`

  // Collective name
  case "son":
		c.Body = `Songhai languages`

  // Sotho; Sesotho
  case "sot":
		c.Body = `Sotho; Sesotho`

  // Spanish
  case "spa":
		c.Body = `Spanish`

  // Macrolanguage
  case "srd":
		c.Body = `Sardinian`

  // Sranan Tongo
  case "srn":
		c.Body = `Sranan Tongo`

  // ONIX local code for Sardinian dialect, equivalent to sro in ISO 639-3. For use in ONIX 3.0 only
  case "sro":
		c.Body = `Campidanese`

  // Serbian
  case "srp":
		c.Body = `Serbian`

  // Serer
  case "srr":
		c.Body = `Serer`

  // Collective name
  case "ssa":
		c.Body = `Nilo-Saharan languages`

  // Swazi; Swati
  case "ssw":
		c.Body = `Swazi; Swati`

  // Sukuma
  case "suk":
		c.Body = `Sukuma`

  // Sundanese
  case "sun":
		c.Body = `Sundanese`

  // Susu
  case "sus":
		c.Body = `Susu`

  // Sumerian
  case "sux":
		c.Body = `Sumerian`

  // Macrolanguage
  case "swa":
		c.Body = `Swahili`

  // Swedish
  case "swe":
		c.Body = `Swedish`

  // Classical Syriac
  case "syc":
		c.Body = `Classical Syriac`

  // Macrolanguage
  case "syr":
		c.Body = `Syriac`

  // Tahitian
  case "tah":
		c.Body = `Tahitian`

  // Collective name
  case "tai":
		c.Body = `Tai languages`

  // Tamil
  case "tam":
		c.Body = `Tamil`

  // Tatar
  case "tat":
		c.Body = `Tatar`

  // Telugu
  case "tel":
		c.Body = `Telugu`

  // Temne; Time
  case "tem":
		c.Body = `Temne; Time`

  // Terena
  case "ter":
		c.Body = `Terena`

  // Tetum
  case "tet":
		c.Body = `Tetum`

  // Tajik; Tajiki Persian
  case "tgk":
		c.Body = `Tajik; Tajiki Persian`

  // Tagalog
  case "tgl":
		c.Body = `Tagalog`

  // Thai
  case "tha":
		c.Body = `Thai`

  // Tibetan
  case "tib":
		c.Body = `Tibetan`

  // Tigré
  case "tig":
		c.Body = `Tigré`

  // Tigrinya
  case "tir":
		c.Body = `Tigrinya`

  // Tiv
  case "tiv":
		c.Body = `Tiv`

  // Tokelauan
  case "tkl":
		c.Body = `Tokelauan`

  // Artificial language
  case "tlh":
		c.Body = `Klingon; tlhIngan-Hol`

  // Tlingit
  case "tli":
		c.Body = `Tlingit`

  // Macrolanguage
  case "tmh":
		c.Body = `Tamashek`

  // Tonga (Nyasa)
  case "tog":
		c.Body = `Tonga (Nyasa)`

  // Tongan
  case "ton":
		c.Body = `Tongan`

  // Tok Pisin
  case "tpi":
		c.Body = `Tok Pisin`

  // Tsimshian
  case "tsi":
		c.Body = `Tsimshian`

  // AKA Setswana
  case "tsn":
		c.Body = `Tswana`

  // Tsonga
  case "tso":
		c.Body = `Tsonga`

  // Turkmen
  case "tuk":
		c.Body = `Turkmen`

  // Tumbuka
  case "tum":
		c.Body = `Tumbuka`

  // Collective name
  case "tup":
		c.Body = `Tupi languages`

  // Turkish
  case "tur":
		c.Body = `Turkish`

  // Altaic languages
  case "tut":
		c.Body = `Altaic languages`

  // Tuvaluan
  case "tvl":
		c.Body = `Tuvaluan`

  // Twi
  case "twi":
		c.Body = `Twi`

  // Tuvinian
  case "tyv":
		c.Body = `Tuvinian`

  // ONIX local code, equivalent to tzo in ISO 639-3
  case "tzo":
		c.Body = `Tzotzil`

  // Udmurt
  case "udm":
		c.Body = `Udmurt`

  // Ugaritic
  case "uga":
		c.Body = `Ugaritic`

  // Uighur; Uyghur
  case "uig":
		c.Body = `Uighur; Uyghur`

  // Ukrainian
  case "ukr":
		c.Body = `Ukrainian`

  // Umbundu
  case "umb":
		c.Body = `Umbundu`

  // Undetermined language
  case "und":
		c.Body = `Undetermined language`

  // Urdu
  case "urd":
		c.Body = `Urdu`

  // Macrolanguage
  case "uzb":
		c.Body = `Uzbek`

  // Vai
  case "vai":
		c.Body = `Vai`

  // ONIX local code for Italian dialect, equivalent to vec in ISO 639-3. For use in ONIX 3.0 only
  case "vec":
		c.Body = `Venetian/Venetan`

  // Venda
  case "ven":
		c.Body = `Venda`

  // Vietnamese
  case "vie":
		c.Body = `Vietnamese`

  // Artificial language
  case "vol":
		c.Body = `Volapük`

  // Votic
  case "vot":
		c.Body = `Votic`

  // Collective name
  case "wak":
		c.Body = `Wakashan languages`

  // Wolaitta; Wolaytta
  case "wal":
		c.Body = `Wolaitta; Wolaytta`

  // Waray
  case "war":
		c.Body = `Waray`

  // Washo
  case "was":
		c.Body = `Washo`

  // Welsh
  case "wel":
		c.Body = `Welsh`

  // Collective name
  case "wen":
		c.Body = `Sorbian languages`

  // Walloon
  case "wln":
		c.Body = `Walloon`

  // Wolof
  case "wol":
		c.Body = `Wolof`

  // Kalmyk
  case "xal":
		c.Body = `Kalmyk`

  // Xhosa
  case "xho":
		c.Body = `Xhosa`

  // ONIX local code, equivalent to xuu in ISO 639-3. For use in ONIX 3.0 only
  case "xuu":
		c.Body = `Khwedam, Kxoe`

  // Yao
  case "yao":
		c.Body = `Yao`

  // Yapese
  case "yap":
		c.Body = `Yapese`

  // Macrolanguage
  case "yid":
		c.Body = `Yiddish`

  // Yoruba
  case "yor":
		c.Body = `Yoruba`

  // Collective name
  case "ypk":
		c.Body = `Yupik languages`

  // ONIX local code, equivalent to yue in ISO 639-3
  case "yue":
		c.Body = `Cantonese`

  // Macrolanguage
  case "zap":
		c.Body = `Zapotec`

  // Artificial language
  case "zbl":
		c.Body = `Blissymbols; Blissymbolics; Bliss`

  // Zenaga
  case "zen":
		c.Body = `Zenaga`

  // Standard Moroccan Tamazight
  case "zgh":
		c.Body = `Standard Moroccan Tamazight`

  // Macrolanguage
  case "zha":
		c.Body = `Zhuang; Chuang`

  // Collective name
  case "znd":
		c.Body = `Zande languages`

  // Zulu
  case "zul":
		c.Body = `Zulu`

  // Zuni
  case "zun":
		c.Body = `Zuni`

  // No linguistic content
  case "zxx":
		c.Body = `No linguistic content`

  // Macrolanguage
  case "zza":
		c.Body = `Zaza; Dimili; Dimli; Kirdki; Kirmanjki; Zazaki`
	default:
		return fmt.Errorf("undefined code for DefaultLanguageOfText has been passed, got [%s]", v)
	}
	return nil
}

// DefaultPriceType Price type
type DefaultPriceType struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *DefaultPriceType) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // Recommended Retail Price, excluding any sales tax or value-added tax. Price recommended by the publisher or supplier for retail sales to the consumer. Also termed the Suggested Retail Price (SRP) or Maximum Suggested Retail Price (MSRP) in some countries. The retailer may choose to use this recommended price, or may choose to sell to the consumer at a lower (or occasionally, a higher) price which is termed the Actual Selling Price (ASP) in sales reports. The net price charged to the retailer depends on the RRP minus a trade discount (which may be customer-specific). Relevant tax detail must be calculated by the data recipient
  case "01":
		c.Body = `RRP excluding tax`

  // Recommended Retail Price, including sales or value-added tax where applicable. The net price charged to the retailer depends on the trade discount. Sales or value-added tax detail is usually supplied in the <Tax> composite
  case "02":
		c.Body = `RRP including tax`

  // Fixed Retail Price, excluding any sales or value-added tax, used in countries were retail price maintenance applies by law to certain products. Price fixed by the publisher or supplier for retail sales to the consumer. The retailer must use this price, or may vary the price only within certain legally-prescribed limits. The net price charged to the retailer depends on the FRP minus a customer-soecific trade discount. Relevant tax detail must be calculated by the data recipient
  case "03":
		c.Body = `FRP excluding tax`

  // Fixed Retail Price, including any sales or value-added tax where applicable, used in countries were retail price maintenance applies by law to certain products. The net price charged to the retailer depends on the trade discount. Sales or value-added tax detail is usually supplied in the <Tax> composite
  case "04":
		c.Body = `FRP including tax`

  // Net or wholesale price, excluding any sales or value-added tax. Unit price charged by supplier for business-to-business transactions, without any direct relationship to the price for retail sales to the consumer, but sometimes subject to a further customer-specific trade discount based on volume. Relevant tax detail must be calculated by the data recipient
  case "05":
		c.Body = `Supplier’s Net price excluding tax`

  // Unit price charged by supplier to reseller / rental outlet, excluding any sales tax or value-added tax: goods for rental (used for video and DVD)
  case "06":
		c.Body = `Supplier’s Net price excluding tax: rental goods`

  // Net or wholesale price, including any sales or value-added tax where applicable. Unit price charged by supplier for business-to-business transactions, without any direct relationship to the price for retail sales to the consumer, but sometimes subject to a further customer-specific trade discount based on volume. Sales or value-added tax detail is usually supplied in the <Tax> composite
  case "07":
		c.Body = `Supplier’s Net price including tax`

  // Net or wholesale price charged by supplier to a specified class of reseller, excluding any sales tax or value-added tax. Relevant tax detail must be calculated by the data recipient. (This value is for use only in countries, eg Finland, where trade practice requires two different Net prices to be listed for different classes of resellers, and where national guidelines specify how the code should be used)
  case "08":
		c.Body = `Supplier’s alternative Net price excluding tax`

  // Net or wholesale price charged by supplier to a specified class of reseller, including any sales tax or value-added tax. Sales or value-added tax detail is usually supplied in the <Tax> composite. (This value is for use only in countries, eg Finland, where trade practice requires two different Net prices to be listed for different classes of resellers, and where national guidelines specify how the code should be used)
  case "09":
		c.Body = `Supplier’s alternative net price including tax`

  // Special sale RRP excluding any sales tax or value-added tax. Note ‘special sales’ are sales where terms and conditions are different from normal trade sales, when for example products that are normally sold on a sale-or-return basis are sold on firm-sale terms, where a particular product is tailored for a specific retail outlet (often termed a ‘premium’ product), or where other specific conditions or qualiifications apply. Further details of the modified terms and conditions should be given in <PriceTypeDescription>
  case "11":
		c.Body = `Special sale RRP excluding tax`

  // Special sale RRP including sales or value-added tax if applicable
  case "12":
		c.Body = `Special sale RRP including tax`

  // In countries where retail price maintenance applies by law to certain products: not used in USA
  case "13":
		c.Body = `Special sale fixed retail price excluding tax`

  // In countries where retail price maintenance applies by law to certain products: not used in USA
  case "14":
		c.Body = `Special sale fixed retail price including tax`

  // Unit price charged by supplier to reseller for special sale excluding any sales tax or value-added tax
  case "15":
		c.Body = `Supplier’s net price for special sale excluding tax`

  // Unit price charged by supplier to reseller for special sale including any sales tax or value-added tax
  case "17":
		c.Body = `Supplier’s net price for special sale including tax`

  // Pre-publication RRP excluding any sales tax or value-added tax. Use where RRP for pre-orders is different from post-publication RRP
  case "21":
		c.Body = `Pre-publication RRP excluding tax`

  // Pre-publication RRP including sales or value-added tax if applicable. Use where RRP for pre-orders is different from post-publication RRP
  case "22":
		c.Body = `Pre-publication RRP including tax`

  // In countries where retail price maintenance applies by law to certain products: not used in USA
  case "23":
		c.Body = `Pre-publication fixed retail price excluding tax`

  // In countries where retail price maintenance applies by law to certain products: not used in USA
  case "24":
		c.Body = `Pre-publication fixed retail price including tax`

  // Unit price charged by supplier to reseller pre-publication excluding any sales tax or value-added tax
  case "25":
		c.Body = `Supplier’s pre-publication net price excluding tax`

  // Unit price charged by supplier to reseller pre-publication including any sales tax or value-added tax
  case "27":
		c.Body = `Supplier’s pre-publication net price including tax`

  // In the US, books are sometimes supplied on ‘freight-pass-through’ terms, where a price that is different from the RRP is used as the basis for calculating the supplier’s charge to a reseller. To make it clear when such terms are being invoked, code 31 is used instead of code 01 to indicate the RRP. Code 32 is used for the ‘billing price’
  case "31":
		c.Body = `Freight-pass-through RRP excluding tax`

  // When freight-pass-through terms apply, the price on which the supplier’s charge to a reseller is calculated, ie the price to which trade discount terms are applied. See also code 31
  case "32":
		c.Body = `Freight-pass-through billing price excluding tax`

  // In countries where retail price maintenance applies by law to certain products, but the price is set by the importer or local sales agent, not the foreign publisher. In France, ‘prix catalogue éditeur étranger’
  case "33":
		c.Body = `Importer’s Fixed retail price excluding tax`

  // In countries where retail price maintenance applies by law to certain products, but the price is set by the importer or local sales agent, not the foreign publisher. In France, ‘prix catalogue éditeur étranger’
  case "34":
		c.Body = `Importer’s Fixed retail price including tax`

  // For a product supplied on agency terms, the retail price set by the publisher, excluding any sales tax or value-added tax
  case "41":
		c.Body = `Publishers retail price excluding tax`

  // For a product supplied on agency terms, the retail price set by the publisher, including sales or value-added tax if applicable
  case "42":
		c.Body = `Publishers retail price including tax`
	default:
		return fmt.Errorf("undefined code for DefaultPriceType has been passed, got [%s]", v)
	}
	return nil
}

// DiscountCodeType Discount code type
type DiscountCodeType struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *DiscountCodeType) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // UK publisher’s or distributor’s discount group code in a format specified by BIC to ensure uniqueness (a five-letter prefix allocated by BIC, plus one to three alphanumeric characters – normally digits – chosen by the supplier)
  case "01":
		c.Body = `BIC discount group code`

  // A publisher’s or supplier’s own code which identifies a trade discount category, as specified in <DiscountCodeTypeName>. The actual discount for each code is set by trading partner agreement (applies to goods supplied on standard trade discounting terms)
  case "02":
		c.Body = `Proprietary discount code`

  // Terms code used in the Netherlands book trade
  case "03":
		c.Body = `Boeksoort`

  // Terms code used in German ONIX applications
  case "04":
		c.Body = `German terms code`

  // A publisher’s or supplier’s own code which identifies a commission rate category, as specified in <DiscountCodeTypeName>. The actual commission rate for each code is set by trading partner agreement (applies to goods supplied on agency terms)
  case "05":
		c.Body = `Proprietary commission code`

  // UK publisher’s or distributor’s commission group code in format specified by BIC to ensure uniqueness. Format is identical to BIC discount group code, but indicates a commission rather than a discount (applies to goods supplied on agency terms)
  case "06":
		c.Body = `BIC commission group code`

  // ISNI-based discount group scheme devised initially by the German IG ProduktMetadaten, in a format comprised of the supplier’s 16-digit ISNI, followed by a hyphen and one to three alphanumeric characters – normally digits – chosen by the supplier. These characters are the index to a discount percentage in a table shared in advance by the supplier with individual customers. In this way, a supplier may maintain individual product-specific discount arrangements with each customer. For use in ONIX 3.0 only
  case "07":
		c.Body = `ISNI-based discount group code`
	default:
		return fmt.Errorf("undefined code for DiscountCodeType has been passed, got [%s]", v)
	}
	return nil
}

// DiscountType Discount type
type DiscountType struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *DiscountType) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // Discount applied to all units in a qualifying order. The default if no <DiscountType> is specified
  case "01":
		c.Body = `Rising discount`

  // Additional discount may be applied retrospectively, based on number of units ordered over a specific period
  case "02":
		c.Body = `Rising discount (cumulative)`

  // Discount applied to marginal units in a qualifying order
  case "03":
		c.Body = `Progressive discount`

  // Previous orders within a specific time period are counted when calculating a progressive discount
  case "04":
		c.Body = `Progressive discount (cumulative)`
	default:
		return fmt.Errorf("undefined code for DiscountType has been passed, got [%s]", v)
	}
	return nil
}

// EditionType Edition type
type EditionType struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *EditionType) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // Content has been shortened: use for abridged, shortened, concise, condensed
  case "ABR":
		c.Body = `Abridged edition`

  // Version of a play or script intended for use of those directly involved in a production, usually including full stage directions in addition to the text of the script
  case "ACT":
		c.Body = `Acting edition`

  // Content has been adapted to serve a different purpose or audience, or from one medium to another: use for dramatization, novelization etc. Use <EditionStatement> to describe the exact nature of the adaptation
  case "ADP":
		c.Body = `Adapted edition`

  // Do not use. This code is now DEPRECATED, but is retained in the list for reasons of backwards compatibility
  case "ALT":
		c.Body = `Alternate`

  // Content is augmented by the addition of notes
  case "ANN":
		c.Body = `Annotated edition`

  // Both languages should be specified in the <Language> group. Use MLL for an edition in more than two languages
  case "BLL":
		c.Body = `Bilingual edition`

  // Use only where the two languages are presented in parallel on facing pages, or in parallel columns of text on a single page (otherwise use BLL). Both languages should be specified in the <Language> group
  case "BLP":
		c.Body = `Bilingual ‘facing page’ edition`

  // Braille edition
  case "BRL":
		c.Body = `Braille edition`

  // An edition in which two or more works also published separately are combined in a single volume; AKA ‘omnibus’ edition (fr: ‘intégrale’)
  case "CMB":
		c.Body = `Combined volume`

  // Content includes critical commentary on the text
  case "CRI":
		c.Body = `Critical edition`

  // Content was compiled for a specified educational course
  case "CSP":
		c.Body = `Coursepack`

  // A digital product that, at the time of publication, has or had no print counterpart and that is or was not expected to have a print counterpart for a reasonable time (recommended at least 30 days following publication)
  case "DGO":
		c.Body = `Digital original`

  // Use for e-publications that have been enhanced with additional text, speech, other audio, video, interactive or other content
  case "ENH":
		c.Body = `Enhanced edition`

  // Content has been enlarged or expanded from that of a previous edition
  case "ENL":
		c.Body = `Enlarged edition`

  // ‘Offensive’ content has been removed
  case "EXP":
		c.Body = `Expurgated edition`

  // Exact reproduction of the content and format of a previous edition
  case "FAC":
		c.Body = `Facsimile edition`

  // A collection of writings published in honor of a person, an institution or a society
  case "FST":
		c.Body = `Festschrift`

  // Edition optimised for high readability, typically featuring colored or tinted page backgrounds to reduce contrast, extra letter, word and line spacing to reduce crowding and isolate individual words, simplified page layouts and an open, sans serif font (or occasionally, an unusual font design) intended to aid readability. Sometimes labelled ‘dyslexia-friendly’. See also code SMP if the text itself is simplified, and codes LTE or ULP if the type size is significantly larger than normal. For use in ONIX 3.0 only
  case "HRE":
		c.Body = `High readability edition`

  // Content includes extensive illustrations which are not part of other editions
  case "ILL":
		c.Body = `Illustrated edition`

  // A product aimed specifically at markets other than the country of original publication, usually titled as an ‘International edition’ and with specification and/or content changes
  case "INT":
		c.Body = `International edition`

  // Large print edition, print sizes 14 to 19pt – see also ULP
  case "LTE":
		c.Body = `Large type / large print edition`

  // A printed edition in a type size too small to be read without a magnifying glass
  case "MCP":
		c.Body = `Microprint edition`

  // An edition published to coincide with the release of a film, TV program, or electronic game based on the same work. Use <EditionStatement> to describe the exact nature of the tie-in
  case "MDT":
		c.Body = `Media tie-in`

  // All languages should be specified in the ‘Language’ group. Use BLL for a bilingual edition
  case "MLL":
		c.Body = `Multilingual edition`

  // Where no other information is given, or no other coded type or edition numbering is applicable
  case "NED":
		c.Body = `New edition`

  // A limited edition in which each copy is individually numbered, and the actual number of copies is strictly limited. Use <EditionStatement> to give details of the number of copies printed
  case "NUM":
		c.Body = `Edition with numbered copies`

  // In the US, a book that was previously bound, normally as a paperback, and has been rebound with a library-quality hardcover binding by a supplier other than the original publisher. See also the <Publisher> and <RelatedProduct> composites for other aspects of the treatment of prebound editions in ONIX
  case "PRB":
		c.Body = `Prebound edition`

  // Content has been revised from that of a previous edition (often used when there has been no corresponding increment in the edition number, or no edition numbering is available)
  case "REV":
		c.Body = `Revised edition`

  // An edition intended specifically for use in schools
  case "SCH":
		c.Body = `School edition`

  // Individually autographed by the author(s)
  case "SIG":
		c.Body = `Signed edition`

  // An edition that uses simplified language (Finnish ‘Selkokirja’)
  case "SMP":
		c.Body = `Simplified language edition`

  // Use for anniversary, collectors’, de luxe, gift, limited (but prefer codes NUM or UNN as appropriate), autographed (but prefer code SIG as appropriate) edition. Use <EditionStatement> to describe the exact nature of the special edition
  case "SPE":
		c.Body = `Special edition`

  // Where a text is available in both student and teacher’s editions
  case "STU":
		c.Body = `Student edition`

  // Where a text is available in both student and teacher’s editions; use also for instructor’s or leader’s editions, and for editions intended exclusively for educators where no specific student edition is available
  case "TCH":
		c.Body = `Teacher’s edition`

  // Where a title has also been published in an abridged edition; also for audiobooks, regardless of whether an abridged audio version also exists
  case "UBR":
		c.Body = `Unabridged edition`

  // For print sizes 20pt and above, and with typefaces designed for the visually impaired – see also LTE
  case "ULP":
		c.Body = `Ultra large print edition`

  // A limited edition in which each copy is not individually numbered – but where the actual number of copies is strictly limited. Use <EditionStatement> to give details of the number of copies printed
  case "UNN":
		c.Body = `Edition with unnumbered copies`

  // Content previously considered ‘offensive’ has been restored
  case "UXP":
		c.Body = `Unexpurgated edition`

  // Content includes notes by various commentators, and/or includes and compares several variant texts of the same work
  case "VAR":
		c.Body = `Variorum edition`
	default:
		return fmt.Errorf("undefined code for EditionType has been passed, got [%s]", v)
	}
	return nil
}

// EpubLicenseExpressionType License expression type
type EpubLicenseExpressionType struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *EpubLicenseExpressionType) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // Document (eg Word file, PDF or web page) Intended for the lay reader
  case "01":
		c.Body = `Human readable`

  // Document (eg Word file, PDF or web page) Intended for the legal specialist reader
  case "02":
		c.Body = `Professional readable`

  // ONIX-PL
  case "10":
		c.Body = `ONIX-PL`
	default:
		return fmt.Errorf("undefined code for EpubLicenseExpressionType has been passed, got [%s]", v)
	}
	return nil
}

// EpubTechnicalProtection E-publication technical protection
type EpubTechnicalProtection struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *EpubTechnicalProtection) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // Has no technical protection
  case "00":
		c.Body = `None`

  // Has DRM protection
  case "01":
		c.Body = `DRM`

  // Has digital watermarking
  case "02":
		c.Body = `Digital watermarking`

  // Has DRM protection applied by the Adobe CS4 Content Server Package or by the Adobe ADEPT hosted service
  case "03":
		c.Body = `Adobe DRM`

  // Has FairPlay DRM protection applied via Apple proprietary online store
  case "04":
		c.Body = `Apple DRM`

  // Has OMA v2 DRM protection applied, as used to protect some mobile phone content
  case "05":
		c.Body = `OMA DRM`

  // Has Licensed Content Protection DRM applied by a Readium License Server
  case "06":
		c.Body = `Readium LCP DRM`

  // Has Sony DADC User Rights Management (URMS) DRM protection applied
  case "07":
		c.Body = `Sony DRM`
	default:
		return fmt.Errorf("undefined code for EpubTechnicalProtection has been passed, got [%s]", v)
	}
	return nil
}

// EpubUsageStatus Usage status
type EpubUsageStatus struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *EpubUsageStatus) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // Permitted unlimited
  case "01":
		c.Body = `Permitted unlimited`

  // Limit should be specified in <EpubUsageLimit> or <PriceConstraintLimit>
  case "02":
		c.Body = `Permitted subject to limit`

  // Prohibited
  case "03":
		c.Body = `Prohibited`
	default:
		return fmt.Errorf("undefined code for EpubUsageStatus has been passed, got [%s]", v)
	}
	return nil
}

// EpubUsageType Usage type
type EpubUsageType struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *EpubUsageType) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // Allows positive indication that there are no particular constraints (that can be specifed in <EpubUsageConstraint>) – the default if <EpubUsageConstraint> is omitted
  case "00":
		c.Body = `No constraints`

  // Preview before purchase. Allows a retail customer, account holder or patron to view or listen to a proportion of the book before purchase. Also applies to borrowers making use of ‘acquisition on demand’ models in libraries, and to ‘subscription’ models where the purchase is made on behalf of the reader
  case "01":
		c.Body = `Preview`

  // Print paper copy of extract
  case "02":
		c.Body = `Print`

  // Make digital copy of extract
  case "03":
		c.Body = `Copy / paste`

  // Share product across multiple concurrent devices. Allows a retail customer, account holder or patron to read the book across multiple devices linked to the same account. Also applies to readers in library borrowing and ‘subscription’ models
  case "04":
		c.Body = `Share`

  // ‘Read aloud’ with text to speech functionality
  case "05":
		c.Body = `Text to speech`

  // Lendable by the purchaser to other device owner or account holder or patron, eg ‘Lend-to-a-friend’, library lending (where the library product has a separate <ProductIdentifier> from the consumer product). The ‘primary’ copy becomes unusable while the secondary copy is ‘on loan’ unless a number of concurrent borrowers is also specified
  case "06":
		c.Body = `Lend`

  // E-publication license is time limited. Use with 02 from List 146 and either a time period in days, weeks or months, or a Valid until date in <EpubUsageLimit>. The purchased copy becomes unusable when the license expires
  case "07":
		c.Body = `Time-limited license`

  // Maximum number of consecutive loans or loan extensions (eg from a library) to a single device owner or account holder. Note that a limit of 1 indicates that a loan cannot be renewed or extended
  case "08":
		c.Body = `Loan renewal`

  // E-publication license is multi-user. Maximum number of concurrent users licensed to use the product should be given in <EpubUsageLimit>
  case "09":
		c.Body = `Multi-user license`

  // Preview locally before purchase. Allows a retail customer, account holder or patron to view a proportion of the book (or the whole book, if no proportion is specified) before purchase, but ONLY while located physically in the retailer’s store (eg while logged on to the store or library wifi). Also applies to patrons making use of ‘acquisition on demand’ models in libraries
  case "10":
		c.Body = `Preview on premises`
	default:
		return fmt.Errorf("undefined code for EpubUsageType has been passed, got [%s]", v)
	}
	return nil
}

// EpubUsageUnit Unit of usage
type EpubUsageUnit struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *EpubUsageUnit) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // Maximum number of copies that may be made of a permitted extract
  case "01":
		c.Body = `Copies`

  // Maximum number of characters in a permitted extract for a specified usage
  case "02":
		c.Body = `Characters`

  // Maximum number of words in a permitted extract for a specified usage
  case "03":
		c.Body = `Words`

  // Maximum number of pages in a permitted extract for a specified usage
  case "04":
		c.Body = `Pages`

  // Maximum percentage of total content in a permitted extract for a specified usage
  case "05":
		c.Body = `Percentage`

  // Maximum number of devices in ‘share group’
  case "06":
		c.Body = `Devices`

  // Maximum number of concurrent users. NB where the number of concurrent users is specifically not limited, set the number of concurrent users to zero
  case "07":
		c.Body = `Concurrent users`

  // Maximum number of licensed individual users, independent of concurrency of use
  case "15":
		c.Body = `Users`

  // A ‘class’ is a group of learners attending a specific course or lesson and generally taught as a group
  case "19":
		c.Body = `Concurrent classes`

  // Maximum number of classes of learners, independent of concurrency of use
  case "20":
		c.Body = `Classes`

  // Maximum percentage of total content which may be used in a specified usage per time period; the time period being specified as another <EpubUsageLimit> Quantity
  case "08":
		c.Body = `Percentage per time period`

  // Maximum time period in days (beginning from product purchase or activation)
  case "09":
		c.Body = `Days`

  // Maximum time period in weeks
  case "13":
		c.Body = `Weeks`

  // Maximum time period in months
  case "14":
		c.Body = `Months`

  // Maximum amount of time in hours, minutes and seconds allowed in a permitted extract for a specified usage, in the format HHHMMSS (7 digits, with leading zeros if necessary)
  case "16":
		c.Body = `Hours minutes and seconds`

  // Maximum time period in days (beginning from the product publication date). In effect, this defines a fixed end date for the license independent of the purchase or activation date
  case "27":
		c.Body = `Days (fixed start)`

  // Maximum time period in weeks
  case "28":
		c.Body = `Weeks (fixed start)`

  // Maximum time period in months
  case "29":
		c.Body = `Months (fixed start)`

  // Maximum number of times a specified usage event may occur (in the lifetime of the product)
  case "10":
		c.Body = `Times`

  // Maximum frequency a specified usage event may occur (per day)
  case "22":
		c.Body = `Times per day`

  // Maximum frequency a specified usage event may occur (per month)
  case "23":
		c.Body = `Times per month`

  // Maximum frequency a specified usage event may occur (per year)
  case "24":
		c.Body = `Times per year`

  // Maximum resolution of printed or copy/pasted extracts
  case "21":
		c.Body = `Dots per inch`

  // Maximum resolution of printed or copy/pasted extracts
  case "26":
		c.Body = `Dots per cm`

  // Page number where allowed usage begins. <Quantity> should contain an absolute page number, counting the cover as page 1. (This type of page numbering should not be used where the e-publication has no fixed pagination). Use with (max number of) Pages, Percentage of content, or End page to specify pages allowed in Preview
  case "11":
		c.Body = `Allowed usage start page`

  // Page number at which allowed usage ends. <Quantity> should contain an absolute page number, counting the cover as page 1. (This type of page numbering should not be used where the e-publication has no fixed pagination). Use with Start page to specify pages allowed in a preview
  case "12":
		c.Body = `Allowed usage end page`

  // Time at which allowed usage begins. <Quantity> should contain an absolute time, counting from the beginning of an audio or video product, in the format HHHMMSS or HHHMMSScc. Use with Time, Percentage of content, or End time to specify time-based extract allowed in Preview
  case "17":
		c.Body = `Allowed usage start time`

  // Time at which allowed usage ends. <Quantity> should contain an absolute time, counting from the beginning of an audio or video product, in the format HHHMMSS or HHHMMSScc. Use with Start time to specify time-based extract allowed in Preview
  case "18":
		c.Body = `Allowed usage end time`

  // The date from which the usage constraint applies. <Quantity> is in the format YYYYMMDD
  case "98":
		c.Body = `Valid from`

  // The date until which the usage constraint applies. <Quantity> is in the format YYYYMMDD
  case "99":
		c.Body = `Valid to`
	default:
		return fmt.Errorf("undefined code for EpubUsageUnit has been passed, got [%s]", v)
	}
	return nil
}

// EventIDType Event identifier type
type EventIDType struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *EventIDType) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // Proprietary
  case "01":
		c.Body = `Proprietary`
	default:
		return fmt.Errorf("undefined code for EventIDType has been passed, got [%s]", v)
	}
	return nil
}

// EventRole Event role
type EventRole struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *EventRole) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // For example an academic, professional or political conference
  case "01":
		c.Body = `Publication linked to conference`

  // Complete proceedings of conference
  case "02":
		c.Body = `Complete proceedings of conference`

  // Selected papers from conference
  case "03":
		c.Body = `Selected papers from conference`

  // For example a competitive match, fixture series or championship
  case "11":
		c.Body = `Publication linked to sporting event`

  // Programme or guide for sporting event
  case "12":
		c.Body = `Programme or guide for sporting event`

  // For example a theatrical or musical event or performance, a season of events or performances, or an exhibition of art
  case "21":
		c.Body = `Publication linked to artistic event`

  // Programme or guide for artistic event
  case "22":
		c.Body = `Programme or guide for artistic event`

  // For example a commercial exposition
  case "31":
		c.Body = `Publication linked to exposition`

  // Programme or guide for exposition
  case "32":
		c.Body = `Programme or guide for exposition`
	default:
		return fmt.Errorf("undefined code for EventRole has been passed, got [%s]", v)
	}
	return nil
}

// EventSponsorIDType Name identifier type
type EventSponsorIDType struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *EventSponsorIDType) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // Note that <IDTypeName> is required with proprietary identifiers
  case "01":
		c.Body = `Proprietary`

  // DEPRECATED – use 01
  case "02":
		c.Body = `Proprietary`

  // Deutsche Nationalbibliothek publisher identifier
  case "03":
		c.Body = `DNB publisher identifier`

  // Börsenverein Verkehrsnummer
  case "04":
		c.Body = `Börsenverein Verkehrsnummer`

  // German ISBN Agency publisher identifier
  case "05":
		c.Body = `German ISBN Agency publisher identifier`

  // GS1 global location number (formerly EAN location number)
  case "06":
		c.Body = `GLN`

  // Book trade Standard Address Number – US, UK etc
  case "07":
		c.Body = `SAN`

  // MARC code list for organizations – see http://www.loc.gov/marc/organizations/orgshome.html
  case "08":
		c.Body = `MARC organization code`

  // Trading party identifier used in the Netherlands
  case "10":
		c.Body = `Centraal Boekhuis Relatie ID`

  // Flemish publisher code
  case "13":
		c.Body = `Fondscode Boekenbank`

  // Business Identity Code (Finland). See http://www.ytj.fi/ (in Finnish)
  case "15":
		c.Body = `Y-tunnus`

  // International Standard Name Identifier. A sixteen digit number. Usually presented with spaces or hyphens dividing the number into four groups of four digits, but in ONIX the spaces or hyphens should be omitted. See http://www.isni.org/
  case "16":
		c.Body = `ISNI`

  // Personennamendatei – person name authority file used by Deutsche Nationalbibliothek and in other German-speaking countries. See http://www.dnb.de/standardisierung/normdateien/pnd.htm (German) or http://www.dnb.de/eng/standardisierung/normdateien/pnd.htm (English). DEPRECATED in favor of the GND
  case "17":
		c.Body = `PND`

  // A control number assigned to a Library of Congress Control Number (LCCN) Name Authority / NACO record
  case "18":
		c.Body = `NACO`

  // Publisher identifier administered by Japanese ISBN Agency
  case "19":
		c.Body = `Japanese Publisher identifier`

  // Gemeinsame Körperschaftsdatei – Corporate Body Authority File in the German-speaking countries. See http://www.dnb.de/standardisierung/normdateien/gkd.htm (German) or http://www.dnb.de/eng/standardisierung/normdateien/gkd.htm (English). DEPRECATED in favor of the GND
  case "20":
		c.Body = `GKD`

  // Open Researcher and Contributor ID. A sixteen digit number. Usually presented with hyphens dividing the number into four groups of four digits, but in ONIX the hyphens should be omitted. See http://www.orcid.org/
  case "21":
		c.Body = `ORCID`

  // Publisher identifier maintained by the Chinese ISBN Agency (GAPP)
  case "22":
		c.Body = `GAPP Publisher Identifier`

  // Identifier for a business organization for VAT purposes, eg within the EU’s VIES system. See http://ec.europa.eu/taxation_customs/vies/faqvies.do for EU VAT ID formats, which vary from country to country. Generally these consist of a two-letter country code followed by the 8–12 digits of the national VAT ID. Some countries include one or two letters within their VAT ID. See http://en.wikipedia.org/wiki/VAT_identification_number for non-EU countries that maintain similar identifiers. Spaces, dashes etc should be omitted
  case "23":
		c.Body = `VAT Identity Number`

  // 4-digit business organization identifier controlled by the Japanese Publication Wholesalers Association
  case "24":
		c.Body = `JP Distribution Identifier`

  // Gemeinsame Normdatei – Joint Authority File in the German-speaking countries. See http://www.dnb.de/EN/gnd (English). Combines the PND, SWD and GKD into a single authority file, and should be used in preference
  case "25":
		c.Body = `GND`

  // Dunn and Bradstreet Universal Numbering System, see http://www.dnb.co.uk/dandb-duns-number
  case "26":
		c.Body = `DUNS`

  // Ringgold organizational identifier, see http://www.ringgold.com/identify.html
  case "27":
		c.Body = `Ringgold ID`

  // French Electre publisher identifier
  case "28":
		c.Body = `Identifiant Editeur Electre`

  // DOI used in EIDR party registry, for example ‘10.5237/C9F6-F41F’ (Sam Raimi). See http://eidr.org
  case "29":
		c.Body = `EIDR Party DOI`

  // French Electre imprint Identifier
  case "30":
		c.Body = `Identifiant Marque Electre`

  // Virtual Internet Authority File. <IDValue> should be a number. The URI form of the identifier can be created by prefixing the number with ‘https://viaf.org/viaf/’. See https://viaf.org
  case "31":
		c.Body = `VIAF ID`

  // DOI used in CrossRef’s Open Funder Registry list of academic research funding bodies, for example ‘10.13039/100004440’ (Wellcome Trust). See http://www.crossref.org/fundingdata/registry.html
  case "32":
		c.Body = `FundRef DOI`

  // Control number assigned to a Name Authority record by the Biblioteca Nacional de España
  case "33":
		c.Body = `BNE CN`

  // Numéro de la notice de personne BNF
  case "34":
		c.Body = `BNF Control Number`

  // Archival Resource Key, as a URL (including the address of the ARK resolver provided by eg a national library)
  case "35":
		c.Body = `ARK`

  // Nasjonalt autoritetsregister for navn – Norwegian national authority file for personal and corporate names. For use in ONIX 3.0 only
  case "36":
		c.Body = `Nasjonalt autoritetsregister`

  // Global Research Identifier Database ID (see https://www.grid.ac). For use in ONIX 3.0 only
  case "37":
		c.Body = `GRID`

  // Party ID from Identifiers and Standards for Higher Education and Research (fr: Identifiants et Référentiels pour l’enseignement supérieur et la recherche). For use on ONIX 3.0 only. See https://www.idref.fr
  case "38":
		c.Body = `IDRef`
	default:
		return fmt.Errorf("undefined code for EventSponsorIDType has been passed, got [%s]", v)
	}
	return nil
}

// EventStatus Event status
type EventStatus struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *EventStatus) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // Announced
  case "A":
		c.Body = `Announced`

  // Abandoned after having previously been announced
  case "C":
		c.Body = `Cancelled`
	default:
		return fmt.Errorf("undefined code for EventStatus has been passed, got [%s]", v)
	}
	return nil
}

// EventType Event type
type EventType struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *EventType) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // Unspecified – see description
  case "00":
		c.Body = `Unspecified – see description`

  // Book signing
  case "01":
		c.Body = `Book signing`

  // Book reading
  case "02":
		c.Body = `Book reading`
	default:
		return fmt.Errorf("undefined code for EventType has been passed, got [%s]", v)
	}
	return nil
}

// ExtentType Extent type
type ExtentType struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *ExtentType) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // The highest-numbered page in a single numbered sequence of main content, usually the highest Arabic-numbered page in a book; or, for books without page numbers or (rarely) with multiple numbered sequences of main content, the total number of pages that carry the main content of the book. Note that this may include numbered but otherwise blank pages (eg pages inserted to ensure chapters start on a recto page) and may exclude unnumbered (but contentful) pages such as those in inserts/plate sections. It should exclude pages of back matter (eg any index) even when their numbering sequence continues from the main content. Either this or the Content Page count is the preferred page count for most books for the general reader. For books with substantial front and/or back matter, include also Front matter (03) and Back matter (04) page counts, or Total numbered pages (05). For books with inserts (plate sections), also include Total unnumbered insert page count whenever possible
  case "00":
		c.Body = `Main content page count`

  // Number of words or characters of natural language text
  case "02":
		c.Body = `Total text length`

  // The total number of numbered (usually Roman-numbered) pages that precede the main content of a book. This usually consists of various title and imprint pages, table of contents, an introduction, preface, foreword, etc
  case "03":
		c.Body = `Front matter page count`

  // The total number of numbered (often Roman-numbered) pages that follow the main content of a book. This usually consists of an afterword, appendices, endnotes, index, etc. It excludes extracts or ‘teaser’ material from other works, and blank (or advertising) pages that are present only for convenience of printing and binding
  case "04":
		c.Body = `Back matter page count`

  // The sum of all Roman- and Arabic-numbered pages. Note that this may include numbered but otherwise blank pages (eg pages inserted to ensure chapters start on a recto page) and may exclude unnumbered (but contentful) pages such as those in inserts/plate sections. It is the sum of the main content (00), front matter (03) and back matter (04) page counts
  case "05":
		c.Body = `Total numbered pages`

  // The total number of pages in a book, including unnumbered pages, front matter, back matter, etc. This includes any extracts or ‘teaser’ material from other works, and blank pages at the back that carry no content and are present only for convenience of printing and binding
  case "06":
		c.Body = `Production page count`

  // The total number of pages of the book counting the cover as page 1. This page count type should be used only for digital publications delivered with fixed pagination
  case "07":
		c.Body = `Absolute page count`

  // The total number of pages (equivalent to the Content page count, code 11) in the print counterpart of a digital product delivered without fixed pagination, or of an audio product
  case "08":
		c.Body = `Number of pages in print counterpart`

  // Total duration in time, expressed in the specified extent unit. This is the ‘running time’ equivalent of code 11
  case "09":
		c.Body = `Duration`

  // An estimate of the number of ‘pages’ in a digital product delivered without fixed pagination, and with no print counterpart, given as an indication of the size of the work. Equivalent to code 08, but exclusively for digital or audio products
  case "10":
		c.Body = `Notional number of pages in digital product`

  // The sum of all Roman- and Arabic-numbered and contentful unnumbered pages. Sum of page counts with codes 00, 03, 04 and 12, and also the sum of 05 and 12
  case "11":
		c.Body = `Content page count`

  // The total number of unnumbered pages with content inserted within the main content of a book – for example inserts/plate sections that are not numbered
  case "12":
		c.Body = `Total unnumbered insert page count`

  // Duration in time, expressed in the specified extent units, of introductory matter. This is the ‘running time’ equivalent of code 03, and comprises any significant amount of running time represented by a musical intro, announcements, titles, introduction or other material prefacing the main content
  case "13":
		c.Body = `Duration of introductory matter`

  // Duration in time, expressed in the specified extent units, of the main content. This is the ‘running time’ equivalent of code 00, and excludes time represented by announcements, titles, introduction or other prefatory material or ‘back matter’
  case "14":
		c.Body = `Duration of main content`

  // Duration in time, expressed in the specified extent units, of any content that follows the main content of a book. This may consist of an afterword, appendices, endnotes, end music etc. It excludes extracts or ‘teaser’ material from other works. This is the ‘running time’ equivalent of code 04
  case "15":
		c.Body = `Duration of back matter`

  // Duration in time, expressed in the specified extent units, of the complete content of a book. This is the ‘running time’ equivalent of code 06, and includes time represented by musical themes, announcements, titles, introductory and other prefatory material, plus ‘back matter’ such as any afterword, appendices, plus any extracts or ‘teaser’ material from other works
  case "16":
		c.Body = `Production duration`

  // Approximate size of a digital file, expressed in the specified extent unit
  case "22":
		c.Body = `Filesize`
	default:
		return fmt.Errorf("undefined code for ExtentType has been passed, got [%s]", v)
	}
	return nil
}

// ExtentUnit Extent unit
type ExtentUnit struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *ExtentUnit) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // Approximate number of characters (including spaces) of natural language text. For use in ONIX 3.0 only
  case "01":
		c.Body = `Characters`

  // Approximate number of words of natural language text
  case "02":
		c.Body = `Words`

  // Pages
  case "03":
		c.Body = `Pages`

  // Hours (integer and decimals)
  case "04":
		c.Body = `Hours (integer and decimals)`

  // Minutes (integer and decimals)
  case "05":
		c.Body = `Minutes (integer and decimals)`

  // Seconds (integer only)
  case "06":
		c.Body = `Seconds (integer only)`

  // Of an audiobook on CD (or a similarly divided selection of audio files). Conventionally, each track is 3–6 minutes of running time, and track counts are misleading and inappropriate if the average track duration is significantly more or less than this. Note that track breaks are not necessarily aligned with structural breaks in the text (eg chapter breaks)
  case "11":
		c.Body = `Tracks`

  // Of an audiobook on multiple Red Book audio CDs. Conventionally, each disc is 60–70 minutes of running time, and disc counts are misleading and inappropriate if the average disc duration is significantly more or less than this (for example if the discs are Yellow Book CDs containing mp3 files). Note that disc breaks are not necessarily aligned with structural breaks in the text (eg chapter breaks). For use in ONIX 3.0 only
  case "12":
		c.Body = `Discs`

  // Fill with leading zeroes if any elements are missing
  case "14":
		c.Body = `Hours HHH`

  // Fill with leading zeroes if any elements are missing
  case "15":
		c.Body = `Hours and minutes HHHMM`

  // Fill with leading zeroes if any elements are missing. If centisecond precision is required, use HHHMMSScc (in ONIX 3.0 only)
  case "16":
		c.Body = `Hours minutes seconds HHHMMSS`

  // Bytes
  case "17":
		c.Body = `Bytes`

  // Kbytes
  case "18":
		c.Body = `Kbytes`

  // Mbytes
  case "19":
		c.Body = `Mbytes`
	default:
		return fmt.Errorf("undefined code for ExtentUnit has been passed, got [%s]", v)
	}
	return nil
}

// FromLanguage Language – based on ISO 639-2/B
type FromLanguage struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *FromLanguage) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // Afar
  case "aar":
		c.Body = `Afar`

  // Abkhaz
  case "abk":
		c.Body = `Abkhaz`

  // Achinese
  case "ace":
		c.Body = `Achinese`

  // Acoli
  case "ach":
		c.Body = `Acoli`

  // Adangme
  case "ada":
		c.Body = `Adangme`

  // Adygei
  case "ady":
		c.Body = `Adygei`

  // Collective name
  case "afa":
		c.Body = `Afro-Asiatic languages`

  // Artificial language
  case "afh":
		c.Body = `Afrihili`

  // Afrikaans
  case "afr":
		c.Body = `Afrikaans`

  // Ainu
  case "ain":
		c.Body = `Ainu`

  // Macrolanguage
  case "aka":
		c.Body = `Akan`

  // Akkadian
  case "akk":
		c.Body = `Akkadian`

  // Macrolanguage
  case "alb":
		c.Body = `Albanian`

  // Aleut
  case "ale":
		c.Body = `Aleut`

  // Collective name
  case "alg":
		c.Body = `Algonquian languages`

  // Southern Altai
  case "alt":
		c.Body = `Southern Altai`

  // Amharic
  case "amh":
		c.Body = `Amharic`

  // English, Old (ca. 450-1100)
  case "ang":
		c.Body = `English, Old (ca. 450-1100)`

  // Angika
  case "anp":
		c.Body = `Angika`

  // Collective name
  case "apa":
		c.Body = `Apache languages`

  // Macrolanguage
  case "ara":
		c.Body = `Arabic`

  // Official Aramaic; Imperial Aramaic (700-300 BCE)
  case "arc":
		c.Body = `Official Aramaic; Imperial Aramaic (700-300 BCE)`

  // Aragonese
  case "arg":
		c.Body = `Aragonese`

  // Armenian
  case "arm":
		c.Body = `Armenian`

  // Mapudungun; Mapuche
  case "arn":
		c.Body = `Mapudungun; Mapuche`

  // Arapaho
  case "arp":
		c.Body = `Arapaho`

  // Collective name
  case "art":
		c.Body = `Artificial languages`

  // Arawak
  case "arw":
		c.Body = `Arawak`

  // Assamese
  case "asm":
		c.Body = `Assamese`

  // Asturian; Bable; Leonese; Asturleonese
  case "ast":
		c.Body = `Asturian; Bable; Leonese; Asturleonese`

  // Collective name
  case "ath":
		c.Body = `Athapascan languages`

  // Collective name
  case "aus":
		c.Body = `Australian languages`

  // Avaric
  case "ava":
		c.Body = `Avaric`

  // Avestan
  case "ave":
		c.Body = `Avestan`

  // Awadhi
  case "awa":
		c.Body = `Awadhi`

  // Macrolanguage
  case "aym":
		c.Body = `Aymara`

  // Macrolanguage
  case "aze":
		c.Body = `Azerbaijani`

  // Collective name
  case "bad":
		c.Body = `Banda languages`

  // Collective name
  case "bai":
		c.Body = `Bamileke languages`

  // Bashkir
  case "bak":
		c.Body = `Bashkir`

  // Macrolanguage
  case "bal":
		c.Body = `Baluchi`

  // Bambara
  case "bam":
		c.Body = `Bambara`

  // Balinese
  case "ban":
		c.Body = `Balinese`

  // Basque
  case "baq":
		c.Body = `Basque`

  // Basa
  case "bas":
		c.Body = `Basa`

  // Collective name
  case "bat":
		c.Body = `Baltic languages`

  // Beja; Bedawiyet
  case "bej":
		c.Body = `Beja; Bedawiyet`

  // Belarusian
  case "bel":
		c.Body = `Belarusian`

  // Bemba
  case "bem":
		c.Body = `Bemba`

  // Bengali
  case "ben":
		c.Body = `Bengali`

  // Collective name
  case "ber":
		c.Body = `Berber languages`

  // Bhojpuri
  case "bho":
		c.Body = `Bhojpuri`

  // Collective name
  case "bih":
		c.Body = `Bihari languages`

  // Macrolanguage
  case "bik":
		c.Body = `Bikol`

  // Bini; Edo
  case "bin":
		c.Body = `Bini; Edo`

  // Bislama
  case "bis":
		c.Body = `Bislama`

  // Siksika
  case "bla":
		c.Body = `Siksika`

  // Collective name
  case "bnt":
		c.Body = `Bantu languages`

  // Bosnian
  case "bos":
		c.Body = `Bosnian`

  // Braj
  case "bra":
		c.Body = `Braj`

  // Breton
  case "bre":
		c.Body = `Breton`

  // Collective name
  case "btk":
		c.Body = `Batak languages`

  // Macrolanguage
  case "bua":
		c.Body = `Buriat`

  // Buginese
  case "bug":
		c.Body = `Buginese`

  // Bulgarian
  case "bul":
		c.Body = `Bulgarian`

  // Burmese
  case "bur":
		c.Body = `Burmese`

  // Blin; Bilin
  case "byn":
		c.Body = `Blin; Bilin`

  // Caddo
  case "cad":
		c.Body = `Caddo`

  // Collective name
  case "cai":
		c.Body = `Central American Indian languages`

  // Galibi Carib
  case "car":
		c.Body = `Galibi Carib`

  // Catalan
  case "cat":
		c.Body = `Catalan`

  // Collective name
  case "cau":
		c.Body = `Caucasian languages`

  // Cebuano
  case "ceb":
		c.Body = `Cebuano`

  // Collective name
  case "cel":
		c.Body = `Celtic languages`

  // Chamorro
  case "cha":
		c.Body = `Chamorro`

  // Chibcha
  case "chb":
		c.Body = `Chibcha`

  // Chechen
  case "che":
		c.Body = `Chechen`

  // Chagatai
  case "chg":
		c.Body = `Chagatai`

  // Macrolanguage
  case "chi":
		c.Body = `Chinese`

  // Chuukese (Truk)
  case "chk":
		c.Body = `Chuukese (Truk)`

  // Macrolanguage
  case "chm":
		c.Body = `Mari`

  // Chinook jargon
  case "chn":
		c.Body = `Chinook jargon`

  // Choctaw
  case "cho":
		c.Body = `Choctaw`

  // Chipewyan; Dene Suline
  case "chp":
		c.Body = `Chipewyan; Dene Suline`

  // Cherokee
  case "chr":
		c.Body = `Cherokee`

  // Church Slavic; Old Slavonic; Church Slavonic; Old Bulgarian; Old Church Slavonic
  case "chu":
		c.Body = `Church Slavic; Old Slavonic; Church Slavonic; Old Bulgarian; Old Church Slavonic`

  // Chuvash
  case "chv":
		c.Body = `Chuvash`

  // Cheyenne
  case "chy":
		c.Body = `Cheyenne`

  // ONIX local code, equivalent to ckb in ISO 639-3. For use in ONIX 3.0 only
  case "ckb":
		c.Body = `Central Kurdish (Sorani)`

  // Collective name
  case "cmc":
		c.Body = `Chamic languages`

  // ONIX local code, equivalent to cmn in ISO 639-3
  case "cmn":
		c.Body = `Mandarin`

  // For use in ONIX 3.0 only
  case "cnr":
		c.Body = `Montenegrin`

  // Coptic
  case "cop":
		c.Body = `Coptic`

  // Cornish
  case "cor":
		c.Body = `Cornish`

  // Corsican
  case "cos":
		c.Body = `Corsican`

  // Collective name
  case "cpe":
		c.Body = `Creoles and pidgins, English-based`

  // Collective name
  case "cpf":
		c.Body = `Creoles and pidgins, French-based`

  // Collective name
  case "cpp":
		c.Body = `Creoles and pidgins, Portuguese-based`

  // Macrolanguage
  case "cre":
		c.Body = `Cree`

  // Crimean Turkish; Crimean Tatar
  case "crh":
		c.Body = `Crimean Turkish; Crimean Tatar`

  // Collective name
  case "crp":
		c.Body = `Creoles and pidgins`

  // Kashubian
  case "csb":
		c.Body = `Kashubian`

  // Collective name
  case "cus":
		c.Body = `Cushitic languages`

  // Czech
  case "cze":
		c.Body = `Czech`

  // Dakota
  case "dak":
		c.Body = `Dakota`

  // Danish
  case "dan":
		c.Body = `Danish`

  // Dargwa
  case "dar":
		c.Body = `Dargwa`

  // Collective name
  case "day":
		c.Body = `Land Dayak languages`

  // Macrolanguage
  case "del":
		c.Body = `Delaware`

  // Macrolanguage
  case "den":
		c.Body = `Slave (Athapascan)`

  // Dogrib
  case "dgr":
		c.Body = `Dogrib`

  // Macrolanguage
  case "din":
		c.Body = `Dinka`

  // Divehi; Dhivehi; Maldivian
  case "div":
		c.Body = `Divehi; Dhivehi; Maldivian`

  // Macrolanguage
  case "doi":
		c.Body = `Dogri`

  // Collective name
  case "dra":
		c.Body = `Dravidian languages`

  // Lower Sorbian
  case "dsb":
		c.Body = `Lower Sorbian`

  // Duala
  case "dua":
		c.Body = `Duala`

  // Dutch, Middle (ca. 1050-1350)
  case "dum":
		c.Body = `Dutch, Middle (ca. 1050-1350)`

  // Dutch; Flemish
  case "dut":
		c.Body = `Dutch; Flemish`

  // Dyula
  case "dyu":
		c.Body = `Dyula`

  // Dzongkha
  case "dzo":
		c.Body = `Dzongkha`

  // Efik
  case "efi":
		c.Body = `Efik`

  // ONIX local code for Italian dialect, equivalent to egl in ISO 639-3. For use in ONIX 3.0 only
  case "egl":
		c.Body = `Emilian`

  // Egyptian (Ancient)
  case "egy":
		c.Body = `Egyptian (Ancient)`

  // Ekajuk
  case "eka":
		c.Body = `Ekajuk`

  // Elamite
  case "elx":
		c.Body = `Elamite`

  // English
  case "eng":
		c.Body = `English`

  // English, Middle (1100-1500)
  case "enm":
		c.Body = `English, Middle (1100-1500)`

  // Artificial language
  case "epo":
		c.Body = `Esperanto`

  // Macrolanguage
  case "est":
		c.Body = `Estonian`

  // Ewe
  case "ewe":
		c.Body = `Ewe`

  // Ewondo
  case "ewo":
		c.Body = `Ewondo`

  // Fang
  case "fan":
		c.Body = `Fang`

  // Faroese
  case "fao":
		c.Body = `Faroese`

  // Fanti
  case "fat":
		c.Body = `Fanti`

  // Fijian
  case "fij":
		c.Body = `Fijian`

  // Filipino; Pilipino
  case "fil":
		c.Body = `Filipino; Pilipino`

  // Finnish
  case "fin":
		c.Body = `Finnish`

  // ONIX local code, equivalent to fit in ISO 639-3
  case "fit":
		c.Body = `Meänkieli / Tornedalen Finnish`

  // Collective name
  case "fiu":
		c.Body = `Finno-Ugrian languages`

  // ONIX local code, equivalent to fkv in ISO 639-3
  case "fkv":
		c.Body = `Kvensk`

  // Fon
  case "fon":
		c.Body = `Fon`

  // French
  case "fre":
		c.Body = `French`

  // French, Middle (ca. 1400-1600)
  case "frm":
		c.Body = `French, Middle (ca. 1400-1600)`

  // French, Old (ca. 842-1400)
  case "fro":
		c.Body = `French, Old (ca. 842-1400)`

  // Northern Frisian
  case "frr":
		c.Body = `Northern Frisian`

  // Eastern Frisian
  case "frs":
		c.Body = `Eastern Frisian`

  // Western Frisian
  case "fry":
		c.Body = `Western Frisian`

  // Fulah
  case "ful":
		c.Body = `Fulah`

  // Friulian
  case "fur":
		c.Body = `Friulian`

  // Gã
  case "gaa":
		c.Body = `Gã`

  // Gayo
  case "gay":
		c.Body = `Gayo`

  // Macrolanguage
  case "gba":
		c.Body = `Gbaya`

  // Collective name
  case "gem":
		c.Body = `Germanic languages`

  // Georgian
  case "geo":
		c.Body = `Georgian`

  // German
  case "ger":
		c.Body = `German`

  // Ethiopic (Ge’ez)
  case "gez":
		c.Body = `Ethiopic (Ge’ez)`

  // Gilbertese
  case "gil":
		c.Body = `Gilbertese`

  // Scottish Gaelic
  case "gla":
		c.Body = `Scottish Gaelic`

  // Irish
  case "gle":
		c.Body = `Irish`

  // Galician
  case "glg":
		c.Body = `Galician`

  // Manx
  case "glv":
		c.Body = `Manx`

  // German, Middle High (ca. 1050-1500)
  case "gmh":
		c.Body = `German, Middle High (ca. 1050-1500)`

  // German, Old High (ca. 750-1050)
  case "goh":
		c.Body = `German, Old High (ca. 750-1050)`

  // Macrolanguage
  case "gon":
		c.Body = `Gondi`

  // Gorontalo
  case "gor":
		c.Body = `Gorontalo`

  // Gothic
  case "got":
		c.Body = `Gothic`

  // Macrolanguage
  case "grb":
		c.Body = `Grebo`

  // Greek, Ancient (to 1453)
  case "grc":
		c.Body = `Greek, Ancient (to 1453)`

  // Greek, Modern (1453-)
  case "gre":
		c.Body = `Greek, Modern (1453-)`

  // Macrolanguage
  case "grn":
		c.Body = `Guarani`

  // ONIX local code, equivalent to grt in ISO 639-3
  case "grt":
		c.Body = `Garo`

  // Swiss German; Alemannic
  case "gsw":
		c.Body = `Swiss German; Alemannic`

  // Gujarati
  case "guj":
		c.Body = `Gujarati`

  // Gwich’in
  case "gwi":
		c.Body = `Gwich’in`

  // Macrolanguage
  case "hai":
		c.Body = `Haida`

  // Haitian French Creole
  case "hat":
		c.Body = `Haitian French Creole`

  // Hausa
  case "hau":
		c.Body = `Hausa`

  // Hawaiian
  case "haw":
		c.Body = `Hawaiian`

  // Hebrew
  case "heb":
		c.Body = `Hebrew`

  // Herero
  case "her":
		c.Body = `Herero`

  // Hiligaynon
  case "hil":
		c.Body = `Hiligaynon`

  // Collective name
  case "him":
		c.Body = `Himachali languages; Western Pahari languages`

  // Hindi
  case "hin":
		c.Body = `Hindi`

  // Hittite
  case "hit":
		c.Body = `Hittite`

  // Macrolanguage
  case "hmn":
		c.Body = `Hmong; Mong`

  // Hiri Motu
  case "hmo":
		c.Body = `Hiri Motu`

  // Croatian
  case "hrv":
		c.Body = `Croatian`

  // Upper Sorbian
  case "hsb":
		c.Body = `Upper Sorbian`

  // Hungarian
  case "hun":
		c.Body = `Hungarian`

  // Hupa
  case "hup":
		c.Body = `Hupa`

  // Iban
  case "iba":
		c.Body = `Iban`

  // Igbo
  case "ibo":
		c.Body = `Igbo`

  // Icelandic
  case "ice":
		c.Body = `Icelandic`

  // Artificial language
  case "ido":
		c.Body = `Ido`

  // Sichuan Yi; Nuosu
  case "iii":
		c.Body = `Sichuan Yi; Nuosu`

  // Collective name
  case "ijo":
		c.Body = `Ijo languages`

  // Macrolanguage
  case "iku":
		c.Body = `Inuktitut`

  // Artificial language
  case "ile":
		c.Body = `Interlingue; Occidental`

  // Iloko
  case "ilo":
		c.Body = `Iloko`

  // Artificial language
  case "ina":
		c.Body = `Interlingua (International Auxiliary Language Association)`

  // Collective name
  case "inc":
		c.Body = `Indic languages`

  // Indonesian
  case "ind":
		c.Body = `Indonesian`

  // Collective name
  case "ine":
		c.Body = `Indo-European languages`

  // Ingush
  case "inh":
		c.Body = `Ingush`

  // Macrolanguage
  case "ipk":
		c.Body = `Inupiaq`

  // Collective name
  case "ira":
		c.Body = `Iranian languages`

  // Collective name
  case "iro":
		c.Body = `Iroquoian languages`

  // Italian
  case "ita":
		c.Body = `Italian`

  // Javanese
  case "jav":
		c.Body = `Javanese`

  // Lojban
  case "jbo":
		c.Body = `Lojban`

  // Japanese
  case "jpn":
		c.Body = `Japanese`

  // Judeo-Persian
  case "jpr":
		c.Body = `Judeo-Persian`

  // Macrolanguage
  case "jrb":
		c.Body = `Judeo-Arabic`

  // Kara-Kalpak
  case "kaa":
		c.Body = `Kara-Kalpak`

  // Kabyle
  case "kab":
		c.Body = `Kabyle`

  // Kachin; Jingpho
  case "kac":
		c.Body = `Kachin; Jingpho`

  // Kalâtdlisut; Greenlandic
  case "kal":
		c.Body = `Kalâtdlisut; Greenlandic`

  // Kamba
  case "kam":
		c.Body = `Kamba`

  // Kannada
  case "kan":
		c.Body = `Kannada`

  // Collective name
  case "kar":
		c.Body = `Karen languages`

  // Kashmiri
  case "kas":
		c.Body = `Kashmiri`

  // Macrolanguage
  case "kau":
		c.Body = `Kanuri`

  // Kawi
  case "kaw":
		c.Body = `Kawi`

  // Kazakh
  case "kaz":
		c.Body = `Kazakh`

  // Kabardian (Circassian)
  case "kbd":
		c.Body = `Kabardian (Circassian)`

  // ONIX local code, equivalent to kdr in ISO 639-3
  case "kdr":
		c.Body = `Karaim`

  // Khasi
  case "kha":
		c.Body = `Khasi`

  // Collective name
  case "khi":
		c.Body = `Khoisan languages`

  // Central Khmer
  case "khm":
		c.Body = `Central Khmer`

  // Khotanese; Sakan
  case "kho":
		c.Body = `Khotanese; Sakan`

  // Kikuyu; Gikuyu
  case "kik":
		c.Body = `Kikuyu; Gikuyu`

  // Kinyarwanda
  case "kin":
		c.Body = `Kinyarwanda`

  // Kirghiz; Kyrgyz
  case "kir":
		c.Body = `Kirghiz; Kyrgyz`

  // Kimbundu
  case "kmb":
		c.Body = `Kimbundu`

  // Macrolanguage
  case "kok":
		c.Body = `Konkani`

  // Macrolanguage
  case "kom":
		c.Body = `Komi`

  // Macrolanguage
  case "kon":
		c.Body = `Kongo`

  // Korean
  case "kor":
		c.Body = `Korean`

  // Kusaiean (Caroline Islands)
  case "kos":
		c.Body = `Kusaiean (Caroline Islands)`

  // Macrolanguage
  case "kpe":
		c.Body = `Kpelle`

  // Karachay-Balkar
  case "krc":
		c.Body = `Karachay-Balkar`

  // Karelian
  case "krl":
		c.Body = `Karelian`

  // Collective name
  case "kro":
		c.Body = `Kru languages`

  // Kurukh
  case "kru":
		c.Body = `Kurukh`

  // Kuanyama
  case "kua":
		c.Body = `Kuanyama`

  // Kumyk
  case "kum":
		c.Body = `Kumyk`

  // Macrolanguage
  case "kur":
		c.Body = `Kurdish`

  // Kutenai
  case "kut":
		c.Body = `Kutenai`

  // Ladino
  case "lad":
		c.Body = `Ladino`

  // Macrolanguage
  case "lah":
		c.Body = `Lahnda`

  // Lamba
  case "lam":
		c.Body = `Lamba`

  // Lao
  case "lao":
		c.Body = `Lao`

  // Latin
  case "lat":
		c.Body = `Latin`

  // Macrolanguage
  case "lav":
		c.Body = `Latvian`

  // Lezgian
  case "lez":
		c.Body = `Lezgian`

  // ONIX local code for Italian dialect, equivalent to lij in ISO 639-3. For use in ONIX 3.0 only
  case "lij":
		c.Body = `Ligurian`

  // Limburgish
  case "lim":
		c.Body = `Limburgish`

  // Lingala
  case "lin":
		c.Body = `Lingala`

  // Lithuanian
  case "lit":
		c.Body = `Lithuanian`

  // ONIX local code for Italian dialect, equivalent to lmo in ISO 639-3. For use in ONIX 3.0 only
  case "lmo":
		c.Body = `Lombard`

  // Mongo-Nkundu
  case "lol":
		c.Body = `Mongo-Nkundu`

  // Lozi
  case "loz":
		c.Body = `Lozi`

  // Luxembourgish; Letzeburgesch
  case "ltz":
		c.Body = `Luxembourgish; Letzeburgesch`

  // Luba-Lulua
  case "lua":
		c.Body = `Luba-Lulua`

  // Luba-Katanga
  case "lub":
		c.Body = `Luba-Katanga`

  // Ganda
  case "lug":
		c.Body = `Ganda`

  // Luiseño
  case "lui":
		c.Body = `Luiseño`

  // Lunda
  case "lun":
		c.Body = `Lunda`

  // Luo (Kenya and Tanzania)
  case "luo":
		c.Body = `Luo (Kenya and Tanzania)`

  // Lushai
  case "lus":
		c.Body = `Lushai`

  // Macedonian
  case "mac":
		c.Body = `Macedonian`

  // Madurese
  case "mad":
		c.Body = `Madurese`

  // Magahi
  case "mag":
		c.Body = `Magahi`

  // Marshallese
  case "mah":
		c.Body = `Marshallese`

  // Maithili
  case "mai":
		c.Body = `Maithili`

  // Makasar
  case "mak":
		c.Body = `Makasar`

  // Malayalam
  case "mal":
		c.Body = `Malayalam`

  // Macrolanguage
  case "man":
		c.Body = `Mandingo`

  // Maori
  case "mao":
		c.Body = `Maori`

  // Collective name
  case "map":
		c.Body = `Austronesian languages`

  // Marathi
  case "mar":
		c.Body = `Marathi`

  // Masai
  case "mas":
		c.Body = `Masai`

  // Macrolanguage
  case "may":
		c.Body = `Malay`

  // Moksha
  case "mdf":
		c.Body = `Moksha`

  // Mandar
  case "mdr":
		c.Body = `Mandar`

  // Mende
  case "men":
		c.Body = `Mende`

  // Irish, Middle (ca. 1100-1550)
  case "mga":
		c.Body = `Irish, Middle (ca. 1100-1550)`

  // Mi’kmaq; Micmac
  case "mic":
		c.Body = `Mi’kmaq; Micmac`

  // Minangkabau
  case "min":
		c.Body = `Minangkabau`

  // Use where no suitable code is available
  case "mis":
		c.Body = `Uncoded languages`

  // Collective name
  case "mkh":
		c.Body = `Mon-Khmer languages`

  // Macrolanguage
  case "mlg":
		c.Body = `Malagasy`

  // Maltese
  case "mlt":
		c.Body = `Maltese`

  // Manchu
  case "mnc":
		c.Body = `Manchu`

  // Manipuri
  case "mni":
		c.Body = `Manipuri`

  // Collective name
  case "mno":
		c.Body = `Manobo languages`

  // Mohawk
  case "moh":
		c.Body = `Mohawk`

  // DEPRECATED – use rum
  case "mol":
		c.Body = `Moldavian; Moldovan`

  // Macrolanguage
  case "mon":
		c.Body = `Mongolian`

  // Mooré; Mossi
  case "mos":
		c.Body = `Mooré; Mossi`

  // Multiple languages
  case "mul":
		c.Body = `Multiple languages`

  // Collective name
  case "mun":
		c.Body = `Munda languages`

  // Creek
  case "mus":
		c.Body = `Creek`

  // ONIX local code, equivalent to mwf in ISO 639-3. For use in ONIX 3.0 only
  case "mwf":
		c.Body = `Murrinh-Patha`

  // Mirandese
  case "mwl":
		c.Body = `Mirandese`

  // Macrolanguage
  case "mwr":
		c.Body = `Marwari`

  // Collective name
  case "myn":
		c.Body = `Mayan languages`

  // Erzya
  case "myv":
		c.Body = `Erzya`

  // Collective name
  case "nah":
		c.Body = `Nahuatl languages`

  // Collective name
  case "nai":
		c.Body = `North American Indian languages`

  // Neapolitan
  case "nap":
		c.Body = `Neapolitan`

  // Nauruan
  case "nau":
		c.Body = `Nauruan`

  // Navajo
  case "nav":
		c.Body = `Navajo`

  // Ndebele, South
  case "nbl":
		c.Body = `Ndebele, South`

  // Ndebele, North
  case "nde":
		c.Body = `Ndebele, North`

  // Ndonga
  case "ndo":
		c.Body = `Ndonga`

  // Low German; Low Saxon
  case "nds":
		c.Body = `Low German; Low Saxon`

  // Macrolanguage
  case "nep":
		c.Body = `Nepali`

  // Newari; Nepal Bhasa
  case "new":
		c.Body = `Newari; Nepal Bhasa`

  // Nias
  case "nia":
		c.Body = `Nias`

  // Collective name
  case "nic":
		c.Body = `Niger-Kordofanian languages`

  // Niuean
  case "niu":
		c.Body = `Niuean`

  // Norwegian Nynorsk
  case "nno":
		c.Body = `Norwegian Nynorsk`

  // Norwegian Bokmål
  case "nob":
		c.Body = `Norwegian Bokmål`

  // Nogai
  case "nog":
		c.Body = `Nogai`

  // Old Norse
  case "non":
		c.Body = `Old Norse`

  // Macrolanguage
  case "nor":
		c.Body = `Norwegian`

  // N’Ko
  case "nqo":
		c.Body = `N’Ko`

  // ONIX local code, equivalent to nrf in ISO 639-3. For use in ONIX 3.0 only
  case "nrf":
		c.Body = `Guernésiais, Jèrriais`

  // Pedi; Sepedi; Northern Sotho
  case "nso":
		c.Body = `Pedi; Sepedi; Northern Sotho`

  // Collective name
  case "nub":
		c.Body = `Nubian languages`

  // Classical Newari; Old Newari; Classical Nepal Bhasa
  case "nwc":
		c.Body = `Classical Newari; Old Newari; Classical Nepal Bhasa`

  // Chichewa; Chewa; Nyanja
  case "nya":
		c.Body = `Chichewa; Chewa; Nyanja`

  // Nyamwezi
  case "nym":
		c.Body = `Nyamwezi`

  // Nyankole
  case "nyn":
		c.Body = `Nyankole`

  // Nyoro
  case "nyo":
		c.Body = `Nyoro`

  // Nzima
  case "nzi":
		c.Body = `Nzima`

  // Occitan (post 1500)
  case "oci":
		c.Body = `Occitan (post 1500)`

  // ONIX local code, equivalent to odt in ISO 639-3
  case "odt":
		c.Body = `Old Dutch / Old Low Franconian (ca. 400–1050)`

  // Macrolanguage
  case "oji":
		c.Body = `Ojibwa`

  // ONIX local code, equivalent to omq in ISO 639-5. Collective name
  case "omq":
		c.Body = `Oto-Manguean languages`

  // Macrolanguage
  case "ori":
		c.Body = `Oriya`

  // Macrolanguage
  case "orm":
		c.Body = `Oromo`

  // Osage
  case "osa":
		c.Body = `Osage`

  // Ossetian; Ossetic
  case "oss":
		c.Body = `Ossetian; Ossetic`

  // Turkish, Ottoman
  case "ota":
		c.Body = `Turkish, Ottoman`

  // Collective name
  case "oto":
		c.Body = `Otomian languages`

  // Collective name
  case "paa":
		c.Body = `Papuan languages`

  // Pangasinan
  case "pag":
		c.Body = `Pangasinan`

  // Pahlavi
  case "pal":
		c.Body = `Pahlavi`

  // Pampanga; Kapampangan
  case "pam":
		c.Body = `Pampanga; Kapampangan`

  // Panjabi
  case "pan":
		c.Body = `Panjabi`

  // Papiamento
  case "pap":
		c.Body = `Papiamento`

  // Palauan
  case "pau":
		c.Body = `Palauan`

  // Old Persian (ca. 600-400 B.C.)
  case "peo":
		c.Body = `Old Persian (ca. 600-400 B.C.)`

  // Macrolanguage
  case "per":
		c.Body = `Persian; Farsi`

  // ONIX local code, equivalent to pes in ISO 639-3. For use in ONIX 3.0 only
  case "pes":
		c.Body = `Iranian Persian; Parsi`

  // Collective name
  case "phi":
		c.Body = `Philippine languages`

  // Phoenician
  case "phn":
		c.Body = `Phoenician`

  // Pali
  case "pli":
		c.Body = `Pali`

  // ONIX local code for Italian dialect, equivalent to pms in ISO 639-3. For use in ONIX 3.0 only
  case "pms":
		c.Body = `Piedmontese`

  // Polish
  case "pol":
		c.Body = `Polish`

  // Ponapeian
  case "pon":
		c.Body = `Ponapeian`

  // Portuguese
  case "por":
		c.Body = `Portuguese`

  // Collective name
  case "pra":
		c.Body = `Prakrit languages`

  // Provençal, Old (to 1500); Occitan, Old (to 1500)
  case "pro":
		c.Body = `Provençal, Old (to 1500); Occitan, Old (to 1500)`

  // ONIX local code, equivalent to prs in ISO 639-3. For use in ONIX 3.0 only
  case "prs":
		c.Body = `Dari; Afghan Persian`

  // Macrolanguage
  case "pus":
		c.Body = `Pushto; Pashto`

  // ONIX local code, distinct dialect of Occitan (not distinguished from oci by ISO 639-3)
  case "qar":
		c.Body = `Aranés`

  // ONIX local code, distinct dialect of Catalan (not distinguished from cat by ISO 639-3)
  case "qav":
		c.Body = `Valencian`

  // ONIX local code, distinct variant of langue d’oïl (old northern French) (not distinguished from fro, or from frm, fre, nrf by ISO 639-3). For use in ONIX 3.0 only
  case "qgl":
		c.Body = `Gallo`

  // ONIX local code, distinct dialect of of Rusyn (not distinguished from rue by ISO 639-3). For use in ONIX 3.0 only
  case "qlk":
		c.Body = `Lemko`

  // ONIX local code, distinct and exclusively spoken variation of Spanish, not distinguished from spa (Spanish, Castilian) by ISO 639-3. Neutral Latin American Spanish should be considered a ‘shorthand’ for spa plus a ‘country code’ for Latin America – but prefer spa plus the relevant country code for specifically Mexican Spanish, Argentine (Rioplatense) Spanish, Puerto Rican Spanish etc. Neutral Latin American Spanish must only be used with audio material (including the audio tracks of TV, video and film) to indicate use of accent, vocabulary and construction suitable for broad use across Latin America. For use in ONIX 3.0 only
  case "qls":
		c.Body = `Neutral Latin American Spanish`

  // Macrolanguage
  case "que":
		c.Body = `Quechua`

  // Macrolanguage
  case "raj":
		c.Body = `Rajasthani`

  // Rapanui
  case "rap":
		c.Body = `Rapanui`

  // Rarotongan; Cook Islands Maori
  case "rar":
		c.Body = `Rarotongan; Cook Islands Maori`

  // ONIX local code, equivalent to rcf in ISO 639-3. For use in ONIX 3.0 only
  case "rcf":
		c.Body = `Réunion Creole French`

  // ONIX local code for Italian dialect, equivalent to rgl in ISO 639-3. For use in ONIX 3.0 only
  case "rgn":
		c.Body = `Romagnol`

  // Collective name
  case "roa":
		c.Body = `Romance languages`

  // Romansh
  case "roh":
		c.Body = `Romansh`

  // Macrolanguage
  case "rom":
		c.Body = `Romany`

  // Romanian
  case "rum":
		c.Body = `Romanian`

  // Rundi
  case "run":
		c.Body = `Rundi`

  // Aromanian; Arumanian; Macedo-Romanian
  case "rup":
		c.Body = `Aromanian; Arumanian; Macedo-Romanian`

  // Russian
  case "rus":
		c.Body = `Russian`

  // Sandawe
  case "sad":
		c.Body = `Sandawe`

  // Sango
  case "sag":
		c.Body = `Sango`

  // Yakut
  case "sah":
		c.Body = `Yakut`

  // Collective name
  case "sai":
		c.Body = `South American Indian languages`

  // Collective name
  case "sal":
		c.Body = `Salishan languages`

  // Samaritan Aramaic
  case "sam":
		c.Body = `Samaritan Aramaic`

  // Sanskrit
  case "san":
		c.Body = `Sanskrit`

  // Sasak
  case "sas":
		c.Body = `Sasak`

  // Santali
  case "sat":
		c.Body = `Santali`

  // DEPRECATED – use srp
  case "scc":
		c.Body = `Serbian`

  // Sicilian
  case "scn":
		c.Body = `Sicilian`

  // Scots
  case "sco":
		c.Body = `Scots`

  // DEPRECATED – use hrv
  case "scr":
		c.Body = `Croatian`

  // ONIX local code for Sardinian dialect, equivalent to sdc in ISO 639-3. For use in ONIX 3.0 only
  case "sdc":
		c.Body = `Sassarese`

  // ONIX local code for Sardinian dialect, equivalent to sdn in ISO 639-3. For use in ONIX 3.0 only
  case "sdn":
		c.Body = `Gallurese`

  // Selkup
  case "sel":
		c.Body = `Selkup`

  // Collective name
  case "sem":
		c.Body = `Semitic languages`

  // Irish, Old (to 1100)
  case "sga":
		c.Body = `Irish, Old (to 1100)`

  // Collective name
  case "sgn":
		c.Body = `Sign languages`

  // Shan
  case "shn":
		c.Body = `Shan`

  // Sidamo
  case "sid":
		c.Body = `Sidamo`

  // Sinhala; Sinhalese
  case "sin":
		c.Body = `Sinhala; Sinhalese`

  // Collective name
  case "sio":
		c.Body = `Siouan languages`

  // Collective name
  case "sit":
		c.Body = `Sino-Tibetan languages`

  // Collective name
  case "sla":
		c.Body = `Slavic languages`

  // Slovak
  case "slo":
		c.Body = `Slovak`

  // Slovenian
  case "slv":
		c.Body = `Slovenian`

  // Southern Sami
  case "sma":
		c.Body = `Southern Sami`

  // Northern Sami
  case "sme":
		c.Body = `Northern Sami`

  // Collective name
  case "smi":
		c.Body = `Sami languages`

  // Lule Sami
  case "smj":
		c.Body = `Lule Sami`

  // Inari Sami
  case "smn":
		c.Body = `Inari Sami`

  // Samoan
  case "smo":
		c.Body = `Samoan`

  // Skolt Sami
  case "sms":
		c.Body = `Skolt Sami`

  // Shona
  case "sna":
		c.Body = `Shona`

  // Sindhi
  case "snd":
		c.Body = `Sindhi`

  // Soninke
  case "snk":
		c.Body = `Soninke`

  // Sogdian
  case "sog":
		c.Body = `Sogdian`

  // Somali
  case "som":
		c.Body = `Somali`

  // Collective name
  case "son":
		c.Body = `Songhai languages`

  // Sotho; Sesotho
  case "sot":
		c.Body = `Sotho; Sesotho`

  // Spanish
  case "spa":
		c.Body = `Spanish`

  // Macrolanguage
  case "srd":
		c.Body = `Sardinian`

  // Sranan Tongo
  case "srn":
		c.Body = `Sranan Tongo`

  // ONIX local code for Sardinian dialect, equivalent to sro in ISO 639-3. For use in ONIX 3.0 only
  case "sro":
		c.Body = `Campidanese`

  // Serbian
  case "srp":
		c.Body = `Serbian`

  // Serer
  case "srr":
		c.Body = `Serer`

  // Collective name
  case "ssa":
		c.Body = `Nilo-Saharan languages`

  // Swazi; Swati
  case "ssw":
		c.Body = `Swazi; Swati`

  // Sukuma
  case "suk":
		c.Body = `Sukuma`

  // Sundanese
  case "sun":
		c.Body = `Sundanese`

  // Susu
  case "sus":
		c.Body = `Susu`

  // Sumerian
  case "sux":
		c.Body = `Sumerian`

  // Macrolanguage
  case "swa":
		c.Body = `Swahili`

  // Swedish
  case "swe":
		c.Body = `Swedish`

  // Classical Syriac
  case "syc":
		c.Body = `Classical Syriac`

  // Macrolanguage
  case "syr":
		c.Body = `Syriac`

  // Tahitian
  case "tah":
		c.Body = `Tahitian`

  // Collective name
  case "tai":
		c.Body = `Tai languages`

  // Tamil
  case "tam":
		c.Body = `Tamil`

  // Tatar
  case "tat":
		c.Body = `Tatar`

  // Telugu
  case "tel":
		c.Body = `Telugu`

  // Temne; Time
  case "tem":
		c.Body = `Temne; Time`

  // Terena
  case "ter":
		c.Body = `Terena`

  // Tetum
  case "tet":
		c.Body = `Tetum`

  // Tajik; Tajiki Persian
  case "tgk":
		c.Body = `Tajik; Tajiki Persian`

  // Tagalog
  case "tgl":
		c.Body = `Tagalog`

  // Thai
  case "tha":
		c.Body = `Thai`

  // Tibetan
  case "tib":
		c.Body = `Tibetan`

  // Tigré
  case "tig":
		c.Body = `Tigré`

  // Tigrinya
  case "tir":
		c.Body = `Tigrinya`

  // Tiv
  case "tiv":
		c.Body = `Tiv`

  // Tokelauan
  case "tkl":
		c.Body = `Tokelauan`

  // Artificial language
  case "tlh":
		c.Body = `Klingon; tlhIngan-Hol`

  // Tlingit
  case "tli":
		c.Body = `Tlingit`

  // Macrolanguage
  case "tmh":
		c.Body = `Tamashek`

  // Tonga (Nyasa)
  case "tog":
		c.Body = `Tonga (Nyasa)`

  // Tongan
  case "ton":
		c.Body = `Tongan`

  // Tok Pisin
  case "tpi":
		c.Body = `Tok Pisin`

  // Tsimshian
  case "tsi":
		c.Body = `Tsimshian`

  // AKA Setswana
  case "tsn":
		c.Body = `Tswana`

  // Tsonga
  case "tso":
		c.Body = `Tsonga`

  // Turkmen
  case "tuk":
		c.Body = `Turkmen`

  // Tumbuka
  case "tum":
		c.Body = `Tumbuka`

  // Collective name
  case "tup":
		c.Body = `Tupi languages`

  // Turkish
  case "tur":
		c.Body = `Turkish`

  // Altaic languages
  case "tut":
		c.Body = `Altaic languages`

  // Tuvaluan
  case "tvl":
		c.Body = `Tuvaluan`

  // Twi
  case "twi":
		c.Body = `Twi`

  // Tuvinian
  case "tyv":
		c.Body = `Tuvinian`

  // ONIX local code, equivalent to tzo in ISO 639-3
  case "tzo":
		c.Body = `Tzotzil`

  // Udmurt
  case "udm":
		c.Body = `Udmurt`

  // Ugaritic
  case "uga":
		c.Body = `Ugaritic`

  // Uighur; Uyghur
  case "uig":
		c.Body = `Uighur; Uyghur`

  // Ukrainian
  case "ukr":
		c.Body = `Ukrainian`

  // Umbundu
  case "umb":
		c.Body = `Umbundu`

  // Undetermined language
  case "und":
		c.Body = `Undetermined language`

  // Urdu
  case "urd":
		c.Body = `Urdu`

  // Macrolanguage
  case "uzb":
		c.Body = `Uzbek`

  // Vai
  case "vai":
		c.Body = `Vai`

  // ONIX local code for Italian dialect, equivalent to vec in ISO 639-3. For use in ONIX 3.0 only
  case "vec":
		c.Body = `Venetian/Venetan`

  // Venda
  case "ven":
		c.Body = `Venda`

  // Vietnamese
  case "vie":
		c.Body = `Vietnamese`

  // Artificial language
  case "vol":
		c.Body = `Volapük`

  // Votic
  case "vot":
		c.Body = `Votic`

  // Collective name
  case "wak":
		c.Body = `Wakashan languages`

  // Wolaitta; Wolaytta
  case "wal":
		c.Body = `Wolaitta; Wolaytta`

  // Waray
  case "war":
		c.Body = `Waray`

  // Washo
  case "was":
		c.Body = `Washo`

  // Welsh
  case "wel":
		c.Body = `Welsh`

  // Collective name
  case "wen":
		c.Body = `Sorbian languages`

  // Walloon
  case "wln":
		c.Body = `Walloon`

  // Wolof
  case "wol":
		c.Body = `Wolof`

  // Kalmyk
  case "xal":
		c.Body = `Kalmyk`

  // Xhosa
  case "xho":
		c.Body = `Xhosa`

  // ONIX local code, equivalent to xuu in ISO 639-3. For use in ONIX 3.0 only
  case "xuu":
		c.Body = `Khwedam, Kxoe`

  // Yao
  case "yao":
		c.Body = `Yao`

  // Yapese
  case "yap":
		c.Body = `Yapese`

  // Macrolanguage
  case "yid":
		c.Body = `Yiddish`

  // Yoruba
  case "yor":
		c.Body = `Yoruba`

  // Collective name
  case "ypk":
		c.Body = `Yupik languages`

  // ONIX local code, equivalent to yue in ISO 639-3
  case "yue":
		c.Body = `Cantonese`

  // Macrolanguage
  case "zap":
		c.Body = `Zapotec`

  // Artificial language
  case "zbl":
		c.Body = `Blissymbols; Blissymbolics; Bliss`

  // Zenaga
  case "zen":
		c.Body = `Zenaga`

  // Standard Moroccan Tamazight
  case "zgh":
		c.Body = `Standard Moroccan Tamazight`

  // Macrolanguage
  case "zha":
		c.Body = `Zhuang; Chuang`

  // Collective name
  case "znd":
		c.Body = `Zande languages`

  // Zulu
  case "zul":
		c.Body = `Zulu`

  // Zuni
  case "zun":
		c.Body = `Zuni`

  // No linguistic content
  case "zxx":
		c.Body = `No linguistic content`

  // Macrolanguage
  case "zza":
		c.Body = `Zaza; Dimili; Dimli; Kirdki; Kirmanjki; Zazaki`
	default:
		return fmt.Errorf("undefined code for FromLanguage has been passed, got [%s]", v)
	}
	return nil
}

// FundingIDType Grant identifier type
type FundingIDType struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *FundingIDType) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // Note that <IDTypeName> is required with proprietary identifiers
  case "01":
		c.Body = `Proprietary`
	default:
		return fmt.Errorf("undefined code for FundingIDType has been passed, got [%s]", v)
	}
	return nil
}

// Gender Gender – based on ISO 5218
type Gender struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *Gender) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // Provides positive indication that the gender is not known or is not specified by the sender for any reason
  case "u":
		c.Body = `Unknown or unspecified`

  // Female
  case "f":
		c.Body = `Female`

  // Male
  case "m":
		c.Body = `Male`
	default:
		return fmt.Errorf("undefined code for Gender has been passed, got [%s]", v)
	}
	return nil
}

// Illustrated Illustrated / not illustrated
type Illustrated struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *Illustrated) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // Not illustrated
  case "01":
		c.Body = `No`

  // Illustrated
  case "02":
		c.Body = `Yes`
	default:
		return fmt.Errorf("undefined code for Illustrated has been passed, got [%s]", v)
	}
	return nil
}

// ImprintIDType Name identifier type
type ImprintIDType struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *ImprintIDType) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // Note that <IDTypeName> is required with proprietary identifiers
  case "01":
		c.Body = `Proprietary`

  // DEPRECATED – use 01
  case "02":
		c.Body = `Proprietary`

  // Deutsche Nationalbibliothek publisher identifier
  case "03":
		c.Body = `DNB publisher identifier`

  // Börsenverein Verkehrsnummer
  case "04":
		c.Body = `Börsenverein Verkehrsnummer`

  // German ISBN Agency publisher identifier
  case "05":
		c.Body = `German ISBN Agency publisher identifier`

  // GS1 global location number (formerly EAN location number)
  case "06":
		c.Body = `GLN`

  // Book trade Standard Address Number – US, UK etc
  case "07":
		c.Body = `SAN`

  // MARC code list for organizations – see http://www.loc.gov/marc/organizations/orgshome.html
  case "08":
		c.Body = `MARC organization code`

  // Trading party identifier used in the Netherlands
  case "10":
		c.Body = `Centraal Boekhuis Relatie ID`

  // Flemish publisher code
  case "13":
		c.Body = `Fondscode Boekenbank`

  // Business Identity Code (Finland). See http://www.ytj.fi/ (in Finnish)
  case "15":
		c.Body = `Y-tunnus`

  // International Standard Name Identifier. A sixteen digit number. Usually presented with spaces or hyphens dividing the number into four groups of four digits, but in ONIX the spaces or hyphens should be omitted. See http://www.isni.org/
  case "16":
		c.Body = `ISNI`

  // Personennamendatei – person name authority file used by Deutsche Nationalbibliothek and in other German-speaking countries. See http://www.dnb.de/standardisierung/normdateien/pnd.htm (German) or http://www.dnb.de/eng/standardisierung/normdateien/pnd.htm (English). DEPRECATED in favor of the GND
  case "17":
		c.Body = `PND`

  // A control number assigned to a Library of Congress Control Number (LCCN) Name Authority / NACO record
  case "18":
		c.Body = `NACO`

  // Publisher identifier administered by Japanese ISBN Agency
  case "19":
		c.Body = `Japanese Publisher identifier`

  // Gemeinsame Körperschaftsdatei – Corporate Body Authority File in the German-speaking countries. See http://www.dnb.de/standardisierung/normdateien/gkd.htm (German) or http://www.dnb.de/eng/standardisierung/normdateien/gkd.htm (English). DEPRECATED in favor of the GND
  case "20":
		c.Body = `GKD`

  // Open Researcher and Contributor ID. A sixteen digit number. Usually presented with hyphens dividing the number into four groups of four digits, but in ONIX the hyphens should be omitted. See http://www.orcid.org/
  case "21":
		c.Body = `ORCID`

  // Publisher identifier maintained by the Chinese ISBN Agency (GAPP)
  case "22":
		c.Body = `GAPP Publisher Identifier`

  // Identifier for a business organization for VAT purposes, eg within the EU’s VIES system. See http://ec.europa.eu/taxation_customs/vies/faqvies.do for EU VAT ID formats, which vary from country to country. Generally these consist of a two-letter country code followed by the 8–12 digits of the national VAT ID. Some countries include one or two letters within their VAT ID. See http://en.wikipedia.org/wiki/VAT_identification_number for non-EU countries that maintain similar identifiers. Spaces, dashes etc should be omitted
  case "23":
		c.Body = `VAT Identity Number`

  // 4-digit business organization identifier controlled by the Japanese Publication Wholesalers Association
  case "24":
		c.Body = `JP Distribution Identifier`

  // Gemeinsame Normdatei – Joint Authority File in the German-speaking countries. See http://www.dnb.de/EN/gnd (English). Combines the PND, SWD and GKD into a single authority file, and should be used in preference
  case "25":
		c.Body = `GND`

  // Dunn and Bradstreet Universal Numbering System, see http://www.dnb.co.uk/dandb-duns-number
  case "26":
		c.Body = `DUNS`

  // Ringgold organizational identifier, see http://www.ringgold.com/identify.html
  case "27":
		c.Body = `Ringgold ID`

  // French Electre publisher identifier
  case "28":
		c.Body = `Identifiant Editeur Electre`

  // DOI used in EIDR party registry, for example ‘10.5237/C9F6-F41F’ (Sam Raimi). See http://eidr.org
  case "29":
		c.Body = `EIDR Party DOI`

  // French Electre imprint Identifier
  case "30":
		c.Body = `Identifiant Marque Electre`

  // Virtual Internet Authority File. <IDValue> should be a number. The URI form of the identifier can be created by prefixing the number with ‘https://viaf.org/viaf/’. See https://viaf.org
  case "31":
		c.Body = `VIAF ID`

  // DOI used in CrossRef’s Open Funder Registry list of academic research funding bodies, for example ‘10.13039/100004440’ (Wellcome Trust). See http://www.crossref.org/fundingdata/registry.html
  case "32":
		c.Body = `FundRef DOI`

  // Control number assigned to a Name Authority record by the Biblioteca Nacional de España
  case "33":
		c.Body = `BNE CN`

  // Numéro de la notice de personne BNF
  case "34":
		c.Body = `BNF Control Number`

  // Archival Resource Key, as a URL (including the address of the ARK resolver provided by eg a national library)
  case "35":
		c.Body = `ARK`

  // Nasjonalt autoritetsregister for navn – Norwegian national authority file for personal and corporate names. For use in ONIX 3.0 only
  case "36":
		c.Body = `Nasjonalt autoritetsregister`

  // Global Research Identifier Database ID (see https://www.grid.ac). For use in ONIX 3.0 only
  case "37":
		c.Body = `GRID`

  // Party ID from Identifiers and Standards for Higher Education and Research (fr: Identifiants et Référentiels pour l’enseignement supérieur et la recherche). For use on ONIX 3.0 only. See https://www.idref.fr
  case "38":
		c.Body = `IDRef`
	default:
		return fmt.Errorf("undefined code for ImprintIDType has been passed, got [%s]", v)
	}
	return nil
}

// LanguageCode Language – based on ISO 639-2/B
type LanguageCode struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *LanguageCode) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // Afar
  case "aar":
		c.Body = `Afar`

  // Abkhaz
  case "abk":
		c.Body = `Abkhaz`

  // Achinese
  case "ace":
		c.Body = `Achinese`

  // Acoli
  case "ach":
		c.Body = `Acoli`

  // Adangme
  case "ada":
		c.Body = `Adangme`

  // Adygei
  case "ady":
		c.Body = `Adygei`

  // Collective name
  case "afa":
		c.Body = `Afro-Asiatic languages`

  // Artificial language
  case "afh":
		c.Body = `Afrihili`

  // Afrikaans
  case "afr":
		c.Body = `Afrikaans`

  // Ainu
  case "ain":
		c.Body = `Ainu`

  // Macrolanguage
  case "aka":
		c.Body = `Akan`

  // Akkadian
  case "akk":
		c.Body = `Akkadian`

  // Macrolanguage
  case "alb":
		c.Body = `Albanian`

  // Aleut
  case "ale":
		c.Body = `Aleut`

  // Collective name
  case "alg":
		c.Body = `Algonquian languages`

  // Southern Altai
  case "alt":
		c.Body = `Southern Altai`

  // Amharic
  case "amh":
		c.Body = `Amharic`

  // English, Old (ca. 450-1100)
  case "ang":
		c.Body = `English, Old (ca. 450-1100)`

  // Angika
  case "anp":
		c.Body = `Angika`

  // Collective name
  case "apa":
		c.Body = `Apache languages`

  // Macrolanguage
  case "ara":
		c.Body = `Arabic`

  // Official Aramaic; Imperial Aramaic (700-300 BCE)
  case "arc":
		c.Body = `Official Aramaic; Imperial Aramaic (700-300 BCE)`

  // Aragonese
  case "arg":
		c.Body = `Aragonese`

  // Armenian
  case "arm":
		c.Body = `Armenian`

  // Mapudungun; Mapuche
  case "arn":
		c.Body = `Mapudungun; Mapuche`

  // Arapaho
  case "arp":
		c.Body = `Arapaho`

  // Collective name
  case "art":
		c.Body = `Artificial languages`

  // Arawak
  case "arw":
		c.Body = `Arawak`

  // Assamese
  case "asm":
		c.Body = `Assamese`

  // Asturian; Bable; Leonese; Asturleonese
  case "ast":
		c.Body = `Asturian; Bable; Leonese; Asturleonese`

  // Collective name
  case "ath":
		c.Body = `Athapascan languages`

  // Collective name
  case "aus":
		c.Body = `Australian languages`

  // Avaric
  case "ava":
		c.Body = `Avaric`

  // Avestan
  case "ave":
		c.Body = `Avestan`

  // Awadhi
  case "awa":
		c.Body = `Awadhi`

  // Macrolanguage
  case "aym":
		c.Body = `Aymara`

  // Macrolanguage
  case "aze":
		c.Body = `Azerbaijani`

  // Collective name
  case "bad":
		c.Body = `Banda languages`

  // Collective name
  case "bai":
		c.Body = `Bamileke languages`

  // Bashkir
  case "bak":
		c.Body = `Bashkir`

  // Macrolanguage
  case "bal":
		c.Body = `Baluchi`

  // Bambara
  case "bam":
		c.Body = `Bambara`

  // Balinese
  case "ban":
		c.Body = `Balinese`

  // Basque
  case "baq":
		c.Body = `Basque`

  // Basa
  case "bas":
		c.Body = `Basa`

  // Collective name
  case "bat":
		c.Body = `Baltic languages`

  // Beja; Bedawiyet
  case "bej":
		c.Body = `Beja; Bedawiyet`

  // Belarusian
  case "bel":
		c.Body = `Belarusian`

  // Bemba
  case "bem":
		c.Body = `Bemba`

  // Bengali
  case "ben":
		c.Body = `Bengali`

  // Collective name
  case "ber":
		c.Body = `Berber languages`

  // Bhojpuri
  case "bho":
		c.Body = `Bhojpuri`

  // Collective name
  case "bih":
		c.Body = `Bihari languages`

  // Macrolanguage
  case "bik":
		c.Body = `Bikol`

  // Bini; Edo
  case "bin":
		c.Body = `Bini; Edo`

  // Bislama
  case "bis":
		c.Body = `Bislama`

  // Siksika
  case "bla":
		c.Body = `Siksika`

  // Collective name
  case "bnt":
		c.Body = `Bantu languages`

  // Bosnian
  case "bos":
		c.Body = `Bosnian`

  // Braj
  case "bra":
		c.Body = `Braj`

  // Breton
  case "bre":
		c.Body = `Breton`

  // Collective name
  case "btk":
		c.Body = `Batak languages`

  // Macrolanguage
  case "bua":
		c.Body = `Buriat`

  // Buginese
  case "bug":
		c.Body = `Buginese`

  // Bulgarian
  case "bul":
		c.Body = `Bulgarian`

  // Burmese
  case "bur":
		c.Body = `Burmese`

  // Blin; Bilin
  case "byn":
		c.Body = `Blin; Bilin`

  // Caddo
  case "cad":
		c.Body = `Caddo`

  // Collective name
  case "cai":
		c.Body = `Central American Indian languages`

  // Galibi Carib
  case "car":
		c.Body = `Galibi Carib`

  // Catalan
  case "cat":
		c.Body = `Catalan`

  // Collective name
  case "cau":
		c.Body = `Caucasian languages`

  // Cebuano
  case "ceb":
		c.Body = `Cebuano`

  // Collective name
  case "cel":
		c.Body = `Celtic languages`

  // Chamorro
  case "cha":
		c.Body = `Chamorro`

  // Chibcha
  case "chb":
		c.Body = `Chibcha`

  // Chechen
  case "che":
		c.Body = `Chechen`

  // Chagatai
  case "chg":
		c.Body = `Chagatai`

  // Macrolanguage
  case "chi":
		c.Body = `Chinese`

  // Chuukese (Truk)
  case "chk":
		c.Body = `Chuukese (Truk)`

  // Macrolanguage
  case "chm":
		c.Body = `Mari`

  // Chinook jargon
  case "chn":
		c.Body = `Chinook jargon`

  // Choctaw
  case "cho":
		c.Body = `Choctaw`

  // Chipewyan; Dene Suline
  case "chp":
		c.Body = `Chipewyan; Dene Suline`

  // Cherokee
  case "chr":
		c.Body = `Cherokee`

  // Church Slavic; Old Slavonic; Church Slavonic; Old Bulgarian; Old Church Slavonic
  case "chu":
		c.Body = `Church Slavic; Old Slavonic; Church Slavonic; Old Bulgarian; Old Church Slavonic`

  // Chuvash
  case "chv":
		c.Body = `Chuvash`

  // Cheyenne
  case "chy":
		c.Body = `Cheyenne`

  // ONIX local code, equivalent to ckb in ISO 639-3. For use in ONIX 3.0 only
  case "ckb":
		c.Body = `Central Kurdish (Sorani)`

  // Collective name
  case "cmc":
		c.Body = `Chamic languages`

  // ONIX local code, equivalent to cmn in ISO 639-3
  case "cmn":
		c.Body = `Mandarin`

  // For use in ONIX 3.0 only
  case "cnr":
		c.Body = `Montenegrin`

  // Coptic
  case "cop":
		c.Body = `Coptic`

  // Cornish
  case "cor":
		c.Body = `Cornish`

  // Corsican
  case "cos":
		c.Body = `Corsican`

  // Collective name
  case "cpe":
		c.Body = `Creoles and pidgins, English-based`

  // Collective name
  case "cpf":
		c.Body = `Creoles and pidgins, French-based`

  // Collective name
  case "cpp":
		c.Body = `Creoles and pidgins, Portuguese-based`

  // Macrolanguage
  case "cre":
		c.Body = `Cree`

  // Crimean Turkish; Crimean Tatar
  case "crh":
		c.Body = `Crimean Turkish; Crimean Tatar`

  // Collective name
  case "crp":
		c.Body = `Creoles and pidgins`

  // Kashubian
  case "csb":
		c.Body = `Kashubian`

  // Collective name
  case "cus":
		c.Body = `Cushitic languages`

  // Czech
  case "cze":
		c.Body = `Czech`

  // Dakota
  case "dak":
		c.Body = `Dakota`

  // Danish
  case "dan":
		c.Body = `Danish`

  // Dargwa
  case "dar":
		c.Body = `Dargwa`

  // Collective name
  case "day":
		c.Body = `Land Dayak languages`

  // Macrolanguage
  case "del":
		c.Body = `Delaware`

  // Macrolanguage
  case "den":
		c.Body = `Slave (Athapascan)`

  // Dogrib
  case "dgr":
		c.Body = `Dogrib`

  // Macrolanguage
  case "din":
		c.Body = `Dinka`

  // Divehi; Dhivehi; Maldivian
  case "div":
		c.Body = `Divehi; Dhivehi; Maldivian`

  // Macrolanguage
  case "doi":
		c.Body = `Dogri`

  // Collective name
  case "dra":
		c.Body = `Dravidian languages`

  // Lower Sorbian
  case "dsb":
		c.Body = `Lower Sorbian`

  // Duala
  case "dua":
		c.Body = `Duala`

  // Dutch, Middle (ca. 1050-1350)
  case "dum":
		c.Body = `Dutch, Middle (ca. 1050-1350)`

  // Dutch; Flemish
  case "dut":
		c.Body = `Dutch; Flemish`

  // Dyula
  case "dyu":
		c.Body = `Dyula`

  // Dzongkha
  case "dzo":
		c.Body = `Dzongkha`

  // Efik
  case "efi":
		c.Body = `Efik`

  // ONIX local code for Italian dialect, equivalent to egl in ISO 639-3. For use in ONIX 3.0 only
  case "egl":
		c.Body = `Emilian`

  // Egyptian (Ancient)
  case "egy":
		c.Body = `Egyptian (Ancient)`

  // Ekajuk
  case "eka":
		c.Body = `Ekajuk`

  // Elamite
  case "elx":
		c.Body = `Elamite`

  // English
  case "eng":
		c.Body = `English`

  // English, Middle (1100-1500)
  case "enm":
		c.Body = `English, Middle (1100-1500)`

  // Artificial language
  case "epo":
		c.Body = `Esperanto`

  // Macrolanguage
  case "est":
		c.Body = `Estonian`

  // Ewe
  case "ewe":
		c.Body = `Ewe`

  // Ewondo
  case "ewo":
		c.Body = `Ewondo`

  // Fang
  case "fan":
		c.Body = `Fang`

  // Faroese
  case "fao":
		c.Body = `Faroese`

  // Fanti
  case "fat":
		c.Body = `Fanti`

  // Fijian
  case "fij":
		c.Body = `Fijian`

  // Filipino; Pilipino
  case "fil":
		c.Body = `Filipino; Pilipino`

  // Finnish
  case "fin":
		c.Body = `Finnish`

  // ONIX local code, equivalent to fit in ISO 639-3
  case "fit":
		c.Body = `Meänkieli / Tornedalen Finnish`

  // Collective name
  case "fiu":
		c.Body = `Finno-Ugrian languages`

  // ONIX local code, equivalent to fkv in ISO 639-3
  case "fkv":
		c.Body = `Kvensk`

  // Fon
  case "fon":
		c.Body = `Fon`

  // French
  case "fre":
		c.Body = `French`

  // French, Middle (ca. 1400-1600)
  case "frm":
		c.Body = `French, Middle (ca. 1400-1600)`

  // French, Old (ca. 842-1400)
  case "fro":
		c.Body = `French, Old (ca. 842-1400)`

  // Northern Frisian
  case "frr":
		c.Body = `Northern Frisian`

  // Eastern Frisian
  case "frs":
		c.Body = `Eastern Frisian`

  // Western Frisian
  case "fry":
		c.Body = `Western Frisian`

  // Fulah
  case "ful":
		c.Body = `Fulah`

  // Friulian
  case "fur":
		c.Body = `Friulian`

  // Gã
  case "gaa":
		c.Body = `Gã`

  // Gayo
  case "gay":
		c.Body = `Gayo`

  // Macrolanguage
  case "gba":
		c.Body = `Gbaya`

  // Collective name
  case "gem":
		c.Body = `Germanic languages`

  // Georgian
  case "geo":
		c.Body = `Georgian`

  // German
  case "ger":
		c.Body = `German`

  // Ethiopic (Ge’ez)
  case "gez":
		c.Body = `Ethiopic (Ge’ez)`

  // Gilbertese
  case "gil":
		c.Body = `Gilbertese`

  // Scottish Gaelic
  case "gla":
		c.Body = `Scottish Gaelic`

  // Irish
  case "gle":
		c.Body = `Irish`

  // Galician
  case "glg":
		c.Body = `Galician`

  // Manx
  case "glv":
		c.Body = `Manx`

  // German, Middle High (ca. 1050-1500)
  case "gmh":
		c.Body = `German, Middle High (ca. 1050-1500)`

  // German, Old High (ca. 750-1050)
  case "goh":
		c.Body = `German, Old High (ca. 750-1050)`

  // Macrolanguage
  case "gon":
		c.Body = `Gondi`

  // Gorontalo
  case "gor":
		c.Body = `Gorontalo`

  // Gothic
  case "got":
		c.Body = `Gothic`

  // Macrolanguage
  case "grb":
		c.Body = `Grebo`

  // Greek, Ancient (to 1453)
  case "grc":
		c.Body = `Greek, Ancient (to 1453)`

  // Greek, Modern (1453-)
  case "gre":
		c.Body = `Greek, Modern (1453-)`

  // Macrolanguage
  case "grn":
		c.Body = `Guarani`

  // ONIX local code, equivalent to grt in ISO 639-3
  case "grt":
		c.Body = `Garo`

  // Swiss German; Alemannic
  case "gsw":
		c.Body = `Swiss German; Alemannic`

  // Gujarati
  case "guj":
		c.Body = `Gujarati`

  // Gwich’in
  case "gwi":
		c.Body = `Gwich’in`

  // Macrolanguage
  case "hai":
		c.Body = `Haida`

  // Haitian French Creole
  case "hat":
		c.Body = `Haitian French Creole`

  // Hausa
  case "hau":
		c.Body = `Hausa`

  // Hawaiian
  case "haw":
		c.Body = `Hawaiian`

  // Hebrew
  case "heb":
		c.Body = `Hebrew`

  // Herero
  case "her":
		c.Body = `Herero`

  // Hiligaynon
  case "hil":
		c.Body = `Hiligaynon`

  // Collective name
  case "him":
		c.Body = `Himachali languages; Western Pahari languages`

  // Hindi
  case "hin":
		c.Body = `Hindi`

  // Hittite
  case "hit":
		c.Body = `Hittite`

  // Macrolanguage
  case "hmn":
		c.Body = `Hmong; Mong`

  // Hiri Motu
  case "hmo":
		c.Body = `Hiri Motu`

  // Croatian
  case "hrv":
		c.Body = `Croatian`

  // Upper Sorbian
  case "hsb":
		c.Body = `Upper Sorbian`

  // Hungarian
  case "hun":
		c.Body = `Hungarian`

  // Hupa
  case "hup":
		c.Body = `Hupa`

  // Iban
  case "iba":
		c.Body = `Iban`

  // Igbo
  case "ibo":
		c.Body = `Igbo`

  // Icelandic
  case "ice":
		c.Body = `Icelandic`

  // Artificial language
  case "ido":
		c.Body = `Ido`

  // Sichuan Yi; Nuosu
  case "iii":
		c.Body = `Sichuan Yi; Nuosu`

  // Collective name
  case "ijo":
		c.Body = `Ijo languages`

  // Macrolanguage
  case "iku":
		c.Body = `Inuktitut`

  // Artificial language
  case "ile":
		c.Body = `Interlingue; Occidental`

  // Iloko
  case "ilo":
		c.Body = `Iloko`

  // Artificial language
  case "ina":
		c.Body = `Interlingua (International Auxiliary Language Association)`

  // Collective name
  case "inc":
		c.Body = `Indic languages`

  // Indonesian
  case "ind":
		c.Body = `Indonesian`

  // Collective name
  case "ine":
		c.Body = `Indo-European languages`

  // Ingush
  case "inh":
		c.Body = `Ingush`

  // Macrolanguage
  case "ipk":
		c.Body = `Inupiaq`

  // Collective name
  case "ira":
		c.Body = `Iranian languages`

  // Collective name
  case "iro":
		c.Body = `Iroquoian languages`

  // Italian
  case "ita":
		c.Body = `Italian`

  // Javanese
  case "jav":
		c.Body = `Javanese`

  // Lojban
  case "jbo":
		c.Body = `Lojban`

  // Japanese
  case "jpn":
		c.Body = `Japanese`

  // Judeo-Persian
  case "jpr":
		c.Body = `Judeo-Persian`

  // Macrolanguage
  case "jrb":
		c.Body = `Judeo-Arabic`

  // Kara-Kalpak
  case "kaa":
		c.Body = `Kara-Kalpak`

  // Kabyle
  case "kab":
		c.Body = `Kabyle`

  // Kachin; Jingpho
  case "kac":
		c.Body = `Kachin; Jingpho`

  // Kalâtdlisut; Greenlandic
  case "kal":
		c.Body = `Kalâtdlisut; Greenlandic`

  // Kamba
  case "kam":
		c.Body = `Kamba`

  // Kannada
  case "kan":
		c.Body = `Kannada`

  // Collective name
  case "kar":
		c.Body = `Karen languages`

  // Kashmiri
  case "kas":
		c.Body = `Kashmiri`

  // Macrolanguage
  case "kau":
		c.Body = `Kanuri`

  // Kawi
  case "kaw":
		c.Body = `Kawi`

  // Kazakh
  case "kaz":
		c.Body = `Kazakh`

  // Kabardian (Circassian)
  case "kbd":
		c.Body = `Kabardian (Circassian)`

  // ONIX local code, equivalent to kdr in ISO 639-3
  case "kdr":
		c.Body = `Karaim`

  // Khasi
  case "kha":
		c.Body = `Khasi`

  // Collective name
  case "khi":
		c.Body = `Khoisan languages`

  // Central Khmer
  case "khm":
		c.Body = `Central Khmer`

  // Khotanese; Sakan
  case "kho":
		c.Body = `Khotanese; Sakan`

  // Kikuyu; Gikuyu
  case "kik":
		c.Body = `Kikuyu; Gikuyu`

  // Kinyarwanda
  case "kin":
		c.Body = `Kinyarwanda`

  // Kirghiz; Kyrgyz
  case "kir":
		c.Body = `Kirghiz; Kyrgyz`

  // Kimbundu
  case "kmb":
		c.Body = `Kimbundu`

  // Macrolanguage
  case "kok":
		c.Body = `Konkani`

  // Macrolanguage
  case "kom":
		c.Body = `Komi`

  // Macrolanguage
  case "kon":
		c.Body = `Kongo`

  // Korean
  case "kor":
		c.Body = `Korean`

  // Kusaiean (Caroline Islands)
  case "kos":
		c.Body = `Kusaiean (Caroline Islands)`

  // Macrolanguage
  case "kpe":
		c.Body = `Kpelle`

  // Karachay-Balkar
  case "krc":
		c.Body = `Karachay-Balkar`

  // Karelian
  case "krl":
		c.Body = `Karelian`

  // Collective name
  case "kro":
		c.Body = `Kru languages`

  // Kurukh
  case "kru":
		c.Body = `Kurukh`

  // Kuanyama
  case "kua":
		c.Body = `Kuanyama`

  // Kumyk
  case "kum":
		c.Body = `Kumyk`

  // Macrolanguage
  case "kur":
		c.Body = `Kurdish`

  // Kutenai
  case "kut":
		c.Body = `Kutenai`

  // Ladino
  case "lad":
		c.Body = `Ladino`

  // Macrolanguage
  case "lah":
		c.Body = `Lahnda`

  // Lamba
  case "lam":
		c.Body = `Lamba`

  // Lao
  case "lao":
		c.Body = `Lao`

  // Latin
  case "lat":
		c.Body = `Latin`

  // Macrolanguage
  case "lav":
		c.Body = `Latvian`

  // Lezgian
  case "lez":
		c.Body = `Lezgian`

  // ONIX local code for Italian dialect, equivalent to lij in ISO 639-3. For use in ONIX 3.0 only
  case "lij":
		c.Body = `Ligurian`

  // Limburgish
  case "lim":
		c.Body = `Limburgish`

  // Lingala
  case "lin":
		c.Body = `Lingala`

  // Lithuanian
  case "lit":
		c.Body = `Lithuanian`

  // ONIX local code for Italian dialect, equivalent to lmo in ISO 639-3. For use in ONIX 3.0 only
  case "lmo":
		c.Body = `Lombard`

  // Mongo-Nkundu
  case "lol":
		c.Body = `Mongo-Nkundu`

  // Lozi
  case "loz":
		c.Body = `Lozi`

  // Luxembourgish; Letzeburgesch
  case "ltz":
		c.Body = `Luxembourgish; Letzeburgesch`

  // Luba-Lulua
  case "lua":
		c.Body = `Luba-Lulua`

  // Luba-Katanga
  case "lub":
		c.Body = `Luba-Katanga`

  // Ganda
  case "lug":
		c.Body = `Ganda`

  // Luiseño
  case "lui":
		c.Body = `Luiseño`

  // Lunda
  case "lun":
		c.Body = `Lunda`

  // Luo (Kenya and Tanzania)
  case "luo":
		c.Body = `Luo (Kenya and Tanzania)`

  // Lushai
  case "lus":
		c.Body = `Lushai`

  // Macedonian
  case "mac":
		c.Body = `Macedonian`

  // Madurese
  case "mad":
		c.Body = `Madurese`

  // Magahi
  case "mag":
		c.Body = `Magahi`

  // Marshallese
  case "mah":
		c.Body = `Marshallese`

  // Maithili
  case "mai":
		c.Body = `Maithili`

  // Makasar
  case "mak":
		c.Body = `Makasar`

  // Malayalam
  case "mal":
		c.Body = `Malayalam`

  // Macrolanguage
  case "man":
		c.Body = `Mandingo`

  // Maori
  case "mao":
		c.Body = `Maori`

  // Collective name
  case "map":
		c.Body = `Austronesian languages`

  // Marathi
  case "mar":
		c.Body = `Marathi`

  // Masai
  case "mas":
		c.Body = `Masai`

  // Macrolanguage
  case "may":
		c.Body = `Malay`

  // Moksha
  case "mdf":
		c.Body = `Moksha`

  // Mandar
  case "mdr":
		c.Body = `Mandar`

  // Mende
  case "men":
		c.Body = `Mende`

  // Irish, Middle (ca. 1100-1550)
  case "mga":
		c.Body = `Irish, Middle (ca. 1100-1550)`

  // Mi’kmaq; Micmac
  case "mic":
		c.Body = `Mi’kmaq; Micmac`

  // Minangkabau
  case "min":
		c.Body = `Minangkabau`

  // Use where no suitable code is available
  case "mis":
		c.Body = `Uncoded languages`

  // Collective name
  case "mkh":
		c.Body = `Mon-Khmer languages`

  // Macrolanguage
  case "mlg":
		c.Body = `Malagasy`

  // Maltese
  case "mlt":
		c.Body = `Maltese`

  // Manchu
  case "mnc":
		c.Body = `Manchu`

  // Manipuri
  case "mni":
		c.Body = `Manipuri`

  // Collective name
  case "mno":
		c.Body = `Manobo languages`

  // Mohawk
  case "moh":
		c.Body = `Mohawk`

  // DEPRECATED – use rum
  case "mol":
		c.Body = `Moldavian; Moldovan`

  // Macrolanguage
  case "mon":
		c.Body = `Mongolian`

  // Mooré; Mossi
  case "mos":
		c.Body = `Mooré; Mossi`

  // Multiple languages
  case "mul":
		c.Body = `Multiple languages`

  // Collective name
  case "mun":
		c.Body = `Munda languages`

  // Creek
  case "mus":
		c.Body = `Creek`

  // ONIX local code, equivalent to mwf in ISO 639-3. For use in ONIX 3.0 only
  case "mwf":
		c.Body = `Murrinh-Patha`

  // Mirandese
  case "mwl":
		c.Body = `Mirandese`

  // Macrolanguage
  case "mwr":
		c.Body = `Marwari`

  // Collective name
  case "myn":
		c.Body = `Mayan languages`

  // Erzya
  case "myv":
		c.Body = `Erzya`

  // Collective name
  case "nah":
		c.Body = `Nahuatl languages`

  // Collective name
  case "nai":
		c.Body = `North American Indian languages`

  // Neapolitan
  case "nap":
		c.Body = `Neapolitan`

  // Nauruan
  case "nau":
		c.Body = `Nauruan`

  // Navajo
  case "nav":
		c.Body = `Navajo`

  // Ndebele, South
  case "nbl":
		c.Body = `Ndebele, South`

  // Ndebele, North
  case "nde":
		c.Body = `Ndebele, North`

  // Ndonga
  case "ndo":
		c.Body = `Ndonga`

  // Low German; Low Saxon
  case "nds":
		c.Body = `Low German; Low Saxon`

  // Macrolanguage
  case "nep":
		c.Body = `Nepali`

  // Newari; Nepal Bhasa
  case "new":
		c.Body = `Newari; Nepal Bhasa`

  // Nias
  case "nia":
		c.Body = `Nias`

  // Collective name
  case "nic":
		c.Body = `Niger-Kordofanian languages`

  // Niuean
  case "niu":
		c.Body = `Niuean`

  // Norwegian Nynorsk
  case "nno":
		c.Body = `Norwegian Nynorsk`

  // Norwegian Bokmål
  case "nob":
		c.Body = `Norwegian Bokmål`

  // Nogai
  case "nog":
		c.Body = `Nogai`

  // Old Norse
  case "non":
		c.Body = `Old Norse`

  // Macrolanguage
  case "nor":
		c.Body = `Norwegian`

  // N’Ko
  case "nqo":
		c.Body = `N’Ko`

  // ONIX local code, equivalent to nrf in ISO 639-3. For use in ONIX 3.0 only
  case "nrf":
		c.Body = `Guernésiais, Jèrriais`

  // Pedi; Sepedi; Northern Sotho
  case "nso":
		c.Body = `Pedi; Sepedi; Northern Sotho`

  // Collective name
  case "nub":
		c.Body = `Nubian languages`

  // Classical Newari; Old Newari; Classical Nepal Bhasa
  case "nwc":
		c.Body = `Classical Newari; Old Newari; Classical Nepal Bhasa`

  // Chichewa; Chewa; Nyanja
  case "nya":
		c.Body = `Chichewa; Chewa; Nyanja`

  // Nyamwezi
  case "nym":
		c.Body = `Nyamwezi`

  // Nyankole
  case "nyn":
		c.Body = `Nyankole`

  // Nyoro
  case "nyo":
		c.Body = `Nyoro`

  // Nzima
  case "nzi":
		c.Body = `Nzima`

  // Occitan (post 1500)
  case "oci":
		c.Body = `Occitan (post 1500)`

  // ONIX local code, equivalent to odt in ISO 639-3
  case "odt":
		c.Body = `Old Dutch / Old Low Franconian (ca. 400–1050)`

  // Macrolanguage
  case "oji":
		c.Body = `Ojibwa`

  // ONIX local code, equivalent to omq in ISO 639-5. Collective name
  case "omq":
		c.Body = `Oto-Manguean languages`

  // Macrolanguage
  case "ori":
		c.Body = `Oriya`

  // Macrolanguage
  case "orm":
		c.Body = `Oromo`

  // Osage
  case "osa":
		c.Body = `Osage`

  // Ossetian; Ossetic
  case "oss":
		c.Body = `Ossetian; Ossetic`

  // Turkish, Ottoman
  case "ota":
		c.Body = `Turkish, Ottoman`

  // Collective name
  case "oto":
		c.Body = `Otomian languages`

  // Collective name
  case "paa":
		c.Body = `Papuan languages`

  // Pangasinan
  case "pag":
		c.Body = `Pangasinan`

  // Pahlavi
  case "pal":
		c.Body = `Pahlavi`

  // Pampanga; Kapampangan
  case "pam":
		c.Body = `Pampanga; Kapampangan`

  // Panjabi
  case "pan":
		c.Body = `Panjabi`

  // Papiamento
  case "pap":
		c.Body = `Papiamento`

  // Palauan
  case "pau":
		c.Body = `Palauan`

  // Old Persian (ca. 600-400 B.C.)
  case "peo":
		c.Body = `Old Persian (ca. 600-400 B.C.)`

  // Macrolanguage
  case "per":
		c.Body = `Persian; Farsi`

  // ONIX local code, equivalent to pes in ISO 639-3. For use in ONIX 3.0 only
  case "pes":
		c.Body = `Iranian Persian; Parsi`

  // Collective name
  case "phi":
		c.Body = `Philippine languages`

  // Phoenician
  case "phn":
		c.Body = `Phoenician`

  // Pali
  case "pli":
		c.Body = `Pali`

  // ONIX local code for Italian dialect, equivalent to pms in ISO 639-3. For use in ONIX 3.0 only
  case "pms":
		c.Body = `Piedmontese`

  // Polish
  case "pol":
		c.Body = `Polish`

  // Ponapeian
  case "pon":
		c.Body = `Ponapeian`

  // Portuguese
  case "por":
		c.Body = `Portuguese`

  // Collective name
  case "pra":
		c.Body = `Prakrit languages`

  // Provençal, Old (to 1500); Occitan, Old (to 1500)
  case "pro":
		c.Body = `Provençal, Old (to 1500); Occitan, Old (to 1500)`

  // ONIX local code, equivalent to prs in ISO 639-3. For use in ONIX 3.0 only
  case "prs":
		c.Body = `Dari; Afghan Persian`

  // Macrolanguage
  case "pus":
		c.Body = `Pushto; Pashto`

  // ONIX local code, distinct dialect of Occitan (not distinguished from oci by ISO 639-3)
  case "qar":
		c.Body = `Aranés`

  // ONIX local code, distinct dialect of Catalan (not distinguished from cat by ISO 639-3)
  case "qav":
		c.Body = `Valencian`

  // ONIX local code, distinct variant of langue d’oïl (old northern French) (not distinguished from fro, or from frm, fre, nrf by ISO 639-3). For use in ONIX 3.0 only
  case "qgl":
		c.Body = `Gallo`

  // ONIX local code, distinct dialect of of Rusyn (not distinguished from rue by ISO 639-3). For use in ONIX 3.0 only
  case "qlk":
		c.Body = `Lemko`

  // ONIX local code, distinct and exclusively spoken variation of Spanish, not distinguished from spa (Spanish, Castilian) by ISO 639-3. Neutral Latin American Spanish should be considered a ‘shorthand’ for spa plus a ‘country code’ for Latin America – but prefer spa plus the relevant country code for specifically Mexican Spanish, Argentine (Rioplatense) Spanish, Puerto Rican Spanish etc. Neutral Latin American Spanish must only be used with audio material (including the audio tracks of TV, video and film) to indicate use of accent, vocabulary and construction suitable for broad use across Latin America. For use in ONIX 3.0 only
  case "qls":
		c.Body = `Neutral Latin American Spanish`

  // Macrolanguage
  case "que":
		c.Body = `Quechua`

  // Macrolanguage
  case "raj":
		c.Body = `Rajasthani`

  // Rapanui
  case "rap":
		c.Body = `Rapanui`

  // Rarotongan; Cook Islands Maori
  case "rar":
		c.Body = `Rarotongan; Cook Islands Maori`

  // ONIX local code, equivalent to rcf in ISO 639-3. For use in ONIX 3.0 only
  case "rcf":
		c.Body = `Réunion Creole French`

  // ONIX local code for Italian dialect, equivalent to rgl in ISO 639-3. For use in ONIX 3.0 only
  case "rgn":
		c.Body = `Romagnol`

  // Collective name
  case "roa":
		c.Body = `Romance languages`

  // Romansh
  case "roh":
		c.Body = `Romansh`

  // Macrolanguage
  case "rom":
		c.Body = `Romany`

  // Romanian
  case "rum":
		c.Body = `Romanian`

  // Rundi
  case "run":
		c.Body = `Rundi`

  // Aromanian; Arumanian; Macedo-Romanian
  case "rup":
		c.Body = `Aromanian; Arumanian; Macedo-Romanian`

  // Russian
  case "rus":
		c.Body = `Russian`

  // Sandawe
  case "sad":
		c.Body = `Sandawe`

  // Sango
  case "sag":
		c.Body = `Sango`

  // Yakut
  case "sah":
		c.Body = `Yakut`

  // Collective name
  case "sai":
		c.Body = `South American Indian languages`

  // Collective name
  case "sal":
		c.Body = `Salishan languages`

  // Samaritan Aramaic
  case "sam":
		c.Body = `Samaritan Aramaic`

  // Sanskrit
  case "san":
		c.Body = `Sanskrit`

  // Sasak
  case "sas":
		c.Body = `Sasak`

  // Santali
  case "sat":
		c.Body = `Santali`

  // DEPRECATED – use srp
  case "scc":
		c.Body = `Serbian`

  // Sicilian
  case "scn":
		c.Body = `Sicilian`

  // Scots
  case "sco":
		c.Body = `Scots`

  // DEPRECATED – use hrv
  case "scr":
		c.Body = `Croatian`

  // ONIX local code for Sardinian dialect, equivalent to sdc in ISO 639-3. For use in ONIX 3.0 only
  case "sdc":
		c.Body = `Sassarese`

  // ONIX local code for Sardinian dialect, equivalent to sdn in ISO 639-3. For use in ONIX 3.0 only
  case "sdn":
		c.Body = `Gallurese`

  // Selkup
  case "sel":
		c.Body = `Selkup`

  // Collective name
  case "sem":
		c.Body = `Semitic languages`

  // Irish, Old (to 1100)
  case "sga":
		c.Body = `Irish, Old (to 1100)`

  // Collective name
  case "sgn":
		c.Body = `Sign languages`

  // Shan
  case "shn":
		c.Body = `Shan`

  // Sidamo
  case "sid":
		c.Body = `Sidamo`

  // Sinhala; Sinhalese
  case "sin":
		c.Body = `Sinhala; Sinhalese`

  // Collective name
  case "sio":
		c.Body = `Siouan languages`

  // Collective name
  case "sit":
		c.Body = `Sino-Tibetan languages`

  // Collective name
  case "sla":
		c.Body = `Slavic languages`

  // Slovak
  case "slo":
		c.Body = `Slovak`

  // Slovenian
  case "slv":
		c.Body = `Slovenian`

  // Southern Sami
  case "sma":
		c.Body = `Southern Sami`

  // Northern Sami
  case "sme":
		c.Body = `Northern Sami`

  // Collective name
  case "smi":
		c.Body = `Sami languages`

  // Lule Sami
  case "smj":
		c.Body = `Lule Sami`

  // Inari Sami
  case "smn":
		c.Body = `Inari Sami`

  // Samoan
  case "smo":
		c.Body = `Samoan`

  // Skolt Sami
  case "sms":
		c.Body = `Skolt Sami`

  // Shona
  case "sna":
		c.Body = `Shona`

  // Sindhi
  case "snd":
		c.Body = `Sindhi`

  // Soninke
  case "snk":
		c.Body = `Soninke`

  // Sogdian
  case "sog":
		c.Body = `Sogdian`

  // Somali
  case "som":
		c.Body = `Somali`

  // Collective name
  case "son":
		c.Body = `Songhai languages`

  // Sotho; Sesotho
  case "sot":
		c.Body = `Sotho; Sesotho`

  // Spanish
  case "spa":
		c.Body = `Spanish`

  // Macrolanguage
  case "srd":
		c.Body = `Sardinian`

  // Sranan Tongo
  case "srn":
		c.Body = `Sranan Tongo`

  // ONIX local code for Sardinian dialect, equivalent to sro in ISO 639-3. For use in ONIX 3.0 only
  case "sro":
		c.Body = `Campidanese`

  // Serbian
  case "srp":
		c.Body = `Serbian`

  // Serer
  case "srr":
		c.Body = `Serer`

  // Collective name
  case "ssa":
		c.Body = `Nilo-Saharan languages`

  // Swazi; Swati
  case "ssw":
		c.Body = `Swazi; Swati`

  // Sukuma
  case "suk":
		c.Body = `Sukuma`

  // Sundanese
  case "sun":
		c.Body = `Sundanese`

  // Susu
  case "sus":
		c.Body = `Susu`

  // Sumerian
  case "sux":
		c.Body = `Sumerian`

  // Macrolanguage
  case "swa":
		c.Body = `Swahili`

  // Swedish
  case "swe":
		c.Body = `Swedish`

  // Classical Syriac
  case "syc":
		c.Body = `Classical Syriac`

  // Macrolanguage
  case "syr":
		c.Body = `Syriac`

  // Tahitian
  case "tah":
		c.Body = `Tahitian`

  // Collective name
  case "tai":
		c.Body = `Tai languages`

  // Tamil
  case "tam":
		c.Body = `Tamil`

  // Tatar
  case "tat":
		c.Body = `Tatar`

  // Telugu
  case "tel":
		c.Body = `Telugu`

  // Temne; Time
  case "tem":
		c.Body = `Temne; Time`

  // Terena
  case "ter":
		c.Body = `Terena`

  // Tetum
  case "tet":
		c.Body = `Tetum`

  // Tajik; Tajiki Persian
  case "tgk":
		c.Body = `Tajik; Tajiki Persian`

  // Tagalog
  case "tgl":
		c.Body = `Tagalog`

  // Thai
  case "tha":
		c.Body = `Thai`

  // Tibetan
  case "tib":
		c.Body = `Tibetan`

  // Tigré
  case "tig":
		c.Body = `Tigré`

  // Tigrinya
  case "tir":
		c.Body = `Tigrinya`

  // Tiv
  case "tiv":
		c.Body = `Tiv`

  // Tokelauan
  case "tkl":
		c.Body = `Tokelauan`

  // Artificial language
  case "tlh":
		c.Body = `Klingon; tlhIngan-Hol`

  // Tlingit
  case "tli":
		c.Body = `Tlingit`

  // Macrolanguage
  case "tmh":
		c.Body = `Tamashek`

  // Tonga (Nyasa)
  case "tog":
		c.Body = `Tonga (Nyasa)`

  // Tongan
  case "ton":
		c.Body = `Tongan`

  // Tok Pisin
  case "tpi":
		c.Body = `Tok Pisin`

  // Tsimshian
  case "tsi":
		c.Body = `Tsimshian`

  // AKA Setswana
  case "tsn":
		c.Body = `Tswana`

  // Tsonga
  case "tso":
		c.Body = `Tsonga`

  // Turkmen
  case "tuk":
		c.Body = `Turkmen`

  // Tumbuka
  case "tum":
		c.Body = `Tumbuka`

  // Collective name
  case "tup":
		c.Body = `Tupi languages`

  // Turkish
  case "tur":
		c.Body = `Turkish`

  // Altaic languages
  case "tut":
		c.Body = `Altaic languages`

  // Tuvaluan
  case "tvl":
		c.Body = `Tuvaluan`

  // Twi
  case "twi":
		c.Body = `Twi`

  // Tuvinian
  case "tyv":
		c.Body = `Tuvinian`

  // ONIX local code, equivalent to tzo in ISO 639-3
  case "tzo":
		c.Body = `Tzotzil`

  // Udmurt
  case "udm":
		c.Body = `Udmurt`

  // Ugaritic
  case "uga":
		c.Body = `Ugaritic`

  // Uighur; Uyghur
  case "uig":
		c.Body = `Uighur; Uyghur`

  // Ukrainian
  case "ukr":
		c.Body = `Ukrainian`

  // Umbundu
  case "umb":
		c.Body = `Umbundu`

  // Undetermined language
  case "und":
		c.Body = `Undetermined language`

  // Urdu
  case "urd":
		c.Body = `Urdu`

  // Macrolanguage
  case "uzb":
		c.Body = `Uzbek`

  // Vai
  case "vai":
		c.Body = `Vai`

  // ONIX local code for Italian dialect, equivalent to vec in ISO 639-3. For use in ONIX 3.0 only
  case "vec":
		c.Body = `Venetian/Venetan`

  // Venda
  case "ven":
		c.Body = `Venda`

  // Vietnamese
  case "vie":
		c.Body = `Vietnamese`

  // Artificial language
  case "vol":
		c.Body = `Volapük`

  // Votic
  case "vot":
		c.Body = `Votic`

  // Collective name
  case "wak":
		c.Body = `Wakashan languages`

  // Wolaitta; Wolaytta
  case "wal":
		c.Body = `Wolaitta; Wolaytta`

  // Waray
  case "war":
		c.Body = `Waray`

  // Washo
  case "was":
		c.Body = `Washo`

  // Welsh
  case "wel":
		c.Body = `Welsh`

  // Collective name
  case "wen":
		c.Body = `Sorbian languages`

  // Walloon
  case "wln":
		c.Body = `Walloon`

  // Wolof
  case "wol":
		c.Body = `Wolof`

  // Kalmyk
  case "xal":
		c.Body = `Kalmyk`

  // Xhosa
  case "xho":
		c.Body = `Xhosa`

  // ONIX local code, equivalent to xuu in ISO 639-3. For use in ONIX 3.0 only
  case "xuu":
		c.Body = `Khwedam, Kxoe`

  // Yao
  case "yao":
		c.Body = `Yao`

  // Yapese
  case "yap":
		c.Body = `Yapese`

  // Macrolanguage
  case "yid":
		c.Body = `Yiddish`

  // Yoruba
  case "yor":
		c.Body = `Yoruba`

  // Collective name
  case "ypk":
		c.Body = `Yupik languages`

  // ONIX local code, equivalent to yue in ISO 639-3
  case "yue":
		c.Body = `Cantonese`

  // Macrolanguage
  case "zap":
		c.Body = `Zapotec`

  // Artificial language
  case "zbl":
		c.Body = `Blissymbols; Blissymbolics; Bliss`

  // Zenaga
  case "zen":
		c.Body = `Zenaga`

  // Standard Moroccan Tamazight
  case "zgh":
		c.Body = `Standard Moroccan Tamazight`

  // Macrolanguage
  case "zha":
		c.Body = `Zhuang; Chuang`

  // Collective name
  case "znd":
		c.Body = `Zande languages`

  // Zulu
  case "zul":
		c.Body = `Zulu`

  // Zuni
  case "zun":
		c.Body = `Zuni`

  // No linguistic content
  case "zxx":
		c.Body = `No linguistic content`

  // Macrolanguage
  case "zza":
		c.Body = `Zaza; Dimili; Dimli; Kirdki; Kirmanjki; Zazaki`
	default:
		return fmt.Errorf("undefined code for LanguageCode has been passed, got [%s]", v)
	}
	return nil
}

// LanguageRole Language role
type LanguageRole struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *LanguageRole) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // Language of text
  case "01":
		c.Body = `Language of text`

  // Where the text in the original language is NOT part of the current product
  case "02":
		c.Body = `Original language of a translated text`

  // Where different from language of text: used mainly for serials
  case "03":
		c.Body = `Language of abstracts`

  // Where the text in the original language is part of a bilingual or multilingual product
  case "06":
		c.Body = `Original language in a multilingual edition`

  // Where the text in a translated language is part of a bilingual or multilingual product
  case "07":
		c.Body = `Translated language in a multilingual edition`

  // For example, on an audiobook or video product. Use for the only available audio track, or where there are multiple tracks (eg on a DVD), for an alternate language audio track that is NOT the original. (In the latter case, use code 11 for the original language audio if it is included in the product, or code 10 to identify an original language that is not present in the product)
  case "08":
		c.Body = `Language of audio track`

  // For example, on a DVD
  case "09":
		c.Body = `Language of subtitles`

  // Where the audio in the original language is NOT part of the current product
  case "10":
		c.Body = `Language of original audio track`

  // Where the audio in the original language is part of a multilingual product with multiple audio tracks
  case "11":
		c.Body = `Original language audio track in a multilingual product`

  // Use for the language of footnotes, endnotes, annotations or commentary, etc, where it is different from the language of the main text
  case "12":
		c.Body = `Language of notes`
	default:
		return fmt.Errorf("undefined code for LanguageRole has been passed, got [%s]", v)
	}
	return nil
}

// LocationIDType Supplier identifier type
type LocationIDType struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *LocationIDType) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // Note that <IDTypeName> is required with proprietary identifiers
  case "01":
		c.Body = `Proprietary`

  // DEPRECATED – use 01
  case "02":
		c.Body = `Proprietary`

  // Börsenverein Verkehrsnummer
  case "04":
		c.Body = `Börsenverein Verkehrsnummer`

  // German ISBN Agency publisher identifier
  case "05":
		c.Body = `German ISBN Agency publisher identifier`

  // GS1 global location number (formerly EAN location number)
  case "06":
		c.Body = `GLN`

  // Book trade Standard Address Number – US, UK etc
  case "07":
		c.Body = `SAN`

  // Flemish supplier code
  case "12":
		c.Body = `Distributeurscode Boekenbank`

  // Flemish publisher code
  case "13":
		c.Body = `Fondscode Boekenbank`

  // Identifier for a business organization for VAT purposes, eg within the EU’s VIES system. See http://ec.europa.eu/taxation_customs/vies/faqvies.do for EU VAT ID formats, which vary from country to country. Generally these consist of a two-letter country code followed by the 8–12 digits of the national VAT ID. Some countries include one or two letters within their VAT ID. See http://en.wikipedia.org/wiki/VAT_identification_number for non-EU countries that maintain similar identifiers. Spaces, dashes etc should be omitted
  case "23":
		c.Body = `VAT Identity Number`
	default:
		return fmt.Errorf("undefined code for LocationIDType has been passed, got [%s]", v)
	}
	return nil
}

// MarketDateRole Publishing date role
type MarketDateRole struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *MarketDateRole) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // Nominal date of publication. This date is primarily used for planning, promotion and other business process purposes, and is not necessarily the first date for retail sales or fulfillment of pre-orders. In the absence of a sales embargo date, retail sales and pre-order fulfillment may begin as soon as stock is available to the retailer
  case "01":
		c.Body = `Publication date`

  // If there is an embargo on retail sales (in the market) before a certain date, the date from which the embargo is lifted and retail sales and fulfillment of pre-orders are permitted. In the absence of an embargo date, retail sales and pre-order fulfillment may begin as soon as stock is available to the retailer
  case "02":
		c.Body = `Sales embargo date`

  // Date when a new product may be announced to the general public. Prior to the announcement date, the product data is intended for internal use by the recipient and supply chain partners only. After the announcement date, or in the absence of an announcement date, the planned product may be announced to the public as soon as metadata is available
  case "09":
		c.Body = `Public announcement date`

  // Date when a new product may be announced to the book trade only. Prior to the announcement date, the product information is intended for internal use by the recipient only. After the announcement date, or in the absence of a trade announcement date, the planned product may be announced to supply chain partners (but not necessarily made public – see the Public announcement date) as soon as metadata is available
  case "10":
		c.Body = `Trade announcement date`

  // Date when the work incorporated in a product was first published. For works in translation, see also Date of first publication in original language (code 20)
  case "11":
		c.Body = `Date of first publication`

  // Date when a product was last reprinted
  case "12":
		c.Body = `Last reprint date`

  // Date when a product was (or will be) declared out-of-print or deleted
  case "13":
		c.Body = `Out-of-print / deletion date`

  // Date when a product was last reissued
  case "16":
		c.Body = `Last reissue date`

  // Date of publication of a printed book which is the direct print counterpart to a digital product. The counterpart product may be included in <RelatedProduct> using code 13
  case "19":
		c.Body = `Publication date of print counterpart`

  // Date when the original language version of work incorporated in a product was first published (note, use only on works in translation – see code 11 for first publication date in the translated language)
  case "20":
		c.Body = `Date of first publication in original language`

  // Date when a product will be reissued
  case "21":
		c.Body = `Forthcoming reissue date`

  // Date when a product that has been temporary withdrawn from sale or recalled for any reason is expected to become available again, eg after correction of quality or technical issues
  case "22":
		c.Body = `Expected availability date after temporary withdrawal`

  // Date from which reviews of a product may be published eg in newspapers and magazines or online. Provided to the book trade for information only: newspapers and magazines are not expected to be recipients of ONIX metadata
  case "23":
		c.Body = `Review embargo date`

  // Latest date on which an order may be placed with the publisher for guaranteed delivery prior to the publication date. May or may not be linked to a special reservation or pre-publication price
  case "25":
		c.Body = `Publisher’s reservation order deadline`

  // Date when a product will be reprinted
  case "26":
		c.Body = `Forthcoming reprint date`

  // Earliest date a retail ‘preorder’ can be placed (in the market), where this is distinct from the public announcement date. In the absence of a preorder embargo, advance orders can be placed as soon as metadata is available to the consumer (this would be the public announcement date, or in the absence of a public announcement date, the earliest date metadata is available to the retailer)
  case "27":
		c.Body = `Preorder embargo date`

  // Date of acquisition of product by new publisher (use with publishing roles 09 and 13)
  case "28":
		c.Body = `Transfer date`

  // For an audiovisual work (eg on DVD)
  case "29":
		c.Body = `Date of production`

  // For digital products that are available to end customers both as a download and streamed, the earliest date the product can be made available on a stream, where the streamed version becomes available later than the download. For the download, see code 02 if it is embargoed or code 01 if there is no embargo
  case "30":
		c.Body = `Streaming embargo date`

  // For digital products that are available to end customers both as purchases and as part of a subscription package, the earliest date the product can be made available by subscription, where the product may not be included in a subscription package until shome while after publication. For ordinary sales, see code 02 if there is a sales embargo or code 01 if there is no embargo
  case "31":
		c.Body = `Subscription embargo date`

  // Date by which CIP copy is required for inclusion in the product
  case "35":
		c.Body = `CIP date`
	default:
		return fmt.Errorf("undefined code for MarketDateRole has been passed, got [%s]", v)
	}
	return nil
}

// MarketPublishingStatus Market publishing status
type MarketPublishingStatus struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *MarketPublishingStatus) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // Status is not specified (as distinct from unknown): the default if the <MarketPublishingStatus> element is not sent
  case "00":
		c.Body = `Unspecified`

  // The product was announced for publication in this market, and subsequently abandoned. A market publication date must not be sent
  case "01":
		c.Body = `Cancelled`

  // Not yet published in this market, should be accompanied by expected local publication date
  case "02":
		c.Body = `Forthcoming`

  // The product was announced for publication in this market, and subsequently postponed with no expected local publication date. A market publication date must not be sent
  case "03":
		c.Body = `Postponed indefinitely`

  // The product was published in this market, and is still active in the sense that the publisher will accept orders for it, though it may or may not be immediately available, for which see <SupplyDetail>
  case "04":
		c.Body = `Active`

  // Responsibility for the product in this market has been transferred elsewhere (with details of acquiring publisher representative in this market if possible in PR.25 (ONIX 2.1) OR P.25 (ONIX 3.0))
  case "05":
		c.Body = `No longer our product`

  // The product was active in this market, but is now inactive in the sense that (a) the publisher representative (local publisher or sales agent) cannot fulfill orders for it, though stock may still be available elsewhere in the supply chain, and (b) there are no current plans to bring it back into stock in this market. Code 06 does not specifically imply that returns are or are not still accepted
  case "06":
		c.Body = `Out of stock indefinitely`

  // The product was active in this market, but is now permanently inactive in this market in the sense that (a) the publisher representative (local publisher or sales agent) will not accept orders for it, though stock may still be available elsewhere in the supply chain, and (b) the product will not be made available again in this market under the same ISBN. Code 07 normally implies that the publisher will not accept returns beyond a specified date
  case "07":
		c.Body = `Out of print`

  // The product was active in this market, but is now permanently or indefinitely inactive in the sense that the publisher representative (local publisher or sales agent) will not accept orders for it, though stock may still be available elsewhere in the supply chain. Code 08 covers both of codes 06 and 07, and may be used where the distinction between those values is either unnecessary or meaningless
  case "08":
		c.Body = `Inactive`

  // The sender of the ONIX record does not know the current publishing status in this market
  case "09":
		c.Body = `Unknown`

  // The product is no longer available in this market from the publisher representative (local publisher or sales agent), under the current ISBN, at the current price. It may be available to be traded through another channel, usually at a reduced price
  case "10":
		c.Body = `Remaindered`

  // Withdrawn from sale in this market, typically for legal reasons or to avoid giving offence
  case "11":
		c.Body = `Withdrawn from sale`

  // Either no rights are held for the product in this market, or for other reasons the publisher has decided not to make it available in this market
  case "12":
		c.Body = `Not available in this market`

  // The product is published in this market and active but, as a publishing decision, it is not sold separately – only in an assembly or as part of a package. Depending on product composition and pricing, it may be saleable separately at retail
  case "13":
		c.Body = `Active, but not sold separately`

  // The product is published in this market and active, but is not available to all customer types, typically because the market is split between exclusive sales agents for different market segments. In ONIX 2.1, should be accompanied by a free-text statement in <MarketRestrictionDetail> describing the nature of the restriction. In ONIX 3.0, the <SalesRestriction> composite in Group P.24 should be used
  case "14":
		c.Body = `Active, with market restrictions`

  // Recalled in this market for reasons of consumer safety
  case "15":
		c.Body = `Recalled`

  // Temporarily withdrawn from sale in this market, typically for quality or technical reasons. In ONIX 3.0, must be accompanied by expected availability date coded ‘22’ within the <MarketDate> composite, except in exceptional circumstances where no date is known
  case "16":
		c.Body = `Temporarily withdrawn from sale`
	default:
		return fmt.Errorf("undefined code for MarketPublishingStatus has been passed, got [%s]", v)
	}
	return nil
}

// MeasureType Measure type
type MeasureType struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *MeasureType) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // For a book, the overall height when standing on a shelf. For a folded map, the height when folded. For packaged products, the height of the retail packaging, and for trade-only products, the height of the trade packaging. In general, the height of a product in the form in which it is presented or packaged for retail sale
  case "01":
		c.Body = `Height`

  // For a book, the overall horizontal dimension of the cover when standing upright. For a folded map, the width when folded. For packaged products, the width of the retail packaging, and for trade-only products, the width of the trade packaging. In general, the width of a product in the form in which it is presented or packaged for retail sale
  case "02":
		c.Body = `Width`

  // For a book, the overall thickness of the spine. For a folded map, the thickness when folded. For packaged products, the depth of the retail packaging, and for trade-only products, the depth of the trade packaging. In general, the thickness or depth of a product in the form in which it is presented or packaged for retail sale
  case "03":
		c.Body = `Thickness`

  // Not recommended for general use
  case "04":
		c.Body = `Page trim height`

  // Not recommended for general use
  case "05":
		c.Body = `Page trim width`

  // The volume of the product, including any retail packaging. Note the <MeasureUnit> is interpreted as a volumetric unit – for example code cm = cubic centimetres (ie millilitres), and code oz = fluid ounces. For use in ONIX 3.0 only
  case "06":
		c.Body = `Unit volume`

  // Volume of the internal (fluid) contents of a product (eg of paint in a can). Note the <MeasureUnit> is interpreted as a volumetric unit – for example code cm = cubic centimetres (ie millilitres), and code oz = fluid ounces. For use in ONIX 3.0 only
  case "07":
		c.Body = `Unit capacity`

  // The weight of the product, including any retail packaging
  case "08":
		c.Body = `Unit weight`

  // Of a globe, for example
  case "09":
		c.Body = `Diameter (sphere)`

  // The height of a folded or rolled sheet map, poster etc when unfolded
  case "10":
		c.Body = `Unfolded/unrolled sheet height`

  // The width of a folded or rolled sheet map, poster etc when unfolded
  case "11":
		c.Body = `Unfolded/unrolled sheet width`

  // The diameter of the cross-section of a tube or cylinder, usually carrying a rolled sheet product. Use 01 ‘Height’ for the height or length of the tube
  case "12":
		c.Body = `Diameter (tube or cylinder)`

  // The length of a side of the cross-section of a long triangular or square package, usually carrying a rolled sheet product. Use 01 ‘Height’ for the height or length of the package
  case "13":
		c.Body = `Rolled sheet package side measure`

  // As height, but of the product without packaging (use only for products supplied in retail packaging, must also supply overall size when packaged using code 01). For use in ONIX 3.0 only
  case "14":
		c.Body = `Unpackaged height`

  // As width, but of the product without packaging (use only for products supplied in retail packaging, must also supply overall size when packaged using code 02). For use in ONIX 3.0 only
  case "15":
		c.Body = `Unpackaged width`

  // As thickness, but of the product without packaging (use only for products supplied in retail packaging, must also supply overall size when packaged using code 03). For use in ONIX 3.0 only
  case "16":
		c.Body = `Unpackaged thickness`

  // Weight of batteries built-in, pre-installed or supplied with the product. Details of the batteries should be provided using <ProductFormFeature>. A per-battery unit weight may be calculated from the number of batteries if required. Use only with ONIX 3.0
  case "17":
		c.Body = `Total battery weight`

  // Mass or equivalent mass of elemental Lithium within the batteries built-in, pre-installed or supplied with the product (eg a Lithium Iron phosphate battery with 160g of cathode material would have a total of around 7g of Lithium). Details of the batteries must be provided using ProductFormFeature. A per-battery unit mass of Lithium may be calculated from the number of batteries if required. Use only with ONIX 3.0
  case "18":
		c.Body = `Total weight of Lithium`

  // For use where product or part of product requires assembly, for example the size of a completed kit, puzzle or assembled display piece. The assembled dimensions may be larger than the product size as supplied. Use only when the unassembled dimensions as supplied (including any retail or trade packaging) are also provided using codes 01, 02 and 03. Use only with ONIX 3.0
  case "19":
		c.Body = `Assembled length`

  // Assembled width
  case "20":
		c.Body = `Assembled width`

  // Assembled height
  case "21":
		c.Body = `Assembled height`

  // Includes packaging. See <PackQuantity> for number of copies of the product per pack, and used only when dimensions of individual copies (codes 01, 02, 03) AND <PackQuantity> are supplied. Note that neither orders nor deliveries have to be aligned with multiples of the pack quantity, but such orders and deliveries may be more convenient to handle. Use only with ONIX 3.0
  case "23":
		c.Body = `Carton length`

  // Carton width
  case "24":
		c.Body = `Carton width`

  // Carton height
  case "25":
		c.Body = `Carton height`

  // Includes the weight of product(s) within the carton. See <PackQuantity> for number of copies per pack, and used only when the weight of individual copies (code 08) AND <PackQuantity> are supplied. Use only with ONIX 3.0
  case "26":
		c.Body = `Carton weight`

  // Includes pallet and packaging. See <PalletQuantity> for number of copies of the product per pallet, and used only when dimensions of individual copies (codes 01, 02, 03) AND <PalletQuantity> are supplied. Note that neither orders nor deliveries have to be aligned with multiples of the pallet quantity, but such orders and deliveries may be more convenient to handle. Use only with ONIX 3.0
  case "27":
		c.Body = `Pallet length`

  // Pallet width
  case "28":
		c.Body = `Pallet width`

  // Pallet height
  case "29":
		c.Body = `Pallet height`

  // Includes the weight of product(s) and cartons stacked on the pallet. See <PalletQuantity> for the number of copies per pallet, and used only when the weight of individual copies (code 08) AND <PalletQuantity> are supplied. Use only with ONIX 3.0
  case "30":
		c.Body = `Pallet weight`
	default:
		return fmt.Errorf("undefined code for MeasureType has been passed, got [%s]", v)
	}
	return nil
}

// MeasureUnitCode Measure unit
type MeasureUnitCode struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *MeasureUnitCode) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // Millimeters are the preferred metric unit of length
  case "cm":
		c.Body = `Centimeters`

  // Grams
  case "gr":
		c.Body = `Grams`

  // Inches (US)
  case "in":
		c.Body = `Inches (US)`

  // Grams are the preferred metric unit of weight
  case "kg":
		c.Body = `Kilograms`

  // Pounds (US)
  case "lb":
		c.Body = `Pounds (US)`

  // Millimeters
  case "mm":
		c.Body = `Millimeters`

  // Ounces (US)
  case "oz":
		c.Body = `Ounces (US)`

  // Pixels
  case "px":
		c.Body = `Pixels`
	default:
		return fmt.Errorf("undefined code for MeasureUnitCode has been passed, got [%s]", v)
	}
	return nil
}

// NameIDType Name identifier type
type NameIDType struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *NameIDType) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // Note that <IDTypeName> is required with proprietary identifiers
  case "01":
		c.Body = `Proprietary`

  // DEPRECATED – use 01
  case "02":
		c.Body = `Proprietary`

  // Deutsche Nationalbibliothek publisher identifier
  case "03":
		c.Body = `DNB publisher identifier`

  // Börsenverein Verkehrsnummer
  case "04":
		c.Body = `Börsenverein Verkehrsnummer`

  // German ISBN Agency publisher identifier
  case "05":
		c.Body = `German ISBN Agency publisher identifier`

  // GS1 global location number (formerly EAN location number)
  case "06":
		c.Body = `GLN`

  // Book trade Standard Address Number – US, UK etc
  case "07":
		c.Body = `SAN`

  // MARC code list for organizations – see http://www.loc.gov/marc/organizations/orgshome.html
  case "08":
		c.Body = `MARC organization code`

  // Trading party identifier used in the Netherlands
  case "10":
		c.Body = `Centraal Boekhuis Relatie ID`

  // Flemish publisher code
  case "13":
		c.Body = `Fondscode Boekenbank`

  // Business Identity Code (Finland). See http://www.ytj.fi/ (in Finnish)
  case "15":
		c.Body = `Y-tunnus`

  // International Standard Name Identifier. A sixteen digit number. Usually presented with spaces or hyphens dividing the number into four groups of four digits, but in ONIX the spaces or hyphens should be omitted. See http://www.isni.org/
  case "16":
		c.Body = `ISNI`

  // Personennamendatei – person name authority file used by Deutsche Nationalbibliothek and in other German-speaking countries. See http://www.dnb.de/standardisierung/normdateien/pnd.htm (German) or http://www.dnb.de/eng/standardisierung/normdateien/pnd.htm (English). DEPRECATED in favor of the GND
  case "17":
		c.Body = `PND`

  // A control number assigned to a Library of Congress Control Number (LCCN) Name Authority / NACO record
  case "18":
		c.Body = `NACO`

  // Publisher identifier administered by Japanese ISBN Agency
  case "19":
		c.Body = `Japanese Publisher identifier`

  // Gemeinsame Körperschaftsdatei – Corporate Body Authority File in the German-speaking countries. See http://www.dnb.de/standardisierung/normdateien/gkd.htm (German) or http://www.dnb.de/eng/standardisierung/normdateien/gkd.htm (English). DEPRECATED in favor of the GND
  case "20":
		c.Body = `GKD`

  // Open Researcher and Contributor ID. A sixteen digit number. Usually presented with hyphens dividing the number into four groups of four digits, but in ONIX the hyphens should be omitted. See http://www.orcid.org/
  case "21":
		c.Body = `ORCID`

  // Publisher identifier maintained by the Chinese ISBN Agency (GAPP)
  case "22":
		c.Body = `GAPP Publisher Identifier`

  // Identifier for a business organization for VAT purposes, eg within the EU’s VIES system. See http://ec.europa.eu/taxation_customs/vies/faqvies.do for EU VAT ID formats, which vary from country to country. Generally these consist of a two-letter country code followed by the 8–12 digits of the national VAT ID. Some countries include one or two letters within their VAT ID. See http://en.wikipedia.org/wiki/VAT_identification_number for non-EU countries that maintain similar identifiers. Spaces, dashes etc should be omitted
  case "23":
		c.Body = `VAT Identity Number`

  // 4-digit business organization identifier controlled by the Japanese Publication Wholesalers Association
  case "24":
		c.Body = `JP Distribution Identifier`

  // Gemeinsame Normdatei – Joint Authority File in the German-speaking countries. See http://www.dnb.de/EN/gnd (English). Combines the PND, SWD and GKD into a single authority file, and should be used in preference
  case "25":
		c.Body = `GND`

  // Dunn and Bradstreet Universal Numbering System, see http://www.dnb.co.uk/dandb-duns-number
  case "26":
		c.Body = `DUNS`

  // Ringgold organizational identifier, see http://www.ringgold.com/identify.html
  case "27":
		c.Body = `Ringgold ID`

  // French Electre publisher identifier
  case "28":
		c.Body = `Identifiant Editeur Electre`

  // DOI used in EIDR party registry, for example ‘10.5237/C9F6-F41F’ (Sam Raimi). See http://eidr.org
  case "29":
		c.Body = `EIDR Party DOI`

  // French Electre imprint Identifier
  case "30":
		c.Body = `Identifiant Marque Electre`

  // Virtual Internet Authority File. <IDValue> should be a number. The URI form of the identifier can be created by prefixing the number with ‘https://viaf.org/viaf/’. See https://viaf.org
  case "31":
		c.Body = `VIAF ID`

  // DOI used in CrossRef’s Open Funder Registry list of academic research funding bodies, for example ‘10.13039/100004440’ (Wellcome Trust). See http://www.crossref.org/fundingdata/registry.html
  case "32":
		c.Body = `FundRef DOI`

  // Control number assigned to a Name Authority record by the Biblioteca Nacional de España
  case "33":
		c.Body = `BNE CN`

  // Numéro de la notice de personne BNF
  case "34":
		c.Body = `BNF Control Number`

  // Archival Resource Key, as a URL (including the address of the ARK resolver provided by eg a national library)
  case "35":
		c.Body = `ARK`

  // Nasjonalt autoritetsregister for navn – Norwegian national authority file for personal and corporate names. For use in ONIX 3.0 only
  case "36":
		c.Body = `Nasjonalt autoritetsregister`

  // Global Research Identifier Database ID (see https://www.grid.ac). For use in ONIX 3.0 only
  case "37":
		c.Body = `GRID`

  // Party ID from Identifiers and Standards for Higher Education and Research (fr: Identifiants et Référentiels pour l’enseignement supérieur et la recherche). For use on ONIX 3.0 only. See https://www.idref.fr
  case "38":
		c.Body = `IDRef`
	default:
		return fmt.Errorf("undefined code for NameIDType has been passed, got [%s]", v)
	}
	return nil
}

// NameType Person / organization name type
type NameType struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *NameType) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // Unspecified
  case "00":
		c.Body = `Unspecified`

  // May be used to give a well-known pseudonym, where the primary name is a ‘real’ name
  case "01":
		c.Body = `Pseudonym`

  // Authority-controlled name
  case "02":
		c.Body = `Authority-controlled name`

  // Use only within <AlternativeName>
  case "03":
		c.Body = `Earlier name`

  // May be used to identify a well-known real name, where the primary name is a pseudonym
  case "04":
		c.Body = `‘Real’ name`

  // Use only within <AlternativeName>, when the primary name type is unspecified
  case "05":
		c.Body = `Transliterated form of primary name`

  // Use only within <AlternativeName>
  case "06":
		c.Body = `Later name`

  // Use only within <NameAsSubject>, to indicate the subject is fictional. For use in ONIX 3.0 only
  case "07":
		c.Body = `Fictional character name`
	default:
		return fmt.Errorf("undefined code for NameType has been passed, got [%s]", v)
	}
	return nil
}

// NotificationType Notification or update type
type NotificationType struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *NotificationType) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // Use for a complete record issued earlier than approximately six months before publication
  case "01":
		c.Body = `Early notification`

  // Use for a complete record issued to confirm advance information approximately six months before publication; or for a complete record issued after that date and before information has been confirmed from the book-in-hand
  case "02":
		c.Body = `Advance notification (confirmed)`

  // Use for a complete record issued to confirm advance information at or just before actual publication date, usually from the book-in-hand, or for a complete record issued at any later date
  case "03":
		c.Body = `Notification confirmed on publication`

  // In ONIX 3.0 only, use when sending a ‘block update’ record. A block update implies using the supplied block(s) to update the existing record for the product, replacing only the blocks included in the block update, and leaving other blocks unchanged – for example, replacing old information from Blocks 4 and 6 with the newly-received data while retailing information from Blocks 1–3 and 5 untouched. In previous ONIX releases, and for ONIX 3.0 using other notification types, updating is by replacing the complete record with the newly-received data
  case "04":
		c.Body = `Update (partial)`

  // Use when sending an instruction to delete a record which was previously issued. Note that a Delete instruction should NOT be used when a product is cancelled, put out of print, or otherwise withdrawn from sale: this should be handled as a change of Publishing status, leaving the receiver to decide whether to retain or delete the record. A Delete instruction is used ONLY when there is a particular reason to withdraw a record completely, eg because it was issued in error
  case "05":
		c.Body = `Delete`

  // Notice of sale of a product, from one publisher to another: sent by the publisher disposing of the product
  case "08":
		c.Body = `Notice of sale`

  // Notice of acquisition of a product, by one publisher from another: sent by the acquiring publisher
  case "09":
		c.Body = `Notice of acquisition`

  // ONIX 3.0 only. Record may be processed for test purposes, but data should be discarded when testing is complete. Sender must ensure the <RecordReference> matches a previously-sent Test record
  case "88":
		c.Body = `Test update (Partial)`

  // Record may be processed for test purposes, but data should be discarded when testing is complete. Sender must ensure the <RecordReference> does not match any previously-sent live product record
  case "89":
		c.Body = `Test record`
	default:
		return fmt.Errorf("undefined code for NotificationType has been passed, got [%s]", v)
	}
	return nil
}

// OccurrenceDateRole Event occurrence date role
type OccurrenceDateRole struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *OccurrenceDateRole) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // Date and (with the default dateformat) time the event occurrence begins
  case "01":
		c.Body = `Date of occurrence`

  // Date and (with the default dateformat) time the event occurrence ends
  case "02":
		c.Body = `Date of occurrence end`
	default:
		return fmt.Errorf("undefined code for OccurrenceDateRole has been passed, got [%s]", v)
	}
	return nil
}

// PositionOnProduct Position on product
type PositionOnProduct struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *PositionOnProduct) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // Position unknown or unspecified
  case "00":
		c.Body = `Unknown / unspecified`

  // The back cover of a book
  case "01":
		c.Body = `Cover 4`

  // The inside back cover of a book
  case "02":
		c.Body = `Cover 3`

  // The inside front cover of a book
  case "03":
		c.Body = `Cover 2`

  // The front cover of a book
  case "04":
		c.Body = `Cover 1`

  // The spine of a book
  case "05":
		c.Body = `On spine`

  // Used only for boxed products
  case "06":
		c.Body = `On box`

  // Used only for products fitted with hanging tags
  case "07":
		c.Body = `On tag`

  // Not be used for books unless they are contained within outer packaging
  case "08":
		c.Body = `On bottom`

  // Not be used for books unless they are contained within outer packaging
  case "09":
		c.Body = `On back`

  // Used only for products packaged in outer sleeves
  case "10":
		c.Body = `On outer sleeve / back`

  // Used only for products packaged in shrink-wrap or other removable wrapping
  case "11":
		c.Body = `On removable wrapping`
	default:
		return fmt.Errorf("undefined code for PositionOnProduct has been passed, got [%s]", v)
	}
	return nil
}

// PriceCodeType Price code type
type PriceCodeType struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *PriceCodeType) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // A publisher or retailer’s proprietary code list as specified in <PriceCodeTypeName> which identifies particular codes with particular price points, price tiers or bands
  case "01":
		c.Body = `Proprietary`

  // Price Code scheme for Finnish Pocket Books (Pokkareiden hintaryhmä). Price codes expressed as letters A–J in <PriceCode>
  case "02":
		c.Body = `Finnish Pocket Book price code`

  // Price Code scheme for Finnish Miki Books (Miki-kirjojen hintaryhmä). Price codes expressed as an integer 1–n in <PriceCode>
  case "03":
		c.Body = `Finnish Miki Book price code`
	default:
		return fmt.Errorf("undefined code for PriceCodeType has been passed, got [%s]", v)
	}
	return nil
}

// PriceConditionQuantityType Price condition quantity type
type PriceConditionQuantityType struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *PriceConditionQuantityType) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // The price condition quantity represents a time period
  case "01":
		c.Body = `Time period`

  // The price condition quantity is a number of updates
  case "02":
		c.Body = `Number of updates`

  // Use with Price condition type 06 and a Quantity of units. Price is valid when purchased with a specific number of products from a list of product identifiers provided in the associated <ProductIdentifier> composites. Use for example when describing a price for this product which is valid if it is purchased along with any two from a list of other products
  case "03":
		c.Body = `Number of linked products`
	default:
		return fmt.Errorf("undefined code for PriceConditionQuantityType has been passed, got [%s]", v)
	}
	return nil
}

// PriceConditionType Price condition type
type PriceConditionType struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *PriceConditionType) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // Allows positive indication that there are no conditions (the default if <PriceCondition> is omitted)
  case "00":
		c.Body = `No conditions`

  // Purchase at this price includes specified updates
  case "01":
		c.Body = `Includes updates`

  // Purchase at this price requires commitment to purchase specified updates, not included in price
  case "02":
		c.Body = `Must also purchase updates`

  // Updates may be purchased separately, no minimum commitment required
  case "03":
		c.Body = `Updates available`

  // Purchase at this price requires commitment to purchase specified other product, not included in price
  case "04":
		c.Body = `Linked subsequent purchase price`

  // Purchase at this price requires prior purchase of other product
  case "05":
		c.Body = `Linked prior purchase price`

  // Purchase at this price requires simultaneous purchase of other product
  case "06":
		c.Body = `Linked price`

  // The rental or subscription will automatically renew at the end of the period unless actively cancelled
  case "07":
		c.Body = `Auto-renewing`

  // The duration of the rental to which the price applies. Deprecated, use <PriceConstraint> instead
  case "10":
		c.Body = `Rental duration`

  // Purchase at this price requires prior rental of the product. <PriceConditionQuantity> gives minimum prior rental period, and <ProductIdentifier> may be used if rental uses a different product identifier
  case "11":
		c.Body = `Rental to purchase`

  // Upgrade to longer rental duration. <PriceConditionQuantity> gives minimum prior rental duration, and <ProductIdentifier> may be used if rental uses a different product identifier. Separate price constraint with time limited license duration (code 07) specifies the new combined rental duration
  case "12":
		c.Body = `Rental extension`
	default:
		return fmt.Errorf("undefined code for PriceConditionType has been passed, got [%s]", v)
	}
	return nil
}

// PriceConstraintStatus Usage status
type PriceConstraintStatus struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *PriceConstraintStatus) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // Permitted unlimited
  case "01":
		c.Body = `Permitted unlimited`

  // Limit should be specified in <EpubUsageLimit> or <PriceConstraintLimit>
  case "02":
		c.Body = `Permitted subject to limit`

  // Prohibited
  case "03":
		c.Body = `Prohibited`
	default:
		return fmt.Errorf("undefined code for PriceConstraintStatus has been passed, got [%s]", v)
	}
	return nil
}

// PriceConstraintType Price constraint type
type PriceConstraintType struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *PriceConstraintType) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // Allows positive indication that there are no additional constraints (other than those specified in <EpubUsageConstraint>) – the default if <PriceConstraint> is omitted
  case "00":
		c.Body = `No constraints`

  // Lendable by the purchaser to other device owner, account holder or patron, eg library lending (use where the library product is not identified with a separate <ProductIdentifier> from the consumer product). The ‘primary’ copy becomes unusable while the secondary copy is on loan, unless a number of concurrent borrowers is also specified
  case "06":
		c.Body = `Lend`

  // E-publication license is time-limited. Use with code 02 from List 146 and a time period in days, weeks or months in <PriceConstraintLimit>. The purchased copy becomes unusable when the license expires
  case "07":
		c.Body = `Time-limited license`

  // Maximum number of consecutive loans or loan extensions (eg from a library) to a single device owner, account holder or patron. Note that a limit of 1 indicates that a loan cannot be renewed or extended
  case "08":
		c.Body = `Loan renewal`

  // E-publication license is multi-user. Maximum number of concurrent users licensed to use the product should be given in <PriceConstraintLimit>
  case "09":
		c.Body = `Multi-user license`

  // Preview locally before purchase. Allows a retail customer, account holder or patron to view a proportion of the book (or the whole book, if no proportion is specified) before purchase, but ONLY while located physically in the retailer’s store (eg while logged on to the store wifi). Also applies to borrowers making use of ‘acquisition on demand’ models in libraries
  case "10":
		c.Body = `Preview on premises`
	default:
		return fmt.Errorf("undefined code for PriceConstraintType has been passed, got [%s]", v)
	}
	return nil
}

// PriceConstraintUnit Unit of usage
type PriceConstraintUnit struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *PriceConstraintUnit) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // Maximum number of copies that may be made of a permitted extract
  case "01":
		c.Body = `Copies`

  // Maximum number of characters in a permitted extract for a specified usage
  case "02":
		c.Body = `Characters`

  // Maximum number of words in a permitted extract for a specified usage
  case "03":
		c.Body = `Words`

  // Maximum number of pages in a permitted extract for a specified usage
  case "04":
		c.Body = `Pages`

  // Maximum percentage of total content in a permitted extract for a specified usage
  case "05":
		c.Body = `Percentage`

  // Maximum number of devices in ‘share group’
  case "06":
		c.Body = `Devices`

  // Maximum number of concurrent users. NB where the number of concurrent users is specifically not limited, set the number of concurrent users to zero
  case "07":
		c.Body = `Concurrent users`

  // Maximum number of licensed individual users, independent of concurrency of use
  case "15":
		c.Body = `Users`

  // A ‘class’ is a group of learners attending a specific course or lesson and generally taught as a group
  case "19":
		c.Body = `Concurrent classes`

  // Maximum number of classes of learners, independent of concurrency of use
  case "20":
		c.Body = `Classes`

  // Maximum percentage of total content which may be used in a specified usage per time period; the time period being specified as another <EpubUsageLimit> Quantity
  case "08":
		c.Body = `Percentage per time period`

  // Maximum time period in days (beginning from product purchase or activation)
  case "09":
		c.Body = `Days`

  // Maximum time period in weeks
  case "13":
		c.Body = `Weeks`

  // Maximum time period in months
  case "14":
		c.Body = `Months`

  // Maximum amount of time in hours, minutes and seconds allowed in a permitted extract for a specified usage, in the format HHHMMSS (7 digits, with leading zeros if necessary)
  case "16":
		c.Body = `Hours minutes and seconds`

  // Maximum time period in days (beginning from the product publication date). In effect, this defines a fixed end date for the license independent of the purchase or activation date
  case "27":
		c.Body = `Days (fixed start)`

  // Maximum time period in weeks
  case "28":
		c.Body = `Weeks (fixed start)`

  // Maximum time period in months
  case "29":
		c.Body = `Months (fixed start)`

  // Maximum number of times a specified usage event may occur (in the lifetime of the product)
  case "10":
		c.Body = `Times`

  // Maximum frequency a specified usage event may occur (per day)
  case "22":
		c.Body = `Times per day`

  // Maximum frequency a specified usage event may occur (per month)
  case "23":
		c.Body = `Times per month`

  // Maximum frequency a specified usage event may occur (per year)
  case "24":
		c.Body = `Times per year`

  // Maximum resolution of printed or copy/pasted extracts
  case "21":
		c.Body = `Dots per inch`

  // Maximum resolution of printed or copy/pasted extracts
  case "26":
		c.Body = `Dots per cm`

  // Page number where allowed usage begins. <Quantity> should contain an absolute page number, counting the cover as page 1. (This type of page numbering should not be used where the e-publication has no fixed pagination). Use with (max number of) Pages, Percentage of content, or End page to specify pages allowed in Preview
  case "11":
		c.Body = `Allowed usage start page`

  // Page number at which allowed usage ends. <Quantity> should contain an absolute page number, counting the cover as page 1. (This type of page numbering should not be used where the e-publication has no fixed pagination). Use with Start page to specify pages allowed in a preview
  case "12":
		c.Body = `Allowed usage end page`

  // Time at which allowed usage begins. <Quantity> should contain an absolute time, counting from the beginning of an audio or video product, in the format HHHMMSS or HHHMMSScc. Use with Time, Percentage of content, or End time to specify time-based extract allowed in Preview
  case "17":
		c.Body = `Allowed usage start time`

  // Time at which allowed usage ends. <Quantity> should contain an absolute time, counting from the beginning of an audio or video product, in the format HHHMMSS or HHHMMSScc. Use with Start time to specify time-based extract allowed in Preview
  case "18":
		c.Body = `Allowed usage end time`

  // The date from which the usage constraint applies. <Quantity> is in the format YYYYMMDD
  case "98":
		c.Body = `Valid from`

  // The date until which the usage constraint applies. <Quantity> is in the format YYYYMMDD
  case "99":
		c.Body = `Valid to`
	default:
		return fmt.Errorf("undefined code for PriceConstraintUnit has been passed, got [%s]", v)
	}
	return nil
}

// PriceDateRole Price date role
type PriceDateRole struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *PriceDateRole) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // Date on which a price becomes effective
  case "14":
		c.Body = `From date`

  // Date on which a price ceases to be effective
  case "15":
		c.Body = `Until date`

  // Combines From date and Until date to define a period (both dates are inclusive). Use with for example dateformat 06
  case "24":
		c.Body = `From… until date`
	default:
		return fmt.Errorf("undefined code for PriceDateRole has been passed, got [%s]", v)
	}
	return nil
}

// PriceIDType Price identifier type
type PriceIDType struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *PriceIDType) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // Note that <IDTypeName> is required for proprietary identifiers
  case "01":
		c.Body = `Proprietary`

  // Proprietary identifier uniquely identifies price amount and currency. Two unrelated products with the same price amount carry the same identifier, though their price types may be different
  case "02":
		c.Body = `Proprietary price point identifier`

  // Proprietary identifier uniquely identifies price type, qualifier and any constraints and conditions. Two unrelated products with the same price type carry the same identifier, though their price points may be different
  case "03":
		c.Body = `Proprietary price type identifier`

  // Proprietary identifier identifies a unique combination of price point and type, though two unrelated products may carry the same identifier if all details of their prices are identical
  case "04":
		c.Body = `Proprietary price point and type identifier`

  // Proprietary identifier is unique to a single price point, price type and product. No two products can carry the same identifier, even if all details of their prices are identical
  case "05":
		c.Body = `Proprietary unique price identifier`

  // Proprietary identifier uniquely identifies a specific combination of product, price amount and currency, independent of the price type
  case "06":
		c.Body = `Proprietary product price point identifier`

  // Proprietary identifier uniquely identifies a specific combination of product, price type, qualifier and any constraints and conditions, independent of the price amount and currency. A product with the same product price type identififer may carry differing price amounts, currencies at different points in time
  case "07":
		c.Body = `Proprietary product price type identifier`
	default:
		return fmt.Errorf("undefined code for PriceIDType has been passed, got [%s]", v)
	}
	return nil
}

// PricePer Unit of pricing
type PricePer struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *PricePer) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // Default
  case "00":
		c.Body = `Per copy of whole product`

  // Per page for printed loose-leaf content only
  case "01":
		c.Body = `Per page for printed loose-leaf content only`
	default:
		return fmt.Errorf("undefined code for PricePer has been passed, got [%s]", v)
	}
	return nil
}

// PriceQualifier Price type qualifier
type PriceQualifier struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *PriceQualifier) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // Price applies to all customers that do not fall within any other group with a specified group-specific qualified price
  case "00":
		c.Body = `Unqualified price`

  // Price applies to a designated group membership
  case "01":
		c.Body = `Member/subscriber price`

  // Price applies to sales outside the territory in which the supplier is located
  case "02":
		c.Body = `Export price`

  // Use in cases where there is no combined price, but a lower price is offered for each part if the whole set / series / collection is purchased (either at one time, as part of a continuing commitment, or in a single purchase)
  case "03":
		c.Body = `Reduced price applicable when the item is purchased as part of a set (or series, or collection)`

  // In the Netherlands (or any other market where similar arrangements exist): a reduced fixed price available for a limited time on presentation of a voucher or coupon published in a specified medium, eg a newspaper. Should be accompanied by Price Type code 13 and additional detail in <PriceTypeDescription>, and by validity dates in <PriceEffectiveFrom> and <PriceEffectiveUntil> (ONIX 2.1) or in the <PriceDate> composite (ONIX 3.0)
  case "04":
		c.Body = `Voucher price`

  // Price for individual consumer sale only
  case "05":
		c.Body = `Consumer price`

  // Price for sale to libraries or other corporate or institutional customers
  case "06":
		c.Body = `Corporate / Library / Education price`

  // Price valid for a specified period prior to publication. Orders placed prior to the end of the period are guaranteed to be delivered to the retailer before the nominal publication date. The price may or may not be different from the ‘normal’ price, which carries no such delivery guarantee. Must be accompanied by a <PriceEffectiveUntil> date (or equivalent <PriceDate> composite in ONIX 3), and should also be accompanied by a ‘normal’ price
  case "07":
		c.Body = `Reservation order price`

  // Temporary ‘Special offer’ price. Must be accompanied by <PriceEffectiveFrom> and <PriceEffectiveUntil> dates (or equivalent <PriceDate> composites in ONIX 3), and may also be accompanied by a ‘normal’ price
  case "08":
		c.Body = `Promotional offer price`

  // Price requires purchase with, or proof of ownership of another product. Further details of purchase or ownership requirements must be given in <PriceTypeDescription>
  case "09":
		c.Body = `Linked price`

  // Price for sale only to libraries (including public, school and academic libraries)
  case "10":
		c.Body = `Library price`

  // Price for sale only to educational institutions (including school and academic libraries), educational buying consortia, government and local government bodies purchasing for use in education
  case "11":
		c.Body = `Education price`

  // Price for sale to corporate customers only
  case "12":
		c.Body = `Corporate price`

  // Price for sale to organisations or services offering consumers subscription access to a library of books
  case "13":
		c.Body = `Subscription service price`

  // Price for primary and secondary education
  case "14":
		c.Body = `School library price`

  // Price for higher education and scholarly institutions
  case "15":
		c.Body = `Academic library price`

  // Public library price
  case "16":
		c.Body = `Public library price`

  // Initial ‘Introductory offer’ price. Must be accompanied by an Effective until date in a <PriceDate> composite in ONIX 3, and may also be accompanied by a ‘normal’ price valid after the introductory offer expires (Fr. Prix de lancement). Only valid in ONIX 3.0
  case "17":
		c.Body = `Introductory price`

  // Price for library consortia. Only valid in ONIX 3.0
  case "18":
		c.Body = `Consortial price`
	default:
		return fmt.Errorf("undefined code for PriceQualifier has been passed, got [%s]", v)
	}
	return nil
}

// PriceStatus Price status
type PriceStatus struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *PriceStatus) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // Default
  case "00":
		c.Body = `Unspecified`

  // Provisional
  case "01":
		c.Body = `Provisional`

  // Firm
  case "02":
		c.Body = `Firm`
	default:
		return fmt.Errorf("undefined code for PriceStatus has been passed, got [%s]", v)
	}
	return nil
}

// PriceType Price type
type PriceType struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *PriceType) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // Recommended Retail Price, excluding any sales tax or value-added tax. Price recommended by the publisher or supplier for retail sales to the consumer. Also termed the Suggested Retail Price (SRP) or Maximum Suggested Retail Price (MSRP) in some countries. The retailer may choose to use this recommended price, or may choose to sell to the consumer at a lower (or occasionally, a higher) price which is termed the Actual Selling Price (ASP) in sales reports. The net price charged to the retailer depends on the RRP minus a trade discount (which may be customer-specific). Relevant tax detail must be calculated by the data recipient
  case "01":
		c.Body = `RRP excluding tax`

  // Recommended Retail Price, including sales or value-added tax where applicable. The net price charged to the retailer depends on the trade discount. Sales or value-added tax detail is usually supplied in the <Tax> composite
  case "02":
		c.Body = `RRP including tax`

  // Fixed Retail Price, excluding any sales or value-added tax, used in countries were retail price maintenance applies by law to certain products. Price fixed by the publisher or supplier for retail sales to the consumer. The retailer must use this price, or may vary the price only within certain legally-prescribed limits. The net price charged to the retailer depends on the FRP minus a customer-soecific trade discount. Relevant tax detail must be calculated by the data recipient
  case "03":
		c.Body = `FRP excluding tax`

  // Fixed Retail Price, including any sales or value-added tax where applicable, used in countries were retail price maintenance applies by law to certain products. The net price charged to the retailer depends on the trade discount. Sales or value-added tax detail is usually supplied in the <Tax> composite
  case "04":
		c.Body = `FRP including tax`

  // Net or wholesale price, excluding any sales or value-added tax. Unit price charged by supplier for business-to-business transactions, without any direct relationship to the price for retail sales to the consumer, but sometimes subject to a further customer-specific trade discount based on volume. Relevant tax detail must be calculated by the data recipient
  case "05":
		c.Body = `Supplier’s Net price excluding tax`

  // Unit price charged by supplier to reseller / rental outlet, excluding any sales tax or value-added tax: goods for rental (used for video and DVD)
  case "06":
		c.Body = `Supplier’s Net price excluding tax: rental goods`

  // Net or wholesale price, including any sales or value-added tax where applicable. Unit price charged by supplier for business-to-business transactions, without any direct relationship to the price for retail sales to the consumer, but sometimes subject to a further customer-specific trade discount based on volume. Sales or value-added tax detail is usually supplied in the <Tax> composite
  case "07":
		c.Body = `Supplier’s Net price including tax`

  // Net or wholesale price charged by supplier to a specified class of reseller, excluding any sales tax or value-added tax. Relevant tax detail must be calculated by the data recipient. (This value is for use only in countries, eg Finland, where trade practice requires two different Net prices to be listed for different classes of resellers, and where national guidelines specify how the code should be used)
  case "08":
		c.Body = `Supplier’s alternative Net price excluding tax`

  // Net or wholesale price charged by supplier to a specified class of reseller, including any sales tax or value-added tax. Sales or value-added tax detail is usually supplied in the <Tax> composite. (This value is for use only in countries, eg Finland, where trade practice requires two different Net prices to be listed for different classes of resellers, and where national guidelines specify how the code should be used)
  case "09":
		c.Body = `Supplier’s alternative net price including tax`

  // Special sale RRP excluding any sales tax or value-added tax. Note ‘special sales’ are sales where terms and conditions are different from normal trade sales, when for example products that are normally sold on a sale-or-return basis are sold on firm-sale terms, where a particular product is tailored for a specific retail outlet (often termed a ‘premium’ product), or where other specific conditions or qualiifications apply. Further details of the modified terms and conditions should be given in <PriceTypeDescription>
  case "11":
		c.Body = `Special sale RRP excluding tax`

  // Special sale RRP including sales or value-added tax if applicable
  case "12":
		c.Body = `Special sale RRP including tax`

  // In countries where retail price maintenance applies by law to certain products: not used in USA
  case "13":
		c.Body = `Special sale fixed retail price excluding tax`

  // In countries where retail price maintenance applies by law to certain products: not used in USA
  case "14":
		c.Body = `Special sale fixed retail price including tax`

  // Unit price charged by supplier to reseller for special sale excluding any sales tax or value-added tax
  case "15":
		c.Body = `Supplier’s net price for special sale excluding tax`

  // Unit price charged by supplier to reseller for special sale including any sales tax or value-added tax
  case "17":
		c.Body = `Supplier’s net price for special sale including tax`

  // Pre-publication RRP excluding any sales tax or value-added tax. Use where RRP for pre-orders is different from post-publication RRP
  case "21":
		c.Body = `Pre-publication RRP excluding tax`

  // Pre-publication RRP including sales or value-added tax if applicable. Use where RRP for pre-orders is different from post-publication RRP
  case "22":
		c.Body = `Pre-publication RRP including tax`

  // In countries where retail price maintenance applies by law to certain products: not used in USA
  case "23":
		c.Body = `Pre-publication fixed retail price excluding tax`

  // In countries where retail price maintenance applies by law to certain products: not used in USA
  case "24":
		c.Body = `Pre-publication fixed retail price including tax`

  // Unit price charged by supplier to reseller pre-publication excluding any sales tax or value-added tax
  case "25":
		c.Body = `Supplier’s pre-publication net price excluding tax`

  // Unit price charged by supplier to reseller pre-publication including any sales tax or value-added tax
  case "27":
		c.Body = `Supplier’s pre-publication net price including tax`

  // In the US, books are sometimes supplied on ‘freight-pass-through’ terms, where a price that is different from the RRP is used as the basis for calculating the supplier’s charge to a reseller. To make it clear when such terms are being invoked, code 31 is used instead of code 01 to indicate the RRP. Code 32 is used for the ‘billing price’
  case "31":
		c.Body = `Freight-pass-through RRP excluding tax`

  // When freight-pass-through terms apply, the price on which the supplier’s charge to a reseller is calculated, ie the price to which trade discount terms are applied. See also code 31
  case "32":
		c.Body = `Freight-pass-through billing price excluding tax`

  // In countries where retail price maintenance applies by law to certain products, but the price is set by the importer or local sales agent, not the foreign publisher. In France, ‘prix catalogue éditeur étranger’
  case "33":
		c.Body = `Importer’s Fixed retail price excluding tax`

  // In countries where retail price maintenance applies by law to certain products, but the price is set by the importer or local sales agent, not the foreign publisher. In France, ‘prix catalogue éditeur étranger’
  case "34":
		c.Body = `Importer’s Fixed retail price including tax`

  // For a product supplied on agency terms, the retail price set by the publisher, excluding any sales tax or value-added tax
  case "41":
		c.Body = `Publishers retail price excluding tax`

  // For a product supplied on agency terms, the retail price set by the publisher, including sales or value-added tax if applicable
  case "42":
		c.Body = `Publishers retail price including tax`
	default:
		return fmt.Errorf("undefined code for PriceType has been passed, got [%s]", v)
	}
	return nil
}

// PrimaryContentType Product content type
type PrimaryContentType struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *PrimaryContentType) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // Readable text of the main work: this value is required, together with applicable <ProductForm> and <ProductFormDetail> values, to designate an e-book or other digital or physical product whose primary content is eye-readable text
  case "10":
		c.Body = `Text (eye-readable)`

  // E-publication contains a significant number of actionable cross-references, hyperlinked notes and annotations, or with other actionable links between largely textual elements (eg quiz/test questions, ‘choose your own ending’ etc)
  case "15":
		c.Body = `Extensive links between internal content`

  // E-publication contains a significant number of actionable (clickable) web links
  case "14":
		c.Body = `Extensive links to external content`

  // Publication contains additional textual content such as interview, feature article, essay, bibliography, quiz/test, other background material or text that is not included in a primary or ‘unenhanced’ version
  case "16":
		c.Body = `Additional eye-readable text not part of main work`

  // Publication contains a significant number of web links (printed URLs, QR codes etc). For use in ONIX 3.0 only
  case "41":
		c.Body = `Additional eye-readable links to external content`

  // eg Teaser chapter
  case "17":
		c.Body = `Promotional text for other book product`

  // Musical notation
  case "11":
		c.Body = `Musical notation`

  // Use only when no more detailed specification is provided
  case "07":
		c.Body = `Still images / graphics`

  // Whether in a plate section / insert, or not
  case "18":
		c.Body = `Photographs`

  // Including other ‘mechanical’ (ie non-photographic) illustrations
  case "19":
		c.Body = `Figures, diagrams, charts, graphs`

  // Publication is enhanced with additional images or graphical content such as supplementary photographs that are not included in a primary or ‘unenhanced’ version
  case "20":
		c.Body = `Additional images / graphics not part of main work`

  // Maps and/or other cartographic content
  case "12":
		c.Body = `Maps and/or other cartographic content`

  // eg Questions or student exercises, problems, quizzes or tests (as an integral part of the work). For use in ONIX 3.0 only
  case "42":
		c.Body = `Assessment material`

  // Audio recording of a reading of a book or other text
  case "01":
		c.Body = `Audiobook`

  // Audio recording of a drama or other spoken word performance
  case "02":
		c.Body = `Performance – spoken word`

  // eg an interview, speech, lecture or discussion, not a ‘reading’ or ‘performance’)
  case "13":
		c.Body = `Other speech content`

  // Audio recording of a music performance, including musical drama and opera
  case "03":
		c.Body = `Music recording`

  // Audio recording of other sound, eg birdsong
  case "04":
		c.Body = `Other audio`

  // Audio recording of a reading, performance or dramatization of part of the work
  case "21":
		c.Body = `Partial performance – spoken word`

  // Product is enhanced with audio recording of full or partial reading, performance, dramatization, interview, background documentary or other audio content not included in the primary or ‘unenhanced’ version
  case "22":
		c.Body = `Additional audio content not part of main work`

  // eg Reading of teaser chapter
  case "23":
		c.Body = `Promotional audio for other book product`

  // Includes Film, video, animation etc. Use only when no more detailed specification is provided. Formerly ‘Moving images’
  case "06":
		c.Body = `Video`

  // Video recording of a reading
  case "26":
		c.Body = `Video recording of a reading`

  // Video recording of a drama or other performance, including musical performance
  case "27":
		c.Body = `Performance – visual`

  // eg animated diagrams, charts, graphs or other illustrations
  case "24":
		c.Body = `Animated / interactive illustrations`

  // eg cartoon, animatic or CGI animation
  case "25":
		c.Body = `Narrative animation`

  // Other video content eg interview, not a reading or performance
  case "28":
		c.Body = `Other video`

  // Video recording of a reading, performance or dramatization of part of the work
  case "29":
		c.Body = `Partial performance – video`

  // E-publication is enhanced with video recording of full or partial reading, performance, dramatization, interview, background documentary or other content not included in the primary or ‘unenhanced’ version
  case "30":
		c.Body = `Additional video content not part of main work`

  // eg Book trailer
  case "31":
		c.Body = `Promotional video for other book product`

  // No multi-user functionality. Formerly just ‘Game’
  case "05":
		c.Body = `Game / Puzzle`

  // Includes some degree of multi-user functionality
  case "32":
		c.Body = `Contest`

  // Largely ‘content free’
  case "08":
		c.Body = `Software`

  // Data files
  case "09":
		c.Body = `Data`

  // Data set plus software
  case "33":
		c.Body = `Data set plus software`

  // Entire pages or blank spaces, forms, boxes etc, intended to be filled in by the reader
  case "34":
		c.Body = `Blank pages or spaces`

  // Use only where type of advertising content is not stated
  case "35":
		c.Body = `Advertising content`

  // ‘Back ads’ – promotional pages for other books (that do not include sample content, cf codes 17, 23)
  case "37":
		c.Body = `Advertising – first party`

  // Eg to obtain discounts on other products
  case "36":
		c.Body = `Advertising – coupons`

  // Advertising – third party display
  case "38":
		c.Body = `Advertising – third party display`

  // Advertising – third party textual
  case "39":
		c.Body = `Advertising – third party textual`

  // E-publication contains microprograms written (eg) in Javascript and executed within the reading system. For use in ONIX 3.0 only
  case "40":
		c.Body = `Scripting`

  // E-publication contains pop-ups or other functionality offering (eg) term definitions, cross-links or glossary entries [Note this should not include (eg) dictionary funcionality that is part of the reading system.] For use in ONIX 3.0 only
  case "43":
		c.Body = `Scripted pop-ups`
	default:
		return fmt.Errorf("undefined code for PrimaryContentType has been passed, got [%s]", v)
	}
	return nil
}

// PrintedOnProduct Printed on product
type PrintedOnProduct struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *PrintedOnProduct) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // Price not printed on product
  case "01":
		c.Body = `No`

  // Price printed on product
  case "02":
		c.Body = `Yes`
	default:
		return fmt.Errorf("undefined code for PrintedOnProduct has been passed, got [%s]", v)
	}
	return nil
}

// PrizeCode Prize or award achievement
type PrizeCode struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *PrizeCode) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // Winner
  case "01":
		c.Body = `Winner`

  // Named as being in second place
  case "02":
		c.Body = `Runner-up`

  // Cited as being worthy of special attention at the final stage of the judging process, but not named specifically as winner or runner-up. Possible terminology used by a particular prize includes ‘specially commended’ or ‘honored’
  case "03":
		c.Body = `Commended`

  // Title named by the judging process to be one of the final list of candidates, such as a ‘short-list’ from which the winner is selected, or a title named as ‘finalist’
  case "04":
		c.Body = `Short-listed`

  // Title named by the judging process to be one of the preliminary list of candidates, such as a ‘long-list’ from which first a shorter list or set of finalists is selected, and then the winner is announced
  case "05":
		c.Body = `Long-listed`

  // Or co-winner
  case "06":
		c.Body = `Joint winner`

  // Selected by judging panel or an official nominating process for final consideration for a prize, award or honour for which no ‘short-list’ or ‘long list’ exists
  case "07":
		c.Body = `Nominated`
	default:
		return fmt.Errorf("undefined code for PrizeCode has been passed, got [%s]", v)
	}
	return nil
}

// PrizeCountry Country – based on ISO 3166-1
type PrizeCountry struct {
	Body []string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *PrizeCountry) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	codes := strings.Split(v, " ")
	tmpeCodes := []string{}
	for _, code := range codes {
		switch code {

		// Andorra
		case "AD":
			tmpeCodes = append(tmpeCodes, `Andorra`)

		// United Arab Emirates
		case "AE":
			tmpeCodes = append(tmpeCodes, `United Arab Emirates`)

		// Afghanistan
		case "AF":
			tmpeCodes = append(tmpeCodes, `Afghanistan`)

		// Antigua and Barbuda
		case "AG":
			tmpeCodes = append(tmpeCodes, `Antigua and Barbuda`)

		// Anguilla
		case "AI":
			tmpeCodes = append(tmpeCodes, `Anguilla`)

		// Albania
		case "AL":
			tmpeCodes = append(tmpeCodes, `Albania`)

		// Armenia
		case "AM":
			tmpeCodes = append(tmpeCodes, `Armenia`)

		// Deprecated – use BQ, CW and SX as appropriate
		case "AN":
			tmpeCodes = append(tmpeCodes, `Netherlands Antilles`)

		// Angola
		case "AO":
			tmpeCodes = append(tmpeCodes, `Angola`)

		// Antarctica
		case "AQ":
			tmpeCodes = append(tmpeCodes, `Antarctica`)

		// Argentina
		case "AR":
			tmpeCodes = append(tmpeCodes, `Argentina`)

		// American Samoa
		case "AS":
			tmpeCodes = append(tmpeCodes, `American Samoa`)

		// Austria
		case "AT":
			tmpeCodes = append(tmpeCodes, `Austria`)

		// Australia
		case "AU":
			tmpeCodes = append(tmpeCodes, `Australia`)

		// Aruba
		case "AW":
			tmpeCodes = append(tmpeCodes, `Aruba`)

		// Åland Islands
		case "AX":
			tmpeCodes = append(tmpeCodes, `Åland Islands`)

		// Azerbaijan
		case "AZ":
			tmpeCodes = append(tmpeCodes, `Azerbaijan`)

		// Bosnia and Herzegovina
		case "BA":
			tmpeCodes = append(tmpeCodes, `Bosnia and Herzegovina`)

		// Barbados
		case "BB":
			tmpeCodes = append(tmpeCodes, `Barbados`)

		// Bangladesh
		case "BD":
			tmpeCodes = append(tmpeCodes, `Bangladesh`)

		// Belgium
		case "BE":
			tmpeCodes = append(tmpeCodes, `Belgium`)

		// Burkina Faso
		case "BF":
			tmpeCodes = append(tmpeCodes, `Burkina Faso`)

		// Bulgaria
		case "BG":
			tmpeCodes = append(tmpeCodes, `Bulgaria`)

		// Bahrain
		case "BH":
			tmpeCodes = append(tmpeCodes, `Bahrain`)

		// Burundi
		case "BI":
			tmpeCodes = append(tmpeCodes, `Burundi`)

		// Benin
		case "BJ":
			tmpeCodes = append(tmpeCodes, `Benin`)

		// Saint Barthélemy
		case "BL":
			tmpeCodes = append(tmpeCodes, `Saint Barthélemy`)

		// Bermuda
		case "BM":
			tmpeCodes = append(tmpeCodes, `Bermuda`)

		// Brunei Darussalam
		case "BN":
			tmpeCodes = append(tmpeCodes, `Brunei Darussalam`)

		// Bolivia, Plurinational State of
		case "BO":
			tmpeCodes = append(tmpeCodes, `Bolivia, Plurinational State of`)

		// Bonaire, Sint Eustatius and Saba
		case "BQ":
			tmpeCodes = append(tmpeCodes, `Bonaire, Sint Eustatius and Saba`)

		// Brazil
		case "BR":
			tmpeCodes = append(tmpeCodes, `Brazil`)

		// Bahamas
		case "BS":
			tmpeCodes = append(tmpeCodes, `Bahamas`)

		// Bhutan
		case "BT":
			tmpeCodes = append(tmpeCodes, `Bhutan`)

		// Bouvet Island
		case "BV":
			tmpeCodes = append(tmpeCodes, `Bouvet Island`)

		// Botswana
		case "BW":
			tmpeCodes = append(tmpeCodes, `Botswana`)

		// Belarus
		case "BY":
			tmpeCodes = append(tmpeCodes, `Belarus`)

		// Belize
		case "BZ":
			tmpeCodes = append(tmpeCodes, `Belize`)

		// Canada
		case "CA":
			tmpeCodes = append(tmpeCodes, `Canada`)

		// Cocos (Keeling) Islands
		case "CC":
			tmpeCodes = append(tmpeCodes, `Cocos (Keeling) Islands`)

		// Congo, Democratic Republic of the
		case "CD":
			tmpeCodes = append(tmpeCodes, `Congo, Democratic Republic of the`)

		// Central African Republic
		case "CF":
			tmpeCodes = append(tmpeCodes, `Central African Republic`)

		// Congo
		case "CG":
			tmpeCodes = append(tmpeCodes, `Congo`)

		// Switzerland
		case "CH":
			tmpeCodes = append(tmpeCodes, `Switzerland`)

		// Cote d’Ivoire
		case "CI":
			tmpeCodes = append(tmpeCodes, `Cote d’Ivoire`)

		// Cook Islands
		case "CK":
			tmpeCodes = append(tmpeCodes, `Cook Islands`)

		// Chile
		case "CL":
			tmpeCodes = append(tmpeCodes, `Chile`)

		// Cameroon
		case "CM":
			tmpeCodes = append(tmpeCodes, `Cameroon`)

		// China
		case "CN":
			tmpeCodes = append(tmpeCodes, `China`)

		// Colombia
		case "CO":
			tmpeCodes = append(tmpeCodes, `Colombia`)

		// Costa Rica
		case "CR":
			tmpeCodes = append(tmpeCodes, `Costa Rica`)

		// DEPRECATED, replaced by ME – Montenegro and RS – Serbia
		case "CS":
			tmpeCodes = append(tmpeCodes, `Serbia and Montenegro`)

		// Cuba
		case "CU":
			tmpeCodes = append(tmpeCodes, `Cuba`)

		// Cabo Verde
		case "CV":
			tmpeCodes = append(tmpeCodes, `Cabo Verde`)

		// Curaçao
		case "CW":
			tmpeCodes = append(tmpeCodes, `Curaçao`)

		// Christmas Island
		case "CX":
			tmpeCodes = append(tmpeCodes, `Christmas Island`)

		// Cyprus
		case "CY":
			tmpeCodes = append(tmpeCodes, `Cyprus`)

		// Formerly Czech Republic
		case "CZ":
			tmpeCodes = append(tmpeCodes, `Czechia`)

		// Germany
		case "DE":
			tmpeCodes = append(tmpeCodes, `Germany`)

		// Djibouti
		case "DJ":
			tmpeCodes = append(tmpeCodes, `Djibouti`)

		// Denmark
		case "DK":
			tmpeCodes = append(tmpeCodes, `Denmark`)

		// Dominica
		case "DM":
			tmpeCodes = append(tmpeCodes, `Dominica`)

		// Dominican Republic
		case "DO":
			tmpeCodes = append(tmpeCodes, `Dominican Republic`)

		// Algeria
		case "DZ":
			tmpeCodes = append(tmpeCodes, `Algeria`)

		// Ecuador
		case "EC":
			tmpeCodes = append(tmpeCodes, `Ecuador`)

		// Estonia
		case "EE":
			tmpeCodes = append(tmpeCodes, `Estonia`)

		// Egypt
		case "EG":
			tmpeCodes = append(tmpeCodes, `Egypt`)

		// Western Sahara
		case "EH":
			tmpeCodes = append(tmpeCodes, `Western Sahara`)

		// Eritrea
		case "ER":
			tmpeCodes = append(tmpeCodes, `Eritrea`)

		// Spain
		case "ES":
			tmpeCodes = append(tmpeCodes, `Spain`)

		// Ethiopia
		case "ET":
			tmpeCodes = append(tmpeCodes, `Ethiopia`)

		// Finland
		case "FI":
			tmpeCodes = append(tmpeCodes, `Finland`)

		// Fiji
		case "FJ":
			tmpeCodes = append(tmpeCodes, `Fiji`)

		// Falkland Islands (Malvinas)
		case "FK":
			tmpeCodes = append(tmpeCodes, `Falkland Islands (Malvinas)`)

		// Micronesia, Federated States of
		case "FM":
			tmpeCodes = append(tmpeCodes, `Micronesia, Federated States of`)

		// Faroe Islands
		case "FO":
			tmpeCodes = append(tmpeCodes, `Faroe Islands`)

		// France
		case "FR":
			tmpeCodes = append(tmpeCodes, `France`)

		// Gabon
		case "GA":
			tmpeCodes = append(tmpeCodes, `Gabon`)

		// United Kingdom
		case "GB":
			tmpeCodes = append(tmpeCodes, `United Kingdom`)

		// Grenada
		case "GD":
			tmpeCodes = append(tmpeCodes, `Grenada`)

		// Georgia
		case "GE":
			tmpeCodes = append(tmpeCodes, `Georgia`)

		// French Guiana
		case "GF":
			tmpeCodes = append(tmpeCodes, `French Guiana`)

		// Guernsey
		case "GG":
			tmpeCodes = append(tmpeCodes, `Guernsey`)

		// Ghana
		case "GH":
			tmpeCodes = append(tmpeCodes, `Ghana`)

		// Gibraltar
		case "GI":
			tmpeCodes = append(tmpeCodes, `Gibraltar`)

		// Greenland
		case "GL":
			tmpeCodes = append(tmpeCodes, `Greenland`)

		// Gambia
		case "GM":
			tmpeCodes = append(tmpeCodes, `Gambia`)

		// Guinea
		case "GN":
			tmpeCodes = append(tmpeCodes, `Guinea`)

		// Guadeloupe
		case "GP":
			tmpeCodes = append(tmpeCodes, `Guadeloupe`)

		// Equatorial Guinea
		case "GQ":
			tmpeCodes = append(tmpeCodes, `Equatorial Guinea`)

		// Greece
		case "GR":
			tmpeCodes = append(tmpeCodes, `Greece`)

		// South Georgia and the South Sandwich Islands
		case "GS":
			tmpeCodes = append(tmpeCodes, `South Georgia and the South Sandwich Islands`)

		// Guatemala
		case "GT":
			tmpeCodes = append(tmpeCodes, `Guatemala`)

		// Guam
		case "GU":
			tmpeCodes = append(tmpeCodes, `Guam`)

		// Guinea-Bissau
		case "GW":
			tmpeCodes = append(tmpeCodes, `Guinea-Bissau`)

		// Guyana
		case "GY":
			tmpeCodes = append(tmpeCodes, `Guyana`)

		// Hong Kong
		case "HK":
			tmpeCodes = append(tmpeCodes, `Hong Kong`)

		// Heard Island and McDonald Islands
		case "HM":
			tmpeCodes = append(tmpeCodes, `Heard Island and McDonald Islands`)

		// Honduras
		case "HN":
			tmpeCodes = append(tmpeCodes, `Honduras`)

		// Croatia
		case "HR":
			tmpeCodes = append(tmpeCodes, `Croatia`)

		// Haiti
		case "HT":
			tmpeCodes = append(tmpeCodes, `Haiti`)

		// Hungary
		case "HU":
			tmpeCodes = append(tmpeCodes, `Hungary`)

		// Indonesia
		case "ID":
			tmpeCodes = append(tmpeCodes, `Indonesia`)

		// Ireland
		case "IE":
			tmpeCodes = append(tmpeCodes, `Ireland`)

		// Israel
		case "IL":
			tmpeCodes = append(tmpeCodes, `Israel`)

		// Isle of Man
		case "IM":
			tmpeCodes = append(tmpeCodes, `Isle of Man`)

		// India
		case "IN":
			tmpeCodes = append(tmpeCodes, `India`)

		// British Indian Ocean Territory
		case "IO":
			tmpeCodes = append(tmpeCodes, `British Indian Ocean Territory`)

		// Iraq
		case "IQ":
			tmpeCodes = append(tmpeCodes, `Iraq`)

		// Iran, Islamic Republic of
		case "IR":
			tmpeCodes = append(tmpeCodes, `Iran, Islamic Republic of`)

		// Iceland
		case "IS":
			tmpeCodes = append(tmpeCodes, `Iceland`)

		// Italy
		case "IT":
			tmpeCodes = append(tmpeCodes, `Italy`)

		// Jersey
		case "JE":
			tmpeCodes = append(tmpeCodes, `Jersey`)

		// Jamaica
		case "JM":
			tmpeCodes = append(tmpeCodes, `Jamaica`)

		// Jordan
		case "JO":
			tmpeCodes = append(tmpeCodes, `Jordan`)

		// Japan
		case "JP":
			tmpeCodes = append(tmpeCodes, `Japan`)

		// Kenya
		case "KE":
			tmpeCodes = append(tmpeCodes, `Kenya`)

		// Kyrgyzstan
		case "KG":
			tmpeCodes = append(tmpeCodes, `Kyrgyzstan`)

		// Cambodia
		case "KH":
			tmpeCodes = append(tmpeCodes, `Cambodia`)

		// Kiribati
		case "KI":
			tmpeCodes = append(tmpeCodes, `Kiribati`)

		// Comoros
		case "KM":
			tmpeCodes = append(tmpeCodes, `Comoros`)

		// Saint Kitts and Nevis
		case "KN":
			tmpeCodes = append(tmpeCodes, `Saint Kitts and Nevis`)

		// Korea, Democratic People’s Republic of
		case "KP":
			tmpeCodes = append(tmpeCodes, `Korea, Democratic People’s Republic of`)

		// Korea, Republic of
		case "KR":
			tmpeCodes = append(tmpeCodes, `Korea, Republic of`)

		// Kuwait
		case "KW":
			tmpeCodes = append(tmpeCodes, `Kuwait`)

		// Cayman Islands
		case "KY":
			tmpeCodes = append(tmpeCodes, `Cayman Islands`)

		// Kazakhstan
		case "KZ":
			tmpeCodes = append(tmpeCodes, `Kazakhstan`)

		// Lao People’s Democratic Republic
		case "LA":
			tmpeCodes = append(tmpeCodes, `Lao People’s Democratic Republic`)

		// Lebanon
		case "LB":
			tmpeCodes = append(tmpeCodes, `Lebanon`)

		// Saint Lucia
		case "LC":
			tmpeCodes = append(tmpeCodes, `Saint Lucia`)

		// Liechtenstein
		case "LI":
			tmpeCodes = append(tmpeCodes, `Liechtenstein`)

		// Sri Lanka
		case "LK":
			tmpeCodes = append(tmpeCodes, `Sri Lanka`)

		// Liberia
		case "LR":
			tmpeCodes = append(tmpeCodes, `Liberia`)

		// Lesotho
		case "LS":
			tmpeCodes = append(tmpeCodes, `Lesotho`)

		// Lithuania
		case "LT":
			tmpeCodes = append(tmpeCodes, `Lithuania`)

		// Luxembourg
		case "LU":
			tmpeCodes = append(tmpeCodes, `Luxembourg`)

		// Latvia
		case "LV":
			tmpeCodes = append(tmpeCodes, `Latvia`)

		// Libya
		case "LY":
			tmpeCodes = append(tmpeCodes, `Libya`)

		// Morocco
		case "MA":
			tmpeCodes = append(tmpeCodes, `Morocco`)

		// Monaco
		case "MC":
			tmpeCodes = append(tmpeCodes, `Monaco`)

		// Moldova, Republic of
		case "MD":
			tmpeCodes = append(tmpeCodes, `Moldova, Republic of`)

		// Montenegro
		case "ME":
			tmpeCodes = append(tmpeCodes, `Montenegro`)

		// Saint Martin (French part)
		case "MF":
			tmpeCodes = append(tmpeCodes, `Saint Martin (French part)`)

		// Madagascar
		case "MG":
			tmpeCodes = append(tmpeCodes, `Madagascar`)

		// Marshall Islands
		case "MH":
			tmpeCodes = append(tmpeCodes, `Marshall Islands`)

		// Formerly FYR Macedonia
		case "MK":
			tmpeCodes = append(tmpeCodes, `North Macedonia`)

		// Mali
		case "ML":
			tmpeCodes = append(tmpeCodes, `Mali`)

		// Myanmar
		case "MM":
			tmpeCodes = append(tmpeCodes, `Myanmar`)

		// Mongolia
		case "MN":
			tmpeCodes = append(tmpeCodes, `Mongolia`)

		// Macao
		case "MO":
			tmpeCodes = append(tmpeCodes, `Macao`)

		// Northern Mariana Islands
		case "MP":
			tmpeCodes = append(tmpeCodes, `Northern Mariana Islands`)

		// Martinique
		case "MQ":
			tmpeCodes = append(tmpeCodes, `Martinique`)

		// Mauritania
		case "MR":
			tmpeCodes = append(tmpeCodes, `Mauritania`)

		// Montserrat
		case "MS":
			tmpeCodes = append(tmpeCodes, `Montserrat`)

		// Malta
		case "MT":
			tmpeCodes = append(tmpeCodes, `Malta`)

		// Mauritius
		case "MU":
			tmpeCodes = append(tmpeCodes, `Mauritius`)

		// Maldives
		case "MV":
			tmpeCodes = append(tmpeCodes, `Maldives`)

		// Malawi
		case "MW":
			tmpeCodes = append(tmpeCodes, `Malawi`)

		// Mexico
		case "MX":
			tmpeCodes = append(tmpeCodes, `Mexico`)

		// Malaysia
		case "MY":
			tmpeCodes = append(tmpeCodes, `Malaysia`)

		// Mozambique
		case "MZ":
			tmpeCodes = append(tmpeCodes, `Mozambique`)

		// Namibia
		case "NA":
			tmpeCodes = append(tmpeCodes, `Namibia`)

		// New Caledonia
		case "NC":
			tmpeCodes = append(tmpeCodes, `New Caledonia`)

		// Niger
		case "NE":
			tmpeCodes = append(tmpeCodes, `Niger`)

		// Norfolk Island
		case "NF":
			tmpeCodes = append(tmpeCodes, `Norfolk Island`)

		// Nigeria
		case "NG":
			tmpeCodes = append(tmpeCodes, `Nigeria`)

		// Nicaragua
		case "NI":
			tmpeCodes = append(tmpeCodes, `Nicaragua`)

		// Netherlands
		case "NL":
			tmpeCodes = append(tmpeCodes, `Netherlands`)

		// Norway
		case "NO":
			tmpeCodes = append(tmpeCodes, `Norway`)

		// Nepal
		case "NP":
			tmpeCodes = append(tmpeCodes, `Nepal`)

		// Nauru
		case "NR":
			tmpeCodes = append(tmpeCodes, `Nauru`)

		// Niue
		case "NU":
			tmpeCodes = append(tmpeCodes, `Niue`)

		// New Zealand
		case "NZ":
			tmpeCodes = append(tmpeCodes, `New Zealand`)

		// Oman
		case "OM":
			tmpeCodes = append(tmpeCodes, `Oman`)

		// Panama
		case "PA":
			tmpeCodes = append(tmpeCodes, `Panama`)

		// Peru
		case "PE":
			tmpeCodes = append(tmpeCodes, `Peru`)

		// French Polynesia
		case "PF":
			tmpeCodes = append(tmpeCodes, `French Polynesia`)

		// Papua New Guinea
		case "PG":
			tmpeCodes = append(tmpeCodes, `Papua New Guinea`)

		// Philippines
		case "PH":
			tmpeCodes = append(tmpeCodes, `Philippines`)

		// Pakistan
		case "PK":
			tmpeCodes = append(tmpeCodes, `Pakistan`)

		// Poland
		case "PL":
			tmpeCodes = append(tmpeCodes, `Poland`)

		// Saint Pierre and Miquelon
		case "PM":
			tmpeCodes = append(tmpeCodes, `Saint Pierre and Miquelon`)

		// Pitcairn
		case "PN":
			tmpeCodes = append(tmpeCodes, `Pitcairn`)

		// Puerto Rico
		case "PR":
			tmpeCodes = append(tmpeCodes, `Puerto Rico`)

		// Palestine, State of
		case "PS":
			tmpeCodes = append(tmpeCodes, `Palestine, State of`)

		// Portugal
		case "PT":
			tmpeCodes = append(tmpeCodes, `Portugal`)

		// Palau
		case "PW":
			tmpeCodes = append(tmpeCodes, `Palau`)

		// Paraguay
		case "PY":
			tmpeCodes = append(tmpeCodes, `Paraguay`)

		// Qatar
		case "QA":
			tmpeCodes = append(tmpeCodes, `Qatar`)

		// Réunion
		case "RE":
			tmpeCodes = append(tmpeCodes, `Réunion`)

		// Romania
		case "RO":
			tmpeCodes = append(tmpeCodes, `Romania`)

		// Serbia
		case "RS":
			tmpeCodes = append(tmpeCodes, `Serbia`)

		// Russian Federation
		case "RU":
			tmpeCodes = append(tmpeCodes, `Russian Federation`)

		// Rwanda
		case "RW":
			tmpeCodes = append(tmpeCodes, `Rwanda`)

		// Saudi Arabia
		case "SA":
			tmpeCodes = append(tmpeCodes, `Saudi Arabia`)

		// Solomon Islands
		case "SB":
			tmpeCodes = append(tmpeCodes, `Solomon Islands`)

		// Seychelles
		case "SC":
			tmpeCodes = append(tmpeCodes, `Seychelles`)

		// Sudan
		case "SD":
			tmpeCodes = append(tmpeCodes, `Sudan`)

		// Sweden
		case "SE":
			tmpeCodes = append(tmpeCodes, `Sweden`)

		// Singapore
		case "SG":
			tmpeCodes = append(tmpeCodes, `Singapore`)

		// Saint Helena, Ascension and Tristan da Cunha
		case "SH":
			tmpeCodes = append(tmpeCodes, `Saint Helena, Ascension and Tristan da Cunha`)

		// Slovenia
		case "SI":
			tmpeCodes = append(tmpeCodes, `Slovenia`)

		// Svalbard and Jan Mayen
		case "SJ":
			tmpeCodes = append(tmpeCodes, `Svalbard and Jan Mayen`)

		// Slovakia
		case "SK":
			tmpeCodes = append(tmpeCodes, `Slovakia`)

		// Sierra Leone
		case "SL":
			tmpeCodes = append(tmpeCodes, `Sierra Leone`)

		// San Marino
		case "SM":
			tmpeCodes = append(tmpeCodes, `San Marino`)

		// Senegal
		case "SN":
			tmpeCodes = append(tmpeCodes, `Senegal`)

		// Somalia
		case "SO":
			tmpeCodes = append(tmpeCodes, `Somalia`)

		// Suriname
		case "SR":
			tmpeCodes = append(tmpeCodes, `Suriname`)

		// South Sudan
		case "SS":
			tmpeCodes = append(tmpeCodes, `South Sudan`)

		// Sao Tome and Principe
		case "ST":
			tmpeCodes = append(tmpeCodes, `Sao Tome and Principe`)

		// El Salvador
		case "SV":
			tmpeCodes = append(tmpeCodes, `El Salvador`)

		// Sint Maarten (Dutch part)
		case "SX":
			tmpeCodes = append(tmpeCodes, `Sint Maarten (Dutch part)`)

		// Syrian Arab Republic
		case "SY":
			tmpeCodes = append(tmpeCodes, `Syrian Arab Republic`)

		// Formerly known as Swaziland
		case "SZ":
			tmpeCodes = append(tmpeCodes, `Eswatini`)

		// Turks and Caicos Islands
		case "TC":
			tmpeCodes = append(tmpeCodes, `Turks and Caicos Islands`)

		// Chad
		case "TD":
			tmpeCodes = append(tmpeCodes, `Chad`)

		// French Southern Territories
		case "TF":
			tmpeCodes = append(tmpeCodes, `French Southern Territories`)

		// Togo
		case "TG":
			tmpeCodes = append(tmpeCodes, `Togo`)

		// Thailand
		case "TH":
			tmpeCodes = append(tmpeCodes, `Thailand`)

		// Tajikistan
		case "TJ":
			tmpeCodes = append(tmpeCodes, `Tajikistan`)

		// Tokelau
		case "TK":
			tmpeCodes = append(tmpeCodes, `Tokelau`)

		// Timor-Leste
		case "TL":
			tmpeCodes = append(tmpeCodes, `Timor-Leste`)

		// Turkmenistan
		case "TM":
			tmpeCodes = append(tmpeCodes, `Turkmenistan`)

		// Tunisia
		case "TN":
			tmpeCodes = append(tmpeCodes, `Tunisia`)

		// Tonga
		case "TO":
			tmpeCodes = append(tmpeCodes, `Tonga`)

		// Turkey
		case "TR":
			tmpeCodes = append(tmpeCodes, `Turkey`)

		// Trinidad and Tobago
		case "TT":
			tmpeCodes = append(tmpeCodes, `Trinidad and Tobago`)

		// Tuvalu
		case "TV":
			tmpeCodes = append(tmpeCodes, `Tuvalu`)

		// Taiwan, Province of China
		case "TW":
			tmpeCodes = append(tmpeCodes, `Taiwan, Province of China`)

		// Tanzania, United Republic of
		case "TZ":
			tmpeCodes = append(tmpeCodes, `Tanzania, United Republic of`)

		// Ukraine
		case "UA":
			tmpeCodes = append(tmpeCodes, `Ukraine`)

		// Uganda
		case "UG":
			tmpeCodes = append(tmpeCodes, `Uganda`)

		// United States Minor Outlying Islands
		case "UM":
			tmpeCodes = append(tmpeCodes, `United States Minor Outlying Islands`)

		// United States
		case "US":
			tmpeCodes = append(tmpeCodes, `United States`)

		// Uruguay
		case "UY":
			tmpeCodes = append(tmpeCodes, `Uruguay`)

		// Uzbekistan
		case "UZ":
			tmpeCodes = append(tmpeCodes, `Uzbekistan`)

		// Holy See (Vatican City State)
		case "VA":
			tmpeCodes = append(tmpeCodes, `Holy See (Vatican City State)`)

		// Saint Vincent and the Grenadines
		case "VC":
			tmpeCodes = append(tmpeCodes, `Saint Vincent and the Grenadines`)

		// Venezuela, Bolivarian Republic of
		case "VE":
			tmpeCodes = append(tmpeCodes, `Venezuela, Bolivarian Republic of`)

		// Virgin Islands, British
		case "VG":
			tmpeCodes = append(tmpeCodes, `Virgin Islands, British`)

		// Virgin Islands, US
		case "VI":
			tmpeCodes = append(tmpeCodes, `Virgin Islands, US`)

		// Viet Nam
		case "VN":
			tmpeCodes = append(tmpeCodes, `Viet Nam`)

		// Vanuatu
		case "VU":
			tmpeCodes = append(tmpeCodes, `Vanuatu`)

		// Wallis and Futuna
		case "WF":
			tmpeCodes = append(tmpeCodes, `Wallis and Futuna`)

		// Samoa
		case "WS":
			tmpeCodes = append(tmpeCodes, `Samoa`)

		// Yemen
		case "YE":
			tmpeCodes = append(tmpeCodes, `Yemen`)

		// Mayotte
		case "YT":
			tmpeCodes = append(tmpeCodes, `Mayotte`)

		// DEPRECATED, replaced by ME – Montenegro and RS – Serbia
		case "YU":
			tmpeCodes = append(tmpeCodes, `Yugoslavia`)

		// South Africa
		case "ZA":
			tmpeCodes = append(tmpeCodes, `South Africa`)

		// Zambia
		case "ZM":
			tmpeCodes = append(tmpeCodes, `Zambia`)

		// Zimbabwe
		case "ZW":
			tmpeCodes = append(tmpeCodes, `Zimbabwe`)
		default:
			return fmt.Errorf("undefined code for PrizeCountry has been passed, got [%s]", v)
		}
	}
	c.Body = tmpeCodes
	return nil
}

// PrizeRegion Region – based on ISO 3166-2
type PrizeRegion struct {
	Body []string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *PrizeRegion) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	codes := strings.Split(v, " ")
	tmpeCodes := []string{}
	for _, code := range codes {
		switch code {

		// Australian Capital Territory
		case "AU-CT":
			tmpeCodes = append(tmpeCodes, `Australian Capital Territory`)

		// New South Wales
		case "AU-NS":
			tmpeCodes = append(tmpeCodes, `New South Wales`)

		// Northern Territory
		case "AU-NT":
			tmpeCodes = append(tmpeCodes, `Northern Territory`)

		// Queensland
		case "AU-QL":
			tmpeCodes = append(tmpeCodes, `Queensland`)

		// South Australia
		case "AU-SA":
			tmpeCodes = append(tmpeCodes, `South Australia`)

		// Tasmania
		case "AU-TS":
			tmpeCodes = append(tmpeCodes, `Tasmania`)

		// Victoria
		case "AU-VI":
			tmpeCodes = append(tmpeCodes, `Victoria`)

		// Western Australia
		case "AU-WA":
			tmpeCodes = append(tmpeCodes, `Western Australia`)

		// For use in ONIX 3.0 only
		case "BE-BRU":
			tmpeCodes = append(tmpeCodes, `Brussels-Capital Region`)

		// For use in ONIX 3.0 only
		case "BE-VLG":
			tmpeCodes = append(tmpeCodes, `Flemish Region`)

		// For use in ONIX 3.0 only
		case "BE-WAL":
			tmpeCodes = append(tmpeCodes, `Walloon Region`)

		// Alberta
		case "CA-AB":
			tmpeCodes = append(tmpeCodes, `Alberta`)

		// British Columbia
		case "CA-BC":
			tmpeCodes = append(tmpeCodes, `British Columbia`)

		// Manitoba
		case "CA-MB":
			tmpeCodes = append(tmpeCodes, `Manitoba`)

		// New Brunswick
		case "CA-NB":
			tmpeCodes = append(tmpeCodes, `New Brunswick`)

		// Newfoundland and Labrador
		case "CA-NL":
			tmpeCodes = append(tmpeCodes, `Newfoundland and Labrador`)

		// Nova Scotia
		case "CA-NS":
			tmpeCodes = append(tmpeCodes, `Nova Scotia`)

		// Northwest Territories
		case "CA-NT":
			tmpeCodes = append(tmpeCodes, `Northwest Territories`)

		// Nunavut
		case "CA-NU":
			tmpeCodes = append(tmpeCodes, `Nunavut`)

		// Ontario
		case "CA-ON":
			tmpeCodes = append(tmpeCodes, `Ontario`)

		// Prince Edward Island
		case "CA-PE":
			tmpeCodes = append(tmpeCodes, `Prince Edward Island`)

		// Quebec
		case "CA-QC":
			tmpeCodes = append(tmpeCodes, `Quebec`)

		// Saskatchewan
		case "CA-SK":
			tmpeCodes = append(tmpeCodes, `Saskatchewan`)

		// Yukon Territory
		case "CA-YT":
			tmpeCodes = append(tmpeCodes, `Yukon Territory`)

		// For use in ONIX 3.0 only
		case "CN-BJ":
			tmpeCodes = append(tmpeCodes, `Beijing Municipality`)

		// For use in ONIX 3.0 only
		case "CN-TJ":
			tmpeCodes = append(tmpeCodes, `Tianjin Municipality`)

		// For use in ONIX 3.0 only
		case "CN-HE":
			tmpeCodes = append(tmpeCodes, `Hebei Province`)

		// For use in ONIX 3.0 only
		case "CN-SX":
			tmpeCodes = append(tmpeCodes, `Shanxi Province`)

		// For use in ONIX 3.0 only
		case "CN-NM":
			tmpeCodes = append(tmpeCodes, `Inner Mongolia Autonomous Region`)

		// For use in ONIX 3.0 only
		case "CN-LN":
			tmpeCodes = append(tmpeCodes, `Liaoning Province`)

		// For use in ONIX 3.0 only
		case "CN-JL":
			tmpeCodes = append(tmpeCodes, `Jilin Province`)

		// For use in ONIX 3.0 only
		case "CN-HL":
			tmpeCodes = append(tmpeCodes, `Heilongjiang Province`)

		// For use in ONIX 3.0 only
		case "CN-SH":
			tmpeCodes = append(tmpeCodes, `Shanghai Municipality`)

		// For use in ONIX 3.0 only
		case "CN-JS":
			tmpeCodes = append(tmpeCodes, `Jiangsu Province`)

		// For use in ONIX 3.0 only
		case "CN-ZJ":
			tmpeCodes = append(tmpeCodes, `Zhejiang Province`)

		// For use in ONIX 3.0 only
		case "CN-AH":
			tmpeCodes = append(tmpeCodes, `Anhui Province`)

		// For use in ONIX 3.0 only
		case "CN-FJ":
			tmpeCodes = append(tmpeCodes, `Fujian Province`)

		// For use in ONIX 3.0 only
		case "CN-JX":
			tmpeCodes = append(tmpeCodes, `Jiangxi Province`)

		// For use in ONIX 3.0 only
		case "CN-SD":
			tmpeCodes = append(tmpeCodes, `Shandong Province`)

		// For use in ONIX 3.0 only
		case "CN-HA":
			tmpeCodes = append(tmpeCodes, `Henan Province`)

		// For use in ONIX 3.0 only
		case "CN-HB":
			tmpeCodes = append(tmpeCodes, `Hubei Province`)

		// For use in ONIX 3.0 only
		case "CN-HN":
			tmpeCodes = append(tmpeCodes, `Hunan Province`)

		// For use in ONIX 3.0 only
		case "CN-GD":
			tmpeCodes = append(tmpeCodes, `Guangdong Province`)

		// For use in ONIX 3.0 only
		case "CN-GX":
			tmpeCodes = append(tmpeCodes, `Guangxi Zhuang Autonomous Region`)

		// For use in ONIX 3.0 only
		case "CN-HI":
			tmpeCodes = append(tmpeCodes, `Hainan Province`)

		// For use in ONIX 3.0 only
		case "CN-CQ":
			tmpeCodes = append(tmpeCodes, `Chongqing Municipality`)

		// For use in ONIX 3.0 only
		case "CN-SC":
			tmpeCodes = append(tmpeCodes, `Sichuan Province`)

		// For use in ONIX 3.0 only
		case "CN-GZ":
			tmpeCodes = append(tmpeCodes, `Guizhou Province`)

		// For use in ONIX 3.0 only
		case "CN-YN":
			tmpeCodes = append(tmpeCodes, `Yunnan Province`)

		// For use in ONIX 3.0 only
		case "CN-XZ":
			tmpeCodes = append(tmpeCodes, `Tibet Autonomous Region`)

		// For use in ONIX 3.0 only
		case "CN-SN":
			tmpeCodes = append(tmpeCodes, `Shaanxi Province`)

		// For use in ONIX 3.0 only
		case "CN-GS":
			tmpeCodes = append(tmpeCodes, `Gansu Province`)

		// For use in ONIX 3.0 only
		case "CN-QH":
			tmpeCodes = append(tmpeCodes, `Qinghai Province`)

		// For use in ONIX 3.0 only
		case "CN-NX":
			tmpeCodes = append(tmpeCodes, `Ningxia Hui Autonomous Region`)

		// For use in ONIX 3.0 only
		case "CN-XJ":
			tmpeCodes = append(tmpeCodes, `Xinjiang Uyghur Autonomous Region`)

		// Prefer code TW (Taiwan, Province of China) from List 91. For use in ONIX 3.0 only
		case "CN-TW":
			tmpeCodes = append(tmpeCodes, `Taiwan Province`)

		// Prefer code HK (Hong Kong) from List 91. For use in ONIX 3.0 only
		case "CN-HK":
			tmpeCodes = append(tmpeCodes, `Hong Kong Special Administrative Region`)

		// Prefer code MO (Macao) from List 91. For use in ONIX 3.0 only
		case "CN-MO":
			tmpeCodes = append(tmpeCodes, `Macau Special Administrative Region`)

		// Deprecated in favor of CN-BJ
		case "CN-11":
			tmpeCodes = append(tmpeCodes, `Beijing Municipality`)

		// Deprecated in favor of CN-TJ
		case "CN-12":
			tmpeCodes = append(tmpeCodes, `Tianjin Municipality`)

		// Deprecated in favor of CN-HE
		case "CN-13":
			tmpeCodes = append(tmpeCodes, `Hebei Province`)

		// Deprecated in favor of CN-SX
		case "CN-14":
			tmpeCodes = append(tmpeCodes, `Shanxi Province`)

		// Deprecated in favor of CN-NM
		case "CN-15":
			tmpeCodes = append(tmpeCodes, `Inner Mongolia Autonomous Region`)

		// Deprecated in favor of CN-LN
		case "CN-21":
			tmpeCodes = append(tmpeCodes, `Liaoning Province`)

		// Deprecated in favor of CN-JL
		case "CN-22":
			tmpeCodes = append(tmpeCodes, `Jilin Province`)

		// Deprecated in favor of CN-HL
		case "CN-23":
			tmpeCodes = append(tmpeCodes, `Heilongjiang Province`)

		// Deprecated in favor of CN-SH
		case "CN-31":
			tmpeCodes = append(tmpeCodes, `Shanghai Municipality`)

		// Deprecated in favor of CN-JS
		case "CN-32":
			tmpeCodes = append(tmpeCodes, `Jiangsu Province`)

		// Deprecated in favor of CN-ZJ
		case "CN-33":
			tmpeCodes = append(tmpeCodes, `Zhejiang Province`)

		// Deprecated in favor of CN-AH
		case "CN-34":
			tmpeCodes = append(tmpeCodes, `Anhui Province`)

		// Deprecated in favor of CN-FJ
		case "CN-35":
			tmpeCodes = append(tmpeCodes, `Fujian Province`)

		// Deprecated in favor of CN-JX
		case "CN-36":
			tmpeCodes = append(tmpeCodes, `Jiangxi Province`)

		// Deprecated in favor of CN-SD
		case "CN-37":
			tmpeCodes = append(tmpeCodes, `Shandong Province`)

		// Deprecated in favor of CN-HA
		case "CN-41":
			tmpeCodes = append(tmpeCodes, `Henan Province`)

		// Deprecated in favor of CN-HB
		case "CN-42":
			tmpeCodes = append(tmpeCodes, `Hubei Province`)

		// Deprecated in favor of CN-HN
		case "CN-43":
			tmpeCodes = append(tmpeCodes, `Hunan Province`)

		// Deprecated in favor of CN-GD
		case "CN-44":
			tmpeCodes = append(tmpeCodes, `Guangdong Province`)

		// Deprecated in favor of CN-GX
		case "CN-45":
			tmpeCodes = append(tmpeCodes, `Guangxi Zhuang Autonomous Region`)

		// Deprecated in favor of CN-HI
		case "CN-46":
			tmpeCodes = append(tmpeCodes, `Hainan Province`)

		// Deprecated in favor of CN-CQ
		case "CN-50":
			tmpeCodes = append(tmpeCodes, `Chongqing Municipality`)

		// Deprecated in favor of CN-SC
		case "CN-51":
			tmpeCodes = append(tmpeCodes, `Sichuan Province`)

		// Deprecated in favor of CN-GZ
		case "CN-52":
			tmpeCodes = append(tmpeCodes, `Guizhou Province`)

		// Deprecated in favor of CN-YN
		case "CN-53":
			tmpeCodes = append(tmpeCodes, `Yunnan Province`)

		// Deprecated in favor of CN-XZ
		case "CN-54":
			tmpeCodes = append(tmpeCodes, `Tibet Autonomous Region`)

		// Deprecated in favor of CN-SN
		case "CN-61":
			tmpeCodes = append(tmpeCodes, `Shaanxi Province`)

		// Deprecated in favor of CN-GS
		case "CN-62":
			tmpeCodes = append(tmpeCodes, `Gansu Province`)

		// Deprecated in favor of CN-QH
		case "CN-63":
			tmpeCodes = append(tmpeCodes, `Qinghai Province`)

		// Deprecated in favor of CN-NX
		case "CN-64":
			tmpeCodes = append(tmpeCodes, `Ningxia Hui Autonomous Region`)

		// Deprecated in favor of CN-XJ
		case "CN-65":
			tmpeCodes = append(tmpeCodes, `Xinjiang Uyghur Autonomous Region`)

		// Deprecated in favor of CN-TW, but prefer code TW (Taiwan, Province of China) from List 91
		case "CN-71":
			tmpeCodes = append(tmpeCodes, `Taiwan Province`)

		// Deprecated in favor of CN-HK, but prefer code HK (Hong Kong) from List 91
		case "CN-91":
			tmpeCodes = append(tmpeCodes, `Hong Kong Special Administrative Region`)

		// Deprecated in favor of CN-MO, but prefer code MO (Macao) from List 91
		case "CN-92":
			tmpeCodes = append(tmpeCodes, `Macau Special Administrative Region`)

		// Canary Islands
		case "ES-CN":
			tmpeCodes = append(tmpeCodes, `Canary Islands`)

		// Corsica
		case "FR-H":
			tmpeCodes = append(tmpeCodes, `Corsica`)

		// Airside outlets at UK international airports only
		case "GB-AIR":
			tmpeCodes = append(tmpeCodes, `UK airside`)

		// All UK airports, including both airside and other outlets
		case "GB-APS":
			tmpeCodes = append(tmpeCodes, `UK airports`)

		// DEPRECATED, replaced by country codes GG – Guernsey, and JE – Jersey from List 91
		case "GB-CHA":
			tmpeCodes = append(tmpeCodes, `Channel Islands`)

		// England
		case "GB-ENG":
			tmpeCodes = append(tmpeCodes, `England`)

		// UK excluding Northern Ireland. DEPRECATED – use separate region codes GB-ENG, GB-SCT, GB-WLS instead
		case "GB-EWS":
			tmpeCodes = append(tmpeCodes, `England, Wales, Scotland`)

		// DEPRECATED, replaced by country code IM – Isle of Man from List 91
		case "GB-IOM":
			tmpeCodes = append(tmpeCodes, `Isle of Man`)

		// Northern Ireland
		case "GB-NIR":
			tmpeCodes = append(tmpeCodes, `Northern Ireland`)

		// Scotland
		case "GB-SCT":
			tmpeCodes = append(tmpeCodes, `Scotland`)

		// Wales
		case "GB-WLS":
			tmpeCodes = append(tmpeCodes, `Wales`)

		// Airside outlets at Irish international airports only
		case "IE-AIR":
			tmpeCodes = append(tmpeCodes, `Ireland airside`)

		// Agrigento
		case "IT-AG":
			tmpeCodes = append(tmpeCodes, `Agrigento`)

		// Alessandria
		case "IT-AL":
			tmpeCodes = append(tmpeCodes, `Alessandria`)

		// Ancona
		case "IT-AN":
			tmpeCodes = append(tmpeCodes, `Ancona`)

		// Aosta
		case "IT-AO":
			tmpeCodes = append(tmpeCodes, `Aosta`)

		// Arezzo
		case "IT-AR":
			tmpeCodes = append(tmpeCodes, `Arezzo`)

		// Ascoli Piceno
		case "IT-AP":
			tmpeCodes = append(tmpeCodes, `Ascoli Piceno`)

		// Asti
		case "IT-AT":
			tmpeCodes = append(tmpeCodes, `Asti`)

		// Avellino
		case "IT-AV":
			tmpeCodes = append(tmpeCodes, `Avellino`)

		// Bari
		case "IT-BA":
			tmpeCodes = append(tmpeCodes, `Bari`)

		// Barletta-Andria-Trani
		case "IT-BT":
			tmpeCodes = append(tmpeCodes, `Barletta-Andria-Trani`)

		// Belluno
		case "IT-BL":
			tmpeCodes = append(tmpeCodes, `Belluno`)

		// Benevento
		case "IT-BN":
			tmpeCodes = append(tmpeCodes, `Benevento`)

		// Bergamo
		case "IT-BG":
			tmpeCodes = append(tmpeCodes, `Bergamo`)

		// Biella
		case "IT-BI":
			tmpeCodes = append(tmpeCodes, `Biella`)

		// Bologna
		case "IT-BO":
			tmpeCodes = append(tmpeCodes, `Bologna`)

		// Bolzano
		case "IT-BZ":
			tmpeCodes = append(tmpeCodes, `Bolzano`)

		// Brescia
		case "IT-BS":
			tmpeCodes = append(tmpeCodes, `Brescia`)

		// Brindisi
		case "IT-BR":
			tmpeCodes = append(tmpeCodes, `Brindisi`)

		// Cagliari
		case "IT-CA":
			tmpeCodes = append(tmpeCodes, `Cagliari`)

		// Caltanissetta
		case "IT-CL":
			tmpeCodes = append(tmpeCodes, `Caltanissetta`)

		// Campobasso
		case "IT-CB":
			tmpeCodes = append(tmpeCodes, `Campobasso`)

		// Carbonia-Iglesias
		case "IT-CI":
			tmpeCodes = append(tmpeCodes, `Carbonia-Iglesias`)

		// Caserta
		case "IT-CE":
			tmpeCodes = append(tmpeCodes, `Caserta`)

		// Catania
		case "IT-CT":
			tmpeCodes = append(tmpeCodes, `Catania`)

		// Catanzaro
		case "IT-CZ":
			tmpeCodes = append(tmpeCodes, `Catanzaro`)

		// Chieti
		case "IT-CH":
			tmpeCodes = append(tmpeCodes, `Chieti`)

		// Como
		case "IT-CO":
			tmpeCodes = append(tmpeCodes, `Como`)

		// Cosenza
		case "IT-CS":
			tmpeCodes = append(tmpeCodes, `Cosenza`)

		// Cremona
		case "IT-CR":
			tmpeCodes = append(tmpeCodes, `Cremona`)

		// Crotone
		case "IT-KR":
			tmpeCodes = append(tmpeCodes, `Crotone`)

		// Cuneo
		case "IT-CN":
			tmpeCodes = append(tmpeCodes, `Cuneo`)

		// Enna
		case "IT-EN":
			tmpeCodes = append(tmpeCodes, `Enna`)

		// Fermo
		case "IT-FM":
			tmpeCodes = append(tmpeCodes, `Fermo`)

		// Ferrara
		case "IT-FE":
			tmpeCodes = append(tmpeCodes, `Ferrara`)

		// Firenze
		case "IT-FI":
			tmpeCodes = append(tmpeCodes, `Firenze`)

		// Foggia
		case "IT-FG":
			tmpeCodes = append(tmpeCodes, `Foggia`)

		// Forlì-Cesena
		case "IT-FC":
			tmpeCodes = append(tmpeCodes, `Forlì-Cesena`)

		// Frosinone
		case "IT-FR":
			tmpeCodes = append(tmpeCodes, `Frosinone`)

		// Genova
		case "IT-GE":
			tmpeCodes = append(tmpeCodes, `Genova`)

		// Gorizia
		case "IT-GO":
			tmpeCodes = append(tmpeCodes, `Gorizia`)

		// Grosseto
		case "IT-GR":
			tmpeCodes = append(tmpeCodes, `Grosseto`)

		// Imperia
		case "IT-IM":
			tmpeCodes = append(tmpeCodes, `Imperia`)

		// Isernia
		case "IT-IS":
			tmpeCodes = append(tmpeCodes, `Isernia`)

		// La Spezia
		case "IT-SP":
			tmpeCodes = append(tmpeCodes, `La Spezia`)

		// L’Aquila
		case "IT-AQ":
			tmpeCodes = append(tmpeCodes, `L’Aquila`)

		// Latina
		case "IT-LT":
			tmpeCodes = append(tmpeCodes, `Latina`)

		// Lecce
		case "IT-LE":
			tmpeCodes = append(tmpeCodes, `Lecce`)

		// Lecco
		case "IT-LC":
			tmpeCodes = append(tmpeCodes, `Lecco`)

		// Livorno
		case "IT-LI":
			tmpeCodes = append(tmpeCodes, `Livorno`)

		// Lodi
		case "IT-LO":
			tmpeCodes = append(tmpeCodes, `Lodi`)

		// Lucca
		case "IT-LU":
			tmpeCodes = append(tmpeCodes, `Lucca`)

		// Macerata
		case "IT-MC":
			tmpeCodes = append(tmpeCodes, `Macerata`)

		// Mantova
		case "IT-MN":
			tmpeCodes = append(tmpeCodes, `Mantova`)

		// Massa-Carrara
		case "IT-MS":
			tmpeCodes = append(tmpeCodes, `Massa-Carrara`)

		// Matera
		case "IT-MT":
			tmpeCodes = append(tmpeCodes, `Matera`)

		// Medio Campidano
		case "IT-VS":
			tmpeCodes = append(tmpeCodes, `Medio Campidano`)

		// Messina
		case "IT-ME":
			tmpeCodes = append(tmpeCodes, `Messina`)

		// Milano
		case "IT-MI":
			tmpeCodes = append(tmpeCodes, `Milano`)

		// Modena
		case "IT-MO":
			tmpeCodes = append(tmpeCodes, `Modena`)

		// Monza e Brianza
		case "IT-MB":
			tmpeCodes = append(tmpeCodes, `Monza e Brianza`)

		// Napoli
		case "IT-NA":
			tmpeCodes = append(tmpeCodes, `Napoli`)

		// Novara
		case "IT-NO":
			tmpeCodes = append(tmpeCodes, `Novara`)

		// Nuoro
		case "IT-NU":
			tmpeCodes = append(tmpeCodes, `Nuoro`)

		// Ogliastra
		case "IT-OG":
			tmpeCodes = append(tmpeCodes, `Ogliastra`)

		// Olbia-Tempio
		case "IT-OT":
			tmpeCodes = append(tmpeCodes, `Olbia-Tempio`)

		// Oristano
		case "IT-OR":
			tmpeCodes = append(tmpeCodes, `Oristano`)

		// Padova
		case "IT-PD":
			tmpeCodes = append(tmpeCodes, `Padova`)

		// Palermo
		case "IT-PA":
			tmpeCodes = append(tmpeCodes, `Palermo`)

		// Parma
		case "IT-PR":
			tmpeCodes = append(tmpeCodes, `Parma`)

		// Pavia
		case "IT-PV":
			tmpeCodes = append(tmpeCodes, `Pavia`)

		// Perugia
		case "IT-PG":
			tmpeCodes = append(tmpeCodes, `Perugia`)

		// Pesaro e Urbino
		case "IT-PU":
			tmpeCodes = append(tmpeCodes, `Pesaro e Urbino`)

		// Pescara
		case "IT-PE":
			tmpeCodes = append(tmpeCodes, `Pescara`)

		// Piacenza
		case "IT-PC":
			tmpeCodes = append(tmpeCodes, `Piacenza`)

		// Pisa
		case "IT-PI":
			tmpeCodes = append(tmpeCodes, `Pisa`)

		// Pistoia
		case "IT-PT":
			tmpeCodes = append(tmpeCodes, `Pistoia`)

		// Pordenone
		case "IT-PN":
			tmpeCodes = append(tmpeCodes, `Pordenone`)

		// Potenza
		case "IT-PZ":
			tmpeCodes = append(tmpeCodes, `Potenza`)

		// Prato
		case "IT-PO":
			tmpeCodes = append(tmpeCodes, `Prato`)

		// Ragusa
		case "IT-RG":
			tmpeCodes = append(tmpeCodes, `Ragusa`)

		// Ravenna
		case "IT-RA":
			tmpeCodes = append(tmpeCodes, `Ravenna`)

		// Reggio Calabria
		case "IT-RC":
			tmpeCodes = append(tmpeCodes, `Reggio Calabria`)

		// Reggio Emilia
		case "IT-RE":
			tmpeCodes = append(tmpeCodes, `Reggio Emilia`)

		// Rieti
		case "IT-RI":
			tmpeCodes = append(tmpeCodes, `Rieti`)

		// Rimini
		case "IT-RN":
			tmpeCodes = append(tmpeCodes, `Rimini`)

		// Roma
		case "IT-RM":
			tmpeCodes = append(tmpeCodes, `Roma`)

		// Rovigo
		case "IT-RO":
			tmpeCodes = append(tmpeCodes, `Rovigo`)

		// Salerno
		case "IT-SA":
			tmpeCodes = append(tmpeCodes, `Salerno`)

		// Sassari
		case "IT-SS":
			tmpeCodes = append(tmpeCodes, `Sassari`)

		// Savona
		case "IT-SV":
			tmpeCodes = append(tmpeCodes, `Savona`)

		// Siena
		case "IT-SI":
			tmpeCodes = append(tmpeCodes, `Siena`)

		// Siracusa
		case "IT-SR":
			tmpeCodes = append(tmpeCodes, `Siracusa`)

		// Sondrio
		case "IT-SO":
			tmpeCodes = append(tmpeCodes, `Sondrio`)

		// Taranto
		case "IT-TA":
			tmpeCodes = append(tmpeCodes, `Taranto`)

		// Teramo
		case "IT-TE":
			tmpeCodes = append(tmpeCodes, `Teramo`)

		// Terni
		case "IT-TR":
			tmpeCodes = append(tmpeCodes, `Terni`)

		// Torino
		case "IT-TO":
			tmpeCodes = append(tmpeCodes, `Torino`)

		// Trapani
		case "IT-TP":
			tmpeCodes = append(tmpeCodes, `Trapani`)

		// Trento
		case "IT-TN":
			tmpeCodes = append(tmpeCodes, `Trento`)

		// Treviso
		case "IT-TV":
			tmpeCodes = append(tmpeCodes, `Treviso`)

		// Trieste
		case "IT-TS":
			tmpeCodes = append(tmpeCodes, `Trieste`)

		// Udine
		case "IT-UD":
			tmpeCodes = append(tmpeCodes, `Udine`)

		// Varese
		case "IT-VA":
			tmpeCodes = append(tmpeCodes, `Varese`)

		// Venezia
		case "IT-VE":
			tmpeCodes = append(tmpeCodes, `Venezia`)

		// Verbano-Cusio-Ossola
		case "IT-VB":
			tmpeCodes = append(tmpeCodes, `Verbano-Cusio-Ossola`)

		// Vercelli
		case "IT-VC":
			tmpeCodes = append(tmpeCodes, `Vercelli`)

		// Verona
		case "IT-VR":
			tmpeCodes = append(tmpeCodes, `Verona`)

		// Vibo Valentia
		case "IT-VV":
			tmpeCodes = append(tmpeCodes, `Vibo Valentia`)

		// Vicenza
		case "IT-VI":
			tmpeCodes = append(tmpeCodes, `Vicenza`)

		// Viterbo
		case "IT-VT":
			tmpeCodes = append(tmpeCodes, `Viterbo`)

		// Kosovo-Metohija
		case "RS-KM":
			tmpeCodes = append(tmpeCodes, `Kosovo-Metohija`)

		// Vojvodina
		case "RS-VO":
			tmpeCodes = append(tmpeCodes, `Vojvodina`)

		// Republic of Adygeya
		case "RU-AD":
			tmpeCodes = append(tmpeCodes, `Republic of Adygeya`)

		// Republic of Altay
		case "RU-AL":
			tmpeCodes = append(tmpeCodes, `Republic of Altay`)

		// Republic of Bashkortostan
		case "RU-BA":
			tmpeCodes = append(tmpeCodes, `Republic of Bashkortostan`)

		// Republic of Buryatiya
		case "RU-BU":
			tmpeCodes = append(tmpeCodes, `Republic of Buryatiya`)

		// Chechenskaya Republic
		case "RU-CE":
			tmpeCodes = append(tmpeCodes, `Chechenskaya Republic`)

		// Chuvashskaya Republic
		case "RU-CU":
			tmpeCodes = append(tmpeCodes, `Chuvashskaya Republic`)

		// Republic of Dagestan
		case "RU-DA":
			tmpeCodes = append(tmpeCodes, `Republic of Dagestan`)

		// Republic of Ingushetiya
		case "RU-IN":
			tmpeCodes = append(tmpeCodes, `Republic of Ingushetiya`)

		// Kabardino-Balkarskaya Republic
		case "RU-KB":
			tmpeCodes = append(tmpeCodes, `Kabardino-Balkarskaya Republic`)

		// Republic of Kalmykiya
		case "RU-KL":
			tmpeCodes = append(tmpeCodes, `Republic of Kalmykiya`)

		// Karachayevo-Cherkesskaya Republic
		case "RU-KC":
			tmpeCodes = append(tmpeCodes, `Karachayevo-Cherkesskaya Republic`)

		// Republic of Kareliya
		case "RU-KR":
			tmpeCodes = append(tmpeCodes, `Republic of Kareliya`)

		// Republic of Khakasiya
		case "RU-KK":
			tmpeCodes = append(tmpeCodes, `Republic of Khakasiya`)

		// Republic of Komi
		case "RU-KO":
			tmpeCodes = append(tmpeCodes, `Republic of Komi`)

		// Republic of Mariy El
		case "RU-ME":
			tmpeCodes = append(tmpeCodes, `Republic of Mariy El`)

		// Republic of Mordoviya
		case "RU-MO":
			tmpeCodes = append(tmpeCodes, `Republic of Mordoviya`)

		// Republic of Sakha (Yakutiya)
		case "RU-SA":
			tmpeCodes = append(tmpeCodes, `Republic of Sakha (Yakutiya)`)

		// Republic of Severnaya Osetiya-Alaniya
		case "RU-SE":
			tmpeCodes = append(tmpeCodes, `Republic of Severnaya Osetiya-Alaniya`)

		// Republic of Tatarstan
		case "RU-TA":
			tmpeCodes = append(tmpeCodes, `Republic of Tatarstan`)

		// Republic of Tyva (Tuva)
		case "RU-TY":
			tmpeCodes = append(tmpeCodes, `Republic of Tyva (Tuva)`)

		// Udmurtskaya Republic
		case "RU-UD":
			tmpeCodes = append(tmpeCodes, `Udmurtskaya Republic`)

		// Altayskiy Administrative Territory
		case "RU-ALT":
			tmpeCodes = append(tmpeCodes, `Altayskiy Administrative Territory`)

		// Kamchatskiy Administrative Territory
		case "RU-KAM":
			tmpeCodes = append(tmpeCodes, `Kamchatskiy Administrative Territory`)

		// Khabarovskiy Administrative Territory
		case "RU-KHA":
			tmpeCodes = append(tmpeCodes, `Khabarovskiy Administrative Territory`)

		// Krasnodarskiy Administrative Territory
		case "RU-KDA":
			tmpeCodes = append(tmpeCodes, `Krasnodarskiy Administrative Territory`)

		// Krasnoyarskiy Administrative Territory
		case "RU-KYA":
			tmpeCodes = append(tmpeCodes, `Krasnoyarskiy Administrative Territory`)

		// Permskiy Administrative Territory
		case "RU-PER":
			tmpeCodes = append(tmpeCodes, `Permskiy Administrative Territory`)

		// Primorskiy Administrative Territory
		case "RU-PRI":
			tmpeCodes = append(tmpeCodes, `Primorskiy Administrative Territory`)

		// Stavropol’skiy Administrative Territory
		case "RU-STA":
			tmpeCodes = append(tmpeCodes, `Stavropol’skiy Administrative Territory`)

		// Zabaykal’skiy Administrative Territory
		case "RU-ZAB":
			tmpeCodes = append(tmpeCodes, `Zabaykal’skiy Administrative Territory`)

		// Amurskaya Administrative Region
		case "RU-AMU":
			tmpeCodes = append(tmpeCodes, `Amurskaya Administrative Region`)

		// Arkhangel’skaya Administrative Region
		case "RU-ARK":
			tmpeCodes = append(tmpeCodes, `Arkhangel’skaya Administrative Region`)

		// Astrakhanskaya Administrative Region
		case "RU-AST":
			tmpeCodes = append(tmpeCodes, `Astrakhanskaya Administrative Region`)

		// Belgorodskaya Administrative Region
		case "RU-BEL":
			tmpeCodes = append(tmpeCodes, `Belgorodskaya Administrative Region`)

		// Bryanskaya Administrative Region
		case "RU-BRY":
			tmpeCodes = append(tmpeCodes, `Bryanskaya Administrative Region`)

		// Chelyabinskaya Administrative Region
		case "RU-CHE":
			tmpeCodes = append(tmpeCodes, `Chelyabinskaya Administrative Region`)

		// Irkutskaya Administrative Region
		case "RU-IRK":
			tmpeCodes = append(tmpeCodes, `Irkutskaya Administrative Region`)

		// Ivanovskaya Administrative Region
		case "RU-IVA":
			tmpeCodes = append(tmpeCodes, `Ivanovskaya Administrative Region`)

		// Kaliningradskaya Administrative Region
		case "RU-KGD":
			tmpeCodes = append(tmpeCodes, `Kaliningradskaya Administrative Region`)

		// Kaluzhskaya Administrative Region
		case "RU-KLU":
			tmpeCodes = append(tmpeCodes, `Kaluzhskaya Administrative Region`)

		// Kemerovskaya Administrative Region
		case "RU-KEM":
			tmpeCodes = append(tmpeCodes, `Kemerovskaya Administrative Region`)

		// Kirovskaya Administrative Region
		case "RU-KIR":
			tmpeCodes = append(tmpeCodes, `Kirovskaya Administrative Region`)

		// Kostromskaya Administrative Region
		case "RU-KOS":
			tmpeCodes = append(tmpeCodes, `Kostromskaya Administrative Region`)

		// Kurganskaya Administrative Region
		case "RU-KGN":
			tmpeCodes = append(tmpeCodes, `Kurganskaya Administrative Region`)

		// Kurskaya Administrative Region
		case "RU-KRS":
			tmpeCodes = append(tmpeCodes, `Kurskaya Administrative Region`)

		// Leningradskaya Administrative Region
		case "RU-LEN":
			tmpeCodes = append(tmpeCodes, `Leningradskaya Administrative Region`)

		// Lipetskaya Administrative Region
		case "RU-LIP":
			tmpeCodes = append(tmpeCodes, `Lipetskaya Administrative Region`)

		// Magadanskaya Administrative Region
		case "RU-MAG":
			tmpeCodes = append(tmpeCodes, `Magadanskaya Administrative Region`)

		// Moskovskaya Administrative Region
		case "RU-MOS":
			tmpeCodes = append(tmpeCodes, `Moskovskaya Administrative Region`)

		// Murmanskaya Administrative Region
		case "RU-MUR":
			tmpeCodes = append(tmpeCodes, `Murmanskaya Administrative Region`)

		// Nizhegorodskaya Administrative Region
		case "RU-NIZ":
			tmpeCodes = append(tmpeCodes, `Nizhegorodskaya Administrative Region`)

		// Novgorodskaya Administrative Region
		case "RU-NGR":
			tmpeCodes = append(tmpeCodes, `Novgorodskaya Administrative Region`)

		// Novosibirskaya Administrative Region
		case "RU-NVS":
			tmpeCodes = append(tmpeCodes, `Novosibirskaya Administrative Region`)

		// Omskaya Administrative Region
		case "RU-OMS":
			tmpeCodes = append(tmpeCodes, `Omskaya Administrative Region`)

		// Orenburgskaya Administrative Region
		case "RU-ORE":
			tmpeCodes = append(tmpeCodes, `Orenburgskaya Administrative Region`)

		// Orlovskaya Administrative Region
		case "RU-ORL":
			tmpeCodes = append(tmpeCodes, `Orlovskaya Administrative Region`)

		// Penzenskaya Administrative Region
		case "RU-PNZ":
			tmpeCodes = append(tmpeCodes, `Penzenskaya Administrative Region`)

		// Pskovskaya Administrative Region
		case "RU-PSK":
			tmpeCodes = append(tmpeCodes, `Pskovskaya Administrative Region`)

		// Rostovskaya Administrative Region
		case "RU-ROS":
			tmpeCodes = append(tmpeCodes, `Rostovskaya Administrative Region`)

		// Ryazanskaya Administrative Region
		case "RU-RYA":
			tmpeCodes = append(tmpeCodes, `Ryazanskaya Administrative Region`)

		// Sakhalinskaya Administrative Region
		case "RU-SAK":
			tmpeCodes = append(tmpeCodes, `Sakhalinskaya Administrative Region`)

		// Samarskaya Administrative Region
		case "RU-SAM":
			tmpeCodes = append(tmpeCodes, `Samarskaya Administrative Region`)

		// Saratovskaya Administrative Region
		case "RU-SAR":
			tmpeCodes = append(tmpeCodes, `Saratovskaya Administrative Region`)

		// Smolenskaya Administrative Region
		case "RU-SMO":
			tmpeCodes = append(tmpeCodes, `Smolenskaya Administrative Region`)

		// Sverdlovskaya Administrative Region
		case "RU-SVE":
			tmpeCodes = append(tmpeCodes, `Sverdlovskaya Administrative Region`)

		// Tambovskaya Administrative Region
		case "RU-TAM":
			tmpeCodes = append(tmpeCodes, `Tambovskaya Administrative Region`)

		// Tomskaya Administrative Region
		case "RU-TOM":
			tmpeCodes = append(tmpeCodes, `Tomskaya Administrative Region`)

		// Tul’skaya Administrative Region
		case "RU-TUL":
			tmpeCodes = append(tmpeCodes, `Tul’skaya Administrative Region`)

		// Tverskaya Administrative Region
		case "RU-TVE":
			tmpeCodes = append(tmpeCodes, `Tverskaya Administrative Region`)

		// Tyumenskaya Administrative Region
		case "RU-TYU":
			tmpeCodes = append(tmpeCodes, `Tyumenskaya Administrative Region`)

		// Ul’yanovskaya Administrative Region
		case "RU-ULY":
			tmpeCodes = append(tmpeCodes, `Ul’yanovskaya Administrative Region`)

		// Vladimirskaya Administrative Region
		case "RU-VLA":
			tmpeCodes = append(tmpeCodes, `Vladimirskaya Administrative Region`)

		// Volgogradskaya Administrative Region
		case "RU-VGG":
			tmpeCodes = append(tmpeCodes, `Volgogradskaya Administrative Region`)

		// Vologodskaya Administrative Region
		case "RU-VLG":
			tmpeCodes = append(tmpeCodes, `Vologodskaya Administrative Region`)

		// Voronezhskaya Administrative Region
		case "RU-VOR":
			tmpeCodes = append(tmpeCodes, `Voronezhskaya Administrative Region`)

		// Yaroslavskaya Administrative Region
		case "RU-YAR":
			tmpeCodes = append(tmpeCodes, `Yaroslavskaya Administrative Region`)

		// Moskva City
		case "RU-MOW":
			tmpeCodes = append(tmpeCodes, `Moskva City`)

		// Sankt-Peterburg City
		case "RU-SPE":
			tmpeCodes = append(tmpeCodes, `Sankt-Peterburg City`)

		// Yevreyskaya Autonomous Administrative Region
		case "RU-YEV":
			tmpeCodes = append(tmpeCodes, `Yevreyskaya Autonomous Administrative Region`)

		// Chukotskiy Autonomous District
		case "RU-CHU":
			tmpeCodes = append(tmpeCodes, `Chukotskiy Autonomous District`)

		// Khanty-Mansiyskiy Autonomous District
		case "RU-KHM":
			tmpeCodes = append(tmpeCodes, `Khanty-Mansiyskiy Autonomous District`)

		// Nenetskiy Autonomous District
		case "RU-NEN":
			tmpeCodes = append(tmpeCodes, `Nenetskiy Autonomous District`)

		// Yamalo-Nenetskiy Autonomous District
		case "RU-YAN":
			tmpeCodes = append(tmpeCodes, `Yamalo-Nenetskiy Autonomous District`)

		// Alaska
		case "US-AK":
			tmpeCodes = append(tmpeCodes, `Alaska`)

		// Alabama
		case "US-AL":
			tmpeCodes = append(tmpeCodes, `Alabama`)

		// Arkansas
		case "US-AR":
			tmpeCodes = append(tmpeCodes, `Arkansas`)

		// Arizona
		case "US-AZ":
			tmpeCodes = append(tmpeCodes, `Arizona`)

		// California
		case "US-CA":
			tmpeCodes = append(tmpeCodes, `California`)

		// Colorado
		case "US-CO":
			tmpeCodes = append(tmpeCodes, `Colorado`)

		// Connecticut
		case "US-CT":
			tmpeCodes = append(tmpeCodes, `Connecticut`)

		// District of Columbia
		case "US-DC":
			tmpeCodes = append(tmpeCodes, `District of Columbia`)

		// Delaware
		case "US-DE":
			tmpeCodes = append(tmpeCodes, `Delaware`)

		// Florida
		case "US-FL":
			tmpeCodes = append(tmpeCodes, `Florida`)

		// Georgia
		case "US-GA":
			tmpeCodes = append(tmpeCodes, `Georgia`)

		// Hawaii
		case "US-HI":
			tmpeCodes = append(tmpeCodes, `Hawaii`)

		// Iowa
		case "US-IA":
			tmpeCodes = append(tmpeCodes, `Iowa`)

		// Idaho
		case "US-ID":
			tmpeCodes = append(tmpeCodes, `Idaho`)

		// Illinois
		case "US-IL":
			tmpeCodes = append(tmpeCodes, `Illinois`)

		// Indiana
		case "US-IN":
			tmpeCodes = append(tmpeCodes, `Indiana`)

		// Kansas
		case "US-KS":
			tmpeCodes = append(tmpeCodes, `Kansas`)

		// Kentucky
		case "US-KY":
			tmpeCodes = append(tmpeCodes, `Kentucky`)

		// Louisiana
		case "US-LA":
			tmpeCodes = append(tmpeCodes, `Louisiana`)

		// Massachusetts
		case "US-MA":
			tmpeCodes = append(tmpeCodes, `Massachusetts`)

		// Maryland
		case "US-MD":
			tmpeCodes = append(tmpeCodes, `Maryland`)

		// Maine
		case "US-ME":
			tmpeCodes = append(tmpeCodes, `Maine`)

		// Michigan
		case "US-MI":
			tmpeCodes = append(tmpeCodes, `Michigan`)

		// Minnesota
		case "US-MN":
			tmpeCodes = append(tmpeCodes, `Minnesota`)

		// Missouri
		case "US-MO":
			tmpeCodes = append(tmpeCodes, `Missouri`)

		// Mississippi
		case "US-MS":
			tmpeCodes = append(tmpeCodes, `Mississippi`)

		// Montana
		case "US-MT":
			tmpeCodes = append(tmpeCodes, `Montana`)

		// North Carolina
		case "US-NC":
			tmpeCodes = append(tmpeCodes, `North Carolina`)

		// North Dakota
		case "US-ND":
			tmpeCodes = append(tmpeCodes, `North Dakota`)

		// Nebraska
		case "US-NE":
			tmpeCodes = append(tmpeCodes, `Nebraska`)

		// New Hampshire
		case "US-NH":
			tmpeCodes = append(tmpeCodes, `New Hampshire`)

		// New Jersey
		case "US-NJ":
			tmpeCodes = append(tmpeCodes, `New Jersey`)

		// New Mexico
		case "US-NM":
			tmpeCodes = append(tmpeCodes, `New Mexico`)

		// Nevada
		case "US-NV":
			tmpeCodes = append(tmpeCodes, `Nevada`)

		// New York
		case "US-NY":
			tmpeCodes = append(tmpeCodes, `New York`)

		// Ohio
		case "US-OH":
			tmpeCodes = append(tmpeCodes, `Ohio`)

		// Oklahoma
		case "US-OK":
			tmpeCodes = append(tmpeCodes, `Oklahoma`)

		// Oregon
		case "US-OR":
			tmpeCodes = append(tmpeCodes, `Oregon`)

		// Pennsylvania
		case "US-PA":
			tmpeCodes = append(tmpeCodes, `Pennsylvania`)

		// Rhode Island
		case "US-RI":
			tmpeCodes = append(tmpeCodes, `Rhode Island`)

		// South Carolina
		case "US-SC":
			tmpeCodes = append(tmpeCodes, `South Carolina`)

		// South Dakota
		case "US-SD":
			tmpeCodes = append(tmpeCodes, `South Dakota`)

		// Tennessee
		case "US-TN":
			tmpeCodes = append(tmpeCodes, `Tennessee`)

		// Texas
		case "US-TX":
			tmpeCodes = append(tmpeCodes, `Texas`)

		// Utah
		case "US-UT":
			tmpeCodes = append(tmpeCodes, `Utah`)

		// Virginia
		case "US-VA":
			tmpeCodes = append(tmpeCodes, `Virginia`)

		// Vermont
		case "US-VT":
			tmpeCodes = append(tmpeCodes, `Vermont`)

		// Washington
		case "US-WA":
			tmpeCodes = append(tmpeCodes, `Washington`)

		// Wisconsin
		case "US-WI":
			tmpeCodes = append(tmpeCodes, `Wisconsin`)

		// West Virginia
		case "US-WV":
			tmpeCodes = append(tmpeCodes, `West Virginia`)

		// Wyoming
		case "US-WY":
			tmpeCodes = append(tmpeCodes, `Wyoming`)

		// Countries geographically within continental Europe which use the Euro as their sole currency. At the time of writing, this is a synonym for ‘AT BE CY EE FI FR DE ES GR IE IT LT LU LV MT NL PT SI SK’ (the official Eurozone 19), plus ‘AD MC SM VA ME’ and Kosovo (other Euro-using countries in continental Europe). Note some other territories using the Euro, but outside continental Europe are excluded from this list, and may need to be specified separately. ONLY valid in ONIX 3, and ONLY within P.26 – and this use is itself DEPRECATED. Use of an explicit list of countries instead of ECZ is strongly encouraged
		case "ECZ":
			tmpeCodes = append(tmpeCodes, `Eurozone`)

		// In ONIX 3, may ONLY be used in <RegionsIncluded>
		case "WORLD":
			tmpeCodes = append(tmpeCodes, `World`)
		default:
			return fmt.Errorf("undefined code for PrizeRegion has been passed, got [%s]", v)
		}
	}
	c.Body = tmpeCodes
	return nil
}

// ProductAvailability Product availability
type ProductAvailability struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *ProductAvailability) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // Product was announced, and subsequently abandoned by the publisher. No expected availability date should be included in <SupplyDate>
  case "01":
		c.Body = `Cancelled`

  // Not yet available from the supplier, and the publisher indicates that it has been postponed indefinitely. Should be used in preference to code 10 where the publisher has indicated that a previously-announced publication date is no longer correct, and no new date has yet been announced. No expected avalabilty date should be included in <SupplyDate>. For use in ONIX 3.0 only
  case "09":
		c.Body = `Not yet available, postponed indefinitely`

  // Not yet available (requires expected date, either as <ExpectedShipDate> (ONIX 2.1) or as <SupplyDate> with <SupplyDateRole> coded ‘08’ (ONIX 3.0), except in exceptional circumstances where no date is known)
  case "10":
		c.Body = `Not yet available`

  // Not yet available, but will be a stock item when available (requires expected date, either as <ExpectedShipDate> (ONIX 2.1) or as <SupplyDate> with <SupplyDateRole> coded ‘08’ (ONIX 3.0), except in exceptional circumstances where no date is known). Used particularly for imports which have been published in the country of origin but have not yet arrived in the importing country
  case "11":
		c.Body = `Awaiting stock`

  // Not yet available, to be published as print-on-demand only (requires expected date, either as <ExpectedShipDate> (ONIX 2.1) or as <SupplyDate> with <SupplyDateRole> coded ‘08’ (ONIX 3.0), except in exceptional circumstances where no date is known). May apply either to a POD successor to an existing conventional edition, when the successor will be published under a different ISBN (normally because different trade terms apply); or to a title that is being published as a POD original
  case "12":
		c.Body = `Not yet available, will be POD`

  // Available from us (form of availability unspecified)
  case "20":
		c.Body = `Available`

  // Available from us as a stock item
  case "21":
		c.Body = `In stock`

  // Available from the supplier as a non-stock item, by special order. Where possible, an <OrderTime> should be included
  case "22":
		c.Body = `To order`

  // Available from the supplier by print-on-demand. If the fulfillment delay is likely to be more than 24 hours, an <OrderTime> should be included
  case "23":
		c.Body = `POD`

  // Temporarily unavailable: temporarily unavailable from us (reason unspecified) (requires expected date, either as <ExpectedShipDate> (ONIX 2.1) or as <SupplyDate> with <SupplyDateRole> coded ‘08’ (ONIX 3.0), except in exceptional circumstances where no date is known)
  case "30":
		c.Body = `Temporarily unavailable`

  // Stock item, temporarily out of stock (requires expected date, either as <ExpectedShipDate> (ONIX 2.1) or as <SupplyDate> with <SupplyDateRole> coded ‘08’ (ONIX 3.0), except in exceptional circumstances where no date is known)
  case "31":
		c.Body = `Out of stock`

  // Temporarily unavailable, reprinting (requires expected date, either as <ExpectedShipDate> (ONIX 2.1) or as <SupplyDate> with <SupplyDateRole> coded ‘08’ (ONIX 3.0), except in exceptional circumstances where no date is known)
  case "32":
		c.Body = `Reprinting`

  // Temporarily unavailable, awaiting reissue (requires expected date, either as <ExpectedShipDate> (ONIX 2.1) or as <SupplyDate> with <SupplyDateRole> coded ‘08’ (ONIX 3.0), except in exceptional circumstances where no date is known)
  case "33":
		c.Body = `Awaiting reissue`

  // May be for quality or technical reasons. Requires expected availability date, either as <ExpectedShipDate> (ONIX 2.1) or as <SupplyDate> with <SupplyDateRole> coded ‘08’ (ONIX 3.0), except in exceptional circumstances where no date is known
  case "34":
		c.Body = `Temporarily withdrawn from sale`

  // Not available from us (for any reason)
  case "40":
		c.Body = `Not available (reason unspecified)`

  // This product is unavailable, but a successor product or edition is or will be available from us (identify successor in <RelatedProduct>)
  case "41":
		c.Body = `Not available, replaced by new product`

  // This product is unavailable, but the same content is or will be available from us in an alternative format (identify other format product in <RelatedProduct>)
  case "42":
		c.Body = `Not available, other format available`

  // Identify new supplier in <NewSupplier> if possible
  case "43":
		c.Body = `No longer supplied by us`

  // Not available to trade, apply direct to publisher
  case "44":
		c.Body = `Apply direct`

  // Individual copies of the product are not available from the supplier, but packs of copies are available, and individual copies of the product may typically be sold at retail. Must be bought as part of a set or trade pack (identify set or pack in <RelatedProduct> using code 02)
  case "45":
		c.Body = `Not sold separately`

  // May be for legal reasons or to avoid giving offence
  case "46":
		c.Body = `Withdrawn from sale`

  // Remaindered
  case "47":
		c.Body = `Remaindered`

  // Out of print, but a print-on-demand edition is or will be available under a different ISBN. Use only when the POD successor has a different ISBN, normally because different trade terms apply
  case "48":
		c.Body = `Not available, replaced by POD`

  // Recalled for reasons of consumer safety
  case "49":
		c.Body = `Recalled`

  // When a collection that is not sold as a set nevertheless has its own ONIX record
  case "50":
		c.Body = `Not sold as set`

  // This product is unavailable from the supplier, no successor product or alternative format is available or planned. Use this code only when the publisher has indicated the product is out of print
  case "51":
		c.Body = `Not available, publisher indicates OP`

  // This product is unavailable from the supplier in this market, no successor product or alternative format is available or planned. Use this code when a publisher has indicated the product is permanently unavailable (in this market) while remaining available elsewhere
  case "52":
		c.Body = `Not available, publisher no longer sells product in this market`

  // Sender has not received any recent update for this product from the publisher/supplier (for use when the sender is a data aggregator): the definition of ‘recent’ must be specified by the aggregator, or by agreement between parties to an exchange
  case "97":
		c.Body = `No recent update received`

  // Sender is no longer receiving any updates from the publisher/supplier of this product (for use when the sender is a data aggregator)
  case "98":
		c.Body = `No longer receiving updates`

  // Availability not known to sender
  case "99":
		c.Body = `Contact supplier`
	default:
		return fmt.Errorf("undefined code for ProductAvailability has been passed, got [%s]", v)
	}
	return nil
}

// ProductClassificationType Product classification type
type ProductClassificationType struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *ProductClassificationType) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // World Customs Organization Harmonized Commodity Coding and Description System. Use 6 (or occasionally 8 or 10) digits, without punctuation
  case "01":
		c.Body = `WCO Harmonized System`

  // UN Standard Product and Service Classification. Use 8 (or occasionally 10) digits, without punctuation
  case "02":
		c.Body = `UNSPSC`

  // UK Revenue and Customs classifications, based on the Harmonized System (8 or 10 digits, without punctuation, for export and import respectively)
  case "03":
		c.Body = `HMRC`

  // German export trade classification, based on the Harmonised System
  case "04":
		c.Body = `Warenverzeichnis für die Außenhandelsstatistik`

  // EU TARIC codes, an extended version of the Harmonized System. Use 10 digits, without punctuation
  case "05":
		c.Body = `TARIC`

  // Centraal Boekhuis free classification field for publishers
  case "06":
		c.Body = `Fondsgroep`

  // A product category (not a subject classification) assigned by the sender
  case "07":
		c.Body = `Sender’s product category`

  // Product classification maintained by the Chinese General Administration of Press and Publication (http://www.gapp.gov.cn)
  case "08":
		c.Body = `GAPP Product Class`

  // Statistical Classification of Products by Activity in the European Economic Community, see http://ec.europa.eu/eurostat/ramon/nomenclatures/index.cfm?TargetUrl=LST_NOM_DTL&StrNom=CPA_2008. Use 6 digits, without punctuation. For example, printed children’s books are ‘58.11.13’, but the periods are normally ommited in ONIX
  case "09":
		c.Body = `CPA`

  // Mercosur/Mercosul Common Nomenclature, based on the Harmonised System. Use 8 digits, without punctuation
  case "10":
		c.Body = `NCM`

  // Common Procurement Vocabulary, uses to describe requirements for tender for public tendering and procurement within the EU. Code is a nine digit number (including the check digit). See http://eur-lex.europa.eu/legal-content/EN/TXT/?uri=URISERV:l22008
  case "11":
		c.Body = `CPV`

  // Polish Classification of Products and Services (2015). Use a single letter followed by 2 to 7 digits, without punctuation. For use in ONIX 3.0 only
  case "12":
		c.Body = `PKWiU`

  // US HTS (or HTSA) commodity codes for import of goods into USA (10 digits, without punctuation). For use in ONIX 3.0 only. See https://hts.usitc.gov/current
  case "13":
		c.Body = `HTSUS`

  // US Schedule B commodity codes for export from USA (10 digits, without punctuation). For use in ONIX 3.0 only. See http://uscensus.prod.3ceonline.com
  case "14":
		c.Body = `US Schedule B`

  // Typologie de marché géré par Electre (Market segment code maintained by Electre)
  case "50":
		c.Body = `Electre genre`
	default:
		return fmt.Errorf("undefined code for ProductClassificationType has been passed, got [%s]", v)
	}
	return nil
}

// ProductComposition Product composition
type ProductComposition struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *ProductComposition) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // Single-component retail product
  case "00":
		c.Body = `Single-component retail product`

  // Multiple-component product retailed as a whole
  case "10":
		c.Body = `Multiple-component retail product`

  // Used only when an ONIX record is required for a collection-as-a-whole, even though it is not currently retailed as such
  case "11":
		c.Body = `Multiple-item collection, retailed as separate parts`

  // Product available to the book trade, but not for retail sale, and not carrying retail items, eg empty dumpbin, empty counterpack, promotional material
  case "20":
		c.Body = `Trade-only product`

  // Product available to the book trade, but not for general retail sale as a whole. It carries multiple components for retailing as separate items, eg shrink-wrapped trade pack, filled dumpbin, filled counterpack
  case "30":
		c.Body = `Multiple-item trade-only pack`

  // Carrying multiple components, primarily for retailing as separate items. The pack may be split and retailed as separate items OR retailed as a single item. Use instead of Multiple-item trade-only pack (code 30) if the data provider specifically wishes to make explicit that the pack may optionally be retailed as a whole
  case "31":
		c.Body = `Multiple-item pack`
	default:
		return fmt.Errorf("undefined code for ProductComposition has been passed, got [%s]", v)
	}
	return nil
}

// ProductContactIDType Name identifier type
type ProductContactIDType struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *ProductContactIDType) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // Note that <IDTypeName> is required with proprietary identifiers
  case "01":
		c.Body = `Proprietary`

  // DEPRECATED – use 01
  case "02":
		c.Body = `Proprietary`

  // Deutsche Nationalbibliothek publisher identifier
  case "03":
		c.Body = `DNB publisher identifier`

  // Börsenverein Verkehrsnummer
  case "04":
		c.Body = `Börsenverein Verkehrsnummer`

  // German ISBN Agency publisher identifier
  case "05":
		c.Body = `German ISBN Agency publisher identifier`

  // GS1 global location number (formerly EAN location number)
  case "06":
		c.Body = `GLN`

  // Book trade Standard Address Number – US, UK etc
  case "07":
		c.Body = `SAN`

  // MARC code list for organizations – see http://www.loc.gov/marc/organizations/orgshome.html
  case "08":
		c.Body = `MARC organization code`

  // Trading party identifier used in the Netherlands
  case "10":
		c.Body = `Centraal Boekhuis Relatie ID`

  // Flemish publisher code
  case "13":
		c.Body = `Fondscode Boekenbank`

  // Business Identity Code (Finland). See http://www.ytj.fi/ (in Finnish)
  case "15":
		c.Body = `Y-tunnus`

  // International Standard Name Identifier. A sixteen digit number. Usually presented with spaces or hyphens dividing the number into four groups of four digits, but in ONIX the spaces or hyphens should be omitted. See http://www.isni.org/
  case "16":
		c.Body = `ISNI`

  // Personennamendatei – person name authority file used by Deutsche Nationalbibliothek and in other German-speaking countries. See http://www.dnb.de/standardisierung/normdateien/pnd.htm (German) or http://www.dnb.de/eng/standardisierung/normdateien/pnd.htm (English). DEPRECATED in favor of the GND
  case "17":
		c.Body = `PND`

  // A control number assigned to a Library of Congress Control Number (LCCN) Name Authority / NACO record
  case "18":
		c.Body = `NACO`

  // Publisher identifier administered by Japanese ISBN Agency
  case "19":
		c.Body = `Japanese Publisher identifier`

  // Gemeinsame Körperschaftsdatei – Corporate Body Authority File in the German-speaking countries. See http://www.dnb.de/standardisierung/normdateien/gkd.htm (German) or http://www.dnb.de/eng/standardisierung/normdateien/gkd.htm (English). DEPRECATED in favor of the GND
  case "20":
		c.Body = `GKD`

  // Open Researcher and Contributor ID. A sixteen digit number. Usually presented with hyphens dividing the number into four groups of four digits, but in ONIX the hyphens should be omitted. See http://www.orcid.org/
  case "21":
		c.Body = `ORCID`

  // Publisher identifier maintained by the Chinese ISBN Agency (GAPP)
  case "22":
		c.Body = `GAPP Publisher Identifier`

  // Identifier for a business organization for VAT purposes, eg within the EU’s VIES system. See http://ec.europa.eu/taxation_customs/vies/faqvies.do for EU VAT ID formats, which vary from country to country. Generally these consist of a two-letter country code followed by the 8–12 digits of the national VAT ID. Some countries include one or two letters within their VAT ID. See http://en.wikipedia.org/wiki/VAT_identification_number for non-EU countries that maintain similar identifiers. Spaces, dashes etc should be omitted
  case "23":
		c.Body = `VAT Identity Number`

  // 4-digit business organization identifier controlled by the Japanese Publication Wholesalers Association
  case "24":
		c.Body = `JP Distribution Identifier`

  // Gemeinsame Normdatei – Joint Authority File in the German-speaking countries. See http://www.dnb.de/EN/gnd (English). Combines the PND, SWD and GKD into a single authority file, and should be used in preference
  case "25":
		c.Body = `GND`

  // Dunn and Bradstreet Universal Numbering System, see http://www.dnb.co.uk/dandb-duns-number
  case "26":
		c.Body = `DUNS`

  // Ringgold organizational identifier, see http://www.ringgold.com/identify.html
  case "27":
		c.Body = `Ringgold ID`

  // French Electre publisher identifier
  case "28":
		c.Body = `Identifiant Editeur Electre`

  // DOI used in EIDR party registry, for example ‘10.5237/C9F6-F41F’ (Sam Raimi). See http://eidr.org
  case "29":
		c.Body = `EIDR Party DOI`

  // French Electre imprint Identifier
  case "30":
		c.Body = `Identifiant Marque Electre`

  // Virtual Internet Authority File. <IDValue> should be a number. The URI form of the identifier can be created by prefixing the number with ‘https://viaf.org/viaf/’. See https://viaf.org
  case "31":
		c.Body = `VIAF ID`

  // DOI used in CrossRef’s Open Funder Registry list of academic research funding bodies, for example ‘10.13039/100004440’ (Wellcome Trust). See http://www.crossref.org/fundingdata/registry.html
  case "32":
		c.Body = `FundRef DOI`

  // Control number assigned to a Name Authority record by the Biblioteca Nacional de España
  case "33":
		c.Body = `BNE CN`

  // Numéro de la notice de personne BNF
  case "34":
		c.Body = `BNF Control Number`

  // Archival Resource Key, as a URL (including the address of the ARK resolver provided by eg a national library)
  case "35":
		c.Body = `ARK`

  // Nasjonalt autoritetsregister for navn – Norwegian national authority file for personal and corporate names. For use in ONIX 3.0 only
  case "36":
		c.Body = `Nasjonalt autoritetsregister`

  // Global Research Identifier Database ID (see https://www.grid.ac). For use in ONIX 3.0 only
  case "37":
		c.Body = `GRID`

  // Party ID from Identifiers and Standards for Higher Education and Research (fr: Identifiants et Référentiels pour l’enseignement supérieur et la recherche). For use on ONIX 3.0 only. See https://www.idref.fr
  case "38":
		c.Body = `IDRef`
	default:
		return fmt.Errorf("undefined code for ProductContactIDType has been passed, got [%s]", v)
	}
	return nil
}

// ProductContactRole Product contact role
type ProductContactRole struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *ProductContactRole) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // For queries and feedback concerning the metadata record itself
  case "00":
		c.Body = `Metadata contact`

  // Eg for requests for supply of mutable digital files for conversion to other formats
  case "01":
		c.Body = `Accessibility request contact`

  // Eg for requests relating to interviews, author events
  case "02":
		c.Body = `Promotional contact`

  // Eg for co-op advertising
  case "03":
		c.Body = `Advertising contact`

  // Eg for requests for review copies
  case "04":
		c.Body = `Review copy contact`

  // Eg for requests for approval or evaluation copies (particularly within education)
  case "05":
		c.Body = `Evaluation copy contact`

  // Eg for requests to reproduce or repurpose parts of the publication
  case "06":
		c.Body = `Permissions contact`

  // Eg for use where authorisation must be gained from the publisher rather than the distributor or wholesaler
  case "07":
		c.Body = `Return authorisation contact`

  // CIP / Legal deposit contact
  case "08":
		c.Body = `CIP / Legal deposit contact`
	default:
		return fmt.Errorf("undefined code for ProductContactRole has been passed, got [%s]", v)
	}
	return nil
}

// ProductContentType Product content type
type ProductContentType struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *ProductContentType) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // Readable text of the main work: this value is required, together with applicable <ProductForm> and <ProductFormDetail> values, to designate an e-book or other digital or physical product whose primary content is eye-readable text
  case "10":
		c.Body = `Text (eye-readable)`

  // E-publication contains a significant number of actionable cross-references, hyperlinked notes and annotations, or with other actionable links between largely textual elements (eg quiz/test questions, ‘choose your own ending’ etc)
  case "15":
		c.Body = `Extensive links between internal content`

  // E-publication contains a significant number of actionable (clickable) web links
  case "14":
		c.Body = `Extensive links to external content`

  // Publication contains additional textual content such as interview, feature article, essay, bibliography, quiz/test, other background material or text that is not included in a primary or ‘unenhanced’ version
  case "16":
		c.Body = `Additional eye-readable text not part of main work`

  // Publication contains a significant number of web links (printed URLs, QR codes etc). For use in ONIX 3.0 only
  case "41":
		c.Body = `Additional eye-readable links to external content`

  // eg Teaser chapter
  case "17":
		c.Body = `Promotional text for other book product`

  // Musical notation
  case "11":
		c.Body = `Musical notation`

  // Use only when no more detailed specification is provided
  case "07":
		c.Body = `Still images / graphics`

  // Whether in a plate section / insert, or not
  case "18":
		c.Body = `Photographs`

  // Including other ‘mechanical’ (ie non-photographic) illustrations
  case "19":
		c.Body = `Figures, diagrams, charts, graphs`

  // Publication is enhanced with additional images or graphical content such as supplementary photographs that are not included in a primary or ‘unenhanced’ version
  case "20":
		c.Body = `Additional images / graphics not part of main work`

  // Maps and/or other cartographic content
  case "12":
		c.Body = `Maps and/or other cartographic content`

  // eg Questions or student exercises, problems, quizzes or tests (as an integral part of the work). For use in ONIX 3.0 only
  case "42":
		c.Body = `Assessment material`

  // Audio recording of a reading of a book or other text
  case "01":
		c.Body = `Audiobook`

  // Audio recording of a drama or other spoken word performance
  case "02":
		c.Body = `Performance – spoken word`

  // eg an interview, speech, lecture or discussion, not a ‘reading’ or ‘performance’)
  case "13":
		c.Body = `Other speech content`

  // Audio recording of a music performance, including musical drama and opera
  case "03":
		c.Body = `Music recording`

  // Audio recording of other sound, eg birdsong
  case "04":
		c.Body = `Other audio`

  // Audio recording of a reading, performance or dramatization of part of the work
  case "21":
		c.Body = `Partial performance – spoken word`

  // Product is enhanced with audio recording of full or partial reading, performance, dramatization, interview, background documentary or other audio content not included in the primary or ‘unenhanced’ version
  case "22":
		c.Body = `Additional audio content not part of main work`

  // eg Reading of teaser chapter
  case "23":
		c.Body = `Promotional audio for other book product`

  // Includes Film, video, animation etc. Use only when no more detailed specification is provided. Formerly ‘Moving images’
  case "06":
		c.Body = `Video`

  // Video recording of a reading
  case "26":
		c.Body = `Video recording of a reading`

  // Video recording of a drama or other performance, including musical performance
  case "27":
		c.Body = `Performance – visual`

  // eg animated diagrams, charts, graphs or other illustrations
  case "24":
		c.Body = `Animated / interactive illustrations`

  // eg cartoon, animatic or CGI animation
  case "25":
		c.Body = `Narrative animation`

  // Other video content eg interview, not a reading or performance
  case "28":
		c.Body = `Other video`

  // Video recording of a reading, performance or dramatization of part of the work
  case "29":
		c.Body = `Partial performance – video`

  // E-publication is enhanced with video recording of full or partial reading, performance, dramatization, interview, background documentary or other content not included in the primary or ‘unenhanced’ version
  case "30":
		c.Body = `Additional video content not part of main work`

  // eg Book trailer
  case "31":
		c.Body = `Promotional video for other book product`

  // No multi-user functionality. Formerly just ‘Game’
  case "05":
		c.Body = `Game / Puzzle`

  // Includes some degree of multi-user functionality
  case "32":
		c.Body = `Contest`

  // Largely ‘content free’
  case "08":
		c.Body = `Software`

  // Data files
  case "09":
		c.Body = `Data`

  // Data set plus software
  case "33":
		c.Body = `Data set plus software`

  // Entire pages or blank spaces, forms, boxes etc, intended to be filled in by the reader
  case "34":
		c.Body = `Blank pages or spaces`

  // Use only where type of advertising content is not stated
  case "35":
		c.Body = `Advertising content`

  // ‘Back ads’ – promotional pages for other books (that do not include sample content, cf codes 17, 23)
  case "37":
		c.Body = `Advertising – first party`

  // Eg to obtain discounts on other products
  case "36":
		c.Body = `Advertising – coupons`

  // Advertising – third party display
  case "38":
		c.Body = `Advertising – third party display`

  // Advertising – third party textual
  case "39":
		c.Body = `Advertising – third party textual`

  // E-publication contains microprograms written (eg) in Javascript and executed within the reading system. For use in ONIX 3.0 only
  case "40":
		c.Body = `Scripting`

  // E-publication contains pop-ups or other functionality offering (eg) term definitions, cross-links or glossary entries [Note this should not include (eg) dictionary funcionality that is part of the reading system.] For use in ONIX 3.0 only
  case "43":
		c.Body = `Scripted pop-ups`
	default:
		return fmt.Errorf("undefined code for ProductContentType has been passed, got [%s]", v)
	}
	return nil
}

// ProductForm Product form
type ProductForm struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *ProductForm) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // Undefined
  case "00":
		c.Body = `Undefined`

  // Audio recording – detail unspecified
  case "AA":
		c.Body = `Audio`

  // Audio cassette (analogue)
  case "AB":
		c.Body = `Audio cassette`

  // Audio compact disc: use for ‘Red book’ discs (conventional audio CD) and SACD, and use coding in <ProductFormDetail> to specify the format, if required
  case "AC":
		c.Body = `CD-Audio`

  // Digital audio tape cassette
  case "AD":
		c.Body = `DAT`

  // Audio disc (excluding CD-Audio): use for ‘Yellow book’ (CD-Rom-style) discs, including for example mp3 CDs, and use coding in <ProductFormDetail> to specify the format of the data on the disc
  case "AE":
		c.Body = `Audio disc`

  // Audio tape (analogue open reel tape)
  case "AF":
		c.Body = `Audio tape`

  // Sony MiniDisc format
  case "AG":
		c.Body = `MiniDisc`

  // Audio compact disc with part CD-ROM content, also termed CD-Plus or Enhanced-CD: use for ‘Blue book’ and ‘Yellow/Red book’ two-session discs
  case "AH":
		c.Body = `CD-Extra`

  // DVD Audio
  case "AI":
		c.Body = `DVD Audio`

  // Digital audio recording downloadable to the purchaser’s own device(s)
  case "AJ":
		c.Body = `Downloadable audio file`

  // For example, Playaway audiobook and player: use coding in <ProductFormDetail> to specify the recording format, if required
  case "AK":
		c.Body = `Pre-recorded digital audio player`

  // For example, Audiofy audiobook chip
  case "AL":
		c.Body = `Pre-recorded SD card`

  // Vinyl disc (analogue).
  case "AM":
		c.Body = `LP`

  // Digital audio recording available both by download to the purchaser’s own device(s) and by online (eg streamed) access
  case "AN":
		c.Body = `Downloadable and online audio file`

  // Digital audio recording available online (eg streamed), not downloadable to the purchaser’s own device(s)
  case "AO":
		c.Body = `Online audio file`

  // Other audio format not specified by AB to AM. Further detail is expected in <ProductFormDescription>, as <ProductFormDetail> and <ProductFormFeature> are unlikely to be sufficient
  case "AZ":
		c.Body = `Other audio format`

  // Book – detail unspecified
  case "BA":
		c.Body = `Book`

  // Hardback or cased book
  case "BB":
		c.Body = `Hardback`

  // Paperback or other softback book
  case "BC":
		c.Body = `Paperback / softback`

  // Loose-leaf book
  case "BD":
		c.Body = `Loose-leaf`

  // Spiral, comb or coil bound book
  case "BE":
		c.Body = `Spiral bound`

  // Pamphlet, stapled; use for German ‘geheftet’. Includes low-extent wire-stitched books bound without a distinct spine (eg many comic books)
  case "BF":
		c.Body = `Pamphlet`

  // Use <ProductFormDetail> to provide additional description
  case "BG":
		c.Body = `Leather / fine binding`

  // Child’s book with all pages printed on board
  case "BH":
		c.Body = `Board book`

  // Child’s book with all pages printed on textile
  case "BI":
		c.Body = `Rag book`

  // Child’s book printed on waterproof material
  case "BJ":
		c.Body = `Bath book`

  // A book whose novelty consists wholly or partly in a format which cannot be described by any other available code – a ‘conventional’ format code is always to be preferred; one or more Product Form Detail codes, eg from the B2nn group, should be used whenever possible to provide additional description
  case "BK":
		c.Body = `Novelty book`

  // Slide bound book
  case "BL":
		c.Body = `Slide bound`

  // Extra-large format for teaching etc; this format and terminology may be specifically UK; required as a top-level differentiator
  case "BM":
		c.Body = `Big book`

  // A part-work issued with its own ISBN and intended to be collected and bound into a complete book.
  case "BN":
		c.Body = `Part-work (fascículo)`

  // Concertina-folded booklet or chart, designed to fold to pocket or regular page size, and usually bound within distinct board or card covers: use for German ‘Leporello’
  case "BO":
		c.Body = `Fold-out book or chart`

  // A children’s book whose cover and pages are made of foam
  case "BP":
		c.Body = `Foam book`

  // Other book format or binding not specified by BB to BP. Further detail is expected in <ProductFormDescription>, as <ProductFormDetail> and <ProductFormFeature> are unlikely to be sufficient
  case "BZ":
		c.Body = `Other book format`

  // Sheet map – detail unspecified
  case "CA":
		c.Body = `Sheet map`

  // Sheet map, folded
  case "CB":
		c.Body = `Sheet map, folded`

  // Sheet map, flat
  case "CC":
		c.Body = `Sheet map, flat`

  // See <ProductPackaging> and Codelist 80 for ‘rolled in tube’
  case "CD":
		c.Body = `Sheet map, rolled`

  // Globe or planisphere
  case "CE":
		c.Body = `Globe`

  // Other cartographic format not specified by CB to CE. Further detail is expected in <ProductFormDescription>, as <ProductFormDetail> and <ProductFormFeature> are unlikely to be sufficient
  case "CZ":
		c.Body = `Other cartographic`

  // Digital content delivered on a physical carrier (detail unspecified)
  case "DA":
		c.Body = `Digital (on physical carrier)`

  // CD-ROM
  case "DB":
		c.Body = `CD-ROM`

  // CD interactive: use for ‘Green book’ discs
  case "DC":
		c.Body = `CD-I`

  // Game cartridge
  case "DE":
		c.Body = `Game cartridge`

  // AKA ‘floppy disc’
  case "DF":
		c.Body = `Diskette`

  // DVD-ROM
  case "DI":
		c.Body = `DVD-ROM`

  // Secure Digital (SD) Memory Card
  case "DJ":
		c.Body = `Secure Digital (SD) Memory Card`

  // Compact Flash Memory Card
  case "DK":
		c.Body = `Compact Flash Memory Card`

  // Memory Stick Memory Card
  case "DL":
		c.Body = `Memory Stick Memory Card`

  // USB Flash Drive
  case "DM":
		c.Body = `USB Flash Drive`

  // Double-sided disc, one side Audio CD/CD-ROM, other side DVD
  case "DN":
		c.Body = `Double-sided CD/DVD`

  // (Blu Ray ROM)
  case "DO":
		c.Body = `BR-ROM`

  // Other carrier of digital content not specified by DB to DO. Further detail is expected in <ProductFormDescription>, as <ProductFormDetail> and <ProductFormFeature> are unlikely to be sufficient
  case "DZ":
		c.Body = `Other digital carrier`

  // Digital content delivered electronically (delivery method unspecified)
  case "EA":
		c.Body = `Digital (delivered electronically)`

  // Digital content available both by download and by online access
  case "EB":
		c.Body = `Digital download and online`

  // Digital content accessed online only
  case "EC":
		c.Body = `Digital online`

  // Digital content delivered by download only
  case "ED":
		c.Body = `Digital download`

  // Film or transparency – detail unspecified
  case "FA":
		c.Body = `Film or transparency`

  // Photographic transparencies mounted for projection
  case "FC":
		c.Body = `Slides`

  // Transparencies for overhead projector
  case "FD":
		c.Body = `OHP transparencies`

  // Photographic transparencies, unmounted but cut into short multi-frame strips
  case "FE":
		c.Body = `Filmstrip`

  // Continuous movie film as opposed to filmstrip
  case "FF":
		c.Body = `Film`

  // Other film or transparency format not specified by FB to FF. Further detail is expected in <ProductFormDescription>, as <ProductFormDetail> and <ProductFormFeature> are unlikely to be sufficient
  case "FZ":
		c.Body = `Other film or transparency format`

  // Digital product license (delivery method not encoded)
  case "LA":
		c.Body = `Digital product license`

  // Digital product license delivered through the retail supply chain as a physical ‘key’, typically a card or booklet containing a code enabling the purchaser to download the associated product
  case "LB":
		c.Body = `Digital product license key`

  // Digital product license delivered by email or other electronic distribution, typically providing a code enabling the purchaser to activate, upgrade or extend the license supplied with the associated product
  case "LC":
		c.Body = `Digital product license code`

  // Microform – detail unspecified
  case "MA":
		c.Body = `Microform`

  // Microfiche
  case "MB":
		c.Body = `Microfiche`

  // Roll microfilm
  case "MC":
		c.Body = `Microfilm`

  // Other microform not specified by MB or MC. Further detail is expected in <ProductFormDescription>, as <ProductFormDetail> and <ProductFormFeature> are unlikely to be sufficient
  case "MZ":
		c.Body = `Other microform`

  // Miscellaneous printed material – detail unspecified
  case "PA":
		c.Body = `Miscellaneous print`

  // May use <ProductFormDetail> codes P201 to P204 to specify binding
  case "PB":
		c.Body = `Address book`

  // Calendar
  case "PC":
		c.Body = `Calendar`

  // Cards, flash cards (eg for teaching reading), revision cards, divination, playing or trading cards
  case "PD":
		c.Body = `Cards`

  // Copymasters, photocopiable sheets
  case "PE":
		c.Body = `Copymasters`

  // May use <ProductFormDetail> codes P201 to P204 to specify binding
  case "PF":
		c.Body = `Diary or journal`

  // Narrow strip-shaped printed sheet used mostly for education or children’s products (eg depicting alphabet, number line, procession of illustrated characters etc). Usually intended for horizontal display
  case "PG":
		c.Body = `Frieze`

  // Parts for post-purchase assembly, including card, wood or plastic parts or model components, interlocking construction blocks, beads and other crafting materials etc
  case "PH":
		c.Body = `Kit`

  // May use <ProductFormDetail> codes P201 to P204 to specify binding
  case "PI":
		c.Body = `Sheet music`

  // Including greeting cards and packs. For bound books (usually with perforated sheets to remove cards), may use <ProductFormDetail> codes P201 to P204 to specify binding
  case "PJ":
		c.Body = `Postcard book or pack`

  // Poster for retail sale – see also XF
  case "PK":
		c.Body = `Poster`

  // Record book (eg ‘birthday book’, ‘baby book’): binding unspecified; may use <ProductFormDetail> codes P201 to P204 to specify binding
  case "PL":
		c.Body = `Record book`

  // Wallet, folder or box (containing loose sheets etc, or empty): it is preferable to code the contents and treat ‘wallet’ (or folder / box) as packaging in <ProductPackaging> with Codelist 80, but if this is not possible (eg where the product is empty and intended for storing other loose items) the product as a whole may be coded as a ‘wallet’. For binders intended for loose leaf or partwork publications intended to be updateable, see codes BD, BN
  case "PM":
		c.Body = `Wallet or folder`

  // Pictures or photographs
  case "PN":
		c.Body = `Pictures or photographs`

  // Wallchart
  case "PO":
		c.Body = `Wallchart`

  // Stickers
  case "PP":
		c.Body = `Stickers`

  // A book-sized (as opposed to poster-sized) sheet, usually in color or high quality print
  case "PQ":
		c.Body = `Plate (lámina)`

  // A book with all pages blank for the buyer’s own use; may use <ProductFormDetail> codes P201 to P204 to specify binding
  case "PR":
		c.Body = `Notebook / blank book`

  // May use <ProductFormDetail> codes P201 to P204 to specify binding
  case "PS":
		c.Body = `Organizer`

  // Bookmark
  case "PT":
		c.Body = `Bookmark`

  // Folded but unbound
  case "PU":
		c.Body = `Leaflet`

  // Ex libris’ book labels and packs
  case "PV":
		c.Body = `Book plates`

  // Other printed item not specified by PB to PQ. Further detail is expected in <ProductFormDescription>, as <ProductFormDetail> and <ProductFormFeature> are unlikely to be sufficient
  case "PZ":
		c.Body = `Other printed item`

  // Presentation unspecified: format of product components must be given in <ProductPart>
  case "SA":
		c.Body = `Multiple-component retail product`

  // Format of product components must be given in <ProductPart>
  case "SB":
		c.Body = `Multiple-component retail product, boxed`

  // Format of product components must be given in <ProductPart>
  case "SC":
		c.Body = `Multiple-component retail product, slip-cased`

  // Format of product components must be given in <ProductPart>. Use code XL for a shrink-wrapped pack for trade supply, where the retail items it contains are intended for sale individually
  case "SD":
		c.Body = `Multiple-component retail product, shrink-wrapped`

  // Format of product components must be given in <ProductPart>
  case "SE":
		c.Body = `Multiple-component retail product, loose`

  // Multiple component product where subsidiary product part(s) is/are supplied as enclosures to the primary part, eg a book with a CD packaged in a sleeve glued within the back cover. Format of product components must be given in <ProductPart>
  case "SF":
		c.Body = `Multiple-component retail product, part(s) enclosed`

  // Video – detail unspecified
  case "VA":
		c.Body = `Video`

  // eg Laserdisc
  case "VF":
		c.Body = `Videodisc`

  // DVD video: specify TV standard in <ProductFormDetail>
  case "VI":
		c.Body = `DVD video`

  // VHS videotape: specify TV standard in <ProductFormDetail>
  case "VJ":
		c.Body = `VHS video`

  // Betamax videotape: specify TV standard in <ProductFormDetail>
  case "VK":
		c.Body = `Betamax video`

  // VideoCD
  case "VL":
		c.Body = `VCD`

  // Super VideoCD
  case "VM":
		c.Body = `SVCD`

  // High definition DVD disc, Toshiba HD DVD format
  case "VN":
		c.Body = `HD DVD`

  // High definition DVD disc, Sony Blu-ray format
  case "VO":
		c.Body = `Blu-ray`

  // Sony Universal Media disc
  case "VP":
		c.Body = `UMD Video`

  // China Blue High-Definition, derivative of HD-DVD
  case "VQ":
		c.Body = `CBHD`

  // Other video format not specified by VB to VQ. Further detail is expected in <ProductFormDescription>, as <ProductFormDetail> and <ProductFormFeature> are unlikely to be sufficient
  case "VZ":
		c.Body = `Other video format`

  // Trade-only material (unspecified)
  case "XA":
		c.Body = `Trade-only material`

  // Dumpbin – empty
  case "XB":
		c.Body = `Dumpbin – empty`

  // Dumpbin with contents. ISBN (where applicable) and format of contained items must be given in <ProductPart>
  case "XC":
		c.Body = `Dumpbin – filled`

  // Counterpack – empty
  case "XD":
		c.Body = `Counterpack – empty`

  // Counterpack with contents. ISBN (where applicable) and format of contained items must be given in <ProductPart>
  case "XE":
		c.Body = `Counterpack – filled`

  // Promotional poster for display, not for sale – see also PK
  case "XF":
		c.Body = `Poster, promotional`

  // Shelf strip
  case "XG":
		c.Body = `Shelf strip`

  // Promotional piece for shop window display
  case "XH":
		c.Body = `Window piece`

  // Streamer
  case "XI":
		c.Body = `Streamer`

  // Spinner – empty
  case "XJ":
		c.Body = `Spinner – empty`

  // Large scale facsimile of book for promotional display
  case "XK":
		c.Body = `Large book display`

  // A quantity pack with its own product code, usually for trade supply only: the retail items it contains are intended for sale individually. ISBN (where applicable) and format of contained items must be given in <ProductPart>. For products or product bundles supplied individually shrink-wrapped for retail sale, use code SD
  case "XL":
		c.Body = `Shrink-wrapped pack`

  // A quantity pack with its own product code, usually for trade supply only: the retail items it contains are intended for sale individually. ISBN (where applicable) and format of contained items must be given in <ProductPart>. For products or product bundles boxed individually for retail sale, use code SB
  case "XM":
		c.Body = `Boxed pack`

  // A quantity pack with its own product code, usually for trade supply only: the retail items it contains are intended for sale individually. ISBN (where applicable) and format of contained items must be given in <ProductPart>. Use only when the pack is neither shrinp-wrapped nor boxed
  case "XN":
		c.Body = `Pack (outer packaging unspecified)`

  // Spinner with contents. ISBN(s) (where applicable) and detail of contained items must be given in <ProductPart>
  case "XO":
		c.Body = `Spinner – filled`

  // Other point of sale material not specified by XB to XO, supplied with product(s) for retail sale. The retail product(s) must be described in <ProductPart>. Further detail of the POS material is expected in <ProductFormDescription>, as <ProductFormDetail> and <ProductFormFeature> are unlikely to be sufficient
  case "XY":
		c.Body = `Other point of sale – including retail product`

  // Other point of sale material not specified by XB to XN. Further detail is expected in <ProductFormDescription>, as <ProductFormDetail> and <ProductFormFeature> are unlikely to be sufficient
  case "XZ":
		c.Body = `Other point of sale`

  // General merchandise – unspecified
  case "ZA":
		c.Body = `General merchandise`

  // Including action figures, figurines
  case "ZB":
		c.Body = `Doll or figure`

  // Soft or plush toy
  case "ZC":
		c.Body = `Soft toy`

  // Including educational toys (where no other code is relevant)
  case "ZD":
		c.Body = `Toy`

  // Board game, or other game (except computer game: see DE and other D* codes)
  case "ZE":
		c.Body = `Game`

  // T-shirt
  case "ZF":
		c.Body = `T-shirt`

  // Dedicated e-book reading device, typically with mono screen
  case "ZG":
		c.Body = `E-book reader`

  // General purpose tablet computer, typically with color screen
  case "ZH":
		c.Body = `Tablet computer`

  // Dedicated audiobook player device, typically including book-related features like bookmarking
  case "ZI":
		c.Body = `Audiobook player`

  // Jigsaw or similar ‘shapes’ puzzle
  case "ZJ":
		c.Body = `Jigsaw`

  // For example, branded, promotional or tie-in drinking mug, cup etc
  case "ZK":
		c.Body = `Mug`

  // For example, branded, promotional or tie-in bag
  case "ZL":
		c.Body = `Tote bag`

  // For example, branded, promotional or tie-in plates, bowls etc (note for mugs and cups, use code ZK)
  case "ZM":
		c.Body = `Tableware`

  // For example, branded, promotional or tie-in umbrella
  case "ZN":
		c.Body = `Umbrella`

  // Coloring set, including pens, chalks, etc
  case "ZO":
		c.Body = `Paints, crayons, pencils`

  // Other toy, game and puzzle items not specified by ZB to ZN, generally accessories to other products etc. Further detail is expected in <ProductFormDescription>, as <ProductFormDetail> and <ProductFormFeature> are unlikely to be sufficient
  case "ZX":
		c.Body = `Other toy/game accessories`

  // Other apparel items not specified by ZB to ZN, including branded, promotional or tie-in scarves, caps, aprons, dress-up costumes etc. Further detail is expected in <ProductFormDescription>, as <ProductFormDetail> and <ProductFormFeature> are unlikely to be sufficient
  case "ZY":
		c.Body = `Other apparel`

  // Other merchandise not specified by ZB to ZY. Further detail is expected in <ProductFormDescription>, as <ProductFormDetail> and <ProductFormFeature> are unlikely to be sufficient
  case "ZZ":
		c.Body = `Other merchandise`
	default:
		return fmt.Errorf("undefined code for ProductForm has been passed, got [%s]", v)
	}
	return nil
}

// ProductFormDetail Product form detail
type ProductFormDetail struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *ProductFormDetail) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // CD ‘red book’ format
  case "A101":
		c.Body = `CD standard audio format`

  // SACD super audio format
  case "A102":
		c.Body = `SACD super audio format`

  // MPEG-1/2 Audio Layer III file
  case "A103":
		c.Body = `MP3 format`

  // WAV format
  case "A104":
		c.Body = `WAV format`

  // Real Audio format
  case "A105":
		c.Body = `Real Audio format`

  // Windows Media Audio format
  case "A106":
		c.Body = `WMA`

  // Advanced Audio Coding format
  case "A107":
		c.Body = `AAC`

  // Vorbis audio format in the Ogg container
  case "A108":
		c.Body = `Ogg/Vorbis`

  // Audio format proprietary to Audible.com
  case "A109":
		c.Body = `Audible`

  // Free lossless audio codec
  case "A110":
		c.Body = `FLAC`

  // Audio Interchangeable File Format
  case "A111":
		c.Body = `AIFF`

  // Apple Lossless Audio Codec
  case "A112":
		c.Body = `ALAC`

  // Audiobook package format
  case "A113":
		c.Body = `W3C Audiobook format`

  // Deprecated, as does not meet DAISY 2 standard. Use conventional audiobook codes instead
  case "A201":
		c.Body = `DAISY 2: full audio with title only (no navigation)`

  // DAISY 2: full audio with navigation (no text)
  case "A202":
		c.Body = `DAISY 2: full audio with navigation (no text)`

  // DAISY 2: full audio with navigation and partial text
  case "A203":
		c.Body = `DAISY 2: full audio with navigation and partial text`

  // DAISY 2: full audio with navigation and full text
  case "A204":
		c.Body = `DAISY 2: full audio with navigation and full text`

  // Reading systems may provide full audio via text-to-speech
  case "A205":
		c.Body = `DAISY 2: full text with navigation and partial audio`

  // Reading systems may provide full audio via text-to-speech
  case "A206":
		c.Body = `DAISY 2: full text with navigation and no audio`

  // Deprecated, as does not meet DAISY 3 standard. Use conventional audiobook codes instead
  case "A207":
		c.Body = `DAISY 3: full audio with title only (no navigation)`

  // DAISY 3: full audio with navigation (no text)
  case "A208":
		c.Body = `DAISY 3: full audio with navigation (no text)`

  // DAISY 3: full audio with navigation and partial text
  case "A209":
		c.Body = `DAISY 3: full audio with navigation and partial text`

  // DAISY 3: full audio with navigation and full text
  case "A210":
		c.Body = `DAISY 3: full audio with navigation and full text`

  // Reading systems may provide full audio via text-to-speech
  case "A211":
		c.Body = `DAISY 3: full text with navigation and partial audio`

  // Reading systems may provide full audio via text-to-speech
  case "A212":
		c.Body = `DAISY 3: full text with navigation and no audio`

  // Standalone audio
  case "A301":
		c.Body = `Standalone audio`

  // Audio intended exclusively for use alongside a printed copy of the book. Most often a children’s product. Normally contains instructions such as ‘turn the page now’ and other references to the printed item, and is usually sold packaged together with a printed copy
  case "A302":
		c.Body = `Readalong audio`

  // Audio intended for musical accompaniment, eg ‘Music minus one’, etc, often used for music learning. Includes singalong backing audio for musical learning or for Karaoke-style entertainment
  case "A303":
		c.Body = `Playalong audio`

  // Audio intended for language learning, which includes speech plus gaps intended to be filled by the listener
  case "A304":
		c.Body = `Speakalong audio`

  // Audio synchronised to text within an e-publication, for example an EPUB3 with audio overlay. Synchronisation at least at paragraph level, and covering the full content
  case "A305":
		c.Body = `Synchronised audio`

  // Incidental sounds added to the audiobook narration (eg background environmental sounds)
  case "A310":
		c.Body = `Sound effects`

  // Incidental music added to the audiobook narration (eg to heighten atmosphere). Do not use where the music is a primary part of the audio
  case "A311":
		c.Body = `Background music`

  // Includes ‘stereo’ where channels are identical
  case "A410":
		c.Body = `Mono`

  // Includes ‘joint stereo’
  case "A420":
		c.Body = `Stereo`

  // Stereo plus low-frequency channel
  case "A421":
		c.Body = `Stereo 2.1`

  // Five-channel audio (including low-frequency channel)
  case "A441":
		c.Body = `Surround 4.1`

  // Six-channel audio (including low-frequency channel)
  case "A451":
		c.Body = `Surround 5.1`

  // In North America, a category of paperback characterized partly by page size (typically from 6¾ up to 7⅛ x 4¼ inches) and partly by target market and terms of trade. Use with Product Form code BC
  case "B101":
		c.Body = `Mass market (rack) paperback`

  // In North America, a category of paperback characterized partly by page size (larger than rack-sized) and partly by target market and terms of trade. AKA ‘quality paperback’, and including textbooks. Most paperback books sold in North America except ‘mass-market’ (B101) and ‘tall rack’ (B107) are correctly described with this code. Use with Product Form code BC
  case "B102":
		c.Body = `Trade paperback (US)`

  // In North America, a category of paperback characterized by page size (typically 7 x 5 inches) and generally used for children’s books; use with Product Form code BC. Note: was wrongly shown as B102 (duplicate entry) in Issue 3
  case "B103":
		c.Body = `Digest format paperback`

  // In UK and IE, a category of paperback characterized by page size (normally 178 x 111 mm approx); use with Product Form code BC
  case "B104":
		c.Body = `A-format paperback`

  // In UK and IE, a category of paperback characterized by page size (normally 198 x 129 mm approx); use with Product Form code BC
  case "B105":
		c.Body = `B-format paperback`

  // In UK and IE, a category of paperback characterized largely by size (usually in traditional hardback dimensions), and often used for paperback originals or retailer/travel/export-exclusives; use with Product Form code BC
  case "B106":
		c.Body = `Trade paperback (UK)`

  // In North America, a category of paperback characterised partly by page size (typically 7½ x 4¼ inches) and partly by target market and terms of trade; use with Product Form code BC
  case "B107":
		c.Body = `Tall rack paperback (US)`

  // Japanese A-series size, 210 x 148mm. A tankobon is a complete collected story originally published in serialised form (eg in a magazine)
  case "B108":
		c.Body = `A5 size Tankobon`

  // Japanese B-series size, 257 x 182mm
  case "B109":
		c.Body = `JIS B5 size Tankobon`

  // Japanese B-series size, 182 x 128mm
  case "B110":
		c.Body = `JIS B6 size Tankobon`

  // Japanese A-series size, 148 x 105mm
  case "B111":
		c.Body = `A6 size Bunko`

  // Japanese format, 182x103mm or 173x105mm
  case "B112":
		c.Body = `B40-dori Shinsho`

  // A Swedish, Norwegian, French paperback format, of no particular fixed size. Use with Product Form Code BC
  case "B113":
		c.Body = `Pocket (Sweden, Norway, France)`

  // A Swedish paperback format, use with Product Form Code BC. In Finnish, Jättipokkari
  case "B114":
		c.Body = `Storpocket (Sweden)`

  // A Swedish hardback format, use with Product Form Code BB
  case "B115":
		c.Body = `Kartonnage (Sweden)`

  // A Swedish softback format, use with Product Form Code BC
  case "B116":
		c.Body = `Flexband (Sweden)`

  // A softback book in the format of a magazine, usually sold like a book. Use with Product Form code BC
  case "B117":
		c.Body = `Mook / Bookazine`

  // Also called ‘Flipback’. A softback book in a specially compact proprietary format with pages printed in landscape on very thin paper and bound along the long (top) edge (ie parallel with the lines of text). Use with Product Form code BC – see www.dwarsligger.com
  case "B118":
		c.Body = `Dwarsligger`

  // Japanese format, 188 x 127mm
  case "B119":
		c.Body = `46 size`

  // Japanese format, 188 x 127mm
  case "B120":
		c.Body = `46-Henkei size`

  // 297 x 210mm
  case "B121":
		c.Body = `A4`

  // Japanese format, 297 x 210mm
  case "B122":
		c.Body = `A4-Henkei size`

  // Japanese format, 210 x 146mm
  case "B123":
		c.Body = `A5-Henkei size`

  // Japanese format, 257 x 182mm
  case "B124":
		c.Body = `B5-Henkei size`

  // Japanese format, 182 x 128mm
  case "B125":
		c.Body = `B6-Henkei size`

  // 257 x 210mm
  case "B126":
		c.Body = `AB size`

  // Japanese B-series size, 128 x 91mm
  case "B127":
		c.Body = `JIS B7 size`

  // Japanese format, 218 x 152mm or 227 x 152mm
  case "B128":
		c.Body = `Kiku size`

  // Japanese format
  case "B129":
		c.Body = `Kiku-Henkei size`

  // Japanese B-series size, 364 x 257 mm
  case "B130":
		c.Body = `JIS B4 size`

  // German large paperback format, greater than about 205mm high, with flaps. Use with Product form code BC
  case "B131":
		c.Body = `Paperback (DE)`

  // Spanish pocket paperback. Use with Product form code BC
  case "B132":
		c.Body = `Libro de bolsillo`

  // German ,Taschenbuch‘, Italian «Tascabile / Supertascabile» pocket-sized format, usually less than about 205mm high. Use with Product form code BB or BC
  case "B133":
		c.Body = `Pocket-sized`

  // 210 x 148mm
  case "B134":
		c.Body = `A5`

  // In North America, a category of paperback characterized partly by page size (typically 7⅛ x 4¾ inches) and partly by target market and terms of trade. Use with Product Form code BC
  case "B135":
		c.Body = `Mass market max paperback`

  // Coloring / join-the-dot book
  case "B201":
		c.Body = `Coloring / join-the-dot book`

  // Lift-the-flap book
  case "B202":
		c.Body = `Lift-the-flap book`

  // Note: was wrongly shown as B203 (duplicate entry) in Issue 3
  case "B204":
		c.Body = `Miniature book`

  // Moving picture / flicker book
  case "B205":
		c.Body = `Moving picture / flicker book`

  // Pop-up book
  case "B206":
		c.Body = `Pop-up book`

  // Scented / ‘smelly’ book
  case "B207":
		c.Body = `Scented / ‘smelly’ book`

  // Sound story / ‘noisy’ book
  case "B208":
		c.Body = `Sound story / ‘noisy’ book`

  // Sticker book
  case "B209":
		c.Body = `Sticker book`

  // A book whose pages have a variety of textured inserts designed to stimulate tactile exploration: see also B214 and B215
  case "B210":
		c.Body = `Touch-and-feel book`

  // A book which is cut into a distinctive non-rectilinear shape and/or in which holes or shapes have been cut internally. (‘Die-cut’ is used here as a convenient shorthand, and does not imply strict limitation to a particular production process)
  case "B212":
		c.Body = `Die-cut book`

  // A book which is also a toy, or which incorporates a toy as an integral part. (Do not, however, use B213 for a multiple-item product which includes a book and a toy as separate items)
  case "B213":
		c.Body = `Book-as-toy`

  // A book whose cover has a soft textured finish, typically over board
  case "B214":
		c.Body = `Soft-to-touch book`

  // A book with detachable felt pieces and textured pages on which they can be arranged
  case "B215":
		c.Body = `Fuzzy-felt book`

  // A book containing pages with die-cut or press-out pieces that can be used as a jigsaw, puzzle pieces, etc
  case "B216":
		c.Body = `Press-out puzzle pieces`

  // Children’s picture book: use with applicable Product Form code
  case "B221":
		c.Body = `Picture book`

  // (aka ‘Star’ book). Tax treatment of products may differ from that of products with similar codes such as Book as toy or Pop-up book)
  case "B222":
		c.Body = `‘Carousel’ book`

  // A book with movable card ‘tabs’ within the pages. Pull a tab to reveal or animate part of a picture (distinct from a ‘lift-the-flap’ book, where flaps simply reveal hidden pictures, and not a ‘pop-up’ book with 3D paper engineering)
  case "B223":
		c.Body = `Pull-the-tab book`

  // Use with Product Form code BD, BN or PM
  case "B301":
		c.Body = `Loose leaf or partwork – sheets / parts and binder / wallet`

  // Use with Product Form code BD, BN or PM
  case "B302":
		c.Body = `Loose leaf or partwork – binder / wallet only`

  // Use with Product Form code BD, BN or PM
  case "B303":
		c.Body = `Loose leaf or partwork – sheets / parts only`

  // AKA stitched; for ‘saddle-sewn’, see code B310
  case "B304":
		c.Body = `Sewn`

  // Including ‘perfect bound’, ‘glued’
  case "B305":
		c.Body = `Unsewn / adhesive bound`

  // Strengthened cloth-over-boards binding intended for libraries: use with Product form code BB
  case "B306":
		c.Body = `Library binding`

  // Strengthened binding, not specifically intended for libraries: use with Product form code BB or BC
  case "B307":
		c.Body = `Reinforced binding`

  // Highest qualiy material used on spine and corners only. Must be accompanied by a code specifiying a material, eg ‘half-bound real leather’
  case "B308":
		c.Body = `Half bound`

  // Highest qualiy material used on spine only. Must be accompanied by a code specifiying a material, eg ‘quarter bound real leather’
  case "B309":
		c.Body = `Quarter bound`

  // AKA ‘saddle-stitched’ or ‘wire-stitched’
  case "B310":
		c.Body = `Saddle-sewn`

  // Round or oval plastic forms in a clamp-like configuration: use with Product Form code BE
  case "B311":
		c.Body = `Comb bound`

  // Twin loop metal wire spine: use with Product Form code BE
  case "B312":
		c.Body = `Wire-O`

  // Cased over Coiled or Wire-O binding: use with Product Form code BE and Product Form Detail code B312 or B314
  case "B313":
		c.Body = `Concealed wire`

  // Spiral wire bound. Use with product form code BE. The default if a spiral binding type is not stated. Cf. Comb and Wire-O binding
  case "B314":
		c.Body = `Coiled wire bound`

  // Hardcover binding intended for general consumers rather than libraries, use with Product form code BB. The default if a hardcover binding detail is not stated. cf. Library binding
  case "B315":
		c.Body = `Trade binding`

  // Cover is attached to the book block along only one edge of the spine, allowing the cover to lay flat
  case "B316":
		c.Body = `Swiss binding`

  // Refinement of perfect binding, with notches cut in the spine of the book block prior to glueing, to improve adhesion and durability
  case "B317":
		c.Body = `Notched binding`

  // Covers do not use a distinctive stock, but are the same as the body pages
  case "B400":
		c.Body = `Self-covered`

  // Cotton, linen or other woven fabric over boards. Use with <ProductForm> BB
  case "B401":
		c.Body = `Cloth over boards`

  // Cellulose-based or similar non-woven material, which may be printed and may be embossed with an artificial cloth or leather-like texture, over boards. Use with <ProductForm> BB
  case "B402":
		c.Body = `Paper over boards`

  // Covered with leather created by tanning animal hide. May be ‘full-grain’ using the entire thickness of the hide, ‘top grain’ using the outer layer of the hide, or ‘split’ using the inner layers of the hide. Split leather may be embossed with an artificial grain or texture. Use with <ProductForm> BG, and if appropriate with codes B308 or B309 (otherwise ‘full-bound’ is implied)
  case "B403":
		c.Body = `Leather, real`

  // Covered with synthetic leather-like material – polymer or non-animal fibre over a textile backing, usually coated and embossed with an artificial grain or texture. Leatherette, pleather etc. Use with <ProductForm> BB (or BG if particularly high-quality), and if appropriate with codes B308 or B309 (otherwise ‘full-bound’ is implied)
  case "B404":
		c.Body = `Leather, imitation`

  // Covered with leather reconstituted from a pulp made from shredded animal hide, layered on a fibre or textile backing, coated and usually embossed with an artificial grain or texture. Use with <ProductForm> BG, and if appropriate with codes B308 or B309 (otherwise ‘full-bound’ is implied)
  case "B405":
		c.Body = `Leather, bonded`

  // Pages made with prepared but untanned animal skin (usually calf, occasionally goat or sheep). Includes parchment, a thicker and less refined form of animal skin, but not ‘paper vellum’ or vegetable parchment made from synthetic or plant fibres
  case "B406":
		c.Body = `Vellum`

  // Cloth, not necessarily over boards – cf B401
  case "B409":
		c.Body = `Cloth`

  // Spanish ‘simil-tela’
  case "B410":
		c.Body = `Imitation cloth`

  // Velvet
  case "B411":
		c.Body = `Velvet`

  // AKA ‘flexibound’: use with Product Form code BC
  case "B412":
		c.Body = `Flexible plastic/vinyl cover`

  // Plastic-covered
  case "B413":
		c.Body = `Plastic-covered`

  // Vinyl-covered
  case "B414":
		c.Body = `Vinyl-covered`

  // Book, laminating material unspecified, often termed PLC or PPC (printed laminated case, printed paper case) when used with Product form BB. Use L101 for ‘whole product laminated’, eg a laminated sheet map or wallchart
  case "B415":
		c.Body = `Laminated cover`

  // With card cover (like a typical paperback). As distinct from a self-cover or more elaborate binding
  case "B416":
		c.Body = `Card cover`

  // Printed both inside and outside the front and/or back cover
  case "B417":
		c.Body = `Duplex-printed cover`

  // Type unspecified
  case "B501":
		c.Body = `With dust jacket`

  // Used to distinguish from B503
  case "B502":
		c.Body = `With printed dust jacket`

  // With translucent paper or plastic protective cover
  case "B503":
		c.Body = `With translucent dust cover`

  // For paperback with flaps
  case "B504":
		c.Body = `With flaps`

  // With thumb index
  case "B505":
		c.Body = `With thumb index`

  // If the number of markers is significant, it can be stated as free text in <ProductFormDescription>
  case "B506":
		c.Body = `With ribbon marker(s)`

  // With zip fastener
  case "B507":
		c.Body = `With zip fastener`

  // With button snap fastener
  case "B508":
		c.Body = `With button snap fastener`

  // AKA yapp edge?
  case "B509":
		c.Body = `With leather edge lining`

  // With edge trimming such that the front edge is ragged, not neatly and squarely trimmed: AKA deckle edge, feather edge, uncut edge, rough cut
  case "B510":
		c.Body = `Rough front`

  // With one or more gatefold or foldout sections bound in
  case "B511":
		c.Body = `Foldout`

  // Pages include extra-wide margin specifically intended for hand-written annotations
  case "B512":
		c.Body = `Wide margin`

  // Book with attached loop for fixing to baby stroller, cot, chair etc
  case "B513":
		c.Body = `With fastening strap`

  // With one or more pages perforated and intended to be torn out for use
  case "B514":
		c.Body = `With perforated pages`

  // Printed on acid-free or alkaline buffered paper conforming with ISO 9706
  case "B515":
		c.Body = `Acid-free paper`

  // Printed on acid-free or alkaline buffered paper with a high cotton content, conforming with ISO 11108
  case "B516":
		c.Body = `Archival paper`

  // Strap acts as closure or as page marker
  case "B517":
		c.Body = `With elasticated strap`

  // For example, holographic sticker such as the banderol used in the Turkish book trade
  case "B518":
		c.Body = `With serialized authenticity token`

  // Jacket in the form of a pamphlet or poster, specifically intended to be removed and read or used separately from the book
  case "B519":
		c.Body = `With dust jacket poster`

  // A book in which half the content is printed upside-down, to be read the other way round. Also known as a ‘flip-book’ or ‘tête-bêche’ (Fr) binding, it has two front covers and a single spine. Usually an omnibus of two works
  case "B601":
		c.Body = `Turn-around book`

  // Manga with pages and panels in the sequence of the original Japanese, but with Western text
  case "B602":
		c.Body = `Unflipped manga format`

  // A book in which half the content is printed so as to be read from the other cover. All content is printed the same way up. Also known as ‘dos-à-dos’ (Fr) binding, it has two front covers and two spines. Usually an omnibus of two works
  case "B603":
		c.Body = `Back-to-back book`

  // Text shows syllable breaks
  case "B610":
		c.Body = `Syllabification`

  // Single letters only. Was formerly identified as UK Braille Grade 1
  case "B701":
		c.Body = `UK Uncontracted Braille`

  // With some letter combinations. Was formerly identified as UK Braille Grade 2
  case "B702":
		c.Body = `UK Contracted Braille`

  // For US Braille, prefer codes B704 and B705 as appropriate
  case "B703":
		c.Body = `US Braille`

  // US Uncontracted Braille
  case "B704":
		c.Body = `US Uncontracted Braille`

  // US Contracted Braille
  case "B705":
		c.Body = `US Contracted Braille`

  // For UEB, prefer codes B708 and B709 as appropriate
  case "B706":
		c.Body = `Unified English Braille`

  // Moon embossed alphabet, used by some print-impaired readers who have difficulties with Braille
  case "B707":
		c.Body = `Moon`

  // Unified English Uncontracted Braille
  case "B708":
		c.Body = `Unified English Uncontracted Braille`

  // Unified English Contracted Braille
  case "B709":
		c.Body = `Unified English Contracted Braille`

  // Proprietary RealNetworks format. Includes Real Video packaged within a .rm RealMedia container
  case "D101":
		c.Body = `Real Video format`

  // Quicktime format
  case "D102":
		c.Body = `Quicktime format`

  // AVI format
  case "D103":
		c.Body = `AVI format`

  // Windows Media Video format
  case "D104":
		c.Body = `Windows Media Video format`

  // MPEG-4
  case "D105":
		c.Body = `MPEG-4`

  // Use with an applicable Product Form code D*; note that more detail of operating system requirements can be given in a Product Form Feature composite
  case "D201":
		c.Body = `MS-DOS`

  // Use with an applicable Product Form code D*; see note on D201
  case "D202":
		c.Body = `Windows`

  // Use with an applicable Product Form code D*; see note on D201
  case "D203":
		c.Body = `Macintosh`

  // Use with an applicable Product Form code D*; see note on D201
  case "D204":
		c.Body = `UNIX / LINUX`

  // Use with an applicable Product Form code D*; see note on D201
  case "D205":
		c.Body = `Other operating system(s)`

  // Use with an applicable Product Form code D*; see note on D201
  case "D206":
		c.Body = `Palm OS`

  // Use with an applicable Product Form code D*; see note on D201
  case "D207":
		c.Body = `Windows Mobile`

  // Use with Product Form code DB or DI as applicable
  case "D301":
		c.Body = `Microsoft XBox`

  // Use with Product Form code DE or DB as applicable
  case "D302":
		c.Body = `Nintendo Gameboy Color`

  // Use with Product Form code DE or DB as applicable
  case "D303":
		c.Body = `Nintendo Gameboy Advanced`

  // Use with Product Form code DE or DB as applicable
  case "D304":
		c.Body = `Nintendo Gameboy`

  // Use with Product Form code DE or DB as applicable
  case "D305":
		c.Body = `Nintendo Gamecube`

  // Use with Product Form code DE or DB as applicable
  case "D306":
		c.Body = `Nintendo 64`

  // Use with Product Form code DE or DB as applicable
  case "D307":
		c.Body = `Sega Dreamcast`

  // Use with Product Form code DE or DB as applicable
  case "D308":
		c.Body = `Sega Genesis/Megadrive`

  // Use with Product Form code DE or DB as applicable
  case "D309":
		c.Body = `Sega Saturn`

  // Use with Product Form code DB as applicable
  case "D310":
		c.Body = `Sony PlayStation 1`

  // Use with Product Form code DB or DI as applicable
  case "D311":
		c.Body = `Sony PlayStation 2`

  // Use with Product Form code DE as applicable
  case "D312":
		c.Body = `Nintendo Dual Screen`

  // Use with Product Form code DB, DI, DO or E* as applicable
  case "D313":
		c.Body = `Sony PlayStation 3`

  // Use with Product Form code DB, DI or VN as applicable
  case "D314":
		c.Body = `Microsoft Xbox 360`

  // Use with Product Form code DA or E* as applicable
  case "D315":
		c.Body = `Nintendo Wii`

  // Use with Product Form code DL or VL as applicable
  case "D316":
		c.Body = `Sony PlayStation Portable (PSP)`

  // Use with Product Form code DB, DI, DO or E* as applicable. DEPRECATED
  case "D317":
		c.Body = `Sony PlayStation 3`

  // Use with Product Form code DB, DI, DO or E* as applicable
  case "D318":
		c.Body = `Sony PlayStation 4`

  // Use with Product Form code DA or E* as applicable
  case "D319":
		c.Body = `Sony PlayStation Vita`

  // Use with Product Form code DB, DI, DO or E* as applicable
  case "D320":
		c.Body = `Microsoft Xbox One`

  // Use with Product Form code DE or DB as applicable
  case "D321":
		c.Body = `Nintendo Switch`

  // Use with Product Form code DE or DB as applicable
  case "D322":
		c.Body = `Nintendo Wii U`

  // Use with Product Form code DB, DI, DO or E* as applicable
  case "D323":
		c.Body = `Sony PlayStation 5`

  // Use with Product Form code DB, DI, DO or E* as applicable
  case "D324":
		c.Body = `Microsoft Xbox Series X / S`

  // No code allocated for this e-publication format yet
  case "E100":
		c.Body = `Other`

  // The Open Publication Structure / OPS Container Format standard of the International Digital Publishing Forum (IDPF) [File extension .epub]
  case "E101":
		c.Body = `EPUB`

  // The Open EBook format of the IDPF, a predecessor of the full EPUB format, still (2008) supported as part of the latter [File extension .opf]. Includes EPUB format up to and including version 2 – but prefer code E101 for EPUB 2, and always use code E101 for EPUB 3
  case "E102":
		c.Body = `OEB`

  // Microsoft Word binary document format [File extension .doc]
  case "E103":
		c.Body = `DOC`

  // Office Open XML / Microsoft Word XML document format (ISO/IEC 29500:2008) [File extension .docx]
  case "E104":
		c.Body = `DOCX`

  // HyperText Mark-up Language [File extension .html, .htm]
  case "E105":
		c.Body = `HTML`

  // Open Document Format [File extension .odt]
  case "E106":
		c.Body = `ODF`

  // Portable Document Format (ISO 32000-1:2008) [File extension .pdf]
  case "E107":
		c.Body = `PDF`

  // PDF archiving format defined by ISO 19005-1:2005 [File extension .pdf]
  case "E108":
		c.Body = `PDF/A`

  // Rich Text Format [File extension .rtf]
  case "E109":
		c.Body = `RTF`

  // Standard Generalized Mark-up Language
  case "E110":
		c.Body = `SGML`

  // A compressed text format mainly used on Psion handheld devices [File extension .tcr]
  case "E111":
		c.Body = `TCR`

  // Text file format [File extension .txt]. Typically ASCII or Unicode UTF-8/16
  case "E112":
		c.Body = `TXT`

  // Extensible Hypertext Markup Language [File extension .xhtml, .xht, .xml, .html, .htm]
  case "E113":
		c.Body = `XHTML`

  // A compressed text format mainly used on Palm handheld devices [File extension .pdb – see also E121, E125, E130]
  case "E114":
		c.Body = `zTXT`

  // XML Paper Specification format [File extension .xps]
  case "E115":
		c.Body = `XPS`

  // A format proprietary to Amazon for use with its Kindle reading devices or software readers [File extensions .azw, .mobi, .prc etc]. Prefer code E148 for Print Replica files
  case "E116":
		c.Body = `Amazon Kindle`

  // A Sony proprietary format for use with the Sony Reader and LIBRIé reading devices [File extension .lrf]
  case "E117":
		c.Body = `BBeB`

  // A proprietary format for use with DXReader software
  case "E118":
		c.Body = `DXReader`

  // A format proprietary to the Ebook Library service
  case "E119":
		c.Body = `EBL`

  // A format proprietary to the Ebrary service
  case "E120":
		c.Body = `Ebrary`

  // A proprietary format for use with eReader (AKA ‘Palm Reader’) software on various hardware platforms [File extension .pdb – see also E114, E125, E130]
  case "E121":
		c.Body = `eReader`

  // A proprietary format with its own reading system for Windows platforms [File extension .exe]
  case "E122":
		c.Body = `Exebook`

  // A proprietary format for use with the Franklin eBookman reader
  case "E123":
		c.Body = `Franklin eBookman`

  // A proprietary format for use with the Gemstar Rocketbook reader [File extension .rb]
  case "E124":
		c.Body = `Gemstar Rocketbook`

  // A proprietary format for use with iSilo software on various hardware platforms [File extension .pdb – see also E114, E121, E130]
  case "E125":
		c.Body = `iSilo`

  // A proprietary format for use with Microsoft Reader software on Windows and Pocket PC platforms [File extension .lit]
  case "E126":
		c.Body = `Microsoft Reader`

  // A proprietary format for use with Mobipocket software on various hardware platforms [File extensions .mobi, .prc]. Includes Amazon Kindle formats up to and including version 7 – but prefer code E116 for version 7, and always use E116 for KF8
  case "E127":
		c.Body = `Mobipocket`

  // A format proprietary to the MyiLibrary service
  case "E128":
		c.Body = `MyiLibrary`

  // A format proprietary to the NetLibrary service
  case "E129":
		c.Body = `NetLibrary`

  // A proprietary format for use with Plucker reader software on Palm and other handheld devices [File extension .pdb – see also E114, E121, E125]
  case "E130":
		c.Body = `Plucker`

  // A format proprietary to the VitalSource service
  case "E131":
		c.Body = `VitalBook`

  // A proprietary digital product combining text and video content and available to be used online or as a downloadable application for a mobile device – see www.vook.com
  case "E132":
		c.Body = `Vook`

  // An epublication made available by Google in association with a publisher; readable online on a browser-enabled device and offline on designated ebook readers
  case "E133":
		c.Body = `Google Edition`

  // Epublication packaged as application for iOS (eg Apple iPhone, iPad etc), containing both executable code and content. Use <ProductContentType> to describe content, and <ProductFormFeatureType> to list detailed technical requirements
  case "E134":
		c.Body = `Book ‘app’ for iOS`

  // Epublication packaged as application for Android (eg Android phone or tablet), containing both executable code and content. Use <ProductContentType> to describe content, and <ProductFormFeatureType> to list detailed technical requirements
  case "E135":
		c.Body = `Book ‘app’ for Android`

  // Epublication packaged as application, containing both executable code and content. Use where other ‘app’ codes are not applicable. Technical requirements such as target operating system and/or device should be provided eg in <ProductFormFeatureType>. Content type (text or text plus various ‘enhancements’) may be described with <ProductContentType>
  case "E136":
		c.Body = `Book ‘app’ for other operating system`

  // Founder Apabi’s proprietary basic e-book format
  case "E139":
		c.Body = `CEB`

  // Founder Apabi’s proprietary XML e-book format
  case "E140":
		c.Body = `CEBX`

  // Apple’s iBook format (a proprietary extension of EPUB), can only be read on Apple iOS devices
  case "E141":
		c.Body = `iBook`

  // Proprietary format based on EPUB used by Barnes and Noble for fixed-format e-books, readable on NOOK devices and Nook reader software
  case "E142":
		c.Body = `ePIB`

  // Sharable Content Object Reference Model, standard content and packaging format for e-learning objects
  case "E143":
		c.Body = `SCORM`

  // E-book Plus (proprietary Norwegian e-book format)
  case "E144":
		c.Body = `EBP`

  // Proprietary format based on PDF used by Barnes and Noble for fixed-format e-books, readable on some NOOK devices and Nook reader software
  case "E145":
		c.Body = `Page Perfect`

  // (Braille-ready file) Electronic Braille file
  case "E146":
		c.Body = `BRF`

  // Proprietary XML format for articles, see for example https://www.cairn.info/services-aux-editeurs.php
  case "E147":
		c.Body = `Erudit`

  // A format proprietary to Amazon for use with its Kindle reading devices or software readers. Essentially a PDF embedded within a KF8 format file
  case "E148":
		c.Body = `Amazon Kindle Print Replica`

  // Format for comic books, consisting primarily of sequentially-named PNG or JPEG images in a zip container
  case "E149":
		c.Body = `Comic Book Archive`

  // Use this and/or code E201 when a particular e-publication type (specified using codes E100 and upwards) has both fixed format and reflowable variants, to indicate which option is included in this product
  case "E200":
		c.Body = `Reflowable`

  // Use this and/or code E200 when a particular e-publication type (specified using codes E100 and upwards) has both fixed format and reflowable variants, to indicate which option is included in this product
  case "E201":
		c.Body = `Fixed format`

  // All e-publication resources are included within the e-publication package
  case "E202":
		c.Body = `Readable offline`

  // E-publication requires a network connection to access some resources (eg an enhanced e-book where video clips are not stored within the e-publication package itself, but are delivered via an internet connection)
  case "E203":
		c.Body = `Requires network connection`

  // Resources (eg images) present in other editions have been removed from this product, eg due to rights issues
  case "E204":
		c.Body = `Content removed`

  // Use for fixed-format e-books optimised for landscape display. Also include an indication of the optimal screen aspect ratio
  case "E210":
		c.Body = `Landscape`

  // Use for fixed-format e-books optimised for portrait display. Also include an indication of the optimal screen aspect ratio
  case "E211":
		c.Body = `Portrait`

  // Use for fixed-format e-books optimised for displays with a 5:4 aspect ratio (eg 1280x1024 pixels etc, assuming square pixels). Note that aspect ratio codes are NOT specific to actual screen dimensions or pixel counts, but to the ratios between two dimensions or two pixel counts
  case "E221":
		c.Body = `5:4`

  // Use for fixed-format e-books optimised for displays with a 4:3 aspect ratio (eg 800x600, 1024x768, 2048x1536 pixels etc)
  case "E222":
		c.Body = `4:3`

  // Use for fixed-format e-books optimised for displays with a 3:2 aspect ratio (eg 960x640, 3072x2048 pixels etc)
  case "E223":
		c.Body = `3:2`

  // Use for fixed-format e-books optimised for displays with a 16:10 aspect ratio (eg 1440x900, 2560x1600 pixels etc)
  case "E224":
		c.Body = `16:10`

  // Use for fixed-format e-books optimised for displays with a 16:9 aspect ratio (eg 1024x576, 1920x1080, 2048x1152 pixels etc)
  case "E225":
		c.Body = `16:9`

  // Whole product laminated (eg laminated map, fold-out chart, wallchart, etc): use B415 for book with laminated cover
  case "L101":
		c.Body = `Laminated`

  // Large format. Use with Product Form code PC or PF
  case "P101":
		c.Body = `Desk calendar or diary`

  // Small format. Use with Product Form code PC or PF
  case "P102":
		c.Body = `Mini calendar or pocket diary`

  // Usually with time-of-day subdivisions (rather than just days). Use with Product Form code PC or PF
  case "P103":
		c.Body = `Engagement calendar or Appointment diary`

  // Eg tear-off calendars. Use with Product Form code PC
  case "P104":
		c.Body = `Day by day calendar`

  // Large single-sheet calendar intended for hanging. Use with Product Form code PC or PK
  case "P105":
		c.Body = `Poster calendar`

  // Large calendar intended for hanging, usually one page per month. Use with Product Form code PC
  case "P106":
		c.Body = `Wall calendar`

  // Usually undated. Use with Product Form code PC or PF
  case "P107":
		c.Body = `Perpetual calendar or diary`

  // Use with Product Form code PC
  case "P108":
		c.Body = `Advent calendar`

  // Use with Product Form code PC or PT
  case "P109":
		c.Body = `Bookmark calendar`

  // Mid-year diary, start and end aligned with the academic year. Use with Product Form code PC or PF
  case "P110":
		c.Body = `Student or Academic calendar or diary`

  // Use with Product Form code PC
  case "P111":
		c.Body = `Project calendar`

  // Use with Product Form code PC
  case "P112":
		c.Body = `Almanac calendar`

  // A calendar, diary or organiser that is not one of the types specified elsewhere: use with Product Form code PC, PF or PS
  case "P113":
		c.Body = `Other calendar, diary or organiser`

  // A product that is associated with or ancillary to a calendar or organiser, eg a deskstand for a calendar, or an insert for an organiser: use with Product Form code PC or PS
  case "P114":
		c.Body = `Other calendar or organiser product`

  // Wall or poster calendar with entries for each family member. Use with Product Form code PC or PK
  case "P115":
		c.Body = `Family planner`

  // Calendar sheets detachable (usually perforated) and intended for mailing as postcards. Use with Product Form code PC
  case "P116":
		c.Body = `Postcard calendar`

  // Kamishibai / Cantastoria cards
  case "P120":
		c.Body = `Picture story cards`

  // For use to specify letter, word, image (etc) recognition cards for teaching reading or other classroom use. Use with Product form code PD
  case "P121":
		c.Body = `Flash cards`

  // Quick reference cards, revision cards, recipe cards etc. Use with Product form code PD
  case "P122":
		c.Body = `Reference cards`

  // For use to specify cards and card decks for gaming, collecting and trading etc. Use also for divination cards. Use with Product form codes PD
  case "P123":
		c.Body = `Recreation cards`

  // And postcard packs / books. Use with Product form code PJ
  case "P124":
		c.Body = `Postcards`

  // And greeting card packs. Use with Product form code PJ
  case "P125":
		c.Body = `Greeting cards`

  // Stationery item in hardback book format
  case "P201":
		c.Body = `Hardback (stationery)`

  // Stationery item in paperback/softback book format
  case "P202":
		c.Body = `Paperback / softback (stationery)`

  // Stationery item in spiral-bound book format
  case "P203":
		c.Body = `Spiral bound (stationery)`

  // Stationery item in leather-bound book format, or other fine binding
  case "P204":
		c.Body = `Leather / fine binding (stationery)`

  // For wall map, poster, wallchart etc
  case "P301":
		c.Body = `With hanging strips`

  // SD TV standard for video or DVD
  case "V201":
		c.Body = `PAL`

  // SD TV standard for video or DVD
  case "V202":
		c.Body = `NTSC`

  // SD TV standard for video or DVD
  case "V203":
		c.Body = `SECAM`

  // Up to 2K resolution (1920 or 2048 pixels wide) eg for Blu-Ray
  case "V205":
		c.Body = `HD`

  // Up to 4K resolution (3840 or 4096 pixels wide) eg for Ultra HD Blu-Ray
  case "V206":
		c.Body = `UHD`

  // Eg for Blu-ray 3D
  case "V207":
		c.Body = `3D video`

  // Licensed for use in domestic contexts only
  case "V220":
		c.Body = `Home use`

  // Licensed for use in education
  case "V221":
		c.Body = `Classroom use`

  // Primary material composition (eg of kit or puzzle pieces, of gameplay tokens or tiles) is wood or has wooden pieces/parts
  case "Z101":
		c.Body = `Wooden`

  // Plastic or plastic pieces/parts
  case "Z102":
		c.Body = `Plastic`

  // Card or board pieces/parts
  case "Z103":
		c.Body = `Board`

  // Puzzle assembles into a 3D object
  case "Z111":
		c.Body = `3D puzzle`

  // Toy makes a noise. See B208 for noisy books
  case "Z112":
		c.Body = `Noisy kit / puzzle / toy`

  // Including finger / hand puppets, marionettes
  case "Z113":
		c.Body = `Puppet`

  // Designed and sized for the very young, or those with visual impairments, limited motor skills, dementia etc
  case "Z121":
		c.Body = `Extra large pieces`
	default:
		return fmt.Errorf("undefined code for ProductFormDetail has been passed, got [%s]", v)
	}
	return nil
}

// ProductFormFeatureType Product form feature type
type ProductFormFeatureType struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *ProductFormFeatureType) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // For Product Form Feature values see code list 98
  case "01":
		c.Body = `Color of cover`

  // For Product Form Feature values see code list 98
  case "02":
		c.Body = `Color of page edge`

  // The principal font used for body text, when this is a significant aspect of product description, eg for some Bibles, and for large print product. The accompanying <ProductFormFeatureDescription> is text specifying the typeface name. The font size may be specified with the font name, but is preferred separately (in points) in <ProductFormFeatureValue>
  case "03":
		c.Body = `Text font`

  // For Product Form Feature values see code list 99
  case "04":
		c.Body = `Special cover material`

  // For Product Form Feature values see code list 76
  case "05":
		c.Body = `DVD region`

  // A computer or handheld device operating system required to use a digital product, with version detail if applicable. The accompanying Product Form Feature Value is a code from List 176. Version detail, when applicable, is carried in Product Form Feature Description
  case "06":
		c.Body = `Operating system requirements`

  // Other system requirements for a digital product, described by free text in Product Form Feature Description
  case "07":
		c.Body = `Other system requirements`

  // Indicates compatibility with proprietary ‘point and listen’ devices such as Ting Pen (http://www.ting.eu), the iSmart Touch and Read Pen. These devices scan invisible codes specially printed on the page to identify the book and position of the word, and the word is then read aloud by the device. The name of the compatible device (or range of devices) should be given in <ProductFormFeatureDescription>
  case "08":
		c.Body = `‘Point and listen’ device compatibility`

  // For <ProductFormFeatureValue> codes, see Codelist 196
  case "09":
		c.Body = `E-publication accessibility detail`

  // For versioned e-book file formats (or in some cases, devices). <ProductFormFeatureValue> should contain the version number as a period-separated list of numbers (eg ‘7’, ‘1.5’ or ‘3.10.7’). Use only with ONIX 3.0 – in ONIX 2.1, use <EpubTypeVersion> instead. For the most common file formats, code 15 and List 220 is strongly preferred
  case "10":
		c.Body = `E-publication format version`

  // Hazard warning required by US Consumer Product Safety Improvement Act (CPSIA) of 2008 or other US or international legislation. Required, when applicable, for products sold in the US. The Product Form Feature Value is a code from List 143. Further explanation may be given in Product Form Feature Description
  case "12":
		c.Body = `US CPSIA or other international hazard warning`

  // Product carries hazard warning required by EU Toy Safety Directive. The Product Form Feature Value is a code from List 184, and (for some codes) the exact wording of the warning may be given in Product Form Feature Description
  case "13":
		c.Body = `EU Toy Safety Hazard warning`

  // Product Form Feature Description must give further details of the warning
  case "14":
		c.Body = `IATA Dangerous Goods warning`

  // For common versioned e-book formats (or in some cases, devices) – for example EPUB 2.0.1 or EPUB 3.0. <ProductFormFeatureValue> is a code from list 220. Use in ONIX 3.0 only
  case "15":
		c.Body = `E-publication format version code`

  // For common versioned e-book formats, the name and version of the validator used to check conformance. <ProductFormFeatureDescription> is the common name of the validator used (eg EpubCheck, Flightdeck), and <ProductFormFeatureValue> is the version number of the validator (eg 4.0.0a). Use with code 15 (or possibly code 10), or with <EpubTypeVersion>, to specify the version the e-publication conforms with
  case "16":
		c.Body = `E-publication format validator version`

  // Indicates compatibility with proprietary ‘point and watch‘ devices or apps. These scan invisible codes specially printed on the page, or the whole page image, to identify the book and page position. Scanning can trigger display of (for example) an augmented reality view of the page. The name of the compatible app or device (or range of apps/devices) should be given in <ProductFormFeatureDescription>. For use in ONIX 3.0 only
  case "17":
		c.Body = `‘Point and watch’ device/app compatibility`

  // Requirement for user authentication prior to use, with detail of authentication method (user enrolment, and login passwords, location- or device-based recognition, authentication via third-party identity service etc) given in <ProductFormFeatureDescription>. For use in ONIX 3.0 only
  case "18":
		c.Body = `E-publication authentication and access control`

  // Use to describe battery requirements, hazards and safety warnings. <ProductFormFeatureValue> is a code from List 242. For use in ONIX 3.0 only
  case "19":
		c.Body = `Battery type`

  // Total capacity (of batteries in the product) in Watt hours. <ProductFormFeatureValue> is an integer or decimal number (eg ‘45’, not ‘45Wh’). For use in ONIX 3.0 only
  case "20":
		c.Body = `Battery capacity`

  // Use to describe regulation of the product for various purposes. <ProductFormFeatureValue> is a code from List 243. For use in ONIX 3.0 only
  case "21":
		c.Body = `Dangerous goods`

  // Number of pieces, eg for jigsaws, puzzles, kits, board games. <ProductFormFeatureValue> is an integer. For use in ONIX 3.0 only
  case "22":
		c.Body = `Game pieces`

  // Number of players, for board games, card games, videogames etc. <ProductFormFeatureValue> must be a required (exact) number as an integer OR a range (eg ‘2–6’), optionally accompanied by the number of players as text (eg ‘suitable for 2–6 players’) in <ProductFormFeatureDescription>. For use in ONIX 3.0 only
  case "23":
		c.Body = `Game players`

  // Typical time to complete a game, for board games, card games, videogames etc, stated as an integer (in minutes) OR range (eg ‘60–90’) in <ProductFormFeatureValue>, optionally accompanied by the playing time as text (eg ‘typically 60–90 minutes’) in <ProductFormFeatureDescription>. For use in ONIX 3.0 only
  case "24":
		c.Body = `Game play time`

  // Product does not carry FSC or PEFC logo. The Product Form Feature Value element is not used. The Product Form Feature Description element may carry free text indicating the grade or type of paper. The product record may also still carry a claimed Pre- and Post-Consumer Waste (PCW) percentage value (type code 37) in a separate repeat of the Product Form Feature composite
  case "30":
		c.Body = `Not FSC or PEFC certified`

  // Product carries FSC logo (Pure, 100%). <ProductFormFeatureValue> is the Certification number (ie either a Chain Of Custody (COC) number or a Trademark License number) printed on the book. Format: Chain of Custody number is two to five letters-COC-six digits (the digits should include leading zeros if necessary), eg ‘AB-COC-001234’ or ‘ABCDE-COC-123456’; Trademark License number is C followed by six digits, eg ‘C005678’ (this would normally be prefixed by ‘FSC®’ when displayed). The Product Form Feature Description element may carry free text indicating the grade or type of paper. By definition, a product certified Pure does not contain Pre- or Post-Consumer-Waste (PCW), so type code 31 can only occur on its own. Certification numbers may be checked at https://info.fsc.org/
  case "31":
		c.Body = `FSC certified – pure`

  // Product carries FSC logo (Mixed sources, Mix). <ProductFormFeatureValue> is the Certification number (ie either a Chain Of Custody (COC) number or a Trademark License number) printed on the book. Format: Chain of Custody number is two to five letters-COC-six digits (the digits should include leading zeros if necessary), eg ‘AB-COC-001234’ or ‘ABCDE-COC-123456’; Trademark License number is C followed by six digits, eg ‘C005678’ (this would normally be prefixed by ‘FSC®’ when displayed). The Product Form Feature Description element may carry free text indicating the grade or type of paper. May be accompanied by a Pre- and Post-Consumer-Waste (PCW) percentage value, to be reported in another instance of <ProductFormFeature> with type code 36. Certification numbers may be checked at https://info.fsc.org/
  case "32":
		c.Body = `FSC certified – mixed sources`

  // Product carries FSC logo (Recycled). <ProductFormFeatureValue> is the Certification number (ie either a Chain Of Custody (COC) number or a Trademark License number) printed on the book. Format: Chain of Custody number is two to five letters-COC-six digits (the digits should include leading zeroes if necessary), eg ‘AB-COC-001234’ or ‘ABCDE-COC-123456’; Trademark License number is C followed by six digits, eg ‘C005678’ (this would normally be prefixed by ‘FSC®’ when displayed). The Product Form Feature Description element may carry free text indicating the grade or type of paper. Should be accompanied by a Pre- and Post-Consumer-Waste (PCW) percentage value, to be reported in another instance of <ProductFormFeature> with type code 36. Certification numbers may be checked at https://info.fsc.org/
  case "33":
		c.Body = `FSC certified – recycled`

  // Product carries PEFC logo (certified). <ProductFormFeatureValue> is the Chain Of Custody (COC) number printed on the book. The Product Form Feature Description element may carry free text indicating the grade or type of paper. May be accompanied by a Post-Consumer Waste (PCW) percentage value, to be reported in another instance of <ProductFormFeature> with type code 36
  case "34":
		c.Body = `PEFC certified`

  // Product carries PEFC logo (recycled). <ProductFormFeatureValue> is the Chain Of Custody (COC) number printed on the book. The Product Form Feature Description element may carry free text indicating the grade or type of paper. Should be accompanied by a Post-Consumer-Waste (PCW) percentage value, to be reported in another instance of <ProductFormFeature> with type code 36
  case "35":
		c.Body = `PEFC recycled`

  // The percentage of recycled Pre- and Post-Consumer-Waste (PCW) used in a product where the composition is certified by FSC or PEFC. <ProductFormFeatureValue> is an integer. May occur together with type code 32, 33, 34 or 35
  case "36":
		c.Body = `FSC or PEFC certified Pre- and Post-Consumer Waste (PCW) percentage`

  // The percentage of recycled Pre- and Post-Consumer Waste (PCW) claimed to be used in a product where the composition is not certified by FSC or PEFC. <Product FormFeatureValue> is an integer. <ProductFormFeatureDescription> may carry free text supporting the claim. Must be accompanied by type code 30
  case "37":
		c.Body = `Claimed Pre- and Post-Consumer Waste (PCW) percentage`

  // Product made from paper produced using environmentally-conscious technology. <ProductFormFeatureDescription> may carry free text with a more detailed statement
  case "40":
		c.Body = `Paper produced by ‘green’ technology`
	default:
		return fmt.Errorf("undefined code for ProductFormFeatureType has been passed, got [%s]", v)
	}
	return nil
}

// ProductIDType Product identifier type
type ProductIDType struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *ProductIDType) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // For example, a publisher’s or wholesaler’s product number or SKU. Note that <IDTypeName> is required with proprietary identifiers
  case "01":
		c.Body = `Proprietary`

  // International Standard Book Number, pre-2007 (10 digits, or 9 digits plus X, without spaces or hyphens) – now DEPRECATED in ONIX for Books, except where providing historical information for compatibility with legacy systems. It should only be used in relation to products published before 2007 – when ISBN-13 superseded it – and should never be used as the ONLY identifier (it should always be accompanied by the correct GTIN-13 / ISBN-13)
  case "02":
		c.Body = `ISBN-10`

  // GS1 Global Trade Item Number, formerly known as EAN article number (13 digits, without spaces or hyphens)
  case "03":
		c.Body = `GTIN-13`

  // UPC product number (12 digits, without spaces or hyphens)
  case "04":
		c.Body = `UPC`

  // International Standard Music Number, pre-2008 (M plus nine digits, without spaces or hyphens) – now DEPRECATED in ONIX for Books, except where providing historical information for compatibility with legacy systems. It should only be used in relation to products published before 2008 – when ISMN-13 superseded it – and should never be used as the ONLY identifier (it should always be accompanied by the correct GTIN-12 / ISMN-13)
  case "05":
		c.Body = `ISMN-10`

  // Digital Object Identifier (variable length and character set beginning ‘10.’, and without https://doi.org/ or the older http://dx.doi.org/)
  case "06":
		c.Body = `DOI`

  // Library of Congress Control Number in normalized form (up to 12 characters, alphanumeric)
  case "13":
		c.Body = `LCCN`

  // GS1 Global Trade Item Number (14 digits, without spaces or hyphens)
  case "14":
		c.Body = `GTIN-14`

  // International Standard Book Number, from 2007 (13 digits starting 978 or 9791–9799, without spaces or hypens)
  case "15":
		c.Body = `ISBN-13`

  // The number assigned to a publication as part of a national legal deposit process
  case "17":
		c.Body = `Legal deposit number`

  // Uniform Resource Name: note that in trade applications an ISBN must be sent as a GTIN-13 and, where required, as an ISBN-13 – it should not be sent as a URN
  case "22":
		c.Body = `URN`

  // A unique number assigned to a bibliographic item by OCLC
  case "23":
		c.Body = `OCLC number`

  // An ISBN-13 assigned by a co-publisher. The ‘main’ ISBN sent with <ProductIDType> codes 03 and/or 15 should always be the ISBN that is used for ordering from the supplier identified in <SupplyDetail>. However, ISBN rules allow a co-published title to carry more than one ISBN. The co-publisher should be identified in an instance of the <Publisher> composite, with the applicable <PublishingRole> code
  case "24":
		c.Body = `Co-publisher’s ISBN-13`

  // International Standard Music Number, from 2008 (13-digit number starting 9790, without spaces or hyphens)
  case "25":
		c.Body = `ISMN-13`

  // Actionable ISBN, in fact a special DOI incorporating the ISBN-13 within the DOI syntax. Begins ‘10.978.’ or ‘10.979.’ and includes a / character between the registrant element (publisher prefix) and publication element of the ISBN, eg 10.978.000/1234567. Note the ISBN-A should always be accompanied by the ISBN itself, using <ProductIDType> codes 03 and/or 15
  case "26":
		c.Body = `ISBN-A`

  // E-publication identifier controlled by JPOIID’s Committee for Research and Management of Electronic Publishing Codes
  case "27":
		c.Body = `JP e-code`

  // Unique number assigned by the Chinese Online Library Cataloging Center (see http://olcc.nlc.gov.cn)
  case "28":
		c.Body = `OLCC number`

  // Japanese magazine identifier, similar in scope to ISSN but identifying a specific issue of a serial publication. Five digits to identify the periodical, plus a hyphen and two digits to identify the issue
  case "29":
		c.Body = `JP Magazine ID`

  // Used only with comic books and other products which use the UPC extension to identify individual issues or products. Do not use where the UPC12 itself identifies the specific product, irrespective of any 5-digit extension – use code 04 instead
  case "30":
		c.Body = `UPC12+5`

  // Numéro de la notice bibliographique BNF
  case "31":
		c.Body = `BNF Control number`

  // Archival Resource Key, as a URL (including the address of the ARK resolver provided by eg a national library)
  case "35":
		c.Body = `ARK`
	default:
		return fmt.Errorf("undefined code for ProductIDType has been passed, got [%s]", v)
	}
	return nil
}

// ProductPackaging Product packaging type
type ProductPackaging struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *ProductPackaging) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // No packaging, or all smaller items enclosed inside largest item
  case "00":
		c.Body = `No outer packaging`

  // Thin card or soft plastic sleeve, much less rigid than a slip case
  case "01":
		c.Body = `Slip-sleeve`

  // Packaging consisting of formed plastic sealed around each side of the product. Not to be confused with single-sided Blister pack
  case "02":
		c.Body = `Clamshell`

  // Typical DVD-style packaging, sometimes known as an ‘Amaray’ case
  case "03":
		c.Body = `Keep case`

  // Typical CD-style packaging
  case "05":
		c.Body = `Jewel case`

  // Common CD-style packaging, a card folder with one or more panels incorporating a tray, hub or pocket to hold the disc(s)
  case "06":
		c.Body = `Digipak`

  // Individual item, items or set in card box with separate or hinged lid: not to be confused with the commonly-used ‘boxed set’
  case "09":
		c.Body = `In box`

  // Slip-case for single item only: German ‘Schuber’
  case "10":
		c.Body = `Slip-cased`

  // Slip-case for multi-volume set: German ‘Kassette’; also commonly referred to as ‘boxed set’
  case "11":
		c.Body = `Slip-cased set`

  // Rolled in tube or cylinder: eg sheet map or poster
  case "12":
		c.Body = `Tube`

  // Use for miscellaneous items such as slides, microfiche, when presented in a binder
  case "13":
		c.Body = `Binder`

  // Use for miscellaneous items such as slides, microfiche, when presented in a wallet or folder
  case "14":
		c.Body = `In wallet or folder`

  // Long package with triangular cross-section used for rolled sheet maps, posters etc
  case "15":
		c.Body = `Long triangular package`

  // Long package with square cross-section used for rolled sheet maps, posters, etc
  case "16":
		c.Body = `Long square package`

  // Softbox (for DVD)
  case "17":
		c.Body = `Softbox (for DVD)`

  // In pouch, eg teaching materials in a plastic bag or pouch
  case "18":
		c.Body = `Pouch`

  // In duroplastic or other rigid plastic case, eg for a class set
  case "19":
		c.Body = `Rigid plastic case`

  // In cardboard case, eg for a class set
  case "20":
		c.Body = `Cardboard case`

  // Use for products or product bundles supplied for retail sale in shrink-wrapped packaging. For shrink-wrapped packs of multiple products for trade supply only, see code XL in List 7
  case "21":
		c.Body = `Shrink-wrapped`

  // A pack comprising a pre-formed plastic blister and a printed card with a heat-seal coating
  case "22":
		c.Body = `Blister pack`

  // A case with carrying handle, typically for a set of educational books and/or learning materials
  case "23":
		c.Body = `Carry case`

  // Individual item, items or set in metal box or can with separate or hinged lid
  case "24":
		c.Body = `In tin`
	default:
		return fmt.Errorf("undefined code for ProductPackaging has been passed, got [%s]", v)
	}
	return nil
}

// ProductRelationCode Product relation
type ProductRelationCode struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *ProductRelationCode) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // <Product> is related to <RelatedProduct> in a way that cannot be specified by another code value
  case "00":
		c.Body = `Unspecified`

  // <Product> includes <RelatedProduct> (inverse of code 02)
  case "01":
		c.Body = `Includes`

  // <Product> is part of <RelatedProduct>: use for ‘also available as part of’ (inverse of code 01)
  case "02":
		c.Body = `Is part of`

  // <Product> replaces, or is new edition of, <RelatedProduct> (inverse of code 05)
  case "03":
		c.Body = `Replaces`

  // <Product> is replaced by, or has new edition, <RelatedProduct> (inverse of code 03)
  case "05":
		c.Body = `Replaced by`

  // <Product> is available in an alternative format as <RelatedProduct> – indicates an alternative format of the same content which is or may be available (is own inverse)
  case "06":
		c.Body = `Alternative format`

  // <Product> has an ancillary or supplementary product <RelatedProduct> (inverse of code 08)
  case "07":
		c.Body = `Has ancillary product`

  // <Product> is ancillary or supplementary to <RelatedProduct> (inverse of code 07)
  case "08":
		c.Body = `Is ancillary to`

  // <Product> is remaindered as <RelatedProduct>, when a remainder merchant assigns its own identifier to the product (inverse of code 10)
  case "09":
		c.Body = `Is remaindered as`

  // <Product> was originally sold as <RelatedProduct>, indicating the publisher’s original identifier for a title which is offered as a remainder under a different identifier (inverse of code 09)
  case "10":
		c.Body = `Is remainder of`

  // <Product> is an other-language version of <RelatedProduct> (is own inverse)
  case "11":
		c.Body = `Is other-language version of`

  // <Product> has a publisher’s suggested alternative <RelatedProduct>, which does not, however, carry the same content (cf 05 and 06)
  case "12":
		c.Body = `Publisher’s suggested alternative`

  // <Product> is an epublication based on printed product <RelatedProduct>. The related product is the source of any print-equivalent page numbering present in the epublication
  case "13":
		c.Body = `Epublication based on (print product)`

  // <Product> is a POD replacement for <RelatedProduct>. <RelatedProduct> is an out-of-print product replaced by a print-on-demand version under a new ISBN (inverse of code 17)
  case "16":
		c.Body = `POD replacement for`

  // <Product> is replaced by POD <RelatedProduct>. <RelatedProduct> is a print-on-demand replacement, under a new ISBN, for an out-of-print <Product> (inverse of code 16)
  case "17":
		c.Body = `Replaced by POD`

  // <Product> is a special edition of <RelatedProduct>. Used for a special edition (German: Sonderausgabe) with different cover, binding, premium content etc – more than ‘alternative format’ – which may be available in limited quantity and for a limited time (inverse of code 19)
  case "18":
		c.Body = `Is special edition of`

  // <Product> has a special edition <RelatedProduct> (inverse of code 18)
  case "19":
		c.Body = `Has special edition`

  // <Product> is a prebound edition of <RelatedProduct> (In the US, a ‘prebound’ edition is ‘a book that was previously bound and has been rebound with a library quality hardcover binding. In almost all commercial cases, the book in question began as a paperback. This might also be termed ‘re-bound’) (inverse of code 21)
  case "20":
		c.Body = `Is prebound edition of`

  // <Product> is the regular edition of which <RelatedProduct> is a prebound edition (inverse of code 20)
  case "21":
		c.Body = `Is original of prebound edition`

  // <Product> and <RelatedProduct> have a common author
  case "22":
		c.Body = `Product by same author`

  // <RelatedProduct> is another product that is suggested as similar to <Product> (‘if you liked <Product>, you may also like <RelatedProduct>’, or vice versa)
  case "23":
		c.Body = `Similar product`

  // <Product> is a facsimile edition of <RelatedProduct> (inverse of code 25)
  case "24":
		c.Body = `Is facsimile of`

  // <Product> is the original edition from which a facsimile edition <RelatedProduct> is taken (inverse of code 24)
  case "25":
		c.Body = `Is original of facsimile`

  // <Product> is a license for a digital <RelatedProduct>, traded or supplied separately
  case "26":
		c.Body = `Is license for`

  // <RelatedProduct> is an electronic version of print <Product> (inverse of code 13)
  case "27":
		c.Body = `Electronic version available as`

  // <RelatedProduct> is an ‘enhanced’ version of <Product>, with additional content. Typically used to link an enhanced e-book to its original ‘unenhanced’ equivalent, but not specifically limited to linking e-books – for example, may be used to link illustrated and non-illustrated print books. <Product> and <RelatedProduct> should share the same <ProductForm> (inverse of code 29)
  case "28":
		c.Body = `Enhanced version available as`

  // <RelatedProduct> is a basic version of <Product>. <Product> and <RelatedProduct> should share the same <ProductForm> (inverse of code 28)
  case "29":
		c.Body = `Basic version available as`

  // <RelatedProduct> and <Product> are part of the same collection (eg two products in same series or set) (is own inverse)
  case "30":
		c.Body = `Product in same collection`

  // <RelatedProduct> is an alternative product in another sector (of the same geographical market). Indicates an alternative that carries the same content, but available to a different set of customers, as one or both products are retailer-, channel- or market sector-specific (is own inverse)
  case "31":
		c.Body = `Has alternative in a different market sector`

  // <RelatedProduct> is an equivalent product, often intended for another (geographical) market. Indicates an alternative that carries essentially the same content, though slightly adapted for local circumstances (as opposed to a translation – use code 11) (is own inverse)
  case "32":
		c.Body = `Has equivalent intended for a different market`

  // <RelatedProduct> is an alternative product, often intended for another (geographical) market. Indicates the content of the alternative is identical in all respects (is own inverse)
  case "33":
		c.Body = `Has alternative intended for different market`

  // <Product> cites <RelatedProduct> (inverse of code 35)
  case "34":
		c.Body = `Cites`

  // <Product> is the object of a citation in <RelatedProduct> (inverse of code 34)
  case "35":
		c.Body = `Is cited by`

  // <Product> is a signed copy of <RelatedProduct>. Use where signed copies are given a distinct product identifier and can be ordered separately, but are otherwise identical (inverse of code 38)
  case "37":
		c.Body = `Is signed version of`

  // <Product> is an unsigned copy of <RelatedProduct>. Use where signed copies are given a distinct product identifier and can be ordered separately, but are otherwise identical (inverse of code 37)
  case "38":
		c.Body = `Has signed version`

  // <Product> is intended for teacher use, and the related product is for student use
  case "39":
		c.Body = `Has related student material`

  // <Product> is intended for student use, and the related product is for teacher use
  case "40":
		c.Body = `Has related teacher material`

  // <Product> includes some content shared with <RelatedProduct>. Note the shared content does not form the whole of either product. Compare with the ‘includes’ / ‘is part of’ relationship pair (codes 01 and 02), where the shared content forms the whole of one of the products, and with the ‘alternative format’ relationship (code 06), where the shared content forms the whole of both products (code 41 is own inverse)
  case "41":
		c.Body = `Some content shared with`

  // <Product> is a later edition of <RelatedProduct>, where the related product is the first edition
  case "42":
		c.Body = `Is later edition of first edition`

  // <Product> is an adapted (dramatized, abridged, novelized etc) version of <RelatedProduct> (inverse of code 44). For use in ONIX 3.0 only
  case "43":
		c.Body = `Adapted from`

  // <Product> is the original from which <RelatedProduct> is adapted (dramatized etc) (inverse of code 43), For use in ONIX 3.0 only
  case "44":
		c.Body = `Adapted as`
	default:
		return fmt.Errorf("undefined code for ProductRelationCode has been passed, got [%s]", v)
	}
	return nil
}

// Proximity Proximity
type Proximity struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *Proximity) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // Less than
  case "01":
		c.Body = `Less than`

  // Not more than
  case "02":
		c.Body = `Not more than`

  // The supplier’s true figure, or at least a best estimate expected to be within 10% of the true figure (ie a quoted figure of 100 could in fact be anything between 91 and 111)
  case "03":
		c.Body = `Exactly`

  // Generally interpreted as within 25% of the true figure (ie a quoted figure of 100 could in fact be anything between 80 and 133). The supplier may introduce a deliberate approximation to reduce the commercial sensitivity of the figure
  case "04":
		c.Body = `Approximately`

  // Generally interpreted as within a factor of two of the true figure (ie a quoted figure of 100 could in fact be anything between 50 and 200). The supplier may introduce a deliberate approximation to reduce the commercial sensitivity of the figure
  case "05":
		c.Body = `About`

  // Not less than
  case "06":
		c.Body = `Not less than`

  // More than
  case "07":
		c.Body = `More than`
	default:
		return fmt.Errorf("undefined code for Proximity has been passed, got [%s]", v)
	}
	return nil
}

// PublisherIDType Name identifier type
type PublisherIDType struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *PublisherIDType) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // Note that <IDTypeName> is required with proprietary identifiers
  case "01":
		c.Body = `Proprietary`

  // DEPRECATED – use 01
  case "02":
		c.Body = `Proprietary`

  // Deutsche Nationalbibliothek publisher identifier
  case "03":
		c.Body = `DNB publisher identifier`

  // Börsenverein Verkehrsnummer
  case "04":
		c.Body = `Börsenverein Verkehrsnummer`

  // German ISBN Agency publisher identifier
  case "05":
		c.Body = `German ISBN Agency publisher identifier`

  // GS1 global location number (formerly EAN location number)
  case "06":
		c.Body = `GLN`

  // Book trade Standard Address Number – US, UK etc
  case "07":
		c.Body = `SAN`

  // MARC code list for organizations – see http://www.loc.gov/marc/organizations/orgshome.html
  case "08":
		c.Body = `MARC organization code`

  // Trading party identifier used in the Netherlands
  case "10":
		c.Body = `Centraal Boekhuis Relatie ID`

  // Flemish publisher code
  case "13":
		c.Body = `Fondscode Boekenbank`

  // Business Identity Code (Finland). See http://www.ytj.fi/ (in Finnish)
  case "15":
		c.Body = `Y-tunnus`

  // International Standard Name Identifier. A sixteen digit number. Usually presented with spaces or hyphens dividing the number into four groups of four digits, but in ONIX the spaces or hyphens should be omitted. See http://www.isni.org/
  case "16":
		c.Body = `ISNI`

  // Personennamendatei – person name authority file used by Deutsche Nationalbibliothek and in other German-speaking countries. See http://www.dnb.de/standardisierung/normdateien/pnd.htm (German) or http://www.dnb.de/eng/standardisierung/normdateien/pnd.htm (English). DEPRECATED in favor of the GND
  case "17":
		c.Body = `PND`

  // A control number assigned to a Library of Congress Control Number (LCCN) Name Authority / NACO record
  case "18":
		c.Body = `NACO`

  // Publisher identifier administered by Japanese ISBN Agency
  case "19":
		c.Body = `Japanese Publisher identifier`

  // Gemeinsame Körperschaftsdatei – Corporate Body Authority File in the German-speaking countries. See http://www.dnb.de/standardisierung/normdateien/gkd.htm (German) or http://www.dnb.de/eng/standardisierung/normdateien/gkd.htm (English). DEPRECATED in favor of the GND
  case "20":
		c.Body = `GKD`

  // Open Researcher and Contributor ID. A sixteen digit number. Usually presented with hyphens dividing the number into four groups of four digits, but in ONIX the hyphens should be omitted. See http://www.orcid.org/
  case "21":
		c.Body = `ORCID`

  // Publisher identifier maintained by the Chinese ISBN Agency (GAPP)
  case "22":
		c.Body = `GAPP Publisher Identifier`

  // Identifier for a business organization for VAT purposes, eg within the EU’s VIES system. See http://ec.europa.eu/taxation_customs/vies/faqvies.do for EU VAT ID formats, which vary from country to country. Generally these consist of a two-letter country code followed by the 8–12 digits of the national VAT ID. Some countries include one or two letters within their VAT ID. See http://en.wikipedia.org/wiki/VAT_identification_number for non-EU countries that maintain similar identifiers. Spaces, dashes etc should be omitted
  case "23":
		c.Body = `VAT Identity Number`

  // 4-digit business organization identifier controlled by the Japanese Publication Wholesalers Association
  case "24":
		c.Body = `JP Distribution Identifier`

  // Gemeinsame Normdatei – Joint Authority File in the German-speaking countries. See http://www.dnb.de/EN/gnd (English). Combines the PND, SWD and GKD into a single authority file, and should be used in preference
  case "25":
		c.Body = `GND`

  // Dunn and Bradstreet Universal Numbering System, see http://www.dnb.co.uk/dandb-duns-number
  case "26":
		c.Body = `DUNS`

  // Ringgold organizational identifier, see http://www.ringgold.com/identify.html
  case "27":
		c.Body = `Ringgold ID`

  // French Electre publisher identifier
  case "28":
		c.Body = `Identifiant Editeur Electre`

  // DOI used in EIDR party registry, for example ‘10.5237/C9F6-F41F’ (Sam Raimi). See http://eidr.org
  case "29":
		c.Body = `EIDR Party DOI`

  // French Electre imprint Identifier
  case "30":
		c.Body = `Identifiant Marque Electre`

  // Virtual Internet Authority File. <IDValue> should be a number. The URI form of the identifier can be created by prefixing the number with ‘https://viaf.org/viaf/’. See https://viaf.org
  case "31":
		c.Body = `VIAF ID`

  // DOI used in CrossRef’s Open Funder Registry list of academic research funding bodies, for example ‘10.13039/100004440’ (Wellcome Trust). See http://www.crossref.org/fundingdata/registry.html
  case "32":
		c.Body = `FundRef DOI`

  // Control number assigned to a Name Authority record by the Biblioteca Nacional de España
  case "33":
		c.Body = `BNE CN`

  // Numéro de la notice de personne BNF
  case "34":
		c.Body = `BNF Control Number`

  // Archival Resource Key, as a URL (including the address of the ARK resolver provided by eg a national library)
  case "35":
		c.Body = `ARK`

  // Nasjonalt autoritetsregister for navn – Norwegian national authority file for personal and corporate names. For use in ONIX 3.0 only
  case "36":
		c.Body = `Nasjonalt autoritetsregister`

  // Global Research Identifier Database ID (see https://www.grid.ac). For use in ONIX 3.0 only
  case "37":
		c.Body = `GRID`

  // Party ID from Identifiers and Standards for Higher Education and Research (fr: Identifiants et Référentiels pour l’enseignement supérieur et la recherche). For use on ONIX 3.0 only. See https://www.idref.fr
  case "38":
		c.Body = `IDRef`
	default:
		return fmt.Errorf("undefined code for PublisherIDType has been passed, got [%s]", v)
	}
	return nil
}

// PublishingDateRole Publishing date role
type PublishingDateRole struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *PublishingDateRole) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // Nominal date of publication. This date is primarily used for planning, promotion and other business process purposes, and is not necessarily the first date for retail sales or fulfillment of pre-orders. In the absence of a sales embargo date, retail sales and pre-order fulfillment may begin as soon as stock is available to the retailer
  case "01":
		c.Body = `Publication date`

  // If there is an embargo on retail sales (in the market) before a certain date, the date from which the embargo is lifted and retail sales and fulfillment of pre-orders are permitted. In the absence of an embargo date, retail sales and pre-order fulfillment may begin as soon as stock is available to the retailer
  case "02":
		c.Body = `Sales embargo date`

  // Date when a new product may be announced to the general public. Prior to the announcement date, the product data is intended for internal use by the recipient and supply chain partners only. After the announcement date, or in the absence of an announcement date, the planned product may be announced to the public as soon as metadata is available
  case "09":
		c.Body = `Public announcement date`

  // Date when a new product may be announced to the book trade only. Prior to the announcement date, the product information is intended for internal use by the recipient only. After the announcement date, or in the absence of a trade announcement date, the planned product may be announced to supply chain partners (but not necessarily made public – see the Public announcement date) as soon as metadata is available
  case "10":
		c.Body = `Trade announcement date`

  // Date when the work incorporated in a product was first published. For works in translation, see also Date of first publication in original language (code 20)
  case "11":
		c.Body = `Date of first publication`

  // Date when a product was last reprinted
  case "12":
		c.Body = `Last reprint date`

  // Date when a product was (or will be) declared out-of-print or deleted
  case "13":
		c.Body = `Out-of-print / deletion date`

  // Date when a product was last reissued
  case "16":
		c.Body = `Last reissue date`

  // Date of publication of a printed book which is the direct print counterpart to a digital product. The counterpart product may be included in <RelatedProduct> using code 13
  case "19":
		c.Body = `Publication date of print counterpart`

  // Date when the original language version of work incorporated in a product was first published (note, use only on works in translation – see code 11 for first publication date in the translated language)
  case "20":
		c.Body = `Date of first publication in original language`

  // Date when a product will be reissued
  case "21":
		c.Body = `Forthcoming reissue date`

  // Date when a product that has been temporary withdrawn from sale or recalled for any reason is expected to become available again, eg after correction of quality or technical issues
  case "22":
		c.Body = `Expected availability date after temporary withdrawal`

  // Date from which reviews of a product may be published eg in newspapers and magazines or online. Provided to the book trade for information only: newspapers and magazines are not expected to be recipients of ONIX metadata
  case "23":
		c.Body = `Review embargo date`

  // Latest date on which an order may be placed with the publisher for guaranteed delivery prior to the publication date. May or may not be linked to a special reservation or pre-publication price
  case "25":
		c.Body = `Publisher’s reservation order deadline`

  // Date when a product will be reprinted
  case "26":
		c.Body = `Forthcoming reprint date`

  // Earliest date a retail ‘preorder’ can be placed (in the market), where this is distinct from the public announcement date. In the absence of a preorder embargo, advance orders can be placed as soon as metadata is available to the consumer (this would be the public announcement date, or in the absence of a public announcement date, the earliest date metadata is available to the retailer)
  case "27":
		c.Body = `Preorder embargo date`

  // Date of acquisition of product by new publisher (use with publishing roles 09 and 13)
  case "28":
		c.Body = `Transfer date`

  // For an audiovisual work (eg on DVD)
  case "29":
		c.Body = `Date of production`

  // For digital products that are available to end customers both as a download and streamed, the earliest date the product can be made available on a stream, where the streamed version becomes available later than the download. For the download, see code 02 if it is embargoed or code 01 if there is no embargo
  case "30":
		c.Body = `Streaming embargo date`

  // For digital products that are available to end customers both as purchases and as part of a subscription package, the earliest date the product can be made available by subscription, where the product may not be included in a subscription package until shome while after publication. For ordinary sales, see code 02 if there is a sales embargo or code 01 if there is no embargo
  case "31":
		c.Body = `Subscription embargo date`

  // Date by which CIP copy is required for inclusion in the product
  case "35":
		c.Body = `CIP date`
	default:
		return fmt.Errorf("undefined code for PublishingDateRole has been passed, got [%s]", v)
	}
	return nil
}

// PublishingRole Publishing role
type PublishingRole struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *PublishingRole) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // Publisher
  case "01":
		c.Body = `Publisher`

  // Use where two or more publishers co-publish the exact same product, either under a single ISBN (in which case both publishers are co-publishers), or under different ISBNs (in which case the publisher of THIS ISBN is the publisher and the publishers of OTHER ISBNs are co-publishers. Note this is different from publication of ‘co-editions’
  case "02":
		c.Body = `Co-publisher`

  // Sponsor
  case "03":
		c.Body = `Sponsor`

  // Of a translated work
  case "04":
		c.Body = `Publisher of original-language version`

  // Host/distributor of electronic content
  case "05":
		c.Body = `Host/distributor of electronic content`

  // Published for/on behalf of
  case "06":
		c.Body = `Published for/on behalf of`

  // Use also for ‘Published in cooperation with’
  case "07":
		c.Body = `Published in association with`

  // When ownership of a product or title is transferred from one publisher to another
  case "09":
		c.Body = `New or acquiring publisher`

  // The group to which a publisher (publishing role 01) belongs: use only if a publisher has been identified with role code 01
  case "10":
		c.Body = `Publishing group`

  // The publisher of the edition of which a product is a facsimile
  case "11":
		c.Body = `Publisher of facsimile original`

  // The repackager of a prebound edition that has been assigned its own identifier. (In the US, a ‘prebound edition’ is a book that was previously bound, normally as a paperback, and has been rebound with a library-quality hardcover binding by a supplier other than the original publisher.) Required when the <EditionType> is coded PRB. The original publisher should be named as the ‘publisher’
  case "12":
		c.Body = `Repackager of prebound edition`

  // When ownership of a product or title is transferred from one publisher to another (complement of code 09)
  case "13":
		c.Body = `Former publisher`

  // Body funding publication fees, if different from the body funding the underlying research. Intended primarily for use with open access publications
  case "14":
		c.Body = `Publication funder`

  // Body funding the research on which publication is based, if different from the body funding the publication. Intended primarily for use with open access publications
  case "15":
		c.Body = `Research funder`

  // Body funding research and publication. Intended primarily for use with open access publications
  case "16":
		c.Body = `Funding body`

  // Organisation responsible for printing a printed product. Supplied primarily to meet legal deposit requirements, and may apply only to the first impression. The organisation may also be responsible for binding, when a separate binder is not specified
  case "17":
		c.Body = `Printer`

  // Organisation responsible for binding a printed product (where distinct from the printer). Supplied primarily to meet legal deposit requirements, and may apply only to the first impression
  case "18":
		c.Body = `Binder`

  // Organisation primarily responsible for physical manufacture of a product, when neither Printer nor Binder is directly appropriate (for example, with disc or tape products, or digital products on a physical carrier)
  case "19":
		c.Body = `Manufacturer`
	default:
		return fmt.Errorf("undefined code for PublishingRole has been passed, got [%s]", v)
	}
	return nil
}

// PublishingStatus Publishing status
type PublishingStatus struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *PublishingStatus) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // Status is not specified (as distinct from unknown): the default if the <PublishingStatus> element is not sent. Also to be used in applications where the element is considered mandatory, but the sender of the ONIX message chooses not to pass on status information
  case "00":
		c.Body = `Unspecified`

  // The product was announced, and subsequently abandoned; the <PublicationDate> element in ONIX 2.1 or its equivalent in <PublishingDate> in ONIX 3.0 must not be sent
  case "01":
		c.Body = `Cancelled`

  // Not yet published; must be accompanied by the expected date in <PublicationDate> in ONIX 2.1, or its equivalent in the <PublishingDate> composite in ONIX 3.0
  case "02":
		c.Body = `Forthcoming`

  // The product was announced, and subsequently postponed with no expected publication date; the <PublicationDate> element in ONIX 2.1, or its equivalent as a <PublishingDate> composite in ONIX 3.0, must not be sent
  case "03":
		c.Body = `Postponed indefinitely`

  // The product was published, and is still active in the sense that the publisher will accept orders for it, though it may or may not be immediately available, for which see <SupplyDetail>
  case "04":
		c.Body = `Active`

  // Ownership of the product has been transferred to another publisher (with details of acquiring publisher if possible in PR.19 (ONIX 2.1) OR P.19 (ONIX 3.0))
  case "05":
		c.Body = `No longer our product`

  // The product was active, but is now inactive in the sense that (a) the publisher cannot fulfill orders for it, though stock may still be available elsewhere in the supply chain, and (b) there are no current plans to bring it back into stock. Use this code for ‘reprint under consideration’. Code 06 does not specifically imply that returns are or are not still accepted
  case "06":
		c.Body = `Out of stock indefinitely`

  // The product was active, but is now permanently inactive in the sense that (a) the publisher will not accept orders for it, though stock may still be available elsewhere in the supply chain, and (b) the product will not be made available again under the same ISBN. Code 07 normally implies that the publisher will not accept returns beyond a specified date
  case "07":
		c.Body = `Out of print`

  // The product was active, but is now permanently or indefinitely inactive in the sense that the publisher will not accept orders for it, though stock may still be available elsewhere in the supply chain. Code 08 covers both of codes 06 and 07, and may be used where the distinction between those values is either unnecessary or meaningless
  case "08":
		c.Body = `Inactive`

  // The sender of the ONIX record does not know the current publishing status
  case "09":
		c.Body = `Unknown`

  // The product is no longer available from the current publisher, under the current ISBN, at the current price. It may be available to be traded through another channel. A Publishing Status code 10 ‘Remaindered’ usually but not always means that the publisher has decided to sell off excess inventory of the book. Copies of books that are remaindered are often made available in the supply chain at a reduced price. However, such remainders are often sold under a product identifier that differs from the ISBN on the full-priced copy of the book. A Publishing Status code 10 ‘Remaindered’ on a given product record may or may not be followed by a Publishing Status code 06 ‘Out of Stock Indefinitely’ or 07 ‘Out of Print’: the practise varies from one publisher to another. Some publishers may revert to a Publishing Status code 04 ‘Active’ if a desired inventory level on the product in question has subsequently been reached. No change in rights should ever be inferred from this (or any other) Publishing Status code value
  case "10":
		c.Body = `Remaindered`

  // Withdrawn, typically for legal reasons or to avoid giving offence
  case "11":
		c.Body = `Withdrawn from sale`

  // Recalled for reasons of consumer safety. Deprecated, use code 15 instead
  case "12":
		c.Body = `Recalled`

  // The product is published and active but, as a publishing decision, it is not sold separately – only in an assembly or as part of a pack. Depending on product composition and pricing, it may be saleable separately at retail
  case "13":
		c.Body = `Active, but not sold separately`

  // Recalled for reasons of consumer safety
  case "15":
		c.Body = `Recalled`

  // Withdrawn temporarily, typically for quality or technical reasons. In ONIX 3.0, must be accompanied by expected availability date coded ‘22’ within the <PublishingDate> composite, except in exceptional circumstances where no date is known
  case "16":
		c.Body = `Temporarily withdrawn from sale`

  // Withdrawn permanently from sale in all markets. Effectively synonymous with ‘Out of print’ (code 07), but specific to downloadable and online digital products (where no ‘stock’ would remain in the supply chain)
  case "17":
		c.Body = `Permanently withdrawn from sale`
	default:
		return fmt.Errorf("undefined code for PublishingStatus has been passed, got [%s]", v)
	}
	return nil
}

// QuantityUnit Quantity unit
type QuantityUnit struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *QuantityUnit) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // The quantity refers to a unit implied by the quantity type
  case "00":
		c.Body = `Units`

  // Days
  case "07":
		c.Body = `Days`

  // Weeks
  case "08":
		c.Body = `Weeks`

  // Months
  case "09":
		c.Body = `Months`

  // Years
  case "10":
		c.Body = `Years`
	default:
		return fmt.Errorf("undefined code for QuantityUnit has been passed, got [%s]", v)
	}
	return nil
}

// ROWSalesRightsType Sales rights type
type ROWSalesRightsType struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *ROWSalesRightsType) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // May only be used with the ONIX 3 <ROWSalesRightsType> element
  case "00":
		c.Body = `Sales rights unknown or unstated for any reason`

  // For sale with exclusive rights in the specified countries or territories
  case "01":
		c.Body = `For sale with exclusive rights in the specified countries or territories`

  // For sale with non-exclusive rights in the specified countries or territories
  case "02":
		c.Body = `For sale with non-exclusive rights in the specified countries or territories`

  // Not for sale in the specified countries or territories (reason unspecified)
  case "03":
		c.Body = `Not for sale in the specified countries or territories (reason unspecified)`

  // Not for sale in the specified countries (but publisher holds exclusive rights in those countries or territories)
  case "04":
		c.Body = `Not for sale in the specified countries (but publisher holds exclusive rights in those countries or territories)`

  // Not for sale in the specified countries (publisher holds non-exclusive rights in those countries or territories)
  case "05":
		c.Body = `Not for sale in the specified countries (publisher holds non-exclusive rights in those countries or territories)`

  // Not for sale in the specified countries (because publisher does not hold rights in those countries or territories)
  case "06":
		c.Body = `Not for sale in the specified countries (because publisher does not hold rights in those countries or territories)`

  // Only for use with ONIX 3. Deprecated
  case "07":
		c.Body = `For sale with exclusive rights in the specified countries or territories (sales restriction applies)`

  // Only for use with ONIX 3. Deprecated
  case "08":
		c.Body = `For sale with non-exclusive rights in the specified countries or territories (sales restriction applies)`
	default:
		return fmt.Errorf("undefined code for ROWSalesRightsType has been passed, got [%s]", v)
	}
	return nil
}

// RecordSourceIDType Name identifier type
type RecordSourceIDType struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *RecordSourceIDType) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // Note that <IDTypeName> is required with proprietary identifiers
  case "01":
		c.Body = `Proprietary`

  // DEPRECATED – use 01
  case "02":
		c.Body = `Proprietary`

  // Deutsche Nationalbibliothek publisher identifier
  case "03":
		c.Body = `DNB publisher identifier`

  // Börsenverein Verkehrsnummer
  case "04":
		c.Body = `Börsenverein Verkehrsnummer`

  // German ISBN Agency publisher identifier
  case "05":
		c.Body = `German ISBN Agency publisher identifier`

  // GS1 global location number (formerly EAN location number)
  case "06":
		c.Body = `GLN`

  // Book trade Standard Address Number – US, UK etc
  case "07":
		c.Body = `SAN`

  // MARC code list for organizations – see http://www.loc.gov/marc/organizations/orgshome.html
  case "08":
		c.Body = `MARC organization code`

  // Trading party identifier used in the Netherlands
  case "10":
		c.Body = `Centraal Boekhuis Relatie ID`

  // Flemish publisher code
  case "13":
		c.Body = `Fondscode Boekenbank`

  // Business Identity Code (Finland). See http://www.ytj.fi/ (in Finnish)
  case "15":
		c.Body = `Y-tunnus`

  // International Standard Name Identifier. A sixteen digit number. Usually presented with spaces or hyphens dividing the number into four groups of four digits, but in ONIX the spaces or hyphens should be omitted. See http://www.isni.org/
  case "16":
		c.Body = `ISNI`

  // Personennamendatei – person name authority file used by Deutsche Nationalbibliothek and in other German-speaking countries. See http://www.dnb.de/standardisierung/normdateien/pnd.htm (German) or http://www.dnb.de/eng/standardisierung/normdateien/pnd.htm (English). DEPRECATED in favor of the GND
  case "17":
		c.Body = `PND`

  // A control number assigned to a Library of Congress Control Number (LCCN) Name Authority / NACO record
  case "18":
		c.Body = `NACO`

  // Publisher identifier administered by Japanese ISBN Agency
  case "19":
		c.Body = `Japanese Publisher identifier`

  // Gemeinsame Körperschaftsdatei – Corporate Body Authority File in the German-speaking countries. See http://www.dnb.de/standardisierung/normdateien/gkd.htm (German) or http://www.dnb.de/eng/standardisierung/normdateien/gkd.htm (English). DEPRECATED in favor of the GND
  case "20":
		c.Body = `GKD`

  // Open Researcher and Contributor ID. A sixteen digit number. Usually presented with hyphens dividing the number into four groups of four digits, but in ONIX the hyphens should be omitted. See http://www.orcid.org/
  case "21":
		c.Body = `ORCID`

  // Publisher identifier maintained by the Chinese ISBN Agency (GAPP)
  case "22":
		c.Body = `GAPP Publisher Identifier`

  // Identifier for a business organization for VAT purposes, eg within the EU’s VIES system. See http://ec.europa.eu/taxation_customs/vies/faqvies.do for EU VAT ID formats, which vary from country to country. Generally these consist of a two-letter country code followed by the 8–12 digits of the national VAT ID. Some countries include one or two letters within their VAT ID. See http://en.wikipedia.org/wiki/VAT_identification_number for non-EU countries that maintain similar identifiers. Spaces, dashes etc should be omitted
  case "23":
		c.Body = `VAT Identity Number`

  // 4-digit business organization identifier controlled by the Japanese Publication Wholesalers Association
  case "24":
		c.Body = `JP Distribution Identifier`

  // Gemeinsame Normdatei – Joint Authority File in the German-speaking countries. See http://www.dnb.de/EN/gnd (English). Combines the PND, SWD and GKD into a single authority file, and should be used in preference
  case "25":
		c.Body = `GND`

  // Dunn and Bradstreet Universal Numbering System, see http://www.dnb.co.uk/dandb-duns-number
  case "26":
		c.Body = `DUNS`

  // Ringgold organizational identifier, see http://www.ringgold.com/identify.html
  case "27":
		c.Body = `Ringgold ID`

  // French Electre publisher identifier
  case "28":
		c.Body = `Identifiant Editeur Electre`

  // DOI used in EIDR party registry, for example ‘10.5237/C9F6-F41F’ (Sam Raimi). See http://eidr.org
  case "29":
		c.Body = `EIDR Party DOI`

  // French Electre imprint Identifier
  case "30":
		c.Body = `Identifiant Marque Electre`

  // Virtual Internet Authority File. <IDValue> should be a number. The URI form of the identifier can be created by prefixing the number with ‘https://viaf.org/viaf/’. See https://viaf.org
  case "31":
		c.Body = `VIAF ID`

  // DOI used in CrossRef’s Open Funder Registry list of academic research funding bodies, for example ‘10.13039/100004440’ (Wellcome Trust). See http://www.crossref.org/fundingdata/registry.html
  case "32":
		c.Body = `FundRef DOI`

  // Control number assigned to a Name Authority record by the Biblioteca Nacional de España
  case "33":
		c.Body = `BNE CN`

  // Numéro de la notice de personne BNF
  case "34":
		c.Body = `BNF Control Number`

  // Archival Resource Key, as a URL (including the address of the ARK resolver provided by eg a national library)
  case "35":
		c.Body = `ARK`

  // Nasjonalt autoritetsregister for navn – Norwegian national authority file for personal and corporate names. For use in ONIX 3.0 only
  case "36":
		c.Body = `Nasjonalt autoritetsregister`

  // Global Research Identifier Database ID (see https://www.grid.ac). For use in ONIX 3.0 only
  case "37":
		c.Body = `GRID`

  // Party ID from Identifiers and Standards for Higher Education and Research (fr: Identifiants et Référentiels pour l’enseignement supérieur et la recherche). For use on ONIX 3.0 only. See https://www.idref.fr
  case "38":
		c.Body = `IDRef`
	default:
		return fmt.Errorf("undefined code for RecordSourceIDType has been passed, got [%s]", v)
	}
	return nil
}

// RecordSourceType Record source type
type RecordSourceType struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *RecordSourceType) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // Unspecified
  case "00":
		c.Body = `Unspecified`

  // Publisher
  case "01":
		c.Body = `Publisher`

  // Use to designate a distributor providing primary warehousing and fulfillment for a publisher or for a publisher’s sales agent, as distinct from a wholesaler
  case "02":
		c.Body = `Publisher’s distributor`

  // Wholesaler
  case "03":
		c.Body = `Wholesaler`

  // Bibliographic data aggregator
  case "04":
		c.Body = `Bibliographic agency`

  // Library supplier. Bookseller selling to libraries (including academic libraries)
  case "05":
		c.Body = `Library bookseller`

  // Use for a publisher’s sales agent responsible for marketing the publisher’s products within a territory, as opposed to a publisher’s distributor who fulfills orders but does not market
  case "06":
		c.Body = `Publisher’s sales agent`

  // Downstream provider of e-publication format conversion services (who might also be a distributor or retailer of the converted e-publication), supplying metadata on behalf of the publisher. The assigned ISBN is taken from the publisher’s ISBN prefix
  case "07":
		c.Body = `Publisher’s conversion service provider`

  // Downstream provider of e-publication format conversion services (who might also be a distributor or retailer of the converted e-publication), supplying metadata on behalf of the publisher. The assigned ISBN is taken from the service provider’s prefix (whether or not the service provider dedicates that prefix to a particular publisher)
  case "08":
		c.Body = `Conversion service provider`

  // ISBN Registration Agency
  case "09":
		c.Body = `ISBN Registration Agency`

  // ISTC Registration Agency
  case "10":
		c.Body = `ISTC Registration Agency`

  // Bookseller selling primarily to consumers
  case "11":
		c.Body = `Retail bookseller`

  // Bookseller selling primarily to educational institutions
  case "12":
		c.Body = `Education bookseller`

  // Library service providing enhanced metadata to publishers or other parties
  case "13":
		c.Body = `Library`
	default:
		return fmt.Errorf("undefined code for RecordSourceType has been passed, got [%s]", v)
	}
	return nil
}

// RegionCode Region – based on ISO 3166-2
type RegionCode struct {
	Body []string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *RegionCode) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	codes := strings.Split(v, " ")
	tmpeCodes := []string{}
	for _, code := range codes {
		switch code {

		// Australian Capital Territory
		case "AU-CT":
			tmpeCodes = append(tmpeCodes, `Australian Capital Territory`)

		// New South Wales
		case "AU-NS":
			tmpeCodes = append(tmpeCodes, `New South Wales`)

		// Northern Territory
		case "AU-NT":
			tmpeCodes = append(tmpeCodes, `Northern Territory`)

		// Queensland
		case "AU-QL":
			tmpeCodes = append(tmpeCodes, `Queensland`)

		// South Australia
		case "AU-SA":
			tmpeCodes = append(tmpeCodes, `South Australia`)

		// Tasmania
		case "AU-TS":
			tmpeCodes = append(tmpeCodes, `Tasmania`)

		// Victoria
		case "AU-VI":
			tmpeCodes = append(tmpeCodes, `Victoria`)

		// Western Australia
		case "AU-WA":
			tmpeCodes = append(tmpeCodes, `Western Australia`)

		// For use in ONIX 3.0 only
		case "BE-BRU":
			tmpeCodes = append(tmpeCodes, `Brussels-Capital Region`)

		// For use in ONIX 3.0 only
		case "BE-VLG":
			tmpeCodes = append(tmpeCodes, `Flemish Region`)

		// For use in ONIX 3.0 only
		case "BE-WAL":
			tmpeCodes = append(tmpeCodes, `Walloon Region`)

		// Alberta
		case "CA-AB":
			tmpeCodes = append(tmpeCodes, `Alberta`)

		// British Columbia
		case "CA-BC":
			tmpeCodes = append(tmpeCodes, `British Columbia`)

		// Manitoba
		case "CA-MB":
			tmpeCodes = append(tmpeCodes, `Manitoba`)

		// New Brunswick
		case "CA-NB":
			tmpeCodes = append(tmpeCodes, `New Brunswick`)

		// Newfoundland and Labrador
		case "CA-NL":
			tmpeCodes = append(tmpeCodes, `Newfoundland and Labrador`)

		// Nova Scotia
		case "CA-NS":
			tmpeCodes = append(tmpeCodes, `Nova Scotia`)

		// Northwest Territories
		case "CA-NT":
			tmpeCodes = append(tmpeCodes, `Northwest Territories`)

		// Nunavut
		case "CA-NU":
			tmpeCodes = append(tmpeCodes, `Nunavut`)

		// Ontario
		case "CA-ON":
			tmpeCodes = append(tmpeCodes, `Ontario`)

		// Prince Edward Island
		case "CA-PE":
			tmpeCodes = append(tmpeCodes, `Prince Edward Island`)

		// Quebec
		case "CA-QC":
			tmpeCodes = append(tmpeCodes, `Quebec`)

		// Saskatchewan
		case "CA-SK":
			tmpeCodes = append(tmpeCodes, `Saskatchewan`)

		// Yukon Territory
		case "CA-YT":
			tmpeCodes = append(tmpeCodes, `Yukon Territory`)

		// For use in ONIX 3.0 only
		case "CN-BJ":
			tmpeCodes = append(tmpeCodes, `Beijing Municipality`)

		// For use in ONIX 3.0 only
		case "CN-TJ":
			tmpeCodes = append(tmpeCodes, `Tianjin Municipality`)

		// For use in ONIX 3.0 only
		case "CN-HE":
			tmpeCodes = append(tmpeCodes, `Hebei Province`)

		// For use in ONIX 3.0 only
		case "CN-SX":
			tmpeCodes = append(tmpeCodes, `Shanxi Province`)

		// For use in ONIX 3.0 only
		case "CN-NM":
			tmpeCodes = append(tmpeCodes, `Inner Mongolia Autonomous Region`)

		// For use in ONIX 3.0 only
		case "CN-LN":
			tmpeCodes = append(tmpeCodes, `Liaoning Province`)

		// For use in ONIX 3.0 only
		case "CN-JL":
			tmpeCodes = append(tmpeCodes, `Jilin Province`)

		// For use in ONIX 3.0 only
		case "CN-HL":
			tmpeCodes = append(tmpeCodes, `Heilongjiang Province`)

		// For use in ONIX 3.0 only
		case "CN-SH":
			tmpeCodes = append(tmpeCodes, `Shanghai Municipality`)

		// For use in ONIX 3.0 only
		case "CN-JS":
			tmpeCodes = append(tmpeCodes, `Jiangsu Province`)

		// For use in ONIX 3.0 only
		case "CN-ZJ":
			tmpeCodes = append(tmpeCodes, `Zhejiang Province`)

		// For use in ONIX 3.0 only
		case "CN-AH":
			tmpeCodes = append(tmpeCodes, `Anhui Province`)

		// For use in ONIX 3.0 only
		case "CN-FJ":
			tmpeCodes = append(tmpeCodes, `Fujian Province`)

		// For use in ONIX 3.0 only
		case "CN-JX":
			tmpeCodes = append(tmpeCodes, `Jiangxi Province`)

		// For use in ONIX 3.0 only
		case "CN-SD":
			tmpeCodes = append(tmpeCodes, `Shandong Province`)

		// For use in ONIX 3.0 only
		case "CN-HA":
			tmpeCodes = append(tmpeCodes, `Henan Province`)

		// For use in ONIX 3.0 only
		case "CN-HB":
			tmpeCodes = append(tmpeCodes, `Hubei Province`)

		// For use in ONIX 3.0 only
		case "CN-HN":
			tmpeCodes = append(tmpeCodes, `Hunan Province`)

		// For use in ONIX 3.0 only
		case "CN-GD":
			tmpeCodes = append(tmpeCodes, `Guangdong Province`)

		// For use in ONIX 3.0 only
		case "CN-GX":
			tmpeCodes = append(tmpeCodes, `Guangxi Zhuang Autonomous Region`)

		// For use in ONIX 3.0 only
		case "CN-HI":
			tmpeCodes = append(tmpeCodes, `Hainan Province`)

		// For use in ONIX 3.0 only
		case "CN-CQ":
			tmpeCodes = append(tmpeCodes, `Chongqing Municipality`)

		// For use in ONIX 3.0 only
		case "CN-SC":
			tmpeCodes = append(tmpeCodes, `Sichuan Province`)

		// For use in ONIX 3.0 only
		case "CN-GZ":
			tmpeCodes = append(tmpeCodes, `Guizhou Province`)

		// For use in ONIX 3.0 only
		case "CN-YN":
			tmpeCodes = append(tmpeCodes, `Yunnan Province`)

		// For use in ONIX 3.0 only
		case "CN-XZ":
			tmpeCodes = append(tmpeCodes, `Tibet Autonomous Region`)

		// For use in ONIX 3.0 only
		case "CN-SN":
			tmpeCodes = append(tmpeCodes, `Shaanxi Province`)

		// For use in ONIX 3.0 only
		case "CN-GS":
			tmpeCodes = append(tmpeCodes, `Gansu Province`)

		// For use in ONIX 3.0 only
		case "CN-QH":
			tmpeCodes = append(tmpeCodes, `Qinghai Province`)

		// For use in ONIX 3.0 only
		case "CN-NX":
			tmpeCodes = append(tmpeCodes, `Ningxia Hui Autonomous Region`)

		// For use in ONIX 3.0 only
		case "CN-XJ":
			tmpeCodes = append(tmpeCodes, `Xinjiang Uyghur Autonomous Region`)

		// Prefer code TW (Taiwan, Province of China) from List 91. For use in ONIX 3.0 only
		case "CN-TW":
			tmpeCodes = append(tmpeCodes, `Taiwan Province`)

		// Prefer code HK (Hong Kong) from List 91. For use in ONIX 3.0 only
		case "CN-HK":
			tmpeCodes = append(tmpeCodes, `Hong Kong Special Administrative Region`)

		// Prefer code MO (Macao) from List 91. For use in ONIX 3.0 only
		case "CN-MO":
			tmpeCodes = append(tmpeCodes, `Macau Special Administrative Region`)

		// Deprecated in favor of CN-BJ
		case "CN-11":
			tmpeCodes = append(tmpeCodes, `Beijing Municipality`)

		// Deprecated in favor of CN-TJ
		case "CN-12":
			tmpeCodes = append(tmpeCodes, `Tianjin Municipality`)

		// Deprecated in favor of CN-HE
		case "CN-13":
			tmpeCodes = append(tmpeCodes, `Hebei Province`)

		// Deprecated in favor of CN-SX
		case "CN-14":
			tmpeCodes = append(tmpeCodes, `Shanxi Province`)

		// Deprecated in favor of CN-NM
		case "CN-15":
			tmpeCodes = append(tmpeCodes, `Inner Mongolia Autonomous Region`)

		// Deprecated in favor of CN-LN
		case "CN-21":
			tmpeCodes = append(tmpeCodes, `Liaoning Province`)

		// Deprecated in favor of CN-JL
		case "CN-22":
			tmpeCodes = append(tmpeCodes, `Jilin Province`)

		// Deprecated in favor of CN-HL
		case "CN-23":
			tmpeCodes = append(tmpeCodes, `Heilongjiang Province`)

		// Deprecated in favor of CN-SH
		case "CN-31":
			tmpeCodes = append(tmpeCodes, `Shanghai Municipality`)

		// Deprecated in favor of CN-JS
		case "CN-32":
			tmpeCodes = append(tmpeCodes, `Jiangsu Province`)

		// Deprecated in favor of CN-ZJ
		case "CN-33":
			tmpeCodes = append(tmpeCodes, `Zhejiang Province`)

		// Deprecated in favor of CN-AH
		case "CN-34":
			tmpeCodes = append(tmpeCodes, `Anhui Province`)

		// Deprecated in favor of CN-FJ
		case "CN-35":
			tmpeCodes = append(tmpeCodes, `Fujian Province`)

		// Deprecated in favor of CN-JX
		case "CN-36":
			tmpeCodes = append(tmpeCodes, `Jiangxi Province`)

		// Deprecated in favor of CN-SD
		case "CN-37":
			tmpeCodes = append(tmpeCodes, `Shandong Province`)

		// Deprecated in favor of CN-HA
		case "CN-41":
			tmpeCodes = append(tmpeCodes, `Henan Province`)

		// Deprecated in favor of CN-HB
		case "CN-42":
			tmpeCodes = append(tmpeCodes, `Hubei Province`)

		// Deprecated in favor of CN-HN
		case "CN-43":
			tmpeCodes = append(tmpeCodes, `Hunan Province`)

		// Deprecated in favor of CN-GD
		case "CN-44":
			tmpeCodes = append(tmpeCodes, `Guangdong Province`)

		// Deprecated in favor of CN-GX
		case "CN-45":
			tmpeCodes = append(tmpeCodes, `Guangxi Zhuang Autonomous Region`)

		// Deprecated in favor of CN-HI
		case "CN-46":
			tmpeCodes = append(tmpeCodes, `Hainan Province`)

		// Deprecated in favor of CN-CQ
		case "CN-50":
			tmpeCodes = append(tmpeCodes, `Chongqing Municipality`)

		// Deprecated in favor of CN-SC
		case "CN-51":
			tmpeCodes = append(tmpeCodes, `Sichuan Province`)

		// Deprecated in favor of CN-GZ
		case "CN-52":
			tmpeCodes = append(tmpeCodes, `Guizhou Province`)

		// Deprecated in favor of CN-YN
		case "CN-53":
			tmpeCodes = append(tmpeCodes, `Yunnan Province`)

		// Deprecated in favor of CN-XZ
		case "CN-54":
			tmpeCodes = append(tmpeCodes, `Tibet Autonomous Region`)

		// Deprecated in favor of CN-SN
		case "CN-61":
			tmpeCodes = append(tmpeCodes, `Shaanxi Province`)

		// Deprecated in favor of CN-GS
		case "CN-62":
			tmpeCodes = append(tmpeCodes, `Gansu Province`)

		// Deprecated in favor of CN-QH
		case "CN-63":
			tmpeCodes = append(tmpeCodes, `Qinghai Province`)

		// Deprecated in favor of CN-NX
		case "CN-64":
			tmpeCodes = append(tmpeCodes, `Ningxia Hui Autonomous Region`)

		// Deprecated in favor of CN-XJ
		case "CN-65":
			tmpeCodes = append(tmpeCodes, `Xinjiang Uyghur Autonomous Region`)

		// Deprecated in favor of CN-TW, but prefer code TW (Taiwan, Province of China) from List 91
		case "CN-71":
			tmpeCodes = append(tmpeCodes, `Taiwan Province`)

		// Deprecated in favor of CN-HK, but prefer code HK (Hong Kong) from List 91
		case "CN-91":
			tmpeCodes = append(tmpeCodes, `Hong Kong Special Administrative Region`)

		// Deprecated in favor of CN-MO, but prefer code MO (Macao) from List 91
		case "CN-92":
			tmpeCodes = append(tmpeCodes, `Macau Special Administrative Region`)

		// Canary Islands
		case "ES-CN":
			tmpeCodes = append(tmpeCodes, `Canary Islands`)

		// Corsica
		case "FR-H":
			tmpeCodes = append(tmpeCodes, `Corsica`)

		// Airside outlets at UK international airports only
		case "GB-AIR":
			tmpeCodes = append(tmpeCodes, `UK airside`)

		// All UK airports, including both airside and other outlets
		case "GB-APS":
			tmpeCodes = append(tmpeCodes, `UK airports`)

		// DEPRECATED, replaced by country codes GG – Guernsey, and JE – Jersey from List 91
		case "GB-CHA":
			tmpeCodes = append(tmpeCodes, `Channel Islands`)

		// England
		case "GB-ENG":
			tmpeCodes = append(tmpeCodes, `England`)

		// UK excluding Northern Ireland. DEPRECATED – use separate region codes GB-ENG, GB-SCT, GB-WLS instead
		case "GB-EWS":
			tmpeCodes = append(tmpeCodes, `England, Wales, Scotland`)

		// DEPRECATED, replaced by country code IM – Isle of Man from List 91
		case "GB-IOM":
			tmpeCodes = append(tmpeCodes, `Isle of Man`)

		// Northern Ireland
		case "GB-NIR":
			tmpeCodes = append(tmpeCodes, `Northern Ireland`)

		// Scotland
		case "GB-SCT":
			tmpeCodes = append(tmpeCodes, `Scotland`)

		// Wales
		case "GB-WLS":
			tmpeCodes = append(tmpeCodes, `Wales`)

		// Airside outlets at Irish international airports only
		case "IE-AIR":
			tmpeCodes = append(tmpeCodes, `Ireland airside`)

		// Agrigento
		case "IT-AG":
			tmpeCodes = append(tmpeCodes, `Agrigento`)

		// Alessandria
		case "IT-AL":
			tmpeCodes = append(tmpeCodes, `Alessandria`)

		// Ancona
		case "IT-AN":
			tmpeCodes = append(tmpeCodes, `Ancona`)

		// Aosta
		case "IT-AO":
			tmpeCodes = append(tmpeCodes, `Aosta`)

		// Arezzo
		case "IT-AR":
			tmpeCodes = append(tmpeCodes, `Arezzo`)

		// Ascoli Piceno
		case "IT-AP":
			tmpeCodes = append(tmpeCodes, `Ascoli Piceno`)

		// Asti
		case "IT-AT":
			tmpeCodes = append(tmpeCodes, `Asti`)

		// Avellino
		case "IT-AV":
			tmpeCodes = append(tmpeCodes, `Avellino`)

		// Bari
		case "IT-BA":
			tmpeCodes = append(tmpeCodes, `Bari`)

		// Barletta-Andria-Trani
		case "IT-BT":
			tmpeCodes = append(tmpeCodes, `Barletta-Andria-Trani`)

		// Belluno
		case "IT-BL":
			tmpeCodes = append(tmpeCodes, `Belluno`)

		// Benevento
		case "IT-BN":
			tmpeCodes = append(tmpeCodes, `Benevento`)

		// Bergamo
		case "IT-BG":
			tmpeCodes = append(tmpeCodes, `Bergamo`)

		// Biella
		case "IT-BI":
			tmpeCodes = append(tmpeCodes, `Biella`)

		// Bologna
		case "IT-BO":
			tmpeCodes = append(tmpeCodes, `Bologna`)

		// Bolzano
		case "IT-BZ":
			tmpeCodes = append(tmpeCodes, `Bolzano`)

		// Brescia
		case "IT-BS":
			tmpeCodes = append(tmpeCodes, `Brescia`)

		// Brindisi
		case "IT-BR":
			tmpeCodes = append(tmpeCodes, `Brindisi`)

		// Cagliari
		case "IT-CA":
			tmpeCodes = append(tmpeCodes, `Cagliari`)

		// Caltanissetta
		case "IT-CL":
			tmpeCodes = append(tmpeCodes, `Caltanissetta`)

		// Campobasso
		case "IT-CB":
			tmpeCodes = append(tmpeCodes, `Campobasso`)

		// Carbonia-Iglesias
		case "IT-CI":
			tmpeCodes = append(tmpeCodes, `Carbonia-Iglesias`)

		// Caserta
		case "IT-CE":
			tmpeCodes = append(tmpeCodes, `Caserta`)

		// Catania
		case "IT-CT":
			tmpeCodes = append(tmpeCodes, `Catania`)

		// Catanzaro
		case "IT-CZ":
			tmpeCodes = append(tmpeCodes, `Catanzaro`)

		// Chieti
		case "IT-CH":
			tmpeCodes = append(tmpeCodes, `Chieti`)

		// Como
		case "IT-CO":
			tmpeCodes = append(tmpeCodes, `Como`)

		// Cosenza
		case "IT-CS":
			tmpeCodes = append(tmpeCodes, `Cosenza`)

		// Cremona
		case "IT-CR":
			tmpeCodes = append(tmpeCodes, `Cremona`)

		// Crotone
		case "IT-KR":
			tmpeCodes = append(tmpeCodes, `Crotone`)

		// Cuneo
		case "IT-CN":
			tmpeCodes = append(tmpeCodes, `Cuneo`)

		// Enna
		case "IT-EN":
			tmpeCodes = append(tmpeCodes, `Enna`)

		// Fermo
		case "IT-FM":
			tmpeCodes = append(tmpeCodes, `Fermo`)

		// Ferrara
		case "IT-FE":
			tmpeCodes = append(tmpeCodes, `Ferrara`)

		// Firenze
		case "IT-FI":
			tmpeCodes = append(tmpeCodes, `Firenze`)

		// Foggia
		case "IT-FG":
			tmpeCodes = append(tmpeCodes, `Foggia`)

		// Forlì-Cesena
		case "IT-FC":
			tmpeCodes = append(tmpeCodes, `Forlì-Cesena`)

		// Frosinone
		case "IT-FR":
			tmpeCodes = append(tmpeCodes, `Frosinone`)

		// Genova
		case "IT-GE":
			tmpeCodes = append(tmpeCodes, `Genova`)

		// Gorizia
		case "IT-GO":
			tmpeCodes = append(tmpeCodes, `Gorizia`)

		// Grosseto
		case "IT-GR":
			tmpeCodes = append(tmpeCodes, `Grosseto`)

		// Imperia
		case "IT-IM":
			tmpeCodes = append(tmpeCodes, `Imperia`)

		// Isernia
		case "IT-IS":
			tmpeCodes = append(tmpeCodes, `Isernia`)

		// La Spezia
		case "IT-SP":
			tmpeCodes = append(tmpeCodes, `La Spezia`)

		// L’Aquila
		case "IT-AQ":
			tmpeCodes = append(tmpeCodes, `L’Aquila`)

		// Latina
		case "IT-LT":
			tmpeCodes = append(tmpeCodes, `Latina`)

		// Lecce
		case "IT-LE":
			tmpeCodes = append(tmpeCodes, `Lecce`)

		// Lecco
		case "IT-LC":
			tmpeCodes = append(tmpeCodes, `Lecco`)

		// Livorno
		case "IT-LI":
			tmpeCodes = append(tmpeCodes, `Livorno`)

		// Lodi
		case "IT-LO":
			tmpeCodes = append(tmpeCodes, `Lodi`)

		// Lucca
		case "IT-LU":
			tmpeCodes = append(tmpeCodes, `Lucca`)

		// Macerata
		case "IT-MC":
			tmpeCodes = append(tmpeCodes, `Macerata`)

		// Mantova
		case "IT-MN":
			tmpeCodes = append(tmpeCodes, `Mantova`)

		// Massa-Carrara
		case "IT-MS":
			tmpeCodes = append(tmpeCodes, `Massa-Carrara`)

		// Matera
		case "IT-MT":
			tmpeCodes = append(tmpeCodes, `Matera`)

		// Medio Campidano
		case "IT-VS":
			tmpeCodes = append(tmpeCodes, `Medio Campidano`)

		// Messina
		case "IT-ME":
			tmpeCodes = append(tmpeCodes, `Messina`)

		// Milano
		case "IT-MI":
			tmpeCodes = append(tmpeCodes, `Milano`)

		// Modena
		case "IT-MO":
			tmpeCodes = append(tmpeCodes, `Modena`)

		// Monza e Brianza
		case "IT-MB":
			tmpeCodes = append(tmpeCodes, `Monza e Brianza`)

		// Napoli
		case "IT-NA":
			tmpeCodes = append(tmpeCodes, `Napoli`)

		// Novara
		case "IT-NO":
			tmpeCodes = append(tmpeCodes, `Novara`)

		// Nuoro
		case "IT-NU":
			tmpeCodes = append(tmpeCodes, `Nuoro`)

		// Ogliastra
		case "IT-OG":
			tmpeCodes = append(tmpeCodes, `Ogliastra`)

		// Olbia-Tempio
		case "IT-OT":
			tmpeCodes = append(tmpeCodes, `Olbia-Tempio`)

		// Oristano
		case "IT-OR":
			tmpeCodes = append(tmpeCodes, `Oristano`)

		// Padova
		case "IT-PD":
			tmpeCodes = append(tmpeCodes, `Padova`)

		// Palermo
		case "IT-PA":
			tmpeCodes = append(tmpeCodes, `Palermo`)

		// Parma
		case "IT-PR":
			tmpeCodes = append(tmpeCodes, `Parma`)

		// Pavia
		case "IT-PV":
			tmpeCodes = append(tmpeCodes, `Pavia`)

		// Perugia
		case "IT-PG":
			tmpeCodes = append(tmpeCodes, `Perugia`)

		// Pesaro e Urbino
		case "IT-PU":
			tmpeCodes = append(tmpeCodes, `Pesaro e Urbino`)

		// Pescara
		case "IT-PE":
			tmpeCodes = append(tmpeCodes, `Pescara`)

		// Piacenza
		case "IT-PC":
			tmpeCodes = append(tmpeCodes, `Piacenza`)

		// Pisa
		case "IT-PI":
			tmpeCodes = append(tmpeCodes, `Pisa`)

		// Pistoia
		case "IT-PT":
			tmpeCodes = append(tmpeCodes, `Pistoia`)

		// Pordenone
		case "IT-PN":
			tmpeCodes = append(tmpeCodes, `Pordenone`)

		// Potenza
		case "IT-PZ":
			tmpeCodes = append(tmpeCodes, `Potenza`)

		// Prato
		case "IT-PO":
			tmpeCodes = append(tmpeCodes, `Prato`)

		// Ragusa
		case "IT-RG":
			tmpeCodes = append(tmpeCodes, `Ragusa`)

		// Ravenna
		case "IT-RA":
			tmpeCodes = append(tmpeCodes, `Ravenna`)

		// Reggio Calabria
		case "IT-RC":
			tmpeCodes = append(tmpeCodes, `Reggio Calabria`)

		// Reggio Emilia
		case "IT-RE":
			tmpeCodes = append(tmpeCodes, `Reggio Emilia`)

		// Rieti
		case "IT-RI":
			tmpeCodes = append(tmpeCodes, `Rieti`)

		// Rimini
		case "IT-RN":
			tmpeCodes = append(tmpeCodes, `Rimini`)

		// Roma
		case "IT-RM":
			tmpeCodes = append(tmpeCodes, `Roma`)

		// Rovigo
		case "IT-RO":
			tmpeCodes = append(tmpeCodes, `Rovigo`)

		// Salerno
		case "IT-SA":
			tmpeCodes = append(tmpeCodes, `Salerno`)

		// Sassari
		case "IT-SS":
			tmpeCodes = append(tmpeCodes, `Sassari`)

		// Savona
		case "IT-SV":
			tmpeCodes = append(tmpeCodes, `Savona`)

		// Siena
		case "IT-SI":
			tmpeCodes = append(tmpeCodes, `Siena`)

		// Siracusa
		case "IT-SR":
			tmpeCodes = append(tmpeCodes, `Siracusa`)

		// Sondrio
		case "IT-SO":
			tmpeCodes = append(tmpeCodes, `Sondrio`)

		// Taranto
		case "IT-TA":
			tmpeCodes = append(tmpeCodes, `Taranto`)

		// Teramo
		case "IT-TE":
			tmpeCodes = append(tmpeCodes, `Teramo`)

		// Terni
		case "IT-TR":
			tmpeCodes = append(tmpeCodes, `Terni`)

		// Torino
		case "IT-TO":
			tmpeCodes = append(tmpeCodes, `Torino`)

		// Trapani
		case "IT-TP":
			tmpeCodes = append(tmpeCodes, `Trapani`)

		// Trento
		case "IT-TN":
			tmpeCodes = append(tmpeCodes, `Trento`)

		// Treviso
		case "IT-TV":
			tmpeCodes = append(tmpeCodes, `Treviso`)

		// Trieste
		case "IT-TS":
			tmpeCodes = append(tmpeCodes, `Trieste`)

		// Udine
		case "IT-UD":
			tmpeCodes = append(tmpeCodes, `Udine`)

		// Varese
		case "IT-VA":
			tmpeCodes = append(tmpeCodes, `Varese`)

		// Venezia
		case "IT-VE":
			tmpeCodes = append(tmpeCodes, `Venezia`)

		// Verbano-Cusio-Ossola
		case "IT-VB":
			tmpeCodes = append(tmpeCodes, `Verbano-Cusio-Ossola`)

		// Vercelli
		case "IT-VC":
			tmpeCodes = append(tmpeCodes, `Vercelli`)

		// Verona
		case "IT-VR":
			tmpeCodes = append(tmpeCodes, `Verona`)

		// Vibo Valentia
		case "IT-VV":
			tmpeCodes = append(tmpeCodes, `Vibo Valentia`)

		// Vicenza
		case "IT-VI":
			tmpeCodes = append(tmpeCodes, `Vicenza`)

		// Viterbo
		case "IT-VT":
			tmpeCodes = append(tmpeCodes, `Viterbo`)

		// Kosovo-Metohija
		case "RS-KM":
			tmpeCodes = append(tmpeCodes, `Kosovo-Metohija`)

		// Vojvodina
		case "RS-VO":
			tmpeCodes = append(tmpeCodes, `Vojvodina`)

		// Republic of Adygeya
		case "RU-AD":
			tmpeCodes = append(tmpeCodes, `Republic of Adygeya`)

		// Republic of Altay
		case "RU-AL":
			tmpeCodes = append(tmpeCodes, `Republic of Altay`)

		// Republic of Bashkortostan
		case "RU-BA":
			tmpeCodes = append(tmpeCodes, `Republic of Bashkortostan`)

		// Republic of Buryatiya
		case "RU-BU":
			tmpeCodes = append(tmpeCodes, `Republic of Buryatiya`)

		// Chechenskaya Republic
		case "RU-CE":
			tmpeCodes = append(tmpeCodes, `Chechenskaya Republic`)

		// Chuvashskaya Republic
		case "RU-CU":
			tmpeCodes = append(tmpeCodes, `Chuvashskaya Republic`)

		// Republic of Dagestan
		case "RU-DA":
			tmpeCodes = append(tmpeCodes, `Republic of Dagestan`)

		// Republic of Ingushetiya
		case "RU-IN":
			tmpeCodes = append(tmpeCodes, `Republic of Ingushetiya`)

		// Kabardino-Balkarskaya Republic
		case "RU-KB":
			tmpeCodes = append(tmpeCodes, `Kabardino-Balkarskaya Republic`)

		// Republic of Kalmykiya
		case "RU-KL":
			tmpeCodes = append(tmpeCodes, `Republic of Kalmykiya`)

		// Karachayevo-Cherkesskaya Republic
		case "RU-KC":
			tmpeCodes = append(tmpeCodes, `Karachayevo-Cherkesskaya Republic`)

		// Republic of Kareliya
		case "RU-KR":
			tmpeCodes = append(tmpeCodes, `Republic of Kareliya`)

		// Republic of Khakasiya
		case "RU-KK":
			tmpeCodes = append(tmpeCodes, `Republic of Khakasiya`)

		// Republic of Komi
		case "RU-KO":
			tmpeCodes = append(tmpeCodes, `Republic of Komi`)

		// Republic of Mariy El
		case "RU-ME":
			tmpeCodes = append(tmpeCodes, `Republic of Mariy El`)

		// Republic of Mordoviya
		case "RU-MO":
			tmpeCodes = append(tmpeCodes, `Republic of Mordoviya`)

		// Republic of Sakha (Yakutiya)
		case "RU-SA":
			tmpeCodes = append(tmpeCodes, `Republic of Sakha (Yakutiya)`)

		// Republic of Severnaya Osetiya-Alaniya
		case "RU-SE":
			tmpeCodes = append(tmpeCodes, `Republic of Severnaya Osetiya-Alaniya`)

		// Republic of Tatarstan
		case "RU-TA":
			tmpeCodes = append(tmpeCodes, `Republic of Tatarstan`)

		// Republic of Tyva (Tuva)
		case "RU-TY":
			tmpeCodes = append(tmpeCodes, `Republic of Tyva (Tuva)`)

		// Udmurtskaya Republic
		case "RU-UD":
			tmpeCodes = append(tmpeCodes, `Udmurtskaya Republic`)

		// Altayskiy Administrative Territory
		case "RU-ALT":
			tmpeCodes = append(tmpeCodes, `Altayskiy Administrative Territory`)

		// Kamchatskiy Administrative Territory
		case "RU-KAM":
			tmpeCodes = append(tmpeCodes, `Kamchatskiy Administrative Territory`)

		// Khabarovskiy Administrative Territory
		case "RU-KHA":
			tmpeCodes = append(tmpeCodes, `Khabarovskiy Administrative Territory`)

		// Krasnodarskiy Administrative Territory
		case "RU-KDA":
			tmpeCodes = append(tmpeCodes, `Krasnodarskiy Administrative Territory`)

		// Krasnoyarskiy Administrative Territory
		case "RU-KYA":
			tmpeCodes = append(tmpeCodes, `Krasnoyarskiy Administrative Territory`)

		// Permskiy Administrative Territory
		case "RU-PER":
			tmpeCodes = append(tmpeCodes, `Permskiy Administrative Territory`)

		// Primorskiy Administrative Territory
		case "RU-PRI":
			tmpeCodes = append(tmpeCodes, `Primorskiy Administrative Territory`)

		// Stavropol’skiy Administrative Territory
		case "RU-STA":
			tmpeCodes = append(tmpeCodes, `Stavropol’skiy Administrative Territory`)

		// Zabaykal’skiy Administrative Territory
		case "RU-ZAB":
			tmpeCodes = append(tmpeCodes, `Zabaykal’skiy Administrative Territory`)

		// Amurskaya Administrative Region
		case "RU-AMU":
			tmpeCodes = append(tmpeCodes, `Amurskaya Administrative Region`)

		// Arkhangel’skaya Administrative Region
		case "RU-ARK":
			tmpeCodes = append(tmpeCodes, `Arkhangel’skaya Administrative Region`)

		// Astrakhanskaya Administrative Region
		case "RU-AST":
			tmpeCodes = append(tmpeCodes, `Astrakhanskaya Administrative Region`)

		// Belgorodskaya Administrative Region
		case "RU-BEL":
			tmpeCodes = append(tmpeCodes, `Belgorodskaya Administrative Region`)

		// Bryanskaya Administrative Region
		case "RU-BRY":
			tmpeCodes = append(tmpeCodes, `Bryanskaya Administrative Region`)

		// Chelyabinskaya Administrative Region
		case "RU-CHE":
			tmpeCodes = append(tmpeCodes, `Chelyabinskaya Administrative Region`)

		// Irkutskaya Administrative Region
		case "RU-IRK":
			tmpeCodes = append(tmpeCodes, `Irkutskaya Administrative Region`)

		// Ivanovskaya Administrative Region
		case "RU-IVA":
			tmpeCodes = append(tmpeCodes, `Ivanovskaya Administrative Region`)

		// Kaliningradskaya Administrative Region
		case "RU-KGD":
			tmpeCodes = append(tmpeCodes, `Kaliningradskaya Administrative Region`)

		// Kaluzhskaya Administrative Region
		case "RU-KLU":
			tmpeCodes = append(tmpeCodes, `Kaluzhskaya Administrative Region`)

		// Kemerovskaya Administrative Region
		case "RU-KEM":
			tmpeCodes = append(tmpeCodes, `Kemerovskaya Administrative Region`)

		// Kirovskaya Administrative Region
		case "RU-KIR":
			tmpeCodes = append(tmpeCodes, `Kirovskaya Administrative Region`)

		// Kostromskaya Administrative Region
		case "RU-KOS":
			tmpeCodes = append(tmpeCodes, `Kostromskaya Administrative Region`)

		// Kurganskaya Administrative Region
		case "RU-KGN":
			tmpeCodes = append(tmpeCodes, `Kurganskaya Administrative Region`)

		// Kurskaya Administrative Region
		case "RU-KRS":
			tmpeCodes = append(tmpeCodes, `Kurskaya Administrative Region`)

		// Leningradskaya Administrative Region
		case "RU-LEN":
			tmpeCodes = append(tmpeCodes, `Leningradskaya Administrative Region`)

		// Lipetskaya Administrative Region
		case "RU-LIP":
			tmpeCodes = append(tmpeCodes, `Lipetskaya Administrative Region`)

		// Magadanskaya Administrative Region
		case "RU-MAG":
			tmpeCodes = append(tmpeCodes, `Magadanskaya Administrative Region`)

		// Moskovskaya Administrative Region
		case "RU-MOS":
			tmpeCodes = append(tmpeCodes, `Moskovskaya Administrative Region`)

		// Murmanskaya Administrative Region
		case "RU-MUR":
			tmpeCodes = append(tmpeCodes, `Murmanskaya Administrative Region`)

		// Nizhegorodskaya Administrative Region
		case "RU-NIZ":
			tmpeCodes = append(tmpeCodes, `Nizhegorodskaya Administrative Region`)

		// Novgorodskaya Administrative Region
		case "RU-NGR":
			tmpeCodes = append(tmpeCodes, `Novgorodskaya Administrative Region`)

		// Novosibirskaya Administrative Region
		case "RU-NVS":
			tmpeCodes = append(tmpeCodes, `Novosibirskaya Administrative Region`)

		// Omskaya Administrative Region
		case "RU-OMS":
			tmpeCodes = append(tmpeCodes, `Omskaya Administrative Region`)

		// Orenburgskaya Administrative Region
		case "RU-ORE":
			tmpeCodes = append(tmpeCodes, `Orenburgskaya Administrative Region`)

		// Orlovskaya Administrative Region
		case "RU-ORL":
			tmpeCodes = append(tmpeCodes, `Orlovskaya Administrative Region`)

		// Penzenskaya Administrative Region
		case "RU-PNZ":
			tmpeCodes = append(tmpeCodes, `Penzenskaya Administrative Region`)

		// Pskovskaya Administrative Region
		case "RU-PSK":
			tmpeCodes = append(tmpeCodes, `Pskovskaya Administrative Region`)

		// Rostovskaya Administrative Region
		case "RU-ROS":
			tmpeCodes = append(tmpeCodes, `Rostovskaya Administrative Region`)

		// Ryazanskaya Administrative Region
		case "RU-RYA":
			tmpeCodes = append(tmpeCodes, `Ryazanskaya Administrative Region`)

		// Sakhalinskaya Administrative Region
		case "RU-SAK":
			tmpeCodes = append(tmpeCodes, `Sakhalinskaya Administrative Region`)

		// Samarskaya Administrative Region
		case "RU-SAM":
			tmpeCodes = append(tmpeCodes, `Samarskaya Administrative Region`)

		// Saratovskaya Administrative Region
		case "RU-SAR":
			tmpeCodes = append(tmpeCodes, `Saratovskaya Administrative Region`)

		// Smolenskaya Administrative Region
		case "RU-SMO":
			tmpeCodes = append(tmpeCodes, `Smolenskaya Administrative Region`)

		// Sverdlovskaya Administrative Region
		case "RU-SVE":
			tmpeCodes = append(tmpeCodes, `Sverdlovskaya Administrative Region`)

		// Tambovskaya Administrative Region
		case "RU-TAM":
			tmpeCodes = append(tmpeCodes, `Tambovskaya Administrative Region`)

		// Tomskaya Administrative Region
		case "RU-TOM":
			tmpeCodes = append(tmpeCodes, `Tomskaya Administrative Region`)

		// Tul’skaya Administrative Region
		case "RU-TUL":
			tmpeCodes = append(tmpeCodes, `Tul’skaya Administrative Region`)

		// Tverskaya Administrative Region
		case "RU-TVE":
			tmpeCodes = append(tmpeCodes, `Tverskaya Administrative Region`)

		// Tyumenskaya Administrative Region
		case "RU-TYU":
			tmpeCodes = append(tmpeCodes, `Tyumenskaya Administrative Region`)

		// Ul’yanovskaya Administrative Region
		case "RU-ULY":
			tmpeCodes = append(tmpeCodes, `Ul’yanovskaya Administrative Region`)

		// Vladimirskaya Administrative Region
		case "RU-VLA":
			tmpeCodes = append(tmpeCodes, `Vladimirskaya Administrative Region`)

		// Volgogradskaya Administrative Region
		case "RU-VGG":
			tmpeCodes = append(tmpeCodes, `Volgogradskaya Administrative Region`)

		// Vologodskaya Administrative Region
		case "RU-VLG":
			tmpeCodes = append(tmpeCodes, `Vologodskaya Administrative Region`)

		// Voronezhskaya Administrative Region
		case "RU-VOR":
			tmpeCodes = append(tmpeCodes, `Voronezhskaya Administrative Region`)

		// Yaroslavskaya Administrative Region
		case "RU-YAR":
			tmpeCodes = append(tmpeCodes, `Yaroslavskaya Administrative Region`)

		// Moskva City
		case "RU-MOW":
			tmpeCodes = append(tmpeCodes, `Moskva City`)

		// Sankt-Peterburg City
		case "RU-SPE":
			tmpeCodes = append(tmpeCodes, `Sankt-Peterburg City`)

		// Yevreyskaya Autonomous Administrative Region
		case "RU-YEV":
			tmpeCodes = append(tmpeCodes, `Yevreyskaya Autonomous Administrative Region`)

		// Chukotskiy Autonomous District
		case "RU-CHU":
			tmpeCodes = append(tmpeCodes, `Chukotskiy Autonomous District`)

		// Khanty-Mansiyskiy Autonomous District
		case "RU-KHM":
			tmpeCodes = append(tmpeCodes, `Khanty-Mansiyskiy Autonomous District`)

		// Nenetskiy Autonomous District
		case "RU-NEN":
			tmpeCodes = append(tmpeCodes, `Nenetskiy Autonomous District`)

		// Yamalo-Nenetskiy Autonomous District
		case "RU-YAN":
			tmpeCodes = append(tmpeCodes, `Yamalo-Nenetskiy Autonomous District`)

		// Alaska
		case "US-AK":
			tmpeCodes = append(tmpeCodes, `Alaska`)

		// Alabama
		case "US-AL":
			tmpeCodes = append(tmpeCodes, `Alabama`)

		// Arkansas
		case "US-AR":
			tmpeCodes = append(tmpeCodes, `Arkansas`)

		// Arizona
		case "US-AZ":
			tmpeCodes = append(tmpeCodes, `Arizona`)

		// California
		case "US-CA":
			tmpeCodes = append(tmpeCodes, `California`)

		// Colorado
		case "US-CO":
			tmpeCodes = append(tmpeCodes, `Colorado`)

		// Connecticut
		case "US-CT":
			tmpeCodes = append(tmpeCodes, `Connecticut`)

		// District of Columbia
		case "US-DC":
			tmpeCodes = append(tmpeCodes, `District of Columbia`)

		// Delaware
		case "US-DE":
			tmpeCodes = append(tmpeCodes, `Delaware`)

		// Florida
		case "US-FL":
			tmpeCodes = append(tmpeCodes, `Florida`)

		// Georgia
		case "US-GA":
			tmpeCodes = append(tmpeCodes, `Georgia`)

		// Hawaii
		case "US-HI":
			tmpeCodes = append(tmpeCodes, `Hawaii`)

		// Iowa
		case "US-IA":
			tmpeCodes = append(tmpeCodes, `Iowa`)

		// Idaho
		case "US-ID":
			tmpeCodes = append(tmpeCodes, `Idaho`)

		// Illinois
		case "US-IL":
			tmpeCodes = append(tmpeCodes, `Illinois`)

		// Indiana
		case "US-IN":
			tmpeCodes = append(tmpeCodes, `Indiana`)

		// Kansas
		case "US-KS":
			tmpeCodes = append(tmpeCodes, `Kansas`)

		// Kentucky
		case "US-KY":
			tmpeCodes = append(tmpeCodes, `Kentucky`)

		// Louisiana
		case "US-LA":
			tmpeCodes = append(tmpeCodes, `Louisiana`)

		// Massachusetts
		case "US-MA":
			tmpeCodes = append(tmpeCodes, `Massachusetts`)

		// Maryland
		case "US-MD":
			tmpeCodes = append(tmpeCodes, `Maryland`)

		// Maine
		case "US-ME":
			tmpeCodes = append(tmpeCodes, `Maine`)

		// Michigan
		case "US-MI":
			tmpeCodes = append(tmpeCodes, `Michigan`)

		// Minnesota
		case "US-MN":
			tmpeCodes = append(tmpeCodes, `Minnesota`)

		// Missouri
		case "US-MO":
			tmpeCodes = append(tmpeCodes, `Missouri`)

		// Mississippi
		case "US-MS":
			tmpeCodes = append(tmpeCodes, `Mississippi`)

		// Montana
		case "US-MT":
			tmpeCodes = append(tmpeCodes, `Montana`)

		// North Carolina
		case "US-NC":
			tmpeCodes = append(tmpeCodes, `North Carolina`)

		// North Dakota
		case "US-ND":
			tmpeCodes = append(tmpeCodes, `North Dakota`)

		// Nebraska
		case "US-NE":
			tmpeCodes = append(tmpeCodes, `Nebraska`)

		// New Hampshire
		case "US-NH":
			tmpeCodes = append(tmpeCodes, `New Hampshire`)

		// New Jersey
		case "US-NJ":
			tmpeCodes = append(tmpeCodes, `New Jersey`)

		// New Mexico
		case "US-NM":
			tmpeCodes = append(tmpeCodes, `New Mexico`)

		// Nevada
		case "US-NV":
			tmpeCodes = append(tmpeCodes, `Nevada`)

		// New York
		case "US-NY":
			tmpeCodes = append(tmpeCodes, `New York`)

		// Ohio
		case "US-OH":
			tmpeCodes = append(tmpeCodes, `Ohio`)

		// Oklahoma
		case "US-OK":
			tmpeCodes = append(tmpeCodes, `Oklahoma`)

		// Oregon
		case "US-OR":
			tmpeCodes = append(tmpeCodes, `Oregon`)

		// Pennsylvania
		case "US-PA":
			tmpeCodes = append(tmpeCodes, `Pennsylvania`)

		// Rhode Island
		case "US-RI":
			tmpeCodes = append(tmpeCodes, `Rhode Island`)

		// South Carolina
		case "US-SC":
			tmpeCodes = append(tmpeCodes, `South Carolina`)

		// South Dakota
		case "US-SD":
			tmpeCodes = append(tmpeCodes, `South Dakota`)

		// Tennessee
		case "US-TN":
			tmpeCodes = append(tmpeCodes, `Tennessee`)

		// Texas
		case "US-TX":
			tmpeCodes = append(tmpeCodes, `Texas`)

		// Utah
		case "US-UT":
			tmpeCodes = append(tmpeCodes, `Utah`)

		// Virginia
		case "US-VA":
			tmpeCodes = append(tmpeCodes, `Virginia`)

		// Vermont
		case "US-VT":
			tmpeCodes = append(tmpeCodes, `Vermont`)

		// Washington
		case "US-WA":
			tmpeCodes = append(tmpeCodes, `Washington`)

		// Wisconsin
		case "US-WI":
			tmpeCodes = append(tmpeCodes, `Wisconsin`)

		// West Virginia
		case "US-WV":
			tmpeCodes = append(tmpeCodes, `West Virginia`)

		// Wyoming
		case "US-WY":
			tmpeCodes = append(tmpeCodes, `Wyoming`)

		// Countries geographically within continental Europe which use the Euro as their sole currency. At the time of writing, this is a synonym for ‘AT BE CY EE FI FR DE ES GR IE IT LT LU LV MT NL PT SI SK’ (the official Eurozone 19), plus ‘AD MC SM VA ME’ and Kosovo (other Euro-using countries in continental Europe). Note some other territories using the Euro, but outside continental Europe are excluded from this list, and may need to be specified separately. ONLY valid in ONIX 3, and ONLY within P.26 – and this use is itself DEPRECATED. Use of an explicit list of countries instead of ECZ is strongly encouraged
		case "ECZ":
			tmpeCodes = append(tmpeCodes, `Eurozone`)

		// In ONIX 3, may ONLY be used in <RegionsIncluded>
		case "WORLD":
			tmpeCodes = append(tmpeCodes, `World`)
		default:
			return fmt.Errorf("undefined code for RegionCode has been passed, got [%s]", v)
		}
	}
	c.Body = tmpeCodes
	return nil
}

// ReligiousTextFeatureCode Religious text feature
type ReligiousTextFeatureCode struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *ReligiousTextFeatureCode) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // Use with code 01 in <ReligiousTextFeatureType>
  case "01":
		c.Body = `Academic year`

  // Use with code 01 in <ReligiousTextFeatureType>
  case "02":
		c.Body = `Catechistic year`

  // Use with code 01 in <ReligiousTextFeatureType>
  case "03":
		c.Body = `Liturgical year`

  // Use with code 01 in <ReligiousTextFeatureType>
  case "04":
		c.Body = `Advent and Christmas`

  // Use with code 01 in <ReligiousTextFeatureType>
  case "05":
		c.Body = `Blessings`

  // Use with code 01 in <ReligiousTextFeatureType>
  case "06":
		c.Body = `Scholastic cycles`

  // Use with code 01 in <ReligiousTextFeatureType>
  case "07":
		c.Body = `Confirmation and Holy Communion`

  // For example, summer camps and other youth recreational activities: use with code 01 in <ReligiousTextFeatureType>
  case "08":
		c.Body = `Summer activites`

  // Use with code 01 in <ReligiousTextFeatureType>
  case "09":
		c.Body = `Easter`

  // Use with code 01 in <ReligiousTextFeatureType>
  case "10":
		c.Body = `Lent`

  // Use with code 01 in <ReligiousTextFeatureType>
  case "11":
		c.Body = `Marian themes`
	default:
		return fmt.Errorf("undefined code for ReligiousTextFeatureCode has been passed, got [%s]", v)
	}
	return nil
}

// ReligiousTextFeatureType Religious text feature type
type ReligiousTextFeatureType struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *ReligiousTextFeatureType) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // A church season or activity for which a religious text is intended. Religious text feature code must be taken from List 90
  case "01":
		c.Body = `Church season or activity`
	default:
		return fmt.Errorf("undefined code for ReligiousTextFeatureType has been passed, got [%s]", v)
	}
	return nil
}

// ReligiousTextIdentifier Religious text identifier
type ReligiousTextIdentifier struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *ReligiousTextIdentifier) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {
	default:
		return fmt.Errorf("undefined code for ReligiousTextIdentifier has been passed, got [%s]", v)
	}
}

// ResourceContentType Resource content type
type ResourceContentType struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *ResourceContentType) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // 2D
  case "01":
		c.Body = `Front cover`

  // 2D
  case "02":
		c.Body = `Back cover`

  // Not limited to front or back, including 3D perspective
  case "03":
		c.Body = `Cover / pack`

  // Photograph or portrait of contributor(s)
  case "04":
		c.Body = `Contributor picture`

  // Series image / artwork
  case "05":
		c.Body = `Series image / artwork`

  // Series logo
  case "06":
		c.Body = `Series logo`

  // For example, an isolated image from the front cover (without text), image of a completed jigsaw
  case "07":
		c.Body = `Product image / artwork`

  // Product logo
  case "08":
		c.Body = `Product logo`

  // Publisher logo
  case "09":
		c.Body = `Publisher logo`

  // Imprint logo
  case "10":
		c.Body = `Imprint logo`

  // Contributor interview
  case "11":
		c.Body = `Contributor interview`

  // Contributor presentation and/or commentary
  case "12":
		c.Body = `Contributor presentation`

  // Contributor reading
  case "13":
		c.Body = `Contributor reading`

  // Link to a schedule in iCalendar format
  case "14":
		c.Body = `Contributor event schedule`

  // For example: a short excerpt, sample text or a complete sample chapter, page images, screenshots etc
  case "15":
		c.Body = `Sample content`

  // A ‘look inside’ feature presented as a small embeddable application
  case "16":
		c.Body = `Widget`

  // Review text held in a separate downloadable file, not in the ONIX record. Equivalent of code 06 in List 153. Use the <TextContent> composite for review quotes carried in the ONIX record. Use the <CitedContent> composite for a third-party review which is referenced from the ONIX record. Use <SupportingResource> for review text offered as a separate file resource for reproduction as part of promotional material for the product
  case "17":
		c.Body = `Review`

  // Other commentary / discussion
  case "18":
		c.Body = `Other commentary / discussion`

  // Reading group guide
  case "19":
		c.Body = `Reading group guide`

  // Incuding associated teacher / instructor resources
  case "20":
		c.Body = `Teacher’s guide`

  // Feature article provided by publisher
  case "21":
		c.Body = `Feature article`

  // Fictional character ‘interview’
  case "22":
		c.Body = `Character ‘interview’`

  // Wallpaper / screensaver
  case "23":
		c.Body = `Wallpaper / screensaver`

  // Press release
  case "24":
		c.Body = `Press release`

  // A table of contents held in a separate downloadable file, not in the ONIX record. Equivalent of code 04 in List 153. Use the <TextContent> composite for a table of contents carried in the ONIX record. Use <Supporting Resource> for text offered as a separate file resource
  case "25":
		c.Body = `Table of contents`

  // A promotional video (or audio), similar to a movie trailer (sometimes referred to as a ‘book trailer’)
  case "26":
		c.Body = `Trailer`

  // Intended ONLY for transitional use, where ONIX 2.1 records referencing existing thumbnail assets of unknown pixel size are being re-expressed in ONIX 3.0. Use code 01 for all new cover assets, and where the pixel size of older assets is known
  case "27":
		c.Body = `Cover thumbnail`

  // The full content of the product (or the product itself), supplied for example to support full-text search or indexing
  case "28":
		c.Body = `Full content`

  // Includes cover, back cover, spine and – where appropriate – any flaps
  case "29":
		c.Body = `Full cover`

  // Master brand logo
  case "30":
		c.Body = `Master brand logo`

  // Descriptive text in a separate downloadable file, not in the ONIX record. Equivalent of code 03 in List 153. Use the <TextContent> composite for descriptions carried in the ONIX record. Use <Supporting Resource> for text offered as a separate file resource for reproduction as part of promotional material for the product
  case "31":
		c.Body = `Description`

  // Index text held in a separate downloadable file, not in the ONIX record. Equivalent of code 15 in List 153. Use the <TextContent> composite for index text carried in the ONIX record. Use <Supporting Resource> for an index offered as a separate file resource
  case "32":
		c.Body = `Index`

  // Including associated student / learner resources
  case "33":
		c.Body = `Student’s guide`

  // For example a PDF or other digital representation of a publisher’s ‘new titles’ or range catalogue
  case "34":
		c.Body = `Publisher’s catalogue`

  // For example a banner ad for the product. Pixel dimensions should typically be included in <ResourceVersionFeature>
  case "35":
		c.Body = `Online advertisement panel`

  // German ‘Búhnenbild’
  case "36":
		c.Body = `Online advertisement page`

  // For example, posters, logos, banners, advertising templates for use in connection with a promotional event
  case "37":
		c.Body = `Promotional event material`

  // Availability of a digital review or digital proof copy, may be limited to authorised users or account holders
  case "38":
		c.Body = `Digital review copy`

  // For example, video showing how to use the product
  case "39":
		c.Body = `Instructional material`

  // Errata
  case "40":
		c.Body = `Errata`

  // Introduction, preface or other preliminary material in a separate resource file
  case "41":
		c.Body = `Introduction`

  // Descriptive material in a separate resource file, not in the ONIX record. Equivalent of code 17 in List 153. Use the <TextContent> composite for collection descriptions carried in the ONIX record. Use <Supporting Resource> for material (which need not be solely only) offered as a separate file resource for reproduction as part of promotional material for the product and collection
  case "42":
		c.Body = `Collection description`

  // Complete list of books by the author(s), supplied as a separate resource file
  case "43":
		c.Body = `Bibliography`

  // Formal summary of content (normally used with academic and scholarly content only)
  case "44":
		c.Body = `Abstract`

  // Image that may be used for promotional purposes in place of a front cover, ONLY where the front cover itself cannot be provided or used for any reason. Typically, holding images may comprise logos, artwork or an unfinished front cover image. Senders should ensure removal of the holding image from the record as soon as a cover image is available. Recipients must ensure replacement of the holding image with the cover image when it is supplied
  case "45":
		c.Body = `Cover holding image`

  // Eg for a game, kit
  case "46":
		c.Body = `Rules or instructions`

  // Full transcript of audio or video content of the product
  case "47":
		c.Body = `Transcript`

  // Link to a license covering permitted usage of the product content. Deprecated in favor of <EpubLicense>. This was a temporary workaround in ONIX 3.0, and use of <EpubLicense> is strongly preferred
  case "99":
		c.Body = `License`
	default:
		return fmt.Errorf("undefined code for ResourceContentType has been passed, got [%s]", v)
	}
	return nil
}

// ResourceFeatureType Resource feature type
type ResourceFeatureType struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *ResourceFeatureType) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // Credit that must be displayed when a resource is used (eg ‘Photo Jerry Bauer’ or ‘© Magnum Photo’). Credit text should be carried in <FeatureNote>
  case "01":
		c.Body = `Required credit`

  // Explanatory caption that may accompany a resource (eg use to identify an author in a photograph). Caption text should be carried in <FeatureNote>
  case "02":
		c.Body = `Caption`

  // Copyright holder of resource (indicative only, as the resource can be used without consultation). Copyright text should be carried in <FeatureNote>
  case "03":
		c.Body = `Copyright holder`

  // Approximate length in minutes of an audio or video resource. <FeatureValue> should contain the length of time as an integer number of minutes
  case "04":
		c.Body = `Length in minutes`

  // Use to link resource to a contributor unambiguously, for example with Resource Content types 04, 11–14 from List 158, particularly where the product has more than a single contributor. <FeatureValue> contains the 16-digit ISNI, which must match an ISNI given in an instance of <Contributor>
  case "05":
		c.Body = `ISNI of resource contributor`

  // Use to link resource to a contributor unambiguously, for example with Resource Content types 04, 11–14 from List 158, particularly where the product has more than a single contributor. <FeatureValue> contains the proprietary ID, which must match a proprietary ID given in an instance of <Contributor>
  case "06":
		c.Body = `Proprietary ID of resource contributor`

  // <FeatureNote> is Alternative text for the resource, which might be presented to visually-impaired readers
  case "07":
		c.Body = `Resource alternative text`

  // <FeatureValue> is a 24-bit RGB or 32-bit RBGA color in hexadecimal, eg fff2de for an opaque warm cream. Used when the resource – for example a 3D image of the product – includes a background, or if used with an alpha channel, when the image is irregularly shaped or contains a semi-transparent shadow thrown against a background
  case "08":
		c.Body = `Background color of image resource`

  // <FeatureValue> is an ONIX code from List 256 that describes an attribute of a product image resource (eg perspective, content)
  case "09":
		c.Body = `Attribute of product image resource`

  // <FeatureValue> is a 24-bit RGB color in hexadecimal, eg ffc300 for a rich yellow-orange, used when the resource supplier requests a specific background color be displayed behind the resource on a web page
  case "10":
		c.Body = `Background color of page`
	default:
		return fmt.Errorf("undefined code for ResourceFeatureType has been passed, got [%s]", v)
	}
	return nil
}

// ResourceForm Resource form
type ResourceForm struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *ResourceForm) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // A resource that may be accessed by a hyperlink. The current host (eg the ONIX sender, who may be the publisher) will provide ongoing hosting services for the resource for the active life of the product (or at least until the Until Date specified in <ContentDate>). The ONIX recipient may embed the URL in a consumer facing-website (eg as the src attribute in an <img> link), and need not host an independent copy of the resource
  case "01":
		c.Body = `Linkable resource`

  // A file that may be downloaded on demand for third-party use. The ONIX sender will host a copy of the resource until the specified Until Date, but only for the ONIX recipient’s direct use. The ONIX recipient should download a copy of the resource, and must host an independent copy of the resource if it is used on a consumer-facing website. Special attention should be paid to the ‘Last Updated’ <ContentDate> to ensure the independent copy of the resource is kept up to date
  case "02":
		c.Body = `Downloadable file`

  // An application which is supplied in a form which can be embedded into a third-party webpage. As type 02, except the resource contains active content such as JavaScript, Flash, etc
  case "03":
		c.Body = `Embeddable application`
	default:
		return fmt.Errorf("undefined code for ResourceForm has been passed, got [%s]", v)
	}
	return nil
}

// ResourceMode Resource mode
type ResourceMode struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *ResourceMode) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // An executable together with data on which it operates
  case "01":
		c.Body = `Application`

  // A sound recording
  case "02":
		c.Body = `Audio`

  // A still image
  case "03":
		c.Body = `Image`

  // Readable text, with or without associated images etc
  case "04":
		c.Body = `Text`

  // Moving images, with or without accompanying sound
  case "05":
		c.Body = `Video`

  // A website or other supporting resource delivering content in a variety of modes
  case "06":
		c.Body = `Multi-mode`
	default:
		return fmt.Errorf("undefined code for ResourceMode has been passed, got [%s]", v)
	}
	return nil
}

// ResourceVersionFeatureType Resource version feature type
type ResourceVersionFeatureType struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *ResourceVersionFeatureType) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // Resource Version Feature Value carries a code from List 178
  case "01":
		c.Body = `File format`

  // Resource Version Feature Value carries an integer
  case "02":
		c.Body = `Image height in pixels`

  // Resource Version Feature Value carries an integer
  case "03":
		c.Body = `Image width in pixels`

  // Resource Version Feature Value carries the filename of the supporting resource, necessary only when it is different from the last part of the path provided in <ResourceLink>
  case "04":
		c.Body = `Filename`

  // Resource Version Feature Value carries a decimal number only, suggested no more than 2 or 3 significant digits (eg 1.7, not 1.7462 or 1.75MB)
  case "05":
		c.Body = `Approximate download file size in megabytes`

  // MD5 hash value of the resource file. <ResourceVersionFeatureValue> should contain the 128-bit digest value (as 32 hexadecimal digits). Can be used as a cryptographic check on the integrity of a resource after it has been retrieved
  case "06":
		c.Body = `MD5 hash value`

  // Resource Version Feature Value carries a integer number only (eg 1831023)
  case "07":
		c.Body = `Exact download file size in bytes`

  // SHA-256 hash value of the resource file. <ResourceVersionFeatureValue> should contain the 256-bit digest value (as 64 hexadecimal digits). Can be used as a cryptographic check on the integrity of a resource after it has been retrieved
  case "08":
		c.Body = `SHA-256 hash value`

  // International Standard Content Code, a ‘similarity hash’ derived algorithmically from the resource content itself (see https://iscc.codes). <IDValue> is the 55-character case-sensitive string (including three hyphens) forming the ISCC of the resource file
  case "09":
		c.Body = `ISCC`
	default:
		return fmt.Errorf("undefined code for ResourceVersionFeatureType has been passed, got [%s]", v)
	}
	return nil
}

// ReturnsCodeType Returns conditions code type
type ReturnsCodeType struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *ReturnsCodeType) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // As specified in <ReturnsCodeTypeName> (ONIX 3.0 only)
  case "00":
		c.Body = `Proprietary`

  // Maintained by CLIL (Commission Interprofessionnel du Livre). Returns conditions values in <ReturnsCode> should be taken from the CLIL list
  case "01":
		c.Body = `French book trade returns conditions code`

  // Maintained by BISAC: Returns conditions values in <ReturnsCode> should be taken from List 66
  case "02":
		c.Body = `BISAC Returnable Indicator code`

  // NOT CURRENTLY USED – BIC has decided that it will not maintain a code list for this purpose, since returns conditions are usually at least partly based on the trading relationship
  case "03":
		c.Body = `UK book trade returns conditions code`

  // Returns conditions values in <ReturnsCode> should be taken from List 204
  case "04":
		c.Body = `ONIX Returns conditions code`
	default:
		return fmt.Errorf("undefined code for ReturnsCodeType has been passed, got [%s]", v)
	}
	return nil
}

// SalesOutletIDType Sales outlet identifier type
type SalesOutletIDType struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *SalesOutletIDType) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // Proprietary list of retail and other end-user sales outlet IDs. Note that <IDTypeName> is required with proprietary identifiers
  case "01":
		c.Body = `Proprietary`

  // Use with ONIX retail and other end-user sales outlet IDs from List 139
  case "03":
		c.Body = `ONIX retail sales outlet ID code`

  // 13-digit GS1 global location number (formerly EAN location number)
  case "04":
		c.Body = `Retail sales outlet GLN`

  // 7-digit Book trade Standard Address Number (US, UK etc)
  case "05":
		c.Body = `Retail sales outlet SAN`
	default:
		return fmt.Errorf("undefined code for SalesOutletIDType has been passed, got [%s]", v)
	}
	return nil
}

// SalesRestrictionType Sales restriction type
type SalesRestrictionType struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *SalesRestrictionType) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // Restriction must be described in <SalesRestrictionDetail> (ONIX 2.1) or <SalesRestrictionNote> (ONIX 3.0)
  case "00":
		c.Body = `Unspecified – see text`

  // Sales rights (or market distribution rights) apply to sales through designated retailer(s), which must be identified or named in an instance of the <SalesOutlet> composite. Use only when it is not possible to assign the more explicit codes 04 or 05
  case "01":
		c.Body = `Retailer exclusive / own brand`

  // Sales rights (or market distribution rights) apply to sales though office supplies channels. Specific outlet(s) may be identified or named in an instance of the <SalesOutlet> composite
  case "02":
		c.Body = `Through office supplies outlets only`

  // For an ISBN that is assigned for a publisher’s internal purposes
  case "03":
		c.Body = `Internal publisher use only: do not list`

  // Sales rights (or market distribution rights) apply to sales (under the publisher’s brand / imprint) through the designated retailer(s), which must be identified or named in an instance of the <SalesOutlet> composite
  case "04":
		c.Body = `Retailer exclusive`

  // Sales rights (or market distribution rights) apply to sales (under the retailer’s own brand / imprint) through the designated retailer(s), which must be identified or named in an instance of the <SalesOutlet> composite
  case "05":
		c.Body = `Retailer own brand`

  // Sales rights (or market distribution rights) apply to supplies to libraries
  case "06":
		c.Body = `To libraries only`

  // Sales rights (or market distribution rights) apply to supplies to schools
  case "07":
		c.Body = `To schools only`

  // Indexed for the German market – in Deutschland indiziert
  case "08":
		c.Body = `Indiziert`

  // Sales rights (or market distribution rights) apply to supplies other than to libraries
  case "09":
		c.Body = `Except to libraries`

  // Sales rights (or market distribution rights) apply to sales though news outlet channels (newsstands / newsagents)
  case "10":
		c.Body = `Through news outlets only`

  // Sales rights (or market distribution rights) apply to sales other than through designated retailer(s), which must be identified or named in the <SalesOutlet> composite
  case "11":
		c.Body = `Retailer exception`

  // Sales rights (or market distribution rights) apply to supplies other than to organisations or services offering consumers subscription access to a catalog of books
  case "12":
		c.Body = `Except to subscription services`

  // Sales rights (or market distribution rights) apply to supplies to organisations or services offering consumers subscription access to a catalog of books
  case "13":
		c.Body = `To subscription services only`

  // Sales rights (or market distribution rights) apply to sales other than through online retail channels
  case "14":
		c.Body = `Except through online retail`

  // Sales rights (or market distribution rights) apply to sales through online retail channels
  case "15":
		c.Body = `Through online retail only`

  // Sales rights (or market distribution rights) apply to supplies other than to schools. For use in ONIX 3.0 only
  case "16":
		c.Body = `Except to schools`

  // POD copies may be manufactured at any time, either to fulfill a customer order immediately or to replace a minimal stockholding (ie near-inventoryless). Only valid in ONIX 3.0
  case "17":
		c.Body = `Through Inventoryless POD`

  // POD copies may be manfactured only to fulfill a customer order immediately while out of stock and awaiting delivery of further stock from the supplier. Only valid in ONIX 3.0
  case "18":
		c.Body = `Through Stock Protection POD`

  // Not eligible for POD. Only valid in ONIX 3.0
  case "19":
		c.Body = `Except through POD`

  // Positive indication that no sales restrictions apply, for example to indicate the product may be sold both online and in bricks-and mortar retail, or to subscription services and non-subscription customers. For use in ONIX 3.0 only
  case "99":
		c.Body = `No restrictions on sales`
	default:
		return fmt.Errorf("undefined code for SalesRestrictionType has been passed, got [%s]", v)
	}
	return nil
}

// SalesRightsType Sales rights type
type SalesRightsType struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *SalesRightsType) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // May only be used with the ONIX 3 <ROWSalesRightsType> element
  case "00":
		c.Body = `Sales rights unknown or unstated for any reason`

  // For sale with exclusive rights in the specified countries or territories
  case "01":
		c.Body = `For sale with exclusive rights in the specified countries or territories`

  // For sale with non-exclusive rights in the specified countries or territories
  case "02":
		c.Body = `For sale with non-exclusive rights in the specified countries or territories`

  // Not for sale in the specified countries or territories (reason unspecified)
  case "03":
		c.Body = `Not for sale in the specified countries or territories (reason unspecified)`

  // Not for sale in the specified countries (but publisher holds exclusive rights in those countries or territories)
  case "04":
		c.Body = `Not for sale in the specified countries (but publisher holds exclusive rights in those countries or territories)`

  // Not for sale in the specified countries (publisher holds non-exclusive rights in those countries or territories)
  case "05":
		c.Body = `Not for sale in the specified countries (publisher holds non-exclusive rights in those countries or territories)`

  // Not for sale in the specified countries (because publisher does not hold rights in those countries or territories)
  case "06":
		c.Body = `Not for sale in the specified countries (because publisher does not hold rights in those countries or territories)`

  // Only for use with ONIX 3. Deprecated
  case "07":
		c.Body = `For sale with exclusive rights in the specified countries or territories (sales restriction applies)`

  // Only for use with ONIX 3. Deprecated
  case "08":
		c.Body = `For sale with non-exclusive rights in the specified countries or territories (sales restriction applies)`
	default:
		return fmt.Errorf("undefined code for SalesRightsType has been passed, got [%s]", v)
	}
	return nil
}

// ScriptCode Text script – based on ISO 15924
type ScriptCode struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *ScriptCode) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // For use in ONIX 3.0 only
  case "Adlm":
		c.Body = `Adlam`

  // Script is not supported by Unicode
  case "Afak":
		c.Body = `Afaka`

  // Ancient/historic script. For use in ONIX 3.0 only
  case "Aghb":
		c.Body = `Caucasian Albanian`

  // Ancient/historic script. For use in ONIX 3.0 only
  case "Ahom":
		c.Body = `Ahom, Tai Ahom`

  // Arabic
  case "Arab":
		c.Body = `Arabic`

  // Typographic variant of Arabic. For use in ONIX 3.0 only
  case "Aran":
		c.Body = `Arabic (Nastaliq variant)`

  // Ancient/historic script
  case "Armi":
		c.Body = `Imperial Aramaic`

  // Armenian
  case "Armn":
		c.Body = `Armenian`

  // Ancient/historic script
  case "Avst":
		c.Body = `Avestan`

  // Balinese
  case "Bali":
		c.Body = `Balinese`

  // Bamun
  case "Bamu":
		c.Body = `Bamun`

  // Ancient/historic script
  case "Bass":
		c.Body = `Bassa Vah`

  // Batak
  case "Batk":
		c.Body = `Batak`

  // Bengali (Bangla)
  case "Beng":
		c.Body = `Bengali (Bangla)`

  // Ancient/historic script. For use in ONIX 3.0 only
  case "Bhks":
		c.Body = `Bhaiksuki`

  // Script is not supported by Unicode
  case "Blis":
		c.Body = `Blissymbols`

  // Bopomofo
  case "Bopo":
		c.Body = `Bopomofo`

  // Ancient/historic script
  case "Brah":
		c.Body = `Brahmi`

  // Braille
  case "Brai":
		c.Body = `Braille`

  // Buginese
  case "Bugi":
		c.Body = `Buginese`

  // Buhid
  case "Buhd":
		c.Body = `Buhid`

  // Chakma
  case "Cakm":
		c.Body = `Chakma`

  // Unified Canadian Aboriginal Syllabics
  case "Cans":
		c.Body = `Unified Canadian Aboriginal Syllabics`

  // Ancient/historic script
  case "Cari":
		c.Body = `Carian`

  // Cham
  case "Cham":
		c.Body = `Cham`

  // Cherokee
  case "Cher":
		c.Body = `Cherokee`

  // Script is not supported by Unicode
  case "Cirt":
		c.Body = `Cirth`

  // Ancient/historic script
  case "Copt":
		c.Body = `Coptic`

  // Ancient/historic script
  case "Cprt":
		c.Body = `Cypriot`

  // Cyrillic
  case "Cyrl":
		c.Body = `Cyrillic`

  // Ancient/historic, typographic variant of Cyrillic
  case "Cyrs":
		c.Body = `Cyrillic (Old Church Slavonic variant)`

  // Devanagari (Nagari)
  case "Deva":
		c.Body = `Devanagari (Nagari)`

  // Script is not supported by Unicode. For use in ONIX 3.0 only
  case "Dogr":
		c.Body = `Dogra`

  // Deseret (Mormon)
  case "Dsrt":
		c.Body = `Deseret (Mormon)`

  // Duployan shorthand, Duployan stenography
  case "Dupl":
		c.Body = `Duployan shorthand, Duployan stenography`

  // Script is not supported by Unicode
  case "Egyd":
		c.Body = `Egyptian demotic`

  // Script is not supported by Unicode
  case "Egyh":
		c.Body = `Egyptian hieratic`

  // Ancient/historic script
  case "Egyp":
		c.Body = `Egyptian hieroglyphs`

  // Ancient/historic script
  case "Elba":
		c.Body = `Elbasan`

  // Ethiopic (Ge‘ez)
  case "Ethi":
		c.Body = `Ethiopic (Ge‘ez)`

  // Georgian in Unicode
  case "Geok":
		c.Body = `Khutsuri (Asomtavruli and Khutsuri)`

  // Georgian (Mkhedruli and Mtavruli)
  case "Geor":
		c.Body = `Georgian (Mkhedruli and Mtavruli)`

  // Ancient/historic script
  case "Glag":
		c.Body = `Glagolitic`

  // Script is not supported by Unicode. For use in ONIX 3.0 only
  case "Gong":
		c.Body = `Gunjala Gondi`

  // For use in ONIX 3.0 only
  case "Gonm":
		c.Body = `Masaram Gondi`

  // Ancient/historic script
  case "Goth":
		c.Body = `Gothic`

  // Ancient/historic script
  case "Gran":
		c.Body = `Grantha`

  // Greek
  case "Grek":
		c.Body = `Greek`

  // Gujarati
  case "Gujr":
		c.Body = `Gujarati`

  // Gurmukhi
  case "Guru":
		c.Body = `Gurmukhi`

  // See Hani, Bopo. For use in ONIX 3.0 only
  case "Hanb":
		c.Body = `Han with Bopomofo`

  // Hangul (Hangŭl, Hangeul)
  case "Hang":
		c.Body = `Hangul (Hangŭl, Hangeul)`

  // Han (Hanzi, Kanji, Hanja)
  case "Hani":
		c.Body = `Han (Hanzi, Kanji, Hanja)`

  // Hanunoo (Hanunóo)
  case "Hano":
		c.Body = `Hanunoo (Hanunóo)`

  // Subset of Hani
  case "Hans":
		c.Body = `Han (Simplified variant)`

  // Subset of Hani
  case "Hant":
		c.Body = `Han (Traditional variant)`

  // Ancient/historic script. For use in ONIX 3.0 only
  case "Hatr":
		c.Body = `Hatran`

  // Hebrew
  case "Hebr":
		c.Body = `Hebrew`

  // Hiragana
  case "Hira":
		c.Body = `Hiragana`

  // Ancient/historic script. For use in ONIX 3.0 only
  case "Hluw":
		c.Body = `Anatolian Hieroglyphs (Luwian Hieroglyphs, Hittite Hieroglyphs)`

  // Pahawh Hmong
  case "Hmng":
		c.Body = `Pahawh Hmong`

  // See Hira, Kana
  case "Hrkt":
		c.Body = `Japanese syllabaries (alias for Hiragana + Katakana)`

  // Ancient/historic script
  case "Hung":
		c.Body = `Old Hungarian (Hungarian Runic)`

  // Script is not supported by Unicode
  case "Inds":
		c.Body = `Indus (Harappan)`

  // Ancient/historic script
  case "Ital":
		c.Body = `Old Italic (Etruscan, Oscan, etc.)`

  // Subset of Hang. For use in ONIX 3.0 only
  case "Jamo":
		c.Body = `Jamo (alias for Jamo subset of Hangul)`

  // Javanese
  case "Java":
		c.Body = `Javanese`

  // See Hani, Hira and Kana
  case "Jpan":
		c.Body = `Japanese (alias for Han + Hiragana + Katakana)`

  // Script is not supported by Unicode
  case "Jurc":
		c.Body = `Jurchen`

  // Kayah Li
  case "Kali":
		c.Body = `Kayah Li`

  // Katakana
  case "Kana":
		c.Body = `Katakana`

  // Ancient/historic script
  case "Khar":
		c.Body = `Kharoshthi`

  // Khmer
  case "Khmr":
		c.Body = `Khmer`

  // Ancient/historic script
  case "Khoj":
		c.Body = `Khojki`

  // Script is not supported by Unicode. For use in ONIX 3.0 only
  case "Kitl":
		c.Body = `Khitan large script`

  // Script is not supported by Unicode. For use in ONIX 3.0 only
  case "Kits":
		c.Body = `Khitan small script`

  // Kannada
  case "Knda":
		c.Body = `Kannada`

  // See Hani and Hang
  case "Kore":
		c.Body = `Korean (alias for Hangul + Han)`

  // Script is not supported by Unicode
  case "Kpel":
		c.Body = `Kpelle`

  // Ancient/historic script
  case "Kthi":
		c.Body = `Kaithi`

  // Tai Tham (Lanna)
  case "Lana":
		c.Body = `Tai Tham (Lanna)`

  // Lao
  case "Laoo":
		c.Body = `Lao`

  // Typographic variant of Latin
  case "Latf":
		c.Body = `Latin (Fraktur variant)`

  // Typographic variant of Latin
  case "Latg":
		c.Body = `Latin (Gaelic variant)`

  // Latin
  case "Latn":
		c.Body = `Latin`

  // Script is not supported by Unicode. For use in ONIX 3.0 only
  case "Leke":
		c.Body = `Leke`

  // Lepcha (Róng)
  case "Lepc":
		c.Body = `Lepcha (Róng)`

  // Limbu
  case "Limb":
		c.Body = `Limbu`

  // Ancient/historic script
  case "Lina":
		c.Body = `Linear A`

  // Ancient/historic script
  case "Linb":
		c.Body = `Linear B`

  // Lisu (Fraser)
  case "Lisu":
		c.Body = `Lisu (Fraser)`

  // Script is not supported by Unicode
  case "Loma":
		c.Body = `Loma`

  // Ancient/historic script
  case "Lyci":
		c.Body = `Lycian`

  // Ancient/historic script
  case "Lydi":
		c.Body = `Lydian`

  // Ancient/historic script. For use in ONIX 3.0 only
  case "Mahj":
		c.Body = `Mahajani`

  // Script is not supported by Unicode. For use in ONIX 3.0 only
  case "Maka":
		c.Body = `Makasar`

  // Mandaic, Mandaean
  case "Mand":
		c.Body = `Mandaic, Mandaean`

  // Ancient/historic script
  case "Mani":
		c.Body = `Manichaean`

  // Ancient/historic script. For use in ONIX 3.0 only
  case "Marc":
		c.Body = `Marchen`

  // Script is not supported by Unicode
  case "Maya":
		c.Body = `Mayan hieroglyphs`

  // Script is not supported by Unicode. For use in ONIX 3.0 only
  case "Medf":
		c.Body = `Medefaidrin (Oberi Okaime, Oberi Ɔkaimɛ)`

  // Mende Kikakui
  case "Mend":
		c.Body = `Mende Kikakui`

  // Ancient/historic script
  case "Merc":
		c.Body = `Meroitic Cursive`

  // Ancient/historic script
  case "Mero":
		c.Body = `Meroitic Hieroglyphs`

  // Malayalam
  case "Mlym":
		c.Body = `Malayalam`

  // Ancient/historic script. For use in ONIX 3.0 only
  case "Modi":
		c.Body = `Modi, Moḍī`

  // Includes Clear, Manchu scripts
  case "Mong":
		c.Body = `Mongolian`

  // Script is not supported by Unicode
  case "Moon":
		c.Body = `Moon (Moon code, Moon script, Moon type)`

  // Mro, Mru
  case "Mroo":
		c.Body = `Mro, Mru`

  // Meitei Mayek (Meithei, Meetei)
  case "Mtei":
		c.Body = `Meitei Mayek (Meithei, Meetei)`

  // Ancient/historic script. For use in ONIX 3.0 only
  case "Mult":
		c.Body = `Multani`

  // Myanmar (Burmese)
  case "Mymr":
		c.Body = `Myanmar (Burmese)`

  // Ancient/historic script
  case "Narb":
		c.Body = `Old North Arabian (Ancient North Arabian)`

  // Ancient/historic script
  case "Nbat":
		c.Body = `Nabatean`

  // For use in ONIX 3.0 only
  case "Newa":
		c.Body = `Newa, Newar, Newari, Nepāla lipi`

  // Script is not supported by Unicode
  case "Nkgb":
		c.Body = `Nakhi Geba (’Na-’Khi ²Ggŏ-¹baw, Naxi Geba)`

  // N’Ko
  case "Nkoo":
		c.Body = `N’Ko`

  // Nüshu
  case "Nshu":
		c.Body = `Nüshu`

  // Ancient/historic script
  case "Ogam":
		c.Body = `Ogham`

  // Ol Chiki (Ol Cemet’, Ol, Santali)
  case "Olck":
		c.Body = `Ol Chiki (Ol Cemet’, Ol, Santali)`

  // Ancient/historic script
  case "Orkh":
		c.Body = `Old Turkic, Orkhon Runic`

  // Oriya (Odia)
  case "Orya":
		c.Body = `Oriya (Odia)`

  // For use in ONIX 3.0 only
  case "Osge":
		c.Body = `Osage`

  // Osmanya
  case "Osma":
		c.Body = `Osmanya`

  // Ancient/historic script
  case "Palm":
		c.Body = `Palmyrene`

  // For use in ONIX 3.0 only
  case "Pauc":
		c.Body = `Pau Cin Hau`

  // Ancient/historic script
  case "Perm":
		c.Body = `Old Permic`

  // Ancient/historic script
  case "Phag":
		c.Body = `Phags-pa`

  // Ancient/historic script
  case "Phli":
		c.Body = `Inscriptional Pahlavi`

  // Ancient/historic script
  case "Phlp":
		c.Body = `Psalter Pahlavi`

  // Script is not supported by Unicode
  case "Phlv":
		c.Body = `Book Pahlavi`

  // Ancient/historic script
  case "Phnx":
		c.Body = `Phoenician`

  // Miao (Pollard)
  case "Plrd":
		c.Body = `Miao (Pollard)`

  // Script is not supported by Unicode. For use in ONIX 3.0 only
  case "Piqd":
		c.Body = `Klingon (KLI plqaD)`

  // Ancient/historic script
  case "Prti":
		c.Body = `Inscriptional Parthian`

  // Reserved for private use (start)
  case "Qaaa":
		c.Body = `Reserved for private use (start)`

  // Reserved for private use (end)
  case "Qabx":
		c.Body = `Reserved for private use (end)`

  // Rejang (Redjang, Kaganga)
  case "Rjng":
		c.Body = `Rejang (Redjang, Kaganga)`

  // Script is not supported by Unicode
  case "Roro":
		c.Body = `Rongorongo`

  // Ancient/historic script
  case "Runr":
		c.Body = `Runic`

  // Samaritan
  case "Samr":
		c.Body = `Samaritan`

  // Script is not supported by Unicode
  case "Sara":
		c.Body = `Sarati`

  // Ancient/historic script
  case "Sarb":
		c.Body = `Old South Arabian`

  // Saurashtra
  case "Saur":
		c.Body = `Saurashtra`

  // SignWriting
  case "Sgnw":
		c.Body = `SignWriting`

  // Shavian (Shaw)
  case "Shaw":
		c.Body = `Shavian (Shaw)`

  // Sharada, Śāradā
  case "Shrd":
		c.Body = `Sharada, Śāradā`

  // Ancient/historic script. For use in ONIX 3.0 only
  case "Sidd":
		c.Body = `Siddham, Siddhaṃ, Siddhamātṛkā`

  // Khudawadi, Sindhi
  case "Sind":
		c.Body = `Khudawadi, Sindhi`

  // Sinhala
  case "Sinh":
		c.Body = `Sinhala`

  // Sora Sompeng
  case "Sora":
		c.Body = `Sora Sompeng`

  // For use in ONIX 3.0 only
  case "Soyo":
		c.Body = `Soyombo`

  // Sundanese
  case "Sund":
		c.Body = `Sundanese`

  // Syloti Nagri
  case "Sylo":
		c.Body = `Syloti Nagri`

  // Syriac
  case "Syrc":
		c.Body = `Syriac`

  // Typographic variant of Syriac
  case "Syre":
		c.Body = `Syriac (Estrangelo variant)`

  // Typographic variant of Syriac
  case "Syrj":
		c.Body = `Syriac (Western variant)`

  // Typographic variant of Syriac
  case "Syrn":
		c.Body = `Syriac (Eastern variant)`

  // Tagbanwa
  case "Tagb":
		c.Body = `Tagbanwa`

  // Takri, Ṭākrī, Ṭāṅkrī
  case "Takr":
		c.Body = `Takri, Ṭākrī, Ṭāṅkrī`

  // Tai Le
  case "Tale":
		c.Body = `Tai Le`

  // New Tai Lue
  case "Talu":
		c.Body = `New Tai Lue`

  // Tamil
  case "Taml":
		c.Body = `Tamil`

  // Ancient/historic script
  case "Tang":
		c.Body = `Tangut`

  // Tai Viet
  case "Tavt":
		c.Body = `Tai Viet`

  // Telugu
  case "Telu":
		c.Body = `Telugu`

  // Script is not supported by Unicode
  case "Teng":
		c.Body = `Tengwar`

  // Tifinagh (Berber)
  case "Tfng":
		c.Body = `Tifinagh (Berber)`

  // Tagalog (Baybayin, Alibata)
  case "Tglg":
		c.Body = `Tagalog (Baybayin, Alibata)`

  // Thaana
  case "Thaa":
		c.Body = `Thaana`

  // Thai
  case "Thai":
		c.Body = `Thai`

  // Tibetan
  case "Tibt":
		c.Body = `Tibetan`

  // Tirhuta
  case "Tirh":
		c.Body = `Tirhuta`

  // Ancient/historic script
  case "Ugar":
		c.Body = `Ugaritic`

  // Vai
  case "Vaii":
		c.Body = `Vai`

  // Script is not supported by Unicode
  case "Visp":
		c.Body = `Visible Speech`

  // Warang Citi (Varang Kshiti)
  case "Wara":
		c.Body = `Warang Citi (Varang Kshiti)`

  // Script is not supported by Unicode
  case "Wole":
		c.Body = `Woleai`

  // Ancient/historic script
  case "Xpeo":
		c.Body = `Old Persian`

  // Ancient/historic script
  case "Xsux":
		c.Body = `Cuneiform, Sumero-Akkadian`

  // Yi
  case "Yiii":
		c.Body = `Yi`

  // For use in ONIX 3.0 only
  case "Zanb":
		c.Body = `Zanabazar Square (Zanabazarin Dörböljin Useg, Xewtee Dörböljin Bicig, Horizontal Square Script)`

  // Code for inherited script
  case "Zinh":
		c.Body = `Code for inherited script`

  // Not a script in Unicode
  case "Zmth":
		c.Body = `Mathematical notation`

  // Not a script in Unicode. For use in ONIX 3.0 only
  case "Zsye":
		c.Body = `Symbols (Emoji variant)`

  // Not a script in Unicode
  case "Zsym":
		c.Body = `Symbols`

  // Not a script in Unicode
  case "Zxxx":
		c.Body = `Code for unwritten documents`

  // Code for undetermined script
  case "Zyyy":
		c.Body = `Code for undetermined script`

  // Code for uncoded script
  case "Zzzz":
		c.Body = `Code for uncoded script`
	default:
		return fmt.Errorf("undefined code for ScriptCode has been passed, got [%s]", v)
	}
	return nil
}

// SenderIDType Name identifier type
type SenderIDType struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *SenderIDType) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // Note that <IDTypeName> is required with proprietary identifiers
  case "01":
		c.Body = `Proprietary`

  // DEPRECATED – use 01
  case "02":
		c.Body = `Proprietary`

  // Deutsche Nationalbibliothek publisher identifier
  case "03":
		c.Body = `DNB publisher identifier`

  // Börsenverein Verkehrsnummer
  case "04":
		c.Body = `Börsenverein Verkehrsnummer`

  // German ISBN Agency publisher identifier
  case "05":
		c.Body = `German ISBN Agency publisher identifier`

  // GS1 global location number (formerly EAN location number)
  case "06":
		c.Body = `GLN`

  // Book trade Standard Address Number – US, UK etc
  case "07":
		c.Body = `SAN`

  // MARC code list for organizations – see http://www.loc.gov/marc/organizations/orgshome.html
  case "08":
		c.Body = `MARC organization code`

  // Trading party identifier used in the Netherlands
  case "10":
		c.Body = `Centraal Boekhuis Relatie ID`

  // Flemish publisher code
  case "13":
		c.Body = `Fondscode Boekenbank`

  // Business Identity Code (Finland). See http://www.ytj.fi/ (in Finnish)
  case "15":
		c.Body = `Y-tunnus`

  // International Standard Name Identifier. A sixteen digit number. Usually presented with spaces or hyphens dividing the number into four groups of four digits, but in ONIX the spaces or hyphens should be omitted. See http://www.isni.org/
  case "16":
		c.Body = `ISNI`

  // Personennamendatei – person name authority file used by Deutsche Nationalbibliothek and in other German-speaking countries. See http://www.dnb.de/standardisierung/normdateien/pnd.htm (German) or http://www.dnb.de/eng/standardisierung/normdateien/pnd.htm (English). DEPRECATED in favor of the GND
  case "17":
		c.Body = `PND`

  // A control number assigned to a Library of Congress Control Number (LCCN) Name Authority / NACO record
  case "18":
		c.Body = `NACO`

  // Publisher identifier administered by Japanese ISBN Agency
  case "19":
		c.Body = `Japanese Publisher identifier`

  // Gemeinsame Körperschaftsdatei – Corporate Body Authority File in the German-speaking countries. See http://www.dnb.de/standardisierung/normdateien/gkd.htm (German) or http://www.dnb.de/eng/standardisierung/normdateien/gkd.htm (English). DEPRECATED in favor of the GND
  case "20":
		c.Body = `GKD`

  // Open Researcher and Contributor ID. A sixteen digit number. Usually presented with hyphens dividing the number into four groups of four digits, but in ONIX the hyphens should be omitted. See http://www.orcid.org/
  case "21":
		c.Body = `ORCID`

  // Publisher identifier maintained by the Chinese ISBN Agency (GAPP)
  case "22":
		c.Body = `GAPP Publisher Identifier`

  // Identifier for a business organization for VAT purposes, eg within the EU’s VIES system. See http://ec.europa.eu/taxation_customs/vies/faqvies.do for EU VAT ID formats, which vary from country to country. Generally these consist of a two-letter country code followed by the 8–12 digits of the national VAT ID. Some countries include one or two letters within their VAT ID. See http://en.wikipedia.org/wiki/VAT_identification_number for non-EU countries that maintain similar identifiers. Spaces, dashes etc should be omitted
  case "23":
		c.Body = `VAT Identity Number`

  // 4-digit business organization identifier controlled by the Japanese Publication Wholesalers Association
  case "24":
		c.Body = `JP Distribution Identifier`

  // Gemeinsame Normdatei – Joint Authority File in the German-speaking countries. See http://www.dnb.de/EN/gnd (English). Combines the PND, SWD and GKD into a single authority file, and should be used in preference
  case "25":
		c.Body = `GND`

  // Dunn and Bradstreet Universal Numbering System, see http://www.dnb.co.uk/dandb-duns-number
  case "26":
		c.Body = `DUNS`

  // Ringgold organizational identifier, see http://www.ringgold.com/identify.html
  case "27":
		c.Body = `Ringgold ID`

  // French Electre publisher identifier
  case "28":
		c.Body = `Identifiant Editeur Electre`

  // DOI used in EIDR party registry, for example ‘10.5237/C9F6-F41F’ (Sam Raimi). See http://eidr.org
  case "29":
		c.Body = `EIDR Party DOI`

  // French Electre imprint Identifier
  case "30":
		c.Body = `Identifiant Marque Electre`

  // Virtual Internet Authority File. <IDValue> should be a number. The URI form of the identifier can be created by prefixing the number with ‘https://viaf.org/viaf/’. See https://viaf.org
  case "31":
		c.Body = `VIAF ID`

  // DOI used in CrossRef’s Open Funder Registry list of academic research funding bodies, for example ‘10.13039/100004440’ (Wellcome Trust). See http://www.crossref.org/fundingdata/registry.html
  case "32":
		c.Body = `FundRef DOI`

  // Control number assigned to a Name Authority record by the Biblioteca Nacional de España
  case "33":
		c.Body = `BNE CN`

  // Numéro de la notice de personne BNF
  case "34":
		c.Body = `BNF Control Number`

  // Archival Resource Key, as a URL (including the address of the ARK resolver provided by eg a national library)
  case "35":
		c.Body = `ARK`

  // Nasjonalt autoritetsregister for navn – Norwegian national authority file for personal and corporate names. For use in ONIX 3.0 only
  case "36":
		c.Body = `Nasjonalt autoritetsregister`

  // Global Research Identifier Database ID (see https://www.grid.ac). For use in ONIX 3.0 only
  case "37":
		c.Body = `GRID`

  // Party ID from Identifiers and Standards for Higher Education and Research (fr: Identifiants et Référentiels pour l’enseignement supérieur et la recherche). For use on ONIX 3.0 only. See https://www.idref.fr
  case "38":
		c.Body = `IDRef`
	default:
		return fmt.Errorf("undefined code for SenderIDType has been passed, got [%s]", v)
	}
	return nil
}

// SourceType Content source type
type SourceType struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *SourceType) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // Printed media
  case "01":
		c.Body = `Printed media`

  // Website
  case "02":
		c.Body = `Website`

  // Radio
  case "03":
		c.Body = `Radio`

  // TV
  case "04":
		c.Body = `TV`
	default:
		return fmt.Errorf("undefined code for SourceType has been passed, got [%s]", v)
	}
	return nil
}

// StockQuantityCodeType Stock quantity code type
type StockQuantityCodeType struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *StockQuantityCodeType) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // As specified in <StockQuantityCodeTypeName>
  case "01":
		c.Body = `Proprietary`

  // Code scheme defined by the Australian Publishers Association. Deprecated
  case "02":
		c.Body = `APA stock quantity code`
	default:
		return fmt.Errorf("undefined code for StockQuantityCodeType has been passed, got [%s]", v)
	}
	return nil
}

// StudyBibleType Study Bible type
type StudyBibleType struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *StudyBibleType) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // Contains the work of Howard Clark Kee including a summary of the development of the canon, introductions to the books, notes and cross references. Originally published in 1993, NRSV
  case "CAM":
		c.Body = `Cambridge Annotated`

  // A project of Tyndale House Publishers and Zondervan intended to help readers apply the Bible to daily living. Living Bible, King James, New International, NASB
  case "LIF":
		c.Body = `Life Application`

  // A King James version study Bible with notes by James Macarthur first published in 1997
  case "MAC":
		c.Body = `Macarthur`

  // A study Bible originally published in the 1960s and based on the RSV / NRSV
  case "OXF":
		c.Body = `Oxford Annotated`

  // Norwegian study Bible, New Testament
  case "NNT":
		c.Body = `Studiebibel, Det Nye testamentet`

  // Published in 1991 and based on the New Revised Standard version
  case "NOX":
		c.Body = `New Oxford Annotated`

  // Norwegian study Bible
  case "NSB":
		c.Body = `Norsk studiebibel`

  // Based on the work of Charles C. Ryrie. King James, NI, NASB
  case "RYR":
		c.Body = `Ryrie`

  // A study Bible based on the early 20th century work of C.I. Scofield. Based on the King James version
  case "SCO":
		c.Body = `Scofield`

  // A transdenominational study Bible for persons from the Pentecostal/Charismatic traditions
  case "SPR":
		c.Body = `Spirit Filled`
	default:
		return fmt.Errorf("undefined code for StudyBibleType has been passed, got [%s]", v)
	}
	return nil
}

// SubjectDateRole Person / organization date role
type SubjectDateRole struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *SubjectDateRole) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // Date of birth
  case "50":
		c.Body = `Date of birth`

  // Date of death
  case "51":
		c.Body = `Date of death`

  // (‘Floruit’). To date the height of or most productive period during a career
  case "56":
		c.Body = `Flourished around`
	default:
		return fmt.Errorf("undefined code for SubjectDateRole has been passed, got [%s]", v)
	}
	return nil
}

// SubjectSchemeIdentifier Subject scheme identifier
type SubjectSchemeIdentifier struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *SubjectSchemeIdentifier) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // Dewey Decimal Classification
  case "01":
		c.Body = `Dewey`

  // Abridged Dewey
  case "02":
		c.Body = `Abridged Dewey`

  // US Library of Congress classification
  case "03":
		c.Body = `LC classification`

  // US Library of Congress subject heading
  case "04":
		c.Body = `LC subject heading`

  // US National Library of Medicine medical classification
  case "05":
		c.Body = `NLM classification`

  // US National Library of Medicine Medical subject heading
  case "06":
		c.Body = `MeSH heading`

  // US National Agricultural Library subject heading
  case "07":
		c.Body = `NAL subject heading`

  // Getty Art and Architecture Thesaurus heading
  case "08":
		c.Body = `AAT`

  // Universal Decimal Classification
  case "09":
		c.Body = `UDC`

  // BISAC Subject Headings are used in the North American market to categorize books based on topical content. They serve as a guideline for shelving books in physical stores and browsing books in online stores. See https://bisg.org/page/BISACSubjectCodes
  case "10":
		c.Body = `BISAC Subject Heading`

  // A geographical qualifier used with a BISAC subject category
  case "11":
		c.Body = `BISAC Regional theme`

  // For all BIC subject codes and qualifiers, see http://www.bic.org.uk/7/BIC-Standard-Subject-Categories/
  case "12":
		c.Body = `BIC subject category`

  // BIC geographical qualifier
  case "13":
		c.Body = `BIC geographical qualifier`

  // BIC language qualifier (language as subject)
  case "14":
		c.Body = `BIC language qualifier (language as subject)`

  // BIC time period qualifier
  case "15":
		c.Body = `BIC time period qualifier`

  // BIC educational purpose qualifier
  case "16":
		c.Body = `BIC educational purpose qualifier`

  // BIC reading level and special interest qualifier
  case "17":
		c.Body = `BIC reading level and special interest qualifier`

  // Used for German National Bibliography since 2004 (100 subjects). Is different from value 30. See http://www.dnb.de/service/pdf/ddc_wv_aktuell.pdf (in German) or http://www.dnb.de/eng/service/pdf/ddc_wv_aktuell_eng.pdf (English)
  case "18":
		c.Body = `DDC-Sachgruppen der Deutschen Nationalbibliografie`

  // LC fiction genre heading
  case "19":
		c.Body = `LC fiction genre heading`

  // For indexing and search purposes, not normally intended for display. Where multiple keywords or keyword phrases are sent, this should be in a single instance of the <SubjectHeadingText> element, and it is recommended that they should be separated by semi-colons (this is consistent with Library of Congress preferred practice)
  case "20":
		c.Body = `Keywords`

  // See PA/BIC CBMC guidelines at http://www.bic.org.uk/8/Children%27s-Books-Marketing-Classifications/
  case "21":
		c.Body = `BIC children’s book marketing category`

  // BISAC Merchandising Themes are used in addition to BISAC Subject Headings to denote an audience to which a work may be of particular appeal, a time of year or event for which a work may be especially appropriate, or to further describe fictional works that have been subject-coded by genre
  case "22":
		c.Body = `BISAC Merchandising Theme`

  // Publisher’s own category code
  case "23":
		c.Body = `Publisher’s own category code`

  // As specified in <SubjectSchemeName>
  case "24":
		c.Body = `Proprietary subject scheme`

  // Latin America
  case "25":
		c.Body = `Tabla de materias ISBN`

  // See http://info.vlb.de/files/wgsneuversion2_0.pdf (in German)
  case "26":
		c.Body = `Warengruppen-Systematik des deutschen Buchhandels`

  // Schlagwortnormdatei – Subject Headings Authority File in the German-speaking countries. See http://www.dnb.de/standardisierung/normdateien/swd.htm (in German) and http://www.dnb.de/eng/standardisierung/normdateien/swd.htm (English). DEPRECATED in favor of the GND
  case "27":
		c.Body = `SWD`

  // Subject classification used by Electre (France)
  case "28":
		c.Body = `Thèmes Electre`

  // France. A four-digit number, see http://www.clil.org/information/documentation.html (in French). The first digit identifies the version of the scheme
  case "29":
		c.Body = `CLIL`

  // Deutsche Bibliothek subject groups. Used for German National Bibliography until 2003 (65 subjects). Is different from value 18. See http://www.dnb.de/service/pdf/ddc_wv_alt_neu.pdf (in German)
  case "30":
		c.Body = `DNB-Sachgruppen`

  // Nederlandse Uniforme Genre-Indeling (former Dutch book trade classification)
  case "31":
		c.Body = `NUGI`

  // Nederlandstalige Uniforme Rubrieksindeling (Dutch book trade classification, from 2002, see http://www.boek.nl/nur (in Dutch)
  case "32":
		c.Body = `NUR`

  // ECPA Christian Product Category Book Codes, consisting of up to three x 3-letter blocks, for Super Category, Primary Category and Sub-Category. See http://www.ecpa.org/ECPA/cbacategories.xls
  case "33":
		c.Body = `ECPA Christian Book Category`

  // Schema Indeling Systematische Catalogus Openbare Bibliotheken (Dutch library classification)
  case "34":
		c.Body = `SISO`

  // A modified Dewey Decimal Classification used in the Republic of Korea
  case "35":
		c.Body = `Korean Decimal Classification (KDC)`

  // German Translation of Dewey Decimal Classification 22. Also known as DDC 22 ger. See http://www.ddc-deutsch.de/produkte/uebersichten/
  case "36":
		c.Body = `DDC Deutsch 22`

  // Norwegian book trade product categories (Bokgrupper) administered by the Norwegian Publishers Association (http://www.forleggerforeningen.no/)
  case "37":
		c.Body = `Bokgrupper`

  // Norwegian bookselling subject categories (Bokhandelens varegrupper) administered by the Norwegian Booksellers Association (http://bokhandlerforeningen.no/)
  case "38":
		c.Body = `Varegrupper`

  // Norwegian school curriculum version. Deprecated
  case "39":
		c.Body = `Læreplaner`

  // Japanese subject classification scheme
  case "40":
		c.Body = `Nippon Decimal Classification`

  // BookSelling Qualifier: Russian book trade classification
  case "41":
		c.Body = `BSQ`

  // Spain: subject coding scheme of the Asociación Nacional de Editores de Libros y Material de Enseñanza
  case "42":
		c.Body = `ANELE Materias`

  // Codes for Norwegian ‘utdanningsprogram’ used in secondary education. See: http://www.udir.no/. (Formerly labelled ‘Skolefag’)
  case "43":
		c.Body = `Utdanningsprogram`

  // Codes for Norwegian ‘programområde’ used in secondary education. See http://www.udir.no/. (Formerly labelled ‘Videregående’ or ‘Programfag’)
  case "44":
		c.Body = `Programområde`

  // Norwegian list of categories for books and other material used in education
  case "45":
		c.Body = `Undervisningsmateriell`

  // Norwegian version of Dewey Decimal Classification
  case "46":
		c.Body = `Norsk DDK`

  // Swedish bookselling subject categories
  case "47":
		c.Body = `Varugrupper`

  // Swedish classification scheme
  case "48":
		c.Body = `SAB`

  // Swedish bookselling educational subject type
  case "49":
		c.Body = `Läromedelstyp`

  // Swedish publishers preliminary subject classification
  case "50":
		c.Body = `Förhandsbeskrivning`

  // Controlled subset of UDC codes used by the Spanish ISBN Agency
  case "51":
		c.Body = `Spanish ISBN UDC subset`

  // Subject categories defined by El Corte Inglés and used widely in the Spanish book trade
  case "52":
		c.Body = `ECI subject categories`

  // Classificazione commerciale editoriale (Italian book trade subject category based on BIC). CCE documentation available at http://www.ie-online.it/CCE2_2.0.pdf
  case "53":
		c.Body = `Soggetto CCE`

  // CCE Geographical qualifier
  case "54":
		c.Body = `Qualificatore geografico CCE`

  // CCE Language qualifier
  case "55":
		c.Body = `Qualificatore di lingua CCE`

  // CCE Time Period qualifier
  case "56":
		c.Body = `Qualificatore di periodo storico CCE`

  // CCE Educational Purpose qualifier
  case "57":
		c.Body = `Qualificatore di livello scolastico CCE`

  // CCE Reading Level Qualifier
  case "58":
		c.Body = `Qualificatore di età di lettura CCE`

  // Subject code list of the German association of educational media publishers. See http://www.bildungsmedien.de/service/onixlisten/unterrichtsfach_onix_codelist27_value59_0408.pdf
  case "59":
		c.Body = `VdS Bildungsmedien Fächer`

  // Norwegian primary and secondary school subject categories (fagkoder), see http://www.udir.no/
  case "60":
		c.Body = `Fagkoder`

  // Journal of Economic Literature classification scheme
  case "61":
		c.Body = `JEL classification`

  // National Library of Canada subject heading (English)
  case "62":
		c.Body = `CSH`

  // Répertoire de vedettes-matière Bibliothèque de l’Université Laval) (French)
  case "63":
		c.Body = `RVM`

  // Yleinen suomalainen asiasanasto: Finnish General Thesaurus. See http://onki.fi/fi/browser/ (in Finnish)
  case "64":
		c.Body = `YSA`

  // Allmän tesaurus på svenska: Swedish translation of the Finnish General Thesaurus. See http://onki.fi/fi/browser/ (in Finnish)
  case "65":
		c.Body = `Allärs`

  // Yleisten kirjastojen luokitusjärjestelmä: Finnish Public Libraries Classification System. See http://ykl.kirjastot.fi/ (in Finnish)
  case "66":
		c.Body = `YKL`

  // Musiikin asiasanasto: Finnish Music Thesaurus. See http://onki.fi/fi/browser/ (in Finnish)
  case "67":
		c.Body = `MUSA`

  // Specialtesaurus för musik: Swedish translation of the Finnish Music Thesaurus. See http://onki.fi/fi/browser/ (in Finnish)
  case "68":
		c.Body = `CILLA`

  // Fiktiivisen aineiston asiasanasto: Finnish thesaurus for fiction. See http://kaunokki.kirjastot.fi/ (in Finnish)
  case "69":
		c.Body = `Kaunokki`

  // Specialtesaurus för fiktivt material: Swedish translation of the Finnish thesaurus for fiction. See http://kaunokki.kirjastot.fi/sv-FI/ (in Finnish)
  case "70":
		c.Body = `Bella`

  // Yleinen suomalainen ontologia: Finnish General Upper Ontology. See http://onki.fi/fi/browser/ (In Finnish)
  case "71":
		c.Body = `YSO`

  // Finnish Place Ontology. See http://onki.fi/fi/browser/ (in Finnish)
  case "72":
		c.Body = `Paikkatieto ontologia`

  // Finnish book trade categorisation
  case "73":
		c.Body = `Suomalainen kirja-alan luokitus`

  // Sears List of Subject Headings
  case "74":
		c.Body = `Sears`

  // BIC E4Libraries Category Headings, see http://www.bic.org.uk/51/E4libraries-Subject-Category-Headings/
  case "75":
		c.Body = `BIC E4L`

  // Code Sujet Rayon: subject categories used by bookstores in France
  case "76":
		c.Body = `CSR`

  // Finnish school subject categories
  case "77":
		c.Body = `Suomalainen oppiaineluokitus`

  // See http://www.asahi-net.or.jp/~ax2s-kmtn/ref/ccode.html (in Japanese)
  case "78":
		c.Body = `Japanese book trade C-Code`

  // Japanese book trade Genre Code
  case "79":
		c.Body = `Japanese book trade Genre Code`

  // Finnish fiction genre classification. See http://ykl.kirjastot.fi/fi-FI/lisaluokat/ (in Finnish)
  case "80":
		c.Body = `Fiktiivisen aineiston lisäluokitus`

  // Arabic Subject heading scheme
  case "81":
		c.Body = `Arabic Subject heading scheme`

  // Arabized version of BIC subject category scheme developed by ElKotob.com
  case "82":
		c.Body = `Arabized BIC subject category`

  // Arabized version of Library of Congress scheme
  case "83":
		c.Body = `Arabized LC subject headings`

  // Classification scheme used by Library of Alexandria
  case "84":
		c.Body = `Bibliotheca Alexandrina Subject Headings`

  // Location defined by postal code. Format is two-letter country code (from List 91), space, postal code. Note some postal codes themselves contain spaces, eg ‘GB N7 9DP’ or ‘US 10125’
  case "85":
		c.Body = `Postal code`

  // ID number for geographical place, as defined at http://www.geonames.org (eg 2825297 is Stuttgart, Germany, see http://www.geonames.org/2825297)
  case "86":
		c.Body = `GeoNames ID`

  // Used for classification of academic and specialist publication in German-speaking countries. See http://www.newbooks-services.com/de/top/unternehmensportrait/klassifikation-und-mapping.html (German) and http://www.newbooks-services.com/en/top/about-newbooks/classification-mapping.html (English)
  case "87":
		c.Body = `NewBooks Subject Classification`

  // Subject classification maintained by the Editorial Board of Chinese Library Classification. See http://cct.nlc.gov.cn for access to details of the scheme
  case "88":
		c.Body = `Chinese Library Classification`

  // Subject classification for Books, Audiovisual products and E-publications formulated by China National Technical Committee 505
  case "89":
		c.Body = `NTCPDSAC Classification`

  // German code scheme indicating association with seasons, holidays, events (eg Autumn, Back to School, Easter)
  case "90":
		c.Body = `Season and Event Indicator`

  // Gemeinsame Normdatei – Joint Authority File in the German-speaking countries. See http://www.dnb.de/EN/gnd (English). Combines the PND, SWD and GKD into a single authority file, and should be used in preference to the older codes
  case "91":
		c.Body = `GND`

  // UK Standard Library Categories, the successor to BIC’s E4L classification scheme
  case "92":
		c.Body = `BIC UKSLC`

  // International multilingual subject category scheme – see https://ns.editeur.org/thema
  case "93":
		c.Body = `Thema subject category`

  // Thema place qualifier
  case "94":
		c.Body = `Thema place qualifier`

  // Thema language qualifier
  case "95":
		c.Body = `Thema language qualifier`

  // Thema time period qualifier
  case "96":
		c.Body = `Thema time period qualifier`

  // Thema educational purpose qualifier
  case "97":
		c.Body = `Thema educational purpose qualifier`

  // Thema interest age / special interest qualifier
  case "98":
		c.Body = `Thema interest age / special interest qualifier`

  // Thema style qualifier
  case "99":
		c.Body = `Thema style qualifier`

  // Swedish subject categories maintained by Bokrondellen
  case "A2":
		c.Body = `Ämnesord`

  // Polish Statistical Book and E-book Classification
  case "A3":
		c.Body = `Statystyka Książek Papierowych, Mówionych I Elektronicznych`

  // Common Core State Standards curriculum alignment, for links to US educational standards. <SubjectCode> uses the full dot notation. See http://www.corestandards.org/developers-and-publishers
  case "A4":
		c.Body = `CCSS`

  // French library subject headings
  case "A5":
		c.Body = `Rameau`

  // French educational subject classification, URI http://data.education.fr/voc/scolomfr/scolomfr-voc-015GTPX
  case "A6":
		c.Body = `Nomenclature discipline scolaire`

  // International Standard Industry Classification, a classification of economic activities. Use for books that are about a particular industry or economic activity. <SubjectCode> should be a single letter denoting an ISIC section OR a 2-, 3- or 4-digit number denoting an ISIC division, group or class. See http://unstats.un.org/unsd/cr/registry/isic-4.asp
  case "A7":
		c.Body = `ISIC`

  // Library of Congress Children’s Subject Headings: LCSHAC supplementary headings for Children’s books
  case "A8":
		c.Body = `LC Children’s Subject Headings`

  // Swedish bookselling educational subject
  case "A9":
		c.Body = `Ny Läromedel`

  // EuroVoc multilingual thesaurus. <SubjectCode> should be a EuroVoc concept dc:identifier (for example, 2777, ‘refrigerated products’). See http://eurovoc.europa.eu
  case "B0":
		c.Body = `EuroVoc`

  // Controlled vocabulary for educational objectives. See https://www.bisg.org/educational-taxonomy
  case "B1":
		c.Body = `BISG Educational Taxonomy`

  // For indexing and search purposes, MUST not be displayed. Where multiple keywords or keyword phrases are sent, this should be in a single instance of the <SubjectHeadingText> element, and it is recommended that they should be separated by semi-colons. Use of code B2 should be very rare: use B2 in preference to code 20 only where it is important to show the keyword list is specifically NOT for display to purchasers (eg some keywords for a medical textbook may appear offensive if displayed out of context)
  case "B2":
		c.Body = `Keywords (not for display)`

  // French higher and vocational educational subject classification, URI http://data.education.fr/voc/scolomfr/scolomfr-voc-029
  case "B3":
		c.Body = `Nomenclature Diplôme`

  // For fiction and non-fiction, one or more key names, provided – like keywords – for indexing and search purposes. Where multiple character names are sent, this should be in a single instance of <SubjectHeadingText>, and multiple names should be separated by semi-colons. Note <NameAsSubject> should be used for people who are the central subject of the book. Code B4 may be used for names of lesser importance
  case "B4":
		c.Body = `Key character names`

  // For fiction and non-fiction, one or more key names, provded – like keywords – for indexing and search purposes. Where multiple place names are sent, this should in a single instance of <SubjectHeadingText>, and multiple names should be separated by semi-colons. For use in ONIX 3.0 only
  case "B5":
		c.Body = `Key place names`

  // Faceted Application of Subject Terminology, OCLC subject scheme based on but different from LCSH (see code 04). For use in ONIX 3.0 only
  case "B6":
		c.Body = `FAST`

  // Next Generation Science Standards for K-12 education in the USA (https://www.nextgenscience.org). <SubjectCode> is a code such as 4-PS3-2. For use in ONIX 3.0 only
  case "B7":
		c.Body = `NGSS`

  // MVB classification of ‘reading rationales’, which classify unconscious motives that lead to a book purchase. Categories are assigned and maintained by MVB. For use in ONIX 3.0 only. See https://vlb.de/lesemotive
  case "B8":
		c.Body = `MVB-Lesemotive`

  // Finnish Suomalainen oppiaineluokitus
  case "B9":
		c.Body = `LOPS21 Subject module`
	default:
		return fmt.Errorf("undefined code for SubjectSchemeIdentifier has been passed, got [%s]", v)
	}
	return nil
}

// SupplierCodeType Supplier own code type
type SupplierCodeType struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *SupplierCodeType) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // A rating applied by a supplier (typically a wholesaler) to indicate its assessment of the expected or actual sales performance of a product
  case "01":
		c.Body = `Supplier’s sales classification`

  // A supplier’s coding of the eligibility of a product for a bonus scheme on overall sales
  case "02":
		c.Body = `Supplier’s bonus eligibility`

  // A rating applied by the publisher to indicate a sales category (eg backlist/frontlist, core stock etc). Use only when the publisher is not the ‘supplier’
  case "03":
		c.Body = `Publisher’s sales classification`

  // A classification applied by a supplier to a product sold on Agency terms, to indicate that retail price restrictions are applicable
  case "04":
		c.Body = `Supplier’s pricing restriction classification`

  // Code is the ISBN of another book that had sales (both in terms of copy numbers and customer profile) comparable to that the distributor or supplier estimates for the product. <SupplierCodeValue> must be an ISBN-13 or GTIN-13
  case "05":
		c.Body = `Supplier’s sales expectation`

  // Code is the ISBN of another book that had sales (both in terms of copy numbers and customer profile) comparable to that the publisher estimates for the product. <SupplierCodeValue> must be an ISBN-13 or GTIN-13
  case "06":
		c.Body = `Publisher’s sales expectation`

  // Code indicates whether an order can be placed with the supplier indirectly via an intermediary system. The code name type indicates the specific intermediate order aggregation/routing platform and the code indicates the eligibility
  case "07":
		c.Body = `Supplier’s order routing eligibility`
	default:
		return fmt.Errorf("undefined code for SupplierCodeType has been passed, got [%s]", v)
	}
	return nil
}

// SupplierIDType Supplier identifier type
type SupplierIDType struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *SupplierIDType) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // Note that <IDTypeName> is required with proprietary identifiers
  case "01":
		c.Body = `Proprietary`

  // DEPRECATED – use 01
  case "02":
		c.Body = `Proprietary`

  // Börsenverein Verkehrsnummer
  case "04":
		c.Body = `Börsenverein Verkehrsnummer`

  // German ISBN Agency publisher identifier
  case "05":
		c.Body = `German ISBN Agency publisher identifier`

  // GS1 global location number (formerly EAN location number)
  case "06":
		c.Body = `GLN`

  // Book trade Standard Address Number – US, UK etc
  case "07":
		c.Body = `SAN`

  // Flemish supplier code
  case "12":
		c.Body = `Distributeurscode Boekenbank`

  // Flemish publisher code
  case "13":
		c.Body = `Fondscode Boekenbank`

  // Identifier for a business organization for VAT purposes, eg within the EU’s VIES system. See http://ec.europa.eu/taxation_customs/vies/faqvies.do for EU VAT ID formats, which vary from country to country. Generally these consist of a two-letter country code followed by the 8–12 digits of the national VAT ID. Some countries include one or two letters within their VAT ID. See http://en.wikipedia.org/wiki/VAT_identification_number for non-EU countries that maintain similar identifiers. Spaces, dashes etc should be omitted
  case "23":
		c.Body = `VAT Identity Number`
	default:
		return fmt.Errorf("undefined code for SupplierIDType has been passed, got [%s]", v)
	}
	return nil
}

// SupplierRole Supplier role
type SupplierRole struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *SupplierRole) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // Default
  case "00":
		c.Body = `Unspecified`

  // Publisher as supplier to retail trade outlets
  case "01":
		c.Body = `Publisher to retailers`

  // Publisher’s exclusive distributor to retailers
  case "02":
		c.Body = `Publisher’s exclusive distributor to retailers`

  // Publisher’s non-exclusive distributor to retailers
  case "03":
		c.Body = `Publisher’s non-exclusive distributor to retailers`

  // Wholesaler supplying retail trade outlets
  case "04":
		c.Body = `Wholesaler`

  // DEPRECATED – use <MarketRepresentation> (ONIX 2.1) or <MarketPublishingDetail> (ONIX 3.0) to specify a sales agent
  case "05":
		c.Body = `Sales agent`

  // In a specified supply territory. Use only where exclusive/non-exclusive status is not known. Prefer 02 or 03 as appropriate, where possible
  case "06":
		c.Body = `Publisher’s distributor to retailers`

  // Where a POD product is supplied to retailers and/or consumers direct from a POD source
  case "07":
		c.Body = `POD supplier`

  // Retailer
  case "08":
		c.Body = `Retailer`

  // Publisher as supplier direct to consumers and/or institutional customers
  case "09":
		c.Body = `Publisher to end-customers`

  // Intermediary as exclusive distributor direct to consumers and/or institutional customers
  case "10":
		c.Body = `Exclusive distributor to end-customers`

  // Intermediary as non-exclusive distributor direct to consumers and/or institutional customers
  case "11":
		c.Body = `Non-exclusive distributor to end-customers`

  // Use only where exclusive/non-exclusive status is not known. Prefer 10 or 11 as appropriate, where possible
  case "12":
		c.Body = `Distributor to end-customers`

  // Intermediary as exclusive distributor to retailers and direct to consumers and/or institutional customers. For use in ONIX 3.0 only
  case "13":
		c.Body = `Exclusive distributor to retailers and end-customers`

  // Intermediary as non-exclusive distributor to retailers and direct to consumers and/or institutional customers. For use in ONIX 3.0 only
  case "14":
		c.Body = `Non-exclusive distributor to retailers and end-customers`

  // Use only where exclusive/non-exclusive status is not known. Prefer codes 13 or 14 as appropriate whenever possible. For use in ONIX 3.0 only
  case "15":
		c.Body = `Distributor to retailers and end-customers`
	default:
		return fmt.Errorf("undefined code for SupplierRole has been passed, got [%s]", v)
	}
	return nil
}

// SupplyContactIDType Name identifier type
type SupplyContactIDType struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *SupplyContactIDType) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // Note that <IDTypeName> is required with proprietary identifiers
  case "01":
		c.Body = `Proprietary`

  // DEPRECATED – use 01
  case "02":
		c.Body = `Proprietary`

  // Deutsche Nationalbibliothek publisher identifier
  case "03":
		c.Body = `DNB publisher identifier`

  // Börsenverein Verkehrsnummer
  case "04":
		c.Body = `Börsenverein Verkehrsnummer`

  // German ISBN Agency publisher identifier
  case "05":
		c.Body = `German ISBN Agency publisher identifier`

  // GS1 global location number (formerly EAN location number)
  case "06":
		c.Body = `GLN`

  // Book trade Standard Address Number – US, UK etc
  case "07":
		c.Body = `SAN`

  // MARC code list for organizations – see http://www.loc.gov/marc/organizations/orgshome.html
  case "08":
		c.Body = `MARC organization code`

  // Trading party identifier used in the Netherlands
  case "10":
		c.Body = `Centraal Boekhuis Relatie ID`

  // Flemish publisher code
  case "13":
		c.Body = `Fondscode Boekenbank`

  // Business Identity Code (Finland). See http://www.ytj.fi/ (in Finnish)
  case "15":
		c.Body = `Y-tunnus`

  // International Standard Name Identifier. A sixteen digit number. Usually presented with spaces or hyphens dividing the number into four groups of four digits, but in ONIX the spaces or hyphens should be omitted. See http://www.isni.org/
  case "16":
		c.Body = `ISNI`

  // Personennamendatei – person name authority file used by Deutsche Nationalbibliothek and in other German-speaking countries. See http://www.dnb.de/standardisierung/normdateien/pnd.htm (German) or http://www.dnb.de/eng/standardisierung/normdateien/pnd.htm (English). DEPRECATED in favor of the GND
  case "17":
		c.Body = `PND`

  // A control number assigned to a Library of Congress Control Number (LCCN) Name Authority / NACO record
  case "18":
		c.Body = `NACO`

  // Publisher identifier administered by Japanese ISBN Agency
  case "19":
		c.Body = `Japanese Publisher identifier`

  // Gemeinsame Körperschaftsdatei – Corporate Body Authority File in the German-speaking countries. See http://www.dnb.de/standardisierung/normdateien/gkd.htm (German) or http://www.dnb.de/eng/standardisierung/normdateien/gkd.htm (English). DEPRECATED in favor of the GND
  case "20":
		c.Body = `GKD`

  // Open Researcher and Contributor ID. A sixteen digit number. Usually presented with hyphens dividing the number into four groups of four digits, but in ONIX the hyphens should be omitted. See http://www.orcid.org/
  case "21":
		c.Body = `ORCID`

  // Publisher identifier maintained by the Chinese ISBN Agency (GAPP)
  case "22":
		c.Body = `GAPP Publisher Identifier`

  // Identifier for a business organization for VAT purposes, eg within the EU’s VIES system. See http://ec.europa.eu/taxation_customs/vies/faqvies.do for EU VAT ID formats, which vary from country to country. Generally these consist of a two-letter country code followed by the 8–12 digits of the national VAT ID. Some countries include one or two letters within their VAT ID. See http://en.wikipedia.org/wiki/VAT_identification_number for non-EU countries that maintain similar identifiers. Spaces, dashes etc should be omitted
  case "23":
		c.Body = `VAT Identity Number`

  // 4-digit business organization identifier controlled by the Japanese Publication Wholesalers Association
  case "24":
		c.Body = `JP Distribution Identifier`

  // Gemeinsame Normdatei – Joint Authority File in the German-speaking countries. See http://www.dnb.de/EN/gnd (English). Combines the PND, SWD and GKD into a single authority file, and should be used in preference
  case "25":
		c.Body = `GND`

  // Dunn and Bradstreet Universal Numbering System, see http://www.dnb.co.uk/dandb-duns-number
  case "26":
		c.Body = `DUNS`

  // Ringgold organizational identifier, see http://www.ringgold.com/identify.html
  case "27":
		c.Body = `Ringgold ID`

  // French Electre publisher identifier
  case "28":
		c.Body = `Identifiant Editeur Electre`

  // DOI used in EIDR party registry, for example ‘10.5237/C9F6-F41F’ (Sam Raimi). See http://eidr.org
  case "29":
		c.Body = `EIDR Party DOI`

  // French Electre imprint Identifier
  case "30":
		c.Body = `Identifiant Marque Electre`

  // Virtual Internet Authority File. <IDValue> should be a number. The URI form of the identifier can be created by prefixing the number with ‘https://viaf.org/viaf/’. See https://viaf.org
  case "31":
		c.Body = `VIAF ID`

  // DOI used in CrossRef’s Open Funder Registry list of academic research funding bodies, for example ‘10.13039/100004440’ (Wellcome Trust). See http://www.crossref.org/fundingdata/registry.html
  case "32":
		c.Body = `FundRef DOI`

  // Control number assigned to a Name Authority record by the Biblioteca Nacional de España
  case "33":
		c.Body = `BNE CN`

  // Numéro de la notice de personne BNF
  case "34":
		c.Body = `BNF Control Number`

  // Archival Resource Key, as a URL (including the address of the ARK resolver provided by eg a national library)
  case "35":
		c.Body = `ARK`

  // Nasjonalt autoritetsregister for navn – Norwegian national authority file for personal and corporate names. For use in ONIX 3.0 only
  case "36":
		c.Body = `Nasjonalt autoritetsregister`

  // Global Research Identifier Database ID (see https://www.grid.ac). For use in ONIX 3.0 only
  case "37":
		c.Body = `GRID`

  // Party ID from Identifiers and Standards for Higher Education and Research (fr: Identifiants et Référentiels pour l’enseignement supérieur et la recherche). For use on ONIX 3.0 only. See https://www.idref.fr
  case "38":
		c.Body = `IDRef`
	default:
		return fmt.Errorf("undefined code for SupplyContactIDType has been passed, got [%s]", v)
	}
	return nil
}

// SupplyContactRole Supply contact role
type SupplyContactRole struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *SupplyContactRole) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // Eg for use where authorisation must be gained from the supplier (distributor or wholesaler)
  case "07":
		c.Body = `Return authorisation contact`

  // For general enquiries
  case "99":
		c.Body = `Customer services contact`
	default:
		return fmt.Errorf("undefined code for SupplyContactRole has been passed, got [%s]", v)
	}
	return nil
}

// SupplyDateRole Supply date role
type SupplyDateRole struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *SupplyDateRole) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // If there is an embargo on retail sales (of copies from the supplier) before a certain date and this is later than any general or market-wide embargo date, the date from which the embargo is lifted and retail sales and fulfillment of pre-orders are permitted. Use code 02 here ONLY in the exceptional case when the embargo is supplier-specific. More general market-wide or global sales embargos should be specified in <MarketDate> or <PublishingDate> codes. In the absence of any supplier-specific, market-wide or general embargo date, retail sales and pre-order fulfillment may begin as soon as stock is available to the retailer
  case "02":
		c.Body = `Sales embargo date`

  // The date on which physical stock is expected to be available to be shipped from the supplier to retailers, or a digital product is expected to be released by the publisher or digital asset distributor to retailers or their retail platform providers
  case "08":
		c.Body = `Expected availability date`

  // Last date when returns will be accepted, generally for a product which is being remaindered or put out of print
  case "18":
		c.Body = `Last date for returns`

  // Latest date on which an order may be placed for guaranteed delivery prior to the publication date. May or may not be linked to a special reservation or pre-publication price
  case "25":
		c.Body = `Reservation order deadline`

  // Latest date on which existing owners or licensees may download or re-download a copy of the product. Existing users may continue to use their local copy of the product
  case "29":
		c.Body = `Last redownload date`

  // Date on which any required technical protection measures (DRM) support will be withdrawn. DRM-protected products may not be usable after this date
  case "30":
		c.Body = `Last TPM date`

  // The date on which physical stock is expected to be delivered to the supplier from the manufacturer or from a primary distributor. For the distributor or wholesaler (the supplier) this is the ‘goods in’ date, as contrasted with the Expected availability date, code 08, which is the ‘goods out’ date
  case "34":
		c.Body = `Expected warehouse date`

  // First date on which the supplier specified in <NewSupplier> will accept orders. Note the first date would typically be the day after the old supplier end date, but they may overlap if there is an agreement to forward any orders between old and new supplier for fulfillment
  case "50":
		c.Body = `New supplier start date`

  // Last date on which the supplier specified in <Supplier> will accept orders. New supplier should be specified where available. Note last date would typically be the day before the new supplier start date, but they may overlap if there is an agreement to forward any orders between old and new supplier for fulfillment
  case "51":
		c.Body = `Supplier end date`
	default:
		return fmt.Errorf("undefined code for SupplyDateRole has been passed, got [%s]", v)
	}
	return nil
}

// TaxRateCode Tax rate type
type TaxRateCode struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *TaxRateCode) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // Specifies that tax is applied at a higher rate than standard
  case "H":
		c.Body = `Higher rate`

  // Under Italian tax rules, VAT on books may be paid at source by the publisher, and subsequent transactions through the supply chain are tax-exempt
  case "P":
		c.Body = `Tax paid at source (Italy)`

  // Specifies that tax is applied at a lower rate than standard. In the EU, use code R for ‘Reduced rates’, and for rates lower than 5%, use code T (‘Super-reduced’) or Z (Zero-rated)
  case "R":
		c.Body = `Lower rate`

  // Standard rate
  case "S":
		c.Body = `Standard rate`

  // Specifies that tax is applied at a rate lower than the Lower rate(s). In the EU, use code T for ‘Super-reduced rates’, and for Reduced rates (5% or above) use code R (Lower rate). For use in ONIX 3.0 only
  case "T":
		c.Body = `Super-low rate`

  // Zero-rated
  case "Z":
		c.Body = `Zero-rated`
	default:
		return fmt.Errorf("undefined code for TaxRateCode has been passed, got [%s]", v)
	}
	return nil
}

// TaxType Tax type
type TaxType struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *TaxType) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // Value-added tax (TVA, IVA, MwSt etc)
  case "01":
		c.Body = `VAT`

  // General sales tax
  case "02":
		c.Body = `GST`

  // ‘Green’ or eco-tax, levied to encourage responsible production or disposal, used only where this is identified separately from value-added or sales taxes
  case "03":
		c.Body = `ECO`
	default:
		return fmt.Errorf("undefined code for TaxType has been passed, got [%s]", v)
	}
	return nil
}

// TextItemIDType Text item identifier type
type TextItemIDType struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *TextItemIDType) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // For example, a publisher’s own identifier. Note that <IDTypeName> is required with proprietary identifiers
  case "01":
		c.Body = `Proprietary`

  // Formerly known as the EAN-13 (unhyphenated)
  case "03":
		c.Body = `GTIN-13`

  // DOI
  case "06":
		c.Body = `DOI`

  // Publisher item identifier
  case "09":
		c.Body = `PII`

  // For serial items only
  case "10":
		c.Body = `SICI`

  // ISTC
  case "11":
		c.Body = `ISTC`

  // (Unhyphenated)
  case "15":
		c.Body = `ISBN-13`

  // International Standard Content Code, a ‘similarity hash’ identifier derived algorithmically from the content itself (see https://iscc.codes). <IDValue> is the 27-character case-sensitive string (including one hyphen) comprising the Meta-ID and Content-ID components of a full ISCC generated from a digital manifestation of the work. Use only with ONIX 3.0
  case "39":
		c.Body = `ISCC`
	default:
		return fmt.Errorf("undefined code for TextItemIDType has been passed, got [%s]", v)
	}
	return nil
}

// TextItemType Text item type
type TextItemType struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *TextItemType) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // A complete work which is published as a content item in a product which carries two or more such works, eg when two or three novels are published in a single omnibus volume
  case "01":
		c.Body = `Textual work`

  // Text components such as Preface, Introduction etc which appear as preliminaries to the main body of text content in a product
  case "02":
		c.Body = `Front matter`

  // Text components such as Part, Chapter, Section etc which appear as part of the main body of text content in a product
  case "03":
		c.Body = `Body matter`

  // Text components such as Index which appear after the main body of text in a product
  case "04":
		c.Body = `Back matter`
	default:
		return fmt.Errorf("undefined code for TextItemType has been passed, got [%s]", v)
	}
	return nil
}

// TextType Text type
type TextType struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *TextType) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // To be used only in circumstances where the parties to an exchange have agreed to include text which (a) is not for general distribution, and (b) cannot be coded elsewhere. If more than one type of text is sent, it must be identified by tagging within the text itself
  case "01":
		c.Body = `Sender-defined text`

  // Limited to a maximum of 350 characters
  case "02":
		c.Body = `Short description/annotation`

  // Length unrestricted
  case "03":
		c.Body = `Description`

  // Used for a table of contents sent as a single text field, which may or may not carry structure expressed using XHTML
  case "04":
		c.Body = `Table of contents`

  // Primary descriptive blurb usually taken from the back cover or jacket, or occasionally from the cover/jacket flaps. See also code 27
  case "05":
		c.Body = `Primary cover copy`

  // A quote taken from a review of the product or of the work in question where there is no need to take account of different editions
  case "06":
		c.Body = `Review quote`

  // A quote taken from a review of a previous edition of the work
  case "07":
		c.Body = `Review quote: previous edition`

  // A quote taken from a review of a previous work by the same author(s) or in the same series
  case "08":
		c.Body = `Review quote: previous work`

  // A quote usually provided by a celebrity or another author to promote a new book, not from a review
  case "09":
		c.Body = `Endorsement`

  // A promotional phrase which is intended to headline a description of the product
  case "10":
		c.Body = `Promotional headline`

  // Text describing a feature of a product to which the publisher wishes to draw attention for promotional purposes. Each separate feature should be described by a separate repeat, so that formatting can be applied at the discretion of the receiver of the ONIX record, or multiple features can be described using appropriate XHTML markup
  case "11":
		c.Body = `Feature`

  // A note referring to all contributors to a product – NOT linked to a single contributor
  case "12":
		c.Body = `Biographical note`

  // A statement included by a publisher in fulfillment of contractual obligations, such as a disclaimer, sponsor statement, or legal notice of any sort. Note that the inclusion of such a notice cannot and does not imply that a user of the ONIX record is obliged to reproduce it
  case "13":
		c.Body = `Publisher’s notice`

  // A short excerpt from the main text of the work
  case "14":
		c.Body = `Excerpt`

  // Used for an index sent as a single text field, which may be structured using XHTML
  case "15":
		c.Body = `Index`

  // (of which the product is a part.) Limited to a maximum of 350 characters
  case "16":
		c.Body = `Short description/annotation for collection`

  // (of which the product is a part.) Length unrestricted
  case "17":
		c.Body = `Description for collection`

  // As code 11 but used for a new feature of this edition or version
  case "18":
		c.Body = `New feature`

  // Version history
  case "19":
		c.Body = `Version history`

  // Short summary statement of open access status and any related conditions (eg ‘Open access – no commercial use’), primarily for marketing purposes. Should always be accompanied by a link to the complete license (see <EpubLicense> or code 99 in List 158)
  case "20":
		c.Body = `Open access statement`

  // Short summary statement that the product is available only in digital formats (eg ‘Digital exclusive’). If a non-digital version is planned, <ContentDate> should be used to specify the date when exclusivity will end (use content date role code 15). If a non-digital version is available, the statement should not be included
  case "21":
		c.Body = `Digital exclusivity statement`

  // For example a recommendation or approval provided by a ministry of education or other official body. Use <Text> to provide details and ideally use <TextSourceCorporate> to name the approver
  case "22":
		c.Body = `Official recommendation`

  // Short description in format specified by Japanese Book Publishers Association
  case "23":
		c.Body = `JBPA description`

  // JSON-LD snippet suitable for use within an HTML <script type="application/ld+json"> tag, containing structured metadata suitable for use with schema.org
  case "24":
		c.Body = `schema.org snippet`

  // Errata
  case "25":
		c.Body = `Errata`

  // Introduction, preface or the text of other preliminary material, sent as a single text field, which may be structured using XHTML
  case "26":
		c.Body = `Introduction`

  // Secondary descriptive blurb taken from the cover/jacket flaps, or occasionally from the back cover or jacket, used only when there are two separate texts and the primary text is included using code 05
  case "27":
		c.Body = `Secondary cover copy`

  // For use with dramatized audiobooks, filmed entertainment etc, for a cast list sent as a single text field, which may or may not carry structure expressed using XHTML
  case "28":
		c.Body = `Full cast and credit list`

  // Complete list of books by the author(s), supplied as a single text field, which may be structured using (X)HTML
  case "29":
		c.Body = `Bibliography`

  // Formal summary of content (normally used with academic and scholarly content only)
  case "30":
		c.Body = `Abstract`

  // Eg for a game, kit
  case "31":
		c.Body = `Rules or instructions`

  // Eg for a game, kit. Note: use code 04 for a Table of Contents of a book
  case "32":
		c.Body = `List of contents`
	default:
		return fmt.Errorf("undefined code for TextType has been passed, got [%s]", v)
	}
	return nil
}

// ThesisType Thesis type
type ThesisType struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *ThesisType) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // Professorial dissertation (thesis for postdoctoral lecturing qualification)
  case "01":
		c.Body = `Habilitationsschrift`

  // Doctoral thesis
  case "02":
		c.Body = `Dissertationsschrift`

  // State examination thesis
  case "03":
		c.Body = `Staatsexamensarbeit`

  // Magisters degree thesis
  case "04":
		c.Body = `Magisterarbeit`

  // Diploma degree thesis
  case "05":
		c.Body = `Diplomarbeit`

  // Bachelors degree thesis
  case "06":
		c.Body = `Bachelorarbeit`

  // Masters degree thesis
  case "07":
		c.Body = `Masterarbeit`
	default:
		return fmt.Errorf("undefined code for ThesisType has been passed, got [%s]", v)
	}
	return nil
}

// TitleElementLevel Title element level
type TitleElementLevel struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *TitleElementLevel) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // The title element refers to an individual product
  case "01":
		c.Body = `Product`

  // The title element refers to the top level of a bibliographic collection
  case "02":
		c.Body = `Collection level`

  // The title element refers to an intermediate level of a bibliographic collection that comprises two or more ‘sub-collections’
  case "03":
		c.Body = `Subcollection`

  // The title element refers to a content item within a product, eg a work included in a combined or ‘omnibus’ edition, or a chapter in a book. Generally used only for titles within <ContentItem> (Block 3)
  case "04":
		c.Body = `Content item`

  // The title element names a master brand where the use of the brand spans multiple collections and product forms, and possibly multiple imprints and publishers. Used only for branded media properties carrying, for example, a children’s character brand
  case "05":
		c.Body = `Master brand`

  // The title element refers to an intermediate level of a bibliographic collection that is a subdivision of a sub-collection (a third level of collective identity)
  case "06":
		c.Body = `Sub-subcollection`
	default:
		return fmt.Errorf("undefined code for TitleElementLevel has been passed, got [%s]", v)
	}
	return nil
}

// TitleType Title type
type TitleType struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *TitleType) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // Undefined
  case "00":
		c.Body = `Undefined`

  // The full text of the distinctive title of the item, without abbreviation or abridgement. For books, where the title alone is not distinctive, elements may be taken from a set or series title and part number etc to create a distinctive title. Where the item is an omnibus edition containing two or more works by the same author, and there is no separate combined title, a distinctive title may be constructed by concatenating the individual titles, with suitable punctuation, as in ‘Pride and prejudice / Sense and sensibility / Northanger Abbey’
  case "01":
		c.Body = `Distinctive title (book); Cover title (serial); Title on item (serial content item or reviewed resource)`

  // Serials only
  case "02":
		c.Body = `ISSN key title of serial`

  // Where the subject of the ONIX record is a translated item
  case "03":
		c.Body = `Title in original language`

  // For serials: an acronym or initialism of Title Type 01, eg ‘JAMA’, ‘JACM’
  case "04":
		c.Body = `Title acronym or initialism`

  // An abbreviated form of Title Type 01
  case "05":
		c.Body = `Abbreviated title`

  // A translation of Title Type 01 into another language
  case "06":
		c.Body = `Title in other language`

  // Serials only: when a journal issue is explicitly devoted to a specified topic
  case "07":
		c.Body = `Thematic title of journal issue`

  // Books or serials: when an item was previously published under another title
  case "08":
		c.Body = `Former title`

  // For books: the title carried in a book distributor’s title file: frequently incomplete, and may include elements not properly part of the title
  case "10":
		c.Body = `Distributor’s title`

  // An alternative title that appears on the cover of a book
  case "11":
		c.Body = `Alternative title on cover`

  // An alternative title that appears on the back of a book
  case "12":
		c.Body = `Alternative title on back`

  // An expanded form of the title, eg the title of a school text book with grade and type and other details added to make the title meaningful, where otherwise it would comprise only the curriculum subject. This title type is required for submissions to the Spanish ISBN Agency
  case "13":
		c.Body = `Expanded title`

  // An alternative title that the book is widely known by, whether it appears on the book or not
  case "14":
		c.Body = `Alternative title`
	default:
		return fmt.Errorf("undefined code for TitleType has been passed, got [%s]", v)
	}
	return nil
}

// ToLanguage Language – based on ISO 639-2/B
type ToLanguage struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *ToLanguage) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // Afar
  case "aar":
		c.Body = `Afar`

  // Abkhaz
  case "abk":
		c.Body = `Abkhaz`

  // Achinese
  case "ace":
		c.Body = `Achinese`

  // Acoli
  case "ach":
		c.Body = `Acoli`

  // Adangme
  case "ada":
		c.Body = `Adangme`

  // Adygei
  case "ady":
		c.Body = `Adygei`

  // Collective name
  case "afa":
		c.Body = `Afro-Asiatic languages`

  // Artificial language
  case "afh":
		c.Body = `Afrihili`

  // Afrikaans
  case "afr":
		c.Body = `Afrikaans`

  // Ainu
  case "ain":
		c.Body = `Ainu`

  // Macrolanguage
  case "aka":
		c.Body = `Akan`

  // Akkadian
  case "akk":
		c.Body = `Akkadian`

  // Macrolanguage
  case "alb":
		c.Body = `Albanian`

  // Aleut
  case "ale":
		c.Body = `Aleut`

  // Collective name
  case "alg":
		c.Body = `Algonquian languages`

  // Southern Altai
  case "alt":
		c.Body = `Southern Altai`

  // Amharic
  case "amh":
		c.Body = `Amharic`

  // English, Old (ca. 450-1100)
  case "ang":
		c.Body = `English, Old (ca. 450-1100)`

  // Angika
  case "anp":
		c.Body = `Angika`

  // Collective name
  case "apa":
		c.Body = `Apache languages`

  // Macrolanguage
  case "ara":
		c.Body = `Arabic`

  // Official Aramaic; Imperial Aramaic (700-300 BCE)
  case "arc":
		c.Body = `Official Aramaic; Imperial Aramaic (700-300 BCE)`

  // Aragonese
  case "arg":
		c.Body = `Aragonese`

  // Armenian
  case "arm":
		c.Body = `Armenian`

  // Mapudungun; Mapuche
  case "arn":
		c.Body = `Mapudungun; Mapuche`

  // Arapaho
  case "arp":
		c.Body = `Arapaho`

  // Collective name
  case "art":
		c.Body = `Artificial languages`

  // Arawak
  case "arw":
		c.Body = `Arawak`

  // Assamese
  case "asm":
		c.Body = `Assamese`

  // Asturian; Bable; Leonese; Asturleonese
  case "ast":
		c.Body = `Asturian; Bable; Leonese; Asturleonese`

  // Collective name
  case "ath":
		c.Body = `Athapascan languages`

  // Collective name
  case "aus":
		c.Body = `Australian languages`

  // Avaric
  case "ava":
		c.Body = `Avaric`

  // Avestan
  case "ave":
		c.Body = `Avestan`

  // Awadhi
  case "awa":
		c.Body = `Awadhi`

  // Macrolanguage
  case "aym":
		c.Body = `Aymara`

  // Macrolanguage
  case "aze":
		c.Body = `Azerbaijani`

  // Collective name
  case "bad":
		c.Body = `Banda languages`

  // Collective name
  case "bai":
		c.Body = `Bamileke languages`

  // Bashkir
  case "bak":
		c.Body = `Bashkir`

  // Macrolanguage
  case "bal":
		c.Body = `Baluchi`

  // Bambara
  case "bam":
		c.Body = `Bambara`

  // Balinese
  case "ban":
		c.Body = `Balinese`

  // Basque
  case "baq":
		c.Body = `Basque`

  // Basa
  case "bas":
		c.Body = `Basa`

  // Collective name
  case "bat":
		c.Body = `Baltic languages`

  // Beja; Bedawiyet
  case "bej":
		c.Body = `Beja; Bedawiyet`

  // Belarusian
  case "bel":
		c.Body = `Belarusian`

  // Bemba
  case "bem":
		c.Body = `Bemba`

  // Bengali
  case "ben":
		c.Body = `Bengali`

  // Collective name
  case "ber":
		c.Body = `Berber languages`

  // Bhojpuri
  case "bho":
		c.Body = `Bhojpuri`

  // Collective name
  case "bih":
		c.Body = `Bihari languages`

  // Macrolanguage
  case "bik":
		c.Body = `Bikol`

  // Bini; Edo
  case "bin":
		c.Body = `Bini; Edo`

  // Bislama
  case "bis":
		c.Body = `Bislama`

  // Siksika
  case "bla":
		c.Body = `Siksika`

  // Collective name
  case "bnt":
		c.Body = `Bantu languages`

  // Bosnian
  case "bos":
		c.Body = `Bosnian`

  // Braj
  case "bra":
		c.Body = `Braj`

  // Breton
  case "bre":
		c.Body = `Breton`

  // Collective name
  case "btk":
		c.Body = `Batak languages`

  // Macrolanguage
  case "bua":
		c.Body = `Buriat`

  // Buginese
  case "bug":
		c.Body = `Buginese`

  // Bulgarian
  case "bul":
		c.Body = `Bulgarian`

  // Burmese
  case "bur":
		c.Body = `Burmese`

  // Blin; Bilin
  case "byn":
		c.Body = `Blin; Bilin`

  // Caddo
  case "cad":
		c.Body = `Caddo`

  // Collective name
  case "cai":
		c.Body = `Central American Indian languages`

  // Galibi Carib
  case "car":
		c.Body = `Galibi Carib`

  // Catalan
  case "cat":
		c.Body = `Catalan`

  // Collective name
  case "cau":
		c.Body = `Caucasian languages`

  // Cebuano
  case "ceb":
		c.Body = `Cebuano`

  // Collective name
  case "cel":
		c.Body = `Celtic languages`

  // Chamorro
  case "cha":
		c.Body = `Chamorro`

  // Chibcha
  case "chb":
		c.Body = `Chibcha`

  // Chechen
  case "che":
		c.Body = `Chechen`

  // Chagatai
  case "chg":
		c.Body = `Chagatai`

  // Macrolanguage
  case "chi":
		c.Body = `Chinese`

  // Chuukese (Truk)
  case "chk":
		c.Body = `Chuukese (Truk)`

  // Macrolanguage
  case "chm":
		c.Body = `Mari`

  // Chinook jargon
  case "chn":
		c.Body = `Chinook jargon`

  // Choctaw
  case "cho":
		c.Body = `Choctaw`

  // Chipewyan; Dene Suline
  case "chp":
		c.Body = `Chipewyan; Dene Suline`

  // Cherokee
  case "chr":
		c.Body = `Cherokee`

  // Church Slavic; Old Slavonic; Church Slavonic; Old Bulgarian; Old Church Slavonic
  case "chu":
		c.Body = `Church Slavic; Old Slavonic; Church Slavonic; Old Bulgarian; Old Church Slavonic`

  // Chuvash
  case "chv":
		c.Body = `Chuvash`

  // Cheyenne
  case "chy":
		c.Body = `Cheyenne`

  // ONIX local code, equivalent to ckb in ISO 639-3. For use in ONIX 3.0 only
  case "ckb":
		c.Body = `Central Kurdish (Sorani)`

  // Collective name
  case "cmc":
		c.Body = `Chamic languages`

  // ONIX local code, equivalent to cmn in ISO 639-3
  case "cmn":
		c.Body = `Mandarin`

  // For use in ONIX 3.0 only
  case "cnr":
		c.Body = `Montenegrin`

  // Coptic
  case "cop":
		c.Body = `Coptic`

  // Cornish
  case "cor":
		c.Body = `Cornish`

  // Corsican
  case "cos":
		c.Body = `Corsican`

  // Collective name
  case "cpe":
		c.Body = `Creoles and pidgins, English-based`

  // Collective name
  case "cpf":
		c.Body = `Creoles and pidgins, French-based`

  // Collective name
  case "cpp":
		c.Body = `Creoles and pidgins, Portuguese-based`

  // Macrolanguage
  case "cre":
		c.Body = `Cree`

  // Crimean Turkish; Crimean Tatar
  case "crh":
		c.Body = `Crimean Turkish; Crimean Tatar`

  // Collective name
  case "crp":
		c.Body = `Creoles and pidgins`

  // Kashubian
  case "csb":
		c.Body = `Kashubian`

  // Collective name
  case "cus":
		c.Body = `Cushitic languages`

  // Czech
  case "cze":
		c.Body = `Czech`

  // Dakota
  case "dak":
		c.Body = `Dakota`

  // Danish
  case "dan":
		c.Body = `Danish`

  // Dargwa
  case "dar":
		c.Body = `Dargwa`

  // Collective name
  case "day":
		c.Body = `Land Dayak languages`

  // Macrolanguage
  case "del":
		c.Body = `Delaware`

  // Macrolanguage
  case "den":
		c.Body = `Slave (Athapascan)`

  // Dogrib
  case "dgr":
		c.Body = `Dogrib`

  // Macrolanguage
  case "din":
		c.Body = `Dinka`

  // Divehi; Dhivehi; Maldivian
  case "div":
		c.Body = `Divehi; Dhivehi; Maldivian`

  // Macrolanguage
  case "doi":
		c.Body = `Dogri`

  // Collective name
  case "dra":
		c.Body = `Dravidian languages`

  // Lower Sorbian
  case "dsb":
		c.Body = `Lower Sorbian`

  // Duala
  case "dua":
		c.Body = `Duala`

  // Dutch, Middle (ca. 1050-1350)
  case "dum":
		c.Body = `Dutch, Middle (ca. 1050-1350)`

  // Dutch; Flemish
  case "dut":
		c.Body = `Dutch; Flemish`

  // Dyula
  case "dyu":
		c.Body = `Dyula`

  // Dzongkha
  case "dzo":
		c.Body = `Dzongkha`

  // Efik
  case "efi":
		c.Body = `Efik`

  // ONIX local code for Italian dialect, equivalent to egl in ISO 639-3. For use in ONIX 3.0 only
  case "egl":
		c.Body = `Emilian`

  // Egyptian (Ancient)
  case "egy":
		c.Body = `Egyptian (Ancient)`

  // Ekajuk
  case "eka":
		c.Body = `Ekajuk`

  // Elamite
  case "elx":
		c.Body = `Elamite`

  // English
  case "eng":
		c.Body = `English`

  // English, Middle (1100-1500)
  case "enm":
		c.Body = `English, Middle (1100-1500)`

  // Artificial language
  case "epo":
		c.Body = `Esperanto`

  // Macrolanguage
  case "est":
		c.Body = `Estonian`

  // Ewe
  case "ewe":
		c.Body = `Ewe`

  // Ewondo
  case "ewo":
		c.Body = `Ewondo`

  // Fang
  case "fan":
		c.Body = `Fang`

  // Faroese
  case "fao":
		c.Body = `Faroese`

  // Fanti
  case "fat":
		c.Body = `Fanti`

  // Fijian
  case "fij":
		c.Body = `Fijian`

  // Filipino; Pilipino
  case "fil":
		c.Body = `Filipino; Pilipino`

  // Finnish
  case "fin":
		c.Body = `Finnish`

  // ONIX local code, equivalent to fit in ISO 639-3
  case "fit":
		c.Body = `Meänkieli / Tornedalen Finnish`

  // Collective name
  case "fiu":
		c.Body = `Finno-Ugrian languages`

  // ONIX local code, equivalent to fkv in ISO 639-3
  case "fkv":
		c.Body = `Kvensk`

  // Fon
  case "fon":
		c.Body = `Fon`

  // French
  case "fre":
		c.Body = `French`

  // French, Middle (ca. 1400-1600)
  case "frm":
		c.Body = `French, Middle (ca. 1400-1600)`

  // French, Old (ca. 842-1400)
  case "fro":
		c.Body = `French, Old (ca. 842-1400)`

  // Northern Frisian
  case "frr":
		c.Body = `Northern Frisian`

  // Eastern Frisian
  case "frs":
		c.Body = `Eastern Frisian`

  // Western Frisian
  case "fry":
		c.Body = `Western Frisian`

  // Fulah
  case "ful":
		c.Body = `Fulah`

  // Friulian
  case "fur":
		c.Body = `Friulian`

  // Gã
  case "gaa":
		c.Body = `Gã`

  // Gayo
  case "gay":
		c.Body = `Gayo`

  // Macrolanguage
  case "gba":
		c.Body = `Gbaya`

  // Collective name
  case "gem":
		c.Body = `Germanic languages`

  // Georgian
  case "geo":
		c.Body = `Georgian`

  // German
  case "ger":
		c.Body = `German`

  // Ethiopic (Ge’ez)
  case "gez":
		c.Body = `Ethiopic (Ge’ez)`

  // Gilbertese
  case "gil":
		c.Body = `Gilbertese`

  // Scottish Gaelic
  case "gla":
		c.Body = `Scottish Gaelic`

  // Irish
  case "gle":
		c.Body = `Irish`

  // Galician
  case "glg":
		c.Body = `Galician`

  // Manx
  case "glv":
		c.Body = `Manx`

  // German, Middle High (ca. 1050-1500)
  case "gmh":
		c.Body = `German, Middle High (ca. 1050-1500)`

  // German, Old High (ca. 750-1050)
  case "goh":
		c.Body = `German, Old High (ca. 750-1050)`

  // Macrolanguage
  case "gon":
		c.Body = `Gondi`

  // Gorontalo
  case "gor":
		c.Body = `Gorontalo`

  // Gothic
  case "got":
		c.Body = `Gothic`

  // Macrolanguage
  case "grb":
		c.Body = `Grebo`

  // Greek, Ancient (to 1453)
  case "grc":
		c.Body = `Greek, Ancient (to 1453)`

  // Greek, Modern (1453-)
  case "gre":
		c.Body = `Greek, Modern (1453-)`

  // Macrolanguage
  case "grn":
		c.Body = `Guarani`

  // ONIX local code, equivalent to grt in ISO 639-3
  case "grt":
		c.Body = `Garo`

  // Swiss German; Alemannic
  case "gsw":
		c.Body = `Swiss German; Alemannic`

  // Gujarati
  case "guj":
		c.Body = `Gujarati`

  // Gwich’in
  case "gwi":
		c.Body = `Gwich’in`

  // Macrolanguage
  case "hai":
		c.Body = `Haida`

  // Haitian French Creole
  case "hat":
		c.Body = `Haitian French Creole`

  // Hausa
  case "hau":
		c.Body = `Hausa`

  // Hawaiian
  case "haw":
		c.Body = `Hawaiian`

  // Hebrew
  case "heb":
		c.Body = `Hebrew`

  // Herero
  case "her":
		c.Body = `Herero`

  // Hiligaynon
  case "hil":
		c.Body = `Hiligaynon`

  // Collective name
  case "him":
		c.Body = `Himachali languages; Western Pahari languages`

  // Hindi
  case "hin":
		c.Body = `Hindi`

  // Hittite
  case "hit":
		c.Body = `Hittite`

  // Macrolanguage
  case "hmn":
		c.Body = `Hmong; Mong`

  // Hiri Motu
  case "hmo":
		c.Body = `Hiri Motu`

  // Croatian
  case "hrv":
		c.Body = `Croatian`

  // Upper Sorbian
  case "hsb":
		c.Body = `Upper Sorbian`

  // Hungarian
  case "hun":
		c.Body = `Hungarian`

  // Hupa
  case "hup":
		c.Body = `Hupa`

  // Iban
  case "iba":
		c.Body = `Iban`

  // Igbo
  case "ibo":
		c.Body = `Igbo`

  // Icelandic
  case "ice":
		c.Body = `Icelandic`

  // Artificial language
  case "ido":
		c.Body = `Ido`

  // Sichuan Yi; Nuosu
  case "iii":
		c.Body = `Sichuan Yi; Nuosu`

  // Collective name
  case "ijo":
		c.Body = `Ijo languages`

  // Macrolanguage
  case "iku":
		c.Body = `Inuktitut`

  // Artificial language
  case "ile":
		c.Body = `Interlingue; Occidental`

  // Iloko
  case "ilo":
		c.Body = `Iloko`

  // Artificial language
  case "ina":
		c.Body = `Interlingua (International Auxiliary Language Association)`

  // Collective name
  case "inc":
		c.Body = `Indic languages`

  // Indonesian
  case "ind":
		c.Body = `Indonesian`

  // Collective name
  case "ine":
		c.Body = `Indo-European languages`

  // Ingush
  case "inh":
		c.Body = `Ingush`

  // Macrolanguage
  case "ipk":
		c.Body = `Inupiaq`

  // Collective name
  case "ira":
		c.Body = `Iranian languages`

  // Collective name
  case "iro":
		c.Body = `Iroquoian languages`

  // Italian
  case "ita":
		c.Body = `Italian`

  // Javanese
  case "jav":
		c.Body = `Javanese`

  // Lojban
  case "jbo":
		c.Body = `Lojban`

  // Japanese
  case "jpn":
		c.Body = `Japanese`

  // Judeo-Persian
  case "jpr":
		c.Body = `Judeo-Persian`

  // Macrolanguage
  case "jrb":
		c.Body = `Judeo-Arabic`

  // Kara-Kalpak
  case "kaa":
		c.Body = `Kara-Kalpak`

  // Kabyle
  case "kab":
		c.Body = `Kabyle`

  // Kachin; Jingpho
  case "kac":
		c.Body = `Kachin; Jingpho`

  // Kalâtdlisut; Greenlandic
  case "kal":
		c.Body = `Kalâtdlisut; Greenlandic`

  // Kamba
  case "kam":
		c.Body = `Kamba`

  // Kannada
  case "kan":
		c.Body = `Kannada`

  // Collective name
  case "kar":
		c.Body = `Karen languages`

  // Kashmiri
  case "kas":
		c.Body = `Kashmiri`

  // Macrolanguage
  case "kau":
		c.Body = `Kanuri`

  // Kawi
  case "kaw":
		c.Body = `Kawi`

  // Kazakh
  case "kaz":
		c.Body = `Kazakh`

  // Kabardian (Circassian)
  case "kbd":
		c.Body = `Kabardian (Circassian)`

  // ONIX local code, equivalent to kdr in ISO 639-3
  case "kdr":
		c.Body = `Karaim`

  // Khasi
  case "kha":
		c.Body = `Khasi`

  // Collective name
  case "khi":
		c.Body = `Khoisan languages`

  // Central Khmer
  case "khm":
		c.Body = `Central Khmer`

  // Khotanese; Sakan
  case "kho":
		c.Body = `Khotanese; Sakan`

  // Kikuyu; Gikuyu
  case "kik":
		c.Body = `Kikuyu; Gikuyu`

  // Kinyarwanda
  case "kin":
		c.Body = `Kinyarwanda`

  // Kirghiz; Kyrgyz
  case "kir":
		c.Body = `Kirghiz; Kyrgyz`

  // Kimbundu
  case "kmb":
		c.Body = `Kimbundu`

  // Macrolanguage
  case "kok":
		c.Body = `Konkani`

  // Macrolanguage
  case "kom":
		c.Body = `Komi`

  // Macrolanguage
  case "kon":
		c.Body = `Kongo`

  // Korean
  case "kor":
		c.Body = `Korean`

  // Kusaiean (Caroline Islands)
  case "kos":
		c.Body = `Kusaiean (Caroline Islands)`

  // Macrolanguage
  case "kpe":
		c.Body = `Kpelle`

  // Karachay-Balkar
  case "krc":
		c.Body = `Karachay-Balkar`

  // Karelian
  case "krl":
		c.Body = `Karelian`

  // Collective name
  case "kro":
		c.Body = `Kru languages`

  // Kurukh
  case "kru":
		c.Body = `Kurukh`

  // Kuanyama
  case "kua":
		c.Body = `Kuanyama`

  // Kumyk
  case "kum":
		c.Body = `Kumyk`

  // Macrolanguage
  case "kur":
		c.Body = `Kurdish`

  // Kutenai
  case "kut":
		c.Body = `Kutenai`

  // Ladino
  case "lad":
		c.Body = `Ladino`

  // Macrolanguage
  case "lah":
		c.Body = `Lahnda`

  // Lamba
  case "lam":
		c.Body = `Lamba`

  // Lao
  case "lao":
		c.Body = `Lao`

  // Latin
  case "lat":
		c.Body = `Latin`

  // Macrolanguage
  case "lav":
		c.Body = `Latvian`

  // Lezgian
  case "lez":
		c.Body = `Lezgian`

  // ONIX local code for Italian dialect, equivalent to lij in ISO 639-3. For use in ONIX 3.0 only
  case "lij":
		c.Body = `Ligurian`

  // Limburgish
  case "lim":
		c.Body = `Limburgish`

  // Lingala
  case "lin":
		c.Body = `Lingala`

  // Lithuanian
  case "lit":
		c.Body = `Lithuanian`

  // ONIX local code for Italian dialect, equivalent to lmo in ISO 639-3. For use in ONIX 3.0 only
  case "lmo":
		c.Body = `Lombard`

  // Mongo-Nkundu
  case "lol":
		c.Body = `Mongo-Nkundu`

  // Lozi
  case "loz":
		c.Body = `Lozi`

  // Luxembourgish; Letzeburgesch
  case "ltz":
		c.Body = `Luxembourgish; Letzeburgesch`

  // Luba-Lulua
  case "lua":
		c.Body = `Luba-Lulua`

  // Luba-Katanga
  case "lub":
		c.Body = `Luba-Katanga`

  // Ganda
  case "lug":
		c.Body = `Ganda`

  // Luiseño
  case "lui":
		c.Body = `Luiseño`

  // Lunda
  case "lun":
		c.Body = `Lunda`

  // Luo (Kenya and Tanzania)
  case "luo":
		c.Body = `Luo (Kenya and Tanzania)`

  // Lushai
  case "lus":
		c.Body = `Lushai`

  // Macedonian
  case "mac":
		c.Body = `Macedonian`

  // Madurese
  case "mad":
		c.Body = `Madurese`

  // Magahi
  case "mag":
		c.Body = `Magahi`

  // Marshallese
  case "mah":
		c.Body = `Marshallese`

  // Maithili
  case "mai":
		c.Body = `Maithili`

  // Makasar
  case "mak":
		c.Body = `Makasar`

  // Malayalam
  case "mal":
		c.Body = `Malayalam`

  // Macrolanguage
  case "man":
		c.Body = `Mandingo`

  // Maori
  case "mao":
		c.Body = `Maori`

  // Collective name
  case "map":
		c.Body = `Austronesian languages`

  // Marathi
  case "mar":
		c.Body = `Marathi`

  // Masai
  case "mas":
		c.Body = `Masai`

  // Macrolanguage
  case "may":
		c.Body = `Malay`

  // Moksha
  case "mdf":
		c.Body = `Moksha`

  // Mandar
  case "mdr":
		c.Body = `Mandar`

  // Mende
  case "men":
		c.Body = `Mende`

  // Irish, Middle (ca. 1100-1550)
  case "mga":
		c.Body = `Irish, Middle (ca. 1100-1550)`

  // Mi’kmaq; Micmac
  case "mic":
		c.Body = `Mi’kmaq; Micmac`

  // Minangkabau
  case "min":
		c.Body = `Minangkabau`

  // Use where no suitable code is available
  case "mis":
		c.Body = `Uncoded languages`

  // Collective name
  case "mkh":
		c.Body = `Mon-Khmer languages`

  // Macrolanguage
  case "mlg":
		c.Body = `Malagasy`

  // Maltese
  case "mlt":
		c.Body = `Maltese`

  // Manchu
  case "mnc":
		c.Body = `Manchu`

  // Manipuri
  case "mni":
		c.Body = `Manipuri`

  // Collective name
  case "mno":
		c.Body = `Manobo languages`

  // Mohawk
  case "moh":
		c.Body = `Mohawk`

  // DEPRECATED – use rum
  case "mol":
		c.Body = `Moldavian; Moldovan`

  // Macrolanguage
  case "mon":
		c.Body = `Mongolian`

  // Mooré; Mossi
  case "mos":
		c.Body = `Mooré; Mossi`

  // Multiple languages
  case "mul":
		c.Body = `Multiple languages`

  // Collective name
  case "mun":
		c.Body = `Munda languages`

  // Creek
  case "mus":
		c.Body = `Creek`

  // ONIX local code, equivalent to mwf in ISO 639-3. For use in ONIX 3.0 only
  case "mwf":
		c.Body = `Murrinh-Patha`

  // Mirandese
  case "mwl":
		c.Body = `Mirandese`

  // Macrolanguage
  case "mwr":
		c.Body = `Marwari`

  // Collective name
  case "myn":
		c.Body = `Mayan languages`

  // Erzya
  case "myv":
		c.Body = `Erzya`

  // Collective name
  case "nah":
		c.Body = `Nahuatl languages`

  // Collective name
  case "nai":
		c.Body = `North American Indian languages`

  // Neapolitan
  case "nap":
		c.Body = `Neapolitan`

  // Nauruan
  case "nau":
		c.Body = `Nauruan`

  // Navajo
  case "nav":
		c.Body = `Navajo`

  // Ndebele, South
  case "nbl":
		c.Body = `Ndebele, South`

  // Ndebele, North
  case "nde":
		c.Body = `Ndebele, North`

  // Ndonga
  case "ndo":
		c.Body = `Ndonga`

  // Low German; Low Saxon
  case "nds":
		c.Body = `Low German; Low Saxon`

  // Macrolanguage
  case "nep":
		c.Body = `Nepali`

  // Newari; Nepal Bhasa
  case "new":
		c.Body = `Newari; Nepal Bhasa`

  // Nias
  case "nia":
		c.Body = `Nias`

  // Collective name
  case "nic":
		c.Body = `Niger-Kordofanian languages`

  // Niuean
  case "niu":
		c.Body = `Niuean`

  // Norwegian Nynorsk
  case "nno":
		c.Body = `Norwegian Nynorsk`

  // Norwegian Bokmål
  case "nob":
		c.Body = `Norwegian Bokmål`

  // Nogai
  case "nog":
		c.Body = `Nogai`

  // Old Norse
  case "non":
		c.Body = `Old Norse`

  // Macrolanguage
  case "nor":
		c.Body = `Norwegian`

  // N’Ko
  case "nqo":
		c.Body = `N’Ko`

  // ONIX local code, equivalent to nrf in ISO 639-3. For use in ONIX 3.0 only
  case "nrf":
		c.Body = `Guernésiais, Jèrriais`

  // Pedi; Sepedi; Northern Sotho
  case "nso":
		c.Body = `Pedi; Sepedi; Northern Sotho`

  // Collective name
  case "nub":
		c.Body = `Nubian languages`

  // Classical Newari; Old Newari; Classical Nepal Bhasa
  case "nwc":
		c.Body = `Classical Newari; Old Newari; Classical Nepal Bhasa`

  // Chichewa; Chewa; Nyanja
  case "nya":
		c.Body = `Chichewa; Chewa; Nyanja`

  // Nyamwezi
  case "nym":
		c.Body = `Nyamwezi`

  // Nyankole
  case "nyn":
		c.Body = `Nyankole`

  // Nyoro
  case "nyo":
		c.Body = `Nyoro`

  // Nzima
  case "nzi":
		c.Body = `Nzima`

  // Occitan (post 1500)
  case "oci":
		c.Body = `Occitan (post 1500)`

  // ONIX local code, equivalent to odt in ISO 639-3
  case "odt":
		c.Body = `Old Dutch / Old Low Franconian (ca. 400–1050)`

  // Macrolanguage
  case "oji":
		c.Body = `Ojibwa`

  // ONIX local code, equivalent to omq in ISO 639-5. Collective name
  case "omq":
		c.Body = `Oto-Manguean languages`

  // Macrolanguage
  case "ori":
		c.Body = `Oriya`

  // Macrolanguage
  case "orm":
		c.Body = `Oromo`

  // Osage
  case "osa":
		c.Body = `Osage`

  // Ossetian; Ossetic
  case "oss":
		c.Body = `Ossetian; Ossetic`

  // Turkish, Ottoman
  case "ota":
		c.Body = `Turkish, Ottoman`

  // Collective name
  case "oto":
		c.Body = `Otomian languages`

  // Collective name
  case "paa":
		c.Body = `Papuan languages`

  // Pangasinan
  case "pag":
		c.Body = `Pangasinan`

  // Pahlavi
  case "pal":
		c.Body = `Pahlavi`

  // Pampanga; Kapampangan
  case "pam":
		c.Body = `Pampanga; Kapampangan`

  // Panjabi
  case "pan":
		c.Body = `Panjabi`

  // Papiamento
  case "pap":
		c.Body = `Papiamento`

  // Palauan
  case "pau":
		c.Body = `Palauan`

  // Old Persian (ca. 600-400 B.C.)
  case "peo":
		c.Body = `Old Persian (ca. 600-400 B.C.)`

  // Macrolanguage
  case "per":
		c.Body = `Persian; Farsi`

  // ONIX local code, equivalent to pes in ISO 639-3. For use in ONIX 3.0 only
  case "pes":
		c.Body = `Iranian Persian; Parsi`

  // Collective name
  case "phi":
		c.Body = `Philippine languages`

  // Phoenician
  case "phn":
		c.Body = `Phoenician`

  // Pali
  case "pli":
		c.Body = `Pali`

  // ONIX local code for Italian dialect, equivalent to pms in ISO 639-3. For use in ONIX 3.0 only
  case "pms":
		c.Body = `Piedmontese`

  // Polish
  case "pol":
		c.Body = `Polish`

  // Ponapeian
  case "pon":
		c.Body = `Ponapeian`

  // Portuguese
  case "por":
		c.Body = `Portuguese`

  // Collective name
  case "pra":
		c.Body = `Prakrit languages`

  // Provençal, Old (to 1500); Occitan, Old (to 1500)
  case "pro":
		c.Body = `Provençal, Old (to 1500); Occitan, Old (to 1500)`

  // ONIX local code, equivalent to prs in ISO 639-3. For use in ONIX 3.0 only
  case "prs":
		c.Body = `Dari; Afghan Persian`

  // Macrolanguage
  case "pus":
		c.Body = `Pushto; Pashto`

  // ONIX local code, distinct dialect of Occitan (not distinguished from oci by ISO 639-3)
  case "qar":
		c.Body = `Aranés`

  // ONIX local code, distinct dialect of Catalan (not distinguished from cat by ISO 639-3)
  case "qav":
		c.Body = `Valencian`

  // ONIX local code, distinct variant of langue d’oïl (old northern French) (not distinguished from fro, or from frm, fre, nrf by ISO 639-3). For use in ONIX 3.0 only
  case "qgl":
		c.Body = `Gallo`

  // ONIX local code, distinct dialect of of Rusyn (not distinguished from rue by ISO 639-3). For use in ONIX 3.0 only
  case "qlk":
		c.Body = `Lemko`

  // ONIX local code, distinct and exclusively spoken variation of Spanish, not distinguished from spa (Spanish, Castilian) by ISO 639-3. Neutral Latin American Spanish should be considered a ‘shorthand’ for spa plus a ‘country code’ for Latin America – but prefer spa plus the relevant country code for specifically Mexican Spanish, Argentine (Rioplatense) Spanish, Puerto Rican Spanish etc. Neutral Latin American Spanish must only be used with audio material (including the audio tracks of TV, video and film) to indicate use of accent, vocabulary and construction suitable for broad use across Latin America. For use in ONIX 3.0 only
  case "qls":
		c.Body = `Neutral Latin American Spanish`

  // Macrolanguage
  case "que":
		c.Body = `Quechua`

  // Macrolanguage
  case "raj":
		c.Body = `Rajasthani`

  // Rapanui
  case "rap":
		c.Body = `Rapanui`

  // Rarotongan; Cook Islands Maori
  case "rar":
		c.Body = `Rarotongan; Cook Islands Maori`

  // ONIX local code, equivalent to rcf in ISO 639-3. For use in ONIX 3.0 only
  case "rcf":
		c.Body = `Réunion Creole French`

  // ONIX local code for Italian dialect, equivalent to rgl in ISO 639-3. For use in ONIX 3.0 only
  case "rgn":
		c.Body = `Romagnol`

  // Collective name
  case "roa":
		c.Body = `Romance languages`

  // Romansh
  case "roh":
		c.Body = `Romansh`

  // Macrolanguage
  case "rom":
		c.Body = `Romany`

  // Romanian
  case "rum":
		c.Body = `Romanian`

  // Rundi
  case "run":
		c.Body = `Rundi`

  // Aromanian; Arumanian; Macedo-Romanian
  case "rup":
		c.Body = `Aromanian; Arumanian; Macedo-Romanian`

  // Russian
  case "rus":
		c.Body = `Russian`

  // Sandawe
  case "sad":
		c.Body = `Sandawe`

  // Sango
  case "sag":
		c.Body = `Sango`

  // Yakut
  case "sah":
		c.Body = `Yakut`

  // Collective name
  case "sai":
		c.Body = `South American Indian languages`

  // Collective name
  case "sal":
		c.Body = `Salishan languages`

  // Samaritan Aramaic
  case "sam":
		c.Body = `Samaritan Aramaic`

  // Sanskrit
  case "san":
		c.Body = `Sanskrit`

  // Sasak
  case "sas":
		c.Body = `Sasak`

  // Santali
  case "sat":
		c.Body = `Santali`

  // DEPRECATED – use srp
  case "scc":
		c.Body = `Serbian`

  // Sicilian
  case "scn":
		c.Body = `Sicilian`

  // Scots
  case "sco":
		c.Body = `Scots`

  // DEPRECATED – use hrv
  case "scr":
		c.Body = `Croatian`

  // ONIX local code for Sardinian dialect, equivalent to sdc in ISO 639-3. For use in ONIX 3.0 only
  case "sdc":
		c.Body = `Sassarese`

  // ONIX local code for Sardinian dialect, equivalent to sdn in ISO 639-3. For use in ONIX 3.0 only
  case "sdn":
		c.Body = `Gallurese`

  // Selkup
  case "sel":
		c.Body = `Selkup`

  // Collective name
  case "sem":
		c.Body = `Semitic languages`

  // Irish, Old (to 1100)
  case "sga":
		c.Body = `Irish, Old (to 1100)`

  // Collective name
  case "sgn":
		c.Body = `Sign languages`

  // Shan
  case "shn":
		c.Body = `Shan`

  // Sidamo
  case "sid":
		c.Body = `Sidamo`

  // Sinhala; Sinhalese
  case "sin":
		c.Body = `Sinhala; Sinhalese`

  // Collective name
  case "sio":
		c.Body = `Siouan languages`

  // Collective name
  case "sit":
		c.Body = `Sino-Tibetan languages`

  // Collective name
  case "sla":
		c.Body = `Slavic languages`

  // Slovak
  case "slo":
		c.Body = `Slovak`

  // Slovenian
  case "slv":
		c.Body = `Slovenian`

  // Southern Sami
  case "sma":
		c.Body = `Southern Sami`

  // Northern Sami
  case "sme":
		c.Body = `Northern Sami`

  // Collective name
  case "smi":
		c.Body = `Sami languages`

  // Lule Sami
  case "smj":
		c.Body = `Lule Sami`

  // Inari Sami
  case "smn":
		c.Body = `Inari Sami`

  // Samoan
  case "smo":
		c.Body = `Samoan`

  // Skolt Sami
  case "sms":
		c.Body = `Skolt Sami`

  // Shona
  case "sna":
		c.Body = `Shona`

  // Sindhi
  case "snd":
		c.Body = `Sindhi`

  // Soninke
  case "snk":
		c.Body = `Soninke`

  // Sogdian
  case "sog":
		c.Body = `Sogdian`

  // Somali
  case "som":
		c.Body = `Somali`

  // Collective name
  case "son":
		c.Body = `Songhai languages`

  // Sotho; Sesotho
  case "sot":
		c.Body = `Sotho; Sesotho`

  // Spanish
  case "spa":
		c.Body = `Spanish`

  // Macrolanguage
  case "srd":
		c.Body = `Sardinian`

  // Sranan Tongo
  case "srn":
		c.Body = `Sranan Tongo`

  // ONIX local code for Sardinian dialect, equivalent to sro in ISO 639-3. For use in ONIX 3.0 only
  case "sro":
		c.Body = `Campidanese`

  // Serbian
  case "srp":
		c.Body = `Serbian`

  // Serer
  case "srr":
		c.Body = `Serer`

  // Collective name
  case "ssa":
		c.Body = `Nilo-Saharan languages`

  // Swazi; Swati
  case "ssw":
		c.Body = `Swazi; Swati`

  // Sukuma
  case "suk":
		c.Body = `Sukuma`

  // Sundanese
  case "sun":
		c.Body = `Sundanese`

  // Susu
  case "sus":
		c.Body = `Susu`

  // Sumerian
  case "sux":
		c.Body = `Sumerian`

  // Macrolanguage
  case "swa":
		c.Body = `Swahili`

  // Swedish
  case "swe":
		c.Body = `Swedish`

  // Classical Syriac
  case "syc":
		c.Body = `Classical Syriac`

  // Macrolanguage
  case "syr":
		c.Body = `Syriac`

  // Tahitian
  case "tah":
		c.Body = `Tahitian`

  // Collective name
  case "tai":
		c.Body = `Tai languages`

  // Tamil
  case "tam":
		c.Body = `Tamil`

  // Tatar
  case "tat":
		c.Body = `Tatar`

  // Telugu
  case "tel":
		c.Body = `Telugu`

  // Temne; Time
  case "tem":
		c.Body = `Temne; Time`

  // Terena
  case "ter":
		c.Body = `Terena`

  // Tetum
  case "tet":
		c.Body = `Tetum`

  // Tajik; Tajiki Persian
  case "tgk":
		c.Body = `Tajik; Tajiki Persian`

  // Tagalog
  case "tgl":
		c.Body = `Tagalog`

  // Thai
  case "tha":
		c.Body = `Thai`

  // Tibetan
  case "tib":
		c.Body = `Tibetan`

  // Tigré
  case "tig":
		c.Body = `Tigré`

  // Tigrinya
  case "tir":
		c.Body = `Tigrinya`

  // Tiv
  case "tiv":
		c.Body = `Tiv`

  // Tokelauan
  case "tkl":
		c.Body = `Tokelauan`

  // Artificial language
  case "tlh":
		c.Body = `Klingon; tlhIngan-Hol`

  // Tlingit
  case "tli":
		c.Body = `Tlingit`

  // Macrolanguage
  case "tmh":
		c.Body = `Tamashek`

  // Tonga (Nyasa)
  case "tog":
		c.Body = `Tonga (Nyasa)`

  // Tongan
  case "ton":
		c.Body = `Tongan`

  // Tok Pisin
  case "tpi":
		c.Body = `Tok Pisin`

  // Tsimshian
  case "tsi":
		c.Body = `Tsimshian`

  // AKA Setswana
  case "tsn":
		c.Body = `Tswana`

  // Tsonga
  case "tso":
		c.Body = `Tsonga`

  // Turkmen
  case "tuk":
		c.Body = `Turkmen`

  // Tumbuka
  case "tum":
		c.Body = `Tumbuka`

  // Collective name
  case "tup":
		c.Body = `Tupi languages`

  // Turkish
  case "tur":
		c.Body = `Turkish`

  // Altaic languages
  case "tut":
		c.Body = `Altaic languages`

  // Tuvaluan
  case "tvl":
		c.Body = `Tuvaluan`

  // Twi
  case "twi":
		c.Body = `Twi`

  // Tuvinian
  case "tyv":
		c.Body = `Tuvinian`

  // ONIX local code, equivalent to tzo in ISO 639-3
  case "tzo":
		c.Body = `Tzotzil`

  // Udmurt
  case "udm":
		c.Body = `Udmurt`

  // Ugaritic
  case "uga":
		c.Body = `Ugaritic`

  // Uighur; Uyghur
  case "uig":
		c.Body = `Uighur; Uyghur`

  // Ukrainian
  case "ukr":
		c.Body = `Ukrainian`

  // Umbundu
  case "umb":
		c.Body = `Umbundu`

  // Undetermined language
  case "und":
		c.Body = `Undetermined language`

  // Urdu
  case "urd":
		c.Body = `Urdu`

  // Macrolanguage
  case "uzb":
		c.Body = `Uzbek`

  // Vai
  case "vai":
		c.Body = `Vai`

  // ONIX local code for Italian dialect, equivalent to vec in ISO 639-3. For use in ONIX 3.0 only
  case "vec":
		c.Body = `Venetian/Venetan`

  // Venda
  case "ven":
		c.Body = `Venda`

  // Vietnamese
  case "vie":
		c.Body = `Vietnamese`

  // Artificial language
  case "vol":
		c.Body = `Volapük`

  // Votic
  case "vot":
		c.Body = `Votic`

  // Collective name
  case "wak":
		c.Body = `Wakashan languages`

  // Wolaitta; Wolaytta
  case "wal":
		c.Body = `Wolaitta; Wolaytta`

  // Waray
  case "war":
		c.Body = `Waray`

  // Washo
  case "was":
		c.Body = `Washo`

  // Welsh
  case "wel":
		c.Body = `Welsh`

  // Collective name
  case "wen":
		c.Body = `Sorbian languages`

  // Walloon
  case "wln":
		c.Body = `Walloon`

  // Wolof
  case "wol":
		c.Body = `Wolof`

  // Kalmyk
  case "xal":
		c.Body = `Kalmyk`

  // Xhosa
  case "xho":
		c.Body = `Xhosa`

  // ONIX local code, equivalent to xuu in ISO 639-3. For use in ONIX 3.0 only
  case "xuu":
		c.Body = `Khwedam, Kxoe`

  // Yao
  case "yao":
		c.Body = `Yao`

  // Yapese
  case "yap":
		c.Body = `Yapese`

  // Macrolanguage
  case "yid":
		c.Body = `Yiddish`

  // Yoruba
  case "yor":
		c.Body = `Yoruba`

  // Collective name
  case "ypk":
		c.Body = `Yupik languages`

  // ONIX local code, equivalent to yue in ISO 639-3
  case "yue":
		c.Body = `Cantonese`

  // Macrolanguage
  case "zap":
		c.Body = `Zapotec`

  // Artificial language
  case "zbl":
		c.Body = `Blissymbols; Blissymbolics; Bliss`

  // Zenaga
  case "zen":
		c.Body = `Zenaga`

  // Standard Moroccan Tamazight
  case "zgh":
		c.Body = `Standard Moroccan Tamazight`

  // Macrolanguage
  case "zha":
		c.Body = `Zhuang; Chuang`

  // Collective name
  case "znd":
		c.Body = `Zande languages`

  // Zulu
  case "zul":
		c.Body = `Zulu`

  // Zuni
  case "zun":
		c.Body = `Zuni`

  // No linguistic content
  case "zxx":
		c.Body = `No linguistic content`

  // Macrolanguage
  case "zza":
		c.Body = `Zaza; Dimili; Dimli; Kirdki; Kirmanjki; Zazaki`
	default:
		return fmt.Errorf("undefined code for ToLanguage has been passed, got [%s]", v)
	}
	return nil
}

// TradeCategory Trade category
type TradeCategory struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *TradeCategory) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // An edition from a UK publisher sold only in territories where exclusive rights are not held. Rights details should be carried in PR.21 (ONIX 2.1) OR P.21 (ONIX 3.0) as usual
  case "01":
		c.Body = `UK open market edition`

  // In UK, an edition intended primarily for airside sales in UK airports, though it may be available for sale in other territories where exclusive rights are not held. Rights details should be carried in PR.21 (ONIX 2.1) OR P.21 (ONIX 3.0) as usual
  case "02":
		c.Body = `Airport edition`

  // In Germany, a special printing sold at a lower price than the regular hardback
  case "03":
		c.Body = `Sonderausgabe`

  // In countries where recognised as a distinct trade category, eg France « livre de poche », Germany ,Taschenbuch‘, Italy «tascabile», Spain «libro de bolsillo»
  case "04":
		c.Body = `Pocket book`

  // Edition produced solely for sale in designated export markets
  case "05":
		c.Body = `International edition (US)`

  // Audio product sold in special durable packaging and with a replacement guarantee for the contained cassettes or CDs for a specified shelf-life
  case "06":
		c.Body = `Library audio edition`

  // An edition from a US publisher sold only in territories where exclusive rights are not held. Rights details should be carried in PR.21 (ONIX 2.1) OR P.21 (ONIX 3.0) as usual
  case "07":
		c.Body = `US open market edition`

  // In France, a category of book that has a particular legal status, claimed by the publisher
  case "08":
		c.Body = `Livre scolaire, déclaré par l’éditeur`

  // In France, a category of book that has a particular legal status, designated independently of the publisher
  case "09":
		c.Body = `Livre scolaire (non spécifié)`

  // Edition published for sale only with a newspaper or periodical
  case "10":
		c.Body = `Supplement to newspaper`

  // In Spain, a school textbook for which there is no fixed or suggested retail price and which is supplied by the publisher on terms individually agreed with the bookseller
  case "11":
		c.Body = `Precio libre textbook`

  // For editions sold only through newsstands/newsagents
  case "12":
		c.Body = `News outlet edition`

  // In the US and Canada, a book that is published primarily for use by students in school or college education as a basis for study. Textbooks published for the elementary and secondary school markets are generally purchased by school districts for the use of students. Textbooks published for the higher education market are generally adopted for use in particular classes by the instructors of those classes. Textbooks are usually not marketed to the general public, which distinguishes them from trade books. Note that trade books adopted for course use are not considered to be textbooks (though a specific education edition of a trade title may be)
  case "13":
		c.Body = `US textbook`

  // ‘Short’ e-book (sometimes also called a ‘single’), typically containing a single short story, an essay or piece of long-form journalism
  case "14":
		c.Body = `E-book short`

  // In countries where recognised as a distinct trade category, eg Italy «supertascabile». For use in ONIX 3.0 only
  case "15":
		c.Body = `Superpocket book`

  // Category of books, usually hardcover and of a large format (A4 or larger) and printed on high-quality paper, where the primary features are illustrations, and these are more important than text. Sometimes called ‘coffee-table books’ or ‘art books’ in English. For use in ONIX 3.0 only
  case "16":
		c.Body = `Beau-livre`

  // Category of audio products typically distinguished by being free of charge (but which may be monetised through advertising content) and episodic. For use in ONIX 3.0 only
  case "17":
		c.Body = `Podcast`

  // Category of books or e-books which are single issues of a periodical publication, sold as independent products. For use in ONIX 3.0 only
  case "18":
		c.Body = `Periodical`
	default:
		return fmt.Errorf("undefined code for TradeCategory has been passed, got [%s]", v)
	}
	return nil
}

// UnnamedPersons Unnamed person(s)
type UnnamedPersons struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *UnnamedPersons) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // Unknown
  case "01":
		c.Body = `Unknown`

  // Note that Anonymous can be interpreted as singular or plural
  case "02":
		c.Body = `Anonymous`

  // And others: additional contributors not listed
  case "03":
		c.Body = `et al`

  // Use for example when the product is a pack of books by different authors
  case "04":
		c.Body = `Various`

  // Use with Contributor role code E07 ‘read by’, eg for audio books
  case "05":
		c.Body = `Synthesised voice – male`

  // Use with Contributor role code E07 ‘read by’, eg for audio books
  case "06":
		c.Body = `Synthesised voice – female`

  // Use with Contributor role code E07 ‘read by’, eg for audio books
  case "07":
		c.Body = `Synthesised voice – unspecified`

  // Use with Contributor role code E07 ‘read by’, eg for audio books, and provide name of voice actor in <AlternativeName>. For use in ONIX 3.0 only
  case "08":
		c.Body = `Synthesised voice – based on real voice actor`
	default:
		return fmt.Errorf("undefined code for UnnamedPersons has been passed, got [%s]", v)
	}
	return nil
}

// UnpricedItemType Unpriced item type
type UnpricedItemType struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *UnpricedItemType) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // Free of charge
  case "01":
		c.Body = `Free of charge`

  // Price to be announced
  case "02":
		c.Body = `Price to be announced`

  // Not sold separately at retail
  case "03":
		c.Body = `Not sold separately`

  // May be used for books that do not carry a recommended retail price; when goods can only be ordered ‘in person’ from a sales representative; when an ONIX file is ‘broadcast’ rather than sent one-to-one to a single trading partner; or for digital products offered on subscription or with pricing which is too complex to specify in ONIX
  case "04":
		c.Body = `Contact supplier`

  // When a collection that is not sold as a set nevertheless has its own ONIX record
  case "05":
		c.Body = `Not sold as set`

  // Unpriced, but available via a pre-determined revenue share agreement
  case "06":
		c.Body = `Revenue share`

  // Price calculated as sum of individual prices of components listed as Product parts. For use in ONIX 3.0 only
  case "07":
		c.Body = `Calculated from contents`
	default:
		return fmt.Errorf("undefined code for UnpricedItemType has been passed, got [%s]", v)
	}
	return nil
}

// VelocityMetric Velocity metric
type VelocityMetric struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *VelocityMetric) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // Typically measured over most recent 1 month period
  case "01":
		c.Body = `Mean daily sale`

  // Typically measured over most recent 1 month period
  case "02":
		c.Body = `Maximum daily sale`

  // Typically measured over most recent 1 month period
  case "03":
		c.Body = `Minimum daily sale`

  // Typically measured over most recent rolling 12 week period
  case "04":
		c.Body = `Mean weekly sale`

  // Typically measured over most recent rolling 12 week period
  case "05":
		c.Body = `Maximum weekly sale`

  // Typically measured over most recent rolling 12 week period
  case "06":
		c.Body = `Minimum weekly sale`

  // Typically measured over most recent rolling 6 month period
  case "07":
		c.Body = `Mean monthly sale`

  // Typically measured over the most recent rolling 6 month period
  case "08":
		c.Body = `Maximum monthly sale`

  // Typically measured over the most recent rolling 6 month period
  case "09":
		c.Body = `Minimum monthly sale`
	default:
		return fmt.Errorf("undefined code for VelocityMetric has been passed, got [%s]", v)
	}
	return nil
}

// WebsiteRole Website role
type WebsiteRole struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *WebsiteRole) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // Unspecified, see website description
  case "00":
		c.Body = `Unspecified, see website description`

  // See also codes 17 and 18
  case "01":
		c.Body = `Publisher’s corporate website`

  // A publisher’s informative and/or promotional webpage relating to a specified work (book, journal, online resource or other publication type)
  case "02":
		c.Body = `Publisher’s website for a specified work`

  // A webpage giving access to an online content hosting service as a whole
  case "03":
		c.Body = `Online hosting service home page`

  // A webpage giving general information about a serial, in print or electronic format or both
  case "04":
		c.Body = `Journal home page`

  // A webpage giving direct access to the content that is available online for a specified resource version. Generally used for content available online under subscription terms
  case "05":
		c.Body = `Online resource ‘available content’ page`

  // A webpage maintained by an author or other contributor about her/his publications and personal background
  case "06":
		c.Body = `Contributor’s own website`

  // A publisher’s webpage devoted to a specific author or other contributor
  case "07":
		c.Body = `Publisher’s website relating to specified contributor`

  // A webpage devoted to a specific author or other contributor, and maintained by a publisher other than the publisher of the item described in the ONIX record
  case "08":
		c.Body = `Other publisher’s website relating to specified contributor`

  // A webpage devoted to a specific author or other contributor, and maintained by a third party (eg a fan site)
  case "09":
		c.Body = `Third-party website relating to specified contributor`

  // A webpage maintained by an author or other contributor and specific to an individual work
  case "10":
		c.Body = `Contributor’s own website for specified work`

  // A webpage devoted to an individual work, and maintained by a publisher other than the publisher of the item described in the ONIX record
  case "11":
		c.Body = `Other publisher’s website relating to specified work`

  // A webpage devoted to an individual work, and maintained by a third party (eg a fan site)
  case "12":
		c.Body = `Third-party website relating to specified work`

  // A webpage maintained by an author or other contributor and specific to a group or series of works
  case "13":
		c.Body = `Contributor’s own website for group or series of works`

  // A publisher’s webpage devoted to a group or series of works
  case "14":
		c.Body = `Publisher’s website relating to group or series of works`

  // A webpage devoted to a group or series of works, and maintained by a publisher other than the publisher of the item described in the ONIX record
  case "15":
		c.Body = `Other publisher’s website relating to group or series of works`

  // A webpage devoted to a group or series of works, and maintained by a third party (eg a fan site)
  case "16":
		c.Body = `Third-party website relating to group or series of works (eg a fan site)`

  // Use instead of code 01 to specify a publisher’s website for trade users
  case "17":
		c.Body = `Publisher’s B2B website`

  // Use instead of code 01 to specify a publisher’s website for end customers (consumers)
  case "18":
		c.Body = `Publisher’s B2C website`

  // For example, a Blogger or Tumblr URL, a Wordpress website or other blog URL
  case "23":
		c.Body = `Author blog`

  // Web page for author presentation / commentary
  case "24":
		c.Body = `Web page for author presentation / commentary`

  // Web page for author interview
  case "25":
		c.Body = `Web page for author interview`

  // Web page for author reading
  case "26":
		c.Body = `Web page for author reading`

  // Web page for cover material
  case "27":
		c.Body = `Web page for cover material`

  // Web page for sample content
  case "28":
		c.Body = `Web page for sample content`

  // Use this value in the <Website> composite (typically within <Publisher> or <SupplyDetail>) when sending a link to a webpage at which a digital product is available for download and/or online access
  case "29":
		c.Body = `Web page for full content`

  // Web page for other commentary / discussion
  case "30":
		c.Body = `Web page for other commentary / discussion`

  // URL needed by the German National Library for direct access, harvesting and storage of an electronic resource
  case "31":
		c.Body = `Transfer-URL`

  // Link needed by German Books in Print (VLB) for DOI registration and ONIX DOI conversion
  case "32":
		c.Body = `DOI Website Link`

  // A corporate website operated by a distributor or other supplier (not the publisher)
  case "33":
		c.Body = `Supplier’s corporate website`

  // A website operated by a distributor or other supplier (not the publisher) and aimed at trade customers
  case "34":
		c.Body = `Supplier’s B2B website`

  // A website operated by a distributor or other supplier (not the publisher) and aimed at consumers
  case "35":
		c.Body = `Supplier’s B2C website`

  // A distributor or supplier’s webpage describing a specified work
  case "36":
		c.Body = `Supplier’s website for a specified work`

  // A distributor or supplier’s webpage describing a specified work, and aimed at trade customers
  case "37":
		c.Body = `Supplier’s B2B website for a specified work`

  // A distributor or supplier’s webpage describing a specified work, and aimed at consumers
  case "38":
		c.Body = `Supplier’s B2C website for a specified work`

  // A distributor or supplier’s webpage describing a group or series of works
  case "39":
		c.Body = `Supplier’s website for a group or series of works`

  // For example an ONIX or MARC record for the product, available online
  case "40":
		c.Body = `URL of full metadata description`

  // For example, a Facebook, Google+ or Twitter URL for the product or work
  case "41":
		c.Body = `Social networking URL for specific work or product`

  // For example, a Facebook, Google+ or Twitter page
  case "42":
		c.Body = `Author’s social networking URL`

  // For example, a Facebook, Google+ or Twitter page
  case "43":
		c.Body = `Publisher’s social networking URL`

  // For example, a Facebook, Google+ or Twitter page. Use only in the context of a specific content item (eg within <ContentItem>)
  case "44":
		c.Body = `Social networking URL for specific article, chapter or content item`

  // For example, a service offering click-through licensing of extracts
  case "45":
		c.Body = `Publisher’s or third party website for permissions requests`

  // For example, a page providing details related to GDPR. For use in ONIX 3.0 only
  case "46":
		c.Body = `Publisher’s or third party website for privacy statement`

  // The URL of the publisher’s preservation service, or a more specific URL for access to its preserved copy. For use in ONIX 3.0 only
  case "47":
		c.Body = `Publisher’s website for digital preservation`

  // The URL of the preservation service (eg https://clockss.org), or a more specific URL for access to its preserved copy. For use in ONIX 3.0 only
  case "48":
		c.Body = `Third-party website for digital preservation`
	default:
		return fmt.Errorf("undefined code for WebsiteRole has been passed, got [%s]", v)
	}
	return nil
}

// WorkIDType Work identifier type
type WorkIDType struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *WorkIDType) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // Note that <IDTypeName> is required with proprietary identifiers
  case "01":
		c.Body = `Proprietary`

  // 10-character ISBN of manifestation of work, when this is the only work identifier available – now DEPRECATED in ONIX for Books, except where providing historical information for compatibility with legacy systems. It should only be used in relation to products published before 2007 – when ISBN-13 superseded it – and should never be used as the ONLY identifier (it should always be accompanied by the correct GTIN-13 / ISBN-13 of the manifestation of the work)
  case "02":
		c.Body = `ISBN-10`

  // Digital Object Identifier (variable length and character set)
  case "06":
		c.Body = `DOI`

  // International Standard Text Code (16 characters: numerals and letters A-F, unhyphenated)
  case "11":
		c.Body = `ISTC`

  // 13-character ISBN of a manifestation of work, when this is the only work identifier available (13 digits, without spaces or hyphens)
  case "15":
		c.Body = `ISBN-13`

  // International Standard Recording Code
  case "18":
		c.Body = `ISRC`

  // Global Library Manifestation Identifier, OCLC’s ‘manifestation cluster’ ID
  case "32":
		c.Body = `GLIMIR`

  // OCLC Work Identifier
  case "33":
		c.Body = `OWI`

  // International Standard Content Code, a ‘similarity hash’ identifier derived algorithmically from the content itself (see https://iscc.codes). <IDValue> is the 27-character case-sensitive string (including one hyphen) comprising the Meta-ID and Content-ID components of a full ISCC generated from a digital manifestation of the work. Use only with ONIX 3.0
  case "39":
		c.Body = `ISCC`
	default:
		return fmt.Errorf("undefined code for WorkIDType has been passed, got [%s]", v)
	}
	return nil
}

// WorkRelationCode Work relation
type WorkRelationCode struct {
	Body string `xml:",innerxml" json:",omitempty"`
	Datestamp DtDotDateOrDateTime `xml:"datestamp,attr,omitempty" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,attr,omitempty" json:",omitempty"`
	Sourcename DtDotNonEmptyString `xml:"sourcename,attr,omitempty" json:",omitempty"`
}

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *WorkRelationCode) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	for _, attr := range start.Attr {
		if attr.Name.Local == "datestamp" {
			c.Datestamp = DtDotDateOrDateTime(attr.Value)
		}
		if attr.Name.Local == "sourcetype" {
			c.Sourcetype = SourceTypeCode(attr.Value)
		}
		if attr.Name.Local == "sourcename" {
			c.Sourcename = DtDotNonEmptyString(attr.Value)
		}
	}
	switch v {

  // Product X is or includes a manifestation of work Y. (There is a direct parent-child relation between work Y and the product)
  case "01":
		c.Body = `Manifestation of`

  // Product X is or includes a manifestation of a work derived (directly) from related work Y in one or more of the ways specified in ISTC rules. (There is a relationship between a grandparent work Y and a parent work, and between that parent work and the product.) This relation type is intended to enable products with a common ‘grandparent’ work to be linked without specifying the precise nature of their derivation, and without necessarily assigning an identifier to the product’s parent
  case "02":
		c.Body = `Derived from`

  // Product X is a manifestation of a work from which related work Y is (directly) derived in one or more of the ways specified in ISTC rules. (There is a relationship between a parent work and a child work Y, and between the parent work and the product)
  case "03":
		c.Body = `Related work is derived from this`

  // Product X is a manifestation of a work in the same collection as related work Y
  case "04":
		c.Body = `Other work in same collection`

  // Product X is a manifestation of a work by the same contributor(s) as related work Y
  case "05":
		c.Body = `Other work by same contributor`
	default:
		return fmt.Errorf("undefined code for WorkRelationCode has been passed, got [%s]", v)
	}
	return nil
}

// Align has not document
type Align string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *Align) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

  // 
  case "left":
		*c = ``

  // 
  case "center":
		*c = ``

  // 
  case "right":
		*c = ``

  // 
  case "justify":
		*c = ``

  // 
  case "char":
		*c = ``
	default:
		return fmt.Errorf("undefined code for Align has been passed, got [%s]", v)
	}
	return nil
}

// Axis has not document
type Axis string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *Axis) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {
	default:
		return fmt.Errorf("undefined code for Axis has been passed, got [%s]", v)
	}
}

// Class has not document
type Class string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *Class) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {
	default:
		return fmt.Errorf("undefined code for Class has been passed, got [%s]", v)
	}
}

// DateformatList55 has not document
type DateformatList55 string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *DateformatList55) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

  // Common Era year, month and day (default for most dates)
  case "00":
		*c = `YYYYMMDD`

  // Year and month
  case "01":
		*c = `YYYYMM`

  // Year and week number
  case "02":
		*c = `YYYYWW`

  // Year and quarter (Q = 1, 2, 3, 4, with 1 = Jan to Mar)
  case "03":
		*c = `YYYYQ`

  // Year and season (S = 1, 2, 3, 4, with 1 = ‘Spring’)
  case "04":
		*c = `YYYYS`

  // Year (default for some dates)
  case "05":
		*c = `YYYY`

  // Spread of exact dates
  case "06":
		*c = `YYYYMMDDYYYYMMDD`

  // Spread of months
  case "07":
		*c = `YYYYMMYYYYMM`

  // Spread of week numbers
  case "08":
		*c = `YYYYWWYYYYWW`

  // Spread of quarters
  case "09":
		*c = `YYYYQYYYYQ`

  // Spread of seasons
  case "10":
		*c = `YYYYSYYYYS`

  // Spread of years
  case "11":
		*c = `YYYYYYYY`

  // For complex, approximate or uncertain dates, or dates BCE
  case "12":
		*c = `Text string`

  // Exact time. Use ONLY when exact times with hour/minute precision are relevant. By default, time is local. Alternatively, the time may be suffixed with an optional ‘Z’ for UTC times, or with ‘+’ or ‘-’ and an hhmm timezone offset from UTC. Times without a timezone are ‘rolling’ local times, times qualified with a timezone (using Z, + or -) specify a particular instant in time
  case "13":
		*c = `YYYYMMDDThhmm`

  // Exact time. Use ONLY when exact times with second precision are relevant. By default, time is local. Alternatively, the time may be suffixed with an optional ‘Z’ for UTC times, or with ‘+’ or ‘-’ and an hhmm timezone offset from UTC. Times without a timezone are ‘rolling’ local times, times qualified with a timezone (using Z, + or -) specify a particular instant in time
  case "14":
		*c = `YYYYMMDDThhmmss`

  // Year month day (Hijri calendar)
  case "20":
		*c = `YYYYMMDD (H)`

  // Year and month (Hijri calendar)
  case "21":
		*c = `YYYYMM (H)`

  // Year (Hijri calendar)
  case "25":
		*c = `YYYY (H)`

  // For complex, approximate or uncertain dates (Hijri calendar), text would usually be in Arabic script
  case "32":
		*c = `Text string (H)`
	default:
		return fmt.Errorf("undefined code for DateformatList55 has been passed, got [%s]", v)
	}
	return nil
}

// Dir has not document
type Dir string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *Dir) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

  // 
  case "ltr":
		*c = ``

  // 
  case "rtl":
		*c = ``
	default:
		return fmt.Errorf("undefined code for Dir has been passed, got [%s]", v)
	}
	return nil
}

// DtDotDateOrDateTime has not document
type DtDotDateOrDateTime string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *DtDotDateOrDateTime) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {
	default:
		return fmt.Errorf("undefined code for DtDotDateOrDateTime has been passed, got [%s]", v)
	}
}

// ID has not document
type ID string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *ID) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {
	default:
		return fmt.Errorf("undefined code for ID has been passed, got [%s]", v)
	}
}

// IDREFS has not document
type IDREFS string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *IDREFS) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {
	default:
		return fmt.Errorf("undefined code for IDREFS has been passed, got [%s]", v)
	}
}

// Ismap has not document
type Ismap string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *Ismap) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

  // 
  case "ismap":
		*c = ``
	default:
		return fmt.Errorf("undefined code for Ismap has been passed, got [%s]", v)
	}
	return nil
}

// LanguageList74 has not document
type LanguageList74 string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *LanguageList74) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

  // Afar
  case "aar":
		*c = `Afar`

  // Abkhaz
  case "abk":
		*c = `Abkhaz`

  // Achinese
  case "ace":
		*c = `Achinese`

  // Acoli
  case "ach":
		*c = `Acoli`

  // Adangme
  case "ada":
		*c = `Adangme`

  // Adygei
  case "ady":
		*c = `Adygei`

  // Collective name
  case "afa":
		*c = `Afro-Asiatic languages`

  // Artificial language
  case "afh":
		*c = `Afrihili`

  // Afrikaans
  case "afr":
		*c = `Afrikaans`

  // Ainu
  case "ain":
		*c = `Ainu`

  // Macrolanguage
  case "aka":
		*c = `Akan`

  // Akkadian
  case "akk":
		*c = `Akkadian`

  // Macrolanguage
  case "alb":
		*c = `Albanian`

  // Aleut
  case "ale":
		*c = `Aleut`

  // Collective name
  case "alg":
		*c = `Algonquian languages`

  // Southern Altai
  case "alt":
		*c = `Southern Altai`

  // Amharic
  case "amh":
		*c = `Amharic`

  // English, Old (ca. 450-1100)
  case "ang":
		*c = `English, Old (ca. 450-1100)`

  // Angika
  case "anp":
		*c = `Angika`

  // Collective name
  case "apa":
		*c = `Apache languages`

  // Macrolanguage
  case "ara":
		*c = `Arabic`

  // Official Aramaic; Imperial Aramaic (700-300 BCE)
  case "arc":
		*c = `Official Aramaic; Imperial Aramaic (700-300 BCE)`

  // Aragonese
  case "arg":
		*c = `Aragonese`

  // Armenian
  case "arm":
		*c = `Armenian`

  // Mapudungun; Mapuche
  case "arn":
		*c = `Mapudungun; Mapuche`

  // Arapaho
  case "arp":
		*c = `Arapaho`

  // Collective name
  case "art":
		*c = `Artificial languages`

  // Arawak
  case "arw":
		*c = `Arawak`

  // Assamese
  case "asm":
		*c = `Assamese`

  // Asturian; Bable; Leonese; Asturleonese
  case "ast":
		*c = `Asturian; Bable; Leonese; Asturleonese`

  // Collective name
  case "ath":
		*c = `Athapascan languages`

  // Collective name
  case "aus":
		*c = `Australian languages`

  // Avaric
  case "ava":
		*c = `Avaric`

  // Avestan
  case "ave":
		*c = `Avestan`

  // Awadhi
  case "awa":
		*c = `Awadhi`

  // Macrolanguage
  case "aym":
		*c = `Aymara`

  // Macrolanguage
  case "aze":
		*c = `Azerbaijani`

  // Collective name
  case "bad":
		*c = `Banda languages`

  // Collective name
  case "bai":
		*c = `Bamileke languages`

  // Bashkir
  case "bak":
		*c = `Bashkir`

  // Macrolanguage
  case "bal":
		*c = `Baluchi`

  // Bambara
  case "bam":
		*c = `Bambara`

  // Balinese
  case "ban":
		*c = `Balinese`

  // Basque
  case "baq":
		*c = `Basque`

  // Basa
  case "bas":
		*c = `Basa`

  // Collective name
  case "bat":
		*c = `Baltic languages`

  // Beja; Bedawiyet
  case "bej":
		*c = `Beja; Bedawiyet`

  // Belarusian
  case "bel":
		*c = `Belarusian`

  // Bemba
  case "bem":
		*c = `Bemba`

  // Bengali
  case "ben":
		*c = `Bengali`

  // Collective name
  case "ber":
		*c = `Berber languages`

  // Bhojpuri
  case "bho":
		*c = `Bhojpuri`

  // Collective name
  case "bih":
		*c = `Bihari languages`

  // Macrolanguage
  case "bik":
		*c = `Bikol`

  // Bini; Edo
  case "bin":
		*c = `Bini; Edo`

  // Bislama
  case "bis":
		*c = `Bislama`

  // Siksika
  case "bla":
		*c = `Siksika`

  // Collective name
  case "bnt":
		*c = `Bantu languages`

  // Bosnian
  case "bos":
		*c = `Bosnian`

  // Braj
  case "bra":
		*c = `Braj`

  // Breton
  case "bre":
		*c = `Breton`

  // Collective name
  case "btk":
		*c = `Batak languages`

  // Macrolanguage
  case "bua":
		*c = `Buriat`

  // Buginese
  case "bug":
		*c = `Buginese`

  // Bulgarian
  case "bul":
		*c = `Bulgarian`

  // Burmese
  case "bur":
		*c = `Burmese`

  // Blin; Bilin
  case "byn":
		*c = `Blin; Bilin`

  // Caddo
  case "cad":
		*c = `Caddo`

  // Collective name
  case "cai":
		*c = `Central American Indian languages`

  // Galibi Carib
  case "car":
		*c = `Galibi Carib`

  // Catalan
  case "cat":
		*c = `Catalan`

  // Collective name
  case "cau":
		*c = `Caucasian languages`

  // Cebuano
  case "ceb":
		*c = `Cebuano`

  // Collective name
  case "cel":
		*c = `Celtic languages`

  // Chamorro
  case "cha":
		*c = `Chamorro`

  // Chibcha
  case "chb":
		*c = `Chibcha`

  // Chechen
  case "che":
		*c = `Chechen`

  // Chagatai
  case "chg":
		*c = `Chagatai`

  // Macrolanguage
  case "chi":
		*c = `Chinese`

  // Chuukese (Truk)
  case "chk":
		*c = `Chuukese (Truk)`

  // Macrolanguage
  case "chm":
		*c = `Mari`

  // Chinook jargon
  case "chn":
		*c = `Chinook jargon`

  // Choctaw
  case "cho":
		*c = `Choctaw`

  // Chipewyan; Dene Suline
  case "chp":
		*c = `Chipewyan; Dene Suline`

  // Cherokee
  case "chr":
		*c = `Cherokee`

  // Church Slavic; Old Slavonic; Church Slavonic; Old Bulgarian; Old Church Slavonic
  case "chu":
		*c = `Church Slavic; Old Slavonic; Church Slavonic; Old Bulgarian; Old Church Slavonic`

  // Chuvash
  case "chv":
		*c = `Chuvash`

  // Cheyenne
  case "chy":
		*c = `Cheyenne`

  // ONIX local code, equivalent to ckb in ISO 639-3. For use in ONIX 3.0 only
  case "ckb":
		*c = `Central Kurdish (Sorani)`

  // Collective name
  case "cmc":
		*c = `Chamic languages`

  // ONIX local code, equivalent to cmn in ISO 639-3
  case "cmn":
		*c = `Mandarin`

  // For use in ONIX 3.0 only
  case "cnr":
		*c = `Montenegrin`

  // Coptic
  case "cop":
		*c = `Coptic`

  // Cornish
  case "cor":
		*c = `Cornish`

  // Corsican
  case "cos":
		*c = `Corsican`

  // Collective name
  case "cpe":
		*c = `Creoles and pidgins, English-based`

  // Collective name
  case "cpf":
		*c = `Creoles and pidgins, French-based`

  // Collective name
  case "cpp":
		*c = `Creoles and pidgins, Portuguese-based`

  // Macrolanguage
  case "cre":
		*c = `Cree`

  // Crimean Turkish; Crimean Tatar
  case "crh":
		*c = `Crimean Turkish; Crimean Tatar`

  // Collective name
  case "crp":
		*c = `Creoles and pidgins`

  // Kashubian
  case "csb":
		*c = `Kashubian`

  // Collective name
  case "cus":
		*c = `Cushitic languages`

  // Czech
  case "cze":
		*c = `Czech`

  // Dakota
  case "dak":
		*c = `Dakota`

  // Danish
  case "dan":
		*c = `Danish`

  // Dargwa
  case "dar":
		*c = `Dargwa`

  // Collective name
  case "day":
		*c = `Land Dayak languages`

  // Macrolanguage
  case "del":
		*c = `Delaware`

  // Macrolanguage
  case "den":
		*c = `Slave (Athapascan)`

  // Dogrib
  case "dgr":
		*c = `Dogrib`

  // Macrolanguage
  case "din":
		*c = `Dinka`

  // Divehi; Dhivehi; Maldivian
  case "div":
		*c = `Divehi; Dhivehi; Maldivian`

  // Macrolanguage
  case "doi":
		*c = `Dogri`

  // Collective name
  case "dra":
		*c = `Dravidian languages`

  // Lower Sorbian
  case "dsb":
		*c = `Lower Sorbian`

  // Duala
  case "dua":
		*c = `Duala`

  // Dutch, Middle (ca. 1050-1350)
  case "dum":
		*c = `Dutch, Middle (ca. 1050-1350)`

  // Dutch; Flemish
  case "dut":
		*c = `Dutch; Flemish`

  // Dyula
  case "dyu":
		*c = `Dyula`

  // Dzongkha
  case "dzo":
		*c = `Dzongkha`

  // Efik
  case "efi":
		*c = `Efik`

  // ONIX local code for Italian dialect, equivalent to egl in ISO 639-3. For use in ONIX 3.0 only
  case "egl":
		*c = `Emilian`

  // Egyptian (Ancient)
  case "egy":
		*c = `Egyptian (Ancient)`

  // Ekajuk
  case "eka":
		*c = `Ekajuk`

  // Elamite
  case "elx":
		*c = `Elamite`

  // English
  case "eng":
		*c = `English`

  // English, Middle (1100-1500)
  case "enm":
		*c = `English, Middle (1100-1500)`

  // Artificial language
  case "epo":
		*c = `Esperanto`

  // Macrolanguage
  case "est":
		*c = `Estonian`

  // Ewe
  case "ewe":
		*c = `Ewe`

  // Ewondo
  case "ewo":
		*c = `Ewondo`

  // Fang
  case "fan":
		*c = `Fang`

  // Faroese
  case "fao":
		*c = `Faroese`

  // Fanti
  case "fat":
		*c = `Fanti`

  // Fijian
  case "fij":
		*c = `Fijian`

  // Filipino; Pilipino
  case "fil":
		*c = `Filipino; Pilipino`

  // Finnish
  case "fin":
		*c = `Finnish`

  // ONIX local code, equivalent to fit in ISO 639-3
  case "fit":
		*c = `Meänkieli / Tornedalen Finnish`

  // Collective name
  case "fiu":
		*c = `Finno-Ugrian languages`

  // ONIX local code, equivalent to fkv in ISO 639-3
  case "fkv":
		*c = `Kvensk`

  // Fon
  case "fon":
		*c = `Fon`

  // French
  case "fre":
		*c = `French`

  // French, Middle (ca. 1400-1600)
  case "frm":
		*c = `French, Middle (ca. 1400-1600)`

  // French, Old (ca. 842-1400)
  case "fro":
		*c = `French, Old (ca. 842-1400)`

  // Northern Frisian
  case "frr":
		*c = `Northern Frisian`

  // Eastern Frisian
  case "frs":
		*c = `Eastern Frisian`

  // Western Frisian
  case "fry":
		*c = `Western Frisian`

  // Fulah
  case "ful":
		*c = `Fulah`

  // Friulian
  case "fur":
		*c = `Friulian`

  // Gã
  case "gaa":
		*c = `Gã`

  // Gayo
  case "gay":
		*c = `Gayo`

  // Macrolanguage
  case "gba":
		*c = `Gbaya`

  // Collective name
  case "gem":
		*c = `Germanic languages`

  // Georgian
  case "geo":
		*c = `Georgian`

  // German
  case "ger":
		*c = `German`

  // Ethiopic (Ge’ez)
  case "gez":
		*c = `Ethiopic (Ge’ez)`

  // Gilbertese
  case "gil":
		*c = `Gilbertese`

  // Scottish Gaelic
  case "gla":
		*c = `Scottish Gaelic`

  // Irish
  case "gle":
		*c = `Irish`

  // Galician
  case "glg":
		*c = `Galician`

  // Manx
  case "glv":
		*c = `Manx`

  // German, Middle High (ca. 1050-1500)
  case "gmh":
		*c = `German, Middle High (ca. 1050-1500)`

  // German, Old High (ca. 750-1050)
  case "goh":
		*c = `German, Old High (ca. 750-1050)`

  // Macrolanguage
  case "gon":
		*c = `Gondi`

  // Gorontalo
  case "gor":
		*c = `Gorontalo`

  // Gothic
  case "got":
		*c = `Gothic`

  // Macrolanguage
  case "grb":
		*c = `Grebo`

  // Greek, Ancient (to 1453)
  case "grc":
		*c = `Greek, Ancient (to 1453)`

  // Greek, Modern (1453-)
  case "gre":
		*c = `Greek, Modern (1453-)`

  // Macrolanguage
  case "grn":
		*c = `Guarani`

  // ONIX local code, equivalent to grt in ISO 639-3
  case "grt":
		*c = `Garo`

  // Swiss German; Alemannic
  case "gsw":
		*c = `Swiss German; Alemannic`

  // Gujarati
  case "guj":
		*c = `Gujarati`

  // Gwich’in
  case "gwi":
		*c = `Gwich’in`

  // Macrolanguage
  case "hai":
		*c = `Haida`

  // Haitian French Creole
  case "hat":
		*c = `Haitian French Creole`

  // Hausa
  case "hau":
		*c = `Hausa`

  // Hawaiian
  case "haw":
		*c = `Hawaiian`

  // Hebrew
  case "heb":
		*c = `Hebrew`

  // Herero
  case "her":
		*c = `Herero`

  // Hiligaynon
  case "hil":
		*c = `Hiligaynon`

  // Collective name
  case "him":
		*c = `Himachali languages; Western Pahari languages`

  // Hindi
  case "hin":
		*c = `Hindi`

  // Hittite
  case "hit":
		*c = `Hittite`

  // Macrolanguage
  case "hmn":
		*c = `Hmong; Mong`

  // Hiri Motu
  case "hmo":
		*c = `Hiri Motu`

  // Croatian
  case "hrv":
		*c = `Croatian`

  // Upper Sorbian
  case "hsb":
		*c = `Upper Sorbian`

  // Hungarian
  case "hun":
		*c = `Hungarian`

  // Hupa
  case "hup":
		*c = `Hupa`

  // Iban
  case "iba":
		*c = `Iban`

  // Igbo
  case "ibo":
		*c = `Igbo`

  // Icelandic
  case "ice":
		*c = `Icelandic`

  // Artificial language
  case "ido":
		*c = `Ido`

  // Sichuan Yi; Nuosu
  case "iii":
		*c = `Sichuan Yi; Nuosu`

  // Collective name
  case "ijo":
		*c = `Ijo languages`

  // Macrolanguage
  case "iku":
		*c = `Inuktitut`

  // Artificial language
  case "ile":
		*c = `Interlingue; Occidental`

  // Iloko
  case "ilo":
		*c = `Iloko`

  // Artificial language
  case "ina":
		*c = `Interlingua (International Auxiliary Language Association)`

  // Collective name
  case "inc":
		*c = `Indic languages`

  // Indonesian
  case "ind":
		*c = `Indonesian`

  // Collective name
  case "ine":
		*c = `Indo-European languages`

  // Ingush
  case "inh":
		*c = `Ingush`

  // Macrolanguage
  case "ipk":
		*c = `Inupiaq`

  // Collective name
  case "ira":
		*c = `Iranian languages`

  // Collective name
  case "iro":
		*c = `Iroquoian languages`

  // Italian
  case "ita":
		*c = `Italian`

  // Javanese
  case "jav":
		*c = `Javanese`

  // Lojban
  case "jbo":
		*c = `Lojban`

  // Japanese
  case "jpn":
		*c = `Japanese`

  // Judeo-Persian
  case "jpr":
		*c = `Judeo-Persian`

  // Macrolanguage
  case "jrb":
		*c = `Judeo-Arabic`

  // Kara-Kalpak
  case "kaa":
		*c = `Kara-Kalpak`

  // Kabyle
  case "kab":
		*c = `Kabyle`

  // Kachin; Jingpho
  case "kac":
		*c = `Kachin; Jingpho`

  // Kalâtdlisut; Greenlandic
  case "kal":
		*c = `Kalâtdlisut; Greenlandic`

  // Kamba
  case "kam":
		*c = `Kamba`

  // Kannada
  case "kan":
		*c = `Kannada`

  // Collective name
  case "kar":
		*c = `Karen languages`

  // Kashmiri
  case "kas":
		*c = `Kashmiri`

  // Macrolanguage
  case "kau":
		*c = `Kanuri`

  // Kawi
  case "kaw":
		*c = `Kawi`

  // Kazakh
  case "kaz":
		*c = `Kazakh`

  // Kabardian (Circassian)
  case "kbd":
		*c = `Kabardian (Circassian)`

  // ONIX local code, equivalent to kdr in ISO 639-3
  case "kdr":
		*c = `Karaim`

  // Khasi
  case "kha":
		*c = `Khasi`

  // Collective name
  case "khi":
		*c = `Khoisan languages`

  // Central Khmer
  case "khm":
		*c = `Central Khmer`

  // Khotanese; Sakan
  case "kho":
		*c = `Khotanese; Sakan`

  // Kikuyu; Gikuyu
  case "kik":
		*c = `Kikuyu; Gikuyu`

  // Kinyarwanda
  case "kin":
		*c = `Kinyarwanda`

  // Kirghiz; Kyrgyz
  case "kir":
		*c = `Kirghiz; Kyrgyz`

  // Kimbundu
  case "kmb":
		*c = `Kimbundu`

  // Macrolanguage
  case "kok":
		*c = `Konkani`

  // Macrolanguage
  case "kom":
		*c = `Komi`

  // Macrolanguage
  case "kon":
		*c = `Kongo`

  // Korean
  case "kor":
		*c = `Korean`

  // Kusaiean (Caroline Islands)
  case "kos":
		*c = `Kusaiean (Caroline Islands)`

  // Macrolanguage
  case "kpe":
		*c = `Kpelle`

  // Karachay-Balkar
  case "krc":
		*c = `Karachay-Balkar`

  // Karelian
  case "krl":
		*c = `Karelian`

  // Collective name
  case "kro":
		*c = `Kru languages`

  // Kurukh
  case "kru":
		*c = `Kurukh`

  // Kuanyama
  case "kua":
		*c = `Kuanyama`

  // Kumyk
  case "kum":
		*c = `Kumyk`

  // Macrolanguage
  case "kur":
		*c = `Kurdish`

  // Kutenai
  case "kut":
		*c = `Kutenai`

  // Ladino
  case "lad":
		*c = `Ladino`

  // Macrolanguage
  case "lah":
		*c = `Lahnda`

  // Lamba
  case "lam":
		*c = `Lamba`

  // Lao
  case "lao":
		*c = `Lao`

  // Latin
  case "lat":
		*c = `Latin`

  // Macrolanguage
  case "lav":
		*c = `Latvian`

  // Lezgian
  case "lez":
		*c = `Lezgian`

  // ONIX local code for Italian dialect, equivalent to lij in ISO 639-3. For use in ONIX 3.0 only
  case "lij":
		*c = `Ligurian`

  // Limburgish
  case "lim":
		*c = `Limburgish`

  // Lingala
  case "lin":
		*c = `Lingala`

  // Lithuanian
  case "lit":
		*c = `Lithuanian`

  // ONIX local code for Italian dialect, equivalent to lmo in ISO 639-3. For use in ONIX 3.0 only
  case "lmo":
		*c = `Lombard`

  // Mongo-Nkundu
  case "lol":
		*c = `Mongo-Nkundu`

  // Lozi
  case "loz":
		*c = `Lozi`

  // Luxembourgish; Letzeburgesch
  case "ltz":
		*c = `Luxembourgish; Letzeburgesch`

  // Luba-Lulua
  case "lua":
		*c = `Luba-Lulua`

  // Luba-Katanga
  case "lub":
		*c = `Luba-Katanga`

  // Ganda
  case "lug":
		*c = `Ganda`

  // Luiseño
  case "lui":
		*c = `Luiseño`

  // Lunda
  case "lun":
		*c = `Lunda`

  // Luo (Kenya and Tanzania)
  case "luo":
		*c = `Luo (Kenya and Tanzania)`

  // Lushai
  case "lus":
		*c = `Lushai`

  // Macedonian
  case "mac":
		*c = `Macedonian`

  // Madurese
  case "mad":
		*c = `Madurese`

  // Magahi
  case "mag":
		*c = `Magahi`

  // Marshallese
  case "mah":
		*c = `Marshallese`

  // Maithili
  case "mai":
		*c = `Maithili`

  // Makasar
  case "mak":
		*c = `Makasar`

  // Malayalam
  case "mal":
		*c = `Malayalam`

  // Macrolanguage
  case "man":
		*c = `Mandingo`

  // Maori
  case "mao":
		*c = `Maori`

  // Collective name
  case "map":
		*c = `Austronesian languages`

  // Marathi
  case "mar":
		*c = `Marathi`

  // Masai
  case "mas":
		*c = `Masai`

  // Macrolanguage
  case "may":
		*c = `Malay`

  // Moksha
  case "mdf":
		*c = `Moksha`

  // Mandar
  case "mdr":
		*c = `Mandar`

  // Mende
  case "men":
		*c = `Mende`

  // Irish, Middle (ca. 1100-1550)
  case "mga":
		*c = `Irish, Middle (ca. 1100-1550)`

  // Mi’kmaq; Micmac
  case "mic":
		*c = `Mi’kmaq; Micmac`

  // Minangkabau
  case "min":
		*c = `Minangkabau`

  // Use where no suitable code is available
  case "mis":
		*c = `Uncoded languages`

  // Collective name
  case "mkh":
		*c = `Mon-Khmer languages`

  // Macrolanguage
  case "mlg":
		*c = `Malagasy`

  // Maltese
  case "mlt":
		*c = `Maltese`

  // Manchu
  case "mnc":
		*c = `Manchu`

  // Manipuri
  case "mni":
		*c = `Manipuri`

  // Collective name
  case "mno":
		*c = `Manobo languages`

  // Mohawk
  case "moh":
		*c = `Mohawk`

  // DEPRECATED – use rum
  case "mol":
		*c = `Moldavian; Moldovan`

  // Macrolanguage
  case "mon":
		*c = `Mongolian`

  // Mooré; Mossi
  case "mos":
		*c = `Mooré; Mossi`

  // Multiple languages
  case "mul":
		*c = `Multiple languages`

  // Collective name
  case "mun":
		*c = `Munda languages`

  // Creek
  case "mus":
		*c = `Creek`

  // ONIX local code, equivalent to mwf in ISO 639-3. For use in ONIX 3.0 only
  case "mwf":
		*c = `Murrinh-Patha`

  // Mirandese
  case "mwl":
		*c = `Mirandese`

  // Macrolanguage
  case "mwr":
		*c = `Marwari`

  // Collective name
  case "myn":
		*c = `Mayan languages`

  // Erzya
  case "myv":
		*c = `Erzya`

  // Collective name
  case "nah":
		*c = `Nahuatl languages`

  // Collective name
  case "nai":
		*c = `North American Indian languages`

  // Neapolitan
  case "nap":
		*c = `Neapolitan`

  // Nauruan
  case "nau":
		*c = `Nauruan`

  // Navajo
  case "nav":
		*c = `Navajo`

  // Ndebele, South
  case "nbl":
		*c = `Ndebele, South`

  // Ndebele, North
  case "nde":
		*c = `Ndebele, North`

  // Ndonga
  case "ndo":
		*c = `Ndonga`

  // Low German; Low Saxon
  case "nds":
		*c = `Low German; Low Saxon`

  // Macrolanguage
  case "nep":
		*c = `Nepali`

  // Newari; Nepal Bhasa
  case "new":
		*c = `Newari; Nepal Bhasa`

  // Nias
  case "nia":
		*c = `Nias`

  // Collective name
  case "nic":
		*c = `Niger-Kordofanian languages`

  // Niuean
  case "niu":
		*c = `Niuean`

  // Norwegian Nynorsk
  case "nno":
		*c = `Norwegian Nynorsk`

  // Norwegian Bokmål
  case "nob":
		*c = `Norwegian Bokmål`

  // Nogai
  case "nog":
		*c = `Nogai`

  // Old Norse
  case "non":
		*c = `Old Norse`

  // Macrolanguage
  case "nor":
		*c = `Norwegian`

  // N’Ko
  case "nqo":
		*c = `N’Ko`

  // ONIX local code, equivalent to nrf in ISO 639-3. For use in ONIX 3.0 only
  case "nrf":
		*c = `Guernésiais, Jèrriais`

  // Pedi; Sepedi; Northern Sotho
  case "nso":
		*c = `Pedi; Sepedi; Northern Sotho`

  // Collective name
  case "nub":
		*c = `Nubian languages`

  // Classical Newari; Old Newari; Classical Nepal Bhasa
  case "nwc":
		*c = `Classical Newari; Old Newari; Classical Nepal Bhasa`

  // Chichewa; Chewa; Nyanja
  case "nya":
		*c = `Chichewa; Chewa; Nyanja`

  // Nyamwezi
  case "nym":
		*c = `Nyamwezi`

  // Nyankole
  case "nyn":
		*c = `Nyankole`

  // Nyoro
  case "nyo":
		*c = `Nyoro`

  // Nzima
  case "nzi":
		*c = `Nzima`

  // Occitan (post 1500)
  case "oci":
		*c = `Occitan (post 1500)`

  // ONIX local code, equivalent to odt in ISO 639-3
  case "odt":
		*c = `Old Dutch / Old Low Franconian (ca. 400–1050)`

  // Macrolanguage
  case "oji":
		*c = `Ojibwa`

  // ONIX local code, equivalent to omq in ISO 639-5. Collective name
  case "omq":
		*c = `Oto-Manguean languages`

  // Macrolanguage
  case "ori":
		*c = `Oriya`

  // Macrolanguage
  case "orm":
		*c = `Oromo`

  // Osage
  case "osa":
		*c = `Osage`

  // Ossetian; Ossetic
  case "oss":
		*c = `Ossetian; Ossetic`

  // Turkish, Ottoman
  case "ota":
		*c = `Turkish, Ottoman`

  // Collective name
  case "oto":
		*c = `Otomian languages`

  // Collective name
  case "paa":
		*c = `Papuan languages`

  // Pangasinan
  case "pag":
		*c = `Pangasinan`

  // Pahlavi
  case "pal":
		*c = `Pahlavi`

  // Pampanga; Kapampangan
  case "pam":
		*c = `Pampanga; Kapampangan`

  // Panjabi
  case "pan":
		*c = `Panjabi`

  // Papiamento
  case "pap":
		*c = `Papiamento`

  // Palauan
  case "pau":
		*c = `Palauan`

  // Old Persian (ca. 600-400 B.C.)
  case "peo":
		*c = `Old Persian (ca. 600-400 B.C.)`

  // Macrolanguage
  case "per":
		*c = `Persian; Farsi`

  // ONIX local code, equivalent to pes in ISO 639-3. For use in ONIX 3.0 only
  case "pes":
		*c = `Iranian Persian; Parsi`

  // Collective name
  case "phi":
		*c = `Philippine languages`

  // Phoenician
  case "phn":
		*c = `Phoenician`

  // Pali
  case "pli":
		*c = `Pali`

  // ONIX local code for Italian dialect, equivalent to pms in ISO 639-3. For use in ONIX 3.0 only
  case "pms":
		*c = `Piedmontese`

  // Polish
  case "pol":
		*c = `Polish`

  // Ponapeian
  case "pon":
		*c = `Ponapeian`

  // Portuguese
  case "por":
		*c = `Portuguese`

  // Collective name
  case "pra":
		*c = `Prakrit languages`

  // Provençal, Old (to 1500); Occitan, Old (to 1500)
  case "pro":
		*c = `Provençal, Old (to 1500); Occitan, Old (to 1500)`

  // ONIX local code, equivalent to prs in ISO 639-3. For use in ONIX 3.0 only
  case "prs":
		*c = `Dari; Afghan Persian`

  // Macrolanguage
  case "pus":
		*c = `Pushto; Pashto`

  // ONIX local code, distinct dialect of Occitan (not distinguished from oci by ISO 639-3)
  case "qar":
		*c = `Aranés`

  // ONIX local code, distinct dialect of Catalan (not distinguished from cat by ISO 639-3)
  case "qav":
		*c = `Valencian`

  // ONIX local code, distinct variant of langue d’oïl (old northern French) (not distinguished from fro, or from frm, fre, nrf by ISO 639-3). For use in ONIX 3.0 only
  case "qgl":
		*c = `Gallo`

  // ONIX local code, distinct dialect of of Rusyn (not distinguished from rue by ISO 639-3). For use in ONIX 3.0 only
  case "qlk":
		*c = `Lemko`

  // ONIX local code, distinct and exclusively spoken variation of Spanish, not distinguished from spa (Spanish, Castilian) by ISO 639-3. Neutral Latin American Spanish should be considered a ‘shorthand’ for spa plus a ‘country code’ for Latin America – but prefer spa plus the relevant country code for specifically Mexican Spanish, Argentine (Rioplatense) Spanish, Puerto Rican Spanish etc. Neutral Latin American Spanish must only be used with audio material (including the audio tracks of TV, video and film) to indicate use of accent, vocabulary and construction suitable for broad use across Latin America. For use in ONIX 3.0 only
  case "qls":
		*c = `Neutral Latin American Spanish`

  // Macrolanguage
  case "que":
		*c = `Quechua`

  // Macrolanguage
  case "raj":
		*c = `Rajasthani`

  // Rapanui
  case "rap":
		*c = `Rapanui`

  // Rarotongan; Cook Islands Maori
  case "rar":
		*c = `Rarotongan; Cook Islands Maori`

  // ONIX local code, equivalent to rcf in ISO 639-3. For use in ONIX 3.0 only
  case "rcf":
		*c = `Réunion Creole French`

  // ONIX local code for Italian dialect, equivalent to rgl in ISO 639-3. For use in ONIX 3.0 only
  case "rgn":
		*c = `Romagnol`

  // Collective name
  case "roa":
		*c = `Romance languages`

  // Romansh
  case "roh":
		*c = `Romansh`

  // Macrolanguage
  case "rom":
		*c = `Romany`

  // Romanian
  case "rum":
		*c = `Romanian`

  // Rundi
  case "run":
		*c = `Rundi`

  // Aromanian; Arumanian; Macedo-Romanian
  case "rup":
		*c = `Aromanian; Arumanian; Macedo-Romanian`

  // Russian
  case "rus":
		*c = `Russian`

  // Sandawe
  case "sad":
		*c = `Sandawe`

  // Sango
  case "sag":
		*c = `Sango`

  // Yakut
  case "sah":
		*c = `Yakut`

  // Collective name
  case "sai":
		*c = `South American Indian languages`

  // Collective name
  case "sal":
		*c = `Salishan languages`

  // Samaritan Aramaic
  case "sam":
		*c = `Samaritan Aramaic`

  // Sanskrit
  case "san":
		*c = `Sanskrit`

  // Sasak
  case "sas":
		*c = `Sasak`

  // Santali
  case "sat":
		*c = `Santali`

  // DEPRECATED – use srp
  case "scc":
		*c = `Serbian`

  // Sicilian
  case "scn":
		*c = `Sicilian`

  // Scots
  case "sco":
		*c = `Scots`

  // DEPRECATED – use hrv
  case "scr":
		*c = `Croatian`

  // ONIX local code for Sardinian dialect, equivalent to sdc in ISO 639-3. For use in ONIX 3.0 only
  case "sdc":
		*c = `Sassarese`

  // ONIX local code for Sardinian dialect, equivalent to sdn in ISO 639-3. For use in ONIX 3.0 only
  case "sdn":
		*c = `Gallurese`

  // Selkup
  case "sel":
		*c = `Selkup`

  // Collective name
  case "sem":
		*c = `Semitic languages`

  // Irish, Old (to 1100)
  case "sga":
		*c = `Irish, Old (to 1100)`

  // Collective name
  case "sgn":
		*c = `Sign languages`

  // Shan
  case "shn":
		*c = `Shan`

  // Sidamo
  case "sid":
		*c = `Sidamo`

  // Sinhala; Sinhalese
  case "sin":
		*c = `Sinhala; Sinhalese`

  // Collective name
  case "sio":
		*c = `Siouan languages`

  // Collective name
  case "sit":
		*c = `Sino-Tibetan languages`

  // Collective name
  case "sla":
		*c = `Slavic languages`

  // Slovak
  case "slo":
		*c = `Slovak`

  // Slovenian
  case "slv":
		*c = `Slovenian`

  // Southern Sami
  case "sma":
		*c = `Southern Sami`

  // Northern Sami
  case "sme":
		*c = `Northern Sami`

  // Collective name
  case "smi":
		*c = `Sami languages`

  // Lule Sami
  case "smj":
		*c = `Lule Sami`

  // Inari Sami
  case "smn":
		*c = `Inari Sami`

  // Samoan
  case "smo":
		*c = `Samoan`

  // Skolt Sami
  case "sms":
		*c = `Skolt Sami`

  // Shona
  case "sna":
		*c = `Shona`

  // Sindhi
  case "snd":
		*c = `Sindhi`

  // Soninke
  case "snk":
		*c = `Soninke`

  // Sogdian
  case "sog":
		*c = `Sogdian`

  // Somali
  case "som":
		*c = `Somali`

  // Collective name
  case "son":
		*c = `Songhai languages`

  // Sotho; Sesotho
  case "sot":
		*c = `Sotho; Sesotho`

  // Spanish
  case "spa":
		*c = `Spanish`

  // Macrolanguage
  case "srd":
		*c = `Sardinian`

  // Sranan Tongo
  case "srn":
		*c = `Sranan Tongo`

  // ONIX local code for Sardinian dialect, equivalent to sro in ISO 639-3. For use in ONIX 3.0 only
  case "sro":
		*c = `Campidanese`

  // Serbian
  case "srp":
		*c = `Serbian`

  // Serer
  case "srr":
		*c = `Serer`

  // Collective name
  case "ssa":
		*c = `Nilo-Saharan languages`

  // Swazi; Swati
  case "ssw":
		*c = `Swazi; Swati`

  // Sukuma
  case "suk":
		*c = `Sukuma`

  // Sundanese
  case "sun":
		*c = `Sundanese`

  // Susu
  case "sus":
		*c = `Susu`

  // Sumerian
  case "sux":
		*c = `Sumerian`

  // Macrolanguage
  case "swa":
		*c = `Swahili`

  // Swedish
  case "swe":
		*c = `Swedish`

  // Classical Syriac
  case "syc":
		*c = `Classical Syriac`

  // Macrolanguage
  case "syr":
		*c = `Syriac`

  // Tahitian
  case "tah":
		*c = `Tahitian`

  // Collective name
  case "tai":
		*c = `Tai languages`

  // Tamil
  case "tam":
		*c = `Tamil`

  // Tatar
  case "tat":
		*c = `Tatar`

  // Telugu
  case "tel":
		*c = `Telugu`

  // Temne; Time
  case "tem":
		*c = `Temne; Time`

  // Terena
  case "ter":
		*c = `Terena`

  // Tetum
  case "tet":
		*c = `Tetum`

  // Tajik; Tajiki Persian
  case "tgk":
		*c = `Tajik; Tajiki Persian`

  // Tagalog
  case "tgl":
		*c = `Tagalog`

  // Thai
  case "tha":
		*c = `Thai`

  // Tibetan
  case "tib":
		*c = `Tibetan`

  // Tigré
  case "tig":
		*c = `Tigré`

  // Tigrinya
  case "tir":
		*c = `Tigrinya`

  // Tiv
  case "tiv":
		*c = `Tiv`

  // Tokelauan
  case "tkl":
		*c = `Tokelauan`

  // Artificial language
  case "tlh":
		*c = `Klingon; tlhIngan-Hol`

  // Tlingit
  case "tli":
		*c = `Tlingit`

  // Macrolanguage
  case "tmh":
		*c = `Tamashek`

  // Tonga (Nyasa)
  case "tog":
		*c = `Tonga (Nyasa)`

  // Tongan
  case "ton":
		*c = `Tongan`

  // Tok Pisin
  case "tpi":
		*c = `Tok Pisin`

  // Tsimshian
  case "tsi":
		*c = `Tsimshian`

  // AKA Setswana
  case "tsn":
		*c = `Tswana`

  // Tsonga
  case "tso":
		*c = `Tsonga`

  // Turkmen
  case "tuk":
		*c = `Turkmen`

  // Tumbuka
  case "tum":
		*c = `Tumbuka`

  // Collective name
  case "tup":
		*c = `Tupi languages`

  // Turkish
  case "tur":
		*c = `Turkish`

  // Altaic languages
  case "tut":
		*c = `Altaic languages`

  // Tuvaluan
  case "tvl":
		*c = `Tuvaluan`

  // Twi
  case "twi":
		*c = `Twi`

  // Tuvinian
  case "tyv":
		*c = `Tuvinian`

  // ONIX local code, equivalent to tzo in ISO 639-3
  case "tzo":
		*c = `Tzotzil`

  // Udmurt
  case "udm":
		*c = `Udmurt`

  // Ugaritic
  case "uga":
		*c = `Ugaritic`

  // Uighur; Uyghur
  case "uig":
		*c = `Uighur; Uyghur`

  // Ukrainian
  case "ukr":
		*c = `Ukrainian`

  // Umbundu
  case "umb":
		*c = `Umbundu`

  // Undetermined language
  case "und":
		*c = `Undetermined language`

  // Urdu
  case "urd":
		*c = `Urdu`

  // Macrolanguage
  case "uzb":
		*c = `Uzbek`

  // Vai
  case "vai":
		*c = `Vai`

  // ONIX local code for Italian dialect, equivalent to vec in ISO 639-3. For use in ONIX 3.0 only
  case "vec":
		*c = `Venetian/Venetan`

  // Venda
  case "ven":
		*c = `Venda`

  // Vietnamese
  case "vie":
		*c = `Vietnamese`

  // Artificial language
  case "vol":
		*c = `Volapük`

  // Votic
  case "vot":
		*c = `Votic`

  // Collective name
  case "wak":
		*c = `Wakashan languages`

  // Wolaitta; Wolaytta
  case "wal":
		*c = `Wolaitta; Wolaytta`

  // Waray
  case "war":
		*c = `Waray`

  // Washo
  case "was":
		*c = `Washo`

  // Welsh
  case "wel":
		*c = `Welsh`

  // Collective name
  case "wen":
		*c = `Sorbian languages`

  // Walloon
  case "wln":
		*c = `Walloon`

  // Wolof
  case "wol":
		*c = `Wolof`

  // Kalmyk
  case "xal":
		*c = `Kalmyk`

  // Xhosa
  case "xho":
		*c = `Xhosa`

  // ONIX local code, equivalent to xuu in ISO 639-3. For use in ONIX 3.0 only
  case "xuu":
		*c = `Khwedam, Kxoe`

  // Yao
  case "yao":
		*c = `Yao`

  // Yapese
  case "yap":
		*c = `Yapese`

  // Macrolanguage
  case "yid":
		*c = `Yiddish`

  // Yoruba
  case "yor":
		*c = `Yoruba`

  // Collective name
  case "ypk":
		*c = `Yupik languages`

  // ONIX local code, equivalent to yue in ISO 639-3
  case "yue":
		*c = `Cantonese`

  // Macrolanguage
  case "zap":
		*c = `Zapotec`

  // Artificial language
  case "zbl":
		*c = `Blissymbols; Blissymbolics; Bliss`

  // Zenaga
  case "zen":
		*c = `Zenaga`

  // Standard Moroccan Tamazight
  case "zgh":
		*c = `Standard Moroccan Tamazight`

  // Macrolanguage
  case "zha":
		*c = `Zhuang; Chuang`

  // Collective name
  case "znd":
		*c = `Zande languages`

  // Zulu
  case "zul":
		*c = `Zulu`

  // Zuni
  case "zun":
		*c = `Zuni`

  // No linguistic content
  case "zxx":
		*c = `No linguistic content`

  // Macrolanguage
  case "zza":
		*c = `Zaza; Dimili; Dimli; Kirdki; Kirmanjki; Zazaki`
	default:
		return fmt.Errorf("undefined code for LanguageList74 has been passed, got [%s]", v)
	}
	return nil
}

// NMTOKEN has not document
type NMTOKEN string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *NMTOKEN) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {
	default:
		return fmt.Errorf("undefined code for NMTOKEN has been passed, got [%s]", v)
	}
}

// Nohref has not document
type Nohref string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *Nohref) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

  // 
  case "nohref":
		*c = ``
	default:
		return fmt.Errorf("undefined code for Nohref has been passed, got [%s]", v)
	}
	return nil
}

// Release has not document
type Release string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *Release) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {
	default:
		return fmt.Errorf("undefined code for Release has been passed, got [%s]", v)
	}
}

// TextscriptList121 has not document
type TextscriptList121 string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *TextscriptList121) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

  // For use in ONIX 3.0 only
  case "Adlm":
		*c = `Adlam`

  // Script is not supported by Unicode
  case "Afak":
		*c = `Afaka`

  // Ancient/historic script. For use in ONIX 3.0 only
  case "Aghb":
		*c = `Caucasian Albanian`

  // Ancient/historic script. For use in ONIX 3.0 only
  case "Ahom":
		*c = `Ahom, Tai Ahom`

  // Arabic
  case "Arab":
		*c = `Arabic`

  // Typographic variant of Arabic. For use in ONIX 3.0 only
  case "Aran":
		*c = `Arabic (Nastaliq variant)`

  // Ancient/historic script
  case "Armi":
		*c = `Imperial Aramaic`

  // Armenian
  case "Armn":
		*c = `Armenian`

  // Ancient/historic script
  case "Avst":
		*c = `Avestan`

  // Balinese
  case "Bali":
		*c = `Balinese`

  // Bamun
  case "Bamu":
		*c = `Bamun`

  // Ancient/historic script
  case "Bass":
		*c = `Bassa Vah`

  // Batak
  case "Batk":
		*c = `Batak`

  // Bengali (Bangla)
  case "Beng":
		*c = `Bengali (Bangla)`

  // Ancient/historic script. For use in ONIX 3.0 only
  case "Bhks":
		*c = `Bhaiksuki`

  // Script is not supported by Unicode
  case "Blis":
		*c = `Blissymbols`

  // Bopomofo
  case "Bopo":
		*c = `Bopomofo`

  // Ancient/historic script
  case "Brah":
		*c = `Brahmi`

  // Braille
  case "Brai":
		*c = `Braille`

  // Buginese
  case "Bugi":
		*c = `Buginese`

  // Buhid
  case "Buhd":
		*c = `Buhid`

  // Chakma
  case "Cakm":
		*c = `Chakma`

  // Unified Canadian Aboriginal Syllabics
  case "Cans":
		*c = `Unified Canadian Aboriginal Syllabics`

  // Ancient/historic script
  case "Cari":
		*c = `Carian`

  // Cham
  case "Cham":
		*c = `Cham`

  // Cherokee
  case "Cher":
		*c = `Cherokee`

  // Script is not supported by Unicode
  case "Cirt":
		*c = `Cirth`

  // Ancient/historic script
  case "Copt":
		*c = `Coptic`

  // Ancient/historic script
  case "Cprt":
		*c = `Cypriot`

  // Cyrillic
  case "Cyrl":
		*c = `Cyrillic`

  // Ancient/historic, typographic variant of Cyrillic
  case "Cyrs":
		*c = `Cyrillic (Old Church Slavonic variant)`

  // Devanagari (Nagari)
  case "Deva":
		*c = `Devanagari (Nagari)`

  // Script is not supported by Unicode. For use in ONIX 3.0 only
  case "Dogr":
		*c = `Dogra`

  // Deseret (Mormon)
  case "Dsrt":
		*c = `Deseret (Mormon)`

  // Duployan shorthand, Duployan stenography
  case "Dupl":
		*c = `Duployan shorthand, Duployan stenography`

  // Script is not supported by Unicode
  case "Egyd":
		*c = `Egyptian demotic`

  // Script is not supported by Unicode
  case "Egyh":
		*c = `Egyptian hieratic`

  // Ancient/historic script
  case "Egyp":
		*c = `Egyptian hieroglyphs`

  // Ancient/historic script
  case "Elba":
		*c = `Elbasan`

  // Ethiopic (Ge‘ez)
  case "Ethi":
		*c = `Ethiopic (Ge‘ez)`

  // Georgian in Unicode
  case "Geok":
		*c = `Khutsuri (Asomtavruli and Khutsuri)`

  // Georgian (Mkhedruli and Mtavruli)
  case "Geor":
		*c = `Georgian (Mkhedruli and Mtavruli)`

  // Ancient/historic script
  case "Glag":
		*c = `Glagolitic`

  // Script is not supported by Unicode. For use in ONIX 3.0 only
  case "Gong":
		*c = `Gunjala Gondi`

  // For use in ONIX 3.0 only
  case "Gonm":
		*c = `Masaram Gondi`

  // Ancient/historic script
  case "Goth":
		*c = `Gothic`

  // Ancient/historic script
  case "Gran":
		*c = `Grantha`

  // Greek
  case "Grek":
		*c = `Greek`

  // Gujarati
  case "Gujr":
		*c = `Gujarati`

  // Gurmukhi
  case "Guru":
		*c = `Gurmukhi`

  // See Hani, Bopo. For use in ONIX 3.0 only
  case "Hanb":
		*c = `Han with Bopomofo`

  // Hangul (Hangŭl, Hangeul)
  case "Hang":
		*c = `Hangul (Hangŭl, Hangeul)`

  // Han (Hanzi, Kanji, Hanja)
  case "Hani":
		*c = `Han (Hanzi, Kanji, Hanja)`

  // Hanunoo (Hanunóo)
  case "Hano":
		*c = `Hanunoo (Hanunóo)`

  // Subset of Hani
  case "Hans":
		*c = `Han (Simplified variant)`

  // Subset of Hani
  case "Hant":
		*c = `Han (Traditional variant)`

  // Ancient/historic script. For use in ONIX 3.0 only
  case "Hatr":
		*c = `Hatran`

  // Hebrew
  case "Hebr":
		*c = `Hebrew`

  // Hiragana
  case "Hira":
		*c = `Hiragana`

  // Ancient/historic script. For use in ONIX 3.0 only
  case "Hluw":
		*c = `Anatolian Hieroglyphs (Luwian Hieroglyphs, Hittite Hieroglyphs)`

  // Pahawh Hmong
  case "Hmng":
		*c = `Pahawh Hmong`

  // See Hira, Kana
  case "Hrkt":
		*c = `Japanese syllabaries (alias for Hiragana + Katakana)`

  // Ancient/historic script
  case "Hung":
		*c = `Old Hungarian (Hungarian Runic)`

  // Script is not supported by Unicode
  case "Inds":
		*c = `Indus (Harappan)`

  // Ancient/historic script
  case "Ital":
		*c = `Old Italic (Etruscan, Oscan, etc.)`

  // Subset of Hang. For use in ONIX 3.0 only
  case "Jamo":
		*c = `Jamo (alias for Jamo subset of Hangul)`

  // Javanese
  case "Java":
		*c = `Javanese`

  // See Hani, Hira and Kana
  case "Jpan":
		*c = `Japanese (alias for Han + Hiragana + Katakana)`

  // Script is not supported by Unicode
  case "Jurc":
		*c = `Jurchen`

  // Kayah Li
  case "Kali":
		*c = `Kayah Li`

  // Katakana
  case "Kana":
		*c = `Katakana`

  // Ancient/historic script
  case "Khar":
		*c = `Kharoshthi`

  // Khmer
  case "Khmr":
		*c = `Khmer`

  // Ancient/historic script
  case "Khoj":
		*c = `Khojki`

  // Script is not supported by Unicode. For use in ONIX 3.0 only
  case "Kitl":
		*c = `Khitan large script`

  // Script is not supported by Unicode. For use in ONIX 3.0 only
  case "Kits":
		*c = `Khitan small script`

  // Kannada
  case "Knda":
		*c = `Kannada`

  // See Hani and Hang
  case "Kore":
		*c = `Korean (alias for Hangul + Han)`

  // Script is not supported by Unicode
  case "Kpel":
		*c = `Kpelle`

  // Ancient/historic script
  case "Kthi":
		*c = `Kaithi`

  // Tai Tham (Lanna)
  case "Lana":
		*c = `Tai Tham (Lanna)`

  // Lao
  case "Laoo":
		*c = `Lao`

  // Typographic variant of Latin
  case "Latf":
		*c = `Latin (Fraktur variant)`

  // Typographic variant of Latin
  case "Latg":
		*c = `Latin (Gaelic variant)`

  // Latin
  case "Latn":
		*c = `Latin`

  // Script is not supported by Unicode. For use in ONIX 3.0 only
  case "Leke":
		*c = `Leke`

  // Lepcha (Róng)
  case "Lepc":
		*c = `Lepcha (Róng)`

  // Limbu
  case "Limb":
		*c = `Limbu`

  // Ancient/historic script
  case "Lina":
		*c = `Linear A`

  // Ancient/historic script
  case "Linb":
		*c = `Linear B`

  // Lisu (Fraser)
  case "Lisu":
		*c = `Lisu (Fraser)`

  // Script is not supported by Unicode
  case "Loma":
		*c = `Loma`

  // Ancient/historic script
  case "Lyci":
		*c = `Lycian`

  // Ancient/historic script
  case "Lydi":
		*c = `Lydian`

  // Ancient/historic script. For use in ONIX 3.0 only
  case "Mahj":
		*c = `Mahajani`

  // Script is not supported by Unicode. For use in ONIX 3.0 only
  case "Maka":
		*c = `Makasar`

  // Mandaic, Mandaean
  case "Mand":
		*c = `Mandaic, Mandaean`

  // Ancient/historic script
  case "Mani":
		*c = `Manichaean`

  // Ancient/historic script. For use in ONIX 3.0 only
  case "Marc":
		*c = `Marchen`

  // Script is not supported by Unicode
  case "Maya":
		*c = `Mayan hieroglyphs`

  // Script is not supported by Unicode. For use in ONIX 3.0 only
  case "Medf":
		*c = `Medefaidrin (Oberi Okaime, Oberi Ɔkaimɛ)`

  // Mende Kikakui
  case "Mend":
		*c = `Mende Kikakui`

  // Ancient/historic script
  case "Merc":
		*c = `Meroitic Cursive`

  // Ancient/historic script
  case "Mero":
		*c = `Meroitic Hieroglyphs`

  // Malayalam
  case "Mlym":
		*c = `Malayalam`

  // Ancient/historic script. For use in ONIX 3.0 only
  case "Modi":
		*c = `Modi, Moḍī`

  // Includes Clear, Manchu scripts
  case "Mong":
		*c = `Mongolian`

  // Script is not supported by Unicode
  case "Moon":
		*c = `Moon (Moon code, Moon script, Moon type)`

  // Mro, Mru
  case "Mroo":
		*c = `Mro, Mru`

  // Meitei Mayek (Meithei, Meetei)
  case "Mtei":
		*c = `Meitei Mayek (Meithei, Meetei)`

  // Ancient/historic script. For use in ONIX 3.0 only
  case "Mult":
		*c = `Multani`

  // Myanmar (Burmese)
  case "Mymr":
		*c = `Myanmar (Burmese)`

  // Ancient/historic script
  case "Narb":
		*c = `Old North Arabian (Ancient North Arabian)`

  // Ancient/historic script
  case "Nbat":
		*c = `Nabatean`

  // For use in ONIX 3.0 only
  case "Newa":
		*c = `Newa, Newar, Newari, Nepāla lipi`

  // Script is not supported by Unicode
  case "Nkgb":
		*c = `Nakhi Geba (’Na-’Khi ²Ggŏ-¹baw, Naxi Geba)`

  // N’Ko
  case "Nkoo":
		*c = `N’Ko`

  // Nüshu
  case "Nshu":
		*c = `Nüshu`

  // Ancient/historic script
  case "Ogam":
		*c = `Ogham`

  // Ol Chiki (Ol Cemet’, Ol, Santali)
  case "Olck":
		*c = `Ol Chiki (Ol Cemet’, Ol, Santali)`

  // Ancient/historic script
  case "Orkh":
		*c = `Old Turkic, Orkhon Runic`

  // Oriya (Odia)
  case "Orya":
		*c = `Oriya (Odia)`

  // For use in ONIX 3.0 only
  case "Osge":
		*c = `Osage`

  // Osmanya
  case "Osma":
		*c = `Osmanya`

  // Ancient/historic script
  case "Palm":
		*c = `Palmyrene`

  // For use in ONIX 3.0 only
  case "Pauc":
		*c = `Pau Cin Hau`

  // Ancient/historic script
  case "Perm":
		*c = `Old Permic`

  // Ancient/historic script
  case "Phag":
		*c = `Phags-pa`

  // Ancient/historic script
  case "Phli":
		*c = `Inscriptional Pahlavi`

  // Ancient/historic script
  case "Phlp":
		*c = `Psalter Pahlavi`

  // Script is not supported by Unicode
  case "Phlv":
		*c = `Book Pahlavi`

  // Ancient/historic script
  case "Phnx":
		*c = `Phoenician`

  // Miao (Pollard)
  case "Plrd":
		*c = `Miao (Pollard)`

  // Script is not supported by Unicode. For use in ONIX 3.0 only
  case "Piqd":
		*c = `Klingon (KLI plqaD)`

  // Ancient/historic script
  case "Prti":
		*c = `Inscriptional Parthian`

  // Reserved for private use (start)
  case "Qaaa":
		*c = `Reserved for private use (start)`

  // Reserved for private use (end)
  case "Qabx":
		*c = `Reserved for private use (end)`

  // Rejang (Redjang, Kaganga)
  case "Rjng":
		*c = `Rejang (Redjang, Kaganga)`

  // Script is not supported by Unicode
  case "Roro":
		*c = `Rongorongo`

  // Ancient/historic script
  case "Runr":
		*c = `Runic`

  // Samaritan
  case "Samr":
		*c = `Samaritan`

  // Script is not supported by Unicode
  case "Sara":
		*c = `Sarati`

  // Ancient/historic script
  case "Sarb":
		*c = `Old South Arabian`

  // Saurashtra
  case "Saur":
		*c = `Saurashtra`

  // SignWriting
  case "Sgnw":
		*c = `SignWriting`

  // Shavian (Shaw)
  case "Shaw":
		*c = `Shavian (Shaw)`

  // Sharada, Śāradā
  case "Shrd":
		*c = `Sharada, Śāradā`

  // Ancient/historic script. For use in ONIX 3.0 only
  case "Sidd":
		*c = `Siddham, Siddhaṃ, Siddhamātṛkā`

  // Khudawadi, Sindhi
  case "Sind":
		*c = `Khudawadi, Sindhi`

  // Sinhala
  case "Sinh":
		*c = `Sinhala`

  // Sora Sompeng
  case "Sora":
		*c = `Sora Sompeng`

  // For use in ONIX 3.0 only
  case "Soyo":
		*c = `Soyombo`

  // Sundanese
  case "Sund":
		*c = `Sundanese`

  // Syloti Nagri
  case "Sylo":
		*c = `Syloti Nagri`

  // Syriac
  case "Syrc":
		*c = `Syriac`

  // Typographic variant of Syriac
  case "Syre":
		*c = `Syriac (Estrangelo variant)`

  // Typographic variant of Syriac
  case "Syrj":
		*c = `Syriac (Western variant)`

  // Typographic variant of Syriac
  case "Syrn":
		*c = `Syriac (Eastern variant)`

  // Tagbanwa
  case "Tagb":
		*c = `Tagbanwa`

  // Takri, Ṭākrī, Ṭāṅkrī
  case "Takr":
		*c = `Takri, Ṭākrī, Ṭāṅkrī`

  // Tai Le
  case "Tale":
		*c = `Tai Le`

  // New Tai Lue
  case "Talu":
		*c = `New Tai Lue`

  // Tamil
  case "Taml":
		*c = `Tamil`

  // Ancient/historic script
  case "Tang":
		*c = `Tangut`

  // Tai Viet
  case "Tavt":
		*c = `Tai Viet`

  // Telugu
  case "Telu":
		*c = `Telugu`

  // Script is not supported by Unicode
  case "Teng":
		*c = `Tengwar`

  // Tifinagh (Berber)
  case "Tfng":
		*c = `Tifinagh (Berber)`

  // Tagalog (Baybayin, Alibata)
  case "Tglg":
		*c = `Tagalog (Baybayin, Alibata)`

  // Thaana
  case "Thaa":
		*c = `Thaana`

  // Thai
  case "Thai":
		*c = `Thai`

  // Tibetan
  case "Tibt":
		*c = `Tibetan`

  // Tirhuta
  case "Tirh":
		*c = `Tirhuta`

  // Ancient/historic script
  case "Ugar":
		*c = `Ugaritic`

  // Vai
  case "Vaii":
		*c = `Vai`

  // Script is not supported by Unicode
  case "Visp":
		*c = `Visible Speech`

  // Warang Citi (Varang Kshiti)
  case "Wara":
		*c = `Warang Citi (Varang Kshiti)`

  // Script is not supported by Unicode
  case "Wole":
		*c = `Woleai`

  // Ancient/historic script
  case "Xpeo":
		*c = `Old Persian`

  // Ancient/historic script
  case "Xsux":
		*c = `Cuneiform, Sumero-Akkadian`

  // Yi
  case "Yiii":
		*c = `Yi`

  // For use in ONIX 3.0 only
  case "Zanb":
		*c = `Zanabazar Square (Zanabazarin Dörböljin Useg, Xewtee Dörböljin Bicig, Horizontal Square Script)`

  // Code for inherited script
  case "Zinh":
		*c = `Code for inherited script`

  // Not a script in Unicode
  case "Zmth":
		*c = `Mathematical notation`

  // Not a script in Unicode. For use in ONIX 3.0 only
  case "Zsye":
		*c = `Symbols (Emoji variant)`

  // Not a script in Unicode
  case "Zsym":
		*c = `Symbols`

  // Not a script in Unicode
  case "Zxxx":
		*c = `Code for unwritten documents`

  // Code for undetermined script
  case "Zyyy":
		*c = `Code for undetermined script`

  // Code for uncoded script
  case "Zzzz":
		*c = `Code for uncoded script`
	default:
		return fmt.Errorf("undefined code for TextscriptList121 has been passed, got [%s]", v)
	}
	return nil
}

// Type has not document
type Type string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *Type) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

  // 
  case "1":
		*c = ``

  // 
  case "A":
		*c = ``

  // 
  case "a":
		*c = ``

  // 
  case "I":
		*c = ``

  // 
  case "i":
		*c = ``
	default:
		return fmt.Errorf("undefined code for Type has been passed, got [%s]", v)
	}
	return nil
}

// Valign has not document
type Valign string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *Valign) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

  // 
  case "top":
		*c = ``

  // 
  case "middle":
		*c = ``

  // 
  case "bottom":
		*c = ``

  // 
  case "baseline":
		*c = ``
	default:
		return fmt.Errorf("undefined code for Valign has been passed, got [%s]", v)
	}
	return nil
}

// int has not document
type int string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *int) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {
	default:
		return fmt.Errorf("undefined code for int has been passed, got [%s]", v)
	}
}
