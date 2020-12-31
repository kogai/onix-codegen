package onix

import (
	"encoding/xml"
	"fmt"
)

// NotificationOrUpdateTypeCode List 1: Notification or update type code
type NotificationOrUpdateTypeCode string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *NotificationOrUpdateTypeCode) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// Use for a complete record issued earlier than approximately six months before publication
	case "01":
		*c = `Early notification`

	// Use for a complete record issued to confirm advance information approximately six months before publication; or for a complete record issued after that date and before information has been confirmed from the book-in-hand
	case "02":
		*c = `Advance notification (confirmed)`

	// Use for a complete record issued to confirm advance information at or just before actual publication date; or for a complete record issued at any later date
	case "03":
		*c = `Notification confirmed on publication`

	// In ONIX 3.0 only, use when sending a ‘block update’ record. In previous ONIX releases, ONIX updating has generally been by complete record replacement using code 03, and code 04 is not used
	case "04":
		*c = `Update (partial)`

	// Use when sending an instruction to delete a record which was previously issued. Note that a Delete instruction should NOT be used when a product is cancelled, put out of print, or otherwise withdrawn from sale: this should be handled as a change of Publishing status, leaving the receiver to decide whether to retain or delete the record. A Delete instruction is only used when there is a particular reason to withdraw a record completely, eg because it was issued in error
	case "05":
		*c = `Delete`

	// Notice of sale of a product, from one publisher to another: sent by the publisher disposing of the product
	case "08":
		*c = `Notice of sale`

	// Notice of acquisition of a product, by one publisher from another: sent by the acquiring publisher
	case "09":
		*c = `Notice of acquisition`

	// ONIX Books 2.1 supply update – <SupplyDetail> only (not used in ONIX 3.0)
	case "12":
		*c = `Update – SupplyDetail only`

	// ONIX Books 2.1 supply update – <MarketRepresentation> only (not used in ONIX 3.0)
	case "13":
		*c = `Update – MarketRepresentation only`

	// ONIX Books 2.1 supply update – both <SupplyDetail> and <MarketRepresentation> (not used in ONIX 3.0)
	case "14":
		*c = `Update – SupplyDetail and MarketRepresentation`

	// ONIX 3.0 only. Record may be processed for test purposes, but data should be discarded. Sender must ensure the <RecordReference> matches a previously-sent Test record
	case "88":
		*c = `Test update (Partial)`

	// Record may be processed for test purposes, but data should be discarded. Sender must ensure the <RecordReference> does not match any previously-sent live product record
	case "89":
		*c = `Test record`
	default:
		return fmt.Errorf("undefined code for NotificationOrUpdateTypeCode has been passed, got [%s]", v)
	}
	return nil
}

// EpublicationTypeCode List 10: Epublication type code
type EpublicationTypeCode string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *EpublicationTypeCode) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// An epublication viewed as a unique package of content which may be converted into any of a number of different types for delivery to the consumer. This code is used when an ONIX <Product> record describes the content package and lists within the record the different forms in which it is available
	case "000":
		*c = `Epublication ‘content package’`

	// An epublication delivered in a basic, unprotected, HTML format. Do NOT use for HTML-based formats which include DRM protection
	case "001":
		*c = `HTML`

	// An epublication delivered in a basic, unprotected, PDF format. Do NOT use for PDF-based formats which include DRM protection
	case "002":
		*c = `PDF`

	// An epublication delivered in PDF format, capable of being read in the standard Acrobat Reader, and protected by PDF-Merchant DRM features. (This format is no longer supported for new applications)
	case "003":
		*c = `PDF-Merchant`

	// An epublication delivered in an enhanced PDF format, using Adobe’s proprietary EBX DRM, capable of being read in the Adobe Ebook Reader software, on any platform which can support this software, which was formerly known as Glassbook
	case "004":
		*c = `Adobe Ebook Reader`

	// An epublication delivered in an unencrypted Microsoft .LIT format, capable of being read in the Microsoft Reader software at any level, on any platform which can support this software. (Level 3 differs from Level 1 only in that it embeds the name of the original purchaser)
	case "005":
		*c = `Microsoft Reader Level 1/Level 3`

	// An epublication delivered in the Microsoft .LIT format, with full encryption, capable of being read in the Microsoft Reader software at Level 5, on any platform which can support this software
	case "006":
		*c = `Microsoft Reader Level 5`

	// An epublication delivered in a proprietary HTML- or OEBF-based format, capable of being read only through subscription to the NetLibrary service
	case "007":
		*c = `NetLibrary`

	// An epublication delivered in a proprietary format through a web browser, capable of being read only through subscription to the MetaText service (the educational division of NetLibrary)
	case "008":
		*c = `MetaText`

	// An epublication delivered in a proprietary PDF-based format, capable of being read only through subscription to the MightyWords service
	case "009":
		*c = `MightyWords`

	// An epublication delivered in a proprietary HTML-based format, capable of being read in reading software which may be used on handheld devices using the Palm OS or Pocket PC/Windows CE operating systems
	case "010":
		*c = `eReader (AKA Palm Reader)`

	// An epublication delivered in a proprietary format capable of being read in reading software which is specific to the Softbook hardware platform. Also capable of being read on the Softbook’s successor, the Gemstar REB 1200
	case "011":
		*c = `Softbook`

	// An epublication delivered in a proprietary .RB format, capable of being read in reading software which is specific to the RocketBook hardware platform. Also capable of being read on the RocketBook’s successor, the Gemstar REB 1100
	case "012":
		*c = `RocketBook`

	// An epublication delivered in a proprietary .RB format, capable of being read in reading software which is specific to the Gemstar REB 1100 hardware platform. Also capable of being read on the RocketBook with some loss of functionality
	case "013":
		*c = `Gemstar REB 1100`

	// An epublication delivered in a proprietary format, capable of being read in reading software which is specific to the Gemstar REB 1200 hardware platform. Also capable of being read on the Softbook with some loss of functionality
	case "014":
		*c = `Gemstar REB 1200`

	// An epublication delivered in Franklin’s proprietary HTML-based format, capable of being read in reading software which is specific to the Franklin eBookman platform
	case "015":
		*c = `Franklin eBookman`

	// An epublication delivered in a proprietary XML-based format and available for online access only through subscription to the Books24x7 service
	case "016":
		*c = `Books24x7`

	// An epublication available through DigitalOwl proprietary packaging, distribution and DRM software, delivered in a variety of formats across a range of platforms
	case "017":
		*c = `DigitalOwl`

	// An epublication delivered in a proprietary HTML-based format, capable of being read in Handheldmed reader software on Palm OS, Windows, and EPOC/Psion handheld devices, available only through the Handheldmed service
	case "018":
		*c = `Handheldmed`

	// An epublication delivered in a proprietary ???-based format and available for download only through the WizeUp service
	case "019":
		*c = `WizeUp`

	// An epublication delivered in the proprietary TK3 format, capable of being read only in the TK3 reader software supplied by Night Kitchen Inc, on any platform which can support this software
	case "020":
		*c = `TK3`

	// An epublication delivered in an encrypted .RTF format, capable of being read only in the Litraweb Visor software, and available only from Litraweb.com
	case "021":
		*c = `Litraweb`

	// An epublication delivered in a proprietary format, capable of being read in the MobiPocket software on PalmOS, WindowsCE /Pocket PC, Franklin eBookman, and EPOC32 handheld devices, available through the MobiPocket service. Includes Amazon Kindle formats up to and including version 7 – but prefer code 031 for version 7, and always use 031 for KF8
	case "022":
		*c = `MobiPocket`

	// An epublication delivered in the standard distribution format specified in the Open Ebook Publication Structure (OEBPS) format and capable of being read in any OEBPS-compliant reading system. Includes EPUB format up to and including version 2 – but prefer code 029 for EPUB 2, and always use 029 for EPUB 3
	case "023":
		*c = `Open Ebook`

	// An epublication delivered in a proprietary format, capable of being read in Town Compass DataViewer reader software on a Palm OS handheld device
	case "024":
		*c = `Town Compass DataViewer`

	// An epublication delivered in an openly available .TXT format, with ASCII or UTF-8 encoding, as used for example in Project Gutenberg
	case "025":
		*c = `TXT`

	// An epublication delivered as a self-executing file including its own reader software, and created with proprietary ExeBook Self-Publisher software
	case "026":
		*c = `ExeBook`

	// An epublication in a Sony proprietary format for use with the Sony Reader and LIBRIé reading devices
	case "027":
		*c = `Sony BBeB`

	//
	case "028":
		*c = `VitalSource Bookshelf`

	// An epublication delivered using the Open Publication Structure / OPS Container Format standard of the International Digital Publishing Forum (IDPF). [This value was originally defined as ‘Adobe Digital Editions’, which is not an epublication format but a reader which supports PDF or EPUB (IDPF) formats. Since PDF is already covered by code 002, it was agreed, and announced to the ONIX listserv in September 2009, that code 029 should be refined to represent EPUB format]
	case "029":
		*c = `EPUB`

	//
	case "030":
		*c = `MyiLibrary`

	// Amazon proprietary file format derived from Mobipocket format, used for Kindle devices and Kindle reader software
	case "031":
		*c = `Kindle`

	// An epublication made available by Google in association with a publisher; readable online on a browser-enabled device and offline on designated ebook readers
	case "032":
		*c = `Google Edition`

	// An epublication in a proprietary format combining text and video content and available to be used online or as a downloadable application for a mobile device – see www.vook.com
	case "033":
		*c = `Vook`

	// An epublication in a proprietary format for use with DXReader software
	case "034":
		*c = `DXReader`

	// An epublication in a format proprietary to the Ebook Library service
	case "035":
		*c = `EBL`

	// An epublication in a format proprietary to the Ebrary service
	case "036":
		*c = `Ebrary`

	// An epublication in a proprietary format for use with iSilo software on various hardware platforms
	case "037":
		*c = `iSilo`

	// An epublication in a proprietary format for use with Plucker reader software on Palm and other handheld devices
	case "038":
		*c = `Plucker`

	// A format proprietary to the VitalSource service
	case "039":
		*c = `VitalBook`

	// Epublication packaged as an application for iOS (eg Apple iPhone, iPad etc) containing both executable code and content. Content can be described with <ProductContentType>
	case "040":
		*c = `Book ‘app’ for iOS`

	// Epublication packaged as an application for Android (eg Android phone or tablet) containing both executable code and content. Content can be described with <ProductContentType>
	case "041":
		*c = `Android ‘app’`

	// Epublication packaged as an application. Technical requirements such as target operating system and/or device should be provided in <EpubTypeNote>. Content can be described with <ProductContentType>
	case "042":
		*c = `Other ‘app’`

	// XML Paper Specification format [File extension .xps] for (eg) Blio
	case "043":
		*c = `XPS`

	// Apple’s iBook format (a proprietary extension of EPUB), can only be read on Apple iOS devices
	case "044":
		*c = `iBook`

	// Proprietary format based on EPUB used by Barnes and Noble for fixed-format e-books, readable on NOOK devices and Nook reader software
	case "045":
		*c = `ePIB`

	// Sharable Content Object Reference Model, standard content and packaging format for e-learning objects
	case "046":
		*c = `SCORM`

	// E-book Plus (proprietary Norwegian e-book format based on HTML5 documents packaged within a zipped .ebp file)
	case "047":
		*c = `EBP`

	// Proprietary format based on PDF used by Barnes and Noble for fixed-format e-books, readable on some NOOK devices and Nook reader software
	case "048":
		*c = `Page Perfect`

	// Product consists of parts in different formats
	case "098":
		*c = `Multiple formats`

	// Unknown, or no code yet assigned for this format
	case "099":
		*c = `Unspecified`
	default:
		return fmt.Errorf("undefined code for EpublicationTypeCode has been passed, got [%s]", v)
	}
	return nil
}

// DiscountCodeType List 100: Discount code type
type DiscountCodeType string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *DiscountCodeType) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// UK publisher’s or distributor’s discount group code in a format specified by BIC to ensure uniqueness
	case "01":
		*c = `BIC discount group code`

	// A publisher’s or supplier’s own code which identifies a trade discount category, as specified in <DiscountCodeTypeName>. The actual discount for each code is set by trading partner agreement (applies to goods supplied on standard trade discounting terms)
	case "02":
		*c = `Proprietary discount code`

	// Terms code used in the Netherlands book trade
	case "03":
		*c = `Boeksoort`

	// Terms code used in German ONIX applications
	case "04":
		*c = `German terms code`

	// A publisher’s or supplier’s own code which identifies a commission rate category, as specified in <DiscountCodeTypeName>. The actual commission rate for each code is set by trading partner agreement (applies to goods supplied on agency terms)
	case "05":
		*c = `Proprietary commission code`

	// UK publisher’s or distributor’s commission group code in format specified by BIC to ensure uniqueness. Format is identical to BIC discount group code, but indicates a commission rather than a discount (applies to goods supplied on agency terms)
	case "06":
		*c = `BIC commission group code`
	default:
		return fmt.Errorf("undefined code for DiscountCodeType has been passed, got [%s]", v)
	}
	return nil
}

// PersonNameIdentifierType List 101: Person name identifier type
type PersonNameIdentifierType string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *PersonNameIdentifierType) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// Note that <IDTypeName> is required with proprietary identifiers
	case "01":
		*c = `Proprietary`

	// Personennamendatei – person name authority file used by Deutsche Nationalbibliothek and in other German-speaking countries. See http://www.d-nb.de/standardisierung/normdateien/pnd.htm (German) or http://www.d-nb.de/eng/standardisierung/normdateien/pnd.htm (English). DEPRECATED in favour of the GND
	case "02":
		*c = `PND`

	// Library of Congress control number assigned to a Library of Congress Name Authority record
	case "04":
		*c = `LCCN`

	// International Standard Name Identifier. See http://www.isni.org/
	case "16":
		*c = `ISNI`

	// Gemeinsame Normdatei – Joint Authority File in the German-speaking countries. See http://www.dnb.de/EN/gnd (English). Combines the PND, SWD and GKD into a single authority file, and should be used in preference
	case "25":
		*c = `GND`
	default:
		return fmt.Errorf("undefined code for PersonNameIdentifierType has been passed, got [%s]", v)
	}
	return nil
}

// SalesOutletIdentifierType List 102: Sales outlet identifier type
type SalesOutletIdentifierType string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *SalesOutletIdentifierType) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// Proprietary list of retail and other end-user sales outlet IDs. Note that <IDTypeName> is required with proprietary identifiers
	case "01":
		*c = `Proprietary`

	// DEPRECATED – use code 03
	case "02":
		*c = `BIC sales outlet ID code`

	// Use with ONIX retail and other end-user sales outlet IDs from List 139
	case "03":
		*c = `ONIX retail sales outlet ID code`
	default:
		return fmt.Errorf("undefined code for SalesOutletIdentifierType has been passed, got [%s]", v)
	}
	return nil
}

// EpublicationFormatCode List 11: Epublication format code
type EpublicationFormatCode string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *EpublicationFormatCode) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	//
	case "01":
		*c = `HTML`

	//
	case "02":
		*c = `PDF`

	// ‘.LIT’ file format used by Microsoft Reader software
	case "03":
		*c = `Microsoft Reader`

	//
	case "04":
		*c = `RocketBook`

	//
	case "05":
		*c = `Rich text format (RTF)`

	//
	case "06":
		*c = `Open Ebook Publication Structure (OEBPS) format standard`

	//
	case "07":
		*c = `XML`

	//
	case "08":
		*c = `SGML`

	// ‘.EXE’ file format used when an epublication is delivered as a self-executing package of software and content
	case "09":
		*c = `EXE`

	// ‘.TXT’ file format
	case "10":
		*c = `ASCII`

	// Proprietary file format used for the MobiPocket reader software
	case "11":
		*c = `MobiPocket format`
	default:
		return fmt.Errorf("undefined code for EpublicationFormatCode has been passed, got [%s]", v)
	}
	return nil
}

// TradeCategoryCode List 12: Trade category code
type TradeCategoryCode string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *TradeCategoryCode) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// An edition from a UK publisher sold only in territories where exclusive rights are not held. Rights details should be carried in PR.21 (ONIX 2.1) OR P.21 (ONIX 3.0) as usual
	case "01":
		*c = `UK open market edition`

	// In UK, an edition intended primarily for airside sales in UK airports, though it may be available for sale in other territories where exclusive rights are not held. Rights details should be carried in PR.21 (ONIX 2.1) OR P.21 (ONIX 3.0) as usual
	case "02":
		*c = `Airport edition`

	// In Germany, a special printing sold at a lower price than the regular hardback
	case "03":
		*c = `Sonderausgabe`

	// In countries where recognised as a distinct trade category, eg France ‘livre de poche’, Germany ‘Taschenbuch’, Italy ‘tascabile’, Spain ‘libro de bolsillo’
	case "04":
		*c = `Pocket paperback`

	// Edition produced solely for sale in designated export markets
	case "05":
		*c = `International edition (US)`

	// Audio product sold in special durable packaging and with a replacement guarantee for the contained cassettes or CDs for a specified shelf-life
	case "06":
		*c = `Library audio edition`

	// An edition from a US publisher sold only in territories where exclusive rights are not held. Rights details should be carried in PR.21 (ONIX 2.1) OR P.21 (ONIX 3.0) as usual
	case "07":
		*c = `US open market edition`

	// In France, a category of book that has a particular legal status, claimed by the publisher
	case "08":
		*c = `Livre scolaire, déclaré par l’éditeur`

	// In France, a category of book that has a particular legal status, designated independently of the publisher
	case "09":
		*c = `Livre scolaire (non spécifié)`

	// Edition published for sale only with a newspaper or periodical
	case "10":
		*c = `Supplement to newspaper`

	// In Spain, a school textbook for which there is no fixed or suggested retail price and which is supplied by the publisher on terms individually agreed with the bookseller
	case "11":
		*c = `Precio libre textbook`

	// For editions sold only through newsstands/newsagents
	case "12":
		*c = `News outlet edition`

	// In the US and Canada, a book that is published primarily for use by students in school or college education as a basis for study. Textbooks published for the elementary and secondary school markets are generally purchased by school districts for the use of students. Textbooks published for the higher education market are generally adopted for use in particular classes by the instructors of those classes. Textbooks are usually not marketed to the general public, which distinguishes them from trade books. Note that trade books adopted for course use are not considered to be textbooks (though a specific education edition of a trade title may be)
	case "13":
		*c = `US textbook`

	// ‘Short’ e-book (sometimes also called a ‘single’), typically containing a single short story, an essay or piece of long-form journalism
	case "14":
		*c = `E-book short`
	default:
		return fmt.Errorf("undefined code for TradeCategoryCode has been passed, got [%s]", v)
	}
	return nil
}

// TextScriptCodeIso15924 List 121: Text script code – ISO 15924
type TextScriptCodeIso15924 string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *TextScriptCodeIso15924) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// Script is not supported by Unicode
	case "Afak":
		*c = `Afaka`

	//
	case "Arab":
		*c = `Arabic`

	//
	case "Armi":
		*c = `Imperial Aramaic`

	//
	case "Armn":
		*c = `Armenian`

	//
	case "Avst":
		*c = `Avestan`

	//
	case "Bali":
		*c = `Balinese`

	//
	case "Bamu":
		*c = `Bamun`

	// Script is not supported by Unicode
	case "Bass":
		*c = `Bassa Vah`

	//
	case "Batk":
		*c = `Batak`

	//
	case "Beng":
		*c = `Bengali`

	// Script is not supported by Unicode
	case "Blis":
		*c = `Blissymbols`

	//
	case "Bopo":
		*c = `Bopomofo`

	//
	case "Brah":
		*c = `Brahmi`

	//
	case "Brai":
		*c = `Braille`

	//
	case "Bugi":
		*c = `Buginese`

	//
	case "Buhd":
		*c = `Buhid`

	// Script is not supported by Unicode
	case "Cakm":
		*c = `Chakma`

	//
	case "Cans":
		*c = `Unified Canadian Aboriginal Syllabics`

	//
	case "Cari":
		*c = `Carian`

	//
	case "Cham":
		*c = `Cham`

	//
	case "Cher":
		*c = `Cherokee`

	// Script is not supported by Unicode
	case "Cirt":
		*c = `Cirth`

	//
	case "Copt":
		*c = `Coptic`

	//
	case "Cprt":
		*c = `Cypriot`

	//
	case "Cyrl":
		*c = `Cyrillic`

	// Script is not supported by Unicode
	case "Cyrs":
		*c = `Cyrillic (Old Church Slavonic variant)`

	//
	case "Deva":
		*c = `Devanagari (Nagari)`

	//
	case "Dsrt":
		*c = `Deseret (Mormon)`

	// Script is not supported by Unicode
	case "Dupl":
		*c = `Duployan shorthand, Duployan stenography`

	// Script is not supported by Unicode
	case "Egyd":
		*c = `Egyptian demotic`

	// Script is not supported by Unicode
	case "Egyh":
		*c = `Egyptian hieratic`

	//
	case "Egyp":
		*c = `Egyptian hieroglyphs`

	// Script is not supported by Unicode
	case "Elba":
		*c = `Elbasan`

	//
	case "Ethi":
		*c = `Ethiopic (Ge‘ez)`

	// Script is not supported by Unicode
	case "Geok":
		*c = `Khutsuri (Asomtavruli and Khutsuri)`

	//
	case "Geor":
		*c = `Georgian (Mkhedruli)`

	//
	case "Glag":
		*c = `Glagolitic`

	//
	case "Goth":
		*c = `Gothic`

	// Script is not supported by Unicode
	case "Gran":
		*c = `Grantha`

	//
	case "Grek":
		*c = `Greek`

	//
	case "Gujr":
		*c = `Gujarati`

	//
	case "Guru":
		*c = `Gurmukhi`

	//
	case "Hang":
		*c = `Hangul (Hangŭl, Hangeul)`

	//
	case "Hani":
		*c = `Han (Hanzi, Kanji, Hanja)`

	//
	case "Hano":
		*c = `Hanunoo (Hanunóo)`

	//
	case "Hans":
		*c = `Han (Simplified variant)`

	//
	case "Hant":
		*c = `Han (Traditional variant)`

	//
	case "Hebr":
		*c = `Hebrew`

	//
	case "Hira":
		*c = `Hiragana`

	// Script is not supported by Unicode
	case "Hmng":
		*c = `Pahawh Hmong`

	//
	case "Hrkt":
		*c = `Japanese syllabaries (alias for Hiragana + Katakana)`

	// Script is not supported by Unicode
	case "Hung":
		*c = `Old Hungarian`

	// Script is not supported by Unicode
	case "Inds":
		*c = `Indus (Harappan)`

	//
	case "Ital":
		*c = `Old Italic (Etruscan, Oscan, etc.)`

	//
	case "Java":
		*c = `Javanese`

	//
	case "Jpan":
		*c = `Japanese syllabaries (alias for Han + Hiragana + Katakana)`

	// Script is not supported by Unicode
	case "Jurc":
		*c = `Jurchen`

	//
	case "Kali":
		*c = `Kayah Li`

	//
	case "Kana":
		*c = `Katakana`

	//
	case "Khar":
		*c = `Kharoshthi`

	//
	case "Khmr":
		*c = `Khmer`

	//
	case "Khoj":
		*c = `Khojki`

	//
	case "Knda":
		*c = `Kannada`

	//
	case "Kore":
		*c = `Korean (alias for Hangul + Han)`

	// Script is not supported by Unicode
	case "Kpel":
		*c = `Kpelle`

	//
	case "Kthi":
		*c = `Kaithi`

	//
	case "Lana":
		*c = `Lanna, Tai Tham`

	//
	case "Laoo":
		*c = `Lao`

	// Script is not supported by Unicode
	case "Latf":
		*c = `Latin (Fraktur variant)`

	// Script is not supported by Unicode
	case "Latg":
		*c = `Latin (Gaelic variant)`

	//
	case "Latn":
		*c = `Latin`

	//
	case "Lepc":
		*c = `Lepcha (Róng)`

	//
	case "Limb":
		*c = `Limbu`

	// Script is not supported by Unicode
	case "Lina":
		*c = `Linear A`

	//
	case "Linb":
		*c = `Linear B`

	//
	case "Lisu":
		*c = `Lisu (Fraser)`

	// Script is not supported by Unicode
	case "Loma":
		*c = `Loma`

	//
	case "Lyci":
		*c = `Lycian`

	//
	case "Lydi":
		*c = `Lydian`

	//
	case "Mand":
		*c = `Mandaic, Mandaean`

	// Script is not supported by Unicode
	case "Mani":
		*c = `Manichaean`

	// Script is not supported by Unicode
	case "Maya":
		*c = `Mayan hieroglyphs`

	// Script is not supported by Unicode
	case "Mend":
		*c = `Mende`

	// Script is not supported by Unicode
	case "Merc":
		*c = `Meroitic Cursive`

	// Script is not supported by Unicode
	case "Mero":
		*c = `Meroitic Hieroglyphs`

	//
	case "Mlym":
		*c = `Malayalam`

	//
	case "Mong":
		*c = `Mongolian`

	// Script is not supported by Unicode
	case "Moon":
		*c = `Moon (Moon code, Moon script, Moon type)`

	// Script is not supported by Unicode
	case "Mroo":
		*c = `Mro, Mru`

	//
	case "Mtei":
		*c = `Meitei Mayek (Meithei, Meetei)`

	//
	case "Mymr":
		*c = `Myanmar (Burmese)`

	// Script is not supported by Unicode
	case "Narb":
		*c = `Old North Arabian (Ancient North Arabian)`

	// Script is not supported by Unicode
	case "Nbat":
		*c = `Nabatean`

	// Script is not supported by Unicode
	case "Nkgb":
		*c = `Nakhi Geba (’Na-’Khi ²Ggŏ-¹baw, Naxi Geba)`

	//
	case "Nkoo":
		*c = `N’Ko`

	// Script is not supported by Unicode
	case "Nshu":
		*c = `Nüshu`

	//
	case "Ogam":
		*c = `Ogham`

	//
	case "Olck":
		*c = `Ol Chiki (Ol Cemet’, Ol, Santali)`

	//
	case "Orkh":
		*c = `Old Turkic, Orkhon Runic`

	//
	case "Orya":
		*c = `Oriya`

	//
	case "Osma":
		*c = `Osmanya`

	// Script is not supported by Unicode
	case "Palm":
		*c = `Palmyrene`

	// Script is not supported by Unicode
	case "Perm":
		*c = `Old Permic`

	//
	case "Phag":
		*c = `Phags-pa`

	//
	case "Phli":
		*c = `Inscriptional Pahlavi`

	// Script is not supported by Unicode
	case "Phlp":
		*c = `Psalter Pahlavi`

	// Script is not supported by Unicode
	case "Phlv":
		*c = `Book Pahlavi`

	//
	case "Phnx":
		*c = `Phoenician`

	// Script is not supported by Unicode
	case "Plrd":
		*c = `Miao (Pollard)`

	//
	case "Prti":
		*c = `Inscriptional Parthian`

	//
	case "Qaaa":
		*c = `Reserved for private use (start)`

	//
	case "Qabx":
		*c = `Reserved for private use (end)`

	//
	case "Rjng":
		*c = `Rejang (Redjang, Kaganga)`

	// Script is not supported by Unicode
	case "Roro":
		*c = `Rongorongo`

	//
	case "Runr":
		*c = `Runic`

	//
	case "Samr":
		*c = `Samaritan`

	// Script is not supported by Unicode
	case "Sara":
		*c = `Sarati`

	//
	case "Sarb":
		*c = `Old South Arabian`

	//
	case "Saur":
		*c = `Saurashtra`

	// Script is not supported by Unicode
	case "Sgnw":
		*c = `SignWriting`

	//
	case "Shaw":
		*c = `Shavian (Shaw)`

	// Script is not supported by Unicode
	case "Shrd":
		*c = `Sharada, Śāradā`

	// Script is not supported by Unicode
	case "Sind":
		*c = `Khudawadi, Sindhi`

	//
	case "Sinh":
		*c = `Sinhala`

	// Script is not supported by Unicode
	case "Sora":
		*c = `Sora Sompeng`

	//
	case "Sund":
		*c = `Sundanese`

	//
	case "Sylo":
		*c = `Syloti Nagri`

	//
	case "Syrc":
		*c = `Syriac`

	//
	case "Syre":
		*c = `Syriac (Estrangelo variant)`

	//
	case "Syrj":
		*c = `Syriac (Western variant)`

	//
	case "Syrn":
		*c = `Syriac (Eastern variant)`

	//
	case "Tagb":
		*c = `Tagbanwa`

	// Script is not supported by Unicode
	case "Takr":
		*c = `Takri, Ṭākrī, Ṭāṅkrī`

	//
	case "Tale":
		*c = `Tai Le`

	//
	case "Talu":
		*c = `New Tai Lue`

	//
	case "Taml":
		*c = `Tamil`

	// Script is not supported by Unicode
	case "Tang":
		*c = `Tangut`

	//
	case "Tavt":
		*c = `Tai Viet`

	//
	case "Telu":
		*c = `Telugu`

	// Script is not supported by Unicode
	case "Teng":
		*c = `Tengwar`

	//
	case "Tfng":
		*c = `Tifinagh (Berber)`

	//
	case "Tglg":
		*c = `Tagalog (Baybayin, Alibata)`

	//
	case "Thaa":
		*c = `Thaana`

	//
	case "Thai":
		*c = `Thai`

	//
	case "Tibt":
		*c = `Tibetan`

	//
	case "Tirh":
		*c = `Tiruta`

	//
	case "Ugar":
		*c = `Ugaritic`

	//
	case "Vaii":
		*c = `Vai`

	// Script is not supported by Unicode
	case "Visp":
		*c = `Visible Speech`

	// Script is not supported by Unicode
	case "Wara":
		*c = `Warang Citi (Varang Kshiti)`

	// Script is not supported by Unicode
	case "Wole":
		*c = `Woleai`

	//
	case "Xpeo":
		*c = `Old Persian`

	//
	case "Xsux":
		*c = `Cuneiform, Sumero-Akkadian`

	//
	case "Yiii":
		*c = `Yi`

	//
	case "Zinh":
		*c = `Code for inherited script`

	// Script is not supported by Unicode
	case "Zmth":
		*c = `Mathematical notation`

	// Script is not supported by Unicode
	case "Zsym":
		*c = `Symbols`

	// Script is not supported by Unicode
	case "Zxxx":
		*c = `Code for unwritten documents`

	//
	case "Zyyy":
		*c = `Code for undetermined script`

	//
	case "Zzzz":
		*c = `Code for uncoded script`
	default:
		return fmt.Errorf("undefined code for TextScriptCodeIso15924 has been passed, got [%s]", v)
	}
	return nil
}

// SeriesIdentifierTypeCode List 13: Series identifier type code
type SeriesIdentifierTypeCode string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *SeriesIdentifierTypeCode) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// For example, publisher’s own series ID. Note that <IDTypeName> is required with proprietary identifiers
	case "01":
		*c = `Proprietary`

	// International Standard Serial Number, unhyphenated, 8 digits
	case "02":
		*c = `ISSN`

	// Maintained by the Deutsche Nationalbibliothek
	case "03":
		*c = `German National Bibliography series ID`

	// Maintained by VLB
	case "04":
		*c = `German Books in Print series ID`

	// Maintained by Electre Information, France
	case "05":
		*c = `Electre series ID`

	// Digital Object Identifier (variable length and character set)
	case "06":
		*c = `DOI`

	// Use only where the collection (series or set) is available as a single product
	case "15":
		*c = `ISBN-13`

	// Uniform Resource Name
	case "22":
		*c = `URN`

	// French National Bibliography series ID. Identifiant des publications en série maintenu par la Bibliothèque Nationale de France
	case "29":
		*c = `BNF Control number`

	// Archival Resource Key, as a URL (including the address of the ARK resolver provided by eg a national library)
	case "35":
		*c = `ARK`
	default:
		return fmt.Errorf("undefined code for SeriesIdentifierTypeCode has been passed, got [%s]", v)
	}
	return nil
}

// TransliterationSchemeCode List 138: Transliteration scheme code
type TransliterationSchemeCode string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *TransliterationSchemeCode) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// Transliteration of Cyrillic characters – Slavic languages
	case "SFS4900":
		*c = `Finnish standard SFS 4900`

	// Transliteration and transcription of Greek characters
	case "SFS5807":
		*c = `Finnish standard SFS 5807`

	// Transliteration of Arabic characters
	case "SFS5755":
		*c = `Finnish standard SFS 5755`

	// Transliteration of Hebrew characters
	case "SFS3602":
		*c = `Finnish standard SFS 5824`

	// Documentation – Romanization of Japanese (kana script)
	case "ISO3602":
		*c = `ISO 3602`

	// Information and documentation – Romanization of Chinese
	case "ISO7098":
		*c = `ISO 7098`
	default:
		return fmt.Errorf("undefined code for TransliterationSchemeCode has been passed, got [%s]", v)
	}
	return nil
}

// OnixRetailSalesOutletIDs List 139: ONIX retail sales outlet IDs
type OnixRetailSalesOutletIDs string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *OnixRetailSalesOutletIDs) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	//
	case "SYM":
		*c = `24Symbols`

	//
	case "ACM":
		*c = `A C Moore`

	//
	case "AAP":
		*c = `AandP`

	//
	case "ALB":
		*c = `Albertson’s`

	//
	case "AMZ":
		*c = `Amazon`

	//
	case "ANR":
		*c = `Angus and Robertson`

	//
	case "ANB":
		*c = `Anobii`

	//
	case "APA":
		*c = `Apabi`

	//
	case "APC":
		*c = `Apple`

	//
	case "ASD":
		*c = `Asda`

	//
	case "ASB":
		*c = `Asia Books`

	//
	case "AUD":
		*c = `Audible`

	//
	case "AUC":
		*c = `Audiobooks.com`

	//
	case "ATK":
		*c = `Audioteka`

	//
	case "BDL":
		*c = `B Dalton`

	//
	case "BNO":
		*c = `Barnes and Noble`

	//
	case "BBB":
		*c = `Bed Bath and Beyond`

	//
	case "BST":
		*c = `Best Buy`

	// Formerly 3M Cloud Library
	case "MMM":
		*c = `Bibliotheca`

	//
	case "BIL":
		*c = `Bilbary`

	//
	case "BJW":
		*c = `BJ’s Wholesale Club`

	//
	case "BLA":
		*c = `Blackstone Audion`

	//
	case "BLK":
		*c = `Blackwell’s`

	// Formerly GoSpoken/Mobcast
	case "GOS":
		*c = `Blinkbox`

	//
	case "BCA":
		*c = `Book Club Associates`

	//
	case "BSH":
		*c = `Bookish`

	//
	case "BKP":
		*c = `Bookpeople`

	//
	case "BKM":
		*c = `Books-A-Million`

	//
	case "BKS":
		*c = `Bookshout`

	//
	case "BTP":
		*c = `Booktopia`

	//
	case "BRD":
		*c = `Borders`

	//
	case "BRB":
		*c = `Borders/Books Etc`

	//
	case "BRT":
		*c = `British Bookshops`

	//
	case "CDL":
		*c = `Casa del Libro`

	//
	case "CEB":
		*c = `Ceebo`

	//
	case "CHD":
		*c = `Christianbook.com`

	//
	case "COP":
		*c = `Copia`

	//
	case "CST":
		*c = `Costco`

	//
	case "CRB":
		*c = `Crate and Barrel`

	//
	case "CVS":
		*c = `CVS Drug Stores`

	// www.cvs-mediatheques.com
	case "CVM":
		*c = `CVS Mediatheques`

	//
	case "CYB":
		*c = `Cyberlibris`

	//
	case "DEM":
		*c = `De Marque`

	//
	case "DSG":
		*c = `Dick’s Sporting Goods`

	//
	case "DIL":
		*c = `Dilicom`

	//
	case "DYM":
		*c = `Dymocks`

	//
	case "EPA":
		*c = `E-Pagine`

	//
	case "ERD":
		*c = `E-Reads`

	//
	case "ELC":
		*c = `Early Learning Centre`

	//
	case "ESN":
		*c = `Eason`

	//
	case "EBC":
		*c = `Ebooks Corp`

	//
	case "ECH":
		*c = `eChristian`

	//
	case "ECI":
		*c = `El Corte Inglés`

	//
	case "ELE":
		*c = `Electre`

	//
	case "ELB":
		*c = `Elib.se`

	//
	case "EMP":
		*c = `Empik`

	//
	case "ENH":
		*c = `English Heritage`

	// www.getepic.com
	case "EPC":
		*c = `Epic!`

	//
	case "FDB":
		*c = `FeedBooks`

	//
	case "FIN":
		*c = `Findaway`

	//
	case "FSP":
		*c = `Fishpond`

	//
	case "FLP":
		*c = `FlipKart`

	//
	case "FNC":
		*c = `Fnac`

	//
	case "FOL":
		*c = `Follett`

	//
	case "FRY":
		*c = `Fry’s Electronics`

	//
	case "GMS":
		*c = `Gamestop`

	//
	case "GLO":
		*c = `Glose`

	//
	case "GOO":
		*c = `Google Books`

	//
	case "HST":
		*c = `Hastings Entertainment`

	//
	case "HMV":
		*c = `HMV`

	//
	case "HMD":
		*c = `Home Depot`

	//
	case "IMM":
		*c = `Immatériel.fr`

	//
	case "IND":
		*c = `Indigo-Chapters`

	//
	case "IZN":
		*c = `Izneo`

	//
	case "JBH":
		*c = `JB Hifi`

	//
	case "JSM":
		*c = `John Smith and Son`

	//
	case "KMT":
		*c = `K-Mart`

	//
	case "KNB":
		*c = `KNFB/Blio`

	//
	case "KNO":
		*c = `Kno Inc`

	//
	case "KBO":
		*c = `Kobo`

	//
	case "KOO":
		*c = `Koorong`

	//
	case "KOR":
		*c = `Kortext`

	//
	case "KRG":
		*c = `Kroger`

	//
	case "LWE":
		*c = `Lowe’s`

	//
	case "MAC":
		*c = `Mackin`

	//
	case "MAG":
		*c = `Magzter`

	//
	case "MKS":
		*c = `Marks and Spencer`

	//
	case "MAT":
		*c = `Matras`

	// Also known as Media World
	case "MMS":
		*c = `Media Markt/Saturn`

	//
	case "ADP":
		*c = `Messageries ADP`

	//
	case "MCR":
		*c = `Microcenter`

	//
	case "MOF":
		*c = `Mofibo`

	//
	case "MRR":
		*c = `Morrisons`

	//
	case "MTC":
		*c = `Mothercare`

	//
	case "MYB":
		*c = `MyBoox`

	//
	case "MYI":
		*c = `MyiLibrary`

	//
	case "NTR":
		*c = `National Trust`

	//
	case "NUM":
		*c = `Numilog`

	//
	case "OFD":
		*c = `Office Depot`

	//
	case "OFM":
		*c = `Office Max`

	//
	case "OLF":
		*c = `OLF`

	//
	case "OVD":
		*c = `OverDrive`

	//
	case "OYS":
		*c = `Oyster`

	//
	case "PST":
		*c = `Past Times`

	//
	case "PTS":
		*c = `Pet Smart`

	//
	case "PTC":
		*c = `Petco`

	//
	case "PLY":
		*c = `Play.com`

	//
	case "PTB":
		*c = `Pottery Barn`

	//
	case "RDB":
		*c = `Readbooks`

	//
	case "RCL":
		*c = `ReadCloud`

	//
	case "RST":
		*c = `Restoration Hardware`

	//
	case "RET":
		*c = `Rethink`

	//
	case "RTZ":
		*c = `Ritz Camera`

	//
	case "RMB":
		*c = `RM Books`

	//
	case "SAF":
		*c = `Safari`

	//
	case "SFW":
		*c = `Safeway`

	//
	case "SNS":
		*c = `Sainsbury’s`

	//
	case "SCR":
		*c = `Scribd`

	//
	case "SLF":
		*c = `Selfridges`

	//
	case "SEQ":
		*c = `Sequencity`

	//
	case "SBT":
		*c = `Shanghai Book Traders`

	//
	case "SKB":
		*c = `Skoobe`

	//
	case "SMW":
		*c = `SmashWords`

	//
	case "SNY":
		*c = `Sony`

	//
	case "STP":
		*c = `Staples`

	//
	case "STT":
		*c = `Storytel`

	//
	case "TRG":
		*c = `Target`

	//
	case "TES":
		*c = `Tesco`

	//
	case "TEA":
		*c = `The Ebook Alternative`

	//
	case "TOL":
		*c = `Tolino`

	//
	case "TSR":
		*c = `Toys ‘R’ Us`

	//
	case "TSO":
		*c = `TSO (The Stationery Office)`

	//
	case "TXR":
		*c = `Txtr`

	//
	case "VRG":
		*c = `Virgin Megastores`

	//
	case "WHS":
		*c = `W H Smith`

	//
	case "WTR":
		*c = `Waitrose`

	//
	case "WLM":
		*c = `Wal-Mart`

	//
	case "WST":
		*c = `Waterstone’s`

	//
	case "WHT":
		*c = `Whitcoul’s`

	//
	case "WLS":
		*c = `Williams Sonoma`

	//
	case "WLW":
		*c = `Woolworths`

	// youboox.fr
	case "YOU":
		*c = `Youboox`

	// Formerly Virgin Megastores (UK)
	case "ZVV":
		*c = `Zavvi`

	// Include retailer name in <SalesOutletName>
	case "ZZZ":
		*c = `Other`
	default:
		return fmt.Errorf("undefined code for OnixRetailSalesOutletIDs has been passed, got [%s]", v)
	}
	return nil
}

// TextCaseFlag List 14: Text case flag
type TextCaseFlag string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *TextCaseFlag) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// Default
	case "00":
		*c = `Undefined`

	// Initial capitals on first word and subsequently on proper names only, eg ‘The conquest of Mexico’
	case "01":
		*c = `Sentence case`

	// Initial capitals on first word and subsequently on all significant words (nouns, pronouns, adjectives, verbs, adverbs, subordinate conjunctions) thereafter. Unless they appear as the first word, articles, prepositions and coordinating conjunctions remain lower case, eg ‘The Conquest of Mexico’
	case "02":
		*c = `Title case`

	// For example, ‘THE CONQUEST OF MEXICO’
	case "03":
		*c = `All capitals`
	default:
		return fmt.Errorf("undefined code for TextCaseFlag has been passed, got [%s]", v)
	}
	return nil
}

// UsCpsiaChokingHazardWarningCode List 140: US CPSIA choking hazard warning code
type UsCpsiaChokingHazardWarningCode string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *UsCpsiaChokingHazardWarningCode) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// List withdrawn – use List 143
	case "01":
		*c = `WARNING: CHOKING HAZARD – Small parts | Not for children under 3 yrs.`

	// List withdrawn – use List 143
	case "02":
		*c = `WARNING: CHOKING HAZARD – This toy is a small ball | Not for children under 3 yrs.`

	// List withdrawn – use List 143
	case "03":
		*c = `WARNING: CHOKING HAZARD – Toy contains a small ball | Not for children under 3 yrs.`

	// List withdrawn – use List 143
	case "04":
		*c = `WARNING: CHOKING HAZARD – Children under 8 yrs. can choke or suffocate on uninflated or broken balloons. Adult supervision
					required | Keep uninflated balloons from children. Discard broken balloons at once.`

	// List withdrawn – use List 143
	case "05":
		*c = `WARNING: CHOKING HAZARD – This toy is a marble | Not for children under 3 yrs.`

	// List withdrawn – use List 143
	case "06":
		*c = `WARNING: CHOKING HAZARD – Toy contains a marble | Not for children under 3 yrs.`

	// List withdrawn – use List 143
	case "07":
		*c = `No choking hazard warning necessary`
	default:
		return fmt.Errorf("undefined code for UsCpsiaChokingHazardWarningCode has been passed, got [%s]", v)
	}
	return nil
}

// BarcodeIndicator List 141: Barcode indicator
type BarcodeIndicator2 string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *BarcodeIndicator2) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	//
	case "00":
		*c = `Not barcoded`

	//
	case "01":
		*c = `Barcoded, scheme unspecified`

	//
	case "02":
		*c = `GTIN-13`

	//
	case "03":
		*c = `GTIN-13+5 (US dollar price encoded)`

	//
	case "04":
		*c = `GTIN-13+5 (CAN dollar price encoded)`

	//
	case "05":
		*c = `GTIN-13+5 (no price encoded)`

	// AKA item/price
	case "06":
		*c = `UPC-12 (item-specific)`

	// AKA item/price
	case "07":
		*c = `UPC-12+5 (item-specific)`

	// AKA price/item
	case "08":
		*c = `UPC-12 (price-point)`

	// AKA price/item
	case "09":
		*c = `UPC-12+5 (price-point)`
	default:
		return fmt.Errorf("undefined code for BarcodeIndicator has been passed, got [%s]", v)
	}
	return nil
}

// PositionOnProduct List 142: Position on product
type PositionOnProduct string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *PositionOnProduct) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// Position unknown or unspecified
	case "00":
		*c = `Unknown / unspecified`

	// The back cover of a book
	case "01":
		*c = `Cover 4`

	// The inside back cover of a book
	case "02":
		*c = `Cover 3`

	// The inside front cover of a book
	case "03":
		*c = `Cover 2`

	// The front cover of a book
	case "04":
		*c = `Cover 1`

	// The spine of a book
	case "05":
		*c = `On spine`

	// Used only for boxed products
	case "06":
		*c = `On box`

	// Used only for products fitted with hanging tags
	case "07":
		*c = `On tag`

	// Not be used for books unless they are contained within outer packaging
	case "08":
		*c = `On bottom`

	// Not be used for books unless they are contained within outer packaging
	case "09":
		*c = `On back`

	// Used only for products packaged in outer sleeves
	case "10":
		*c = `On outer sleeve / back`

	// Used only for products packaged in shrink-wrap or other removable wrapping
	case "11":
		*c = `On removable wrapping`
	default:
		return fmt.Errorf("undefined code for PositionOnProduct has been passed, got [%s]", v)
	}
	return nil
}

// UsCpsiaHazardWarningCode List 143: US CPSIA hazard warning code
type UsCpsiaHazardWarningCode string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *UsCpsiaHazardWarningCode) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// Required on applicable products sold in the US
	case "01":
		*c = `WARNING: CHOKING HAZARD – Small parts | Not for children under 3 yrs.`

	// Required on applicable products sold in the US
	case "02":
		*c = `WARNING: CHOKING HAZARD – Children under 8 yrs. can choke or suffocate on uninflated or broken balloons. Adult supervision
					required | Keep uninflated balloons from children. Discard broken balloons at once.`

	// Required on applicable products sold in the US
	case "03":
		*c = `WARNING: CHOKING HAZARD – This toy is a small ball | Not for children under 3 yrs.`

	// Required on applicable products sold in the US
	case "04":
		*c = `WARNING: CHOKING HAZARD – Toy contains a small ball | Not for children under 3 yrs.`

	// Required on applicable products sold in the US
	case "05":
		*c = `WARNING: CHOKING HAZARD – This toy is a marble | Not for children under 3 yrs.`

	// Required on applicable products sold in the US
	case "06":
		*c = `WARNING: CHOKING HAZARD – Toy contains a marble | Not for children under 3 yrs.`

	// To be used when a supplier wishes to make a clear statement that no such warning is applicable to product
	case "07":
		*c = `No choking hazard warning necessary`

	// Required on applicable products sold in the US. Should be accompanied by additional text: Swallowed magnets can stick together across intestines causing serious infections and death. Seek immediate medical attention if magnet(s) are swallowed or inhaled
	case "11":
		*c = `WARNING: MAGNET HAZARD – Product contains (a) small magnet(s)`
	default:
		return fmt.Errorf("undefined code for UsCpsiaHazardWarningCode has been passed, got [%s]", v)
	}
	return nil
}

// EPublicationTechnicalProtection List 144: E-publication technical protection
type EPublicationTechnicalProtection string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *EPublicationTechnicalProtection) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// Has no technical protection
	case "00":
		*c = `None`

	// Has DRM protection
	case "01":
		*c = `DRM`

	// Has digital watermarking
	case "02":
		*c = `Digital watermarking`

	// Has DRM protection applied by the Adobe CS4 Content Server Package or by the Adobe ADEPT hosted service
	case "03":
		*c = `Adobe DRM`

	// Has 'FairPlay’ DRM protection applied via Apple proprietary online store
	case "04":
		*c = `Apple DRM`

	// Has OMA v2 DRM protection applied, as used to protect some mobile phone content
	case "05":
		*c = `OMA DRM`

	// Has ‘Licensed Content Protection' DRM applied by a Readium License Server
	case "06":
		*c = `Readium LCP DRM`

	// Has Sony DADC User Rights Management (URMS) DRM protection applied
	case "07":
		*c = `Sony DRM`
	default:
		return fmt.Errorf("undefined code for EPublicationTechnicalProtection has been passed, got [%s]", v)
	}
	return nil
}

// UsageType List 145: Usage type
type UsageType string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *UsageType) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// Preview before purchase. Allows a retail customer, account holder or patron to view a proportion of the book before purchase. Also applies to borrowers making use of ‘acquisition on demand’ models in libraries, and to ‘subscription’ models where the purchase is made on behalf of the reader
	case "01":
		*c = `Preview`

	// Print paper copy of extract
	case "02":
		*c = `Print`

	// Make digital copy of extract
	case "03":
		*c = `Copy / paste`

	// Share product across multiple concurrent devices. Allows a retail customer, account holder or patron to read the book across multiple devices linked to the same account. Also applies to readers in library borrowing and ‘subscription’ models
	case "04":
		*c = `Share`

	// ‘Read aloud’ with text to speech functionality
	case "05":
		*c = `Text to speech`

	// Lendable by the purchaser to other device owner or account holder or patron, eg ‘Lend-to-a-friend’, library lending (where the library product has a separate <ProductIdentifier> from the consumer product). The ‘primary’ copy becomes unusable while the secondary copy is ‘on loan’ unless a number of concurrent borrowers is also specified
	case "06":
		*c = `Lend`

	// E-publication license is time limited. Use with 02 from List 146 and a time period in days, weeks or months in <EpubUsageLimit>. The purchased copy becomes unusable when the license expires
	case "07":
		*c = `Time-limited license`

	// Maximum number of consecutive loans (eg from a library) to a single device owner or account holder. Note that a limit of 1 indicates that a loan cannot be renewed
	case "08":
		*c = `Loan renewal`

	// E-publication license is multi-user. Maximum number of concurrent users licensed to use the product should be given in <EpubUsageLimit>
	case "09":
		*c = `Multi-user license`
	default:
		return fmt.Errorf("undefined code for UsageType has been passed, got [%s]", v)
	}
	return nil
}

// UsageStatus List 146: Usage status
type UsageStatus string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *UsageStatus) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	//
	case "01":
		*c = `Permitted unlimited`

	// Limit should be specified in <EpubUsageLimit>
	case "02":
		*c = `Permitted subject to limit`

	//
	case "03":
		*c = `Prohibited`
	default:
		return fmt.Errorf("undefined code for UsageStatus has been passed, got [%s]", v)
	}
	return nil
}

// UnitOfUsage List 147: Unit of usage
type UnitOfUsage string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *UnitOfUsage) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// Maximum number of copies that may be made of a permitted extract
	case "01":
		*c = `Copies`

	// Maximum number of characters in a permitted extract for a specified usage
	case "02":
		*c = `Characters`

	// Maximum number of words in a permitted extract for a specified usage
	case "03":
		*c = `Words`

	// Maximum number of pages in a permitted extract for a specified usage
	case "04":
		*c = `Pages`

	// Maximum percentage of total content in a permitted extract for a specified usage
	case "05":
		*c = `Percentage`

	// Maximum number of devices in ‘share group’
	case "06":
		*c = `Devices`

	// Maximum number of concurrent users. NB where the number of concurrent users is specifically not limited, set the number of concurrent users to zero
	case "07":
		*c = `Concurrent users`

	// Maximum percentage of total content which may be used in a specified usage per time period; the time period being specified as another EpubUsageQuantity
	case "08":
		*c = `Percentage per time period`

	// Maximum time period in days (beginning from product purchase or activation)
	case "09":
		*c = `Days`

	// Maximum time period in weeks
	case "13":
		*c = `Weeks`

	// Maximum time period in months
	case "14":
		*c = `Months`

	// Maximum time period in days (beginning from the product publication date). In effect, this defines a fixed end date for the license independent of the purchase or activation date
	case "27":
		*c = `Days (fixed start)`

	// Maximum time period in weeks
	case "28":
		*c = `Weeks (fixed start)`

	// Maximum time period in months
	case "29":
		*c = `Months (fixed start)`

	// Maximum number of times a specified usage event may occur (in the lifetime of the product)
	case "10":
		*c = `Times`

	// Maximum frequency a specified usage event may occur (per day)
	case "22":
		*c = `Times per day`

	// Maximum frequency a specified usage event may occur (per month)
	case "23":
		*c = `Times per month`

	// Maximum frequency a specified usage event may occur (per year)
	case "24":
		*c = `Times per year`

	// Maximum resolution of printed or copy/pasted extracts
	case "21":
		*c = `Dots per inch`

	// Maximum resolution of printed or copy/pasted extracts
	case "26":
		*c = `Dots per cm`

	// Page number where allowed usage begins. <Quantity> should contain an absolute page number, counting the cover as page 1. (This type of page numbering should not be used where the e-publication has no fixed pagination). Use with (max number of) Pages, Percentage of content, or End page to specify pages allowed in Preview
	case "11":
		*c = `Allowed usage start page`

	// Page number at which allowed usage ends. <Quantity> should contain an absolute page number, counting the cover as page 1. (This type of page numbering should not be used where the e-publication has no fixed pagination). Use with Start page to specify pages allowed in a preview
	case "12":
		*c = `Allowed usage end page`
	default:
		return fmt.Errorf("undefined code for UnitOfUsage has been passed, got [%s]", v)
	}
	return nil
}

// CollectionType List 148: Collection type
type CollectionType string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *CollectionType) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// Collection type is not determined
	case "00":
		*c = `Unspecified (default)`

	// The collection is a bibliographic collection (eg a series or set (Fr. série)) defined and identified by a publisher, either on the product itself or in product information supplied by the publisher. The books in the collection generally share a subject, narrative, design style or authorship. They may may have a specific order, or the collection may be unordered
	case "10":
		*c = `Publisher collection`

	// The collection is a bibliographic collection defined and identified by a publisher, either on the product itself or in product information supplied by the publisher, where the books in the collection have no specific order, shared subject, narrative, style or shared authorship, and are grouped by the publisher largely for marketing purposes. The collection has many of the characteristics of an imprint or marque. Used primarily in French book publishing, to distinguish between ‘série’ (using the normal code 10) and ‘collection’ (code 11), and where the collection éditoriale is not an imprint
	case "11":
		*c = `Collection éditoriale`

	// The collection has been defined and identified by a party in the metadata supply chain other than the publisher, typically an aggregator.
	case "20":
		*c = `Ascribed collection`
	default:
		return fmt.Errorf("undefined code for CollectionType has been passed, got [%s]", v)
	}
	return nil
}

// TitleElementLevel List 149: Title element level
type TitleElementLevel string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *TitleElementLevel) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// The title element refers to an individual product
	case "01":
		*c = `Product`

	// The title element refers to the top level of a bibliographic collection
	case "02":
		*c = `Collection level`

	// The title element refers to an intermediate level of a bibliographic collection that comprises two or more ‘sub-collections’
	case "03":
		*c = `Subcollection`

	// The title element refers to a content item within a product, eg a work included in a combined or ‘omnibus’ edition, or a chapter in a book
	case "04":
		*c = `Content item`

	// The title element names a master brand where the use of the brand spans multiple collections and product forms, and possibly multiple imprints and publishers. Used only for branded media properties carrying, for example, a children’s character brand
	case "05":
		*c = `Master brand`

	// The title element refers to an intermediate level of a bibliographic collection that is a subdivision of a sub-collection (a third level of collective identity)
	case "06":
		*c = `Sub-subcollection`
	default:
		return fmt.Errorf("undefined code for TitleElementLevel has been passed, got [%s]", v)
	}
	return nil
}

// TitleTypeCode List 15: Title type code
type TitleTypeCode string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *TitleTypeCode) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	//
	case "00":
		*c = `Undefined`

	// The full text of the distinctive title of the item, without abbreviation or abridgement. For books, where the title alone is not distinctive, elements may be taken from a set or series title and part number etc to create a distinctive title. Where the item is an omnibus edition containing two or more works by the same author, and there is no separate combined title, a distinctive title may be constructed by concatenating the individual titles, with suitable punctuation, as in ‘Pride and prejudice / Sense and sensibility / Northanger Abbey’
	case "01":
		*c = `Distinctive title (book); Cover title (serial); Title on item (serial content item or reviewed resource)`

	// Serials only
	case "02":
		*c = `ISSN key title of serial`

	// Where the subject of the ONIX record is a translated item
	case "03":
		*c = `Title in original language`

	// For serials: an acronym or initialism of Title Type 01, eg ‘JAMA’, ‘JACM’
	case "04":
		*c = `Title acronym or initialism`

	// An abbreviated form of Title Type 01
	case "05":
		*c = `Abbreviated title`

	// A translation of Title Type 01 into another language
	case "06":
		*c = `Title in other language`

	// Serials only: when a journal issue is explicitly devoted to a specified topic
	case "07":
		*c = `Thematic title of journal issue`

	// Books or serials: when an item was previously published under another title
	case "08":
		*c = `Former title`

	// For books: the title carried in a book distributor’s title file: frequently incomplete, and may include elements not properly part of the title
	case "10":
		*c = `Distributor’s title`

	// An alternative title that appears on the cover of a book
	case "11":
		*c = `Alternative title on cover`

	// An alternative title that appears on the back of a book
	case "12":
		*c = `Alternative title on back`

	// An expanded form of the title, eg the title of a school text book with grade and type and other details added to make the title meaningful, where otherwise it would comprise only the curriculum subject. This title type is required for submissions to the Spanish ISBN Agency
	case "13":
		*c = `Expanded title`

	// An alternative title that the book is widely known by, whether it appears on the book or not
	case "14":
		*c = `Alternative title`
	default:
		return fmt.Errorf("undefined code for TitleTypeCode has been passed, got [%s]", v)
	}
	return nil
}

// ProductForm List 150: Product form
type ProductForm string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *ProductForm) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	//
	case "00":
		*c = `Undefined`

	// Audio recording – detail unspecified
	case "AA":
		*c = `Audio`

	// Audio cassette (analogue)
	case "AB":
		*c = `Audio cassette`

	// Audio compact disc, in any recording format: use for ‘red book’ (conventional audio CD) and SACD, and use coding in Product Form Detail to specify the format, if required
	case "AC":
		*c = `CD-Audio`

	// Digital audio tape cassette
	case "AD":
		*c = `DAT`

	// Audio disc (excluding CD-Audio)
	case "AE":
		*c = `Audio disc`

	// Audio tape (analogue open reel tape)
	case "AF":
		*c = `Audio tape`

	// Sony MiniDisc format
	case "AG":
		*c = `MiniDisc`

	// Audio compact disc with part CD-ROM content, also termed CD-Plus or Enhanced-CD: use for ‘blue book’ and ‘yellow/red book’ two-session discs
	case "AH":
		*c = `CD-Extra`

	//
	case "AI":
		*c = `DVD Audio`

	// Audio recording downloadable online
	case "AJ":
		*c = `Downloadable audio file`

	// For example, Playaway audiobook and player: use coding in Product Form Detail to specify the recording format, if required
	case "AK":
		*c = `Pre-recorded digital audio player`

	// For example, Audiofy audiobook chip
	case "AL":
		*c = `Pre-recorded SD card`

	// Other audio format not specified by AB to AK
	case "AZ":
		*c = `Other audio format`

	// Book – detail unspecified
	case "BA":
		*c = `Book`

	// Hardback or cased book
	case "BB":
		*c = `Hardback`

	// Paperback or other softback book
	case "BC":
		*c = `Paperback / softback`

	// Loose-leaf book
	case "BD":
		*c = `Loose-leaf`

	// Spiral, comb or coil bound book
	case "BE":
		*c = `Spiral bound`

	// Pamphlet or brochure, stapled; German ‘geheftet’. Includes low-extent wire-stitched books bound without a distinct spine (eg many comic books)
	case "BF":
		*c = `Pamphlet`

	//
	case "BG":
		*c = `Leather / fine binding`

	// Child’s book with all pages printed on board
	case "BH":
		*c = `Board book`

	// Child’s book with all pages printed on textile
	case "BI":
		*c = `Rag book`

	// Child’s book printed on waterproof material
	case "BJ":
		*c = `Bath book`

	// A book whose novelty consists wholly or partly in a format which cannot be described by any other available code – a ‘conventional’ format code is always to be preferred; one or more Product Form Detail codes, eg from the B2nn group, should be used whenever possible to provide additional description
	case "BK":
		*c = `Novelty book`

	// Slide bound book
	case "BL":
		*c = `Slide bound`

	// Extra-large format for teaching etc; this format and terminology may be specifically UK; required as a top-level differentiator
	case "BM":
		*c = `Big book`

	// A part-work issued with its own ISBN and intended to be collected and bound into a complete book
	case "BN":
		*c = `Part-work (fascículo)`

	// Concertina-folded book or chart, designed to fold to pocket or regular page size: use for German ‘Leporello’
	case "BO":
		*c = `Fold-out book or chart`

	// A children’s book whose cover and pages are made of foam
	case "BP":
		*c = `Foam book`

	// Other book format or binding not specified by BB to BO
	case "BZ":
		*c = `Other book format`

	// Sheet map – detail unspecified
	case "CA":
		*c = `Sheet map`

	//
	case "CB":
		*c = `Sheet map, folded`

	//
	case "CC":
		*c = `Sheet map, flat`

	// See Code List 80 for ‘rolled in tube’
	case "CD":
		*c = `Sheet map, rolled`

	// Globe or planisphere
	case "CE":
		*c = `Globe`

	// Other cartographic format not specified by CB to CE
	case "CZ":
		*c = `Other cartographic`

	// Digital content delivered on a physical carrier (detail unspecified)
	case "DA":
		*c = `Digital (on physical carrier)`

	//
	case "DB":
		*c = `CD-ROM`

	// CD interactive: use for ‘green book’ discs
	case "DC":
		*c = `CD-I`

	//
	case "DE":
		*c = `Game cartridge`

	// AKA ‘floppy disc’
	case "DF":
		*c = `Diskette`

	//
	case "DI":
		*c = `DVD-ROM`

	//
	case "DJ":
		*c = `Secure Digital (SD) Memory Card`

	//
	case "DK":
		*c = `Compact Flash Memory Card`

	//
	case "DL":
		*c = `Memory Stick Memory Card`

	//
	case "DM":
		*c = `USB Flash Drive`

	// Double-sided disc, one side Audio CD/CD-ROM, other side DVD
	case "DN":
		*c = `Double-sided CD/DVD`

	// Other carrier of digital content not specified by DB to DN
	case "DZ":
		*c = `Other digital carrier`

	// Digital content delivered electronically (delivery method unspecified)
	case "EA":
		*c = `Digital (delivered electronically)`

	// Digital content available both by download and by online access
	case "EB":
		*c = `Digital download and online`

	// Digital content accessed online only
	case "EC":
		*c = `Digital online`

	// Digital content delivered by download only
	case "ED":
		*c = `Digital download`

	// Film or transparency – detail unspecified
	case "FA":
		*c = `Film or transparency`

	// Photographic transparencies mounted for projection
	case "FC":
		*c = `Slides`

	// Transparencies for overhead projector
	case "FD":
		*c = `OHP transparencies`

	//
	case "FE":
		*c = `Filmstrip`

	// Continuous movie film as opposed to filmstrip
	case "FF":
		*c = `Film`

	// Other film or transparency format not specified by FB to FF
	case "FZ":
		*c = `Other film or transparency format`

	// Digital product license (delivery method not encoded)
	case "LA":
		*c = `Digital product license`

	// Digital product license delivered through the retail supply chain as a physical ‘key’, typically a card or booklet containing a code enabling the purchaser to download the associated product
	case "LB":
		*c = `Digital product license key`

	// Digital product license delivered by email or other electronic distribution, typically providing a code enabling the purchaser to upgrade or extend the license supplied with the associated product
	case "LC":
		*c = `Digital product license code`

	// Microform – detail unspecified
	case "MA":
		*c = `Microform`

	//
	case "MB":
		*c = `Microfiche`

	// Roll microfilm
	case "MC":
		*c = `Microfilm`

	// Other microform not specified by MB or MC
	case "MZ":
		*c = `Other microform`

	// Miscellaneous printed material – detail unspecified
	case "PA":
		*c = `Miscellaneous print`

	// May use product form detail codes P201 to P204 to specify binding
	case "PB":
		*c = `Address book`

	//
	case "PC":
		*c = `Calendar`

	// Cards, flash cards (eg for teaching reading)
	case "PD":
		*c = `Cards`

	// Copymasters, photocopiable sheets
	case "PE":
		*c = `Copymasters`

	// May use product form detail codes P201 to P204 to specify binding
	case "PF":
		*c = `Diary`

	// Narrow strip-shaped printed sheet used mostly for education or children’s products (eg depicting alphabet, number line, procession of illustrated characters etc). Usually intended for horizontal display
	case "PG":
		*c = `Frieze`

	// Parts for post-purchase assembly
	case "PH":
		*c = `Kit`

	//
	case "PI":
		*c = `Sheet music`

	//
	case "PJ":
		*c = `Postcard book or pack`

	// Poster for retail sale – see also XF
	case "PK":
		*c = `Poster`

	// Record book (eg ‘birthday book’, ‘baby book’): binding unspecified; may use product form detail codes P201 to P204 to specify binding
	case "PL":
		*c = `Record book`

	// Wallet or folder (containing loose sheets etc): it is preferable to code the contents and treat ‘wallet’ as packaging (List 80), but if this is not possible the product as a whole may be coded as a ‘wallet’
	case "PM":
		*c = `Wallet or folder`

	//
	case "PN":
		*c = `Pictures or photographs`

	//
	case "PO":
		*c = `Wallchart`

	//
	case "PP":
		*c = `Stickers`

	// A book-sized (as opposed to poster-sized) sheet, usually in color or high quality print
	case "PQ":
		*c = `Plate (lámina)`

	// A book with all pages blank for the buyer’s own use; may use product form detail codes P201 to P204 to specify binding
	case "PR":
		*c = `Notebook / blank book`

	// May use product form detail codes P201 to P204 to specify binding
	case "PS":
		*c = `Organizer`

	//
	case "PT":
		*c = `Bookmark`

	// Other printed item not specified by PB to PQ
	case "PZ":
		*c = `Other printed item`

	// Presentation unspecified: format of product items must be given in <ProductPart>
	case "SA":
		*c = `Multiple-item retail product`

	// Format of product items must be given in <ProductPart>
	case "SB":
		*c = `Multiple-item retail product, boxed`

	// Format of product items must be given in <ProductPart>
	case "SC":
		*c = `Multiple-item retail product, slip-cased`

	// Format of product items must be given in <ProductPart>. Use code XL for a shrink-wrapped pack for trade supply, where the retail items it contains are intended for sale individually
	case "SD":
		*c = `Multiple-item retail product, shrinkwrapped`

	// Format of product items must be given in <ProductPart>
	case "SE":
		*c = `Multiple-item retail product, loose`

	// Multiple item product where subsidiary product part(s) is/are supplied as enclosures to the primary part, eg a book with a CD packaged in a sleeve glued within the back cover. Format of product items must be given in <ProductPart>
	case "SF":
		*c = `Multiple-item retail product, part(s) enclosed`

	// Video – detail unspecified
	case "VA":
		*c = `Video`

	// eg Laserdisc
	case "VF":
		*c = `Videodisc`

	// DVD video: specify TV standard in List 175
	case "VI":
		*c = `DVD video`

	// VHS videotape: specify TV standard in List 175
	case "VJ":
		*c = `VHS video`

	// Betamax videotape: specify TV standard in List 175
	case "VK":
		*c = `Betamax video`

	// VideoCD
	case "VL":
		*c = `VCD`

	// Super VideoCD
	case "VM":
		*c = `SVCD`

	// High definition DVD disc, Toshiba HD DVD format
	case "VN":
		*c = `HD DVD`

	// High definition DVD disc, Sony Blu-ray format
	case "VO":
		*c = `Blu-ray`

	// Sony Universal Media disc
	case "VP":
		*c = `UMD Video`

	// China Blue High-Definition, derivative of HD-DVD
	case "VQ":
		*c = `CBHD`

	// Other video format not specified by VB to VP
	case "VZ":
		*c = `Other video format`

	// Trade-only material (unspecified)
	case "XA":
		*c = `Trade-only material`

	//
	case "XB":
		*c = `Dumpbin – empty`

	// Dumpbin with contents. ISBN (where applicable) and format of contained items must be given in Product Part
	case "XC":
		*c = `Dumpbin – filled`

	//
	case "XD":
		*c = `Counterpack – empty`

	// Counterpack with contents. ISBN (where applicable) and format of contained items must be given in Product Part
	case "XE":
		*c = `Counterpack – filled`

	// Promotional poster for display, not for sale – see also PK
	case "XF":
		*c = `Poster, promotional`

	//
	case "XG":
		*c = `Shelf strip`

	// Promotional piece for shop window display
	case "XH":
		*c = `Window piece`

	//
	case "XI":
		*c = `Streamer`

	//
	case "XJ":
		*c = `Spinner`

	// Large scale facsimile of book for promotional display
	case "XK":
		*c = `Large book display`

	// A quantity pack with its own product code, for trade supply only: the retail items it contains are intended for sale individually. ISBN (where applicable) and format of contained items must be given in Product Part. For products or product bundles supplied individually shrink-wrapped for retail sale, use code SD
	case "XL":
		*c = `Shrink-wrapped pack`

	// A quantity pack with its own product code, for trade supply only: the retail items it contains are intended for sale individually. ISBN (where applicable) and format of contained items must be given in Product Part. For products or product bundles boxed individually for retail sale, use code SB
	case "XM":
		*c = `Boxed pack`

	// Other point of sale material not specified by XB to XL
	case "XZ":
		*c = `Other point of sale`

	// General merchandise – unspecified
	case "ZA":
		*c = `General merchandise`

	//
	case "ZB":
		*c = `Doll`

	// Soft or plush toy
	case "ZC":
		*c = `Soft toy`

	//
	case "ZD":
		*c = `Toy`

	// Board game, or other game (except computer game: see DE)
	case "ZE":
		*c = `Game`

	//
	case "ZF":
		*c = `T-shirt`

	// Dedicated e-book reading device, typically with mono screen
	case "ZG":
		*c = `E-book reader`

	// General purpose tablet computer, typically with color screen
	case "ZH":
		*c = `Tablet computer`

	// Dedicated audiobook player device, typically including book-related features like bookmarking
	case "ZI":
		*c = `Audiobook player`

	//
	case "ZJ":
		*c = `Jigsaw`

	//
	case "ZK":
		*c = `Mug`

	//
	case "ZL":
		*c = `Tote bag`

	// Other apparel items not specified by ZB to ZJ, including promotional or branded scarves, caps, aprons etc
	case "ZY":
		*c = `Other apparel`

	// Other merchandise not specified by ZB to ZY
	case "ZZ":
		*c = `Other merchandise`
	default:
		return fmt.Errorf("undefined code for ProductForm has been passed, got [%s]", v)
	}
	return nil
}

// ContributorPlaceRelator List 151: Contributor place relator
type ContributorPlaceRelator string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *ContributorPlaceRelator) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	//
	case "01":
		*c = `Born in`

	//
	case "02":
		*c = `Died in`

	//
	case "03":
		*c = `Formerly resided in`

	//
	case "04":
		*c = `Currently resides in`

	//
	case "05":
		*c = `Educated in`

	//
	case "06":
		*c = `Worked in`

	// (‘Floruit’)
	case "07":
		*c = `Flourished in`

	// Or nationality. For use with country codes only
	case "08":
		*c = `Citizen of`
	default:
		return fmt.Errorf("undefined code for ContributorPlaceRelator has been passed, got [%s]", v)
	}
	return nil
}

// IllustratedNotIllustrated List 152: Illustrated / not illustrated
type IllustratedNotIllustrated string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *IllustratedNotIllustrated) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// Not illustrated
	case "01":
		*c = `No`

	// Illustrated
	case "02":
		*c = `Yes`
	default:
		return fmt.Errorf("undefined code for IllustratedNotIllustrated has been passed, got [%s]", v)
	}
	return nil
}

// TextType List 153: Text type
type TextType string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *TextType) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// To be used only in circumstances where the parties to an exchange have agreed to include text which (a) is not for general distribution, and (b) cannot be coded elsewhere. If more than one type of text is sent, it must be identified by tagging within the text itself
	case "01":
		*c = `Sender-defined text`

	// Limited to a maximum of 350 characters
	case "02":
		*c = `Short description/annotation`

	// Length unrestricted
	case "03":
		*c = `Description`

	// Used for a table of contents sent as a single text field, which may or may not carry structure expressed as XHTML
	case "04":
		*c = `Table of contents`

	// Descriptive blurb taken from the back cover and/or flaps
	case "05":
		*c = `Flap / cover copy`

	// A quote taken from a review of the product or of the work in question where there is no need to take account of different editions
	case "06":
		*c = `Review quote`

	// A quote taken from a review of a previous edition of the work
	case "07":
		*c = `Review quote: previous edition`

	// A quote taken from a review of a previous work by the same author(s) or in the same series
	case "08":
		*c = `Review quote: previous work`

	// A quote usually provided by a celebrity or another author to promote a new book, not from a review
	case "09":
		*c = `Endorsement`

	// A promotional phrase which is intended to headline a description of the product
	case "10":
		*c = `Promotional headline`

	// Text describing a feature of a product to which the publisher wishes to draw attention for promotional purposes. Each separate feature should be described by a separate repeat, so that formatting can be applied at the discretion of the receiver of the ONIX record, or multiple features can be described using appropriate XHTML markup
	case "11":
		*c = `Feature`

	// A note referring to all contributors to a product – NOT linked to a single contributor
	case "12":
		*c = `Biographical note`

	// A statement included by a publisher in fulfillment of contractual obligations, such as a disclaimer, sponsor statement, or legal notice of any sort. Note that the inclusion of such a notice cannot and does not imply that a user of the ONIX record is obliged to reproduce it
	case "13":
		*c = `Publisher’s notice`

	// A short excerpt from the work
	case "14":
		*c = `Excerpt`

	// Used for an index sent as a single text field, which may be structured using XHTML
	case "15":
		*c = `Index`

	// (of which the product is a part.) Limited to a maximum of 350 characters
	case "16":
		*c = `Short description/annotation for collection`

	// (of which the product is a part.) Length unrestricted
	case "17":
		*c = `Description for collection`

	// As code 11 but used for a new feature of this edition or version
	case "18":
		*c = `New feature`

	//
	case "19":
		*c = `Version history`

	// Short summary statement of open access status and any related conditions (eg ‘Open access – no commercial use’), primarily for marketing purposes. Should always be accompanied by a link to the complete license (see <EpubLicense> or code 99 in List 158)
	case "20":
		*c = `Open access statement`

	// Short summary statement that the product is available only in digital formats (eg ‘Digital exclusive’). If a non-digital version is planned, <ContentDate> should be used to specify the date when exclusivity will end (use content date role code 15). If a non-digital version is available, the statement should not be included
	case "21":
		*c = `Digital exclusivity statement`

	// For example a recommendation or approval provided by a ministry of education or other official body. Use <Text> to provide details and ideally use <TextSourceCorporate> to name the approver
	case "22":
		*c = `Official recommendation`

	// Short description in format specified by Japanese Book Publishers Association
	case "23":
		*c = `JBPA description`

	// JSON-LD snippet suitable for use within an HTML <script type="application/ld+json"> tag, containing structured metadata suitable for use with schema.org
	case "24":
		*c = `schema.org snippet`
	default:
		return fmt.Errorf("undefined code for TextType has been passed, got [%s]", v)
	}
	return nil
}

// ContentAudience List 154: Content audience
type ContentAudience string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *ContentAudience) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// Any audience
	case "00":
		*c = `Unrestricted`

	// Distribution by agreement between the parties to the ONIX exchange (this value is provided to cover applications where ONIX content includes material which is not for general distribution)
	case "01":
		*c = `Restricted`

	// Distributors, bookstores, publisher’s own staff etc
	case "02":
		*c = `Booktrade`

	//
	case "03":
		*c = `End-customers`

	//
	case "04":
		*c = `Librarians`

	//
	case "05":
		*c = `Teachers`

	//
	case "06":
		*c = `Students`

	// Press or other media
	case "07":
		*c = `Press`

	// Where a specially formatted description is required for this audience
	case "08":
		*c = `Shopping comparison service`

	// Text not intended for display, but may be used (in addition to any less restricted text) for indexing and search
	case "09":
		*c = `Search engine index`
	default:
		return fmt.Errorf("undefined code for ContentAudience has been passed, got [%s]", v)
	}
	return nil
}

// ContentDateRole List 155: Content date role
type ContentDateRole string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *ContentDateRole) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// Nominal date of publication (of the content item or supporting resource)
	case "01":
		*c = `Publication date`

	// Date when a TV or radio program was / will be broadcast
	case "04":
		*c = `Broadcast date`

	// Date from which a content item or supporting resource may be referenced or used. The content is embargoed until this date
	case "14":
		*c = `From date`

	// Date until which a content item or supporting resource may be referenced or used
	case "15":
		*c = `Until date`

	// Date when a resource was last changed or updated
	case "17":
		*c = `Last updated`

	// Combines From date and Until date to define a period (both dates are inclusive). Use with for example dateformat 06
	case "24":
		*c = `From… until date`

	// Date from which a supporting resource is available for download. Note that this date also implies that it can be immediately displayed to the intended audience, unless a From date (code 14) is also supplied and is later than the Available from date
	case "27":
		*c = `Available from`

	// Date until which a supporting resource is available for download. Note that this date does not imply it must be removed from display to the intended audience on this date – for this, use Until date (code 15)
	case "28":
		*c = `Available until`
	default:
		return fmt.Errorf("undefined code for ContentDateRole has been passed, got [%s]", v)
	}
	return nil
}

// CitedContentType List 156: Cited content type
type CitedContentType string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *CitedContentType) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// The full text of a review in a third-party publication in any medium
	case "01":
		*c = `Review`

	//
	case "02":
		*c = `Bestseller list`

	// Other than a review
	case "03":
		*c = `Media mention`

	// (North America) Inclusion in a program such as ‘Chicago Reads’, ‘Seattle Reads’
	case "04":
		*c = `‘One locality, one book’ program`
	default:
		return fmt.Errorf("undefined code for CitedContentType has been passed, got [%s]", v)
	}
	return nil
}

// ContentSourceType List 157: Content source type
type ContentSourceType string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *ContentSourceType) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	//
	case "01":
		*c = `Printed media`

	//
	case "02":
		*c = `Website`

	//
	case "03":
		*c = `Radio`

	//
	case "04":
		*c = `TV`
	default:
		return fmt.Errorf("undefined code for ContentSourceType has been passed, got [%s]", v)
	}
	return nil
}

// ResourceContentType List 158: Resource content type
type ResourceContentType string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *ResourceContentType) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// 2D
	case "01":
		*c = `Front cover`

	// 2D
	case "02":
		*c = `Back cover`

	// Not limited to front or back, including 3D perspective
	case "03":
		*c = `Cover / pack`

	// Photograph or portrait of contributor(s)
	case "04":
		*c = `Contributor picture`

	//
	case "05":
		*c = `Series image / artwork`

	//
	case "06":
		*c = `Series logo`

	//
	case "07":
		*c = `Product image / artwork`

	//
	case "08":
		*c = `Product logo`

	//
	case "09":
		*c = `Publisher logo`

	//
	case "10":
		*c = `Imprint logo`

	//
	case "11":
		*c = `Contributor interview`

	// Contributor presentation and/or commentary
	case "12":
		*c = `Contributor presentation`

	//
	case "13":
		*c = `Contributor reading`

	// Link to a schedule in iCalendar format
	case "14":
		*c = `Contributor event schedule`

	// For example: sample chapter text, page images, screenshots
	case "15":
		*c = `Sample content`

	// A ‘look inside’ feature presented as a small embeddable application
	case "16":
		*c = `Widget`

	// Review text held in a separate downloadable file, not in the ONIX record. Equivalent of code 06 in List 153. Use the <TextContent> composite for review quotes carried in the ONIX record. Use the <CitedContent> composite for a third-party review which is referenced from the ONIX record. Use <SupportingResource> for review text offered as a separate file resource for reproduction as part of promotional material for the product
	case "17":
		*c = `Review`

	//
	case "18":
		*c = `Other commentary / discussion`

	//
	case "19":
		*c = `Reading group guide`

	// Incuding associated teacher / instructor resources
	case "20":
		*c = `Teacher’s guide`

	// Feature article provided by publisher
	case "21":
		*c = `Feature article`

	// Fictional character ‘interview’
	case "22":
		*c = `Character ‘interview’`

	//
	case "23":
		*c = `Wallpaper / screensaver`

	//
	case "24":
		*c = `Press release`

	// A table of contents held in a separate downloadable file, not in the ONIX record. Equivalent of code 04 in List 153. Use the <TextContent> composite for a table of contents carried in the ONIX record. Use <Supporting Resource> for text offered as a separate file resource
	case "25":
		*c = `Table of contents`

	// A promotional video (or audio), similar to a movie trailer (sometimes referred to as a ‘book trailer’)
	case "26":
		*c = `Trailer`

	// Intended ONLY for transitional use, where ONIX 2.1 records referencing existing thumbnail assets of unknown pixel size are being re-expressed in ONIX 3.0. Use code 01 for all new cover assets, and where the pixel size of older assets is known
	case "27":
		*c = `Cover thumbnail`

	// The full content of the product (or the product itself), supplied for example to support full-text search or indexing
	case "28":
		*c = `Full content`

	// Includes cover, back cover, spine and – where appropriate – any flaps
	case "29":
		*c = `Full cover`

	//
	case "30":
		*c = `Master brand logo`

	// Descriptive text in a separate downloadable file, not in the ONIX record. Equivalent of code 03 in List 153. Use the <TextContent> composite for descriptions carried in the ONIX record. Use <Supporting Resource> for text offered as a separate file resource for reproduction as part of promotional material for the product
	case "31":
		*c = `Description`

	// Index text held in a separate downloadable file, not in the ONIX record. Equivalent of code 15 in List 153. Use the <TextContent> composite for index text carried in the ONIX record. Use <Supporting Resource> for an index offered as a separate file resource
	case "32":
		*c = `Index`

	// Including associated student / learner resources
	case "33":
		*c = `Student’s guide`

	// For example a PDF or other digital representation of a publisher’s ‘new titles’ or range catalogue
	case "34":
		*c = `Publisher’s catalogue`

	// For example a banner ad for the product. Pixel dimensions should typically be included in <ResourceVersionFeature>
	case "35":
		*c = `Online advertisement panel`

	// German ‘Búhnenbild’
	case "36":
		*c = `Online advertisement page`

	// For example, posters, logos, banners, advertising templates for use in connection with a promotional event
	case "37":
		*c = `Promotional event material`

	// Availability of a digital review or digital proof copy, may be limited to authorised users or account holders
	case "38":
		*c = `Digital review copy`

	// For example, video showing how to use the product
	case "39":
		*c = `Instructional material`

	// Link to a license covering permitted usage of the product content. Deprecated in favor of <EpubLicense>. This was a temporary workaround in ONIX 3.0, and use of <EpubLicense> is strongly preferred
	case "99":
		*c = `License`
	default:
		return fmt.Errorf("undefined code for ResourceContentType has been passed, got [%s]", v)
	}
	return nil
}

// ResourceMode List 159: Resource mode
type ResourceMode string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *ResourceMode) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// An executable together with data on which it operates
	case "01":
		*c = `Application`

	// A sound recording
	case "02":
		*c = `Audio`

	// A still image
	case "03":
		*c = `Image`

	// Readable text, with or without associated images etc
	case "04":
		*c = `Text`

	// Moving images, with or without accompanying sound
	case "05":
		*c = `Video`

	// A website or other supporting resource delivering content in a variety of modes
	case "06":
		*c = `Multi-mode`
	default:
		return fmt.Errorf("undefined code for ResourceMode has been passed, got [%s]", v)
	}
	return nil
}

// WorkIdentifierTypeCode List 16: Work identifier type code
type WorkIdentifierTypeCode string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *WorkIdentifierTypeCode) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// Note that <IDTypeName> is required with proprietary identifiers
	case "01":
		*c = `Proprietary`

	// 10-character ISBN of manifestation of work, when this is the only work identifier available – now DEPRECATED in ONIX for Books, except where providing historical information for compatibility with legacy systems. It should only be used in relation to products published before 2007 – when ISBN-13 superseded it – and should never be used as the ONLY identifier (it should always be accompanied by the correct GTIN-13 / ISBN-13 of the manifestation of the work)
	case "02":
		*c = `ISBN-10`

	// Digital Object Identifier (variable length and character set)
	case "06":
		*c = `DOI`

	// International Standard Text Code (16 characters: numerals and letters A-F, unhyphenated)
	case "11":
		*c = `ISTC`

	// 13-character ISBN of manifestation of work, when this is the only work identifier available
	case "15":
		*c = `ISBN-13`

	// International Standard Recording Code
	case "18":
		*c = `ISRC`

	// Global Library Manifestation Identifier, OCLC’s ‘manifestation cluster’ ID
	case "32":
		*c = `GLIMIR`

	// OCLC Work Identifier
	case "33":
		*c = `OWI`
	default:
		return fmt.Errorf("undefined code for WorkIdentifierTypeCode has been passed, got [%s]", v)
	}
	return nil
}

// ResourceFeatureType List 160: Resource feature type
type ResourceFeatureType string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *ResourceFeatureType) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// Credit that must be displayed when a resource is used (eg ‘Photo Jerry Bauer’ or ‘© Magnum Photo’). Credit text should be carried in <FeatureNote>
	case "01":
		*c = `Required credit`

	// Explanatory caption that may accompany a resource (eg use to identify an author in a photograph). Caption text should be carried in <FeatureNote>
	case "02":
		*c = `Caption`

	// Copyright holder of resource (indicative only, as the resource can be used without consultation). Copyright text should be carried in <FeatureNote>
	case "03":
		*c = `Copyright holder`

	// Approximate length in minutes of an audio or video resource. <FeatureValue> should contain the length of time as an integer number of minutes
	case "04":
		*c = `Length in minutes`

	// Use to link resource to a contributor unambiguously, for example with Resource Content types 04, 11–14 from List 158, particularly where the product has more than a single contributor. <FeatureValue> contains the 16-digit ISNI
	case "05":
		*c = `ISNI of resource contributor`

	// Use to link resource to a contributor unambiguously, for example with Resource Content types 04, 11–14 from List 158, particularly where the product has more than a single contributor. <FeatureValue> contains the proprietary ID, which must match a proprietary ID given in an instance of <Contributor>
	case "06":
		*c = `Proprietary ID of resource contributor`
	default:
		return fmt.Errorf("undefined code for ResourceFeatureType has been passed, got [%s]", v)
	}
	return nil
}

// ResourceForm List 161: Resource form
type ResourceForm string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *ResourceForm) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// A resource that may be accessed by a hyperlink. The current host (eg the ONIX sender, who may be the publisher) will provide ongoing hosting services for the resource for the active life of the product (or at least until the Until Date specified in <ContentDate>). The ONIX recipient may embed the URL in a consumer facing-website (eg as the src attribute in an <img> link), and need not host an independent copy of the resource
	case "01":
		*c = `Linkable resource`

	// A file that may be downloaded on demand for third-party use. The ONIX sender will host a copy of the resource until the specified Until Date, but only for the ONIX recipient’s direct use. The ONIX recipient should download a copy of the resource, and must host an independent copy of the resource if it is used on a consumer-facing website. Special attention should be paid to the ‘Last Updated’ <ContentDate> to ensure the independent copy of the resource is kept up to date
	case "02":
		*c = `Downloadable file`

	// An application which is supplied in a form which can be embedded into a third-party webpage. As type 02, except the resource contains active content such as JavaScript, Flash, etc
	case "03":
		*c = `Embeddable application`
	default:
		return fmt.Errorf("undefined code for ResourceForm has been passed, got [%s]", v)
	}
	return nil
}

// ResourceVersionFeatureType List 162: Resource version feature type
type ResourceVersionFeatureType string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *ResourceVersionFeatureType) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// Resource Version Feature Value carries a code from List 178
	case "01":
		*c = `File format`

	// Resource Version Feature Value carries an integer
	case "02":
		*c = `Image height in pixels`

	// Resource Version Feature Value carries an integer
	case "03":
		*c = `Image width in pixels`

	// Resource Version Feature Value carries the filename of the supporting resource
	case "04":
		*c = `Filename`

	// Resource Version Feature Value carries a decimal number only, suggested no more than 2 significant digits (eg 1.7, not 1.7462)
	case "05":
		*c = `Approximate download file size in megabytes`

	// MD5 hash value of the resource file. <ResourceVersionFeatureValue> should contain the 128-bit digest value (as 32 hexadecimal digits). Can be used as a cryptographic check on the integrity of a resource after it has been retrieved
	case "06":
		*c = `MD5 hash value`

	// Resource Version Feature Value carries a integer number only (eg 1831023)
	case "07":
		*c = `Exact download file size in bytes`

	// SHA-256 hash value of the resource file. <ResourceVersionFeatureValue> should contain the 256-bit digest value (as 64 hexadecimal digits). Can be used as a cryptographic check on the integrity of a resource after it has been retrieved
	case "08":
		*c = `SHA-256 hash value`
	default:
		return fmt.Errorf("undefined code for ResourceVersionFeatureType has been passed, got [%s]", v)
	}
	return nil
}

// PublishingDateRole List 163: Publishing date role
type PublishingDateRole string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *PublishingDateRole) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// Nominal date of publication
	case "01":
		*c = `Publication date`

	// If there is an embargo on retail sales in this market before a certain date, the date from which the embargo is lifted and retail sales are permitted
	case "02":
		*c = `Embargo date`

	// Date when a new product may be announced to the general public
	case "09":
		*c = `Public announcement date`

	// Date when a new product may be announced for trade only
	case "10":
		*c = `Trade announcement date`

	// Date when the work incorporated in a product was first published
	case "11":
		*c = `Date of first publication`

	// Date when a product was last reprinted
	case "12":
		*c = `Last reprint date`

	// Date when a product was (or will be) declared out-of-print or deleted
	case "13":
		*c = `Out-of-print / deletion date`

	// Date when a product was last reissued
	case "16":
		*c = `Last reissue date`

	// Date of publication of a printed book which is the print counterpart to a digital edition
	case "19":
		*c = `Publication date of print counterpart`

	// Year when the original language version of work incorporated in a product was first published (note, use only when different from code 11)
	case "20":
		*c = `Date of first publication in original language`

	// Date when a product will be reissued
	case "21":
		*c = `Forthcoming reissue date`

	// Date when a product that has been temporary withdrawn from sale or recalled for any reason is expected to become available again, eg after correction of quality or technical issues
	case "22":
		*c = `Expected availability date after temporary withdrawal`

	// Date from which reviews of a product may be published eg in newspapers and magazines or online. Provided to the book trade for information only: newspapers and magazines are not expected to be recipients of ONIX metadata
	case "23":
		*c = `Review embargo date`

	// Latest date on which an order may be placed with the publisher for guaranteed delivery prior to the publication date. May or may not be linked to a special reservation or pre-publication price
	case "25":
		*c = `Publisher’s reservation order deadline`

	// Date when a product will be reprinted
	case "26":
		*c = `Forthcoming reprint date`

	// Earliest date a retail ‘preorder’ can be placed (where this is distinct from the public announcement date). In the absence of a preorder embargo, advance orders can be placed as soon as metadata is available to the consumer (this would be the public announcement date, or in the absence of a public announcement date, the earliest date metadata is available to the retailer)
	case "27":
		*c = `Preorder embargo date`

	// Date of acquisition of product by new publisher (use with publishing roles 09 and 13)
	case "28":
		*c = `Transfer date`

	// For an audiovisual work (eg on DVD)
	case "29":
		*c = `Date of production`
	default:
		return fmt.Errorf("undefined code for PublishingDateRole has been passed, got [%s]", v)
	}
	return nil
}

// WorkRelationCode List 164: Work relation code
type WorkRelationCode string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *WorkRelationCode) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// Product X is or includes a manifestation of work Y
	case "01":
		*c = `Manifestation of`

	// Product X is or includes a manifestation of a work derived (directly) from related work Y in one or more of the ways specified in ISTC rules. This relation type is intended to enable products with a common ‘parent’ work to be linked without specifying the precise nature of their derivation
	case "02":
		*c = `Derived from`

	// Product X is a manifestation of a work from which related work Y is (directly) derived in one or more of the ways specified in ISTC rules (reciprocal of code 02)
	case "03":
		*c = `Related work is derived from this`

	// Product X is a manifestation of a work in the same collection as related work Y
	case "04":
		*c = `Other work in same collection`

	// Product X is a manifestation of a work by the same contributor(s) as related work Y
	case "05":
		*c = `Other work by same contributor`
	default:
		return fmt.Errorf("undefined code for WorkRelationCode has been passed, got [%s]", v)
	}
	return nil
}

// SupplierOwnCodeType List 165: Supplier own code type
type SupplierOwnCodeType string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *SupplierOwnCodeType) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// A rating applied by a supplier (typically a wholesaler) to indicate its assessment of the expected or actual sales performance of a product
	case "01":
		*c = `Supplier’s sales classification`

	// A supplier’s coding of the eligibility of a product for a bonus scheme on overall sales
	case "02":
		*c = `Supplier’s bonus eligibility`

	// A rating applied by the publisher to indicate a sales category (eg backlist/frontlist, core stock etc). Use only when the publisher is not the ‘supplier’
	case "03":
		*c = `Publisher’s sales classification`

	// A classification applied by a supplier to a product sold on Agency terms, to indicate that retail price restrictions are applicable
	case "04":
		*c = `Supplier’s pricing restriction classification`

	// Code is the ISBN of another book that had sales (both in terms of copy numbers and customer profile) comparable to that the distributor or supplier estimates for the product. <SupplierCodeValue> must be an ISBN-13 or GTIN-13
	case "05":
		*c = `Supplier’s sales expectation`

	// Code is the ISBN of another book that had sales (both in terms of copy numbers and customer profile) comparable to that the publisher estimates for the product. <SupplierCodeValue> must be an ISBN-13 or GTIN-13
	case "06":
		*c = `Publisher’s sales expectation`
	default:
		return fmt.Errorf("undefined code for SupplierOwnCodeType has been passed, got [%s]", v)
	}
	return nil
}

// SupplyDateRole List 166: Supply date role
type SupplyDateRole string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *SupplyDateRole) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// If there is an embargo on retail sales before a certain date, the date from which the embargo is lifted and retail sales are permitted
	case "02":
		*c = `Embargo date`

	// The date on which physical stock is expected to be available to be shipped to retailers, or a digital product is expected to be released by the publisher or digital asset distributor to retailers or their retail platform providers
	case "08":
		*c = `Expected availability date`

	// Last date when returns will be accepted, generally for a product which is being remaindered or put out of print
	case "18":
		*c = `Last date for returns`

	// Latest date on which an order may be placed for guaranteed delivery prior to the publication date. May or may not be linked to a special reservation or pre-publication price
	case "25":
		*c = `Reservation order deadline`

	// First date on which the supplier specified in <NewSupplier> will accept orders. Note the first date would typically be the day after the old supplier end date, but they may overlap if there is an agreement to forward any orders between old and new supplier for fulfillment
	case "50":
		*c = `New supplier start date`

	// Last date on which the supplier specified in <Supplier> will accept orders. New supplier should be specified where available. Note last date would typically be the day before the new supplier start date, but they may overlap if there is an agreement to forward any orders between old and new supplier for fulfillment
	case "51":
		*c = `Supplier end date`
	default:
		return fmt.Errorf("undefined code for SupplyDateRole has been passed, got [%s]", v)
	}
	return nil
}

// PriceConditionType List 167: Price condition type
type PriceConditionType string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *PriceConditionType) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// Allows positive indication that there are no conditions (the default if <PriceCondition> is omitted)
	case "00":
		*c = `No conditions`

	// Purchase at this price includes specified updates
	case "01":
		*c = `Includes updates`

	// Purchase at this price requires commitment to purchase specified updates, not included in price
	case "02":
		*c = `Must also purchase updates`

	// Updates may be purchased separately, no minimum commitment required
	case "03":
		*c = `Updates available`

	// Purchase at this price requires prior purchase of other product
	case "05":
		*c = `Linked prior purchase price`

	// Purchase at this price requires simultaneous purchase of other product
	case "06":
		*c = `Linked price`

	// The duration of the rental to which the price applies. Deprecated, use <PriceConstraint> instead
	case "10":
		*c = `Rental duration`

	// Purchase at this price requires prior rental of the product. <PriceConditionQuantity> gives minimum prior rental period, and <ProductIdentifier> may be used if rental uses a different product identifier
	case "11":
		*c = `Rental to purchase`

	// Upgrade to longer rental duration. <PriceConditionQuantity> gives minimum prior rental duration, and <ProductIdentifier> may be used if rental uses a different product identifier. Separate price constraint with time limited licence duration (code 07) specifies the new combined rental duration
	case "12":
		*c = `Rental extension`
	default:
		return fmt.Errorf("undefined code for PriceConditionType has been passed, got [%s]", v)
	}
	return nil
}

// PriceConditionQuantityType List 168: Price condition quantity type
type PriceConditionQuantityType string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *PriceConditionQuantityType) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// The price condition quantity represents a time period
	case "01":
		*c = `Time period`

	// The price condition quantity is a number of updates
	case "02":
		*c = `Number of updates`
	default:
		return fmt.Errorf("undefined code for PriceConditionQuantityType has been passed, got [%s]", v)
	}
	return nil
}

// QuantityUnit List 169: Quantity unit
type QuantityUnit string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *QuantityUnit) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// The quantity refers to a unit implied by the quantity type
	case "00":
		*c = `Units`

	//
	case "07":
		*c = `Days`

	//
	case "08":
		*c = `Weeks`

	//
	case "09":
		*c = `Months`

	//
	case "10":
		*c = `Years`
	default:
		return fmt.Errorf("undefined code for QuantityUnit has been passed, got [%s]", v)
	}
	return nil
}

// ContributorRoleCode List 17: Contributor role code
type ContributorRoleCode string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *ContributorRoleCode) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// Author of a textual work
	case "A01":
		*c = `By (author)`

	// With or as told to: ‘ghost’ author of a literary work
	case "A02":
		*c = `With`

	// Writer of screenplay or script (film or video)
	case "A03":
		*c = `Screenplay by`

	// Writer of libretto (opera): see also A31
	case "A04":
		*c = `Libretto by`

	// Author of lyrics (song): see also A31
	case "A05":
		*c = `Lyrics by`

	// Composer of music
	case "A06":
		*c = `By (composer)`

	// Visual artist when named as the primary creator of, eg, a book of reproductions of artworks
	case "A07":
		*c = `By (artist)`

	// Photographer when named as the primary creator of, eg, a book of photographs
	case "A08":
		*c = `By (photographer)`

	//
	case "A09":
		*c = `Created by`

	//
	case "A10":
		*c = `From an idea by`

	//
	case "A11":
		*c = `Designed by`

	// Artist when named as the creator of artwork which illustrates a text, or the originator (sometimes ‘penciller’ for collaborative art) of the artwork of a graphic novel or comic book
	case "A12":
		*c = `Illustrated by`

	// Photographer when named as the creator of photographs which illustrate a text
	case "A13":
		*c = `Photographs by`

	// Author of text which accompanies art reproductions or photographs, or which is part of a graphic novel or comic book
	case "A14":
		*c = `Text by`

	// Author of preface
	case "A15":
		*c = `Preface by`

	// Author of prologue
	case "A16":
		*c = `Prologue by`

	// Author of summary
	case "A17":
		*c = `Summary by`

	// Author of supplement
	case "A18":
		*c = `Supplement by`

	// Author of afterword
	case "A19":
		*c = `Afterword by`

	// Author of notes or annotations: see also A29
	case "A20":
		*c = `Notes by`

	// Author of commentaries on the main text
	case "A21":
		*c = `Commentaries by`

	// Author of epilogue
	case "A22":
		*c = `Epilogue by`

	// Author of foreword
	case "A23":
		*c = `Foreword by`

	// Author of introduction: see also A29
	case "A24":
		*c = `Introduction by`

	// Author/compiler of footnotes
	case "A25":
		*c = `Footnotes by`

	// Author of memoir accompanying main text
	case "A26":
		*c = `Memoir by`

	// Person who carried out experiments reported in the text
	case "A27":
		*c = `Experiments by`

	// Author of introduction and notes: see also A20 and A24
	case "A29":
		*c = `Introduction and notes by`

	// Writer of computer programs ancillary to the text
	case "A30":
		*c = `Software written by`

	// Author of the textual content of a musical drama: see also A04 and A05
	case "A31":
		*c = `Book and lyrics by`

	// Author of additional contributions to the text
	case "A32":
		*c = `Contributions by`

	// Author of appendix
	case "A33":
		*c = `Appendix by`

	// Compiler of index
	case "A34":
		*c = `Index by`

	//
	case "A35":
		*c = `Drawings by`

	// Use also for the cover artist of a graphic novel or comic book if named separately
	case "A36":
		*c = `Cover design or artwork by`

	// Responsible for preliminary work on which the work is based
	case "A37":
		*c = `Preliminary work by`

	// Author of the first edition (usually of a standard work) who is not an author of the current edition
	case "A38":
		*c = `Original author`

	// Maps drawn or otherwise contributed by
	case "A39":
		*c = `Maps by`

	// Use for secondary creators when separate persons are named as having respectively drawn and inked/colored/finished artwork, eg for a graphic novel or comic book. Use with A12 for ‘drawn by’. Use A40 for 'finished by', but prefer more specific codes A46 to A48 instead of A40 unless the more specific secondary roles are inappropriate, unclear or unavailable
	case "A40":
		*c = `Inked or colored by`

	// Designer of pop-ups in a pop-up book, who may be different from the illustrator
	case "A41":
		*c = `Pop-ups by`

	// Use where a standard work is being continued by somebody other than the original author
	case "A42":
		*c = `Continued by`

	//
	case "A43":
		*c = `Interviewer`

	//
	case "A44":
		*c = `Interviewee`

	// Writer of dialogue, captions in a comic book (following an outline by the primary writer)
	case "A45":
		*c = `Comic script by`

	// Renders final comic book line art based on work of the illustrator or penciller. Preferred to code A40
	case "A46":
		*c = `Inker`

	// Provides comic book color art and effects. Preferred to code A40
	case "A47":
		*c = `Colorist`

	// Creates comic book text balloons and other text elements (where this is a distinct role from script writer and/or illustrator)
	case "A48":
		*c = `Letterer`

	// Other type of primary creator not specified above
	case "A99":
		*c = `Other primary creator`

	//
	case "B01":
		*c = `Edited by`

	//
	case "B02":
		*c = `Revised by`

	//
	case "B03":
		*c = `Retold by`

	//
	case "B04":
		*c = `Abridged by`

	//
	case "B05":
		*c = `Adapted by`

	//
	case "B06":
		*c = `Translated by`

	//
	case "B07":
		*c = `As told by`

	// This code applies where a translator has provided a commentary on issues relating to the translation. If the translator has also provided a commentary on the work itself, codes B06 and A21 should be used
	case "B08":
		*c = `Translated with commentary by`

	// Name of a series editor when the product belongs to a series
	case "B09":
		*c = `Series edited by`

	//
	case "B10":
		*c = `Edited and translated by`

	//
	case "B11":
		*c = `Editor-in-chief`

	//
	case "B12":
		*c = `Guest editor`

	//
	case "B13":
		*c = `Volume editor`

	//
	case "B14":
		*c = `Editorial board member`

	//
	case "B15":
		*c = `Editorial coordination by`

	//
	case "B16":
		*c = `Managing editor`

	// Usually the founder editor of a serial publication: Begruendet von
	case "B17":
		*c = `Founded by`

	//
	case "B18":
		*c = `Prepared for publication by`

	//
	case "B19":
		*c = `Associate editor`

	// Use also for ‘advisory editor’, ‘series advisor’, ‘editorial consultant’ etc
	case "B20":
		*c = `Consultant editor`

	//
	case "B21":
		*c = `General editor`

	//
	case "B22":
		*c = `Dramatized by`

	// In Europe, an expert editor who takes responsibility for the legal content of a collaborative law volume
	case "B23":
		*c = `General rapporteur`

	// An editor who is responsible for establishing the text used in an edition of a literary work, where this is recognised as a distinctive role (in Spain, ‘editor literario’)
	case "B24":
		*c = `Literary editor`

	//
	case "B25":
		*c = `Arranged by (music)`

	// Responsible for the technical accuracy and language, may also be involved in coordinating and preparing technical material for publication
	case "B26":
		*c = `Technical editor`

	//
	case "B27":
		*c = `Thesis advisor or supervisor`

	//
	case "B28":
		*c = `Thesis examiner`

	// Responsible overall for the scientific content of the publication
	case "B29":
		*c = `Scientific editor`

	// Other type of adaptation or editing not specified above
	case "B99":
		*c = `Other adaptation by`

	// For puzzles, directories, statistics, etc
	case "C01":
		*c = `Compiled by`

	// For textual material (eg for an anthology)
	case "C02":
		*c = `Selected by`

	// Eg for a collection of photographs etc
	case "C03":
		*c = `Non-text material selected by`

	// Eg for an exhibition
	case "C04":
		*c = `Curated by`

	// Other type of compilation not specified above
	case "C99":
		*c = `Other compilation by`

	//
	case "D01":
		*c = `Producer`

	//
	case "D02":
		*c = `Director`

	// Conductor of a musical performance
	case "D03":
		*c = `Conductor`

	// Other type of direction not specified above
	case "D99":
		*c = `Other direction by`

	//
	case "E01":
		*c = `Actor`

	//
	case "E02":
		*c = `Dancer`

	//
	case "E03":
		*c = `Narrator`

	//
	case "E04":
		*c = `Commentator`

	// Singer etc
	case "E05":
		*c = `Vocal soloist`

	//
	case "E06":
		*c = `Instrumental soloist`

	// Reader of recorded text, as in an audiobook
	case "E07":
		*c = `Read by`

	// Name of a musical group in a performing role
	case "E08":
		*c = `Performed by (orchestra, band, ensemble)`

	// Of a speech, lecture etc
	case "E09":
		*c = `Speaker`

	// Introduces and links other contributors and material, eg within a documentary
	case "E10":
		*c = `Presenter`

	// Other type of performer not specified above: use for a recorded performance which does not fit a category above, eg a performance by a stand-up comedian
	case "E99":
		*c = `Performed by`

	// Cinematographer, etc
	case "F01":
		*c = `Filmed/photographed by`

	//
	case "F02":
		*c = `Editor (film or video)`

	// Other type of recording not specified above
	case "F99":
		*c = `Other recording by`

	// May be associated with any contributor role, and placement should therefore be controlled by contributor sequence numbering
	case "Z01":
		*c = `Assisted by`

	//
	case "Z02":
		*c = `Honored/dedicated to`

	// For use ONLY with ‘et al’ or ‘Various’ within <UnnamedPersons>, where the roles of the multiple contributors vary
	case "Z98":
		*c = `(Various roles)`

	// Other creative responsibility not falling within A to F above
	case "Z99":
		*c = `Other`
	default:
		return fmt.Errorf("undefined code for ContributorRoleCode has been passed, got [%s]", v)
	}
	return nil
}

// DiscountType List 170: Discount type
type DiscountType string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *DiscountType) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// Discount applied to all units in a qualifying order
	case "01":
		*c = `Rising discount`

	// Additional discount may be applied retrospectively, based on number of units ordered over a specific period
	case "02":
		*c = `Rising discount (cumulative)`

	// Discount applied to marginal units in a qualifying order
	case "03":
		*c = `Progressive discount`

	// Previous orders within a specific time period are counted when calculating a progressive discount
	case "04":
		*c = `Progressive discount (cumulative)`
	default:
		return fmt.Errorf("undefined code for DiscountType has been passed, got [%s]", v)
	}
	return nil
}

// TaxType List 171: Tax type
type TaxType string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *TaxType) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// Value-added tax (TVA, IVA, MwSt etc)
	case "01":
		*c = `VAT`

	// General sales tax
	case "02":
		*c = `GST`

	// ‘Green’ or eco-tax, levied to encourage responsible production or disposal, used only where this is identified separately from value-added or sales taxes
	case "03":
		*c = `ECO`
	default:
		return fmt.Errorf("undefined code for TaxType has been passed, got [%s]", v)
	}
	return nil
}

// CurrencyZone List 172: Currency zone
type CurrencyZone string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *CurrencyZone) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// Countries that at the time being have the Euro as their national currency. Deprecated
	case "EUR":
		*c = `Eurozone`
	default:
		return fmt.Errorf("undefined code for CurrencyZone has been passed, got [%s]", v)
	}
	return nil
}

// PriceDateRole List 173: Price date role
type PriceDateRole string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *PriceDateRole) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// Date on which a price becomes effective
	case "14":
		*c = `From date`

	// Date on which a price ceases to be effective
	case "15":
		*c = `Until date`

	// Combines From date and Until date to define a period (both dates are inclusive). Use with for example dateformat 06
	case "24":
		*c = `From… until date`
	default:
		return fmt.Errorf("undefined code for PriceDateRole has been passed, got [%s]", v)
	}
	return nil
}

// PrintedOnProduct List 174: Printed on product
type PrintedOnProduct string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *PrintedOnProduct) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// Price not printed on product
	case "01":
		*c = `No`

	// Price printed on product
	case "02":
		*c = `Yes`
	default:
		return fmt.Errorf("undefined code for PrintedOnProduct has been passed, got [%s]", v)
	}
	return nil
}

// ProductFormDetail List 175: Product form detail
type ProductFormDetail2 string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *ProductFormDetail2) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// CD ‘red book’ format
	case "A101":
		*c = `CD standard audio format`

	//
	case "A102":
		*c = `SACD super audio format`

	// MPEG-1/2 Audio Layer III file
	case "A103":
		*c = `MP3 format`

	//
	case "A104":
		*c = `WAV format`

	//
	case "A105":
		*c = `Real Audio format`

	// Windows Media Audio format
	case "A106":
		*c = `WMA`

	// Advanced Audio Coding format
	case "A107":
		*c = `AAC`

	// Vorbis audio format in the Ogg container
	case "A108":
		*c = `Ogg/Vorbis`

	// Audio format proprietary to Audible.com
	case "A109":
		*c = `Audible`

	// Free lossless audio codec
	case "A110":
		*c = `FLAC`

	// Audio Interchangeable File Format
	case "A111":
		*c = `AIFF`

	// Apple Lossless Audio Codec
	case "A112":
		*c = `ALAC`

	// Deprecated, as does not meet DAISY 2 standard. Use conventional audiobook codes instead
	case "A201":
		*c = `DAISY 2: full audio with title only (no navigation)`

	//
	case "A202":
		*c = `DAISY 2: full audio with navigation (no text)`

	//
	case "A203":
		*c = `DAISY 2: full audio with navigation and partial text`

	//
	case "A204":
		*c = `DAISY 2: full audio with navigation and full text`

	// Reading systems may provide full audio via text-to-speech
	case "A205":
		*c = `DAISY 2: full text with navigation and partial audio`

	// Reading systems may provide full audio via text-to-speech
	case "A206":
		*c = `DAISY 2: full text with navigation and no audio`

	// Deprecated, as does not meet DAISY 3 standard. Use conventional audiobook codes instead
	case "A207":
		*c = `DAISY 3: full audio with title only (no navigation)`

	//
	case "A208":
		*c = `DAISY 3: full audio with navigation (no text)`

	//
	case "A209":
		*c = `DAISY 3: full audio with navigation and partial text`

	//
	case "A210":
		*c = `DAISY 3: full audio with navigation and full text`

	// Reading systems may provide full audio via text-to-speech
	case "A211":
		*c = `DAISY 3: full text with navigation and partial audio`

	// Reading systems may provide full audio via text-to-speech
	case "A212":
		*c = `DAISY 3: full text with navigation and no audio`

	//
	case "A301":
		*c = `Standalone audio`

	// Audio intended exclusively for use alongside a printed copy of the book. Most often a children’s product. Normally contains instructions such as ‘turn the page now’ and other references to the printed item, and is usually sold packaged together with a printed copy
	case "A302":
		*c = `Readalong audio`

	// Audio intended for musical accompaniment, eg ‘Music minus one’, etc, often used for music learning. Includes singalong backing audio for musical learning or for Karaoke-style entertainment
	case "A303":
		*c = `Playalong audio`

	// Audio intended for language learning, which includes speech plus gaps intended to be filled by the listener
	case "A304":
		*c = `Speakalong audio`

	// Audio synchronised to text within an e-publication, for example an EPUB3 with audio overlay. Synchronisation at least at paragraph level, and covering the full content
	case "A305":
		*c = `Synchronised audio`

	// Includes 'stereo' where channels are identical
	case "A410":
		*c = `Mono`

	//
	case "A420":
		*c = `Stereo`

	// Stereo plus low-frequency channel
	case "A421":
		*c = `Stereo 2.1`

	// Five-channel audio (including low-frequency channel)
	case "A441":
		*c = `Surround 4.1`

	// Six-channel audio (including low-frequency channel)
	case "A451":
		*c = `Surround 5.1`

	// In North America, a category of paperback characterized partly by page size (typically 4¼ x 7 1/8 inches) and partly by target market and terms of trade. Use with Product Form code BC
	case "B101":
		*c = `Mass market (rack) paperback`

	// In North America, a category of paperback characterized partly by page size and partly by target market and terms of trade. AKA ‘quality paperback’, and including textbooks. Most paperback books sold in North America except ‘mass-market’ (B101) and ‘tall rack’ (B107) are correctly described with this code. Use with Product Form code BC
	case "B102":
		*c = `Trade paperback (US)`

	// In North America, a category of paperback characterized by page size and generally used for children’s books; use with Product Form code BC. Note: was wrongly shown as B102 (duplicate entry) in Issue 3
	case "B103":
		*c = `Digest format paperback`

	// In UK, a category of paperback characterized by page size (normally 178 x 111 mm approx); use with Product Form code BC
	case "B104":
		*c = `A-format paperback`

	// In UK, a category of paperback characterized by page size (normally 198 x 129 mm approx); use with Product Form code BC
	case "B105":
		*c = `B-format paperback`

	// In UK, a category of paperback characterized partly by size (usually in traditional hardback dimensions), and often used for paperback originals; use with Product Form code BC (replaces ‘C-format’ from former List 8)
	case "B106":
		*c = `Trade paperback (UK)`

	// In North America, a category of paperback characterised partly by page size and partly by target market and terms of trade; use with Product Form code BC
	case "B107":
		*c = `Tall rack paperback (US)`

	// 210x148mm
	case "B108":
		*c = `A5 size Tankobon`

	// Japanese B-series size, 257x182mm
	case "B109":
		*c = `JIS B5 size Tankobon`

	// Japanese B-series size, 182x128mm
	case "B110":
		*c = `JIS B6 size Tankobon`

	// 148x105mm
	case "B111":
		*c = `A6 size Bunko`

	// Japanese format, 182x103mm or 173x105mm
	case "B112":
		*c = `B40-dori Shinsho`

	// A Swedish, Norwegian, French paperback format, of no particular fixed size. Use with Product Form Code BC
	case "B113":
		*c = `Pocket (Sweden, Norway, France)`

	// A Swedish paperback format, use with Product Form Code BC
	case "B114":
		*c = `Storpocket (Sweden)`

	// A Swedish hardback format, use with Product Form Code BB
	case "B115":
		*c = `Kartonnage (Sweden)`

	// A Swedish softback format, use with Product Form Code BC
	case "B116":
		*c = `Flexband (Sweden)`

	// A softback book in the format of a magazine, usually sold like a book. Use with Product Form code BC
	case "B117":
		*c = `Mook / Bookazine`

	// Also called ‘Flipback’. A softback book in a specially compact proprietary format with pages printed in landscape on very thin paper and bound along the long (top) edge – see www.dwarsligger.com
	case "B118":
		*c = `Dwarsligger`

	// Japanese format: 188x127mm
	case "B119":
		*c = `46 size`

	// Japanese format
	case "B120":
		*c = `46-Henkei size`

	// 297x210mm
	case "B121":
		*c = `A4`

	// Japanese format
	case "B122":
		*c = `A4-Henkei size`

	// Japanese format
	case "B123":
		*c = `A5-Henkei size`

	// Japanese format
	case "B124":
		*c = `B5-Henkei size`

	// Japanese format
	case "B125":
		*c = `B6-Henkei size`

	// 257x210mm
	case "B126":
		*c = `AB size`

	// Japanese B-series size, 128x91mm
	case "B127":
		*c = `JIS B7 size`

	// Japanese format: 218x152mm or 227x152mm
	case "B128":
		*c = `Kiku size`

	// Japanese format
	case "B129":
		*c = `Kiku-Henkei size`

	// Japanese B-series size, 257x364mm
	case "B130":
		*c = `JIS B4 size`

	// German paperback format, greater than 205mm high, with flaps. Use with Product form code BC
	case "B131":
		*c = `Paperback (DE)`

	// Spanish pocket paperback
	case "B132":
		*c = `Libro de bolsillo`

	//
	case "B201":
		*c = `Coloring / join-the-dot book`

	//
	case "B202":
		*c = `Lift-the-flap book`

	// Note: was wrongly shown as B203 (duplicate entry) in Issue 3
	case "B204":
		*c = `Miniature book`

	//
	case "B205":
		*c = `Moving picture / flicker book`

	//
	case "B206":
		*c = `Pop-up book`

	//
	case "B207":
		*c = `Scented / ‘smelly’ book`

	//
	case "B208":
		*c = `Sound story / ‘noisy’ book`

	//
	case "B209":
		*c = `Sticker book`

	// A book whose pages have a variety of textured inserts designed to stimulate tactile exploration: see also B214 and B215
	case "B210":
		*c = `Touch-and-feel book`

	// A book which is cut into a distinctive non-rectilinear shape and/or in which holes or shapes have been cut internally. (‘Die-cut’ is used here as a convenient shorthand, and does not imply strict limitation to a particular production process)
	case "B212":
		*c = `Die-cut book`

	// A book which is also a toy, or which incorporates a toy as an integral part. (Do not, however, use B213 for a multiple-item product which includes a book and a toy as separate items)
	case "B213":
		*c = `Book-as-toy`

	// A book whose cover has a soft textured finish, typically over board
	case "B214":
		*c = `Soft-to-touch book`

	// A book with detachable felt pieces and textured pages on which they can be arranged
	case "B215":
		*c = `Fuzzy-felt book`

	// Children’s picture book: use with applicable Product Form code
	case "B221":
		*c = `Picture book`

	// (aka ‘Star’ book). Tax treatment of products may differ from that of products with similar codes such as Book as toy or Pop-up book)
	case "B222":
		*c = `‘Carousel’ book`

	// A book with movable card 'tabs' within the pages. Pull a tab to reveal or animate part of a picture (distinct from a ‘lift-the-flap’ book, where flaps simply reveal hidden pictures, and not a ‘pop-up’ book with 3D paper engineering)
	case "B223":
		*c = `Pull-the-tab book`

	// Use with Product Form code BD
	case "B301":
		*c = `Loose leaf – sheets and binder`

	// Use with Product Form code BD
	case "B302":
		*c = `Loose leaf – binder only`

	// Use with Product Form code BD
	case "B303":
		*c = `Loose leaf – sheets only`

	// AKA stitched; for ‘saddle-sewn’, see code B310
	case "B304":
		*c = `Sewn`

	// Including ‘perfect bound’, ‘glued’
	case "B305":
		*c = `Unsewn / adhesive bound`

	// Strengthened cloth-over-boards binding intended for libraries: use with Product form code BB
	case "B306":
		*c = `Library binding`

	// Strengthened binding, not specifically intended for libraries: use with Product form code BB or BC
	case "B307":
		*c = `Reinforced binding`

	// Must be accompanied by a code specifiying a material, eg ‘half-bound real leather’
	case "B308":
		*c = `Half bound`

	// Must be accompanied by a code specifiying a material, eg ‘quarter bound real leather’
	case "B309":
		*c = `Quarter bound`

	// AKA ‘saddle-stitched’ or ‘wire-stitched’
	case "B310":
		*c = `Saddle-sewn`

	// Round or oval plastic forms in a clamp-like configuration: use with Product Form code BE
	case "B311":
		*c = `Comb bound`

	// Twin loop metal wire spine: use with Product Form code BE
	case "B312":
		*c = `Wire-O`

	// Cased over Coiled or Wire-O binding: use with Product Form code BE and Product Form Detail code B312 or B315
	case "B313":
		*c = `Concealed wire`

	// Spiral wire bound. Use with product form code BE. The default if a spiral binding type is not stated. Cf. Comb and Wire-O binding
	case "B314":
		*c = `Coiled wire bound`

	// Hardcover binding intended for general consumers rather than libraries, use with Product form code BB. The default if a hardcover binding detail is not stated. cf. Library binding
	case "B315":
		*c = `Trade binding`

	// Cover is attached to the book block along only one edge of the spine, allowing the cover to lay flat
	case "B316":
		*c = `Swiss binding`

	// Refinement of perfect binding, with notches cut in the spine of the book block prior to glueing, to improve adhesion and durability
	case "B317":
		*c = `Notched binding`

	// Covers do not use a distinctive stock, but are the same as the body pages
	case "B400":
		*c = `Self-covered`

	// AKA fabric, linen over boards
	case "B401":
		*c = `Cloth over boards`

	//
	case "B402":
		*c = `Paper over boards`

	//
	case "B403":
		*c = `Leather, real`

	//
	case "B404":
		*c = `Leather, imitation`

	//
	case "B405":
		*c = `Leather, bonded`

	//
	case "B406":
		*c = `Vellum`

	// Cloth, not necessarily over boards – cf B401
	case "B409":
		*c = `Cloth`

	// Spanish ‘simil-tela’
	case "B410":
		*c = `Imitation cloth`

	//
	case "B411":
		*c = `Velvet`

	// AKA ‘flexibound’: use with Product Form code BC
	case "B412":
		*c = `Flexible plastic/vinyl cover`

	//
	case "B413":
		*c = `Plastic-covered`

	//
	case "B414":
		*c = `Vinyl-covered`

	// Book, laminating material unspecified: use L101 for ‘whole product laminated’, eg a laminated sheet map or wallchart
	case "B415":
		*c = `Laminated cover`

	// With card cover (like a typical paperback). As distinct from a self-cover or more elaborate binding
	case "B416":
		*c = `Card cover`

	// Type unspecified
	case "B501":
		*c = `With dust jacket`

	// Used to distinguish from B503
	case "B502":
		*c = `With printed dust jacket`

	// With translucent paper or plastic protective cover
	case "B503":
		*c = `With translucent dust cover`

	// For paperback with flaps
	case "B504":
		*c = `With flaps`

	//
	case "B505":
		*c = `With thumb index`

	// If the number of markers is significant, it can be stated as free text in <ProductFormDescription>
	case "B506":
		*c = `With ribbon marker(s)`

	//
	case "B507":
		*c = `With zip fastener`

	//
	case "B508":
		*c = `With button snap fastener`

	// AKA yapp edge?
	case "B509":
		*c = `With leather edge lining`

	// With edge trimming such that the front edge is ragged, not neatly and squarely trimmed: AKA deckle edge, feather edge, uncut edge, rough cut
	case "B510":
		*c = `Rough front`

	// With one or more gatefold or foldout sections bound in
	case "B511":
		*c = `Foldout`

	// Pages include extra-wide margin specifically intended for hand-written annotations
	case "B512":
		*c = `Wide margin`

	// Book with attached loop for fixing to baby stroller, cot, chair etc
	case "B513":
		*c = `With fastening strap`

	// With one or more pages perforated and intended to be torn out for use
	case "B514":
		*c = `With perforated pages`

	// Printed on acid-free or alkaline buffered paper conforming with ISO 9706
	case "B515":
		*c = `Acid-free paper`

	// Printed on acid-free or alkaline bufferred paper with a high cotton content, conforming with ISO 11108
	case "B516":
		*c = `Archival paper`

	// A book in which half the content is printed upside-down, to be read the other way round. Also known as a ‘flip-book’, ‘back-to-back’, (fr.) ‘tête-bêche’ (usually an omnibus of two works)
	case "B601":
		*c = `Turn-around book`

	// Manga with pages and panels in the sequence of the original Japanese, but with Western text
	case "B602":
		*c = `Unflipped manga format`

	// Text shows syllable breaks
	case "B610":
		*c = `Syllabification`

	// Single letters only. Was formerly identified as UK Braille Grade 1
	case "B701":
		*c = `UK Uncontracted Braille`

	// With some letter combinations. Was formerly identified as UK Braille Grade 2
	case "B702":
		*c = `UK Contracted Braille`

	// For US Braille, prefer codes B704 and B705 as appropriate
	case "B703":
		*c = `US Braille`

	//
	case "B704":
		*c = `US Uncontracted Braille`

	//
	case "B705":
		*c = `US Contracted Braille`

	// For UEB, prefer codes B708 and B709 as appropriate
	case "B706":
		*c = `Unified English Braille`

	// Moon embossed alphabet, used by some print-impaired readers who have difficulties with Braille
	case "B707":
		*c = `Moon`

	//
	case "B708":
		*c = `Unified English Uncontracted Braille`

	//
	case "B709":
		*c = `Unified English Contracted Braille`

	// Proprietary RealNetworks format. Includes Real Video packaged within a .rm RealMedia container
	case "D101":
		*c = `Real Video format`

	//
	case "D102":
		*c = `Quicktime format`

	//
	case "D103":
		*c = `AVI format`

	//
	case "D104":
		*c = `Windows Media Video format`

	//
	case "D105":
		*c = `MPEG-4`

	// Use with an applicable Product Form code D*; note that more detail of operating system requirements can be given in a Product Form Feature composite
	case "D201":
		*c = `MS-DOS`

	// Use with an applicable Product Form code D*; see note on D201
	case "D202":
		*c = `Windows`

	// Use with an applicable Product Form code D*; see note on D201
	case "D203":
		*c = `Macintosh`

	// Use with an applicable Product Form code D*; see note on D201
	case "D204":
		*c = `UNIX / LINUX`

	// Use with an applicable Product Form code D*; see note on D201
	case "D205":
		*c = `Other operating system(s)`

	// Use with an applicable Product Form code D*; see note on D201
	case "D206":
		*c = `Palm OS`

	// Use with an applicable Product Form code D*; see note on D201
	case "D207":
		*c = `Windows Mobile`

	// Use with Product Form code DE or DB as applicable
	case "D301":
		*c = `Microsoft XBox`

	// Use with Product Form code DE or DB as applicable
	case "D302":
		*c = `Nintendo Gameboy Color`

	// Use with Product Form code DE or DB as applicable
	case "D303":
		*c = `Nintendo Gameboy Advanced`

	// Use with Product Form code DE or DB as applicable
	case "D304":
		*c = `Nintendo Gameboy`

	// Use with Product Form code DE or DB as applicable
	case "D305":
		*c = `Nintendo Gamecube`

	// Use with Product Form code DE or DB as applicable
	case "D306":
		*c = `Nintendo 64`

	// Use with Product Form code DE or DB as applicable
	case "D307":
		*c = `Sega Dreamcast`

	// Use with Product Form code DE or DB as applicable
	case "D308":
		*c = `Sega Genesis/Megadrive`

	// Use with Product Form code DE or DB as applicable
	case "D309":
		*c = `Sega Saturn`

	// Use with Product Form code DE or DB as applicable
	case "D310":
		*c = `Sony PlayStation 1`

	// Use with Product Form code DE or DB as applicable
	case "D311":
		*c = `Sony PlayStation 2`

	//
	case "D312":
		*c = `Nintendo Dual Screen`

	//
	case "D313":
		*c = `Sony PlayStation 3`

	//
	case "D314":
		*c = `Xbox 360`

	//
	case "D315":
		*c = `Nintendo Wii`

	//
	case "D316":
		*c = `Sony PlayStation Portable (PSP)`

	// No code allocated for this e-publication format yet
	case "E100":
		*c = `Other`

	// The Open Publication Structure / OPS Container Format standard of the International Digital Publishing Forum (IDPF) [File extension .epub]
	case "E101":
		*c = `EPUB`

	// The Open EBook format of the IDPF, a predecessor of the full EPUB format, still (2008) supported as part of the latter [File extension .opf]. Includes EPUB format up to and including version 2 – but prefer code E101 for EPUB 2, and always use code E101 for EPUB 3
	case "E102":
		*c = `OEB`

	// Microsoft Word binary document format [File extension .doc]
	case "E103":
		*c = `DOC`

	// Office Open XML / Microsoft Word XML document format (ISO/IEC 29500:2008) [File extension .docx]
	case "E104":
		*c = `DOCX`

	// HyperText Mark-up Language [File extension .html, .htm]
	case "E105":
		*c = `HTML`

	// Open Document Format [File extension .odt]
	case "E106":
		*c = `ODF`

	// Portable Document Format (ISO 32000-1:2008) [File extension .pdf]
	case "E107":
		*c = `PDF`

	// PDF archiving format defined by ISO 19005-1:2005 [File extension .pdf]
	case "E108":
		*c = `PDF/A`

	// Rich Text Format [File extension .rtf]
	case "E109":
		*c = `RTF`

	// Standard Generalized Mark-up Language
	case "E110":
		*c = `SGML`

	// A compressed text format mainly used on Psion handheld devices [File extension .tcr]
	case "E111":
		*c = `TCR`

	// Text file format [File extension .txt]. Typically ASCII or Unicode UTF-8/16
	case "E112":
		*c = `TXT`

	// Extensible Hypertext Markup Language [File extension .xhtml, .xht, .xml, .html, .htm]
	case "E113":
		*c = `XHTML`

	// A compressed text format mainly used on Palm handheld devices [File extension .pdb – see also E121, E125, E130]
	case "E114":
		*c = `zTXT`

	// XML Paper Specification format [File extension .xps]
	case "E115":
		*c = `XPS`

	// A format proprietary to Amazon for use with its Kindle reading devices or software readers [File extensions .azw, .mobi, .prc]
	case "E116":
		*c = `Amazon Kindle`

	// A Sony proprietary format for use with the Sony Reader and LIBRIé reading devices [File extension .lrf]
	case "E117":
		*c = `BBeB`

	// A proprietary format for use with DXReader software
	case "E118":
		*c = `DXReader`

	// A format proprietary to the Ebook Library service
	case "E119":
		*c = `EBL`

	// A format proprietary to the Ebrary service
	case "E120":
		*c = `Ebrary`

	// A proprietary format for use with eReader (AKA ‘Palm Reader’) software on various hardware platforms [File extension .pdb – see also E114, E125, E130]
	case "E121":
		*c = `eReader`

	// A proprietary format with its own reading system for Windows platforms [File extension .exe]
	case "E122":
		*c = `Exebook`

	// A proprietary format for use with the Franklin eBookman reader
	case "E123":
		*c = `Franklin eBookman`

	// A proprietary format for use with the Gemstar Rocketbook reader [File extension .rb]
	case "E124":
		*c = `Gemstar Rocketbook`

	// A proprietary format for use with iSilo software on various hardware platforms [File extension .pdb – see also E114, E121, E130]
	case "E125":
		*c = `iSilo`

	// A proprietary format for use with Microsoft Reader software on Windows and Pocket PC platforms [File extension .lit]
	case "E126":
		*c = `Microsoft Reader`

	// A proprietary format for use with Mobipocket software on various hardware platforms [File extensions .mobi, .prc]. Includes Amazon Kindle formats up to and including version 7 – but prefer code E116 for version 7, and always use E116 for KF8
	case "E127":
		*c = `Mobipocket`

	// A format proprietary to the MyiLibrary service
	case "E128":
		*c = `MyiLibrary`

	// A format proprietary to the NetLibrary service
	case "E129":
		*c = `NetLibrary`

	// A proprietary format for use with Plucker reader software on Palm and other handheld devices [File extension .pdb – see also E114, E121, E125]
	case "E130":
		*c = `Plucker`

	// A format proprietary to the VitalSource service
	case "E131":
		*c = `VitalBook`

	// A proprietary digital product combining text and video content and available to be used online or as a downloadable application for a mobile device – see www.vook.com
	case "E132":
		*c = `Vook`

	// An epublication made available by Google in association with a publisher; readable online on a browser-enabled device and offline on designated ebook readers
	case "E133":
		*c = `Google Edition`

	// Epublication packaged as application for iOS (eg Apple iPhone, iPad etc), containing both executable code and content. Use <ProductContentType> to describe content, and <ProductFormFeatureType> to list detailed technical requirements
	case "E134":
		*c = `Book ‘app’ for iOS`

	// Epublication packaged as application for Android (eg Android phone or tablet), containing both executable code and content. Use <ProductContentType> to describe content, and <ProductFormFeatureType> to list detailed technical requirements
	case "E135":
		*c = `Book ‘app’ for Android`

	// Epublication packaged as application, containing both executable code and content. Use where other ‘app’ codes are not applicable. Technical requirements such as target operating system and/or device should be provided eg in <ProductFormFeatureType>. Content type (text or text plus various ‘enhancements’) may be described with <ProductContentType>
	case "E136":
		*c = `Book ‘app’ for other operating system`

	// Founder Apabi’s proprietary basic e-book format
	case "E139":
		*c = `CEB`

	// Founder Apabi’s proprietary XML e-book format
	case "E140":
		*c = `CEBX`

	// Apple’s iBook format (a proprietary extension of EPUB), can only be read on Apple iOS devices
	case "E141":
		*c = `iBook`

	// Proprietary format based on EPUB used by Barnes and Noble for fixed-format e-books, readable on NOOK devices and Nook reader software
	case "E142":
		*c = `ePIB`

	// Sharable Content Object Reference Model, standard content and packaging format for e-learning objects
	case "E143":
		*c = `SCORM`

	// E-book Plus (proprietary Norwegian e-book format)
	case "E144":
		*c = `EBP`

	// Proprietary format based on PDF used by Barnes and Noble for fixed-format e-books, readable on some NOOK devices and Nook reader software
	case "E145":
		*c = `Page Perfect`

	// Use when a particular e-publication type (specified using codes E100 and upwards) has both fixed format and reflowable variants
	case "E200":
		*c = `Reflowable`

	// Use when a particular e-publication type (specified using codes E100 and upwards) has both fixed format and reflowable variants
	case "E201":
		*c = `Fixed format`

	// All e-publication resources are included within the e-publication package
	case "E202":
		*c = `Readable offline`

	// E-publication requires a network connection to access some resources (eg an enhanced e-book where video clips are not stored within the e-publication package itself, but are delivered via an internet connection)
	case "E203":
		*c = `Requires network connection`

	// Resources (eg images) present in other editions have been removed from this product, eg due to rights issues
	case "E204":
		*c = `Content removed`

	// Use for fixed-format e-books optimised for landscape display. Also include an indication of the optimal screen aspect ratio
	case "E210":
		*c = `Landscape`

	// Use for fixed-format e-books optimised for portrait display. Also include an indication of the optimal screen aspect ratio
	case "E211":
		*c = `Portrait`

	// Use for fixed-format e-books optimised for displays with a 5:4 aspect ratio (eg 1280x1024 pixels etc, assuming square pixels). Note that aspect ratio codes are NOT specific to actual screen dimensions or pixel counts, but to the ratios between two dimensions or two pixel counts
	case "E221":
		*c = `5:4`

	// Use for fixed-format e-books optimised for displays with a 4:3 aspect ratio (eg 800x600, 1024x768, 2048x1536 pixels etc)
	case "E222":
		*c = `4:3`

	// Use for fixed-format e-books optimised for displays with a 3:2 aspect ratio (eg 960x640, 3072x2048 pixels etc)
	case "E223":
		*c = `3:2`

	// Use for fixed-format e-books optimised for displays with a 16:10 aspect ratio (eg 1440x900, 2560x1600 pixels etc)
	case "E224":
		*c = `16:10`

	// Use for fixed-format e-books optimised for displays with a 16:9 aspect ratio (eg 1024x576, 1920x1080, 2048x1152 pixels etc)
	case "E225":
		*c = `16:9`

	// Whole product laminated (eg laminated map, fold-out chart, wallchart, etc): use B415 for book with laminated cover
	case "L101":
		*c = `Laminated`

	// Use with Product Form code PC
	case "P101":
		*c = `Desk calendar`

	// Use with Product Form code PC
	case "P102":
		*c = `Mini calendar`

	// Use with Product Form code PC
	case "P103":
		*c = `Engagement calendar`

	// Use with Product Form code PC
	case "P104":
		*c = `Day by day calendar`

	// Use with Product Form code PC
	case "P105":
		*c = `Poster calendar`

	// Use with Product Form code PC
	case "P106":
		*c = `Wall calendar`

	// Use with Product Form code PC
	case "P107":
		*c = `Perpetual calendar`

	// Use with Product Form code PC
	case "P108":
		*c = `Advent calendar`

	// Use with Product Form code PC
	case "P109":
		*c = `Bookmark calendar`

	// Use with Product Form code PC
	case "P110":
		*c = `Student calendar`

	// Use with Product Form code PC
	case "P111":
		*c = `Project calendar`

	// Use with Product Form code PC
	case "P112":
		*c = `Almanac calendar`

	// A calendar that is not one of the types specified elsewhere: use with Product Form code PC
	case "P113":
		*c = `Other calendar`

	// A product that is associated with or ancillary to a calendar or organiser, eg a deskstand for a calendar, or an insert for an organiser: use with Product Form code PC or PS
	case "P114":
		*c = `Other calendar or organiser product`

	// Kamishibai / Cantastoria cards
	case "P120":
		*c = `Picture story cards`

	// Stationery item in hardback book format
	case "P201":
		*c = `Hardback (stationery)`

	// Stationery item in paperback/softback book format
	case "P202":
		*c = `Paperback / softback (stationery)`

	// Stationery item in spiral-bound book format
	case "P203":
		*c = `Spiral bound (stationery)`

	// Stationery item in leather-bound book format, or other fine binding
	case "P204":
		*c = `Leather / fine binding (stationery)`

	// For wall map, poster, wallchart etc
	case "P301":
		*c = `With hanging strips`

	// TV standard for video or DVD
	case "V201":
		*c = `PAL`

	// TV standard for video or DVD
	case "V202":
		*c = `NTSC`

	// TV standard for video or DVD
	case "V203":
		*c = `SECAM`

	// Licensed for use in domestic contexts only
	case "V220":
		*c = `Home use`

	// Licensed for use in education
	case "V221":
		*c = `Classroom use`
	default:
		return fmt.Errorf("undefined code for ProductFormDetail has been passed, got [%s]", v)
	}
	return nil
}

// ProductFormFeatureValueOperatingSystem List 176: Product form feature value – operating system
type ProductFormFeatureValueOperatingSystem string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *ProductFormFeatureValueOperatingSystem) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// An Open Source mobile device operating system originally developed by Google and supported by the Open Handset Alliance
	case "01":
		*c = `Android`

	// A proprietary operating system supplied by Research In Motion for its BlackBerry handheld devices
	case "02":
		*c = `BlackBerry OS`

	// A proprietary operating system based on Mac OS X supplied by Apple for its iPhone, iPad and iPod Touch handheld devices
	case "03":
		*c = `iOS`

	// An operating system based on the Linux kernel
	case "04":
		*c = `Linux`

	// [A proprietary operating system supplied by Apple on Macintosh computers up to 2002] DEPRECATED – use code 13 for all Mac OS versions
	case "05":
		*c = `Mac OS`

	// [A proprietary operating system supplied by Apple on Macintosh computers from 2001/2002] DEPRECATED – use code 13 for all Mac OS versions
	case "06":
		*c = `Mac OS X`

	// A proprietary operating system (AKA Garnet OS) originally developed for handheld devices
	case "07":
		*c = `Palm OS`

	// A proprietry Linux-based operating system for handheld devices, originally developed by Palm (now owned by LG)
	case "08":
		*c = `webOS`

	// An operating system for hand-held devices, originally developed as a proprietary system, but planned to become wholly Open Source by 2010
	case "09":
		*c = `Symbian`

	// A proprietary operating system supplied by Microsoft
	case "10":
		*c = `Windows`

	// A proprietary operating system (AKA Windows Embedded Compact, WinCE) supplied by Microsoft for small-scale devices
	case "11":
		*c = `Windows CE`

	// A proprietary operating system supplied by Microsoft for mobile devices
	case "12":
		*c = `Windows Mobile`

	// A proprietary operating system supplied by Apple on Macintosh computers
	case "13":
		*c = `Mac OS`

	// A proprietary operating system supplied by Microsoft for mobile devices, successor to Windows Mobile
	case "14":
		*c = `Windows Phone 7`
	default:
		return fmt.Errorf("undefined code for ProductFormFeatureValueOperatingSystem has been passed, got [%s]", v)
	}
	return nil
}

// PersonOrganizationDateRole List 177: Person / organization date role
type PersonOrganizationDateRole string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *PersonOrganizationDateRole) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	//
	case "50":
		*c = `Date of birth`

	//
	case "51":
		*c = `Date of death`

	// (‘Floruit’). To date the height of or most productive period during a career
	case "56":
		*c = `Flourished around`
	default:
		return fmt.Errorf("undefined code for PersonOrganizationDateRole has been passed, got [%s]", v)
	}
	return nil
}

// SupportingResourceFileFormat List 178: Supporting resource file format
type SupportingResourceFileFormat string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *SupportingResourceFileFormat) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// MPEG 1/2 Audio Layer III file
	case "A103":
		*c = `MP3`

	// Waveform Audio file
	case "A104":
		*c = `WAV`

	// Proprietary RealNetworks format
	case "A105":
		*c = `Real Audio`

	// Windows Media Audio format
	case "A106":
		*c = `WMA`

	// Advanced Audio Coding format
	case "A107":
		*c = `AAC`

	// Vorbis audio format in the Ogg container
	case "A108":
		*c = `Ogg/Vorbis`

	// Audio Interchange File format
	case "A111":
		*c = `AIFF`

	// Proprietary RealNetworks format. Includes Real Video packaged within a .rm RealMedia container
	case "D101":
		*c = `Real Video`

	// Quicktime container format (.mov)
	case "D102":
		*c = `Quicktime`

	// Audio Video Interleave format
	case "D103":
		*c = `AVI`

	// Windows Media Video format
	case "D104":
		*c = `WMV`

	// MPEG-4 container format (.mp4, .m4a)
	case "D105":
		*c = `MPEG-4`

	// Flash Video (.flv, .f4v)
	case "D106":
		*c = `FLV`

	// ShockWave (.swf)
	case "D107":
		*c = `SWF`

	// 3GPP container format (.3gp, .3g2)
	case "D108":
		*c = `3GP`

	// WebM container format (includes .mkv)
	case "D109":
		*c = `WebM`

	// Portable Document File format
	case "D401":
		*c = `PDF`

	// Graphic Interchange File format
	case "D501":
		*c = `GIF`

	// Joint Photographic Experts Group format
	case "D502":
		*c = `JPEG`

	// Portable Network Graphics format
	case "D503":
		*c = `PNG`

	// Tagged Image File format
	case "D504":
		*c = `TIFF`

	// The Open Publication Structure / OPS Container Format standard of the International Digital Publishing Forum (IDPF) [File extension .epub]
	case "E101":
		*c = `EPUB`

	// HyperText Mark-up Language [File extension .html, .htm]
	case "E105":
		*c = `HTML`

	// Portable Document Format (ISO 32000-1:2008) [File extension .pdf]
	case "E107":
		*c = `PDF`

	// Plain text (either ASCII or UTF-8/16 Unicode)
	case "E112":
		*c = `TXT`

	// Extensible Hypertext Markup Language [File extension .xhtml, .xht, .xml, .html, .htm]
	case "E113":
		*c = `XHTML`

	// XML Paper Specification
	case "E115":
		*c = `XPS`

	// A format proprietary to Amazon for use with its Kindle reading devices or software readers [File extensions .azw, .mobi, .prc]
	case "E116":
		*c = `Amazon Kindle`

	// Founder Apabi’s proprietary basic e-book format
	case "E139":
		*c = `CEB`

	// Founder Apabi’s proprietary XML e-book format
	case "E140":
		*c = `CEBX`
	default:
		return fmt.Errorf("undefined code for SupportingResourceFileFormat has been passed, got [%s]", v)
	}
	return nil
}

// PriceCodeType List 179: Price code type
type PriceCodeType string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *PriceCodeType) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// A publisher or retailer’s proprietary code list as specified in <PriceCodeTypeName> which identifies particular codes with particular price points, price tiers or bands
	case "01":
		*c = `Proprietary`

	// Price Code scheme for Finnish Pocket Books (Pokkareiden hintaryhmä). Price codes expressed as letters A–J in <PriceCode>
	case "02":
		*c = `Finnish Pocket Book price code`

	// Price Code scheme for Finnish Miki Books (Miki-kirjojen hintaryhmä). Price codes expressed as an integer 1–n in <PriceCode>
	case "03":
		*c = `Finnish Miki Book price code`
	default:
		return fmt.Errorf("undefined code for PriceCodeType has been passed, got [%s]", v)
	}
	return nil
}

// PersonOrganizationNameType List 18: Person / organization name type
type PersonOrganizationNameType string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *PersonOrganizationNameType) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	//
	case "00":
		*c = `Unspecified`

	// May be used to give a well-known pseudonym, where the primary name is a ‘real’ name
	case "01":
		*c = `Pseudonym`

	//
	case "02":
		*c = `Authority-controlled name`

	// Use only within <AlternativeName>
	case "03":
		*c = `Earlier name`

	// May be used to identify a well-known real name, where the primary name is a pseudonym
	case "04":
		*c = `‘Real’ name`

	// Use only within <AlternativeName>, when the primary name type is unspecified
	case "05":
		*c = `Transliterated form of primary name`

	// Use only within <AlternativeName>
	case "06":
		*c = `Later name`
	default:
		return fmt.Errorf("undefined code for PersonOrganizationNameType has been passed, got [%s]", v)
	}
	return nil
}

// EuToySafetyDirectiveHazardWarning List 184: EU Toy Safety Directive hazard warning
type EuToySafetyDirectiveHazardWarning string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *EuToySafetyDirectiveHazardWarning) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// Use to provide positive indication that no warnings are applicable
	case "00":
		*c = `No warning`

	//
	case "01":
		*c = `Carries ‘CE’ logo`

	// Use to specify age (in years, or years and months). Provide specific wording in <ProductFormFeatureDescription>
	case "02":
		*c = `Carries minimum age warning`

	// Carries logo, and must be accompanied by the default warning ‘Not suitable for children under 36 months’ (or its approved equivalent in a language other than English, as appropriate), unless specific wording is provided in <ProductFormFeatureDescription>. If specific alternative wording is carried in <ProductFormFeatureDescription>, this must be used in place of the default text
	case "03":
		*c = `Carries EU Toy Safety Directive ‘Unsuitable for children ages 0–3’ warning logo`

	// Exact text of warning must be included in <ProductFormFeatureDescription>
	case "04":
		*c = `Carries EU Toy Safety Directive hazard warning`

	// Exact text (not in itself a warning) must be included in <ProductFormFeatureDescription>. May be used either without any warning, or as text additional to a warning. Note that if no warnings apply, code 00 can provide positive indication of this. Example use: ‘Suitable for all ages’
	case "05":
		*c = `Carries other text associated with toy safety`

	// Material Safety Data Sheet (a document required by the EU Toy Safety Directive) available online, typically as a PDF file or similar. <ProductFormFeatureDescription> must carry the URL of the document
	case "06":
		*c = `Material Safety Data Sheet available`

	// Declaration of Conformity (the document that backs up the CE mark) available online, typically as a PDF file or similar. <ProductFormFeatureDescription> must carry the URL of the document
	case "07":
		*c = `Declaration of Conformity available`
	default:
		return fmt.Errorf("undefined code for EuToySafetyDirectiveHazardWarning has been passed, got [%s]", v)
	}
	return nil
}

// UnnamedPersons List 19: Unnamed person(s)
type UnnamedPersons string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *UnnamedPersons) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	//
	case "01":
		*c = `Unknown`

	//
	case "02":
		*c = `Anonymous`

	// And others: additional contributors not listed
	case "03":
		*c = `et al`

	// When the product is a pack of books by different authors
	case "04":
		*c = `Various authors`

	// Use with Contributor role code E07 ‘read by’, for audio books for the blind
	case "05":
		*c = `Synthesized voice – male`

	// Use with Contributor role code E07 ‘read by’, for audio books for the blind
	case "06":
		*c = `Synthesized voice – female`

	// Use with Contributor role code E07 ‘read by’, for audio books for the blind
	case "07":
		*c = `Synthesized voice – unspecified`
	default:
		return fmt.Errorf("undefined code for UnnamedPersons has been passed, got [%s]", v)
	}
	return nil
}

// EPublicationAccessibilityDetails List 196: E-publication Accessibility Details
type EPublicationAccessibilityDetails string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *EPublicationAccessibilityDetails) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	//
	case "01":
		*c = `LIA Compliance Scheme`

	// Conforms with the requirements of EPUB Accessibility Spec 1.0 and WCAG level A. <ProductFormFeatureDescription> may carry a URL linking to a compliance report or certification provided by an independent third party certifier. In the absence of a URL, conformance with the requirements of the Accessibility Specification is self-certified by the publisher
	case "02":
		*c = `Accessibility Specification 1.0 A`

	// Conforms with the requirements of EPUB Accessibility Spec 1.0 and WCAG level AA. <ProductFormFeatureDescription> may carry a URL linking to a compliance report or certification provided by an independent third party certifier. In the absence of a URL, conformance with the requirements of the Accessibility Specification is self-certified by the publisher
	case "03":
		*c = `Accessibility Specification 1.0 AA`

	// No accessibility features offered by the reading system, device or reading software (including but not limited to choice of text size or typeface, choice of text or background color, text-to-speech) are disabled, overridden or otherwise unusable with the product EXCEPT – in ONIX 3 messages only – those specifically noted as subject to restriction or prohibition in <EpubUsageConstraint>. Note that provision of any significant part of the textual content as images (ie as pictures of text, rather than as text) inevitably prevents use of these accessibility options
	case "10":
		*c = `No reading system accessibility options disabled (except)`

	// Table of contents allows direct (eg hyperlinked) access to all levels of text organization above individual paragraphs (eg to all sections and subsections) and to all tables, figures, illustrations etc. Non-textual items such as illustrations, tables, audio or video content may be directly accessible from the Table of contents, or from a similar List of illustrations, List of tables, etc
	case "11":
		*c = `Table of contents navigation`

	// Index provides direct (eg hyperlinked) access to uses of the index terms in the document body
	case "12":
		*c = `Index navigation`

	// All or substantially all textual matter is arranged in a single logical reading order (including text that is visually presented as separate from the main text flow, eg in boxouts, captions, tables, footnotes, endnotes, citations, etc). Non-textual content is also linked from within this logical reading order. (Purely decorative non-text content can be ignored)
	case "13":
		*c = `Reading order`

	// All or substantially all non-text content has short alternative descriptions, usually provided via alt attributes. Note this applies to normal images (eg photographs, charts and diagrams) and also to any embedded audio, video etc. Audio and video content should include alternative descriptions suitable for hearing-impaired as well as for visually-impaired readers. (Purely decorative non-text content can be ignored, but the accessibility of resources delivered via a network connection rather than as part of the e-publication package must be included)
	case "14":
		*c = `Short alternative descriptions`

	// All or substantially all non-text content has full alternative descriptions. Note this applies to normal images (eg photographs, charts and diagrams) and also to any embedded audio, video etc. Audio and video content should include full alternative descriptions (eg audio-described video) and subtitles or closed captions suitable for hearing-impaired as well as for visually-impaired readers. (Purely decorative non-text content can be ignored, but the accessibility of resources delivered via a network connection rather than as part of the e-publication package must be included)
	case "15":
		*c = `Full alternative descriptions`

	// Where data visualisations are provided (eg graphs and charts), the underlying data is also available in non-graphical (usually tabular, textual) form
	case "16":
		*c = `Visualised data also available as non-graphical data`

	// Mathematical content such as equations is usable with assistive technology, eg through use of MathML. Semantic MathML is preferred but Presentational MathML is acceptable
	case "17":
		*c = `Accessible math content`

	// Chemistry content such as chemical formulae is usable with assistive technology, eg through use of ChemML
	case "18":
		*c = `Accessible chem content`

	// For a reflowable e-publication, contains references to the page numbering of an equivalent printed product
	case "19":
		*c = `Print-equivalent page numbering`

	// Text-synchronised pre-recorded audio narration (natural or synthesised voice) is included for substantially all textual matter, including all alternative descriptions
	case "20":
		*c = `Synchronised pre-recorded audio`

	// Text-to-speech has been optimised through provision of PLS lexicons, SSML or CSS Speech synthesis hints
	case "21":
		*c = `Text-to-speech hinting provided`

	// The language of the text has been specified (eg via the HTML or XML lang attribute) to optimise text-to-speech (and other alternative renderings), both at whole document level and, where appropriate, for individual words, phrases or passages in a different language
	case "22":
		*c = `Language tagging provided`

	// Specialised font, character and line spacing, justification and paragraph spacing, colouring and other options provided specifically to improve readability for dyslexic readers. Details, including the name of the font, should be listed in <ProductFormFeatureDescription>
	case "24":
		*c = `Dyslexia readability`

	// <ProductFormFeatureDescription> carries the URL of a web page giving further detailed description of the accessibility features, compatibility, testing etc. The web page should be maintained by an independent compliance scheme or testing organization
	case "94":
		*c = `Compliance web page for detailed accessibility information`

	// <ProductFormFeatureDescription> carries the URL of a web page giving further detailed description of the accessibility features, compatibility, testing etc. The web page should be provided by a trusted intermediary or third party nominated by the publisher
	case "95":
		*c = `Trusted intermediary’s web page for detailed accessibility information`

	// <ProductFormFeatureDescription> carries the URL of a web page giving further detailed description of the accessibility features, compatibility, testing etc. The web page should be provided by the publisher
	case "96":
		*c = `Publisher’s web page for detailed accessibility information`

	// <ProductFormFeatureDescription> carries a short description of compatibility testing carried out for this product, including detailed compatibility with various assistive technology such as third-party screen-reading software
	case "97":
		*c = `Compatibility tested`

	// <ProductFormFeatureDescription> carries the e-mail address for a contact at a ‘trusted intermediary’, to whom detailed questions about accessibility for this product may be directed
	case "98":
		*c = `Trusted Intermediary contact`

	// <ProductFormFeatureDescription> carries the e-mail address for a contact at the publisher to whom detailed questions about accessibility of this product may be directed
	case "99":
		*c = `Publisher contact for further accessibility information`
	default:
		return fmt.Errorf("undefined code for EPublicationAccessibilityDetails has been passed, got [%s]", v)
	}
	return nil
}

// CollectionSequenceType List 197: Collection sequence type
type CollectionSequenceType string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *CollectionSequenceType) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// A short explanatory label for the sequence should be provided in <CollectionSequenceTypeName>
	case "01":
		*c = `Proprietary`

	// Order as specified by the title, eg by volume or part number sequence, provided for confirmation
	case "02":
		*c = `Title order`

	// Order of publication of products within the collection
	case "03":
		*c = `Publication order`

	// Order defined by a continuing narrative or temporal sequence within products in the collection. Applicable to either fiction or to non-fiction (eg within a collection of history textbooks)
	case "04":
		*c = `Temporal/narrative order`

	// Original publication order, for a republished collection or collected works originally published outside a collection
	case "05":
		*c = `Original publication order`
	default:
		return fmt.Errorf("undefined code for CollectionSequenceType has been passed, got [%s]", v)
	}
	return nil
}

// ProductContactRole List 198: Product contact role
type ProductContactRole string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *ProductContactRole) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// Eg for requests for supply of mutable digital files for conversion to other formats
	case "01":
		*c = `Accessibility request contact`

	// Eg for requests relating to interviews, author events
	case "02":
		*c = `Promotional contact`

	// Eg for co-op advertising
	case "03":
		*c = `Advertising contact`

	// Eg for requests for review copies
	case "04":
		*c = `Review copy contact`

	// Eg for requests for approval or evaluation copies (particularly within education)
	case "05":
		*c = `Evaluation copy contact`

	// Eg for requests to reproduce or repurpose parts of the publication
	case "06":
		*c = `Permissions contact`
	default:
		return fmt.Errorf("undefined code for ProductContactRole has been passed, got [%s]", v)
	}
	return nil
}

// ProductComposition List 2: Product composition
type ProductComposition string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *ProductComposition) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	//
	case "00":
		*c = `Single-item retail product`

	// Multiple-item product retailed as a whole
	case "10":
		*c = `Multiple-item retail product`

	// Used when an ONIX record is required for a collection-as-a-whole, even though it is not currently retailed as such
	case "11":
		*c = `Multiple-item collection, retailed as separate parts`

	// Product not for retail, and not carrying retail items, eg empty dumpbin, empty counterpack, promotional material
	case "20":
		*c = `Trade-only product`

	// Carrying multiple copies for retailing as separate items, eg shrink-wrapped trade pack, filled dumpbin, filled counterpack
	case "30":
		*c = `Multiple-item trade pack`

	// Carrying multiple copies, primarily for retailing as separate items. The pack may be split and retailed as separate items OR retailed as a single item. Use instead of Multiple item trade pack (code 30) only if the data provider specifically wishes to make explicit that the pack may optionally be retailed as a whole
	case "31":
		*c = `Multiple-item pack`
	default:
		return fmt.Errorf("undefined code for ProductComposition has been passed, got [%s]", v)
	}
	return nil
}

// EventRole List 20: Event role
type EventRole string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *EventRole) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// For example an academic, professional or political conference
	case "01":
		*c = `Publication linked to conference`

	//
	case "02":
		*c = `Complete proceedings of conference`

	//
	case "03":
		*c = `Selected papers from conference`

	// For example a competitive match, fixture series or championship
	case "11":
		*c = `Publication linked to sporting event`

	//
	case "12":
		*c = `Programme or guide for sporting event`

	// For example a theatrical or musical event or performance, a season of events or performances, or an exhibition of art
	case "21":
		*c = `Publication linked to artistic event`

	//
	case "22":
		*c = `Programme or guide for artistic event`

	// For example a commercial exposition
	case "31":
		*c = `Publication linked to exposition`

	//
	case "32":
		*c = `Programme or guide for exposition`
	default:
		return fmt.Errorf("undefined code for EventRole has been passed, got [%s]", v)
	}
	return nil
}

// OnixAdultAudienceRating List 203: ONIX Adult Audience rating
type OnixAdultAudienceRating string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *OnixAdultAudienceRating) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	//
	case "00":
		*c = `Unrated`

	// The publisher states that the product is suitable for any adult audience
	case "01":
		*c = `Any adult audience`

	// The publisher warns the content may offend parts of the adult audience (for any reason)
	case "02":
		*c = `Content warning`

	// The publisher warns the product includes content of an explicit sexual nature
	case "03":
		*c = `Content warning (sex)`

	// The publisher warns the product includes content of an extreme violent nature
	case "04":
		*c = `Content warning (violence)`

	// The publisher warns the product includes content involving severe misuse of drugs
	case "05":
		*c = `Content warning (drug-taking)`

	// The publisher warns the product includes extreme / offensive / explicit language
	case "06":
		*c = `Content warning (language)`

	// The publisher warns the product includes content involving intolerance of particular groups (eg religious, ethnic, racial, social)
	case "07":
		*c = `Content warning (intolerance)`
	default:
		return fmt.Errorf("undefined code for OnixAdultAudienceRating has been passed, got [%s]", v)
	}
	return nil
}

// OnixReturnsConditionsCode List 204: ONIX Returns conditions code
type OnixReturnsConditionsCode string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *OnixReturnsConditionsCode) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// Unspecified, contact supplier for details
	case "00":
		*c = `Unspecified`

	// The retailer pays for goods only after they are sold by the retailer to an end consumer, and may return excess unsold inventory to the supplier at any time. The goods remain the property of the supplier until they are paid for, even while they are physically located at the retailer
	case "01":
		*c = `Consignment`

	// The retailer is invoiced and pays immediately as in the sale or return model, but any excess unsold inventory cannot be returned to the supplier
	case "02":
		*c = `Firm sale`

	// Contact supplier for applicable returns authorization process. The retailer is invoiced immediately for the goods and pays within the specified credit period, but can return excess unsold inventory to the supplier for full credit at a later date (some kind of returns authorisation process is normally required, and returns of stripped covers or proof of destruction may be allowed instead)
	case "03":
		*c = `Sale or return`

	// The retailer pays for goods only after they are sold by the retailer to an end consumer, but all inventory remains physically located at the supplier (thus there can be no returns of unsold inventory). When ordered by the retailer, the goods are delivered direct to the consumer
	case "04":
		*c = `Direct fulfillment`
	default:
		return fmt.Errorf("undefined code for OnixReturnsConditionsCode has been passed, got [%s]", v)
	}
	return nil
}

// EditionTypeCode List 21: Edition type code
type EditionTypeCode string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *EditionTypeCode) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// Content has been shortened: use for abridged, shortened, concise, condensed
	case "ABR":
		*c = `Abridged edition`

	// Version of a play or script intended for use of those directly involved in a production, usually including full stage directions in addition to the text of the script
	case "ACT":
		*c = `Acting edition`

	// Content has been adapted to serve a different purpose or audience, or from one medium to another: use for dramatization, novelization etc. Use <EditionStatement> to describe the exact nature of the adaptation
	case "ADP":
		*c = `Adapted edition`

	// Do not use. This code is now DEPRECATED, but is retained in the list for reasons of backwards compatibility
	case "ALT":
		*c = `Alternate`

	// Content is augmented by the addition of notes
	case "ANN":
		*c = `Annotated edition`

	// Both languages should be specified in the <Language> group. Use MLL for an edition in more than two languages
	case "BLL":
		*c = `Bilingual edition`

	// Use only where the two languages are presented in parallel on facing pages, or in parallel columns of text on a single page (otherwise use BLL). Both languages should be specified in the <Language> group
	case "BLP":
		*c = `Bilingual ‘facing page’ edition`

	// Braille edition
	case "BRL":
		*c = `Braille edition`

	// An edition in which two or more works also published separately are combined in a single volume; AKA ‘omnibus’ edition
	case "CMB":
		*c = `Combined volume`

	// Content includes critical commentary on the text
	case "CRI":
		*c = `Critical edition`

	// Content was compiled for a specified educational course
	case "CSP":
		*c = `Coursepack`

	// A digital product that, at the time of publication, has or had no print counterpart and that is or was not expected to have a print counterpart for a reasonable time (recommended at least 30 days following publication)
	case "DGO":
		*c = `Digital original`

	// Use for e-publications that have been enhanced with additional text, speech, other audio, video, interactive or other content
	case "ENH":
		*c = `Enhanced edition`

	// Content has been enlarged or expanded from that of a previous edition
	case "ENL":
		*c = `Enlarged edition`

	// ‘Offensive’ content has been removed
	case "EXP":
		*c = `Expurgated edition`

	// Exact reproduction of the content and format of a previous edition
	case "FAC":
		*c = `Facsimile edition`

	// A collection of writings published in honor of a person, an institution or a society
	case "FST":
		*c = `Festschrift`

	// Content includes extensive illustrations which are not part of other editions
	case "ILL":
		*c = `Illustrated edition`

	// A product aimed specifically at markets other than the country of original publication, usually titled as an ‘International edition’ and with specification and/or content changes
	case "INT":
		*c = `International edition`

	// Large print edition, print sizes 14 to 19pt – see also ULP
	case "LTE":
		*c = `Large type / large print edition`

	// A printed edition in a type size too small to be read without a magnifying glass
	case "MCP":
		*c = `Microprint edition`

	// An edition published to coincide with the release of a film, TV program, or electronic game based on the same work. Use <EditionStatement> to describe the exact nature of the tie-in
	case "MDT":
		*c = `Media tie-in`

	// All languages should be specified in the ‘Language’ group. Use BLL for a bilingual edition
	case "MLL":
		*c = `Multilingual edition`

	// Where no other information is given, or no other coded type is applicable
	case "NED":
		*c = `New edition`

	// A limited edition in which each copy is individually numbered. Use <EditionStatement> to give details of the number of copies printed
	case "NUM":
		*c = `Edition with numbered copies`

	// In the US, a book that was previously bound, normally as a paperback, and has been rebound with a library-quality hardcover binding by a supplier other than the original publisher. See also the <Publisher> and <RelatedProduct> composites for other aspects of the treatment of prebound editions in ONIX
	case "PRB":
		*c = `Prebound edition`

	// Content has been revised from that of a previous edition
	case "REV":
		*c = `Revised edition`

	// An edition intended specifically for use in schools
	case "SCH":
		*c = `School edition`

	// Individually autographed by the author(s)
	case "SIG":
		*c = `Signed edition`

	// An edition that uses simplified language (Finnish ‘Selkokirja’)
	case "SMP":
		*c = `Simplified language edition`

	// Use for anniversary, collectors’, de luxe, gift, limited (but prefer codes NUM or UNN as appropriate), autographed (but prefer code SIG as appropriate) edition. Use <EditionStatement> to describe the exact nature of the special edition
	case "SPE":
		*c = `Special edition`

	// Where a text is available in both student and teacher’s editions
	case "STU":
		*c = `Student edition`

	// Where a text is available in both student and teacher’s editions; use also for instructor’s or leader’s editions, and for editions intended exclusively for educators where no specific student edition is available
	case "TCH":
		*c = `Teacher’s edition`

	// Where a title has also been published in an abridged edition; also for audiobooks, regardless of whether an abridged audio version also exists
	case "UBR":
		*c = `Unabridged edition`

	// For print sizes 20pt and above, and with typefaces designed for the visually impaired – see also LTE
	case "ULP":
		*c = `Ultra large print edition`

	// A limited edition in which each copy is not individually numbered – but where the actual number of copies is strictly limited. Use <EditionStatement> to give details of the number of copies printed
	case "UNN":
		*c = `Edition with unnumbered copies`

	// Content previously considered ‘offensive’ has been restored
	case "UXP":
		*c = `Unexpurgated edition`

	// Content includes notes by various commentators, and/or includes and compares several variant texts of the same work
	case "VAR":
		*c = `Variorum edition`
	default:
		return fmt.Errorf("undefined code for EditionTypeCode has been passed, got [%s]", v)
	}
	return nil
}

// Proximity List 215: Proximity
type Proximity string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *Proximity) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	//
	case "01":
		*c = `Less than`

	//
	case "02":
		*c = `Not more than`

	// The supplier’s true figure, or at least a best estimate expected to be within 10% of the true figure (ie a quoted figure of 100 could in fact be anything between 91 and 111)
	case "03":
		*c = `Exactly`

	// Generally interpreted as within 25% of the true figure (ie a quoted figure of 100 could in fact be anything between 80 and 133). The supplier may introduce a deliberate approximation to reduce the commercial sensitivity of the figure
	case "04":
		*c = `Approximately`

	// Generally interpreted as within a factor of two of the true figure (ie a quoted figure of 100 could in fact be anything between 50 and 200). The supplier may introduce a deliberate approximation to reduce the commercial sensitivity of the figure
	case "05":
		*c = `About`

	//
	case "06":
		*c = `Not less than`

	//
	case "07":
		*c = `More than`
	default:
		return fmt.Errorf("undefined code for Proximity has been passed, got [%s]", v)
	}
	return nil
}

// Velocity List 216: Velocity
type Velocity string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *Velocity) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// Typically measured over most recent 1 month period
	case "01":
		*c = `Mean daily sale`

	// Typically measured over most recent 1 month period
	case "02":
		*c = `Maximum daily sale`

	// Typically measured over most recent 1 month period
	case "03":
		*c = `Minimum daily sale`

	// Typically measured over most recent rolling 12 week period
	case "04":
		*c = `Mean weekly sale`

	// Typically measured over most recent rolling 12 week period
	case "05":
		*c = `Maximum weekly sale`

	// Typically measured over most recent rolling 12 week period
	case "06":
		*c = `Minimum weekly sale`

	// Typically measured over most recent rolling 6 month period
	case "07":
		*c = `Mean monthly sale`

	// Typically measured over the most recent rolling 6 month period
	case "08":
		*c = `Maximum monthly sale`

	// Typically measured over the most recent rolling 6 month period
	case "09":
		*c = `Minimum monthly sale`
	default:
		return fmt.Errorf("undefined code for Velocity has been passed, got [%s]", v)
	}
	return nil
}

// PriceIdentifierTypeCode List 217: Price identifier type code
type PriceIdentifierTypeCode string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *PriceIdentifierTypeCode) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// Note that <IDTypeName> is required for proprietary identifiers
	case "01":
		*c = `Proprietary`

	// Proprietary identifier uniquely identifies price amount and currency. Two unrelated products with the same price amount carry the same identifier, though their price types may be different
	case "02":
		*c = `Proprietary price point identifier`

	// Proprietary identifier uniquely identifies price type, qualifier and any constraints and conditions. Two unrelated products with the same price type carry the same identifier, though their price points may be different
	case "03":
		*c = `Proprietary price type identifier`

	// Proprietary identifier identifies a unique combination of price point and type, though two unrelated products may carry the same identifier if all details of their prices are identical
	case "04":
		*c = `Proprietary price point and type identifier`

	// Proprietary identifier is unique to a single price point, price type and product. No two products can carry the same identifier, even if all details of their prices are identical
	case "05":
		*c = `Proprietary unique price identifier`

	// Proprietary identifier uniquely identifies a specific combination of product, price amount and currency, independent of the price type
	case "06":
		*c = `Proprietary product price point identifier`

	// Proprietary identifier uniquely identifies a specific combination of product, price type, qualifier and any constraints and conditions, independent of the price amount and currency. A product with the same product price type identififer may carry differing price amounts, currencies at different points in time
	case "07":
		*c = `Proprietary product price type identifier`
	default:
		return fmt.Errorf("undefined code for PriceIdentifierTypeCode has been passed, got [%s]", v)
	}
	return nil
}

// LicenseExpressionTypeCode List 218: License expression type code
type LicenseExpressionTypeCode string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *LicenseExpressionTypeCode) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// Document (eg Word file, PDF or web page) Intended for the lay reader
	case "01":
		*c = `Human readable`

	// Document (eg Word file, PDF or web page) Intended for the legal specialist reader
	case "02":
		*c = `Professional readable`

	//
	case "10":
		*c = `ONIX-PL`
	default:
		return fmt.Errorf("undefined code for LicenseExpressionTypeCode has been passed, got [%s]", v)
	}
	return nil
}

// RightsTypeCode List 219: Rights type code
type RightsTypeCode string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *RightsTypeCode) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// Text or image copyright (normally indicated by the © symbol)
	case "C":
		*c = `Copyright`

	// Phonogram copyright or neighbouring right (normally indicated by the ℗ symbol)
	case "P":
		*c = `Phonogram right`

	// Sui generis database right
	case "D":
		*c = `Database right`
	default:
		return fmt.Errorf("undefined code for RightsTypeCode has been passed, got [%s]", v)
	}
	return nil
}

// LanguageRoleCode List 22: Language role code
type LanguageRoleCode string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *LanguageRoleCode) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	//
	case "01":
		*c = `Language of text`

	// Where the text in the original language is NOT part of the current product
	case "02":
		*c = `Original language of a translated text`

	// Where different from language of text: used mainly for serials
	case "03":
		*c = `Language of abstracts`

	// Language to which specified rights apply
	case "04":
		*c = `Rights language`

	// Language to which specified rights do not apply
	case "05":
		*c = `Rights-excluded language`

	// Where the text in the original language is part of a bilingual or multilingual product
	case "06":
		*c = `Original language in a multilingual edition`

	// Where the text in a translated language is part of a bilingual or multilingual product
	case "07":
		*c = `Translated language in a multilingual edition`

	// For example, on a DVD. Use for the only available audio track, or for an alternate language audio track when the original language audio is also present (code 11), or is missing (code 10)
	case "08":
		*c = `Language of audio track`

	// For example, on a DVD
	case "09":
		*c = `Language of subtitles`

	// Where the audio in the original language is NOT part of the current product
	case "10":
		*c = `Language of original audio track`

	// Where the audio in the original language is part of a multilingual product with multiple audio tracks
	case "11":
		*c = `Original language audio track in a multilingual product`

	// Use for the language of footnotes, endnotes, annotations or commentary, where it is different from the language of the main text
	case "12":
		*c = `Language of notes`
	default:
		return fmt.Errorf("undefined code for LanguageRoleCode has been passed, got [%s]", v)
	}
	return nil
}

// EPublicationVersionNumber List 220: E-publication version number
type EPublicationVersionNumber string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *EPublicationVersionNumber) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// Use only with <ProductFormDetail> codes E101 or E102
	case "101A":
		*c = `EPUB 2.0.1`

	// Use only with <ProductFormDetail> code E101
	case "101B":
		*c = `EPUB 3.0`

	// Use only with <ProductFormDetail> code E101
	case "101C":
		*c = `EPUB 3.0.1`

	// Use only with <ProductFormDetail> code E101
	case "101D":
		*c = `EPUB 3.1`

	// Use only with <ProductFormDetail> codes E116 or E127
	case "116A":
		*c = `Kindle mobi 7`

	// Use only with <ProductFormDetail> code E116
	case "116B":
		*c = `Kindle KF8`
	default:
		return fmt.Errorf("undefined code for EPublicationVersionNumber has been passed, got [%s]", v)
	}
	return nil
}

// MessageStatus List 221: Message status
type MessageStatus string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *MessageStatus) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// Message received but not yet parsed (Acknowledgement must contain neither <MessageStatusDetail> nor <RecordStatusSummary>, and should include <NoProduct/>). There is no particular implication that the acknowledgement message is valid – the status is based solely on receipt of a file and minimal parsing of the original ONIX message header to ascertain <MessageNumber> etc.. The Acknowledgement message MAY give a date when parsing is planned
	case "00":
		*c = `Message received`

	// Entire original ONIX message rejected (ie NONE of the data records have been ingested). The status of any recognisable records MAY be summarised in the remainder of the Acknowledgement Message
	case "01":
		*c = `Message rejected`

	// Original ONIX message partially parsed (ie at least SOME of the data records have been ingested, in whole or in part). Records processed to date MUST be summarised in the remainder of the Acknowledgement Message
	case "02":
		*c = `Message part-processed`

	// Original ONIX message parsed and processed in full, and at least SOME of the data records have been ingested, in whole or in part), Results MUST be summarised in the remainder of the Acknowledgement Message
	case "03":
		*c = `Message processed`
	default:
		return fmt.Errorf("undefined code for MessageStatus has been passed, got [%s]", v)
	}
	return nil
}

// MessageStatusDateRole List 222: Message status date role
type MessageStatusDateRole string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *MessageStatusDateRole) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// Expected or actual date of processing and ingestion of data to recipient’s system
	case "01":
		*c = `Ingest date`

	// Expected or actual date for data to be available from the recipient’s system to downstream supply chain partners (or where the recipient is a retailer, to consumers)
	case "02":
		*c = `Export date`
	default:
		return fmt.Errorf("undefined code for MessageStatusDateRole has been passed, got [%s]", v)
	}
	return nil
}

// StatusDetailCodeType List 223: Status detail code type
type StatusDetailCodeType string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *StatusDetailCodeType) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	//
	case "01":
		*c = `Proprietary`

	// Status detail code is taken from List 225
	case "02":
		*c = `ONIX Status detail code`
	default:
		return fmt.Errorf("undefined code for StatusDetailCodeType has been passed, got [%s]", v)
	}
	return nil
}

// StatusDetailTypeSeverity List 224: Status detail type severity
type StatusDetailTypeSeverity string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *StatusDetailTypeSeverity) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// Use ONLY if the message severity cannot be determined (eg with a legacy system unable to provide detailed error codes)
	case "U":
		*c = `Unclassifiable`

	// For information only, provided to encourage the ONIX data supplier to improve the quality of their data (eg provision of a non-mandatory data element that is currently missing, better conformity with best practice, etc)
	case "I":
		*c = `Info`

	// Request for clarification or further detail of the meaning of the data provided
	case "Q":
		*c = `Query`

	// The ONIX data has been accepted as provided, but there may be issues in the way it is supplied
	case "W":
		*c = `Warning`

	// Some data in an ONIX data element or message structure caused an error due to not meeting the requirements of the standard. The data in question has been rejected, but processing of the remaining elements in the record (or the remain records in the message, if used within <MessageStatusDetail>) has continued
	case "E":
		*c = `Error`

	// Some data in an ONIX data element or message structure caused an unrecoverable error due to not meeting the requirements of the standard. The entire ONIX record has been rejected (or the entire message, if used within <MessageStatusDetail>)
	case "F":
		*c = `Fatal error`
	default:
		return fmt.Errorf("undefined code for StatusDetailTypeSeverity has been passed, got [%s]", v)
	}
	return nil
}

// MessageRecordStatusDetailCode List 225: Message / Record status detail code
type MessageRecordStatusDetailCode string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *MessageRecordStatusDetailCode) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	//
	case "000":
		*c = `Unknown error`

	//
	case "001":
		*c = `Unknown warning`
	default:
		return fmt.Errorf("undefined code for MessageRecordStatusDetailCode has been passed, got [%s]", v)
	}
	return nil
}

// RecordStatus List 226: Record status
type RecordStatus string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *RecordStatus) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// Entire record parsed and ingested without errors, record may have a Product record in the Acknowledgement which itself may have a <RecordStatusNote> or <RecordStatusDetail> to convey information, editorial queries or warnings
	case "00":
		*c = `No record errors`

	// Entire record parsed and ingested without errors, record MUST have a Product record in the Acknowledgement with a <RecordStatusNote> or at least one <RecordStatusDetail> to convey errors in associated media files (and possibly supplementary editorial queries)
	case "01":
		*c = `No record errors – errors in collateral`

	// Record parsed and ingested with errors, record MUST have a Product record in the Acknowledgement with a <RecordStatusNote> or at least one <RecordStatusDetail> to convey errors (and possibly supplementary information, editorial queries or warnings). At least SOME of the data in the original Product record has been ingested. There may also be errors in associated media files
	case "02":
		*c = `Record with errors`

	// Entire record rejected, record MUST have a Product record in the Acknowedgemet, with a <RecordStatusNote> or at least one <RecordStatusDetail> to convey errors (and possibly supplementary information, editorial queries or warnings). NONE of the data in the original Product record has been ingested
	case "03":
		*c = `Record rejected`

	// Record status reported in an earlier Acknowledgement message, based on partial processing of ONIX message. The record MUST NOT have a Product record in this Acknowledgement. Code not valid in <RecordStatusDetail>
	case "09":
		*c = `Reported previously`
	default:
		return fmt.Errorf("undefined code for RecordStatus has been passed, got [%s]", v)
	}
	return nil
}

// ChineseSchoolGradeCode List 227: Chinese School Grade Code
type ChineseSchoolGradeCode string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *ChineseSchoolGradeCode) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// Typical age less than 3
	case "P":
		*c = `Preschool`

	// Typical age 3–5
	case "K":
		*c = `Kindergarten`

	// Typical age 6
	case "1":
		*c = `Primary school First grade`

	// Typical age 7
	case "2":
		*c = `Primary school Second grade`

	// Typical age 8
	case "3":
		*c = `Primary school Third grade`

	// Typical age 9
	case "4":
		*c = `Primary school Fourth grade`

	// Typical age 10
	case "5":
		*c = `Primary school Fifth grade`

	// Typical age 11
	case "6":
		*c = `Primary school Sixth grade`

	// Typical age 12
	case "7":
		*c = `Junior secondary school Seventh grade`

	// Typical age 13
	case "8":
		*c = `Junior secondary school Eighth grade`

	// Typical age 14
	case "9":
		*c = `Junior secondary school Ninth grade`

	// Typical age 15
	case "10":
		*c = `Senior secondary school Tenth grade`

	// Typical age 16
	case "11":
		*c = `Senior secondary school Eleventh grade`

	// Typical age 17
	case "12":
		*c = `Senior secondary school Twelfth grade`

	// Typical age 18
	case "13":
		*c = `University first year`

	// Typical age 19
	case "14":
		*c = `University second year`

	// Typical age 20
	case "15":
		*c = `University third year`

	// Typical age 21
	case "16":
		*c = `University fourth year`

	// Typical age 22+
	case "17":
		*c = `Graduate level`
	default:
		return fmt.Errorf("undefined code for ChineseSchoolGradeCode has been passed, got [%s]", v)
	}
	return nil
}

// GrantIdentifierTypeCode List 228: Grant identifier type code
type GrantIdentifierTypeCode string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *GrantIdentifierTypeCode) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// Note that <IDTypeName> is required with proprietary identifiers
	case "01":
		*c = `Proprietary`
	default:
		return fmt.Errorf("undefined code for GrantIdentifierTypeCode has been passed, got [%s]", v)
	}
	return nil
}

// GenderCodeBasedOnIso5218 List 229: Gender code – based on ISO 5218
type GenderCodeBasedOnIso5218 string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *GenderCodeBasedOnIso5218) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// Provides positive indication that the gender is not known or is not specified by the sender for any reason
	case "u":
		*c = `Unknown or unspecified`

	//
	case "f":
		*c = `Female`

	//
	case "m":
		*c = `Male`
	default:
		return fmt.Errorf("undefined code for GenderCodeBasedOnIso5218 has been passed, got [%s]", v)
	}
	return nil
}

// ExtentTypeCode List 23: Extent type code
type ExtentTypeCode string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *ExtentTypeCode) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// The highest-numbered page in a single numbered sequence of main content, usually the highest Arabic-numbered page in a book; or, for books without page numbers or (rarely) with multiple numbered sequences of main content, the total number of pages that carry the main content of the book. Note that this may include numbered but otherwise blank pages (eg pages inserted to ensure chapters start on a recto page) and may exclude unnumbered (but contentful) pages such as those in inserts/plate sections. It should exclude pages of back matter (eg any index) even when their numbering sequence continues from the main content. Either this or the Content Page count is the preferred page count for most books for the general reader. For books with substantial front and/or back matter, include also Front matter (03) and Back matter (04) page counts, or Total numbered pages (05). For books with inserts (plate sections), also include Total unnumbered insert page count whenever possible
	case "00":
		*c = `Main content page count`

	// Number of words of natural language text
	case "02":
		*c = `Number of words`

	// The total number of numbered (usually Roman-numbered) pages that precede the main content of a book. This usually consists of various title and imprint pages, table of contents, an introduction, preface, foreword, etc
	case "03":
		*c = `Front matter page count`

	// The total number of numbered (often Roman-numbered) pages that follow the main content of a book. This usually consists of an afterword, appendices, endnotes, index, etc. It excludes extracts or ‘teaser’ material from other works, and blank (or advertising) pages that are present only for convenience of printing and binding
	case "04":
		*c = `Back matter page count`

	// The sum of all Roman- and Arabic-numbered pages. Note that this may include numbered but otherwise blank pages (eg pages inserted to ensure chapters start on a recto page) and may exclude unnumbered (but contentful) pages such as those in inserts/plate sections. It is the sum of the main content (00), front matter (03) and back matter (04) page counts
	case "05":
		*c = `Total numbered pages`

	// The total number of pages in a book, including unnumbered pages, front matter, back matter, etc. This includes any extracts or ‘teaser’ material from other works, and blank pages at the back that carry no content and are present only for convenience of printing and binding
	case "06":
		*c = `Production page count`

	// The total number of pages of the book counting the cover as page 1. This page count type should be used only for digital publications delivered with fixed pagination
	case "07":
		*c = `Absolute page count`

	// The total number of pages (equivalent to the Content page count) in the print counterpart of a digital product delivered without fixed pagination
	case "08":
		*c = `Number of pages in print counterpart`

	// Total duration in time, expressed in the specified extent unit. This is the ‘running time’ equivalent of codes 05 or 11
	case "09":
		*c = `Duration`

	// An estimate of the number of ‘pages’ in a digital product delivered without fixed pagination, and with no print counterpart, given as an indication of the size of the work. Equivalent to code 08, but for exclusively digital products
	case "10":
		*c = `Notional number of pages in digital product`

	// The sum of all Roman- and Arabic-numbered and contentful unnumbered pages. Sum of page counts with codes 00, 03, 04 and 12, and also the sum of 05 and 12
	case "11":
		*c = `Content page count`

	// The total number of unnumbered pages with content inserted within the main content of a book – for example inserts/plate sections that are not numbered
	case "12":
		*c = `Total unnumbered insert page count`

	// Duration in time, expressed in the specified extent units, of introductory matter. This is the ‘running time’ equivalent of code 03, and comprises any significant amount of running time represented by announcements, titles, introduction or other material prefacing the main content
	case "13":
		*c = `Duration of introductory matter`

	// Duration in time, expressed in the specified extent units, of the main content. This is the ‘running time’ equivalent of code 00, and excludes time represented by announcements, titles, introduction or other prefatory material or ‘back matter’
	case "14":
		*c = `Duration of main content`

	// Duration in time, expressed in the specified extent units, of any content that follows the main content of a book. This usually consists of an afterword, appendices, endnotes etc. It excludes extracts or ‘teaser’ material from other works. This is the ‘running time’ equivalent of code 04
	case "15":
		*c = `Duration of back matter`

	// Duration in time, expressed in the specified extent units, of the complete content of a book. This is the ‘running time’ equivalent of code 06, and includes time represented by announcements, titles, introductory and other prefatory material, plus ‘back matter’ such as any afterword, appendices, and any extracts or ‘teaser’ material from other works
	case "16":
		*c = `Production duration`

	// The size of a digital file, expressed in the specified extent unit
	case "22":
		*c = `Filesize`
	default:
		return fmt.Errorf("undefined code for ExtentTypeCode has been passed, got [%s]", v)
	}
	return nil
}

// PriceConstraintType List 230: Price constraint type
type PriceConstraintType string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *PriceConstraintType) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// Allows positive indication that there are no conditions (the default if <PriceConstraint> is omitted)
	case "00":
		*c = `No constraints`

	// Lendable by the purchaser to other device owner, account holder or patron, eg library lending (use where the library product is not identified with a separate <ProductIdentifier> from the consumer product). The ‘primary’ copy becomes unusable while the secondary copy is on loan, unless a number of concurrent borrowers is also specified
	case "06":
		*c = `Lend`

	// E-publication license is time-limited. Use with code 02 from List 146 and a time period in days, weeks or months in <PriceConstraintLimit>. The purchased copy becomes unusable when the license expires
	case "07":
		*c = `Time-limited license`

	// Maximum number of consecutive loans (eg from a library) to a single device owner, account holder or patron. Note that a limit of 1 indicates that a loan cannot be renewed
	case "08":
		*c = `Loan renewal`

	// E-publication license is multi-user. Maximum number of concurrent users licensed to use the product should be given in <PriceConstraintLimit>
	case "09":
		*c = `Multi-user license`
	default:
		return fmt.Errorf("undefined code for PriceConstraintType has been passed, got [%s]", v)
	}
	return nil
}

// ExtentUnitCode List 24: Extent unit code
type ExtentUnitCode string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *ExtentUnitCode) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// Words of natural language text
	case "02":
		*c = `Words`

	//
	case "03":
		*c = `Pages`

	//
	case "04":
		*c = `Hours (integer and decimals)`

	//
	case "05":
		*c = `Minutes (integer and decimals)`

	//
	case "06":
		*c = `Seconds (integer only)`

	// Of an audiobook on CD (or a similarly divided selection of audio files). Conventionally, each track is 3–6 minutes of running time, and track counts are misleading and inappropriate if the average track duration is significantly more or less than this. Note that track breaks are not necessarily aligned with structural breaks in the text (eg chapter breaks)
	case "11":
		*c = `Tracks`

	// Fill with leading zeroes if any elements are missing
	case "14":
		*c = `Hours HHH`

	// Fill with leading zeroes if any elements are missing
	case "15":
		*c = `Hours and minutes HHHMM`

	// Fill with leading zeroes if any elements are missing
	case "16":
		*c = `Hours minutes seconds HHHMMSS`

	//
	case "17":
		*c = `Bytes`

	//
	case "18":
		*c = `Kbytes`

	//
	case "19":
		*c = `Mbytes`
	default:
		return fmt.Errorf("undefined code for ExtentUnitCode has been passed, got [%s]", v)
	}
	return nil
}

// IllustrationAndOtherContentTypeCode List 25: Illustration and other content type code
type IllustrationAndOtherContentTypeCode string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *IllustrationAndOtherContentTypeCode) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// See description in the <IllustrationTypeDescription> element
	case "00":
		*c = `Unspecified, see description`

	//
	case "01":
		*c = `Illustrations, black and white`

	//
	case "02":
		*c = `Illustrations, color`

	// Including black and white photographs
	case "03":
		*c = `Halftones, black and white`

	// Including color photographs
	case "04":
		*c = `Halftones, color`

	//
	case "05":
		*c = `Line drawings, black and white`

	//
	case "06":
		*c = `Line drawings, color`

	//
	case "07":
		*c = `Tables, black and white`

	//
	case "08":
		*c = `Tables, color`

	//
	case "09":
		*c = `Illustrations, unspecified`

	// Including photographs
	case "10":
		*c = `Halftones, unspecified`

	//
	case "11":
		*c = `Tables, unspecified`

	//
	case "12":
		*c = `Line drawings, unspecified`

	//
	case "13":
		*c = `Halftones, duotone`

	//
	case "14":
		*c = `Maps`

	//
	case "15":
		*c = `Frontispiece`

	//
	case "16":
		*c = `Diagrams`

	//
	case "17":
		*c = `Figures`

	//
	case "18":
		*c = `Charts`

	// Recorded music extracts or examples, or complete recorded work(s), accompanying textual or other content
	case "19":
		*c = `Recorded music items`

	// Printed music extracts or examples, or complete music score(s), accompanying textual or other content
	case "20":
		*c = `Printed music items`

	// To be used in the mathematical sense of a diagram that represents numerical values plotted against an origin and axes, cf codes 16 and 18
	case "21":
		*c = `Graphs`

	// ‘Plates’ means illustrations that are on separate pages bound into the body of a book
	case "22":
		*c = `Plates, unspecified`

	// ‘Plates’ means illustrations that are on separate pages bound into the body of a book
	case "23":
		*c = `Plates, black and white`

	// ‘Plates’ means illustrations that are on separate pages bound into the body of a book
	case "24":
		*c = `Plates, color`

	//
	case "25":
		*c = `Index`

	//
	case "26":
		*c = `Bibliography`

	// Larger-scale inset maps of places or features of interest included in a map product
	case "27":
		*c = `Inset maps`

	// GPS grids included in a map product
	case "28":
		*c = `GPS grids`

	//
	case "29":
		*c = `Glossary`
	default:
		return fmt.Errorf("undefined code for IllustrationAndOtherContentTypeCode has been passed, got [%s]", v)
	}
	return nil
}

// MainSubjectSchemeIdentifierCode List 26: Main subject scheme identifier code
type MainSubjectSchemeIdentifierCode string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *MainSubjectSchemeIdentifierCode) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// Dewey Decimal Classification
	case "01":
		*c = `Dewey`

	//
	case "02":
		*c = `Abridged Dewey`

	// US Library of Congress classification
	case "03":
		*c = `LC classification`

	// US Library of Congress subject heading
	case "04":
		*c = `LC subject heading`

	// US National Library of Medicine medical classification
	case "05":
		*c = `NLM classification`

	// US National Library of Medicine Medical subject heading
	case "06":
		*c = `MeSH heading`

	// US National Agricultural Library subject heading
	case "07":
		*c = `NAL subject heading`

	// Getty Art and Architecture Thesaurus heading
	case "08":
		*c = `AAT`

	// Universal Decimal Classification
	case "09":
		*c = `UDC`

	// BISAC Subject Headings are used in the North American market to categorize books based on topical content. They serve as a guideline for shelving books in physical stores and browsing books in online stores. See https://www.bisg.org/complete-bisac-subject-headings-2013-edition
	case "10":
		*c = `BISAC Subject Heading`

	// A geographical qualifier used with a BISAC subject category
	case "11":
		*c = `BISAC region code`

	// For all BIC subject codes and qualifiers, see http://www.bic.org.uk/7/BIC-Standard-Subject-Categories/
	case "12":
		*c = `BIC subject category`

	//
	case "13":
		*c = `BIC geographical qualifier`

	//
	case "14":
		*c = `BIC language qualifier (language as subject)`

	//
	case "15":
		*c = `BIC time period qualifier`

	//
	case "16":
		*c = `BIC educational purpose qualifier`

	//
	case "17":
		*c = `BIC reading level and special interest qualifier`

	// Used for German National Bibliography since 2004 (100 subjects). Is different from value 30. See http://www.d-nb.de/service/pdf/ddc_wv_aktuell.pdf (in German) or http://www.d-nb.de/eng/service/pdf/ddc_wv_aktuell_eng.pdf (English)
	case "18":
		*c = `DDC-Sachgruppen der Deutschen Nationalbibliografie`

	//
	case "19":
		*c = `LC fiction genre heading`

	// For indexing and search purposes, not normally intended for display. Where multiple keywords or keyword phrases are sent, this should be in a single instance of the <SubjectHeadingText> element, and it is recommended that they should be separated by semi-colons (this is consistent with Library of Congress preferred practice)
	case "20":
		*c = `Keywords`

	// See http://www.bic.org.uk/8/Children’s-Books-Marketing-Classifications/
	case "21":
		*c = `BIC children’s book marketing category`

	// BISAC Merchandising Themes are used in addition to BISAC Subject Headings to denote an audience to which a work may be of particular appeal, a time of year or event for which a work may be especially appropriate, or to further describe fictional works that have been subject-coded by genre
	case "22":
		*c = `BISAC Merchandising Theme`

	//
	case "23":
		*c = `Publisher’s own category code`

	// As specified in <SubjectSchemeName>
	case "24":
		*c = `Proprietary subject scheme`

	// Latin America
	case "25":
		*c = `Tabla de materias ISBN`

	// See http://info.vlb.de/files/wgsneuversion2_0.pdf (in German)
	case "26":
		*c = `Warengruppen-Systematik des deutschen Buchhandels`

	// Schlagwortnormdatei – Subject Headings Authority File in the German-speaking countries. See http://www.d-nb.de/standardisierung/normdateien/swd.htm (in German) and http://www.d-nb.de/eng/standardisierung/normdateien/swd.htm (English). DEPRECATED in favour of the GND
	case "27":
		*c = `SWD`

	// Subject classification used by Electre (France)
	case "28":
		*c = `Thèmes Electre`

	// France. A four-digit number, see http://www.clil.org/information/documentation.html (in French). The first digit identifies the version of the scheme
	case "29":
		*c = `CLIL`

	// Deutsche Bibliothek subject groups. Used for German National Bibliography until 2003 (65 subjects). Is different from value 18. See http://www.d-nb.de/service/pdf/ddc_wv_alt_neu.pdf
	case "30":
		*c = `DNB-Sachgruppen`

	// Nederlandse Uniforme Genre-Indeling (former Dutch book trade classification)
	case "31":
		*c = `NUGI`

	// Nederlandstalige Uniforme Rubrieksindeling (Dutch book trade classification, from 2002),see http://www.boek.nl/nur (in Dutch)
	case "32":
		*c = `NUR`

	// ECPA Christian Product Category Book Codes, consisting of up to three x 3-letter blocks, for Super Category, Primary Category and Sub-Category. See http://www.ecpa.org/ECPA/cbacategories.xls
	case "33":
		*c = `ECPA Christian Book Category`

	// Schema Indeling Systematische Catalogus Openbare Bibliotheken (Dutch library classification)
	case "34":
		*c = `SISO`

	// A modified Dewey Decimal Classification used in the Republic of Korea
	case "35":
		*c = `Korean Decimal Classification (KDC)`

	// German Translation of Dewey Decimal Classification 22. Also known as DDC 22 ger. See http://www.ddc-deutsch.de/produkte/uebersichten/
	case "36":
		*c = `DDC Deutsch 22`

	// Norwegian book trade product categories (Bokgrupper) administered by the Norwegian Publishers Association (http://www.forleggerforeningen.no/)
	case "37":
		*c = `Bokgrupper`

	// Norwegian bookselling subject categories (Bokhandelens varegrupper) administered by the Norwegian Booksellers Association (http://bokhandlerforeningen.no/)
	case "38":
		*c = `Varegrupper`

	// Norwegian school curriculum version. Deprecated
	case "39":
		*c = `Læreplaner`

	// Japanese subject classification scheme
	case "40":
		*c = `Nippon Decimal Classification`

	// BookSelling Qualifier: Russian book trade classification
	case "41":
		*c = `BSQ`

	// Spain: subject coding scheme of the Asociación Nacional de Editores de Libros y Material de Enseñanza
	case "42":
		*c = `ANELE Materias`

	// Codes for Norwegian ‘utdanningsprogram’ used in secondary education. See: http://www.udir.no/. (Formerly labelled ‘Skolefag’)
	case "43":
		*c = `Utdanningsprogram`

	// Codes for Norwegian ‘programområde’ used in secondary education. See http://www.udir.no/. (Formerly labelled ‘Videregående’ or ‘Programfag’)
	case "44":
		*c = `Programområde`

	// Norwegian list of categories for books and other material used in education
	case "45":
		*c = `Undervisningsmateriell`

	// Norwegian version of Dewey Decimal Classification
	case "46":
		*c = `Norsk DDK`

	// Swedish bookselling subject categories
	case "47":
		*c = `Varugrupper`

	// Swedish classification scheme
	case "48":
		*c = `SAB`

	// Swedish bookselling educational subject type
	case "49":
		*c = `Läromedelstyp`

	// Swedish publishers preliminary subject classification
	case "50":
		*c = `Förhandsbeskrivning`

	// Controlled subset of UDC codes used by the Spanish ISBN Agency
	case "51":
		*c = `Spanish ISBN UDC subset`

	// Subject categories defined by El Corte Inglés and used widely in the Spanish book trade
	case "52":
		*c = `ECI subject categories`

	// Classificazione commerciale editoriale (Italian book trade subject category based on BIC). CCE documentation available at http://www.ie-online.it/CCE2_2.0.pdf
	case "53":
		*c = `Soggetto CCE`

	//
	case "54":
		*c = `Qualificatore geografico CCE`

	//
	case "55":
		*c = `Qualificatore di lingua CCE`

	//
	case "56":
		*c = `Qualificatore di periodo storico CCE`

	//
	case "57":
		*c = `Qualificatore di livello scolastico CCE`

	//
	case "58":
		*c = `Qualificatore di età di lettura CCE`

	// Subject code list of the German association of educational media publishers
	case "59":
		*c = `VdS Bildungsmedien Fächer`

	// Norwegian primary and secondary school subject categories (fagkoder), see http://www.udir.no/
	case "60":
		*c = `Fagkoder`

	// Journal of Economic Literature classification scheme
	case "61":
		*c = `JEL classification`

	// National Library of Canada subject heading (English)
	case "62":
		*c = `CSH`

	// Répertoire de vedettes-matière (Bibliothèque de l’Université Laval) (French)
	case "63":
		*c = `RVM`

	// Yleinen suomalainen asiasanasto: Finnish General Thesaurus. See http://onki.fi/fi/browser/ (in Finnish)
	case "64":
		*c = `YSA`

	// Allmän tesaurus på svenska: Swedish translation of the Finnish General Thesaurus. See http://onki.fi/fi/browser/ (in Finnish)
	case "65":
		*c = `Allärs`

	// Yleisten kirjastojen luokitusjärjestelmä: Finnish Public Libraries Classification System. See http://ykl.kirjastot.fi/ (in Finnish)
	case "66":
		*c = `YKL`

	// Musiikin asiasanasto: Finnish Music Thesaurus. See http://onki.fi/fi/browser/ (in Finnish)
	case "67":
		*c = `MUSA`

	// Specialtesaurus för musik: Swedish translation of the Finnish Music Thesaurus. See http://onki.fi/fi/browser/ (in Finnish)
	case "68":
		*c = `CILLA`

	// Fiktiivisen aineiston asiasanasto: Finnish thesaurus for fiction. See http://kaunokki.kirjastot.fi/ (in Finnish)
	case "69":
		*c = `Kaunokki`

	// Specialtesaurus för fiktivt material: Swedish translation of the Finnish thesaurus for fiction. See http://kaunokki.kirjastot.fi/sv-FI/ (in Finnish)
	case "70":
		*c = `Bella`

	// Yleinen suomalainen ontologia: Finnish General Upper Ontology. See http://onki.fi/fi/browser/ (In Finnish)
	case "71":
		*c = `YSO`

	// Finnish Place Ontology. See http://onki.fi/fi/browser/ (in Finnish)
	case "72":
		*c = `Paikkatieto ontologia`

	// Finnish book trade categorisation
	case "73":
		*c = `Suomalainen kirja-alan luokitus`

	// Sears List of Subject Headings
	case "74":
		*c = `Sears`

	// BIC E4Libraries Category Headings, see http://www.bic.org.uk/51/E4libraries-Subject-Category-Headings/
	case "75":
		*c = `BIC E4L`

	// Code Sujet Rayon: subject categories used by bookstores in France
	case "76":
		*c = `CSR`

	// Finnish school subject categories
	case "77":
		*c = `Suomalainen oppiaineluokitus`

	// See http://www.asahi-net.or.jp/~ax2s-kmtn/ref/ccode.html (in Japanese)
	case "78":
		*c = `Japanese book trade C-Code`

	//
	case "79":
		*c = `Japanese book trade Genre Code`

	// Finnish fiction genre classification. See http://ykl.kirjastot.fi/fi-FI/lisaluokat/ (in Finnish)
	case "80":
		*c = `Fiktiivisen aineiston lisäluokitus`

	// Location defined by postal code. Format is two-letter country code (from List 91), space, postal code. Note some postal codes themselves contain spaces, eg ‘GB N7 9DP’ or ‘US 10125’
	case "85":
		*c = `Postal code`

	// ID number for geographical place, as defined at http://www.geonames.org (eg 2825297 is Stuttgart, Germany, see http://www.geonames.org/2825297)
	case "86":
		*c = `GeoNames ID`

	// Used for classification of academic and specialist publication in German-speaking countries. See http://www.newbooks-services.com/de/top/unternehmensportrait/klassifikation-und-mapping.html (German) and http://www.newbooks-services.com/en/top/about-newbooks/classification-mapping.html (English)
	case "87":
		*c = `NewBooks Subject Classification`

	// Gemeinsame Normdatei – Joint Authority File in the German-speaking countries. See http://www.dnb.de/EN/gnd (English). Combines the PND, SWD and GKD into a single authority file, and should be used in preference to the older codes
	case "91":
		*c = `GND`

	// UK Standard Library Categories, the successor to BIC’s E4L classification scheme
	case "92":
		*c = `BIC UKSLC`

	//
	case "93":
		*c = `Thema subject category`

	//
	case "94":
		*c = `Thema geographical qualifier`

	//
	case "95":
		*c = `Thema language qualifier`

	//
	case "96":
		*c = `Thema time period qualifier`

	//
	case "97":
		*c = `Thema educational purpose qualifier`

	//
	case "98":
		*c = `Thema interest age / special interest qualifier`

	//
	case "99":
		*c = `Thema style qualifier`

	// Swedish subject categories maintained by Bokrondellen
	case "A2":
		*c = `Ämnesord`

	// Polish Statistical Book and E-book Classification
	case "A3":
		*c = `Statystyka Książek Papierowych, Mówionych I Elektronicznych`

	// Common Core State Standards curriculum alignment, for links to US educational standards. <SubjectCode> uses the full dot notation. See http://www.corestandards.org/developers-and-publishers
	case "A4":
		*c = `CCSS`

	// French library subject headings
	case "A5":
		*c = `Rameau`

	// French educational subject classification scolomfr-voc-015, used for example on WizWiz.fr. See http://www.lom-fr.fr/scolomfr/vocabulaires/consultation-des-vocabulaires.html
	case "A6":
		*c = `Nomenclature discipline scolaire`

	// International Standard Industry Classification, a classification of economic activities. Use for books that are about a particular industry or economic activity. <SubjectCode> should be a single letter denoting an ISIC section OR a 2-, 3- or 4-digit number denoting an ISIC division, group or class. See http://unstats.un.org/unsd/cr/registry/isic-4.asp
	case "A7":
		*c = `ISIC`

	// Library of Congress Children’s Subject Headings: LCSHAC supplementary headings for Children’s books
	case "A8":
		*c = `LC Children’s Subject Headings`

	// Swedish bookselling educational subject
	case "A9":
		*c = `Ny Läromedel`

	// EuroVoc multilingual thesaurus. <SubjectCode> should be a EuroVoc concept dc:identifier (for example, 2777, ‘Refrigerated products’). See http://eurovoc.europa.eu
	case "B0":
		*c = `EuroVoc`

	// Controlled vocabulary for educational objectives. See https://www.bisg.org/educational-taxonomy
	case "B1":
		*c = `BISG Educational Taxonomy`
	default:
		return fmt.Errorf("undefined code for MainSubjectSchemeIdentifierCode has been passed, got [%s]", v)
	}
	return nil
}

// SubjectSchemeIdentifierCode List 27: Subject scheme identifier code
type SubjectSchemeIdentifierCode string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *SubjectSchemeIdentifierCode) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// Dewey Decimal Classification
	case "01":
		*c = `Dewey`

	//
	case "02":
		*c = `Abridged Dewey`

	// US Library of Congress classification
	case "03":
		*c = `LC classification`

	// US Library of Congress subject heading
	case "04":
		*c = `LC subject heading`

	// US National Library of Medicine medical classification
	case "05":
		*c = `NLM classification`

	// US National Library of Medicine Medical subject heading
	case "06":
		*c = `MeSH heading`

	// US National Agricultural Library subject heading
	case "07":
		*c = `NAL subject heading`

	// Getty Art and Architecture Thesaurus heading
	case "08":
		*c = `AAT`

	// Universal Decimal Classification
	case "09":
		*c = `UDC`

	// BISAC Subject Headings are used in the North American market to categorize books based on topical content. They serve as a guideline for shelving books in physical stores and browsing books in online stores. See https://www.bisg.org/complete-bisac-subject-headings-2014-edition
	case "10":
		*c = `BISAC Subject Heading`

	// A geographical qualifier used with a BISAC subject category
	case "11":
		*c = `BISAC region code`

	// For all BIC subject codes and qualifiers, see http://www.bic.org.uk/7/BIC-Standard-Subject-Categories/
	case "12":
		*c = `BIC subject category`

	//
	case "13":
		*c = `BIC geographical qualifier`

	//
	case "14":
		*c = `BIC language qualifier (language as subject)`

	//
	case "15":
		*c = `BIC time period qualifier`

	//
	case "16":
		*c = `BIC educational purpose qualifier`

	//
	case "17":
		*c = `BIC reading level and special interest qualifier`

	// Used for German National Bibliography since 2004 (100 subjects). Is different from value 30. See http://www.d-nb.de/service/pdf/ddc_wv_aktuell.pdf (in German) or http://www.d-nb.de/eng/service/pdf/ddc_wv_aktuell_eng.pdf (English)
	case "18":
		*c = `DDC-Sachgruppen der Deutschen Nationalbibliografie`

	//
	case "19":
		*c = `LC fiction genre heading`

	// For indexing and search purposes, not normally intended for display. Where multiple keywords or keyword phrases are sent, this should be in a single instance of the <SubjectHeadingText> element, and it is recommended that they should be separated by semi-colons (this is consistent with Library of Congress preferred practice)
	case "20":
		*c = `Keywords`

	// See http://www.bic.org.uk/8/Children’s-Books-Marketing-Classifications/
	case "21":
		*c = `BIC children’s book marketing category`

	// BISAC Merchandising Themes are used in addition to BISAC Subject Headings to denote an audience to which a work may be of particular appeal, a time of year or event for which a work may be especially appropriate, or to further describe fictional works that have been subject-coded by genre
	case "22":
		*c = `BISAC Merchandising Theme`

	//
	case "23":
		*c = `Publisher’s own category code`

	// As specified in <SubjectSchemeName>
	case "24":
		*c = `Proprietary subject scheme`

	// Latin America
	case "25":
		*c = `Tabla de materias ISBN`

	// See http://info.vlb.de/files/wgsneuversion2_0.pdf (in German)
	case "26":
		*c = `Warengruppen-Systematik des deutschen Buchhandels`

	// Schlagwortnormdatei – Subject Headings Authority File in the German-speaking countries. See http://www.d-nb.de/standardisierung/normdateien/swd.htm (in German) and http://www.d-nb.de/eng/standardisierung/normdateien/swd.htm (English). DEPRECATED in favour of the GND
	case "27":
		*c = `SWD`

	// Subject classification used by Electre (France)
	case "28":
		*c = `Thèmes Electre`

	// France. A four-digit number, see http://www.clil.org/information/documentation.html (in French). The first digit identifies the version of the scheme
	case "29":
		*c = `CLIL`

	// Deutsche Bibliothek subject groups. Used for German National Bibliography until 2003 (65 subjects). Is different from value 18. See http://www.d-nb.de/service/pdf/ddc_wv_alt_neu.pdf (in German)
	case "30":
		*c = `DNB-Sachgruppen`

	// Nederlandse Uniforme Genre-Indeling (former Dutch book trade classification)
	case "31":
		*c = `NUGI`

	// Nederlandstalige Uniforme Rubrieksindeling (Dutch book trade classification, from 2002, see http://www.boek.nl/nur (in Dutch)
	case "32":
		*c = `NUR`

	// ECPA Christian Product Category Book Codes, consisting of up to three x 3-letter blocks, for Super Category, Primary Category and Sub-Category. See http://www.ecpa.org/ECPA/cbacategories.xls
	case "33":
		*c = `ECPA Christian Book Category`

	// Schema Indeling Systematische Catalogus Openbare Bibliotheken (Dutch library classification)
	case "34":
		*c = `SISO`

	// A modified Dewey Decimal Classification used in the Republic of Korea
	case "35":
		*c = `Korean Decimal Classification (KDC)`

	// German Translation of Dewey Decimal Classification 22. Also known as DDC 22 ger. See http://www.ddc-deutsch.de/produkte/uebersichten/
	case "36":
		*c = `DDC Deutsch 22`

	// Norwegian book trade product categories (Bokgrupper) administered by the Norwegian Publishers Association (http://www.forleggerforeningen.no/)
	case "37":
		*c = `Bokgrupper`

	// Norwegian bookselling subject categories (Bokhandelens varegrupper) administered by the Norwegian Booksellers Association (http://bokhandlerforeningen.no/)
	case "38":
		*c = `Varegrupper`

	// Norwegian school curriculum version. Deprecated
	case "39":
		*c = `Læreplaner`

	// Japanese subject classification scheme
	case "40":
		*c = `Nippon Decimal Classification`

	// BookSelling Qualifier: Russian book trade classification
	case "41":
		*c = `BSQ`

	// Spain: subject coding scheme of the Asociación Nacional de Editores de Libros y Material de Enseñanza
	case "42":
		*c = `ANELE Materias`

	// Codes for Norwegian ‘utdanningsprogram’ used in secondary education. See: http://www.udir.no/. (Formerly labelled ‘Skolefag’)
	case "43":
		*c = `Utdanningsprogram`

	// Codes for Norwegian ‘programområde’ used in secondary education. See http://www.udir.no/. (Formerly labelled ‘Videregående’ or ‘Programfag’)
	case "44":
		*c = `Programområde`

	// Norwegian list of categories for books and other material used in education
	case "45":
		*c = `Undervisningsmateriell`

	// Norwegian version of Dewey Decimal Classification
	case "46":
		*c = `Norsk DDK`

	// Swedish bookselling subject categories
	case "47":
		*c = `Varugrupper`

	// Swedish classification scheme
	case "48":
		*c = `SAB`

	// Swedish bookselling educational subject type
	case "49":
		*c = `Läromedelstyp`

	// Swedish publishers preliminary subject classification
	case "50":
		*c = `Förhandsbeskrivning`

	// Controlled subset of UDC codes used by the Spanish ISBN Agency
	case "51":
		*c = `Spanish ISBN UDC subset`

	// Subject categories defined by El Corte Inglés and used widely in the Spanish book trade
	case "52":
		*c = `ECI subject categories`

	// Classificazione commerciale editoriale (Italian book trade subject category based on BIC). CCE documentation available at http://www.ie-online.it/CCE2_2.0.pdf
	case "53":
		*c = `Soggetto CCE`

	// CCE Geographical qualifier
	case "54":
		*c = `Qualificatore geografico CCE`

	// CCE Language qualifier
	case "55":
		*c = `Qualificatore di lingua CCE`

	// CCE Time Period qualifier
	case "56":
		*c = `Qualificatore di periodo storico CCE`

	// CCE Educational Purpose qualifier
	case "57":
		*c = `Qualificatore di livello scolastico CCE`

	// CCE Reading Level Qualifier
	case "58":
		*c = `Qualificatore di età di lettura CCE`

	// Subject code list of the German association of educational media publishers. See http://www.bildungsmedien.de/service/onixlisten/unterrichtsfach_onix_codelist27_value59_0408.pdf
	case "59":
		*c = `VdS Bildungsmedien Fächer`

	// Norwegian primary and secondary school subject categories (fagkoder), see http://www.udir.no/
	case "60":
		*c = `Fagkoder`

	// Journal of Economic Literature classification scheme
	case "61":
		*c = `JEL classification`

	// National Library of Canada subject heading (English)
	case "62":
		*c = `CSH`

	// Répertoire de vedettes-matière Bibliothèque de l’Université Laval) (French)
	case "63":
		*c = `RVM`

	// Yleinen suomalainen asiasanasto: Finnish General Thesaurus. See http://onki.fi/fi/browser/ (in Finnish)
	case "64":
		*c = `YSA`

	// Allmän tesaurus på svenska: Swedish translation of the Finnish General Thesaurus. See http://onki.fi/fi/browser/ (in Finnish)
	case "65":
		*c = `Allärs`

	// Yleisten kirjastojen luokitusjärjestelmä: Finnish Public Libraries Classification System. See http://ykl.kirjastot.fi/ (in Finnish)
	case "66":
		*c = `YKL`

	// Musiikin asiasanasto: Finnish Music Thesaurus. See http://onki.fi/fi/browser/ (in Finnish)
	case "67":
		*c = `MUSA`

	// Specialtesaurus för musik: Swedish translation of the Finnish Music Thesaurus. See http://onki.fi/fi/browser/ (in Finnish)
	case "68":
		*c = `CILLA`

	// Fiktiivisen aineiston asiasanasto: Finnish thesaurus for fiction. See http://kaunokki.kirjastot.fi/ (in Finnish)
	case "69":
		*c = `Kaunokki`

	// Specialtesaurus för fiktivt material: Swedish translation of the Finnish thesaurus for fiction. See http://kaunokki.kirjastot.fi/sv-FI/ (in Finnish)
	case "70":
		*c = `Bella`

	// Yleinen suomalainen ontologia: Finnish General Upper Ontology. See http://onki.fi/fi/browser/ (In Finnish)
	case "71":
		*c = `YSO`

	// Finnish Place Ontology. See http://onki.fi/fi/browser/ (in Finnish)
	case "72":
		*c = `Paikkatieto ontologia`

	// Finnish book trade categorisation
	case "73":
		*c = `Suomalainen kirja-alan luokitus`

	// Sears List of Subject Headings
	case "74":
		*c = `Sears`

	// BIC E4Libraries Category Headings, see http://www.bic.org.uk/51/E4libraries-Subject-Category-Headings/
	case "75":
		*c = `BIC E4L`

	// Code Sujet Rayon: subject categories used by bookstores in France
	case "76":
		*c = `CSR`

	// Finnish school subject categories
	case "77":
		*c = `Suomalainen oppiaineluokitus`

	// See http://www.asahi-net.or.jp/~ax2s-kmtn/ref/ccode.html (in Japanese)
	case "78":
		*c = `Japanese book trade C-Code`

	//
	case "79":
		*c = `Japanese book trade Genre Code`

	// Finnish fiction genre classification. See http://ykl.kirjastot.fi/fi-FI/lisaluokat/ (in Finnish)
	case "80":
		*c = `Fiktiivisen aineiston lisäluokitus`

	//
	case "81":
		*c = `Arabic Subject heading scheme`

	// Arabized version of BIC subject category scheme developed by ElKotob.com
	case "82":
		*c = `Arabized BIC subject category`

	// Arabized version of Library of Congress scheme
	case "83":
		*c = `Arabized LC subject headings`

	// Classification scheme used by Library of Alexandria
	case "84":
		*c = `Bibliotheca Alexandrina Subject Headings`

	// Location defined by postal code. Format is two-letter country code (from List 91), space, postal code. Note some postal codes themselves contain spaces, eg ‘GB N7 9DP’ or ‘US 10125’
	case "85":
		*c = `Postal code`

	// ID number for geographical place, as defined at http://www.geonames.org (eg 2825297 is Stuttgart, Germany, see http://www.geonames.org/2825297)
	case "86":
		*c = `GeoNames ID`

	// Used for classification of academic and specialist publication in German-speaking countries. See http://www.newbooks-services.com/de/top/unternehmensportrait/klassifikation-und-mapping.html (German) and http://www.newbooks-services.com/en/top/about-newbooks/classification-mapping.html (English)
	case "87":
		*c = `NewBooks Subject Classification`

	// Subject classification maintained by the Editorial Board of Chinese Library Classification. See http://cct.nlc.gov.cn for access to details of the scheme
	case "88":
		*c = `Chinese Library Classification`

	// Subject classification for Books, Audiovisual products and E-publications formulated by China National Technical Committee 505
	case "89":
		*c = `NTCPDSAC Classification`

	// German code scheme indicating association with seasons, holidays, events (eg Autumn, Back to School, Easter)
	case "90":
		*c = `Season and Event Indicator`

	// Gemeinsame Normdatei – Joint Authority File in the German-speaking countries. See http://www.dnb.de/EN/gnd (English). Combines the PND, SWD and GKD into a single authority file, and should be used in preference to the older codes
	case "91":
		*c = `GND`

	// UK Standard Library Categories, the successor to BIC’s E4L classification scheme
	case "92":
		*c = `BIC UKSLC`

	//
	case "93":
		*c = `Thema subject category`

	//
	case "94":
		*c = `Thema geographical qualifier`

	//
	case "95":
		*c = `Thema language qualifier`

	//
	case "96":
		*c = `Thema time period qualifier`

	//
	case "97":
		*c = `Thema educational purpose qualifier`

	//
	case "98":
		*c = `Thema interest age / special interest qualifier`

	//
	case "99":
		*c = `Thema style qualifier`

	// Swedish subject categories maintained by Bokrondellen
	case "A2":
		*c = `Ämnesord`

	// Polish Statistical Book and E-book Classification
	case "A3":
		*c = `Statystyka Książek Papierowych, Mówionych I Elektronicznych`

	// Common Core State Standards curriculum alignment, for links to US educational standards. <SubjectCode> uses the full dot notation. See http://www.corestandards.org/developers-and-publishers
	case "A4":
		*c = `CCSS`

	// French library subject headings
	case "A5":
		*c = `Rameau`

	// French educational subject classification scolomfr-voc-015, used for example on WizWiz.fr. See http://www.lom-fr.fr/scolomfr/vocabulaires/consultation-des-vocabulaires.html
	case "A6":
		*c = `Nomenclature discipline scolaire`

	// International Standard Industry Classification, a classification of economic activities. Use for books that are about a particular industry or economic activity. <SubjectCode> should be a single letter denoting an ISIC section OR a 2-, 3- or 4-digit number denoting an ISIC division, group or class. See http://unstats.un.org/unsd/cr/registry/isic-4.asp
	case "A7":
		*c = `ISIC`

	// Library of Congress Children’s Subject Headings: LCSHAC supplementary headings for Children’s books
	case "A8":
		*c = `LC Children’s Subject Headings`

	// Swedish bookselling educational subject
	case "A9":
		*c = `Ny Läromedel`

	// EuroVoc multilingual thesaurus. <SubjectCode> should be a EuroVoc concept dc:identifier (for example, 2777, ‘refrigerated products’). See http://eurovoc.europa.eu
	case "B0":
		*c = `EuroVoc`

	// Controlled vocabulary for educational objectives. See https://www.bisg.org/educational-taxonomy
	case "B1":
		*c = `BISG Educational Taxonomy`

	// For indexing and search purposes, MUST not be displayed. Where multiple keywords or keyword phrases are sent, this should be in a single instance of the <SubjectHeadingText> element, and it is recommended that they should be separated by semi-colons. Use of code B2 should be very rare: use B2 in preference to code 20 only where it is important to show the keyword list is specifically NOT for display to purchasers (eg some keywords for a medical textbook may appear offensive if displayed out of context)
	case "B2":
		*c = `Keywords (not for display)`

	// French higher and vocational educational subject classification scolomfr-voc-29 subject category for degree and diploma study. See http://www.lom-fr.fr/scolomfr/vocabulaires/consultation-des-vocabulaires.html
	case "B3":
		*c = `Nomenclature Diplôme`

	// For fiction only, one or more key names, provided – like keywords – for indexing and search purposes. Where multiple character names are sent, this should be in a single instance of <SubjectHeadingText>, and it is recommended they should be separated by semi-colons
	case "B4":
		*c = `Key character names`
	default:
		return fmt.Errorf("undefined code for SubjectSchemeIdentifierCode has been passed, got [%s]", v)
	}
	return nil
}

// AudienceCode List 28: Audience code
type AudienceCode string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *AudienceCode) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// For a non-specialist adult audience
	case "01":
		*c = `General/trade`

	// For a juvenile audience, not specifically for any educational purpose
	case "02":
		*c = `Children/juvenile`

	// For a teenage audience, not specifically for any educational purpose
	case "03":
		*c = `Young adult`

	// Kindergarten, pre-school, primary/elementary or secondary/high school education
	case "04":
		*c = `Primary and secondary/elementary and high school`

	// For universities and colleges of further and higher education
	case "05":
		*c = `College/higher education`

	// For an expert adult audience, including professional development and academic research
	case "06":
		*c = `Professional and scholarly`

	// Intended for use in teaching English as a second language
	case "07":
		*c = `ELT/ESL`

	// For centres providing academic, vocational or recreational courses for adults
	case "08":
		*c = `Adult education`

	// Intended for use in teaching second languages, for example teaching German to Spanish speakers. Prefer code 07 for products specific to teaching English
	case "09":
		*c = `Second language teaching`
	default:
		return fmt.Errorf("undefined code for AudienceCode has been passed, got [%s]", v)
	}
	return nil
}

// AudienceCodeType List 29: Audience code type
type AudienceCodeType string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *AudienceCodeType) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// Using a code from List 28
	case "01":
		*c = `ONIX audience codes`

	// As specified in <AudienceCodeTypeName>
	case "02":
		*c = `Proprietary`

	// Motion Picture Association of America rating applied to movies
	case "03":
		*c = `MPAA rating`

	// British Board of Film Classification rating applied to movies
	case "04":
		*c = `BBFC rating`

	// German FSK (Freiwillige Selbstkontrolle der Filmwirtschaft) rating applied to movies
	case "05":
		*c = `FSK rating`

	// French Canadian audience code list, used by BTLF for Memento
	case "06":
		*c = `BTLF audience code`

	// Audience code used by Electre (France)
	case "07":
		*c = `Electre audience code`

	// Spain: educational audience and material type code of the Asociación Nacional de Editores de Libros y Material de Enseñanza
	case "08":
		*c = `ANELE Tipo`

	// Code list used to specify reading levels for children’s books, used in Flanders, and formerly in the Netherlands – see also code 18
	case "09":
		*c = `AVI`

	// German USK (Unterhaltungssoftware Selbstkontrolle) rating applied to video or computer games
	case "10":
		*c = `USK rating`

	// Audience code used in Flanders
	case "11":
		*c = `AWS`

	// Type of school: codelist maintained by VdS Bildungsmedien eV, the German association of educational media publishers. See http://www.bildungsmedien.de/service/onixlisten/schulform_onix_codelist29_value12_0408.pdf
	case "12":
		*c = `Schulform`

	// School region: codelist maintained by VdS Bildungsmedien eV, the German association of educational media publishers, indicating where products are licensed to be used in schools. See http://www.bildungsmedien.de/service/onixlisten/bundesland_onix_codelist29_value13_0408.pdf
	case "13":
		*c = `Bundesland`

	// Occupation: codelist for vocational training materials, maintained by VdS Bildungsmedien eV, the German association of educational media publishers. See http://www.bildungsmedien.de/service/onixlisten/ausbildungsberufe_onix_codelist29_value14_0408.pdf
	case "14":
		*c = `Ausbildungsberuf`

	// Finnish school or college level
	case "15":
		*c = `Suomalainen kouluasteluokitus`

	// UK Publishers Association, Children’s Book Group, coded indication of intended reader age, carried on book covers
	case "16":
		*c = `CBG age guidance`

	// Audience code used in Nielsen Book Services
	case "17":
		*c = `Nielsen Book audience code`

	// Code list used to specify reading levels for children’s books, used in the Netherlands – see also code 09
	case "18":
		*c = `AVI (revised)`

	// Lexile measure (the Lexile measure in <AudienceCodeValue> may optionally be prefixed by the Lexile code). Examples might be ‘880L’, ‘AD0L’ or ‘HL600L’. Deprecated – use <Complexity> instead
	case "19":
		*c = `Lexile measure`

	// Fry readability metric based on number of sentences and syllables per 100 words. Expressed as a number from 1 to 15 in <AudienceCodeValue>. Deprecated – use <Complexity> instead
	case "20":
		*c = `Fry Readability score`

	// Children’s audience code (対象読者), two-digit encoding of intended target readership from 0–2 years up to High School level
	case "21":
		*c = `Japanese Children’s audience code`

	// Publisher’s rating indicating suitability for an particular adult audience, using a code from List 203
	case "22":
		*c = `ONIX Adult audience rating`

	// Codes A1 to C2 indicating standardised level of language learning or teaching material, from beginner to advanced, used in EU
	case "23":
		*c = `Common European Framework for Language Learning`

	// Rating used in Korea to control selling of books and e-books to minors. Current values are 0 (suitable for all) and 19 (only for sale to ages 19+). See http://www.kpec.or.kr/english/
	case "24":
		*c = `Korean Publication Ethics Commission rating`

	// UK Institute of Education Book Bands for Guided Reading scheme (see http://www.ioe.ac.uk/research/4664.html). <AudienceCodeValue> is a color, eg ‘Pink A’ or ‘Copper’. Deprecated – use <Complexity> instead
	case "25":
		*c = `IoE Book Band`

	// Used for German videos/DVDs with educational or informative content; value for <AudienceCodeValue> must be either ‘Infoprogramm gemäß § 14 JuSchG’ or ‘Lehrprogramm gemäß § 14 JuSchG’
	case "26":
		*c = `FSK Lehr-/Infoprogramm`

	// Where this is different from the language of the text of the book recorded in <Language>. <AudienceCodeValue> should be a value from List 74
	case "27":
		*c = `Intended audience language`

	// Pan European Game Information rating used primarily for video games
	case "28":
		*c = `PEGI rating`

	// Code indicating the intended curriculum (eg Naturvetenskapsprogrammet, Estetica programmet) in Swedish higher secondary education
	case "29":
		*c = `Gymnasieprogram`
	default:
		return fmt.Errorf("undefined code for AudienceCodeType has been passed, got [%s]", v)
	}
	return nil
}

// RecordSourceTypeCode List 3: Record source type code
type RecordSourceTypeCode string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *RecordSourceTypeCode) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	//
	case "00":
		*c = `Unspecified`

	//
	case "01":
		*c = `Publisher`

	// Use to designate a distributor providing warehousing and fulfillment for a publisher or for a publisher’s sales agent, as distinct from a wholesaler
	case "02":
		*c = `Publisher’s distributor`

	//
	case "03":
		*c = `Wholesaler`

	// Bibliographic data aggregator
	case "04":
		*c = `Bibliographic agency`

	// Bookseller selling to libraries (including academic libraries)
	case "05":
		*c = `Library bookseller`

	// Use for a publisher’s sales agent responsible for marketing the publisher’s products within a territory, as opposed to a publisher’s distributor who fulfills orders but does not market
	case "06":
		*c = `Publisher’s sales agent`

	// Downstream provider of e-publication format conversion service (who might also be a distributor or retailer of the converted e-publication), supplying metadata on behalf of the publisher. The assigned ISBN is taken from the publisher’s ISBN prefix
	case "07":
		*c = `Publisher’s conversion service provider`

	// Downstream provider of e-publication format conversion service (who might also be a distributor or retailer of the converted e-publication), supplying metadata on behalf of the publisher. The assigned ISBN is taken from the service provider’s prefix (whether or not the service provider dedicates that prefix to a particular publisher)
	case "08":
		*c = `Conversion service provider`

	//
	case "09":
		*c = `ISBN Registration Agency`

	//
	case "10":
		*c = `ISTC Registration Agency`

	// Bookseller selling primarily to consumers
	case "11":
		*c = `Retail bookseller`

	// Bookseller selling primarily to educational institutions
	case "12":
		*c = `Education bookseller`

	// Library service providing enhanced metadata to publishers or other parties
	case "13":
		*c = `Library`
	default:
		return fmt.Errorf("undefined code for RecordSourceTypeCode has been passed, got [%s]", v)
	}
	return nil
}

// AudienceRangeQualifier List 30: Audience range qualifier
type AudienceRangeQualifier string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *AudienceRangeQualifier) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// Values for <AudienceRangeValue> are specified in List 77
	case "11":
		*c = `US school grade range`

	// Values are defined by BIC for England and Wales, Scotland and N Ireland
	case "12":
		*c = `UK school grade`

	// Values in <AudienceRangeValue> must be integers
	case "15":
		*c = `Reading speed, words per minute`

	// For use up to 36 months only: values in <AudienceRangeValue> must be integers
	case "16":
		*c = `Interest age, months`

	// Values in <AudienceRangeValue> must be integers
	case "17":
		*c = `Interest age, years`

	// Values in <AudienceRangeValue> must be integers
	case "18":
		*c = `Reading age, years`

	// Spain: combined grade and region code, maintained by the Ministerio de Educación
	case "19":
		*c = `Spanish school grade`

	// Norwegian educational level for primary and secondary education
	case "20":
		*c = `Skoletrinn`

	// Swedish educational qualifier (code)
	case "21":
		*c = `Nivå`

	//
	case "22":
		*c = `Italian school grade`

	// DEPRECATED – assigned in error: see List 29
	case "23":
		*c = `Schulform`

	// DEPRECATED – assigned in error: see List 29
	case "24":
		*c = `Bundesland`

	// DEPRECATED – assigned in error: see List 29
	case "25":
		*c = `Ausbildungsberuf`

	// Values for <AudienceRangeValue> are specified in List 77
	case "26":
		*c = `Canadian school grade range`

	//
	case "27":
		*c = `Finnish school grade range`

	// Lukion kurssi
	case "28":
		*c = `Finnish Upper secondary school course`

	// Values are P, K, 1–17 (including college-level audiences), see List 227
	case "29":
		*c = `Chinese School Grade range`

	// French educational level classification scolomfr-voc-022, used for example on WizWiz.fr. See http://www.lom-fr.fr/scolomfr/vocabulaires/consultation-des-vocabulaires.html
	case "30":
		*c = `Nomenclature niveaux`
	default:
		return fmt.Errorf("undefined code for AudienceRangeQualifier has been passed, got [%s]", v)
	}
	return nil
}

// AudienceRangePrecision List 31: Audience range precision
type AudienceRangePrecision string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *AudienceRangePrecision) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	//
	case "01":
		*c = `Exact`

	//
	case "03":
		*c = `From`

	//
	case "04":
		*c = `To`
	default:
		return fmt.Errorf("undefined code for AudienceRangePrecision has been passed, got [%s]", v)
	}
	return nil
}

// ComplexitySchemeIdentifierCode List 32: Complexity scheme identifier code
type ComplexitySchemeIdentifierCode string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *ComplexitySchemeIdentifierCode) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// For example AD or HL. DEPRECATED in ONIX 3 – use code 06 instead
	case "01":
		*c = `Lexile code`

	// For example 880L. DEPRECATED in ONIX 3 – use code 06 instead
	case "02":
		*c = `Lexile number`

	// Fry readability metric based on number of sentences and syllables per 100 words. Expressed as a number from 1 to 15 in <ComplexityCode>
	case "03":
		*c = `Fry Readability score`

	// UK Institute of Education Book Bands for Guided Reading scheme (see http://www.ioe.ac.uk/research/4664.html). <ComplexityCode> is a color, eg ‘Pink A’ or ‘Copper’
	case "04":
		*c = `IoE Book Band`

	// <ComplexityCode> is a code from ‘A’ to Z+’. See http://www.fountasandpinnellleveledbooks.com/aboutLeveledTexts.aspx
	case "05":
		*c = `Fountas &amp; Pinnell Text Level Gradient`

	// The Lexile measure in <ComplexityCode> combines the Lexile number (for example 620L or 880L) and optionally the Lexile code (for example AD or HL). Examples might be ‘880L’, ‘AD0L’ or ‘HL600L’. See https://lexile.com/about-lexile/lexile-overview/
	case "06":
		*c = `Lexile measure`

	// Advantage-TASA Open Standard book readability score, used for example within the Renaissance Learning Accelerated Reader scheme. <ComplexityCode> is a real number between 0 and 17. See http://www.renaissance.com/products/accelerated-reader/atos-analyzer
	case "07":
		*c = `ATOS for Books`

	// Flesch-Kincaid Grade Level Formula, a standard readability measure based on the weighted number of syllables per word and words per sentence. <ComplexityCode> is a real number between about -1 and 20
	case "08":
		*c = `Flesch-Kincaid Grade Level`

	// Use this code for books levelled by the publisher or a third party using the Fountas and Pinnell Guided Reading methodology
	case "09":
		*c = `Guided Reading Level`

	// Used for books aimed at K-2 literacy intervention. <ComplexityCode> is an integer between 1 and 20
	case "10":
		*c = `Reading Recovery Level`
	default:
		return fmt.Errorf("undefined code for ComplexitySchemeIdentifierCode has been passed, got [%s]", v)
	}
	return nil
}

// OtherTextTypeCode List 33: Other text type code
type OtherTextTypeCode string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *OtherTextTypeCode) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	//
	case "01":
		*c = `Main description`

	// Limited to a maximum of 350 characters
	case "02":
		*c = `Short description/annotation`

	//
	case "03":
		*c = `Long description`

	// Used for a table of contents sent as a single text field, which may or may not carry structure expressed through HTML etc. Alternatively, a fully structured table of contents may be sent by using the <ContentItem> composite
	case "04":
		*c = `Table of contents`

	// A review quote that is restricted to a maximum length agreed between the sender and receiver of an ONIX file
	case "05":
		*c = `Review quote, restricted length`

	// A review quote taken from a review of a previous edition of the work
	case "06":
		*c = `Quote from review of previous edition`

	// Full text of a review of the product
	case "07":
		*c = `Review text`

	// A quote from a review of the product
	case "08":
		*c = `Review quote`

	// A promotional phrase which is intended to headline a description of the product
	case "09":
		*c = `Promotional ‘headline’`

	// A quote from a review of a previous work by the same author(s) or in the same series
	case "10":
		*c = `Previous review quote`

	// May be part of Reading Group Guide material: for other commentary, see code 42
	case "11":
		*c = `Author comments`

	//
	case "12":
		*c = `Description for reader`

	// A note referring to all contributors to a product – NOT linked to a single contributor
	case "13":
		*c = `Biographical note`

	// For linking to a complete Reading Group Guide, see code 41
	case "14":
		*c = `Description for Reading Group Guide`

	// Each instance must carry a single question: for linking to a complete Reading Group Guide, see code 41
	case "15":
		*c = `Discussion question for Reading Group Guide`

	// Free text listing of other titles with which the product is in competition: although this text might not appear in ‘public’ ONIX records, it could be required where ONIX Is used as a communication format within a group of publishing and distribution companies
	case "16":
		*c = `Competing titles`

	//
	case "17":
		*c = `Flap copy`

	//
	case "18":
		*c = `Back cover copy`

	// Text describing a feature of a product to which the publisher wishes to draw attention for promotional purposes. Each separate feature should be described by a separate repeat, so that formatting can be applied at the discretion of the receiver of the ONIX record
	case "19":
		*c = `Feature`

	// As code 19, but used for a feature which is new in a new edition of the product
	case "20":
		*c = `New feature`

	// A statement included by a publisher in fulfillment of its contractual obligations, such as a disclaimer, sponsor statement, or legal notice of any sort. Note that the inclusion of such a notice cannot and does not imply that a user of the ONIX record is obliged to reproduce it
	case "21":
		*c = `Publisher’s notice`

	//
	case "22":
		*c = `Index`

	//
	case "23":
		*c = `Excerpt from book`

	//
	case "24":
		*c = `First chapter`

	//
	case "25":
		*c = `Description for sales people`

	//
	case "26":
		*c = `Description for press or other media`

	//
	case "27":
		*c = `Description for subsidiary rights department`

	//
	case "28":
		*c = `Description for teachers/educators`

	// A quote usually provided by a celebrity to promote a new book, not from a review
	case "30":
		*c = `Unpublished endorsement`

	//
	case "31":
		*c = `Description for bookstore`

	//
	case "32":
		*c = `Description for library`

	//
	case "33":
		*c = `Introduction or preface`

	//
	case "34":
		*c = `Full text`

	// Promotional text not covered elsewhere
	case "35":
		*c = `Promotional text`

	//
	case "40":
		*c = `Author interview / QandA`

	// Complete guide: see also codes 14 and 15
	case "41":
		*c = `Reading Group Guide`

	// Other than author comments: see code 11
	case "42":
		*c = `Commentary / discussion`

	// (of which the product is a part.) Limited to a maximum of 350 characters
	case "43":
		*c = `Short description for series or set`

	// (of which the product is a part)
	case "44":
		*c = `Long description for series or set`

	// Link to a schedule in iCalendar format
	case "45":
		*c = `Contributor event schedule`

	// Link to a license covering permitted usage of the product content
	case "46":
		*c = `License`

	// Short summary statement of open access status and any related conditions (eg ‘Open access – no commercial use’), primarily for marketing purposes. Should always be accompanied by a link to the complete license (see code 46)
	case "47":
		*c = `Open access statement`

	// Short summary statement that the product is available only in digital formats (eg ‘Digital exclusive’). If a non-digital version is planned, an <EndDate> should be used to specify the date when exclusivity will end. If a non-digital version is available, the statement should not be included
	case "48":
		*c = `Digital exclusivity statement`

	// For example a recommendation or approval provided by a ministry of education or other official body. Use <Text> to provide details and <TextSourceCorporate> to name the approver
	case "49":
		*c = `Official recommendation`

	// A master brand name or title, where the use of the brand spans multiple sets, series and product forms, and possibly multiple imprints and publishers. Used only for branded media properties carrying, for example, a children’s character brand. (This functionality is provided as a workaround in ONIX 2.1 only. ONIX 3.0 has specific provision for master brands as title elements
	case "98":
		*c = `Master brand name`

	// A single ISO 3166-1 country code from List 91 designating the country of final manufacture of the product. (This functionality is provided as a workaround in ONIX 2.1. ONIX 3.0 has specific provision for country of manufacture as a separate element)
	case "99":
		*c = `Country of final manufacture`
	default:
		return fmt.Errorf("undefined code for OtherTextTypeCode has been passed, got [%s]", v)
	}
	return nil
}

// TextFormatCode List 34: Text format code
type TextFormatCode string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *TextFormatCode) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// DEPRECATED: use code 06 or 07 as appropriate
	case "00":
		*c = `ASCII text`

	//
	case "01":
		*c = `SGML`

	// Other than XHTML
	case "02":
		*c = `HTML`

	// Other than XHTML
	case "03":
		*c = `XML`

	// DEPRECATED: was formerly assigned both to PDF and to XHTML
	case "04":
		*c = `PDF`

	//
	case "05":
		*c = `XHTML`

	// Default: text containing no tags of any kind, except for the tags &amp; and &lt; that XML insists must be used to represent ampersand and less-than characters in text, and in the encoding declared at the head of the message or in the XML default (UTF-8 or UTF-16) if there is no explicit declaration
	case "06":
		*c = `Default text format`

	// Plain text containing no tags of any kind, except for the tags &amp; and &lt; that XML insists must be used to represent ampersand and less-than characters in text, and with the character set limited to the ASCII range, i.e. valid UTF-8 characters whose character number lies between 32 (space) and 126 (tilde)
	case "07":
		*c = `Basic ASCII text`

	// Replaces 04 for the <TextFormat> element, but cannot of course be used as a textformat attribute
	case "08":
		*c = `PDF`

	//
	case "09":
		*c = `Microsoft rich text format (RTF)`

	//
	case "10":
		*c = `Microsoft Word binary format (DOC)`

	// Office Open XML file format / OOXML / DOCX
	case "11":
		*c = `ECMA 376 WordprocessingML`

	// ISO Open Document Format
	case "12":
		*c = `ISO 26300 ODF`

	//
	case "13":
		*c = `Corel Wordperfect binary format (DOC)`

	// The Open Publication Structure / OPS Container Format standard of the International Digital Publishing Forum (IDPF) [File extension .epub]
	case "14":
		*c = `EPUB`

	// XML Paper Specification
	case "15":
		*c = `XPS`
	default:
		return fmt.Errorf("undefined code for TextFormatCode has been passed, got [%s]", v)
	}
	return nil
}

// TextLinkTypeCode List 35: Text link type code
type TextLinkTypeCode string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *TextLinkTypeCode) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	//
	case "01":
		*c = `URL`

	//
	case "02":
		*c = `DOI`

	//
	case "03":
		*c = `PURL`

	//
	case "04":
		*c = `URN`

	//
	case "05":
		*c = `FTP address`

	//
	case "06":
		*c = `filename`
	default:
		return fmt.Errorf("undefined code for TextLinkTypeCode has been passed, got [%s]", v)
	}
	return nil
}

// FrontCoverImageFileFormatCode List 36: Front cover image file format code
type FrontCoverImageFileFormatCode string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *FrontCoverImageFileFormatCode) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	//
	case "02":
		*c = `GIF`

	//
	case "03":
		*c = `JPEG`

	//
	case "05":
		*c = `TIF`
	default:
		return fmt.Errorf("undefined code for FrontCoverImageFileFormatCode has been passed, got [%s]", v)
	}
	return nil
}

// FrontCoverImageFileLinkTypeCode List 37: Front cover image file link type code
type FrontCoverImageFileLinkTypeCode string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *FrontCoverImageFileLinkTypeCode) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	//
	case "01":
		*c = `URL`

	//
	case "02":
		*c = `DOI`

	//
	case "03":
		*c = `PURL`

	//
	case "04":
		*c = `URN`

	//
	case "05":
		*c = `FTP address`

	//
	case "06":
		*c = `filename`
	default:
		return fmt.Errorf("undefined code for FrontCoverImageFileLinkTypeCode has been passed, got [%s]", v)
	}
	return nil
}

// ImageaudiovideoFileTypeCode List 38: Image/audio/video file type code
type ImageaudiovideoFileTypeCode string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *ImageaudiovideoFileTypeCode) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// Link to a location where the whole product may be found – used for epublications
	case "01":
		*c = `Whole product`

	//
	case "02":
		*c = `Application: software demo`

	// Includes cover, back cover, spine and – where appropriate – any flaps. Quality unspecified: if sending both a standard quality and a high quality image, use 03 for standard quality and 05 for high quality
	case "03":
		*c = `Image: whole cover`

	// Quality unspecified: if sending both a standard quality and a high quality image, use 04 for standard quality and 06 for high quality
	case "04":
		*c = `Image: front cover`

	// Should have a minimum resolution of 300 dpi when rendered at the intended size for display or print
	case "05":
		*c = `Image: whole cover, high quality`

	// Should have a minimum resolution of 300 dpi when rendered at the intended size for display or print
	case "06":
		*c = `Image: front cover, high quality`

	//
	case "07":
		*c = `Image: front cover thumbnail`

	//
	case "08":
		*c = `Image: contributor(s)`

	// Use for an image, other than a logo, that is part of the ‘branding’ of a series
	case "10":
		*c = `Image: for series`

	//
	case "11":
		*c = `Image: series logo`

	// Use only for a logo which is specific to an individual product
	case "12":
		*c = `Image: product logo`

	//
	case "16":
		*c = `Image: Master brand logo`

	//
	case "17":
		*c = `Image: publisher logo`

	//
	case "18":
		*c = `Image: imprint logo`

	//
	case "22":
		*c = `Image: table of contents`

	// Use for inside page image for book, or screenshot for software or game (revised definition from Issue 8)
	case "23":
		*c = `Image: sample content`

	// Quality unspecified: if sending both a standard quality and a high quality image, use 24 for standard quality and 25 for high quality
	case "24":
		*c = `Image: back cover`

	// Should have a minimum resolution of 300 dpi when rendered at the intended size for display or print
	case "25":
		*c = `Image: back cover, high quality`

	//
	case "26":
		*c = `Image: back cover thumbnail`

	//
	case "27":
		*c = `Image: other cover material`

	//
	case "28":
		*c = `Image: promotional material`

	//
	case "29":
		*c = `Video segment: unspecified`

	//
	case "30":
		*c = `Audio segment: unspecified`

	//
	case "31":
		*c = `Video: author presentation / commentary`

	//
	case "32":
		*c = `Video: author interview`

	//
	case "33":
		*c = `Video: author reading`

	//
	case "34":
		*c = `Video: cover material`

	//
	case "35":
		*c = `Video: sample content`

	//
	case "36":
		*c = `Video: promotional material`

	//
	case "37":
		*c = `Video: review`

	//
	case "38":
		*c = `Video: other commentary / discussion`

	//
	case "41":
		*c = `Audio: author presentation / commentary`

	//
	case "42":
		*c = `Audio: author interview`

	//
	case "43":
		*c = `Audio: author reading`

	//
	case "44":
		*c = `Audio: sample content`

	//
	case "45":
		*c = `Audio: promotional material`

	//
	case "46":
		*c = `Audio: review`

	//
	case "47":
		*c = `Audio: other commentary / discussion`

	// Use for ‘look inside’ facility or ‘widget’
	case "51":
		*c = `Application: sample content`

	//
	case "52":
		*c = `Application: promotional material`
	default:
		return fmt.Errorf("undefined code for ImageaudiovideoFileTypeCode has been passed, got [%s]", v)
	}
	return nil
}

// ImageaudiovideoFileFormatCode List 39: Image/audio/video file format code
type ImageaudiovideoFileFormatCode string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *ImageaudiovideoFileFormatCode) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	//
	case "02":
		*c = `GIF`

	//
	case "03":
		*c = `JPEG`

	//
	case "04":
		*c = `PDF`

	//
	case "05":
		*c = `TIF`

	//
	case "06":
		*c = `RealAudio 28.8`

	// MPEG-1/2 Audio Layer III file (.mp3)
	case "07":
		*c = `MP3`

	// MPEG-4 container format (.mp4, .m4a)
	case "08":
		*c = `MPEG-4`

	// Portable Network Graphics bitmapped image format (.png)
	case "09":
		*c = `PNG`

	// Windows Media Audio format (.wma)
	case "10":
		*c = `WMA`

	// Advanced Audio Codec format (.aac)
	case "11":
		*c = `AAC`

	// Waveform audio file (.wav)
	case "12":
		*c = `WAV`

	// Audio Interchange File Format (.aiff)
	case "13":
		*c = `AIFF`

	// Windows Media Video format (.wmv)
	case "14":
		*c = `WMV`

	// Ogg container format (.ogg)
	case "15":
		*c = `OGG`

	// Audio Video Interleaved container format (.avi)
	case "16":
		*c = `AVI`

	// Quicktime container format (.mov)
	case "17":
		*c = `MOV`

	// Flash container format (includes .flv, .swf, .f4v etc)
	case "18":
		*c = `Flash`

	// 3GP container format (.3gp, 3g2)
	case "19":
		*c = `3GP`

	// WebM container format (includes .webm and .mkv)
	case "20":
		*c = `WebM`
	default:
		return fmt.Errorf("undefined code for ImageaudiovideoFileFormatCode has been passed, got [%s]", v)
	}
	return nil
}

// ImageaudiovideoFileLinkType List 40: Image/audio/video file link type
type ImageaudiovideoFileLinkType string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *ImageaudiovideoFileLinkType) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	//
	case "01":
		*c = `URL`

	//
	case "02":
		*c = `DOI`

	//
	case "03":
		*c = `PURL`

	//
	case "04":
		*c = `URN`

	//
	case "05":
		*c = `FTP address`

	//
	case "06":
		*c = `filename`
	default:
		return fmt.Errorf("undefined code for ImageaudiovideoFileLinkType has been passed, got [%s]", v)
	}
	return nil
}

// PrizeOrAwardAchievementCode List 41: Prize or award achievement code
type PrizeOrAwardAchievementCode string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *PrizeOrAwardAchievementCode) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	//
	case "01":
		*c = `Winner`

	// Named as being in second place
	case "02":
		*c = `Runner-up`

	// Cited as being worthy of special attention at the final stage of the judging process, but not named specifically as winner or runner-up. Possible terminology used by a particular prize includes ‘specially commended’ or ‘honored’
	case "03":
		*c = `Commended`

	// Title named by the judging process to be one of the final list of candidates, such as a ‘short-list’ from which the winner is selected, or a title named as ‘finalist’
	case "04":
		*c = `Short-listed`

	// Title named by the judging process to be one of the preliminary list of candidates, such as a ‘long-list’ from which first a shorter list or set of finalists is selected, and then the winner is announced
	case "05":
		*c = `Long-listed`

	// Or co-winner
	case "06":
		*c = `Joint winner`

	// Selected by judging panel or an official nominating process for final consideration for a prize, award or honour for which no ‘short-list’ or ‘long list’ exists
	case "07":
		*c = `Nominated`
	default:
		return fmt.Errorf("undefined code for PrizeOrAwardAchievementCode has been passed, got [%s]", v)
	}
	return nil
}

// TextItemTypeCode List 42: Text item type code
type TextItemTypeCode string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *TextItemTypeCode) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// A complete work which is published as a content item in a product which carries two or more such works, eg when two or three novels are published in a single omnibus volume
	case "01":
		*c = `Textual work`

	// Text components such as Preface, Introduction etc which appear as preliminaries to the main body of text content in a product
	case "02":
		*c = `Front matter`

	// Text components such as Part, Chapter, Section etc which appear as part of the main body of text content in a product
	case "03":
		*c = `Body matter`

	// Text components such as Index which appear after the main body of text in a product
	case "04":
		*c = `Back matter`

	// For journals
	case "10":
		*c = `Serial item, miscellaneous or unspecified`

	// For journals
	case "11":
		*c = `Research article`

	// For journals
	case "12":
		*c = `Review article`

	// For journals
	case "13":
		*c = `Letter`

	// For journals
	case "14":
		*c = `Short communication`

	// For journals
	case "15":
		*c = `Erratum`

	// For journals
	case "16":
		*c = `Abstract`

	// For journals
	case "17":
		*c = `Book review (or review of other publication)`

	// For journals
	case "18":
		*c = `Editorial`

	// For journals
	case "19":
		*c = `Product review`

	//
	case "20":
		*c = `Index`

	// For journals
	case "21":
		*c = `Obituary`
	default:
		return fmt.Errorf("undefined code for TextItemTypeCode has been passed, got [%s]", v)
	}
	return nil
}

// TextItemIdentifierTypeCode List 43: Text item identifier type code
type TextItemIdentifierTypeCode string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *TextItemIdentifierTypeCode) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// For example, a publisher’s own identifier. Note that <IDTypeName> is required with proprietary identifiers
	case "01":
		*c = `Proprietary`

	// Formerly known as the EAN-13 (unhyphenated)
	case "03":
		*c = `GTIN-13`

	//
	case "06":
		*c = `DOI`

	// Publisher item identifier
	case "09":
		*c = `PII`

	// For serial items only
	case "10":
		*c = `SICI`

	//
	case "11":
		*c = `ISTC`

	// (Unhyphenated)
	case "15":
		*c = `ISBN-13`
	default:
		return fmt.Errorf("undefined code for TextItemIdentifierTypeCode has been passed, got [%s]", v)
	}
	return nil
}

// NameCodeType List 44: Name code type
type NameCodeType string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *NameCodeType) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// Note that <IDTypeName> is required with proprietary identifiers
	case "01":
		*c = `Proprietary`

	// DEPRECATED – use 01
	case "02":
		*c = `Proprietary`

	// Deutsche Nationalbibliothek publisher identifier
	case "03":
		*c = `DNB publisher identifier`

	//
	case "04":
		*c = `Börsenverein Verkehrsnummer`

	//
	case "05":
		*c = `German ISBN Agency publisher identifier`

	// GS1 global location number (formerly EAN location number)
	case "06":
		*c = `GLN`

	// Book trade Standard Address Number – US, UK etc
	case "07":
		*c = `SAN`

	// MARC code list for organizations – see http://www.loc.gov/marc/organizations/orgshome.html
	case "08":
		*c = `MARC organization code`

	// Trading party identifier used in the Netherlands
	case "10":
		*c = `Centraal Boekhuis Relatie ID`

	// Flemish publisher code
	case "13":
		*c = `Fondscode Boekenbank`

	// Business Identity Code (Finland). See http://www.ytj.fi/ (in Finnish)
	case "15":
		*c = `Y-tunnus`

	// International Standard Name Identifier. See http://www.isni.org/
	case "16":
		*c = `ISNI`

	// Personennamendatei – person name authority file used by Deutsche Nationalbibliothek and in other German-speaking countries. See http://www.d-nb.de/standardisierung/normdateien/pnd.htm (German) or http://www.d-nb.de/eng/standardisierung/normdateien/pnd.htm (English). DEPRECATED in favour of the GND
	case "17":
		*c = `PND`

	// A control number assigned to a Library of Congress Name Authority record
	case "18":
		*c = `LCCN`

	// Publisher identifier administered by Japanese ISBN Agency
	case "19":
		*c = `Japanese Publisher identifier`

	// Gemeinsame Körperschaftsdatei – Corporate Body Authority File in the German-speaking countries. See http://www.d-nb.de/standardisierung/normdateien/gkd.htm (German) or http://www.d-nb.de/eng/standardisierung/normdateien/gkd.htm (English). DEPRECATED in favour of the GND
	case "20":
		*c = `GKD`

	// Open Researcher and Contributor ID. See http://www.orcid.org/
	case "21":
		*c = `ORCID`

	// Publisher identifier maintained by the Chinese ISBN Agency (GAPP)
	case "22":
		*c = `GAPP Publisher Identifier`

	// Identifier for a business organization for VAT purposes, eg within the EU’s VIES system. See http://ec.europa.eu/taxation_customs/vies/faqvies.do for EU VAT ID formats, which vary from country to country. Generally these consist of a two-letter country code followed by the 8–12 digits of the national VAT ID. Some countries include one or two letters within their VAT ID. See http://en.wikipedia.org/wiki/VAT_identification_number for non-EU countries that maintain similar identifiers. Spaces, dashes etc should be omitted
	case "23":
		*c = `VAT Identity Number`

	// 4-digit business organization identifier controlled by the Japanese Publication Wholesalers Association
	case "24":
		*c = `JP Distribution Identifier`

	// Gemeinsame Normdatei – Joint Authority File in the German-speaking countries. See http://www.dnb.de/EN/gnd (English). Combines the PND, SWD and GKD into a single authority file, and should be used in preference
	case "25":
		*c = `GND`

	// Dunn and Bradstreet Universal Numbering System, see http://www.dnb.co.uk/dandb-duns-number
	case "26":
		*c = `DUNS`

	// Ringgold organizational identifier, see http://www.ringgold.com/pages/identify.html
	case "27":
		*c = `Ringgold ID`

	// French Electre publisher identifier
	case "28":
		*c = `Identifiant Editeur Electre`

	// DOI used in EIDR party registry, for example ‘10.5237/C9F6-F41F’ (Sam Raimi). See http://eidr.org
	case "29":
		*c = `EIDR Party DOI`

	// French Electre imprint Identifier
	case "30":
		*c = `Identifiant Marque Electre`

	// Virtual Internet Authority File. <IDValue> should be a number. The URI form of the identifier can be created by prefixing the number with ‘https://viaf.org/viaf/’. See https://viaf.org
	case "31":
		*c = `VIAF ID`

	// DOI used in CrossRef’s Open Funder Registry list of academic research funding bodies, for example ‘10.13039/100004440’ (Wellcome Trust). See http://www.crossref.org/fundingdata/registry.html
	case "32":
		*c = `FundRef DOI`

	// Control number assigned to a Name Authority record by the Biblioteca Nacional de España
	case "33":
		*c = `BNE CN`

	// Numéro de la notice de personne BNF
	case "34":
		*c = `BNF Control Number`

	// Archival Resource Key, as a URL (including the address of the ARK resolver provided by eg a national library)
	case "35":
		*c = `ARK`
	default:
		return fmt.Errorf("undefined code for NameCodeType has been passed, got [%s]", v)
	}
	return nil
}

// PublishingRoleCode List 45: Publishing role code
type PublishingRoleCode string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *PublishingRoleCode) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	//
	case "01":
		*c = `Publisher`

	// Use where two or more publishers co-publish the exact same product, either under a single ISBN (in which case both publishers are co-publishers), or under different ISBNs (in which case the publisher of THIS ISBN is the publisher and the publishers of OTHER ISBNs are co-publishers. Note this is different from publication of ‘co-editions’
	case "02":
		*c = `Co-publisher`

	//
	case "03":
		*c = `Sponsor`

	// Of a translated work
	case "04":
		*c = `Publisher of original-language version`

	//
	case "05":
		*c = `Host/distributor of electronic content`

	//
	case "06":
		*c = `Published for/on behalf of`

	// Use also for ‘Published in cooperation with’
	case "07":
		*c = `Published in association with`

	// DEPRECATED: use code 06
	case "08":
		*c = `Published on behalf of`

	// When ownership of a product or title is transferred from one publisher to another
	case "09":
		*c = `New or acquiring publisher`

	// The group to which a publisher (publishing role 01) belongs: use only if a publisher has been identified with role code 01
	case "10":
		*c = `Publishing group`

	// The publisher of the edition of which a product is a facsimile
	case "11":
		*c = `Publisher of facsimile original`

	// The repackager of a prebound edition that has been assigned its own identifier. (In the US, a ‘prebound edition’ is a book that was previously bound, normally as a paperback, and has been rebound with a library-quality hardcover binding by a supplier other than the original publisher.) Required when the <EditionType> is coded PRB. The original publisher should be named as the ‘publisher’
	case "12":
		*c = `Repackager of prebound edition`

	// When ownership of a product or title is transferred from one publisher to another (complement of code 09)
	case "13":
		*c = `Former publisher`

	// Body funding publication fees, if different from the body funding the underlying research. For use with open access publications
	case "14":
		*c = `Publication funder`

	// Body funding the research on which publication is based, if different from the body funding the publication. For use with open access publications
	case "15":
		*c = `Research funder`

	// Body funding research and publication. For use with open access publications
	case "16":
		*c = `Funding body`

	// Organisation responsible for printing a printed product. Supplied primarily to meet legal deposit requirements, and may apply only to the first impression. The organisation may also be responsible for binding, when a separate binder is not specified
	case "17":
		*c = `Printer`

	// Organisation responsible for binding a printed product (where distinct from the printer). Supplied primarily to meet legal deposit requirements, and may apply only to the first impression
	case "18":
		*c = `Binder`

	// Organisation primarily responsible for physical manufacture of a product, when neither Printer nor Binder is directly appropriate (for example, with disc or tape products, or digital products on a physical carrier)
	case "19":
		*c = `Manufacturer`
	default:
		return fmt.Errorf("undefined code for PublishingRoleCode has been passed, got [%s]", v)
	}
	return nil
}

// SalesRightsTypeCode List 46: Sales rights type code
type SalesRightsTypeCode string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *SalesRightsTypeCode) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// May only be used with the ONIX 3 <ROWSalesRightsType> element
	case "00":
		*c = `Sales rights unknown or unstated for any reason`

	//
	case "01":
		*c = `For sale with exclusive rights in the specified countries or territories`

	//
	case "02":
		*c = `For sale with non-exclusive rights in the specified countries or territories`

	//
	case "03":
		*c = `Not for sale in the specified countries or territories (reason unspecified)`

	//
	case "04":
		*c = `Not for sale in the specified countries (but publisher holds exclusive rights in those countries or territories)`

	//
	case "05":
		*c = `Not for sale in the specified countries (publisher holds non-exclusive rights in those countries or territories)`

	//
	case "06":
		*c = `Not for sale in the specified countries (because publisher does not hold rights in those countries or territories)`

	// Only for use with ONIX 3. Deprecated
	case "07":
		*c = `For sale with exclusive rights in the specified countries or territories (sales restriction applies)`

	// Only for use with ONIX 3. Deprecated
	case "08":
		*c = `For sale with non-exclusive rights in the specified countries or territories (sales restriction applies)`
	default:
		return fmt.Errorf("undefined code for SalesRightsTypeCode has been passed, got [%s]", v)
	}
	return nil
}

// RightsRegion List 47: Rights region
type RightsRegion string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *RightsRegion) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	//
	case "000":
		*c = `World`

	//
	case "001":
		*c = `World except territories specified elsewhere in rights statements`

	//
	case "002":
		*c = `UK airports`

	// Use when an open market edition is published under its own ISBN
	case "003":
		*c = `UK ‘open market’`
	default:
		return fmt.Errorf("undefined code for RightsRegion has been passed, got [%s]", v)
	}
	return nil
}

// MeasureTypeCode List 48: Measure type code
type MeasureTypeCode string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *MeasureTypeCode) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// For a book, the overall spine height when standing on a shelf. For a folded map, the height when folded. In general, the height of a product in the form in which it is presented or packaged for retail sale
	case "01":
		*c = `Height`

	// For a book, the overall horizontal dimension of the cover when standing upright. For a folded map, the width when folded. In general, the width of a product in the form in which it is presented or packaged for retail sale
	case "02":
		*c = `Width`

	// For a book, the overall thickness of the spine. For a folded map, the thickness when folded. In general, the thickness or depth of a product in the form in which it is presented or packaged for retail sale
	case "03":
		*c = `Thickness`

	// Not recommended for general use
	case "04":
		*c = `Page trim height`

	// Not recommended for general use
	case "05":
		*c = `Page trim width`

	//
	case "08":
		*c = `Unit weight`

	// Of a globe, for example
	case "09":
		*c = `Diameter (sphere)`

	// The height of a folded or rolled sheet map, poster etc when unfolded
	case "10":
		*c = `Unfolded/unrolled sheet height`

	// The width of a folded or rolled sheet map, poster etc when unfolded
	case "11":
		*c = `Unfolded/unrolled sheet width`

	// The diameter of the cross-section of a tube or cylinder, usually carrying a rolled sheet product. Use 01 ‘Height’ for the height or length of the tube
	case "12":
		*c = `Diameter (tube or cylinder)`

	// The length of a side of the cross-section of a long triangular or square package, usually carrying a rolled sheet product. Use 01 ‘Height’ for the height or length of the package
	case "13":
		*c = `Rolled sheet package side measure`
	default:
		return fmt.Errorf("undefined code for MeasureTypeCode has been passed, got [%s]", v)
	}
	return nil
}

// RegionCode List 49: Region code
type RegionCode string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *RegionCode) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	//
	case "AU-CT":
		*c = `Australian Capital Territory`

	//
	case "AU-NS":
		*c = `New South Wales`

	//
	case "AU-NT":
		*c = `Northern Territory`

	//
	case "AU-QL":
		*c = `Queensland`

	//
	case "AU-SA":
		*c = `South Australia`

	//
	case "AU-TS":
		*c = `Tasmania`

	//
	case "AU-VI":
		*c = `Victoria`

	//
	case "AU-WA":
		*c = `Western Australia`

	//
	case "CA-AB":
		*c = `Alberta`

	//
	case "CA-BC":
		*c = `British Columbia`

	//
	case "CA-MB":
		*c = `Manitoba`

	//
	case "CA-NB":
		*c = `New Brunswick`

	//
	case "CA-NL":
		*c = `Newfoundland and Labrador`

	//
	case "CA-NS":
		*c = `Nova Scotia`

	//
	case "CA-NT":
		*c = `Northwest Territories`

	//
	case "CA-NU":
		*c = `Nunavut`

	//
	case "CA-ON":
		*c = `Ontario`

	//
	case "CA-PE":
		*c = `Prince Edward Island`

	//
	case "CA-QC":
		*c = `Quebec`

	//
	case "CA-SK":
		*c = `Saskatchewan`

	//
	case "CA-YT":
		*c = `Yukon Territory`

	//
	case "CN-11":
		*c = `Beijing Municipality`

	//
	case "CN-12":
		*c = `Tianjin Municipality`

	//
	case "CN-13":
		*c = `Hebei Province`

	//
	case "CN-14":
		*c = `Shanxi Province`

	//
	case "CN-15":
		*c = `Inner Mongolia Autonomous Region`

	//
	case "CN-21":
		*c = `Liaoning Province`

	//
	case "CN-22":
		*c = `Jilin Province`

	//
	case "CN-23":
		*c = `Heilongjiang Province`

	//
	case "CN-31":
		*c = `Shanghai Municipality`

	//
	case "CN-32":
		*c = `Jiangsu Province`

	//
	case "CN-33":
		*c = `Zhejiang Province`

	//
	case "CN-34":
		*c = `Anhui Province`

	//
	case "CN-35":
		*c = `Fujian Province`

	//
	case "CN-36":
		*c = `Jiangxi Province`

	//
	case "CN-37":
		*c = `Shandong Province`

	//
	case "CN-41":
		*c = `Henan Province`

	//
	case "CN-42":
		*c = `Hubei Province`

	//
	case "CN-43":
		*c = `Hunan Province`

	//
	case "CN-44":
		*c = `Guangdong Province`

	//
	case "CN-45":
		*c = `Guangxi Zhuang Autonomous Region`

	//
	case "CN-46":
		*c = `Hainan Province`

	//
	case "CN-50":
		*c = `Chongqing Municipality`

	//
	case "CN-51":
		*c = `Sichuan Province`

	//
	case "CN-52":
		*c = `Guizhou Province`

	//
	case "CN-53":
		*c = `Yunnan Province`

	//
	case "CN-54":
		*c = `Tibet Autonomous Region`

	//
	case "CN-61":
		*c = `Shaanxi Province`

	//
	case "CN-62":
		*c = `Gansu Province`

	//
	case "CN-63":
		*c = `Qinghai Province`

	//
	case "CN-64":
		*c = `Ningxia Hui Autonomous Region`

	//
	case "CN-65":
		*c = `Xinjiang Uyghur Autonomous Region`

	// Prefer code TW (Taiwan, Province of China) from List 91
	case "CN-71":
		*c = `Taiwan Province`

	// Prefer code HK (Hong Kong) from List 91
	case "CN-91":
		*c = `Hong Kong Special Administrative Region`

	// Prefer code MO (Macao) from List 91
	case "CN-92":
		*c = `Macau Special Administrative Region`

	//
	case "ES-CN":
		*c = `Canary Islands`

	//
	case "FR-H":
		*c = `Corsica`

	// Airside outlets at UK international airports only
	case "GB-AIR":
		*c = `UK airside`

	// All UK airports, including both airside and other outlets
	case "GB-APS":
		*c = `UK airports`

	// DEPRECATED, replaced by country codes GG – Guernsey, and JE – Jersey
	case "GB-CHA":
		*c = `Channel Islands`

	//
	case "GB-ENG":
		*c = `England`

	// UK excluding Northern Ireland
	case "GB-EWS":
		*c = `England, Wales, Scotland`

	// DEPRECATED, replaced by country code IM – Isle of Man
	case "GB-IOM":
		*c = `Isle of Man`

	//
	case "GB-NIR":
		*c = `Northern Ireland`

	//
	case "GB-SCT":
		*c = `Scotland`

	//
	case "GB-WLS":
		*c = `Wales`

	// Airside outlets at Irish international airports only
	case "IE-AIR":
		*c = `Ireland airside`

	//
	case "IT-AG":
		*c = `Agrigento`

	//
	case "IT-AL":
		*c = `Alessandria`

	//
	case "IT-AN":
		*c = `Ancona`

	//
	case "IT-AO":
		*c = `Aosta`

	//
	case "IT-AR":
		*c = `Arezzo`

	//
	case "IT-AP":
		*c = `Ascoli Piceno`

	//
	case "IT-AT":
		*c = `Asti`

	//
	case "IT-AV":
		*c = `Avellino`

	//
	case "IT-BA":
		*c = `Bari`

	//
	case "IT-BT":
		*c = `Barletta-Andria-Trani`

	//
	case "IT-BL":
		*c = `Belluno`

	//
	case "IT-BN":
		*c = `Benevento`

	//
	case "IT-BG":
		*c = `Bergamo`

	//
	case "IT-BI":
		*c = `Biella`

	//
	case "IT-BO":
		*c = `Bologna`

	//
	case "IT-BZ":
		*c = `Bolzano`

	//
	case "IT-BS":
		*c = `Brescia`

	//
	case "IT-BR":
		*c = `Brindisi`

	//
	case "IT-CA":
		*c = `Cagliari`

	//
	case "IT-CL":
		*c = `Caltanissetta`

	//
	case "IT-CB":
		*c = `Campobasso`

	//
	case "IT-CI":
		*c = `Carbonia-Iglesias`

	//
	case "IT-CE":
		*c = `Caserta`

	//
	case "IT-CT":
		*c = `Catania`

	//
	case "IT-CZ":
		*c = `Catanzaro`

	//
	case "IT-CH":
		*c = `Chieti`

	//
	case "IT-CO":
		*c = `Como`

	//
	case "IT-CS":
		*c = `Cosenza`

	//
	case "IT-CR":
		*c = `Cremona`

	//
	case "IT-KR":
		*c = `Crotone`

	//
	case "IT-CN":
		*c = `Cuneo`

	//
	case "IT-EN":
		*c = `Enna`

	//
	case "IT-FM":
		*c = `Fermo`

	//
	case "IT-FE":
		*c = `Ferrara`

	//
	case "IT-FI":
		*c = `Firenze`

	//
	case "IT-FG":
		*c = `Foggia`

	//
	case "IT-FC":
		*c = `Forlì-Cesena`

	//
	case "IT-FR":
		*c = `Frosinone`

	//
	case "IT-GE":
		*c = `Genova`

	//
	case "IT-GO":
		*c = `Gorizia`

	//
	case "IT-GR":
		*c = `Grosseto`

	//
	case "IT-IM":
		*c = `Imperia`

	//
	case "IT-IS":
		*c = `Isernia`

	//
	case "IT-SP":
		*c = `La Spezia`

	//
	case "IT-AQ":
		*c = `L’Aquila`

	//
	case "IT-LT":
		*c = `Latina`

	//
	case "IT-LE":
		*c = `Lecce`

	//
	case "IT-LC":
		*c = `Lecco`

	//
	case "IT-LI":
		*c = `Livorno`

	//
	case "IT-LO":
		*c = `Lodi`

	//
	case "IT-LU":
		*c = `Lucca`

	//
	case "IT-MC":
		*c = `Macerata`

	//
	case "IT-MN":
		*c = `Mantova`

	//
	case "IT-MS":
		*c = `Massa-Carrara`

	//
	case "IT-MT":
		*c = `Matera`

	//
	case "IT-VS":
		*c = `Medio Campidano`

	//
	case "IT-ME":
		*c = `Messina`

	//
	case "IT-MI":
		*c = `Milano`

	//
	case "IT-MO":
		*c = `Modena`

	//
	case "IT-MB":
		*c = `Monza e Brianza`

	//
	case "IT-NA":
		*c = `Napoli`

	//
	case "IT-NO":
		*c = `Novara`

	//
	case "IT-NU":
		*c = `Nuoro`

	//
	case "IT-OG":
		*c = `Ogliastra`

	//
	case "IT-OT":
		*c = `Olbia-Tempio`

	//
	case "IT-OR":
		*c = `Oristano`

	//
	case "IT-PD":
		*c = `Padova`

	//
	case "IT-PA":
		*c = `Palermo`

	//
	case "IT-PR":
		*c = `Parma`

	//
	case "IT-PV":
		*c = `Pavia`

	//
	case "IT-PG":
		*c = `Perugia`

	//
	case "IT-PU":
		*c = `Pesaro e Urbino`

	//
	case "IT-PE":
		*c = `Pescara`

	//
	case "IT-PC":
		*c = `Piacenza`

	//
	case "IT-PI":
		*c = `Pisa`

	//
	case "IT-PT":
		*c = `Pistoia`

	//
	case "IT-PN":
		*c = `Pordenone`

	//
	case "IT-PZ":
		*c = `Potenza`

	//
	case "IT-PO":
		*c = `Prato`

	//
	case "IT-RG":
		*c = `Ragusa`

	//
	case "IT-RA":
		*c = `Ravenna`

	//
	case "IT-RC":
		*c = `Reggio Calabria`

	//
	case "IT-RE":
		*c = `Reggio Emilia`

	//
	case "IT-RI":
		*c = `Rieti`

	//
	case "IT-RN":
		*c = `Rimini`

	//
	case "IT-RM":
		*c = `Roma`

	//
	case "IT-RO":
		*c = `Rovigo`

	//
	case "IT-SA":
		*c = `Salerno`

	//
	case "IT-SS":
		*c = `Sassari`

	//
	case "IT-SV":
		*c = `Savona`

	//
	case "IT-SI":
		*c = `Siena`

	//
	case "IT-SR":
		*c = `Siracusa`

	//
	case "IT-SO":
		*c = `Sondrio`

	//
	case "IT-TA":
		*c = `Taranto`

	//
	case "IT-TE":
		*c = `Teramo`

	//
	case "IT-TR":
		*c = `Terni`

	//
	case "IT-TO":
		*c = `Torino`

	//
	case "IT-TP":
		*c = `Trapani`

	//
	case "IT-TN":
		*c = `Trento`

	//
	case "IT-TV":
		*c = `Treviso`

	//
	case "IT-TS":
		*c = `Trieste`

	//
	case "IT-UD":
		*c = `Udine`

	//
	case "IT-VA":
		*c = `Varese`

	//
	case "IT-VE":
		*c = `Venezia`

	//
	case "IT-VB":
		*c = `Verbano-Cusio-Ossola`

	//
	case "IT-VC":
		*c = `Vercelli`

	//
	case "IT-VR":
		*c = `Verona`

	//
	case "IT-VV":
		*c = `Vibo Valentia`

	//
	case "IT-VI":
		*c = `Vicenza`

	//
	case "IT-VT":
		*c = `Viterbo`

	//
	case "RS-KM":
		*c = `Kosovo-Metohija`

	//
	case "RS-VO":
		*c = `Vojvodina`

	//
	case "RU-AD":
		*c = `Republic of Adygeya`

	//
	case "RU-AL":
		*c = `Republic of Altay`

	//
	case "RU-BA":
		*c = `Republic of Bashkortostan`

	//
	case "RU-BU":
		*c = `Republic of Buryatiya`

	//
	case "RU-CE":
		*c = `Chechenskaya Republic`

	//
	case "RU-CU":
		*c = `Chuvashskaya Republic`

	//
	case "RU-DA":
		*c = `Republic of Dagestan`

	//
	case "RU-IN":
		*c = `Republic of Ingushetiya`

	//
	case "RU-KB":
		*c = `Kabardino-Balkarskaya Republic`

	//
	case "RU-KL":
		*c = `Republic of Kalmykiya`

	//
	case "RU-KC":
		*c = `Karachayevo-Cherkesskaya Republic`

	//
	case "RU-KR":
		*c = `Republic of Kareliya`

	//
	case "RU-KK":
		*c = `Republic of Khakasiya`

	//
	case "RU-KO":
		*c = `Republic of Komi`

	//
	case "RU-ME":
		*c = `Republic of Mariy El`

	//
	case "RU-MO":
		*c = `Republic of Mordoviya`

	//
	case "RU-SA":
		*c = `Republic of Sakha (Yakutiya)`

	//
	case "RU-SE":
		*c = `Republic of Severnaya Osetiya-Alaniya`

	//
	case "RU-TA":
		*c = `Republic of Tatarstan`

	//
	case "RU-TY":
		*c = `Republic of Tyva (Tuva)`

	//
	case "RU-UD":
		*c = `Udmurtskaya Republic`

	//
	case "RU-ALT":
		*c = `Altayskiy Administrative Territory`

	//
	case "RU-KAM":
		*c = `Kamchatskiy Administrative Territory`

	//
	case "RU-KHA":
		*c = `Khabarovskiy Administrative Territory`

	//
	case "RU-KDA":
		*c = `Krasnodarskiy Administrative Territory`

	//
	case "RU-KYA":
		*c = `Krasnoyarskiy Administrative Territory`

	//
	case "RU-PER":
		*c = `Permskiy Administrative Territory`

	//
	case "RU-PRI":
		*c = `Primorskiy Administrative Territory`

	//
	case "RU-STA":
		*c = `Stavropol’skiy Administrative Territory`

	//
	case "RU-ZAB":
		*c = `Zabaykal’skiy Administrative Territory`

	//
	case "RU-AMU":
		*c = `Amurskaya Administrative Region`

	//
	case "RU-ARK":
		*c = `Arkhangel’skaya Administrative Region`

	//
	case "RU-AST":
		*c = `Astrakhanskaya Administrative Region`

	//
	case "RU-BEL":
		*c = `Belgorodskaya Administrative Region`

	//
	case "RU-BRY":
		*c = `Bryanskaya Administrative Region`

	//
	case "RU-CHE":
		*c = `Chelyabinskaya Administrative Region`

	//
	case "RU-IRK":
		*c = `Irkutskaya Administrative Region`

	//
	case "RU-IVA":
		*c = `Ivanovskaya Administrative Region`

	//
	case "RU-KGD":
		*c = `Kaliningradskaya Administrative Region`

	//
	case "RU-KLU":
		*c = `Kaluzhskaya Administrative Region`

	//
	case "RU-KEM":
		*c = `Kemerovskaya Administrative Region`

	//
	case "RU-KIR":
		*c = `Kirovskaya Administrative Region`

	//
	case "RU-KOS":
		*c = `Kostromskaya Administrative Region`

	//
	case "RU-KGN":
		*c = `Kurganskaya Administrative Region`

	//
	case "RU-KRS":
		*c = `Kurskaya Administrative Region`

	//
	case "RU-LEN":
		*c = `Leningradskaya Administrative Region`

	//
	case "RU-LIP":
		*c = `Lipetskaya Administrative Region`

	//
	case "RU-MAG":
		*c = `Magadanskaya Administrative Region`

	//
	case "RU-MOS":
		*c = `Moskovskaya Administrative Region`

	//
	case "RU-MUR":
		*c = `Murmanskaya Administrative Region`

	//
	case "RU-NIZ":
		*c = `Nizhegorodskaya Administrative Region`

	//
	case "RU-NGR":
		*c = `Novgorodskaya Administrative Region`

	//
	case "RU-NVS":
		*c = `Novosibirskaya Administrative Region`

	//
	case "RU-OMS":
		*c = `Omskaya Administrative Region`

	//
	case "RU-ORE":
		*c = `Orenburgskaya Administrative Region`

	//
	case "RU-ORL":
		*c = `Orlovskaya Administrative Region`

	//
	case "RU-PNZ":
		*c = `Penzenskaya Administrative Region`

	//
	case "RU-PSK":
		*c = `Pskovskaya Administrative Region`

	//
	case "RU-ROS":
		*c = `Rostovskaya Administrative Region`

	//
	case "RU-RYA":
		*c = `Ryazanskaya Administrative Region`

	//
	case "RU-SAK":
		*c = `Sakhalinskaya Administrative Region`

	//
	case "RU-SAM":
		*c = `Samarskaya Administrative Region`

	//
	case "RU-SAR":
		*c = `Saratovskaya Administrative Region`

	//
	case "RU-SMO":
		*c = `Smolenskaya Administrative Region`

	//
	case "RU-SVE":
		*c = `Sverdlovskaya Administrative Region`

	//
	case "RU-TAM":
		*c = `Tambovskaya Administrative Region`

	//
	case "RU-TOM":
		*c = `Tomskaya Administrative Region`

	//
	case "RU-TUL":
		*c = `Tul’skaya Administrative Region`

	//
	case "RU-TVE":
		*c = `Tverskaya Administrative Region`

	//
	case "RU-TYU":
		*c = `Tyumenskaya Administrative Region`

	//
	case "RU-ULY":
		*c = `Ul’yanovskaya Administrative Region`

	//
	case "RU-VLA":
		*c = `Vladimirskaya Administrative Region`

	//
	case "RU-VGG":
		*c = `Volgogradskaya Administrative Region`

	//
	case "RU-VLG":
		*c = `Vologodskaya Administrative Region`

	//
	case "RU-VOR":
		*c = `Voronezhskaya Administrative Region`

	//
	case "RU-YAR":
		*c = `Yaroslavskaya Administrative Region`

	//
	case "RU-MOW":
		*c = `Moskva City`

	//
	case "RU-SPE":
		*c = `Sankt-Peterburg City`

	//
	case "RU-YEV":
		*c = `Yevreyskaya Autonomous Administrative Region`

	//
	case "RU-CHU":
		*c = `Chukotskiy Autonomous District`

	//
	case "RU-KHM":
		*c = `Khanty-Mansiyskiy Autonomous District`

	//
	case "RU-NEN":
		*c = `Nenetskiy Autonomous District`

	//
	case "RU-YAN":
		*c = `Yamalo-Nenetskiy Autonomous District`

	//
	case "US-AK":
		*c = `Alaska`

	//
	case "US-AL":
		*c = `Alabama`

	//
	case "US-AR":
		*c = `Arkansas`

	//
	case "US-AZ":
		*c = `Arizona`

	//
	case "US-CA":
		*c = `California`

	//
	case "US-CO":
		*c = `Colorado`

	//
	case "US-CT":
		*c = `Connecticut`

	//
	case "US-DC":
		*c = `District of Columbia`

	//
	case "US-DE":
		*c = `Delaware`

	//
	case "US-FL":
		*c = `Florida`

	//
	case "US-GA":
		*c = `Georgia`

	//
	case "US-HI":
		*c = `Hawaii`

	//
	case "US-IA":
		*c = `Iowa`

	//
	case "US-ID":
		*c = `Idaho`

	//
	case "US-IL":
		*c = `Illinois`

	//
	case "US-IN":
		*c = `Indiana`

	//
	case "US-KS":
		*c = `Kansas`

	//
	case "US-KY":
		*c = `Kentucky`

	//
	case "US-LA":
		*c = `Louisiana`

	//
	case "US-MA":
		*c = `Massachusetts`

	//
	case "US-MD":
		*c = `Maryland`

	//
	case "US-ME":
		*c = `Maine`

	//
	case "US-MI":
		*c = `Michigan`

	//
	case "US-MN":
		*c = `Minnesota`

	//
	case "US-MO":
		*c = `Missouri`

	//
	case "US-MS":
		*c = `Mississippi`

	//
	case "US-MT":
		*c = `Montana`

	//
	case "US-NC":
		*c = `North Carolina`

	//
	case "US-ND":
		*c = `North Dakota`

	//
	case "US-NE":
		*c = `Nebraska`

	//
	case "US-NH":
		*c = `New Hampshire`

	//
	case "US-NJ":
		*c = `New Jersey`

	//
	case "US-NM":
		*c = `New Mexico`

	//
	case "US-NV":
		*c = `Nevada`

	//
	case "US-NY":
		*c = `New York`

	//
	case "US-OH":
		*c = `Ohio`

	//
	case "US-OK":
		*c = `Oklahoma`

	//
	case "US-OR":
		*c = `Oregon`

	//
	case "US-PA":
		*c = `Pennsylvania`

	//
	case "US-RI":
		*c = `Rhode Island`

	//
	case "US-SC":
		*c = `South Carolina`

	//
	case "US-SD":
		*c = `South Dakota`

	//
	case "US-TN":
		*c = `Tennessee`

	//
	case "US-TX":
		*c = `Texas`

	//
	case "US-UT":
		*c = `Utah`

	//
	case "US-VA":
		*c = `Virginia`

	//
	case "US-VT":
		*c = `Vermont`

	//
	case "US-WA":
		*c = `Washington`

	//
	case "US-WI":
		*c = `Wisconsin`

	//
	case "US-WV":
		*c = `West Virginia`

	//
	case "US-WY":
		*c = `Wyoming`

	// Countries geographically within continental Europe which use the Euro as their sole currency. At the time of writing, this is a synonym for ‘AT BE CY EE FI FR DE ES GR IE IT LT LU LV MT NL PT SI SK’ (the official Eurozone 19), plus ‘AD MC SM VA ME’ and Kosovo (other Euro-using countries in continental Europe). Note some other territories using the Euro, but outside continental Europe are excluded from this list, and may need to be specified separately. ONLY valid in ONIX 3, and ONLY within P.26 – and this use is itself DEPRECATED. Use of an explicit list of countries instead of ECZ is strongly encouraged
	case "ECZ":
		*c = `Eurozone`

	// World except as otherwise specified. NOT USED in ONIX 3
	case "ROW":
		*c = `Rest of world`

	// In ONIX 3, may ONLY be used in <RegionsIncluded>
	case "WORLD":
		*c = `World`
	default:
		return fmt.Errorf("undefined code for RegionCode has been passed, got [%s]", v)
	}
	return nil
}

// ProductIdentifierTypeCode List 5: Product identifier type code
type ProductIdentifierTypeCode string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *ProductIdentifierTypeCode) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// For example, a publisher’s or wholesaler’s product number. Note that <IDTypeName> is required with proprietary identifiers
	case "01":
		*c = `Proprietary`

	// International Standard Book Number, pre-2007, unhyphenated (10 characters) – now DEPRECATED in ONIX for Books, except where providing historical information for compatibility with legacy systems. It should only be used in relation to products published before 2007 – when ISBN-13 superseded it – and should never be used as the ONLY identifier (it should always be accompanied by the correct GTIN-13 / ISBN-13)
	case "02":
		*c = `ISBN-10`

	// GS1 Global Trade Item Number, formerly known as EAN article number (13 digits)
	case "03":
		*c = `GTIN-13`

	// UPC product number (12 digits)
	case "04":
		*c = `UPC`

	// International Standard Music Number (M plus nine digits). Pre-2008 – now DEPRECATED in ONIX for Books, except where providing historical information for compatibility with legacy systems. It should only be used in relation to products published before 2008 – when ISMN-13 superseded it – and should never be used as the ONLY identifier (it should always be accompanied by the correct ISMN-13)
	case "05":
		*c = `ISMN-10`

	// Digital Object Identifier (variable length and character set)
	case "06":
		*c = `DOI`

	// Library of Congress Control Number (12 characters, alphanumeric)
	case "13":
		*c = `LCCN`

	// GS1 Global Trade Item Number (14 digits)
	case "14":
		*c = `GTIN-14`

	// International Standard Book Number, from 2007, unhyphenated (13 digits starting 978 or 9791–9799)
	case "15":
		*c = `ISBN-13`

	// The number assigned to a publication as part of a national legal deposit process
	case "17":
		*c = `Legal deposit number`

	// Uniform Resource Name: note that in trade applications an ISBN must be sent as a GTIN-13 and, where required, as an ISBN-13 – it should not be sent as a URN
	case "22":
		*c = `URN`

	// A unique number assigned to a bibliographic item by OCLC
	case "23":
		*c = `OCLC number`

	// An ISBN-13 assigned by a co-publisher. The ‘main’ ISBN sent with ID type code 03 and/or 15 should always be the ISBN that is used for ordering from the supplier identified in Supply Detail. However, ISBN rules allow a co-published title to carry more than one ISBN. The co-publisher should be identified in an instance of the <Publisher> composite, with the applicable <PublishingRole> code
	case "24":
		*c = `Co-publisher’s ISBN-13`

	// International Standard Music Number, from 2008 (13-digit number starting 9790)
	case "25":
		*c = `ISMN-13`

	// Actionable ISBN, in fact a special DOI incorporating the ISBN-13 within the DOI syntax. Begins ‘10.978.’ or ‘10.979.’ and includes a / character between the registrant element (publisher prefix) and publication element of the ISBN, eg 10.978.000/1234567. Note the ISBN-A should always be accompanied by the ISBN itself, using codes 03 and/or 15
	case "26":
		*c = `ISBN-A`

	// E-publication identifier controlled by JPOIID’s Committee for Research and Management of Electronic Publishing Codes
	case "27":
		*c = `JP e-code`

	// Unique number assigned by the Chinese Online Library Cataloging Center (see http://olcc.nlc.gov.cn)
	case "28":
		*c = `OLCC number`

	// Japanese magazine identifier, similar in scope to ISSN but identifying a specific issue of a serial publication. Five digits to identify the periodical, plus a hyphen and two digits to identify the issue
	case "29":
		*c = `JP Magazine ID`

	// Used only with comic books and other products which use the UPC extension to identify individual issues or products. Do not use where the UPC12 itself identifies the specific product, irrespective of any 5-digit extension – use code 04 instead
	case "30":
		*c = `UPC12+5`

	// Numéro de la notice bibliographique BNF
	case "31":
		*c = `BNF Control number`

	// Archival Resource Key, as a URL (including the address of the ARK resolver provided by eg a national library)
	case "35":
		*c = `ARK`
	default:
		return fmt.Errorf("undefined code for ProductIdentifierTypeCode has been passed, got [%s]", v)
	}
	return nil
}

// MeasureUnitCode List 50: Measure unit code
type MeasureUnitCode string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *MeasureUnitCode) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// Millimeters are the preferred metric unit of length
	case "cm":
		*c = `Centimeters`

	//
	case "gr":
		*c = `Grams`

	//
	case "in":
		*c = `Inches (US)`

	// Grams are the preferred metric unit of weight
	case "kg":
		*c = `Kilograms`

	//
	case "lb":
		*c = `Pounds (US)`

	//
	case "mm":
		*c = `Millimeters`

	//
	case "oz":
		*c = `Ounces (US)`

	//
	case "px":
		*c = `Pixels`
	default:
		return fmt.Errorf("undefined code for MeasureUnitCode has been passed, got [%s]", v)
	}
	return nil
}

// ProductRelationCode List 51: Product relation code
type ProductRelationCode string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *ProductRelationCode) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// <Product> is related to <RelatedProduct> in a way that cannot be specified by another code value
	case "00":
		*c = `Unspecified`

	// <Product> includes <RelatedProduct>
	case "01":
		*c = `Includes`

	// <Product> is part of <RelatedProduct>: use for ‘also available as part of’
	case "02":
		*c = `Is part of`

	// <Product> replaces, or is new edition of, <RelatedProduct>
	case "03":
		*c = `Replaces`

	// <Product> is replaced by, or has new edition, <RelatedProduct> (reciprocal of code 03)
	case "05":
		*c = `Replaced by`

	// <Product> is available in an alternative format as <RelatedProduct> – indicates an alternative format of the same content which is or may be available
	case "06":
		*c = `Alternative format`

	// <Product> has an ancillary or supplementary product <RelatedProduct>
	case "07":
		*c = `Has ancillary product`

	// <Product> is ancillary or supplementary to <RelatedProduct>
	case "08":
		*c = `Is ancillary to`

	// <Product> is remaindered as <RelatedProduct>, when a remainder merchant assigns its own identifier to the product
	case "09":
		*c = `Is remaindered as`

	// <Product> was originally sold as <RelatedProduct>, indicating the publisher’s original identifier for a title which is offered as a remainder under a different identifier (reciprocal of code 09)
	case "10":
		*c = `Is remainder of`

	// <Product> is an other-language version of <RelatedProduct>
	case "11":
		*c = `Is other-language version of`

	// <Product> has a publisher’s suggested alternative <RelatedProduct>, which does not, however, carry the same content (cf 05 and 06)
	case "12":
		*c = `Publisher’s suggested alternative`

	// <Product> is an epublication based on printed product <RelatedProduct>
	case "13":
		*c = `Epublication based on (print product)`

	// <Product> is an epublication ‘rendered’ as <RelatedProduct>: use in ONIX 2.1 only when the <Product> record describes a package of electronic content which is available in multiple ‘renderings’ (coded 000 in <EpubTypeCode>): NOT USED in ONIX 3.0
	case "14":
		*c = `Epublication is distributed as`

	// <Product> is a ‘rendering’ of an epublication <RelatedProduct>: use in ONIX 2.1 only when the <Product> record describes a specific rendering of an epublication content package, to identify the package: NOT USED in ONIX 3.0
	case "15":
		*c = `Epublication is a rendering of`

	// <Product> is a POD replacement for <RelatedProduct>. <RelatedProduct> is an out-of-print product replaced by a print-on-demand version under a new ISBN
	case "16":
		*c = `POD replacement for`

	// <Product> is replaced by POD <RelatedProduct>. <RelatedProduct> is a print-on-demand replacement, under a new ISBN, for an out-of-print <Product> (reciprocal of code 16)
	case "17":
		*c = `Replaced by POD`

	// <Product> is a special edition of <RelatedProduct>. Used for a special edition (German: Sonderausgabe) with different cover, binding, premium content etc – more than ‘alternative format’ – which may be available in limited quantity and for a limited time
	case "18":
		*c = `Is special edition of`

	// <Product> has a special edition <RelatedProduct> (reciprocal of code 18)
	case "19":
		*c = `Has special edition`

	// <Product> is a prebound edition of <RelatedProduct> (in the US, a prebound edition is ‘a book that was previously bound and has been rebound with a library quality hardcover binding. In almost all commercial cases, the book in question began as a paperback.’)
	case "20":
		*c = `Is prebound edition of`

	// <Product> is the regular edition of which <RelatedProduct> is a prebound edition
	case "21":
		*c = `Is original of prebound edition`

	// <Product> and <RelatedProduct> have a common author
	case "22":
		*c = `Product by same author`

	// <RelatedProduct> is another product that is suggested as similar to <Product> (‘if you liked <Product>, you may also like <RelatedProduct>’, or vice versa)
	case "23":
		*c = `Similar product`

	// <Product> is a facsimile edition of <RelatedProduct>
	case "24":
		*c = `Is facsimile of`

	// <Product> is the original edition from which a facsimile edition <RelatedProduct> is taken (reciprocal of code 25)
	case "25":
		*c = `Is original of facsimile`

	// <Product> is a license for a digital <RelatedProduct>, traded or supplied separately
	case "26":
		*c = `Is license for`

	// <RelatedProduct> is an electronic version of print <Product> (reciprocal of code 13)
	case "27":
		*c = `Electronic version available as`

	// <RelatedProduct> is an ‘enhanced’ version of <Product>, with additional content. Typically used to link an enhanced e-book to its original ‘unenhanced’ equivalent, but not specifically limited to linking e-books – for example, may be used to link illustrated and non-illustrated print books. <Product> and <RelatedProduct> should share the same <ProductForm>
	case "28":
		*c = `Enhanced version available as`

	// <RelatedProduct> is a basic version of <Product> (reciprocal of code 28). <Product> and <RelatedProduct> should share the same <ProductForm>
	case "29":
		*c = `Basic version available as`

	// <RelatedProduct> and <Product> are part of the same collection (eg two products in same series or set)
	case "30":
		*c = `Product in same collection`

	// <RelatedProduct> is an alternative product in another sector (of the same geographical market). Indicates an alternative that carries the same content, but available to a different set of customers, as one or both products are retailer-, channel- or market sector-specific
	case "31":
		*c = `Has alternative in a different market sector`

	// <RelatedProduct> is an equivalent product, often intended for another (geographical) market. Indicates an alternative that carries essentially the same content, though slightly adapted for local circumstances (as opposed to a translation – use code 11)
	case "32":
		*c = `Has equivalent intended for a different market`

	// <RelatedProduct> is an alternative product, often intended for another (geographical) market. Indicates the content of the alternative is identical in all respects
	case "33":
		*c = `Has alternative intended for different market`

	// <Product> cites <RelatedProduct>
	case "34":
		*c = `Cites`

	// <Product> is the object of a citation in <RelatedProduct>
	case "35":
		*c = `Is cited by`

	// Use to give the ISBN of another book that had sales (both in terms of copy numbers and customer profile) comparable to that the publisher or distributor estimates for the product. Use in ONIX 2.1 ONLY
	case "36":
		*c = `Sales expectation`

	// <Product> is a signed copy of <RelatedProduct>. Use where signed copies are given a distinct product identifier and can be ordered separately, but are otherwise identical
	case "37":
		*c = `Is signed version of`

	// <Product> is an unsigned copy of <RelatedProduct>. Use where signed copies are given a distinct product identifier and can be ordered separately, but are otherwise identical
	case "38":
		*c = `Has signed version`

	// <Product> is intended for teacher use, and the related product is for student use
	case "39":
		*c = `Has related student material`

	// <Product> is intended for student use, and the related product is for teacher use
	case "40":
		*c = `Has related teacher material`

	// <Product> includes some content shared with <RelatedProduct>. Note the shared content does not form the whole of either product. Compare with the includes / is part of relationship pair, where the shared content forms the whole of one of the products, and with the alternative format relationship, where the shared content forms the whole of both products
	case "41":
		*c = `Some content shared with`

	// <Product> is a later edition of <RelatedProduct>, where the related product is the first edition
	case "42":
		*c = `Is later edition of first edition`
	default:
		return fmt.Errorf("undefined code for ProductRelationCode has been passed, got [%s]", v)
	}
	return nil
}

// SupplyToRegionCode List 52: Supply-to region code
type SupplyToRegionCode string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *SupplyToRegionCode) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// When the same ISBN is used for open market and UK editions
	case "004":
		*c = `UK ‘open market’`
	default:
		return fmt.Errorf("undefined code for SupplyToRegionCode has been passed, got [%s]", v)
	}
	return nil
}

// ReturnsConditionsCodeType List 53: Returns conditions code type
type ReturnsConditionsCodeType string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *ReturnsConditionsCodeType) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// As specified in <ReturnsCodeTypeName> (ONIX 3.0 only)
	case "00":
		*c = `Proprietary`

	// Maintained by CLIL (Commission Interprofessionnel du Livre). Returns conditions values in <ReturnsCode> should be taken from the CLIL list
	case "01":
		*c = `French book trade returns conditions code`

	// Maintained by BISAC: Returns conditions values in <ReturnsCode> should be taken from List 66
	case "02":
		*c = `BISAC Returnable Indicator code`

	// NOT CURRENTLY USED – BIC has decided that it will not maintain a code list for this purpose, since returns conditions are usually at least partly based on the trading relationship
	case "03":
		*c = `UK book trade returns conditions code`

	// Returns conditions values in <ReturnsCode> should be taken from List 204
	case "04":
		*c = `ONIX Returns conditions code`
	default:
		return fmt.Errorf("undefined code for ReturnsConditionsCodeType has been passed, got [%s]", v)
	}
	return nil
}

// AvailabilityStatusCode List 54: Availability status code
type AvailabilityStatusCode string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *AvailabilityStatusCode) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// Publication abandoned after having been announced
	case "AB":
		*c = `Cancelled`

	// Apply direct to publisher, item not available to trade
	case "AD":
		*c = `Available direct from publisher only`

	// Check with customer service
	case "CS":
		*c = `Availability uncertain`

	// Wholesaler or vendor only
	case "EX":
		*c = `No longer stocked by us`

	// In-print and in stock
	case "IP":
		*c = `Available`

	// May be accompanied by an estimated average time to supply
	case "MD":
		*c = `Manufactured on demand`

	// MUST be accompanied by an expected availability date
	case "NP":
		*c = `Not yet published`

	// Wholesaler or vendor only: MUST be accompanied by expected availability date
	case "NY":
		*c = `Newly catalogued, not yet in stock`

	// This format is out of print, but another format is available: should be accompanied by an identifier for the alternative product
	case "OF":
		*c = `Other format available`

	// No current plan to reprint
	case "OI":
		*c = `Out of stock indefinitely`

	// Discontinued, deleted from catalogue
	case "OP":
		*c = `Out of print`

	// This edition is out of print, but a new edition has been or will soon be published: should be accompanied by an identifier for the new edition
	case "OR":
		*c = `Replaced by new edition`

	// Publication has been announced, and subsequently postponed with no new date
	case "PP":
		*c = `Publication postponed indefinitely`

	// Supply of this item has been transferred to another publisher or distributor: should be accompanied by an identifier for the new supplier
	case "RF":
		*c = `Refer to another supplier`

	//
	case "RM":
		*c = `Remaindered`

	// MUST be accompanied by an expected availability date
	case "RP":
		*c = `Reprinting`

	// Use instead of RP as a last resort, only if it is really impossible to give an expected availability date
	case "RU":
		*c = `Reprinting, undated`

	// This item is not stocked but has to be specially ordered from a supplier (eg import item not stocked locally): may be accompanied by an estimated average time to supply
	case "TO":
		*c = `Special order`

	// Wholesaler or vendor only
	case "TP":
		*c = `Temporarily out of stock because publisher cannot supply`

	// MUST be accompanied by an expected availability date
	case "TU":
		*c = `Temporarily unavailable`

	// The item is out of stock but will be reissued under the same ISBN: MUST be accompanied by an expected availability date and by the reissue date in the <Reissue> composite. See notes on the <Reissue> composite for details on treatment of availability status during reissue
	case "UR":
		*c = `Unavailable, awaiting reissue`

	// MUST be accompanied by the remainder date
	case "WR":
		*c = `Will be remaindered as of (date)`

	// Typically, withdrawn indefinitely for legal reasons
	case "WS":
		*c = `Withdrawn from sale`
	default:
		return fmt.Errorf("undefined code for AvailabilityStatusCode has been passed, got [%s]", v)
	}
	return nil
}

// DateFormat List 55: Date format
type DateFormat string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *DateFormat) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// Year month day (default)
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

	// Year
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

	// For complex, approximate or uncertain dates
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
		return fmt.Errorf("undefined code for DateFormat has been passed, got [%s]", v)
	}
	return nil
}

// AudienceRestrictionFlag List 56: Audience restriction flag
type AudienceRestrictionFlag string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *AudienceRestrictionFlag) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	//
	case "R":
		*c = `Restrictions apply, see note`

	// Indexed for the German market – in Deutschland indiziert
	case "X":
		*c = `Indiziert`
	default:
		return fmt.Errorf("undefined code for AudienceRestrictionFlag has been passed, got [%s]", v)
	}
	return nil
}

// UnpricedItemTypeCode List 57: Unpriced item type code
type UnpricedItemTypeCode string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *UnpricedItemTypeCode) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	//
	case "01":
		*c = `Free of charge`

	//
	case "02":
		*c = `Price to be announced`

	// Not sold separately at retail
	case "03":
		*c = `Not sold separately`

	// May be used for books that do not carry a recommended retail price; when goods can only be ordered ‘in person’ from a sales representative; when an ONIX file is ‘broadcast’ rather than sent one-to-one to a single trading partner; or for digital products offered on subscription or with pricing which is too complex to specify in ONIX
	case "04":
		*c = `Contact supplier`

	// When a collection that is not sold as a set nevertheless has its own ONIX record
	case "05":
		*c = `Not sold as set`

	// Unpriced, but available via a pre-determined revenue share agreement
	case "06":
		*c = `Revenue share`
	default:
		return fmt.Errorf("undefined code for UnpricedItemTypeCode has been passed, got [%s]", v)
	}
	return nil
}

// PriceTypeCode List 58: Price type code
type PriceTypeCode string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *PriceTypeCode) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// RRP excluding any sales tax or value-added tax
	case "01":
		*c = `RRP excluding tax`

	// RRP including sales or value-added tax if applicable
	case "02":
		*c = `RRP including tax`

	// In countries where retail price maintenance applies by law to certain products: not used in USA
	case "03":
		*c = `Fixed retail price excluding tax`

	// In countries where retail price maintenance applies by law to certain products: not used in USA
	case "04":
		*c = `Fixed retail price including tax`

	// Unit price charged by supplier to reseller excluding any sales tax or value-added tax: goods for retail sale
	case "05":
		*c = `Supplier’s net price excluding tax`

	// Unit price charged by supplier to reseller / rental outlet, excluding any sales tax or value-added tax: goods for rental (used for video and DVD)
	case "06":
		*c = `Supplier’s net price excluding tax: rental goods`

	// Unit price charged by supplier to reseller including any sales tax or value-added tax if applicable: goods for retail sale
	case "07":
		*c = `Supplier’s net price including tax`

	// Unit price charged by supplier to a specified class of reseller excluding any sales tax or value-added tax: goods for retail sale (this value is for use only in countries, eg Finland, where trade practice requires two different net prices to be listed for different classes of resellers, and where national guidelines specify how the code should be used)
	case "08":
		*c = `Supplier’s alternative net price excluding tax`

	// Unit price charged by supplier to a specified class of reseller including any sales tax or value-added tax: goods for retail sale (this value is for use only in countries, eg Finland, where trade practice requires two different net prices to be listed for different classes of resellers, and where national guidelines specify how the code should be used)
	case "09":
		*c = `Supplier’s alternative net price including tax`

	// Special sale RRP excluding any sales tax or value-added tax. Note ‘special sales’ are sales where terms and conditions are different from normal trade sales, when for example products that are normally sold on a sale-or-return basis are sold on firm-sale terms, where a particular product is tailored for a specific retail outlet (often termed a ‘premium’ product), or where other specific conditions or qualiifications apply. Further details of the modified terms and conditions should be given in <PriceTypeDescription>
	case "11":
		*c = `Special sale RRP excluding tax`

	// Special sale RRP including sales or value-added tax if applicable
	case "12":
		*c = `Special sale RRP including tax`

	// In countries where retail price maintenance applies by law to certain products: not used in USA
	case "13":
		*c = `Special sale fixed retail price excluding tax`

	// In countries where retail price maintenance applies by law to certain products: not used in USA
	case "14":
		*c = `Special sale fixed retail price including tax`

	// Unit price charged by supplier to reseller for special sale excluding any sales tax or value-added tax
	case "15":
		*c = `Supplier’s net price for special sale excluding tax`

	// Unit price charged by supplier to reseller for special sale including any sales tax or value-added tax
	case "17":
		*c = `Supplier’s net price for special sale including tax`

	// Pre-publication RRP excluding any sales tax or value-added tax. Use where RRP for pre-orders is different from post-publication RRP
	case "21":
		*c = `Pre-publication RRP excluding tax`

	// Pre-publication RRP including sales or value-added tax if applicable. Use where RRP for pre-orders is different from post-publication RRP
	case "22":
		*c = `Pre-publication RRP including tax`

	// In countries where retail price maintenance applies by law to certain products: not used in USA
	case "23":
		*c = `Pre-publication fixed retail price excluding tax`

	// In countries where retail price maintenance applies by law to certain products: not used in USA
	case "24":
		*c = `Pre-publication fixed retail price including tax`

	// Unit price charged by supplier to reseller pre-publication excluding any sales tax or value-added tax
	case "25":
		*c = `Supplier’s pre-publication net price excluding tax`

	// Unit price charged by supplier to reseller pre-publication including any sales tax or value-added tax
	case "27":
		*c = `Supplier’s pre-publication net price including tax`

	// In the US, books are sometimes supplied on ‘freight-pass-through’ terms, where a price that is different from the RRP is used as the basis for calculating the supplier’s charge to a reseller. To make it clear when such terms are being invoked, code 31 is used instead of code 01 to indicate the RRP. Code 32 is used for the ‘billing price’
	case "31":
		*c = `Freight-pass-through RRP excluding tax`

	// When freight-pass-through terms apply, the price on which the supplier’s charge to a reseller is calculated, ie the price to which trade discount terms are applied. See also code 31
	case "32":
		*c = `Freight-pass-through billing price excluding tax`

	// In countries where retail price maintenance applies by law to certain products, but the price is set by the importer or local sales agent, not the foreign publisher. In France, ‘prix catalogue éditeur étranger’
	case "33":
		*c = `Importer’s Fixed retail price excluding tax`

	// In countries where retail price maintenance applies by law to certain products, but the price is set by the importer or local sales agent, not the foreign publisher. In France, ‘prix catalogue éditeur étranger’
	case "34":
		*c = `Importer’s Fixed retail price including tax`

	// For a product supplied on agency terms, the retail price set by the publisher, excluding any sales tax or value-added tax
	case "41":
		*c = `Publishers retail price excluding tax`

	// For a product supplied on agency terms, the retail price set by the publisher, including sales or value-added tax if applicable
	case "42":
		*c = `Publishers retail price including tax`
	default:
		return fmt.Errorf("undefined code for PriceTypeCode has been passed, got [%s]", v)
	}
	return nil
}

// PriceTypeQualifier List 59: Price type qualifier
type PriceTypeQualifier string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *PriceTypeQualifier) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// Price applies to all customers that do not fall within any other group with a specified group-specific qualified price
	case "00":
		*c = `Unqualified price`

	// Price applies to a designated group membership
	case "01":
		*c = `Member/subscriber price`

	// Price applies to sales outside the territory in which the supplier is located
	case "02":
		*c = `Export price`

	// Use in cases where there is no combined price, but a lower price is offered for each part if the whole set / series / collection is purchased (either at one time, as part of a continuing commitment, or in a single purchase)
	case "03":
		*c = `Reduced price applicable when the item is purchased as part of a set (or series, or collection)`

	// In the Netherlands (or any other market where similar arrangements exist): a reduced fixed price available for a limited time on presentation of a voucher or coupon published in a specified medium, eg a newspaper. Should be accompanied by Price Type code 13 and additional detail in <PriceTypeDescription>, and by validity dates in <PriceEffectiveFrom> and <PriceEffectiveUntil> (ONIX 2.1) or in the <PriceDate> composite (ONIX 3.0)
	case "04":
		*c = `Voucher price`

	// Price for individual consumer sale only
	case "05":
		*c = `Consumer price`

	// Price for sale to libraries or other corporate or institutional customers
	case "06":
		*c = `Corporate / Library / Education price`

	// Price valid for a specified period prior to publication. Orders placed prior to the end of the period are guaranteed to be delivered to the retailer before the nominal publication date. The price may or may not be different from the ‘normal’ price, which carries no such delivery guarantee. Must be accompanied by a <PriceEffectiveUntil> date (or equivalent <PriceDate> composite in ONIX 3), and should also be accompanied by a ‘normal’ price
	case "07":
		*c = `Reservation order price`

	// Temporary ‘Special offer’ price. Must be accompanied by <PriceEffectiveFrom> and <PriceEffectiveUntil> dates (or equivalent <PriceDate> composites in ONIX 3), and may also be accompanied by a ‘normal’ price
	case "08":
		*c = `Promotional offer price`

	// Price requires purchase with, or proof of ownership of another product. Further details of purchase or ownership requirements must be given in <PriceTypeDescription>
	case "09":
		*c = `Linked price`

	// Price for sale only to libraries (including public, school and academic libraries)
	case "10":
		*c = `Library price`

	// Price for sale only to educational institutions (including school and academic libraries), educational buying consortia, government and local government bodies purchasing for use in education
	case "11":
		*c = `Education price`

	// Price for sale to corporate customers only
	case "12":
		*c = `Corporate price`

	// Price for sale to organisations or services offering consumers subscription access to a library of books
	case "13":
		*c = `Subscription service price`

	// Price for primary and secondary education
	case "14":
		*c = `School library price`

	// Price for higher education and scholarly institutions
	case "15":
		*c = `Academic library price`

	//
	case "16":
		*c = `Public library price`
	default:
		return fmt.Errorf("undefined code for PriceTypeQualifier has been passed, got [%s]", v)
	}
	return nil
}

// BarcodeIndicator List 6: Barcode indicator
type BarcodeIndicator string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *BarcodeIndicator) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	//
	case "00":
		*c = `Not barcoded`

	//
	case "01":
		*c = `Barcoded, scheme unspecified`

	// Position unspecified
	case "02":
		*c = `EAN13`

	// Position unspecified
	case "03":
		*c = `EAN13+5 (US dollar price encoded)`

	// Type and position unspecified. DEPRECATED: if possible, use more specific values below
	case "04":
		*c = `UPC12`

	// Type and position unspecified. DEPRECATED: if possible, use more specific values below
	case "05":
		*c = `UPC12+5`

	// AKA item/price: position unspecified
	case "06":
		*c = `UPC12 (item-specific)`

	// AKA item/price: position unspecified
	case "07":
		*c = `UPC12+5 (item-specific)`

	// AKA price/item: position unspecified
	case "08":
		*c = `UPC12 (price-point)`

	// AKA price/item: position unspecified
	case "09":
		*c = `UPC12+5 (price-point)`

	// ‘Cover 4’ is defined as the back cover of a book
	case "10":
		*c = `EAN13 on cover 4`

	// ‘Cover 4’ is defined as the back cover of a book
	case "11":
		*c = `EAN13+5 on cover 4 (US dollar price encoded)`

	// AKA item/price; ‘cover 4’ is defined as the back cover of a book
	case "12":
		*c = `UPC12 (item-specific) on cover 4`

	// AKA item/price; ‘cover 4’ is defined as the back cover of a book
	case "13":
		*c = `UPC12+5 (item-specific) on cover 4`

	// AKA price/item; ‘cover 4’ is defined as the back cover of a book
	case "14":
		*c = `UPC12 (price-point) on cover 4`

	// AKA price/item; ‘cover 4’ is defined as the back cover of a book
	case "15":
		*c = `UPC12+5 (price-point) on cover 4`

	// ‘Cover 3’ is defined as the inside back cover of a book
	case "16":
		*c = `EAN13 on cover 3`

	// ‘Cover 3’ is defined as the inside back cover of a book
	case "17":
		*c = `EAN13+5 on cover 3 (US dollar price encoded)`

	// AKA item/price; ‘cover 3’ is defined as the inside back cover of a book
	case "18":
		*c = `UPC12 (item-specific) on cover 3`

	// AKA item/price; ‘cover 3’ is defined as the inside back cover of a book
	case "19":
		*c = `UPC12+5 (item-specific) on cover 3`

	// AKA price/item; ‘cover 3’ is defined as the inside back cover of a book
	case "20":
		*c = `UPC12 (price-point) on cover 3`

	// AKA price/item; ‘cover 3’ is defined as the inside back cover of a book
	case "21":
		*c = `UPC12+5 (price-point) on cover 3`

	// ‘Cover 2’ is defined as the inside front cover of a book
	case "22":
		*c = `EAN13 on cover 2`

	// ‘Cover 2’ is defined as the inside front cover of a book
	case "23":
		*c = `EAN13+5 on cover 2 (US dollar price encoded)`

	// AKA item/price; ‘cover 2’ is defined as the inside front cover of a book
	case "24":
		*c = `UPC12 (item-specific) on cover 2`

	// AKA item/price; ‘cover 2’ is defined as the inside front cover of a book
	case "25":
		*c = `UPC12+5 (item-specific) on cover 2`

	// AKA price/item; ‘cover 2’ is defined as the inside front cover of a book
	case "26":
		*c = `UPC12 (price-point) on cover 2`

	// AKA price/item; ‘cover 2’ is defined as the inside front cover of a book
	case "27":
		*c = `UPC12+5 (price-point) on cover 2`

	// To be used only on boxed products
	case "28":
		*c = `EAN13 on box`

	// To be used only on boxed products
	case "29":
		*c = `EAN13+5 on box (US dollar price encoded)`

	// AKA item/price; to be used only on boxed products
	case "30":
		*c = `UPC12 (item-specific) on box`

	// AKA item/price; to be used only on boxed products
	case "31":
		*c = `UPC12+5 (item-specific) on box`

	// AKA price/item; to be used only on boxed products
	case "32":
		*c = `UPC12 (price-point) on box`

	// AKA price/item; to be used only on boxed products
	case "33":
		*c = `UPC12+5 (price-point) on box`

	// To be used only on products fitted with hanging tags
	case "34":
		*c = `EAN13 on tag`

	// To be used only on products fitted with hanging tags
	case "35":
		*c = `EAN13+5 on tag (US dollar price encoded)`

	// AKA item/price; to be used only on products fitted with hanging tags
	case "36":
		*c = `UPC12 (item-specific) on tag`

	// AKA item/price; to be used only on products fitted with hanging tags
	case "37":
		*c = `UPC12+5 (item-specific) on tag`

	// AKA price/item; to be used only on products fitted with hanging tags
	case "38":
		*c = `UPC12 (price-point) on tag`

	// AKA price/item; to be used only on products fitted with hanging tags
	case "39":
		*c = `UPC12+5 (price-point) on tag`

	// Not be used on books unless they are contained within outer packaging
	case "40":
		*c = `EAN13 on bottom`

	// Not be used on books unless they are contained within outer packaging
	case "41":
		*c = `EAN13+5 on bottom (US dollar price encoded)`

	// AKA item/price; not be used on books unless they are contained within outer packaging
	case "42":
		*c = `UPC12 (item-specific) on bottom`

	// AKA item/price; not be used on books unless they are contained within outer packaging
	case "43":
		*c = `UPC12+5 (item-specific) on bottom`

	// AKA price/item; not be used on books unless they are contained within outer packaging
	case "44":
		*c = `UPC12 (price-point) on bottom`

	// AKA price/item; not be used on books unless they are contained within outer packaging
	case "45":
		*c = `UPC12+5 (price-point) on bottom`

	// Not be used on books unless they are contained within outer packaging
	case "46":
		*c = `EAN13 on back`

	// Not be used on books unless they are contained within outer packaging
	case "47":
		*c = `EAN13+5 on back (US dollar price encoded)`

	// AKA item/price; not be used on books unless they are contained within outer packaging
	case "48":
		*c = `UPC12 (item-specific) on back`

	// AKA item/price; not be used on books unless they are contained within outer packaging
	case "49":
		*c = `UPC12+5 (item-specific) on back`

	// AKA price/item; not be used on books unless they are contained within outer packaging
	case "50":
		*c = `UPC12 (price-point) on back`

	// AKA price/item; not be used on books unless they are contained within outer packaging
	case "51":
		*c = `UPC12+5 (price-point) on back`

	// To be used only on products packaged in outer sleeves
	case "52":
		*c = `EAN13 on outer sleeve/back`

	// To be used only on products packaged in outer sleeves
	case "53":
		*c = `EAN13+5 on outer sleeve/back (US dollar price encoded)`

	// AKA item/price; to be used only on products packaged in outer sleeves
	case "54":
		*c = `UPC12 (item-specific) on outer sleeve/back`

	// AKA item/price; to be used only on products packaged in outer sleeves
	case "55":
		*c = `UPC12+5 (item-specific) on outer sleeve/back`

	// AKA price/item; to be used only on products packaged in outer sleeves
	case "56":
		*c = `UPC12 (price-point) on outer sleeve/back`

	// AKA price/item; to be used only on products packaged in outer sleeves
	case "57":
		*c = `UPC12+5 (price-point) on outer sleeve/back`

	// Position unspecified
	case "58":
		*c = `EAN13+5 (no price encoded)`

	// ‘Cover 4’ is defined as the back cover of a book
	case "59":
		*c = `EAN13+5 on cover 4 (no price encoded)`

	// ‘Cover 3’ is defined as the inside back cover of a book
	case "60":
		*c = `EAN13+5 on cover 3 (no price encoded)`

	// ‘Cover 2’ is defined as the inside front cover of a book
	case "61":
		*c = `EAN13+5 on cover 2 (no price encoded)`

	// To be used only on boxed products
	case "62":
		*c = `EAN13+5 on box (no price encoded)`

	// To be used only on products fitted with hanging tags
	case "63":
		*c = `EAN13+5 on tag (no price encoded)`

	// Not be used on books unless they are contained within outer packaging
	case "64":
		*c = `EAN13+5 on bottom (no price encoded)`

	// Not be used on books unless they are contained within outer packaging
	case "65":
		*c = `EAN13+5 on back (no price encoded)`

	// To be used only on products packaged in outer sleeves
	case "66":
		*c = `EAN13+5 on outer sleeve/back (no price encoded)`

	// Position unspecified
	case "67":
		*c = `EAN13+5 (CAN dollar price encoded)`

	// ‘Cover 4’ is defined as the back cover of a book
	case "68":
		*c = `EAN13+5 on cover 4 (CAN dollar price encoded)`

	// ‘Cover 3’ is defined as the inside back cover of a book
	case "69":
		*c = `EAN13+5 on cover 3 (CAN dollar price encoded)`

	// ‘Cover 2’ is defined as the inside front cover of a book
	case "70":
		*c = `EAN13+5 on cover 2 (CAN dollar price encoded)`

	// To be used only on boxed products
	case "71":
		*c = `EAN13+5 on box (CAN dollar price encoded)`

	// To be used only on products fitted with hanging tags
	case "72":
		*c = `EAN13+5 on tag (CAN dollar price encoded)`

	// Not be used on books unless they are contained within outer packaging
	case "73":
		*c = `EAN13+5 on bottom (CAN dollar price encoded)`

	// Not be used on books unless they are contained within outer packaging
	case "74":
		*c = `EAN13+5 on back (CAN dollar price encoded)`

	// To be used only on products packaged in outer sleeves
	case "75":
		*c = `EAN13+5 on outer sleeve/back (CAN dollar price encoded)`
	default:
		return fmt.Errorf("undefined code for BarcodeIndicator has been passed, got [%s]", v)
	}
	return nil
}

// UnitOfPricingCode List 60: Unit of pricing code
type UnitOfPricingCode string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *UnitOfPricingCode) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// Default
	case "00":
		*c = `Per copy of whole product`

	//
	case "01":
		*c = `Per page for printed loose-leaf content only`
	default:
		return fmt.Errorf("undefined code for UnitOfPricingCode has been passed, got [%s]", v)
	}
	return nil
}

// PriceStatusCode List 61: Price status code
type PriceStatusCode string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *PriceStatusCode) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// Default
	case "00":
		*c = `Unspecified`

	//
	case "01":
		*c = `Provisional`

	//
	case "02":
		*c = `Firm`
	default:
		return fmt.Errorf("undefined code for PriceStatusCode has been passed, got [%s]", v)
	}
	return nil
}

// TaxRateCoded List 62: Tax rate, coded
type TaxRateCoded string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *TaxRateCoded) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// Specifies that tax is applied at a higher rate than standard
	case "H":
		*c = `Higher rate`

	// Under Italian tax rules, VAT on books may be paid at source by the publisher, and subsequent transactions through the supply chain are tax-exempt
	case "P":
		*c = `Tax paid at source (Italy)`

	// Specifies that tax is applied at a lower rate than standard
	case "R":
		*c = `Lower rate`

	//
	case "S":
		*c = `Standard rate`

	//
	case "Z":
		*c = `Zero-rated`
	default:
		return fmt.Errorf("undefined code for TaxRateCoded has been passed, got [%s]", v)
	}
	return nil
}

// IntermediarySupplierAvailability List 63: Intermediary supplier availability
type IntermediarySupplierAvailability string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *IntermediarySupplierAvailability) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {
	default:
		return fmt.Errorf("undefined code for IntermediarySupplierAvailability has been passed, got [%s]", v)
	}
	return nil
}

// PublishingStatus List 64: Publishing status
type PublishingStatus string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *PublishingStatus) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// Status is not specified (as distinct from unknown): the default if the <PublishingStatus> element is not sent. Also to be used in applications where the element is considered mandatory, but the sender of the ONIX message chooses not to pass on status information
	case "00":
		*c = `Unspecified`

	// The product was announced, and subsequently abandoned; the <PublicationDate> element in ONIX 2.1 or its equivalent in <PublishingDate> in ONIX 3.0 must not be sent
	case "01":
		*c = `Cancelled`

	// Not yet published, must be accompanied by the expected date in <PublicationDate> in ONIX 2.1, or its equivalent in the <PublishingDate> composite in ONIX 3.0
	case "02":
		*c = `Forthcoming`

	// The product was announced, and subsequently postponed with no expected publication date; the <Publication Date> element in ONIX 2.1, or its equivalent as a <PublishingDate> composite in ONIX 3.0, must not be sent
	case "03":
		*c = `Postponed indefinitely`

	// The product was published, and is still active in the sense that the publisher will accept orders for it, though it may or may not be immediately available, for which see <SupplyDetail>
	case "04":
		*c = `Active`

	// Ownership of the product has been transferred to another publisher (with details of acquiring publisher if possible in PR.19 (ONIX 2.1) OR P.19 (ONIX 3.0))
	case "05":
		*c = `No longer our product`

	// The product was active, but is now inactive in the sense that (a) the publisher cannot fulfill orders for it, though stock may still be available elsewhere in the supply chain, and (b) there are no current plans to bring it back into stock. Use this code for ‘reprint under consideration’. Code 06 does not specifically imply that returns are or are not still accepted
	case "06":
		*c = `Out of stock indefinitely`

	// The product was active, but is now permanently inactive in the sense that (a) the publisher will not accept orders for it, though stock may still be available elsewhere in the supply chain, and (b) the product will not be made available again under the same ISBN. Code 07 normally implies that the publisher will not accept returns beyond a specified date
	case "07":
		*c = `Out of print`

	// The product was active, but is now permanently or indefinitely inactive in the sense that the publisher will not accept orders for it, though stock may still be available elsewhere in the supply chain. Code 08 covers both of codes 06 and 07, and may be used where the distinction between those values is either unnecessary or meaningless
	case "08":
		*c = `Inactive`

	// The sender of the ONIX record does not know the current publishing status
	case "09":
		*c = `Unknown`

	// The product is no longer available from the current publisher, under the current ISBN, at the current price. It may be available to be traded through another channel. A Publishing Status code 10 ‘Remaindered’ usually but not always means that the publisher has decided to sell off excess inventory of the book. Copies of books that are remaindered are often made available in the supply chain at a reduced price. However, such remainders are often sold under a product identifier that differs from the ISBN on the full-priced copy of the book. A Publishing Status code 10 ‘Remaindered’ on a given product record may or may not be followed by a Publishing Status code 06 ‘Out of Stock Indefinitely’ or 07 ‘Out of Print’: the practise varies from one publisher to another. Some publishers may revert to a Publishing Status code 04 ‘Active’ if a desired inventory level on the product in question has subsequently been reached. No change in rights should ever be inferred from this (or any other) Publishing Status code value
	case "10":
		*c = `Remaindered`

	// Withdrawn, typically for legal reasons or to avoid giving offence
	case "11":
		*c = `Withdrawn from sale`

	// Recalled for reasons of consumer safety. Deprecated, use code 15 instead
	case "12":
		*c = `Recalled`

	// The product is published and active but, as a publishing decision, it is not sold separately – only in an assembly or as part of a pack. Depending on product composition and pricing, it may be saleable separately at retail
	case "13":
		*c = `Active, but not sold separately`

	// Recalled for reasons of consumer safety
	case "15":
		*c = `Recalled`

	// Withdrawn temporarily, typically for quality or technical reasons. In ONIX 3.0, must be accompanied by expected availability date coded ‘22’ within the <PublishingDate> composite, except in exceptional circumstances where no date is known
	case "16":
		*c = `Temporarily withdrawn from sale`

	// Withdrawn permanently from sale in all markets. Effectively synonymous with ‘Out of print’ (code 07), but specific to downloadable and online digital products (where no ‘stock’ would remain in the supply chain)
	case "17":
		*c = `Permanently withdrawn from sale`
	default:
		return fmt.Errorf("undefined code for PublishingStatus has been passed, got [%s]", v)
	}
	return nil
}

// ProductAvailability List 65: Product availability
type ProductAvailability string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *ProductAvailability) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// Cancelled: product was announced, and subsequently abandoned
	case "01":
		*c = `Cancelled`

	// Not yet available (requires expected date, either as <ExpectedShipDate> (ONIX 2.1) or as <SupplyDate> with <SupplyDateRole> coded ‘08’ (ONIX 3.0), except in exceptional circumstances where no date is known)
	case "10":
		*c = `Not yet available`

	// Not yet available, but will be a stock item when available (requires expected date, either as <ExpectedShipDate> (ONIX 2.1) or as <SupplyDate> with <SupplyDateRole> coded ‘08’ (ONIX 3.0), except in exceptional circumstances where no date is known). Used particularly for imports which have been published in the country of origin but have not yet arrived in the importing country
	case "11":
		*c = `Awaiting stock`

	// Not yet available, to be published as print-on-demand only (requires expected date, either as <ExpectedShipDate> (ONIX 2.1) or as <SupplyDate> with <SupplyDateRole> coded ‘08’ (ONIX 3.0), except in exceptional circumstances where no date is known). May apply either to a POD successor to an existing conventional edition, when the successor will be published under a different ISBN (normally because different trade terms apply); or to a title that is being published as a POD original
	case "12":
		*c = `Not yet available, will be POD`

	// Available from us (form of availability unspecified)
	case "20":
		*c = `Available`

	// Available from us as a stock item
	case "21":
		*c = `In stock`

	// Available from us as a non-stock item, by special order
	case "22":
		*c = `To order`

	// Available from us by print-on-demand
	case "23":
		*c = `POD`

	// Temporarily unavailable: temporarily unavailable from us (reason unspecified) (requires expected date, either as <ExpectedShipDate> (ONIX 2.1) or as <SupplyDate> with <SupplyDateRole> coded ‘08’ (ONIX 3.0), except in exceptional circumstances where no date is known)
	case "30":
		*c = `Temporarily unavailable`

	// Stock item, temporarily out of stock (requires expected date, either as <ExpectedShipDate> (ONIX 2.1) or as <SupplyDate> with <SupplyDateRole> coded ‘08’ (ONIX 3.0), except in exceptional circumstances where no date is known)
	case "31":
		*c = `Out of stock`

	// Temporarily unavailable, reprinting (requires expected date, either as <ExpectedShipDate> (ONIX 2.1) or as <SupplyDate> with <SupplyDateRole> coded ‘08’ (ONIX 3.0), except in exceptional circumstances where no date is known)
	case "32":
		*c = `Reprinting`

	// Temporarily unavailable, awaiting reissue (requires expected date, either as <ExpectedShipDate> (ONIX 2.1) or as <SupplyDate> with <SupplyDateRole> coded ‘08’ (ONIX 3.0), except in exceptional circumstances where no date is known)
	case "33":
		*c = `Awaiting reissue`

	// May be for quality or technical reasons. Requires expected availability date, either as <ExpectedShipDate> (ONIX 2.1) or as <SupplyDate> with <SupplyDateRole> coded ‘08’ (ONIX 3.0), except in exceptional circumstances where no date is known
	case "34":
		*c = `Temporarily withdrawn from sale`

	// Not available from us (for any reason)
	case "40":
		*c = `Not available (reason unspecified)`

	// This product is unavailable, but a successor product or edition is or will be available from us (identify successor in <RelatedProduct>)
	case "41":
		*c = `Not available, replaced by new product`

	// This product is unavailable, but the same content is or will be available from us in an alternative format (identify other format product in <RelatedProduct>)
	case "42":
		*c = `Not available, other format available`

	// Identify new supplier in <NewSupplier> if possible
	case "43":
		*c = `No longer supplied by us`

	// Not available to trade, apply direct to publisher
	case "44":
		*c = `Apply direct`

	// Must be bought as part of a set or trade pack (identify set or pack in <RelatedProduct>)
	case "45":
		*c = `Not sold separately`

	// May be for legal reasons or to avoid giving offence
	case "46":
		*c = `Withdrawn from sale`

	// Remaindered
	case "47":
		*c = `Remaindered`

	// Out of print, but a print-on-demand edition is or will be available under a different ISBN. Use only when the POD successor has a different ISBN, normally because different trade terms apply
	case "48":
		*c = `Not available, replaced by POD`

	// Recalled for reasons of consumer safety
	case "49":
		*c = `Recalled`

	// When a collection that is not sold as a set nevertheless has its own ONIX record
	case "50":
		*c = `Not sold as set`

	// This product is unavailable, no successor product or alternative format is available or planned. Use this code only when the publisher has indicated the product is out of print
	case "51":
		*c = `Not available, publisher indicates OP`

	// This product is unavailable in this market, no successor product or alternative format is available or planned. Use this code when a publisher has indicated the product is permanently unavailable (in this market) while remaining available elsewhere
	case "52":
		*c = `Not available, publisher no longer sells product in this market`

	// Sender has not received any recent update for this product from the publisher/supplier (for use when the sender is a data aggregator): the definition of ‘recent’ must be specified by the aggregator, or by agreement between parties to an exchange
	case "97":
		*c = `No recent update received`

	// Sender is no longer receiving any updates from the publisher/supplier of this product (for use when the sender is a data aggregator)
	case "98":
		*c = `No longer receiving updates`

	// Availability not known to sender
	case "99":
		*c = `Contact supplier`
	default:
		return fmt.Errorf("undefined code for ProductAvailability has been passed, got [%s]", v)
	}
	return nil
}

// BisacReturnableIndicator List 66: BISAC returnable indicator
type BisacReturnableIndicator string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *BisacReturnableIndicator) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	//
	case "N":
		*c = `No, not returnable`

	//
	case "Y":
		*c = `Yes, returnable, full copies only`

	//
	case "S":
		*c = `Yes, returnable, stripped cover`

	// Contact publisher for requirements and/or authorization
	case "C":
		*c = `Conditional`
	default:
		return fmt.Errorf("undefined code for BisacReturnableIndicator has been passed, got [%s]", v)
	}
	return nil
}

// MarketDateRole List 67: Market date role
type MarketDateRole string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *MarketDateRole) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// The nominal date of publication in this market. If there is a strict embargo on retail sales before the expected date, it should be specified separately as an embargo date
	case "01":
		*c = `Publication date`

	// If there is an embargo on retail sales in this market before a certain date, the date from which the embargo is lifted and retail sales are permitted
	case "02":
		*c = `Embargo date`
	default:
		return fmt.Errorf("undefined code for MarketDateRole has been passed, got [%s]", v)
	}
	return nil
}

// MarketPublishingStatus List 68: Market publishing status
type MarketPublishingStatus string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *MarketPublishingStatus) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// Status is not specified (as distinct from unknown): the default if the <MarketPublishingStatus> element is not sent
	case "00":
		*c = `Unspecified`

	// The product was announced for publication in this market, and subsequently abandoned. A market publication date must not be sent
	case "01":
		*c = `Cancelled`

	// Not yet published in this market, should be accompanied by expected local publication date
	case "02":
		*c = `Forthcoming`

	// The product was announced for publication in this market, and subsequently postponed with no expected local publication date. A market publication date must not be sent
	case "03":
		*c = `Postponed indefinitely`

	// The product was published in this market, and is still active in the sense that the publisher will accept orders for it, though it may or may not be immediately available, for which see <SupplyDetail>
	case "04":
		*c = `Active`

	// Responsibility for the product in this market has been transferred elsewhere (with details of acquiring publisher representative in this market if possible in PR.25 (ONIX 2.1) OR P.25 (ONIX 3.0))
	case "05":
		*c = `No longer our product`

	// The product was active in this market, but is now inactive in the sense that (a) the publisher representative (local publisher or sales agent) cannot fulfill orders for it, though stock may still be available elsewhere in the supply chain, and (b) there are no current plans to bring it back into stock in this market. Code 06 does not specifically imply that returns are or are not still accepted
	case "06":
		*c = `Out of stock indefinitely`

	// The product was active in this market, but is now permanently inactive in this market in the sense that (a) the publisher representative (local publisher or sales agent) will not accept orders for it, though stock may still be available elsewhere in the supply chain, and (b) the product will not be made available again in this market under the same ISBN. Code 07 normally implies that the publisher will not accept returns beyond a specified date
	case "07":
		*c = `Out of print`

	// The product was active in this market, but is now permanently or indefinitely inactive in the sense that the publisher representative (local publisher or sales agent) will not accept orders for it, though stock may still be available elsewhere in the supply chain. Code 08 covers both of codes 06 and 07, and may be used where the distinction between those values is either unnecessary or meaningless
	case "08":
		*c = `Inactive`

	// The sender of the ONIX record does not know the current publishing status in this market
	case "09":
		*c = `Unknown`

	// The product is no longer available in this market from the publisher representative (local publisher or sales agent), under the current ISBN, at the current price. It may be available to be traded through another channel, usually at a reduced price
	case "10":
		*c = `Remaindered`

	// Withdrawn from sale in this market, typically for legal reasons or to avoid giving offence
	case "11":
		*c = `Withdrawn from sale`

	// Either no rights are held for the product in this market, or for other reasons the publisher has decided not to make it available in this market
	case "12":
		*c = `Not available in this market`

	// The product is published in this market and active but, as a publishing decision, it is not sold separately – only in an assembly or as part of a package. Depending on product composition and pricing, it may be saleable separately at retail
	case "13":
		*c = `Active, but not sold separately`

	// The product is published in this market and active, but is not available to all customer types, typically because the market is split between exclusive sales agents for different market segments. In ONIX 2.1, should be accompanied by a free-text statement in <MarketRestrictionDetail> describing the nature of the restriction. In ONIX 3.0, the <SalesRestriction> composite in Group P.24 should be used
	case "14":
		*c = `Active, with market restrictions`

	// Recalled in this market for reasons of consumer safety
	case "15":
		*c = `Recalled`

	// Temporarily withdrawn from sale in this market, typically for quality or technical reasons. In ONIX 3.0, must be accompanied by expected availability date coded ‘22’ within the <MarketPublishingDate> composite, except in exceptional circumstances where no date is known
	case "16":
		*c = `Temporarily withdrawn from sale`
	default:
		return fmt.Errorf("undefined code for MarketPublishingStatus has been passed, got [%s]", v)
	}
	return nil
}

// AgentRole List 69: Agent role
type AgentRole string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *AgentRole) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// Publisher’s exclusive sales agent in a specified territory
	case "05":
		*c = `Exclusive sales agent`

	// Publisher’s non-exclusive sales agent in a specified territory
	case "06":
		*c = `Non-exclusive sales agent`

	// Publisher for a specified territory
	case "07":
		*c = `Local publisher`

	// Publisher’s sales agent in a specific territory. Use only where exclusive / non-exclusive status is not known. Prefer 05 or 06 as appropriate, where possible
	case "08":
		*c = `Sales agent`
	default:
		return fmt.Errorf("undefined code for AgentRole has been passed, got [%s]", v)
	}
	return nil
}

// ProductFormCode List 7: Product form code
type ProductFormCode string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *ProductFormCode) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	//
	case "00":
		*c = `Undefined`

	// Audio recording – detail unspecified
	case "AA":
		*c = `Audio`

	// Audio cassette (analogue)
	case "AB":
		*c = `Audio cassette`

	// Audio compact disc, in any recording format: use for ‘red book’ (conventional audio CD) and SACD, and use coding in Product Form Detail to specify the format, if required
	case "AC":
		*c = `CD-Audio`

	// Digital audio tape cassette
	case "AD":
		*c = `DAT`

	// Audio disc (excluding CD-Audio)
	case "AE":
		*c = `Audio disc`

	// Audio tape (analogue open reel tape)
	case "AF":
		*c = `Audio tape`

	// Sony MiniDisc format
	case "AG":
		*c = `MiniDisc`

	// Audio compact disc with part CD-ROM content, also termed CD-Plus or Enhanced-CD: use for ‘blue book’ and ‘yellow/red book’ two-session discs
	case "AH":
		*c = `CD-Extra`

	//
	case "AI":
		*c = `DVD Audio`

	// Audio recording downloadable online
	case "AJ":
		*c = `Downloadable audio file`

	// For example, Playaway audiobook and player: use coding in Product Form Detail to specify the recording format, if required
	case "AK":
		*c = `Pre-recorded digital audio player`

	// For example, Audiofy audiobook chip
	case "AL":
		*c = `Pre-recorded SD card`

	// Other audio format not specified by AB to AL
	case "AZ":
		*c = `Other audio format`

	// Book – detail unspecified
	case "BA":
		*c = `Book`

	// Hardback or cased book
	case "BB":
		*c = `Hardback`

	// Paperback or other softback book
	case "BC":
		*c = `Paperback / softback`

	// Loose-leaf book
	case "BD":
		*c = `Loose-leaf`

	// Spiral, comb or coil bound book
	case "BE":
		*c = `Spiral bound`

	// Pamphlet or brochure, stapled; German ‘geheftet’. Includes low-extent wire-stitched books bound without a distinct spine (eg many comic books)
	case "BF":
		*c = `Pamphlet`

	//
	case "BG":
		*c = `Leather / fine binding`

	// Child’s book with all pages printed on board
	case "BH":
		*c = `Board book`

	// Child’s book with all pages printed on textile
	case "BI":
		*c = `Rag book`

	// Child’s book printed on waterproof material
	case "BJ":
		*c = `Bath book`

	// A book whose novelty consists wholly or partly in a format which cannot be described by any other available code – a ‘conventional’ format code is always to be preferred; one or more Product Form Detail codes, eg from the B2nn group, should be used whenever possible to provide additional description
	case "BK":
		*c = `Novelty book`

	// Slide bound book
	case "BL":
		*c = `Slide bound`

	// Extra-large format for teaching etc; this format and terminology may be specifically UK; required as a top-level differentiator
	case "BM":
		*c = `Big book`

	// A part-work issued with its own ISBN and intended to be collected and bound into a complete book
	case "BN":
		*c = `Part-work (fascículo)`

	// Concertina-folded book or chart, designed to fold to pocket or regular page size: use for German ‘Leporello’
	case "BO":
		*c = `Fold-out book or chart`

	// A children’s book whose cover and pages are made of foam
	case "BP":
		*c = `Foam book`

	// Other book format or binding not specified by BB to BP
	case "BZ":
		*c = `Other book format`

	// Sheet map – detail unspecified
	case "CA":
		*c = `Sheet map`

	//
	case "CB":
		*c = `Sheet map, folded`

	//
	case "CC":
		*c = `Sheet map, flat`

	// See Code List 80 for ‘rolled in tube’
	case "CD":
		*c = `Sheet map, rolled`

	// Globe or planisphere
	case "CE":
		*c = `Globe`

	// Other cartographic format not specified by CB to CE
	case "CZ":
		*c = `Other cartographic`

	// Digital or multimedia (detail unspecified)
	case "DA":
		*c = `Digital`

	//
	case "DB":
		*c = `CD-ROM`

	// CD interactive, use for ‘green book’ discs
	case "DC":
		*c = `CD-I`

	// DEPRECATED – use VI for DVD video, AI for DVD audio, DI for DVD-ROM
	case "DD":
		*c = `DVD`

	//
	case "DE":
		*c = `Game cartridge`

	// AKA ‘floppy disc’
	case "DF":
		*c = `Diskette`

	// Electronic book text in proprietary or open standard format
	case "DG":
		*c = `Electronic book text`

	// An electronic database or other resource or service accessible through online networks
	case "DH":
		*c = `Online resource`

	//
	case "DI":
		*c = `DVD-ROM`

	//
	case "DJ":
		*c = `Secure Digital (SD) Memory Card`

	//
	case "DK":
		*c = `Compact Flash Memory Card`

	//
	case "DL":
		*c = `Memory Stick Memory Card`

	//
	case "DM":
		*c = `USB Flash Drive`

	// Double-sided disc, one side CD-Audio/CD-ROM, other side DVD-Audio/DVD-Video/DVD-ROM (at least one side must be -ROM)
	case "DN":
		*c = `Double-sided CD/DVD`

	// Digital product license delivered through the retail supply chain as a physical ‘key’, typically a card or booklet containing a code enabling the purchaser to download or activate the associated product
	case "DO":
		*c = `Digital product license key`

	// Other digital or multimedia not specified by DB to DN
	case "DZ":
		*c = `Other digital`

	// Film or transparency – detail unspecified
	case "FA":
		*c = `Film or transparency`

	// Continuous film or filmstrip: DEPRECATED – use FE or FF
	case "FB":
		*c = `Film`

	// Photographic transparencies mounted for projection
	case "FC":
		*c = `Slides`

	// Transparencies for overhead projector
	case "FD":
		*c = `OHP transparencies`

	//
	case "FE":
		*c = `Filmstrip`

	// Continuous movie film as opposed to filmstrip
	case "FF":
		*c = `Film`

	// Other film or transparency format not specified by FB to FF
	case "FZ":
		*c = `Other film or transparency format`

	// Microform – detail unspecified
	case "MA":
		*c = `Microform`

	//
	case "MB":
		*c = `Microfiche`

	// Roll microfilm
	case "MC":
		*c = `Microfilm`

	// Other microform not specified by MB or MC
	case "MZ":
		*c = `Other microform`

	// Miscellaneous printed material – detail unspecified
	case "PA":
		*c = `Miscellaneous print`

	// May use product form detail codes P201 to P204 to specify binding
	case "PB":
		*c = `Address book`

	//
	case "PC":
		*c = `Calendar`

	// Cards, flash cards (eg for teaching reading)
	case "PD":
		*c = `Cards`

	// Copymasters, photocopiable sheets
	case "PE":
		*c = `Copymasters`

	// May use product form detail codes P201 to P204 to specify binding
	case "PF":
		*c = `Diary`

	// Narrow strip-shaped printed sheet used mostly for education or children’s products (eg depicting alphabet, number line, procession of illustrated characters etc). Usually intended for horizontal display
	case "PG":
		*c = `Frieze`

	// Parts for post-purchase assembly
	case "PH":
		*c = `Kit`

	//
	case "PI":
		*c = `Sheet music`

	//
	case "PJ":
		*c = `Postcard book or pack`

	// Poster for retail sale – see also XF
	case "PK":
		*c = `Poster`

	// Record book (eg ‘birthday book’, ‘baby book’): may use product form detail codes P201 to P204 to specify binding
	case "PL":
		*c = `Record book`

	// Wallet or folder (containing loose sheets etc): it is preferable to code the contents and treat ‘wallet’ as packaging (List 80), but if this is not possible the product as a whole may be coded as a ‘wallet’
	case "PM":
		*c = `Wallet or folder`

	//
	case "PN":
		*c = `Pictures or photographs`

	//
	case "PO":
		*c = `Wallchart`

	//
	case "PP":
		*c = `Stickers`

	// A book-sized (as opposed to poster-sized) sheet, usually in color or high quality print
	case "PQ":
		*c = `Plate (lámina)`

	// A book with all pages blank for the buyer’s own use: may use product form detail codes P201 to P204 to specify binding
	case "PR":
		*c = `Notebook / blank book`

	// May use product form detail codes P201 to P204 to specify binding
	case "PS":
		*c = `Organizer`

	//
	case "PT":
		*c = `Bookmark`

	// Other printed item not specified by PB to PT
	case "PZ":
		*c = `Other printed item`

	// Video – detail unspecified
	case "VA":
		*c = `Video`

	// DEPRECATED – use new VJ
	case "VB":
		*c = `Video, VHS, PAL`

	// DEPRECATED – use new VJ
	case "VC":
		*c = `Video, VHS, NTSC`

	// DEPRECATED – use new VK
	case "VD":
		*c = `Video, Betamax, PAL`

	// DEPRECATED – use new VK
	case "VE":
		*c = `Video, Betamax, NTSC`

	// eg Laserdisc
	case "VF":
		*c = `Videodisc`

	// DEPRECATED – use new VJ
	case "VG":
		*c = `Video, VHS, SECAM`

	// DEPRECATED – use new VK
	case "VH":
		*c = `Video, Betamax, SECAM`

	// DVD video: specify TV standard in List 78
	case "VI":
		*c = `DVD video`

	// VHS videotape: specify TV standard in List 78
	case "VJ":
		*c = `VHS video`

	// Betamax videotape: specify TV standard in List 78
	case "VK":
		*c = `Betamax video`

	// VideoCD
	case "VL":
		*c = `VCD`

	// Super VideoCD
	case "VM":
		*c = `SVCD`

	// High definition DVD disc, Toshiba HD DVD format
	case "VN":
		*c = `HD DVD`

	// High definition DVD disc, Sony Blu-ray format
	case "VO":
		*c = `Blu-ray`

	// Sony Universal Media disc
	case "VP":
		*c = `UMD Video`

	// Other video format not specified by VB to VP
	case "VZ":
		*c = `Other video format`

	// A product consisting of two or more items in different media or different product forms, eg book and CD-ROM, book and toy, hardback book and e-book, etc
	case "WW":
		*c = `Mixed media product`

	// A product containing multiple copies of one or more items packaged together for retail sale, consisting of either (a) several copies of a single item (eg 6 copies of a graded reader), or (b) several copies of each of several items (eg 3 copies each of 3 different graded readers), or (c) several copies of one or more single items plus a single copy of one or more related items (eg 30 copies of a pupil’s textbook plus 1 of teacher’s text). NOT TO BE CONFUSED WITH: multi-volume sets, or sets containing a single copy of a number of different items (boxed, slip-cased or otherwise); items with several components of different physical forms (see WW); or packs intended for trade distribution only, where the contents are retailed separately (see XC, XE, XL)
	case "WX":
		*c = `Multiple copy pack`

	// Trade-only material (unspecified)
	case "XA":
		*c = `Trade-only material`

	//
	case "XB":
		*c = `Dumpbin – empty`

	// Dumpbin with contents
	case "XC":
		*c = `Dumpbin – filled`

	//
	case "XD":
		*c = `Counterpack – empty`

	// Counterpack with contents
	case "XE":
		*c = `Counterpack – filled`

	// Promotional poster for display, not for sale – see also PK
	case "XF":
		*c = `Poster, promotional`

	//
	case "XG":
		*c = `Shelf strip`

	// Promotional piece for shop window display
	case "XH":
		*c = `Window piece`

	//
	case "XI":
		*c = `Streamer`

	//
	case "XJ":
		*c = `Spinner`

	// Large scale facsimile of book for promotional display
	case "XK":
		*c = `Large book display`

	// A quantity pack with its own product code, for trade supply only: the retail items it contains are intended for sale individually – see also WX. For products or product bundles supplied shrink-wrapped for retail sale, use the Product Form code of the contents plus code 21 from List 80
	case "XL":
		*c = `Shrink-wrapped pack`

	// A quantity pack with its own product code, for trade supply only: the retail items it contains are intended for sale individually – see also WX. For products or product bundles supplied boxed for retail sale, use the Product Form code of the contents plus code 09 from List 80
	case "XM":
		*c = `Boxed pack`

	// Other point of sale material not specified by XB to XL
	case "XZ":
		*c = `Other point of sale`

	// General merchandise – unspecified
	case "ZA":
		*c = `General merchandise`

	//
	case "ZB":
		*c = `Doll`

	// Soft or plush toy
	case "ZC":
		*c = `Soft toy`

	//
	case "ZD":
		*c = `Toy`

	// Board game, or other game (except computer game: see DE)
	case "ZE":
		*c = `Game`

	//
	case "ZF":
		*c = `T-shirt`

	// Dedicated e-book reading device, typically with mono screen
	case "ZG":
		*c = `E-book reader`

	// General purpose tablet computer, typically with color screen
	case "ZH":
		*c = `Tablet computer`

	// Dedicated audiobook player device, typically including book-related features like bookmarking
	case "ZI":
		*c = `Audiobook player`

	//
	case "ZJ":
		*c = `Jigsaw`

	// Other apparel items not specified by ZB to ZJ, including promotional or branded scarves, caps, aprons etc
	case "ZY":
		*c = `Other apparel`

	// Other merchandise not specified by ZB to ZY
	case "ZZ":
		*c = `Other merchandise`
	default:
		return fmt.Errorf("undefined code for ProductFormCode has been passed, got [%s]", v)
	}
	return nil
}

// StockQuantityCodeType List 70: Stock quantity code type
type StockQuantityCodeType string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *StockQuantityCodeType) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// As specified in <StockQuantityCodeTypeName>
	case "01":
		*c = `Proprietary`

	// Code scheme defined by the Australian Publishers Association
	case "02":
		*c = `APA stock quantity code`
	default:
		return fmt.Errorf("undefined code for StockQuantityCodeType has been passed, got [%s]", v)
	}
	return nil
}

// SalesRestrictionTypeCode List 71: Sales restriction type code
type SalesRestrictionTypeCode string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *SalesRestrictionTypeCode) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// Restriction must be described in <SalesRestrictionDetail> (ONIX 2.1) or <SalesRestrictionNote> (ONIX 3.0)
	case "00":
		*c = `Unspecified – see text`

	// For sale only through designated retailer. Retailer must be identified or named in an instance of the <SalesOutlet> composite. Use only when it is not possible to assign the more explicit code 04 or 05
	case "01":
		*c = `Retailer exclusive / own brand`

	// For editions sold only though office supplies wholesalers. Retailer(s) and/or distributor(s) may be identified or named in an instance of the <SalesOutlet> composite
	case "02":
		*c = `Office supplies edition`

	// For an ISBN that is assigned for a publisher’s internal purposes
	case "03":
		*c = `Internal publisher use only: do not list`

	// For sale only through designated retailer, though not under retailer’s own brand/imprint. Retailer must be identified or named in an instance of the <SalesOutlet> composite
	case "04":
		*c = `Retailer exclusive`

	// For sale only through designated retailer under retailer’s own brand/imprint. Retailer must be identified or named in an instance of the <SalesOutlet> composite
	case "05":
		*c = `Retailer own brand`

	// For sale to libraries only; not for sale through retail trade
	case "06":
		*c = `Library edition`

	// For sale directly to schools only; not for sale through retail trade
	case "07":
		*c = `Schools only edition`

	// Indexed for the German market – in Deutschland indiziert
	case "08":
		*c = `Indiziert`

	// Expected to apply in particular to digital products for consumer sale where the publisher does not permit the product to be supplied to libraries who provide an ebook loan service
	case "09":
		*c = `Not for sale to libraries`

	// For editions sold only through newsstands/newsagents
	case "10":
		*c = `News outlet edition`

	// Not for sale through designated retailer. Retailer must be identified or named in an instance of the <SalesOutlet> composite
	case "11":
		*c = `Retailer exception`

	// Not for sale to organisations or services offering consumers subscription access to a library of books
	case "12":
		*c = `Not for sale to subscription services`

	// Restricted to organisations or services offering consumers subscription access to a library of books
	case "13":
		*c = `Subscription services only`

	// Exclusive to bricks-and-mortar retail outlets
	case "14":
		*c = `Not for retail online`

	// Exclusive to online retail outlets
	case "15":
		*c = `Online retail only`
	default:
		return fmt.Errorf("undefined code for SalesRestrictionTypeCode has been passed, got [%s]", v)
	}
	return nil
}

// ThesisTypeCode List 72: Thesis type code
type ThesisTypeCode string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *ThesisTypeCode) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// Professorial dissertation (thesis for postdoctoral lecturing qualification)
	case "01":
		*c = `Habilitationsschrift`

	// Doctoral thesis
	case "02":
		*c = `Dissertationsschrift`

	// State examination thesis
	case "03":
		*c = `Staatsexamensarbeit`

	// Magisters degree thesis
	case "04":
		*c = `Magisterarbeit`

	// Diploma degree thesis
	case "05":
		*c = `Diplomarbeit`

	// Bachelors degree thesis
	case "06":
		*c = `Bachelorarbeit`

	// Masters degree thesis
	case "07":
		*c = `Masterarbeit`
	default:
		return fmt.Errorf("undefined code for ThesisTypeCode has been passed, got [%s]", v)
	}
	return nil
}

// WebsiteRole List 73: Website role
type WebsiteRole string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *WebsiteRole) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	//
	case "00":
		*c = `Unspecified, see website description`

	// See also codes 17 and 18
	case "01":
		*c = `Publisher’s corporate website`

	// A publisher’s informative and/or promotional webpage relating to a specified work (book, journal, online resource or other publication type)
	case "02":
		*c = `Publisher’s website for a specified work`

	// A webpage giving access to an online content hosting service as a whole
	case "03":
		*c = `Online hosting service home page`

	// A webpage giving general information about a serial, in print or electronic format or both
	case "04":
		*c = `Journal home page`

	// A webpage giving direct access to the content that is available online for a specified resource version. Generally used for content available online under subscription terms
	case "05":
		*c = `Online resource ‘available content’ page`

	// A webpage maintained by an author or other contributor about her/his publications and personal background
	case "06":
		*c = `Contributor’s own website`

	// A publisher’s webpage devoted to a specific author or other contributor
	case "07":
		*c = `Publisher’s website relating to specified contributor`

	// A webpage devoted to a specific author or other contributor, and maintained by a publisher other than the publisher of the item described in the ONIX record
	case "08":
		*c = `Other publisher’s website relating to specified contributor`

	// A webpage devoted to a specific author or other contributor, and maintained by a third party (eg a fan site)
	case "09":
		*c = `Third-party website relating to specified contributor`

	// A webpage maintained by an author or other contributor and specific to an individual work
	case "10":
		*c = `Contributor’s own website for specified work`

	// A webpage devoted to an individual work, and maintained by a publisher other than the publisher of the item described in the ONIX record
	case "11":
		*c = `Other publisher’s website relating to specified work`

	// A webpage devoted to an individual work, and maintained by a third party (eg a fan site)
	case "12":
		*c = `Third-party website relating to specified work`

	// A webpage maintained by an author or other contributor and specific to a group or series of works
	case "13":
		*c = `Contributor’s own website for group or series of works`

	// A publisher’s webpage devoted to a group or series of works
	case "14":
		*c = `Publisher’s website relating to group or series of works`

	// A webpage devoted to a group or series of works, and maintained by a publisher other than the publisher of the item described in the ONIX record
	case "15":
		*c = `Other publisher’s website relating to group or series of works`

	// A webpage devoted to a group or series of works, and maintained by a third party (eg a fan site)
	case "16":
		*c = `Third-party website relating to group or series of works (eg a fan site)`

	// Use instead of code 01 to specify a publisher’s website for trade users
	case "17":
		*c = `Publisher’s B2B website`

	// Use instead of code 01 to specify a publisher’s website for end customers (consumers)
	case "18":
		*c = `Publisher’s B2C website`

	// For example, a Blogger or Tumblr URL, a Wordpress website or other blog URL
	case "23":
		*c = `Author blog`

	//
	case "24":
		*c = `Web page for author presentation / commentary`

	//
	case "25":
		*c = `Web page for author interview`

	//
	case "26":
		*c = `Web page for author reading`

	//
	case "27":
		*c = `Web page for cover material`

	//
	case "28":
		*c = `Web page for sample content`

	// Use this value in the <Website> composite in <SupplyDetail> when sending a link to a webpage at which a digital product is available for download and/or online access
	case "29":
		*c = `Web page for full content`

	//
	case "30":
		*c = `Web page for other commentary / discussion`

	// URL needed by the German National Library for direct access, harvesting and storage of an electronic resource
	case "31":
		*c = `Transfer-URL`

	// Link needed by German Books in Print (VLB) for DOI registration and ONIX DOI conversion
	case "32":
		*c = `DOI Website Link`

	// A corporate website operated by a distributor or other supplier (not the publisher)
	case "33":
		*c = `Supplier’s corporate website`

	// A website operated by a distributor or other supplier (not the publisher) and aimed at trade customers
	case "34":
		*c = `Supplier’s B2B website`

	// A website operated by a distributor or other supplier (not the publisher) and aimed at consumers
	case "35":
		*c = `Supplier’s B2C website`

	// A distributor or supplier’s webpage describing a specified work
	case "36":
		*c = `Supplier’s website for a specified work`

	// A distributor or supplier’s webpage describing a specified work, and aimed at trade customers
	case "37":
		*c = `Supplier’s B2B website for a specified work`

	// A distributor or supplier’s webpage describing a specified work, and aimed at consumers
	case "38":
		*c = `Supplier’s B2C website for a specified work`

	// A distributor or supplier’s webpage describing a group or series of works
	case "39":
		*c = `Supplier’s website for a group or series of works`

	// For example an ONIX or MARC record for the product, available online
	case "40":
		*c = `URL of full metadata description`

	// For example, a Facebook, Google+ or Twitter URL for the product or work
	case "41":
		*c = `Social networking URL for specific work or product`

	// For example, a Facebook, Google+ or Twitter page
	case "42":
		*c = `Author’s social networking URL`

	// For example, a Facebook, Google+ or Twitter page
	case "43":
		*c = `Publisher’s social networking URL`

	// For example, a Facebook, Google+ or Twitter page. Use only in the context of a specific content item (eg within <ContentItem>)
	case "44":
		*c = `Social networking URL for specific article, chapter or content item`

	// For example, a service offering click-through licensing of extracts
	case "45":
		*c = `Publisher’s or third party website for permissions requests`
	default:
		return fmt.Errorf("undefined code for WebsiteRole has been passed, got [%s]", v)
	}
	return nil
}

// LanguageCodeIso6392B List 74: Language code – ISO 639-2/B
type LanguageCodeIso6392B string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *LanguageCodeIso6392B) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	//
	case "aar":
		*c = `Afar`

	//
	case "abk":
		*c = `Abkhaz`

	//
	case "ace":
		*c = `Achinese`

	//
	case "ach":
		*c = `Acoli`

	//
	case "ada":
		*c = `Adangme`

	//
	case "ady":
		*c = `Adygei`

	// Collective name
	case "afa":
		*c = `Afro-Asiatic languages`

	// Artificial language
	case "afh":
		*c = `Afrihili`

	//
	case "afr":
		*c = `Afrikaans`

	//
	case "ain":
		*c = `Ainu`

	// Macrolanguage
	case "aka":
		*c = `Akan`

	//
	case "akk":
		*c = `Akkadian`

	// Macrolanguage
	case "alb":
		*c = `Albanian`

	//
	case "ale":
		*c = `Aleut`

	// Collective name
	case "alg":
		*c = `Algonquian languages`

	//
	case "alt":
		*c = `Southern Altai`

	//
	case "amh":
		*c = `Amharic`

	//
	case "ang":
		*c = `English, Old (ca. 450-1100)`

	//
	case "anp":
		*c = `Angika`

	// Collective name
	case "apa":
		*c = `Apache languages`

	// Macrolanguage
	case "ara":
		*c = `Arabic`

	//
	case "arc":
		*c = `Official Aramaic; Imperial Aramaic (700-300 BCE)`

	//
	case "arg":
		*c = `Aragonese`

	//
	case "arm":
		*c = `Armenian`

	//
	case "arn":
		*c = `Mapudungun; Mapuche`

	//
	case "arp":
		*c = `Arapaho`

	// Collective name
	case "art":
		*c = `Artificial languages`

	//
	case "arw":
		*c = `Arawak`

	//
	case "asm":
		*c = `Assamese`

	//
	case "ast":
		*c = `Asturian; Bable; Leonese; Asturleonese`

	// Collective name
	case "ath":
		*c = `Athapascan languages`

	// Collective name
	case "aus":
		*c = `Australian languages`

	//
	case "ava":
		*c = `Avaric`

	//
	case "ave":
		*c = `Avestan`

	//
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

	//
	case "bak":
		*c = `Bashkir`

	// Macrolanguage
	case "bal":
		*c = `Baluchi`

	//
	case "bam":
		*c = `Bambara`

	//
	case "ban":
		*c = `Balinese`

	//
	case "baq":
		*c = `Basque`

	//
	case "bas":
		*c = `Basa`

	// Collective name
	case "bat":
		*c = `Baltic languages`

	//
	case "bej":
		*c = `Beja; Bedawiyet`

	//
	case "bel":
		*c = `Belarusian`

	//
	case "bem":
		*c = `Bemba`

	//
	case "ben":
		*c = `Bengali`

	// Collective name
	case "ber":
		*c = `Berber languages`

	//
	case "bho":
		*c = `Bhojpuri`

	// Collective name
	case "bih":
		*c = `Bihari languages`

	// Macrolanguage
	case "bik":
		*c = `Bikol`

	//
	case "bin":
		*c = `Bini; Edo`

	//
	case "bis":
		*c = `Bislama`

	//
	case "bla":
		*c = `Siksika`

	// Collective name
	case "bnt":
		*c = `Bantu languages`

	//
	case "bos":
		*c = `Bosnian`

	//
	case "bra":
		*c = `Braj`

	//
	case "bre":
		*c = `Breton`

	// Collective name
	case "btk":
		*c = `Batak languages`

	// Macrolanguage
	case "bua":
		*c = `Buriat`

	//
	case "bug":
		*c = `Buginese`

	//
	case "bul":
		*c = `Bulgarian`

	//
	case "bur":
		*c = `Burmese`

	//
	case "byn":
		*c = `Blin; Bilin`

	//
	case "cad":
		*c = `Caddo`

	// Collective name
	case "cai":
		*c = `Central American Indian languages`

	//
	case "car":
		*c = `Galibi Carib`

	//
	case "cat":
		*c = `Catalan`

	// Collective name
	case "cau":
		*c = `Caucasian languages`

	//
	case "ceb":
		*c = `Cebuano`

	// Collective name
	case "cel":
		*c = `Celtic languages`

	//
	case "cha":
		*c = `Chamorro`

	//
	case "chb":
		*c = `Chibcha`

	//
	case "che":
		*c = `Chechen`

	//
	case "chg":
		*c = `Chagatai`

	// Macrolanguage
	case "chi":
		*c = `Chinese`

	//
	case "chk":
		*c = `Chuukese (Truk)`

	// Macrolanguage
	case "chm":
		*c = `Mari`

	//
	case "chn":
		*c = `Chinook jargon`

	//
	case "cho":
		*c = `Choctaw`

	//
	case "chp":
		*c = `Chipewyan; Dene Suline`

	//
	case "chr":
		*c = `Cherokee`

	//
	case "chu":
		*c = `Church Slavic; Old Slavonic; Church Slavonic; Old Bulgarian; Old Church Slavonic`

	//
	case "chv":
		*c = `Chuvash`

	//
	case "chy":
		*c = `Cheyenne`

	// Collective name
	case "cmc":
		*c = `Chamic languages`

	// ONIX local code, equivalent to cmn in ISO 639-3
	case "cmn":
		*c = `Mandarin`

	//
	case "cop":
		*c = `Coptic`

	//
	case "cor":
		*c = `Cornish`

	//
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

	//
	case "crh":
		*c = `Crimean Turkish; Crimean Tatar`

	// Collective name
	case "crp":
		*c = `Creoles and pidgins`

	//
	case "csb":
		*c = `Kashubian`

	// Collective name
	case "cus":
		*c = `Cushitic languages`

	//
	case "cze":
		*c = `Czech`

	//
	case "dak":
		*c = `Dakota`

	//
	case "dan":
		*c = `Danish`

	//
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

	//
	case "dgr":
		*c = `Dogrib`

	// Macrolanguage
	case "din":
		*c = `Dinka`

	//
	case "div":
		*c = `Divehi; Dhivehi; Maldivian`

	// Macrolanguage
	case "doi":
		*c = `Dogri`

	// Collective name
	case "dra":
		*c = `Dravidian languages`

	//
	case "dsb":
		*c = `Lower Sorbian`

	//
	case "dua":
		*c = `Duala`

	//
	case "dum":
		*c = `Dutch, Middle (ca. 1050-1350)`

	//
	case "dut":
		*c = `Dutch; Flemish`

	//
	case "dyu":
		*c = `Dyula`

	//
	case "dzo":
		*c = `Dzongkha`

	//
	case "efi":
		*c = `Efik`

	//
	case "egy":
		*c = `Egyptian (Ancient)`

	//
	case "eka":
		*c = `Ekajuk`

	//
	case "elx":
		*c = `Elamite`

	//
	case "eng":
		*c = `English`

	//
	case "enm":
		*c = `English, Middle (1100-1500)`

	// Artificial language
	case "epo":
		*c = `Esperanto`

	// Macrolanguage
	case "est":
		*c = `Estonian`

	//
	case "ewe":
		*c = `Ewe`

	//
	case "ewo":
		*c = `Ewondo`

	//
	case "fan":
		*c = `Fang`

	//
	case "fao":
		*c = `Faroese`

	//
	case "fat":
		*c = `Fanti`

	//
	case "fij":
		*c = `Fijian`

	//
	case "fil":
		*c = `Filipino; Pilipino`

	//
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

	//
	case "fon":
		*c = `Fon`

	//
	case "fre":
		*c = `French`

	//
	case "frm":
		*c = `French, Middle (ca. 1400-1600)`

	//
	case "fro":
		*c = `French, Old (ca. 842-1400)`

	//
	case "frr":
		*c = `Northern Frisian`

	//
	case "frs":
		*c = `Eastern Frisian`

	//
	case "fry":
		*c = `Western Frisian`

	//
	case "ful":
		*c = `Fulah`

	//
	case "fur":
		*c = `Friulian`

	//
	case "gaa":
		*c = `Gã`

	//
	case "gay":
		*c = `Gayo`

	// Macrolanguage
	case "gba":
		*c = `Gbaya`

	// Collective name
	case "gem":
		*c = `Germanic languages`

	//
	case "geo":
		*c = `Georgian`

	//
	case "ger":
		*c = `German`

	//
	case "gez":
		*c = `Ethiopic (Ge’ez)`

	//
	case "gil":
		*c = `Gilbertese`

	//
	case "gla":
		*c = `Scottish Gaelic`

	//
	case "gle":
		*c = `Irish`

	//
	case "glg":
		*c = `Galician`

	//
	case "glv":
		*c = `Manx`

	//
	case "gmh":
		*c = `German, Middle High (ca. 1050-1500)`

	//
	case "goh":
		*c = `German, Old High (ca. 750-1050)`

	// Macrolanguage
	case "gon":
		*c = `Gondi`

	//
	case "gor":
		*c = `Gorontalo`

	//
	case "got":
		*c = `Gothic`

	// Macrolanguage
	case "grb":
		*c = `Grebo`

	//
	case "grc":
		*c = `Greek, Ancient (to 1453)`

	//
	case "gre":
		*c = `Greek, Modern (1453-)`

	// Macrolanguage
	case "grn":
		*c = `Guarani`

	//
	case "gsw":
		*c = `Swiss German; Alemannic`

	//
	case "guj":
		*c = `Gujarati`

	//
	case "gwi":
		*c = `Gwich’in`

	// Macrolanguage
	case "hai":
		*c = `Haida`

	//
	case "hat":
		*c = `Haitian French Creole`

	//
	case "hau":
		*c = `Hausa`

	//
	case "haw":
		*c = `Hawaiian`

	//
	case "heb":
		*c = `Hebrew`

	//
	case "her":
		*c = `Herero`

	//
	case "hil":
		*c = `Hiligaynon`

	// Collective name
	case "him":
		*c = `Himachali languages; Western Pahari languages`

	//
	case "hin":
		*c = `Hindi`

	//
	case "hit":
		*c = `Hittite`

	// Macrolanguage
	case "hmn":
		*c = `Hmong; Mong`

	//
	case "hmo":
		*c = `Hiri Motu`

	//
	case "hrv":
		*c = `Croatian`

	//
	case "hsb":
		*c = `Upper Sorbian`

	//
	case "hun":
		*c = `Hungarian`

	//
	case "hup":
		*c = `Hupa`

	//
	case "iba":
		*c = `Iban`

	//
	case "ibo":
		*c = `Igbo`

	//
	case "ice":
		*c = `Icelandic`

	// Artificial language
	case "ido":
		*c = `Ido`

	//
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

	//
	case "ilo":
		*c = `Iloko`

	// Artificial language
	case "ina":
		*c = `Interlingua (International Auxiliary Language Association)`

	// Collective name
	case "inc":
		*c = `Indic languages`

	//
	case "ind":
		*c = `Indonesian`

	// Collective name
	case "ine":
		*c = `Indo-European languages`

	//
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

	//
	case "ita":
		*c = `Italian`

	//
	case "jav":
		*c = `Javanese`

	//
	case "jbo":
		*c = `Lojban`

	//
	case "jpn":
		*c = `Japanese`

	//
	case "jpr":
		*c = `Judeo-Persian`

	// Macrolanguage
	case "jrb":
		*c = `Judeo-Arabic`

	//
	case "kaa":
		*c = `Kara-Kalpak`

	//
	case "kab":
		*c = `Kabyle`

	//
	case "kac":
		*c = `Kachin; Jingpho`

	//
	case "kal":
		*c = `Kalâtdlisut; Greenlandic`

	//
	case "kam":
		*c = `Kamba`

	//
	case "kan":
		*c = `Kannada`

	// Collective name
	case "kar":
		*c = `Karen languages`

	//
	case "kas":
		*c = `Kashmiri`

	// Macrolanguage
	case "kau":
		*c = `Kanuri`

	//
	case "kaw":
		*c = `Kawi`

	//
	case "kaz":
		*c = `Kazakh`

	//
	case "kbd":
		*c = `Kabardian (Circassian)`

	// ONIX local code, equivalent to kdr in ISO 639-3
	case "kdr":
		*c = `Karaim`

	//
	case "kha":
		*c = `Khasi`

	// Collective name
	case "khi":
		*c = `Khoisan languages`

	//
	case "khm":
		*c = `Central Khmer`

	//
	case "kho":
		*c = `Khotanese; Sakan`

	//
	case "kik":
		*c = `Kikuyu; Gikuyu`

	//
	case "kin":
		*c = `Kinyarwanda`

	//
	case "kir":
		*c = `Kirghiz; Kyrgyz`

	//
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

	//
	case "kor":
		*c = `Korean`

	//
	case "kos":
		*c = `Kusaiean (Caroline Islands)`

	// Macrolanguage
	case "kpe":
		*c = `Kpelle`

	//
	case "krc":
		*c = `Karachay-Balkar`

	//
	case "krl":
		*c = `Karelian`

	// Collective name
	case "kro":
		*c = `Kru languages`

	//
	case "kru":
		*c = `Kurukh`

	//
	case "kua":
		*c = `Kuanyama`

	//
	case "kum":
		*c = `Kumyk`

	// Macrolanguage
	case "kur":
		*c = `Kurdish`

	//
	case "kut":
		*c = `Kutenai`

	//
	case "lad":
		*c = `Ladino`

	// Macrolanguage
	case "lah":
		*c = `Lahnda`

	//
	case "lam":
		*c = `Lamba`

	//
	case "lao":
		*c = `Lao`

	//
	case "lat":
		*c = `Latin`

	// Macrolanguage
	case "lav":
		*c = `Latvian`

	//
	case "lez":
		*c = `Lezgian`

	//
	case "lim":
		*c = `Limburgish`

	//
	case "lin":
		*c = `Lingala`

	//
	case "lit":
		*c = `Lithuanian`

	//
	case "lol":
		*c = `Mongo-Nkundu`

	//
	case "loz":
		*c = `Lozi`

	//
	case "ltz":
		*c = `Luxembourgish; Letzeburgesch`

	//
	case "lua":
		*c = `Luba-Lulua`

	//
	case "lub":
		*c = `Luba-Katanga`

	//
	case "lug":
		*c = `Ganda`

	//
	case "lui":
		*c = `Luiseño`

	//
	case "lun":
		*c = `Lunda`

	//
	case "luo":
		*c = `Luo (Kenya and Tanzania)`

	//
	case "lus":
		*c = `Lushai`

	//
	case "mac":
		*c = `Macedonian`

	//
	case "mad":
		*c = `Madurese`

	//
	case "mag":
		*c = `Magahi`

	//
	case "mah":
		*c = `Marshallese`

	//
	case "mai":
		*c = `Maithili`

	//
	case "mak":
		*c = `Makasar`

	//
	case "mal":
		*c = `Malayalam`

	// Macrolanguage
	case "man":
		*c = `Mandingo`

	//
	case "mao":
		*c = `Maori`

	// Collective name
	case "map":
		*c = `Austronesian languages`

	//
	case "mar":
		*c = `Marathi`

	//
	case "mas":
		*c = `Masai`

	// Macrolanguage
	case "may":
		*c = `Malay`

	//
	case "mdf":
		*c = `Moksha`

	//
	case "mdr":
		*c = `Mandar`

	//
	case "men":
		*c = `Mende`

	//
	case "mga":
		*c = `Irish, Middle (ca. 1100-1550)`

	//
	case "mic":
		*c = `Mi’kmaq; Micmac`

	//
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

	//
	case "mlt":
		*c = `Maltese`

	//
	case "mnc":
		*c = `Manchu`

	//
	case "mni":
		*c = `Manipuri`

	// Collective name
	case "mno":
		*c = `Manobo languages`

	//
	case "moh":
		*c = `Mohawk`

	// DEPRECATED – use rum
	case "mol":
		*c = `Moldavian; Moldovan`

	// Macrolanguage
	case "mon":
		*c = `Mongolian`

	//
	case "mos":
		*c = `Mooré; Mossi`

	//
	case "mul":
		*c = `Multiple languages`

	// Collective name
	case "mun":
		*c = `Munda languages`

	//
	case "mus":
		*c = `Creek`

	//
	case "mwl":
		*c = `Mirandese`

	// Macrolanguage
	case "mwr":
		*c = `Marwari`

	// Collective name
	case "myn":
		*c = `Mayan languages`

	//
	case "myv":
		*c = `Erzya`

	// Collective name
	case "nah":
		*c = `Nahuatl languages`

	// Collective name
	case "nai":
		*c = `North American Indian languages`

	//
	case "nap":
		*c = `Neapolitan`

	//
	case "nau":
		*c = `Nauruan`

	//
	case "nav":
		*c = `Navajo`

	//
	case "nbl":
		*c = `Ndebele, South`

	//
	case "nde":
		*c = `Ndebele, North`

	//
	case "ndo":
		*c = `Ndonga`

	//
	case "nds":
		*c = `Low German; Low Saxon`

	// Macrolanguage
	case "nep":
		*c = `Nepali`

	//
	case "new":
		*c = `Newari; Nepal Bhasa`

	//
	case "nia":
		*c = `Nias`

	// Collective name
	case "nic":
		*c = `Niger-Kordofanian languages`

	//
	case "niu":
		*c = `Niuean`

	//
	case "nno":
		*c = `Norwegian Nynorsk`

	//
	case "nob":
		*c = `Norwegian Bokmål`

	//
	case "nog":
		*c = `Nogai`

	//
	case "non":
		*c = `Old Norse`

	// Macrolanguage
	case "nor":
		*c = `Norwegian`

	//
	case "nqo":
		*c = `N’Ko`

	//
	case "nso":
		*c = `Pedi; Sepedi; Northern Sotho`

	// Collective name
	case "nub":
		*c = `Nubian languages`

	//
	case "nwc":
		*c = `Classical Newari; Old Newari; Classical Nepal Bhasa`

	//
	case "nya":
		*c = `Chichewa; Chewa; Nyanja`

	//
	case "nym":
		*c = `Nyamwezi`

	//
	case "nyn":
		*c = `Nyankole`

	//
	case "nyo":
		*c = `Nyoro`

	//
	case "nzi":
		*c = `Nzima`

	//
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

	//
	case "osa":
		*c = `Osage`

	//
	case "oss":
		*c = `Ossetian; Ossetic`

	//
	case "ota":
		*c = `Turkish, Ottoman`

	// Collective name
	case "oto":
		*c = `Otomian languages`

	// Collective name
	case "paa":
		*c = `Papuan languages`

	//
	case "pag":
		*c = `Pangasinan`

	//
	case "pal":
		*c = `Pahlavi`

	//
	case "pam":
		*c = `Pampanga; Kapampangan`

	//
	case "pan":
		*c = `Panjabi`

	//
	case "pap":
		*c = `Papiamento`

	//
	case "pau":
		*c = `Palauan`

	//
	case "peo":
		*c = `Old Persian (ca. 600-400 B.C.)`

	// Macrolanguage
	case "per":
		*c = `Persian`

	// Collective name
	case "phi":
		*c = `Philippine languages`

	//
	case "phn":
		*c = `Phoenician`

	//
	case "pli":
		*c = `Pali`

	//
	case "pol":
		*c = `Polish`

	//
	case "pon":
		*c = `Ponapeian`

	//
	case "por":
		*c = `Portuguese`

	// Collective name
	case "pra":
		*c = `Prakrit languages`

	//
	case "pro":
		*c = `Provençal, Old (to 1500); Occitan, Old (to 1500)`

	// Macrolanguage
	case "pus":
		*c = `Pushto; Pashto`

	// ONIX local code, distinct dialect of Occitan (not distinguished from oci by ISO 639-3)
	case "qar":
		*c = `Aranés`

	// ONIX local code, distinct dialect of Catalan (not distinguished from cat by ISO 639-3)
	case "qav":
		*c = `Valencian`

	// Macrolanguage
	case "que":
		*c = `Quechua`

	// Macrolanguage
	case "raj":
		*c = `Rajasthani`

	//
	case "rap":
		*c = `Rapanui`

	//
	case "rar":
		*c = `Rarotongan; Cook Islands Maori`

	// Collective name
	case "roa":
		*c = `Romance languages`

	//
	case "roh":
		*c = `Romansh`

	// Macrolanguage
	case "rom":
		*c = `Romany`

	//
	case "rum":
		*c = `Romanian`

	//
	case "run":
		*c = `Rundi`

	//
	case "rup":
		*c = `Aromanian; Arumanian; Macedo-Romanian`

	//
	case "rus":
		*c = `Russian`

	//
	case "sad":
		*c = `Sandawe`

	//
	case "sag":
		*c = `Sango`

	//
	case "sah":
		*c = `Yakut`

	// Collective name
	case "sai":
		*c = `South American Indian languages`

	// Collective name
	case "sal":
		*c = `Salishan languages`

	//
	case "sam":
		*c = `Samaritan Aramaic`

	//
	case "san":
		*c = `Sanskrit`

	//
	case "sas":
		*c = `Sasak`

	//
	case "sat":
		*c = `Santali`

	// DEPRECATED – use srp
	case "scc":
		*c = `Serbian`

	//
	case "scn":
		*c = `Sicilian`

	//
	case "sco":
		*c = `Scots (lallans)`

	// DEPRECATED – use hrv
	case "scr":
		*c = `Croatian`

	//
	case "sel":
		*c = `Selkup`

	// Collective name
	case "sem":
		*c = `Semitic languages`

	//
	case "sga":
		*c = `Irish, Old (to 1100)`

	// Collective name
	case "sgn":
		*c = `Sign languages`

	//
	case "shn":
		*c = `Shan`

	//
	case "sid":
		*c = `Sidamo`

	//
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

	//
	case "slo":
		*c = `Slovak`

	//
	case "slv":
		*c = `Slovenian`

	//
	case "sma":
		*c = `Southern Sami`

	//
	case "sme":
		*c = `Northern Sami`

	// Collective name
	case "smi":
		*c = `Sami languages`

	//
	case "smj":
		*c = `Lule Sami`

	//
	case "smn":
		*c = `Inari Sami`

	//
	case "smo":
		*c = `Samoan`

	//
	case "sms":
		*c = `Skolt Sami`

	//
	case "sna":
		*c = `Shona`

	//
	case "snd":
		*c = `Sindhi`

	//
	case "snk":
		*c = `Soninke`

	//
	case "sog":
		*c = `Sogdian`

	//
	case "som":
		*c = `Somali`

	// Collective name
	case "son":
		*c = `Songhai languages`

	//
	case "sot":
		*c = `Sotho; Sesotho`

	//
	case "spa":
		*c = `Spanish`

	// Macrolanguage
	case "srd":
		*c = `Sardinian`

	//
	case "srn":
		*c = `Sranan Tongo`

	//
	case "srp":
		*c = `Serbian`

	//
	case "srr":
		*c = `Serer`

	// Collective name
	case "ssa":
		*c = `Nilo-Saharan languages`

	//
	case "ssw":
		*c = `Swazi; Swati`

	//
	case "suk":
		*c = `Sukuma`

	//
	case "sun":
		*c = `Sundanese`

	//
	case "sus":
		*c = `Susu`

	//
	case "sux":
		*c = `Sumerian`

	// Macrolanguage
	case "swa":
		*c = `Swahili`

	//
	case "swe":
		*c = `Swedish`

	//
	case "syc":
		*c = `Classical Syriac`

	// Macrolanguage
	case "syr":
		*c = `Syriac`

	//
	case "tah":
		*c = `Tahitian`

	// Collective name
	case "tai":
		*c = `Tai languages`

	//
	case "tam":
		*c = `Tamil`

	//
	case "tat":
		*c = `Tatar`

	//
	case "tel":
		*c = `Telugu`

	//
	case "tem":
		*c = `Temne; Time`

	//
	case "ter":
		*c = `Terena`

	//
	case "tet":
		*c = `Tetum`

	//
	case "tgk":
		*c = `Tajik`

	//
	case "tgl":
		*c = `Tagalog`

	//
	case "tha":
		*c = `Thai`

	//
	case "tib":
		*c = `Tibetan`

	//
	case "tig":
		*c = `Tigré`

	//
	case "tir":
		*c = `Tigrinya`

	//
	case "tiv":
		*c = `Tiv`

	//
	case "tkl":
		*c = `Tokelauan`

	// Artificial language
	case "tlh":
		*c = `Klingon; tlhIngan-Hol`

	//
	case "tli":
		*c = `Tlingit`

	// Macrolanguage
	case "tmh":
		*c = `Tamashek`

	//
	case "tog":
		*c = `Tonga (Nyasa)`

	//
	case "ton":
		*c = `Tongan`

	//
	case "tpi":
		*c = `Tok Pisin`

	//
	case "tsi":
		*c = `Tsimshian`

	// AKA Setswana
	case "tsn":
		*c = `Tswana`

	//
	case "tso":
		*c = `Tsonga`

	//
	case "tuk":
		*c = `Turkmen`

	//
	case "tum":
		*c = `Tumbuka`

	// Collective name
	case "tup":
		*c = `Tupi languages`

	//
	case "tur":
		*c = `Turkish`

	//
	case "tut":
		*c = `Altaic languages`

	//
	case "tvl":
		*c = `Tuvaluan`

	//
	case "twi":
		*c = `Twi`

	//
	case "tyv":
		*c = `Tuvinian`

	// ONIX local code, equivalent to tzo in ISO 639-3
	case "tzo":
		*c = `Tzotzil`

	//
	case "udm":
		*c = `Udmurt`

	//
	case "uga":
		*c = `Ugaritic`

	//
	case "uig":
		*c = `Uighur; Uyghur`

	//
	case "ukr":
		*c = `Ukrainian`

	//
	case "umb":
		*c = `Umbundu`

	//
	case "und":
		*c = `Undetermined language`

	//
	case "urd":
		*c = `Urdu`

	// Macrolanguage
	case "uzb":
		*c = `Uzbek`

	//
	case "vai":
		*c = `Vai`

	//
	case "ven":
		*c = `Venda`

	//
	case "vie":
		*c = `Vietnamese`

	// Artificial language
	case "vol":
		*c = `Volapük`

	//
	case "vot":
		*c = `Votic`

	// Collective name
	case "wak":
		*c = `Wakashan languages`

	//
	case "wal":
		*c = `Wolaitta; Wolaytta`

	//
	case "war":
		*c = `Waray`

	//
	case "was":
		*c = `Washo`

	//
	case "wel":
		*c = `Welsh`

	// Collective name
	case "wen":
		*c = `Sorbian languages`

	//
	case "wln":
		*c = `Walloon`

	//
	case "wol":
		*c = `Wolof`

	//
	case "xal":
		*c = `Kalmyk`

	//
	case "xho":
		*c = `Xhosa`

	//
	case "yao":
		*c = `Yao`

	//
	case "yap":
		*c = `Yapese`

	// Macrolanguage
	case "yid":
		*c = `Yiddish`

	//
	case "yor":
		*c = `Yoruba`

	// ONIX local code, equivalent to yue in ISO 639-3
	case "yue":
		*c = `Cantonese`

	// Collective name
	case "ypk":
		*c = `Yupik languages`

	// Macrolanguage
	case "zap":
		*c = `Zapotec`

	// Artificial language
	case "zbl":
		*c = `Blissymbols; Blissymbolics; Bliss`

	//
	case "zen":
		*c = `Zenaga`

	//
	case "zgh":
		*c = `Standard Moroccan Tamazight`

	// Macrolanguage
	case "zha":
		*c = `Zhuang; Chuang`

	// Collective name
	case "znd":
		*c = `Zande languages`

	//
	case "zul":
		*c = `Zulu`

	//
	case "zun":
		*c = `Zuni`

	//
	case "zxx":
		*c = `No linguistic content`

	// Macrolanguage
	case "zza":
		*c = `Zaza; Dimili; Dimli; Kirdki; Kirmanjki; Zazaki`
	default:
		return fmt.Errorf("undefined code for LanguageCodeIso6392B has been passed, got [%s]", v)
	}
	return nil
}

// PersonDateRole List 75: Person date role
type PersonDateRole string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *PersonDateRole) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	//
	case "007":
		*c = `Date of birth`

	//
	case "008":
		*c = `Date of death`
	default:
		return fmt.Errorf("undefined code for PersonDateRole has been passed, got [%s]", v)
	}
	return nil
}

// ProductFormFeatureValueDvdRegionCodes List 76: Product form feature value – DVD region codes
type ProductFormFeatureValueDvdRegionCodes string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *ProductFormFeatureValueDvdRegionCodes) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// DVD or Blu-Ray
	case "0":
		*c = `All regions`

	// US, US Territories, Canada
	case "1":
		*c = `DVD region 1`

	// Japan, Europe, South Africa and Middle East (including Egypt)
	case "2":
		*c = `DVD region 2`

	// Southeast Asia, Hong Kong, Macau, South Korea, and Taiwan
	case "3":
		*c = `DVD region 3`

	// Australia, New Zealand, Pacific Islands, Central America, Mexico, South America and the Caribbean
	case "4":
		*c = `DVD region 4`

	// Eastern Europe (former Soviet Union), Indian subcontinent, Africa, North Korea and Mongolia
	case "5":
		*c = `DVD region 5`

	// People’s Republic of China (except Macau and Hong Kong)
	case "6":
		*c = `DVD region 6`

	// Reserved for future use
	case "7":
		*c = `DVD region 7`

	// International venues: aircraft, cruise ships etc
	case "8":
		*c = `DVD region 8`

	// North America, Central America, South America, Japan, Taiwan, North Korea, South Korea, Hong Kong, and Southeast Asia
	case "A":
		*c = `Blu-Ray region A`

	// Most of Europe, Greenland, French territories, Middle East, Africa, Australia, and New Zealand, plus all of Oceania
	case "B":
		*c = `Blu-Ray region B`

	// India, Bangladesh, Nepal, Mainland China, Pakistan, Russia, Ukraine, Belarus, Central, and South Asia
	case "C":
		*c = `Blu-Ray region C`
	default:
		return fmt.Errorf("undefined code for ProductFormFeatureValueDvdRegionCodes has been passed, got [%s]", v)
	}
	return nil
}

// UsAndCanadaExcludingQuébecSchoolOrCollegeGrade List 77: US and Canada (excluding Québec) school or college grade
type UsAndCanadaExcludingQuébecSchoolOrCollegeGrade string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *UsAndCanadaExcludingQuébecSchoolOrCollegeGrade) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// Age typically 0-4 years
	case "P":
		*c = `Preschool`

	// Age typically 5 years
	case "K":
		*c = `Kindergarten`

	// Age typically 6 years
	case "1":
		*c = `First Grade`

	// Age typically 7 years
	case "2":
		*c = `Second Grade`

	// Age typically 8 years
	case "3":
		*c = `Third Grade`

	// Age typically 9 years
	case "4":
		*c = `Fourth Grade`

	// Age typically 10 years
	case "5":
		*c = `Fifth Grade`

	// Age typically 11 years
	case "6":
		*c = `Sixth Grade`

	// Age typically 12 years
	case "7":
		*c = `Seventh Grade`

	// Age typically 13 years
	case "8":
		*c = `Eighth Grade`

	// High School Freshman – age typically 14 years
	case "9":
		*c = `Ninth Grade`

	// High School Sophomore – age typically 15 years
	case "10":
		*c = `Tenth Grade`

	// High School Junior – age typically 16 years
	case "11":
		*c = `Eleventh Grade`

	// High School Senior – age typically 17 years
	case "12":
		*c = `Twelfth Grade`

	// Age typically 18 years
	case "13":
		*c = `College Freshman`

	// Age typically 19 years
	case "14":
		*c = `College Sophomore`

	// Age typically 20 years
	case "15":
		*c = `College Junior`

	// Age typically 21 years
	case "16":
		*c = `College Senior`

	// Age typically 22+ years
	case "17":
		*c = `College Graduate Student`
	default:
		return fmt.Errorf("undefined code for UsAndCanadaExcludingQuébecSchoolOrCollegeGrade has been passed, got [%s]", v)
	}
	return nil
}

// ProductFormDetail List 78: Product form detail
type ProductFormDetail string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *ProductFormDetail) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// CD ‘red book’ format
	case "A101":
		*c = `CD standard audio format`

	//
	case "A102":
		*c = `SACD super audio format`

	// MPEG-1/2 Audio Layer III file
	case "A103":
		*c = `MP3 format`

	// Waveform audio file
	case "A104":
		*c = `WAV format`

	//
	case "A105":
		*c = `Real Audio format`

	// Windows Media Audio format
	case "A106":
		*c = `WMA`

	// Advanced Audio Coding format
	case "A107":
		*c = `AAC`

	// Vorbis audio format in the Ogg container
	case "A108":
		*c = `Ogg/Vorbis`

	// Audio format proprietary to Audible.com
	case "A109":
		*c = `Audible`

	// Free lossless audio codec
	case "A110":
		*c = `FLAC`

	// Audio Interchangeable File Format
	case "A111":
		*c = `AIFF`

	// Apple Lossless Audio Codec
	case "A112":
		*c = `ALAC`

	// Deprecated, as does not meet DAISY 2 standard. Use conventional audiobook codes instead
	case "A201":
		*c = `DAISY 2: full audio with title only (no navigation)`

	//
	case "A202":
		*c = `DAISY 2: full audio with navigation (no text)`

	//
	case "A203":
		*c = `DAISY 2: full audio with navigation and partial text`

	//
	case "A204":
		*c = `DAISY 2: full audio with navigation and full text`

	// Reading systems may provide full audio via text-to-speech
	case "A205":
		*c = `DAISY 2: full text with navigation and partial audio`

	// Reading systems may provide full audio via text-to-speech
	case "A206":
		*c = `DAISY 2: full text with navigation and no audio`

	// Deprecated, as does not meet DAISY 3 standard. Use conventional audiobook codes instead
	case "A207":
		*c = `DAISY 3: full audio with title only (no navigation)`

	//
	case "A208":
		*c = `DAISY 3: full audio with navigation (no text)`

	//
	case "A209":
		*c = `DAISY 3: full audio with navigation and partial text`

	//
	case "A210":
		*c = `DAISY 3: full audio with navigation and full text`

	// Reading systems may provide full audio via text-to-speech
	case "A211":
		*c = `DAISY 3: full text with navigation and some audio`

	// Reading systems may provide full audio via text-to-speech
	case "A212":
		*c = `DAISY 3: full text with navigation (no audio)`

	//
	case "A301":
		*c = `Standalone audio`

	// Audio intended exclusively for use alongside a printed copy of the book. Most often a children’s product. Normally contains instructions such as ‘turn the page now’ and other references to the printed item, and is usually sold packaged together with a printed copy
	case "A302":
		*c = `Readalong audio`

	// Audio intended for musical accompaniment, eg ‘Music minus one’, etc, often used for music learning. Includes singalong backing audio for musical learning or for Karaoke-style entertainment
	case "A303":
		*c = `Playalong audio`

	// Audio intended for language learning, which includes speech plus gaps intended to be filled by the listener
	case "A304":
		*c = `Speakalong audio`

	// Audio synchronised to text within an e-publication, for example an EPUB3 with audio overlay. Synchronisation at least at paragraph level, and covering the full content
	case "A305":
		*c = `Synchronised audio`

	// Includes 'stereo' where channels are identical
	case "A410":
		*c = `Mono`

	//
	case "A420":
		*c = `Stereo`

	// Stereo plus low-frequency channel
	case "A421":
		*c = `Stereo 2.1`

	// Five-channel audio (including low-frequency channel)
	case "A441":
		*c = `Surround 4.1`

	// Six-channel audio (including low-frequency channel)
	case "A451":
		*c = `Surround 5.1`

	// In North America, a category of paperback characterized partly by page size (typically from 6¾ up to 7⅛ x 4¼ inches) and partly by target market and terms of trade. Use with Product Form code BC
	case "B101":
		*c = `Mass market (rack) paperback`

	// In North America, a category of paperback characterized partly by page size and partly by target market and terms of trade. AKA ‘quality paperback’, and including textbooks. Most paperback books sold in North America except ‘mass-market’ (B101) and ‘tall rack’ (B107) are correctly described with this code. Use with Product Form code BC
	case "B102":
		*c = `Trade paperback (US)`

	// In North America, a category of paperback characterized by page size and generally used for children’s books; use with Product Form code BC. Note: was wrongly shown as B102 (duplicate entry) in Issue 3
	case "B103":
		*c = `Digest format paperback`

	// In UK, a category of paperback characterized by page size (normally 178 x 111 mm approx); use with Product Form code BC
	case "B104":
		*c = `A-format paperback`

	// In UK, a category of paperback characterized by page size (normally 198 x 129 mm approx); use with Product Form code BC
	case "B105":
		*c = `B-format paperback`

	// In UK, a category of paperback characterized partly by size (usually in traditional hardback dimensions), and often used for paperback originals; use with Product Form code BC (replaces ‘C-format’ from former List 8)
	case "B106":
		*c = `Trade paperback (UK)`

	// In North America, a category of paperback characterised partly by page size (typically 7½ x 4¼ inches) and partly by target market and terms of trade; use with Product Form code BC
	case "B107":
		*c = `Tall rack paperback (US)`

	// 210 x 148mm
	case "B108":
		*c = `A5 size Tankobon`

	// Japanese B-series size, 257 x 182 mm
	case "B109":
		*c = `JIS B5 size Tankobon`

	// Japanese B-series size, 182 x 128 mm
	case "B110":
		*c = `JIS B6 size Tankobon`

	// 148 x 105 mm
	case "B111":
		*c = `A6 size Bunko`

	// Japanese format, 182 x 103 mm or 173 x 105 mm
	case "B112":
		*c = `B40-dori Shinsho`

	// A Swedish, Norwegian, French paperback format, of no particular fixed size. Use with Product Form Code BC
	case "B113":
		*c = `Pocket (Sweden, Norway, France)`

	// A Swedish paperback format, use with Product Form Code BC
	case "B114":
		*c = `Storpocket (Sweden)`

	// A Swedish hardback format, use with Product Form Code BB
	case "B115":
		*c = `Kartonnage (Sweden)`

	// A Swedish softback format, use with Product Form Code BC
	case "B116":
		*c = `Flexband (Sweden)`

	// A softback book in the format of a magazine, usually sold like a book. Use with Product Form code BC
	case "B117":
		*c = `Mook / Bookazine`

	// Also called ‘Flipback’. A softback book in a specially compact proprietary format with pages printed in landscape on very thin paper and bound along the long (top) edge – see www.dwarsligger.com
	case "B118":
		*c = `Dwarsligger`

	// Japanese format: 188 x 127 mm
	case "B119":
		*c = `46 size`

	// Japanese format
	case "B120":
		*c = `46-Henkei size`

	// 297 x 210 mm
	case "B121":
		*c = `A4`

	// Japanese format
	case "B122":
		*c = `A4-Henkei size`

	// Japanese format
	case "B123":
		*c = `A5-Henkei size`

	// Japanese format
	case "B124":
		*c = `B5-Henkei size`

	// Japanese format
	case "B125":
		*c = `B6-Henkei size`

	// 257 x 210 mm
	case "B126":
		*c = `AB size`

	// Japanese B-series size, 128 x 91 mm
	case "B127":
		*c = `JIS B7 size`

	// Japanese format, 218 x 152 mm or 227 x 152 mm
	case "B128":
		*c = `Kiku size`

	// Japanese format
	case "B129":
		*c = `Kiku-Henkei size`

	// Japanese B-series size, 364 x 257 mm
	case "B130":
		*c = `JIS B4 size`

	// German paperback format, greater than 205 mm high, with flaps. Use with Product form code BC
	case "B131":
		*c = `Paperback (DE)`

	//
	case "B201":
		*c = `Coloring / join-the-dot book`

	//
	case "B202":
		*c = `Lift-the-flap book`

	// DEPRECATED because of ambiguity – use B210, B214 or B215 as appropriate
	case "B203":
		*c = `Fuzzy book`

	// Note: was wrongly shown as B203 (duplicate entry) in Issue 3
	case "B204":
		*c = `Miniature book`

	//
	case "B205":
		*c = `Moving picture / flicker book`

	//
	case "B206":
		*c = `Pop-up book`

	//
	case "B207":
		*c = `Scented / ‘smelly’ book`

	//
	case "B208":
		*c = `Sound story / ‘noisy’ book`

	//
	case "B209":
		*c = `Sticker book`

	// A book whose pages have a variety of textured inserts designed to stimulate tactile exploration: see also B214 and B215
	case "B210":
		*c = `Touch-and-feel book`

	// DEPRECATED – use B212 or B213 as appropriate
	case "B211":
		*c = `Toy / die-cut book`

	// A book which is cut into a distinctive non-rectilinear shape and/or in which holes or shapes have been cut internally. (‘Die-cut’ is used here as a convenient shorthand, and does not imply strict limitation to a particular production process)
	case "B212":
		*c = `Die-cut book`

	// A book which is also a toy, or which incorporates a toy as an integral part. (Do not, however, use B213 for a multiple-item product which includes a book and a toy as separate items)
	case "B213":
		*c = `Book-as-toy`

	// A book whose cover has a soft textured finish, typically over board
	case "B214":
		*c = `Soft-to-touch book`

	// A book with detachable felt pieces and textured pages on which they can be arranged
	case "B215":
		*c = `Fuzzy-felt book`

	// Children’s picture book: use with applicable Product Form code
	case "B221":
		*c = `Picture book`

	// (aka ‘Star’ book). Tax treatment of products may differ from that of products with similar codes such as Book as toy or Pop-up book)
	case "B222":
		*c = `‘Carousel’ book`

	// Use with Product Form code BD
	case "B301":
		*c = `Loose leaf – sheets and binder`

	// Use with Product Form code BD
	case "B302":
		*c = `Loose leaf – binder only`

	// Use with Product Form code BD
	case "B303":
		*c = `Loose leaf – sheets only`

	// AKA stitched; for ‘saddle-sewn’, see code B310
	case "B304":
		*c = `Sewn`

	// Including ‘perfect bound’, ‘glued’
	case "B305":
		*c = `Unsewn / adhesive bound`

	// Strengthened cloth-over-boards binding intended for libraries: use with Product form code BB
	case "B306":
		*c = `Library binding`

	// Strengthened binding, not specifically intended for libraries
	case "B307":
		*c = `Reinforced binding`

	// Must be accompanied by a code specifiying a material, eg ‘half-bound real leather’
	case "B308":
		*c = `Half bound`

	// Must be accompanied by a code specifiying a material, eg ‘quarter bound real leather’
	case "B309":
		*c = `Quarter bound`

	// AKA ‘saddle-stitched’ or ‘wire-stitched’
	case "B310":
		*c = `Saddle-sewn`

	// Round or oval plastic forms in a clamp-like configuration: use with Product Form code BE
	case "B311":
		*c = `Comb bound`

	// Twin loop metal wire spine: use with Product Form code BE
	case "B312":
		*c = `Wire-O`

	// Cased over Coiled or Wire-O binding: use with Product Form code BE and Product Form Detail code B312 or B315
	case "B313":
		*c = `Concealed wire`

	// Spiral wire bound. Use with product form code BE. The default if a spiral binding type is not stated. Cf. Comb and Wire-O binding
	case "B314":
		*c = `Coiled wire bound`

	// Hardcover binding intended for general consumers rather than libraries, use with Product form code BB. The default if a hardcover binding detail is not stated. cf. Library binding
	case "B315":
		*c = `Trade binding`

	// Covers do not use a distinctive stock, but are the same as the body pages
	case "B400":
		*c = `Self-cover`

	// AKA fabric, linen over boards
	case "B401":
		*c = `Cloth over boards`

	//
	case "B402":
		*c = `Paper over boards`

	//
	case "B403":
		*c = `Leather, real`

	//
	case "B404":
		*c = `Leather, imitation`

	//
	case "B405":
		*c = `Leather, bonded`

	//
	case "B406":
		*c = `Vellum`

	// DEPRECATED – use new B412 or B413 as appropriate
	case "B407":
		*c = `Plastic`

	// DEPRECATED – use new B412 or B414 as appropriate
	case "B408":
		*c = `Vinyl`

	// Cloth, not necessarily over boards – cf B401
	case "B409":
		*c = `Cloth`

	// Spanish ‘simil-tela’
	case "B410":
		*c = `Imitation cloth`

	//
	case "B411":
		*c = `Velvet`

	// AKA ‘flexibound’: use with Product Form code BC
	case "B412":
		*c = `Flexible plastic/vinyl cover`

	//
	case "B413":
		*c = `Plastic-covered`

	//
	case "B414":
		*c = `Vinyl-covered`

	// Book, laminating material unspecified: use L101 for ‘whole product laminated’, eg a laminated sheet map or wallchart
	case "B415":
		*c = `Laminated cover`

	// With card cover (like a typical paperback). As distinct from a self-cover or more elaborate binding
	case "B416":
		*c = `Card cover`

	// Type unspecified
	case "B501":
		*c = `With dust jacket`

	// Used to distinguish from B503
	case "B502":
		*c = `With printed dust jacket`

	// With translucent paper or plastic protective cover
	case "B503":
		*c = `With translucent dust cover`

	// For paperback with flaps
	case "B504":
		*c = `With flaps`

	//
	case "B505":
		*c = `With thumb index`

	// If the number of markers is significant, it can be stated as free text in <ProductFormDescription>
	case "B506":
		*c = `With ribbon marker(s)`

	//
	case "B507":
		*c = `With zip fastener`

	//
	case "B508":
		*c = `With button snap fastener`

	// AKA yapp edge?
	case "B509":
		*c = `With leather edge lining`

	// With edge trimming such that the front edge is ragged, not neatly and squarely trimmed: AKA deckle edge, feather edge, uncut edge, rough cut
	case "B510":
		*c = `Rough front`

	// With one or more gatefold or foldout sections bound in
	case "B511":
		*c = `With foldout`

	// Pages include extra-wide margin specifically intended for hand-written annotations
	case "B512":
		*c = `Wide margin`

	// Book with attached loop for fixing to baby stroller, cot, chair etc
	case "B513":
		*c = `With fastening strap`

	// With one or more pages perforated and intended to be torn out for use
	case "B514":
		*c = `With perforated pages`

	// A book in which half the content is printed upside-down, to be read the other way round. Also known as a ‘flip-book’, ‘back-to-back’, (fr.) ‘tête-bêche’ (usually an omnibus of two works)
	case "B601":
		*c = `Turn-around book`

	// Manga with pages and panels in the sequence of the original Japanese, but with Western text
	case "B602":
		*c = `Unflipped manga format`

	// Text shows syllable breaks
	case "B610":
		*c = `Syllabification`

	// Single letters only. Was formerly identified as UK Braille Grade 1
	case "B701":
		*c = `UK Uncontracted Braille`

	// With some letter combinations. Was formerly identified as UK Braille Grade 2
	case "B702":
		*c = `UK Contracted Braille`

	// DEPRECATED- use B704/B705 as appropriate instead
	case "B703":
		*c = `US Braille`

	//
	case "B704":
		*c = `US Uncontracted Braille`

	//
	case "B705":
		*c = `US Contracted Braille`

	//
	case "B706":
		*c = `Unified English Braille`

	// Moon embossed alphabet, used by some print-impaired readers who have difficulties with Braille
	case "B707":
		*c = `Moon`

	// Includes RealVideo packaged within a .rm RealMedia container
	case "D101":
		*c = `Real Video format`

	//
	case "D102":
		*c = `Quicktime format`

	//
	case "D103":
		*c = `AVI format`

	//
	case "D104":
		*c = `Windows Media Video format`

	//
	case "D105":
		*c = `MPEG-4`

	// Use with an applicable Product Form code D*; note that more detail of operating system requirements can be given in a Product Form Feature composite
	case "D201":
		*c = `MS-DOS`

	// Use with an applicable Product Form code D*; see note on D201
	case "D202":
		*c = `Windows`

	// Use with an applicable Product Form code D*; see note on D201
	case "D203":
		*c = `Macintosh`

	// Use with an applicable Product Form code D*; see note on D201
	case "D204":
		*c = `UNIX / LINUX`

	// Use with an applicable Product Form code D*; see note on D201
	case "D205":
		*c = `Other operating system(s)`

	// Use with an applicable Product Form code D*; see note on D201
	case "D206":
		*c = `Palm OS`

	// Use with an applicable Product Form code D*; see note on D201
	case "D207":
		*c = `Windows Mobile`

	// Use with Product Form code DE or DB as applicable
	case "D301":
		*c = `Microsoft XBox`

	// Use with Product Form code DE or DB as applicable
	case "D302":
		*c = `Nintendo Gameboy Color`

	// Use with Product Form code DE or DB as applicable
	case "D303":
		*c = `Nintendo Gameboy Advanced`

	// Use with Product Form code DE or DB as applicable
	case "D304":
		*c = `Nintendo Gameboy`

	// Use with Product Form code DE or DB as applicable
	case "D305":
		*c = `Nintendo Gamecube`

	// Use with Product Form code DE or DB as applicable
	case "D306":
		*c = `Nintendo 64`

	// Use with Product Form code DE or DB as applicable
	case "D307":
		*c = `Sega Dreamcast`

	// Use with Product Form code DE or DB as applicable
	case "D308":
		*c = `Sega Genesis/Megadrive`

	// Use with Product Form code DE or DB as applicable
	case "D309":
		*c = `Sega Saturn`

	// Use with Product Form code DE or DB as applicable
	case "D310":
		*c = `Sony PlayStation 1`

	// Use with Product Form code DE or DB as applicable
	case "D311":
		*c = `Sony PlayStation 2`

	//
	case "D312":
		*c = `Nintendo Dual Screen`

	//
	case "D313":
		*c = `Sony PlayStation 3`

	//
	case "D314":
		*c = `Xbox 360`

	//
	case "D315":
		*c = `Nintendo Wii`

	//
	case "D316":
		*c = `Sony PlayStation Portable (PSP)`

	// Use where a particular e-publication type (specified in <EpubType>) has both reflowable and fixed-format variants
	case "E200":
		*c = `Reflowable`

	// Use where a particular e-publication type (specified in <EpubType>) has both reflowable and fixed-format variants
	case "E201":
		*c = `Fixed format`

	// All e-publication resources are included within the e-publication package
	case "E202":
		*c = `Readable offline`

	// E-publication requires a network connection to access some resources (eg an enhanced e-book where video clips are not stored within the e-publication ‘package’ itself, but are delivered via an internet connection)
	case "E203":
		*c = `Requires network connection`

	// Resources (eg images) present in other editions have been removed from this product, eg due to rights issues
	case "E204":
		*c = `Content removed`

	// Use for fixed-format e-books optimised for landscape display. Also include an indication of the optimal screen aspect ratio
	case "E210":
		*c = `Landscape`

	// Use for fixed-format e-books optimised for portrait display. Also include an indication of the optimal screen aspect ratio
	case "E211":
		*c = `Portrait`

	// Use for fixed-format e-books optimised for displays with a 5:4 aspect ratio (eg 1280x1024 pixels etc, assuming square pixels). Note that aspect ratio codes are NOT specific to actual screen dimensions or pixel counts, but to the ratios between two dimensions or two pixel counts
	case "E221":
		*c = `5:4`

	// Use for fixed-format e-books optimised for displays with a 4:3 aspect ratio (eg 800x600, 1024x768, 2048x1536 pixels etc)
	case "E222":
		*c = `4:3`

	// Use for fixed-format e-books optimised for displays with a 3:2 aspect ratio (eg 960x640, 3072x2048 pixels etc)
	case "E223":
		*c = `3:2`

	// Use for fixed-format e-books optimised for displays with a 16:10 aspect ratio (eg 1440x900, 2560x1600 pixels etc)
	case "E224":
		*c = `16:10`

	// Use for fixed-format e-books optimised for displays with a 16:9 aspect ratio (eg 1024x576, 1920x1080, 2048x1152 pixels etc)
	case "E225":
		*c = `16:9`

	// Whole product laminated (eg laminated map, fold-out chart, wallchart, etc): use B415 for book with laminated cover
	case "L101":
		*c = `Laminated`

	// Use with Product Form code PC
	case "P101":
		*c = `Desk calendar`

	// Use with Product Form code PC
	case "P102":
		*c = `Mini calendar`

	// Use with Product Form code PC
	case "P103":
		*c = `Engagement calendar`

	// Use with Product Form code PC
	case "P104":
		*c = `Day by day calendar`

	// Use with Product Form code PC
	case "P105":
		*c = `Poster calendar`

	// Use with Product Form code PC
	case "P106":
		*c = `Wall calendar`

	// Use with Product Form code PC
	case "P107":
		*c = `Perpetual calendar`

	// Use with Product Form code PC
	case "P108":
		*c = `Advent calendar`

	// Use with Product Form code PC
	case "P109":
		*c = `Bookmark calendar`

	// Use with Product Form code PC
	case "P110":
		*c = `Student calendar`

	// Use with Product Form code PC
	case "P111":
		*c = `Project calendar`

	// Use with Product Form code PC
	case "P112":
		*c = `Almanac calendar`

	// A calendar that is not one of the types specified elsewhere: use with Product Form code PC
	case "P113":
		*c = `Other calendar`

	// A product that is associated with or ancillary to a calendar or organiser, eg a deskstand for a calendar, or an insert for an organiser: use with Product Form code PC or PS
	case "P114":
		*c = `Other calendar or organiser product`

	// Kamishibai / Cantastoria cards
	case "P120":
		*c = `Picture story cards`

	// Stationery item in hardback book format
	case "P201":
		*c = `Hardback (stationery)`

	// Stationery item in paperback/softback book format
	case "P202":
		*c = `Paperback / softback (stationery)`

	// Stationery item in spiral-bound book format
	case "P203":
		*c = `Spiral bound (stationery)`

	// Stationery item in leather-bound book format, or other fine binding
	case "P204":
		*c = `Leather / fine binding (stationery)`

	// For map, poster, wallchart etc
	case "P301":
		*c = `With hanging strips`

	// TV standard for video or DVD
	case "V201":
		*c = `PAL`

	// TV standard for video or DVD
	case "V202":
		*c = `NTSC`

	// TV standard for video or DVD
	case "V203":
		*c = `SECAM`

	// Licensed for use in domestic contexts only
	case "V220":
		*c = `Home use`

	// Licensed for use in education
	case "V221":
		*c = `Classroom use`
	default:
		return fmt.Errorf("undefined code for ProductFormDetail has been passed, got [%s]", v)
	}
	return nil
}

// ProductFormFeatureType List 79: Product form feature type
type ProductFormFeatureType string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *ProductFormFeatureType) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// For Product Form Feature values see code list 98
	case "01":
		*c = `Color of cover`

	// For Product Form Feature values see code list 98
	case "02":
		*c = `Color of page edge`

	// The principal font used for body text, when this is a significant aspect of product description, eg for some Bibles, and for large print product. The accompanying Product Form Feature Description is text specifying font size and, if desired, typeface
	case "03":
		*c = `Text font`

	// For Product Form Feature values see code list 99
	case "04":
		*c = `Special cover material`

	// For Product Form Feature values see code list 76
	case "05":
		*c = `DVD region`

	// A computer or handheld device operating system required to use a digital product, with version detail if applicable. The accompanying Product Form Feature Value is a code from List 176. Version detail, when applicable, is carried in Product Form Feature Description
	case "06":
		*c = `Operating system requirements`

	// Other system requirements for a digital product, described by free text in Product Form Feature Description
	case "07":
		*c = `Other system requirements`

	// Indicates compatibility with proprietary ‘point and listen’ devices such as Ting Pen (http://www.ting.eu) or the iSmart Touch and Read Pen. These devices scan invisible codes specially printed on the page to identify the book and position of the word, and the word is then read aloud by the device. The name of the compatible device (or range of devices) should be given in <ProductFormFeatureDescription>
	case "08":
		*c = `‘Point and listen’ device compatibility`

	// For <ProductFormFeatureValue> codes, see Codelist 196
	case "09":
		*c = `E-publication accessibility detail`

	// For versioned e-book file formats (or in some cases, devices). <ProductFormFeatureValue> should contain the version number as a period-separated list of numbers (eg ‘7’, ‘1.5’ or ‘3.10.7’). Use only with ONIX 3.0 – in ONIX 2.1, use <EpubTypeVersion> instead. For the most common file formats, code 15 and List 220 is strongly preferred
	case "10":
		*c = `E-publication format version`

	// DEPRECATED – use code 12 and List 143
	case "11":
		*c = `CPSIA choking hazard warning`

	// Choking hazard warning required by US Consumer Product Safety Improvement Act (CPSIA) of 2008. Required, when applicable, for products sold in the US. The Product Form Feature Value is a code from List 143. Further explanation may be given in Product Form Feature Description
	case "12":
		*c = `CPSIA choking hazard warning`

	// Product carries hazard warning required by EU Toy Safety Directive. The Product Form Feature Value is a code from List 184, and (for some codes) the exact wording of the warning may be given in Product Form Feature Description
	case "13":
		*c = `EU Toy Safety Hazard warning`

	// Product Form Feature Description must give further details of the warning
	case "14":
		*c = `IATA Dangerous Goods warning`

	// For common versioned e-book formats (or in some cases, devices) – for example EPUB 2.0.1 or EPUB 3.0. <ProductFormFeatureValue> is a code from list 220. Use in ONIX 3.0 only
	case "15":
		*c = `E-publication format version code`

	// For common versioned e-book formats, the name and version of the validator used to check conformance. <ProductFormFeatureDescription> is the common name of the validator used (eg EpubCheck, Flightdeck), and <ProductFormFeatureValue> is the version number of the validator (eg 4.0.0a). Use with code 15 (or possibly code 10), or with <EpubTypeVersion>, to specify the version the e-publication conforms with
	case "16":
		*c = `E-publication format validator version`

	// Product does not carry FSC or PEFC logo. The Product Form Feature Value and Description elements are not used. The product may, however, still carry a claimed Pre- and Post-Consumer Waste (PCW) content (type code 37) in a separate repeat of the Product Form Feature composite
	case "30":
		*c = `Not FSC or PEFC certified`

	// Product carries FSC logo (Pure, 100%). <ProductFormFeatureValue> is the Certification number (ie either a Chain Of Custody (COC) number or a Trademark License number) printed on the book. Format: Chain of Custody number is two to five letters-COC-six digits (the digits should include leading zeros if necessary), eg ‘AB-COC-001234’ or ‘ABCDE-COC-123456’; Trademark License number is C followed by six digits, eg ‘C005678’ (this would normally be prefixed by ‘FSC®’ when displayed). By definition, a product certified Pure does not contain Pre- or Post-Consumer-Waste (PCW), so type code 31 can only occur on its own. Certification numbers may be checked at http://info.fsc.org/
	case "31":
		*c = `FSC certified – pure`

	// Product carries FSC logo (Mixed sources, Mix). <ProductFormFeatureValue> is the Certification number (ie either a Chain Of Custody (COC) number or a Trademark License number) printed on the book. Format: Chain of Custody number is two to five letters-COC-six digits (the digits should include leading zeros if necessary), eg ‘AB-COC-001234’ or ‘ABCDE-COC-123456’; Trademark License number is C followed by six digits, eg ‘C005678’ (this would normally be prefixed by ‘FSC®’ when displayed). May be accompanied by a Pre- and Post-Consumer-Waste (PCW) percentage value, to be reported in another instance of <ProductFormFeature> with type code 36. Certification numbers may be checked at http://info.fsc.org/
	case "32":
		*c = `FSC certified – mixed sources`

	// Product carries FSC logo (Recycled). <ProductFormFeatureValue> is the Certification number (ie either a Chain Of Custody (COC) number or a Trademark License number) printed on the book. Format: Chain of Custody number is two to five letters-COC-six digits (the digits should include leading zeroes if necessary), eg ‘AB-COC-001234’ or ‘ABCDE-COC-123456’; Trademark License number is C followed by six digits, eg ‘C005678’ (this would normally be prefixed by ‘FSC®’ when displayed). Should be accompanied by a Pre- and Post-Consumer-Waste (PCW) percentage value, to be reported in another instance of <ProductFormFeature> with type code 36. Certification numbers may be checked at http://info.fsc.org/
	case "33":
		*c = `FSC certified – recycled`

	// Product carries PEFC logo (certified). <ProductFormFeatureValue> is the Chain Of Custody (COC) number printed on the book. May be accompanied by a Post-Consumer Waste (PCW) percentage value, to be reported in another instance of <ProductFormFeature> with type code 36
	case "34":
		*c = `PEFC certified`

	// Product carries PEFC logo (recycled). <ProductFormFeatureValue> is the Chain Of Custody (COC) number printed on the book. Should be accompanied by a Post-Consumer-Waste (PCW) percentage value, to be reported in another instance of <ProductFormFeature> with type code 36
	case "35":
		*c = `PEFC recycled`

	// The percentage of recycled Pre- and Post-Consumer-Waste (PCW) used in a product where the composition is certified by FSC or PEFC. <ProductFormFeatureValue> is an integer. May occur together with type code 32, 33, 34 or 35
	case "36":
		*c = `FSC or PEFC certified Pre- and Post-Consumer Waste (PCW) percentage`

	// The percentage of recycled Pre- and Post-Consumer Waste (PCW) claimed to be used in a product where the composition is not certified by FSC or PEFC. <Product FormFeatureValue> is an integer. <ProductFormFeatureDescription> may carry free text supporting the claim. Must be accompanied by type code 30
	case "37":
		*c = `Claimed Pre- and Post-Consumer Waste (PCW) percentage`

	// Product made from paper produced using environmentally-conscious technology. <ProductFormFeatureDescription> may carry free text with a more detailed statement
	case "40":
		*c = `Paper produced by ‘green’ technology`
	default:
		return fmt.Errorf("undefined code for ProductFormFeatureType has been passed, got [%s]", v)
	}
	return nil
}

// BookFormDetail List 8: Book form detail
type BookFormDetail string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *BookFormDetail) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// DEPRECATED
	case "01":
		*c = `A-format paperback`

	// ‘B’ format paperback: UK 198 x 129 mm – DEPRECATED
	case "02":
		*c = `B-format paperback`

	// ‘C’ format paperback: UK 216 x 135 mm – DEPRECATED
	case "03":
		*c = `C-format paperback`

	// DEPRECATED
	case "04":
		*c = `Paper over boards`

	// DEPRECATED
	case "05":
		*c = `Cloth`

	// DEPRECATED
	case "06":
		*c = `With dust jacket`

	// DEPRECATED
	case "07":
		*c = `Reinforced binding`
	default:
		return fmt.Errorf("undefined code for BookFormDetail has been passed, got [%s]", v)
	}
	return nil
}

// ProductPackagingType List 80: Product packaging type
type ProductPackagingType string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *ProductPackagingType) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// No packaging, or all smaller items enclosed inside largest item
	case "00":
		*c = `No outer packaging`

	// Thin card or soft plastic sleeve, much less rigid than a slip case
	case "01":
		*c = `Slip-sleeve`

	// Packaging consisting of formed plastic sealed around each side of the product. Not to be confused with single-sided Blister pack
	case "02":
		*c = `Clamshell`

	// Typical DVD-style packaging, sometimes known as an ‘Amaray’ case
	case "03":
		*c = `Keep case`

	// Typical CD-style packaging
	case "05":
		*c = `Jewel case`

	// Common CD-style packaging, a card folder with one or more panels incorporating a tray, hub or pocket to hold the disc(s)
	case "06":
		*c = `Digipak`

	// Individual item, items or set in card box with separate or hinged lid: not to be confused with the commonly-used ‘boxed set’
	case "09":
		*c = `In box`

	// Slip-case for single item only: German ‘Schuber’
	case "10":
		*c = `Slip-cased`

	// Slip-case for multi-volume set: German ‘Kassette’; also commonly referred to as ‘boxed set’
	case "11":
		*c = `Slip-cased set`

	// Rolled in tube or cylinder: eg sheet map or poster
	case "12":
		*c = `Tube`

	// Use for miscellaneous items such as slides, microfiche, when presented in a binder
	case "13":
		*c = `Binder`

	// Use for miscellaneous items such as slides, microfiche, when presented in a wallet or folder
	case "14":
		*c = `In wallet or folder`

	// Long package with triangular cross-section used for rolled sheet maps, posters etc
	case "15":
		*c = `Long triangular package`

	// Long package with square cross-section used for rolled sheet maps, posters, etc
	case "16":
		*c = `Long square package`

	//
	case "17":
		*c = `Softbox (for DVD)`

	// In pouch, eg teaching materials in a plastic bag or pouch
	case "18":
		*c = `Pouch`

	// In duroplastic or other rigid plastic case, eg for a class set
	case "19":
		*c = `Rigid plastic case`

	// In cardboard case, eg for a class set
	case "20":
		*c = `Cardboard case`

	// Use for products or product bundles supplied for retail sale in shrink-wrapped packaging. For shrink-wrapped packs of multiple products for trade supply only, see code XL in List 7
	case "21":
		*c = `Shrink-wrapped`

	// A pack comprising a pre-formed plastic blister and a printed card with a heat-seal coating
	case "22":
		*c = `Blister pack`

	// A case with carrying handle, typically for a set of educational books and/or learning materials
	case "23":
		*c = `Carry case`

	// Individual item, items or set in metal box or can with separate or hinged lid
	case "24":
		*c = `In tin`
	default:
		return fmt.Errorf("undefined code for ProductPackagingType has been passed, got [%s]", v)
	}
	return nil
}

// ProductContentType List 81: Product content type
type ProductContentType string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *ProductContentType) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// Readable text of the main work: this value is required, together with applicable <ProductForm> and <ProductFormDetail> values, to designate an e-book or other digital product whose primary content is eye-readable text
	case "10":
		*c = `Text (eye-readable)`

	// E-publication is enhanced with a significant number of actionable cross-references, hyperlinked notes and annotations, or with other links between largely textual elements (eg quiz/test questions, ‘choose your own ending’ etc)
	case "15":
		*c = `Extensive links between internal content`

	// E-publication is enhanced with a significant number of actionable (clickable) web links
	case "14":
		*c = `Extensive links to external content`

	// E-publication is enhanced with additional textual content such as interview, feature article, essay, bibliography, quiz/test, other background material or text that is not included in a primary or ‘unenhanced’ version
	case "16":
		*c = `Additional eye-readable text not part of main work`

	// eg Teaser chapter
	case "17":
		*c = `Promotional text for other book product`

	//
	case "11":
		*c = `Musical notation`

	// Use only when no more detailed specification is provided
	case "07":
		*c = `Still images / graphics`

	// Whether in a plate section / insert, or not
	case "18":
		*c = `Photographs`

	// Including other ‘mechanical’ (ie non-photographic) illustrations
	case "19":
		*c = `Figures, diagrams, charts, graphs`

	// E-publication is enhanced with additional images or graphical content such as supplementary photographs that are not included in a primary or ‘unenhanced’ version
	case "20":
		*c = `Additional images / graphics not part of main work`

	//
	case "12":
		*c = `Maps and/or other cartographic content`

	// Audio recording of a reading of a book or other text
	case "01":
		*c = `Audiobook`

	// Audio recording of a drama or other spoken word performance
	case "02":
		*c = `Performance – spoken word`

	// eg an interview, not a ‘reading’ or ‘performance’)
	case "13":
		*c = `Other speech content`

	// Audio recording of a music performance, including musical drama and opera
	case "03":
		*c = `Music recording`

	// Audio recording of other sound, eg birdsong
	case "04":
		*c = `Other audio`

	// Audio recording of a reading, performance or dramatization of part of the work
	case "21":
		*c = `Partial performance – spoken word`

	// Product is enhanced with audio recording of full or partial reading, performance, dramatization, interview, background documentary or other audio content not included in the primary or ‘unenhanced’ version
	case "22":
		*c = `Additional audio content not part of main work`

	// eg Reading of teaser chapter
	case "23":
		*c = `Promotional audio for other book product`

	// Includes Film, video, animation etc. Use only when no more detailed specification is provided. Formerly ‘Moving images’
	case "06":
		*c = `Video`

	//
	case "26":
		*c = `Video recording of a reading`

	// Video recording of a drama or other performance, including musical performance
	case "27":
		*c = `Performance – visual`

	// eg animated diagrams, charts, graphs or other illustrations
	case "24":
		*c = `Animated / interactive illustrations`

	// eg cartoon, animatic or CGI animation
	case "25":
		*c = `Narrative animation`

	// Other video content eg interview, not a reading or performance
	case "28":
		*c = `Other video`

	// Video recording of a reading, performance or dramatization of part of the work
	case "29":
		*c = `Partial performance – video`

	// E-publication is enhanced with video recording of full or partial reading, performance, dramatization, interview, background documentary or other content not included in the primary or ‘unenhanced’ version
	case "30":
		*c = `Additional video content not part of main work`

	// eg Book trailer
	case "31":
		*c = `Promotional video for other book product`

	// No multi-user functionality. Formerly just ‘Game’
	case "05":
		*c = `Game / Puzzle`

	// Includes some degree of multi-user functionality
	case "32":
		*c = `Contest`

	// Largely ‘content free’
	case "08":
		*c = `Software`

	// Data files
	case "09":
		*c = `Data`

	//
	case "33":
		*c = `Data set plus software`

	// Intended to be filled in by the reader
	case "34":
		*c = `Blank pages`

	// Use only where type of advertising content is not stated
	case "35":
		*c = `Advertising content`

	// ‘Back ads’ – promotional pages for other books (that do not include sample content, cf codes 17, 23)
	case "37":
		*c = `Advertising – first party`

	// Eg to obtain discounts on other products
	case "36":
		*c = `Advertising – coupons`

	//
	case "38":
		*c = `Advertising – third party display`

	//
	case "39":
		*c = `Advertising – third party textual`
	default:
		return fmt.Errorf("undefined code for ProductContentType has been passed, got [%s]", v)
	}
	return nil
}

// BibleContents List 82: Bible contents
type BibleContents string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *BibleContents) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// The seven portions of the Apocrypha added to the Catholic canon at the Council of Trent in 1546: Tobit; Judith; Wisdom of Solomon; Sirach (Ecclesiasticus); Baruch, including the Letter of Jeremiah; I and II Maccabees; Extra portions of Esther and Daniel (Additions to Esther; the Prayer of Azariah; Song of the Three Jews; Susannah; Bel and the Dragon). These are not generally included in the Protestant canon
	case "AP":
		*c = `Apocrypha (Catholic canon)`

	// A collection of Apocryphal texts, canon not specified
	case "AQ":
		*c = `Apocrypha (canon unspecified)`

	// I Esdras; Prayer of Manasseh; Psalm 151; III Maccabees
	case "AX":
		*c = `Additional Apocryphal texts: Greek Orthodox canon`

	// I and II Esdras; Prayer of Manasseh; Psalm 151; III and IV Maccabees
	case "AY":
		*c = `Additional Apocryphal texts: Slavonic Orthodox canon`

	// Additional Apocryphal texts included in some Bible versions: I and II Esdras; Prayer of Manasseh
	case "AZ":
		*c = `Additional Apocryphal texts`

	// The 66 books included in the Protestant, Catholic and Orthodox canons, together with the seven portions of the Apocrypha included in the Catholic canon. (Equivalent to OT plus NT plus AP)
	case "GA":
		*c = `General canon with Apocrypha (Catholic canon)`

	// The 66 books included in the Protestant, Catholic and Orthodox canons, together with Apocryphal texts, canon not specified. (Equivalent to OT plus NT plus AQ)
	case "GC":
		*c = `General canon with Apocryphal texts (canon unspecified)`

	// The 66 books included in the Protestant, Catholic and Orthodox canons, 39 from the Old Testament and 27 from the New Testament. The sequence of books may differ in different canons. (Equivalent to OT plus NT)
	case "GE":
		*c = `General canon`

	// The books of Matthew, Mark, Luke and John
	case "GS":
		*c = `Gospels`

	// Those 39 books which were included in the Jewish canon by the rabbinical academy established at Jamma in 90 CE. Also known as the Jewish or Hebrew scriptures
	case "OT":
		*c = `Old Testament`

	// The 27 books included in the Christian canon through the Easter Letter of Athanasius, Bishop of Alexandria and also by a general council of the Christian church held near the end of the 4th century CE
	case "NT":
		*c = `New Testament`

	// Includes the 27 books of the New Testament plus Psalms and Proverbs from the Old Testament. Equivalent to NT plus PP)
	case "NP":
		*c = `New Testament with Psalms and Proverbs`

	// The books containing the letters of Paul to the various early Christian churches
	case "PE":
		*c = `Paul’s Epistles`

	// The book of Psalms and the book of Proverbs combined
	case "PP":
		*c = `Psalms and Proverbs`

	// The book of Psalms
	case "PS":
		*c = `Psalms`

	// The first five books of the Bible: Genesis, Exodus, Numbers, Leviticus, Deuteronomy. Also applied to the Torah
	case "PT":
		*c = `Pentateuch`

	// Selected books of either the OT or NT not otherwise noted
	case "ZZ":
		*c = `Other portions`
	default:
		return fmt.Errorf("undefined code for BibleContents has been passed, got [%s]", v)
	}
	return nil
}

// BibleVersion List 83: Bible version
type BibleVersion string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *BibleVersion) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// Alberto Vaccari – Pontificio Istituto Biblico
	case "ALV":
		*c = `Alberto Vaccari`

	// A translation based on the American Standard Version and showing multiple options for the translation of ancient text. Published in full in 1965. Sponsored by the Lockman Foundation
	case "AMP":
		*c = `Amplified`

	// Most popular Catholic Bible translation in Italian prior to the CEI translation in 1971
	case "ANM":
		*c = `Antonio Martini`

	// A 1901 translation using verbal equivalence techniques with the purpose of Americanizing the REV
	case "ASV":
		*c = `American Standard`

	// 2011 contemporary English translation of the Bible sponsored by the US-based Christian Resources Development Corporation. The translation includes Old Testament, Apocrypha and New Testament, and is aimed to be accessible to most English readers (minimum 7th grade reading age)
	case "CEB":
		*c = `Common English Bible`

	// Italian Episcopal Conference 1971 translation suitable for Italian Catholic liturgy. (Includes minor 1974 revision)
	case "CEI":
		*c = `Conferenza Episcopale Italiana`

	// New translation of the C.E.I. first published in 2008 – the version most widely used by the Italian Catholic Church
	case "CEN":
		*c = `Conferenza Episcopale Italiana 2008`

	// A translation completed in 1995 and sponsored by the American Bible Society under the leadership of Barclay Newman
	case "CEV":
		*c = `Contemporary English`

	// 1968 Interfaith version promoted by the Italian Bible Society. Has a Catholic ‘imprimateur’, but its ecumenical approach has Jewish, Protestant and Christian Orthodox approval
	case "CNC":
		*c = `Concordata`

	// Version based on original documents, edited by Giovanni Diodati in 1607, revised by Diodati in 1641 and again in 1894. It is the reference version for many Italian Protestants
	case "DDI":
		*c = `Diodati`

	// Revision of the Diodati Bible dating to the 1990s, aiming at highest fidelity to original ancient Greek (New Testament) and Hebrew (Old Testament) texts
	case "DDN":
		*c = `Nuova Diodati`

	// An early (1580-1609) English translation from the Latin Vulgate designed for Catholics and performed by George Martin
	case "DOU":
		*c = `Douay-Rheims`

	// A German translation of the Bible for use in Roman Catholic churches
	case "EIN":
		*c = `Einheitsübersetzung`

	// An update of the Revised Standard Version that makes ‘modest’ use of gender-free terminology
	case "ESV":
		*c = `English Standard`

	// Finnish Bible translation
	case "FBB":
		*c = `Biblia (1776)`

	// Finnish Bible translation
	case "FRA":
		*c = `Raamattu (1933/1938)`

	// Finnish Bible translation
	case "FRK":
		*c = `Raamattu kansalle`

	// Finnish Bible translation
	case "FRM":
		*c = `Raamattu (1992)`

	// A 1995 translation by the World Bible Publishing Company using the English language in a manner to communicate to the late 20th century American
	case "GDW":
		*c = `God’s Word`

	// An early (1560) English version of the Bible translated by William Whittingham with strong Protestant leanings
	case "GEN":
		*c = `Geneva`

	// A translation sponsored by the American Bible Society. The New Testament was first published (as ‘Today’s English Version’ TEV) in 1966. The Old Testament was completed in 1976, and the whole was published as the ‘Good News Bible’
	case "GNB":
		*c = `Good News`

	// Version edited by E. Galbiati, A. Penna and P. Rossano, and published by UTET. This version, based on original texts, is rich in notes and has been used as the basis for CEI translation
	case "GPR":
		*c = `Galbiati, Penna, Rossano – UTET`

	// New Testament text in an original Greek version
	case "GRK":
		*c = `Original Greek`

	// Richly annotated 1963 Version edited by S. Garofano and S. Rinaldi, and published by Marietti
	case "GRM":
		*c = `Garofano, Rinaldi – Marietti`

	// Old Testament text in an original Hebrew version
	case "HBR":
		*c = `Original Hebrew`

	// Published by Broadman and Holman this translation rejects all forms of gender-neutral wording and is written with strong influences from the Southern Baptist perspective of biblical scholarship
	case "HCS":
		*c = `Holman Christian Standard`

	// A translation completed in 1986 targeting readability at the US third grade level
	case "ICB":
		*c = `International Children’s`

	// Interconfessional translation resulting from 1985 effort by Catholic and Protestant scholars, aimed at delivering an easy-to-understand message
	case "ILC":
		*c = `Traduzione Interconfessionale in Lingua Corrente`

	// A translation designed for English speaking Catholics based on the original languages. It is based on French as well as ancient texts and was first published in 1966
	case "JER":
		*c = `Jerusalem`

	// A translation commissioned by King James I of England and first published in 1611
	case "KJV":
		*c = `King James`

	// A verbal translation led by William Prindele. Published in 1994, it was designed to modernize the language of the King James Version based on Webster’s New International Dictionary, 2nd edition, unabridged
	case "KJT":
		*c = `21st Century King James`

	// A paraphrase translation led by Kenneth N Taylor and first published in 1972
	case "LVB":
		*c = `Living Bible`

	// 1924 translation by Giovanni Luzzi, Professor at the Waldensian Faculty of Theology in Rome, who revised the 17th Century Diodati version
	case "LZZ":
		*c = `Luzzi`

	// A paraphrase translation of the New Testament by Eugene Peterson first published in 1993
	case "MSG":
		*c = `Message Bible`

	// A translation aimed at Catholic readers first published in its entirety in 1970. A revised New Testament was issued in 1986 as the 2nd Edition. The 3rd Edtion was published in 1991 with a revision to Psalms. The 4th Edition (also known as the New American Bible Revised Edition) was published in 2011, incorporating revisions to the Old Testament
	case "NAB":
		*c = `New American`

	// A translation commissioned by the Lockman Foundation. The New Testament was published in 1960 followed by the entire Bible in 1971
	case "NAS":
		*c = `New American Standard`

	// A 1995 translation using more modern language than the NASB
	case "NAU":
		*c = `New American Standard, Updated`

	// Norwegian Bible translation
	case "NBA":
		*c = `Bibelen 1895`

	// Norwegian Bible translation
	case "NBB":
		*c = `Bibelen 1930`

	// Norwegian Bible translation
	case "NBC":
		*c = `Bibelen 1938`

	// Norwegian Bible translation
	case "NBD":
		*c = `Bibelen 1978-85`

	// Norwegian Bible translation
	case "NBE":
		*c = `Bibelen 1978`

	// Norwegian Bible translation
	case "NBF":
		*c = `Bibelen 1985`

	// Norwegian Bible translation
	case "NBG":
		*c = `Bibelen 1988`

	// Norwegian Bible translation
	case "NBH":
		*c = `Bibelen 1978-85/rev. 2005`

	// Norwegian Bible translation
	case "NBI":
		*c = `Bibelen 2011`

	// A translation inspired by the International Children’s version. First published by World Publishing in 1991
	case "NCV":
		*c = `New Century`

	// A translation first issued in 1961 (New Testament) and 1970 (complete Bible) as a result of a proposal at the 1946 General Assembly of the Church of Scotland
	case "NEB":
		*c = `New English`

	// Norwegian Bible translation
	case "NGO":
		*c = `Bibelen Guds ord`

	// A translation underwritten by Biblica (formerly the International Bible Society, and previously the New York Bible Society). The New Testament was published in 1973 followed by the entire Bible in 1978. The NIV text was revised in 1984 and again in 2011
	case "NIV":
		*c = `New International`

	// A 1996 translation designed for people with limited literacy in English and based on the NIV
	case "NIR":
		*c = `New International Reader’s`

	// A revision of the Jerusalem Bible. First published in 1986
	case "NJB":
		*c = `New Jerusalem`

	// A version issued by Thomas Nelson Publishers in 1982-83 designed to update the language of the King James Version while maintaining the phrasing and rhythm and using the same sources as its predecessor
	case "NKJ":
		*c = `New King James`

	// Norwegian ‘nynorsk’ Bible translation
	case "NNK":
		*c = `Bibelen, nynorsk`

	// A translation sponsored by Tyndale House and first released in 1996. It is considered a revision and updating of the Living Bible
	case "NLV":
		*c = `New Living`

	// A revision of the Revised Standard based on ancient texts but updating language to American usage of the 1980s
	case "NRS":
		*c = `New Revised Standard`

	// A Spanish translation from the original Greek and Hebrew, sponsored by Tyndale House
	case "NTV":
		*c = `Nueva Traduccion Vivienta`

	// Nuovissima version – a Catholic-oriented translation in modern Italian, edited by a group including Carlo Martini, Gianfranco Ravasi and Ugo Vanni and first published (in 48 volumes, 1967-1980) by Edizioni San Paolo
	case "NVB":
		*c = `Novissima Versione della Bibbia`

	// A Spanish translation from the original Greek and Hebrew, sponsored by the International Bible Society/Sociedad Bíblica Internacional
	case "NVD":
		*c = `Nueva Biblia al Dia`

	// A Spanish translation underwritten by the International Bible Society
	case "NVI":
		*c = `Nueva Version Internacional`

	// An idiomatic translation by J B Phillips, first completed in 1966
	case "PHP":
		*c = `New Testament in Modern English (Phillips)`

	// A 1989 revision of the NEB. A significant effort was made to reduce the British flavor present in the NEB
	case "REB":
		*c = `Revised English`

	// The first major revision of the King James Version, the Revised Version incorporates insights from early manuscripts discovered between 1611 and 1870, and corrects readings in the KJV which nineteenth-century scholarship deemed mistaken. The New Testament was published in 1881, the Old Testament in 1885, and the Apocrypha in 1895
	case "REV":
		*c = `Revised Version`

	// A translation authorized by the National Council of Churches of Christ in the USA. The New Testament was published in 1946 followed by a complete Protestant canon in 1951
	case "RSV":
		*c = `Revised Standard`

	// A Spanish translation based on the original texts
	case "RVL":
		*c = `Reina Valera`

	// Swedish Bible translation
	case "SBB":
		*c = `Bibel 2000`

	// Norwegian ‘samisk’ Bible translation
	case "SMK":
		*c = `Bibelen, samisk`

	// A translation of the New Testament sponsored by the American Bible Society and first published in 1966. It was incorporated into the ‘Good News Bible’ (GNB) in 1976
	case "TEV":
		*c = `Today’s English`

	// An updating of the New International Version. The New Testament was published in 2002, and the entire Bible in 2005. Superseded by the 2011 NIV update
	case "TNI":
		*c = `Today’s New International`

	// Other translations not otherwise noted
	case "ZZZ":
		*c = `Other`
	default:
		return fmt.Errorf("undefined code for BibleVersion has been passed, got [%s]", v)
	}
	return nil
}

// StudyBibleType List 84: Study Bible type
type StudyBibleType string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *StudyBibleType) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// Contains the work of Howard Clark Kee including a summary of the development of the canon, introductions to the books, notes and cross references. Originally published in 1993, NRSV
	case "CAM":
		*c = `Cambridge Annotated`

	// A project of Tyndale House Publishers and Zondervan intended to help readers apply the Bible to daily living. Living Bible, King James, New International, NASB
	case "LIF":
		*c = `Life Application`

	// A King James version study Bible with notes by James Macarthur first published in 1997
	case "MAC":
		*c = `Macarthur`

	// A study Bible originally published in the 1960s and based on the RSV / NRSV
	case "OXF":
		*c = `Oxford Annotated`

	// Norwegian study Bible, New Testament
	case "NNT":
		*c = `Studiebibel, Det Nye testamentet`

	// Published in 1991 and based on the New Revised Standard version
	case "NOX":
		*c = `New Oxford Annotated`

	// Norwegian study Bible
	case "NSB":
		*c = `Norsk studiebibel`

	// Based on the work of Charles C. Ryrie. King James, NI, NASB
	case "RYR":
		*c = `Ryrie`

	// A study Bible based on the early 20th century work of C.I. Scofield. Based on the King James version
	case "SCO":
		*c = `Scofield`

	// A transdenominational study Bible for persons from the Pentecostal/Charismatic traditions
	case "SPR":
		*c = `Spirit Filled`
	default:
		return fmt.Errorf("undefined code for StudyBibleType has been passed, got [%s]", v)
	}
	return nil
}

// BiblePurpose List 85: Bible purpose
type BiblePurpose string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *BiblePurpose) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// A Bible (or selected Biblical text) designed for presentation from a religious organization
	case "AW":
		*c = `Award`

	// A Bible (or selected Biblical text) designed to be a gift to commemorate a child’s birth
	case "BB":
		*c = `Baby`

	// A special gift Bible (or selected Biblical text) designed for the bride on her wedding day. Usually white
	case "BR":
		*c = `Bride`

	// A Bible (or selected Biblical text) designed to be used in the confirmation reading or as a gift to a confirmand
	case "CF":
		*c = `Confirmation`

	// A text Bible (or selected Biblical text) designed in presentation and readability for a child
	case "CH":
		*c = `Children’s`

	// A small Bible (or selected Biblical text) with a trim height of five inches or less
	case "CM":
		*c = `Compact`

	// A Bible (or selected Biblical text) which includes text conveying cross-references to related scripture passages
	case "CR":
		*c = `Cross-reference`

	// A Bible (or selected Biblical text) laid out to provide readings for each day of the year
	case "DR":
		*c = `Daily readings`

	// A Bible (or selected Biblical text) containing devotional content together with the scripture
	case "DV":
		*c = `Devotional`

	// A Bible (or selected Biblical text) containing family record pages and/or additional study material for family devotion
	case "FM":
		*c = `Family`

	// A standard Bible (or selected Biblical text) of any version with no distinguishing characteristics beyond the canonical text
	case "GT":
		*c = `General/Text`

	// A Bible (or selected Biblical text) designed for gift or presentation, often including a presentation page
	case "GF":
		*c = `Gift`

	// A large Bible (or selected Biblical text) with large print designed for use in reading scriptures in public worship from either the pulpit or lectern
	case "LP":
		*c = `Lectern/Pulpit`

	// A Bible (or selected Biblical text) especially designed with helps and study guides oriented to the adult male
	case "MN":
		*c = `Men’s`

	// A Bible (or selected Biblical text) designed for use in primary school
	case "PS":
		*c = `Primary school`

	// Usually inexpensive but sturdy, a Bible (or selected Biblical text) designed for use in church pews
	case "PW":
		*c = `Pew`

	// A Bible (or selected Biblical text) including texts in Greek and/or Hebrew and designed for scholarly study
	case "SC":
		*c = `Scholarly`

	//
	case "SL":
		*c = `Slimline`

	// A Bible (or selected Biblical text) with study articles and helps especially for use in the classroom
	case "ST":
		*c = `Student`

	// A Bible (or selected Biblical text) with many extra features, e.g. book introductions, dictionary, concordance, references, maps, etc., to help readers better understand the scripture
	case "SU":
		*c = `Study`

	// A special gift Bible (or selected Biblical text) designed as a gift to the couple on their wedding day
	case "WG":
		*c = `Wedding gift`

	// A devotional or study Bible (or selected Biblical text) with helps targeted at the adult woman
	case "WM":
		*c = `Women’s`

	// A Bible (or selected Biblical text) containing special study and devotional helps designed specifically for the needs of teenagers
	case "YT":
		*c = `Youth`
	default:
		return fmt.Errorf("undefined code for BiblePurpose has been passed, got [%s]", v)
	}
	return nil
}

// BibleTextOrganization List 86: Bible text organization
type BibleTextOrganization string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *BibleTextOrganization) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// A Bible with the text organized in the order in which events are believed to have happened
	case "CHR":
		*c = `Chronological`

	// A Bible which explores keywords or themes by referring text to preceding or following text
	case "CHA":
		*c = `Chain reference`

	// A Bible or other text in which different versions are printed one line above the other, so that the variations can easily be detected
	case "INT":
		*c = `Interlinear`

	// A Bible with two or more versions printed side by side
	case "PAR":
		*c = `Parallel`

	// A Bible in which the text is presented in the traditional order
	case "STN":
		*c = `Standard`
	default:
		return fmt.Errorf("undefined code for BibleTextOrganization has been passed, got [%s]", v)
	}
	return nil
}

// BibleReferenceLocation List 87: Bible reference location
type BibleReferenceLocation string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *BibleReferenceLocation) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// References are printed in a narrow column in the center of the page between two columns of text
	case "CCL":
		*c = `Center column`

	// References are printed at the foot of the page
	case "PGE":
		*c = `Page end`

	// References are printed in a column to the side of the scripture
	case "SID":
		*c = `Side column`

	// References are printed at the end of the applicable verse
	case "VER":
		*c = `Verse end`

	// The person creating the ONIX record does not know where the references are located
	case "UNK":
		*c = `Unknown`

	// Other locations not otherwise identified
	case "ZZZ":
		*c = `Other`
	default:
		return fmt.Errorf("undefined code for BibleReferenceLocation has been passed, got [%s]", v)
	}
	return nil
}

// ReligiousTextIdentifier List 88: Religious text identifier
type ReligiousTextIdentifier string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *ReligiousTextIdentifier) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {
	default:
		return fmt.Errorf("undefined code for ReligiousTextIdentifier has been passed, got [%s]", v)
	}
	return nil
}

// ReligiousTextFeatureType List 89: Religious text feature type
type ReligiousTextFeatureType string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *ReligiousTextFeatureType) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// A church season or activity for which a religious text is intended. Religious text feature code must be taken from List 90
	case "01":
		*c = `Church season or activity`
	default:
		return fmt.Errorf("undefined code for ReligiousTextFeatureType has been passed, got [%s]", v)
	}
	return nil
}

// ProductClassificationTypeCode List 9: Product classification type code
type ProductClassificationTypeCode string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *ProductClassificationTypeCode) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// World Customs Organization Harmonized Commodity Coding and Description System. Use 6 or 8 digits, without punctuation
	case "01":
		*c = `WCO Harmonized System`

	// UN Standard Product and Service Classification
	case "02":
		*c = `UNSPSC`

	// UK Revenue and Customs classifications, based on the Harmonized System
	case "03":
		*c = `HMRC`

	// German export trade classification, based on the Harmonised System
	case "04":
		*c = `Warenverzeichnis für die Außenhandelsstatistik`

	// EU TARIC codes, an extended version of the Harmonized System
	case "05":
		*c = `TARIC`

	// Centraal Boekhuis free classification field for publishers
	case "06":
		*c = `Fondsgroep`

	// A product category (not a subject classification) assigned by the sender
	case "07":
		*c = `Sender’s product category`

	// Product classification maintained by the Chinese General Administration of Press and Publication (http://www.gapp.gov.cn)
	case "08":
		*c = `GAPP Product Class`

	// Statistical Classification of Products by Activity in the European Economic Community, see http://ec.europa.eu/eurostat/ramon/nomenclatures/index.cfm?TargetUrl=LST_NOM_DTL&StrNom=CPA_2008. Up to six digits, with one or two periods. For example, printed children’s books are ‘58.11.13’, but the periods are normally ommited in ONIX
	case "09":
		*c = `CPA`

	// Mercosur/Mercosul Common Nomenclature, based on the Harmonised System
	case "10":
		*c = `NCM`

	// Common Procurement Vocabulary, uses to describe requirements for tender for public tendering and procurement within the EU. Code is a nine digit number (including the check digit). See http://eur-lex.europa.eu/legal-content/EN/TXT/?uri=URISERV:l22008
	case "11":
		*c = `CPV`

	// Typologie de marché géré par Electre (Market segment code maintained by Electre)
	case "50":
		*c = `Electre genre`
	default:
		return fmt.Errorf("undefined code for ProductClassificationTypeCode has been passed, got [%s]", v)
	}
	return nil
}

// ReligiousTextFeatureCode List 90: Religious text feature code
type ReligiousTextFeatureCode string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *ReligiousTextFeatureCode) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// Use with code 01 in <ReligiousTextFeatureType>
	case "01":
		*c = `Academic year`

	// Use with code 01 in <ReligiousTextFeatureType>
	case "02":
		*c = `Catechistic year`

	// Use with code 01 in <ReligiousTextFeatureType>
	case "03":
		*c = `Liturgical year`

	// Use with code 01 in <ReligiousTextFeatureType>
	case "04":
		*c = `Advent and Christmas`

	// Use with code 01 in <ReligiousTextFeatureType>
	case "05":
		*c = `Blessings`

	// Use with code 01 in <ReligiousTextFeatureType>
	case "06":
		*c = `Scholastic cycles`

	// Use with code 01 in <ReligiousTextFeatureType>
	case "07":
		*c = `Confirmation and Holy Communion`

	// For example, summer camps and other youth recreational activities: use with code 01 in <ReligiousTextFeatureType>
	case "08":
		*c = `Summer activites`

	// Use with code 01 in <ReligiousTextFeatureType>
	case "09":
		*c = `Easter`

	// Use with code 01 in <ReligiousTextFeatureType>
	case "10":
		*c = `Lent`

	// Use with code 01 in <ReligiousTextFeatureType>
	case "11":
		*c = `Marian themes`
	default:
		return fmt.Errorf("undefined code for ReligiousTextFeatureCode has been passed, got [%s]", v)
	}
	return nil
}

// CountryCodeIso31661 List 91: Country code – ISO 3166-1
type CountryCodeIso31661 string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *CountryCodeIso31661) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	//
	case "AD":
		*c = `Andorra`

	//
	case "AE":
		*c = `United Arab Emirates`

	//
	case "AF":
		*c = `Afghanistan`

	//
	case "AG":
		*c = `Antigua and Barbuda`

	//
	case "AI":
		*c = `Anguilla`

	//
	case "AL":
		*c = `Albania`

	//
	case "AM":
		*c = `Armenia`

	// Deprecated – use BQ, CW or SX as appropriate
	case "AN":
		*c = `Netherlands Antilles`

	//
	case "AO":
		*c = `Angola`

	//
	case "AQ":
		*c = `Antarctica`

	//
	case "AR":
		*c = `Argentina`

	//
	case "AS":
		*c = `American Samoa`

	//
	case "AT":
		*c = `Austria`

	//
	case "AU":
		*c = `Australia`

	//
	case "AW":
		*c = `Aruba`

	//
	case "AX":
		*c = `Åland Islands`

	//
	case "AZ":
		*c = `Azerbaijan`

	//
	case "BA":
		*c = `Bosnia and Herzegovina`

	//
	case "BB":
		*c = `Barbados`

	//
	case "BD":
		*c = `Bangladesh`

	//
	case "BE":
		*c = `Belgium`

	//
	case "BF":
		*c = `Burkina Faso`

	//
	case "BG":
		*c = `Bulgaria`

	//
	case "BH":
		*c = `Bahrain`

	//
	case "BI":
		*c = `Burundi`

	//
	case "BJ":
		*c = `Benin`

	//
	case "BL":
		*c = `Saint Barthélemy`

	//
	case "BM":
		*c = `Bermuda`

	//
	case "BN":
		*c = `Brunei Darussalam`

	//
	case "BO":
		*c = `Bolivia, Plurinational State of`

	//
	case "BQ":
		*c = `Bonaire, Sint Eustatius and Saba`

	//
	case "BR":
		*c = `Brazil`

	//
	case "BS":
		*c = `Bahamas`

	//
	case "BT":
		*c = `Bhutan`

	//
	case "BV":
		*c = `Bouvet Island`

	//
	case "BW":
		*c = `Botswana`

	//
	case "BY":
		*c = `Belarus`

	//
	case "BZ":
		*c = `Belize`

	//
	case "CA":
		*c = `Canada`

	//
	case "CC":
		*c = `Cocos (Keeling) Islands`

	//
	case "CD":
		*c = `Congo, Democratic Republic of the`

	//
	case "CF":
		*c = `Central African Republic`

	//
	case "CG":
		*c = `Congo`

	//
	case "CH":
		*c = `Switzerland`

	//
	case "CI":
		*c = `Cote d’Ivoire`

	//
	case "CK":
		*c = `Cook Islands`

	//
	case "CL":
		*c = `Chile`

	//
	case "CM":
		*c = `Cameroon`

	//
	case "CN":
		*c = `China`

	//
	case "CO":
		*c = `Colombia`

	//
	case "CR":
		*c = `Costa Rica`

	// DEPRECATED, replaced by ME – Montenegro and RS – Serbia
	case "CS":
		*c = `Serbia and Montenegro`

	//
	case "CU":
		*c = `Cuba`

	//
	case "CV":
		*c = `Cabo Verde`

	//
	case "CW":
		*c = `Curaçao`

	//
	case "CX":
		*c = `Christmas Island`

	//
	case "CY":
		*c = `Cyprus`

	//
	case "CZ":
		*c = `Czech Republic`

	//
	case "DE":
		*c = `Germany`

	//
	case "DJ":
		*c = `Djibouti`

	//
	case "DK":
		*c = `Denmark`

	//
	case "DM":
		*c = `Dominica`

	//
	case "DO":
		*c = `Dominican Republic`

	//
	case "DZ":
		*c = `Algeria`

	//
	case "EC":
		*c = `Ecuador`

	//
	case "EE":
		*c = `Estonia`

	//
	case "EG":
		*c = `Egypt`

	//
	case "EH":
		*c = `Western Sahara`

	//
	case "ER":
		*c = `Eritrea`

	//
	case "ES":
		*c = `Spain`

	//
	case "ET":
		*c = `Ethiopia`

	//
	case "FI":
		*c = `Finland`

	//
	case "FJ":
		*c = `Fiji`

	//
	case "FK":
		*c = `Falkland Islands (Malvinas)`

	//
	case "FM":
		*c = `Micronesia, Federated States of`

	//
	case "FO":
		*c = `Faroe Islands`

	//
	case "FR":
		*c = `France`

	//
	case "GA":
		*c = `Gabon`

	//
	case "GB":
		*c = `United Kingdom`

	//
	case "GD":
		*c = `Grenada`

	//
	case "GE":
		*c = `Georgia`

	//
	case "GF":
		*c = `French Guiana`

	//
	case "GG":
		*c = `Guernsey`

	//
	case "GH":
		*c = `Ghana`

	//
	case "GI":
		*c = `Gibraltar`

	//
	case "GL":
		*c = `Greenland`

	//
	case "GM":
		*c = `Gambia`

	//
	case "GN":
		*c = `Guinea`

	//
	case "GP":
		*c = `Guadeloupe`

	//
	case "GQ":
		*c = `Equatorial Guinea`

	//
	case "GR":
		*c = `Greece`

	//
	case "GS":
		*c = `South Georgia and the South Sandwich Islands`

	//
	case "GT":
		*c = `Guatemala`

	//
	case "GU":
		*c = `Guam`

	//
	case "GW":
		*c = `Guinea-Bissau`

	//
	case "GY":
		*c = `Guyana`

	//
	case "HK":
		*c = `Hong Kong`

	//
	case "HM":
		*c = `Heard Island and McDonald Islands`

	//
	case "HN":
		*c = `Honduras`

	//
	case "HR":
		*c = `Croatia`

	//
	case "HT":
		*c = `Haiti`

	//
	case "HU":
		*c = `Hungary`

	//
	case "ID":
		*c = `Indonesia`

	//
	case "IE":
		*c = `Ireland`

	//
	case "IL":
		*c = `Israel`

	//
	case "IM":
		*c = `Isle of Man`

	//
	case "IN":
		*c = `India`

	//
	case "IO":
		*c = `British Indian Ocean Territory`

	//
	case "IQ":
		*c = `Iraq`

	//
	case "IR":
		*c = `Iran, Islamic Republic of`

	//
	case "IS":
		*c = `Iceland`

	//
	case "IT":
		*c = `Italy`

	//
	case "JE":
		*c = `Jersey`

	//
	case "JM":
		*c = `Jamaica`

	//
	case "JO":
		*c = `Jordan`

	//
	case "JP":
		*c = `Japan`

	//
	case "KE":
		*c = `Kenya`

	//
	case "KG":
		*c = `Kyrgyzstan`

	//
	case "KH":
		*c = `Cambodia`

	//
	case "KI":
		*c = `Kiribati`

	//
	case "KM":
		*c = `Comoros`

	//
	case "KN":
		*c = `Saint Kitts and Nevis`

	//
	case "KP":
		*c = `Korea, Democratic People’s Republic of`

	//
	case "KR":
		*c = `Korea, Republic of`

	//
	case "KW":
		*c = `Kuwait`

	//
	case "KY":
		*c = `Cayman Islands`

	//
	case "KZ":
		*c = `Kazakhstan`

	//
	case "LA":
		*c = `Lao People’s Democratic Republic`

	//
	case "LB":
		*c = `Lebanon`

	//
	case "LC":
		*c = `Saint Lucia`

	//
	case "LI":
		*c = `Liechtenstein`

	//
	case "LK":
		*c = `Sri Lanka`

	//
	case "LR":
		*c = `Liberia`

	//
	case "LS":
		*c = `Lesotho`

	//
	case "LT":
		*c = `Lithuania`

	//
	case "LU":
		*c = `Luxembourg`

	//
	case "LV":
		*c = `Latvia`

	//
	case "LY":
		*c = `Libya`

	//
	case "MA":
		*c = `Morocco`

	//
	case "MC":
		*c = `Monaco`

	//
	case "MD":
		*c = `Moldova, Repubic of`

	//
	case "ME":
		*c = `Montenegro`

	//
	case "MF":
		*c = `Saint Martin (French part)`

	//
	case "MG":
		*c = `Madagascar`

	//
	case "MH":
		*c = `Marshall Islands`

	//
	case "MK":
		*c = `Macedonia, the former Yugoslav Republic of`

	//
	case "ML":
		*c = `Mali`

	//
	case "MM":
		*c = `Myanmar`

	//
	case "MN":
		*c = `Mongolia`

	//
	case "MO":
		*c = `Macao`

	//
	case "MP":
		*c = `Northern Mariana Islands`

	//
	case "MQ":
		*c = `Martinique`

	//
	case "MR":
		*c = `Mauritania`

	//
	case "MS":
		*c = `Montserrat`

	//
	case "MT":
		*c = `Malta`

	//
	case "MU":
		*c = `Mauritius`

	//
	case "MV":
		*c = `Maldives`

	//
	case "MW":
		*c = `Malawi`

	//
	case "MX":
		*c = `Mexico`

	//
	case "MY":
		*c = `Malaysia`

	//
	case "MZ":
		*c = `Mozambique`

	//
	case "NA":
		*c = `Namibia`

	//
	case "NC":
		*c = `New Caledonia`

	//
	case "NE":
		*c = `Niger`

	//
	case "NF":
		*c = `Norfolk Island`

	//
	case "NG":
		*c = `Nigeria`

	//
	case "NI":
		*c = `Nicaragua`

	//
	case "NL":
		*c = `Netherlands`

	//
	case "NO":
		*c = `Norway`

	//
	case "NP":
		*c = `Nepal`

	//
	case "NR":
		*c = `Nauru`

	//
	case "NU":
		*c = `Niue`

	//
	case "NZ":
		*c = `New Zealand`

	//
	case "OM":
		*c = `Oman`

	//
	case "PA":
		*c = `Panama`

	//
	case "PE":
		*c = `Peru`

	//
	case "PF":
		*c = `French Polynesia`

	//
	case "PG":
		*c = `Papua New Guinea`

	//
	case "PH":
		*c = `Philippines`

	//
	case "PK":
		*c = `Pakistan`

	//
	case "PL":
		*c = `Poland`

	//
	case "PM":
		*c = `Saint Pierre and Miquelon`

	//
	case "PN":
		*c = `Pitcairn`

	//
	case "PR":
		*c = `Puerto Rico`

	//
	case "PS":
		*c = `Palestine, State of`

	//
	case "PT":
		*c = `Portugal`

	//
	case "PW":
		*c = `Palau`

	//
	case "PY":
		*c = `Paraguay`

	//
	case "QA":
		*c = `Qatar`

	//
	case "RE":
		*c = `Réunion`

	//
	case "RO":
		*c = `Romania`

	//
	case "RS":
		*c = `Serbia`

	//
	case "RU":
		*c = `Russian Federation`

	//
	case "RW":
		*c = `Rwanda`

	//
	case "SA":
		*c = `Saudi Arabia`

	//
	case "SB":
		*c = `Solomon Islands`

	//
	case "SC":
		*c = `Seychelles`

	//
	case "SD":
		*c = `Sudan`

	//
	case "SE":
		*c = `Sweden`

	//
	case "SG":
		*c = `Singapore`

	//
	case "SH":
		*c = `Saint Helena, Ascension and Tristan da Cunha`

	//
	case "SI":
		*c = `Slovenia`

	//
	case "SJ":
		*c = `Svalbard and Jan Mayen`

	//
	case "SK":
		*c = `Slovakia`

	//
	case "SL":
		*c = `Sierra Leone`

	//
	case "SM":
		*c = `San Marino`

	//
	case "SN":
		*c = `Senegal`

	//
	case "SO":
		*c = `Somalia`

	//
	case "SR":
		*c = `Suriname`

	//
	case "SS":
		*c = `South Sudan`

	//
	case "ST":
		*c = `Sao Tome and Principe`

	//
	case "SV":
		*c = `El Salvador`

	//
	case "SX":
		*c = `Sint Maarten (Dutch part)`

	//
	case "SY":
		*c = `Syrian Arab Republic`

	//
	case "SZ":
		*c = `Swaziland`

	//
	case "TC":
		*c = `Turks and Caicos Islands`

	//
	case "TD":
		*c = `Chad`

	//
	case "TF":
		*c = `French Southern Territories`

	//
	case "TG":
		*c = `Togo`

	//
	case "TH":
		*c = `Thailand`

	//
	case "TJ":
		*c = `Tajikistan`

	//
	case "TK":
		*c = `Tokelau`

	//
	case "TL":
		*c = `Timor-Leste`

	//
	case "TM":
		*c = `Turkmenistan`

	//
	case "TN":
		*c = `Tunisia`

	//
	case "TO":
		*c = `Tonga`

	//
	case "TR":
		*c = `Turkey`

	//
	case "TT":
		*c = `Trinidad and Tobago`

	//
	case "TV":
		*c = `Tuvalu`

	//
	case "TW":
		*c = `Taiwan, Province of China`

	//
	case "TZ":
		*c = `Tanzania, United Republic of`

	//
	case "UA":
		*c = `Ukraine`

	//
	case "UG":
		*c = `Uganda`

	//
	case "UM":
		*c = `United States Minor Outlying Islands`

	//
	case "US":
		*c = `United States`

	//
	case "UY":
		*c = `Uruguay`

	//
	case "UZ":
		*c = `Uzbekistan`

	//
	case "VA":
		*c = `Holy See (Vatican City State)`

	//
	case "VC":
		*c = `Saint Vincent and the Grenadines`

	//
	case "VE":
		*c = `Venezuela, Bolivarian Republic of`

	//
	case "VG":
		*c = `Virgin Islands, British`

	//
	case "VI":
		*c = `Virgin Islands, US`

	//
	case "VN":
		*c = `Viet Nam`

	//
	case "VU":
		*c = `Vanuatu`

	//
	case "WF":
		*c = `Wallis and Futuna`

	//
	case "WS":
		*c = `Samoa`

	//
	case "YE":
		*c = `Yemen`

	//
	case "YT":
		*c = `Mayotte`

	// DEPRECATED, replaced by ME – Montenegro and RS – Serbia
	case "YU":
		*c = `Yugoslavia`

	//
	case "ZA":
		*c = `South Africa`

	//
	case "ZM":
		*c = `Zambia`

	//
	case "ZW":
		*c = `Zimbabwe`
	default:
		return fmt.Errorf("undefined code for CountryCodeIso31661 has been passed, got [%s]", v)
	}
	return nil
}

// SupplierIdentifierType List 92: Supplier identifier type
type SupplierIdentifierType string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *SupplierIdentifierType) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// Note that <IDTypeName> is required with proprietary identifiers
	case "01":
		*c = `Proprietary`

	// DEPRECATED – use 01
	case "02":
		*c = `Proprietary`

	//
	case "04":
		*c = `Börsenverein Verkehrsnummer`

	//
	case "05":
		*c = `German ISBN Agency publisher identifier`

	// GS1 global location number (formerly EAN location number)
	case "06":
		*c = `GLN`

	// Book trade Standard Address Number – US, UK etc
	case "07":
		*c = `SAN`

	// Flemish supplier code
	case "12":
		*c = `Distributeurscode Boekenbank`

	// Flemish publisher code
	case "13":
		*c = `Fondscode Boekenbank`

	// Identifier for a business organization for VAT purposes, eg within the EU’s VIES system. See http://ec.europa.eu/taxation_customs/vies/faqvies.do for EU VAT ID formats, which vary from country to country. Generally these consist of a two-letter country code followed by the 8–12 digits of the national VAT ID. Some countries include one or two letters within their VAT ID. See http://en.wikipedia.org/wiki/VAT_identification_number for non-EU countries that maintain similar identifiers. Spaces, dashes etc should be omitted
	case "23":
		*c = `VAT Identity Number`
	default:
		return fmt.Errorf("undefined code for SupplierIdentifierType has been passed, got [%s]", v)
	}
	return nil
}

// SupplierRole List 93: Supplier role
type SupplierRole string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *SupplierRole) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// Default
	case "00":
		*c = `Unspecified`

	// Publisher as supplier to retail trade outlets
	case "01":
		*c = `Publisher to retailers`

	//
	case "02":
		*c = `Publisher’s exclusive distributor to retailers`

	//
	case "03":
		*c = `Publisher’s non-exclusive distributor to retailers`

	// Wholesaler supplying retail trade outlets
	case "04":
		*c = `Wholesaler`

	// DEPRECATED – use <MarketRepresentation> (ONIX 2.1) or <MarketPublishingDetail> (ONIX 3.0) to specify a sales agent
	case "05":
		*c = `Sales agent`

	// In a specified supply territory. Use only where exclusive/non-exclusive status is not known. Prefer 02 or 03 as appropriate, where possible
	case "06":
		*c = `Publisher’s distributor to retailers`

	// Where a POD product is supplied to retailers and/or consumers direct from a POD source
	case "07":
		*c = `POD supplier`

	//
	case "08":
		*c = `Retailer`

	// Publisher as supplier direct to consumers and/or institutional customers
	case "09":
		*c = `Publisher to end-customers`

	// Intermediary as exclusive distributor direct to consumers and/or institutional customers
	case "10":
		*c = `Exclusive distributor to end-customers`

	// Intermediary as non-exclusive distributor direct to consumers and/or institutional customers
	case "11":
		*c = `Non-exclusive distributor to end-customers`

	// Use only where exclusive/non-exclusive status is not known. Prefer 10 or 11 as appropriate, where possible
	case "12":
		*c = `Distributor to end-customers`
	default:
		return fmt.Errorf("undefined code for SupplierRole has been passed, got [%s]", v)
	}
	return nil
}

// DefaultLinearUnit List 94: Default linear unit
type DefaultLinearUnit string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *DefaultLinearUnit) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// Millimeters are the preferred metric unit of length
	case "cm":
		*c = `Centimeters`

	//
	case "in":
		*c = `Inches (US)`

	//
	case "mm":
		*c = `Millimeters`
	default:
		return fmt.Errorf("undefined code for DefaultLinearUnit has been passed, got [%s]", v)
	}
	return nil
}

// DefaultUnitOfWeight List 95: Default unit of weight
type DefaultUnitOfWeight string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *DefaultUnitOfWeight) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	//
	case "lb":
		*c = `Pounds (US)`

	//
	case "gr":
		*c = `Grams`

	//
	case "oz":
		*c = `Ounces (US)`
	default:
		return fmt.Errorf("undefined code for DefaultUnitOfWeight has been passed, got [%s]", v)
	}
	return nil
}

// CurrencyCodeIso4217 List 96: Currency code – ISO 4217
type CurrencyCodeIso4217 string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *CurrencyCodeIso4217) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// United Arab Emirates
	case "AED":
		*c = `UAE Dirham`

	// Afghanistan. DEPRECATED, replaced by AFN
	case "AFA":
		*c = `Afghani`

	// Afghanistan (prices normally quoted as integers)
	case "AFN":
		*c = `Afghani`

	// Albania (prices normally quoted as integers)
	case "ALL":
		*c = `Lek`

	// Armenia (prices normally quoted as integers)
	case "AMD":
		*c = `Armenian Dram`

	// Curaçao, Sint Maarten
	case "ANG":
		*c = `Netherlands Antillian Guilder`

	// Angola
	case "AOA":
		*c = `Kwanza`

	// Argentina
	case "ARS":
		*c = `Argentine Peso`

	// Austria. Now replaced by the Euro (EUR): use only for historical prices that pre-date the introduction of the Euro
	case "ATS":
		*c = `Schilling`

	// Australia, Christmas Island, Cocos (Keeling) Islands, Heard Island and McDonald Islands, Kiribati, Nauru, Norfolk Island, Tuvalu
	case "AUD":
		*c = `Australian Dollar`

	// Aruba
	case "AWG":
		*c = `Aruban Florin`

	// Azerbaijan
	case "AZN":
		*c = `Azerbaijanian Manat`

	// Bosnia and Herzegovina
	case "BAM":
		*c = `Convertible Marks`

	// Barbados
	case "BBD":
		*c = `Barbados Dollar`

	// Bangladesh
	case "BDT":
		*c = `Taka`

	// Belgium. Now replaced by the Euro (EUR): use only for historical prices that pre-date the introduction of the Euro
	case "BEF":
		*c = `Belgian Franc`

	// DEPRECATED, replaced by BGN
	case "BGL":
		*c = `Bulgarian Lev`

	// Bulgaria
	case "BGN":
		*c = `Bulgarian Lev`

	// Bahrain (prices normally quoted with 3 decimal places)
	case "BHD":
		*c = `Bahraini Dinar`

	// Burundi (prices normally quoted as integers)
	case "BIF":
		*c = `Burundi Franc`

	// Bermuda
	case "BMD":
		*c = `Bermudian Dollar`

	// Brunei Darussalam
	case "BND":
		*c = `Brunei Dollar`

	// Bolivia
	case "BOB":
		*c = `Boliviano`

	// Brazil
	case "BRL":
		*c = `Brazilian Real`

	// Bahamas
	case "BSD":
		*c = `Bahamian Dollar`

	// Bhutan
	case "BTN":
		*c = `Ngultrun`

	// Botswana
	case "BWP":
		*c = `Pula`

	// Belarus (prices normally quoted as integers). Now replaced by new Belarussian Ruble (BYN): use only for historical prices that pre-date the introduction of the new Belarussian Ruble
	case "BYR":
		*c = `Belarussian Ruble`

	// Belarus
	case "BYN":
		*c = `Belarussian Ruble`

	// Belize
	case "BZD":
		*c = `Belize Dollar`

	// Canada
	case "CAD":
		*c = `Canadian Dollar`

	// Congo (Democratic Republic of the)
	case "CDF":
		*c = `Franc Congolais`

	// Switzerland, Liechtenstein
	case "CHF":
		*c = `Swiss Franc`

	// Chile (prices normally quoted as integers)
	case "CLP":
		*c = `Chilean Peso`

	// China
	case "CNY":
		*c = `Yuan Renminbi`

	// Colombia (prices normally quoted as integers)
	case "COP":
		*c = `Colombian Peso`

	// Costa Rica (prices normally quoted as integers)
	case "CRC":
		*c = `Costa Rican Colon`

	// Deprecated, replaced by RSD
	case "CSD":
		*c = `Serbian Dinar`

	// Cuba (alternative currency)
	case "CUC":
		*c = `Cuban Convertible Peso`

	// Cuba
	case "CUP":
		*c = `Cuban Peso`

	// Cabo Verde (prices normally quoted as integers)
	case "CVE":
		*c = `Cabo Verde Escudo`

	// Cyprus. Now replaced by the Euro (EUR): use only for historical prices that pre-date the introduction of the Euro
	case "CYP":
		*c = `Cyprus Pound`

	// Czech Republic
	case "CZK":
		*c = `Czech Koruna`

	// Germany. Now replaced by the Euro (EUR): use only for historical prices that pre-date the introduction of the Euro
	case "DEM":
		*c = `Mark`

	// Djibouti (prices normally quoted as integers)
	case "DJF":
		*c = `Djibouti Franc`

	// Denmark, Faroe Islands, Greenland
	case "DKK":
		*c = `Danish Krone`

	// Dominican Republic
	case "DOP":
		*c = `Dominican Peso`

	// Algeria
	case "DZD":
		*c = `Algerian Dinar`

	// Estonia.Now replaced by the Euro (EUR): use only for historical prices that pre-date the introduction of the Euro
	case "EEK":
		*c = `Kroon`

	// Egypt
	case "EGP":
		*c = `Egyptian Pound`

	// Eritrea
	case "ERN":
		*c = `Nakfa`

	// Spain. Now replaced by the Euro (EUR): use only for historical prices that pre-date the introduction of the Euro (prices normally quoted as integers)
	case "ESP":
		*c = `Peseta`

	// Ethiopia
	case "ETB":
		*c = `Ethiopian Birr`

	// Eurozone: Andorra, Austria, Belgium, Cyprus, Estonia, Finland, France, Fr Guiana, Fr S Territories, Germany, Greece, Guadeloupe, Holy See (Vatican City), Ireland, Italy, Latvia, Lithuania, Luxembourg, Martinique, Malta, Mayotte, Monaco, Montenegro, Netherlands, Portugal, Réunion, St Barthelemy, St Martin, St Pierre and Miquelon, San Marino, Slovakia, Slovenia, Spain
	case "EUR":
		*c = `Euro`

	// Finland. Now replaced by the Euro (EUR): use only for historical prices that pre-date the introduction of the Euro
	case "FIM":
		*c = `Markka`

	// Fiji
	case "FJD":
		*c = `Fiji Dollar`

	// Falkland Islands (Malvinas)
	case "FKP":
		*c = `Falkland Islands Pound`

	// France. Now replaced by the Euro (EUR): use only for historical prices that pre-date the introduction of the Euro
	case "FRF":
		*c = `Franc`

	// United Kingdom, Isle of Man, Channel Islands, South Georgia, South Sandwich Islands
	case "GBP":
		*c = `Pound Sterling`

	// Georgia
	case "GEL":
		*c = `Lari`

	// Deprecated, replaced by GHS
	case "GHC":
		*c = `Ghana Cedi`

	// Ghana
	case "GHS":
		*c = `Ghana Cedi`

	// Gibraltar
	case "GIP":
		*c = `Gibraltar Pound`

	// Gambia
	case "GMD":
		*c = `Dalasi`

	// Guinea (prices normally quoted as integers)
	case "GNF":
		*c = `Guinea Franc`

	// Greece. Now replaced by the Euro (EUR): use only for historical prices that pre-date the introduction of the Euro
	case "GRD":
		*c = `Drachma`

	// Guatemala
	case "GTQ":
		*c = `Quetzal`

	// Now replaced by the CFA Franc BCEAO XOF use only for historical prices that pre-date use of the CFA Franc
	case "GWP":
		*c = `Guinea-Bissau Peso`

	// Guyana (prices normally quoted as integers)
	case "GYD":
		*c = `Guyana Dollar`

	// Hong Kong
	case "HKD":
		*c = `Hong Kong Dollar`

	// Honduras
	case "HNL":
		*c = `Lempira`

	// Croatia
	case "HRK":
		*c = `Kuna`

	// Haiti
	case "HTG":
		*c = `Gourde`

	// Hungary (prices normally quoted as integers)
	case "HUF":
		*c = `Forint`

	// Indonesia (prices normally quoted as integers)
	case "IDR":
		*c = `Rupiah`

	// Ireland. Now replaced by the Euro (EUR): use only for historical prices that pre-date the introduction of the Euro
	case "IEP":
		*c = `Punt`

	// Israel
	case "ILS":
		*c = `New Israeli Sheqel`

	// India, Bhutan (prices normally quoted as integers)
	case "INR":
		*c = `Indian Rupee`

	// Iraq (prices normally quoted as integers)
	case "IQD":
		*c = `Iraqi Dinar`

	// Iran (Islamic Republic of) (prices normally quoted as integers)
	case "IRR":
		*c = `Iranian Rial`

	// Iceland (prices normally quoted as integers)
	case "ISK":
		*c = `Iceland Krona`

	// italy. Now replaced by the Euro (EUR): use only for historical prices that pre-date the introduction of the Euro (prices normally quoted as integers)
	case "ITL":
		*c = `Lira`

	// Jamaica
	case "JMD":
		*c = `Jamaican Dollar`

	// Jordan (prices normally quoted with 3 decimal places)
	case "JOD":
		*c = `Jordanian Dinar`

	// Japan (prices normally quoted as integers)
	case "JPY":
		*c = `Yen`

	// Kenya
	case "KES":
		*c = `Kenyan Shilling`

	// Kyrgyzstan
	case "KGS":
		*c = `Som`

	// Cambodia
	case "KHR":
		*c = `Riel`

	// Comoros (prices normally quoted as integers)
	case "KMF":
		*c = `Comoro Franc`

	// Korea (Democratic People’s Republic of) (prices normally quoted as integers)
	case "KPW":
		*c = `North Korean Won`

	// Korea (Republic of) (prices normally quoted as integers)
	case "KRW":
		*c = `Won`

	// Kuwait (prices normally quoted with 3 decimal places)
	case "KWD":
		*c = `Kuwaiti Dinar`

	// Cayman Islands
	case "KYD":
		*c = `Cayman Islands Dollar`

	// Kazakstan
	case "KZT":
		*c = `Tenge`

	// Lao People’s Democratic Republic (prices normally quoted as integers)
	case "LAK":
		*c = `Kip`

	// Lebanon (prices normally quoted as integers)
	case "LBP":
		*c = `Lebanese Pound`

	// Sri Lanka
	case "LKR":
		*c = `Sri Lanka Rupee`

	// Liberia
	case "LRD":
		*c = `Liberian Dollar`

	// Lesotho
	case "LSL":
		*c = `Loti`

	// Lithuania. Now replaced by the Euro (EUR): use only for historical prices that pre-date the introduction of the Euro
	case "LTL":
		*c = `Litus`

	// Luxembourg. Now replaced by the Euro (EUR): use only for historical prices that pre-date the introduction of the Euro (prices normally quoted as integers)
	case "LUF":
		*c = `Luxembourg Franc`

	// Latvia. Now replaced by the Euro (EUR): use only for historical prices that pre-date the introduction of the Euro
	case "LVL":
		*c = `Latvian Lats`

	// Libyan Arab Jamahiriya (prices normally quoted with 3 decimal places)
	case "LYD":
		*c = `Libyan Dinar`

	// Morocco, Western Sahara
	case "MAD":
		*c = `Moroccan Dirham`

	// Moldova, Republic of
	case "MDL":
		*c = `Moldovan Leu`

	// Madagascar (prices normally quoted with 0 or 1 decimal place – 1 iraimbilanja = Ar0.2)
	case "MGA":
		*c = `Malagasy Ariary`

	// Now replaced by the Ariary (MGA) (prices normally quoted as integers)
	case "MGF":
		*c = `Malagasy Franc`

	// Macedonia (former Yugoslav Republic of)
	case "MKD":
		*c = `Denar`

	// Myanmar (prices normally quoted as integers)
	case "MMK":
		*c = `Kyat`

	// Mongolia (prices normally quoted as integers)
	case "MNT":
		*c = `Tugrik`

	// Macau
	case "MOP":
		*c = `Pataca`

	// Mauritania (0 or 1 – 1 khoums = UM0.2)
	case "MRO":
		*c = `Ouguiya`

	// Malta. Now replaced by the Euro (EUR): use only for historical prices that pre-date the introduction of the Euro
	case "MTL":
		*c = `Maltese Lira`

	// Mauritius (prices normally quoted as integers)
	case "MUR":
		*c = `Mauritius Rupee`

	// Maldives
	case "MVR":
		*c = `Rufiyaa`

	// Malawi
	case "MWK":
		*c = `Malawi Kwacha`

	// Mexico
	case "MXN":
		*c = `Mexican Peso`

	// Malaysia
	case "MYR":
		*c = `Malaysian Ringgit`

	// Mozambique
	case "MZN":
		*c = `Mozambique Metical`

	// Namibia
	case "NAD":
		*c = `Namibia Dollar`

	// Nigeria
	case "NGN":
		*c = `Naira`

	// Nicaragua
	case "NIO":
		*c = `Cordoba Oro`

	// Netherlands. Now replaced by the Euro (EUR): use only for historical prices that pre-date the introduction of the Euro
	case "NLG":
		*c = `Guilder`

	// Norway, Bouvet Island, Svalbard and Jan Mayen
	case "NOK":
		*c = `Norwegian Krone`

	// Nepal
	case "NPR":
		*c = `Nepalese Rupee`

	// New Zealand, Cook Islands, Niue, Pitcairn, Tokelau
	case "NZD":
		*c = `New Zealand Dollar`

	// Oman (prices normally quoted with 3 decimal places)
	case "OMR":
		*c = `Rial Omani`

	// Panama
	case "PAB":
		*c = `Balboa`

	// Peru (formerly Nuevo Sol)
	case "PEN":
		*c = `Sol`

	// Papua New Guinea
	case "PGK":
		*c = `Kina`

	// Philippines
	case "PHP":
		*c = `Philippine Peso`

	// Pakistan (prices normally quoted as integers)
	case "PKR":
		*c = `Pakistan Rupee`

	// Poland
	case "PLN":
		*c = `Zloty`

	// Portugal. Now replaced by the Euro (EUR): use only for historical prices that pre-date the introduction of the Euro
	case "PTE":
		*c = `Escudo`

	// Paraguay (prices normally quoted as integers)
	case "PYG":
		*c = `Guarani`

	// Qatar
	case "QAR":
		*c = `Qatari Rial`

	// Deprecated, replaced by RON
	case "ROL":
		*c = `Romanian Old Leu`

	// Romania
	case "RON":
		*c = `Romanian Leu`

	// Serbia (prices normally quoted as integers)
	case "RSD":
		*c = `Serbian Dinar`

	// Russian Federation
	case "RUB":
		*c = `Russian Ruble`

	// DEPRECATED, replaced by RUB
	case "RUR":
		*c = `Russian Ruble`

	// Rwanda (prices normally quoted as integers)
	case "RWF":
		*c = `Rwanda Franc`

	// Saudi Arabia
	case "SAR":
		*c = `Saudi Riyal`

	// Solomon Islands
	case "SBD":
		*c = `Solomon Islands Dollar`

	// Seychelles
	case "SCR":
		*c = `Seychelles Rupee`

	// Now replaced by the Sudanese Pound (SDG)
	case "SDD":
		*c = `Sudanese Dinar`

	// Sudan
	case "SDG":
		*c = `Sudanese Pound`

	// Sweden
	case "SEK":
		*c = `Swedish Krona`

	// Singapore
	case "SGD":
		*c = `Singapore Dollar`

	// Saint Helena
	case "SHP":
		*c = `Saint Helena Pound`

	// Slovenia. Now replaced by the Euro (EUR): use only for historical prices that pre-date the introduction of the Euro
	case "SIT":
		*c = `Tolar`

	// Slovakia. Now replaced by the Euro (EUR): use only for historical prices that pre-date the introduction of the Euro
	case "SKK":
		*c = `Slovak Koruna`

	// Sierra Leone (prices normally quoted as integers)
	case "SLL":
		*c = `Leone`

	// Somalia (prices normally quoted as integers)
	case "SOS":
		*c = `Somali Shilling`

	// Suriname
	case "SRD":
		*c = `Surinam Dollar`

	// DEPRECATED, replaced by SRD
	case "SRG":
		*c = `Suriname Guilder`

	// São Tome and Principe (prices normally quoted as integers)
	case "STD":
		*c = `Dobra`

	// El Salvador
	case "SVC":
		*c = `El Salvador Colon`

	// Syrian Arab Republic (prices normally quoted as integers)
	case "SYP":
		*c = `Syrian Pound`

	// Swaziland
	case "SZL":
		*c = `Lilangeni`

	// Thailand
	case "THB":
		*c = `Baht`

	// Tajikistan
	case "TJS":
		*c = `Somoni`

	// Deprecated, replaced by TMT (prices normally quoted as integers)
	case "TMM":
		*c = `Turkmenistan Manat`

	// Turkmenistan
	case "TMT":
		*c = `Turkmenistan New Manat`

	// Tunisia (prices normally quoted with 3 decimal places)
	case "TND":
		*c = `Tunisian Dinar`

	// Tonga
	case "TOP":
		*c = `Pa’anga`

	// Deprecated. Timor-Leste now uses the US Dollar
	case "TPE":
		*c = `Timor Escudo`

	// Deprecated, replaced by TRY (prices normally quoted as integers)
	case "TRL":
		*c = `Turkish Lira (old)`

	// Turkey, from 1 January 2005
	case "TRY":
		*c = `Turkish Lira`

	// Trinidad and Tobago
	case "TTD":
		*c = `Trinidad and Tobago Dollar`

	// Taiwan (Province of China)
	case "TWD":
		*c = `New Taiwan Dollar`

	// Tanzania (United Republic of) (prices normally quoted as integers)
	case "TZS":
		*c = `Tanzanian Shilling`

	// Ukraine
	case "UAH":
		*c = `Hryvnia`

	// Uganda (prices normally quoted as integers)
	case "UGX":
		*c = `Uganda Shilling`

	// United States, American Samoa, Bonaire, Sint Eustatius and Saba, British Indian Ocean Territory, Ecuador, El Salvador, Guam, Haiti, Marshall Is, Micronesia (Federated States of), Northern Mariana Is, Palau, Panama, Puerto Rico, Timor-Leste, Turks and Caicos Is, US Minor Outlying Is, Virgin Is (British), Virgin Is (US)
	case "USD":
		*c = `US Dollar`

	// Uruguay
	case "UYU":
		*c = `Peso Uruguayo`

	// Uzbekistan (prices normally quoted as integers)
	case "UZS":
		*c = `Uzbekistan Sum`

	// Deprecated, replaced by VEF
	case "VEB":
		*c = `Bolivar`

	// Venezuela (formerly Bolívar fuerte)
	case "VEF":
		*c = `Bolívar`

	// Viet Nam (prices normally quoted as integers)
	case "VND":
		*c = `Dong`

	// Vanuatu (prices normally quoted as integers)
	case "VUV":
		*c = `Vatu`

	// Samoa
	case "WST":
		*c = `Tala`

	// Cameroon, Central African Republic, Chad, Congo, Equatorial Guinea, Gabon (prices normally quoted as integers)
	case "XAF":
		*c = `CFA Franc BEAC`

	// Anguilla, Antigua and Barbuda, Dominica, Grenada, Montserrat, Saint Kitts and Nevis, Saint Lucia, Saint Vincent and the Grenadines
	case "XCD":
		*c = `East Caribbean Dollar`

	// Benin, Burkina Faso, Côte D’Ivoire, Guinea-Bissau, Mali, Niger, Senegal, Togo (prices normally quoted as integers)
	case "XOF":
		*c = `CFA Franc BCEAO`

	// French Polynesia, New Caledonia, Wallis and Futuna (prices normally quoted as integers)
	case "XPF":
		*c = `CFP Franc`

	// Yemen (prices normally quoted as integers)
	case "YER":
		*c = `Yemeni Rial`

	// DEPRECATED, replaced by CSD
	case "YUM":
		*c = `Yugoslavian Dinar`

	// South Africa, Namibia, Lesotho
	case "ZAR":
		*c = `Rand`

	// Zambia. Deprecated, replaced with ZMW (prices normally quoted as integers)
	case "ZMK":
		*c = `Kwacha`

	// Zambia
	case "ZMW":
		*c = `Zambian Kwacha`

	// Deprecated, replaced with ZWL (prices normally quoted as integers)
	case "ZWD":
		*c = `Zimbabwe Dollar`

	// Zimbabwe
	case "ZWL":
		*c = `Zimbabwe Dollar`
	default:
		return fmt.Errorf("undefined code for CurrencyCodeIso4217 has been passed, got [%s]", v)
	}
	return nil
}

// BibleTextFeature List 97: Bible text feature
type BibleTextFeature string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *BibleTextFeature) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// Words spoken by Christ are printed in red
	case "RL":
		*c = `Red letter`
	default:
		return fmt.Errorf("undefined code for BibleTextFeature has been passed, got [%s]", v)
	}
	return nil
}

// ProductFormFeatureValueBindingOrPageEdgeColor List 98: Product form feature value – binding or page edge color
type ProductFormFeatureValueBindingOrPageEdgeColor string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *ProductFormFeatureValueBindingOrPageEdgeColor) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	//
	case "BLK":
		*c = `Black`

	//
	case "BLU":
		*c = `Blue`

	//
	case "BRN":
		*c = `Brown`

	//
	case "BUR":
		*c = `Burgundy/maroon`

	//
	case "CRE":
		*c = `Cream`

	//
	case "FCO":
		*c = `Four-color`

	//
	case "FCS":
		*c = `Four-color and spot-color`

	//
	case "GLD":
		*c = `Gold`

	//
	case "GRN":
		*c = `Green`

	//
	case "GRY":
		*c = `Grey`

	// Use <ProductFormFeatureDescription> to add brief details if required
	case "MUL":
		*c = `Multicolor`

	//
	case "NAV":
		*c = `Navy/Dark blue`

	//
	case "ORG":
		*c = `Orange`

	//
	case "PNK":
		*c = `Pink`

	//
	case "PUR":
		*c = `Purple`

	//
	case "RED":
		*c = `Red`

	//
	case "SKY":
		*c = `Sky/Pale blue`

	//
	case "SLV":
		*c = `Silver`

	//
	case "TAN":
		*c = `Tan/Light brown`

	//
	case "TEA":
		*c = `Teal/Turquoise green`

	//
	case "WHI":
		*c = `White`

	//
	case "YEL":
		*c = `Yellow`

	//
	case "ZZZ":
		*c = `Other`
	default:
		return fmt.Errorf("undefined code for ProductFormFeatureValueBindingOrPageEdgeColor has been passed, got [%s]", v)
	}
	return nil
}

// ProductFormFeatureValueSpecialCoverMaterial List 99: Product form feature value – special cover material
type ProductFormFeatureValueSpecialCoverMaterial string

// UnmarshalXML is unmarshaler from code to human readable description as of defined at codelists.
func (c *ProductFormFeatureValueSpecialCoverMaterial) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	switch v {

	// Pigskin
	case "01":
		*c = `Berkshire leather`

	//
	case "02":
		*c = `Calfskin`

	// Calf split or sheep split
	case "03":
		*c = `French Morocco`

	// Goatskin
	case "04":
		*c = `Morocco`

	//
	case "05":
		*c = `Bonded buffalo grain`

	//
	case "06":
		*c = `Bonded calf grain`

	//
	case "07":
		*c = `Bonded Cordova`

	//
	case "08":
		*c = `Bonded eelskin`

	//
	case "09":
		*c = `Bonded Ostraleg`

	//
	case "10":
		*c = `Bonded ostrich`

	//
	case "11":
		*c = `Bonded reptile grain`

	//
	case "12":
		*c = `Bonded leather`

	//
	case "13":
		*c = `Cowhide`

	//
	case "14":
		*c = `Eelskin`

	//
	case "15":
		*c = `Kivar`

	// An imitation leather binding material
	case "16":
		*c = `Leatherflex`

	//
	case "17":
		*c = `Moleskin`

	//
	case "18":
		*c = `Softhide leather`

	//
	case "19":
		*c = `Metal`

	// German ‘Samt’
	case "20":
		*c = `Velvet`

	// Spanish ‘nácar’
	case "21":
		*c = `Mother-of-pearl`

	//
	case "22":
		*c = `Papyrus`

	// An imitation cloth binding material
	case "23":
		*c = `Géltex`

	// An imitation leather binding material
	case "24":
		*c = `Guaflex`

	// An imitation made of any non-leather material
	case "25":
		*c = `Imitation leather`

	//
	case "26":
		*c = `Pigskin`

	//
	case "27":
		*c = `Goatskin`
	default:
		return fmt.Errorf("undefined code for ProductFormFeatureValueSpecialCoverMaterial has been passed, got [%s]", v)
	}
	return nil
}
