package onix

import (
	"encoding/xml"
	"fmt"
)

// Header is not documented yet.
type Header struct {
	FromCompany           string `xml:"m174"`
	SentDate              string `xml:"m182"`
	DefaultLanguageOfText string `xml:"m184"`
	DefaultPriceTypeCode  string `xml:"m185"`
	DefaultCurrencyCode   string `xml:"m186"`
}

// Productidentifier is not documented yet.
type Productidentifier struct {
	ProductIDType ProductIDType `xml:"b221"`
	IDValue       string        `xml:"b244"` // EAN
}

// Productidentifiers is not documented yet.
type Productidentifiers []Productidentifier

// FindByIDType findx identifier by id-type.
func (c *Productidentifiers) FindByIDType(idType ProductIDType) *string {
	for _, p := range *c {
		if p.ProductIDType == idType {
			return &p.IDValue
		}
	}
	return nil
}

// Price is not documented yet.
type Price struct {
	PriceTypeCode  string `xml:"j148"`
	DiscountCodeds []struct {
		DiscountCodeType     DiscountCodeType `xml:"j363"`
		DiscountCodeTypeName string           `xml:"j378"`
		DiscountCode         string           `xml:"j364"`
	} `xml:"discountcoded"`
	PriceAmount  float64 `xml:"j151"`
	CurrencyCode string  `xml:"j152"`
	CountryCode  string  `xml:"b251"`
}

// Prices is not documented yet.
type Prices []Price

// FindByType findx identifier by id-type.
func (c *Prices) FindByType(ty string) *Price {
	for _, p := range *c {
		if p.CurrencyCode == ty {
			return &p
		}
	}
	return nil
}

// SupplyDetail is not documented yet.
type SupplyDetail struct {
	SupplierName        string `xml:"j137"`
	SupplierRole        string `xml:"j292"`
	ReturnsCodeType     string `xml:"j268"`
	ReturnsCode         string `xml:"j269"`
	ProductAvailability string `xml:"j396"`
	PackQuantity        int    `xml:"j145"`
	Prices              Prices `xml:"price"`
}

// Measure is not documented yet.
type Measure struct {
	MeasureTypeCode MeasureTypeCode `xml:"c093"`
	Measurement     float64         `xml:"c094"`
	MeasureUnitCode string          `xml:"c095"`
}

const kirogramPerPound float64 = 0.4535924277
const gramPerOunce float64 = 28.349

// ToKg convert measure to kirogram.
func (m *Measure) ToKg() (float64, error) {
	switch m.MeasureUnitCode {
	case "gr":
		return m.Measurement / 1000, nil
	case "kg":
		return m.Measurement, nil
	case "lb":
		return m.Measurement * kirogramPerPound, nil
	case "oz":
		return m.Measurement * gramPerOunce / 1000, nil
	default:
		return 0, fmt.Errorf("Unexpected value of the unit was passed, got [%s] [%s]", m.MeasureTypeCode, m.MeasureUnitCode)
	}
}

// Measures is not documented yet.
type Measures []Measure

// FindByType findx identifier by id-type.
func (c *Measures) FindByType(ty MeasureTypeCode) *Measure {
	for _, p := range *c {
		if p.MeasureTypeCode == ty {
			return &p
		}
	}
	return nil
}

// Text is not documented yet.
type Text struct {
	Body       string     `xml:",cdata"`
	TextFormat TextFormat `xml:"textformat,attr"`
}

// OtherText is not documented yet.
type OtherText struct {
	TextTypeCode TextTypeCode `xml:"d102"`
	Text         Text         `xml:"d104"`
}

// OtherTexts is not documented yet.
type OtherTexts []OtherText

// FindByType findx identifier by id-type.
func (c *OtherTexts) FindByType(ty TextTypeCode) *OtherText {
	for _, p := range *c {
		if p.TextTypeCode == ty {
			return &p
		}
	}
	return nil
}

// Subject is not documented yet.
type Subject struct {
	SubjectSchemeIdentifier SubjectSchemeIdentifier `xml:"b067"`
	SubjectSchemeName       string                  `xml:"b171,omitempty"`
	SubjectCode             string                  `xml:"b069,omitempty"`
	SubjectHeadingText      string                  `xml:"b070,omitempty"`
}

// Subjects is not documented yet.
type Subjects []Subject

// FindByIDType findx identifier by id-type.
func (c *Subjects) FindByIDType(idType SubjectSchemeIdentifier) *Subject {
	for _, p := range *c {
		if p.SubjectSchemeIdentifier == idType {
			return &p
		}
	}
	return nil
}

// Imprint is not documented yet.
type Imprint struct {
	NameCodeType     string `xml:"b241"`
	NameCodeTypeName string `xml:"b242"`
	NameCodeValue    string `xml:"b243"`
	ImprintName      string `xml:"b079"`
}

// Imprints is not documented yet.
type Imprints = []Imprint

// Product is not documented yet.
type Product struct {
	RecordReference       string             `xml:"a001"`
	NotificationType      string             `xml:"a002"`
	Productidentifiers    Productidentifiers `xml:"productidentifier"`
	ProductForm           string             `xml:"b012"`
	ProductFormDetail     []string           `xml:"b333"`
	ProductClassification struct {
		ProductClassificationType string `xml:"b274"`
		ProductClassificationCode string `xml:"b275"`
	} `xml:"productclassification"`
	NoSeries BoolIfElementPresent `xml:"n338"`
	Title    struct {
		TextCase  string `xml:"textcase,attr"`
		Language  string `xml:"language,attr"`
		TitleType string `xml:"b202"`
		TitleText string `xml:"b203"`
		Subtitle  string `xml:"b029,omitempty"`
	} `xml:"title"`
	Contributor struct {
		ContributorRole  string `xml:"b035"`
		NamesBeforeKey   string `xml:"b039"`
		KeyNames         string `xml:"b040"`
		BiographicalNote string `xml:"b044"`
	} `xml:"contributor"`
	Language struct {
		LanguageRole string `xml:"b253"`
		LanguageCode string `xml:"b252"`
	} `xml:"language"`
	NumberOfPages    int    `xml:"b061"`
	BASICMainSubject string `xml:"b064"`
	MainSubject      struct {
		MainSubjectSchemeIdentifier int    `xml:"b191"`
		SubjectCode                 string `xml:"b069"`
		SubjectHeadingText          string `xml:"b070"`
	} `xml:"mainsubject"`
	Subjects     Subjects   `xml:"subject"`
	AudienceCode string     `xml:"b073"`
	OtherTexts   OtherTexts `xml:"othertext"`
	Imprints     Imprints   `xml:"imprint"`
	Publisher    struct {
		PublishingRole string `xml:"b291"`
		PublisherName  string `xml:"b081"`
	} `xml:"publisher"`
	PublishingStatus struct {
		Body      string `xml:",innerxml"`
		Datestamp string `xml:"datestamp,attr"`
	} `xml:"b394"`
	PublicationDate string `xml:"b003"`
	SalesRights     []struct {
		SalesRightsType string `xml:"b089"`
		RightsCountry   string `xml:"b090"`
		RightsTerritory string `xml:"b388"`
	} `xml:"salesrights"`
	Measures        Measures `xml:"measure"`
	RelatedProducts []struct {
		RelationCode      string              `xml:"h208"`
		Productidentifier []Productidentifier `xml:"productidentifier"`
		ProductForm       string              `xml:"b012"`
	} `xml:"relatedproduct"`
	SupplyDetail SupplyDetail `xml:"supplydetail"`
}

// BoolIfElementPresent represent whether exsits self-closing tag.
type BoolIfElementPresent bool

// UnmarshalXML convert self-closing tag to bool.
// NOTE: https://stackoverflow.com/questions/23724591/golang-unmarshal-self-closing-tags
func (c *BoolIfElementPresent) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var v string
	d.DecodeElement(&v, &start)
	*c = true
	return nil
}

// Onix is not documented yet.
type Onix struct {
	// Root of XML
	XMLName  xml.Name  `xml:"ONIXmessage"`
	Header   Header    `xml:"header"`
	Products []Product `xml:"product"`
}