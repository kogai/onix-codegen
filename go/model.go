package onix

import (
	"encoding/xml"
	"fmt"
)

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


// ONIXMessage is not documented.
type ONIXMessage struct {
	Header Header `xml:"header"` 
	Product Product `xml:"product"` 
	MainSeriesRecord MainSeriesRecord `xml:"mainseriesrecord"` 
	SubSeriesRecord SubSeriesRecord `xml:"subseriesrecord"` 
}

// AddresseeIdentifier is not documented.
type AddresseeIdentifier struct {
	AddresseeIDType AddresseeIDType `xml:"m380"` 
	IDTypeName IDTypeName `xml:"b233"` 
	IDValue IDValue `xml:"b244"` 
}

// AgentIdentifier is not documented.
type AgentIdentifier struct {
	AgentIDType AgentIDType `xml:"j400"` 
	IDTypeName IDTypeName `xml:"b233"` 
	IDValue IDValue `xml:"b244"` 
}

// Audience is not documented.
type Audience struct {
	AudienceCodeType AudienceCodeType `xml:"b204"` 
	AudienceCodeTypeName AudienceCodeTypeName `xml:"b205"` 
	AudienceCodeValue AudienceCodeValue `xml:"b206"` 
}

// AudienceRange is not documented.
type AudienceRange struct {
	AudienceRangeQualifier AudienceRangeQualifier `xml:"b074"` 
	AudienceRangePrecision AudienceRangePrecision `xml:"b075"` 
	AudienceRangeValue AudienceRangeValue `xml:"b076"` 
	AudienceRangePrecision AudienceRangePrecision `xml:"b075"` 
	AudienceRangeValue AudienceRangeValue `xml:"b076"` 
	AudienceRangePrecision AudienceRangePrecision `xml:"b075"` 
	AudienceRangeValue AudienceRangeValue `xml:"b076"` 
}

// BatchBonus is not documented.
type BatchBonus struct {
	BatchQuantity BatchQuantity `xml:"j264"` 
	FreeQuantity FreeQuantity `xml:"j265"` 
}

// Bible is not documented.
type Bible struct {
	BibleContents BibleContents `xml:"b352"` 
	BibleVersion BibleVersion `xml:"b353"` 
	StudyBibleType StudyBibleType `xml:"b389"` 
	BiblePurpose BiblePurpose `xml:"b354"` 
	BibleTextOrganization BibleTextOrganization `xml:"b355"` 
	BibleReferenceLocation BibleReferenceLocation `xml:"b356"` 
	BibleTextFeature BibleTextFeature `xml:"b357"` 
}

// Complexity is not documented.
type Complexity struct {
	ComplexitySchemeIdentifier ComplexitySchemeIdentifier `xml:"b077"` 
	ComplexityCode ComplexityCode `xml:"b078"` 
}

// Conference is not documented.
type Conference struct {
	ConferenceRole ConferenceRole `xml:"b051"` 
	ConferenceName ConferenceName `xml:"b052"` 
	ConferenceAcronym ConferenceAcronym `xml:"b341"` 
	ConferenceNumber ConferenceNumber `xml:"b053"` 
	ConferenceTheme ConferenceTheme `xml:"b342"` 
	ConferenceDate ConferenceDate `xml:"b054"` 
	ConferencePlace ConferencePlace `xml:"b055"` 
	ConferenceSponsor ConferenceSponsor `xml:"conferencesponsor"` 
	Website Website `xml:"website"` 
}

// ConferenceSponsor is not documented.
type ConferenceSponsor struct {
	ConferenceSponsorIdentifier ConferenceSponsorIdentifier `xml:"conferencesponsoridentifier"` 
	PersonName PersonName `xml:"b036"` 
	CorporateName CorporateName `xml:"b047"` 
}

// ConferenceSponsorIdentifier is not documented.
type ConferenceSponsorIdentifier struct {
	ConferenceSponsorIDType ConferenceSponsorIDType `xml:"b391"` 
	IDTypeName IDTypeName `xml:"b233"` 
	IDValue IDValue `xml:"b244"` 
}

// ContainedItem is not documented.
type ContainedItem struct {
	ISBN ISBN `xml:"b004"` 
	EAN13 EAN13 `xml:"b005"` 
	ProductIdentifier ProductIdentifier `xml:"productidentifier"` 
	ProductForm ProductForm `xml:"b012"` 
	ProductFormDetail ProductFormDetail `xml:"b333"` 
	ProductFormFeature ProductFormFeature `xml:"productformfeature"` 
	BookFormDetail BookFormDetail `xml:"b013"` 
	ProductPackaging ProductPackaging `xml:"b225"` 
	ProductFormDescription ProductFormDescription `xml:"b014"` 
	EAN13 EAN13 `xml:"b005"` 
	ProductIdentifier ProductIdentifier `xml:"productidentifier"` 
	ProductForm ProductForm `xml:"b012"` 
	ProductFormDetail ProductFormDetail `xml:"b333"` 
	ProductFormFeature ProductFormFeature `xml:"productformfeature"` 
	BookFormDetail BookFormDetail `xml:"b013"` 
	ProductPackaging ProductPackaging `xml:"b225"` 
	ProductFormDescription ProductFormDescription `xml:"b014"` 
	ProductIdentifier ProductIdentifier `xml:"productidentifier"` 
	ProductForm ProductForm `xml:"b012"` 
	ProductFormDetail ProductFormDetail `xml:"b333"` 
	ProductFormFeature ProductFormFeature `xml:"productformfeature"` 
	BookFormDetail BookFormDetail `xml:"b013"` 
	ProductPackaging ProductPackaging `xml:"b225"` 
	ProductFormDescription ProductFormDescription `xml:"b014"` 
	ProductForm ProductForm `xml:"b012"` 
	ProductFormDetail ProductFormDetail `xml:"b333"` 
	ProductFormFeature ProductFormFeature `xml:"productformfeature"` 
	BookFormDetail BookFormDetail `xml:"b013"` 
	ProductPackaging ProductPackaging `xml:"b225"` 
	ProductFormDescription ProductFormDescription `xml:"b014"` 
	NumberOfPieces NumberOfPieces `xml:"b210"` 
	TradeCategory TradeCategory `xml:"b384"` 
	ProductContentType ProductContentType `xml:"b385"` 
	ItemQuantity ItemQuantity `xml:"b015"` 
	ISBN ISBN `xml:"b004"` 
	EAN13 EAN13 `xml:"b005"` 
	ProductIdentifier ProductIdentifier `xml:"productidentifier"` 
	ProductForm ProductForm `xml:"b012"` 
	ProductFormDetail ProductFormDetail `xml:"b333"` 
	ProductFormFeature ProductFormFeature `xml:"productformfeature"` 
	BookFormDetail BookFormDetail `xml:"b013"` 
	ProductPackaging ProductPackaging `xml:"b225"` 
	ProductFormDescription ProductFormDescription `xml:"b014"` 
	ProductForm ProductForm `xml:"b012"` 
	ProductFormDetail ProductFormDetail `xml:"b333"` 
	ProductFormFeature ProductFormFeature `xml:"productformfeature"` 
	BookFormDetail BookFormDetail `xml:"b013"` 
	ProductPackaging ProductPackaging `xml:"b225"` 
	ProductFormDescription ProductFormDescription `xml:"b014"` 
	EAN13 EAN13 `xml:"b005"` 
	ProductIdentifier ProductIdentifier `xml:"productidentifier"` 
	ProductForm ProductForm `xml:"b012"` 
	ProductFormDetail ProductFormDetail `xml:"b333"` 
	ProductFormFeature ProductFormFeature `xml:"productformfeature"` 
	BookFormDetail BookFormDetail `xml:"b013"` 
	ProductPackaging ProductPackaging `xml:"b225"` 
	ProductFormDescription ProductFormDescription `xml:"b014"` 
	ProductForm ProductForm `xml:"b012"` 
	ProductFormDetail ProductFormDetail `xml:"b333"` 
	ProductFormFeature ProductFormFeature `xml:"productformfeature"` 
	BookFormDetail BookFormDetail `xml:"b013"` 
	ProductPackaging ProductPackaging `xml:"b225"` 
	ProductFormDescription ProductFormDescription `xml:"b014"` 
	ProductIdentifier ProductIdentifier `xml:"productidentifier"` 
	ProductForm ProductForm `xml:"b012"` 
	ProductFormDetail ProductFormDetail `xml:"b333"` 
	ProductFormFeature ProductFormFeature `xml:"productformfeature"` 
	BookFormDetail BookFormDetail `xml:"b013"` 
	ProductPackaging ProductPackaging `xml:"b225"` 
	ProductFormDescription ProductFormDescription `xml:"b014"` 
	ProductForm ProductForm `xml:"b012"` 
	ProductFormDetail ProductFormDetail `xml:"b333"` 
	ProductFormFeature ProductFormFeature `xml:"productformfeature"` 
	BookFormDetail BookFormDetail `xml:"b013"` 
	ProductPackaging ProductPackaging `xml:"b225"` 
	ProductFormDescription ProductFormDescription `xml:"b014"` 
	ProductForm ProductForm `xml:"b012"` 
	ProductFormDetail ProductFormDetail `xml:"b333"` 
	ProductFormFeature ProductFormFeature `xml:"productformfeature"` 
	BookFormDetail BookFormDetail `xml:"b013"` 
	ProductPackaging ProductPackaging `xml:"b225"` 
	ProductFormDescription ProductFormDescription `xml:"b014"` 
}

// ContentItem is not documented.
type ContentItem struct {
	LevelSequenceNumber LevelSequenceNumber `xml:"b284"` 
	TextItem TextItem `xml:"textitem"` 
	Website Website `xml:"website"` 
	ComponentTypeName ComponentTypeName `xml:"b288"` 
	ComponentNumber ComponentNumber `xml:"b289"` 
	DistinctiveTitle DistinctiveTitle `xml:"b028"` 
	Title Title `xml:"title"` 
	ComponentNumber ComponentNumber `xml:"b289"` 
	DistinctiveTitle DistinctiveTitle `xml:"b028"` 
	Title Title `xml:"title"` 
	DistinctiveTitle DistinctiveTitle `xml:"b028"` 
	Title Title `xml:"title"` 
	Title Title `xml:"title"` 
	WorkIdentifier WorkIdentifier `xml:"workidentifier"` 
	Contributor Contributor `xml:"contributor"` 
	ContributorStatement ContributorStatement `xml:"b049"` 
	Subject Subject `xml:"subject"` 
	PersonAsSubject PersonAsSubject `xml:"personassubject"` 
	CorporateBodyAsSubject CorporateBodyAsSubject `xml:"b071"` 
	PlaceAsSubject PlaceAsSubject `xml:"b072"` 
	OtherText OtherText `xml:"othertext"` 
	MediaFile MediaFile `xml:"mediafile"` 
	ComponentTypeName ComponentTypeName `xml:"b288"` 
	ComponentNumber ComponentNumber `xml:"b289"` 
	DistinctiveTitle DistinctiveTitle `xml:"b028"` 
	Title Title `xml:"title"` 
	ComponentNumber ComponentNumber `xml:"b289"` 
	DistinctiveTitle DistinctiveTitle `xml:"b028"` 
	Title Title `xml:"title"` 
	DistinctiveTitle DistinctiveTitle `xml:"b028"` 
	Title Title `xml:"title"` 
	Contributor Contributor `xml:"contributor"` 
	ContributorStatement ContributorStatement `xml:"b049"` 
}

// Contributor is not documented.
type Contributor struct {
	SequenceNumber SequenceNumber `xml:"b034"` 
	ContributorRole ContributorRole `xml:"b035"` 
	LanguageCode LanguageCode `xml:"b252"` 
	SequenceNumberWithinRole SequenceNumberWithinRole `xml:"b340"` 
	ContributorRole ContributorRole `xml:"b035"` 
	LanguageCode LanguageCode `xml:"b252"` 
	PersonName PersonName `xml:"b036"` 
	PersonNameInverted PersonNameInverted `xml:"b037"` 
	TitlesBeforeNames TitlesBeforeNames `xml:"b038"` 
	NamesBeforeKey NamesBeforeKey `xml:"b039"` 
	PrefixToKey PrefixToKey `xml:"b247"` 
	KeyNames KeyNames `xml:"b040"` 
	NamesAfterKey NamesAfterKey `xml:"b041"` 
	SuffixToKey SuffixToKey `xml:"b248"` 
	LettersAfterNames LettersAfterNames `xml:"b042"` 
	TitlesAfterNames TitlesAfterNames `xml:"b043"` 
	Name Name `xml:"name"` 
	PersonNameIdentifier PersonNameIdentifier `xml:"personnameidentifier"` 
	PersonNameInverted PersonNameInverted `xml:"b037"` 
	TitlesBeforeNames TitlesBeforeNames `xml:"b038"` 
	NamesBeforeKey NamesBeforeKey `xml:"b039"` 
	PrefixToKey PrefixToKey `xml:"b247"` 
	KeyNames KeyNames `xml:"b040"` 
	NamesAfterKey NamesAfterKey `xml:"b041"` 
	SuffixToKey SuffixToKey `xml:"b248"` 
	LettersAfterNames LettersAfterNames `xml:"b042"` 
	TitlesAfterNames TitlesAfterNames `xml:"b043"` 
	Name Name `xml:"name"` 
	PersonNameIdentifier PersonNameIdentifier `xml:"personnameidentifier"` 
	TitlesBeforeNames TitlesBeforeNames `xml:"b038"` 
	NamesBeforeKey NamesBeforeKey `xml:"b039"` 
	PrefixToKey PrefixToKey `xml:"b247"` 
	KeyNames KeyNames `xml:"b040"` 
	NamesAfterKey NamesAfterKey `xml:"b041"` 
	SuffixToKey SuffixToKey `xml:"b248"` 
	LettersAfterNames LettersAfterNames `xml:"b042"` 
	TitlesAfterNames TitlesAfterNames `xml:"b043"` 
	Name Name `xml:"name"` 
	PersonNameIdentifier PersonNameIdentifier `xml:"personnameidentifier"` 
	Name Name `xml:"name"` 
	PersonNameIdentifier PersonNameIdentifier `xml:"personnameidentifier"` 
	PersonNameIdentifier PersonNameIdentifier `xml:"personnameidentifier"` 
	PersonDate PersonDate `xml:"persondate"` 
	ProfessionalAffiliation ProfessionalAffiliation `xml:"professionalaffiliation"` 
	CorporateName CorporateName `xml:"b047"` 
	BiographicalNote BiographicalNote `xml:"b044"` 
	Website Website `xml:"website"` 
	ProfessionalPosition ProfessionalPosition `xml:"b045"` 
	Affiliation Affiliation `xml:"b046"` 
	ContributorDescription ContributorDescription `xml:"b048"` 
	UnnamedPersons UnnamedPersons `xml:"b249"` 
	CountryCode CountryCode `xml:"b251"` 
	RegionCode RegionCode `xml:"b398"` 
	ContributorRole ContributorRole `xml:"b035"` 
	LanguageCode LanguageCode `xml:"b252"` 
	PersonName PersonName `xml:"b036"` 
	PersonNameInverted PersonNameInverted `xml:"b037"` 
	TitlesBeforeNames TitlesBeforeNames `xml:"b038"` 
	NamesBeforeKey NamesBeforeKey `xml:"b039"` 
	PrefixToKey PrefixToKey `xml:"b247"` 
	KeyNames KeyNames `xml:"b040"` 
	NamesAfterKey NamesAfterKey `xml:"b041"` 
	SuffixToKey SuffixToKey `xml:"b248"` 
	LettersAfterNames LettersAfterNames `xml:"b042"` 
	TitlesAfterNames TitlesAfterNames `xml:"b043"` 
	Name Name `xml:"name"` 
	PersonNameIdentifier PersonNameIdentifier `xml:"personnameidentifier"` 
	PersonNameInverted PersonNameInverted `xml:"b037"` 
	TitlesBeforeNames TitlesBeforeNames `xml:"b038"` 
	NamesBeforeKey NamesBeforeKey `xml:"b039"` 
	PrefixToKey PrefixToKey `xml:"b247"` 
	KeyNames KeyNames `xml:"b040"` 
	NamesAfterKey NamesAfterKey `xml:"b041"` 
	SuffixToKey SuffixToKey `xml:"b248"` 
	LettersAfterNames LettersAfterNames `xml:"b042"` 
	TitlesAfterNames TitlesAfterNames `xml:"b043"` 
	Name Name `xml:"name"` 
	PersonNameIdentifier PersonNameIdentifier `xml:"personnameidentifier"` 
	TitlesBeforeNames TitlesBeforeNames `xml:"b038"` 
	NamesBeforeKey NamesBeforeKey `xml:"b039"` 
	PrefixToKey PrefixToKey `xml:"b247"` 
	KeyNames KeyNames `xml:"b040"` 
	NamesAfterKey NamesAfterKey `xml:"b041"` 
	SuffixToKey SuffixToKey `xml:"b248"` 
	LettersAfterNames LettersAfterNames `xml:"b042"` 
	TitlesAfterNames TitlesAfterNames `xml:"b043"` 
	Name Name `xml:"name"` 
	PersonNameIdentifier PersonNameIdentifier `xml:"personnameidentifier"` 
	Name Name `xml:"name"` 
	PersonNameIdentifier PersonNameIdentifier `xml:"personnameidentifier"` 
	PersonNameIdentifier PersonNameIdentifier `xml:"personnameidentifier"` 
	PersonDate PersonDate `xml:"persondate"` 
	ProfessionalAffiliation ProfessionalAffiliation `xml:"professionalaffiliation"` 
	CorporateName CorporateName `xml:"b047"` 
	BiographicalNote BiographicalNote `xml:"b044"` 
	Website Website `xml:"website"` 
	ProfessionalPosition ProfessionalPosition `xml:"b045"` 
	Affiliation Affiliation `xml:"b046"` 
	ContributorDescription ContributorDescription `xml:"b048"` 
	PersonName PersonName `xml:"b036"` 
	PersonNameInverted PersonNameInverted `xml:"b037"` 
	TitlesBeforeNames TitlesBeforeNames `xml:"b038"` 
	NamesBeforeKey NamesBeforeKey `xml:"b039"` 
	PrefixToKey PrefixToKey `xml:"b247"` 
	KeyNames KeyNames `xml:"b040"` 
	NamesAfterKey NamesAfterKey `xml:"b041"` 
	SuffixToKey SuffixToKey `xml:"b248"` 
	LettersAfterNames LettersAfterNames `xml:"b042"` 
	TitlesAfterNames TitlesAfterNames `xml:"b043"` 
	Name Name `xml:"name"` 
	PersonNameIdentifier PersonNameIdentifier `xml:"personnameidentifier"` 
	PersonNameInverted PersonNameInverted `xml:"b037"` 
	TitlesBeforeNames TitlesBeforeNames `xml:"b038"` 
	NamesBeforeKey NamesBeforeKey `xml:"b039"` 
	PrefixToKey PrefixToKey `xml:"b247"` 
	KeyNames KeyNames `xml:"b040"` 
	NamesAfterKey NamesAfterKey `xml:"b041"` 
	SuffixToKey SuffixToKey `xml:"b248"` 
	LettersAfterNames LettersAfterNames `xml:"b042"` 
	TitlesAfterNames TitlesAfterNames `xml:"b043"` 
	Name Name `xml:"name"` 
	PersonNameIdentifier PersonNameIdentifier `xml:"personnameidentifier"` 
	TitlesBeforeNames TitlesBeforeNames `xml:"b038"` 
	NamesBeforeKey NamesBeforeKey `xml:"b039"` 
	PrefixToKey PrefixToKey `xml:"b247"` 
	KeyNames KeyNames `xml:"b040"` 
	NamesAfterKey NamesAfterKey `xml:"b041"` 
	SuffixToKey SuffixToKey `xml:"b248"` 
	LettersAfterNames LettersAfterNames `xml:"b042"` 
	TitlesAfterNames TitlesAfterNames `xml:"b043"` 
	Name Name `xml:"name"` 
	PersonNameIdentifier PersonNameIdentifier `xml:"personnameidentifier"` 
	Name Name `xml:"name"` 
	PersonNameIdentifier PersonNameIdentifier `xml:"personnameidentifier"` 
	PersonNameIdentifier PersonNameIdentifier `xml:"personnameidentifier"` 
	PersonDate PersonDate `xml:"persondate"` 
	ProfessionalAffiliation ProfessionalAffiliation `xml:"professionalaffiliation"` 
	PersonName PersonName `xml:"b036"` 
	PersonNameInverted PersonNameInverted `xml:"b037"` 
	TitlesBeforeNames TitlesBeforeNames `xml:"b038"` 
	NamesBeforeKey NamesBeforeKey `xml:"b039"` 
	PrefixToKey PrefixToKey `xml:"b247"` 
	KeyNames KeyNames `xml:"b040"` 
	NamesAfterKey NamesAfterKey `xml:"b041"` 
	SuffixToKey SuffixToKey `xml:"b248"` 
	LettersAfterNames LettersAfterNames `xml:"b042"` 
	TitlesAfterNames TitlesAfterNames `xml:"b043"` 
	Name Name `xml:"name"` 
	PersonNameIdentifier PersonNameIdentifier `xml:"personnameidentifier"` 
	TitlesBeforeNames TitlesBeforeNames `xml:"b038"` 
	NamesBeforeKey NamesBeforeKey `xml:"b039"` 
	PrefixToKey PrefixToKey `xml:"b247"` 
	KeyNames KeyNames `xml:"b040"` 
	NamesAfterKey NamesAfterKey `xml:"b041"` 
	SuffixToKey SuffixToKey `xml:"b248"` 
	LettersAfterNames LettersAfterNames `xml:"b042"` 
	TitlesAfterNames TitlesAfterNames `xml:"b043"` 
	PersonNameInverted PersonNameInverted `xml:"b037"` 
	TitlesBeforeNames TitlesBeforeNames `xml:"b038"` 
	NamesBeforeKey NamesBeforeKey `xml:"b039"` 
	PrefixToKey PrefixToKey `xml:"b247"` 
	KeyNames KeyNames `xml:"b040"` 
	NamesAfterKey NamesAfterKey `xml:"b041"` 
	SuffixToKey SuffixToKey `xml:"b248"` 
	LettersAfterNames LettersAfterNames `xml:"b042"` 
	TitlesAfterNames TitlesAfterNames `xml:"b043"` 
	Name Name `xml:"name"` 
	PersonNameIdentifier PersonNameIdentifier `xml:"personnameidentifier"` 
	TitlesBeforeNames TitlesBeforeNames `xml:"b038"` 
	NamesBeforeKey NamesBeforeKey `xml:"b039"` 
	PrefixToKey PrefixToKey `xml:"b247"` 
	KeyNames KeyNames `xml:"b040"` 
	NamesAfterKey NamesAfterKey `xml:"b041"` 
	SuffixToKey SuffixToKey `xml:"b248"` 
	LettersAfterNames LettersAfterNames `xml:"b042"` 
	TitlesAfterNames TitlesAfterNames `xml:"b043"` 
	TitlesBeforeNames TitlesBeforeNames `xml:"b038"` 
	NamesBeforeKey NamesBeforeKey `xml:"b039"` 
	PrefixToKey PrefixToKey `xml:"b247"` 
	KeyNames KeyNames `xml:"b040"` 
	NamesAfterKey NamesAfterKey `xml:"b041"` 
	SuffixToKey SuffixToKey `xml:"b248"` 
	LettersAfterNames LettersAfterNames `xml:"b042"` 
	TitlesAfterNames TitlesAfterNames `xml:"b043"` 
	Name Name `xml:"name"` 
	PersonNameIdentifier PersonNameIdentifier `xml:"personnameidentifier"` 
	Name Name `xml:"name"` 
	PersonNameIdentifier PersonNameIdentifier `xml:"personnameidentifier"` 
}

// CopyrightOwner is not documented.
type CopyrightOwner struct {
	CopyrightOwnerIdentifier CopyrightOwnerIdentifier `xml:"copyrightowneridentifier"` 
	PersonName PersonName `xml:"b036"` 
	CorporateName CorporateName `xml:"b047"` 
}

// CopyrightOwnerIdentifier is not documented.
type CopyrightOwnerIdentifier struct {
	CopyrightOwnerIDType CopyrightOwnerIDType `xml:"b392"` 
	IDTypeName IDTypeName `xml:"b233"` 
	IDValue IDValue `xml:"b244"` 
}

// CopyrightStatement is not documented.
type CopyrightStatement struct {
	CopyrightYear CopyrightYear `xml:"b087"` 
	CopyrightOwner CopyrightOwner `xml:"copyrightowner"` 
}

// DiscountCoded is not documented.
type DiscountCoded struct {
	DiscountCodeType DiscountCodeType `xml:"j363"` 
	DiscountCodeTypeName DiscountCodeTypeName `xml:"j378"` 
	DiscountCode DiscountCode `xml:"j364"` 
}

// Extent is not documented.
type Extent struct {
	ExtentType ExtentType `xml:"b218"` 
	ExtentValue ExtentValue `xml:"b219"` 
	ExtentUnit ExtentUnit `xml:"b220"` 
}

// Header is not documented.
type Header struct {
	FromEANNumber FromEANNumber `xml:"m172"` 
	FromSAN FromSAN `xml:"m173"` 
	SenderIdentifier SenderIdentifier `xml:"senderidentifier"` 
	FromCompany FromCompany `xml:"m174"` 
	FromSAN FromSAN `xml:"m173"` 
	SenderIdentifier SenderIdentifier `xml:"senderidentifier"` 
	FromCompany FromCompany `xml:"m174"` 
	SenderIdentifier SenderIdentifier `xml:"senderidentifier"` 
	FromCompany FromCompany `xml:"m174"` 
	FromCompany FromCompany `xml:"m174"` 
	FromPerson FromPerson `xml:"m175"` 
	FromEmail FromEmail `xml:"m283"` 
	ToEANNumber ToEANNumber `xml:"m176"` 
	ToSAN ToSAN `xml:"m177"` 
	AddresseeIdentifier AddresseeIdentifier `xml:"addresseeidentifier"` 
	ToCompany ToCompany `xml:"m178"` 
	ToPerson ToPerson `xml:"m179"` 
	MessageNumber MessageNumber `xml:"m180"` 
	MessageRepeat MessageRepeat `xml:"m181"` 
	SentDate SentDate `xml:"m182"` 
	MessageNote MessageNote `xml:"m183"` 
	DefaultLanguageOfText DefaultLanguageOfText `xml:"m184"` 
	DefaultPriceTypeCode DefaultPriceTypeCode `xml:"m185"` 
	DefaultCurrencyCode DefaultCurrencyCode `xml:"m186"` 
	DefaultLinearUnit DefaultLinearUnit `xml:"m187"` 
	DefaultWeightUnit DefaultWeightUnit `xml:"m188"` 
	DefaultClassOfTrade DefaultClassOfTrade `xml:"m193"` 
	FromEANNumber FromEANNumber `xml:"m172"` 
	FromSAN FromSAN `xml:"m173"` 
	SenderIdentifier SenderIdentifier `xml:"senderidentifier"` 
	FromCompany FromCompany `xml:"m174"` 
	FromSAN FromSAN `xml:"m173"` 
	SenderIdentifier SenderIdentifier `xml:"senderidentifier"` 
	FromCompany FromCompany `xml:"m174"` 
	SenderIdentifier SenderIdentifier `xml:"senderidentifier"` 
	FromCompany FromCompany `xml:"m174"` 
}

// Illustrations is not documented.
type Illustrations struct {
	IllustrationType IllustrationType `xml:"b256"` 
	IllustrationTypeDescription IllustrationTypeDescription `xml:"b361"` 
	Number Number `xml:"b257"` 
}

// Imprint is not documented.
type Imprint struct {
	NameCodeType NameCodeType `xml:"b241"` 
	NameCodeTypeName NameCodeTypeName `xml:"b242"` 
	NameCodeValue NameCodeValue `xml:"b243"` 
	ImprintName ImprintName `xml:"b079"` 
}

// Language is not documented.
type Language struct {
	LanguageRole LanguageRole `xml:"b253"` 
	LanguageCode LanguageCode `xml:"b252"` 
	CountryCode CountryCode `xml:"b251"` 
}

// LocationIdentifier is not documented.
type LocationIdentifier struct {
	LocationIDType LocationIDType `xml:"j377"` 
	IDTypeName IDTypeName `xml:"b233"` 
	IDValue IDValue `xml:"b244"` 
}

// MainSeriesRecord is not documented.
type MainSeriesRecord struct {
	RecordReference RecordReference `xml:"a001"` 
	NotificationType NotificationType `xml:"a002"` 
	DeletionCode DeletionCode `xml:"a198"` 
	DeletionText DeletionText `xml:"a199"` 
	RecordSourceType RecordSourceType `xml:"a194"` 
	RecordSourceIdentifierType RecordSourceIdentifierType `xml:"a195"` 
	RecordSourceIdentifier RecordSourceIdentifier `xml:"a196"` 
	RecordSourceName RecordSourceName `xml:"a197"` 
	SeriesIdentifier SeriesIdentifier `xml:"seriesidentifier"` 
	Title Title `xml:"title"` 
	Contributor Contributor `xml:"contributor"` 
	OtherText OtherText `xml:"othertext"` 
	Publisher Publisher `xml:"publisher"` 
	SubordinateEntries SubordinateEntries `xml:"a245"` 
	RecordSourceIdentifierType RecordSourceIdentifierType `xml:"a195"` 
	RecordSourceIdentifier RecordSourceIdentifier `xml:"a196"` 
}

// MainSubject is not documented.
type MainSubject struct {
	MainSubjectSchemeIdentifier MainSubjectSchemeIdentifier `xml:"b191"` 
	SubjectSchemeVersion SubjectSchemeVersion `xml:"b068"` 
	SubjectCode SubjectCode `xml:"b069"` 
	SubjectHeadingText SubjectHeadingText `xml:"b070"` 
	SubjectHeadingText SubjectHeadingText `xml:"b070"` 
	SubjectCode SubjectCode `xml:"b069"` 
	SubjectHeadingText SubjectHeadingText `xml:"b070"` 
}

// MarketDate is not documented.
type MarketDate struct {
	MarketDateRole MarketDateRole `xml:"j408"` 
	DateFormat DateFormat `xml:"j260"` 
	Date Date `xml:"b306"` 
}

// MarketRepresentation is not documented.
type MarketRepresentation struct {
	AgentIdentifier AgentIdentifier `xml:"agentidentifier"` 
	AgentName AgentName `xml:"j401"` 
	AgentName AgentName `xml:"j401"` 
	TelephoneNumber TelephoneNumber `xml:"j270"` 
	FaxNumber FaxNumber `xml:"j271"` 
	EmailAddress EmailAddress `xml:"j272"` 
	Website Website `xml:"website"` 
	AgentRole AgentRole `xml:"j402"` 
	MarketCountry MarketCountry `xml:"j403"` 
	MarketTerritory MarketTerritory `xml:"j404"` 
	MarketCountryExcluded MarketCountryExcluded `xml:"j405"` 
	MarketRestrictionDetail MarketRestrictionDetail `xml:"j406"` 
	MarketPublishingStatus MarketPublishingStatus `xml:"j407"` 
	MarketDate MarketDate `xml:"marketdate"` 
	AgentIdentifier AgentIdentifier `xml:"agentidentifier"` 
	AgentName AgentName `xml:"j401"` 
	MarketTerritory MarketTerritory `xml:"j404"` 
	MarketCountryExcluded MarketCountryExcluded `xml:"j405"` 
}

// Measure is not documented.
type Measure struct {
	MeasureTypeCode MeasureTypeCode `xml:"c093"` 
	Measurement Measurement `xml:"c094"` 
	MeasureUnitCode MeasureUnitCode `xml:"c095"` 
}

// MediaFile is not documented.
type MediaFile struct {
	MediaFileTypeCode MediaFileTypeCode `xml:"f114"` 
	MediaFileFormatCode MediaFileFormatCode `xml:"f115"` 
	ImageResolution ImageResolution `xml:"f259"` 
	MediaFileLinkTypeCode MediaFileLinkTypeCode `xml:"f116"` 
	MediaFileLink MediaFileLink `xml:"f117"` 
	TextWithDownload TextWithDownload `xml:"f118"` 
	DownloadCaption DownloadCaption `xml:"f119"` 
	DownloadCredit DownloadCredit `xml:"f120"` 
	DownloadCopyrightNotice DownloadCopyrightNotice `xml:"f121"` 
	DownloadCredit DownloadCredit `xml:"f120"` 
	DownloadCopyrightNotice DownloadCopyrightNotice `xml:"f121"` 
	DownloadCopyrightNotice DownloadCopyrightNotice `xml:"f121"` 
	DownloadTerms DownloadTerms `xml:"f122"` 
	MediaFileDate MediaFileDate `xml:"f373"` 
	DownloadCaption DownloadCaption `xml:"f119"` 
	DownloadCredit DownloadCredit `xml:"f120"` 
	DownloadCopyrightNotice DownloadCopyrightNotice `xml:"f121"` 
	DownloadCredit DownloadCredit `xml:"f120"` 
	DownloadCopyrightNotice DownloadCopyrightNotice `xml:"f121"` 
}

// Name is not documented.
type Name struct {
	PersonNameType PersonNameType `xml:"b250"` 
	PersonName PersonName `xml:"b036"` 
	PersonNameInverted PersonNameInverted `xml:"b037"` 
	TitlesBeforeNames TitlesBeforeNames `xml:"b038"` 
	NamesBeforeKey NamesBeforeKey `xml:"b039"` 
	PrefixToKey PrefixToKey `xml:"b247"` 
	KeyNames KeyNames `xml:"b040"` 
	NamesAfterKey NamesAfterKey `xml:"b041"` 
	SuffixToKey SuffixToKey `xml:"b248"` 
	LettersAfterNames LettersAfterNames `xml:"b042"` 
	TitlesAfterNames TitlesAfterNames `xml:"b043"` 
	PersonNameIdentifier PersonNameIdentifier `xml:"personnameidentifier"` 
	PersonNameInverted PersonNameInverted `xml:"b037"` 
	TitlesBeforeNames TitlesBeforeNames `xml:"b038"` 
	NamesBeforeKey NamesBeforeKey `xml:"b039"` 
	PrefixToKey PrefixToKey `xml:"b247"` 
	KeyNames KeyNames `xml:"b040"` 
	NamesAfterKey NamesAfterKey `xml:"b041"` 
	SuffixToKey SuffixToKey `xml:"b248"` 
	LettersAfterNames LettersAfterNames `xml:"b042"` 
	TitlesAfterNames TitlesAfterNames `xml:"b043"` 
	PersonNameIdentifier PersonNameIdentifier `xml:"personnameidentifier"` 
	TitlesBeforeNames TitlesBeforeNames `xml:"b038"` 
	NamesBeforeKey NamesBeforeKey `xml:"b039"` 
	PrefixToKey PrefixToKey `xml:"b247"` 
	KeyNames KeyNames `xml:"b040"` 
	NamesAfterKey NamesAfterKey `xml:"b041"` 
	SuffixToKey SuffixToKey `xml:"b248"` 
	LettersAfterNames LettersAfterNames `xml:"b042"` 
	TitlesAfterNames TitlesAfterNames `xml:"b043"` 
	PersonNameIdentifier PersonNameIdentifier `xml:"personnameidentifier"` 
	PersonNameIdentifier PersonNameIdentifier `xml:"personnameidentifier"` 
	PersonName PersonName `xml:"b036"` 
	PersonNameInverted PersonNameInverted `xml:"b037"` 
	TitlesBeforeNames TitlesBeforeNames `xml:"b038"` 
	NamesBeforeKey NamesBeforeKey `xml:"b039"` 
	PrefixToKey PrefixToKey `xml:"b247"` 
	KeyNames KeyNames `xml:"b040"` 
	NamesAfterKey NamesAfterKey `xml:"b041"` 
	SuffixToKey SuffixToKey `xml:"b248"` 
	LettersAfterNames LettersAfterNames `xml:"b042"` 
	TitlesAfterNames TitlesAfterNames `xml:"b043"` 
	PersonNameIdentifier PersonNameIdentifier `xml:"personnameidentifier"` 
	TitlesBeforeNames TitlesBeforeNames `xml:"b038"` 
	NamesBeforeKey NamesBeforeKey `xml:"b039"` 
	PrefixToKey PrefixToKey `xml:"b247"` 
	KeyNames KeyNames `xml:"b040"` 
	NamesAfterKey NamesAfterKey `xml:"b041"` 
	SuffixToKey SuffixToKey `xml:"b248"` 
	LettersAfterNames LettersAfterNames `xml:"b042"` 
	TitlesAfterNames TitlesAfterNames `xml:"b043"` 
	PersonNameInverted PersonNameInverted `xml:"b037"` 
	TitlesBeforeNames TitlesBeforeNames `xml:"b038"` 
	NamesBeforeKey NamesBeforeKey `xml:"b039"` 
	PrefixToKey PrefixToKey `xml:"b247"` 
	KeyNames KeyNames `xml:"b040"` 
	NamesAfterKey NamesAfterKey `xml:"b041"` 
	SuffixToKey SuffixToKey `xml:"b248"` 
	LettersAfterNames LettersAfterNames `xml:"b042"` 
	TitlesAfterNames TitlesAfterNames `xml:"b043"` 
	PersonNameIdentifier PersonNameIdentifier `xml:"personnameidentifier"` 
	TitlesBeforeNames TitlesBeforeNames `xml:"b038"` 
	NamesBeforeKey NamesBeforeKey `xml:"b039"` 
	PrefixToKey PrefixToKey `xml:"b247"` 
	KeyNames KeyNames `xml:"b040"` 
	NamesAfterKey NamesAfterKey `xml:"b041"` 
	SuffixToKey SuffixToKey `xml:"b248"` 
	LettersAfterNames LettersAfterNames `xml:"b042"` 
	TitlesAfterNames TitlesAfterNames `xml:"b043"` 
	TitlesBeforeNames TitlesBeforeNames `xml:"b038"` 
	NamesBeforeKey NamesBeforeKey `xml:"b039"` 
	PrefixToKey PrefixToKey `xml:"b247"` 
	KeyNames KeyNames `xml:"b040"` 
	NamesAfterKey NamesAfterKey `xml:"b041"` 
	SuffixToKey SuffixToKey `xml:"b248"` 
	LettersAfterNames LettersAfterNames `xml:"b042"` 
	TitlesAfterNames TitlesAfterNames `xml:"b043"` 
	PersonNameIdentifier PersonNameIdentifier `xml:"personnameidentifier"` 
}

// NewSupplier is not documented.
type NewSupplier struct {
	SupplierIdentifier SupplierIdentifier `xml:"supplieridentifier"` 
	SupplierSAN SupplierSAN `xml:"j136"` 
	SupplierEANLocationNumber SupplierEANLocationNumber `xml:"j135"` 
	SupplierSAN SupplierSAN `xml:"j136"` 
	SupplierName SupplierName `xml:"j137"` 
	SupplierName SupplierName `xml:"j137"` 
	TelephoneNumber TelephoneNumber `xml:"j270"` 
	FaxNumber FaxNumber `xml:"j271"` 
	EmailAddress EmailAddress `xml:"j272"` 
	SupplierIdentifier SupplierIdentifier `xml:"supplieridentifier"` 
	SupplierSAN SupplierSAN `xml:"j136"` 
	SupplierEANLocationNumber SupplierEANLocationNumber `xml:"j135"` 
	SupplierSAN SupplierSAN `xml:"j136"` 
	SupplierName SupplierName `xml:"j137"` 
	SupplierEANLocationNumber SupplierEANLocationNumber `xml:"j135"` 
	SupplierSAN SupplierSAN `xml:"j136"` 
}

// NotForSale is not documented.
type NotForSale struct {
	RightsCountry RightsCountry `xml:"b090"` 
	RightsTerritory RightsTerritory `xml:"b388"` 
	RightsTerritory RightsTerritory `xml:"b388"` 
	ISBN ISBN `xml:"b004"` 
	EAN13 EAN13 `xml:"b005"` 
	ProductIdentifier ProductIdentifier `xml:"productidentifier"` 
	PublisherName PublisherName `xml:"b081"` 
	RightsCountry RightsCountry `xml:"b090"` 
	RightsTerritory RightsTerritory `xml:"b388"` 
}

// OnOrderDetail is not documented.
type OnOrderDetail struct {
	OnOrder OnOrder `xml:"j351"` 
	ExpectedDate ExpectedDate `xml:"j302"` 
}

// OtherText is not documented.
type OtherText struct {
	TextTypeCode TextTypeCode `xml:"d102"` 
	TextFormat TextFormat `xml:"d103"` 
	Text Text `xml:"d104"` 
	TextLinkType TextLinkType `xml:"d105"` 
	TextLink TextLink `xml:"d106"` 
	TextAuthor TextAuthor `xml:"d107"` 
	TextSourceCorporate TextSourceCorporate `xml:"b374"` 
	TextSourceTitle TextSourceTitle `xml:"d108"` 
	TextPublicationDate TextPublicationDate `xml:"d109"` 
	StartDate StartDate `xml:"b324"` 
	EndDate EndDate `xml:"b325"` 
	TextLinkType TextLinkType `xml:"d105"` 
	TextLink TextLink `xml:"d106"` 
	StartDate StartDate `xml:"b324"` 
	EndDate EndDate `xml:"b325"` 
}

// PageRun is not documented.
type PageRun struct {
	FirstPageNumber FirstPageNumber `xml:"b286"` 
	LastPageNumber LastPageNumber `xml:"b287"` 
}

// ParentIdentifier is not documented.
type ParentIdentifier struct {
	SeriesIDType SeriesIDType `xml:"b273"` 
	IDTypeName IDTypeName `xml:"b233"` 
	IDValue IDValue `xml:"b244"` 
}

// PersonAsSubject is not documented.
type PersonAsSubject struct {
	PersonName PersonName `xml:"b036"` 
	PersonNameInverted PersonNameInverted `xml:"b037"` 
	TitlesBeforeNames TitlesBeforeNames `xml:"b038"` 
	NamesBeforeKey NamesBeforeKey `xml:"b039"` 
	PrefixToKey PrefixToKey `xml:"b247"` 
	KeyNames KeyNames `xml:"b040"` 
	NamesAfterKey NamesAfterKey `xml:"b041"` 
	SuffixToKey SuffixToKey `xml:"b248"` 
	LettersAfterNames LettersAfterNames `xml:"b042"` 
	TitlesAfterNames TitlesAfterNames `xml:"b043"` 
	Name Name `xml:"name"` 
	PersonNameIdentifier PersonNameIdentifier `xml:"personnameidentifier"` 
	TitlesBeforeNames TitlesBeforeNames `xml:"b038"` 
	NamesBeforeKey NamesBeforeKey `xml:"b039"` 
	PrefixToKey PrefixToKey `xml:"b247"` 
	KeyNames KeyNames `xml:"b040"` 
	NamesAfterKey NamesAfterKey `xml:"b041"` 
	SuffixToKey SuffixToKey `xml:"b248"` 
	LettersAfterNames LettersAfterNames `xml:"b042"` 
	TitlesAfterNames TitlesAfterNames `xml:"b043"` 
	PersonNameInverted PersonNameInverted `xml:"b037"` 
	TitlesBeforeNames TitlesBeforeNames `xml:"b038"` 
	NamesBeforeKey NamesBeforeKey `xml:"b039"` 
	PrefixToKey PrefixToKey `xml:"b247"` 
	KeyNames KeyNames `xml:"b040"` 
	NamesAfterKey NamesAfterKey `xml:"b041"` 
	SuffixToKey SuffixToKey `xml:"b248"` 
	LettersAfterNames LettersAfterNames `xml:"b042"` 
	TitlesAfterNames TitlesAfterNames `xml:"b043"` 
	Name Name `xml:"name"` 
	PersonNameIdentifier PersonNameIdentifier `xml:"personnameidentifier"` 
	TitlesBeforeNames TitlesBeforeNames `xml:"b038"` 
	NamesBeforeKey NamesBeforeKey `xml:"b039"` 
	PrefixToKey PrefixToKey `xml:"b247"` 
	KeyNames KeyNames `xml:"b040"` 
	NamesAfterKey NamesAfterKey `xml:"b041"` 
	SuffixToKey SuffixToKey `xml:"b248"` 
	LettersAfterNames LettersAfterNames `xml:"b042"` 
	TitlesAfterNames TitlesAfterNames `xml:"b043"` 
	TitlesBeforeNames TitlesBeforeNames `xml:"b038"` 
	NamesBeforeKey NamesBeforeKey `xml:"b039"` 
	PrefixToKey PrefixToKey `xml:"b247"` 
	KeyNames KeyNames `xml:"b040"` 
	NamesAfterKey NamesAfterKey `xml:"b041"` 
	SuffixToKey SuffixToKey `xml:"b248"` 
	LettersAfterNames LettersAfterNames `xml:"b042"` 
	TitlesAfterNames TitlesAfterNames `xml:"b043"` 
	Name Name `xml:"name"` 
	PersonNameIdentifier PersonNameIdentifier `xml:"personnameidentifier"` 
	Name Name `xml:"name"` 
	PersonNameIdentifier PersonNameIdentifier `xml:"personnameidentifier"` 
}

// PersonDate is not documented.
type PersonDate struct {
	PersonDateRole PersonDateRole `xml:"b305"` 
	DateFormat DateFormat `xml:"j260"` 
	Date Date `xml:"b306"` 
}

// PersonNameIdentifier is not documented.
type PersonNameIdentifier struct {
	PersonNameIDType PersonNameIDType `xml:"b390"` 
	IDTypeName IDTypeName `xml:"b233"` 
	IDValue IDValue `xml:"b244"` 
}

// Price is not documented.
type Price struct {
	PriceTypeCode PriceTypeCode `xml:"j148"` 
	PriceQualifier PriceQualifier `xml:"j261"` 
	PriceTypeDescription PriceTypeDescription `xml:"j262"` 
	PricePer PricePer `xml:"j239"` 
	MinimumOrderQuantity MinimumOrderQuantity `xml:"j263"` 
	BatchBonus BatchBonus `xml:"batchbonus"` 
	ClassOfTrade ClassOfTrade `xml:"j149"` 
	BICDiscountGroupCode BICDiscountGroupCode `xml:"j150"` 
	DiscountCoded DiscountCoded `xml:"discountcoded"` 
	DiscountPercent DiscountPercent `xml:"j267"` 
	PriceStatus PriceStatus `xml:"j266"` 
	PriceAmount PriceAmount `xml:"j151"` 
	CurrencyCode CurrencyCode `xml:"j152"` 
	CountryCode CountryCode `xml:"b251"` 
	Territory Territory `xml:"j303"` 
	Territory Territory `xml:"j303"` 
	CountryExcluded CountryExcluded `xml:"j304"` 
	TerritoryExcluded TerritoryExcluded `xml:"j308"` 
	TaxRateCode1 TaxRateCode1 `xml:"j153"` 
	TaxRatePercent1 TaxRatePercent1 `xml:"j154"` 
	TaxableAmount1 TaxableAmount1 `xml:"j155"` 
	TaxAmount1 TaxAmount1 `xml:"j156"` 
	TaxRateCode2 TaxRateCode2 `xml:"j157"` 
	TaxRatePercent2 TaxRatePercent2 `xml:"j158"` 
	TaxableAmount2 TaxableAmount2 `xml:"j159"` 
	TaxAmount2 TaxAmount2 `xml:"j160"` 
	PriceEffectiveFrom PriceEffectiveFrom `xml:"j161"` 
	PriceEffectiveUntil PriceEffectiveUntil `xml:"j162"` 
	CountryCode CountryCode `xml:"b251"` 
	Territory Territory `xml:"j303"` 
	Territory Territory `xml:"j303"` 
	CountryExcluded CountryExcluded `xml:"j304"` 
	TerritoryExcluded TerritoryExcluded `xml:"j308"` 
	CountryCode CountryCode `xml:"b251"` 
	Territory Territory `xml:"j303"` 
	TaxRateCode1 TaxRateCode1 `xml:"j153"` 
	TaxRatePercent1 TaxRatePercent1 `xml:"j154"` 
	TaxableAmount1 TaxableAmount1 `xml:"j155"` 
	TaxAmount1 TaxAmount1 `xml:"j156"` 
	TaxRateCode2 TaxRateCode2 `xml:"j157"` 
	TaxRatePercent2 TaxRatePercent2 `xml:"j158"` 
	TaxableAmount2 TaxableAmount2 `xml:"j159"` 
	TaxAmount2 TaxAmount2 `xml:"j160"` 
	TaxRateCode2 TaxRateCode2 `xml:"j157"` 
	TaxRatePercent2 TaxRatePercent2 `xml:"j158"` 
	TaxableAmount2 TaxableAmount2 `xml:"j159"` 
	TaxAmount2 TaxAmount2 `xml:"j160"` 
}

// Prize is not documented.
type Prize struct {
	PrizeName PrizeName `xml:"g126"` 
	PrizeYear PrizeYear `xml:"g127"` 
	PrizeCountry PrizeCountry `xml:"g128"` 
	PrizeCode PrizeCode `xml:"g129"` 
	PrizeJury PrizeJury `xml:"g343"` 
}

// Product is not documented.
type Product struct {
	RecordReference RecordReference `xml:"a001"` 
	NotificationType NotificationType `xml:"a002"` 
	DeletionCode DeletionCode `xml:"a198"` 
	DeletionText DeletionText `xml:"a199"` 
	RecordSourceType RecordSourceType `xml:"a194"` 
	RecordSourceIdentifierType RecordSourceIdentifierType `xml:"a195"` 
	RecordSourceIdentifier RecordSourceIdentifier `xml:"a196"` 
	RecordSourceName RecordSourceName `xml:"a197"` 
	ISBN ISBN `xml:"b004"` 
	EAN13 EAN13 `xml:"b005"` 
	UPC UPC `xml:"b006"` 
	PublisherProductNo PublisherProductNo `xml:"b007"` 
	ISMN ISMN `xml:"b008"` 
	DOI DOI `xml:"b009"` 
	ProductIdentifier ProductIdentifier `xml:"productidentifier"` 
	EAN13 EAN13 `xml:"b005"` 
	UPC UPC `xml:"b006"` 
	PublisherProductNo PublisherProductNo `xml:"b007"` 
	ISMN ISMN `xml:"b008"` 
	DOI DOI `xml:"b009"` 
	ProductIdentifier ProductIdentifier `xml:"productidentifier"` 
	UPC UPC `xml:"b006"` 
	PublisherProductNo PublisherProductNo `xml:"b007"` 
	ISMN ISMN `xml:"b008"` 
	DOI DOI `xml:"b009"` 
	ProductIdentifier ProductIdentifier `xml:"productidentifier"` 
	PublisherProductNo PublisherProductNo `xml:"b007"` 
	ISMN ISMN `xml:"b008"` 
	DOI DOI `xml:"b009"` 
	ProductIdentifier ProductIdentifier `xml:"productidentifier"` 
	ISMN ISMN `xml:"b008"` 
	DOI DOI `xml:"b009"` 
	ProductIdentifier ProductIdentifier `xml:"productidentifier"` 
	DOI DOI `xml:"b009"` 
	ProductIdentifier ProductIdentifier `xml:"productidentifier"` 
	ProductIdentifier ProductIdentifier `xml:"productidentifier"` 
	Barcode Barcode `xml:"b246"` 
	ReplacesISBN ReplacesISBN `xml:"b010"` 
	ReplacesEAN13 ReplacesEAN13 `xml:"b011"` 
	ProductForm ProductForm `xml:"b012"` 
	ProductFormDetail ProductFormDetail `xml:"b333"` 
	ProductFormFeature ProductFormFeature `xml:"productformfeature"` 
	BookFormDetail BookFormDetail `xml:"b013"` 
	ProductPackaging ProductPackaging `xml:"b225"` 
	ProductFormDescription ProductFormDescription `xml:"b014"` 
	NumberOfPieces NumberOfPieces `xml:"b210"` 
	TradeCategory TradeCategory `xml:"b384"` 
	ProductContentType ProductContentType `xml:"b385"` 
	ContainedItem ContainedItem `xml:"containeditem"` 
	ProductClassification ProductClassification `xml:"productclassification"` 
	EpubType EpubType `xml:"b211"` 
	EpubTypeVersion EpubTypeVersion `xml:"b212"` 
	EpubTypeDescription EpubTypeDescription `xml:"b213"` 
	EpubFormat EpubFormat `xml:"b214"` 
	EpubFormatVersion EpubFormatVersion `xml:"b215"` 
	EpubFormatDescription EpubFormatDescription `xml:"b216"` 
	EpubSource EpubSource `xml:"b278"` 
	EpubSourceVersion EpubSourceVersion `xml:"b279"` 
	EpubSourceDescription EpubSourceDescription `xml:"b280"` 
	EpubTypeNote EpubTypeNote `xml:"b277"` 
	Series Series `xml:"series"` 
	NoSeries NoSeries `xml:"n338"` 
	Set Set `xml:"set"` 
	TextCaseFlag TextCaseFlag `xml:"b027"` 
	DistinctiveTitle DistinctiveTitle `xml:"b028"` 
	TitlePrefix TitlePrefix `xml:"b030"` 
	TitleWithoutPrefix TitleWithoutPrefix `xml:"b031"` 
	TitlePrefix TitlePrefix `xml:"b030"` 
	TitleWithoutPrefix TitleWithoutPrefix `xml:"b031"` 
	Subtitle Subtitle `xml:"b029"` 
	TranslationOfTitle TranslationOfTitle `xml:"b032"` 
	FormerTitle FormerTitle `xml:"b033"` 
	Title Title `xml:"title"` 
	Title Title `xml:"title"` 
	WorkIdentifier WorkIdentifier `xml:"workidentifier"` 
	Website Website `xml:"website"` 
	ThesisType ThesisType `xml:"b368"` 
	ThesisPresentedTo ThesisPresentedTo `xml:"b369"` 
	ThesisYear ThesisYear `xml:"b370"` 
	Contributor Contributor `xml:"contributor"` 
	ContributorStatement ContributorStatement `xml:"b049"` 
	NoContributor NoContributor `xml:"n339"` 
	ConferenceDescription ConferenceDescription `xml:"b050"` 
	ConferenceRole ConferenceRole `xml:"b051"` 
	ConferenceName ConferenceName `xml:"b052"` 
	ConferenceNumber ConferenceNumber `xml:"b053"` 
	ConferenceDate ConferenceDate `xml:"b054"` 
	ConferencePlace ConferencePlace `xml:"b055"` 
	Conference Conference `xml:"conference"` 
	EditionTypeCode EditionTypeCode `xml:"b056"` 
	EditionNumber EditionNumber `xml:"b057"` 
	EditionVersionNumber EditionVersionNumber `xml:"b217"` 
	EditionStatement EditionStatement `xml:"b058"` 
	NoEdition NoEdition `xml:"n386"` 
	ReligiousText ReligiousText `xml:"religioustext"` 
	LanguageOfText LanguageOfText `xml:"b059"` 
	OriginalLanguage OriginalLanguage `xml:"b060"` 
	Language Language `xml:"language"` 
	NumberOfPages NumberOfPages `xml:"b061"` 
	PagesRoman PagesRoman `xml:"b254"` 
	PagesArabic PagesArabic `xml:"b255"` 
	Extent Extent `xml:"extent"` 
	NumberOfIllustrations NumberOfIllustrations `xml:"b125"` 
	IllustrationsNote IllustrationsNote `xml:"b062"` 
	Illustrations Illustrations `xml:"illustrations"` 
	MapScale MapScale `xml:"b063"` 
	BASICMainSubject BASICMainSubject `xml:"b064"` 
	BASICVersion BASICVersion `xml:"b200"` 
	BICMainSubject BICMainSubject `xml:"b065"` 
	BICVersion BICVersion `xml:"b066"` 
	MainSubject MainSubject `xml:"mainsubject"` 
	Subject Subject `xml:"subject"` 
	PersonAsSubject PersonAsSubject `xml:"personassubject"` 
	CorporateBodyAsSubject CorporateBodyAsSubject `xml:"b071"` 
	PlaceAsSubject PlaceAsSubject `xml:"b072"` 
	AudienceCode AudienceCode `xml:"b073"` 
	Audience Audience `xml:"audience"` 
	USSchoolGrade USSchoolGrade `xml:"b189"` 
	InterestAge InterestAge `xml:"b190"` 
	AudienceRange AudienceRange `xml:"audiencerange"` 
	AudienceDescription AudienceDescription `xml:"b207"` 
	Complexity Complexity `xml:"complexity"` 
	Annotation Annotation `xml:"d100"` 
	MainDescription MainDescription `xml:"d101"` 
	OtherText OtherText `xml:"othertext"` 
	ReviewQuote ReviewQuote `xml:"e110"` 
	CoverImageFormatCode CoverImageFormatCode `xml:"f111"` 
	CoverImageLinkTypeCode CoverImageLinkTypeCode `xml:"f112"` 
	CoverImageLink CoverImageLink `xml:"f113"` 
	MediaFile MediaFile `xml:"mediafile"` 
	ProductWebsite ProductWebsite `xml:"productwebsite"` 
	PrizesDescription PrizesDescription `xml:"g124"` 
	Prize Prize `xml:"prize"` 
	ContentItem ContentItem `xml:"contentitem"` 
	ImprintName ImprintName `xml:"b079"` 
	Imprint Imprint `xml:"imprint"` 
	PublisherName PublisherName `xml:"b081"` 
	Publisher Publisher `xml:"publisher"` 
	Imprint Imprint `xml:"imprint"` 
	PublisherName PublisherName `xml:"b081"` 
	Publisher Publisher `xml:"publisher"` 
	PublisherName PublisherName `xml:"b081"` 
	Publisher Publisher `xml:"publisher"` 
	Publisher Publisher `xml:"publisher"` 
	CityOfPublication CityOfPublication `xml:"b209"` 
	CountryOfPublication CountryOfPublication `xml:"b083"` 
	CopublisherName CopublisherName `xml:"b084"` 
	SponsorName SponsorName `xml:"b085"` 
	OriginalPublisher OriginalPublisher `xml:"b240"` 
	PublishingStatus PublishingStatus `xml:"b394"` 
	PublishingStatusNote PublishingStatusNote `xml:"b395"` 
	AnnouncementDate AnnouncementDate `xml:"b086"` 
	TradeAnnouncementDate TradeAnnouncementDate `xml:"b362"` 
	PublicationDate PublicationDate `xml:"b003"` 
	CopyrightStatement CopyrightStatement `xml:"copyrightstatement"` 
	CopyrightYear CopyrightYear `xml:"b087"` 
	YearFirstPublished YearFirstPublished `xml:"b088"` 
	SalesRights SalesRights `xml:"salesrights"` 
	NotForSale NotForSale `xml:"notforsale"` 
	SalesRestriction SalesRestriction `xml:"salesrestriction"` 
	Height Height `xml:"c096"` 
	Width Width `xml:"c097"` 
	Thickness Thickness `xml:"c098"` 
	Weight Weight `xml:"c099"` 
	Weight Weight `xml:"c099"` 
	Measure Measure `xml:"measure"` 
	Dimensions Dimensions `xml:"c258"` 
	Dimensions Dimensions `xml:"c258"` 
	ReplacedByISBN ReplacedByISBN `xml:"h130"` 
	ReplacedByEAN13 ReplacedByEAN13 `xml:"h131"` 
	AlternativeFormatISBN AlternativeFormatISBN `xml:"h132"` 
	AlternativeFormatEAN13 AlternativeFormatEAN13 `xml:"h133"` 
	AlternativeProductISBN AlternativeProductISBN `xml:"h163"` 
	AlternativeProductEAN13 AlternativeProductEAN13 `xml:"h164"` 
	RelatedProduct RelatedProduct `xml:"relatedproduct"` 
	OutOfPrintDate OutOfPrintDate `xml:"h134"` 
	SupplyDetail SupplyDetail `xml:"supplydetail"` 
	MarketRepresentation MarketRepresentation `xml:"marketrepresentation"` 
	PromotionCampaign PromotionCampaign `xml:"k165"` 
	PromotionContact PromotionContact `xml:"k166"` 
	InitialPrintRun InitialPrintRun `xml:"k167"` 
	ReprintDetail ReprintDetail `xml:"k309"` 
	CopiesSold CopiesSold `xml:"k168"` 
	BookClubAdoption BookClubAdoption `xml:"k169"` 
	RecordSourceIdentifierType RecordSourceIdentifierType `xml:"a195"` 
	RecordSourceIdentifier RecordSourceIdentifier `xml:"a196"` 
	ISBN ISBN `xml:"b004"` 
	EAN13 EAN13 `xml:"b005"` 
	UPC UPC `xml:"b006"` 
	PublisherProductNo PublisherProductNo `xml:"b007"` 
	ISMN ISMN `xml:"b008"` 
	DOI DOI `xml:"b009"` 
	ProductIdentifier ProductIdentifier `xml:"productidentifier"` 
	EAN13 EAN13 `xml:"b005"` 
	UPC UPC `xml:"b006"` 
	PublisherProductNo PublisherProductNo `xml:"b007"` 
	ISMN ISMN `xml:"b008"` 
	DOI DOI `xml:"b009"` 
	ProductIdentifier ProductIdentifier `xml:"productidentifier"` 
	UPC UPC `xml:"b006"` 
	PublisherProductNo PublisherProductNo `xml:"b007"` 
	ISMN ISMN `xml:"b008"` 
	DOI DOI `xml:"b009"` 
	ProductIdentifier ProductIdentifier `xml:"productidentifier"` 
	PublisherProductNo PublisherProductNo `xml:"b007"` 
	ISMN ISMN `xml:"b008"` 
	DOI DOI `xml:"b009"` 
	ProductIdentifier ProductIdentifier `xml:"productidentifier"` 
	ISMN ISMN `xml:"b008"` 
	DOI DOI `xml:"b009"` 
	ProductIdentifier ProductIdentifier `xml:"productidentifier"` 
	DOI DOI `xml:"b009"` 
	ProductIdentifier ProductIdentifier `xml:"productidentifier"` 
	ProductIdentifier ProductIdentifier `xml:"productidentifier"` 
	Barcode Barcode `xml:"b246"` 
	ReplacesISBN ReplacesISBN `xml:"b010"` 
	ReplacesEAN13 ReplacesEAN13 `xml:"b011"` 
	ProductForm ProductForm `xml:"b012"` 
	ProductFormDetail ProductFormDetail `xml:"b333"` 
	ProductFormFeature ProductFormFeature `xml:"productformfeature"` 
	BookFormDetail BookFormDetail `xml:"b013"` 
	ProductPackaging ProductPackaging `xml:"b225"` 
	ProductFormDescription ProductFormDescription `xml:"b014"` 
	NumberOfPieces NumberOfPieces `xml:"b210"` 
	TradeCategory TradeCategory `xml:"b384"` 
	ProductContentType ProductContentType `xml:"b385"` 
	ContainedItem ContainedItem `xml:"containeditem"` 
	ProductClassification ProductClassification `xml:"productclassification"` 
	EpubType EpubType `xml:"b211"` 
	EpubTypeVersion EpubTypeVersion `xml:"b212"` 
	EpubTypeDescription EpubTypeDescription `xml:"b213"` 
	EpubFormat EpubFormat `xml:"b214"` 
	EpubFormatVersion EpubFormatVersion `xml:"b215"` 
	EpubFormatDescription EpubFormatDescription `xml:"b216"` 
	EpubSource EpubSource `xml:"b278"` 
	EpubSourceVersion EpubSourceVersion `xml:"b279"` 
	EpubSourceDescription EpubSourceDescription `xml:"b280"` 
	EpubTypeNote EpubTypeNote `xml:"b277"` 
	Series Series `xml:"series"` 
	NoSeries NoSeries `xml:"n338"` 
	Set Set `xml:"set"` 
	TextCaseFlag TextCaseFlag `xml:"b027"` 
	DistinctiveTitle DistinctiveTitle `xml:"b028"` 
	TitlePrefix TitlePrefix `xml:"b030"` 
	TitleWithoutPrefix TitleWithoutPrefix `xml:"b031"` 
	TitlePrefix TitlePrefix `xml:"b030"` 
	TitleWithoutPrefix TitleWithoutPrefix `xml:"b031"` 
	Subtitle Subtitle `xml:"b029"` 
	TranslationOfTitle TranslationOfTitle `xml:"b032"` 
	FormerTitle FormerTitle `xml:"b033"` 
	Title Title `xml:"title"` 
	Title Title `xml:"title"` 
	WorkIdentifier WorkIdentifier `xml:"workidentifier"` 
	Website Website `xml:"website"` 
	ThesisType ThesisType `xml:"b368"` 
	ThesisPresentedTo ThesisPresentedTo `xml:"b369"` 
	ThesisYear ThesisYear `xml:"b370"` 
	Contributor Contributor `xml:"contributor"` 
	ContributorStatement ContributorStatement `xml:"b049"` 
	NoContributor NoContributor `xml:"n339"` 
	ConferenceDescription ConferenceDescription `xml:"b050"` 
	ConferenceRole ConferenceRole `xml:"b051"` 
	ConferenceName ConferenceName `xml:"b052"` 
	ConferenceNumber ConferenceNumber `xml:"b053"` 
	ConferenceDate ConferenceDate `xml:"b054"` 
	ConferencePlace ConferencePlace `xml:"b055"` 
	Conference Conference `xml:"conference"` 
	EditionTypeCode EditionTypeCode `xml:"b056"` 
	EditionNumber EditionNumber `xml:"b057"` 
	EditionVersionNumber EditionVersionNumber `xml:"b217"` 
	EditionStatement EditionStatement `xml:"b058"` 
	NoEdition NoEdition `xml:"n386"` 
	ReligiousText ReligiousText `xml:"religioustext"` 
	LanguageOfText LanguageOfText `xml:"b059"` 
	OriginalLanguage OriginalLanguage `xml:"b060"` 
	Language Language `xml:"language"` 
	NumberOfPages NumberOfPages `xml:"b061"` 
	PagesRoman PagesRoman `xml:"b254"` 
	PagesArabic PagesArabic `xml:"b255"` 
	Extent Extent `xml:"extent"` 
	NumberOfIllustrations NumberOfIllustrations `xml:"b125"` 
	IllustrationsNote IllustrationsNote `xml:"b062"` 
	Illustrations Illustrations `xml:"illustrations"` 
	MapScale MapScale `xml:"b063"` 
	BASICMainSubject BASICMainSubject `xml:"b064"` 
	BASICVersion BASICVersion `xml:"b200"` 
	BICMainSubject BICMainSubject `xml:"b065"` 
	BICVersion BICVersion `xml:"b066"` 
	MainSubject MainSubject `xml:"mainsubject"` 
	Subject Subject `xml:"subject"` 
	PersonAsSubject PersonAsSubject `xml:"personassubject"` 
	CorporateBodyAsSubject CorporateBodyAsSubject `xml:"b071"` 
	PlaceAsSubject PlaceAsSubject `xml:"b072"` 
	AudienceCode AudienceCode `xml:"b073"` 
	Audience Audience `xml:"audience"` 
	USSchoolGrade USSchoolGrade `xml:"b189"` 
	InterestAge InterestAge `xml:"b190"` 
	AudienceRange AudienceRange `xml:"audiencerange"` 
	AudienceDescription AudienceDescription `xml:"b207"` 
	Complexity Complexity `xml:"complexity"` 
	Annotation Annotation `xml:"d100"` 
	MainDescription MainDescription `xml:"d101"` 
	OtherText OtherText `xml:"othertext"` 
	ReviewQuote ReviewQuote `xml:"e110"` 
	CoverImageFormatCode CoverImageFormatCode `xml:"f111"` 
	CoverImageLinkTypeCode CoverImageLinkTypeCode `xml:"f112"` 
	CoverImageLink CoverImageLink `xml:"f113"` 
	MediaFile MediaFile `xml:"mediafile"` 
	ProductWebsite ProductWebsite `xml:"productwebsite"` 
	PrizesDescription PrizesDescription `xml:"g124"` 
	Prize Prize `xml:"prize"` 
	ContentItem ContentItem `xml:"contentitem"` 
	ImprintName ImprintName `xml:"b079"` 
	Imprint Imprint `xml:"imprint"` 
	PublisherName PublisherName `xml:"b081"` 
	Publisher Publisher `xml:"publisher"` 
	Imprint Imprint `xml:"imprint"` 
	PublisherName PublisherName `xml:"b081"` 
	Publisher Publisher `xml:"publisher"` 
	PublisherName PublisherName `xml:"b081"` 
	Publisher Publisher `xml:"publisher"` 
	Publisher Publisher `xml:"publisher"` 
	CityOfPublication CityOfPublication `xml:"b209"` 
	CountryOfPublication CountryOfPublication `xml:"b083"` 
	CopublisherName CopublisherName `xml:"b084"` 
	SponsorName SponsorName `xml:"b085"` 
	OriginalPublisher OriginalPublisher `xml:"b240"` 
	PublishingStatus PublishingStatus `xml:"b394"` 
	PublishingStatusNote PublishingStatusNote `xml:"b395"` 
	AnnouncementDate AnnouncementDate `xml:"b086"` 
	TradeAnnouncementDate TradeAnnouncementDate `xml:"b362"` 
	PublicationDate PublicationDate `xml:"b003"` 
	CopyrightStatement CopyrightStatement `xml:"copyrightstatement"` 
	CopyrightYear CopyrightYear `xml:"b087"` 
	YearFirstPublished YearFirstPublished `xml:"b088"` 
	SalesRights SalesRights `xml:"salesrights"` 
	NotForSale NotForSale `xml:"notforsale"` 
	SalesRestriction SalesRestriction `xml:"salesrestriction"` 
	ISBN ISBN `xml:"b004"` 
	EAN13 EAN13 `xml:"b005"` 
	UPC UPC `xml:"b006"` 
	PublisherProductNo PublisherProductNo `xml:"b007"` 
	ISMN ISMN `xml:"b008"` 
	DOI DOI `xml:"b009"` 
	ProductIdentifier ProductIdentifier `xml:"productidentifier"` 
	EAN13 EAN13 `xml:"b005"` 
	UPC UPC `xml:"b006"` 
	PublisherProductNo PublisherProductNo `xml:"b007"` 
	ISMN ISMN `xml:"b008"` 
	DOI DOI `xml:"b009"` 
	ProductIdentifier ProductIdentifier `xml:"productidentifier"` 
	UPC UPC `xml:"b006"` 
	PublisherProductNo PublisherProductNo `xml:"b007"` 
	ISMN ISMN `xml:"b008"` 
	DOI DOI `xml:"b009"` 
	ProductIdentifier ProductIdentifier `xml:"productidentifier"` 
	PublisherProductNo PublisherProductNo `xml:"b007"` 
	ISMN ISMN `xml:"b008"` 
	DOI DOI `xml:"b009"` 
	ProductIdentifier ProductIdentifier `xml:"productidentifier"` 
	ISMN ISMN `xml:"b008"` 
	DOI DOI `xml:"b009"` 
	ProductIdentifier ProductIdentifier `xml:"productidentifier"` 
	DOI DOI `xml:"b009"` 
	ProductIdentifier ProductIdentifier `xml:"productidentifier"` 
	EpubType EpubType `xml:"b211"` 
	EpubTypeVersion EpubTypeVersion `xml:"b212"` 
	EpubTypeDescription EpubTypeDescription `xml:"b213"` 
	EpubFormat EpubFormat `xml:"b214"` 
	EpubFormatVersion EpubFormatVersion `xml:"b215"` 
	EpubFormatDescription EpubFormatDescription `xml:"b216"` 
	EpubSource EpubSource `xml:"b278"` 
	EpubSourceVersion EpubSourceVersion `xml:"b279"` 
	EpubSourceDescription EpubSourceDescription `xml:"b280"` 
	EpubTypeNote EpubTypeNote `xml:"b277"` 
	EpubFormat EpubFormat `xml:"b214"` 
	EpubFormatVersion EpubFormatVersion `xml:"b215"` 
	EpubSource EpubSource `xml:"b278"` 
	EpubSourceVersion EpubSourceVersion `xml:"b279"` 
	DistinctiveTitle DistinctiveTitle `xml:"b028"` 
	TitlePrefix TitlePrefix `xml:"b030"` 
	TitleWithoutPrefix TitleWithoutPrefix `xml:"b031"` 
	TitlePrefix TitlePrefix `xml:"b030"` 
	TitleWithoutPrefix TitleWithoutPrefix `xml:"b031"` 
	Subtitle Subtitle `xml:"b029"` 
	TranslationOfTitle TranslationOfTitle `xml:"b032"` 
	FormerTitle FormerTitle `xml:"b033"` 
	Title Title `xml:"title"` 
	DistinctiveTitle DistinctiveTitle `xml:"b028"` 
	TitlePrefix TitlePrefix `xml:"b030"` 
	TitleWithoutPrefix TitleWithoutPrefix `xml:"b031"` 
	TitlePrefix TitlePrefix `xml:"b030"` 
	TitleWithoutPrefix TitleWithoutPrefix `xml:"b031"` 
	TitlePrefix TitlePrefix `xml:"b030"` 
	TitleWithoutPrefix TitleWithoutPrefix `xml:"b031"` 
	ThesisType ThesisType `xml:"b368"` 
	ThesisPresentedTo ThesisPresentedTo `xml:"b369"` 
	ThesisYear ThesisYear `xml:"b370"` 
	Contributor Contributor `xml:"contributor"` 
	ContributorStatement ContributorStatement `xml:"b049"` 
	ConferenceRole ConferenceRole `xml:"b051"` 
	ConferenceName ConferenceName `xml:"b052"` 
	ConferenceNumber ConferenceNumber `xml:"b053"` 
	ConferenceDate ConferenceDate `xml:"b054"` 
	ConferencePlace ConferencePlace `xml:"b055"` 
	EditionTypeCode EditionTypeCode `xml:"b056"` 
	EditionNumber EditionNumber `xml:"b057"` 
	EditionVersionNumber EditionVersionNumber `xml:"b217"` 
	EditionStatement EditionStatement `xml:"b058"` 
	BASICMainSubject BASICMainSubject `xml:"b064"` 
	BASICVersion BASICVersion `xml:"b200"` 
	BICMainSubject BICMainSubject `xml:"b065"` 
	BICVersion BICVersion `xml:"b066"` 
	CoverImageFormatCode CoverImageFormatCode `xml:"f111"` 
	CoverImageLinkTypeCode CoverImageLinkTypeCode `xml:"f112"` 
	CoverImageLink CoverImageLink `xml:"f113"` 
	ImprintName ImprintName `xml:"b079"` 
	Imprint Imprint `xml:"imprint"` 
	PublisherName PublisherName `xml:"b081"` 
	Publisher Publisher `xml:"publisher"` 
	Imprint Imprint `xml:"imprint"` 
	PublisherName PublisherName `xml:"b081"` 
	Publisher Publisher `xml:"publisher"` 
	PublisherName PublisherName `xml:"b081"` 
	Publisher Publisher `xml:"publisher"` 
	PublishingStatus PublishingStatus `xml:"b394"` 
	PublishingStatusNote PublishingStatusNote `xml:"b395"` 
	Height Height `xml:"c096"` 
	Width Width `xml:"c097"` 
	Thickness Thickness `xml:"c098"` 
	Weight Weight `xml:"c099"` 
	Weight Weight `xml:"c099"` 
	Measure Measure `xml:"measure"` 
	Dimensions Dimensions `xml:"c258"` 
	Height Height `xml:"c096"` 
	Width Width `xml:"c097"` 
	Thickness Thickness `xml:"c098"` 
	Weight Weight `xml:"c099"` 
	Height Height `xml:"c096"` 
	Width Width `xml:"c097"` 
	Thickness Thickness `xml:"c098"` 
}

// ProductClassification is not documented.
type ProductClassification struct {
	ProductClassificationType ProductClassificationType `xml:"b274"` 
	ProductClassificationCode ProductClassificationCode `xml:"b275"` 
	Percent Percent `xml:"b337"` 
}

// ProductFormFeature is not documented.
type ProductFormFeature struct {
	ProductFormFeatureType ProductFormFeatureType `xml:"b334"` 
	ProductFormFeatureValue ProductFormFeatureValue `xml:"b335"` 
	ProductFormFeatureDescription ProductFormFeatureDescription `xml:"b336"` 
}

// ProductIdentifier is not documented.
type ProductIdentifier struct {
	ProductIDType ProductIDType `xml:"b221"` 
	IDTypeName IDTypeName `xml:"b233"` 
	IDValue IDValue `xml:"b244"` 
}

// ProductWebsite is not documented.
type ProductWebsite struct {
	WebsiteRole WebsiteRole `xml:"b367"` 
	ProductWebsiteDescription ProductWebsiteDescription `xml:"f170"` 
	ProductWebsiteLink ProductWebsiteLink `xml:"f123"` 
}

// ProfessionalAffiliation is not documented.
type ProfessionalAffiliation struct {
	ProfessionalPosition ProfessionalPosition `xml:"b045"` 
	Affiliation Affiliation `xml:"b046"` 
}

// Publisher is not documented.
type Publisher struct {
	PublishingRole PublishingRole `xml:"b291"` 
	PublisherName PublisherName `xml:"b081"` 
	NameCodeType NameCodeType `xml:"b241"` 
	NameCodeTypeName NameCodeTypeName `xml:"b242"` 
	NameCodeValue NameCodeValue `xml:"b243"` 
	PublisherName PublisherName `xml:"b081"` 
	Website Website `xml:"website"` 
	NameCodeType NameCodeType `xml:"b241"` 
	NameCodeTypeName NameCodeTypeName `xml:"b242"` 
	NameCodeValue NameCodeValue `xml:"b243"` 
	PublisherName PublisherName `xml:"b081"` 
}

// Reissue is not documented.
type Reissue struct {
	ReissueDate ReissueDate `xml:"j365"` 
	ReissueDescription ReissueDescription `xml:"j366"` 
	Price Price `xml:"price"` 
	MediaFile MediaFile `xml:"mediafile"` 
}

// RelatedProduct is not documented.
type RelatedProduct struct {
	RelationCode RelationCode `xml:"h208"` 
	ISBN ISBN `xml:"b004"` 
	EAN13 EAN13 `xml:"b005"` 
	ProductIdentifier ProductIdentifier `xml:"productidentifier"` 
	Website Website `xml:"website"` 
	ProductForm ProductForm `xml:"b012"` 
	ProductFormDetail ProductFormDetail `xml:"b333"` 
	ProductFormFeature ProductFormFeature `xml:"productformfeature"` 
	BookFormDetail BookFormDetail `xml:"b013"` 
	ProductPackaging ProductPackaging `xml:"b225"` 
	ProductFormDescription ProductFormDescription `xml:"b014"` 
	EAN13 EAN13 `xml:"b005"` 
	ProductIdentifier ProductIdentifier `xml:"productidentifier"` 
	Website Website `xml:"website"` 
	ProductForm ProductForm `xml:"b012"` 
	ProductFormDetail ProductFormDetail `xml:"b333"` 
	ProductFormFeature ProductFormFeature `xml:"productformfeature"` 
	BookFormDetail BookFormDetail `xml:"b013"` 
	ProductPackaging ProductPackaging `xml:"b225"` 
	ProductFormDescription ProductFormDescription `xml:"b014"` 
	ProductIdentifier ProductIdentifier `xml:"productidentifier"` 
	Website Website `xml:"website"` 
	ProductForm ProductForm `xml:"b012"` 
	ProductFormDetail ProductFormDetail `xml:"b333"` 
	ProductFormFeature ProductFormFeature `xml:"productformfeature"` 
	BookFormDetail BookFormDetail `xml:"b013"` 
	ProductPackaging ProductPackaging `xml:"b225"` 
	ProductFormDescription ProductFormDescription `xml:"b014"` 
	Website Website `xml:"website"` 
	ProductForm ProductForm `xml:"b012"` 
	ProductFormDetail ProductFormDetail `xml:"b333"` 
	ProductFormFeature ProductFormFeature `xml:"productformfeature"` 
	BookFormDetail BookFormDetail `xml:"b013"` 
	ProductPackaging ProductPackaging `xml:"b225"` 
	ProductFormDescription ProductFormDescription `xml:"b014"` 
	NumberOfPieces NumberOfPieces `xml:"b210"` 
	TradeCategory TradeCategory `xml:"b384"` 
	ProductContentType ProductContentType `xml:"b385"` 
	EpubType EpubType `xml:"b211"` 
	EpubTypeVersion EpubTypeVersion `xml:"b212"` 
	EpubTypeDescription EpubTypeDescription `xml:"b213"` 
	EpubFormat EpubFormat `xml:"b214"` 
	EpubFormatVersion EpubFormatVersion `xml:"b215"` 
	EpubFormatDescription EpubFormatDescription `xml:"b216"` 
	EpubTypeNote EpubTypeNote `xml:"b277"` 
	Publisher Publisher `xml:"publisher"` 
	ISBN ISBN `xml:"b004"` 
	EAN13 EAN13 `xml:"b005"` 
	ProductIdentifier ProductIdentifier `xml:"productidentifier"` 
	Website Website `xml:"website"` 
	ProductForm ProductForm `xml:"b012"` 
	ProductFormDetail ProductFormDetail `xml:"b333"` 
	ProductFormFeature ProductFormFeature `xml:"productformfeature"` 
	BookFormDetail BookFormDetail `xml:"b013"` 
	ProductPackaging ProductPackaging `xml:"b225"` 
	ProductFormDescription ProductFormDescription `xml:"b014"` 
	ProductForm ProductForm `xml:"b012"` 
	ProductFormDetail ProductFormDetail `xml:"b333"` 
	ProductFormFeature ProductFormFeature `xml:"productformfeature"` 
	BookFormDetail BookFormDetail `xml:"b013"` 
	ProductPackaging ProductPackaging `xml:"b225"` 
	ProductFormDescription ProductFormDescription `xml:"b014"` 
	EAN13 EAN13 `xml:"b005"` 
	ProductIdentifier ProductIdentifier `xml:"productidentifier"` 
	Website Website `xml:"website"` 
	ProductForm ProductForm `xml:"b012"` 
	ProductFormDetail ProductFormDetail `xml:"b333"` 
	ProductFormFeature ProductFormFeature `xml:"productformfeature"` 
	BookFormDetail BookFormDetail `xml:"b013"` 
	ProductPackaging ProductPackaging `xml:"b225"` 
	ProductFormDescription ProductFormDescription `xml:"b014"` 
	ProductForm ProductForm `xml:"b012"` 
	ProductFormDetail ProductFormDetail `xml:"b333"` 
	ProductFormFeature ProductFormFeature `xml:"productformfeature"` 
	BookFormDetail BookFormDetail `xml:"b013"` 
	ProductPackaging ProductPackaging `xml:"b225"` 
	ProductFormDescription ProductFormDescription `xml:"b014"` 
	ProductIdentifier ProductIdentifier `xml:"productidentifier"` 
	Website Website `xml:"website"` 
	ProductForm ProductForm `xml:"b012"` 
	ProductFormDetail ProductFormDetail `xml:"b333"` 
	ProductFormFeature ProductFormFeature `xml:"productformfeature"` 
	BookFormDetail BookFormDetail `xml:"b013"` 
	ProductPackaging ProductPackaging `xml:"b225"` 
	ProductFormDescription ProductFormDescription `xml:"b014"` 
	ProductForm ProductForm `xml:"b012"` 
	ProductFormDetail ProductFormDetail `xml:"b333"` 
	ProductFormFeature ProductFormFeature `xml:"productformfeature"` 
	BookFormDetail BookFormDetail `xml:"b013"` 
	ProductPackaging ProductPackaging `xml:"b225"` 
	ProductFormDescription ProductFormDescription `xml:"b014"` 
	Website Website `xml:"website"` 
	ProductForm ProductForm `xml:"b012"` 
	ProductFormDetail ProductFormDetail `xml:"b333"` 
	ProductFormFeature ProductFormFeature `xml:"productformfeature"` 
	BookFormDetail BookFormDetail `xml:"b013"` 
	ProductPackaging ProductPackaging `xml:"b225"` 
	ProductFormDescription ProductFormDescription `xml:"b014"` 
	EpubType EpubType `xml:"b211"` 
	EpubTypeVersion EpubTypeVersion `xml:"b212"` 
	EpubTypeDescription EpubTypeDescription `xml:"b213"` 
	EpubFormat EpubFormat `xml:"b214"` 
	EpubFormatVersion EpubFormatVersion `xml:"b215"` 
	EpubFormatDescription EpubFormatDescription `xml:"b216"` 
	EpubTypeNote EpubTypeNote `xml:"b277"` 
	EpubFormat EpubFormat `xml:"b214"` 
	EpubFormatVersion EpubFormatVersion `xml:"b215"` 
}

// ReligiousText is not documented.
type ReligiousText struct {
	ReligiousTextID ReligiousTextID `xml:"b376"` 
	ReligiousTextFeature ReligiousTextFeature `xml:"religioustextfeature"` 
}

// ReligiousTextFeature is not documented.
type ReligiousTextFeature struct {
	ReligiousTextFeatureType ReligiousTextFeatureType `xml:"b358"` 
	ReligiousTextFeatureCode ReligiousTextFeatureCode `xml:"b359"` 
	ReligiousTextFeatureDescription ReligiousTextFeatureDescription `xml:"b360"` 
}

// SalesOutlet is not documented.
type SalesOutlet struct {
	SalesOutletIdentifier SalesOutletIdentifier `xml:"salesoutletidentifier"` 
	SalesOutletName SalesOutletName `xml:"b382"` 
}

// SalesOutletIdentifier is not documented.
type SalesOutletIdentifier struct {
	SalesOutletIDType SalesOutletIDType `xml:"b393"` 
	IDTypeName IDTypeName `xml:"b233"` 
	IDValue IDValue `xml:"b244"` 
}

// SalesRestriction is not documented.
type SalesRestriction struct {
	SalesRestrictionType SalesRestrictionType `xml:"b381"` 
	SalesOutlet SalesOutlet `xml:"salesoutlet"` 
	SalesRestrictionDetail SalesRestrictionDetail `xml:"b383"` 
}

// SalesRights is not documented.
type SalesRights struct {
	SalesRightsType SalesRightsType `xml:"b089"` 
	RightsCountry RightsCountry `xml:"b090"` 
	RightsTerritory RightsTerritory `xml:"b388"` 
	RightsRegion RightsRegion `xml:"b091"` 
	RightsTerritory RightsTerritory `xml:"b388"` 
	RightsRegion RightsRegion `xml:"b091"` 
	RightsCountry RightsCountry `xml:"b090"` 
	RightsTerritory RightsTerritory `xml:"b388"` 
	RightsRegion RightsRegion `xml:"b091"` 
}

// SenderIdentifier is not documented.
type SenderIdentifier struct {
	SenderIDType SenderIDType `xml:"m379"` 
	IDTypeName IDTypeName `xml:"b233"` 
	IDValue IDValue `xml:"b244"` 
}

// Series is not documented.
type Series struct {
	SeriesISSN SeriesISSN `xml:"b016"` 
	PublisherSeriesCode PublisherSeriesCode `xml:"b017"` 
	SeriesIdentifier SeriesIdentifier `xml:"seriesidentifier"` 
	TitleOfSeries TitleOfSeries `xml:"b018"` 
	Title Title `xml:"title"` 
	Title Title `xml:"title"` 
	Contributor Contributor `xml:"contributor"` 
	NumberWithinSeries NumberWithinSeries `xml:"b019"` 
	YearOfAnnual YearOfAnnual `xml:"b020"` 
	TitleOfSeries TitleOfSeries `xml:"b018"` 
	Title Title `xml:"title"` 
}

// SeriesIdentifier is not documented.
type SeriesIdentifier struct {
	SeriesIDType SeriesIDType `xml:"b273"` 
	IDTypeName IDTypeName `xml:"b233"` 
	IDValue IDValue `xml:"b244"` 
}

// Set is not documented.
type Set struct {
	ISBNOfSet ISBNOfSet `xml:"b021"` 
	EAN13OfSet EAN13OfSet `xml:"b022"` 
	ProductIdentifier ProductIdentifier `xml:"productidentifier"` 
	TitleOfSet TitleOfSet `xml:"b023"` 
	Title Title `xml:"title"` 
	Title Title `xml:"title"` 
	SetPartNumber SetPartNumber `xml:"b024"` 
	SetPartTitle SetPartTitle `xml:"b025"` 
	ItemNumberWithinSet ItemNumberWithinSet `xml:"b026"` 
	LevelSequenceNumber LevelSequenceNumber `xml:"b284"` 
	SetItemTitle SetItemTitle `xml:"b281"` 
	TitleOfSet TitleOfSet `xml:"b023"` 
	Title Title `xml:"title"` 
}

// Stock is not documented.
type Stock struct {
	LocationIdentifier LocationIdentifier `xml:"locationidentifier"` 
	LocationName LocationName `xml:"j349"` 
	StockQuantityCoded StockQuantityCoded `xml:"stockquantitycoded"` 
	OnHand OnHand `xml:"j350"` 
	OnHand OnHand `xml:"j350"` 
	OnOrder OnOrder `xml:"j351"` 
	CBO CBO `xml:"j375"` 
	OnOrderDetail OnOrderDetail `xml:"onorderdetail"` 
	StockQuantityCoded StockQuantityCoded `xml:"stockquantitycoded"` 
	OnHand OnHand `xml:"j350"` 
}

// StockQuantityCoded is not documented.
type StockQuantityCoded struct {
	StockQuantityCodeType StockQuantityCodeType `xml:"j293"` 
	StockQuantityCodeTypeName StockQuantityCodeTypeName `xml:"j296"` 
	StockQuantityCode StockQuantityCode `xml:"j297"` 
}

// SubSeriesRecord is not documented.
type SubSeriesRecord struct {
	RecordReference RecordReference `xml:"a001"` 
	NotificationType NotificationType `xml:"a002"` 
	DeletionCode DeletionCode `xml:"a198"` 
	DeletionText DeletionText `xml:"a199"` 
	RecordSourceType RecordSourceType `xml:"a194"` 
	RecordSourceIdentifierType RecordSourceIdentifierType `xml:"a195"` 
	RecordSourceIdentifier RecordSourceIdentifier `xml:"a196"` 
	RecordSourceName RecordSourceName `xml:"a197"` 
	SeriesIdentifier SeriesIdentifier `xml:"seriesidentifier"` 
	ParentIdentifier ParentIdentifier `xml:"parentidentifier"` 
	LevelSequenceNumber LevelSequenceNumber `xml:"b284"` 
	SeriesPartName SeriesPartName `xml:"b282"` 
	NumberWithinSeries NumberWithinSeries `xml:"b019"` 
	Title Title `xml:"title"` 
	Contributor Contributor `xml:"contributor"` 
	OtherText OtherText `xml:"othertext"` 
	Publisher Publisher `xml:"publisher"` 
	SubordinateEntries SubordinateEntries `xml:"a245"` 
	RecordSourceIdentifierType RecordSourceIdentifierType `xml:"a195"` 
	RecordSourceIdentifier RecordSourceIdentifier `xml:"a196"` 
	SeriesPartName SeriesPartName `xml:"b282"` 
	NumberWithinSeries NumberWithinSeries `xml:"b019"` 
}

// Subject is not documented.
type Subject struct {
	SubjectSchemeIdentifier SubjectSchemeIdentifier `xml:"b067"` 
	SubjectSchemeName SubjectSchemeName `xml:"b171"` 
	SubjectSchemeVersion SubjectSchemeVersion `xml:"b068"` 
	SubjectCode SubjectCode `xml:"b069"` 
	SubjectHeadingText SubjectHeadingText `xml:"b070"` 
	SubjectHeadingText SubjectHeadingText `xml:"b070"` 
	SubjectCode SubjectCode `xml:"b069"` 
	SubjectHeadingText SubjectHeadingText `xml:"b070"` 
}

// SupplierIdentifier is not documented.
type SupplierIdentifier struct {
	SupplierIDType SupplierIDType `xml:"j345"` 
	IDTypeName IDTypeName `xml:"b233"` 
	IDValue IDValue `xml:"b244"` 
}

// SupplyDetail is not documented.
type SupplyDetail struct {
	SupplierIdentifier SupplierIdentifier `xml:"supplieridentifier"` 
	SupplierSAN SupplierSAN `xml:"j136"` 
	SupplierEANLocationNumber SupplierEANLocationNumber `xml:"j135"` 
	SupplierSAN SupplierSAN `xml:"j136"` 
	SupplierName SupplierName `xml:"j137"` 
	SupplierName SupplierName `xml:"j137"` 
	TelephoneNumber TelephoneNumber `xml:"j270"` 
	FaxNumber FaxNumber `xml:"j271"` 
	EmailAddress EmailAddress `xml:"j272"` 
	Website Website `xml:"website"` 
	SupplierRole SupplierRole `xml:"j292"` 
	SupplyToCountry SupplyToCountry `xml:"j138"` 
	SupplyToTerritory SupplyToTerritory `xml:"j397"` 
	SupplyToRegion SupplyToRegion `xml:"j139"` 
	SupplyToTerritory SupplyToTerritory `xml:"j397"` 
	SupplyToRegion SupplyToRegion `xml:"j139"` 
	SupplyToCountryExcluded SupplyToCountryExcluded `xml:"j140"` 
	SupplyRestrictionDetail SupplyRestrictionDetail `xml:"j399"` 
	ReturnsCodeType ReturnsCodeType `xml:"j268"` 
	ReturnsCode ReturnsCode `xml:"j269"` 
	LastDateForReturns LastDateForReturns `xml:"j387"` 
	AvailabilityCode AvailabilityCode `xml:"j141"` 
	ProductAvailability ProductAvailability `xml:"j396"` 
	IntermediaryAvailabilityCode IntermediaryAvailabilityCode `xml:"j348"` 
	ProductAvailability ProductAvailability `xml:"j396"` 
	IntermediaryAvailabilityCode IntermediaryAvailabilityCode `xml:"j348"` 
	IntermediaryAvailabilityCode IntermediaryAvailabilityCode `xml:"j348"` 
	NewSupplier NewSupplier `xml:"newsupplier"` 
	DateFormat DateFormat `xml:"j260"` 
	ExpectedShipDate ExpectedShipDate `xml:"j142"` 
	OnSaleDate OnSaleDate `xml:"j143"` 
	OrderTime OrderTime `xml:"j144"` 
	Stock Stock `xml:"stock"` 
	PackQuantity PackQuantity `xml:"j145"` 
	AudienceRestrictionFlag AudienceRestrictionFlag `xml:"j146"` 
	AudienceRestrictionNote AudienceRestrictionNote `xml:"j147"` 
	PriceAmount PriceAmount `xml:"j151"` 
	UnpricedItemType UnpricedItemType `xml:"j192"` 
	Price Price `xml:"price"` 
	Reissue Reissue `xml:"reissue"` 
	SupplierIdentifier SupplierIdentifier `xml:"supplieridentifier"` 
	SupplierSAN SupplierSAN `xml:"j136"` 
	SupplierEANLocationNumber SupplierEANLocationNumber `xml:"j135"` 
	SupplierSAN SupplierSAN `xml:"j136"` 
	SupplierName SupplierName `xml:"j137"` 
	SupplierEANLocationNumber SupplierEANLocationNumber `xml:"j135"` 
	SupplierSAN SupplierSAN `xml:"j136"` 
	SupplyToCountry SupplyToCountry `xml:"j138"` 
	SupplyToTerritory SupplyToTerritory `xml:"j397"` 
	SupplyToRegion SupplyToRegion `xml:"j139"` 
	SupplyToTerritory SupplyToTerritory `xml:"j397"` 
	SupplyToRegion SupplyToRegion `xml:"j139"` 
	SupplyToCountryExcluded SupplyToCountryExcluded `xml:"j140"` 
	SupplyToCountry SupplyToCountry `xml:"j138"` 
	SupplyToTerritory SupplyToTerritory `xml:"j397"` 
	SupplyToRegion SupplyToRegion `xml:"j139"` 
	ReturnsCodeType ReturnsCodeType `xml:"j268"` 
	ReturnsCode ReturnsCode `xml:"j269"` 
	AvailabilityCode AvailabilityCode `xml:"j141"` 
	ProductAvailability ProductAvailability `xml:"j396"` 
	IntermediaryAvailabilityCode IntermediaryAvailabilityCode `xml:"j348"` 
	ProductAvailability ProductAvailability `xml:"j396"` 
	IntermediaryAvailabilityCode IntermediaryAvailabilityCode `xml:"j348"` 
	DateFormat DateFormat `xml:"j260"` 
	ExpectedShipDate ExpectedShipDate `xml:"j142"` 
	AudienceRestrictionFlag AudienceRestrictionFlag `xml:"j146"` 
	AudienceRestrictionNote AudienceRestrictionNote `xml:"j147"` 
}

// TextItem is not documented.
type TextItem struct {
	TextItemType TextItemType `xml:"b290"` 
	TextItemIdentifier TextItemIdentifier `xml:"textitemidentifier"` 
	FirstPageNumber FirstPageNumber `xml:"b286"` 
	LastPageNumber LastPageNumber `xml:"b287"` 
	PageRun PageRun `xml:"pagerun"` 
	NumberOfPages NumberOfPages `xml:"b061"` 
	FirstPageNumber FirstPageNumber `xml:"b286"` 
	LastPageNumber LastPageNumber `xml:"b287"` 
}

// TextItemIdentifier is not documented.
type TextItemIdentifier struct {
	TextItemIDType TextItemIDType `xml:"b285"` 
	IDTypeName IDTypeName `xml:"b233"` 
	IDValue IDValue `xml:"b244"` 
}

// Title is not documented.
type Title struct {
	TitleType TitleType `xml:"b202"` 
	AbbreviatedLength AbbreviatedLength `xml:"b276"` 
	TextCaseFlag TextCaseFlag `xml:"b027"` 
	TitleText TitleText `xml:"b203"` 
	TitlePrefix TitlePrefix `xml:"b030"` 
	TitleWithoutPrefix TitleWithoutPrefix `xml:"b031"` 
	TitlePrefix TitlePrefix `xml:"b030"` 
	TitleWithoutPrefix TitleWithoutPrefix `xml:"b031"` 
	Subtitle Subtitle `xml:"b029"` 
	TitleText TitleText `xml:"b203"` 
	TitlePrefix TitlePrefix `xml:"b030"` 
	TitleWithoutPrefix TitleWithoutPrefix `xml:"b031"` 
	TitlePrefix TitlePrefix `xml:"b030"` 
	TitleWithoutPrefix TitleWithoutPrefix `xml:"b031"` 
	TitlePrefix TitlePrefix `xml:"b030"` 
	TitleWithoutPrefix TitleWithoutPrefix `xml:"b031"` 
}

// Website is not documented.
type Website struct {
	WebsiteRole WebsiteRole `xml:"b367"` 
	WebsiteDescription WebsiteDescription `xml:"b294"` 
	WebsiteLink WebsiteLink `xml:"b295"` 
}

// WorkIdentifier is not documented.
type WorkIdentifier struct {
	WorkIDType WorkIDType `xml:"b201"` 
	IDTypeName IDTypeName `xml:"b233"` 
	IDValue IDValue `xml:"b244"` 
}
