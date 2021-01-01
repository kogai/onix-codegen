package onix

import (
	"encoding/xml"
	"fmt"
)

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
	AudienceRangePrecision AudienceRangePrecision `xml:"b075,omitempty"`
	AudienceRangeValue AudienceRangeValue `xml:"b076,omitempty"`
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
	ISBN ISBN `xml:"b004,omitempty"`
	EAN13 EAN13 `xml:"b005,omitempty"`
	ProductIdentifier ProductIdentifier `xml:"productidentifier,omitempty"`
	ProductForm ProductForm `xml:"b012,omitempty"`
	ProductFormDetail ProductFormDetail `xml:"b333,omitempty"`
	ProductFormFeature ProductFormFeature `xml:"productformfeature,omitempty"`
	BookFormDetail BookFormDetail `xml:"b013,omitempty"`
	ProductPackaging ProductPackaging `xml:"b225,omitempty"`
	ProductFormDescription ProductFormDescription `xml:"b014,omitempty"`
	NumberOfPieces NumberOfPieces `xml:"b210"`
	TradeCategory TradeCategory `xml:"b384"`
	ProductContentType ProductContentType `xml:"b385"`
	ItemQuantity ItemQuantity `xml:"b015"`
}

// ContentItem is not documented.
type ContentItem struct {
	LevelSequenceNumber LevelSequenceNumber `xml:"b284"`
	TextItem TextItem `xml:"textitem"`
	Website Website `xml:"website"`
	ComponentTypeName ComponentTypeName `xml:"b288,omitempty"`
	ComponentNumber ComponentNumber `xml:"b289,omitempty"`
	DistinctiveTitle DistinctiveTitle `xml:"b028,omitempty"`
	Title Title `xml:"title,omitempty"`
	WorkIdentifier WorkIdentifier `xml:"workidentifier"`
	Contributor Contributor `xml:"contributor,omitempty"`
	ContributorStatement ContributorStatement `xml:"b049,omitempty"`
	Subject Subject `xml:"subject"`
	PersonAsSubject PersonAsSubject `xml:"personassubject"`
	CorporateBodyAsSubject CorporateBodyAsSubject `xml:"b071"`
	PlaceAsSubject PlaceAsSubject `xml:"b072"`
	OtherText OtherText `xml:"othertext"`
	MediaFile MediaFile `xml:"mediafile"`
}

// Contributor is not documented.
type Contributor struct {
	SequenceNumber SequenceNumber `xml:"b034"`
	ContributorRole ContributorRole `xml:"b035,omitempty"`
	LanguageCode LanguageCode `xml:"b252,omitempty"`
	SequenceNumberWithinRole SequenceNumberWithinRole `xml:"b340"`
	PersonName PersonName `xml:"b036,omitempty"`
	PersonNameInverted PersonNameInverted `xml:"b037,omitempty"`
	TitlesBeforeNames TitlesBeforeNames `xml:"b038,omitempty"`
	NamesBeforeKey NamesBeforeKey `xml:"b039,omitempty"`
	PrefixToKey PrefixToKey `xml:"b247,omitempty"`
	KeyNames KeyNames `xml:"b040,omitempty"`
	NamesAfterKey NamesAfterKey `xml:"b041,omitempty"`
	SuffixToKey SuffixToKey `xml:"b248,omitempty"`
	LettersAfterNames LettersAfterNames `xml:"b042,omitempty"`
	TitlesAfterNames TitlesAfterNames `xml:"b043,omitempty"`
	Name Name `xml:"name,omitempty"`
	PersonNameIdentifier PersonNameIdentifier `xml:"personnameidentifier,omitempty"`
	PersonDate PersonDate `xml:"persondate,omitempty"`
	ProfessionalAffiliation ProfessionalAffiliation `xml:"professionalaffiliation,omitempty"`
	CorporateName CorporateName `xml:"b047,omitempty"`
	BiographicalNote BiographicalNote `xml:"b044,omitempty"`
	Website Website `xml:"website,omitempty"`
	ProfessionalPosition ProfessionalPosition `xml:"b045,omitempty"`
	Affiliation Affiliation `xml:"b046,omitempty"`
	ContributorDescription ContributorDescription `xml:"b048,omitempty"`
	UnnamedPersons UnnamedPersons `xml:"b249"`
	CountryCode CountryCode `xml:"b251"`
	RegionCode RegionCode `xml:"b398"`
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
	FromEANNumber FromEANNumber `xml:"m172,omitempty"`
	FromSAN FromSAN `xml:"m173,omitempty"`
	SenderIdentifier SenderIdentifier `xml:"senderidentifier,omitempty"`
	FromCompany FromCompany `xml:"m174,omitempty"`
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
	RecordSourceIdentifierType RecordSourceIdentifierType `xml:"a195,omitempty"`
	RecordSourceIdentifier RecordSourceIdentifier `xml:"a196,omitempty"`
	RecordSourceName RecordSourceName `xml:"a197"`
	SeriesIdentifier SeriesIdentifier `xml:"seriesidentifier"`
	Title Title `xml:"title"`
	Contributor Contributor `xml:"contributor"`
	OtherText OtherText `xml:"othertext"`
	Publisher Publisher `xml:"publisher"`
	SubordinateEntries SubordinateEntries `xml:"a245"`
}

// MainSubject is not documented.
type MainSubject struct {
	MainSubjectSchemeIdentifier MainSubjectSchemeIdentifier `xml:"b191"`
	SubjectSchemeVersion SubjectSchemeVersion `xml:"b068"`
	SubjectCode SubjectCode `xml:"b069,omitempty"`
	SubjectHeadingText SubjectHeadingText `xml:"b070,omitempty"`
}

// MarketDate is not documented.
type MarketDate struct {
	MarketDateRole MarketDateRole `xml:"j408"`
	DateFormat DateFormat `xml:"j260"`
	Date Date `xml:"b306"`
}

// MarketRepresentation is not documented.
type MarketRepresentation struct {
	AgentIdentifier AgentIdentifier `xml:"agentidentifier,omitempty"`
	AgentName AgentName `xml:"j401,omitempty"`
	TelephoneNumber TelephoneNumber `xml:"j270"`
	FaxNumber FaxNumber `xml:"j271"`
	EmailAddress EmailAddress `xml:"j272"`
	Website Website `xml:"website"`
	AgentRole AgentRole `xml:"j402"`
	MarketCountry MarketCountry `xml:"j403"`
	MarketTerritory MarketTerritory `xml:"j404,omitempty"`
	MarketCountryExcluded MarketCountryExcluded `xml:"j405,omitempty"`
	MarketRestrictionDetail MarketRestrictionDetail `xml:"j406"`
	MarketPublishingStatus MarketPublishingStatus `xml:"j407"`
	MarketDate MarketDate `xml:"marketdate"`
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
	DownloadCaption DownloadCaption `xml:"f119,omitempty"`
	DownloadCredit DownloadCredit `xml:"f120,omitempty"`
	DownloadCopyrightNotice DownloadCopyrightNotice `xml:"f121,omitempty"`
	DownloadTerms DownloadTerms `xml:"f122"`
	MediaFileDate MediaFileDate `xml:"f373"`
}

// Name is not documented.
type Name struct {
	PersonNameType PersonNameType `xml:"b250"`
	PersonName PersonName `xml:"b036,omitempty"`
	PersonNameInverted PersonNameInverted `xml:"b037,omitempty"`
	TitlesBeforeNames TitlesBeforeNames `xml:"b038,omitempty"`
	NamesBeforeKey NamesBeforeKey `xml:"b039,omitempty"`
	PrefixToKey PrefixToKey `xml:"b247,omitempty"`
	KeyNames KeyNames `xml:"b040,omitempty"`
	NamesAfterKey NamesAfterKey `xml:"b041,omitempty"`
	SuffixToKey SuffixToKey `xml:"b248,omitempty"`
	LettersAfterNames LettersAfterNames `xml:"b042,omitempty"`
	TitlesAfterNames TitlesAfterNames `xml:"b043,omitempty"`
	PersonNameIdentifier PersonNameIdentifier `xml:"personnameidentifier,omitempty"`
}

// NewSupplier is not documented.
type NewSupplier struct {
	SupplierIdentifier SupplierIdentifier `xml:"supplieridentifier,omitempty"`
	SupplierSAN SupplierSAN `xml:"j136,omitempty"`
	SupplierEANLocationNumber SupplierEANLocationNumber `xml:"j135,omitempty"`
	SupplierName SupplierName `xml:"j137,omitempty"`
	TelephoneNumber TelephoneNumber `xml:"j270"`
	FaxNumber FaxNumber `xml:"j271"`
	EmailAddress EmailAddress `xml:"j272"`
}

// NotForSale is not documented.
type NotForSale struct {
	RightsCountry RightsCountry `xml:"b090,omitempty"`
	RightsTerritory RightsTerritory `xml:"b388,omitempty"`
	ISBN ISBN `xml:"b004"`
	EAN13 EAN13 `xml:"b005"`
	ProductIdentifier ProductIdentifier `xml:"productidentifier"`
	PublisherName PublisherName `xml:"b081"`
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
	TextLinkType TextLinkType `xml:"d105,omitempty"`
	TextLink TextLink `xml:"d106,omitempty"`
	TextAuthor TextAuthor `xml:"d107"`
	TextSourceCorporate TextSourceCorporate `xml:"b374"`
	TextSourceTitle TextSourceTitle `xml:"d108"`
	TextPublicationDate TextPublicationDate `xml:"d109"`
	StartDate StartDate `xml:"b324,omitempty"`
	EndDate EndDate `xml:"b325,omitempty"`
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
	PersonNameInverted PersonNameInverted `xml:"b037,omitempty"`
	TitlesBeforeNames TitlesBeforeNames `xml:"b038,omitempty"`
	NamesBeforeKey NamesBeforeKey `xml:"b039,omitempty"`
	PrefixToKey PrefixToKey `xml:"b247,omitempty"`
	KeyNames KeyNames `xml:"b040,omitempty"`
	NamesAfterKey NamesAfterKey `xml:"b041,omitempty"`
	SuffixToKey SuffixToKey `xml:"b248,omitempty"`
	LettersAfterNames LettersAfterNames `xml:"b042,omitempty"`
	TitlesAfterNames TitlesAfterNames `xml:"b043,omitempty"`
	Name Name `xml:"name,omitempty"`
	PersonNameIdentifier PersonNameIdentifier `xml:"personnameidentifier,omitempty"`
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
	CountryCode CountryCode `xml:"b251,omitempty"`
	Territory Territory `xml:"j303,omitempty"`
	CountryExcluded CountryExcluded `xml:"j304,omitempty"`
	TerritoryExcluded TerritoryExcluded `xml:"j308,omitempty"`
	TaxRateCode1 TaxRateCode1 `xml:"j153,omitempty"`
	TaxRatePercent1 TaxRatePercent1 `xml:"j154,omitempty"`
	TaxableAmount1 TaxableAmount1 `xml:"j155,omitempty"`
	TaxAmount1 TaxAmount1 `xml:"j156,omitempty"`
	TaxRateCode2 TaxRateCode2 `xml:"j157,omitempty"`
	TaxRatePercent2 TaxRatePercent2 `xml:"j158,omitempty"`
	TaxableAmount2 TaxableAmount2 `xml:"j159,omitempty"`
	TaxAmount2 TaxAmount2 `xml:"j160,omitempty"`
	PriceEffectiveFrom PriceEffectiveFrom `xml:"j161"`
	PriceEffectiveUntil PriceEffectiveUntil `xml:"j162"`
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
	RecordSourceIdentifierType RecordSourceIdentifierType `xml:"a195,omitempty"`
	RecordSourceIdentifier RecordSourceIdentifier `xml:"a196,omitempty"`
	RecordSourceName RecordSourceName `xml:"a197"`
	ISBN ISBN `xml:"b004,omitempty"`
	EAN13 EAN13 `xml:"b005,omitempty"`
	UPC UPC `xml:"b006,omitempty"`
	PublisherProductNo PublisherProductNo `xml:"b007,omitempty"`
	ISMN ISMN `xml:"b008,omitempty"`
	DOI DOI `xml:"b009,omitempty"`
	ProductIdentifier ProductIdentifier `xml:"productidentifier,omitempty"`
	Barcode Barcode `xml:"b246,omitempty"`
	ReplacesISBN ReplacesISBN `xml:"b010,omitempty"`
	ReplacesEAN13 ReplacesEAN13 `xml:"b011,omitempty"`
	ProductForm ProductForm `xml:"b012,omitempty"`
	ProductFormDetail ProductFormDetail `xml:"b333,omitempty"`
	ProductFormFeature ProductFormFeature `xml:"productformfeature,omitempty"`
	BookFormDetail BookFormDetail `xml:"b013,omitempty"`
	ProductPackaging ProductPackaging `xml:"b225,omitempty"`
	ProductFormDescription ProductFormDescription `xml:"b014,omitempty"`
	NumberOfPieces NumberOfPieces `xml:"b210,omitempty"`
	TradeCategory TradeCategory `xml:"b384,omitempty"`
	ProductContentType ProductContentType `xml:"b385,omitempty"`
	ContainedItem ContainedItem `xml:"containeditem,omitempty"`
	ProductClassification ProductClassification `xml:"productclassification,omitempty"`
	EpubType EpubType `xml:"b211,omitempty"`
	EpubTypeVersion EpubTypeVersion `xml:"b212,omitempty"`
	EpubTypeDescription EpubTypeDescription `xml:"b213,omitempty"`
	EpubFormat EpubFormat `xml:"b214,omitempty"`
	EpubFormatVersion EpubFormatVersion `xml:"b215,omitempty"`
	EpubFormatDescription EpubFormatDescription `xml:"b216,omitempty"`
	EpubSource EpubSource `xml:"b278,omitempty"`
	EpubSourceVersion EpubSourceVersion `xml:"b279,omitempty"`
	EpubSourceDescription EpubSourceDescription `xml:"b280,omitempty"`
	EpubTypeNote EpubTypeNote `xml:"b277,omitempty"`
	Series Series `xml:"series,omitempty"`
	NoSeries NoSeries `xml:"n338,omitempty"`
	Set Set `xml:"set,omitempty"`
	TextCaseFlag TextCaseFlag `xml:"b027,omitempty"`
	DistinctiveTitle DistinctiveTitle `xml:"b028,omitempty"`
	TitlePrefix TitlePrefix `xml:"b030,omitempty"`
	TitleWithoutPrefix TitleWithoutPrefix `xml:"b031,omitempty"`
	Subtitle Subtitle `xml:"b029,omitempty"`
	TranslationOfTitle TranslationOfTitle `xml:"b032,omitempty"`
	FormerTitle FormerTitle `xml:"b033,omitempty"`
	Title Title `xml:"title,omitempty"`
	WorkIdentifier WorkIdentifier `xml:"workidentifier,omitempty"`
	Website Website `xml:"website,omitempty"`
	ThesisType ThesisType `xml:"b368,omitempty"`
	ThesisPresentedTo ThesisPresentedTo `xml:"b369,omitempty"`
	ThesisYear ThesisYear `xml:"b370,omitempty"`
	Contributor Contributor `xml:"contributor,omitempty"`
	ContributorStatement ContributorStatement `xml:"b049,omitempty"`
	NoContributor NoContributor `xml:"n339,omitempty"`
	ConferenceDescription ConferenceDescription `xml:"b050,omitempty"`
	ConferenceRole ConferenceRole `xml:"b051,omitempty"`
	ConferenceName ConferenceName `xml:"b052,omitempty"`
	ConferenceNumber ConferenceNumber `xml:"b053,omitempty"`
	ConferenceDate ConferenceDate `xml:"b054,omitempty"`
	ConferencePlace ConferencePlace `xml:"b055,omitempty"`
	Conference Conference `xml:"conference,omitempty"`
	EditionTypeCode EditionTypeCode `xml:"b056,omitempty"`
	EditionNumber EditionNumber `xml:"b057,omitempty"`
	EditionVersionNumber EditionVersionNumber `xml:"b217,omitempty"`
	EditionStatement EditionStatement `xml:"b058,omitempty"`
	NoEdition NoEdition `xml:"n386,omitempty"`
	ReligiousText ReligiousText `xml:"religioustext,omitempty"`
	LanguageOfText LanguageOfText `xml:"b059,omitempty"`
	OriginalLanguage OriginalLanguage `xml:"b060,omitempty"`
	Language Language `xml:"language,omitempty"`
	NumberOfPages NumberOfPages `xml:"b061,omitempty"`
	PagesRoman PagesRoman `xml:"b254,omitempty"`
	PagesArabic PagesArabic `xml:"b255,omitempty"`
	Extent Extent `xml:"extent,omitempty"`
	NumberOfIllustrations NumberOfIllustrations `xml:"b125,omitempty"`
	IllustrationsNote IllustrationsNote `xml:"b062,omitempty"`
	Illustrations Illustrations `xml:"illustrations,omitempty"`
	MapScale MapScale `xml:"b063,omitempty"`
	BASICMainSubject BASICMainSubject `xml:"b064,omitempty"`
	BASICVersion BASICVersion `xml:"b200,omitempty"`
	BICMainSubject BICMainSubject `xml:"b065,omitempty"`
	BICVersion BICVersion `xml:"b066,omitempty"`
	MainSubject MainSubject `xml:"mainsubject,omitempty"`
	Subject Subject `xml:"subject,omitempty"`
	PersonAsSubject PersonAsSubject `xml:"personassubject,omitempty"`
	CorporateBodyAsSubject CorporateBodyAsSubject `xml:"b071,omitempty"`
	PlaceAsSubject PlaceAsSubject `xml:"b072,omitempty"`
	AudienceCode AudienceCode `xml:"b073,omitempty"`
	Audience Audience `xml:"audience,omitempty"`
	USSchoolGrade USSchoolGrade `xml:"b189,omitempty"`
	InterestAge InterestAge `xml:"b190,omitempty"`
	AudienceRange AudienceRange `xml:"audiencerange,omitempty"`
	AudienceDescription AudienceDescription `xml:"b207,omitempty"`
	Complexity Complexity `xml:"complexity,omitempty"`
	Annotation Annotation `xml:"d100,omitempty"`
	MainDescription MainDescription `xml:"d101,omitempty"`
	OtherText OtherText `xml:"othertext,omitempty"`
	ReviewQuote ReviewQuote `xml:"e110,omitempty"`
	CoverImageFormatCode CoverImageFormatCode `xml:"f111,omitempty"`
	CoverImageLinkTypeCode CoverImageLinkTypeCode `xml:"f112,omitempty"`
	CoverImageLink CoverImageLink `xml:"f113,omitempty"`
	MediaFile MediaFile `xml:"mediafile,omitempty"`
	ProductWebsite ProductWebsite `xml:"productwebsite,omitempty"`
	PrizesDescription PrizesDescription `xml:"g124,omitempty"`
	Prize Prize `xml:"prize,omitempty"`
	ContentItem ContentItem `xml:"contentitem,omitempty"`
	ImprintName ImprintName `xml:"b079,omitempty"`
	Imprint Imprint `xml:"imprint,omitempty"`
	PublisherName PublisherName `xml:"b081,omitempty"`
	Publisher Publisher `xml:"publisher,omitempty"`
	CityOfPublication CityOfPublication `xml:"b209,omitempty"`
	CountryOfPublication CountryOfPublication `xml:"b083,omitempty"`
	CopublisherName CopublisherName `xml:"b084,omitempty"`
	SponsorName SponsorName `xml:"b085,omitempty"`
	OriginalPublisher OriginalPublisher `xml:"b240,omitempty"`
	PublishingStatus PublishingStatus `xml:"b394,omitempty"`
	PublishingStatusNote PublishingStatusNote `xml:"b395,omitempty"`
	AnnouncementDate AnnouncementDate `xml:"b086,omitempty"`
	TradeAnnouncementDate TradeAnnouncementDate `xml:"b362,omitempty"`
	PublicationDate PublicationDate `xml:"b003,omitempty"`
	CopyrightStatement CopyrightStatement `xml:"copyrightstatement,omitempty"`
	CopyrightYear CopyrightYear `xml:"b087,omitempty"`
	YearFirstPublished YearFirstPublished `xml:"b088,omitempty"`
	SalesRights SalesRights `xml:"salesrights,omitempty"`
	NotForSale NotForSale `xml:"notforsale,omitempty"`
	SalesRestriction SalesRestriction `xml:"salesrestriction,omitempty"`
	Height Height `xml:"c096,omitempty"`
	Width Width `xml:"c097,omitempty"`
	Thickness Thickness `xml:"c098,omitempty"`
	Weight Weight `xml:"c099,omitempty"`
	Measure Measure `xml:"measure,omitempty"`
	Dimensions Dimensions `xml:"c258,omitempty"`
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
	PublisherName PublisherName `xml:"b081,omitempty"`
	NameCodeType NameCodeType `xml:"b241,omitempty"`
	NameCodeTypeName NameCodeTypeName `xml:"b242,omitempty"`
	NameCodeValue NameCodeValue `xml:"b243,omitempty"`
	Website Website `xml:"website"`
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
	ISBN ISBN `xml:"b004,omitempty"`
	EAN13 EAN13 `xml:"b005,omitempty"`
	ProductIdentifier ProductIdentifier `xml:"productidentifier,omitempty"`
	Website Website `xml:"website,omitempty"`
	ProductForm ProductForm `xml:"b012,omitempty"`
	ProductFormDetail ProductFormDetail `xml:"b333,omitempty"`
	ProductFormFeature ProductFormFeature `xml:"productformfeature,omitempty"`
	BookFormDetail BookFormDetail `xml:"b013,omitempty"`
	ProductPackaging ProductPackaging `xml:"b225,omitempty"`
	ProductFormDescription ProductFormDescription `xml:"b014,omitempty"`
	NumberOfPieces NumberOfPieces `xml:"b210"`
	TradeCategory TradeCategory `xml:"b384"`
	ProductContentType ProductContentType `xml:"b385"`
	EpubType EpubType `xml:"b211,omitempty"`
	EpubTypeVersion EpubTypeVersion `xml:"b212,omitempty"`
	EpubTypeDescription EpubTypeDescription `xml:"b213,omitempty"`
	EpubFormat EpubFormat `xml:"b214,omitempty"`
	EpubFormatVersion EpubFormatVersion `xml:"b215,omitempty"`
	EpubFormatDescription EpubFormatDescription `xml:"b216,omitempty"`
	EpubTypeNote EpubTypeNote `xml:"b277,omitempty"`
	Publisher Publisher `xml:"publisher"`
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
	RightsCountry RightsCountry `xml:"b090,omitempty"`
	RightsTerritory RightsTerritory `xml:"b388,omitempty"`
	RightsRegion RightsRegion `xml:"b091,omitempty"`
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
	TitleOfSeries TitleOfSeries `xml:"b018,omitempty"`
	Title Title `xml:"title,omitempty"`
	Contributor Contributor `xml:"contributor"`
	NumberWithinSeries NumberWithinSeries `xml:"b019"`
	YearOfAnnual YearOfAnnual `xml:"b020"`
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
	TitleOfSet TitleOfSet `xml:"b023,omitempty"`
	Title Title `xml:"title,omitempty"`
	SetPartNumber SetPartNumber `xml:"b024"`
	SetPartTitle SetPartTitle `xml:"b025"`
	ItemNumberWithinSet ItemNumberWithinSet `xml:"b026"`
	LevelSequenceNumber LevelSequenceNumber `xml:"b284"`
	SetItemTitle SetItemTitle `xml:"b281"`
}

// Stock is not documented.
type Stock struct {
	LocationIdentifier LocationIdentifier `xml:"locationidentifier"`
	LocationName LocationName `xml:"j349"`
	StockQuantityCoded StockQuantityCoded `xml:"stockquantitycoded,omitempty"`
	OnHand OnHand `xml:"j350,omitempty"`
	OnOrder OnOrder `xml:"j351"`
	CBO CBO `xml:"j375"`
	OnOrderDetail OnOrderDetail `xml:"onorderdetail"`
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
	RecordSourceIdentifierType RecordSourceIdentifierType `xml:"a195,omitempty"`
	RecordSourceIdentifier RecordSourceIdentifier `xml:"a196,omitempty"`
	RecordSourceName RecordSourceName `xml:"a197"`
	SeriesIdentifier SeriesIdentifier `xml:"seriesidentifier"`
	ParentIdentifier ParentIdentifier `xml:"parentidentifier"`
	LevelSequenceNumber LevelSequenceNumber `xml:"b284"`
	SeriesPartName SeriesPartName `xml:"b282,omitempty"`
	NumberWithinSeries NumberWithinSeries `xml:"b019,omitempty"`
	Title Title `xml:"title"`
	Contributor Contributor `xml:"contributor"`
	OtherText OtherText `xml:"othertext"`
	Publisher Publisher `xml:"publisher"`
	SubordinateEntries SubordinateEntries `xml:"a245"`
}

// Subject is not documented.
type Subject struct {
	SubjectSchemeIdentifier SubjectSchemeIdentifier `xml:"b067"`
	SubjectSchemeName SubjectSchemeName `xml:"b171"`
	SubjectSchemeVersion SubjectSchemeVersion `xml:"b068"`
	SubjectCode SubjectCode `xml:"b069,omitempty"`
	SubjectHeadingText SubjectHeadingText `xml:"b070,omitempty"`
}

// SupplierIdentifier is not documented.
type SupplierIdentifier struct {
	SupplierIDType SupplierIDType `xml:"j345"`
	IDTypeName IDTypeName `xml:"b233"`
	IDValue IDValue `xml:"b244"`
}

// SupplyDetail is not documented.
type SupplyDetail struct {
	SupplierIdentifier SupplierIdentifier `xml:"supplieridentifier,omitempty"`
	SupplierSAN SupplierSAN `xml:"j136,omitempty"`
	SupplierEANLocationNumber SupplierEANLocationNumber `xml:"j135,omitempty"`
	SupplierName SupplierName `xml:"j137,omitempty"`
	TelephoneNumber TelephoneNumber `xml:"j270"`
	FaxNumber FaxNumber `xml:"j271"`
	EmailAddress EmailAddress `xml:"j272"`
	Website Website `xml:"website"`
	SupplierRole SupplierRole `xml:"j292"`
	SupplyToCountry SupplyToCountry `xml:"j138,omitempty"`
	SupplyToTerritory SupplyToTerritory `xml:"j397,omitempty"`
	SupplyToRegion SupplyToRegion `xml:"j139,omitempty"`
	SupplyToCountryExcluded SupplyToCountryExcluded `xml:"j140,omitempty"`
	SupplyRestrictionDetail SupplyRestrictionDetail `xml:"j399"`
	ReturnsCodeType ReturnsCodeType `xml:"j268,omitempty"`
	ReturnsCode ReturnsCode `xml:"j269,omitempty"`
	LastDateForReturns LastDateForReturns `xml:"j387"`
	AvailabilityCode AvailabilityCode `xml:"j141,omitempty"`
	ProductAvailability ProductAvailability `xml:"j396,omitempty"`
	IntermediaryAvailabilityCode IntermediaryAvailabilityCode `xml:"j348,omitempty"`
	NewSupplier NewSupplier `xml:"newsupplier"`
	DateFormat DateFormat `xml:"j260,omitempty"`
	ExpectedShipDate ExpectedShipDate `xml:"j142,omitempty"`
	OnSaleDate OnSaleDate `xml:"j143"`
	OrderTime OrderTime `xml:"j144"`
	Stock Stock `xml:"stock"`
	PackQuantity PackQuantity `xml:"j145"`
	AudienceRestrictionFlag AudienceRestrictionFlag `xml:"j146,omitempty"`
	AudienceRestrictionNote AudienceRestrictionNote `xml:"j147,omitempty"`
	PriceAmount PriceAmount `xml:"j151"`
	UnpricedItemType UnpricedItemType `xml:"j192"`
	Price Price `xml:"price"`
	Reissue Reissue `xml:"reissue"`
}

// TextItem is not documented.
type TextItem struct {
	TextItemType TextItemType `xml:"b290"`
	TextItemIdentifier TextItemIdentifier `xml:"textitemidentifier"`
	FirstPageNumber FirstPageNumber `xml:"b286,omitempty"`
	LastPageNumber LastPageNumber `xml:"b287,omitempty"`
	PageRun PageRun `xml:"pagerun"`
	NumberOfPages NumberOfPages `xml:"b061"`
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
	TitleText TitleText `xml:"b203,omitempty"`
	TitlePrefix TitlePrefix `xml:"b030,omitempty"`
	TitleWithoutPrefix TitleWithoutPrefix `xml:"b031,omitempty"`
	Subtitle Subtitle `xml:"b029"`
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
