package onix


// AddresseeIdentifier is not documented.
type AddresseeIdentifier struct {
	AddresseeIDType AddresseeIDType `xml:"m380"`
	IDTypeName string `xml:"b233,omitempty"`
	IDValue string `xml:"b244"`
}

// AgentIdentifier is not documented.
type AgentIdentifier struct {
	AgentIDType string `xml:"j400"`
	IDTypeName string `xml:"b233,omitempty"`
	IDValue string `xml:"b244"`
}

// Audience is not documented.
type Audience struct {
	AudienceCodeType AudienceCodeType `xml:"b204"`
	AudienceCodeTypeName string `xml:"b205,omitempty"`
	AudienceCodeValue string `xml:"b206"`
}

// AudienceRange is not documented.
type AudienceRange struct {
	AudienceRangeQualifier AudienceRangeQualifier `xml:"b074"`
	AudienceRangePrecision AudienceRangePrecision `xml:"b075"`
	AudienceRangeValue string `xml:"b076"`
}

// BatchBonus is not documented.
type BatchBonus struct {
	BatchQuantity string `xml:"j264"`
	FreeQuantity string `xml:"j265"`
}

// Bible is not documented.
type Bible struct {
	BibleContents []BibleContents `xml:"b352"`
	BibleVersion []BibleVersion `xml:"b353"`
	StudyBibleType StudyBibleType `xml:"b389,omitempty"`
	BiblePurpose []BiblePurpose `xml:"b354,omitempty"`
	BibleTextOrganization BibleTextOrganization `xml:"b355,omitempty"`
	BibleReferenceLocation BibleReferenceLocation `xml:"b356,omitempty"`
	BibleTextFeature []BibleTextFeature `xml:"b357,omitempty"`
}

// Complexity is not documented.
type Complexity struct {
	ComplexitySchemeIdentifier ComplexitySchemeIdentifier `xml:"b077"`
	ComplexityCode string `xml:"b078"`
}

// Conference is not documented.
type Conference struct {
	ConferenceRole ConferenceRole `xml:"b051,omitempty"`
	ConferenceName string `xml:"b052"`
	ConferenceAcronym string `xml:"b341,omitempty"`
	ConferenceNumber string `xml:"b053,omitempty"`
	ConferenceTheme string `xml:"b342,omitempty"`
	ConferenceDate string `xml:"b054,omitempty"`
	ConferencePlace string `xml:"b055,omitempty"`
	ConferenceSponsor []ConferenceSponsor `xml:"conferencesponsor,omitempty"`
	Website []Website `xml:"website,omitempty"`
}

// ConferenceSponsor is not documented.
type ConferenceSponsor struct {
	PersonName string `xml:"b036,omitempty"`
	CorporateName string `xml:"b047,omitempty"`
	ConferenceSponsorIdentifier ConferenceSponsorIdentifier `xml:"conferencesponsoridentifier,omitempty"`
}

// ConferenceSponsorIdentifier is not documented.
type ConferenceSponsorIdentifier struct {
	ConferenceSponsorIDType ConferenceSponsorIDType `xml:"b391"`
	IDTypeName string `xml:"b233,omitempty"`
	IDValue string `xml:"b244"`
}

// ContainedItem is not documented.
type ContainedItem struct {
	ISBN string `xml:"b004,omitempty"`
	EAN13 string `xml:"b005,omitempty"`
	ProductIdentifier []ProductIdentifier `xml:"productidentifier,omitempty"`
	ProductForm ProductForm `xml:"b012,omitempty"`
	ProductFormDetail []ProductFormDetail `xml:"b333,omitempty"`
	ProductFormFeature []ProductFormFeature `xml:"productformfeature,omitempty"`
	BookFormDetail []BookFormDetail `xml:"b013,omitempty"`
	ProductPackaging ProductPackaging `xml:"b225,omitempty"`
	ProductFormDescription string `xml:"b014,omitempty"`
	NumberOfPieces string `xml:"b210,omitempty"`
	TradeCategory TradeCategory `xml:"b384,omitempty"`
	ProductContentType []ProductContentType `xml:"b385,omitempty"`
	ItemQuantity string `xml:"b015,omitempty"`
}

// ContentItem is not documented.
type ContentItem struct {
	LevelSequenceNumber string `xml:"b284"`
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
	CorporateBodyAsSubject string `xml:"b071"`
	PlaceAsSubject string `xml:"b072"`
	OtherText OtherText `xml:"othertext"`
	MediaFile MediaFile `xml:"mediafile"`
}

// Contributor is not documented.
type Contributor struct {
	SequenceNumberWithinRole string `xml:"b340,omitempty"`
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
	RegionCode string `xml:"b398"`
}

// CopyrightOwner is not documented.
type CopyrightOwner struct {
	PersonName string `xml:"b036,omitempty"`
	CorporateName string `xml:"b047,omitempty"`
	CopyrightOwnerIdentifier CopyrightOwnerIdentifier `xml:"copyrightowneridentifier,omitempty"`
}

// CopyrightOwnerIdentifier is not documented.
type CopyrightOwnerIdentifier struct {
	CopyrightOwnerIDType CopyrightOwnerIDType `xml:"b392"`
	IDTypeName string `xml:"b233,omitempty"`
	IDValue string `xml:"b244"`
}

// CopyrightStatement is not documented.
type CopyrightStatement struct {
	CopyrightYear []string `xml:"b087"`
	CopyrightOwner []CopyrightOwner `xml:"copyrightowner"`
}

// DiscountCoded is not documented.
type DiscountCoded struct {
	DiscountCodeType DiscountCodeType `xml:"j363"`
	DiscountCodeTypeName string `xml:"j378,omitempty"`
	DiscountCode string `xml:"j364"`
}

// Extent is not documented.
type Extent struct {
	ExtentType ExtentType `xml:"b218"`
	ExtentValue string `xml:"b219"`
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
	ToCompany string `xml:"m178"`
	ToPerson string `xml:"m179"`
	MessageNumber string `xml:"m180"`
	MessageRepeat string `xml:"m181"`
	SentDate string `xml:"m182"`
	MessageNote string `xml:"m183"`
	DefaultLanguageOfText DefaultLanguageOfText `xml:"m184"`
	DefaultPriceTypeCode DefaultPriceTypeCode `xml:"m185"`
	DefaultCurrencyCode DefaultCurrencyCode `xml:"m186"`
	DefaultLinearUnit DefaultLinearUnit `xml:"m187"`
	DefaultWeightUnit DefaultWeightUnit `xml:"m188"`
	DefaultClassOfTrade string `xml:"m193"`
}

// Illustrations is not documented.
type Illustrations struct {
	IllustrationType IllustrationType `xml:"b256"`
	IllustrationTypeDescription string `xml:"b361,omitempty"`
	Number string `xml:"b257,omitempty"`
}

// Imprint is not documented.
type Imprint struct {
	ImprintName string `xml:"b079,omitempty"`
	NameCodeType NameCodeType `xml:"b241,omitempty"`
	NameCodeTypeName string `xml:"b242,omitempty"`
	NameCodeValue string `xml:"b243,omitempty"`
}

// Language is not documented.
type Language struct {
	LanguageRole LanguageRole `xml:"b253"`
	LanguageCode LanguageCode `xml:"b252"`
	CountryCode CountryCode `xml:"b251,omitempty"`
}

// LocationIdentifier is not documented.
type LocationIdentifier struct {
	LocationIDType LocationIDType `xml:"j377"`
	IDTypeName string `xml:"b233,omitempty"`
	IDValue string `xml:"b244"`
}

// MainSeriesRecord is not documented.
type MainSeriesRecord struct {
	RecordReference string `xml:"a001"`
	NotificationType NotificationType `xml:"a002"`
	DeletionCode DeletionCode `xml:"a198"`
	DeletionText string `xml:"a199"`
	RecordSourceType RecordSourceType `xml:"a194"`
	RecordSourceIdentifierType RecordSourceIdentifierType `xml:"a195,omitempty"`
	RecordSourceIdentifier RecordSourceIdentifier `xml:"a196,omitempty"`
	RecordSourceName string `xml:"a197"`
	SeriesIdentifier SeriesIdentifier `xml:"seriesidentifier"`
	Title Title `xml:"title"`
	Contributor Contributor `xml:"contributor"`
	OtherText OtherText `xml:"othertext"`
	Publisher Publisher `xml:"publisher"`
	SubordinateEntries string `xml:"a245"`
}

// MainSubject is not documented.
type MainSubject struct {
	SubjectHeadingText string `xml:"b070,omitempty"`
	SubjectCode string `xml:"b069,omitempty"`
	MainSubjectSchemeIdentifier MainSubjectSchemeIdentifier `xml:"b191"`
	SubjectSchemeVersion string `xml:"b068,omitempty"`
}

// MarketDate is not documented.
type MarketDate struct {
	MarketDateRole string `xml:"j408"`
	DateFormat DateFormat `xml:"j260,omitempty"`
	Date string `xml:"b306"`
}

// MarketRepresentation is not documented.
type MarketRepresentation struct {
	AgentName string `xml:"j401,omitempty"`
	AgentIdentifier []AgentIdentifier `xml:"agentidentifier,omitempty"`
	MarketCountry string `xml:"j403,omitempty"`
	MarketTerritory string `xml:"j404,omitempty"`
	MarketCountryExcluded string `xml:"j405,omitempty"`
	TelephoneNumber []string `xml:"j270,omitempty"`
	FaxNumber []string `xml:"j271,omitempty"`
	EmailAddress []string `xml:"j272,omitempty"`
	Website []Website `xml:"website,omitempty"`
	AgentRole string `xml:"j402,omitempty"`
	MarketRestrictionDetail string `xml:"j406,omitempty"`
	MarketPublishingStatus string `xml:"j407,omitempty"`
	MarketDate []MarketDate `xml:"marketdate,omitempty"`
}

// Measure is not documented.
type Measure struct {
	MeasureTypeCode MeasureTypeCode `xml:"c093"`
	Measurement string `xml:"c094"`
	MeasureUnitCode MeasureUnitCode `xml:"c095"`
}

// MediaFile is not documented.
type MediaFile struct {
	TextWithDownload TextWithDownload `xml:"f118,omitempty"`
	DownloadCopyrightNotice DownloadCopyrightNotice `xml:"f121,omitempty"`
	DownloadCaption DownloadCaption `xml:"f119,omitempty"`
	DownloadCredit DownloadCredit `xml:"f120,omitempty"`
	MediaFileTypeCode MediaFileTypeCode `xml:"f114"`
	MediaFileFormatCode MediaFileFormatCode `xml:"f115,omitempty"`
	ImageResolution string `xml:"f259,omitempty"`
	MediaFileLinkTypeCode MediaFileLinkTypeCode `xml:"f116"`
	MediaFileLink string `xml:"f117"`
	DownloadTerms DownloadTerms `xml:"f122,omitempty"`
	MediaFileDate string `xml:"f373,omitempty"`
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
	TelephoneNumber string `xml:"j270"`
	FaxNumber string `xml:"j271"`
	EmailAddress string `xml:"j272"`

// NoContributor is not documented.
type NoContributor struct {
}

// NoEdition is not documented.
type NoEdition struct {
}

// NoSeries is not documented.
type NoSeries struct {
}

// NotForSale is not documented.
type NotForSale struct {
	RightsTerritory TerritoryCodeList `xml:"b388,omitempty"`
	RightsCountry []CountryCodeList `xml:"b090,omitempty"`
	ISBN string `xml:"b004,omitempty"`
	EAN13 string `xml:"b005,omitempty"`
	ProductIdentifier []ProductIdentifier `xml:"productidentifier,omitempty"`
	PublisherName string `xml:"b081,omitempty"`
}

// ONIXMessage is not documented.
type ONIXMessage struct {
	Header Header `xml:"header,omitempty"`
	Product []Product `xml:"product,omitempty"`
	MainSeriesRecord []MainSeriesRecord `xml:"mainseriesrecord,omitempty"`
	SubSeriesRecord []SubSeriesRecord `xml:"subseriesrecord,omitempty"`
}

// OnOrderDetail is not documented.
type OnOrderDetail struct {
	OnOrder string `xml:"j351"`
	ExpectedDate string `xml:"j302"`
}

// OtherText is not documented.
type OtherText struct {
	Text Text `xml:"d104,omitempty"`
	TextLinkType TextLinkType `xml:"d105,omitempty"`
	StartDate StartDate `xml:"b324,omitempty"`
	EndDate EndDate `xml:"b325,omitempty"`
	TextLink string `xml:"d106,omitempty"`
	TextTypeCode TextTypeCode `xml:"d102"`
	TextFormat TextFormat `xml:"d103,omitempty"`
	TextAuthor string `xml:"d107,omitempty"`
	TextSourceCorporate string `xml:"b374,omitempty"`
	TextSourceTitle string `xml:"d108,omitempty"`
	TextPublicationDate string `xml:"d109,omitempty"`
}

// PageRun is not documented.
type PageRun struct {
	FirstPageNumber string `xml:"b286"`
	LastPageNumber string `xml:"b287,omitempty"`
}

// ParentIdentifier is not documented.
type ParentIdentifier struct {
	SeriesIDType SeriesIDType `xml:"b273"`
	IDTypeName string `xml:"b233,omitempty"`
	IDValue string `xml:"b244"`
}

// PersonAsSubject is not documented.
type PersonAsSubject struct {
	TitlesBeforeNames TitlesBeforeNames `xml:"b038,omitempty"`
	NamesBeforeKey NamesBeforeKey `xml:"b039,omitempty"`
	PrefixToKey PrefixToKey `xml:"b247,omitempty"`
	KeyNames KeyNames `xml:"b040,omitempty"`
	NamesAfterKey NamesAfterKey `xml:"b041,omitempty"`
	SuffixToKey SuffixToKey `xml:"b248,omitempty"`
	LettersAfterNames LettersAfterNames `xml:"b042,omitempty"`
	TitlesAfterNames TitlesAfterNames `xml:"b043,omitempty"`
	Name Name `xml:"name,omitempty"`
	PersonNameIdentifier []PersonNameIdentifier `xml:"personnameidentifier,omitempty"`
	PersonName string `xml:"b036,omitempty"`
	PersonNameInverted string `xml:"b037,omitempty"`
}

// PersonDate is not documented.
type PersonDate struct {
	PersonDateRole PersonDateRole `xml:"b305"`
	DateFormat DateFormat `xml:"j260,omitempty"`
	Date string `xml:"b306"`
}

// PersonNameIdentifier is not documented.
type PersonNameIdentifier struct {
	PersonNameIDType PersonNameIDType `xml:"b390"`
	IDTypeName string `xml:"b233,omitempty"`
	IDValue string `xml:"b244"`
}

// Price is not documented.
type Price struct {
	PriceTypeCode PriceTypeCode `xml:"j148,omitempty"`
	PriceQualifier PriceQualifier `xml:"j261,omitempty"`
	PriceTypeDescription string `xml:"j262,omitempty"`
	PricePer PricePer `xml:"j239,omitempty"`
	MinimumOrderQuantity string `xml:"j263,omitempty"`
	BatchBonus []BatchBonus `xml:"batchbonus,omitempty"`
	ClassOfTrade string `xml:"j149,omitempty"`
	BICDiscountGroupCode string `xml:"j150,omitempty"`
	DiscountCoded []DiscountCoded `xml:"discountcoded,omitempty"`
	DiscountPercent string `xml:"j267,omitempty"`
	PriceStatus PriceStatus `xml:"j266,omitempty"`
	PriceAmount string `xml:"j151"`
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
	CurrencyCode CurrencyCode `xml:"j152,omitempty"`
	PriceEffectiveFrom string `xml:"j161,omitempty"`
	PriceEffectiveUntil string `xml:"j162,omitempty"`
}

// Prize is not documented.
type Prize struct {
	PrizeName string `xml:"g126"`
	PrizeYear string `xml:"g127,omitempty"`
	PrizeCountry PrizeCountry `xml:"g128,omitempty"`
	PrizeCode PrizeCode `xml:"g129,omitempty"`
	PrizeJury PrizeJury `xml:"g343,omitempty"`
}

// Product is not documented.
type Product struct {
	RecordReference string `xml:"a001"`
	NotificationType NotificationType `xml:"a002"`
	DeletionCode DeletionCode `xml:"a198"`
	DeletionText string `xml:"a199"`
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
	ReplacedByISBN string `xml:"h130"`
	ReplacedByEAN13 string `xml:"h131"`
	AlternativeFormatISBN string `xml:"h132"`
	AlternativeFormatEAN13 string `xml:"h133"`
	AlternativeProductISBN string `xml:"h163"`
	AlternativeProductEAN13 string `xml:"h164"`
	RelatedProduct RelatedProduct `xml:"relatedproduct"`
	OutOfPrintDate string `xml:"h134"`
	SupplyDetail SupplyDetail `xml:"supplydetail"`
	MarketRepresentation MarketRepresentation `xml:"marketrepresentation"`
	PromotionCampaign string `xml:"k165"`
	PromotionContact string `xml:"k166"`
	InitialPrintRun string `xml:"k167"`
	ReprintDetail string `xml:"k309"`
	CopiesSold string `xml:"k168"`
	BookClubAdoption string `xml:"k169"`
}

// ProductClassification is not documented.
type ProductClassification struct {
	ProductClassificationType ProductClassificationType `xml:"b274"`
	ProductClassificationCode string `xml:"b275"`
	Percent string `xml:"b337,omitempty"`
}

// ProductFormFeature is not documented.
type ProductFormFeature struct {
	ProductFormFeatureType ProductFormFeatureType `xml:"b334"`
	ProductFormFeatureValue string `xml:"b335,omitempty"`
	ProductFormFeatureDescription string `xml:"b336,omitempty"`
}

// ProductIdentifier is not documented.
type ProductIdentifier struct {
	ProductIDType ProductIDType `xml:"b221"`
	IDTypeName string `xml:"b233,omitempty"`
	IDValue string `xml:"b244"`
}

// ProductWebsite is not documented.
type ProductWebsite struct {
	WebsiteRole WebsiteRole `xml:"b367,omitempty"`
	ProductWebsiteDescription ProductWebsiteDescription `xml:"f170,omitempty"`
	ProductWebsiteLink string `xml:"f123"`
}

// ProfessionalAffiliation is not documented.
type ProfessionalAffiliation struct {
	Affiliation string `xml:"b046,omitempty"`
	ProfessionalPosition string `xml:"b045,omitempty"`
}

// Publisher is not documented.
type Publisher struct {
	PublisherName string `xml:"b081,omitempty"`
	NameCodeType NameCodeType `xml:"b241,omitempty"`
	NameCodeTypeName string `xml:"b242,omitempty"`
	NameCodeValue string `xml:"b243,omitempty"`
	PublishingRole PublishingRole `xml:"b291,omitempty"`
	Website []Website `xml:"website,omitempty"`
}

// Reissue is not documented.
type Reissue struct {
	ReissueDate string `xml:"j365"`
	ReissueDescription string `xml:"j366,omitempty"`
	Price []Price `xml:"price,omitempty"`
	MediaFile []MediaFile `xml:"mediafile,omitempty"`
}

// RelatedProduct is not documented.
type RelatedProduct struct {
	ISBN string `xml:"b004,omitempty"`
	EAN13 string `xml:"b005,omitempty"`
	ProductIdentifier []ProductIdentifier `xml:"productidentifier,omitempty"`
	Website []Website `xml:"website,omitempty"`
	ProductForm ProductForm `xml:"b012,omitempty"`
	ProductFormDetail []ProductFormDetail `xml:"b333,omitempty"`
	ProductFormFeature []ProductFormFeature `xml:"productformfeature,omitempty"`
	BookFormDetail []BookFormDetail `xml:"b013,omitempty"`
	ProductPackaging ProductPackaging `xml:"b225,omitempty"`
	EpubType EpubType `xml:"b211,omitempty"`
	EpubTypeVersion EpubTypeVersion `xml:"b212,omitempty"`
	EpubTypeDescription EpubTypeDescription `xml:"b213,omitempty"`
	EpubFormat EpubFormat `xml:"b214,omitempty"`
	EpubFormatVersion EpubFormatVersion `xml:"b215,omitempty"`
	EpubFormatDescription EpubFormatDescription `xml:"b216,omitempty"`
	EpubTypeNote EpubTypeNote `xml:"b277,omitempty"`
	ProductFormDescription string `xml:"b014,omitempty"`
	RelationCode RelationCode `xml:"h208"`
	NumberOfPieces string `xml:"b210,omitempty"`
	TradeCategory TradeCategory `xml:"b384,omitempty"`
	ProductContentType []ProductContentType `xml:"b385,omitempty"`
	Publisher []Publisher `xml:"publisher,omitempty"`
}

// ReligiousText is not documented.
type ReligiousText struct {
	Bible Bible `xml:"bible,omitempty"`
	ReligiousTextID ReligiousTextID `xml:"b376,omitempty"`
	ReligiousTextFeature []ReligiousTextFeature `xml:"religioustextfeature,omitempty"`
}

// ReligiousTextFeature is not documented.
type ReligiousTextFeature struct {
	ReligiousTextFeatureType ReligiousTextFeatureType `xml:"b358"`
	ReligiousTextFeatureCode ReligiousTextFeatureCode `xml:"b359"`
	ReligiousTextFeatureDescription string `xml:"b360,omitempty"`
}

// SalesOutlet is not documented.
type SalesOutlet struct {
	SalesOutletName string `xml:"b382,omitempty"`
	SalesOutletIdentifier SalesOutletIdentifier `xml:"salesoutletidentifier,omitempty"`
}

// SalesOutletIdentifier is not documented.
type SalesOutletIdentifier struct {
	SalesOutletIDType SalesOutletIDType `xml:"b393"`
	IDTypeName string `xml:"b233,omitempty"`
	IDValue string `xml:"b244"`
}

// SalesRestriction is not documented.
type SalesRestriction struct {
	SalesRestrictionType SalesRestrictionType `xml:"b381"`
	SalesOutlet []SalesOutlet `xml:"salesoutlet,omitempty"`
	SalesRestrictionDetail string `xml:"b383,omitempty"`
}

// SalesRights is not documented.
type SalesRights struct {
	RightsTerritory TerritoryCodeList `xml:"b388,omitempty"`
	RightsRegion []RightsRegion `xml:"b091,omitempty"`
	RightsCountry []CountryCodeList `xml:"b090,omitempty"`
	SalesRightsType SalesRightsType `xml:"b089"`
}

// SenderIdentifier is not documented.
type SenderIdentifier struct {
	SenderIDType SenderIDType `xml:"m379"`
	IDTypeName string `xml:"b233,omitempty"`
	IDValue string `xml:"b244"`
}

// Series is not documented.
type Series struct {
	Title []Title `xml:"title,omitempty"`
	TitleOfSeries string `xml:"b018,omitempty"`
	SeriesISSN string `xml:"b016,omitempty"`
	PublisherSeriesCode string `xml:"b017,omitempty"`
	SeriesIdentifier []SeriesIdentifier `xml:"seriesidentifier,omitempty"`
	Contributor []Contributor `xml:"contributor,omitempty"`
	NumberWithinSeries string `xml:"b019,omitempty"`
	YearOfAnnual string `xml:"b020,omitempty"`
}

// SeriesIdentifier is not documented.
type SeriesIdentifier struct {
	SeriesIDType SeriesIDType `xml:"b273"`
	IDTypeName string `xml:"b233,omitempty"`
	IDValue string `xml:"b244"`
}

// Set is not documented.
type Set struct {
	Title []Title `xml:"title,omitempty"`
	TitleOfSet string `xml:"b023,omitempty"`
	ISBNOfSet string `xml:"b021,omitempty"`
	EAN13OfSet string `xml:"b022,omitempty"`
	ProductIdentifier []ProductIdentifier `xml:"productidentifier,omitempty"`
	SetPartNumber string `xml:"b024,omitempty"`
	SetPartTitle string `xml:"b025,omitempty"`
	ItemNumberWithinSet string `xml:"b026,omitempty"`
	LevelSequenceNumber string `xml:"b284,omitempty"`
	SetItemTitle string `xml:"b281,omitempty"`
}

// Stock is not documented.
type Stock struct {
	OnHand string `xml:"j350,omitempty"`
	StockQuantityCoded StockQuantityCoded `xml:"stockquantitycoded,omitempty"`
	LocationIdentifier LocationIdentifier `xml:"locationidentifier,omitempty"`
	LocationName string `xml:"j349,omitempty"`
	OnOrder string `xml:"j351,omitempty"`
	CBO string `xml:"j375,omitempty"`
	OnOrderDetail []OnOrderDetail `xml:"onorderdetail,omitempty"`
}

// StockQuantityCoded is not documented.
type StockQuantityCoded struct {
	StockQuantityCodeType StockQuantityCodeType `xml:"j293"`
	StockQuantityCodeTypeName string `xml:"j296,omitempty"`
	StockQuantityCode string `xml:"j297"`
}

// SubSeriesRecord is not documented.
type SubSeriesRecord struct {
	RecordReference string `xml:"a001"`
	NotificationType NotificationType `xml:"a002"`
	RecordSourceIdentifierType RecordSourceIdentifierType `xml:"a195,omitempty"`
	RecordSourceIdentifier RecordSourceIdentifier `xml:"a196,omitempty"`
	DeletionCode DeletionCode `xml:"a198,omitempty"`
	DeletionText string `xml:"a199,omitempty"`
	RecordSourceType RecordSourceType `xml:"a194,omitempty"`
	RecordSourceName string `xml:"a197,omitempty"`
	SeriesIdentifier []SeriesIdentifier `xml:"seriesidentifier"`
	ParentIdentifier ParentIdentifier `xml:"parentidentifier"`
	SeriesPartName SeriesPartName `xml:"b282,omitempty"`
	NumberWithinSeries NumberWithinSeries `xml:"b019,omitempty"`
	LevelSequenceNumber string `xml:"b284"`
	Title []Title `xml:"title"`
	Contributor []Contributor `xml:"contributor,omitempty"`
	OtherText []OtherText `xml:"othertext,omitempty"`
	Publisher []Publisher `xml:"publisher,omitempty"`
	SubordinateEntries string `xml:"a245,omitempty"`
}

// Subject is not documented.
type Subject struct {
	SubjectHeadingText string `xml:"b070,omitempty"`
	SubjectCode string `xml:"b069,omitempty"`
	SubjectSchemeIdentifier SubjectSchemeIdentifier `xml:"b067"`
	SubjectSchemeName string `xml:"b171,omitempty"`
	SubjectSchemeVersion string `xml:"b068,omitempty"`
}

// SupplierIdentifier is not documented.
type SupplierIdentifier struct {
	SupplierIDType SupplierIDType `xml:"j345"`
	IDTypeName string `xml:"b233,omitempty"`
	IDValue string `xml:"b244"`
}

// SupplyDetail is not documented.
type SupplyDetail struct {
	TelephoneNumber string `xml:"j270"`
	FaxNumber string `xml:"j271"`
	EmailAddress string `xml:"j272"`
	Website Website `xml:"website"`
	SupplierRole SupplierRole `xml:"j292"`
	SupplyToCountry SupplyToCountry `xml:"j138,omitempty"`
	SupplyToTerritory SupplyToTerritory `xml:"j397,omitempty"`
	SupplyToRegion SupplyToRegion `xml:"j139,omitempty"`
	SupplyToCountryExcluded SupplyToCountryExcluded `xml:"j140,omitempty"`
	ReturnsCodeType ReturnsCodeType `xml:"j268,omitempty"`
	ReturnsCode ReturnsCode `xml:"j269,omitempty"`
	SupplierName string `xml:"j137,omitempty"`
	SupplierIdentifier []SupplierIdentifier `xml:"supplieridentifier,omitempty"`
	SupplierSAN string `xml:"j136,omitempty"`
	SupplierEANLocationNumber string `xml:"j135,omitempty"`
	IntermediaryAvailabilityCode IntermediaryAvailabilityCode `xml:"j348,omitempty"`
	AvailabilityCode AvailabilityCode `xml:"j141,omitempty"`
	ProductAvailability ProductAvailability `xml:"j396,omitempty"`
	SupplyRestrictionDetail string `xml:"j399"`
	LastDateForReturns string `xml:"j387"`
	NewSupplier NewSupplier `xml:"newsupplier"`
	DateFormat DateFormat `xml:"j260,omitempty"`
	ExpectedShipDate ExpectedShipDate `xml:"j142,omitempty"`
	OnSaleDate string `xml:"j143"`
	OrderTime string `xml:"j144"`
	Stock Stock `xml:"stock"`
	AudienceRestrictionFlag AudienceRestrictionFlag `xml:"j146,omitempty"`
	AudienceRestrictionNote AudienceRestrictionNote `xml:"j147,omitempty"`
	PriceAmount PriceAmount `xml:"j151"`
	UnpricedItemType UnpricedItemType `xml:"j192"`
	Price Price `xml:"price"`
	PackQuantity string `xml:"j145"`
	Reissue Reissue `xml:"reissue"`
}

// TextItem is not documented.
type TextItem struct {
	PageRun []PageRun `xml:"pagerun,omitempty"`
	FirstPageNumber string `xml:"b286,omitempty"`
	LastPageNumber string `xml:"b287,omitempty"`
	TextItemType TextItemType `xml:"b290"`
	TextItemIdentifier []TextItemIdentifier `xml:"textitemidentifier,omitempty"`
	NumberOfPages string `xml:"b061,omitempty"`
}

// TextItemIdentifier is not documented.
type TextItemIdentifier struct {
	TextItemIDType TextItemIDType `xml:"b285"`
	IDTypeName string `xml:"b233,omitempty"`
	IDValue string `xml:"b244"`
}

// Title is not documented.
type Title struct {
	TitleText string `xml:"b203,omitempty"`
	TitlePrefix string `xml:"b030,omitempty"`
	TitleWithoutPrefix string `xml:"b031,omitempty"`
	TitleType TitleType `xml:"b202"`
	AbbreviatedLength string `xml:"b276,omitempty"`
	TextCaseFlag TextCaseFlag `xml:"b027,omitempty"`
	Subtitle string `xml:"b029,omitempty"`
}

// Website is not documented.
type Website struct {
	WebsiteRole WebsiteRole `xml:"b367,omitempty"`
	WebsiteDescription WebsiteDescription `xml:"b294,omitempty"`
	WebsiteLink string `xml:"b295"`
}

// WorkIdentifier is not documented.
type WorkIdentifier struct {
	WorkIDType WorkIDType `xml:"b201"`
	IDTypeName string `xml:"b233,omitempty"`
	IDValue string `xml:"b244"`
}
