package onix


// AddresseeIdentifier is not documented.
type AddresseeIdentifier struct {
	AddresseeIDType AddresseeIDType `xml:"m380"`
	IDTypeName string `xml:"b233,omitempty" json:",omitempty"`
	IDValue string `xml:"b244"`
	Textformat TextFormatCode `xml:"textformat,omitempty,attr" json:",omitempty"`
	Textcase TextCaseCode `xml:"textcase,omitempty,attr" json:",omitempty"`
	Language LanguageList74 `xml:"language,omitempty,attr" json:",omitempty"`
	Transliteration TransliterationCode `xml:"transliteration,omitempty,attr" json:",omitempty"`
	Datestamp DateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename string `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// AgentIdentifier is not documented.
type AgentIdentifier struct {
	AgentIDType string `xml:"j400"`
	IDTypeName string `xml:"b233,omitempty" json:",omitempty"`
	IDValue string `xml:"b244"`
	Textformat TextFormatCode `xml:"textformat,omitempty,attr" json:",omitempty"`
	Textcase TextCaseCode `xml:"textcase,omitempty,attr" json:",omitempty"`
	Language LanguageList74 `xml:"language,omitempty,attr" json:",omitempty"`
	Transliteration TransliterationCode `xml:"transliteration,omitempty,attr" json:",omitempty"`
	Datestamp DateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename string `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// Audience is not documented.
type Audience struct {
	AudienceCodeType AudienceCodeType `xml:"b204"`
	AudienceCodeTypeName string `xml:"b205,omitempty" json:",omitempty"`
	AudienceCodeValue string `xml:"b206"`
	Textformat TextFormatCode `xml:"textformat,omitempty,attr" json:",omitempty"`
	Textcase TextCaseCode `xml:"textcase,omitempty,attr" json:",omitempty"`
	Language LanguageList74 `xml:"language,omitempty,attr" json:",omitempty"`
	Transliteration TransliterationCode `xml:"transliteration,omitempty,attr" json:",omitempty"`
	Datestamp DateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename string `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// AudienceRange is not documented.
type AudienceRange struct {
	AudienceRangeQualifier AudienceRangeQualifier `xml:"b074"`
	AudienceRangePrecision AudienceRangePrecision `xml:"b075,omitempty" json:",omitempty"`
	AudienceRangeValue string `xml:"b076,omitempty" json:",omitempty"`
	Textformat TextFormatCode `xml:"textformat,omitempty,attr" json:",omitempty"`
	Textcase TextCaseCode `xml:"textcase,omitempty,attr" json:",omitempty"`
	Language LanguageList74 `xml:"language,omitempty,attr" json:",omitempty"`
	Transliteration TransliterationCode `xml:"transliteration,omitempty,attr" json:",omitempty"`
	Datestamp DateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename string `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// BatchBonus is not documented.
type BatchBonus struct {
	BatchQuantity string `xml:"j264"`
	FreeQuantity string `xml:"j265"`
	Textformat TextFormatCode `xml:"textformat,omitempty,attr" json:",omitempty"`
	Textcase TextCaseCode `xml:"textcase,omitempty,attr" json:",omitempty"`
	Language LanguageList74 `xml:"language,omitempty,attr" json:",omitempty"`
	Transliteration TransliterationCode `xml:"transliteration,omitempty,attr" json:",omitempty"`
	Datestamp DateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename string `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// Bible is not documented.
type Bible struct {
	BibleContentss []BibleContents `xml:"b352"`
	BibleVersions []BibleVersion `xml:"b353"`
	StudyBibleType StudyBibleType `xml:"b389,omitempty" json:",omitempty"`
	BiblePurposes []BiblePurpose `xml:"b354,omitempty" json:",omitempty"`
	BibleTextOrganization BibleTextOrganization `xml:"b355,omitempty" json:",omitempty"`
	BibleReferenceLocation BibleReferenceLocation `xml:"b356,omitempty" json:",omitempty"`
	BibleTextFeatures []BibleTextFeature `xml:"b357,omitempty" json:",omitempty"`
	Textformat TextFormatCode `xml:"textformat,omitempty,attr" json:",omitempty"`
	Textcase TextCaseCode `xml:"textcase,omitempty,attr" json:",omitempty"`
	Language LanguageList74 `xml:"language,omitempty,attr" json:",omitempty"`
	Transliteration TransliterationCode `xml:"transliteration,omitempty,attr" json:",omitempty"`
	Datestamp DateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename string `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// Complexity is not documented.
type Complexity struct {
	ComplexitySchemeIdentifier ComplexitySchemeIdentifier `xml:"b077"`
	ComplexityCode string `xml:"b078"`
	Textformat TextFormatCode `xml:"textformat,omitempty,attr" json:",omitempty"`
	Textcase TextCaseCode `xml:"textcase,omitempty,attr" json:",omitempty"`
	Language LanguageList74 `xml:"language,omitempty,attr" json:",omitempty"`
	Transliteration TransliterationCode `xml:"transliteration,omitempty,attr" json:",omitempty"`
	Datestamp DateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename string `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// Conference is not documented.
type Conference struct {
	ConferenceRole ConferenceRole `xml:"b051,omitempty" json:",omitempty"`
	ConferenceName string `xml:"b052"`
	ConferenceAcronym string `xml:"b341,omitempty" json:",omitempty"`
	ConferenceNumber string `xml:"b053,omitempty" json:",omitempty"`
	ConferenceTheme string `xml:"b342,omitempty" json:",omitempty"`
	ConferenceDate string `xml:"b054,omitempty" json:",omitempty"`
	ConferencePlace string `xml:"b055,omitempty" json:",omitempty"`
	ConferenceSponsors []ConferenceSponsor `xml:"conferencesponsor,omitempty" json:",omitempty"`
	Websites []Website `xml:"website,omitempty" json:",omitempty"`
	Textformat TextFormatCode `xml:"textformat,omitempty,attr" json:",omitempty"`
	Textcase TextCaseCode `xml:"textcase,omitempty,attr" json:",omitempty"`
	Language LanguageList74 `xml:"language,omitempty,attr" json:",omitempty"`
	Transliteration TransliterationCode `xml:"transliteration,omitempty,attr" json:",omitempty"`
	Datestamp DateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename string `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// ConferenceSponsor is not documented.
type ConferenceSponsor struct {
	PersonName string `xml:"b036,omitempty" json:",omitempty"`
	CorporateName string `xml:"b047,omitempty" json:",omitempty"`
	ConferenceSponsorIdentifier ConferenceSponsorIdentifier `xml:"conferencesponsoridentifier,omitempty" json:",omitempty"`
	Textformat TextFormatCode `xml:"textformat,omitempty,attr" json:",omitempty"`
	Textcase TextCaseCode `xml:"textcase,omitempty,attr" json:",omitempty"`
	Language LanguageList74 `xml:"language,omitempty,attr" json:",omitempty"`
	Transliteration TransliterationCode `xml:"transliteration,omitempty,attr" json:",omitempty"`
	Datestamp DateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename string `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// ConferenceSponsorIdentifier is not documented.
type ConferenceSponsorIdentifier struct {
	ConferenceSponsorIDType ConferenceSponsorIDType `xml:"b391"`
	IDTypeName string `xml:"b233,omitempty" json:",omitempty"`
	IDValue string `xml:"b244"`
	Textformat TextFormatCode `xml:"textformat,omitempty,attr" json:",omitempty"`
	Textcase TextCaseCode `xml:"textcase,omitempty,attr" json:",omitempty"`
	Language LanguageList74 `xml:"language,omitempty,attr" json:",omitempty"`
	Transliteration TransliterationCode `xml:"transliteration,omitempty,attr" json:",omitempty"`
	Datestamp DateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename string `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// ContainedItem is not documented.
type ContainedItem struct {
	ISBN string `xml:"b004,omitempty" json:",omitempty"`
	EAN13 string `xml:"b005,omitempty" json:",omitempty"`
	ProductIdentifiers []ProductIdentifier `xml:"productidentifier,omitempty" json:",omitempty"`
	ProductForm ProductForm `xml:"b012,omitempty" json:",omitempty"`
	ProductFormDetails []ProductFormDetail `xml:"b333,omitempty" json:",omitempty"`
	ProductFormFeatures []ProductFormFeature `xml:"productformfeature,omitempty" json:",omitempty"`
	BookFormDetails []BookFormDetail `xml:"b013,omitempty" json:",omitempty"`
	ProductPackaging ProductPackaging `xml:"b225,omitempty" json:",omitempty"`
	ProductFormDescription string `xml:"b014,omitempty" json:",omitempty"`
	NumberOfPieces string `xml:"b210,omitempty" json:",omitempty"`
	TradeCategory TradeCategory `xml:"b384,omitempty" json:",omitempty"`
	ProductContentTypes []ProductContentType `xml:"b385,omitempty" json:",omitempty"`
	ItemQuantity string `xml:"b015,omitempty" json:",omitempty"`
	Textformat TextFormatCode `xml:"textformat,omitempty,attr" json:",omitempty"`
	Textcase TextCaseCode `xml:"textcase,omitempty,attr" json:",omitempty"`
	Language LanguageList74 `xml:"language,omitempty,attr" json:",omitempty"`
	Transliteration TransliterationCode `xml:"transliteration,omitempty,attr" json:",omitempty"`
	Datestamp DateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename string `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// ContentItem is not documented.
type ContentItem struct {
	Titles []Title `xml:"title,omitempty" json:",omitempty"`
	ComponentTypeName string `xml:"b288,omitempty" json:",omitempty"`
	ComponentNumber string `xml:"b289,omitempty" json:",omitempty"`
	DistinctiveTitle string `xml:"b028,omitempty" json:",omitempty"`
	LevelSequenceNumber string `xml:"b284,omitempty" json:",omitempty"`
	TextItem TextItem `xml:"textitem"`
	Websites []Website `xml:"website,omitempty" json:",omitempty"`
	WorkIdentifiers []WorkIdentifier `xml:"workidentifier,omitempty" json:",omitempty"`
	Subjects []Subject `xml:"subject,omitempty" json:",omitempty"`
	PersonAsSubjects []PersonAsSubject `xml:"personassubject,omitempty" json:",omitempty"`
	CorporateBodyAsSubjects []string `xml:"b071,omitempty" json:",omitempty"`
	PlaceAsSubjects []string `xml:"b072,omitempty" json:",omitempty"`
	OtherTexts []OtherText `xml:"othertext,omitempty" json:",omitempty"`
	MediaFiles []MediaFile `xml:"mediafile,omitempty" json:",omitempty"`
	Contributors []Contributor `xml:"contributor,omitempty" json:",omitempty"`
	ContributorStatement string `xml:"b049,omitempty" json:",omitempty"`
	Textformat TextFormatCode `xml:"textformat,omitempty,attr" json:",omitempty"`
	Textcase TextCaseCode `xml:"textcase,omitempty,attr" json:",omitempty"`
	Language LanguageList74 `xml:"language,omitempty,attr" json:",omitempty"`
	Transliteration TransliterationCode `xml:"transliteration,omitempty,attr" json:",omitempty"`
	Datestamp DateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename string `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// Contributor is not documented.
type Contributor struct {
	SequenceNumberWithinRole string `xml:"b340,omitempty" json:",omitempty"`
	ContributorRole ContributorRole `xml:"b035,omitempty" json:",omitempty"`
	LanguageCodes []LanguageCode `xml:"b252,omitempty" json:",omitempty"`
	UnnamedPersons UnnamedPersons `xml:"b249,omitempty" json:",omitempty"`
	CorporateName string `xml:"b047,omitempty" json:",omitempty"`
	PersonNameIdentifiers []PersonNameIdentifier `xml:"personnameidentifier,omitempty" json:",omitempty"`
	PersonName string `xml:"b036,omitempty" json:",omitempty"`
	PersonNameInverted string `xml:"b037,omitempty" json:",omitempty"`
	Names []Name `xml:"name,omitempty" json:",omitempty"`
	TitlesBeforeNames string `xml:"b038,omitempty" json:",omitempty"`
	NamesBeforeKey string `xml:"b039,omitempty" json:",omitempty"`
	PrefixToKey string `xml:"b247,omitempty" json:",omitempty"`
	KeyNames string `xml:"b040,omitempty" json:",omitempty"`
	NamesAfterKey string `xml:"b041,omitempty" json:",omitempty"`
	SuffixToKey string `xml:"b248,omitempty" json:",omitempty"`
	LettersAfterNames string `xml:"b042,omitempty" json:",omitempty"`
	TitlesAfterNames string `xml:"b043,omitempty" json:",omitempty"`
	PersonDates []PersonDate `xml:"persondate,omitempty" json:",omitempty"`
	ProfessionalAffiliations []ProfessionalAffiliation `xml:"professionalaffiliation,omitempty" json:",omitempty"`
	BiographicalNote BiographicalNote `xml:"b044,omitempty" json:",omitempty"`
	Websites []Website `xml:"website,omitempty" json:",omitempty"`
	ProfessionalPosition string `xml:"b045,omitempty" json:",omitempty"`
	Affiliation string `xml:"b046,omitempty" json:",omitempty"`
	ContributorDescription string `xml:"b048,omitempty" json:",omitempty"`
	SequenceNumber string `xml:"b034,omitempty" json:",omitempty"`
	CountryCodes []CountryCode `xml:"b251,omitempty" json:",omitempty"`
	RegionCodes []string `xml:"b398,omitempty" json:",omitempty"`
	Textformat TextFormatCode `xml:"textformat,omitempty,attr" json:",omitempty"`
	Textcase TextCaseCode `xml:"textcase,omitempty,attr" json:",omitempty"`
	Language LanguageList74 `xml:"language,omitempty,attr" json:",omitempty"`
	Transliteration TransliterationCode `xml:"transliteration,omitempty,attr" json:",omitempty"`
	Datestamp DateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename string `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// CopyrightOwner is not documented.
type CopyrightOwner struct {
	PersonName string `xml:"b036,omitempty" json:",omitempty"`
	CorporateName string `xml:"b047,omitempty" json:",omitempty"`
	CopyrightOwnerIdentifier CopyrightOwnerIdentifier `xml:"copyrightowneridentifier,omitempty" json:",omitempty"`
	Textformat TextFormatCode `xml:"textformat,omitempty,attr" json:",omitempty"`
	Textcase TextCaseCode `xml:"textcase,omitempty,attr" json:",omitempty"`
	Language LanguageList74 `xml:"language,omitempty,attr" json:",omitempty"`
	Transliteration TransliterationCode `xml:"transliteration,omitempty,attr" json:",omitempty"`
	Datestamp DateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename string `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// CopyrightOwnerIdentifier is not documented.
type CopyrightOwnerIdentifier struct {
	CopyrightOwnerIDType CopyrightOwnerIDType `xml:"b392"`
	IDTypeName string `xml:"b233,omitempty" json:",omitempty"`
	IDValue string `xml:"b244"`
	Textformat TextFormatCode `xml:"textformat,omitempty,attr" json:",omitempty"`
	Textcase TextCaseCode `xml:"textcase,omitempty,attr" json:",omitempty"`
	Language LanguageList74 `xml:"language,omitempty,attr" json:",omitempty"`
	Transliteration TransliterationCode `xml:"transliteration,omitempty,attr" json:",omitempty"`
	Datestamp DateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename string `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// CopyrightStatement is not documented.
type CopyrightStatement struct {
	CopyrightYears []string `xml:"b087"`
	CopyrightOwners []CopyrightOwner `xml:"copyrightowner"`
	Textformat TextFormatCode `xml:"textformat,omitempty,attr" json:",omitempty"`
	Textcase TextCaseCode `xml:"textcase,omitempty,attr" json:",omitempty"`
	Language LanguageList74 `xml:"language,omitempty,attr" json:",omitempty"`
	Transliteration TransliterationCode `xml:"transliteration,omitempty,attr" json:",omitempty"`
	Datestamp DateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename string `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// DiscountCoded is not documented.
type DiscountCoded struct {
	DiscountCodeType DiscountCodeType `xml:"j363"`
	DiscountCodeTypeName string `xml:"j378,omitempty" json:",omitempty"`
	DiscountCode string `xml:"j364"`
	Textformat TextFormatCode `xml:"textformat,omitempty,attr" json:",omitempty"`
	Textcase TextCaseCode `xml:"textcase,omitempty,attr" json:",omitempty"`
	Language LanguageList74 `xml:"language,omitempty,attr" json:",omitempty"`
	Transliteration TransliterationCode `xml:"transliteration,omitempty,attr" json:",omitempty"`
	Datestamp DateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename string `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// Extent is not documented.
type Extent struct {
	ExtentType ExtentType `xml:"b218"`
	ExtentValue string `xml:"b219"`
	ExtentUnit ExtentUnit `xml:"b220"`
	Textformat TextFormatCode `xml:"textformat,omitempty,attr" json:",omitempty"`
	Textcase TextCaseCode `xml:"textcase,omitempty,attr" json:",omitempty"`
	Language LanguageList74 `xml:"language,omitempty,attr" json:",omitempty"`
	Transliteration TransliterationCode `xml:"transliteration,omitempty,attr" json:",omitempty"`
	Datestamp DateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename string `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// Header is not documented.
type Header struct {
	FromCompany string `xml:"m174,omitempty" json:",omitempty"`
	FromEANNumber string `xml:"m172,omitempty" json:",omitempty"`
	FromSAN string `xml:"m173,omitempty" json:",omitempty"`
	SenderIdentifiers []SenderIdentifier `xml:"senderidentifier,omitempty" json:",omitempty"`
	FromPerson string `xml:"m175,omitempty" json:",omitempty"`
	FromEmail string `xml:"m283,omitempty" json:",omitempty"`
	ToEANNumber string `xml:"m176,omitempty" json:",omitempty"`
	ToSAN string `xml:"m177,omitempty" json:",omitempty"`
	AddresseeIdentifiers []AddresseeIdentifier `xml:"addresseeidentifier,omitempty" json:",omitempty"`
	ToCompany string `xml:"m178,omitempty" json:",omitempty"`
	ToPerson string `xml:"m179,omitempty" json:",omitempty"`
	MessageNumber string `xml:"m180,omitempty" json:",omitempty"`
	MessageRepeat string `xml:"m181,omitempty" json:",omitempty"`
	SentDate string `xml:"m182"`
	MessageNote string `xml:"m183,omitempty" json:",omitempty"`
	DefaultLanguageOfText DefaultLanguageOfText `xml:"m184,omitempty" json:",omitempty"`
	DefaultPriceTypeCode DefaultPriceTypeCode `xml:"m185,omitempty" json:",omitempty"`
	DefaultCurrencyCode DefaultCurrencyCode `xml:"m186,omitempty" json:",omitempty"`
	DefaultLinearUnit DefaultLinearUnit `xml:"m187,omitempty" json:",omitempty"`
	DefaultWeightUnit DefaultWeightUnit `xml:"m188,omitempty" json:",omitempty"`
	DefaultClassOfTrade string `xml:"m193,omitempty" json:",omitempty"`
	Textformat TextFormatCode `xml:"textformat,omitempty,attr" json:",omitempty"`
	Textcase TextCaseCode `xml:"textcase,omitempty,attr" json:",omitempty"`
	Language LanguageList74 `xml:"language,omitempty,attr" json:",omitempty"`
	Transliteration TransliterationCode `xml:"transliteration,omitempty,attr" json:",omitempty"`
	Datestamp DateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename string `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// Illustrations is not documented.
type Illustrations struct {
	IllustrationType IllustrationType `xml:"b256"`
	IllustrationTypeDescription string `xml:"b361,omitempty" json:",omitempty"`
	Number string `xml:"b257,omitempty" json:",omitempty"`
	Textformat TextFormatCode `xml:"textformat,omitempty,attr" json:",omitempty"`
	Textcase TextCaseCode `xml:"textcase,omitempty,attr" json:",omitempty"`
	Language LanguageList74 `xml:"language,omitempty,attr" json:",omitempty"`
	Transliteration TransliterationCode `xml:"transliteration,omitempty,attr" json:",omitempty"`
	Datestamp DateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename string `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// Imprint is not documented.
type Imprint struct {
	ImprintName string `xml:"b079,omitempty" json:",omitempty"`
	NameCodeType NameCodeType `xml:"b241,omitempty" json:",omitempty"`
	NameCodeTypeName string `xml:"b242,omitempty" json:",omitempty"`
	NameCodeValue string `xml:"b243,omitempty" json:",omitempty"`
	Textformat TextFormatCode `xml:"textformat,omitempty,attr" json:",omitempty"`
	Textcase TextCaseCode `xml:"textcase,omitempty,attr" json:",omitempty"`
	Language LanguageList74 `xml:"language,omitempty,attr" json:",omitempty"`
	Transliteration TransliterationCode `xml:"transliteration,omitempty,attr" json:",omitempty"`
	Datestamp DateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename string `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// Language is not documented.
type Language struct {
	LanguageRole LanguageRole `xml:"b253"`
	LanguageCode LanguageCode `xml:"b252"`
	CountryCode CountryCode `xml:"b251,omitempty" json:",omitempty"`
	Textformat TextFormatCode `xml:"textformat,omitempty,attr" json:",omitempty"`
	Textcase TextCaseCode `xml:"textcase,omitempty,attr" json:",omitempty"`
	Language LanguageList74 `xml:"language,omitempty,attr" json:",omitempty"`
	Transliteration TransliterationCode `xml:"transliteration,omitempty,attr" json:",omitempty"`
	Datestamp DateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename string `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// LocationIdentifier is not documented.
type LocationIdentifier struct {
	LocationIDType LocationIDType `xml:"j377"`
	IDTypeName string `xml:"b233,omitempty" json:",omitempty"`
	IDValue string `xml:"b244"`
	Textformat TextFormatCode `xml:"textformat,omitempty,attr" json:",omitempty"`
	Textcase TextCaseCode `xml:"textcase,omitempty,attr" json:",omitempty"`
	Language LanguageList74 `xml:"language,omitempty,attr" json:",omitempty"`
	Transliteration TransliterationCode `xml:"transliteration,omitempty,attr" json:",omitempty"`
	Datestamp DateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename string `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// MainSeriesRecord is not documented.
type MainSeriesRecord struct {
	RecordReference string `xml:"a001"`
	NotificationType NotificationType `xml:"a002"`
	DeletionCode DeletionCode `xml:"a198,omitempty" json:",omitempty"`
	DeletionText string `xml:"a199,omitempty" json:",omitempty"`
	RecordSourceType RecordSourceType `xml:"a194,omitempty" json:",omitempty"`
	RecordSourceName string `xml:"a197,omitempty" json:",omitempty"`
	SeriesIdentifiers []SeriesIdentifier `xml:"seriesidentifier"`
	Titles []Title `xml:"title"`
	Contributors []Contributor `xml:"contributor,omitempty" json:",omitempty"`
	OtherTexts []OtherText `xml:"othertext,omitempty" json:",omitempty"`
	Publishers []Publisher `xml:"publisher,omitempty" json:",omitempty"`
	SubordinateEntries string `xml:"a245,omitempty" json:",omitempty"`
	RecordSourceIdentifierType RecordSourceIdentifierType `xml:"a195,omitempty" json:",omitempty"`
	RecordSourceIdentifier string `xml:"a196,omitempty" json:",omitempty"`
	Textformat TextFormatCode `xml:"textformat,omitempty,attr" json:",omitempty"`
	Textcase TextCaseCode `xml:"textcase,omitempty,attr" json:",omitempty"`
	Language LanguageList74 `xml:"language,omitempty,attr" json:",omitempty"`
	Transliteration TransliterationCode `xml:"transliteration,omitempty,attr" json:",omitempty"`
	Datestamp DateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename string `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// MainSubject is not documented.
type MainSubject struct {
	SubjectHeadingText string `xml:"b070,omitempty" json:",omitempty"`
	SubjectCode string `xml:"b069,omitempty" json:",omitempty"`
	MainSubjectSchemeIdentifier MainSubjectSchemeIdentifier `xml:"b191"`
	SubjectSchemeVersion string `xml:"b068,omitempty" json:",omitempty"`
	Textformat TextFormatCode `xml:"textformat,omitempty,attr" json:",omitempty"`
	Textcase TextCaseCode `xml:"textcase,omitempty,attr" json:",omitempty"`
	Language LanguageList74 `xml:"language,omitempty,attr" json:",omitempty"`
	Transliteration TransliterationCode `xml:"transliteration,omitempty,attr" json:",omitempty"`
	Datestamp DateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename string `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// MarketDate is not documented.
type MarketDate struct {
	MarketDateRole string `xml:"j408"`
	DateFormat DateFormat `xml:"j260,omitempty" json:",omitempty"`
	Date string `xml:"b306"`
	Textformat TextFormatCode `xml:"textformat,omitempty,attr" json:",omitempty"`
	Textcase TextCaseCode `xml:"textcase,omitempty,attr" json:",omitempty"`
	Language LanguageList74 `xml:"language,omitempty,attr" json:",omitempty"`
	Transliteration TransliterationCode `xml:"transliteration,omitempty,attr" json:",omitempty"`
	Datestamp DateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename string `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// MarketRepresentation is not documented.
type MarketRepresentation struct {
	AgentName string `xml:"j401,omitempty" json:",omitempty"`
	AgentIdentifiers []AgentIdentifier `xml:"agentidentifier,omitempty" json:",omitempty"`
	MarketCountry string `xml:"j403,omitempty" json:",omitempty"`
	MarketTerritory string `xml:"j404,omitempty" json:",omitempty"`
	MarketCountryExcluded string `xml:"j405,omitempty" json:",omitempty"`
	TelephoneNumbers []string `xml:"j270,omitempty" json:",omitempty"`
	FaxNumbers []string `xml:"j271,omitempty" json:",omitempty"`
	EmailAddresss []string `xml:"j272,omitempty" json:",omitempty"`
	Websites []Website `xml:"website,omitempty" json:",omitempty"`
	AgentRole string `xml:"j402,omitempty" json:",omitempty"`
	MarketRestrictionDetail string `xml:"j406,omitempty" json:",omitempty"`
	MarketPublishingStatus string `xml:"j407,omitempty" json:",omitempty"`
	MarketDates []MarketDate `xml:"marketdate,omitempty" json:",omitempty"`
	Textformat TextFormatCode `xml:"textformat,omitempty,attr" json:",omitempty"`
	Textcase TextCaseCode `xml:"textcase,omitempty,attr" json:",omitempty"`
	Language LanguageList74 `xml:"language,omitempty,attr" json:",omitempty"`
	Transliteration TransliterationCode `xml:"transliteration,omitempty,attr" json:",omitempty"`
	Datestamp DateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename string `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// Measure is not documented.
type Measure struct {
	MeasureTypeCode MeasureTypeCode `xml:"c093"`
	Measurement string `xml:"c094"`
	MeasureUnitCode MeasureUnitCode `xml:"c095"`
	Textformat TextFormatCode `xml:"textformat,omitempty,attr" json:",omitempty"`
	Textcase TextCaseCode `xml:"textcase,omitempty,attr" json:",omitempty"`
	Language LanguageList74 `xml:"language,omitempty,attr" json:",omitempty"`
	Transliteration TransliterationCode `xml:"transliteration,omitempty,attr" json:",omitempty"`
	Datestamp DateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename string `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// MediaFile is not documented.
type MediaFile struct {
	TextWithDownload TextWithDownload `xml:"f118,omitempty" json:",omitempty"`
	DownloadCopyrightNotice DownloadCopyrightNotice `xml:"f121,omitempty" json:",omitempty"`
	DownloadCaption DownloadCaption `xml:"f119,omitempty" json:",omitempty"`
	DownloadCredit DownloadCredit `xml:"f120,omitempty" json:",omitempty"`
	MediaFileTypeCode MediaFileTypeCode `xml:"f114"`
	MediaFileFormatCode MediaFileFormatCode `xml:"f115,omitempty" json:",omitempty"`
	ImageResolution string `xml:"f259,omitempty" json:",omitempty"`
	MediaFileLinkTypeCode MediaFileLinkTypeCode `xml:"f116"`
	MediaFileLink string `xml:"f117"`
	DownloadTerms DownloadTerms `xml:"f122,omitempty" json:",omitempty"`
	MediaFileDate string `xml:"f373,omitempty" json:",omitempty"`
	Textformat TextFormatCode `xml:"textformat,omitempty,attr" json:",omitempty"`
	Textcase TextCaseCode `xml:"textcase,omitempty,attr" json:",omitempty"`
	Language LanguageList74 `xml:"language,omitempty,attr" json:",omitempty"`
	Transliteration TransliterationCode `xml:"transliteration,omitempty,attr" json:",omitempty"`
	Datestamp DateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename string `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// Name is not documented.
type Name struct {
	PersonNameIdentifiers []PersonNameIdentifier `xml:"personnameidentifier,omitempty" json:",omitempty"`
	PersonName string `xml:"b036,omitempty" json:",omitempty"`
	PersonNameInverted string `xml:"b037,omitempty" json:",omitempty"`
	TitlesBeforeNames string `xml:"b038,omitempty" json:",omitempty"`
	NamesBeforeKey string `xml:"b039,omitempty" json:",omitempty"`
	PrefixToKey string `xml:"b247,omitempty" json:",omitempty"`
	KeyNames string `xml:"b040,omitempty" json:",omitempty"`
	NamesAfterKey string `xml:"b041,omitempty" json:",omitempty"`
	SuffixToKey string `xml:"b248,omitempty" json:",omitempty"`
	LettersAfterNames string `xml:"b042,omitempty" json:",omitempty"`
	TitlesAfterNames string `xml:"b043,omitempty" json:",omitempty"`
	PersonNameType PersonNameType `xml:"b250"`
	Textformat TextFormatCode `xml:"textformat,omitempty,attr" json:",omitempty"`
	Textcase TextCaseCode `xml:"textcase,omitempty,attr" json:",omitempty"`
	Language LanguageList74 `xml:"language,omitempty,attr" json:",omitempty"`
	Transliteration TransliterationCode `xml:"transliteration,omitempty,attr" json:",omitempty"`
	Datestamp DateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename string `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// NewSupplier is not documented.
type NewSupplier struct {
	SupplierName string `xml:"j137,omitempty" json:",omitempty"`
	SupplierIdentifiers []SupplierIdentifier `xml:"supplieridentifier,omitempty" json:",omitempty"`
	SupplierSAN string `xml:"j136,omitempty" json:",omitempty"`
	SupplierEANLocationNumber string `xml:"j135,omitempty" json:",omitempty"`
	TelephoneNumbers []string `xml:"j270,omitempty" json:",omitempty"`
	FaxNumbers []string `xml:"j271,omitempty" json:",omitempty"`
	EmailAddresss []string `xml:"j272,omitempty" json:",omitempty"`
	Textformat TextFormatCode `xml:"textformat,omitempty,attr" json:",omitempty"`
	Textcase TextCaseCode `xml:"textcase,omitempty,attr" json:",omitempty"`
	Language LanguageList74 `xml:"language,omitempty,attr" json:",omitempty"`
	Transliteration TransliterationCode `xml:"transliteration,omitempty,attr" json:",omitempty"`
	Datestamp DateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename string `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// NoContributor is not documented.
type NoContributor struct {
	Textformat TextFormatCode `xml:"textformat,omitempty,attr" json:",omitempty"`
	Textcase TextCaseCode `xml:"textcase,omitempty,attr" json:",omitempty"`
	Language LanguageList74 `xml:"language,omitempty,attr" json:",omitempty"`
	Transliteration TransliterationCode `xml:"transliteration,omitempty,attr" json:",omitempty"`
	Datestamp DateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename string `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// NoEdition is not documented.
type NoEdition struct {
	Textformat TextFormatCode `xml:"textformat,omitempty,attr" json:",omitempty"`
	Textcase TextCaseCode `xml:"textcase,omitempty,attr" json:",omitempty"`
	Language LanguageList74 `xml:"language,omitempty,attr" json:",omitempty"`
	Transliteration TransliterationCode `xml:"transliteration,omitempty,attr" json:",omitempty"`
	Datestamp DateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename string `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// NoSeries is not documented.
type NoSeries struct {
	Textformat TextFormatCode `xml:"textformat,omitempty,attr" json:",omitempty"`
	Textcase TextCaseCode `xml:"textcase,omitempty,attr" json:",omitempty"`
	Language LanguageList74 `xml:"language,omitempty,attr" json:",omitempty"`
	Transliteration TransliterationCode `xml:"transliteration,omitempty,attr" json:",omitempty"`
	Datestamp DateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename string `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// NotForSale is not documented.
type NotForSale struct {
	RightsTerritory TerritoryCodeList `xml:"b388,omitempty" json:",omitempty"`
	RightsCountrys []CountryCodeList `xml:"b090,omitempty" json:",omitempty"`
	ISBN string `xml:"b004,omitempty" json:",omitempty"`
	EAN13 string `xml:"b005,omitempty" json:",omitempty"`
	ProductIdentifiers []ProductIdentifier `xml:"productidentifier,omitempty" json:",omitempty"`
	PublisherName string `xml:"b081,omitempty" json:",omitempty"`
	Textformat TextFormatCode `xml:"textformat,omitempty,attr" json:",omitempty"`
	Textcase TextCaseCode `xml:"textcase,omitempty,attr" json:",omitempty"`
	Language LanguageList74 `xml:"language,omitempty,attr" json:",omitempty"`
	Transliteration TransliterationCode `xml:"transliteration,omitempty,attr" json:",omitempty"`
	Datestamp DateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename string `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// ONIXMessage is not documented.
type ONIXMessage struct {
	Header Header `xml:"header,omitempty" json:",omitempty"`
	Products []Product `xml:"product,omitempty" json:",omitempty"`
	MainSeriesRecords []MainSeriesRecord `xml:"mainseriesrecord,omitempty" json:",omitempty"`
	SubSeriesRecords []SubSeriesRecord `xml:"subseriesrecord,omitempty" json:",omitempty"`
	Textformat TextFormatCode `xml:"textformat,omitempty,attr" json:",omitempty"`
	Textcase TextCaseCode `xml:"textcase,omitempty,attr" json:",omitempty"`
	Language LanguageList74 `xml:"language,omitempty,attr" json:",omitempty"`
	Transliteration TransliterationCode `xml:"transliteration,omitempty,attr" json:",omitempty"`
	Datestamp DateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename string `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// OnOrderDetail is not documented.
type OnOrderDetail struct {
	OnOrder string `xml:"j351"`
	ExpectedDate string `xml:"j302"`
	Textformat TextFormatCode `xml:"textformat,omitempty,attr" json:",omitempty"`
	Textcase TextCaseCode `xml:"textcase,omitempty,attr" json:",omitempty"`
	Language LanguageList74 `xml:"language,omitempty,attr" json:",omitempty"`
	Transliteration TransliterationCode `xml:"transliteration,omitempty,attr" json:",omitempty"`
	Datestamp DateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename string `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// OtherText is not documented.
type OtherText struct {
	Text Text `xml:"d104,omitempty" json:",omitempty"`
	TextLinkType TextLinkType `xml:"d105,omitempty" json:",omitempty"`
	TextLink string `xml:"d106,omitempty" json:",omitempty"`
	TextTypeCode TextTypeCode `xml:"d102"`
	TextFormat TextFormat `xml:"d103,omitempty" json:",omitempty"`
	TextAuthor string `xml:"d107,omitempty" json:",omitempty"`
	TextSourceCorporate string `xml:"b374,omitempty" json:",omitempty"`
	TextSourceTitle string `xml:"d108,omitempty" json:",omitempty"`
	TextPublicationDate string `xml:"d109,omitempty" json:",omitempty"`
	StartDate string `xml:"b324,omitempty" json:",omitempty"`
	EndDate string `xml:"b325,omitempty" json:",omitempty"`
	Textformat TextFormatCode `xml:"textformat,omitempty,attr" json:",omitempty"`
	Textcase TextCaseCode `xml:"textcase,omitempty,attr" json:",omitempty"`
	Language LanguageList74 `xml:"language,omitempty,attr" json:",omitempty"`
	Transliteration TransliterationCode `xml:"transliteration,omitempty,attr" json:",omitempty"`
	Datestamp DateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename string `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// PageRun is not documented.
type PageRun struct {
	FirstPageNumber string `xml:"b286"`
	LastPageNumber string `xml:"b287,omitempty" json:",omitempty"`
	Textformat TextFormatCode `xml:"textformat,omitempty,attr" json:",omitempty"`
	Textcase TextCaseCode `xml:"textcase,omitempty,attr" json:",omitempty"`
	Language LanguageList74 `xml:"language,omitempty,attr" json:",omitempty"`
	Transliteration TransliterationCode `xml:"transliteration,omitempty,attr" json:",omitempty"`
	Datestamp DateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename string `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// ParentIdentifier is not documented.
type ParentIdentifier struct {
	SeriesIDType SeriesIDType `xml:"b273"`
	IDTypeName string `xml:"b233,omitempty" json:",omitempty"`
	IDValue string `xml:"b244"`
	Textformat TextFormatCode `xml:"textformat,omitempty,attr" json:",omitempty"`
	Textcase TextCaseCode `xml:"textcase,omitempty,attr" json:",omitempty"`
	Language LanguageList74 `xml:"language,omitempty,attr" json:",omitempty"`
	Transliteration TransliterationCode `xml:"transliteration,omitempty,attr" json:",omitempty"`
	Datestamp DateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename string `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// PersonAsSubject is not documented.
type PersonAsSubject struct {
	PersonNameIdentifiers []PersonNameIdentifier `xml:"personnameidentifier,omitempty" json:",omitempty"`
	PersonName string `xml:"b036,omitempty" json:",omitempty"`
	PersonNameInverted string `xml:"b037,omitempty" json:",omitempty"`
	Names []Name `xml:"name,omitempty" json:",omitempty"`
	TitlesBeforeNames string `xml:"b038,omitempty" json:",omitempty"`
	NamesBeforeKey string `xml:"b039,omitempty" json:",omitempty"`
	PrefixToKey string `xml:"b247,omitempty" json:",omitempty"`
	KeyNames string `xml:"b040,omitempty" json:",omitempty"`
	NamesAfterKey string `xml:"b041,omitempty" json:",omitempty"`
	SuffixToKey string `xml:"b248,omitempty" json:",omitempty"`
	LettersAfterNames string `xml:"b042,omitempty" json:",omitempty"`
	TitlesAfterNames string `xml:"b043,omitempty" json:",omitempty"`
	Textformat TextFormatCode `xml:"textformat,omitempty,attr" json:",omitempty"`
	Textcase TextCaseCode `xml:"textcase,omitempty,attr" json:",omitempty"`
	Language LanguageList74 `xml:"language,omitempty,attr" json:",omitempty"`
	Transliteration TransliterationCode `xml:"transliteration,omitempty,attr" json:",omitempty"`
	Datestamp DateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename string `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// PersonDate is not documented.
type PersonDate struct {
	PersonDateRole PersonDateRole `xml:"b305"`
	DateFormat DateFormat `xml:"j260,omitempty" json:",omitempty"`
	Date string `xml:"b306"`
	Textformat TextFormatCode `xml:"textformat,omitempty,attr" json:",omitempty"`
	Textcase TextCaseCode `xml:"textcase,omitempty,attr" json:",omitempty"`
	Language LanguageList74 `xml:"language,omitempty,attr" json:",omitempty"`
	Transliteration TransliterationCode `xml:"transliteration,omitempty,attr" json:",omitempty"`
	Datestamp DateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename string `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// PersonNameIdentifier is not documented.
type PersonNameIdentifier struct {
	PersonNameIDType PersonNameIDType `xml:"b390"`
	IDTypeName string `xml:"b233,omitempty" json:",omitempty"`
	IDValue string `xml:"b244"`
	Textformat TextFormatCode `xml:"textformat,omitempty,attr" json:",omitempty"`
	Textcase TextCaseCode `xml:"textcase,omitempty,attr" json:",omitempty"`
	Language LanguageList74 `xml:"language,omitempty,attr" json:",omitempty"`
	Transliteration TransliterationCode `xml:"transliteration,omitempty,attr" json:",omitempty"`
	Datestamp DateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename string `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// Price is not documented.
type Price struct {
	PriceTypeCode PriceTypeCode `xml:"j148,omitempty" json:",omitempty"`
	PriceQualifier PriceQualifier `xml:"j261,omitempty" json:",omitempty"`
	PriceTypeDescription string `xml:"j262,omitempty" json:",omitempty"`
	PricePer PricePer `xml:"j239,omitempty" json:",omitempty"`
	MinimumOrderQuantity string `xml:"j263,omitempty" json:",omitempty"`
	BatchBonuss []BatchBonus `xml:"batchbonus,omitempty" json:",omitempty"`
	ClassOfTrade string `xml:"j149,omitempty" json:",omitempty"`
	BICDiscountGroupCode string `xml:"j150,omitempty" json:",omitempty"`
	DiscountCodeds []DiscountCoded `xml:"discountcoded,omitempty" json:",omitempty"`
	DiscountPercent string `xml:"j267,omitempty" json:",omitempty"`
	PriceStatus PriceStatus `xml:"j266,omitempty" json:",omitempty"`
	PriceAmount string `xml:"j151"`
	CurrencyCode CurrencyCode `xml:"j152,omitempty" json:",omitempty"`
	PriceEffectiveFrom string `xml:"j161,omitempty" json:",omitempty"`
	PriceEffectiveUntil string `xml:"j162,omitempty" json:",omitempty"`
	Territory TerritoryCodeList `xml:"j303,omitempty" json:",omitempty"`
	CountryCodes []CountryCode `xml:"b251,omitempty" json:",omitempty"`
	CountryExcluded CountryCodeList `xml:"j304,omitempty" json:",omitempty"`
	TerritoryExcluded TerritoryCodeList `xml:"j308,omitempty" json:",omitempty"`
	TaxRateCode1 TaxRateCode1 `xml:"j153,omitempty" json:",omitempty"`
	TaxRatePercent1 string `xml:"j154,omitempty" json:",omitempty"`
	TaxableAmount1 string `xml:"j155,omitempty" json:",omitempty"`
	TaxAmount1 string `xml:"j156,omitempty" json:",omitempty"`
	TaxRateCode2 TaxRateCode2 `xml:"j157,omitempty" json:",omitempty"`
	TaxRatePercent2 string `xml:"j158,omitempty" json:",omitempty"`
	TaxableAmount2 string `xml:"j159,omitempty" json:",omitempty"`
	TaxAmount2 string `xml:"j160,omitempty" json:",omitempty"`
	Textformat TextFormatCode `xml:"textformat,omitempty,attr" json:",omitempty"`
	Textcase TextCaseCode `xml:"textcase,omitempty,attr" json:",omitempty"`
	Language LanguageList74 `xml:"language,omitempty,attr" json:",omitempty"`
	Transliteration TransliterationCode `xml:"transliteration,omitempty,attr" json:",omitempty"`
	Datestamp DateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename string `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// Prize is not documented.
type Prize struct {
	PrizeName string `xml:"g126"`
	PrizeYear string `xml:"g127,omitempty" json:",omitempty"`
	PrizeCountry PrizeCountry `xml:"g128,omitempty" json:",omitempty"`
	PrizeCode PrizeCode `xml:"g129,omitempty" json:",omitempty"`
	PrizeJury PrizeJury `xml:"g343,omitempty" json:",omitempty"`
	Textformat TextFormatCode `xml:"textformat,omitempty,attr" json:",omitempty"`
	Textcase TextCaseCode `xml:"textcase,omitempty,attr" json:",omitempty"`
	Language LanguageList74 `xml:"language,omitempty,attr" json:",omitempty"`
	Transliteration TransliterationCode `xml:"transliteration,omitempty,attr" json:",omitempty"`
	Datestamp DateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename string `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// Product is not documented.
type Product struct {
	Dimensions string `xml:"c258,omitempty" json:",omitempty"`
	Weight string `xml:"c099,omitempty" json:",omitempty"`
	Measures []Measure `xml:"measure,omitempty" json:",omitempty"`
	Height string `xml:"c096,omitempty" json:",omitempty"`
	Width string `xml:"c097,omitempty" json:",omitempty"`
	Thickness string `xml:"c098,omitempty" json:",omitempty"`
	RecordReference string `xml:"a001"`
	NotificationType NotificationType `xml:"a002"`
	DeletionCode DeletionCode `xml:"a198,omitempty" json:",omitempty"`
	DeletionText string `xml:"a199,omitempty" json:",omitempty"`
	RecordSourceType RecordSourceType `xml:"a194,omitempty" json:",omitempty"`
	RecordSourceName string `xml:"a197,omitempty" json:",omitempty"`
	ReplacedByISBN string `xml:"h130,omitempty" json:",omitempty"`
	ReplacedByEAN13 string `xml:"h131,omitempty" json:",omitempty"`
	AlternativeFormatISBN string `xml:"h132,omitempty" json:",omitempty"`
	AlternativeFormatEAN13 string `xml:"h133,omitempty" json:",omitempty"`
	AlternativeProductISBN string `xml:"h163,omitempty" json:",omitempty"`
	AlternativeProductEAN13 string `xml:"h164,omitempty" json:",omitempty"`
	RelatedProducts []RelatedProduct `xml:"relatedproduct,omitempty" json:",omitempty"`
	OutOfPrintDate string `xml:"h134,omitempty" json:",omitempty"`
	SupplyDetails []SupplyDetail `xml:"supplydetail,omitempty" json:",omitempty"`
	MarketRepresentations []MarketRepresentation `xml:"marketrepresentation,omitempty" json:",omitempty"`
	PromotionCampaign string `xml:"k165,omitempty" json:",omitempty"`
	PromotionContact string `xml:"k166,omitempty" json:",omitempty"`
	InitialPrintRun string `xml:"k167,omitempty" json:",omitempty"`
	ReprintDetails []string `xml:"k309,omitempty" json:",omitempty"`
	CopiesSold string `xml:"k168,omitempty" json:",omitempty"`
	BookClubAdoption string `xml:"k169,omitempty" json:",omitempty"`
	RecordSourceIdentifierType RecordSourceIdentifierType `xml:"a195,omitempty" json:",omitempty"`
	RecordSourceIdentifier string `xml:"a196,omitempty" json:",omitempty"`
	ProductIdentifiers []ProductIdentifier `xml:"productidentifier,omitempty" json:",omitempty"`
	ISBN string `xml:"b004,omitempty" json:",omitempty"`
	EAN13 string `xml:"b005,omitempty" json:",omitempty"`
	UPC string `xml:"b006,omitempty" json:",omitempty"`
	PublisherProductNo string `xml:"b007,omitempty" json:",omitempty"`
	ISMN string `xml:"b008,omitempty" json:",omitempty"`
	DOI string `xml:"b009,omitempty" json:",omitempty"`
	Seriess []Series `xml:"series,omitempty" json:",omitempty"`
	NoSeries NoSeries `xml:"n338,omitempty" json:",omitempty"`
	Sets []Set `xml:"set,omitempty" json:",omitempty"`
	Titles []Title `xml:"title,omitempty" json:",omitempty"`
	DistinctiveTitle string `xml:"b028,omitempty" json:",omitempty"`
	TitlePrefix string `xml:"b030,omitempty" json:",omitempty"`
	TitleWithoutPrefix string `xml:"b031,omitempty" json:",omitempty"`
	Subtitle string `xml:"b029,omitempty" json:",omitempty"`
	TranslationOfTitle string `xml:"b032,omitempty" json:",omitempty"`
	FormerTitles []string `xml:"b033,omitempty" json:",omitempty"`
	NoContributor NoContributor `xml:"n339,omitempty" json:",omitempty"`
	Contributors []Contributor `xml:"contributor,omitempty" json:",omitempty"`
	ContributorStatement string `xml:"b049,omitempty" json:",omitempty"`
	ConferenceDescription string `xml:"b050,omitempty" json:",omitempty"`
	Conferences []Conference `xml:"conference,omitempty" json:",omitempty"`
	ConferenceRole ConferenceRole `xml:"b051,omitempty" json:",omitempty"`
	ConferenceName string `xml:"b052,omitempty" json:",omitempty"`
	ConferenceNumber string `xml:"b053,omitempty" json:",omitempty"`
	ConferenceDate string `xml:"b054,omitempty" json:",omitempty"`
	ConferencePlace string `xml:"b055,omitempty" json:",omitempty"`
	NoEdition NoEdition `xml:"n386,omitempty" json:",omitempty"`
	EditionTypeCodes []EditionTypeCode `xml:"b056,omitempty" json:",omitempty"`
	EditionNumber string `xml:"b057,omitempty" json:",omitempty"`
	EditionVersionNumber string `xml:"b217,omitempty" json:",omitempty"`
	EditionStatement string `xml:"b058,omitempty" json:",omitempty"`
	PrizesDescription string `xml:"g124,omitempty" json:",omitempty"`
	Prizes []Prize `xml:"prize,omitempty" json:",omitempty"`
	Publishers []Publisher `xml:"publisher,omitempty" json:",omitempty"`
	ImprintName string `xml:"b079,omitempty" json:",omitempty"`
	Imprints []Imprint `xml:"imprint,omitempty" json:",omitempty"`
	PublisherName string `xml:"b081,omitempty" json:",omitempty"`
	CopyrightStatements []CopyrightStatement `xml:"copyrightstatement,omitempty" json:",omitempty"`
	CopyrightYear string `xml:"b087,omitempty" json:",omitempty"`
	Barcodes []Barcode `xml:"b246,omitempty" json:",omitempty"`
	ReplacesISBN string `xml:"b010,omitempty" json:",omitempty"`
	ReplacesEAN13 string `xml:"b011,omitempty" json:",omitempty"`
	ProductForm ProductForm `xml:"b012,omitempty" json:",omitempty"`
	ProductFormDetails []ProductFormDetail `xml:"b333,omitempty" json:",omitempty"`
	ProductFormFeatures []ProductFormFeature `xml:"productformfeature,omitempty" json:",omitempty"`
	BookFormDetails []BookFormDetail `xml:"b013,omitempty" json:",omitempty"`
	ProductPackaging ProductPackaging `xml:"b225,omitempty" json:",omitempty"`
	ProductFormDescription string `xml:"b014,omitempty" json:",omitempty"`
	NumberOfPieces string `xml:"b210,omitempty" json:",omitempty"`
	TradeCategory TradeCategory `xml:"b384,omitempty" json:",omitempty"`
	ProductContentTypes []ProductContentType `xml:"b385,omitempty" json:",omitempty"`
	ContainedItems []ContainedItem `xml:"containeditem,omitempty" json:",omitempty"`
	ProductClassifications []ProductClassification `xml:"productclassification,omitempty" json:",omitempty"`
	TextCaseFlag TextCaseFlag `xml:"b027,omitempty" json:",omitempty"`
	WorkIdentifiers []WorkIdentifier `xml:"workidentifier,omitempty" json:",omitempty"`
	Websites []Website `xml:"website,omitempty" json:",omitempty"`
	ReligiousText ReligiousText `xml:"religioustext,omitempty" json:",omitempty"`
	LanguageOfTexts []LanguageOfText `xml:"b059,omitempty" json:",omitempty"`
	OriginalLanguage OriginalLanguage `xml:"b060,omitempty" json:",omitempty"`
	Languages []Language `xml:"language,omitempty" json:",omitempty"`
	NumberOfPages string `xml:"b061,omitempty" json:",omitempty"`
	PagesRoman string `xml:"b254,omitempty" json:",omitempty"`
	PagesArabic string `xml:"b255,omitempty" json:",omitempty"`
	Extents []Extent `xml:"extent,omitempty" json:",omitempty"`
	NumberOfIllustrations string `xml:"b125,omitempty" json:",omitempty"`
	IllustrationsNote string `xml:"b062,omitempty" json:",omitempty"`
	Illustrationss []Illustrations `xml:"illustrations,omitempty" json:",omitempty"`
	MapScales []string `xml:"b063,omitempty" json:",omitempty"`
	MainSubjects []MainSubject `xml:"mainsubject,omitempty" json:",omitempty"`
	Subjects []Subject `xml:"subject,omitempty" json:",omitempty"`
	PersonAsSubjects []PersonAsSubject `xml:"personassubject,omitempty" json:",omitempty"`
	CorporateBodyAsSubjects []string `xml:"b071,omitempty" json:",omitempty"`
	PlaceAsSubjects []string `xml:"b072,omitempty" json:",omitempty"`
	AudienceCodes []AudienceCode `xml:"b073,omitempty" json:",omitempty"`
	Audiences []Audience `xml:"audience,omitempty" json:",omitempty"`
	USSchoolGrade string `xml:"b189,omitempty" json:",omitempty"`
	InterestAge string `xml:"b190,omitempty" json:",omitempty"`
	AudienceRanges []AudienceRange `xml:"audiencerange,omitempty" json:",omitempty"`
	AudienceDescription string `xml:"b207,omitempty" json:",omitempty"`
	Complexitys []Complexity `xml:"complexity,omitempty" json:",omitempty"`
	Annotation Annotation `xml:"d100,omitempty" json:",omitempty"`
	MainDescription MainDescription `xml:"d101,omitempty" json:",omitempty"`
	OtherTexts []OtherText `xml:"othertext,omitempty" json:",omitempty"`
	ReviewQuotes []ReviewQuote `xml:"e110,omitempty" json:",omitempty"`
	MediaFiles []MediaFile `xml:"mediafile,omitempty" json:",omitempty"`
	ProductWebsites []ProductWebsite `xml:"productwebsite,omitempty" json:",omitempty"`
	ContentItems []ContentItem `xml:"contentitem,omitempty" json:",omitempty"`
	CityOfPublications []string `xml:"b209,omitempty" json:",omitempty"`
	CountryOfPublication CountryOfPublication `xml:"b083,omitempty" json:",omitempty"`
	CopublisherNames []string `xml:"b084,omitempty" json:",omitempty"`
	SponsorNames []string `xml:"b085,omitempty" json:",omitempty"`
	OriginalPublisher string `xml:"b240,omitempty" json:",omitempty"`
	AnnouncementDate string `xml:"b086,omitempty" json:",omitempty"`
	TradeAnnouncementDate string `xml:"b362,omitempty" json:",omitempty"`
	PublicationDate string `xml:"b003,omitempty" json:",omitempty"`
	YearFirstPublished string `xml:"b088,omitempty" json:",omitempty"`
	SalesRightss []SalesRights `xml:"salesrights,omitempty" json:",omitempty"`
	NotForSales []NotForSale `xml:"notforsale,omitempty" json:",omitempty"`
	SalesRestrictions []SalesRestriction `xml:"salesrestriction,omitempty" json:",omitempty"`
	EpubType EpubType `xml:"b211,omitempty" json:",omitempty"`
	EpubTypeVersion string `xml:"b212,omitempty" json:",omitempty"`
	EpubTypeDescription string `xml:"b213,omitempty" json:",omitempty"`
	EpubFormatDescription string `xml:"b216,omitempty" json:",omitempty"`
	EpubSourceDescription string `xml:"b280,omitempty" json:",omitempty"`
	EpubTypeNote string `xml:"b277,omitempty" json:",omitempty"`
	EpubFormat EpubFormat `xml:"b214,omitempty" json:",omitempty"`
	EpubFormatVersion string `xml:"b215,omitempty" json:",omitempty"`
	EpubSource EpubSource `xml:"b278,omitempty" json:",omitempty"`
	EpubSourceVersion string `xml:"b279,omitempty" json:",omitempty"`
	ThesisType ThesisType `xml:"b368,omitempty" json:",omitempty"`
	ThesisPresentedTo string `xml:"b369,omitempty" json:",omitempty"`
	ThesisYear string `xml:"b370,omitempty" json:",omitempty"`
	BASICMainSubject string `xml:"b064,omitempty" json:",omitempty"`
	BASICVersion string `xml:"b200,omitempty" json:",omitempty"`
	BICMainSubject string `xml:"b065,omitempty" json:",omitempty"`
	BICVersion string `xml:"b066,omitempty" json:",omitempty"`
	CoverImageFormatCode CoverImageFormatCode `xml:"f111,omitempty" json:",omitempty"`
	CoverImageLinkTypeCode CoverImageLinkTypeCode `xml:"f112,omitempty" json:",omitempty"`
	CoverImageLink string `xml:"f113,omitempty" json:",omitempty"`
	PublishingStatus PublishingStatus `xml:"b394,omitempty" json:",omitempty"`
	PublishingStatusNote string `xml:"b395,omitempty" json:",omitempty"`
	Textformat TextFormatCode `xml:"textformat,omitempty,attr" json:",omitempty"`
	Textcase TextCaseCode `xml:"textcase,omitempty,attr" json:",omitempty"`
	Language LanguageList74 `xml:"language,omitempty,attr" json:",omitempty"`
	Transliteration TransliterationCode `xml:"transliteration,omitempty,attr" json:",omitempty"`
	Datestamp DateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename string `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// ProductClassification is not documented.
type ProductClassification struct {
	ProductClassificationType ProductClassificationType `xml:"b274"`
	ProductClassificationCode string `xml:"b275"`
	Percent string `xml:"b337,omitempty" json:",omitempty"`
	Textformat TextFormatCode `xml:"textformat,omitempty,attr" json:",omitempty"`
	Textcase TextCaseCode `xml:"textcase,omitempty,attr" json:",omitempty"`
	Language LanguageList74 `xml:"language,omitempty,attr" json:",omitempty"`
	Transliteration TransliterationCode `xml:"transliteration,omitempty,attr" json:",omitempty"`
	Datestamp DateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename string `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// ProductFormFeature is not documented.
type ProductFormFeature struct {
	ProductFormFeatureType ProductFormFeatureType `xml:"b334"`
	ProductFormFeatureValue string `xml:"b335,omitempty" json:",omitempty"`
	ProductFormFeatureDescription string `xml:"b336,omitempty" json:",omitempty"`
	Textformat TextFormatCode `xml:"textformat,omitempty,attr" json:",omitempty"`
	Textcase TextCaseCode `xml:"textcase,omitempty,attr" json:",omitempty"`
	Language LanguageList74 `xml:"language,omitempty,attr" json:",omitempty"`
	Transliteration TransliterationCode `xml:"transliteration,omitempty,attr" json:",omitempty"`
	Datestamp DateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename string `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// ProductIdentifier is not documented.
type ProductIdentifier struct {
	ProductIDType ProductIDType `xml:"b221"`
	IDTypeName string `xml:"b233,omitempty" json:",omitempty"`
	IDValue string `xml:"b244"`
	Textformat TextFormatCode `xml:"textformat,omitempty,attr" json:",omitempty"`
	Textcase TextCaseCode `xml:"textcase,omitempty,attr" json:",omitempty"`
	Language LanguageList74 `xml:"language,omitempty,attr" json:",omitempty"`
	Transliteration TransliterationCode `xml:"transliteration,omitempty,attr" json:",omitempty"`
	Datestamp DateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename string `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// ProductWebsite is not documented.
type ProductWebsite struct {
	WebsiteRole WebsiteRole `xml:"b367,omitempty" json:",omitempty"`
	ProductWebsiteDescription ProductWebsiteDescription `xml:"f170,omitempty" json:",omitempty"`
	ProductWebsiteLink string `xml:"f123"`
	Textformat TextFormatCode `xml:"textformat,omitempty,attr" json:",omitempty"`
	Textcase TextCaseCode `xml:"textcase,omitempty,attr" json:",omitempty"`
	Language LanguageList74 `xml:"language,omitempty,attr" json:",omitempty"`
	Transliteration TransliterationCode `xml:"transliteration,omitempty,attr" json:",omitempty"`
	Datestamp DateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename string `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// ProfessionalAffiliation is not documented.
type ProfessionalAffiliation struct {
	Affiliation string `xml:"b046,omitempty" json:",omitempty"`
	ProfessionalPosition string `xml:"b045,omitempty" json:",omitempty"`
	Textformat TextFormatCode `xml:"textformat,omitempty,attr" json:",omitempty"`
	Textcase TextCaseCode `xml:"textcase,omitempty,attr" json:",omitempty"`
	Language LanguageList74 `xml:"language,omitempty,attr" json:",omitempty"`
	Transliteration TransliterationCode `xml:"transliteration,omitempty,attr" json:",omitempty"`
	Datestamp DateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename string `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// Publisher is not documented.
type Publisher struct {
	PublisherName string `xml:"b081,omitempty" json:",omitempty"`
	NameCodeType NameCodeType `xml:"b241,omitempty" json:",omitempty"`
	NameCodeTypeName string `xml:"b242,omitempty" json:",omitempty"`
	NameCodeValue string `xml:"b243,omitempty" json:",omitempty"`
	PublishingRole PublishingRole `xml:"b291,omitempty" json:",omitempty"`
	Websites []Website `xml:"website,omitempty" json:",omitempty"`
	Textformat TextFormatCode `xml:"textformat,omitempty,attr" json:",omitempty"`
	Textcase TextCaseCode `xml:"textcase,omitempty,attr" json:",omitempty"`
	Language LanguageList74 `xml:"language,omitempty,attr" json:",omitempty"`
	Transliteration TransliterationCode `xml:"transliteration,omitempty,attr" json:",omitempty"`
	Datestamp DateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename string `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// Reissue is not documented.
type Reissue struct {
	ReissueDate string `xml:"j365"`
	ReissueDescription string `xml:"j366,omitempty" json:",omitempty"`
	Prices []Price `xml:"price,omitempty" json:",omitempty"`
	MediaFiles []MediaFile `xml:"mediafile,omitempty" json:",omitempty"`
	Textformat TextFormatCode `xml:"textformat,omitempty,attr" json:",omitempty"`
	Textcase TextCaseCode `xml:"textcase,omitempty,attr" json:",omitempty"`
	Language LanguageList74 `xml:"language,omitempty,attr" json:",omitempty"`
	Transliteration TransliterationCode `xml:"transliteration,omitempty,attr" json:",omitempty"`
	Datestamp DateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename string `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// RelatedProduct is not documented.
type RelatedProduct struct {
	ISBN string `xml:"b004,omitempty" json:",omitempty"`
	EAN13 string `xml:"b005,omitempty" json:",omitempty"`
	ProductIdentifiers []ProductIdentifier `xml:"productidentifier,omitempty" json:",omitempty"`
	Websites []Website `xml:"website,omitempty" json:",omitempty"`
	ProductForm ProductForm `xml:"b012,omitempty" json:",omitempty"`
	ProductFormDetails []ProductFormDetail `xml:"b333,omitempty" json:",omitempty"`
	ProductFormFeatures []ProductFormFeature `xml:"productformfeature,omitempty" json:",omitempty"`
	BookFormDetails []BookFormDetail `xml:"b013,omitempty" json:",omitempty"`
	ProductPackaging ProductPackaging `xml:"b225,omitempty" json:",omitempty"`
	ProductFormDescription string `xml:"b014,omitempty" json:",omitempty"`
	RelationCode RelationCode `xml:"h208"`
	NumberOfPieces string `xml:"b210,omitempty" json:",omitempty"`
	TradeCategory TradeCategory `xml:"b384,omitempty" json:",omitempty"`
	ProductContentTypes []ProductContentType `xml:"b385,omitempty" json:",omitempty"`
	Publishers []Publisher `xml:"publisher,omitempty" json:",omitempty"`
	EpubType EpubType `xml:"b211,omitempty" json:",omitempty"`
	EpubTypeVersion string `xml:"b212,omitempty" json:",omitempty"`
	EpubTypeDescription string `xml:"b213,omitempty" json:",omitempty"`
	EpubFormatDescription string `xml:"b216,omitempty" json:",omitempty"`
	EpubTypeNote string `xml:"b277,omitempty" json:",omitempty"`
	EpubFormat EpubFormat `xml:"b214,omitempty" json:",omitempty"`
	EpubFormatVersion string `xml:"b215,omitempty" json:",omitempty"`
	Textformat TextFormatCode `xml:"textformat,omitempty,attr" json:",omitempty"`
	Textcase TextCaseCode `xml:"textcase,omitempty,attr" json:",omitempty"`
	Language LanguageList74 `xml:"language,omitempty,attr" json:",omitempty"`
	Transliteration TransliterationCode `xml:"transliteration,omitempty,attr" json:",omitempty"`
	Datestamp DateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename string `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// ReligiousText is not documented.
type ReligiousText struct {
	Bible Bible `xml:"bible,omitempty" json:",omitempty"`
	ReligiousTextID ReligiousTextID `xml:"b376,omitempty" json:",omitempty"`
	ReligiousTextFeatures []ReligiousTextFeature `xml:"religioustextfeature,omitempty" json:",omitempty"`
	Textformat TextFormatCode `xml:"textformat,omitempty,attr" json:",omitempty"`
	Textcase TextCaseCode `xml:"textcase,omitempty,attr" json:",omitempty"`
	Language LanguageList74 `xml:"language,omitempty,attr" json:",omitempty"`
	Transliteration TransliterationCode `xml:"transliteration,omitempty,attr" json:",omitempty"`
	Datestamp DateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename string `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// ReligiousTextFeature is not documented.
type ReligiousTextFeature struct {
	ReligiousTextFeatureType ReligiousTextFeatureType `xml:"b358"`
	ReligiousTextFeatureCode ReligiousTextFeatureCode `xml:"b359"`
	ReligiousTextFeatureDescription string `xml:"b360,omitempty" json:",omitempty"`
	Textformat TextFormatCode `xml:"textformat,omitempty,attr" json:",omitempty"`
	Textcase TextCaseCode `xml:"textcase,omitempty,attr" json:",omitempty"`
	Language LanguageList74 `xml:"language,omitempty,attr" json:",omitempty"`
	Transliteration TransliterationCode `xml:"transliteration,omitempty,attr" json:",omitempty"`
	Datestamp DateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename string `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// SalesOutlet is not documented.
type SalesOutlet struct {
	SalesOutletName string `xml:"b382,omitempty" json:",omitempty"`
	SalesOutletIdentifier SalesOutletIdentifier `xml:"salesoutletidentifier,omitempty" json:",omitempty"`
	Textformat TextFormatCode `xml:"textformat,omitempty,attr" json:",omitempty"`
	Textcase TextCaseCode `xml:"textcase,omitempty,attr" json:",omitempty"`
	Language LanguageList74 `xml:"language,omitempty,attr" json:",omitempty"`
	Transliteration TransliterationCode `xml:"transliteration,omitempty,attr" json:",omitempty"`
	Datestamp DateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename string `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// SalesOutletIdentifier is not documented.
type SalesOutletIdentifier struct {
	SalesOutletIDType SalesOutletIDType `xml:"b393"`
	IDTypeName string `xml:"b233,omitempty" json:",omitempty"`
	IDValue string `xml:"b244"`
	Textformat TextFormatCode `xml:"textformat,omitempty,attr" json:",omitempty"`
	Textcase TextCaseCode `xml:"textcase,omitempty,attr" json:",omitempty"`
	Language LanguageList74 `xml:"language,omitempty,attr" json:",omitempty"`
	Transliteration TransliterationCode `xml:"transliteration,omitempty,attr" json:",omitempty"`
	Datestamp DateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename string `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// SalesRestriction is not documented.
type SalesRestriction struct {
	SalesRestrictionType SalesRestrictionType `xml:"b381"`
	SalesOutlets []SalesOutlet `xml:"salesoutlet,omitempty" json:",omitempty"`
	SalesRestrictionDetail string `xml:"b383,omitempty" json:",omitempty"`
	Textformat TextFormatCode `xml:"textformat,omitempty,attr" json:",omitempty"`
	Textcase TextCaseCode `xml:"textcase,omitempty,attr" json:",omitempty"`
	Language LanguageList74 `xml:"language,omitempty,attr" json:",omitempty"`
	Transliteration TransliterationCode `xml:"transliteration,omitempty,attr" json:",omitempty"`
	Datestamp DateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename string `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// SalesRights is not documented.
type SalesRights struct {
	RightsTerritory TerritoryCodeList `xml:"b388,omitempty" json:",omitempty"`
	RightsRegions []RightsRegion `xml:"b091,omitempty" json:",omitempty"`
	RightsCountrys []CountryCodeList `xml:"b090,omitempty" json:",omitempty"`
	SalesRightsType SalesRightsType `xml:"b089"`
	Textformat TextFormatCode `xml:"textformat,omitempty,attr" json:",omitempty"`
	Textcase TextCaseCode `xml:"textcase,omitempty,attr" json:",omitempty"`
	Language LanguageList74 `xml:"language,omitempty,attr" json:",omitempty"`
	Transliteration TransliterationCode `xml:"transliteration,omitempty,attr" json:",omitempty"`
	Datestamp DateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename string `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// SenderIdentifier is not documented.
type SenderIdentifier struct {
	SenderIDType SenderIDType `xml:"m379"`
	IDTypeName string `xml:"b233,omitempty" json:",omitempty"`
	IDValue string `xml:"b244"`
	Textformat TextFormatCode `xml:"textformat,omitempty,attr" json:",omitempty"`
	Textcase TextCaseCode `xml:"textcase,omitempty,attr" json:",omitempty"`
	Language LanguageList74 `xml:"language,omitempty,attr" json:",omitempty"`
	Transliteration TransliterationCode `xml:"transliteration,omitempty,attr" json:",omitempty"`
	Datestamp DateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename string `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// Series is not documented.
type Series struct {
	Titles []Title `xml:"title,omitempty" json:",omitempty"`
	TitleOfSeries string `xml:"b018,omitempty" json:",omitempty"`
	SeriesISSN string `xml:"b016,omitempty" json:",omitempty"`
	PublisherSeriesCode string `xml:"b017,omitempty" json:",omitempty"`
	SeriesIdentifiers []SeriesIdentifier `xml:"seriesidentifier,omitempty" json:",omitempty"`
	Contributors []Contributor `xml:"contributor,omitempty" json:",omitempty"`
	NumberWithinSeries string `xml:"b019,omitempty" json:",omitempty"`
	YearOfAnnual string `xml:"b020,omitempty" json:",omitempty"`
	Textformat TextFormatCode `xml:"textformat,omitempty,attr" json:",omitempty"`
	Textcase TextCaseCode `xml:"textcase,omitempty,attr" json:",omitempty"`
	Language LanguageList74 `xml:"language,omitempty,attr" json:",omitempty"`
	Transliteration TransliterationCode `xml:"transliteration,omitempty,attr" json:",omitempty"`
	Datestamp DateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename string `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// SeriesIdentifier is not documented.
type SeriesIdentifier struct {
	SeriesIDType SeriesIDType `xml:"b273"`
	IDTypeName string `xml:"b233,omitempty" json:",omitempty"`
	IDValue string `xml:"b244"`
	Textformat TextFormatCode `xml:"textformat,omitempty,attr" json:",omitempty"`
	Textcase TextCaseCode `xml:"textcase,omitempty,attr" json:",omitempty"`
	Language LanguageList74 `xml:"language,omitempty,attr" json:",omitempty"`
	Transliteration TransliterationCode `xml:"transliteration,omitempty,attr" json:",omitempty"`
	Datestamp DateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename string `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// Set is not documented.
type Set struct {
	Titles []Title `xml:"title,omitempty" json:",omitempty"`
	TitleOfSet string `xml:"b023,omitempty" json:",omitempty"`
	ISBNOfSet string `xml:"b021,omitempty" json:",omitempty"`
	EAN13OfSet string `xml:"b022,omitempty" json:",omitempty"`
	ProductIdentifiers []ProductIdentifier `xml:"productidentifier,omitempty" json:",omitempty"`
	SetPartNumber string `xml:"b024,omitempty" json:",omitempty"`
	SetPartTitle string `xml:"b025,omitempty" json:",omitempty"`
	ItemNumberWithinSet string `xml:"b026,omitempty" json:",omitempty"`
	LevelSequenceNumber string `xml:"b284,omitempty" json:",omitempty"`
	SetItemTitle string `xml:"b281,omitempty" json:",omitempty"`
	Textformat TextFormatCode `xml:"textformat,omitempty,attr" json:",omitempty"`
	Textcase TextCaseCode `xml:"textcase,omitempty,attr" json:",omitempty"`
	Language LanguageList74 `xml:"language,omitempty,attr" json:",omitempty"`
	Transliteration TransliterationCode `xml:"transliteration,omitempty,attr" json:",omitempty"`
	Datestamp DateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename string `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// Stock is not documented.
type Stock struct {
	OnHand string `xml:"j350,omitempty" json:",omitempty"`
	StockQuantityCoded StockQuantityCoded `xml:"stockquantitycoded,omitempty" json:",omitempty"`
	LocationIdentifier LocationIdentifier `xml:"locationidentifier,omitempty" json:",omitempty"`
	LocationName string `xml:"j349,omitempty" json:",omitempty"`
	OnOrder string `xml:"j351,omitempty" json:",omitempty"`
	CBO string `xml:"j375,omitempty" json:",omitempty"`
	OnOrderDetails []OnOrderDetail `xml:"onorderdetail,omitempty" json:",omitempty"`
	Textformat TextFormatCode `xml:"textformat,omitempty,attr" json:",omitempty"`
	Textcase TextCaseCode `xml:"textcase,omitempty,attr" json:",omitempty"`
	Language LanguageList74 `xml:"language,omitempty,attr" json:",omitempty"`
	Transliteration TransliterationCode `xml:"transliteration,omitempty,attr" json:",omitempty"`
	Datestamp DateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename string `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// StockQuantityCoded is not documented.
type StockQuantityCoded struct {
	StockQuantityCodeType StockQuantityCodeType `xml:"j293"`
	StockQuantityCodeTypeName string `xml:"j296,omitempty" json:",omitempty"`
	StockQuantityCode string `xml:"j297"`
	Textformat TextFormatCode `xml:"textformat,omitempty,attr" json:",omitempty"`
	Textcase TextCaseCode `xml:"textcase,omitempty,attr" json:",omitempty"`
	Language LanguageList74 `xml:"language,omitempty,attr" json:",omitempty"`
	Transliteration TransliterationCode `xml:"transliteration,omitempty,attr" json:",omitempty"`
	Datestamp DateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename string `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// SubSeriesRecord is not documented.
type SubSeriesRecord struct {
	RecordReference string `xml:"a001"`
	NotificationType NotificationType `xml:"a002"`
	DeletionCode DeletionCode `xml:"a198,omitempty" json:",omitempty"`
	DeletionText string `xml:"a199,omitempty" json:",omitempty"`
	RecordSourceType RecordSourceType `xml:"a194,omitempty" json:",omitempty"`
	RecordSourceName string `xml:"a197,omitempty" json:",omitempty"`
	SeriesIdentifiers []SeriesIdentifier `xml:"seriesidentifier"`
	ParentIdentifier ParentIdentifier `xml:"parentidentifier"`
	LevelSequenceNumber string `xml:"b284"`
	Titles []Title `xml:"title"`
	Contributors []Contributor `xml:"contributor,omitempty" json:",omitempty"`
	OtherTexts []OtherText `xml:"othertext,omitempty" json:",omitempty"`
	Publishers []Publisher `xml:"publisher,omitempty" json:",omitempty"`
	SubordinateEntries string `xml:"a245,omitempty" json:",omitempty"`
	RecordSourceIdentifierType RecordSourceIdentifierType `xml:"a195,omitempty" json:",omitempty"`
	RecordSourceIdentifier string `xml:"a196,omitempty" json:",omitempty"`
	SeriesPartName string `xml:"b282,omitempty" json:",omitempty"`
	NumberWithinSeries string `xml:"b019,omitempty" json:",omitempty"`
	Textformat TextFormatCode `xml:"textformat,omitempty,attr" json:",omitempty"`
	Textcase TextCaseCode `xml:"textcase,omitempty,attr" json:",omitempty"`
	Language LanguageList74 `xml:"language,omitempty,attr" json:",omitempty"`
	Transliteration TransliterationCode `xml:"transliteration,omitempty,attr" json:",omitempty"`
	Datestamp DateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename string `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// Subject is not documented.
type Subject struct {
	SubjectHeadingText string `xml:"b070,omitempty" json:",omitempty"`
	SubjectCode string `xml:"b069,omitempty" json:",omitempty"`
	SubjectSchemeIdentifier SubjectSchemeIdentifier `xml:"b067"`
	SubjectSchemeName string `xml:"b171,omitempty" json:",omitempty"`
	SubjectSchemeVersion string `xml:"b068,omitempty" json:",omitempty"`
	Textformat TextFormatCode `xml:"textformat,omitempty,attr" json:",omitempty"`
	Textcase TextCaseCode `xml:"textcase,omitempty,attr" json:",omitempty"`
	Language LanguageList74 `xml:"language,omitempty,attr" json:",omitempty"`
	Transliteration TransliterationCode `xml:"transliteration,omitempty,attr" json:",omitempty"`
	Datestamp DateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename string `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// SupplierIdentifier is not documented.
type SupplierIdentifier struct {
	SupplierIDType SupplierIDType `xml:"j345"`
	IDTypeName string `xml:"b233,omitempty" json:",omitempty"`
	IDValue string `xml:"b244"`
	Textformat TextFormatCode `xml:"textformat,omitempty,attr" json:",omitempty"`
	Textcase TextCaseCode `xml:"textcase,omitempty,attr" json:",omitempty"`
	Language LanguageList74 `xml:"language,omitempty,attr" json:",omitempty"`
	Transliteration TransliterationCode `xml:"transliteration,omitempty,attr" json:",omitempty"`
	Datestamp DateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename string `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// SupplyDetail is not documented.
type SupplyDetail struct {
	SupplierName string `xml:"j137,omitempty" json:",omitempty"`
	SupplierIdentifiers []SupplierIdentifier `xml:"supplieridentifier,omitempty" json:",omitempty"`
	SupplierSAN string `xml:"j136,omitempty" json:",omitempty"`
	SupplierEANLocationNumber string `xml:"j135,omitempty" json:",omitempty"`
	IntermediaryAvailabilityCode IntermediaryAvailabilityCode `xml:"j348,omitempty" json:",omitempty"`
	AvailabilityCode AvailabilityCode `xml:"j141,omitempty" json:",omitempty"`
	ProductAvailability ProductAvailability `xml:"j396,omitempty" json:",omitempty"`
	PriceAmount string `xml:"j151,omitempty" json:",omitempty"`
	UnpricedItemType UnpricedItemType `xml:"j192,omitempty" json:",omitempty"`
	Prices []Price `xml:"price,omitempty" json:",omitempty"`
	TelephoneNumbers []string `xml:"j270,omitempty" json:",omitempty"`
	FaxNumbers []string `xml:"j271,omitempty" json:",omitempty"`
	EmailAddresss []string `xml:"j272,omitempty" json:",omitempty"`
	Websites []Website `xml:"website,omitempty" json:",omitempty"`
	SupplierRole SupplierRole `xml:"j292,omitempty" json:",omitempty"`
	SupplyRestrictionDetail string `xml:"j399,omitempty" json:",omitempty"`
	LastDateForReturns string `xml:"j387,omitempty" json:",omitempty"`
	NewSupplier NewSupplier `xml:"newsupplier,omitempty" json:",omitempty"`
	OnSaleDate string `xml:"j143,omitempty" json:",omitempty"`
	OrderTime string `xml:"j144,omitempty" json:",omitempty"`
	Stocks []Stock `xml:"stock,omitempty" json:",omitempty"`
	PackQuantity string `xml:"j145,omitempty" json:",omitempty"`
	Reissue Reissue `xml:"reissue,omitempty" json:",omitempty"`
	SupplyToTerritory TerritoryCodeList `xml:"j397,omitempty" json:",omitempty"`
	SupplyToRegions []SupplyToRegion `xml:"j139,omitempty" json:",omitempty"`
	SupplyToCountrys []CountryCodeList `xml:"j138,omitempty" json:",omitempty"`
	SupplyToCountryExcludeds []CountryCodeList `xml:"j140,omitempty" json:",omitempty"`
	ReturnsCodeType ReturnsCodeType `xml:"j268,omitempty" json:",omitempty"`
	ReturnsCode string `xml:"j269,omitempty" json:",omitempty"`
	DateFormat DateFormat `xml:"j260,omitempty" json:",omitempty"`
	ExpectedShipDate string `xml:"j142,omitempty" json:",omitempty"`
	AudienceRestrictionFlag AudienceRestrictionFlag `xml:"j146,omitempty" json:",omitempty"`
	AudienceRestrictionNote string `xml:"j147,omitempty" json:",omitempty"`
	Textformat TextFormatCode `xml:"textformat,omitempty,attr" json:",omitempty"`
	Textcase TextCaseCode `xml:"textcase,omitempty,attr" json:",omitempty"`
	Language LanguageList74 `xml:"language,omitempty,attr" json:",omitempty"`
	Transliteration TransliterationCode `xml:"transliteration,omitempty,attr" json:",omitempty"`
	Datestamp DateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename string `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// TextItem is not documented.
type TextItem struct {
	PageRuns []PageRun `xml:"pagerun,omitempty" json:",omitempty"`
	FirstPageNumber string `xml:"b286,omitempty" json:",omitempty"`
	LastPageNumber string `xml:"b287,omitempty" json:",omitempty"`
	TextItemType TextItemType `xml:"b290"`
	TextItemIdentifiers []TextItemIdentifier `xml:"textitemidentifier,omitempty" json:",omitempty"`
	NumberOfPages string `xml:"b061,omitempty" json:",omitempty"`
	Textformat TextFormatCode `xml:"textformat,omitempty,attr" json:",omitempty"`
	Textcase TextCaseCode `xml:"textcase,omitempty,attr" json:",omitempty"`
	Language LanguageList74 `xml:"language,omitempty,attr" json:",omitempty"`
	Transliteration TransliterationCode `xml:"transliteration,omitempty,attr" json:",omitempty"`
	Datestamp DateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename string `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// TextItemIdentifier is not documented.
type TextItemIdentifier struct {
	TextItemIDType TextItemIDType `xml:"b285"`
	IDTypeName string `xml:"b233,omitempty" json:",omitempty"`
	IDValue string `xml:"b244"`
	Textformat TextFormatCode `xml:"textformat,omitempty,attr" json:",omitempty"`
	Textcase TextCaseCode `xml:"textcase,omitempty,attr" json:",omitempty"`
	Language LanguageList74 `xml:"language,omitempty,attr" json:",omitempty"`
	Transliteration TransliterationCode `xml:"transliteration,omitempty,attr" json:",omitempty"`
	Datestamp DateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename string `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// Title is not documented.
type Title struct {
	TitleText string `xml:"b203,omitempty" json:",omitempty"`
	TitlePrefix string `xml:"b030,omitempty" json:",omitempty"`
	TitleWithoutPrefix string `xml:"b031,omitempty" json:",omitempty"`
	TitleType TitleType `xml:"b202"`
	AbbreviatedLength string `xml:"b276,omitempty" json:",omitempty"`
	TextCaseFlag TextCaseFlag `xml:"b027,omitempty" json:",omitempty"`
	Subtitle string `xml:"b029,omitempty" json:",omitempty"`
	Textformat TextFormatCode `xml:"textformat,omitempty,attr" json:",omitempty"`
	Textcase TextCaseCode `xml:"textcase,omitempty,attr" json:",omitempty"`
	Language LanguageList74 `xml:"language,omitempty,attr" json:",omitempty"`
	Transliteration TransliterationCode `xml:"transliteration,omitempty,attr" json:",omitempty"`
	Datestamp DateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename string `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// Website is not documented.
type Website struct {
	WebsiteRole WebsiteRole `xml:"b367,omitempty" json:",omitempty"`
	WebsiteDescription WebsiteDescription `xml:"b294,omitempty" json:",omitempty"`
	WebsiteLink string `xml:"b295"`
	Textformat TextFormatCode `xml:"textformat,omitempty,attr" json:",omitempty"`
	Textcase TextCaseCode `xml:"textcase,omitempty,attr" json:",omitempty"`
	Language LanguageList74 `xml:"language,omitempty,attr" json:",omitempty"`
	Transliteration TransliterationCode `xml:"transliteration,omitempty,attr" json:",omitempty"`
	Datestamp DateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename string `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// WorkIdentifier is not documented.
type WorkIdentifier struct {
	WorkIDType WorkIDType `xml:"b201"`
	IDTypeName string `xml:"b233,omitempty" json:",omitempty"`
	IDValue string `xml:"b244"`
	Textformat TextFormatCode `xml:"textformat,omitempty,attr" json:",omitempty"`
	Textcase TextCaseCode `xml:"textcase,omitempty,attr" json:",omitempty"`
	Language LanguageList74 `xml:"language,omitempty,attr" json:",omitempty"`
	Transliteration TransliterationCode `xml:"transliteration,omitempty,attr" json:",omitempty"`
	Datestamp DateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename string `xml:"sourcename,omitempty,attr" json:",omitempty"`
}
