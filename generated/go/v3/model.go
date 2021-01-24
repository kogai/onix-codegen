package onix


// AVItem is not documented.
type AVItem struct {
	AVItemType AVItemType `xml:"x540"`
	AVItemIdentifiers []AVItemIdentifier `xml:"avitemidentifier,omitempty" json:",omitempty"`
	TimeRuns []TimeRun `xml:"timerun,omitempty" json:",omitempty"`
	AVDuration *DtDotTimeOrDuration `xml:"x544,omitempty" json:",omitempty"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// AVItemIdentifier is not documented.
type AVItemIdentifier struct {
	AVItemIDType AVItemIDType `xml:"x541"`
	IDTypeName *DtDotNonEmptyString `xml:"b233,omitempty" json:",omitempty"`
	IDValue DtDotNonEmptyString `xml:"b244"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// Addressee is not documented.
type Addressee struct {
	AddresseeName *DtDotNonEmptyString `xml:"x300,omitempty" json:",omitempty"`
	AddresseeIdentifiers []AddresseeIdentifier `xml:"addresseeidentifier,omitempty" json:",omitempty"`
	ContactName *DtDotNonEmptyString `xml:"x299,omitempty" json:",omitempty"`
	EmailAddress *DtDotEmailString `xml:"j272,omitempty" json:",omitempty"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// AddresseeIdentifier is not documented.
type AddresseeIdentifier struct {
	AddresseeIDType AddresseeIDType `xml:"m380"`
	IDTypeName *DtDotNonEmptyString `xml:"b233,omitempty" json:",omitempty"`
	IDValue DtDotNonEmptyString `xml:"b244"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// AgentIdentifier is not documented.
type AgentIdentifier struct {
	AgentIDType AgentIDType `xml:"j400"`
	IDTypeName *DtDotNonEmptyString `xml:"b233,omitempty" json:",omitempty"`
	IDValue DtDotNonEmptyString `xml:"b244"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// AlternativeName is not documented.
type AlternativeName struct {
	Gender *Gender `xml:"x524,omitempty" json:",omitempty"`
	NameIdentifiers []NameIdentifier `xml:"nameidentifier,omitempty" json:",omitempty"`
	NameType NameType `xml:"x414"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// AncillaryContent is not documented.
type AncillaryContent struct {
	AncillaryContentType AncillaryContentType `xml:"x423"`
	AncillaryContentDescriptions []Flow `xml:"x424,omitempty" json:",omitempty"`
	Number *DtDotPositiveInteger `xml:"b257,omitempty" json:",omitempty"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// Audience is not documented.
type Audience struct {
	AudienceCodeType AudienceCodeType `xml:"b204"`
	AudienceCodeTypeName *DtDotNonEmptyString `xml:"b205,omitempty" json:",omitempty"`
	AudienceCodeValue DtDotNonEmptyString `xml:"b206"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// AudienceRange is not documented.
type AudienceRange struct {
	AudienceRangeQualifier AudienceRangeQualifier `xml:"b074"`
	AudienceRangePrecision *AudienceRangePrecision `xml:"b075,omitempty" json:",omitempty"`
	AudienceRangeValue *DtDotNonEmptyString `xml:"b076,omitempty" json:",omitempty"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// Barcode is not documented.
type Barcode struct {
	BarcodeType BarcodeType `xml:"x312"`
	PositionOnProduct *PositionOnProduct `xml:"x313,omitempty" json:",omitempty"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// BatchBonus is not documented.
type BatchBonus struct {
	BatchQuantity DtDotStrictPositiveInteger `xml:"j264"`
	FreeQuantity DtDotStrictPositiveInteger `xml:"j265"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// Bible is not documented.
type Bible struct {
	BibleContentss []BibleContents `xml:"b352"`
	BibleVersions []BibleVersion `xml:"b353"`
	StudyBibleType *StudyBibleType `xml:"b389,omitempty" json:",omitempty"`
	BiblePurposes []BiblePurpose `xml:"b354,omitempty" json:",omitempty"`
	BibleTextOrganization *BibleTextOrganization `xml:"b355,omitempty" json:",omitempty"`
	BibleReferenceLocation *BibleReferenceLocation `xml:"b356,omitempty" json:",omitempty"`
	BibleTextFeatures []BibleTextFeature `xml:"b357,omitempty" json:",omitempty"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// CitedContent is not documented.
type CitedContent struct {
	ReviewRating *ReviewRating `xml:"reviewrating,omitempty" json:",omitempty"`
	SourceTitles []DtDotNonEmptyString `xml:"x428,omitempty" json:",omitempty"`
	ListNames []DtDotNonEmptyString `xml:"x432,omitempty" json:",omitempty"`
	PositionOnList *DtDotStrictPositiveInteger `xml:"x433,omitempty" json:",omitempty"`
	CitedContentType CitedContentType `xml:"x430"`
	ContentAudiences []ContentAudience `xml:"x427,omitempty" json:",omitempty"`
	Territory *Territory `xml:"territory,omitempty" json:",omitempty"`
	SourceType *SourceType `xml:"x431,omitempty" json:",omitempty"`
	CitationNotes []Flow `xml:"x434,omitempty" json:",omitempty"`
	ResourceLinks []DtDotNonEmptyURI `xml:"x435,omitempty" json:",omitempty"`
	ContentDates []ContentDate `xml:"contentdate,omitempty" json:",omitempty"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// CollateralDetail is not documented.
type CollateralDetail struct {
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// Collection is not documented.
type Collection struct {
	CollectionType CollectionType `xml:"x329"`
	SourceName *DtDotNonEmptyString `xml:"x330,omitempty" json:",omitempty"`
	CollectionIdentifiers []CollectionIdentifier `xml:"collectionidentifier,omitempty" json:",omitempty"`
	CollectionSequences []CollectionSequence `xml:"collectionsequence,omitempty" json:",omitempty"`
	TitleDetails []TitleDetail `xml:"titledetail,omitempty" json:",omitempty"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// CollectionIdentifier is not documented.
type CollectionIdentifier struct {
	CollectionIDType CollectionIDType `xml:"x344"`
	IDTypeName *DtDotNonEmptyString `xml:"b233,omitempty" json:",omitempty"`
	IDValue DtDotNonEmptyString `xml:"b244"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// CollectionSequence is not documented.
type CollectionSequence struct {
	CollectionSequenceType CollectionSequenceType `xml:"x479"`
	CollectionSequenceTypeName *DtDotNonEmptyString `xml:"x480,omitempty" json:",omitempty"`
	CollectionSequenceNumber DtDotMultiLevelNumberOrHyphen `xml:"x481"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// ComparisonProductPrice is not documented.
type ComparisonProductPrice struct {
	ProductIdentifiers []ProductIdentifier `xml:"productidentifier"`
	PriceType *PriceType `xml:"x462,omitempty" json:",omitempty"`
	PriceAmount DtDotStrictPositiveDecimal `xml:"j151"`
	CurrencyCode *CurrencyCode `xml:"j152,omitempty" json:",omitempty"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// Complexity is not documented.
type Complexity struct {
	ComplexitySchemeIdentifier ComplexitySchemeIdentifier `xml:"b077"`
	ComplexityCode DtDotNonEmptyString `xml:"b078"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// Conference is not documented.
type Conference struct {
	ConferenceRole *ConferenceRole `xml:"b051,omitempty" json:",omitempty"`
	ConferenceName DtDotNonEmptyString `xml:"b052"`
	ConferenceAcronym *DtDotNonEmptyString `xml:"b341,omitempty" json:",omitempty"`
	ConferenceNumber *DtDotStrictPositiveInteger `xml:"b053,omitempty" json:",omitempty"`
	ConferenceTheme *Flow `xml:"b342,omitempty" json:",omitempty"`
	ConferenceDate *DtDotNonEmptyString `xml:"b054,omitempty" json:",omitempty"`
	ConferencePlace *DtDotNonEmptyString `xml:"b055,omitempty" json:",omitempty"`
	ConferenceSponsors []ConferenceSponsor `xml:"conferencesponsor,omitempty" json:",omitempty"`
	Websites []Website `xml:"website,omitempty" json:",omitempty"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// ConferenceSponsor is not documented.
type ConferenceSponsor struct {
	PersonName *DtDotNonEmptyString `xml:"b036,omitempty" json:",omitempty"`
	CorporateName *DtDotNonEmptyString `xml:"b047,omitempty" json:",omitempty"`
	ConferenceSponsorIdentifiers []ConferenceSponsorIdentifier `xml:"conferencesponsoridentifier,omitempty" json:",omitempty"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// ConferenceSponsorIdentifier is not documented.
type ConferenceSponsorIdentifier struct {
	ConferenceSponsorIDType ConferenceSponsorIDType `xml:"b391"`
	IDTypeName *DtDotNonEmptyString `xml:"b233,omitempty" json:",omitempty"`
	IDValue DtDotNonEmptyString `xml:"b244"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// ContentDate is not documented.
type ContentDate struct {
	ContentDateRole ContentDateRole `xml:"x429"`
	DateFormat *DateFormat `xml:"j260,omitempty" json:",omitempty"`
	Date DtDotNonEmptyString `xml:"b306"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// ContentDetail is not documented.
type ContentDetail struct {
	ContentItems []ContentItem `xml:"contentitem,omitempty" json:",omitempty"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// ContentItem is not documented.
type ContentItem struct {
	TextItem *TextItem `xml:"textitem,omitempty" json:",omitempty"`
	AVItem *AVItem `xml:"avitem,omitempty" json:",omitempty"`
	ComponentTypeName *DtDotNonEmptyString `xml:"b288,omitempty" json:",omitempty"`
	ComponentNumber *DtDotNonEmptyString `xml:"b289,omitempty" json:",omitempty"`
	TitleDetails []TitleDetail `xml:"titledetail,omitempty" json:",omitempty"`
	LevelSequenceNumber *DtDotMultiLevelNumber `xml:"b284,omitempty" json:",omitempty"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// Contributor is not documented.
type Contributor struct {
	UnnamedPersons *UnnamedPersons `xml:"b249,omitempty" json:",omitempty"`
	Gender *Gender `xml:"x524,omitempty" json:",omitempty"`
	NameIdentifiers []NameIdentifier `xml:"nameidentifier,omitempty" json:",omitempty"`
	AlternativeNames []AlternativeName `xml:"alternativename,omitempty" json:",omitempty"`
	ContributorDates []ContributorDate `xml:"contributordate,omitempty" json:",omitempty"`
	ProfessionalAffiliations []ProfessionalAffiliation `xml:"professionalaffiliation,omitempty" json:",omitempty"`
	Prizes []Prize `xml:"prize,omitempty" json:",omitempty"`
	BiographicalNotes []Flow `xml:"b044,omitempty" json:",omitempty"`
	Websites []Website `xml:"website,omitempty" json:",omitempty"`
	ContributorDescriptions []Flow `xml:"b048,omitempty" json:",omitempty"`
	ContributorPlaces []ContributorPlace `xml:"contributorplace,omitempty" json:",omitempty"`
	SequenceNumber *DtDotStrictPositiveInteger `xml:"b034,omitempty" json:",omitempty"`
	ContributorRoles []ContributorRole `xml:"b035"`
	FromLanguages []FromLanguage `xml:"x412,omitempty" json:",omitempty"`
	ToLanguages []ToLanguage `xml:"x413,omitempty" json:",omitempty"`
	NameType *NameType `xml:"x414,omitempty" json:",omitempty"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// ContributorDate is not documented.
type ContributorDate struct {
	ContributorDateRole ContributorDateRole `xml:"x417"`
	DateFormat *DateFormat `xml:"j260,omitempty" json:",omitempty"`
	Date DtDotNonEmptyString `xml:"b306"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// ContributorPlace is not documented.
type ContributorPlace struct {
	RegionCode *RegionCode `xml:"b398,omitempty" json:",omitempty"`
	CountryCode *CountryCode `xml:"b251,omitempty" json:",omitempty"`
	ContributorPlaceRelator ContributorPlaceRelator `xml:"x418"`
	LocationNames []DtDotNonEmptyString `xml:"j349,omitempty" json:",omitempty"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// ContributorReference is not documented.
type ContributorReference struct {
	SequenceNumber *DtDotStrictPositiveInteger `xml:"b034,omitempty" json:",omitempty"`
	ContributorRoles []ContributorRole `xml:"b035"`
	NameIdentifiers []NameIdentifier `xml:"nameidentifier"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// CopyrightOwner is not documented.
type CopyrightOwner struct {
	PersonName *DtDotNonEmptyString `xml:"b036,omitempty" json:",omitempty"`
	CorporateName *DtDotNonEmptyString `xml:"b047,omitempty" json:",omitempty"`
	CopyrightOwnerIdentifiers []CopyrightOwnerIdentifier `xml:"copyrightowneridentifier,omitempty" json:",omitempty"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// CopyrightOwnerIdentifier is not documented.
type CopyrightOwnerIdentifier struct {
	CopyrightOwnerIDType CopyrightOwnerIDType `xml:"b392"`
	IDTypeName *DtDotNonEmptyString `xml:"b233,omitempty" json:",omitempty"`
	IDValue DtDotNonEmptyString `xml:"b244"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// CopyrightStatement is not documented.
type CopyrightStatement struct {
	CopyrightOwners []CopyrightOwner `xml:"copyrightowner,omitempty" json:",omitempty"`
	CopyrightYears []DtDotNonEmptyString `xml:"b087,omitempty" json:",omitempty"`
	CopyrightType *CopyrightType `xml:"x512,omitempty" json:",omitempty"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// DescriptiveDetail is not documented.
type DescriptiveDetail struct {
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// Discount is not documented.
type Discount struct {
	DiscountAmount *DtDotPositiveDecimal `xml:"x469,omitempty" json:",omitempty"`
	DiscountPercent *DtDotPercentDecimal `xml:"j267,omitempty" json:",omitempty"`
	DiscountType *DiscountType `xml:"x467,omitempty" json:",omitempty"`
	Quantity *DtDotPositiveDecimal `xml:"x320,omitempty" json:",omitempty"`
	ToQuantity *DtDotPositiveDecimal `xml:"x514,omitempty" json:",omitempty"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// DiscountCoded is not documented.
type DiscountCoded struct {
	DiscountCodeType DiscountCodeType `xml:"j363"`
	DiscountCodeTypeName *DtDotNonEmptyString `xml:"j378,omitempty" json:",omitempty"`
	DiscountCode DtDotNonEmptyString `xml:"j364"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// EpubLicense is not documented.
type EpubLicense struct {
	EpubLicenseNames []DtDotNonEmptyString `xml:"x511"`
	EpubLicenseExpressions []EpubLicenseExpression `xml:"epublicenseexpression,omitempty" json:",omitempty"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// EpubLicenseExpression is not documented.
type EpubLicenseExpression struct {
	EpubLicenseExpressionType EpubLicenseExpressionType `xml:"x508"`
	EpubLicenseExpressionTypeName *DtDotNonEmptyString `xml:"x509,omitempty" json:",omitempty"`
	EpubLicenseExpressionLink DtDotNonEmptyURI `xml:"x510"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// EpubUsageConstraint is not documented.
type EpubUsageConstraint struct {
	EpubUsageType EpubUsageType `xml:"x318"`
	EpubUsageStatus EpubUsageStatus `xml:"x319"`
	EpubUsageLimits []EpubUsageLimit `xml:"epubusagelimit,omitempty" json:",omitempty"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// EpubUsageLimit is not documented.
type EpubUsageLimit struct {
	Quantity DtDotPositiveDecimal `xml:"x320"`
	EpubUsageUnit EpubUsageUnit `xml:"x321"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// Event is not documented.
type Event struct {
	EventRole EventRole `xml:"x515"`
	EventNames []DtDotNonEmptyString `xml:"x516"`
	EventAcronyms []DtDotNonEmptyString `xml:"x517,omitempty" json:",omitempty"`
	EventNumber *DtDotStrictPositiveInteger `xml:"x518,omitempty" json:",omitempty"`
	EventThemes []DtDotNonEmptyString `xml:"x519,omitempty" json:",omitempty"`
	EventDate *DtDotNonEmptyString `xml:"x520,omitempty" json:",omitempty"`
	EventPlaces []DtDotNonEmptyString `xml:"x521,omitempty" json:",omitempty"`
	EventSponsors []EventSponsor `xml:"eventsponsor,omitempty" json:",omitempty"`
	Websites []Website `xml:"website,omitempty" json:",omitempty"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// EventIdentifier is not documented.
type EventIdentifier struct {
	EventIDType EventIDType `xml:"x547"`
	IDTypeName *DtDotNonEmptyString `xml:"b233,omitempty" json:",omitempty"`
	IDValue DtDotNonEmptyString `xml:"b244"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// EventOccurrence is not documented.
type EventOccurrence struct {
	EventIdentifiers []EventIdentifier `xml:"eventidentifier,omitempty" json:",omitempty"`
	OccurrenceDates []OccurrenceDate `xml:"occurrencedate"`
	EventStatus EventStatus `xml:"x549"`
	EventDescriptions []Flow `xml:"x550,omitempty" json:",omitempty"`
	EventSponsors []EventSponsor `xml:"eventsponsor,omitempty" json:",omitempty"`
	Websites []Website `xml:"website,omitempty" json:",omitempty"`
	RegionCode *RegionCode `xml:"b398,omitempty" json:",omitempty"`
	CountryCode *CountryCode `xml:"b251,omitempty" json:",omitempty"`
	LocationNames []DtDotNonEmptyString `xml:"j349,omitempty" json:",omitempty"`
	VenueName *DtDotNonEmptyString `xml:"x551,omitempty" json:",omitempty"`
	StreetAddress *DtDotNonEmptyString `xml:"x552,omitempty" json:",omitempty"`
	VenueNotes []Flow `xml:"x553,omitempty" json:",omitempty"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// EventSponsor is not documented.
type EventSponsor struct {
	PersonName *DtDotNonEmptyString `xml:"b036,omitempty" json:",omitempty"`
	CorporateName *DtDotNonEmptyString `xml:"b047,omitempty" json:",omitempty"`
	EventSponsorIdentifiers []EventSponsorIdentifier `xml:"eventsponsoridentifier,omitempty" json:",omitempty"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// EventSponsorIdentifier is not documented.
type EventSponsorIdentifier struct {
	EventSponsorIDType EventSponsorIDType `xml:"x522"`
	IDTypeName *DtDotNonEmptyString `xml:"b233,omitempty" json:",omitempty"`
	IDValue DtDotNonEmptyString `xml:"b244"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// Extent is not documented.
type Extent struct {
	ExtentValueRoman *DtDotRomanNumeralString `xml:"x421,omitempty" json:",omitempty"`
	ExtentValue *DtDotStrictPositiveDecimal `xml:"b219,omitempty" json:",omitempty"`
	ExtentType ExtentType `xml:"b218"`
	ExtentUnit ExtentUnit `xml:"b220"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// Funding is not documented.
type Funding struct {
	FundingIdentifiers []FundingIdentifier `xml:"fundingidentifier"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// FundingIdentifier is not documented.
type FundingIdentifier struct {
	FundingIDType FundingIDType `xml:"x523"`
	IDTypeName *DtDotNonEmptyString `xml:"b233,omitempty" json:",omitempty"`
	IDValue DtDotNonEmptyString `xml:"b244"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// Header is not documented.
type Header struct {
	Sender Sender `xml:"sender"`
	Addressees []Addressee `xml:"addressee,omitempty" json:",omitempty"`
	MessageNumber *DtDotStrictPositiveInteger `xml:"m180,omitempty" json:",omitempty"`
	MessageRepeat *DtDotStrictPositiveInteger `xml:"m181,omitempty" json:",omitempty"`
	SentDateTime DtDotDateOrDateTime `xml:"x307"`
	MessageNotes []DtDotNonEmptyString `xml:"m183,omitempty" json:",omitempty"`
	DefaultLanguageOfText *DefaultLanguageOfText `xml:"m184,omitempty" json:",omitempty"`
	DefaultPriceType *DefaultPriceType `xml:"x310,omitempty" json:",omitempty"`
	DefaultCurrencyCode *DefaultCurrencyCode `xml:"m186,omitempty" json:",omitempty"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// Imprint is not documented.
type Imprint struct {
	ImprintName *DtDotNonEmptyString `xml:"b079,omitempty" json:",omitempty"`
	ImprintIdentifiers []ImprintIdentifier `xml:"imprintidentifier,omitempty" json:",omitempty"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// ImprintIdentifier is not documented.
type ImprintIdentifier struct {
	ImprintIDType ImprintIDType `xml:"x445"`
	IDTypeName *DtDotNonEmptyString `xml:"b233,omitempty" json:",omitempty"`
	IDValue DtDotNonEmptyString `xml:"b244"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// Language is not documented.
type Language struct {
	LanguageRole LanguageRole `xml:"b253"`
	LanguageCode LanguageCode `xml:"b252"`
	CountryCode *CountryCode `xml:"b251,omitempty" json:",omitempty"`
	RegionCode *RegionCode `xml:"b398,omitempty" json:",omitempty"`
	ScriptCode *ScriptCode `xml:"x420,omitempty" json:",omitempty"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// LocationIdentifier is not documented.
type LocationIdentifier struct {
	LocationIDType LocationIDType `xml:"j377"`
	IDTypeName *DtDotNonEmptyString `xml:"b233,omitempty" json:",omitempty"`
	IDValue DtDotNonEmptyString `xml:"b244"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// MainSubject is not documented.
type MainSubject struct {
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// Market is not documented.
type Market struct {
	Territory Territory `xml:"territory"`
	SalesRestrictions []SalesRestriction `xml:"salesrestriction,omitempty" json:",omitempty"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// MarketDate is not documented.
type MarketDate struct {
	MarketDateRole MarketDateRole `xml:"j408"`
	DateFormat *DateFormat `xml:"j260,omitempty" json:",omitempty"`
	Date DtDotNonEmptyString `xml:"b306"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// MarketPublishingDetail is not documented.
type MarketPublishingDetail struct {
	PublisherRepresentatives []PublisherRepresentative `xml:"publisherrepresentative,omitempty" json:",omitempty"`
	ProductContacts []ProductContact `xml:"productcontact,omitempty" json:",omitempty"`
	MarketPublishingStatus MarketPublishingStatus `xml:"j407"`
	MarketPublishingStatusNotes []Flow `xml:"x406,omitempty" json:",omitempty"`
	MarketDates []MarketDate `xml:"marketdate,omitempty" json:",omitempty"`
	PromotionCampaigns []Flow `xml:"k165,omitempty" json:",omitempty"`
	PromotionContact *Flow `xml:"k166,omitempty" json:",omitempty"`
	InitialPrintRuns []Flow `xml:"k167,omitempty" json:",omitempty"`
	ReprintDetails []Flow `xml:"k309,omitempty" json:",omitempty"`
	CopiesSolds []Flow `xml:"k168,omitempty" json:",omitempty"`
	BookClubAdoptions []Flow `xml:"k169,omitempty" json:",omitempty"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// Measure is not documented.
type Measure struct {
	MeasureType MeasureType `xml:"x315"`
	Measurement DtDotStrictPositiveDecimal `xml:"c094"`
	MeasureUnitCode MeasureUnitCode `xml:"c095"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// NameAsSubject is not documented.
type NameAsSubject struct {
	Gender *Gender `xml:"x524,omitempty" json:",omitempty"`
	NameIdentifiers []NameIdentifier `xml:"nameidentifier,omitempty" json:",omitempty"`
	NameType *NameType `xml:"x414,omitempty" json:",omitempty"`
	AlternativeNames []AlternativeName `xml:"alternativename,omitempty" json:",omitempty"`
	SubjectDates []SubjectDate `xml:"subjectdate,omitempty" json:",omitempty"`
	ProfessionalAffiliations []ProfessionalAffiliation `xml:"professionalaffiliation,omitempty" json:",omitempty"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// NameIdentifier is not documented.
type NameIdentifier struct {
	NameIDType NameIDType `xml:"x415"`
	IDTypeName *DtDotNonEmptyString `xml:"b233,omitempty" json:",omitempty"`
	IDValue DtDotNonEmptyString `xml:"b244"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// NewSupplier is not documented.
type NewSupplier struct {
	SupplierName *DtDotNonEmptyString `xml:"j137,omitempty" json:",omitempty"`
	SupplierIdentifiers []SupplierIdentifier `xml:"supplieridentifier,omitempty" json:",omitempty"`
	TelephoneNumbers []DtDotNonEmptyString `xml:"j270,omitempty" json:",omitempty"`
	FaxNumbers []DtDotNonEmptyString `xml:"j271,omitempty" json:",omitempty"`
	EmailAddresss []DtDotEmailString `xml:"j272,omitempty" json:",omitempty"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// NoCollection is not documented.
type NoCollection struct {
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// NoContributor is not documented.
type NoContributor struct {
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// NoEdition is not documented.
type NoEdition struct {
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// NoPrefix is not documented.
type NoPrefix struct {
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// NoProduct is not documented.
type NoProduct struct {
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// ONIXMessage is not documented.
type ONIXMessage struct {
	NoProduct *NoProduct `xml:"x507,omitempty" json:",omitempty"`
	Products []Product `xml:"product,omitempty" json:",omitempty"`
	Header Header `xml:"header"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
	Release Release `xml:"release,attr"`
}

// OccurrenceDate is not documented.
type OccurrenceDate struct {
	OccurrenceDateRole OccurrenceDateRole `xml:"x554"`
	DateFormat *DateFormat `xml:"j260,omitempty" json:",omitempty"`
	Date DtDotNonEmptyString `xml:"b306"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// OnOrderDetail is not documented.
type OnOrderDetail struct {
	OnOrder DtDotPositiveInteger `xml:"j351"`
	Proximity *Proximity `xml:"x502,omitempty" json:",omitempty"`
	ExpectedDate DtDotNonEmptyString `xml:"j302"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// PageRun is not documented.
type PageRun struct {
	FirstPageNumber DtDotNonEmptyString `xml:"b286"`
	LastPageNumber *DtDotNonEmptyString `xml:"b287,omitempty" json:",omitempty"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// Price is not documented.
type Price struct {
	UnpricedItemType *UnpricedItemType `xml:"j192,omitempty" json:",omitempty"`
	PriceAmount *DtDotStrictPositiveDecimal `xml:"j151,omitempty" json:",omitempty"`
	PriceCoded *PriceCoded `xml:"pricecoded,omitempty" json:",omitempty"`
	Taxs []Tax `xml:"tax,omitempty" json:",omitempty"`
	TaxExempt *TaxExempt `xml:"x546,omitempty" json:",omitempty"`
	PriceIdentifiers []PriceIdentifier `xml:"priceidentifier,omitempty" json:",omitempty"`
	PriceType *PriceType `xml:"x462,omitempty" json:",omitempty"`
	PriceQualifier *PriceQualifier `xml:"j261,omitempty" json:",omitempty"`
	EpubTechnicalProtections []EpubTechnicalProtection `xml:"x317,omitempty" json:",omitempty"`
	PriceConstraints []PriceConstraint `xml:"priceconstraint,omitempty" json:",omitempty"`
	EpubLicense *EpubLicense `xml:"epublicense,omitempty" json:",omitempty"`
	PriceTypeDescriptions []DtDotNonEmptyString `xml:"j262,omitempty" json:",omitempty"`
	PricePer *PricePer `xml:"j239,omitempty" json:",omitempty"`
	PriceConditions []PriceCondition `xml:"pricecondition,omitempty" json:",omitempty"`
	MinimumOrderQuantity *DtDotStrictPositiveInteger `xml:"j263,omitempty" json:",omitempty"`
	BatchBonuss []BatchBonus `xml:"batchbonus,omitempty" json:",omitempty"`
	DiscountCodeds []DiscountCoded `xml:"discountcoded,omitempty" json:",omitempty"`
	Discounts []Discount `xml:"discount,omitempty" json:",omitempty"`
	PriceStatus *PriceStatus `xml:"j266,omitempty" json:",omitempty"`
	CurrencyCode *CurrencyCode `xml:"j152,omitempty" json:",omitempty"`
	Territory *Territory `xml:"territory,omitempty" json:",omitempty"`
	CurrencyZone *CurrencyZone `xml:"x475,omitempty" json:",omitempty"`
	ComparisonProductPrices []ComparisonProductPrice `xml:"comparisonproductprice,omitempty" json:",omitempty"`
	PriceDates []PriceDate `xml:"pricedate,omitempty" json:",omitempty"`
	PrintedOnProduct *PrintedOnProduct `xml:"x301,omitempty" json:",omitempty"`
	PositionOnProduct *PositionOnProduct `xml:"x313,omitempty" json:",omitempty"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// PriceCoded is not documented.
type PriceCoded struct {
	PriceCodeType PriceCodeType `xml:"x465"`
	PriceCodeTypeName *DtDotNonEmptyString `xml:"x477,omitempty" json:",omitempty"`
	PriceCode DtDotNonEmptyString `xml:"x468"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// PriceCondition is not documented.
type PriceCondition struct {
	PriceConditionType PriceConditionType `xml:"x463"`
	PriceConditionQuantitys []PriceConditionQuantity `xml:"priceconditionquantity,omitempty" json:",omitempty"`
	ProductIdentifiers []ProductIdentifier `xml:"productidentifier,omitempty" json:",omitempty"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// PriceConditionQuantity is not documented.
type PriceConditionQuantity struct {
	PriceConditionQuantityType PriceConditionQuantityType `xml:"x464"`
	Quantity DtDotPositiveDecimal `xml:"x320"`
	QuantityUnit QuantityUnit `xml:"x466"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// PriceConstraint is not documented.
type PriceConstraint struct {
	PriceConstraintType PriceConstraintType `xml:"x529"`
	PriceConstraintStatus PriceConstraintStatus `xml:"x530"`
	PriceConstraintLimits []PriceConstraintLimit `xml:"priceconstraintlimit,omitempty" json:",omitempty"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// PriceConstraintLimit is not documented.
type PriceConstraintLimit struct {
	Quantity DtDotPositiveDecimal `xml:"x320"`
	PriceConstraintUnit PriceConstraintUnit `xml:"x531"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// PriceDate is not documented.
type PriceDate struct {
	PriceDateRole PriceDateRole `xml:"x476"`
	DateFormat *DateFormat `xml:"j260,omitempty" json:",omitempty"`
	Date DtDotNonEmptyString `xml:"b306"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// PriceIdentifier is not documented.
type PriceIdentifier struct {
	PriceIDType PriceIDType `xml:"x506"`
	IDTypeName *DtDotNonEmptyString `xml:"b233,omitempty" json:",omitempty"`
	IDValue DtDotNonEmptyString `xml:"b244"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// PrimaryPart is not documented.
type PrimaryPart struct {
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// Prize is not documented.
type Prize struct {
	PrizeNames []DtDotNonEmptyString `xml:"g126"`
	PrizeYear *DtDotYear `xml:"g127,omitempty" json:",omitempty"`
	PrizeCountry *PrizeCountry `xml:"g128,omitempty" json:",omitempty"`
	PrizeRegion *PrizeRegion `xml:"x556,omitempty" json:",omitempty"`
	PrizeCode *PrizeCode `xml:"g129,omitempty" json:",omitempty"`
	PrizeStatements []Flow `xml:"x503,omitempty" json:",omitempty"`
	PrizeJurys []Flow `xml:"g343,omitempty" json:",omitempty"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// Product is not documented.
type Product struct {
	DescriptiveDetail *DescriptiveDetail `xml:"descriptivedetail,omitempty" json:",omitempty"`
	CollateralDetail *CollateralDetail `xml:"collateraldetail,omitempty" json:",omitempty"`
	PromotionDetail *PromotionDetail `xml:"promotiondetail,omitempty" json:",omitempty"`
	ContentDetail *ContentDetail `xml:"contentdetail,omitempty" json:",omitempty"`
	PublishingDetail *PublishingDetail `xml:"publishingdetail,omitempty" json:",omitempty"`
	RelatedMaterial *RelatedMaterial `xml:"relatedmaterial,omitempty" json:",omitempty"`
	ProductSupplys []ProductSupply `xml:"productsupply,omitempty" json:",omitempty"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// ProductClassification is not documented.
type ProductClassification struct {
	ProductClassificationType ProductClassificationType `xml:"b274"`
	ProductClassificationTypeName *DtDotNonEmptyString `xml:"x555,omitempty" json:",omitempty"`
	ProductClassificationCode DtDotNonEmptyString `xml:"b275"`
	Percent *DtDotPercentDecimal `xml:"b337,omitempty" json:",omitempty"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// ProductContact is not documented.
type ProductContact struct {
	ProductContactName *DtDotNonEmptyString `xml:"x484,omitempty" json:",omitempty"`
	ProductContactIdentifiers []ProductContactIdentifier `xml:"productcontactidentifier,omitempty" json:",omitempty"`
	ProductContactRole ProductContactRole `xml:"x482"`
	ContactName *DtDotNonEmptyString `xml:"x299,omitempty" json:",omitempty"`
	EmailAddress *DtDotEmailString `xml:"j272,omitempty" json:",omitempty"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// ProductContactIdentifier is not documented.
type ProductContactIdentifier struct {
	ProductContactIDType ProductContactIDType `xml:"x483"`
	IDTypeName *DtDotNonEmptyString `xml:"b233,omitempty" json:",omitempty"`
	IDValue DtDotNonEmptyString `xml:"b244"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// ProductFormFeature is not documented.
type ProductFormFeature struct {
	ProductFormFeatureType ProductFormFeatureType `xml:"b334"`
	ProductFormFeatureValue *DtDotNonEmptyString `xml:"b335,omitempty" json:",omitempty"`
	ProductFormFeatureDescriptions []DtDotNonEmptyString `xml:"b336,omitempty" json:",omitempty"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// ProductIdentifier is not documented.
type ProductIdentifier struct {
	ProductIDType ProductIDType `xml:"b221"`
	IDTypeName *DtDotNonEmptyString `xml:"b233,omitempty" json:",omitempty"`
	IDValue DtDotNonEmptyString `xml:"b244"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// ProductPart is not documented.
type ProductPart struct {
	NumberOfCopies *DtDotStrictPositiveInteger `xml:"x323,omitempty" json:",omitempty"`
	NumberOfItemsOfThisForm *DtDotStrictPositiveInteger `xml:"x322,omitempty" json:",omitempty"`
	PrimaryPart *PrimaryPart `xml:"x457,omitempty" json:",omitempty"`
	ProductIdentifiers []ProductIdentifier `xml:"productidentifier,omitempty" json:",omitempty"`
	ProductForm ProductForm `xml:"b012"`
	ProductFormDetails []ProductFormDetail `xml:"b333,omitempty" json:",omitempty"`
	ProductFormFeatures []ProductFormFeature `xml:"productformfeature,omitempty" json:",omitempty"`
	ProductPackaging *ProductPackaging `xml:"b225,omitempty" json:",omitempty"`
	ProductFormDescriptions []DtDotNonEmptyString `xml:"b014,omitempty" json:",omitempty"`
	ProductContentTypes []ProductContentType `xml:"b385,omitempty" json:",omitempty"`
	Measures []Measure `xml:"measure,omitempty" json:",omitempty"`
	CountryOfManufacture *CountryOfManufacture `xml:"x316,omitempty" json:",omitempty"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// ProductSupply is not documented.
type ProductSupply struct {
	Markets []Market `xml:"market,omitempty" json:",omitempty"`
	MarketPublishingDetail *MarketPublishingDetail `xml:"marketpublishingdetail,omitempty" json:",omitempty"`
	SupplyDetails []SupplyDetail `xml:"supplydetail"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// ProfessionalAffiliation is not documented.
type ProfessionalAffiliation struct {
	Affiliation *DtDotNonEmptyString `xml:"b046,omitempty" json:",omitempty"`
	ProfessionalPositions []DtDotNonEmptyString `xml:"b045,omitempty" json:",omitempty"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// PromotionDetail is not documented.
type PromotionDetail struct {
	PromotionalEvents []PromotionalEvent `xml:"promotionalevent,omitempty" json:",omitempty"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// PromotionalEvent is not documented.
type PromotionalEvent struct {
	NoContributor *NoContributor `xml:"n339,omitempty" json:",omitempty"`
	Contributors []Contributor `xml:"contributor,omitempty" json:",omitempty"`
	ContributorReferences []ContributorReference `xml:"contributorreference,omitempty" json:",omitempty"`
	ContributorStatements []Flow `xml:"b049,omitempty" json:",omitempty"`
	EventIdentifiers []EventIdentifier `xml:"eventidentifier,omitempty" json:",omitempty"`
	EventTypes []EventType `xml:"x548"`
	EventStatus *EventStatus `xml:"x549,omitempty" json:",omitempty"`
	ContentAudiences []ContentAudience `xml:"x427"`
	EventNames []DtDotNonEmptyString `xml:"x516"`
	EventDescriptions []Flow `xml:"x550,omitempty" json:",omitempty"`
	EventOccurrences []EventOccurrence `xml:"eventoccurrence"`
	EventSponsors []EventSponsor `xml:"eventsponsor,omitempty" json:",omitempty"`
	Websites []Website `xml:"website,omitempty" json:",omitempty"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// Publisher is not documented.
type Publisher struct {
	PublisherName *DtDotNonEmptyString `xml:"b081,omitempty" json:",omitempty"`
	PublisherIdentifiers []PublisherIdentifier `xml:"publisheridentifier,omitempty" json:",omitempty"`
	PublishingRole PublishingRole `xml:"b291"`
	Fundings []Funding `xml:"funding,omitempty" json:",omitempty"`
	Websites []Website `xml:"website,omitempty" json:",omitempty"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// PublisherIdentifier is not documented.
type PublisherIdentifier struct {
	PublisherIDType PublisherIDType `xml:"x447"`
	IDTypeName *DtDotNonEmptyString `xml:"b233,omitempty" json:",omitempty"`
	IDValue DtDotNonEmptyString `xml:"b244"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// PublisherRepresentative is not documented.
type PublisherRepresentative struct {
	AgentName *DtDotNonEmptyString `xml:"j401,omitempty" json:",omitempty"`
	AgentIdentifiers []AgentIdentifier `xml:"agentidentifier,omitempty" json:",omitempty"`
	AgentRole AgentRole `xml:"j402"`
	TelephoneNumbers []DtDotNonEmptyString `xml:"j270,omitempty" json:",omitempty"`
	FaxNumbers []DtDotNonEmptyString `xml:"j271,omitempty" json:",omitempty"`
	EmailAddresss []DtDotEmailString `xml:"j272,omitempty" json:",omitempty"`
	Websites []Website `xml:"website,omitempty" json:",omitempty"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// PublishingDate is not documented.
type PublishingDate struct {
	PublishingDateRole PublishingDateRole `xml:"x448"`
	DateFormat *DateFormat `xml:"j260,omitempty" json:",omitempty"`
	Date DtDotNonEmptyString `xml:"b306"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// PublishingDetail is not documented.
type PublishingDetail struct {
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// RecordSourceIdentifier is not documented.
type RecordSourceIdentifier struct {
	RecordSourceIDType RecordSourceIDType `xml:"x311"`
	IDTypeName *DtDotNonEmptyString `xml:"b233,omitempty" json:",omitempty"`
	IDValue DtDotNonEmptyString `xml:"b244"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// Reissue is not documented.
type Reissue struct {
	ReissueDate DtDotNonEmptyString `xml:"j365"`
	ReissueDescription *Flow `xml:"j366,omitempty" json:",omitempty"`
	Prices []Price `xml:"price,omitempty" json:",omitempty"`
	SupportingResources []SupportingResource `xml:"supportingresource,omitempty" json:",omitempty"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// RelatedMaterial is not documented.
type RelatedMaterial struct {
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// RelatedProduct is not documented.
type RelatedProduct struct {
	ProductRelationCodes []ProductRelationCode `xml:"x455"`
	ProductIdentifiers []ProductIdentifier `xml:"productidentifier"`
	ProductForm *ProductForm `xml:"b012,omitempty" json:",omitempty"`
	ProductFormDetails []ProductFormDetail `xml:"b333,omitempty" json:",omitempty"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// RelatedWork is not documented.
type RelatedWork struct {
	WorkRelationCode WorkRelationCode `xml:"x454"`
	WorkIdentifiers []WorkIdentifier `xml:"workidentifier"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// ReligiousText is not documented.
type ReligiousText struct {
	Bible *Bible `xml:"bible,omitempty" json:",omitempty"`
	ReligiousTextIdentifier *ReligiousTextIdentifier `xml:"b376,omitempty" json:",omitempty"`
	ReligiousTextFeatures []ReligiousTextFeature `xml:"religioustextfeature,omitempty" json:",omitempty"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// ReligiousTextFeature is not documented.
type ReligiousTextFeature struct {
	ReligiousTextFeatureType ReligiousTextFeatureType `xml:"b358"`
	ReligiousTextFeatureCode ReligiousTextFeatureCode `xml:"b359"`
	ReligiousTextFeatureDescriptions []Flow `xml:"b360,omitempty" json:",omitempty"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// ResourceFeature is not documented.
type ResourceFeature struct {
	ResourceFeatureType ResourceFeatureType `xml:"x438"`
	FeatureValue *DtDotNonEmptyString `xml:"x439,omitempty" json:",omitempty"`
	FeatureNotes []Flow `xml:"x440,omitempty" json:",omitempty"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// ResourceVersion is not documented.
type ResourceVersion struct {
	ResourceForm ResourceForm `xml:"x441"`
	ResourceVersionFeatures []ResourceVersionFeature `xml:"resourceversionfeature,omitempty" json:",omitempty"`
	ResourceLinks []DtDotNonEmptyURI `xml:"x435"`
	ContentDates []ContentDate `xml:"contentdate,omitempty" json:",omitempty"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// ResourceVersionFeature is not documented.
type ResourceVersionFeature struct {
	ResourceVersionFeatureType ResourceVersionFeatureType `xml:"x442"`
	FeatureValue *DtDotNonEmptyString `xml:"x439,omitempty" json:",omitempty"`
	FeatureNotes []Flow `xml:"x440,omitempty" json:",omitempty"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// ReturnsConditions is not documented.
type ReturnsConditions struct {
	ReturnsCodeType ReturnsCodeType `xml:"j268"`
	ReturnsCodeTypeName *DtDotNonEmptyString `xml:"x460,omitempty" json:",omitempty"`
	ReturnsCode DtDotNonEmptyString `xml:"j269"`
	ReturnsNotes []DtDotNonEmptyString `xml:"x528,omitempty" json:",omitempty"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// ReviewRating is not documented.
type ReviewRating struct {
	Rating DtDotPositiveDecimal `xml:"x525"`
	RatingLimit *DtDotPositiveInteger `xml:"x526,omitempty" json:",omitempty"`
	RatingUnitss []DtDotNonEmptyString `xml:"x527,omitempty" json:",omitempty"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// SalesOutlet is not documented.
type SalesOutlet struct {
	SalesOutletName *DtDotNonEmptyString `xml:"b382,omitempty" json:",omitempty"`
	SalesOutletIdentifiers []SalesOutletIdentifier `xml:"salesoutletidentifier,omitempty" json:",omitempty"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// SalesOutletIdentifier is not documented.
type SalesOutletIdentifier struct {
	SalesOutletIDType SalesOutletIDType `xml:"b393"`
	IDTypeName *DtDotNonEmptyString `xml:"b233,omitempty" json:",omitempty"`
	IDValue DtDotNonEmptyString `xml:"b244"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// SalesRestriction is not documented.
type SalesRestriction struct {
	SalesRestrictionType SalesRestrictionType `xml:"b381"`
	SalesOutlets []SalesOutlet `xml:"salesoutlet,omitempty" json:",omitempty"`
	SalesRestrictionNotes []Flow `xml:"x453,omitempty" json:",omitempty"`
	StartDate *DtDotNonEmptyString `xml:"b324,omitempty" json:",omitempty"`
	EndDate *DtDotNonEmptyString `xml:"b325,omitempty" json:",omitempty"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// SalesRights is not documented.
type SalesRights struct {
	SalesRightsType SalesRightsType `xml:"b089"`
	Territory Territory `xml:"territory"`
	SalesRestrictions []SalesRestriction `xml:"salesrestriction,omitempty" json:",omitempty"`
	ProductIdentifiers []ProductIdentifier `xml:"productidentifier,omitempty" json:",omitempty"`
	PublisherName *DtDotNonEmptyString `xml:"b081,omitempty" json:",omitempty"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// Sender is not documented.
type Sender struct {
	SenderName *DtDotNonEmptyString `xml:"x298,omitempty" json:",omitempty"`
	SenderIdentifiers []SenderIdentifier `xml:"senderidentifier,omitempty" json:",omitempty"`
	ContactName *DtDotNonEmptyString `xml:"x299,omitempty" json:",omitempty"`
	EmailAddress *DtDotEmailString `xml:"j272,omitempty" json:",omitempty"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// SenderIdentifier is not documented.
type SenderIdentifier struct {
	SenderIDType SenderIDType `xml:"m379"`
	IDTypeName *DtDotNonEmptyString `xml:"b233,omitempty" json:",omitempty"`
	IDValue DtDotNonEmptyString `xml:"b244"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// Stock is not documented.
type Stock struct {
	StockQuantityCodeds []StockQuantityCoded `xml:"stockquantitycoded,omitempty" json:",omitempty"`
	OnHand *DtDotInteger `xml:"j350,omitempty" json:",omitempty"`
	Proximity *Proximity `xml:"x502,omitempty" json:",omitempty"`
	Reserved *DtDotPositiveInteger `xml:"x536,omitempty" json:",omitempty"`
	OnOrder *DtDotPositiveInteger `xml:"j351,omitempty" json:",omitempty"`
	CBO *DtDotPositiveInteger `xml:"j375,omitempty" json:",omitempty"`
	LocationIdentifiers []LocationIdentifier `xml:"locationidentifier,omitempty" json:",omitempty"`
	LocationNames []DtDotNonEmptyString `xml:"j349,omitempty" json:",omitempty"`
	OnOrderDetails []OnOrderDetail `xml:"onorderdetail,omitempty" json:",omitempty"`
	Velocitys []Velocity `xml:"velocity,omitempty" json:",omitempty"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// StockQuantityCoded is not documented.
type StockQuantityCoded struct {
	StockQuantityCodeType StockQuantityCodeType `xml:"j293"`
	StockQuantityCodeTypeName *DtDotNonEmptyString `xml:"j296,omitempty" json:",omitempty"`
	StockQuantityCode DtDotNonEmptyString `xml:"j297"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// Subject is not documented.
type Subject struct {
	SubjectHeadingTexts []DtDotNonEmptyString `xml:"b070,omitempty" json:",omitempty"`
	SubjectCode *DtDotNonEmptyString `xml:"b069,omitempty" json:",omitempty"`
	MainSubject *MainSubject `xml:"x425,omitempty" json:",omitempty"`
	SubjectSchemeIdentifier SubjectSchemeIdentifier `xml:"b067"`
	SubjectSchemeName *DtDotNonEmptyString `xml:"b171,omitempty" json:",omitempty"`
	SubjectSchemeVersion *DtDotNonEmptyString `xml:"b068,omitempty" json:",omitempty"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// SubjectDate is not documented.
type SubjectDate struct {
	SubjectDateRole SubjectDateRole `xml:"x534"`
	DateFormat *DateFormat `xml:"j260,omitempty" json:",omitempty"`
	Date DtDotNonEmptyString `xml:"b306"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// Supplier is not documented.
type Supplier struct {
	SupplierName *DtDotNonEmptyString `xml:"j137,omitempty" json:",omitempty"`
	SupplierIdentifiers []SupplierIdentifier `xml:"supplieridentifier,omitempty" json:",omitempty"`
	SupplierRole SupplierRole `xml:"j292"`
	TelephoneNumbers []DtDotNonEmptyString `xml:"j270,omitempty" json:",omitempty"`
	FaxNumbers []DtDotNonEmptyString `xml:"j271,omitempty" json:",omitempty"`
	EmailAddresss []DtDotEmailString `xml:"j272,omitempty" json:",omitempty"`
	Websites []Website `xml:"website,omitempty" json:",omitempty"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// SupplierIdentifier is not documented.
type SupplierIdentifier struct {
	SupplierIDType SupplierIDType `xml:"j345"`
	IDTypeName *DtDotNonEmptyString `xml:"b233,omitempty" json:",omitempty"`
	IDValue DtDotNonEmptyString `xml:"b244"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// SupplierOwnCoding is not documented.
type SupplierOwnCoding struct {
	SupplierCodeType SupplierCodeType `xml:"x458"`
	SupplierCodeTypeName *DtDotNonEmptyString `xml:"x513,omitempty" json:",omitempty"`
	SupplierCodeValue DtDotNonEmptyString `xml:"x459"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// SupplyContact is not documented.
type SupplyContact struct {
	SupplyContactName *DtDotNonEmptyString `xml:"x539,omitempty" json:",omitempty"`
	SupplyContactIdentifiers []SupplyContactIdentifier `xml:"supplycontactidentifier,omitempty" json:",omitempty"`
	SupplyContactRole SupplyContactRole `xml:"x537"`
	ContactName *DtDotNonEmptyString `xml:"x299,omitempty" json:",omitempty"`
	EmailAddress *DtDotEmailString `xml:"j272,omitempty" json:",omitempty"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// SupplyContactIdentifier is not documented.
type SupplyContactIdentifier struct {
	SupplyContactIDType SupplyContactIDType `xml:"x538"`
	IDTypeName *DtDotNonEmptyString `xml:"b233,omitempty" json:",omitempty"`
	IDValue DtDotNonEmptyString `xml:"b244"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// SupplyDate is not documented.
type SupplyDate struct {
	SupplyDateRole SupplyDateRole `xml:"x461"`
	DateFormat *DateFormat `xml:"j260,omitempty" json:",omitempty"`
	Date DtDotNonEmptyString `xml:"b306"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// SupplyDetail is not documented.
type SupplyDetail struct {
	UnpricedItemType *UnpricedItemType `xml:"j192,omitempty" json:",omitempty"`
	Prices []Price `xml:"price,omitempty" json:",omitempty"`
	Supplier Supplier `xml:"supplier"`
	SupplyContacts []SupplyContact `xml:"supplycontact,omitempty" json:",omitempty"`
	SupplierOwnCodings []SupplierOwnCoding `xml:"supplierowncoding,omitempty" json:",omitempty"`
	ReturnsConditionss []ReturnsConditions `xml:"returnsconditions,omitempty" json:",omitempty"`
	ProductAvailability ProductAvailability `xml:"j396"`
	SupplyDates []SupplyDate `xml:"supplydate,omitempty" json:",omitempty"`
	OrderTime *DtDotPositiveInteger `xml:"j144,omitempty" json:",omitempty"`
	NewSupplier *NewSupplier `xml:"newsupplier,omitempty" json:",omitempty"`
	Stocks []Stock `xml:"stock,omitempty" json:",omitempty"`
	PackQuantity *DtDotStrictPositiveInteger `xml:"j145,omitempty" json:",omitempty"`
	PalletQuantity *DtDotStrictPositiveInteger `xml:"x545,omitempty" json:",omitempty"`
	Reissue *Reissue `xml:"reissue,omitempty" json:",omitempty"`
	OrderQuantityMinimums []DtDotStrictPositiveInteger `xml:"x532,omitempty" json:",omitempty"`
	OrderQuantityMultiple *DtDotStrictPositiveInteger `xml:"x533,omitempty" json:",omitempty"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// SupportingResource is not documented.
type SupportingResource struct {
	ResourceContentType ResourceContentType `xml:"x436"`
	ContentAudiences []ContentAudience `xml:"x427"`
	Territory *Territory `xml:"territory,omitempty" json:",omitempty"`
	ResourceMode ResourceMode `xml:"x437"`
	ResourceFeatures []ResourceFeature `xml:"resourcefeature,omitempty" json:",omitempty"`
	ResourceVersions []ResourceVersion `xml:"resourceversion"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// Tax is not documented.
type Tax struct {
	TaxRatePercent *DtDotPercentDecimal `xml:"x472,omitempty" json:",omitempty"`
	TaxableAmount *DtDotStrictPositiveDecimal `xml:"x473,omitempty" json:",omitempty"`
	TaxAmount *DtDotPositiveDecimal `xml:"x474,omitempty" json:",omitempty"`
	ProductIdentifiers []ProductIdentifier `xml:"productidentifier,omitempty" json:",omitempty"`
	PricePartDescriptions []DtDotNonEmptyString `xml:"x535,omitempty" json:",omitempty"`
	TaxType *TaxType `xml:"x470,omitempty" json:",omitempty"`
	TaxRateCode *TaxRateCode `xml:"x471,omitempty" json:",omitempty"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// TaxExempt is not documented.
type TaxExempt struct {
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// Territory is not documented.
type Territory struct {
	CountriesIncluded *DtDotCountryCodeList `xml:"x449,omitempty" json:",omitempty"`
	RegionsIncluded *DtDotRegionCodeList `xml:"x450,omitempty" json:",omitempty"`
	RegionsExcluded *DtDotRegionCodeList `xml:"x452,omitempty" json:",omitempty"`
	CountriesExcluded *DtDotCountryCodeList `xml:"x451,omitempty" json:",omitempty"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// TextContent is not documented.
type TextContent struct {
	TextType TextType `xml:"x426"`
	ContentAudiences []ContentAudience `xml:"x427"`
	Territory *Territory `xml:"territory,omitempty" json:",omitempty"`
	Texts []Flow `xml:"d104"`
	ReviewRating *ReviewRating `xml:"reviewrating,omitempty" json:",omitempty"`
	TextAuthors []DtDotNonEmptyString `xml:"d107,omitempty" json:",omitempty"`
	TextSourceCorporate *DtDotNonEmptyString `xml:"b374,omitempty" json:",omitempty"`
	TextSourceDescriptions []Flow `xml:"x557,omitempty" json:",omitempty"`
	SourceTitles []DtDotNonEmptyString `xml:"x428,omitempty" json:",omitempty"`
	ContentDates []ContentDate `xml:"contentdate,omitempty" json:",omitempty"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// TextItem is not documented.
type TextItem struct {
	TextItemType TextItemType `xml:"b290"`
	TextItemIdentifiers []TextItemIdentifier `xml:"textitemidentifier,omitempty" json:",omitempty"`
	PageRuns []PageRun `xml:"pagerun,omitempty" json:",omitempty"`
	NumberOfPages *DtDotStrictPositiveInteger `xml:"b061,omitempty" json:",omitempty"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// TextItemIdentifier is not documented.
type TextItemIdentifier struct {
	TextItemIDType TextItemIDType `xml:"b285"`
	IDTypeName *DtDotNonEmptyString `xml:"b233,omitempty" json:",omitempty"`
	IDValue DtDotNonEmptyString `xml:"b244"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// TimeRun is not documented.
type TimeRun struct {
	StartTime DtDotTimeOrDuration `xml:"x542"`
	EndTime *DtDotTimeOrDuration `xml:"x543,omitempty" json:",omitempty"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// TitleDetail is not documented.
type TitleDetail struct {
	TitleType TitleType `xml:"b202"`
	TitleElements []TitleElement `xml:"titleelement"`
	TitleStatement *Flow `xml:"x478,omitempty" json:",omitempty"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// TitleElement is not documented.
type TitleElement struct {
	TitleText *DtDotNonEmptyString `xml:"b203,omitempty" json:",omitempty"`
	TitlePrefix *DtDotNonEmptyString `xml:"b030,omitempty" json:",omitempty"`
	NoPrefix *NoPrefix `xml:"x501,omitempty" json:",omitempty"`
	TitleWithoutPrefix *DtDotNonEmptyString `xml:"b031,omitempty" json:",omitempty"`
	PartNumber *DtDotNonEmptyString `xml:"x410,omitempty" json:",omitempty"`
	YearOfAnnual *DtDotYearOrYearRange `xml:"b020,omitempty" json:",omitempty"`
	SequenceNumber *DtDotStrictPositiveInteger `xml:"b034,omitempty" json:",omitempty"`
	TitleElementLevel TitleElementLevel `xml:"x409"`
	Subtitle *DtDotNonEmptyString `xml:"b029,omitempty" json:",omitempty"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// Velocity is not documented.
type Velocity struct {
	VelocityMetric VelocityMetric `xml:"x504"`
	Rate DtDotInteger `xml:"x505"`
	Proximity *Proximity `xml:"x502,omitempty" json:",omitempty"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// Website is not documented.
type Website struct {
	WebsiteRole *WebsiteRole `xml:"b367,omitempty" json:",omitempty"`
	WebsiteDescriptions []Flow `xml:"b294,omitempty" json:",omitempty"`
	WebsiteLinks []DtDotNonEmptyURI `xml:"b295"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}

// WorkIdentifier is not documented.
type WorkIdentifier struct {
	WorkIDType WorkIDType `xml:"b201"`
	IDTypeName *DtDotNonEmptyString `xml:"b233,omitempty" json:",omitempty"`
	IDValue DtDotNonEmptyString `xml:"b244"`
	Datestamp *DtDotDateOrDateTime `xml:"datestamp,omitempty,attr" json:",omitempty"`
	Sourcetype *SourceTypeCode `xml:"sourcetype,omitempty,attr" json:",omitempty"`
	Sourcename *DtDotNonEmptyString `xml:"sourcename,omitempty,attr" json:",omitempty"`
}
