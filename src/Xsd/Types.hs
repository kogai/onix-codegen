{-# LANGUAGE OverloadedStrings #-}

module Xsd.Types
  ( Xsd (..),
    Namespace (..),
    Child (..),
    Type (..),
    SimpleType (..),
    Constraint (..),
    ComplexType (..),
    Content (..),
    PlainContent (..),
    SimpleContent (..),
    SimpleExtension (..),
    SimpleRestriction (..),
    ComplexContent (..),
    ComplexExtension (..),
    ComplexRestriction (..),
    RefOr (..),
    refOr,
    Element (..),
    ElementInline (..),
    ElementRef (..),
    Occurs (..),
    MaxOccurs (..),
    Attribute (..),
    AttributeRef (..),
    AttributeInline (..),
    Annotation (..),
    Use (..),
    ModelGroup (..),
    ChoiceInChild (..),
    SequenceInChild (..),
    Import (..),
    Include (..),
    QName (..),
    schemaNamespace,
  )
where

import Data.Text (Text)

data Xsd = Xsd
  { targetNamespace :: Maybe Namespace,
    children :: [Child]
    -- XXX: Investigate whether there could be any dependency on the order
    -- on children. E.g whether an element can reference type from other
    -- namespace only after the namespace is imported.
    -- If there is no such dependency, then get rid of Child and list
    -- types and elements separately here.
  }
  deriving (Show, Eq)

-- | Namespace name, e.g. \"http://www.w3.org/2001/XMLSchema\"
newtype Namespace = Namespace
  { fromNamespace :: Text
  }
  deriving (Show, Eq, Ord)

data Child
  = ChildElement Element
  | ChildType QName Type
  | ChildImport Import
  | ChildInclude Include
  | ChildAttribute Attribute
  deriving (Show, Eq)

newtype Annotation = Documentation Text
  deriving (Show, Eq)

--  (annotation?,(element|group|choice|sequence|any)*)
data ElementInline = ElementInline
  { elementName :: QName,
    elementType :: RefOr Type,
    elementOccurs :: Occurs,
    elementNillable :: Bool,
    elementAnnotations :: [Annotation]
  }
  deriving (Show, Eq)

data ElementRef = ElementRef
  { elementRefName :: QName,
    elementRefOccurs :: Occurs
  }
  deriving (Show, Eq)

data Element
  = RefElement ElementRef
  | InlineElement ElementInline
  deriving (Show, Eq)

data MaxOccurs
  = MaxOccurs Int
  | MaxOccursUnbound
  deriving (Show, Eq)

newtype Occurs = Occurs (Int, MaxOccurs)
  deriving (Show, Eq)

data Type
  = TypeSimple SimpleType
  | TypeComplex ComplexType
  deriving (Show, Eq)

data Import = Import
  { importNamespace :: Maybe Namespace,
    importLocation :: Maybe Text
  }
  deriving (Show, Eq)

newtype Include = Include
  { includeLocation :: Text
  }
  deriving (Show, Eq)

data SimpleType
  = AtomicType SimpleRestriction [Annotation]
  | ListType (RefOr SimpleType) [Annotation]
  | UnionType [RefOr SimpleType] [Annotation]
  deriving (Show, Eq)

data ComplexType = ComplexType
  { complexAnnotations :: [Annotation],
    complexContent :: Content,
    complexName :: Maybe QName,
    complexMixed :: Bool
  }
  deriving (Show, Eq)

data Content
  = ContentComplex ComplexContent
  | ContentSimple SimpleContent
  | ContentPlain PlainContent
  deriving (Show, Eq)

-- | Represent content of complexType without simpleContent or complexContent
data PlainContent = PlainContent
  { plainContentModel :: Maybe ModelGroup,
    plainContentAttributes :: [Attribute]
  }
  deriving (Show, Eq)

data ComplexContent
  = ComplexContentExtension ComplexExtension
  | ComplexContentRestriction ComplexRestriction
  deriving (Show, Eq)

data ComplexExtension = ComplexExtension
  { complexExtensionBase :: QName,
    complexExtensionModel :: Maybe ModelGroup,
    complexExtensionAttributes :: [Attribute]
  }
  deriving (Show, Eq)

data ComplexRestriction = ComplexRestriction
  { complexRestrictionBase :: QName,
    complexRestrictionModel :: Maybe ModelGroup,
    complexRestrictionAttributes :: [Attribute]
  }
  deriving (Show, Eq)

data SimpleContent
  = SimpleContentExtension SimpleExtension
  | SimpleContentRestriction SimpleRestriction
  deriving (Show, Eq)

data ChoiceInChild
  = ElementOfChoice Occurs [Element]
  | SequenceOfChoice Occurs [RefOr SequenceInChild]
  | ChoiceInChoice Occurs [RefOr ChoiceInChild]
  deriving (Show, Eq)

data SequenceInChild
  = ElementOfSequence Occurs [Element]
  | ChoiceOfSequence Occurs [RefOr ChoiceInChild]
  | SequenceInSequence Occurs [RefOr SequenceInChild]
  deriving (Show, Eq)

data ModelGroup
  = Sequence [RefOr SequenceInChild]
  | Choice [RefOr ChoiceInChild]
  | All [Element]
  deriving (Show, Eq)

data Attribute
  = RefAttribute AttributeRef
  | InlineAttribute AttributeInline
  | AttributeGroupRef QName
  | AttributeGroupInline QName [Attribute]
  deriving (Show, Eq)

data AttributeInline = AttributeInline
  { attributeInlineName :: QName,
    attributeInlineType :: RefOr SimpleType,
    attributeInlineFixed :: Maybe Text,
    attributeInlineUse :: Use
  }
  deriving (Show, Eq)

data AttributeRef = AttributeRef
  { attributeRefRef :: QName,
    attributeRefUse :: Use
  }
  deriving (Show, Eq)

data Use
  = Optional
  | Prohibited
  | Required
  deriving (Show, Eq)

data SimpleRestriction = SimpleRestriction
  { simpleRestrictionBase :: RefOr SimpleType,
    -- XXX: could simpleRestrictionBase be an inline type? Probably not.
    simpleRestrictionConstraints :: [Constraint]
  }
  deriving (Show, Eq)

data SimpleExtension = SimpleExtension
  { simpleExtensionBase :: QName,
    simpleExtensionAttributes :: [Attribute]
  }
  deriving (Show, Eq)

-- | Reference to a type or an inline one
data RefOr t
  = Ref QName
  | Inline t
  deriving (Show, Eq)

refOr :: (QName -> a) -> (t -> a) -> RefOr t -> a
refOr f g ref = case ref of
  Ref name -> f name
  Inline t -> g t

data Constraint
  = Enumeration Text [Annotation]
  deriving (Show, Eq)

data QName = QName
  { qnNamespace :: Maybe Namespace,
    qnName :: Text
  }
  deriving (Show, Eq, Ord)

-- | XMLSchema namespace, all relevant xml nodes should live in it
schemaNamespace :: Text
schemaNamespace = "http://www.w3.org/2001/XMLSchema"
