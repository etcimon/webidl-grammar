import pegged.grammar;

void main() {
asModule("webidl.grammar","../source/webidl/grammar",
`WebIDL:
  Definitions < (ExtendedAttributeList Definition)* eoi

  Definition <
    CallbackOrInterfaceOrMixin /
    Namespace /
    Partial /
    Dictionary /
    Enum /
    Typedef /
    IncludesStatement

  CallbackOrInterfaceOrMixin <
    ("callback" CallbackRestOrInterface) /
    ("interface" InterfaceOrMixin)

  CallbackRestOrInterface <
    CallbackRest / ("interface" InterfaceRest)

  InterfaceOrMixin < InterfaceRest / MixinRest

  InterfaceRest < Identifier Inheritance '{' InterfaceMembers '}' ';'

  Partial < "partial" PartialDefinition

  PartialDefinition <
    ("interface" PartialInterfaceOrPartialMixin) /
    PartialDictionary /
    Namespace

  PartialInterfaceOrPartialMixin < PartialInterfaceRest / MixinRest

  PartialInterfaceRest < Identifier '{' InterfaceMembers '}' ';'

  InterfaceMembers < InterfaceMember+ / eps

  InterfaceMember < ExtendedAttributeList (Const /
    Operation /
    Stringifier /
    StaticMember /
    Iterable /
    ReadOnlyMember /
    ReadWriteAttribute /
    ReadWriteMaplike /
    ReadWriteSetlike)

  Inheritance < (':' Identifier) / eps

  MixinRest < "mixin" Identifier '{' MixinMembers '}' ';'

  MixinMembers < (ExtendedAttributeList MixinMember) / eps

  MixinMember <
    Const /
    RegularOperation /
    Stringifier /
    (ReadOnly AttributeRest)

  IncludesStatement < Identifier "includes" Identifier ';'

  Const < "const" ConstType Identifier '=' ConstValue ';'

  ConstValue <
    BooleanLiteral /
    FloatLiteral /
    Integer /
    "null"

  BooleanLiteral < "true" / "false"

  FloatLiteral < Float / "-Infinity" / "Infinity" / "NaN"

  ConstType < (PrimitiveType Null) / (Identifier Null)

  ReadOnlyMember < "readonly" ReadOnlyMemberRest

  ReadOnlyMemberRest < AttributeRest / ReadWriteMaplike / ReadWriteSetlike

  ReadWriteAttribute < ("inherit" ReadOnly AttributeRest) / AttributeRest

  AttributeRest < "attribute" TypeWithExtendedAttributes AttributeName ';'

  AttributeName < AttributeNameKeyword / Identifier

  AttributeNameKeyword < "required"

  ReadOnly < "readonly" / eps

  DefaultValue < ConstValue / String / ('[' ']')

  Operation < RegularOperation / SpecialOperation

  RegularOperation < ReturnType OperationRest

  SpecialOperation < Special RegularOperation

  Special < "getter" / "setter" / "deleter"

  OperationRest < OptionalIdentifier '(' ArgumentList ')' ';'

  OptionalIdentifier < Identifier / eps

  ArgumentList < (Argument (',' Argument)*) / eps

  Argument < ExtendedAttributeList ArgumentRest

  ArgumentRest < ("optional" TypeWithExtendedAttributes ArgumentName Default) / (Type Ellipsis ArgumentName)

  ArgumentName < ArgumentNameKeyword | Identifier

  Ellipsis < "..." / eps

  ReturnType < Type / "void"

  Stringifier < "stringifier" StringifierRest

  StringifierRest < (ReadOnly AttributeRest) / (RegularOperation) / ';'

  StaticMember < "static" StaticMemberRest

  StaticMemberRest < (ReadOnly AttributeRest) / RegularOperation

  Iterable < "iterable" '<' TypeWithExtendedAttributes OptionalType '>' ';'

  OptionalType < (',' TypeWithExtendedAttributes) / eps

  ReadWriteMaplike < MaplikeRest

  MaplikeRest < "maplike" '<' TypeWithExtendedAttributes ',' TypeWithExtendedAttributes '>' ';'

  ReadWriteSetlike < SetlikeRest

  SetlikeRest < "setlike" '<' TypeWithExtendedAttributes '>' ';'

  Namespace < "namespace" Identifier '{' NamespaceMembers '}' ';'

  NamespaceMembers < (ExtendedAttributeList NamespaceMember NamespaceMembers) / eps

  NamespaceMember < RegularOperation / ("readonly" AttributeRest)

  Dictionary < "dictionary" Identifier Inheritance '{' DictionaryMembers '}' ';'

  DictionaryMembers < DictionaryMember+ / eps

  DictionaryMember < ExtendedAttributeList DictionaryMemberRest

  DictionaryMemberRest < ("required" TypeWithExtendedAttributes Identifier Default ';') / (Type Identifier Default ';')

  PartialDictionary < "dictionary" Identifier '{' DictionaryMembers '}' ';'

  Default < ('=' DefaultValue) / eps

  Enum < "enum" Identifier '{' EnumValueList '}' ';'

  EnumValueList < String (',' String)*

  CallbackRest < Identifier '=' ReturnType '(' ArgumentList ')' ';'

  Typedef < "typedef" TypeWithExtendedAttributes Identifier ';'

  Type < SingleType / (UnionType Null)

  TypeWithExtendedAttributes < ExtendedAttributeList Type

  SingleType < "any" / NonAnyType

  UnionType < '(' UnionMemberType ("or" UnionMemberType)+ ')'

  UnionMemberType < (ExtendedAttributeList NonAnyType) / (UnionType Null)

  SequenceType < "sequence" '<' TypeWithExtendedAttributes '>' Null

  NonAnyType <
    (SequenceType) /
    ("object" Null) /
    ("symbol" Null) /
    ("Error" Null) /
    ("FrozenArray" '<' TypeWithExtendedAttributes '>' Null) /
    (RecordType Null) /
    PromiseType /
    (PrimitiveType Null) /
    (StringType Null) /
    (Identifier Null) /
    (BufferRelatedType Null)

  PrimitiveType <
    UnsignedIntegerType /
    UnrestrictedFloatType /
    "boolean" /
    "byte" /
    "octet"

  UnrestrictedFloatType < "unrestricted"? FloatType

  FloatType < "float" / "double"

  UnsignedIntegerType < "unsigned"? IntegerType

  IntegerType < "short" / ("long" "long") / "long"

  StringType < "ByteString" / "DOMString" / "USVString"

  PromiseType < "Promise" '<' ReturnType '>'

  RecordType < "record" '<' StringType ',' TypeWithExtendedAttributes '>'

  Null < "?" / eps

  BufferRelatedType <
    "ArrayBuffer" /
    "DataView" /
    "Int8Array" /
    "Int16Array" /
    "Int32Array" /
    "Uint8Array" /
    "Uint16Array" /
    "Uint32Array" /
    "Uint8ClampedArray" /
    "Float32Array" /
    "Float64Array"

  ExtendedAttributeList < ('[' ExtendedAttribute (',' ExtendedAttribute)* ']') / eps

  ExtendedAttributeOuter <
    ('(' ExtendedAttributeInner ')') /
    ('[' ExtendedAttributeInner ']') /
    ('{' ExtendedAttributeInner '}') /
    ExtendedAttribute

  ExtendedAttributeInner <
    (('(' ExtendedAttributeInner ')') /
    ('[' ExtendedAttributeInner ']') /
    ('{' ExtendedAttributeInner '}') /
    OtherOrComma)+

  ExtendedAttribute < ExtendedAttributeArgList / ExtendedAttributeNamedArgList / ExtendedAttributeIdent / ExtendedAttributeIdentList / ExtendedAttributeNoArgs

  Other <
    "ByteString" /
    "DOMString" /
    "FrozenArray" /
    "Infinity" /
    "NaN" /
    "USVString" /
    "any" /
    "boolean" /
    "byte" /
    "double" /
    "false" /
    "float" /
    "long" /
    "null" /
    "object" /
    "octet" /
    "or" /
    "optional" /
    "sequence" /
    "short" /
    "true" /
    "unsigned" /
    "void" /
    Integer /
    Float /
    Identifier /
    String /
    Other2 /
    '-' /
    "-Infinity" /
    '.' /
    "..." /
    ':' /
    ';' /
    '<' /
    '=' /
    '>' /
    '?' /
    ArgumentNameKeyword /
    BufferRelatedType

  OtherOrComma < Other / ','

  IdentifierList < Identifier (',' Identifier)*

  Integer <~ '-'? ([1-9] [0-9]*) / ('0' [Xx] [0-9A-Fa-f]+) / ('0' [0-7]*)

  Float <~ '-'? ((([0-9]+ '.' [0-9]*) / ([0-9]* '.' [0-9]+))(('E' / 'e') ('+' / '-')? [0-9]+)?) / ([0-9]+ ('E' / 'e') ('+' / '-')? [0-9]+)

  Identifier <~ '_'? [A-Za-z] [0-9A-Z_a-z-]*

  String <~ doublequote (!doublequote .)* doublequote

  Whitespace <~ (' ' / '\t' / '\n' / '\r')+

  Comment <~ ("//" (!eol .)*) / ("/*" ((!"*/" .) / eol)* "*/")

  Spacing <: Whitespace / Comment / eps

  Other2 <~ [^\t\n\r 0-9A-Za-z]

  ArgumentNameKeyword <
    "attribute" /
    "callback" /
    "const" /
    "deleter" /
    "dictionary" /
    "enum" /
    "getter" /
    "includes" /
    "inherit" /
    "interface" /
    "iterable" /
    "maplike" /
    "namespace" /
    "partial" /
    "required" /
    "setlike" /
    "setter" /
    "static" /
    "stringifier" /
    "typedef" /
    "unrestricted"

  ExtendedAttributeNoArgs < Identifier

  ExtendedAttributeArgList < Identifier '(' ArgumentList ')'

  ExtendedAttributeIdent < Identifier '=' Identifier

  ExtendedAttributeIdentList < Identifier '=' '(' IdentifierList ')'

  ExtendedAttributeNamedArgList < Identifier '=' Identifier '(' ArgumentList ')'
`);
}
