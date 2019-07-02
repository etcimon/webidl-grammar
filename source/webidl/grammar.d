/++
This module was automatically generated from the following grammar:

WebIDL:
  Definitions < (ExtendedAttributeList Definition)+ eoi

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

  InterfaceOrMixin < MixinRest / InterfaceRest

  InterfaceRest < Identifier Inheritance '{' InterfaceMembers '}' ';'

  Partial < "partial" PartialDefinition

  PartialDefinition <
    ("interface" PartialInterfaceOrPartialMixin) /
    PartialDictionary /
    Namespace

  PartialInterfaceOrPartialMixin < MixinRest / PartialInterfaceRest

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

  MixinMembers < (ExtendedAttributeList MixinMember)+ / eps

  MixinMember <
    Const /
    (ReadOnly AttributeRest) /
    RegularOperation /
    Stringifier

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

  AttributeNameKeyword <- "required" !Identifier

  ReadOnly < "readonly" / eps

  DefaultValue < ConstValue / String / ('[' ']')

  Operation < SpecialOperation / RegularOperation

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

  NamespaceMembers < (ExtendedAttributeList NamespaceMember)+ / eps

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

  SingleType <- ("any" !Identifier) / NonAnyType

  UnionType < '(' UnionMemberType ("or" UnionMemberType)+ ')'

  UnionMemberType < (ExtendedAttributeList NonAnyType) / (UnionType Null)

  SequenceType < "sequence" '<' TypeWithExtendedAttributes '>' Null

  NonAnyType <-
    (SequenceType) /
    ("object" (!Identifier Spacing Null)) /
    ("symbol" (!Identifier Spacing Null)) /
    ("Error" (!Identifier Spacing Null)) /
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

  StringType <- ("ByteString" / "DOMString" / "USVString" / "CSSOMString") !(Identifier)

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

  Whitespace <- :(' ' / '\t' / EndOfLine)+

  EndOfLine <: ('\r' '\n') / '\n'

  Comment <~ (Whitespace? (("//" (!EndOfLine .)*) / ("/*" ((!"*/" .) / EndOfLine)* "*/")) Whitespace?)+

  Spacing <- :(Whitespace / Comment)*

  Other2 <~ [^\t\n\r 0-9A-Za-z]

  ArgumentNameKeyword <-
    ("attribute" /
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
    "unrestricted") !Identifier

  ExtendedAttributeNoArgs < Identifier

  ExtendedAttributeArgList < Identifier '(' ArgumentList ')'

  ExtendedAttributeIdent < Identifier '=' Identifier

  ExtendedAttributeIdentList < Identifier '=' '(' IdentifierList ')'

  ExtendedAttributeNamedArgList < Identifier '=' Identifier '(' ArgumentList ')'


+/
module webidl.grammar;

public import pegged.peg;
import std.algorithm: startsWith;
import std.functional: toDelegate;

struct GenericWebIDL(TParseTree)
{
    import std.functional : toDelegate;
    import pegged.dynamic.grammar;
    static import pegged.peg;
    struct WebIDL
    {
    enum name = "WebIDL";
    static ParseTree delegate(ParseTree)[string] before;
    static ParseTree delegate(ParseTree)[string] after;
    static ParseTree delegate(ParseTree)[string] rules;
    import std.typecons:Tuple, tuple;
    static TParseTree[Tuple!(string, size_t)] memo;
    static this()
    {
        rules["Definitions"] = toDelegate(&Definitions);
        rules["Definition"] = toDelegate(&Definition);
        rules["CallbackOrInterfaceOrMixin"] = toDelegate(&CallbackOrInterfaceOrMixin);
        rules["CallbackRestOrInterface"] = toDelegate(&CallbackRestOrInterface);
        rules["InterfaceOrMixin"] = toDelegate(&InterfaceOrMixin);
        rules["InterfaceRest"] = toDelegate(&InterfaceRest);
        rules["Partial"] = toDelegate(&Partial);
        rules["PartialDefinition"] = toDelegate(&PartialDefinition);
        rules["PartialInterfaceOrPartialMixin"] = toDelegate(&PartialInterfaceOrPartialMixin);
        rules["PartialInterfaceRest"] = toDelegate(&PartialInterfaceRest);
        rules["InterfaceMembers"] = toDelegate(&InterfaceMembers);
        rules["InterfaceMember"] = toDelegate(&InterfaceMember);
        rules["Inheritance"] = toDelegate(&Inheritance);
        rules["MixinRest"] = toDelegate(&MixinRest);
        rules["MixinMembers"] = toDelegate(&MixinMembers);
        rules["MixinMember"] = toDelegate(&MixinMember);
        rules["IncludesStatement"] = toDelegate(&IncludesStatement);
        rules["Const"] = toDelegate(&Const);
        rules["ConstValue"] = toDelegate(&ConstValue);
        rules["BooleanLiteral"] = toDelegate(&BooleanLiteral);
        rules["FloatLiteral"] = toDelegate(&FloatLiteral);
        rules["ConstType"] = toDelegate(&ConstType);
        rules["ReadOnlyMember"] = toDelegate(&ReadOnlyMember);
        rules["ReadOnlyMemberRest"] = toDelegate(&ReadOnlyMemberRest);
        rules["ReadWriteAttribute"] = toDelegate(&ReadWriteAttribute);
        rules["AttributeRest"] = toDelegate(&AttributeRest);
        rules["AttributeName"] = toDelegate(&AttributeName);
        rules["AttributeNameKeyword"] = toDelegate(&AttributeNameKeyword);
        rules["ReadOnly"] = toDelegate(&ReadOnly);
        rules["DefaultValue"] = toDelegate(&DefaultValue);
        rules["Operation"] = toDelegate(&Operation);
        rules["RegularOperation"] = toDelegate(&RegularOperation);
        rules["SpecialOperation"] = toDelegate(&SpecialOperation);
        rules["Special"] = toDelegate(&Special);
        rules["OperationRest"] = toDelegate(&OperationRest);
        rules["OptionalIdentifier"] = toDelegate(&OptionalIdentifier);
        rules["ArgumentList"] = toDelegate(&ArgumentList);
        rules["Argument"] = toDelegate(&Argument);
        rules["ArgumentRest"] = toDelegate(&ArgumentRest);
        rules["ArgumentName"] = toDelegate(&ArgumentName);
        rules["Ellipsis"] = toDelegate(&Ellipsis);
        rules["ReturnType"] = toDelegate(&ReturnType);
        rules["Stringifier"] = toDelegate(&Stringifier);
        rules["StringifierRest"] = toDelegate(&StringifierRest);
        rules["StaticMember"] = toDelegate(&StaticMember);
        rules["StaticMemberRest"] = toDelegate(&StaticMemberRest);
        rules["Iterable"] = toDelegate(&Iterable);
        rules["OptionalType"] = toDelegate(&OptionalType);
        rules["ReadWriteMaplike"] = toDelegate(&ReadWriteMaplike);
        rules["MaplikeRest"] = toDelegate(&MaplikeRest);
        rules["ReadWriteSetlike"] = toDelegate(&ReadWriteSetlike);
        rules["SetlikeRest"] = toDelegate(&SetlikeRest);
        rules["Namespace"] = toDelegate(&Namespace);
        rules["NamespaceMembers"] = toDelegate(&NamespaceMembers);
        rules["NamespaceMember"] = toDelegate(&NamespaceMember);
        rules["Dictionary"] = toDelegate(&Dictionary);
        rules["DictionaryMembers"] = toDelegate(&DictionaryMembers);
        rules["DictionaryMember"] = toDelegate(&DictionaryMember);
        rules["DictionaryMemberRest"] = toDelegate(&DictionaryMemberRest);
        rules["PartialDictionary"] = toDelegate(&PartialDictionary);
        rules["Default"] = toDelegate(&Default);
        rules["Enum"] = toDelegate(&Enum);
        rules["EnumValueList"] = toDelegate(&EnumValueList);
        rules["CallbackRest"] = toDelegate(&CallbackRest);
        rules["Typedef"] = toDelegate(&Typedef);
        rules["Type"] = toDelegate(&Type);
        rules["TypeWithExtendedAttributes"] = toDelegate(&TypeWithExtendedAttributes);
        rules["SingleType"] = toDelegate(&SingleType);
        rules["UnionType"] = toDelegate(&UnionType);
        rules["UnionMemberType"] = toDelegate(&UnionMemberType);
        rules["SequenceType"] = toDelegate(&SequenceType);
        rules["NonAnyType"] = toDelegate(&NonAnyType);
        rules["PrimitiveType"] = toDelegate(&PrimitiveType);
        rules["UnrestrictedFloatType"] = toDelegate(&UnrestrictedFloatType);
        rules["FloatType"] = toDelegate(&FloatType);
        rules["UnsignedIntegerType"] = toDelegate(&UnsignedIntegerType);
        rules["IntegerType"] = toDelegate(&IntegerType);
        rules["StringType"] = toDelegate(&StringType);
        rules["PromiseType"] = toDelegate(&PromiseType);
        rules["RecordType"] = toDelegate(&RecordType);
        rules["Null"] = toDelegate(&Null);
        rules["BufferRelatedType"] = toDelegate(&BufferRelatedType);
        rules["ExtendedAttributeList"] = toDelegate(&ExtendedAttributeList);
        rules["ExtendedAttributeOuter"] = toDelegate(&ExtendedAttributeOuter);
        rules["ExtendedAttributeInner"] = toDelegate(&ExtendedAttributeInner);
        rules["ExtendedAttribute"] = toDelegate(&ExtendedAttribute);
        rules["Other"] = toDelegate(&Other);
        rules["OtherOrComma"] = toDelegate(&OtherOrComma);
        rules["IdentifierList"] = toDelegate(&IdentifierList);
        rules["Integer"] = toDelegate(&Integer);
        rules["Float"] = toDelegate(&Float);
        rules["Identifier"] = toDelegate(&Identifier);
        rules["String"] = toDelegate(&String);
        rules["Whitespace"] = toDelegate(&Whitespace);
        rules["EndOfLine"] = toDelegate(&EndOfLine);
        rules["Comment"] = toDelegate(&Comment);
        rules["Spacing"] = toDelegate(&Spacing);
    }

    template hooked(alias r, string name)
    {
        static ParseTree hooked(ParseTree p)
        {
            ParseTree result;

            if (name in before)
            {
                result = before[name](p);
                if (result.successful)
                    return result;
            }

            result = r(p);
            if (result.successful || name !in after)
                return result;

            result = after[name](p);
            return result;
        }

        static ParseTree hooked(string input)
        {
            return hooked!(r, name)(ParseTree("",false,[],input));
        }
    }

    static void addRuleBefore(string parentRule, string ruleSyntax)
    {
        // enum name is the current grammar name
        DynamicGrammar dg = pegged.dynamic.grammar.grammar(name ~ ": " ~ ruleSyntax, rules);
        foreach(ruleName,rule; dg.rules)
            if (ruleName != "Spacing") // Keep the local Spacing rule, do not overwrite it
                rules[ruleName] = rule;
        before[parentRule] = rules[dg.startingRule];
    }

    static void addRuleAfter(string parentRule, string ruleSyntax)
    {
        // enum name is the current grammar named
        DynamicGrammar dg = pegged.dynamic.grammar.grammar(name ~ ": " ~ ruleSyntax, rules);
        foreach(name,rule; dg.rules)
        {
            if (name != "Spacing")
                rules[name] = rule;
        }
        after[parentRule] = rules[dg.startingRule];
    }

    static bool isRule(string s)
    {
		import std.algorithm : startsWith;
        return s.startsWith("WebIDL.");
    }
    mixin decimateTree;

    static TParseTree Definitions(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ExtendedAttributeList, Spacing), pegged.peg.wrapAround!(Spacing, Definition, Spacing)), Spacing)), pegged.peg.wrapAround!(Spacing, eoi, Spacing)), "WebIDL.Definitions")(p);
        }
        else
        {
            if (auto m = tuple(`Definitions`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ExtendedAttributeList, Spacing), pegged.peg.wrapAround!(Spacing, Definition, Spacing)), Spacing)), pegged.peg.wrapAround!(Spacing, eoi, Spacing)), "WebIDL.Definitions"), "Definitions")(p);
                memo[tuple(`Definitions`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Definitions(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ExtendedAttributeList, Spacing), pegged.peg.wrapAround!(Spacing, Definition, Spacing)), Spacing)), pegged.peg.wrapAround!(Spacing, eoi, Spacing)), "WebIDL.Definitions")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ExtendedAttributeList, Spacing), pegged.peg.wrapAround!(Spacing, Definition, Spacing)), Spacing)), pegged.peg.wrapAround!(Spacing, eoi, Spacing)), "WebIDL.Definitions"), "Definitions")(TParseTree("", false,[], s));
        }
    }
    static string Definitions(GetName g)
    {
        return "WebIDL.Definitions";
    }

    static TParseTree Definition(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, CallbackOrInterfaceOrMixin, Spacing), pegged.peg.wrapAround!(Spacing, Namespace, Spacing), pegged.peg.wrapAround!(Spacing, Partial, Spacing), pegged.peg.wrapAround!(Spacing, Dictionary, Spacing), pegged.peg.wrapAround!(Spacing, Enum, Spacing), pegged.peg.wrapAround!(Spacing, Typedef, Spacing), pegged.peg.wrapAround!(Spacing, IncludesStatement, Spacing)), "WebIDL.Definition")(p);
        }
        else
        {
            if (auto m = tuple(`Definition`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, CallbackOrInterfaceOrMixin, Spacing), pegged.peg.wrapAround!(Spacing, Namespace, Spacing), pegged.peg.wrapAround!(Spacing, Partial, Spacing), pegged.peg.wrapAround!(Spacing, Dictionary, Spacing), pegged.peg.wrapAround!(Spacing, Enum, Spacing), pegged.peg.wrapAround!(Spacing, Typedef, Spacing), pegged.peg.wrapAround!(Spacing, IncludesStatement, Spacing)), "WebIDL.Definition"), "Definition")(p);
                memo[tuple(`Definition`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Definition(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, CallbackOrInterfaceOrMixin, Spacing), pegged.peg.wrapAround!(Spacing, Namespace, Spacing), pegged.peg.wrapAround!(Spacing, Partial, Spacing), pegged.peg.wrapAround!(Spacing, Dictionary, Spacing), pegged.peg.wrapAround!(Spacing, Enum, Spacing), pegged.peg.wrapAround!(Spacing, Typedef, Spacing), pegged.peg.wrapAround!(Spacing, IncludesStatement, Spacing)), "WebIDL.Definition")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, CallbackOrInterfaceOrMixin, Spacing), pegged.peg.wrapAround!(Spacing, Namespace, Spacing), pegged.peg.wrapAround!(Spacing, Partial, Spacing), pegged.peg.wrapAround!(Spacing, Dictionary, Spacing), pegged.peg.wrapAround!(Spacing, Enum, Spacing), pegged.peg.wrapAround!(Spacing, Typedef, Spacing), pegged.peg.wrapAround!(Spacing, IncludesStatement, Spacing)), "WebIDL.Definition"), "Definition")(TParseTree("", false,[], s));
        }
    }
    static string Definition(GetName g)
    {
        return "WebIDL.Definition";
    }

    static TParseTree CallbackOrInterfaceOrMixin(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("callback"), Spacing), pegged.peg.wrapAround!(Spacing, CallbackRestOrInterface, Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("interface"), Spacing), pegged.peg.wrapAround!(Spacing, InterfaceOrMixin, Spacing)), Spacing)), "WebIDL.CallbackOrInterfaceOrMixin")(p);
        }
        else
        {
            if (auto m = tuple(`CallbackOrInterfaceOrMixin`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("callback"), Spacing), pegged.peg.wrapAround!(Spacing, CallbackRestOrInterface, Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("interface"), Spacing), pegged.peg.wrapAround!(Spacing, InterfaceOrMixin, Spacing)), Spacing)), "WebIDL.CallbackOrInterfaceOrMixin"), "CallbackOrInterfaceOrMixin")(p);
                memo[tuple(`CallbackOrInterfaceOrMixin`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree CallbackOrInterfaceOrMixin(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("callback"), Spacing), pegged.peg.wrapAround!(Spacing, CallbackRestOrInterface, Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("interface"), Spacing), pegged.peg.wrapAround!(Spacing, InterfaceOrMixin, Spacing)), Spacing)), "WebIDL.CallbackOrInterfaceOrMixin")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("callback"), Spacing), pegged.peg.wrapAround!(Spacing, CallbackRestOrInterface, Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("interface"), Spacing), pegged.peg.wrapAround!(Spacing, InterfaceOrMixin, Spacing)), Spacing)), "WebIDL.CallbackOrInterfaceOrMixin"), "CallbackOrInterfaceOrMixin")(TParseTree("", false,[], s));
        }
    }
    static string CallbackOrInterfaceOrMixin(GetName g)
    {
        return "WebIDL.CallbackOrInterfaceOrMixin";
    }

    static TParseTree CallbackRestOrInterface(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, CallbackRest, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("interface"), Spacing), pegged.peg.wrapAround!(Spacing, InterfaceRest, Spacing)), Spacing)), "WebIDL.CallbackRestOrInterface")(p);
        }
        else
        {
            if (auto m = tuple(`CallbackRestOrInterface`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, CallbackRest, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("interface"), Spacing), pegged.peg.wrapAround!(Spacing, InterfaceRest, Spacing)), Spacing)), "WebIDL.CallbackRestOrInterface"), "CallbackRestOrInterface")(p);
                memo[tuple(`CallbackRestOrInterface`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree CallbackRestOrInterface(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, CallbackRest, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("interface"), Spacing), pegged.peg.wrapAround!(Spacing, InterfaceRest, Spacing)), Spacing)), "WebIDL.CallbackRestOrInterface")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, CallbackRest, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("interface"), Spacing), pegged.peg.wrapAround!(Spacing, InterfaceRest, Spacing)), Spacing)), "WebIDL.CallbackRestOrInterface"), "CallbackRestOrInterface")(TParseTree("", false,[], s));
        }
    }
    static string CallbackRestOrInterface(GetName g)
    {
        return "WebIDL.CallbackRestOrInterface";
    }

    static TParseTree InterfaceOrMixin(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, MixinRest, Spacing), pegged.peg.wrapAround!(Spacing, InterfaceRest, Spacing)), "WebIDL.InterfaceOrMixin")(p);
        }
        else
        {
            if (auto m = tuple(`InterfaceOrMixin`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, MixinRest, Spacing), pegged.peg.wrapAround!(Spacing, InterfaceRest, Spacing)), "WebIDL.InterfaceOrMixin"), "InterfaceOrMixin")(p);
                memo[tuple(`InterfaceOrMixin`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree InterfaceOrMixin(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, MixinRest, Spacing), pegged.peg.wrapAround!(Spacing, InterfaceRest, Spacing)), "WebIDL.InterfaceOrMixin")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, MixinRest, Spacing), pegged.peg.wrapAround!(Spacing, InterfaceRest, Spacing)), "WebIDL.InterfaceOrMixin"), "InterfaceOrMixin")(TParseTree("", false,[], s));
        }
    }
    static string InterfaceOrMixin(GetName g)
    {
        return "WebIDL.InterfaceOrMixin";
    }

    static TParseTree InterfaceRest(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, Inheritance, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, InterfaceMembers, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "WebIDL.InterfaceRest")(p);
        }
        else
        {
            if (auto m = tuple(`InterfaceRest`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, Inheritance, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, InterfaceMembers, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "WebIDL.InterfaceRest"), "InterfaceRest")(p);
                memo[tuple(`InterfaceRest`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree InterfaceRest(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, Inheritance, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, InterfaceMembers, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "WebIDL.InterfaceRest")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, Inheritance, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, InterfaceMembers, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "WebIDL.InterfaceRest"), "InterfaceRest")(TParseTree("", false,[], s));
        }
    }
    static string InterfaceRest(GetName g)
    {
        return "WebIDL.InterfaceRest";
    }

    static TParseTree Partial(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("partial"), Spacing), pegged.peg.wrapAround!(Spacing, PartialDefinition, Spacing)), "WebIDL.Partial")(p);
        }
        else
        {
            if (auto m = tuple(`Partial`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("partial"), Spacing), pegged.peg.wrapAround!(Spacing, PartialDefinition, Spacing)), "WebIDL.Partial"), "Partial")(p);
                memo[tuple(`Partial`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Partial(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("partial"), Spacing), pegged.peg.wrapAround!(Spacing, PartialDefinition, Spacing)), "WebIDL.Partial")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("partial"), Spacing), pegged.peg.wrapAround!(Spacing, PartialDefinition, Spacing)), "WebIDL.Partial"), "Partial")(TParseTree("", false,[], s));
        }
    }
    static string Partial(GetName g)
    {
        return "WebIDL.Partial";
    }

    static TParseTree PartialDefinition(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("interface"), Spacing), pegged.peg.wrapAround!(Spacing, PartialInterfaceOrPartialMixin, Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, PartialDictionary, Spacing), pegged.peg.wrapAround!(Spacing, Namespace, Spacing)), "WebIDL.PartialDefinition")(p);
        }
        else
        {
            if (auto m = tuple(`PartialDefinition`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("interface"), Spacing), pegged.peg.wrapAround!(Spacing, PartialInterfaceOrPartialMixin, Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, PartialDictionary, Spacing), pegged.peg.wrapAround!(Spacing, Namespace, Spacing)), "WebIDL.PartialDefinition"), "PartialDefinition")(p);
                memo[tuple(`PartialDefinition`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree PartialDefinition(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("interface"), Spacing), pegged.peg.wrapAround!(Spacing, PartialInterfaceOrPartialMixin, Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, PartialDictionary, Spacing), pegged.peg.wrapAround!(Spacing, Namespace, Spacing)), "WebIDL.PartialDefinition")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("interface"), Spacing), pegged.peg.wrapAround!(Spacing, PartialInterfaceOrPartialMixin, Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, PartialDictionary, Spacing), pegged.peg.wrapAround!(Spacing, Namespace, Spacing)), "WebIDL.PartialDefinition"), "PartialDefinition")(TParseTree("", false,[], s));
        }
    }
    static string PartialDefinition(GetName g)
    {
        return "WebIDL.PartialDefinition";
    }

    static TParseTree PartialInterfaceOrPartialMixin(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, MixinRest, Spacing), pegged.peg.wrapAround!(Spacing, PartialInterfaceRest, Spacing)), "WebIDL.PartialInterfaceOrPartialMixin")(p);
        }
        else
        {
            if (auto m = tuple(`PartialInterfaceOrPartialMixin`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, MixinRest, Spacing), pegged.peg.wrapAround!(Spacing, PartialInterfaceRest, Spacing)), "WebIDL.PartialInterfaceOrPartialMixin"), "PartialInterfaceOrPartialMixin")(p);
                memo[tuple(`PartialInterfaceOrPartialMixin`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree PartialInterfaceOrPartialMixin(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, MixinRest, Spacing), pegged.peg.wrapAround!(Spacing, PartialInterfaceRest, Spacing)), "WebIDL.PartialInterfaceOrPartialMixin")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, MixinRest, Spacing), pegged.peg.wrapAround!(Spacing, PartialInterfaceRest, Spacing)), "WebIDL.PartialInterfaceOrPartialMixin"), "PartialInterfaceOrPartialMixin")(TParseTree("", false,[], s));
        }
    }
    static string PartialInterfaceOrPartialMixin(GetName g)
    {
        return "WebIDL.PartialInterfaceOrPartialMixin";
    }

    static TParseTree PartialInterfaceRest(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, InterfaceMembers, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "WebIDL.PartialInterfaceRest")(p);
        }
        else
        {
            if (auto m = tuple(`PartialInterfaceRest`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, InterfaceMembers, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "WebIDL.PartialInterfaceRest"), "PartialInterfaceRest")(p);
                memo[tuple(`PartialInterfaceRest`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree PartialInterfaceRest(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, InterfaceMembers, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "WebIDL.PartialInterfaceRest")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, InterfaceMembers, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "WebIDL.PartialInterfaceRest"), "PartialInterfaceRest")(TParseTree("", false,[], s));
        }
    }
    static string PartialInterfaceRest(GetName g)
    {
        return "WebIDL.PartialInterfaceRest";
    }

    static TParseTree InterfaceMembers(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, InterfaceMember, Spacing)), pegged.peg.wrapAround!(Spacing, eps, Spacing)), "WebIDL.InterfaceMembers")(p);
        }
        else
        {
            if (auto m = tuple(`InterfaceMembers`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, InterfaceMember, Spacing)), pegged.peg.wrapAround!(Spacing, eps, Spacing)), "WebIDL.InterfaceMembers"), "InterfaceMembers")(p);
                memo[tuple(`InterfaceMembers`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree InterfaceMembers(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, InterfaceMember, Spacing)), pegged.peg.wrapAround!(Spacing, eps, Spacing)), "WebIDL.InterfaceMembers")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, InterfaceMember, Spacing)), pegged.peg.wrapAround!(Spacing, eps, Spacing)), "WebIDL.InterfaceMembers"), "InterfaceMembers")(TParseTree("", false,[], s));
        }
    }
    static string InterfaceMembers(GetName g)
    {
        return "WebIDL.InterfaceMembers";
    }

    static TParseTree InterfaceMember(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ExtendedAttributeList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Const, Spacing), pegged.peg.wrapAround!(Spacing, Operation, Spacing), pegged.peg.wrapAround!(Spacing, Stringifier, Spacing), pegged.peg.wrapAround!(Spacing, StaticMember, Spacing), pegged.peg.wrapAround!(Spacing, Iterable, Spacing), pegged.peg.wrapAround!(Spacing, ReadOnlyMember, Spacing), pegged.peg.wrapAround!(Spacing, ReadWriteAttribute, Spacing), pegged.peg.wrapAround!(Spacing, ReadWriteMaplike, Spacing), pegged.peg.wrapAround!(Spacing, ReadWriteSetlike, Spacing)), Spacing)), "WebIDL.InterfaceMember")(p);
        }
        else
        {
            if (auto m = tuple(`InterfaceMember`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ExtendedAttributeList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Const, Spacing), pegged.peg.wrapAround!(Spacing, Operation, Spacing), pegged.peg.wrapAround!(Spacing, Stringifier, Spacing), pegged.peg.wrapAround!(Spacing, StaticMember, Spacing), pegged.peg.wrapAround!(Spacing, Iterable, Spacing), pegged.peg.wrapAround!(Spacing, ReadOnlyMember, Spacing), pegged.peg.wrapAround!(Spacing, ReadWriteAttribute, Spacing), pegged.peg.wrapAround!(Spacing, ReadWriteMaplike, Spacing), pegged.peg.wrapAround!(Spacing, ReadWriteSetlike, Spacing)), Spacing)), "WebIDL.InterfaceMember"), "InterfaceMember")(p);
                memo[tuple(`InterfaceMember`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree InterfaceMember(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ExtendedAttributeList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Const, Spacing), pegged.peg.wrapAround!(Spacing, Operation, Spacing), pegged.peg.wrapAround!(Spacing, Stringifier, Spacing), pegged.peg.wrapAround!(Spacing, StaticMember, Spacing), pegged.peg.wrapAround!(Spacing, Iterable, Spacing), pegged.peg.wrapAround!(Spacing, ReadOnlyMember, Spacing), pegged.peg.wrapAround!(Spacing, ReadWriteAttribute, Spacing), pegged.peg.wrapAround!(Spacing, ReadWriteMaplike, Spacing), pegged.peg.wrapAround!(Spacing, ReadWriteSetlike, Spacing)), Spacing)), "WebIDL.InterfaceMember")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ExtendedAttributeList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Const, Spacing), pegged.peg.wrapAround!(Spacing, Operation, Spacing), pegged.peg.wrapAround!(Spacing, Stringifier, Spacing), pegged.peg.wrapAround!(Spacing, StaticMember, Spacing), pegged.peg.wrapAround!(Spacing, Iterable, Spacing), pegged.peg.wrapAround!(Spacing, ReadOnlyMember, Spacing), pegged.peg.wrapAround!(Spacing, ReadWriteAttribute, Spacing), pegged.peg.wrapAround!(Spacing, ReadWriteMaplike, Spacing), pegged.peg.wrapAround!(Spacing, ReadWriteSetlike, Spacing)), Spacing)), "WebIDL.InterfaceMember"), "InterfaceMember")(TParseTree("", false,[], s));
        }
    }
    static string InterfaceMember(GetName g)
    {
        return "WebIDL.InterfaceMember";
    }

    static TParseTree Inheritance(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, eps, Spacing)), "WebIDL.Inheritance")(p);
        }
        else
        {
            if (auto m = tuple(`Inheritance`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, eps, Spacing)), "WebIDL.Inheritance"), "Inheritance")(p);
                memo[tuple(`Inheritance`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Inheritance(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, eps, Spacing)), "WebIDL.Inheritance")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, eps, Spacing)), "WebIDL.Inheritance"), "Inheritance")(TParseTree("", false,[], s));
        }
    }
    static string Inheritance(GetName g)
    {
        return "WebIDL.Inheritance";
    }

    static TParseTree MixinRest(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("mixin"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, MixinMembers, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "WebIDL.MixinRest")(p);
        }
        else
        {
            if (auto m = tuple(`MixinRest`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("mixin"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, MixinMembers, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "WebIDL.MixinRest"), "MixinRest")(p);
                memo[tuple(`MixinRest`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree MixinRest(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("mixin"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, MixinMembers, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "WebIDL.MixinRest")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("mixin"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, MixinMembers, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "WebIDL.MixinRest"), "MixinRest")(TParseTree("", false,[], s));
        }
    }
    static string MixinRest(GetName g)
    {
        return "WebIDL.MixinRest";
    }

    static TParseTree MixinMembers(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ExtendedAttributeList, Spacing), pegged.peg.wrapAround!(Spacing, MixinMember, Spacing)), Spacing)), pegged.peg.wrapAround!(Spacing, eps, Spacing)), "WebIDL.MixinMembers")(p);
        }
        else
        {
            if (auto m = tuple(`MixinMembers`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ExtendedAttributeList, Spacing), pegged.peg.wrapAround!(Spacing, MixinMember, Spacing)), Spacing)), pegged.peg.wrapAround!(Spacing, eps, Spacing)), "WebIDL.MixinMembers"), "MixinMembers")(p);
                memo[tuple(`MixinMembers`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree MixinMembers(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ExtendedAttributeList, Spacing), pegged.peg.wrapAround!(Spacing, MixinMember, Spacing)), Spacing)), pegged.peg.wrapAround!(Spacing, eps, Spacing)), "WebIDL.MixinMembers")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ExtendedAttributeList, Spacing), pegged.peg.wrapAround!(Spacing, MixinMember, Spacing)), Spacing)), pegged.peg.wrapAround!(Spacing, eps, Spacing)), "WebIDL.MixinMembers"), "MixinMembers")(TParseTree("", false,[], s));
        }
    }
    static string MixinMembers(GetName g)
    {
        return "WebIDL.MixinMembers";
    }

    static TParseTree MixinMember(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Const, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ReadOnly, Spacing), pegged.peg.wrapAround!(Spacing, AttributeRest, Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, RegularOperation, Spacing), pegged.peg.wrapAround!(Spacing, Stringifier, Spacing)), "WebIDL.MixinMember")(p);
        }
        else
        {
            if (auto m = tuple(`MixinMember`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Const, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ReadOnly, Spacing), pegged.peg.wrapAround!(Spacing, AttributeRest, Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, RegularOperation, Spacing), pegged.peg.wrapAround!(Spacing, Stringifier, Spacing)), "WebIDL.MixinMember"), "MixinMember")(p);
                memo[tuple(`MixinMember`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree MixinMember(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Const, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ReadOnly, Spacing), pegged.peg.wrapAround!(Spacing, AttributeRest, Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, RegularOperation, Spacing), pegged.peg.wrapAround!(Spacing, Stringifier, Spacing)), "WebIDL.MixinMember")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Const, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ReadOnly, Spacing), pegged.peg.wrapAround!(Spacing, AttributeRest, Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, RegularOperation, Spacing), pegged.peg.wrapAround!(Spacing, Stringifier, Spacing)), "WebIDL.MixinMember"), "MixinMember")(TParseTree("", false,[], s));
        }
    }
    static string MixinMember(GetName g)
    {
        return "WebIDL.MixinMember";
    }

    static TParseTree IncludesStatement(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("includes"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "WebIDL.IncludesStatement")(p);
        }
        else
        {
            if (auto m = tuple(`IncludesStatement`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("includes"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "WebIDL.IncludesStatement"), "IncludesStatement")(p);
                memo[tuple(`IncludesStatement`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree IncludesStatement(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("includes"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "WebIDL.IncludesStatement")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("includes"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "WebIDL.IncludesStatement"), "IncludesStatement")(TParseTree("", false,[], s));
        }
    }
    static string IncludesStatement(GetName g)
    {
        return "WebIDL.IncludesStatement";
    }

    static TParseTree Const(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("const"), Spacing), pegged.peg.wrapAround!(Spacing, ConstType, Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, ConstValue, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "WebIDL.Const")(p);
        }
        else
        {
            if (auto m = tuple(`Const`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("const"), Spacing), pegged.peg.wrapAround!(Spacing, ConstType, Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, ConstValue, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "WebIDL.Const"), "Const")(p);
                memo[tuple(`Const`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Const(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("const"), Spacing), pegged.peg.wrapAround!(Spacing, ConstType, Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, ConstValue, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "WebIDL.Const")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("const"), Spacing), pegged.peg.wrapAround!(Spacing, ConstType, Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, ConstValue, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "WebIDL.Const"), "Const")(TParseTree("", false,[], s));
        }
    }
    static string Const(GetName g)
    {
        return "WebIDL.Const";
    }

    static TParseTree ConstValue(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, BooleanLiteral, Spacing), pegged.peg.wrapAround!(Spacing, FloatLiteral, Spacing), pegged.peg.wrapAround!(Spacing, Integer, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("null"), Spacing)), "WebIDL.ConstValue")(p);
        }
        else
        {
            if (auto m = tuple(`ConstValue`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, BooleanLiteral, Spacing), pegged.peg.wrapAround!(Spacing, FloatLiteral, Spacing), pegged.peg.wrapAround!(Spacing, Integer, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("null"), Spacing)), "WebIDL.ConstValue"), "ConstValue")(p);
                memo[tuple(`ConstValue`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ConstValue(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, BooleanLiteral, Spacing), pegged.peg.wrapAround!(Spacing, FloatLiteral, Spacing), pegged.peg.wrapAround!(Spacing, Integer, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("null"), Spacing)), "WebIDL.ConstValue")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, BooleanLiteral, Spacing), pegged.peg.wrapAround!(Spacing, FloatLiteral, Spacing), pegged.peg.wrapAround!(Spacing, Integer, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("null"), Spacing)), "WebIDL.ConstValue"), "ConstValue")(TParseTree("", false,[], s));
        }
    }
    static string ConstValue(GetName g)
    {
        return "WebIDL.ConstValue";
    }

    static TParseTree BooleanLiteral(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("true"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("false"), Spacing)), "WebIDL.BooleanLiteral")(p);
        }
        else
        {
            if (auto m = tuple(`BooleanLiteral`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("true"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("false"), Spacing)), "WebIDL.BooleanLiteral"), "BooleanLiteral")(p);
                memo[tuple(`BooleanLiteral`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree BooleanLiteral(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("true"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("false"), Spacing)), "WebIDL.BooleanLiteral")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("true"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("false"), Spacing)), "WebIDL.BooleanLiteral"), "BooleanLiteral")(TParseTree("", false,[], s));
        }
    }
    static string BooleanLiteral(GetName g)
    {
        return "WebIDL.BooleanLiteral";
    }

    static TParseTree FloatLiteral(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Float, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("-Infinity"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("Infinity"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("NaN"), Spacing)), "WebIDL.FloatLiteral")(p);
        }
        else
        {
            if (auto m = tuple(`FloatLiteral`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Float, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("-Infinity"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("Infinity"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("NaN"), Spacing)), "WebIDL.FloatLiteral"), "FloatLiteral")(p);
                memo[tuple(`FloatLiteral`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree FloatLiteral(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Float, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("-Infinity"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("Infinity"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("NaN"), Spacing)), "WebIDL.FloatLiteral")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Float, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("-Infinity"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("Infinity"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("NaN"), Spacing)), "WebIDL.FloatLiteral"), "FloatLiteral")(TParseTree("", false,[], s));
        }
    }
    static string FloatLiteral(GetName g)
    {
        return "WebIDL.FloatLiteral";
    }

    static TParseTree ConstType(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, PrimitiveType, Spacing), pegged.peg.wrapAround!(Spacing, Null, Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, Null, Spacing)), Spacing)), "WebIDL.ConstType")(p);
        }
        else
        {
            if (auto m = tuple(`ConstType`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, PrimitiveType, Spacing), pegged.peg.wrapAround!(Spacing, Null, Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, Null, Spacing)), Spacing)), "WebIDL.ConstType"), "ConstType")(p);
                memo[tuple(`ConstType`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ConstType(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, PrimitiveType, Spacing), pegged.peg.wrapAround!(Spacing, Null, Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, Null, Spacing)), Spacing)), "WebIDL.ConstType")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, PrimitiveType, Spacing), pegged.peg.wrapAround!(Spacing, Null, Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, Null, Spacing)), Spacing)), "WebIDL.ConstType"), "ConstType")(TParseTree("", false,[], s));
        }
    }
    static string ConstType(GetName g)
    {
        return "WebIDL.ConstType";
    }

    static TParseTree ReadOnlyMember(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("readonly"), Spacing), pegged.peg.wrapAround!(Spacing, ReadOnlyMemberRest, Spacing)), "WebIDL.ReadOnlyMember")(p);
        }
        else
        {
            if (auto m = tuple(`ReadOnlyMember`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("readonly"), Spacing), pegged.peg.wrapAround!(Spacing, ReadOnlyMemberRest, Spacing)), "WebIDL.ReadOnlyMember"), "ReadOnlyMember")(p);
                memo[tuple(`ReadOnlyMember`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ReadOnlyMember(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("readonly"), Spacing), pegged.peg.wrapAround!(Spacing, ReadOnlyMemberRest, Spacing)), "WebIDL.ReadOnlyMember")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("readonly"), Spacing), pegged.peg.wrapAround!(Spacing, ReadOnlyMemberRest, Spacing)), "WebIDL.ReadOnlyMember"), "ReadOnlyMember")(TParseTree("", false,[], s));
        }
    }
    static string ReadOnlyMember(GetName g)
    {
        return "WebIDL.ReadOnlyMember";
    }

    static TParseTree ReadOnlyMemberRest(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, AttributeRest, Spacing), pegged.peg.wrapAround!(Spacing, ReadWriteMaplike, Spacing), pegged.peg.wrapAround!(Spacing, ReadWriteSetlike, Spacing)), "WebIDL.ReadOnlyMemberRest")(p);
        }
        else
        {
            if (auto m = tuple(`ReadOnlyMemberRest`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, AttributeRest, Spacing), pegged.peg.wrapAround!(Spacing, ReadWriteMaplike, Spacing), pegged.peg.wrapAround!(Spacing, ReadWriteSetlike, Spacing)), "WebIDL.ReadOnlyMemberRest"), "ReadOnlyMemberRest")(p);
                memo[tuple(`ReadOnlyMemberRest`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ReadOnlyMemberRest(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, AttributeRest, Spacing), pegged.peg.wrapAround!(Spacing, ReadWriteMaplike, Spacing), pegged.peg.wrapAround!(Spacing, ReadWriteSetlike, Spacing)), "WebIDL.ReadOnlyMemberRest")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, AttributeRest, Spacing), pegged.peg.wrapAround!(Spacing, ReadWriteMaplike, Spacing), pegged.peg.wrapAround!(Spacing, ReadWriteSetlike, Spacing)), "WebIDL.ReadOnlyMemberRest"), "ReadOnlyMemberRest")(TParseTree("", false,[], s));
        }
    }
    static string ReadOnlyMemberRest(GetName g)
    {
        return "WebIDL.ReadOnlyMemberRest";
    }

    static TParseTree ReadWriteAttribute(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("inherit"), Spacing), pegged.peg.wrapAround!(Spacing, ReadOnly, Spacing), pegged.peg.wrapAround!(Spacing, AttributeRest, Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, AttributeRest, Spacing)), "WebIDL.ReadWriteAttribute")(p);
        }
        else
        {
            if (auto m = tuple(`ReadWriteAttribute`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("inherit"), Spacing), pegged.peg.wrapAround!(Spacing, ReadOnly, Spacing), pegged.peg.wrapAround!(Spacing, AttributeRest, Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, AttributeRest, Spacing)), "WebIDL.ReadWriteAttribute"), "ReadWriteAttribute")(p);
                memo[tuple(`ReadWriteAttribute`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ReadWriteAttribute(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("inherit"), Spacing), pegged.peg.wrapAround!(Spacing, ReadOnly, Spacing), pegged.peg.wrapAround!(Spacing, AttributeRest, Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, AttributeRest, Spacing)), "WebIDL.ReadWriteAttribute")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("inherit"), Spacing), pegged.peg.wrapAround!(Spacing, ReadOnly, Spacing), pegged.peg.wrapAround!(Spacing, AttributeRest, Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, AttributeRest, Spacing)), "WebIDL.ReadWriteAttribute"), "ReadWriteAttribute")(TParseTree("", false,[], s));
        }
    }
    static string ReadWriteAttribute(GetName g)
    {
        return "WebIDL.ReadWriteAttribute";
    }

    static TParseTree AttributeRest(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("attribute"), Spacing), pegged.peg.wrapAround!(Spacing, TypeWithExtendedAttributes, Spacing), pegged.peg.wrapAround!(Spacing, AttributeName, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "WebIDL.AttributeRest")(p);
        }
        else
        {
            if (auto m = tuple(`AttributeRest`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("attribute"), Spacing), pegged.peg.wrapAround!(Spacing, TypeWithExtendedAttributes, Spacing), pegged.peg.wrapAround!(Spacing, AttributeName, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "WebIDL.AttributeRest"), "AttributeRest")(p);
                memo[tuple(`AttributeRest`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree AttributeRest(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("attribute"), Spacing), pegged.peg.wrapAround!(Spacing, TypeWithExtendedAttributes, Spacing), pegged.peg.wrapAround!(Spacing, AttributeName, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "WebIDL.AttributeRest")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("attribute"), Spacing), pegged.peg.wrapAround!(Spacing, TypeWithExtendedAttributes, Spacing), pegged.peg.wrapAround!(Spacing, AttributeName, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "WebIDL.AttributeRest"), "AttributeRest")(TParseTree("", false,[], s));
        }
    }
    static string AttributeRest(GetName g)
    {
        return "WebIDL.AttributeRest";
    }

    static TParseTree AttributeName(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, AttributeNameKeyword, Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), "WebIDL.AttributeName")(p);
        }
        else
        {
            if (auto m = tuple(`AttributeName`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, AttributeNameKeyword, Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), "WebIDL.AttributeName"), "AttributeName")(p);
                memo[tuple(`AttributeName`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree AttributeName(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, AttributeNameKeyword, Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), "WebIDL.AttributeName")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, AttributeNameKeyword, Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), "WebIDL.AttributeName"), "AttributeName")(TParseTree("", false,[], s));
        }
    }
    static string AttributeName(GetName g)
    {
        return "WebIDL.AttributeName";
    }

    static TParseTree AttributeNameKeyword(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("required"), pegged.peg.negLookahead!(Identifier)), "WebIDL.AttributeNameKeyword")(p);
        }
        else
        {
            if (auto m = tuple(`AttributeNameKeyword`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("required"), pegged.peg.negLookahead!(Identifier)), "WebIDL.AttributeNameKeyword"), "AttributeNameKeyword")(p);
                memo[tuple(`AttributeNameKeyword`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree AttributeNameKeyword(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("required"), pegged.peg.negLookahead!(Identifier)), "WebIDL.AttributeNameKeyword")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("required"), pegged.peg.negLookahead!(Identifier)), "WebIDL.AttributeNameKeyword"), "AttributeNameKeyword")(TParseTree("", false,[], s));
        }
    }
    static string AttributeNameKeyword(GetName g)
    {
        return "WebIDL.AttributeNameKeyword";
    }

    static TParseTree ReadOnly(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("readonly"), Spacing), pegged.peg.wrapAround!(Spacing, eps, Spacing)), "WebIDL.ReadOnly")(p);
        }
        else
        {
            if (auto m = tuple(`ReadOnly`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("readonly"), Spacing), pegged.peg.wrapAround!(Spacing, eps, Spacing)), "WebIDL.ReadOnly"), "ReadOnly")(p);
                memo[tuple(`ReadOnly`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ReadOnly(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("readonly"), Spacing), pegged.peg.wrapAround!(Spacing, eps, Spacing)), "WebIDL.ReadOnly")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("readonly"), Spacing), pegged.peg.wrapAround!(Spacing, eps, Spacing)), "WebIDL.ReadOnly"), "ReadOnly")(TParseTree("", false,[], s));
        }
    }
    static string ReadOnly(GetName g)
    {
        return "WebIDL.ReadOnly";
    }

    static TParseTree DefaultValue(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, ConstValue, Spacing), pegged.peg.wrapAround!(Spacing, String, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), Spacing)), "WebIDL.DefaultValue")(p);
        }
        else
        {
            if (auto m = tuple(`DefaultValue`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, ConstValue, Spacing), pegged.peg.wrapAround!(Spacing, String, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), Spacing)), "WebIDL.DefaultValue"), "DefaultValue")(p);
                memo[tuple(`DefaultValue`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree DefaultValue(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, ConstValue, Spacing), pegged.peg.wrapAround!(Spacing, String, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), Spacing)), "WebIDL.DefaultValue")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, ConstValue, Spacing), pegged.peg.wrapAround!(Spacing, String, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), Spacing)), "WebIDL.DefaultValue"), "DefaultValue")(TParseTree("", false,[], s));
        }
    }
    static string DefaultValue(GetName g)
    {
        return "WebIDL.DefaultValue";
    }

    static TParseTree Operation(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, SpecialOperation, Spacing), pegged.peg.wrapAround!(Spacing, RegularOperation, Spacing)), "WebIDL.Operation")(p);
        }
        else
        {
            if (auto m = tuple(`Operation`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, SpecialOperation, Spacing), pegged.peg.wrapAround!(Spacing, RegularOperation, Spacing)), "WebIDL.Operation"), "Operation")(p);
                memo[tuple(`Operation`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Operation(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, SpecialOperation, Spacing), pegged.peg.wrapAround!(Spacing, RegularOperation, Spacing)), "WebIDL.Operation")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, SpecialOperation, Spacing), pegged.peg.wrapAround!(Spacing, RegularOperation, Spacing)), "WebIDL.Operation"), "Operation")(TParseTree("", false,[], s));
        }
    }
    static string Operation(GetName g)
    {
        return "WebIDL.Operation";
    }

    static TParseTree RegularOperation(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ReturnType, Spacing), pegged.peg.wrapAround!(Spacing, OperationRest, Spacing)), "WebIDL.RegularOperation")(p);
        }
        else
        {
            if (auto m = tuple(`RegularOperation`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ReturnType, Spacing), pegged.peg.wrapAround!(Spacing, OperationRest, Spacing)), "WebIDL.RegularOperation"), "RegularOperation")(p);
                memo[tuple(`RegularOperation`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree RegularOperation(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ReturnType, Spacing), pegged.peg.wrapAround!(Spacing, OperationRest, Spacing)), "WebIDL.RegularOperation")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ReturnType, Spacing), pegged.peg.wrapAround!(Spacing, OperationRest, Spacing)), "WebIDL.RegularOperation"), "RegularOperation")(TParseTree("", false,[], s));
        }
    }
    static string RegularOperation(GetName g)
    {
        return "WebIDL.RegularOperation";
    }

    static TParseTree SpecialOperation(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Special, Spacing), pegged.peg.wrapAround!(Spacing, RegularOperation, Spacing)), "WebIDL.SpecialOperation")(p);
        }
        else
        {
            if (auto m = tuple(`SpecialOperation`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Special, Spacing), pegged.peg.wrapAround!(Spacing, RegularOperation, Spacing)), "WebIDL.SpecialOperation"), "SpecialOperation")(p);
                memo[tuple(`SpecialOperation`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree SpecialOperation(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Special, Spacing), pegged.peg.wrapAround!(Spacing, RegularOperation, Spacing)), "WebIDL.SpecialOperation")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Special, Spacing), pegged.peg.wrapAround!(Spacing, RegularOperation, Spacing)), "WebIDL.SpecialOperation"), "SpecialOperation")(TParseTree("", false,[], s));
        }
    }
    static string SpecialOperation(GetName g)
    {
        return "WebIDL.SpecialOperation";
    }

    static TParseTree Special(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("getter"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("setter"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("deleter"), Spacing)), "WebIDL.Special")(p);
        }
        else
        {
            if (auto m = tuple(`Special`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("getter"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("setter"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("deleter"), Spacing)), "WebIDL.Special"), "Special")(p);
                memo[tuple(`Special`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Special(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("getter"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("setter"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("deleter"), Spacing)), "WebIDL.Special")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("getter"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("setter"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("deleter"), Spacing)), "WebIDL.Special"), "Special")(TParseTree("", false,[], s));
        }
    }
    static string Special(GetName g)
    {
        return "WebIDL.Special";
    }

    static TParseTree OperationRest(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, OptionalIdentifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, ArgumentList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "WebIDL.OperationRest")(p);
        }
        else
        {
            if (auto m = tuple(`OperationRest`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, OptionalIdentifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, ArgumentList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "WebIDL.OperationRest"), "OperationRest")(p);
                memo[tuple(`OperationRest`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree OperationRest(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, OptionalIdentifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, ArgumentList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "WebIDL.OperationRest")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, OptionalIdentifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, ArgumentList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "WebIDL.OperationRest"), "OperationRest")(TParseTree("", false,[], s));
        }
    }
    static string OperationRest(GetName g)
    {
        return "WebIDL.OperationRest";
    }

    static TParseTree OptionalIdentifier(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, eps, Spacing)), "WebIDL.OptionalIdentifier")(p);
        }
        else
        {
            if (auto m = tuple(`OptionalIdentifier`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, eps, Spacing)), "WebIDL.OptionalIdentifier"), "OptionalIdentifier")(p);
                memo[tuple(`OptionalIdentifier`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree OptionalIdentifier(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, eps, Spacing)), "WebIDL.OptionalIdentifier")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, eps, Spacing)), "WebIDL.OptionalIdentifier"), "OptionalIdentifier")(TParseTree("", false,[], s));
        }
    }
    static string OptionalIdentifier(GetName g)
    {
        return "WebIDL.OptionalIdentifier";
    }

    static TParseTree ArgumentList(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Argument, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, Argument, Spacing)), Spacing))), Spacing), pegged.peg.wrapAround!(Spacing, eps, Spacing)), "WebIDL.ArgumentList")(p);
        }
        else
        {
            if (auto m = tuple(`ArgumentList`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Argument, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, Argument, Spacing)), Spacing))), Spacing), pegged.peg.wrapAround!(Spacing, eps, Spacing)), "WebIDL.ArgumentList"), "ArgumentList")(p);
                memo[tuple(`ArgumentList`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ArgumentList(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Argument, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, Argument, Spacing)), Spacing))), Spacing), pegged.peg.wrapAround!(Spacing, eps, Spacing)), "WebIDL.ArgumentList")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Argument, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, Argument, Spacing)), Spacing))), Spacing), pegged.peg.wrapAround!(Spacing, eps, Spacing)), "WebIDL.ArgumentList"), "ArgumentList")(TParseTree("", false,[], s));
        }
    }
    static string ArgumentList(GetName g)
    {
        return "WebIDL.ArgumentList";
    }

    static TParseTree Argument(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ExtendedAttributeList, Spacing), pegged.peg.wrapAround!(Spacing, ArgumentRest, Spacing)), "WebIDL.Argument")(p);
        }
        else
        {
            if (auto m = tuple(`Argument`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ExtendedAttributeList, Spacing), pegged.peg.wrapAround!(Spacing, ArgumentRest, Spacing)), "WebIDL.Argument"), "Argument")(p);
                memo[tuple(`Argument`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Argument(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ExtendedAttributeList, Spacing), pegged.peg.wrapAround!(Spacing, ArgumentRest, Spacing)), "WebIDL.Argument")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ExtendedAttributeList, Spacing), pegged.peg.wrapAround!(Spacing, ArgumentRest, Spacing)), "WebIDL.Argument"), "Argument")(TParseTree("", false,[], s));
        }
    }
    static string Argument(GetName g)
    {
        return "WebIDL.Argument";
    }

    static TParseTree ArgumentRest(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("optional"), Spacing), pegged.peg.wrapAround!(Spacing, TypeWithExtendedAttributes, Spacing), pegged.peg.wrapAround!(Spacing, ArgumentName, Spacing), pegged.peg.wrapAround!(Spacing, Default, Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.wrapAround!(Spacing, Ellipsis, Spacing), pegged.peg.wrapAround!(Spacing, ArgumentName, Spacing)), Spacing)), "WebIDL.ArgumentRest")(p);
        }
        else
        {
            if (auto m = tuple(`ArgumentRest`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("optional"), Spacing), pegged.peg.wrapAround!(Spacing, TypeWithExtendedAttributes, Spacing), pegged.peg.wrapAround!(Spacing, ArgumentName, Spacing), pegged.peg.wrapAround!(Spacing, Default, Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.wrapAround!(Spacing, Ellipsis, Spacing), pegged.peg.wrapAround!(Spacing, ArgumentName, Spacing)), Spacing)), "WebIDL.ArgumentRest"), "ArgumentRest")(p);
                memo[tuple(`ArgumentRest`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ArgumentRest(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("optional"), Spacing), pegged.peg.wrapAround!(Spacing, TypeWithExtendedAttributes, Spacing), pegged.peg.wrapAround!(Spacing, ArgumentName, Spacing), pegged.peg.wrapAround!(Spacing, Default, Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.wrapAround!(Spacing, Ellipsis, Spacing), pegged.peg.wrapAround!(Spacing, ArgumentName, Spacing)), Spacing)), "WebIDL.ArgumentRest")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("optional"), Spacing), pegged.peg.wrapAround!(Spacing, TypeWithExtendedAttributes, Spacing), pegged.peg.wrapAround!(Spacing, ArgumentName, Spacing), pegged.peg.wrapAround!(Spacing, Default, Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.wrapAround!(Spacing, Ellipsis, Spacing), pegged.peg.wrapAround!(Spacing, ArgumentName, Spacing)), Spacing)), "WebIDL.ArgumentRest"), "ArgumentRest")(TParseTree("", false,[], s));
        }
    }
    static string ArgumentRest(GetName g)
    {
        return "WebIDL.ArgumentRest";
    }

    static TParseTree ArgumentName(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.longest_match!(pegged.peg.wrapAround!(Spacing, ArgumentNameKeyword, Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), "WebIDL.ArgumentName")(p);
        }
        else
        {
            if (auto m = tuple(`ArgumentName`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.longest_match!(pegged.peg.wrapAround!(Spacing, ArgumentNameKeyword, Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), "WebIDL.ArgumentName"), "ArgumentName")(p);
                memo[tuple(`ArgumentName`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ArgumentName(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.longest_match!(pegged.peg.wrapAround!(Spacing, ArgumentNameKeyword, Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), "WebIDL.ArgumentName")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.longest_match!(pegged.peg.wrapAround!(Spacing, ArgumentNameKeyword, Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), "WebIDL.ArgumentName"), "ArgumentName")(TParseTree("", false,[], s));
        }
    }
    static string ArgumentName(GetName g)
    {
        return "WebIDL.ArgumentName";
    }

    static TParseTree Ellipsis(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("..."), Spacing), pegged.peg.wrapAround!(Spacing, eps, Spacing)), "WebIDL.Ellipsis")(p);
        }
        else
        {
            if (auto m = tuple(`Ellipsis`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("..."), Spacing), pegged.peg.wrapAround!(Spacing, eps, Spacing)), "WebIDL.Ellipsis"), "Ellipsis")(p);
                memo[tuple(`Ellipsis`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Ellipsis(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("..."), Spacing), pegged.peg.wrapAround!(Spacing, eps, Spacing)), "WebIDL.Ellipsis")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("..."), Spacing), pegged.peg.wrapAround!(Spacing, eps, Spacing)), "WebIDL.Ellipsis"), "Ellipsis")(TParseTree("", false,[], s));
        }
    }
    static string Ellipsis(GetName g)
    {
        return "WebIDL.Ellipsis";
    }

    static TParseTree ReturnType(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("void"), Spacing)), "WebIDL.ReturnType")(p);
        }
        else
        {
            if (auto m = tuple(`ReturnType`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("void"), Spacing)), "WebIDL.ReturnType"), "ReturnType")(p);
                memo[tuple(`ReturnType`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ReturnType(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("void"), Spacing)), "WebIDL.ReturnType")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("void"), Spacing)), "WebIDL.ReturnType"), "ReturnType")(TParseTree("", false,[], s));
        }
    }
    static string ReturnType(GetName g)
    {
        return "WebIDL.ReturnType";
    }

    static TParseTree Stringifier(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("stringifier"), Spacing), pegged.peg.wrapAround!(Spacing, StringifierRest, Spacing)), "WebIDL.Stringifier")(p);
        }
        else
        {
            if (auto m = tuple(`Stringifier`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("stringifier"), Spacing), pegged.peg.wrapAround!(Spacing, StringifierRest, Spacing)), "WebIDL.Stringifier"), "Stringifier")(p);
                memo[tuple(`Stringifier`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Stringifier(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("stringifier"), Spacing), pegged.peg.wrapAround!(Spacing, StringifierRest, Spacing)), "WebIDL.Stringifier")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("stringifier"), Spacing), pegged.peg.wrapAround!(Spacing, StringifierRest, Spacing)), "WebIDL.Stringifier"), "Stringifier")(TParseTree("", false,[], s));
        }
    }
    static string Stringifier(GetName g)
    {
        return "WebIDL.Stringifier";
    }

    static TParseTree StringifierRest(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ReadOnly, Spacing), pegged.peg.wrapAround!(Spacing, AttributeRest, Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.wrapAround!(Spacing, RegularOperation, Spacing), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "WebIDL.StringifierRest")(p);
        }
        else
        {
            if (auto m = tuple(`StringifierRest`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ReadOnly, Spacing), pegged.peg.wrapAround!(Spacing, AttributeRest, Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.wrapAround!(Spacing, RegularOperation, Spacing), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "WebIDL.StringifierRest"), "StringifierRest")(p);
                memo[tuple(`StringifierRest`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree StringifierRest(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ReadOnly, Spacing), pegged.peg.wrapAround!(Spacing, AttributeRest, Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.wrapAround!(Spacing, RegularOperation, Spacing), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "WebIDL.StringifierRest")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ReadOnly, Spacing), pegged.peg.wrapAround!(Spacing, AttributeRest, Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.wrapAround!(Spacing, RegularOperation, Spacing), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "WebIDL.StringifierRest"), "StringifierRest")(TParseTree("", false,[], s));
        }
    }
    static string StringifierRest(GetName g)
    {
        return "WebIDL.StringifierRest";
    }

    static TParseTree StaticMember(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("static"), Spacing), pegged.peg.wrapAround!(Spacing, StaticMemberRest, Spacing)), "WebIDL.StaticMember")(p);
        }
        else
        {
            if (auto m = tuple(`StaticMember`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("static"), Spacing), pegged.peg.wrapAround!(Spacing, StaticMemberRest, Spacing)), "WebIDL.StaticMember"), "StaticMember")(p);
                memo[tuple(`StaticMember`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree StaticMember(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("static"), Spacing), pegged.peg.wrapAround!(Spacing, StaticMemberRest, Spacing)), "WebIDL.StaticMember")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("static"), Spacing), pegged.peg.wrapAround!(Spacing, StaticMemberRest, Spacing)), "WebIDL.StaticMember"), "StaticMember")(TParseTree("", false,[], s));
        }
    }
    static string StaticMember(GetName g)
    {
        return "WebIDL.StaticMember";
    }

    static TParseTree StaticMemberRest(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ReadOnly, Spacing), pegged.peg.wrapAround!(Spacing, AttributeRest, Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, RegularOperation, Spacing)), "WebIDL.StaticMemberRest")(p);
        }
        else
        {
            if (auto m = tuple(`StaticMemberRest`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ReadOnly, Spacing), pegged.peg.wrapAround!(Spacing, AttributeRest, Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, RegularOperation, Spacing)), "WebIDL.StaticMemberRest"), "StaticMemberRest")(p);
                memo[tuple(`StaticMemberRest`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree StaticMemberRest(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ReadOnly, Spacing), pegged.peg.wrapAround!(Spacing, AttributeRest, Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, RegularOperation, Spacing)), "WebIDL.StaticMemberRest")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ReadOnly, Spacing), pegged.peg.wrapAround!(Spacing, AttributeRest, Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, RegularOperation, Spacing)), "WebIDL.StaticMemberRest"), "StaticMemberRest")(TParseTree("", false,[], s));
        }
    }
    static string StaticMemberRest(GetName g)
    {
        return "WebIDL.StaticMemberRest";
    }

    static TParseTree Iterable(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("iterable"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<"), Spacing), pegged.peg.wrapAround!(Spacing, TypeWithExtendedAttributes, Spacing), pegged.peg.wrapAround!(Spacing, OptionalType, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "WebIDL.Iterable")(p);
        }
        else
        {
            if (auto m = tuple(`Iterable`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("iterable"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<"), Spacing), pegged.peg.wrapAround!(Spacing, TypeWithExtendedAttributes, Spacing), pegged.peg.wrapAround!(Spacing, OptionalType, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "WebIDL.Iterable"), "Iterable")(p);
                memo[tuple(`Iterable`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Iterable(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("iterable"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<"), Spacing), pegged.peg.wrapAround!(Spacing, TypeWithExtendedAttributes, Spacing), pegged.peg.wrapAround!(Spacing, OptionalType, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "WebIDL.Iterable")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("iterable"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<"), Spacing), pegged.peg.wrapAround!(Spacing, TypeWithExtendedAttributes, Spacing), pegged.peg.wrapAround!(Spacing, OptionalType, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "WebIDL.Iterable"), "Iterable")(TParseTree("", false,[], s));
        }
    }
    static string Iterable(GetName g)
    {
        return "WebIDL.Iterable";
    }

    static TParseTree OptionalType(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, TypeWithExtendedAttributes, Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, eps, Spacing)), "WebIDL.OptionalType")(p);
        }
        else
        {
            if (auto m = tuple(`OptionalType`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, TypeWithExtendedAttributes, Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, eps, Spacing)), "WebIDL.OptionalType"), "OptionalType")(p);
                memo[tuple(`OptionalType`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree OptionalType(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, TypeWithExtendedAttributes, Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, eps, Spacing)), "WebIDL.OptionalType")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, TypeWithExtendedAttributes, Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, eps, Spacing)), "WebIDL.OptionalType"), "OptionalType")(TParseTree("", false,[], s));
        }
    }
    static string OptionalType(GetName g)
    {
        return "WebIDL.OptionalType";
    }

    static TParseTree ReadWriteMaplike(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, MaplikeRest, Spacing), "WebIDL.ReadWriteMaplike")(p);
        }
        else
        {
            if (auto m = tuple(`ReadWriteMaplike`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, MaplikeRest, Spacing), "WebIDL.ReadWriteMaplike"), "ReadWriteMaplike")(p);
                memo[tuple(`ReadWriteMaplike`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ReadWriteMaplike(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, MaplikeRest, Spacing), "WebIDL.ReadWriteMaplike")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, MaplikeRest, Spacing), "WebIDL.ReadWriteMaplike"), "ReadWriteMaplike")(TParseTree("", false,[], s));
        }
    }
    static string ReadWriteMaplike(GetName g)
    {
        return "WebIDL.ReadWriteMaplike";
    }

    static TParseTree MaplikeRest(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("maplike"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<"), Spacing), pegged.peg.wrapAround!(Spacing, TypeWithExtendedAttributes, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, TypeWithExtendedAttributes, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "WebIDL.MaplikeRest")(p);
        }
        else
        {
            if (auto m = tuple(`MaplikeRest`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("maplike"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<"), Spacing), pegged.peg.wrapAround!(Spacing, TypeWithExtendedAttributes, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, TypeWithExtendedAttributes, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "WebIDL.MaplikeRest"), "MaplikeRest")(p);
                memo[tuple(`MaplikeRest`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree MaplikeRest(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("maplike"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<"), Spacing), pegged.peg.wrapAround!(Spacing, TypeWithExtendedAttributes, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, TypeWithExtendedAttributes, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "WebIDL.MaplikeRest")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("maplike"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<"), Spacing), pegged.peg.wrapAround!(Spacing, TypeWithExtendedAttributes, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, TypeWithExtendedAttributes, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "WebIDL.MaplikeRest"), "MaplikeRest")(TParseTree("", false,[], s));
        }
    }
    static string MaplikeRest(GetName g)
    {
        return "WebIDL.MaplikeRest";
    }

    static TParseTree ReadWriteSetlike(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, SetlikeRest, Spacing), "WebIDL.ReadWriteSetlike")(p);
        }
        else
        {
            if (auto m = tuple(`ReadWriteSetlike`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, SetlikeRest, Spacing), "WebIDL.ReadWriteSetlike"), "ReadWriteSetlike")(p);
                memo[tuple(`ReadWriteSetlike`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ReadWriteSetlike(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, SetlikeRest, Spacing), "WebIDL.ReadWriteSetlike")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, SetlikeRest, Spacing), "WebIDL.ReadWriteSetlike"), "ReadWriteSetlike")(TParseTree("", false,[], s));
        }
    }
    static string ReadWriteSetlike(GetName g)
    {
        return "WebIDL.ReadWriteSetlike";
    }

    static TParseTree SetlikeRest(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("setlike"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<"), Spacing), pegged.peg.wrapAround!(Spacing, TypeWithExtendedAttributes, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "WebIDL.SetlikeRest")(p);
        }
        else
        {
            if (auto m = tuple(`SetlikeRest`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("setlike"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<"), Spacing), pegged.peg.wrapAround!(Spacing, TypeWithExtendedAttributes, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "WebIDL.SetlikeRest"), "SetlikeRest")(p);
                memo[tuple(`SetlikeRest`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree SetlikeRest(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("setlike"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<"), Spacing), pegged.peg.wrapAround!(Spacing, TypeWithExtendedAttributes, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "WebIDL.SetlikeRest")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("setlike"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<"), Spacing), pegged.peg.wrapAround!(Spacing, TypeWithExtendedAttributes, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "WebIDL.SetlikeRest"), "SetlikeRest")(TParseTree("", false,[], s));
        }
    }
    static string SetlikeRest(GetName g)
    {
        return "WebIDL.SetlikeRest";
    }

    static TParseTree Namespace(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("namespace"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, NamespaceMembers, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "WebIDL.Namespace")(p);
        }
        else
        {
            if (auto m = tuple(`Namespace`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("namespace"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, NamespaceMembers, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "WebIDL.Namespace"), "Namespace")(p);
                memo[tuple(`Namespace`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Namespace(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("namespace"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, NamespaceMembers, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "WebIDL.Namespace")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("namespace"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, NamespaceMembers, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "WebIDL.Namespace"), "Namespace")(TParseTree("", false,[], s));
        }
    }
    static string Namespace(GetName g)
    {
        return "WebIDL.Namespace";
    }

    static TParseTree NamespaceMembers(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ExtendedAttributeList, Spacing), pegged.peg.wrapAround!(Spacing, NamespaceMember, Spacing)), Spacing)), pegged.peg.wrapAround!(Spacing, eps, Spacing)), "WebIDL.NamespaceMembers")(p);
        }
        else
        {
            if (auto m = tuple(`NamespaceMembers`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ExtendedAttributeList, Spacing), pegged.peg.wrapAround!(Spacing, NamespaceMember, Spacing)), Spacing)), pegged.peg.wrapAround!(Spacing, eps, Spacing)), "WebIDL.NamespaceMembers"), "NamespaceMembers")(p);
                memo[tuple(`NamespaceMembers`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree NamespaceMembers(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ExtendedAttributeList, Spacing), pegged.peg.wrapAround!(Spacing, NamespaceMember, Spacing)), Spacing)), pegged.peg.wrapAround!(Spacing, eps, Spacing)), "WebIDL.NamespaceMembers")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ExtendedAttributeList, Spacing), pegged.peg.wrapAround!(Spacing, NamespaceMember, Spacing)), Spacing)), pegged.peg.wrapAround!(Spacing, eps, Spacing)), "WebIDL.NamespaceMembers"), "NamespaceMembers")(TParseTree("", false,[], s));
        }
    }
    static string NamespaceMembers(GetName g)
    {
        return "WebIDL.NamespaceMembers";
    }

    static TParseTree NamespaceMember(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, RegularOperation, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("readonly"), Spacing), pegged.peg.wrapAround!(Spacing, AttributeRest, Spacing)), Spacing)), "WebIDL.NamespaceMember")(p);
        }
        else
        {
            if (auto m = tuple(`NamespaceMember`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, RegularOperation, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("readonly"), Spacing), pegged.peg.wrapAround!(Spacing, AttributeRest, Spacing)), Spacing)), "WebIDL.NamespaceMember"), "NamespaceMember")(p);
                memo[tuple(`NamespaceMember`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree NamespaceMember(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, RegularOperation, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("readonly"), Spacing), pegged.peg.wrapAround!(Spacing, AttributeRest, Spacing)), Spacing)), "WebIDL.NamespaceMember")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, RegularOperation, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("readonly"), Spacing), pegged.peg.wrapAround!(Spacing, AttributeRest, Spacing)), Spacing)), "WebIDL.NamespaceMember"), "NamespaceMember")(TParseTree("", false,[], s));
        }
    }
    static string NamespaceMember(GetName g)
    {
        return "WebIDL.NamespaceMember";
    }

    static TParseTree Dictionary(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("dictionary"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, Inheritance, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, DictionaryMembers, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "WebIDL.Dictionary")(p);
        }
        else
        {
            if (auto m = tuple(`Dictionary`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("dictionary"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, Inheritance, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, DictionaryMembers, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "WebIDL.Dictionary"), "Dictionary")(p);
                memo[tuple(`Dictionary`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Dictionary(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("dictionary"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, Inheritance, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, DictionaryMembers, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "WebIDL.Dictionary")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("dictionary"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, Inheritance, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, DictionaryMembers, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "WebIDL.Dictionary"), "Dictionary")(TParseTree("", false,[], s));
        }
    }
    static string Dictionary(GetName g)
    {
        return "WebIDL.Dictionary";
    }

    static TParseTree DictionaryMembers(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, DictionaryMember, Spacing)), pegged.peg.wrapAround!(Spacing, eps, Spacing)), "WebIDL.DictionaryMembers")(p);
        }
        else
        {
            if (auto m = tuple(`DictionaryMembers`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, DictionaryMember, Spacing)), pegged.peg.wrapAround!(Spacing, eps, Spacing)), "WebIDL.DictionaryMembers"), "DictionaryMembers")(p);
                memo[tuple(`DictionaryMembers`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree DictionaryMembers(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, DictionaryMember, Spacing)), pegged.peg.wrapAround!(Spacing, eps, Spacing)), "WebIDL.DictionaryMembers")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, DictionaryMember, Spacing)), pegged.peg.wrapAround!(Spacing, eps, Spacing)), "WebIDL.DictionaryMembers"), "DictionaryMembers")(TParseTree("", false,[], s));
        }
    }
    static string DictionaryMembers(GetName g)
    {
        return "WebIDL.DictionaryMembers";
    }

    static TParseTree DictionaryMember(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ExtendedAttributeList, Spacing), pegged.peg.wrapAround!(Spacing, DictionaryMemberRest, Spacing)), "WebIDL.DictionaryMember")(p);
        }
        else
        {
            if (auto m = tuple(`DictionaryMember`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ExtendedAttributeList, Spacing), pegged.peg.wrapAround!(Spacing, DictionaryMemberRest, Spacing)), "WebIDL.DictionaryMember"), "DictionaryMember")(p);
                memo[tuple(`DictionaryMember`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree DictionaryMember(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ExtendedAttributeList, Spacing), pegged.peg.wrapAround!(Spacing, DictionaryMemberRest, Spacing)), "WebIDL.DictionaryMember")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ExtendedAttributeList, Spacing), pegged.peg.wrapAround!(Spacing, DictionaryMemberRest, Spacing)), "WebIDL.DictionaryMember"), "DictionaryMember")(TParseTree("", false,[], s));
        }
    }
    static string DictionaryMember(GetName g)
    {
        return "WebIDL.DictionaryMember";
    }

    static TParseTree DictionaryMemberRest(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("required"), Spacing), pegged.peg.wrapAround!(Spacing, TypeWithExtendedAttributes, Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, Default, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, Default, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), Spacing)), "WebIDL.DictionaryMemberRest")(p);
        }
        else
        {
            if (auto m = tuple(`DictionaryMemberRest`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("required"), Spacing), pegged.peg.wrapAround!(Spacing, TypeWithExtendedAttributes, Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, Default, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, Default, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), Spacing)), "WebIDL.DictionaryMemberRest"), "DictionaryMemberRest")(p);
                memo[tuple(`DictionaryMemberRest`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree DictionaryMemberRest(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("required"), Spacing), pegged.peg.wrapAround!(Spacing, TypeWithExtendedAttributes, Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, Default, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, Default, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), Spacing)), "WebIDL.DictionaryMemberRest")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("required"), Spacing), pegged.peg.wrapAround!(Spacing, TypeWithExtendedAttributes, Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, Default, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, Default, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), Spacing)), "WebIDL.DictionaryMemberRest"), "DictionaryMemberRest")(TParseTree("", false,[], s));
        }
    }
    static string DictionaryMemberRest(GetName g)
    {
        return "WebIDL.DictionaryMemberRest";
    }

    static TParseTree PartialDictionary(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("dictionary"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, DictionaryMembers, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "WebIDL.PartialDictionary")(p);
        }
        else
        {
            if (auto m = tuple(`PartialDictionary`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("dictionary"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, DictionaryMembers, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "WebIDL.PartialDictionary"), "PartialDictionary")(p);
                memo[tuple(`PartialDictionary`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree PartialDictionary(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("dictionary"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, DictionaryMembers, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "WebIDL.PartialDictionary")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("dictionary"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, DictionaryMembers, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "WebIDL.PartialDictionary"), "PartialDictionary")(TParseTree("", false,[], s));
        }
    }
    static string PartialDictionary(GetName g)
    {
        return "WebIDL.PartialDictionary";
    }

    static TParseTree Default(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, DefaultValue, Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, eps, Spacing)), "WebIDL.Default")(p);
        }
        else
        {
            if (auto m = tuple(`Default`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, DefaultValue, Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, eps, Spacing)), "WebIDL.Default"), "Default")(p);
                memo[tuple(`Default`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Default(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, DefaultValue, Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, eps, Spacing)), "WebIDL.Default")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, DefaultValue, Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, eps, Spacing)), "WebIDL.Default"), "Default")(TParseTree("", false,[], s));
        }
    }
    static string Default(GetName g)
    {
        return "WebIDL.Default";
    }

    static TParseTree Enum(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("enum"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, EnumValueList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "WebIDL.Enum")(p);
        }
        else
        {
            if (auto m = tuple(`Enum`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("enum"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, EnumValueList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "WebIDL.Enum"), "Enum")(p);
                memo[tuple(`Enum`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Enum(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("enum"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, EnumValueList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "WebIDL.Enum")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("enum"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, EnumValueList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "WebIDL.Enum"), "Enum")(TParseTree("", false,[], s));
        }
    }
    static string Enum(GetName g)
    {
        return "WebIDL.Enum";
    }

    static TParseTree EnumValueList(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, String, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, String, Spacing)), Spacing))), "WebIDL.EnumValueList")(p);
        }
        else
        {
            if (auto m = tuple(`EnumValueList`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, String, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, String, Spacing)), Spacing))), "WebIDL.EnumValueList"), "EnumValueList")(p);
                memo[tuple(`EnumValueList`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree EnumValueList(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, String, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, String, Spacing)), Spacing))), "WebIDL.EnumValueList")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, String, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, String, Spacing)), Spacing))), "WebIDL.EnumValueList"), "EnumValueList")(TParseTree("", false,[], s));
        }
    }
    static string EnumValueList(GetName g)
    {
        return "WebIDL.EnumValueList";
    }

    static TParseTree CallbackRest(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, ReturnType, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, ArgumentList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "WebIDL.CallbackRest")(p);
        }
        else
        {
            if (auto m = tuple(`CallbackRest`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, ReturnType, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, ArgumentList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "WebIDL.CallbackRest"), "CallbackRest")(p);
                memo[tuple(`CallbackRest`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree CallbackRest(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, ReturnType, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, ArgumentList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "WebIDL.CallbackRest")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, ReturnType, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, ArgumentList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "WebIDL.CallbackRest"), "CallbackRest")(TParseTree("", false,[], s));
        }
    }
    static string CallbackRest(GetName g)
    {
        return "WebIDL.CallbackRest";
    }

    static TParseTree Typedef(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("typedef"), Spacing), pegged.peg.wrapAround!(Spacing, TypeWithExtendedAttributes, Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "WebIDL.Typedef")(p);
        }
        else
        {
            if (auto m = tuple(`Typedef`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("typedef"), Spacing), pegged.peg.wrapAround!(Spacing, TypeWithExtendedAttributes, Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "WebIDL.Typedef"), "Typedef")(p);
                memo[tuple(`Typedef`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Typedef(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("typedef"), Spacing), pegged.peg.wrapAround!(Spacing, TypeWithExtendedAttributes, Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "WebIDL.Typedef")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("typedef"), Spacing), pegged.peg.wrapAround!(Spacing, TypeWithExtendedAttributes, Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "WebIDL.Typedef"), "Typedef")(TParseTree("", false,[], s));
        }
    }
    static string Typedef(GetName g)
    {
        return "WebIDL.Typedef";
    }

    static TParseTree Type(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, SingleType, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, UnionType, Spacing), pegged.peg.wrapAround!(Spacing, Null, Spacing)), Spacing)), "WebIDL.Type")(p);
        }
        else
        {
            if (auto m = tuple(`Type`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, SingleType, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, UnionType, Spacing), pegged.peg.wrapAround!(Spacing, Null, Spacing)), Spacing)), "WebIDL.Type"), "Type")(p);
                memo[tuple(`Type`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Type(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, SingleType, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, UnionType, Spacing), pegged.peg.wrapAround!(Spacing, Null, Spacing)), Spacing)), "WebIDL.Type")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, SingleType, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, UnionType, Spacing), pegged.peg.wrapAround!(Spacing, Null, Spacing)), Spacing)), "WebIDL.Type"), "Type")(TParseTree("", false,[], s));
        }
    }
    static string Type(GetName g)
    {
        return "WebIDL.Type";
    }

    static TParseTree TypeWithExtendedAttributes(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ExtendedAttributeList, Spacing), pegged.peg.wrapAround!(Spacing, Type, Spacing)), "WebIDL.TypeWithExtendedAttributes")(p);
        }
        else
        {
            if (auto m = tuple(`TypeWithExtendedAttributes`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ExtendedAttributeList, Spacing), pegged.peg.wrapAround!(Spacing, Type, Spacing)), "WebIDL.TypeWithExtendedAttributes"), "TypeWithExtendedAttributes")(p);
                memo[tuple(`TypeWithExtendedAttributes`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree TypeWithExtendedAttributes(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ExtendedAttributeList, Spacing), pegged.peg.wrapAround!(Spacing, Type, Spacing)), "WebIDL.TypeWithExtendedAttributes")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ExtendedAttributeList, Spacing), pegged.peg.wrapAround!(Spacing, Type, Spacing)), "WebIDL.TypeWithExtendedAttributes"), "TypeWithExtendedAttributes")(TParseTree("", false,[], s));
        }
    }
    static string TypeWithExtendedAttributes(GetName g)
    {
        return "WebIDL.TypeWithExtendedAttributes";
    }

    static TParseTree SingleType(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.literal!("any"), pegged.peg.negLookahead!(Identifier)), NonAnyType), "WebIDL.SingleType")(p);
        }
        else
        {
            if (auto m = tuple(`SingleType`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.literal!("any"), pegged.peg.negLookahead!(Identifier)), NonAnyType), "WebIDL.SingleType"), "SingleType")(p);
                memo[tuple(`SingleType`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree SingleType(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.literal!("any"), pegged.peg.negLookahead!(Identifier)), NonAnyType), "WebIDL.SingleType")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.literal!("any"), pegged.peg.negLookahead!(Identifier)), NonAnyType), "WebIDL.SingleType"), "SingleType")(TParseTree("", false,[], s));
        }
    }
    static string SingleType(GetName g)
    {
        return "WebIDL.SingleType";
    }

    static TParseTree UnionType(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, UnionMemberType, Spacing), pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("or"), Spacing), pegged.peg.wrapAround!(Spacing, UnionMemberType, Spacing)), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "WebIDL.UnionType")(p);
        }
        else
        {
            if (auto m = tuple(`UnionType`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, UnionMemberType, Spacing), pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("or"), Spacing), pegged.peg.wrapAround!(Spacing, UnionMemberType, Spacing)), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "WebIDL.UnionType"), "UnionType")(p);
                memo[tuple(`UnionType`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree UnionType(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, UnionMemberType, Spacing), pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("or"), Spacing), pegged.peg.wrapAround!(Spacing, UnionMemberType, Spacing)), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "WebIDL.UnionType")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, UnionMemberType, Spacing), pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("or"), Spacing), pegged.peg.wrapAround!(Spacing, UnionMemberType, Spacing)), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "WebIDL.UnionType"), "UnionType")(TParseTree("", false,[], s));
        }
    }
    static string UnionType(GetName g)
    {
        return "WebIDL.UnionType";
    }

    static TParseTree UnionMemberType(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ExtendedAttributeList, Spacing), pegged.peg.wrapAround!(Spacing, NonAnyType, Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, UnionType, Spacing), pegged.peg.wrapAround!(Spacing, Null, Spacing)), Spacing)), "WebIDL.UnionMemberType")(p);
        }
        else
        {
            if (auto m = tuple(`UnionMemberType`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ExtendedAttributeList, Spacing), pegged.peg.wrapAround!(Spacing, NonAnyType, Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, UnionType, Spacing), pegged.peg.wrapAround!(Spacing, Null, Spacing)), Spacing)), "WebIDL.UnionMemberType"), "UnionMemberType")(p);
                memo[tuple(`UnionMemberType`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree UnionMemberType(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ExtendedAttributeList, Spacing), pegged.peg.wrapAround!(Spacing, NonAnyType, Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, UnionType, Spacing), pegged.peg.wrapAround!(Spacing, Null, Spacing)), Spacing)), "WebIDL.UnionMemberType")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ExtendedAttributeList, Spacing), pegged.peg.wrapAround!(Spacing, NonAnyType, Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, UnionType, Spacing), pegged.peg.wrapAround!(Spacing, Null, Spacing)), Spacing)), "WebIDL.UnionMemberType"), "UnionMemberType")(TParseTree("", false,[], s));
        }
    }
    static string UnionMemberType(GetName g)
    {
        return "WebIDL.UnionMemberType";
    }

    static TParseTree SequenceType(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("sequence"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<"), Spacing), pegged.peg.wrapAround!(Spacing, TypeWithExtendedAttributes, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">"), Spacing), pegged.peg.wrapAround!(Spacing, Null, Spacing)), "WebIDL.SequenceType")(p);
        }
        else
        {
            if (auto m = tuple(`SequenceType`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("sequence"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<"), Spacing), pegged.peg.wrapAround!(Spacing, TypeWithExtendedAttributes, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">"), Spacing), pegged.peg.wrapAround!(Spacing, Null, Spacing)), "WebIDL.SequenceType"), "SequenceType")(p);
                memo[tuple(`SequenceType`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree SequenceType(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("sequence"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<"), Spacing), pegged.peg.wrapAround!(Spacing, TypeWithExtendedAttributes, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">"), Spacing), pegged.peg.wrapAround!(Spacing, Null, Spacing)), "WebIDL.SequenceType")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("sequence"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<"), Spacing), pegged.peg.wrapAround!(Spacing, TypeWithExtendedAttributes, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">"), Spacing), pegged.peg.wrapAround!(Spacing, Null, Spacing)), "WebIDL.SequenceType"), "SequenceType")(TParseTree("", false,[], s));
        }
    }
    static string SequenceType(GetName g)
    {
        return "WebIDL.SequenceType";
    }

    static TParseTree NonAnyType(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(SequenceType, pegged.peg.and!(pegged.peg.literal!("object"), pegged.peg.and!(pegged.peg.negLookahead!(Identifier), Spacing, Null)), pegged.peg.and!(pegged.peg.literal!("symbol"), pegged.peg.and!(pegged.peg.negLookahead!(Identifier), Spacing, Null)), pegged.peg.and!(pegged.peg.literal!("Error"), pegged.peg.and!(pegged.peg.negLookahead!(Identifier), Spacing, Null)), pegged.peg.and!(pegged.peg.literal!("FrozenArray"), pegged.peg.literal!("<"), TypeWithExtendedAttributes, pegged.peg.literal!(">"), Null), pegged.peg.and!(RecordType, Null), PromiseType, pegged.peg.and!(PrimitiveType, Null), pegged.peg.and!(StringType, Null), pegged.peg.and!(Identifier, Null), pegged.peg.and!(BufferRelatedType, Null)), "WebIDL.NonAnyType")(p);
        }
        else
        {
            if (auto m = tuple(`NonAnyType`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(SequenceType, pegged.peg.and!(pegged.peg.literal!("object"), pegged.peg.and!(pegged.peg.negLookahead!(Identifier), Spacing, Null)), pegged.peg.and!(pegged.peg.literal!("symbol"), pegged.peg.and!(pegged.peg.negLookahead!(Identifier), Spacing, Null)), pegged.peg.and!(pegged.peg.literal!("Error"), pegged.peg.and!(pegged.peg.negLookahead!(Identifier), Spacing, Null)), pegged.peg.and!(pegged.peg.literal!("FrozenArray"), pegged.peg.literal!("<"), TypeWithExtendedAttributes, pegged.peg.literal!(">"), Null), pegged.peg.and!(RecordType, Null), PromiseType, pegged.peg.and!(PrimitiveType, Null), pegged.peg.and!(StringType, Null), pegged.peg.and!(Identifier, Null), pegged.peg.and!(BufferRelatedType, Null)), "WebIDL.NonAnyType"), "NonAnyType")(p);
                memo[tuple(`NonAnyType`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree NonAnyType(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(SequenceType, pegged.peg.and!(pegged.peg.literal!("object"), pegged.peg.and!(pegged.peg.negLookahead!(Identifier), Spacing, Null)), pegged.peg.and!(pegged.peg.literal!("symbol"), pegged.peg.and!(pegged.peg.negLookahead!(Identifier), Spacing, Null)), pegged.peg.and!(pegged.peg.literal!("Error"), pegged.peg.and!(pegged.peg.negLookahead!(Identifier), Spacing, Null)), pegged.peg.and!(pegged.peg.literal!("FrozenArray"), pegged.peg.literal!("<"), TypeWithExtendedAttributes, pegged.peg.literal!(">"), Null), pegged.peg.and!(RecordType, Null), PromiseType, pegged.peg.and!(PrimitiveType, Null), pegged.peg.and!(StringType, Null), pegged.peg.and!(Identifier, Null), pegged.peg.and!(BufferRelatedType, Null)), "WebIDL.NonAnyType")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(SequenceType, pegged.peg.and!(pegged.peg.literal!("object"), pegged.peg.and!(pegged.peg.negLookahead!(Identifier), Spacing, Null)), pegged.peg.and!(pegged.peg.literal!("symbol"), pegged.peg.and!(pegged.peg.negLookahead!(Identifier), Spacing, Null)), pegged.peg.and!(pegged.peg.literal!("Error"), pegged.peg.and!(pegged.peg.negLookahead!(Identifier), Spacing, Null)), pegged.peg.and!(pegged.peg.literal!("FrozenArray"), pegged.peg.literal!("<"), TypeWithExtendedAttributes, pegged.peg.literal!(">"), Null), pegged.peg.and!(RecordType, Null), PromiseType, pegged.peg.and!(PrimitiveType, Null), pegged.peg.and!(StringType, Null), pegged.peg.and!(Identifier, Null), pegged.peg.and!(BufferRelatedType, Null)), "WebIDL.NonAnyType"), "NonAnyType")(TParseTree("", false,[], s));
        }
    }
    static string NonAnyType(GetName g)
    {
        return "WebIDL.NonAnyType";
    }

    static TParseTree PrimitiveType(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, UnsignedIntegerType, Spacing), pegged.peg.wrapAround!(Spacing, UnrestrictedFloatType, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("boolean"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("byte"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("octet"), Spacing)), "WebIDL.PrimitiveType")(p);
        }
        else
        {
            if (auto m = tuple(`PrimitiveType`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, UnsignedIntegerType, Spacing), pegged.peg.wrapAround!(Spacing, UnrestrictedFloatType, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("boolean"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("byte"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("octet"), Spacing)), "WebIDL.PrimitiveType"), "PrimitiveType")(p);
                memo[tuple(`PrimitiveType`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree PrimitiveType(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, UnsignedIntegerType, Spacing), pegged.peg.wrapAround!(Spacing, UnrestrictedFloatType, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("boolean"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("byte"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("octet"), Spacing)), "WebIDL.PrimitiveType")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, UnsignedIntegerType, Spacing), pegged.peg.wrapAround!(Spacing, UnrestrictedFloatType, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("boolean"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("byte"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("octet"), Spacing)), "WebIDL.PrimitiveType"), "PrimitiveType")(TParseTree("", false,[], s));
        }
    }
    static string PrimitiveType(GetName g)
    {
        return "WebIDL.PrimitiveType";
    }

    static TParseTree UnrestrictedFloatType(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("unrestricted"), Spacing)), pegged.peg.wrapAround!(Spacing, FloatType, Spacing)), "WebIDL.UnrestrictedFloatType")(p);
        }
        else
        {
            if (auto m = tuple(`UnrestrictedFloatType`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("unrestricted"), Spacing)), pegged.peg.wrapAround!(Spacing, FloatType, Spacing)), "WebIDL.UnrestrictedFloatType"), "UnrestrictedFloatType")(p);
                memo[tuple(`UnrestrictedFloatType`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree UnrestrictedFloatType(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("unrestricted"), Spacing)), pegged.peg.wrapAround!(Spacing, FloatType, Spacing)), "WebIDL.UnrestrictedFloatType")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("unrestricted"), Spacing)), pegged.peg.wrapAround!(Spacing, FloatType, Spacing)), "WebIDL.UnrestrictedFloatType"), "UnrestrictedFloatType")(TParseTree("", false,[], s));
        }
    }
    static string UnrestrictedFloatType(GetName g)
    {
        return "WebIDL.UnrestrictedFloatType";
    }

    static TParseTree FloatType(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("float"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("double"), Spacing)), "WebIDL.FloatType")(p);
        }
        else
        {
            if (auto m = tuple(`FloatType`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("float"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("double"), Spacing)), "WebIDL.FloatType"), "FloatType")(p);
                memo[tuple(`FloatType`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree FloatType(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("float"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("double"), Spacing)), "WebIDL.FloatType")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("float"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("double"), Spacing)), "WebIDL.FloatType"), "FloatType")(TParseTree("", false,[], s));
        }
    }
    static string FloatType(GetName g)
    {
        return "WebIDL.FloatType";
    }

    static TParseTree UnsignedIntegerType(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("unsigned"), Spacing)), pegged.peg.wrapAround!(Spacing, IntegerType, Spacing)), "WebIDL.UnsignedIntegerType")(p);
        }
        else
        {
            if (auto m = tuple(`UnsignedIntegerType`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("unsigned"), Spacing)), pegged.peg.wrapAround!(Spacing, IntegerType, Spacing)), "WebIDL.UnsignedIntegerType"), "UnsignedIntegerType")(p);
                memo[tuple(`UnsignedIntegerType`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree UnsignedIntegerType(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("unsigned"), Spacing)), pegged.peg.wrapAround!(Spacing, IntegerType, Spacing)), "WebIDL.UnsignedIntegerType")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("unsigned"), Spacing)), pegged.peg.wrapAround!(Spacing, IntegerType, Spacing)), "WebIDL.UnsignedIntegerType"), "UnsignedIntegerType")(TParseTree("", false,[], s));
        }
    }
    static string UnsignedIntegerType(GetName g)
    {
        return "WebIDL.UnsignedIntegerType";
    }

    static TParseTree IntegerType(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("short"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("long"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("long"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("long"), Spacing)), "WebIDL.IntegerType")(p);
        }
        else
        {
            if (auto m = tuple(`IntegerType`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("short"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("long"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("long"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("long"), Spacing)), "WebIDL.IntegerType"), "IntegerType")(p);
                memo[tuple(`IntegerType`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree IntegerType(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("short"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("long"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("long"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("long"), Spacing)), "WebIDL.IntegerType")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("short"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("long"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("long"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("long"), Spacing)), "WebIDL.IntegerType"), "IntegerType")(TParseTree("", false,[], s));
        }
    }
    static string IntegerType(GetName g)
    {
        return "WebIDL.IntegerType";
    }

    static TParseTree StringType(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.keywords!("ByteString", "DOMString", "USVString", "CSSOMString"), pegged.peg.negLookahead!(Identifier)), "WebIDL.StringType")(p);
        }
        else
        {
            if (auto m = tuple(`StringType`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.keywords!("ByteString", "DOMString", "USVString", "CSSOMString"), pegged.peg.negLookahead!(Identifier)), "WebIDL.StringType"), "StringType")(p);
                memo[tuple(`StringType`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree StringType(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.keywords!("ByteString", "DOMString", "USVString", "CSSOMString"), pegged.peg.negLookahead!(Identifier)), "WebIDL.StringType")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.keywords!("ByteString", "DOMString", "USVString", "CSSOMString"), pegged.peg.negLookahead!(Identifier)), "WebIDL.StringType"), "StringType")(TParseTree("", false,[], s));
        }
    }
    static string StringType(GetName g)
    {
        return "WebIDL.StringType";
    }

    static TParseTree PromiseType(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("Promise"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<"), Spacing), pegged.peg.wrapAround!(Spacing, ReturnType, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">"), Spacing)), "WebIDL.PromiseType")(p);
        }
        else
        {
            if (auto m = tuple(`PromiseType`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("Promise"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<"), Spacing), pegged.peg.wrapAround!(Spacing, ReturnType, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">"), Spacing)), "WebIDL.PromiseType"), "PromiseType")(p);
                memo[tuple(`PromiseType`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree PromiseType(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("Promise"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<"), Spacing), pegged.peg.wrapAround!(Spacing, ReturnType, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">"), Spacing)), "WebIDL.PromiseType")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("Promise"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<"), Spacing), pegged.peg.wrapAround!(Spacing, ReturnType, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">"), Spacing)), "WebIDL.PromiseType"), "PromiseType")(TParseTree("", false,[], s));
        }
    }
    static string PromiseType(GetName g)
    {
        return "WebIDL.PromiseType";
    }

    static TParseTree RecordType(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("record"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<"), Spacing), pegged.peg.wrapAround!(Spacing, StringType, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, TypeWithExtendedAttributes, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">"), Spacing)), "WebIDL.RecordType")(p);
        }
        else
        {
            if (auto m = tuple(`RecordType`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("record"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<"), Spacing), pegged.peg.wrapAround!(Spacing, StringType, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, TypeWithExtendedAttributes, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">"), Spacing)), "WebIDL.RecordType"), "RecordType")(p);
                memo[tuple(`RecordType`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree RecordType(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("record"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<"), Spacing), pegged.peg.wrapAround!(Spacing, StringType, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, TypeWithExtendedAttributes, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">"), Spacing)), "WebIDL.RecordType")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("record"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<"), Spacing), pegged.peg.wrapAround!(Spacing, StringType, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, TypeWithExtendedAttributes, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">"), Spacing)), "WebIDL.RecordType"), "RecordType")(TParseTree("", false,[], s));
        }
    }
    static string RecordType(GetName g)
    {
        return "WebIDL.RecordType";
    }

    static TParseTree Null(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("?"), Spacing), pegged.peg.wrapAround!(Spacing, eps, Spacing)), "WebIDL.Null")(p);
        }
        else
        {
            if (auto m = tuple(`Null`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("?"), Spacing), pegged.peg.wrapAround!(Spacing, eps, Spacing)), "WebIDL.Null"), "Null")(p);
                memo[tuple(`Null`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Null(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("?"), Spacing), pegged.peg.wrapAround!(Spacing, eps, Spacing)), "WebIDL.Null")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("?"), Spacing), pegged.peg.wrapAround!(Spacing, eps, Spacing)), "WebIDL.Null"), "Null")(TParseTree("", false,[], s));
        }
    }
    static string Null(GetName g)
    {
        return "WebIDL.Null";
    }

    static TParseTree BufferRelatedType(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ArrayBuffer"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("DataView"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("Int8Array"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("Int16Array"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("Int32Array"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("Uint8Array"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("Uint16Array"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("Uint32Array"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("Uint8ClampedArray"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("Float32Array"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("Float64Array"), Spacing)), "WebIDL.BufferRelatedType")(p);
        }
        else
        {
            if (auto m = tuple(`BufferRelatedType`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ArrayBuffer"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("DataView"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("Int8Array"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("Int16Array"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("Int32Array"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("Uint8Array"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("Uint16Array"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("Uint32Array"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("Uint8ClampedArray"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("Float32Array"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("Float64Array"), Spacing)), "WebIDL.BufferRelatedType"), "BufferRelatedType")(p);
                memo[tuple(`BufferRelatedType`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree BufferRelatedType(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ArrayBuffer"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("DataView"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("Int8Array"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("Int16Array"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("Int32Array"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("Uint8Array"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("Uint16Array"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("Uint32Array"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("Uint8ClampedArray"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("Float32Array"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("Float64Array"), Spacing)), "WebIDL.BufferRelatedType")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ArrayBuffer"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("DataView"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("Int8Array"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("Int16Array"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("Int32Array"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("Uint8Array"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("Uint16Array"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("Uint32Array"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("Uint8ClampedArray"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("Float32Array"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("Float64Array"), Spacing)), "WebIDL.BufferRelatedType"), "BufferRelatedType")(TParseTree("", false,[], s));
        }
    }
    static string BufferRelatedType(GetName g)
    {
        return "WebIDL.BufferRelatedType";
    }

    static TParseTree ExtendedAttributeList(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, ExtendedAttribute, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, ExtendedAttribute, Spacing)), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, eps, Spacing)), "WebIDL.ExtendedAttributeList")(p);
        }
        else
        {
            if (auto m = tuple(`ExtendedAttributeList`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, ExtendedAttribute, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, ExtendedAttribute, Spacing)), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, eps, Spacing)), "WebIDL.ExtendedAttributeList"), "ExtendedAttributeList")(p);
                memo[tuple(`ExtendedAttributeList`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ExtendedAttributeList(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, ExtendedAttribute, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, ExtendedAttribute, Spacing)), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, eps, Spacing)), "WebIDL.ExtendedAttributeList")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, ExtendedAttribute, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, ExtendedAttribute, Spacing)), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, eps, Spacing)), "WebIDL.ExtendedAttributeList"), "ExtendedAttributeList")(TParseTree("", false,[], s));
        }
    }
    static string ExtendedAttributeList(GetName g)
    {
        return "WebIDL.ExtendedAttributeList";
    }

    static TParseTree ExtendedAttributeOuter(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, ExtendedAttributeInner, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, ExtendedAttributeInner, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, ExtendedAttributeInner, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, ExtendedAttribute, Spacing)), "WebIDL.ExtendedAttributeOuter")(p);
        }
        else
        {
            if (auto m = tuple(`ExtendedAttributeOuter`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, ExtendedAttributeInner, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, ExtendedAttributeInner, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, ExtendedAttributeInner, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, ExtendedAttribute, Spacing)), "WebIDL.ExtendedAttributeOuter"), "ExtendedAttributeOuter")(p);
                memo[tuple(`ExtendedAttributeOuter`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ExtendedAttributeOuter(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, ExtendedAttributeInner, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, ExtendedAttributeInner, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, ExtendedAttributeInner, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, ExtendedAttribute, Spacing)), "WebIDL.ExtendedAttributeOuter")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, ExtendedAttributeInner, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, ExtendedAttributeInner, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, ExtendedAttributeInner, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, ExtendedAttribute, Spacing)), "WebIDL.ExtendedAttributeOuter"), "ExtendedAttributeOuter")(TParseTree("", false,[], s));
        }
    }
    static string ExtendedAttributeOuter(GetName g)
    {
        return "WebIDL.ExtendedAttributeOuter";
    }

    static TParseTree ExtendedAttributeInner(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, ExtendedAttributeInner, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, ExtendedAttributeInner, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, ExtendedAttributeInner, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, OtherOrComma, Spacing)), Spacing)), "WebIDL.ExtendedAttributeInner")(p);
        }
        else
        {
            if (auto m = tuple(`ExtendedAttributeInner`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, ExtendedAttributeInner, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, ExtendedAttributeInner, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, ExtendedAttributeInner, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, OtherOrComma, Spacing)), Spacing)), "WebIDL.ExtendedAttributeInner"), "ExtendedAttributeInner")(p);
                memo[tuple(`ExtendedAttributeInner`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ExtendedAttributeInner(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, ExtendedAttributeInner, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, ExtendedAttributeInner, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, ExtendedAttributeInner, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, OtherOrComma, Spacing)), Spacing)), "WebIDL.ExtendedAttributeInner")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, ExtendedAttributeInner, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, ExtendedAttributeInner, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, ExtendedAttributeInner, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, OtherOrComma, Spacing)), Spacing)), "WebIDL.ExtendedAttributeInner"), "ExtendedAttributeInner")(TParseTree("", false,[], s));
        }
    }
    static string ExtendedAttributeInner(GetName g)
    {
        return "WebIDL.ExtendedAttributeInner";
    }

    static TParseTree ExtendedAttribute(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, ExtendedAttributeArgList, Spacing), pegged.peg.wrapAround!(Spacing, ExtendedAttributeNamedArgList, Spacing), pegged.peg.wrapAround!(Spacing, ExtendedAttributeIdent, Spacing), pegged.peg.wrapAround!(Spacing, ExtendedAttributeIdentList, Spacing), pegged.peg.wrapAround!(Spacing, ExtendedAttributeNoArgs, Spacing)), "WebIDL.ExtendedAttribute")(p);
        }
        else
        {
            if (auto m = tuple(`ExtendedAttribute`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, ExtendedAttributeArgList, Spacing), pegged.peg.wrapAround!(Spacing, ExtendedAttributeNamedArgList, Spacing), pegged.peg.wrapAround!(Spacing, ExtendedAttributeIdent, Spacing), pegged.peg.wrapAround!(Spacing, ExtendedAttributeIdentList, Spacing), pegged.peg.wrapAround!(Spacing, ExtendedAttributeNoArgs, Spacing)), "WebIDL.ExtendedAttribute"), "ExtendedAttribute")(p);
                memo[tuple(`ExtendedAttribute`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ExtendedAttribute(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, ExtendedAttributeArgList, Spacing), pegged.peg.wrapAround!(Spacing, ExtendedAttributeNamedArgList, Spacing), pegged.peg.wrapAround!(Spacing, ExtendedAttributeIdent, Spacing), pegged.peg.wrapAround!(Spacing, ExtendedAttributeIdentList, Spacing), pegged.peg.wrapAround!(Spacing, ExtendedAttributeNoArgs, Spacing)), "WebIDL.ExtendedAttribute")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, ExtendedAttributeArgList, Spacing), pegged.peg.wrapAround!(Spacing, ExtendedAttributeNamedArgList, Spacing), pegged.peg.wrapAround!(Spacing, ExtendedAttributeIdent, Spacing), pegged.peg.wrapAround!(Spacing, ExtendedAttributeIdentList, Spacing), pegged.peg.wrapAround!(Spacing, ExtendedAttributeNoArgs, Spacing)), "WebIDL.ExtendedAttribute"), "ExtendedAttribute")(TParseTree("", false,[], s));
        }
    }
    static string ExtendedAttribute(GetName g)
    {
        return "WebIDL.ExtendedAttribute";
    }

    static TParseTree Other(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ByteString"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("DOMString"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("FrozenArray"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("Infinity"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("NaN"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("USVString"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("any"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("boolean"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("byte"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("double"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("false"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("float"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("long"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("null"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("object"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("octet"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("or"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("optional"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("sequence"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("short"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("true"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("unsigned"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("void"), Spacing), pegged.peg.wrapAround!(Spacing, Integer, Spacing), pegged.peg.wrapAround!(Spacing, Float, Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, String, Spacing), pegged.peg.wrapAround!(Spacing, Other2, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("-"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("-Infinity"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("..."), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("?"), Spacing), pegged.peg.wrapAround!(Spacing, ArgumentNameKeyword, Spacing), pegged.peg.wrapAround!(Spacing, BufferRelatedType, Spacing)), "WebIDL.Other")(p);
        }
        else
        {
            if (auto m = tuple(`Other`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ByteString"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("DOMString"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("FrozenArray"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("Infinity"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("NaN"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("USVString"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("any"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("boolean"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("byte"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("double"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("false"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("float"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("long"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("null"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("object"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("octet"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("or"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("optional"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("sequence"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("short"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("true"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("unsigned"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("void"), Spacing), pegged.peg.wrapAround!(Spacing, Integer, Spacing), pegged.peg.wrapAround!(Spacing, Float, Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, String, Spacing), pegged.peg.wrapAround!(Spacing, Other2, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("-"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("-Infinity"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("..."), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("?"), Spacing), pegged.peg.wrapAround!(Spacing, ArgumentNameKeyword, Spacing), pegged.peg.wrapAround!(Spacing, BufferRelatedType, Spacing)), "WebIDL.Other"), "Other")(p);
                memo[tuple(`Other`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Other(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ByteString"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("DOMString"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("FrozenArray"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("Infinity"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("NaN"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("USVString"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("any"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("boolean"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("byte"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("double"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("false"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("float"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("long"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("null"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("object"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("octet"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("or"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("optional"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("sequence"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("short"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("true"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("unsigned"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("void"), Spacing), pegged.peg.wrapAround!(Spacing, Integer, Spacing), pegged.peg.wrapAround!(Spacing, Float, Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, String, Spacing), pegged.peg.wrapAround!(Spacing, Other2, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("-"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("-Infinity"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("..."), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("?"), Spacing), pegged.peg.wrapAround!(Spacing, ArgumentNameKeyword, Spacing), pegged.peg.wrapAround!(Spacing, BufferRelatedType, Spacing)), "WebIDL.Other")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ByteString"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("DOMString"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("FrozenArray"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("Infinity"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("NaN"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("USVString"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("any"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("boolean"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("byte"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("double"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("false"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("float"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("long"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("null"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("object"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("octet"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("or"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("optional"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("sequence"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("short"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("true"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("unsigned"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("void"), Spacing), pegged.peg.wrapAround!(Spacing, Integer, Spacing), pegged.peg.wrapAround!(Spacing, Float, Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, String, Spacing), pegged.peg.wrapAround!(Spacing, Other2, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("-"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("-Infinity"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("..."), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("?"), Spacing), pegged.peg.wrapAround!(Spacing, ArgumentNameKeyword, Spacing), pegged.peg.wrapAround!(Spacing, BufferRelatedType, Spacing)), "WebIDL.Other"), "Other")(TParseTree("", false,[], s));
        }
    }
    static string Other(GetName g)
    {
        return "WebIDL.Other";
    }

    static TParseTree OtherOrComma(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Other, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing)), "WebIDL.OtherOrComma")(p);
        }
        else
        {
            if (auto m = tuple(`OtherOrComma`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Other, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing)), "WebIDL.OtherOrComma"), "OtherOrComma")(p);
                memo[tuple(`OtherOrComma`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree OtherOrComma(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Other, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing)), "WebIDL.OtherOrComma")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Other, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing)), "WebIDL.OtherOrComma"), "OtherOrComma")(TParseTree("", false,[], s));
        }
    }
    static string OtherOrComma(GetName g)
    {
        return "WebIDL.OtherOrComma";
    }

    static TParseTree IdentifierList(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), Spacing))), "WebIDL.IdentifierList")(p);
        }
        else
        {
            if (auto m = tuple(`IdentifierList`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), Spacing))), "WebIDL.IdentifierList"), "IdentifierList")(p);
                memo[tuple(`IdentifierList`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree IdentifierList(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), Spacing))), "WebIDL.IdentifierList")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), Spacing))), "WebIDL.IdentifierList"), "IdentifierList")(TParseTree("", false,[], s));
        }
    }
    static string IdentifierList(GetName g)
    {
        return "WebIDL.IdentifierList";
    }

    static TParseTree Integer(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.and!(pegged.peg.option!(pegged.peg.literal!("-")), pegged.peg.and!(pegged.peg.charRange!('1', '9'), pegged.peg.zeroOrMore!(pegged.peg.charRange!('0', '9')))), pegged.peg.and!(pegged.peg.literal!("0"), pegged.peg.or!(pegged.peg.literal!("X"), pegged.peg.literal!("x")), pegged.peg.oneOrMore!(pegged.peg.or!(pegged.peg.charRange!('0', '9'), pegged.peg.charRange!('A', 'F'), pegged.peg.charRange!('a', 'f')))), pegged.peg.and!(pegged.peg.literal!("0"), pegged.peg.zeroOrMore!(pegged.peg.charRange!('0', '7'))))), "WebIDL.Integer")(p);
        }
        else
        {
            if (auto m = tuple(`Integer`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.and!(pegged.peg.option!(pegged.peg.literal!("-")), pegged.peg.and!(pegged.peg.charRange!('1', '9'), pegged.peg.zeroOrMore!(pegged.peg.charRange!('0', '9')))), pegged.peg.and!(pegged.peg.literal!("0"), pegged.peg.or!(pegged.peg.literal!("X"), pegged.peg.literal!("x")), pegged.peg.oneOrMore!(pegged.peg.or!(pegged.peg.charRange!('0', '9'), pegged.peg.charRange!('A', 'F'), pegged.peg.charRange!('a', 'f')))), pegged.peg.and!(pegged.peg.literal!("0"), pegged.peg.zeroOrMore!(pegged.peg.charRange!('0', '7'))))), "WebIDL.Integer"), "Integer")(p);
                memo[tuple(`Integer`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Integer(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.and!(pegged.peg.option!(pegged.peg.literal!("-")), pegged.peg.and!(pegged.peg.charRange!('1', '9'), pegged.peg.zeroOrMore!(pegged.peg.charRange!('0', '9')))), pegged.peg.and!(pegged.peg.literal!("0"), pegged.peg.or!(pegged.peg.literal!("X"), pegged.peg.literal!("x")), pegged.peg.oneOrMore!(pegged.peg.or!(pegged.peg.charRange!('0', '9'), pegged.peg.charRange!('A', 'F'), pegged.peg.charRange!('a', 'f')))), pegged.peg.and!(pegged.peg.literal!("0"), pegged.peg.zeroOrMore!(pegged.peg.charRange!('0', '7'))))), "WebIDL.Integer")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.and!(pegged.peg.option!(pegged.peg.literal!("-")), pegged.peg.and!(pegged.peg.charRange!('1', '9'), pegged.peg.zeroOrMore!(pegged.peg.charRange!('0', '9')))), pegged.peg.and!(pegged.peg.literal!("0"), pegged.peg.or!(pegged.peg.literal!("X"), pegged.peg.literal!("x")), pegged.peg.oneOrMore!(pegged.peg.or!(pegged.peg.charRange!('0', '9'), pegged.peg.charRange!('A', 'F'), pegged.peg.charRange!('a', 'f')))), pegged.peg.and!(pegged.peg.literal!("0"), pegged.peg.zeroOrMore!(pegged.peg.charRange!('0', '7'))))), "WebIDL.Integer"), "Integer")(TParseTree("", false,[], s));
        }
    }
    static string Integer(GetName g)
    {
        return "WebIDL.Integer";
    }

    static TParseTree Float(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.and!(pegged.peg.option!(pegged.peg.literal!("-")), pegged.peg.and!(pegged.peg.or!(pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.charRange!('0', '9')), pegged.peg.literal!("."), pegged.peg.zeroOrMore!(pegged.peg.charRange!('0', '9'))), pegged.peg.and!(pegged.peg.zeroOrMore!(pegged.peg.charRange!('0', '9')), pegged.peg.literal!("."), pegged.peg.oneOrMore!(pegged.peg.charRange!('0', '9')))), pegged.peg.option!(pegged.peg.and!(pegged.peg.keywords!("E", "e"), pegged.peg.option!(pegged.peg.keywords!("+", "-")), pegged.peg.oneOrMore!(pegged.peg.charRange!('0', '9')))))), pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.charRange!('0', '9')), pegged.peg.keywords!("E", "e"), pegged.peg.option!(pegged.peg.keywords!("+", "-")), pegged.peg.oneOrMore!(pegged.peg.charRange!('0', '9'))))), "WebIDL.Float")(p);
        }
        else
        {
            if (auto m = tuple(`Float`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.and!(pegged.peg.option!(pegged.peg.literal!("-")), pegged.peg.and!(pegged.peg.or!(pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.charRange!('0', '9')), pegged.peg.literal!("."), pegged.peg.zeroOrMore!(pegged.peg.charRange!('0', '9'))), pegged.peg.and!(pegged.peg.zeroOrMore!(pegged.peg.charRange!('0', '9')), pegged.peg.literal!("."), pegged.peg.oneOrMore!(pegged.peg.charRange!('0', '9')))), pegged.peg.option!(pegged.peg.and!(pegged.peg.keywords!("E", "e"), pegged.peg.option!(pegged.peg.keywords!("+", "-")), pegged.peg.oneOrMore!(pegged.peg.charRange!('0', '9')))))), pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.charRange!('0', '9')), pegged.peg.keywords!("E", "e"), pegged.peg.option!(pegged.peg.keywords!("+", "-")), pegged.peg.oneOrMore!(pegged.peg.charRange!('0', '9'))))), "WebIDL.Float"), "Float")(p);
                memo[tuple(`Float`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Float(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.and!(pegged.peg.option!(pegged.peg.literal!("-")), pegged.peg.and!(pegged.peg.or!(pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.charRange!('0', '9')), pegged.peg.literal!("."), pegged.peg.zeroOrMore!(pegged.peg.charRange!('0', '9'))), pegged.peg.and!(pegged.peg.zeroOrMore!(pegged.peg.charRange!('0', '9')), pegged.peg.literal!("."), pegged.peg.oneOrMore!(pegged.peg.charRange!('0', '9')))), pegged.peg.option!(pegged.peg.and!(pegged.peg.keywords!("E", "e"), pegged.peg.option!(pegged.peg.keywords!("+", "-")), pegged.peg.oneOrMore!(pegged.peg.charRange!('0', '9')))))), pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.charRange!('0', '9')), pegged.peg.keywords!("E", "e"), pegged.peg.option!(pegged.peg.keywords!("+", "-")), pegged.peg.oneOrMore!(pegged.peg.charRange!('0', '9'))))), "WebIDL.Float")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.and!(pegged.peg.option!(pegged.peg.literal!("-")), pegged.peg.and!(pegged.peg.or!(pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.charRange!('0', '9')), pegged.peg.literal!("."), pegged.peg.zeroOrMore!(pegged.peg.charRange!('0', '9'))), pegged.peg.and!(pegged.peg.zeroOrMore!(pegged.peg.charRange!('0', '9')), pegged.peg.literal!("."), pegged.peg.oneOrMore!(pegged.peg.charRange!('0', '9')))), pegged.peg.option!(pegged.peg.and!(pegged.peg.keywords!("E", "e"), pegged.peg.option!(pegged.peg.keywords!("+", "-")), pegged.peg.oneOrMore!(pegged.peg.charRange!('0', '9')))))), pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.charRange!('0', '9')), pegged.peg.keywords!("E", "e"), pegged.peg.option!(pegged.peg.keywords!("+", "-")), pegged.peg.oneOrMore!(pegged.peg.charRange!('0', '9'))))), "WebIDL.Float"), "Float")(TParseTree("", false,[], s));
        }
    }
    static string Float(GetName g)
    {
        return "WebIDL.Float";
    }

    static TParseTree Identifier(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.option!(pegged.peg.literal!("_")), pegged.peg.or!(pegged.peg.charRange!('A', 'Z'), pegged.peg.charRange!('a', 'z')), pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.charRange!('0', '9'), pegged.peg.charRange!('A', 'Z'), pegged.peg.literal!("_"), pegged.peg.charRange!('a', 'z'), pegged.peg.literal!("-"))))), "WebIDL.Identifier")(p);
        }
        else
        {
            if (auto m = tuple(`Identifier`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.option!(pegged.peg.literal!("_")), pegged.peg.or!(pegged.peg.charRange!('A', 'Z'), pegged.peg.charRange!('a', 'z')), pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.charRange!('0', '9'), pegged.peg.charRange!('A', 'Z'), pegged.peg.literal!("_"), pegged.peg.charRange!('a', 'z'), pegged.peg.literal!("-"))))), "WebIDL.Identifier"), "Identifier")(p);
                memo[tuple(`Identifier`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Identifier(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.option!(pegged.peg.literal!("_")), pegged.peg.or!(pegged.peg.charRange!('A', 'Z'), pegged.peg.charRange!('a', 'z')), pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.charRange!('0', '9'), pegged.peg.charRange!('A', 'Z'), pegged.peg.literal!("_"), pegged.peg.charRange!('a', 'z'), pegged.peg.literal!("-"))))), "WebIDL.Identifier")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.option!(pegged.peg.literal!("_")), pegged.peg.or!(pegged.peg.charRange!('A', 'Z'), pegged.peg.charRange!('a', 'z')), pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.charRange!('0', '9'), pegged.peg.charRange!('A', 'Z'), pegged.peg.literal!("_"), pegged.peg.charRange!('a', 'z'), pegged.peg.literal!("-"))))), "WebIDL.Identifier"), "Identifier")(TParseTree("", false,[], s));
        }
    }
    static string Identifier(GetName g)
    {
        return "WebIDL.Identifier";
    }

    static TParseTree String(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(doublequote, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(doublequote), pegged.peg.any)), doublequote)), "WebIDL.String")(p);
        }
        else
        {
            if (auto m = tuple(`String`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(doublequote, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(doublequote), pegged.peg.any)), doublequote)), "WebIDL.String"), "String")(p);
                memo[tuple(`String`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree String(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(doublequote, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(doublequote), pegged.peg.any)), doublequote)), "WebIDL.String")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(doublequote, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(doublequote), pegged.peg.any)), doublequote)), "WebIDL.String"), "String")(TParseTree("", false,[], s));
        }
    }
    static string String(GetName g)
    {
        return "WebIDL.String";
    }

    static TParseTree Whitespace(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.discard!(pegged.peg.oneOrMore!(pegged.peg.or!(pegged.peg.literal!(" "), pegged.peg.literal!("\t"), EndOfLine))), "WebIDL.Whitespace")(p);
        }
        else
        {
            if (auto m = tuple(`Whitespace`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.discard!(pegged.peg.oneOrMore!(pegged.peg.or!(pegged.peg.literal!(" "), pegged.peg.literal!("\t"), EndOfLine))), "WebIDL.Whitespace"), "Whitespace")(p);
                memo[tuple(`Whitespace`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Whitespace(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.discard!(pegged.peg.oneOrMore!(pegged.peg.or!(pegged.peg.literal!(" "), pegged.peg.literal!("\t"), EndOfLine))), "WebIDL.Whitespace")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.discard!(pegged.peg.oneOrMore!(pegged.peg.or!(pegged.peg.literal!(" "), pegged.peg.literal!("\t"), EndOfLine))), "WebIDL.Whitespace"), "Whitespace")(TParseTree("", false,[], s));
        }
    }
    static string Whitespace(GetName g)
    {
        return "WebIDL.Whitespace";
    }

    static TParseTree EndOfLine(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.discard!(pegged.peg.or!(pegged.peg.and!(pegged.peg.literal!("\r"), pegged.peg.literal!("\n")), pegged.peg.literal!("\n"))), "WebIDL.EndOfLine")(p);
        }
        else
        {
            if (auto m = tuple(`EndOfLine`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.discard!(pegged.peg.or!(pegged.peg.and!(pegged.peg.literal!("\r"), pegged.peg.literal!("\n")), pegged.peg.literal!("\n"))), "WebIDL.EndOfLine"), "EndOfLine")(p);
                memo[tuple(`EndOfLine`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree EndOfLine(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.discard!(pegged.peg.or!(pegged.peg.and!(pegged.peg.literal!("\r"), pegged.peg.literal!("\n")), pegged.peg.literal!("\n"))), "WebIDL.EndOfLine")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.discard!(pegged.peg.or!(pegged.peg.and!(pegged.peg.literal!("\r"), pegged.peg.literal!("\n")), pegged.peg.literal!("\n"))), "WebIDL.EndOfLine"), "EndOfLine")(TParseTree("", false,[], s));
        }
    }
    static string EndOfLine(GetName g)
    {
        return "WebIDL.EndOfLine";
    }

    static TParseTree Comment(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.option!(Whitespace), pegged.peg.or!(pegged.peg.and!(pegged.peg.literal!("//"), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(EndOfLine), pegged.peg.any))), pegged.peg.and!(pegged.peg.literal!("/*"), pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("*/")), pegged.peg.any), EndOfLine)), pegged.peg.literal!("*/"))), pegged.peg.option!(Whitespace)))), "WebIDL.Comment")(p);
        }
        else
        {
            if (auto m = tuple(`Comment`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.option!(Whitespace), pegged.peg.or!(pegged.peg.and!(pegged.peg.literal!("//"), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(EndOfLine), pegged.peg.any))), pegged.peg.and!(pegged.peg.literal!("/*"), pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("*/")), pegged.peg.any), EndOfLine)), pegged.peg.literal!("*/"))), pegged.peg.option!(Whitespace)))), "WebIDL.Comment"), "Comment")(p);
                memo[tuple(`Comment`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Comment(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.option!(Whitespace), pegged.peg.or!(pegged.peg.and!(pegged.peg.literal!("//"), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(EndOfLine), pegged.peg.any))), pegged.peg.and!(pegged.peg.literal!("/*"), pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("*/")), pegged.peg.any), EndOfLine)), pegged.peg.literal!("*/"))), pegged.peg.option!(Whitespace)))), "WebIDL.Comment")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.option!(Whitespace), pegged.peg.or!(pegged.peg.and!(pegged.peg.literal!("//"), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(EndOfLine), pegged.peg.any))), pegged.peg.and!(pegged.peg.literal!("/*"), pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("*/")), pegged.peg.any), EndOfLine)), pegged.peg.literal!("*/"))), pegged.peg.option!(Whitespace)))), "WebIDL.Comment"), "Comment")(TParseTree("", false,[], s));
        }
    }
    static string Comment(GetName g)
    {
        return "WebIDL.Comment";
    }

    static TParseTree Spacing(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.discard!(pegged.peg.zeroOrMore!(pegged.peg.or!(Whitespace, Comment))), "WebIDL.Spacing")(p);
        }
        else
        {
            if (auto m = tuple(`Spacing`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.discard!(pegged.peg.zeroOrMore!(pegged.peg.or!(Whitespace, Comment))), "WebIDL.Spacing"), "Spacing")(p);
                memo[tuple(`Spacing`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Spacing(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.discard!(pegged.peg.zeroOrMore!(pegged.peg.or!(Whitespace, Comment))), "WebIDL.Spacing")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.discard!(pegged.peg.zeroOrMore!(pegged.peg.or!(Whitespace, Comment))), "WebIDL.Spacing"), "Spacing")(TParseTree("", false,[], s));
        }
    }
    static string Spacing(GetName g)
    {
        return "WebIDL.Spacing";
    }

    static TParseTree Other2(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.literal!("^"), pegged.peg.literal!("\t"), pegged.peg.literal!("\n"), pegged.peg.literal!("\r"), pegged.peg.literal!(" "), pegged.peg.charRange!('0', '9'), pegged.peg.charRange!('A', 'Z'), pegged.peg.charRange!('a', 'z'))), "WebIDL.Other2")(p);
        }
        else
        {
            if (auto m = tuple(`Other2`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.literal!("^"), pegged.peg.literal!("\t"), pegged.peg.literal!("\n"), pegged.peg.literal!("\r"), pegged.peg.literal!(" "), pegged.peg.charRange!('0', '9'), pegged.peg.charRange!('A', 'Z'), pegged.peg.charRange!('a', 'z'))), "WebIDL.Other2"), "Other2")(p);
                memo[tuple(`Other2`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Other2(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.literal!("^"), pegged.peg.literal!("\t"), pegged.peg.literal!("\n"), pegged.peg.literal!("\r"), pegged.peg.literal!(" "), pegged.peg.charRange!('0', '9'), pegged.peg.charRange!('A', 'Z'), pegged.peg.charRange!('a', 'z'))), "WebIDL.Other2")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.literal!("^"), pegged.peg.literal!("\t"), pegged.peg.literal!("\n"), pegged.peg.literal!("\r"), pegged.peg.literal!(" "), pegged.peg.charRange!('0', '9'), pegged.peg.charRange!('A', 'Z'), pegged.peg.charRange!('a', 'z'))), "WebIDL.Other2"), "Other2")(TParseTree("", false,[], s));
        }
    }
    static string Other2(GetName g)
    {
        return "WebIDL.Other2";
    }

    static TParseTree ArgumentNameKeyword(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.keywords!("attribute", "callback", "const", "deleter", "dictionary", "enum", "getter", "includes", "inherit", "interface", "iterable", "maplike", "namespace", "partial", "required", "setlike", "setter", "static", "stringifier", "typedef", "unrestricted"), pegged.peg.negLookahead!(Identifier)), "WebIDL.ArgumentNameKeyword")(p);
        }
        else
        {
            if (auto m = tuple(`ArgumentNameKeyword`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.keywords!("attribute", "callback", "const", "deleter", "dictionary", "enum", "getter", "includes", "inherit", "interface", "iterable", "maplike", "namespace", "partial", "required", "setlike", "setter", "static", "stringifier", "typedef", "unrestricted"), pegged.peg.negLookahead!(Identifier)), "WebIDL.ArgumentNameKeyword"), "ArgumentNameKeyword")(p);
                memo[tuple(`ArgumentNameKeyword`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ArgumentNameKeyword(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.keywords!("attribute", "callback", "const", "deleter", "dictionary", "enum", "getter", "includes", "inherit", "interface", "iterable", "maplike", "namespace", "partial", "required", "setlike", "setter", "static", "stringifier", "typedef", "unrestricted"), pegged.peg.negLookahead!(Identifier)), "WebIDL.ArgumentNameKeyword")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.keywords!("attribute", "callback", "const", "deleter", "dictionary", "enum", "getter", "includes", "inherit", "interface", "iterable", "maplike", "namespace", "partial", "required", "setlike", "setter", "static", "stringifier", "typedef", "unrestricted"), pegged.peg.negLookahead!(Identifier)), "WebIDL.ArgumentNameKeyword"), "ArgumentNameKeyword")(TParseTree("", false,[], s));
        }
    }
    static string ArgumentNameKeyword(GetName g)
    {
        return "WebIDL.ArgumentNameKeyword";
    }

    static TParseTree ExtendedAttributeNoArgs(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), "WebIDL.ExtendedAttributeNoArgs")(p);
        }
        else
        {
            if (auto m = tuple(`ExtendedAttributeNoArgs`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), "WebIDL.ExtendedAttributeNoArgs"), "ExtendedAttributeNoArgs")(p);
                memo[tuple(`ExtendedAttributeNoArgs`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ExtendedAttributeNoArgs(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), "WebIDL.ExtendedAttributeNoArgs")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), "WebIDL.ExtendedAttributeNoArgs"), "ExtendedAttributeNoArgs")(TParseTree("", false,[], s));
        }
    }
    static string ExtendedAttributeNoArgs(GetName g)
    {
        return "WebIDL.ExtendedAttributeNoArgs";
    }

    static TParseTree ExtendedAttributeArgList(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, ArgumentList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "WebIDL.ExtendedAttributeArgList")(p);
        }
        else
        {
            if (auto m = tuple(`ExtendedAttributeArgList`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, ArgumentList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "WebIDL.ExtendedAttributeArgList"), "ExtendedAttributeArgList")(p);
                memo[tuple(`ExtendedAttributeArgList`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ExtendedAttributeArgList(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, ArgumentList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "WebIDL.ExtendedAttributeArgList")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, ArgumentList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "WebIDL.ExtendedAttributeArgList"), "ExtendedAttributeArgList")(TParseTree("", false,[], s));
        }
    }
    static string ExtendedAttributeArgList(GetName g)
    {
        return "WebIDL.ExtendedAttributeArgList";
    }

    static TParseTree ExtendedAttributeIdent(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), "WebIDL.ExtendedAttributeIdent")(p);
        }
        else
        {
            if (auto m = tuple(`ExtendedAttributeIdent`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), "WebIDL.ExtendedAttributeIdent"), "ExtendedAttributeIdent")(p);
                memo[tuple(`ExtendedAttributeIdent`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ExtendedAttributeIdent(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), "WebIDL.ExtendedAttributeIdent")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), "WebIDL.ExtendedAttributeIdent"), "ExtendedAttributeIdent")(TParseTree("", false,[], s));
        }
    }
    static string ExtendedAttributeIdent(GetName g)
    {
        return "WebIDL.ExtendedAttributeIdent";
    }

    static TParseTree ExtendedAttributeIdentList(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, IdentifierList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "WebIDL.ExtendedAttributeIdentList")(p);
        }
        else
        {
            if (auto m = tuple(`ExtendedAttributeIdentList`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, IdentifierList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "WebIDL.ExtendedAttributeIdentList"), "ExtendedAttributeIdentList")(p);
                memo[tuple(`ExtendedAttributeIdentList`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ExtendedAttributeIdentList(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, IdentifierList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "WebIDL.ExtendedAttributeIdentList")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, IdentifierList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "WebIDL.ExtendedAttributeIdentList"), "ExtendedAttributeIdentList")(TParseTree("", false,[], s));
        }
    }
    static string ExtendedAttributeIdentList(GetName g)
    {
        return "WebIDL.ExtendedAttributeIdentList";
    }

    static TParseTree ExtendedAttributeNamedArgList(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, ArgumentList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "WebIDL.ExtendedAttributeNamedArgList")(p);
        }
        else
        {
            if (auto m = tuple(`ExtendedAttributeNamedArgList`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, ArgumentList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "WebIDL.ExtendedAttributeNamedArgList"), "ExtendedAttributeNamedArgList")(p);
                memo[tuple(`ExtendedAttributeNamedArgList`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ExtendedAttributeNamedArgList(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, ArgumentList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "WebIDL.ExtendedAttributeNamedArgList")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, ArgumentList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "WebIDL.ExtendedAttributeNamedArgList"), "ExtendedAttributeNamedArgList")(TParseTree("", false,[], s));
        }
    }
    static string ExtendedAttributeNamedArgList(GetName g)
    {
        return "WebIDL.ExtendedAttributeNamedArgList";
    }

    static TParseTree opCall(TParseTree p)
    {
        TParseTree result = decimateTree(Definitions(p));
        result.children = [result];
        result.name = "WebIDL";
        return result;
    }

    static TParseTree opCall(string input)
    {
        if(__ctfe)
        {
            return WebIDL(TParseTree(``, false, [], input, 0, 0));
        }
        else
        {
            forgetMemo();
            return WebIDL(TParseTree(``, false, [], input, 0, 0));
        }
    }
    static string opCall(GetName g)
    {
        return "WebIDL";
    }


    static void forgetMemo()
    {
        memo = null;
    }
    }
}

alias GenericWebIDL!(ParseTree).WebIDL WebIDL;

