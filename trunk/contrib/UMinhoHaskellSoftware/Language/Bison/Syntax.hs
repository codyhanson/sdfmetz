module Language.Bison.Syntax where
 
data BisonSyntax = BisonSyntax [Declaration] Grammar
                               (Maybe (Epilogue))
 
data Declaration = GrammarDecl GrammarDeclaration
                 | PrologueDecl Prologue
                 | DebugDecl
                 | DefineDecl StrLex StrLex
                 | DefinesDecl
                 | ErrorVerboseDecl
                 | ExpectDecl UInt
                 | ExpectRRDecl ExpectRR UInt
                 | FilePrefixDecl StrLex
                 | GlrParserDecl
                 | InitialActionDecl BracedCode
                 | LexParamDecl BracedCode
                 | LocationsDecl
                 | NamePrefixDecl NamePrefix StrLex
                 | NoLinesDecl NoLines
                 | NonDetParserDecl
                 | OutputDecl StrLex
                 | ParseParamDecl BracedCode
                 | PureParserDecl PureParser
                 | SkeletonDecl StrLex
                 | TokenTableDecl TokenTable
                 | VerboseDecl
                 | YaccDecl
                 | SemiColon
 
data GrammarDeclaration = LeftPriorDecl (Maybe Type) [Symbol]
                        | RightPriorDecl (Maybe Type) [Symbol]
                        | BinaryPriorDecl (Maybe Type) [Symbol]
                        | NonAssocPriorDecl (Maybe Type) [Symbol]
                        | NTermDecl [SymbolDef]
                        | TermDecl [SymbolDef]
                        | TokenDecl Token [SymbolDef]
                        | TypeDecl Type [Symbol]
                        | StartDecl Symbol
                        | UnionDecl BracedCode
                        | DestructorDecl BracedCode [Symbol]
                        | PrinterDecl BracedCode [Symbol]
                        | DefaultPrecDecl DefaultPrec
                        | NoDefaultPrecDecl NoDefaultPrec
 
data Type = Type Tag
 
data SymbolDef = TypeDef Type
               | IdDef Id
               | IdIntDef Id UInt
               | IdStrLexDef Id StrLex
               | IdIntStrLexDef Id UInt StrLex
 
data Grammar = Grammar [RulesOrGrammarDeclaration]
 
data RulesOrGrammarDeclaration = RulesOrGrammarDeclaration_1 Rules
                               | RulesOrGrammarDeclaration_2 GrammarDeclaration
 
data Rules = Rules Id Rhses
 
data Rhses = Rhses_1 Rhs
           | Rhses_2 Rhses Rhs
           | Rhses_3 Rhses
 
data Rhs = Rhs [RhsSymb]
 
data RhsSymb = Symb Symbol
             | Code BracedCode
             | PrecDecl Symbol
             | DPrecDecl UInt
             | MergeDecl Type
 
data Symbol = SymbolId Id
            | SymbolStrLex StrLex
            | SymbolChar Character
 
type Token = String
 
type ExpectRR = String
 
type NamePrefix = String
 
type NoLines = String
 
type PureParser = String
 
type TokenTable = String
 
type DefaultPrec = String
 
type NoDefaultPrec = String
 
type Letter = String
 
type Id = String
 
type UInt = String
 
type Tag = String
 
type Character = String
 
type StrLex = String
 
type ESC = String
 
type XDIGIT = String
 
type NotFollowedBy0_7 = String
 
type NotFollowedByXDIGIT = String
 
type Prologue = String
 
type Percent = String
 
type LonelyDoubleQuote = String
 
type LonelyQuote = String
 
type LonelySlash = String
 
type BracedCode = String
 
type Epilogue = String
 
type Comment = String
 
type SingleLineComment = String
 
type CR = String
 
type CRLF = String
 
type LF = String
 
type MultiLineComment = String
 
type Asterisk = String
 
type LineDirective = String