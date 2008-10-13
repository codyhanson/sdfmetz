module Language.Antlr.Syntax where
 
data GrammarHeader = GrammarHeader (Maybe STRING_LITERAL) ACTION
 
data Grammar = Grammar [GrammarHeader] (Maybe OptionsSpec)
                       [ClassDef]
 
data ClassDef = ClassDef (Maybe ACTION) (Maybe DOC_COMMENT) Spec
                         [Rule]
 
data OptionAssignmentSemi = OptionAssignmentSemi Id OptionValue
 
data OptionAssignment = OptionAssignment Id OptionValue
 
data OptionsSpec = OptionsSpec [OptionAssignmentSemi]
 
data SingleOptionSpec = LexOption CharSet
                      | Option Id OptionValue
 
data LexerOptionsSpec = LexerOptionsSpec [SingleOptionSpec]
 
data OptionValue = IdValue QualifiedID
                 | StringValue STRING_LITERAL
                 | CharValue CHAR_LITERAL
                 | IntValue INT
 
data CharSet = CharSet [SetBlockElement]
 
data SetBlockElement = SetBlockElement CHAR_LITERAL
                                       (Maybe (CHAR_LITERAL))
 
data SingleTokenSpec = TokenRefSpec TOKEN_REF
                                    (Maybe (STRING_LITERAL)) (Maybe TokensSpecOptions)
                     | StringLitTokenSpec STRING_LITERAL (Maybe TokensSpecOptions)
 
data TokensSpec = TokensSpec [(SingleTokenSpec)]
 
data TokensSpecOptions = TokensSpecOptions [OptionAssignment]
 
data SuperClass = SuperClass STRING_LITERAL
 
data ClassHeader = LexClassHeader Id
                 | ClassHeader Id Id (Maybe SuperClass)
 
data Spec = Spec ClassHeader (Maybe LexerOptionsSpec)
                 (Maybe TokensSpec) (Maybe ACTION)
 
data Rule = Rule (Maybe DOC_COMMENT) (Maybe Modifier) Id
                 (Maybe BANG) (Maybe ARG_ACTION) (Maybe (ARG_ACTION))
                 (Maybe ThrowsSpec) (Maybe OptionsSpec) (Maybe ACTION) Block
                 [ExceptionSpec]
 
data ThrowsSpec = ThrowsSpec [Id]
 
data Block = Block [Alternative]
 
data Alternative = Alternative (Maybe BANG) [Element]
                               (Maybe ExceptionSpecNoLabel)
 
data ExceptionSpec = ExceptionSpec (Maybe ARG_ACTION)
                                   [ExceptionHandler]
 
data ExceptionSpecNoLabel = ExceptionSpecNoLabel [ExceptionHandler]
 
data ExceptionHandler = ExceptionHandler ARG_ACTION ACTION
 
data Element = Element ElementNoOptionSpec
                       (Maybe ElementOptionSpec)
 
data ElementOptionSpec = ElementOptionSpec [OptionAssignment]
 
data AssignElementBody = RuleRefElt RULE_REF (Maybe ARG_ACTION)
                                    (Maybe BANG)
                       | TokenRefElt TOKEN_REF (Maybe ARG_ACTION)
 
data NegatedElement = NegatedNotTerminal NotTerminal
                    | NegatedEbnf Ebnf
 
data NoAssignElementBody = RuleRefEltNoAssign RULE_REF
                                              (Maybe ARG_ACTION) (Maybe BANG)
                         | RangeElt Range
                         | TerminalElt Terminal
                         | NegatedElt NegatedElement
                         | EbnfElt Ebnf
 
data ElementNoOptionSpec = AssignElement Id (Maybe (Id))
                                         AssignElementBody
                         | NoAssignElement (Maybe (Id)) NoAssignElementBody
                         | ActionElement ACTION
                         | SemPredElement SEMPRED
                         | TreeElement Tree
 
data Tree = Tree RootNode [Element]
 
data RootNode = RootNode (Maybe (Id)) Terminal
 
data OptionsSpecOrAction = OptionsSpecAndMaybeAction OptionsSpec
                                                     (Maybe ACTION)
                         | Action ACTION
 
data RegExpOperator = Question QUESTION
                    | Star STAR
                    | Plus PLUS
 
data EbnfBody = EbnfBodyRegExp (Maybe RegExpOperator) (Maybe BANG)
              | EbnfBodyImplies IMPLIES
 
data Ebnf = Ebnf (Maybe OptionsSpecOrAction) Block EbnfBody
 
data Ast_type_spec = CaretAstType CARET
                   | BangAstType BANG
                   | NoAstType
 
data TokenRefOrStringLit = TokenRef TOKEN_REF
                         | StringLit STRING_LITERAL
 
data Range = CharRange CHAR_LITERAL CHAR_LITERAL (Maybe BANG)
           | StringTokenRange TokenRefOrStringLit TokenRefOrStringLit
                              Ast_type_spec
 
data Terminal = CharLiteralT CHAR_LITERAL (Maybe BANG)
              | TokenRefT TOKEN_REF Ast_type_spec (Maybe ARG_ACTION)
              | StringLiteralT STRING_LITERAL Ast_type_spec
              | WildCardT Ast_type_spec
 
data NotTerminal = CharLiteralNT CHAR_LITERAL (Maybe BANG)
                 | TokenRefNT TOKEN_REF Ast_type_spec
 
data QualifiedID = QualifiedID [Id]
 
data Id = TokenRefId TOKEN_REF
        | RuleRefId RULE_REF
 
type COMMENT = String
 
type SL_COMMENT = String
 
type CR = String
 
type CRLF = String
 
type LF = String
 
type ML_COMMENT = String
 
type DOC_COMMENT = String
 
type Asterisk = String
 
type QUESTION = String
 
type STAR = String
 
type PLUS = String
 
type IMPLIES = String
 
type CARET = String
 
type BANG = String
 
type CHAR_LITERAL = String
 
type STRING_LITERAL = String
 
type ESC = String
 
type DIGIT = String
 
type XDIGIT = String
 
type INT = String
 
type NotFollowedBy0_7 = String
 
type ARG_ACTION = String
 
type NESTED_ARG_ACTION = String
 
type NotRBRACKET = String
 
type ACTION = String
 
type SEMPRED = String
 
type NESTED_ACTION = String
 
type NotRCURLY = String
 
type TOKEN_REF = String
 
type RULE_REF = String
 
type INTERNAL_RULE_REF = String
 
type Modifier = String