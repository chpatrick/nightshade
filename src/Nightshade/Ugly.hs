{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Nightshade.Ugly where

import Control.Lens hiding (indexing)
import Data.Data
import Data.Data.Lens
import qualified Data.HashMap.Strict as HMS
import Language.GLSL.Syntax
import Text.PrettyPrint.HughesPJClass
import Text.Printf

import Nightshade.Analysis
import Nightshade.Types

shortNames :: [ String ]
shortNames = map (:[]) alphabet ++ (flip (:) <$> shortNames <*> alphabet)
  where
    alphabet = ['a'..'z'] ++ ['A'..'Z']

type NameMapping = HMS.HashMap String String

-- | Assign short names to the names that are only declared internally.
nameMapping :: [ Name ] -> NameMapping
nameMapping names = HMS.fromList $ zip internal shortNames
  where
    -- Find the names that are never external.
    internal = [ n | ( n, e ) <- HMS.toList $ HMS.fromListWith (||) names, not e ]

applyMapping :: Data s => NameMapping -> s -> s
applyMapping nm = updateFuncs . updateExprs . updateDecls
  where
    mapName n = HMS.lookupDefault n n nm
    updateDecls = over template (\(InitDecl n ce ae) -> InitDecl (mapName n) ce ae)
    updateExprs = transformOnOf template uniplate $ \case
      Variable n -> Variable (HMS.lookupDefault n n nm)
      FunctionCall (FuncId n) ps -> FunctionCall (FuncId (mapName n)) ps
      e -> e
    updateParam = \case
      ParameterDeclaration a b c (Just ( n, d ))
        -> ParameterDeclaration a b c (Just ( mapName n, d ))
      pd -> pd
    updateFuncs = over template $ \(FuncProt t fn params) ->
      FuncProt t (mapName fn) (map updateParam params)

uglify :: TranslationUnit -> String
uglify tu = renderStyle style { mode = OneLineMode } $ uglyPrint renamed
  where
    decls = findDeclarations tu
    nm = nameMapping decls
    renamed = applyMapping nm tu

class Ugly a where
    uglyPrintPrec :: PrettyLevel -> Rational -> a -> Doc
    uglyPrintPrec _ _ = uglyPrint

    uglyPrint :: a -> Doc
    uglyPrint = uglyPrintPrec prettyNormal 0

uglyBinary :: Ugly a =>
  PrettyLevel -> Rational -> Rational -> String -> a -> a -> Doc
uglyBinary l p op_ o e1 e2 = maybeParens (p > op_) $
  uglyPrintPrec l op_ e1 <> text o <> uglyPrintPrec l op_ e2

option :: Ugly a => Maybe a -> Doc
option Nothing = empty
option (Just x) = uglyPrint x

indexing :: Ugly a => Maybe (Maybe a) -> Doc
indexing Nothing = empty
indexing (Just Nothing) = brackets empty
indexing (Just (Just e)) = brackets $ uglyPrint e

indexing' :: Ugly a => Maybe (String, Maybe a) -> Doc
indexing' Nothing = empty
indexing' (Just (i, Nothing)) = text i
indexing' (Just (i, Just e)) = text i <> brackets (uglyPrint e)

initialize :: Ugly a => Maybe a -> Doc
initialize Nothing = empty
initialize (Just e) = equals <> uglyPrint e

ident :: Ugly a => Maybe (String, Maybe (Maybe a)) -> Doc
ident Nothing = empty
ident (Just (i, Nothing)) = text i
ident (Just (i, Just Nothing)) = text i <> brackets empty
ident (Just (i, Just (Just e))) = text i <> brackets (uglyPrint e)

initialize' :: Ugly a => Maybe (String, Maybe a) -> Doc
initialize' Nothing = empty
initialize' (Just (i, Nothing)) = text i
initialize' (Just (i, Just e)) = text i <> char '=' <> uglyPrint e

----------------------------------------------------------------------
-- Ugly instances
----------------------------------------------------------------------

instance Ugly String where
  uglyPrint = pPrint
  uglyPrintPrec = pPrintPrec

instance Ugly TranslationUnit where
  uglyPrint (TranslationUnit ds) = hcat $ map uglyPrint ds
--  uglyPrint (Alternative p e) = text "(" <> nest 2 (hcat [uglyPrint p, uglyPrint e]) <> text ")"

instance Ugly ExternalDeclaration where
  uglyPrint (FunctionDeclaration p) = uglyPrint p <> semi
  uglyPrint (FunctionDefinition p s) = hcat [uglyPrint p, uglyPrint s]
  uglyPrint (Declaration d) = uglyPrint d

instance Ugly Declaration where
  uglyPrint (InitDeclaration it ds) = uglyPrint it <+> hcat (punctuate comma (map uglyPrint ds)) <> semi
  uglyPrint (Precision pq t) = text "precision" <+> uglyPrint pq <+> uglyPrint t <> semi
  uglyPrint (Block tq i ds n) = hcat [uglyPrint tq <+> text i, lbrace, nest 2 (hcat $ map uglyPrint ds), rbrace <+> ident n <> semi]
  uglyPrint (TQ tq) = uglyPrint tq <> semi

instance Ugly InitDeclarator where
  uglyPrint (InitDecl i a b) = text i <> indexing a <> initialize b

instance Ugly InvariantOrType where
  uglyPrint InvariantDeclarator = text "invariant"
  uglyPrint (TypeDeclarator ft) = uglyPrint ft

instance Ugly FullType where
  uglyPrint (FullType tq ts) = option tq <+> uglyPrint ts

instance Ugly TypeQualifier where
  uglyPrint (TypeQualSto sq) = uglyPrint sq
  uglyPrint (TypeQualLay lq sq) = uglyPrint lq <+> option sq
  uglyPrint (TypeQualInt iq sq) = uglyPrint iq <+> option sq
  uglyPrint (TypeQualInv iq sq) = uglyPrint iq <+> option sq
  uglyPrint (TypeQualInv3 iq iq' sq) = uglyPrint iq <+> uglyPrint iq' <+> uglyPrint sq

instance Ugly StorageQualifier where
  uglyPrint q = case q of
    Language.GLSL.Syntax.Const -> text "const"
    Attribute -> text "attribute"
    Varying -> text "varying"
    CentroidVarying -> text "centroid varying"
    In -> text "in"
    Out -> text "out"
    CentroidIn -> text "centroid in"
    CentroidOut -> text "centroid out"
    Uniform -> text "uniform"

instance Ugly LayoutQualifier where
  uglyPrint (Layout is) = text "layout" <+> char '(' <>
    (hsep $ punctuate comma $ map uglyPrint is) <> char ')'

instance Ugly LayoutQualifierId where
  uglyPrint (LayoutQualId i Nothing) = text i
  uglyPrint (LayoutQualId i (Just e)) = text i <+> char '=' <+> uglyPrint e

instance Ugly InterpolationQualifier where
  uglyPrint q = case q of
    Smooth -> text "smooth"
    Flat -> text "flat"
    NoPerspective -> text "noperspective"

instance Ugly InvariantQualifier where
  uglyPrint Invariant = text "invariant"

instance Ugly TypeSpecifier where
  uglyPrint (TypeSpec (Just pq) t) = uglyPrint pq <+> uglyPrint t
  uglyPrint (TypeSpec Nothing t) = uglyPrint t

instance Ugly PrecisionQualifier where
  uglyPrint HighP = text "highp"
  uglyPrint MediumP = text "mediump"
  uglyPrint LowP = text "lowp"

instance Ugly TypeSpecifierNoPrecision where
  uglyPrint (TypeSpecNoPrecision t a) = uglyPrint t <+> indexing a

instance Ugly TypeSpecifierNonArray where
  uglyPrint t = case t of
    Void -> text "void"
    Float -> text "float"
    Int -> text "int"
    UInt -> text "uint"
    Bool -> text "bool"
    Vec2 -> text "vec2"
    Vec3 -> text "vec3"
    Vec4 -> text "vec4"
    BVec2 -> text "bvec2"
    BVec3 -> text "bvec3"
    BVec4 -> text "bvec4"
    IVec2 -> text "ivec2"
    IVec3 -> text "ivec3"
    IVec4 -> text "ivec4"
    UVec2 -> text "uvec2"
    UVec3 -> text "uvec3"
    UVec4 -> text "uvec4"
    Mat2 -> text "mat2"
    Mat3 -> text "mat3"
    Mat4 -> text "mat4"
    Mat2x2 -> text "mat2x2"
    Mat2x3 -> text "mat2x3"
    Mat2x4 -> text "mat2x4"
    Mat3x2 -> text "mat3x2"
    Mat3x3 -> text "mat3x3"
    Mat3x4 -> text "mat3x4"
    Mat4x2 -> text "mat4x2"
    Mat4x3 -> text "mat4x3"
    Mat4x4 -> text "mat4x4"
    Sampler1D -> text "sampler1D"
    Sampler2D -> text "sampler2D"
    Sampler3D -> text "sampler3D"
    SamplerCube -> text "samplerCube"
    Sampler1DShadow -> text "sampler1DShadow"
    Sampler2DShadow -> text "sampler2DShadow"
    SamplerCubeShadow -> text "samplerCubeShadow"
    Sampler1DArray -> text "sampler1DArray"
    Sampler2DArray -> text "sampler2DArray"
    Sampler1DArrayShadow -> text "sampler1DArrayShadow"
    Sampler2DArrayShadow -> text "sampler2DArrayShadow"
    ISampler1D -> text "isampler1D"
    ISampler2D -> text "isampler2D"
    ISampler3D -> text "isampler3D"
    ISamplerCube -> text "isamplerCube"
    ISampler1DArray -> text "isampler1DArray"
    ISampler2DArray -> text "isampler2DArray"
    USampler1D -> text "usampler1D"
    USampler2D -> text "usampler2D"
    USampler3D -> text "usampler3D"
    USamplerCube -> text "usamplerCube"
    USampler1DArray -> text "usampler1DArray"
    USampler2DArray -> text "usampler2DArray"
    Sampler2DRect -> text "sampler2DRect"
    Sampler2DRectShadow -> text "sampler2DRectShadow"
    ISampler2DRect -> text "isampler2DRect"
    USampler2DRect -> text "usampler2DRect"
    SamplerBuffer -> text "samplerBuffer"
    ISamplerBuffer -> text "isamplerBuffer"
    USamplerBuffer -> text "usamplerBuffer"
    Sampler2DMS -> text "sampler2DMS"
    ISampler2DMS -> text "isampler2DMS"
    USampler2DMS -> text "usampler2DMS"
    Sampler2DMSArray -> text "sampler2DMSArray"
    ISampler2DMSArray -> text "isampler2DMSArray"
    USampler2DMSArray -> text "usampler2DMSArray"
    StructSpecifier i ds ->
      hcat [text "struct" <+> i', lbrace, nest 2 (hcat $ map uglyPrint ds), rbrace]
      where i' = case i of { Nothing -> empty ; Just n -> text n }
    TypeName i -> text i

instance Ugly Field where
  uglyPrint (Field tq s ds) =
    option tq <+> uglyPrint s <+> hsep (punctuate comma $ map uglyPrint ds) <> semi

instance Ugly StructDeclarator where
  uglyPrint (StructDeclarator i e) = ident (Just (i, e))

instance Ugly Expr where
  uglyPrintPrec l p e = case e of
  -- primaryExpression
    Variable v -> text v
    IntConstant Decimal i -> text (show i)
    IntConstant Hexadecimal i -> text (printf "0x%x" i)
    IntConstant Octal i -> text (printf "0%o" i)
    FloatConstant f -> text (show f)
    BoolConstant True -> text "true"
    BoolConstant False -> text "false"
  -- postfixExpression
    Bracket e1 e2 -> maybeParens (p > 16) $
      uglyPrintPrec l 16 e1 <> brackets (uglyPrint e2)
    FieldSelection e1 f -> maybeParens (p > 16) $
      uglyPrintPrec l 16 e1 <> char '.' <> text f
    MethodCall e1 i ps -> maybeParens (p > 16) $
      uglyPrintPrec l 16 e1 <> char '.' <> uglyPrint i <> parens (uglyPrint ps)
    FunctionCall i ps -> maybeParens (p > 16) $
      uglyPrint i <> parens (uglyPrint ps)
    PostInc e1 -> maybeParens (p > 15) $
      uglyPrintPrec l 15 e1 <+> text "++"
    PostDec e1 -> maybeParens (p > 15) $
      uglyPrintPrec l 15 e1 <+> text "--"
    PreInc e1 -> maybeParens (p > 15) $
      text "++" <+> uglyPrintPrec l 15 e1
    PreDec e1 -> maybeParens (p > 15) $
      text "--" <+> uglyPrintPrec l 15 e1
  -- unary expression
    UnaryPlus e1 -> maybeParens (p > 15) $
      text "+" <> uglyPrintPrec l 15 e1
    UnaryNegate e1 -> maybeParens (p > 15) $
      text "-" <> uglyPrintPrec l 15 e1
    UnaryNot e1 -> maybeParens (p > 15) $
      text "!" <> uglyPrintPrec l 15 e1
    UnaryOneComplement e1 -> maybeParens (p > 15) $
      text "~" <> uglyPrintPrec l 15 e1
  -- binary expression
    Mul e1 e2 -> uglyBinary l p 14 "*" e1 e2
    Div e1 e2 -> uglyBinary l p 14 "/" e1 e2
    Mod e1 e2 -> uglyBinary l p 14 "%" e1 e2
    Add e1 e2 -> uglyBinary l p 13 "+" e1 e2
    Sub e1 e2 -> uglyBinary l p 13 "-" e1 e2
    LeftShift e1 e2 -> uglyBinary l p 12 "<<" e1 e2
    RightShift e1 e2 -> uglyBinary l p 12 ">>" e1 e2
    Lt e1 e2 -> uglyBinary l p 11 "<" e1 e2
    Gt e1 e2 -> uglyBinary l p 11 ">" e1 e2
    Lte e1 e2 -> uglyBinary l p 11 "<=" e1 e2
    Gte e1 e2 -> uglyBinary l p 11 ">=" e1 e2
    Equ e1 e2 -> uglyBinary l p 10 "==" e1 e2
    Neq e1 e2 -> uglyBinary l p 10 "!=" e1 e2
    BitAnd e1 e2 -> uglyBinary l p 9 "&" e1 e2
    BitXor e1 e2 -> uglyBinary l p 8 "^" e1 e2
    BitOr e1 e2 -> uglyBinary l p 7 "|" e1 e2
    And e1 e2 -> uglyBinary l p 6 "&&" e1 e2
-- TODO Xor 5 "^^"
    Or e1 e2 -> uglyBinary l p 4 "||" e1 e2
    Selection e1 e2 e3 -> maybeParens (p > 3) $
      uglyPrintPrec l 3 e1 <> char '?' <> uglyPrintPrec l 3 e2
      <> char ':' <> uglyPrintPrec l 3 e3
  -- assignment, the left Expr should be unary expression
    Equal e1 e2 -> uglyBinary l p 2 "=" e1 e2
    MulAssign e1 e2 -> uglyBinary l p 2 "*=" e1 e2
    DivAssign e1 e2 -> uglyBinary l p 2 "/=" e1 e2
    ModAssign e1 e2 -> uglyBinary l p 2 "%=" e1 e2
    AddAssign e1 e2 -> uglyBinary l p 2 "+=" e1 e2
    SubAssign e1 e2 -> uglyBinary l p 2 "-=" e1 e2
    LeftAssign e1 e2 -> uglyBinary l p 2 "<<=" e1 e2
    RightAssign e1 e2 -> uglyBinary l p 2 ">>=" e1 e2
    AndAssign e1 e2 -> uglyBinary l p 2 "&=" e1 e2
    XorAssign e1 e2 -> uglyBinary l p 2 "^=" e1 e2
    OrAssign e1 e2 -> uglyBinary l p 2 "|=" e1 e2
  -- sequence
    Sequence e1 e2 -> maybeParens (p > 1) $
      uglyPrintPrec l 1 e1 <> char ',' <+> uglyPrintPrec l 1 e2

instance Ugly FunctionIdentifier where
  uglyPrint (FuncIdTypeSpec t) = uglyPrint t
  uglyPrint (FuncId i) = text i

instance Ugly Parameters where
  uglyPrint ParamVoid = empty
  uglyPrint (Params es) = hcat $ punctuate comma $ map uglyPrint es

instance Ugly FunctionPrototype where
  uglyPrint (FuncProt t i ps) = uglyPrint t <+> text i <> char '(' <> hcat (punctuate comma $ map uglyPrint ps) <> text ")"

instance Ugly ParameterDeclaration where
  uglyPrint (ParameterDeclaration tq q s i) =
    option tq <+> option q <+> uglyPrint s <+> indexing' i

instance Ugly ParameterTypeQualifier  where
  uglyPrint ConstParameter = text "const"

instance Ugly ParameterQualifier where
  uglyPrint InParameter = text "in"
  uglyPrint OutParameter = text "out"
  uglyPrint InOutParameter = text "inout"

instance Ugly Statement where
  uglyPrint s = case s of
  -- declaration statement
    DeclarationStatement d -> uglyPrint d
  -- jump statement
    Continue -> text "continue" <> semi
    Break -> text "break" <> semi
    Return e -> text "return" <+> option e <> semi
    Discard -> text "discard" <> semi
  -- compound statement
    CompoundStatement c -> uglyPrint c
  -- expression statement
    ExpressionStatement e -> option e <> semi
  -- selection statement
    SelectionStatement e s1 s2 -> hcat [text "if" <> parens (uglyPrint e), nest 2 $ uglyPrint s1, option s2]
  -- switch statement
    SwitchStatement e s1 -> hcat [text "switch" <> parens (uglyPrint e), lbrace, nest 2 $ hcat $ map uglyPrint s1, rbrace]
    CaseLabel l -> uglyPrint l
  -- iteration statement
    While c s1 -> hcat [text "while" <> parens (uglyPrint c), uglyPrint s1]
    DoWhile s1 e -> hcat [text "do", uglyPrint s1, text "while" <+> parens (uglyPrint e)]
    For (Left e1) c e2 s1 -> hcat [text "for", parens (option e1 <+> semi <+> option c <+> semi <+> option e2), uglyPrint s1]
    For (Right d) c e2 s1 -> hcat [text "for", parens (uglyPrint d <+> semi <+> option c <+> semi <+> option e2), uglyPrint s1]

instance Ugly Compound where
  uglyPrint (Compound s) = hcat [lbrace, nest 2 $ hcat $ map uglyPrint s, rbrace]

instance Ugly Condition where
  uglyPrint (Condition e) = uglyPrint e
  uglyPrint (InitializedCondition t i e) = uglyPrint t <+> uglyPrint i <+> uglyPrint e

instance Ugly CaseLabel where
  uglyPrint  (Case e) = text "case" <+> uglyPrint e <> colon
  uglyPrint Default = text "default:"
