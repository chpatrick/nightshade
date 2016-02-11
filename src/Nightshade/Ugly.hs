{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances#-}

module Nightshade.Ugly where

import Text.Printf
import Text.PrettyPrint.HughesPJClass

import Language.GLSL.Syntax

class Ugly a where
    uglifyPrec :: PrettyLevel -> Rational -> a -> Doc
    uglifyPrec _ _ = uglify

    uglify :: a -> Doc
    uglify = uglifyPrec prettyNormal 0

uglyBinary :: Ugly a =>
  PrettyLevel -> Rational -> Rational -> String -> a -> a -> Doc
uglyBinary l p op o e1 e2 = prettyParen (p > op) $
  uglifyPrec l op e1 <> text o <> uglifyPrec l op e2

option :: Ugly a => Maybe a -> Doc
option Nothing = empty
option (Just x) = uglify x

indexing :: Ugly a => Maybe (Maybe a) -> Doc
indexing Nothing = empty
indexing (Just Nothing) = brackets empty
indexing (Just (Just e)) = brackets $ uglify e

indexing' :: Ugly a => Maybe (String, Maybe a) -> Doc
indexing' Nothing = empty
indexing' (Just (i, Nothing)) = text i
indexing' (Just (i, Just e)) = text i <> brackets (uglify e)

initialize :: Ugly a => Maybe a -> Doc
initialize Nothing = empty
initialize (Just e) = equals <> uglify e

ident :: Ugly a => Maybe (String, Maybe (Maybe a)) -> Doc
ident Nothing = empty
ident (Just (i, Nothing)) = text i
ident (Just (i, Just Nothing)) = text i <> brackets empty
ident (Just (i, Just (Just e))) = text i <> brackets (uglify e)

initialize' :: Ugly a => Maybe (String, Maybe a) -> Doc
initialize' Nothing = empty
initialize' (Just (i, Nothing)) = text i
initialize' (Just (i, Just e)) = text i <> char '=' <> uglify e

----------------------------------------------------------------------
-- Ugly instances
----------------------------------------------------------------------

instance Ugly String where
  uglify = pPrint
  uglifyPrec = pPrintPrec

instance Ugly TranslationUnit where
  uglify (TranslationUnit ds) = hcat $ map uglify ds
--  uglify (Alternative p e) = text "(" <> nest 2 (hcat [uglify p, uglify e]) <> text ")"

instance Ugly ExternalDeclaration where
  uglify (FunctionDeclaration p) = uglify p <> semi
  uglify (FunctionDefinition p s) = hcat [uglify p, uglify s]
  uglify (Declaration d) = uglify d

instance Ugly Declaration where
  uglify (InitDeclaration it ds) = uglify it <+> hcat (punctuate comma (map uglify ds)) <> semi
  uglify (Precision pq t) = text "precision" <+> uglify pq <+> uglify t <> semi
  uglify (Block tq i ds n) = hcat [uglify tq <+> text i, lbrace, nest 2 (hcat $ map uglify ds), rbrace <+> ident n <> semi]
  uglify (TQ tq) = uglify tq <> semi

instance Ugly InitDeclarator where
  uglify (InitDecl i a b) = text i <> indexing a <> initialize b

instance Ugly InvariantOrType where
  uglify InvariantDeclarator = text "invariant"
  uglify (TypeDeclarator ft) = uglify ft

instance Ugly FullType where
  uglify (FullType tq ts) = option tq <+> uglify ts

instance Ugly TypeQualifier where
  uglify (TypeQualSto sq) = uglify sq
  uglify (TypeQualLay lq sq) = uglify lq <+> option sq
  uglify (TypeQualInt iq sq) = uglify iq <+> option sq
  uglify (TypeQualInv iq sq) = uglify iq <+> option sq
  uglify (TypeQualInv3 iq iq' sq) = uglify iq <+> uglify iq' <+> uglify sq

instance Ugly StorageQualifier where
  uglify q = case q of
    Const -> text "const"
    Attribute -> text "attribute"
    Varying -> text "varying"
    CentroidVarying -> text "centroid varying"
    In -> text "in"
    Out -> text "out"
    CentroidIn -> text "centroid in"
    CentroidOut -> text "centroid out"
    Uniform -> text "uniform"

instance Ugly LayoutQualifier where
  uglify (Layout is) = text "layout" <+> char '(' <>
    (hsep $ punctuate comma $ map uglify is) <> char ')'

instance Ugly LayoutQualifierId where
  uglify (LayoutQualId i Nothing) = text i
  uglify (LayoutQualId i (Just e)) = text i <+> char '=' <+> uglify e

instance Ugly InterpolationQualifier where
  uglify q = case q of
    Smooth -> text "smooth"
    Flat -> text "flat"
    NoPerspective -> text "noperspective"

instance Ugly InvariantQualifier where
  uglify Invariant = text "invariant"

instance Ugly TypeSpecifier where
  uglify (TypeSpec (Just pq) t) = uglify pq <+> uglify t
  uglify (TypeSpec Nothing t) = uglify t

instance Ugly PrecisionQualifier where
  uglify HighP = text "highp"
  uglify MediumP = text "mediump"
  uglify LowP = text "lowp"

instance Ugly TypeSpecifierNoPrecision where
  uglify (TypeSpecNoPrecision t a) = uglify t <+> indexing a

instance Ugly TypeSpecifierNonArray where
  uglify t = case t of
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
      hcat [text "struct" <+> i', lbrace, nest 2 (hcat $ map uglify ds), rbrace]
      where i' = case i of { Nothing -> empty ; Just n -> text n }
    TypeName i -> text i

instance Ugly Field where
  uglify (Field tq s ds) =
    option tq <+> uglify s <+> hsep (punctuate comma $ map uglify ds) <> semi

instance Ugly StructDeclarator where
  uglify (StructDeclarator i e) = ident (Just (i, e))

instance Ugly Expr where
  uglifyPrec l p e = case e of
  -- primaryExpression
    Variable v -> text v
    IntConstant Decimal i -> text (show i)
    IntConstant Hexadecimal i -> text (printf "0x%x" i)
    IntConstant Octal i -> text (printf "0%o" i)
    FloatConstant f -> text (show f)
    BoolConstant True -> text "true"
    BoolConstant False -> text "false"
  -- postfixExpression
    Bracket e1 e2 -> prettyParen (p > 16) $
      uglifyPrec l 16 e1 <> brackets (uglify e2)
    FieldSelection e1 f -> prettyParen (p > 16) $
      uglifyPrec l 16 e1 <> char '.' <> text f
    MethodCall e1 i ps -> prettyParen (p > 16) $
      uglifyPrec l 16 e1 <> char '.' <> uglify i <> parens (uglify ps)
    FunctionCall i ps -> prettyParen (p > 16) $
      uglify i <> parens (uglify ps)
    PostInc e1 -> prettyParen (p > 15) $
      uglifyPrec l 15 e1 <+> text "++"
    PostDec e1 -> prettyParen (p > 15) $
      uglifyPrec l 15 e1 <+> text "--"
    PreInc e1 -> prettyParen (p > 15) $
      text "++" <+> uglifyPrec l 15 e1
    PreDec e1 -> prettyParen (p > 15) $
      text "--" <+> uglifyPrec l 15 e1
  -- unary expression
    UnaryPlus e1 -> prettyParen (p > 15) $
      text "+" <> uglifyPrec l 15 e1
    UnaryNegate e1 -> prettyParen (p > 15) $
      text "-" <> uglifyPrec l 15 e1
    UnaryNot e1 -> prettyParen (p > 15) $
      text "!" <> uglifyPrec l 15 e1
    UnaryOneComplement e1 -> prettyParen (p > 15) $
      text "~" <> uglifyPrec l 15 e1
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
    Selection e1 e2 e3 -> prettyParen (p > 3) $
      uglifyPrec l 3 e1 <> char '?' <> uglifyPrec l 3 e2
      <> char ':' <> uglifyPrec l 3 e3
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
    Sequence e1 e2 -> prettyParen (p > 1) $
      uglifyPrec l 1 e1 <> char ',' <+> uglifyPrec l 1 e2

instance Ugly FunctionIdentifier where
  uglify (FuncIdTypeSpec t) = uglify t
  uglify (FuncId i) = text i

instance Ugly Parameters where
  uglify ParamVoid = empty
  uglify (Params es) = hcat $ punctuate comma $ map uglify es

instance Ugly FunctionPrototype where
  uglify (FuncProt t i ps) = uglify t <+> text i <> char '(' <> hcat (punctuate comma $ map uglify ps) <> text ")"

instance Ugly ParameterDeclaration where
  uglify (ParameterDeclaration tq q s i) =
    option tq <+> option q <+> uglify s <+> indexing' i

instance Ugly ParameterTypeQualifier  where
  uglify ConstParameter = text "const"

instance Ugly ParameterQualifier where
  uglify InParameter = text "in"
  uglify OutParameter = text "out"
  uglify InOutParameter = text "inout"

instance Ugly Statement where
  uglify s = case s of
  -- declaration statement
    DeclarationStatement d -> uglify d
  -- jump statement
    Continue -> text "continue" <> semi
    Break -> text "break" <> semi
    Return e -> text "return" <+> option e <> semi
    Discard -> text "discard" <> semi
  -- compound statement
    CompoundStatement c -> uglify c
  -- expression statement
    ExpressionStatement e -> option e <> semi
  -- selection statement
    SelectionStatement e s1 s2 -> hcat [text "if" <> parens (uglify e), nest 2 $ uglify s1, option s2]
  -- switch statement
    SwitchStatement e s1 -> hcat [text "switch" <> parens (uglify e), lbrace, nest 2 $ hcat $ map uglify s1, rbrace]
    CaseLabel l -> uglify l
  -- iteration statement
    While c s1 -> hcat [text "while" <> parens (uglify c), uglify s1]
    DoWhile s1 e -> hcat [text "do", uglify s1, text "while" <+> parens (uglify e)]
    For (Left e1) c e2 s1 -> hcat [text "for", parens (option e1 <+> semi <+> option c <+> semi <+> option e2), uglify s1]
    For (Right d) c e2 s1 -> hcat [text "for", parens (uglify d <+> semi <+> option c <+> semi <+> option e2), uglify s1]

instance Ugly Compound where
  uglify (Compound s) = hcat [lbrace, nest 2 $ hcat $ map uglify s, rbrace]

instance Ugly Condition where
  uglify (Condition e) = uglify e
  uglify (InitializedCondition t i e) = uglify t <+> uglify i <+> uglify e

instance Ugly CaseLabel where
  uglify  (Case e) = text "case" <+> uglify e <> colon
  uglify Default = text "default:"
