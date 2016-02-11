module Nightshade.Types where

import Language.GLSL.Syntax

type Name = ( String, Bool )

type Uniform = ( String, TypeSpecifierNonArray )
