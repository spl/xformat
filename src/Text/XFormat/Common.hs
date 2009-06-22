
module Text.XFormat.Common where

data d1 :%: d2 = d1 :%: d2
  deriving (Eq, Show)

infixr 8 :%:

(%) = (:%:)
infixr 8 %


data CharF = Char

data IntF = Int

data IntegerF = Integer

data FloatF = Float

data DoubleF = Double

data StringF = String

data ShowF a = Show

data ReadF a = Read

data NumF a = Num

data SpaceF = Space

data SpacesF = Spaces Int

data WrapF outer inner = Wrap outer inner outer

