# posit 3.2.0.3

The [Posit Standard 3.2](https://posithub.org/docs/posit_standard.pdf),
where Real numbers are approximated by Maybe Rational.  The Posit 
Numbers are a drop in replacement for `Float` or `Double` mapped to a 
2's complement integer type; smoothly and with tapering precision, in a 
similar way to the projective real line.  The 'posit' library implements
the following standard classes:

 * Show
 * Eq
 * Ord  -- compare as an integer representation
 * Num  -- Addition, subtraction, multiplication, and other operations
 * Enum  -- Successor and Predecessor
 * Fractional  -- division, divide by zero is Not a Real (NaR) number
 * Real
 * Bounded
 * FusedOps  -- dot product and others
 * Convertable  -- Conversions between different posit formats
 * AltShow
 * Read
 * Storable  -- Formats for binary data, for computation and data interchange
 * RealFrac
 * RealFloat
 * Floating  -- Mathematical functions such as logarithm, exponential, trigonometric, and hyperbolic functions. Warning! May induce trance.

The Posits are indexed by the type (es :: ES) where exponent size and
word size are related.  In `posit-3.2.0.3` es is instantiated as Z, I,
II, III, IV, V.  The word size (in bits) of the value is `= 8 * 2^es`,
that is `2^es` bytes.  The Types: 'Posit8', 'Posit16', 'Posit32',
'Posit64', 'Posit128', and 'Posit256' are implemented and include a
couple of auxiliary classes, like AltShow, AltFloating, and FusedOps.

```
class AltShow a where
  -- Display the Posit in its Binary Representation
  displayBinary :: a -> String
  -- Display the Posit in its Integral Representation
  displayIntegral :: a -> String
  -- Display the Posit as a Rational
  displayRational :: a -> String
  -- Display the Posit as a Decimal until the Repented occurs
  displayDecimal :: a -> String
```

```
class AltFloating p where
  phi :: p
  gamma :: p -> p
  sinc :: p -> p
  expm1 :: p -> p
```

```
class Num a => FusedOps a where
  -- |Fused Multiply Add: (a * b) + c
  fma :: a -> a -> a -> a
  -- |Fused Add Multiply: (a + b) * c
  fam :: a -> a -> a -> a
  -- |Fused Multiply Multiply Subtract: (a * b) - (c * d)
  fmms :: a -> a -> a -> a -> a
  -- |Fused Sum of 3 values: a + b + c
  fsum3 :: a -> a -> a -> a
  -- |Fused Sum of 4 values: a + b + c + d
  fsum4 :: a -> a -> a -> a -> a
  -- |Fused Sum of a List of Posits
  fsumL :: Foldable t => t a -> a
  -- |Fused Dot Product of 3 element vector: (a1 * b1) + (a2 * b2) + (a3 * b3)
  fdot3 :: a -> a -> a -> a -> a -> a -> a
  -- |Fused Dot Product of 4 element veector: (a0 * b0) + (a1 * b1) + (a2 * b2) + (a3 * b3)
  fdot4 :: a -> a -> a -> a -> a -> a -> a -> a -> a
  -- |Fused Dot Product of Two Lists
  fdotL :: Foldable t => t a -> t a -> a
  -- |Fused Subtract Multiply: a - (b * c)
  fsm :: a -> a -> a -> a
```

The Posit type is 'Convertible' between other Posit lengths.

```
class Convertible a b where
  convert :: a -> b
```

The Posit Library is built on top of two of the most excellent libraries:
[data-dword](https://hackage.haskell.org/package/data-dword), and
[scientific](https://hackage.haskell.org/package/scientific).  The
'data-dword' library provides the underlining machine word
representation, it can provide 2^es word size, 2's complement fixed
length integers.  The 'scientific' library provides 'read' and 'show'
instances.

