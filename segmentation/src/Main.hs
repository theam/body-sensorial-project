{-# OPTIONS_GHC -F -pgmF inlitpp #-}
```haskell hide top
import Inliterate.Import
```
# Segmentation
This module comprises the segmentation part of the preprocessing of the data, applying whats
described in the paper

> **Robust heart sound segmentation algorithm for commonly occurring heart valve diseases**
> *Ari et al, 2008*

## Process

The process that is described in the paper can be mapped to the following commutative diagram:

![commdiag](http://i.imgur.com/PSjAwGO.png)

The [classes](https://en.wikipedia.org/wiki/Class_(set_theory))
in this commutative diagram can be represented directly as data types in Haskell:

```haskell top
data NoisySound'
data FilteredSound'
data Spectrogram'
data BarkScaledSpectrogram'
data SmoothSpectrogram'
data EventDetectionFunction'
```

These are [Tags](https://hackage.haskell.org/package/tagged-0.8.5/docs/Data-Tagged.html)
that will be used for the definition of our format underneath. They help us not being
constrained to any kind of format (`Vector`, `Array`, `ByteString`, etc...).

The [morphisms](https://en.wikipedia.org/wiki/Morphism) represented in the diagram would
transform the data in some way:

1. `prefilter` would apply a [Chebyshev type I Lowpass filter](https://en.wikipedia.org/wiki/Chebyshev_filter)
2. An `stft` would be applied to all the `FilteredSound` using a *3ms* [Hann Window](https://en.wikipedia.org/wiki/Window_function#Cosine-sum_windows)
3. `barkscale` scales the `Spectrogram` using a [Bark scale](https://en.wikipedia.org/wiki/Bark_scale)
4. `smoothen` convolves each of the frequency bands using a *200ms* Hamming window (raised cosine).
5. `loudnessEvaluation` sums all the amplitudes of all frequency bands:

![eq](http://i.imgur.com/l4h6Hr8.png)

where *Ek* represents the magnitude of the kth frequency band in the spectrogram.
There are *N* of them.
