{-# OPTIONS_GHC -F -pgmF inlitpp #-}
```haskell hide top
import Inliterate.Import
```

# Segmentation
This module comprises the segmentation part of the preprocessing of the data, applying whats
described in the paper

> **Automatic segmentation of Phonocardiogram using the occurrence of the cardiac events**
> *Vishwanath et al., 2017*

*Required libraries*:

```haskell top
import Data.Complex                  as Complex
import Data.Array                    as Array
import DSP.Basic				     as DSP
import DSP.Filter.IIR.IIR            as DSP
import DSP.Filter.IIR.Design         as DSP
import Numeric.Transform.Fourier.FFT as FFT
import Graphics.Plotly			     as Plotly
```

## 1. Process

The process that is described in the paper can be mapped to the following commutative diagram:

$$\require{AMScd}$$

$$\begin{CD}
Noisy Sound @>prefilter>> Filtered Sound @>makeSpectrogram>> Spectrogram\\\\
@. @. @VV{barkscale}V\\\\
Event Detection Function @<loudnessEvaluation<< SmoothSpectrogram @<smoothen<< BarkScaled Spectrogram
\end{CD}$$


The [classes](https://en.wikipedia.org/wiki/Class_(set_theory))
in this commutative diagram can be represented directly as newtypes in Haskell:

```haskell top
newtype NoisySound             = NoisySound [Double]
newtype FilteredSound          = FilteredSound [Double]
newtype Spectrogram            = Spectrogram [Array Int Double]
newtype BarkScaledSpectrogram  = BarkScaledSpectrogram (Array Int Double)
newtype SmoothSpectrogram      = SmoothSpectrogram (Array Int Double)
newtype EventDetectionFunction = EventDetectionFunction (Array Int Double)
```

The [morphisms](https://en.wikipedia.org/wiki/Morphism) represented in the diagram are functions
that would transform the data in some way:

1. `prefilter` would apply a [Chebyshev type I Lowpass filter](https://en.wikipedia.org/wiki/Chebyshev_filter)
2. An `stft` would be applied to all the `FilteredSound` using a *3ms* [Hann Window](https://en.wikipedia.org/wiki/Window_function#Cosine-sum_windows)
3. `barkscale` scales the `Spectrogram` using a [Bark scale](https://en.wikipedia.org/wiki/Bark_scale)
4. `smoothen` convolves each of the frequency bands using a *200ms* Hamming window (raised cosine).
5. `loudnessEvaluation` sums all the amplitudes of all frequency bands:

	$$ L_{dB}(t) = \frac{\sum_{k=1}{N}E_k}{N} $$

	where \\(E_k\\) represents the magnitude of the kth frequency band in the spectrogram.
	There are \\(N\\) of them.
6. Now we convolute the loudness with a *300ms* Hamming window, so we obtain a smoothed loudness
	index function, where positive peaks are **onsets**, and negative ones are **offsets**
    
After doing this process, we can locate manually the \\(S_1\\) and \\(S_2\\) sounds, which help us
to locate the *systole* and *diastole*. After that, it's just a matter of alternation. So, we
can automate the segmentation of the next sounds.

## 2. Implementation

To implement the `prefilter` function, we can use the `dsp` package that comes with many
nice DSP functions that are helpful here.

```haskell top
prefilter :: NoisySound -> FilteredSound
prefilter (NoisySound ns) = FilteredSound filteredSound
  where
  	wp = 0.5
    rp = 0.05
    ws = 0
    rs = 0.05
    (b, a) = DSP.chebyshev1Lowpass (wp, rp) (ws, rs)
    filteredSound = DSP.iir_df2 (b, a) ns
```


Now we can proceed to define `makeSpectrogram`, but first we need to separate all of our
samples in frames, and get their magnitudes:

```haskell top
getFrames :: Array Int Double -> Int -> Int -> [Array Int Double]
getFrames inArr frameSize hop =
     [getFrame inArr start frameSize | start <- [0, hop .. l-1]]
   where
     (_,l) = Array.bounds inArr
 
getFrame :: Array Int Double -> Int -> Int -> Array Int Double
getFrame inVect start len =
	DSP.pad slice len
  where
    slice = Array.ixmap (0, l - 1) (+ start) inVect
    l = min len (end - start)
    (_,end) = Array.bounds inVect

getFrameMagnitude :: Array Int (Complex Double) -> Array Int Double
getFrameMagnitude frame =
		Array.array (0,(l-1) `div` 2) 
        	[(i,log (magnitude (frame!(i+(l-1) `div` 2)) + 1))
            	| i <- [0..((l-1) `div` 2)]]
 	where
 		(_,l) = Array.bounds frame

makeSpectrogram :: FilteredSound -> Spectrogram
makeSpectrogram (FilteredSound fs) = Spectrogram spectrogram
  where
    fsArray = Array.array (0, length fs) [(i, fs !! i) | i <- [0..length fs]]
    spectrogram = map (getFrameMagnitude . rfft) (getFrames fsArray 1024 512)
```

