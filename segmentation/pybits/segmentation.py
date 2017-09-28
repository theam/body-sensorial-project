import numpy as np
from numpy.lib import stride_tricks
from scipy import signal
from scipy.signal import filter_design as filtering
import scipy.io.wavfile as wav


def prefilter(noisy_sound):
    wp = 0.01
    rp = 1.0
    ws = 0.0111
    rs = 42
    b, a = filtering.iirdesign(wp, ws, rp, rs, ftype='cheby1')
    return filtering.lfilter(b, a, noisy_sound, axis=0)


def _stft(sig, frame_size, overlap_fac=0.5, window=np.hanning):
    win = window(frame_size)
    hop_size = int(frame_size - np.floor(overlap_fac * frame_size))
    samples = np.append(np.zeroes(np.floor(frame_size/2.0), sig))
    cols = np.ceil((len(samples) - frame_size) / float(hop_size)) + 1
    samples = np.append(samples, np.zeros(frame_size))
    frames = stride_tricks.as_strided(samples, shape=(cols, frameSize), strides=(samples.strides[0]*hopSize, samples.strides[0])).copy()
    frames *= win
    return np.fft.rfft(frames)


def _logscale(spectrogram, sr=192000, factor=20.):
    timebins, freqbins = np.shape(spectrogram)
    scale = np.linspace(0, 1, freqbins) ** factor
    scale *= (freqbins-1)/max(scale)
    scale = np.unique(np.round(scale))

    newspec = np.complex128(np.zeros([timebins, len(scale)]))
    for i in range(0, len(scale)):
        if i == len(scale)-1:
            newspec[:,i] = np.sum(spec[:,scale[i]:], axis=1)
        else:
            newspec[:,i] = np.sum(spec[:,scale[i]:scale[i+1]], axis=1)

    allfreqs = np.abs(np.fft.fftfreq(freqbins*2, 1./sr)[:freqbins+1])
    freqs = []
    for i in range(0, len(scale)):
        if i == len(scale)-1:
            freqs += [np.mean(allfreqs[scale[i]:])]
        else:
            freqs += [np.mean(allfreqs[scale[i]:scale[i+1]])]
    return newspec, freqs

def pyprint(s):
    print(s)
    return True
