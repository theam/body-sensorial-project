#!/usr/bin/env python

import numpy as np
import scipy.io.wavfile as wav
from scipy import signal
from itertools import tee, izip
import sys

def _barkScale (f):
    return 13 * np.arctan(0.00076 * f) + 3.5 * np.arctan(np.square(f/7500))


def _filter_signal(wavData):
    b, a = signal.cheby1(6, 0.0003, 0.045)
    return signal.filtfilt(b, a, wavData)


def _make_spectrogram(filtered_signal, sampleRate):
    hannWindow = signal.hann(1024)
    f,t,Sxx = signal.spectrogram(filtered_signal,nperseg=1024, fs=sampleRate, window=hannWindow)
    return Sxx


def _barkscale_spectrogram(Sxx):
    barkscaledSxx = np.copy(Sxx)
    for i in range(len(barkscaledSxx)):
        barkscaledSxx[i] = _barkScale(Sxx[i])
    return barkscaledSxx


def _get_loudness_evaluation_function(barkscaledSxx):
    loudness = np.sum(barkscaledSxx, axis=0) / len(barkscaledSxx)
    hann = np.hanning(10)
    smoothLoudness = np.convolve(loudness, hann, 'full')[:len(loudness)]
    return smoothLoudness


def _get_start_of_cardiac_cycle(smoothLoudness):
    pts = []
    for i in range(len(smoothLoudness)):
        if smoothLoudness[i] > 0.2:
            pts.append(round(i*len(wavData)/len(smoothLoudness)))
    return pts


def _get_cardiac_slices(wavData, pts, msPerSlice):
    i = 0
    j = 0
    slices = []
    while j < len(pts):
        if pts[j] - pts[i] >= round((msPerSlice / 1000) * 16000):     # 3 seconds
            ix = int(pts[i])
            jx = int(pts[j])
            slices.append(wavData[ix:jx])
            i = j
        j += 1
    return slices


def _write_slices(slices, outputFolder):
    for i in range(len(slices)):
        wav.write(outputFolder + "/slice_"+str(i)+".wav", 16000, slices[i])


def main():
    args = sys.argv
    if (len(args) != 3) or (len(args) != 4):
        print("\n\nUsage: " + args[0] + " filename output_folder [milliseconds per slice]\n\n")
        exit(128)

    filename = args[1]
    outputFolder = args[2].strip("/")
    msPerSlice = args[3] or 3000

    sampleRate, wavData = wav.read()
    filtered_signal = _filter_signal(wavData)
    Sxx = _make_spectrogram(filtered_signal, sampleRate)
    barkscaledSxx = _barkscale_spectrogram(Sxx)
    smoothLoudness = _get_loudness_evaluation_function(barkscaledSxx)
    pts = _get_start_of_cardiac_cycle(smoothLoudness)
    slices = _get_cardiac_slices(wavData, pts, msPerSlice)
    _write_slices(slices, outputFolder)

if __name__ == "__main__":
    main()
