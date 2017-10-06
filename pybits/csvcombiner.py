#!/usr/bin/env python

import sys
import pandas

datadir = sys.argv[1]
header = sys.argv[2]
acc_filepath = datadir + "/" + header + "acc00.csv"
gyr_filepath = datadir + "/" + header + "gyr00.csv"
mag_filepath = datadir + "/" + header + "mag00.csv"
prs_filepath = datadir + "/" + header + "prs00.csv"
tmp_filepath = datadir + "/" + header + "tmp00.csv"
output_path  = datadir + "/" + header + "combined.csv"

print "Reading CSVs..."
acc = pandas.read_csv(acc_filepath)
gyr = pandas.read_csv(gyr_filepath)
mag = pandas.read_csv(mag_filepath)
prs = pandas.read_csv(prs_filepath)
tmp = pandas.read_csv(tmp_filepath)

print "Joining..."
total = pandas.concat([acc, gyr, mag, prs, tmp])
total = total.sort_values('elapsed (s)')

print "Filling NAs..."
total = total.fillna(method='ffill')
total = total.fillna(method='bfill')

print "Saving..."
total.to_csv(output_path, index=False)
