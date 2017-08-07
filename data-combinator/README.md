# data-combinator

## Required software:
On an Ubuntu system:

`sudo apt install -y ffmpeg sox r-base`

After installing that, get into an `R` prompt and issue:

`install.packages('data.table')`
`install.packages('zoo')`


## Usage:

1. Compile the project using `stack build` (Haskell build tool, [Stack](http://haskellstack.org) should be installed)

2. Place the data in the `body-sensorial-project/data` folder

3. Execute `stack exec data-combinator -- <file extension to merge> --header <header of files>`, for example:

    `stack exec data-combinator -- csv --header 20.659698_-103.349609_170629_1754_00013_`

> Note the trailing underscore.

The program will join all the `acc`, `gyr`, `mag`, `prs` and `tmp` files into one for that patient.
We also can use `flac` instead of `csv` to join all the `vas*` files into a single mono `WAV`.
