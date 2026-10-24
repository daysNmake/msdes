
<!-- README.md is generated from README.Rmd. Please edit that file -->

# msdes

## package info

msdes is package version of the MS-MTA (NICE appraisal version). The
model is documented at: <https://daysnmake.github.io/msdes/>

## Installation

You can install the development version of msdes like so:

``` r
pak::pak("daysNmake/msdes")
```

or

``` r
install.packages("msdes", repos = "https://daysNmake.r-universe.dev")
```

## To run the model from your machine

Download the main script form here:
<https://github.com/daysNmake/msdes/blob/main/analysis/WARDEN_MAIN.R>

By default `n_patients` and `n_samples` are set to `10` but it is
advised to try running this with a larger number as a test (e.g., `100`
each). Running with the full `1000` each may take up to 30 hours.
Convergence checks found that `100` each was sufficient for
experimentation.

## About the project

*NIHR 2026*

- NIHR Journal article <https://doi.org/10.3310/GJHT1811>

*NICE 2026*

- Description of the Model
  <https://www.nice.org.uk/guidance/ta1126/chapter/3-Committee-discussion#economic-model>

*ISPOR Europe 2025*

- Abstract & Poster <https://tinyurl.com/DESMSMTA>

*R-HTA 2025*

- Presentation slides
  <https://github.com/r-hta/r-hta-workshop-2025/blob/main/README.md>

- Presentation Recording <https://www.youtube.com/watch?v=MhyEtZG_52Q>
