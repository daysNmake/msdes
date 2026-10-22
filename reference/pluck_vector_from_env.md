# Pluck vector from enviroment

`pluck_vector_from_env()` function to extract vectors or condition on
them when using environment in WARDEN pick_val() or input_block().

## Usage

``` r
pluck_vector_from_env(anylist, anyindex, ...)
```

## Arguments

- anylist:

  The input parameters saved an environment object

- anyindex:

  conditioning on treatment or severity

## Details

Vector of objects pulled from WARDEN environment (ex. lists saved in
common inputs) and indexing on a vector (ex. treatments, severity)
