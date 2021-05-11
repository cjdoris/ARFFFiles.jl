# ARFFFiles.jl

Load and save [ARFF (Attribute Relation File Format)](https://waikato.github.io/weka-wiki/formats_and_processing/arff/) files.

Integrated into [`Tables.jl`](https://github.com/JuliaData/Tables.jl) (for easily converting to your favourite table type) and [`FileIO.jl`](https://github.com/JuliaIO/FileIO.jl) (for generic file loading).

## Install

```
] add ARFFFiles
```

## Quick start

To load an ARFF file as a `DataFrame`:
```julia
using ARFFFiles, DataFrames
df = ARFFFiles.load(DataFrame, "mytable.arff")
```
Replace `DataFrame` with your favourite table type, or leave it out to get a vector of named tuples.

## Loading

- `load(file)` loads the table in the given file as a vector of named tuples.
- `load(io, [own=false])` loads the table from the given io stream.
- `load(f, ...)` is equivalent to `f(loadstreaming(...))` but ensures the file is closed afterwards.
- `loadstreaming(io, [own=false])` returns a `ARFFReader` object `r`:
    - `r.header` contains the header parsed from `io`.
    - `read(r)` reads the whole table as a vector of named tuples.
    - `read(r, n)` reads up to `n` rows.
    - `read!(r, x)` reads into the pre-allocated vector `x` and returns the number of rows read.
    - `close(r)` closes the underlying io stream, unless `own=false`.
    - `r` satisfies the `Tables.jl` interface, so can be materialized as a table.

**Types.** Numbers load as `Float64`, strings as `String`, dates as `DateTime` and nominals as `CategoricalValue{String}` (from [`CategoricalArrays`](https://github.com/JuliaData/CategoricalArrays.jl)).

**Missing data.** By default we assume all columns can contain missing data (`?`).
Option `missingcols` to the above functions controls this behaviour, it can be `true`, `false`,
a vector or set of symbols, or a function taking a symbol and returning true if that column can contain missings.

## Saving

Not implemented.
