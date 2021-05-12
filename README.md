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

To save any Tables.jl-compatible table:
```julia
using ARFFFiles
ARFFFiles.save("mytable.arff", df)
```

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

**Keyword options.**
- `missingcols=true`: By default we assume all columns can contain missing data (`?`). This option controls this behaviour. It can be `true`, `false`, a vector or set of symbols, or a function taking a symbol and returning true if that column can contain missings.
- `missingnan=false`: Convert missing values in numeric columns to NaN. This is equivalent to excluding these columns in `missingcols`.
- `categorical=true`: When false, nominal columns are converted to `String` instead of `CategoricalValue{String}`.

## Saving

- `save(file, table)` saves the Tables.jl-compatible `table` to `file` (a filename or IO stream).

**Types.** `Real` is saved as numeric, `AbstractString` as string, `DateTime` and `Date` as date, and `CategoricalValue{<:AbstractString}` as nominal.

**Keyword options.**
- `relation="data"`: The relation name.
- `comment`: A comment to print at the top of the file.
