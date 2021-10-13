# ARFFFiles.jl

Load and save [ARFF (Attribute Relation File Format)](https://waikato.github.io/weka-wiki/formats_and_processing/arff/) files.

Integrated into [`Tables.jl`](https://github.com/JuliaData/Tables.jl) for easily converting to your favourite table types.

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
Replace `DataFrame` with your favourite table type, or leave it out to get an `ARFFTable`.

To save any Tables.jl-compatible table:
```julia
using ARFFFiles
ARFFFiles.save("mytable.arff", df)
```

## Loading

- `load(file)` loads the table in the given file (filename or IO stream) as an `ARFFTable`.
- `load(func, file)` is equivalent to `func(loadstreaming(file))` but ensures the file is closed afterwards.
- `loadstreaming(file)` returns a `ARFFReader` object `r`:
    - Satisfies the `Tables.jl` interface, so can be materialized as a table.
    - `r.header` contains the header parsed from `io`.
    - Iterates rows of type `ARFFRow`.
    - `read(r)`, `read(r, n)` and `read!(r, x)` reads rows of the table.
    - `readcolumns(r, [maxbytes=nothing])` reads the whole table into a columnar format. Specify `maxbytes` to read a portion of the rows.
    - `close(r)` closes the underlying io stream, unless `own=false`.
- `loadchunks(file)` returns an iterator of `ARFFTable`s for efficiently streaming very large tables. Equivalent to `Tables.partitions(loadstreaming(file))`.
- `loadchunks(func, file)` is equivalent to `func(loadchunks(file))` but ensures the file is closed afterwards.

**Types.** Numbers load as `Float64`, strings as `String`, dates as `DateTime` and nominals as `CategoricalValue{String}` (from [`CategoricalArrays`](https://github.com/JuliaData/CategoricalArrays.jl)).

**Keyword options.**
- `missingcols=:auto`: Controls which columns may contain missing data (`?`). It can be `:auto`, `:all`, `:none`, a set or vector of column names (symbols), or a function taking a symbol and returning true if that column can contain missing. If the table is being read in a streaming fashion, then `:auto` behaves the same as `:all`.
- `missingnan=false`: Convert missing values in numeric columns to NaN. This is equivalent to excluding these columns in `missingcols`.
- `categorical=true`: When false, nominal columns are converted to `String` instead of `CategoricalValue{String}`.
- `chunkbytes=2^26`: Read approximately this many bytes per chunk when iterating over chunks or rows.
- `own=false`: Signals whether or not to close the underlying IO stream when `close(::ARFFReader)` is called.

## Saving

- `save(file, table)` saves the Tables.jl-compatible `table` to `file` (a filename or IO stream).

**Types.** `Real` is saved as numeric, `AbstractString` as string, `DateTime` and `Date` as date, and `CategoricalValue{<:AbstractString}` as nominal.

**Keyword options.**
- `relation="data"`: The relation name.
- `comment`: A comment to print at the top of the file.
