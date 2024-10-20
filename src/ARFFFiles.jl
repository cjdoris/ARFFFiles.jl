"""
Module for loading, and saving of ARFFFiles.

See `ARFFFiles.load` and `ARFFFiles.save`.
"""
module ARFFFiles

using Dates, Tables, CategoricalArrays, Parsers

export ARFFType, ARFFNumericType, ARFFStringType, ARFFDateType, ARFFNominalType, ARFFRelation, ARFFAttribute, ARFFDataStart, ARFFHeader, ARFFReader, ARFFRow, ARFFChunks

"""
    ARFFType

Abstract type of ARFF types. Concrete subtypes are `ARFFNumericType`, `ARFFStringType`, `ARFFDateType` and `ARFFNominalType`.
"""
abstract type ARFFType end

abstract type ARFFHeaderItem end

"""
    ARFFAttribute

Represents a single ARFF @attribute.

It has a `name` and a `type` (a [`ARFFType`](@ref)).
"""
struct ARFFAttribute <: ARFFHeaderItem
    name::String
    type::ARFFType
end

struct ARFFNumericType <: ARFFType end

struct ARFFStringType <: ARFFType end

struct ARFFDateType <: ARFFType
    format::String
end
ARFFDateType() = ARFFDateType("yyyy-MM-dd'T'HH:mm:ss")

struct ARFFNominalType <: ARFFType
    classes::Vector{String}
end

struct ARFFRelationalType <: ARFFType
    attributes::Vector{ARFFAttribute}
end
ARFFRelationalType() = ARFFRelationalType(ARFFAttribute[])

struct ARFFRelation <: ARFFHeaderItem
    name::String
end

struct ARFFDataStart <: ARFFHeaderItem end

"""
    ARFFHeader

Represents the header information in an ARFF file.

It has these fields:
- `relation`: the @relation name.
- `attributes`: vector of each @attribute as an [`ARFFAttribute`](@ref).
"""
struct ARFFHeader
    relation::String
    attributes::Vector{ARFFAttribute}
end

const ARFFTable = Tables.DictColumnTable
const ARFFRow = Tables.ColumnsRow

"""
    Parsing

Sub-module handling low-level parsing of lines of ARFF files.
"""
module Parsing

using Parsers
using ..ARFFFiles: ARFFType, ARFFNumericType, ARFFStringType, ARFFDateType, ARFFNominalType, ARFFRelationalType, ARFFHeaderItem, ARFFRelation, ARFFAttribute, ARFFDataStart, ARFFHeader

function skipspace(data, pos, len)
    @inbounds while pos ≤ len
        c = data[pos]
        if c == 0x20 || c == 0x09
            pos += 1
        elseif c == 0x25
            pos = len + 1
        else
            break
        end
    end
    return pos
end

function expect(data, pos, len, offset, char)
    @inbounds if pos ≤ len && data[pos] in char
        return pos + 1
    else
        error("expecting $(map(Char, char)) at byte $(pos+offset)")
    end
end

function parse_header(io::IO,
    opts_sq=options('''),
    opts_dq=options('"'),
)
    relation = missing
    attributes = ARFFAttribute[]
    depth = 0
    @inbounds while !eof(io)
        offset = position(io)
        data = codeunits(readline(io))
        len = length(data)
        pos = skipspace(data, 1, len)
        pos ≤ len || continue
        # there is something on this line, it better start with '@'
        pos0 = expect(data, pos, len, offset, 0x40) # @
        pos, str = parse_string(data, pos0, len, offset, opts_sq, opts_dq)
        lstr = lowercase(str)
        # @RELATION
        if lstr == "relation"
            depth == 0 || error("@relation specified inside relational attribute")
            pos = skipspace(data, pos, len)
            relation === missing || error("@relation seen twice")
            pos, relation = parse_string(data, pos, len, offset, opts_sq, opts_dq)
            # @ATTRIBUTE
        elseif lstr == "attribute"
            pos = skipspace(data, pos, len)
            relation === missing && error("@attribute specified before @relation")
            pos, attrname = parse_string(data, pos, len, offset, opts_sq, opts_dq)
            pos = skipspace(data, pos, len)
            pos, attrtype = parse_type(data, pos, len, offset, opts_sq, opts_dq)
            attrs = attributes
            for i in 1:depth
                attrs = (attrs[end].type::ARFFRelationalType).attributes
            end
            push!(attrs, ARFFAttribute(attrname, attrtype))
            if attrtype isa ARFFRelationalType
                depth += 1
            end
            # @END
        elseif lstr == "end"
            depth > 0 || error("@end specified outside any relational attribute")
            pos = skipspace(data, pos, len)
            pos, attrname = parse_string(data, pos, len, offset, opts_sq, opts_dq)
            attr = ARFFAttribute("", ARFFRelationalType(attributes))
            for i in 1:depth
                attr = (attr.type::ARFFRelationalType).attributes[end]
            end
            attrname == attr.name || error("@end for $(repr(attrname)) but inside $(repr(attr.name))")
            depth -= 1
            # @DATA
        elseif lstr == "data"
            depth == 0 || error("@data specified inside relational attribute")
            relation === missing && error("@data specified before @relation")
            return ARFFHeader(relation, attributes)
        else
            error("invalid header item $(repr(str)) at byte $(pos0+offset)")
        end
        pos = skipspace(data, pos, len)
        pos > len || error("expecting end of line at byte $(pos+offset)")
    end
end

function parse_string(data, pos, len, offset, opts1, opts2)
    pos ≤ len || error("expecting string at byte $(pos+offset)")
    c = data[pos]
    escaped = false
    @inbounds if c in (0x27, 0x22)  # ' "
        q = c
        pos1 = pos
        pos += 1
        pos0 = pos
        while true
            pos ≤ len || error("unexpected end of line while parsing string at byte $(pos+offset)")
            c = data[pos]
            if c == q
                pos += 1
                break
            elseif c == 0x5C # \
                escaped = true
                pos += 1
                pos ≤ len || error("invalid escape sequence at byte $(pos+offset)")
                pos1 = pos
                pos += 1
            else
                pos1 = pos
                pos += 1
            end
        end
    else
        pos0 = pos
        pos1 = pos - 1
        while pos ≤ len
            c = data[pos]
            if c in (0x27, 0x22, 0x20, 0x09, 0x2C, 0x5C, 0x7B, 0x7D)
                break
            else
                pos1 = pos
                pos += 1
            end
        end
        pos1 < pos0 && error("invalid string at byte $(pos+offset)")
    end
    str = String(data[pos0:pos1])
    if escaped
        str = replace(str, r"\\.?" => parse_escape)
    end
    return pos, str
end

function parse_type(data, pos, len, offset, opts1, opts2)
    pos ≤ len || error("expecting type at byte $(pos+offset)")
    pos0 = pos
    if data[pos] == 0x7B # {
        pos += 1
        items = String[]
        while true
            pos = skipspace(data, pos, len)
            pos ≤ len || error("expecting nominal value at byte $(pos+offset)")
            c = data[pos]
            if c == 0x7D && isempty(items) # }
                pos += 1
                break
            end
            pos, item = parse_string(data, pos, len, offset, opts1, opts2)
            push!(items, item)
            pos = skipspace(data, pos, len)
            pos ≤ len || error("expecting ',' or '}' at byte $(pos+offset)")
            c = data[pos]
            if c == 0x7D # }
                pos += 1
                break
            elseif c == 0x2C # ,
                pos += 1
                continue
            else
                error("expecting ',' or '}' at byte $(pos+offset)")
            end
        end
        return pos, ARFFNominalType(items)
    else
        pos, str = parse_string(data, pos, len, offset, opts1, opts2)
        pos = skipspace(data, pos, len)
        lstr = lowercase(str)
        if lstr in ("numeric", "integer", "real")
            return pos, ARFFNumericType()
        elseif lstr == "string"
            return pos, ARFFStringType()
        elseif lstr == "date"
            if pos ≤ len
                pos, fmt = parse_string(data, pos, len, offset, opts1, opts2)
                return pos, ARFFDateType(fmt)
            else
                return pos, ARFFDateType(fmt)
            end
        elseif lstr == "relational"
            return pos, ARFFRelationalType()
        else
            error("invalid type $(repr(str)) at byte $(pos0+offset)")
        end
    end
end

function parse_index(data, pos, len, offset)
    value = 0
    count = 0
    @inbounds while pos ≤ len
        c = data[pos]
        if 0x30 ≤ c ≤ 0x39
            pos += 1
            count += 1
            digit = c - 0x30
            value > fld(typemax(Int), 10) && error("column index too large at byte $(pos+offset)")
            value *= 10
            value > typemax(Int) - digit && error("column index too large at byte $(pos+offset)")
            value += digit
        else
            break
        end
    end
    count == 0 && error("expecting column index at byte $(pos+offset)")
    return pos, value
end

options(qc; df=nothing) = Parsers.Options(sentinel=["?"], openquotechar=qc, closequotechar=qc, escapechar='\\', delim=',', quoted=true, comment="%", ignoreemptylines=true, dateformat=df)

function parse_datum(::Type{T}, data::AbstractVector{UInt8}, pos::Integer=1, len::Integer=length(data) - (pos - 1), opts1=parse_opts('''), opts2=parse_opts('"')) where {T}
    # first try parsing single-quoted
    res1 = Parsers.xparse(T, data, pos, len, opts1)
    if Parsers.invalid(res1.code)
        # invalid: try again
    elseif T == String && !Parsers.quoted(res1.code) && res1.val.len > 0 && @inbounds data[skipspace(data, res1.val.pos, len)] == opts2.oq
        # double quoted: try again
    else
        return res1
    end
    # now try double-quoted
    res2 = Parsers.xparse(T, data, pos, len, opts2)
    if T == String && !Parsers.invalid(res2.code) && !Parsers.quoted(res2.code) && res2.val.len > 0 && @inbounds data[skipspace(data, res2.val.pos, len)] == opts1.oq
        # single-quoted (can't also be double-quoted)
        @assert Parsers.invalid(res1.code)
        return res1
    else
        return res2
    end
end

function parse_escape(str)
    if length(str) == 2
        c = str[2]
        c == '0' ? '\0' :
        c == 'a' ? '\a' :
        c == 'b' ? '\b' :
        c == 'e' ? '\e' :
        c == 'f' ? '\f' :
        c == 'n' ? '\n' :
        c == 'r' ? '\r' :
        c == 't' ? '\t' :
        c == 'v' ? '\v' :
        ispunct(c) ? c :
        error("invalid escape sequence: $(repr(str))")
    else
        error("backslash at end of string")
    end
end

function get_parsed_string(data, res)
    str::SubString{String} = Parsers.getstring(data, Parsers.PosLen(res.val.pos, res.val.len), 0x00)
    if !Parsers.quoted(res.code)
        str = strip(str)
    end
    if Parsers.escapedstring(res.code)
        str = replace(str, r"\\.?" => parse_escape)
    end
    return str
end

end

"""
    parse_javadateformat(java::AbstractString)

Convert the given Java date format string to the equivalent Julia `DateFormat`.

See https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html.

Only the following format characters are currently supported:
`y` → `y` (year),
`M` → `m` (month),
`d` → `d` (day),
`H` → `H` (hour),
`m` → `M` (minute),
`s` → `S` (second) and
`S` → `s` (millisecond).
"""
function parse_javadateformat(java::AbstractString)
    io = IOBuffer()
    quoted = false # true if a quote has been opened
    justquoted = false # true if a quote has just been opened (so closing it inserts a quote)
    for c in java
        if c == '\''
            if justquoted
                write(io, '\\')
                write(io, '\'')
                quoted = justquoted = false
            elseif quoted
                quoted = justquoted = false
            else
                quoted = justquoted = true
            end
        elseif quoted
            justquoted = false
            write(io, '\\')
            write(io, c)
        elseif c in 'a':'z' || c in 'A':'Z'
            # year
            if c == 'y'
                write(io, 'y')
                # month of year
            elseif c == 'M'
                write(io, 'm')
                # day of month
            elseif c == 'd'
                write(io, 'd')
                # hour of day
            elseif c == 'H'
                write(io, 'H')
                # minute of hour
            elseif c == 'm'
                write(io, 'M')
                # second of minute
            elseif c == 's'
                write(io, 'S')
                # millisecond of second
            elseif c == 'S'
                write(io, 's')
            else
                error("unsupported format character: $c")
            end
        else
            write(io, '\\')
            write(io, c)
        end
    end
    quoted && error("unclosed quote in date format: $(repr(arff))")
    return DateFormat(String(take!(io)))
end

const CatVal = eltype(CategoricalVector{String,UInt32}(undef, 0))
const CatVec = typeof(CategoricalVector{String,UInt32}(undef, 0))
const CatMissVec = typeof(CategoricalVector{Union{Missing,String},UInt32}(undef, 0))
const CatPool = typeof(CategoricalVector{String,UInt32}(undef, 0).pool)

"""
    ARFFReader

An object holding an IO stream of an ARFF file, used to access its data.

Header information is in the `header` field, of type [`ARFFHeader`](@ref).

It has the following functionality:
- `nextrow(r)` returns the next row of data as a `NamedTuple{names, types}`, or `nothing` if everything has been read.
- `read(r, [n])` reads up to `n` rows as a vector.
- `read!(xs, r)` reads up to `length(xs)` rows into the given vector, returning the number of rows read.
- `close(r)` closes the underlying IO stream, unless it was created with `own=false`.
- `eof(r)` tests whether the IO stream is at the end.
- Iteration yields rows of `r`.
- It satisfies the `Tables.jl` interface, so e.g. `DataFrame(r)` does what you think.
"""
mutable struct ARFFReader{IO}
    # parsing
    io::IO
    own_io::Bool
    header::ARFFHeader
    # columns
    colnames::Vector{Symbol} # name
    colkinds::Vector{Symbol} # :N, :S, :D, :C, :R (numeric, string, date, categorical, relational)
    colmissings::BitVector # true if the column can have missing elements
    automissingcols::Bool # true if missincols === :auto
    colmissingsdetected::BitVector # true if missing is detected in the column
    coltypes::Vector{Type} # combines kind and missing
    colkindidxs::Vector{Int} # the ith column is the colkindidxs[i]th of its kind
    coltypeidxs::Vector{Int} # the ith column is the coltypeidxs[i]th of its type
    pools::Vector{CatPool} # pools for categorical columns
    dateformats::Vector{DateFormat} # formats for date columns
    readers::Vector{ARFFReader{IOBuffer}} # readers for relational columns
    # row iteration
    chunk::ARFFTable
    chunklen::Int
    chunkidx::Int
    chunkbytes::Int
end

Base.close(r::ARFFReader) = r.own_io ? close(r.io) : nothing

Base.eof(r::ARFFReader) = eof(r.io)

_schema(names, types) = Tables.Schema(names, types, stored=true)

"""
    loadstreaming(io::IO, own=false; [missingcols=true], [missingnan=false], [categorical=true], [chunkbytes=2^26])
    loadstreaming(filename::AbstractString; ...)
    loadstreaming(f, file, ...)

An [`ARFFReader`](@ref) object for reading the given ARFF file one record at a time.

If `f` is given, then this is like `f(loadstreaming(file, ...))` but ensures the reader is
closed afterwards.

Option `missingcols` specifies which columns can contain missing data. It can be `:auto`
(columns with missing values are automatically detected, the default), `:all` or `true` (all
columns), `:none` or `false` (no columns), a set or vector of column names, or a function
taking a column name and returning true or false. Note that `:auto` does not apply if the
table is being read in a streaming fashion, in which case it behaves like `:all`.

Option `missingnan` specifies whether or not to convert missing values in numeric columns to
`NaN`. This is equivalent to excluding these columns in `missingcols`.

Option `categorical` specifies whether or not to convert nominal columns to `CategoricalValue`
or `String`.

Option `chunkbytes` specifies approximately how many bytes to read per chunk when iterating
over chunks or rows.
"""
function loadstreaming(io::IO, own::Bool=false; missingcols=:auto, missingnan::Bool=false, categorical::Bool=true, chunkbytes::Integer=1 << 26, header::Union{ARFFHeader,Nothing}=nothing)
    automissingcols = missingcols === :auto
    missingcols =
        missingcols === true || missingcols === :auto || missingcols === :all ? c -> true :
        missingcols === false || missingcols === :none ? c -> false :
        missingcols isa Union{AbstractSet,AbstractVector} ? ∈(missingcols) :
        missingcols
    if header === nothing
        header = Parsing.parse_header(io)
    end
    colnames = Symbol[]
    colkinds = Symbol[]
    colmissings = BitVector()
    coltypes = Type[]
    coltypeidxs = Int[]
    colkindidxs = Int[]
    pools = CatPool[]
    dateformats = DateFormat[]
    readers = ARFFReader{IOBuffer}[]
    nN = nNX = nS = nSX = nC = nCX = nD = nDX = nR = nRX = 0
    for a in header.attributes
        n = Symbol(a.name)
        m = missingcols(n)::Bool && !(missingnan && a.type isa ARFFNumericType)
        at = a.type
        if at isa ARFFNumericType
            k = :N
            t = Float64
            jt = m ? (nNX += 1) : (nN += 1)
            jk = nN + nNX
        elseif categorical && at isa ARFFNominalType
            k = :C
            t = CatVal
            jt = m ? (nCX += 1) : (nC += 1)
            jk = nC + nCX
            push!(pools, CategoricalPool{String,UInt32}(a.type.classes))
        elseif at isa ARFFStringType || at isa ARFFNominalType
            k = :S
            t = String
            jt = m ? (nSX += 1) : (nS += 1)
            jk = nS + nSX
        elseif at isa ARFFDateType
            k = :D
            t = DateTime
            jt = m ? (nDX += 1) : (nD += 1)
            jk = nD + nDX
            push!(dateformats, parse_javadateformat(at.format))
        elseif at isa ARFFRelationalType
            k = :R
            t = ARFFTable
            jt = m ? (nRX += 1) : (nR += 1)
            jk = nR + nRX
            rdr = loadstreaming(IOBuffer(); missingcols, missingnan, categorical, chunkbytes, header=ARFFHeader("<nested>", at.attributes))
            rdr.automissingcols = automissingcols
            push!(readers, rdr)
        else
            error("not implemented")
        end
        if m
            t = Union{t,Missing}
        end
        push!(colnames, n)
        push!(colkinds, k)
        push!(coltypes, t)
        push!(colmissings, m)
        push!(coltypeidxs, jt)
        push!(colkindidxs, jk)
    end
    ARFFReader{typeof(io)}(io, own, header, colnames, colkinds, colmissings, automissingcols, falses(length(colmissings)), coltypes, colkindidxs, coltypeidxs, pools, dateformats, readers, ARFFTable(_schema([], []), Dict()), 0, 0, chunkbytes)
end

loadstreaming(fn::AbstractString; opts...) = loadstreaming(open(fn), true; opts...)

function loadstreaming(f, fn::Union{IO,AbstractString}, args...; opts...)
    r = loadstreaming(fn, args...; opts...)
    try
        return f(r)
    finally
        close(r)
    end
end

"""
    load([f], file, ...)

Load the entire ARFF file (IO or filename) as a table.

If `f` is given, then this function is like `f(load(file, ...))`, except that any
relational columns (where each data point is itself a table) also have `f` applied.

For example `load(DataFrame, file)` loads the file as a `DataFrame`. Replace `DataFrame`
with your favourite table type.

See [`loadstreaming`](@ref) for the keyword arguments.
"""
function load(fn::Union{IO,AbstractString}, args...; opts...)
    r = loadstreaming(fn, args...; opts...)
    try
        return readcolumns(r)
    finally
        close(r)
    end
end
function load(f, fn::Union{IO,AbstractString}, args...; opts...)
    table = load(fn, args...; opts...)
    return materialize_recursive(f, table)
end

function materialize_recursive(f, table::ARFFTable)
    schema = Tables.schema(table)
    if !any(ARFFTable <: T for T in schema.types)
        return f(table)
    end
    colnames = Symbol[]
    coltypes = Type[]
    cols = Dict{Symbol,AbstractVector}()
    for (colname, coltype) in zip(schema.names, schema.types)
        col = Tables.getcolumn(table, colname)
        if ARFFTable <: coltype
            col = [x isa ARFFTable ? materialize_recursive(f, x) : x for x in col]
            coltype = eltype(col)
        end
        push!(colnames, colname)
        push!(coltypes, coltype)
        cols[colname] = col
    end
    return f(ARFFTable(_schema(colnames, coltypes), cols))
end

"""
    load_header(file, ...)

Equivalent to `load(r->r.header, file, ...)`, which loads just the header from the given file as a `ARFFHeader`.
"""
function load_header(fn::Union{IO,AbstractString}, args...; opts...)
    r = loadstreaming(fn, args...; opts...)
    h = r.header
    close(r)
    return h
end

"""
    loadchunks(file, ...)
    loadchunks(f, file, ...)

The first form opens the ARFF file and returns an iterator over chunks of the file.
It is equivalent to `Tables.partitions(loadstreaming(file, ...))`.

The second form is equivalent to `f(loadchunks(file, ...))` but ensures that the file is
closed afterwards.
"""
function loadchunks(f, fn::Union{IO,AbstractString}, args...; opts...)
    r = loadstreaming(fn, args...; opts...)
    try
        return f(Tables.partitions(r))
    finally
        close(r)
    end
end
loadchunks(fn::Union{IO,AbstractString}, args...; opts...) = loadchunks(identity, fn, args...; opts...)

"""
    nextrow(r::ARFFReader{names, types}) :: Union{Nothing, NamedTuple{names, types}}

The next row of data from the given `ARFFReader`, or `nothing` if everything has been read.
"""
function nextrow(r::ARFFReader)::Union{Nothing,ARFFRow}
    while r.chunkidx ≥ r.chunklen
        eof(r.io) && return nothing
        r.chunk = readcolumns(r, maxbytes=r.chunkbytes)
        r.chunklen = Tables.rowcount(r.chunk)
        r.chunkidx = 0
    end
    r.chunkidx += 1
    return ARFFRow(r.chunk, r.chunkidx)
end

function Base.read!(r::ARFFReader, x::AbstractVector)
    n = 0
    for i in eachindex(x)
        z = nextrow(r)
        if z === nothing
            return n
        else
            x[i] = z
            n += 1
        end
    end
    return n
end

function Base.read(r::ARFFReader)
    x = ARFFRow[]
    while true
        z = nextrow(r)
        if z === nothing
            return x
        else
            push!(x, z)
        end
    end
end

function Base.read(r::ARFFReader, n::Integer)
    x = Vector{ARFFRow}(undef, n)
    n = read!(r, x)
    resize!(x, n)
    x
end

"""
    readcolumns(r::ARFFReader, maxbytes=nothing)

Read the data from `r` into a columnar table.

By default the entire table is read. If `maxbytes` is given, approximately this many bytes
of the input stream is read instead, allowing for reading the table in chunks.

The same can be achieved by iterating over `Tables.partitions(r)`.
"""
function readcolumns(
    r::ARFFReader;
    opts_sq=Parsing.options('''),
    opts_dq=Parsing.options('"'),
    date_opts_sq=[Parsing.options('''; df=df) for df in r.dateformats],
    date_opts_dq=[Parsing.options('"'; df=df) for df in r.dateformats],
    maxbytes=nothing,
    chunkbytes=1 << 20,
)::ARFFTable
    # initialize columns
    ncols = length(r.colnames)
    Ncols = Vector{Float64}[]
    NXcols = Vector{Union{Missing,Float64}}[]
    Scols = Vector{String}[]
    SXcols = Vector{Union{Missing,String}}[]
    Dcols = Vector{DateTime}[]
    DXcols = Vector{Union{Missing,DateTime}}[]
    Ccols = CatVec[]
    CXcols = CatMissVec[]
    Rcols = Vector{ARFFTable}[]
    RXcols = Vector{Union{Missing,ARFFTable}}[]
    cols = AbstractVector[]
    iC = 0
    @inbounds for i in 1:ncols
        kind = r.colkinds[i]
        missable = r.colmissings[i]
        if kind == :N
            if !missable
                col = Float64[]
                push!(Ncols, col)
                push!(cols, col)
            else
                col = Union{Missing,Float64}[]
                push!(NXcols, col)
                push!(cols, col)
            end
        elseif kind == :S
            if !missable
                col = String[]
                push!(Scols, col)
                push!(cols, col)
            else
                col = Union{Missing,String}[]
                push!(SXcols, col)
                push!(cols, col)
            end
        elseif kind == :D
            if !missable
                col = DateTime[]
                push!(Dcols, col)
                push!(cols, col)
            else
                col = Union{Missing,DateTime}[]
                push!(DXcols, col)
                push!(cols, col)
            end
        elseif kind == :C
            iC += 1
            if !missable
                col = CategoricalVector{String}(UInt32[], r.pools[iC])
                push!(Ccols, col)
                push!(cols, col)
            else
                col = CategoricalVector{Union{String,Missing}}(UInt32[], r.pools[iC])
                push!(CXcols, col)
                push!(cols, col)
            end
        elseif kind == :R
            if !missable
                col = ARFFTable[]
                push!(Rcols, col)
                push!(cols, col)
            else
                col = Union{Missing,ARFFTable}[]
                push!(RXcols, col)
                push!(cols, col)
            end
        else
            error("Not implemented: Columns of kind $kind")
        end
    end
    # reading loop
    nrows = 0
    nbytes = 0
    io = r.io
    @inbounds while !eof(io) && (maxbytes === nothing || nbytes < maxbytes)
        # remeber where we started
        offset = position(io)
        # read a chunk
        if chunkbytes === nothing && maxbytes === nothing
            chunk = read(io)
        else
            chunk = read(io, max(0, min(chunkbytes === nothing ? typemax(Int) : chunkbytes, maxbytes === nothing ? typemax(Int) : maxbytes - nbytes)))
        end
        # ensure we get a whole line
        append!(chunk, codeunits(readline(io)))
        # read each line
        pos = 1
        len = length(chunk)
        nbytes += len
        while pos ≤ len
            # skip any initial whitespace (including newlines)
            c = chunk[pos]
            if c == 0x20 || c == 0x09 || c == 0x0A || c == 0x0D
                pos += 1
                continue
            end
            # skip comments
            if c == 0x25
                while pos ≤ len
                    c = chunk[pos]
                    if c == 0x0A || c == 0x0D
                        break
                    else
                        pos += 1
                    end
                end
                continue
            end
            nrows += 1
            if c == 0x7B
                # sparse format
                pos += 1
                pos = Parsing.skipspace(chunk, pos, len)
                pos ≤ len || error("unexpected end of input at byte $(pos+offset)")
                if chunk[pos] == 0x7D
                    pos += 1
                else
                    # HACK: replace the final } on the line with a newline so it's CSV-like
                    bpos = 0
                    for cpos in pos:len
                        c = chunk[cpos]
                        if c == 0x7D
                            bpos = cpos
                        elseif c == 0x0A || c == 0x0D
                            break
                        end
                    end
                    if bpos == 0
                        error("no closing } in row $nrows")
                    else
                        chunk[bpos] = 0x0A
                    end
                    # read each item
                    while true
                        pos = Parsing.skipspace(chunk, pos, len)
                        pos, i = Parsing.parse_index(chunk, pos, len, offset)
                        i += 1
                        1 ≤ i ≤ ncols || error("sparse column index out of range at byte $(pos+offset)")
                        pos = Parsing.skipspace(chunk, pos, len)
                        pos, done = _readcolumns_readdatum(r, chunk, pos, len, offset, i, true, nrows, ncols, opts_sq, opts_dq, date_opts_sq, date_opts_dq, Ncols, NXcols, Scols, SXcols, Dcols, DXcols, Ccols, CXcols, Rcols, RXcols)
                        done && break
                    end
                end
                # insert zeros
                for i in 1:ncols
                    kind = r.colkinds[i]
                    missable = r.colmissings[i]
                    idx = r.coltypeidxs[i]
                    if kind == :N
                        if missable
                            _readcolumns_pushzero(r, i, NXcols[idx], nrows, true)
                        else
                            _readcolumns_pushzero(r, i, Ncols[idx], nrows, true)
                        end
                    elseif kind == :S
                        if missable
                            _readcolumns_pushzero(r, i, SXcols[idx], nrows, true)
                        else
                            _readcolumns_pushzero(r, i, Scols[idx], nrows, true)
                        end
                    elseif kind == :D
                        if missable
                            _readcolumns_pushzero(r, i, DXcols[idx], nrows, false)
                        else
                            _readcolumns_pushzero(r, i, Dcols[idx], nrows, false)
                        end
                    elseif kind == :C
                        if missable
                            _readcolumns_pushzero(r, i, CXcols[idx], nrows, true)
                        else
                            _readcolumns_pushzero(r, i, Ccols[idx], nrows, true)
                        end
                    else
                        error()
                    end
                end
            else
                # dense format
                for i in 1:ncols
                    pos, done = _readcolumns_readdatum(r, chunk, pos, len, offset, i, false, nrows, ncols, opts_sq, opts_dq, date_opts_sq, date_opts_dq, Ncols, NXcols, Scols, SXcols, Dcols, DXcols, Ccols, CXcols, Rcols, RXcols)
                    @assert done == (i == ncols)
                end
            end
        end
    end
    # strip unnecessary missings
    if maxbytes === nothing && r.automissingcols
        coltypes = copy(r.coltypes) # don't overwrite r.coltypes
        for i in 1:length(cols)
            if r.colmissings[i] && !r.colmissingsdetected[i]
                coltypes[i] = Base.nonmissingtype(coltypes[i])
                cols[i] = convert(AbstractVector{coltypes[i]}, cols[i])
            end
        end
    else
        coltypes = r.coltypes
    end
    # construct the output table
    schema = _schema(r.colnames, coltypes)
    dict = Dict(zip(r.colnames, cols))
    return ARFFTable(schema, dict)
end

@inline function _readcolumns_readdatum(r, chunk, pos, len, offset, i, sparse, nrows, ncols, opts_sq, opts_dq, date_opts_sq, date_opts_dq, Ncols, NXcols, Scols, SXcols, Dcols, DXcols, Ccols, CXcols, Rcols, RXcols)
    @inbounds begin
        k = r.colkinds[i]
        m = r.colmissings[i]
        jt = r.coltypeidxs[i]
        jk = r.colkindidxs[i]
        if k == :N
            if m
                _readcolumns_readdatum(r, Val(:NX), chunk, pos, len, offset, i, sparse, nrows, ncols, opts_sq, opts_dq, NXcols[jt], nothing)
            else
                _readcolumns_readdatum(r, Val(:N), chunk, pos, len, offset, i, sparse, nrows, ncols, opts_sq, opts_dq, Ncols[jt], nothing)
            end
        elseif k == :S
            if m
                _readcolumns_readdatum(r, Val(:SX), chunk, pos, len, offset, i, sparse, nrows, ncols, opts_sq, opts_dq, SXcols[jt], nothing)
            else
                _readcolumns_readdatum(r, Val(:S), chunk, pos, len, offset, i, sparse, nrows, ncols, opts_sq, opts_dq, Scols[jt], nothing)
            end
        elseif k == :D
            opts1 = date_opts_sq[jk]
            opts2 = date_opts_dq[jk]
            if m
                _readcolumns_readdatum(r, Val(:DX), chunk, pos, len, offset, i, sparse, nrows, ncols, opts1, opts2, DXcols[jt], nothing)
            else
                _readcolumns_readdatum(r, Val(:D), chunk, pos, len, offset, i, sparse, nrows, ncols, opts1, opts2, Dcols[jt], nothing)
            end
        elseif k == :C
            pool = r.pools[jk]
            if m
                _readcolumns_readdatum(r, Val(:CX), chunk, pos, len, offset, i, sparse, nrows, ncols, opts_sq, opts_dq, CXcols[jt], pool)
            else
                _readcolumns_readdatum(r, Val(:C), chunk, pos, len, offset, i, sparse, nrows, ncols, opts_sq, opts_dq, Ccols[jt], pool)
            end
        elseif k == :R
            r2 = r.readers[jk]
            if m
                _readcolumns_readdatum(r, Val(:RX), chunk, pos, len, offset, i, sparse, nrows, ncols, opts_sq, opts_dq, RXcols[jt], r2)
            else
                _readcolumns_readdatum(r, Val(:R), chunk, pos, len, offset, i, sparse, nrows, ncols, opts_sq, opts_dq, Rcols[jt], r2)
            end
        else
            error()
        end
    end
end

@inline function _readcolumns_readdatum(r, ::Val{kind}, chunk, pos, len, offset, i, sparse, nrows, ncols, opts1, opts2, col, info) where {kind}
    # select the type to parse
    if kind == :N || kind == :NX
        T = Float64
    elseif kind == :S || kind == :SX || kind == :C || kind == :CX || kind == :R || kind == :RX
        T = String
    elseif kind == :D || kind == :DX
        T = DateTime
    else
        error()
    end
    # parse a datum
    res = Parsing.parse_datum(T, chunk, pos, len, opts1, opts2)
    # check for errors
    Parsers.invalid(res.code) && error("Could not parse $T at byte $(pos+offset)")
    # increment
    pos += res.tlen
    # check delimiters
    if sparse
        done = Parsers.newline(res.code) || Parsers.eof(res.code)
        done || Parsers.delimited(res.code) || error("Expecting delimiter or new line at byte $(pos+offset)")
    elseif i < ncols
        Parsers.delimited(res.code) || error("Expecting delimiter at byte $(pos+offset)")
        done = false
    else
        Parsers.newline(res.code) || Parsers.eof(res.code) || error("Expecting new line at byte $(pos+offset)")
        done = true
    end
    # ensure the column is the right size for the value
    if sparse && length(col) == nrows
        error("Column '$(r.colnames[i])' (index $(i-1)) seen twice in row $nrows")
    end
    @assert length(col) == nrows - 1
    # push the value
    if Parsers.sentinel(res.code)
        if kind == :N
            push!(col, NaN)
        elseif kind == :NX || kind == :SX || kind == :DX || kind == :CX || kind == :RX
            push!(col, missing)
            r.colmissingsdetected[i] = true
        else
            error("Got missing value in column '$(r.colnames[i])' of row $nrows")
        end
    else
        if kind == :N || kind == :NX || kind == :D || kind == :DX
            push!(col, res.val)
        elseif kind == :S || kind == :SX
            str = Parsing.get_parsed_string(chunk, res)
            push!(col, str)
        elseif kind == :C || kind == :CX
            str = Parsing.get_parsed_string(chunk, res)
            pool = info
            if haskey(pool.invindex, str)
                push!(col.refs, get(pool, str))
            else
                error("Invalid nominal $(repr(str)) in column '$(r.colnames[i])' of row $nrows, expecting one of $(join(map(repr, pool.levels), ", ", " or "))")
            end
        elseif kind == :R || kind == :RX
            str = Parsing.get_parsed_string(chunk, res)
            r2 = info
            r2.io = IOBuffer(str)
            push!(col, readcolumns(r2))
        else
            error()
        end
    end
    # done
    return (pos, done)
end

_zero(::Any, ::Any, ::Any, ::Any) = 0
function _zero(::AbstractVector{<:Union{<:AbstractString,Missing}}, r, i, nrows)
    @warn "Value of string column '$(r.colnames[i])' (index $(i-1)) is not specified in sparse row $nrows, inserting empty string (see warning at https://waikato.github.io/weka-wiki/formats_and_processing/arff_stable/#sparse-arff-files)"
    ""
end
function _zero(::CategoricalVector, r, i, nrows)
    r.pools[r.colkindidxs[i]][1]
end
@inline function _readcolumns_pushzero(r, i, col, nrows, avail)
    n = length(col)
    if n == nrows - 1
        if avail
            push!(col, _zero(col, r, i, nrows))
        else
            error("Value of non-numeric column '$(r.colnames[i])' (index $(i-1)) not specified in sparse row $nrows")
        end
    else
        @assert n == nrows
    end
    return
end

### PARTITIONS

struct ARFFChunks
    reader::ARFFReader
end

function Base.iterate(x::ARFFChunks, st=nothing)
    if eof(x.reader)
        return nothing
    else
        return (readcolumns(x.reader, maxbytes=x.reader.chunkbytes), nothing)
    end
end

Base.eltype(::Type{ARFFChunks}) = ARFFTable

Base.IteratorSize(::Type{ARFFChunks}) = Base.SizeUnknown()

Base.eof(x::ARFFChunks) = eof(x.reader)

Base.close(x::ARFFChunks) = close(x.reader)

### SAVING

function write_datum(io::IO, x::AbstractString)
    if eltype(x) != Char
        x = convert(String, x)
    end
    write(io, ''')
    for c in x
        c == '\0' ? write(io, "\\0") :
        c == ''' ? write(io, "\\'") :
        c == '\\' ? write(io, "\\\\") :
        c == '\a' ? write(io, "\\a") :
        c == '\b' ? write(io, "\\b") :
        c == '\e' ? write(io, "\\e") :
        c == '\f' ? write(io, "\\f") :
        c == '\n' ? write(io, "\\n") :
        c == '\r' ? write(io, "\\r") :
        c == '\t' ? write(io, "\\t") :
        c == '\v' ? write(io, "\\v") :
        write(io, c)
    end
    write(io, ''')
end
write_datum(io::IO, x::Union{Int8,UInt8,Int16,UInt16,Int32,UInt32,Int64,UInt64,Int128,UInt128,BigInt,Float16,Float32,Float64,BigFloat}) = print(io, x)
write_datum(io::IO, x::Bool) = print(io, x ? "1" : "0")
write_datum(io::IO, x::Integer) = write_datum(io, convert(BigInt, x))
write_datum(io::IO, x::Real) = write_datum(io, convert(BigFloat, x))
write_datum(io::IO, x::DateTime) = write_datum(io, Dates.format(x, dateformat"YYYY-mm-dd\THH:MM:SS.sss"))
write_datum(io::IO, x::Date) = write_datum(io, DateTime(x))
write_datum(io::IO, x::CategoricalValue{<:AbstractString}) = write_datum(io, x.pool.levels[x.ref])
write_datum(io::IO, ::Missing) = write(io, "?")

@generated function write_data(io::IO, rows, ::Val{N}) where {N}
    exs = []
    for i in 1:N
        push!(exs, :(write_datum(io, row[$i])))
        push!(exs, :(print(io, $(i == N ? "\n" : ","))))
    end
    quote
        for row in rows
            $(exs...)
        end
    end
end

"""
    save(file, table; relation="data", comment=...)

Save the Tables.jl-compatible `table` in ARFF format to `file`,
which must be an IO stream or file.

The relation name is `relation`. The given `comment` is written at
the top of the file.
"""
function save(io::IO, df;
    relation::AbstractString="data",
    comment::AbstractString="Written by ARFFFiles.jl at $(Dates.now())",
)
    rows = Tables.rows(df)
    schema = Tables.schema(rows)
    schema === nothing && error("schema unknown")
    # comment
    if !isempty(comment)
        for line in split(comment, '\n')
            println(io, "% ", line)
        end
        println(io)
    end
    # relation
    print(io, "@RELATION ")
    write_datum(io, relation)
    println(io)
    println(io)
    # attributes
    for (name, type) in zip(schema.names, schema.types)
        print(io, "@ATTRIBUTE ")
        write_datum(io, string(name))
        print(io, " ")
        if type <: Missing
            println(io, "{}")
        elseif type <: Union{Real,Missing}
            println(io, "NUMERIC")
        elseif type <: Union{AbstractString,Missing}
            println(io, "STRING")
        elseif type <: Union{Date,DateTime,Missing}
            println(io, "DATE \"yyyy-MM-dd'T'HH:mm:ss.SSS\"")
        elseif type <: Union{<:CategoricalValue{<:AbstractString},Missing}
            # find the levels of the first non-missing entry
            levels = nothing
            for x in Tables.getcolumn(Tables.columns(df), name)
                if x !== missing
                    levels = x.pool.levels
                    break
                end
            end
            if levels === nothing || isempty(levels)
                println(io, "{}")
            else
                for (i, level) in enumerate(levels)
                    print(io, i == 1 ? "{" : ",")
                    write_datum(io, level)
                end
                println(io, "}")
            end
        else
            error("ARFF does not support data of type $type in column $name")
        end
    end
    println(io)
    # data
    println(io, "@DATA")
    write_data(io, rows, Val(length(schema.names)))
end

save(filename::AbstractString, df; opts...) = open(io -> save(io, df; opts...), filename, "w")

### ITERATION

Base.IteratorSize(::Type{<:ARFFReader}) = Base.SizeUnknown()

Base.eltype(::Type{<:ARFFReader}) = ARFFRow

function Base.iterate(r::ARFFReader, ::Nothing=nothing)
    x = nextrow(r)
    x === nothing ? nothing : (x, nothing)
end

### TABLES.JL INTEGRATION

Tables.istable(::Type{<:ARFFReader}) = true
Tables.rowaccess(::Type{<:ARFFReader}) = true
Tables.rows(r::ARFFReader) = r
Tables.columnaccess(::Type{<:ARFFReader}) = true
Tables.columns(r::ARFFReader) = readcolumns(r)
Tables.schema(r::ARFFReader) = _schema(r.colnames, r.coltypes)
Tables.partitions(r::ARFFReader) = ARFFChunks(r)

end # module
