module ARFFFiles

using Dates, Tables, CategoricalArrays, FileIO, Parsers

export ARFFType, ARFFNumericType, ARFFStringType, ARFFDateType, ARFFNominalType, ARFFRelation, ARFFAttribute, ARFFDataStart, ARFFHeader, ARFFReader, ARFFRow

"""
    ARFFType

Abstract type of ARFF types. Concrete subtypes are `ARFFNumericType`, `ARFFStringType`, `ARFFDateType` and `ARFFNominalType`.
"""
abstract type ARFFType end
struct ARFFNumericType <: ARFFType end
struct ARFFStringType <:ARFFType end
struct ARFFDateType <: ARFFType
    format :: String
end
ARFFDateType() = ARFFDateType("yyyy-MM-dd'T'HH:mm:ss")
struct ARFFNominalType <: ARFFType
    classes :: Vector{String}
end

abstract type ARFFHeaderItem end

struct ARFFRelation <: ARFFHeaderItem
    name :: String
end

"""
    ARFFAttribute

Represents a single ARFF @attribute.

It has a `name` and a `type` (a [`ARFFType`](@ref)).
"""
struct ARFFAttribute <: ARFFHeaderItem
    name :: String
    type :: ARFFType
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
    relation :: String
    attributes :: Vector{ARFFAttribute}
end

"""
    Parsing

Sub-module handling low-level parsing of lines of ARFF files.
"""
module Parsing

    using ..ARFFFiles: ARFFType, ARFFNumericType, ARFFStringType, ARFFDateType, ARFFNominalType, ARFFHeaderItem, ARFFRelation, ARFFAttribute, ARFFDataStart, ARFFHeader

    mutable struct State{S<:IO}
        io :: S
        eof :: Bool
        char :: Char
    end

    function State(io::IO)
        s = State{typeof(io)}(io, false, '\0')
        inc!(s)
        return s
    end

    function inc!(s::State)
        s.eof = eof(s.io)
        if !s.eof
            s.char = read(s.io, Char)
        end
        return
    end

    function skipspace!(s::State)
        while !s.eof
            if s.char in ('\n', '\r')
                break
            elseif isspace(s.char)
                inc!(s)
            elseif s.char == '%'
                inc!(s)
                while !s.eof
                    if s.char in ('\n', '\r')
                        break
                    else
                        inc!(s)
                    end
                end
                break
            else
                break
            end
        end
        return
    end

    maybeskip!(s::State, c::Char) = (!s.eof && s.char == c) ? (inc!(s); true) : false
    maybeskip!(s::State, cs::Tuple{Vararg{Char}}) = (!s.eof && s.char in cs) ? (inc!(s); true) : false

    skip!(s::State, arg) = maybeskip!(s, arg) ? nothing : errorexpecting(s, arg)
    skip!(s::State, args...) = foreach(a->skip!(s, a), args)

    function skipnewline!(s::State)
        if s.eof
            errorexpecting(s, "new line")
        elseif s.char == '\n'
            inc!(s)
        elseif s.char == '\r'
            inc!(s)
            s.char == '\n' && inc!(s)
        else
            errorexpecting(s, "new line")
        end
        return
    end

    error(s::State, msg...) = Base.error(sprint(print, "byte ", position(s.io), ": syntax error: ", msg...))

    errorexpecting(s::State, msg::AbstractString) = error(s, "expecting ", msg, ", got ", s.eof ? "end of file" : repr(s.char))
    errorexpecting(s::State, cs::Tuple{Vararg{Char}}) = error(s, join(map(repr, cs), ", ", " or "))
    errorexpecting(s::State, c::Char) = errorexpecting(s, repr(c))

    israwchar(c::Char) = !isspace(c) && c ∉ ('%', '"', '\'', ',', '\\', '{', '}')

    isstringstart(c::Char) = israwchar(c) || c ∈ ('"', '\'')

    function parse_type(s::State) :: ARFFType
        if maybeskip!(s, ('n', 'N'))
            skip!(s, ('u', 'U'), ('m', 'M'), ('e', 'E'), ('r', 'R'), ('i', 'I'), ('c', 'C'))
            return ARFFNumericType()
        elseif maybeskip!(s, ('r', 'R'))
            skip!(s, ('e', 'E'), ('a', 'A'), ('l', 'L'))
            return ARFFNumericType()
        elseif maybeskip!(s, ('i', 'I'))
            skip!(s, ('n', 'N'), ('t', 'T'), ('e', 'E'), ('g', 'G'), ('e', 'E'), ('r', 'R'))
            return ARFFNumericType()
        elseif maybeskip!(s, ('s', 'S'))
            skip!(s, ('t', 'T'), ('r', 'R'), ('i', 'I'), ('n', 'N'), ('g', 'G'))
            return ARFFStringType()
        elseif maybeskip!(s, ('d', 'D'))
            skip!(s, ('a', 'A'), ('t', 'T'), ('e', 'E'))
            skipspace!(s)
            if !s.eof && isstringstart(s.char)
                return ARFFDateType(parse_string(s))
            else
                return ARFFDateType()
            end
        elseif maybeskip!(s, '{')
            skipspace!(s)
            xs = String[]
            if !maybeskip!(s, '}')
                while true
                    push!(xs, parse_string(s))
                    skipspace!(s)
                    if maybeskip!(s, ',')
                        skipspace!(s)
                    elseif maybeskip!(s, '}')
                        break
                    else
                        errorexpecting(s, (',', '}'))
                    end
                end
            end
            return ARFFNominalType(xs)
        else
            errorexpecting(s, "a type")
        end
    end

    function parse_esc(s::State) :: Char
        if s.eof
            errorexpecting(s, "escape character")
        else
            c = s.char
            c in ('"', '\'', '\\', '%') ? (inc!(s); c) :
            c == '0' ? (inc!(s); '\0') :
            c == 'a' ? (inc!(s); '\a') :
            c == 'b' ? (inc!(s); '\b') :
            c == 'e' ? (inc!(s); '\e') :
            c == 'f' ? (inc!(s); '\f') :
            c == 'n' ? (inc!(s); '\n') :
            c == 'r' ? (inc!(s); '\r') :
            c == 't' ? (inc!(s); '\t') :
            c == 'v' ? (inc!(s); '\v') :
            errorexpecting(s, "escape character")
        end
    end

    function parse_string(s::State, io::IO) :: Nothing
        if !s.eof
            if s.char in ('"', '\'')
                q = s.char
                inc!(s)
                while true
                    if s.eof
                        errorexpecting(s, "character")
                    else
                        if s.char == q
                            inc!(s)
                            return
                        elseif s.char == '\\'
                            inc!(s)
                            write(io, parse_esc(s))
                        elseif s.char in ('\n', '\t')
                            errorexpecting(s, "character")
                        else
                            write(io, s.char)
                            inc!(s)
                        end
                    end
                end
            elseif israwchar(s.char)
                write(io, s.char)
                inc!(s)
                while !s.eof && israwchar(s.char)
                    write(io, s.char)
                    inc!(s)
                end
                return
            end
        end
        errorexpecting(s, "string")
    end

    function parse_string(s::State) :: String
        io = IOBuffer()
        parse_string(s, io)
        return String(take!(io))
    end

    function parse_header_line(s::State) :: Union{Nothing, ARFFHeaderItem}
        skipspace!(s)
        r = nothing
        if maybeskip!(s, '@')
            if maybeskip!(s, ('d', 'D'))
                skip!(s, ('a', 'A'), ('t', 'T'), ('a', 'A'))
                r = ARFFDataStart()
            elseif maybeskip!(s, ('a', 'A'))
                skip!(s, ('t', 'T'), ('t', 'T'), ('r', 'R'), ('i', 'I'), ('b', 'B'), ('u', 'U'), ('t', 'T'), ('e', 'E'))
                skipspace!(s)
                name = parse_string(s)
                skipspace!(s)
                tp = parse_type(s)
                r = ARFFAttribute(name, tp)
            elseif maybeskip!(s, ('r', 'R'))
                skip!(s, ('e', 'E'), ('l', 'L'), ('a', 'A'), ('t', 'T'), ('i', 'I'), ('o', 'O'), ('n', 'N'))
                skipspace!(s)
                name = parse_string(s)
                r = ARFFRelation(name)
            else
                errorexpecting(s, "\"data\", \"attribute\" or \"relation\"")
            end
            skipspace!(s)
        end
        s.eof || skipnewline!(s)
        return r
    end

    function parse_header(s::State) :: ARFFHeader
        relation = missing
        attributes = ARFFAttribute[]
        while !s.eof
            h = parse_header_line(s)
            if h === nothing
                # blank line
            elseif h isa ARFFRelation
                relation === missing || error(s, "@relation seen twice")
                relation = h.name
            elseif h isa ARFFAttribute
                relation === missing && error(s, "@relation required before @attribute")
                push!(attributes, h)
            elseif h isa ARFFDataStart
                relation === missing && error(s, "@relation required before @data")
                return ARFFHeader(relation, attributes)
            else
                error(s, "INTERNAL ERROR")
            end
        end
        error(s, "reached end of file before seeing @data")
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

const CatType = CategoricalValue{String, UInt32}

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
    state :: Parsing.State{IO}
    own_io :: Bool
    header :: ARFFHeader
    # columns
    colnames :: Vector{Symbol} # name
    collookup :: Dict{Symbol,Int} # inverse of i->colnames[i]
    colkinds :: Vector{Symbol} # :N, :S, :D, :C (numeric, string, date, categorical)
    colidxs :: Vector{Int} # this is the idxth column of its kind
    colmissidxs :: Vector{Int} # this is the idxth column which allows missing values
    coltypes :: Vector{Type} # combines kind and missing
    pools :: Vector{CategoricalPool{String,UInt32}}
    dateformats :: Vector{DateFormat}
    # row iteration
    chunk :: Tables.DictColumnTable
    chunklen :: Int
    chunkidx :: Int
    chunkbytes :: Int
end

Base.close(r::ARFFReader) = r.own_io ? close(r.state.io) : nothing

Base.eof(r::ARFFReader) = eof(r.state.io)

"""
    loadstreaming(io::IO, own=false; [missingcols=true], [missingnan=false], [categorical=true])
    loadstreaming(filename::AbstractString; ...)

An [`ARFFReader`](@ref) object for reading the given ARFF file one record at a time.

Option `missingcols` specifies which columns can contain missing data. It can be `true` (all
columns, the default), `false` (no columns), a set/vector of column names, a single column
name, or a function taking a column name and returning true or false.

Option `missingnan` specifies whether or not to convert missing values in numeric columns to
`NaN`. This is equivalent to excluding these columns in `missingcols`.

Option `categorical` specifies whether or not to convert nominal columns to `CategoricalValue`
or `String`.
"""
function loadstreaming(io::IO, own::Bool=false; missingcols=true, missingnan::Bool=false, categorical::Bool=true, chunkbytes::Integer=1<<26)
    missingcols =
        missingcols === true ? c->true :
        missingcols === false ? c->false :
        missingcols isa Union{AbstractSet,AbstractVector} ? ∈(missingcols) :
        missingcols isa Symbol ? ==(missingcols) :
        missingcols
    state = Parsing.State(io)
    header = Parsing.parse_header(state)
    numnumeric = 0
    numstring = 0
    numdate = 0
    numcategorical = 0
    nummissing = 0
    colnames = Symbol[]
    colkinds = Symbol[]
    colidxs = Int[]
    colmissidxs = Int[]
    coltypes = Type[]
    pools = CategoricalPool{String,UInt32}[]
    dateformats = DateFormat[]
    for a in header.attributes
        n = Symbol(a.name)
        if a.type isa ARFFNumericType
            k = :N
            t = Float64
            i = (numnumeric += 1)
        elseif a.type isa ARFFStringType
            k = :S
            t = String
            i = (numstring += 1)
        elseif a.type isa ARFFNominalType
            if categorical
                k = :C
                t = CatType
                i = (numcategorical += 1)
                push!(pools, CategoricalPool{String,UInt32}(a.type.classes))
            else
                k = :S
                t = String
                i = (numstring += 1)
            end
        elseif a.type isa ARFFDateType
            k = :D
            t = DateTime
            i = (numdate += 1)
            push!(dateformats, parse_javadateformat(a.type.format))
        else
            error("not implemented")
        end
        if missingcols(n) && !(missingnan && a.type isa ARFFNumericType)
            m = true
            t = Union{t, Missing}
        else
            m = false
        end
        push!(colnames, n)
        push!(colkinds, k)
        push!(coltypes, t)
        push!(colidxs, i)
        push!(colmissidxs, m ? (nummissing += 1) : 0)
    end
    collookup = Dict{Symbol,Int}(n=>i for (i,n) in enumerate(colnames))
    ARFFReader{typeof(io)}(state, own, header, colnames, collookup, colkinds, colidxs, colmissidxs, coltypes, pools, dateformats, Tables.DictColumnTable(Tables.Schema([], []), Dict()), 0, 0, chunkbytes)
end

loadstreaming(fn::AbstractString; opts...) = loadstreaming(open(fn), true; opts...)

"""
    load(f, file, ...)
    load(file, ...)

The first form is equivalent to `f(loadstreaming(file, ...))` but ensures that the file is closed afterwards.

The second form is equivalent to `load(readcolumns, file, ...)`, which loads the entire ARFF file as a table.
"""
function load(f::Base.Callable, args...; opts...)
    r = loadstreaming(args...; opts...)
    try
        return f(r)
    finally
        close(r)
    end
end

"""
    load_header(file, ...)

Equivalent to `load(r->r.header, file, ...)`, which loads just the header from the given file as a `ARFFHeader`.
"""
load_header(fn::Union{IO,AbstractString}, args...; opts...) = load(r->r.header, fn, args...; opts...)
load(fn::Union{IO,AbstractString}, args...; opts...) = load(readcolumns, fn, args...; opts...)

"""
    nextrow(r::ARFFReader{names, types}) :: Union{Nothing, NamedTuple{names, types}}

The next row of data from the given `ARFFReader`, or `nothing` if everything has been read.
"""
function nextrow(r::ARFFReader) :: Union{Nothing,Tables.ColumnsRow}
    while r.chunkidx ≥ r.chunklen
        eof(r.state.io) && return nothing
        r.chunk = readcolumns(r, maxbytes=r.chunkbytes)
        r.chunklen = Tables.rowcount(r.chunk)
        r.chunkidx = 0
    end
    r.chunkidx += 1
    return Tables.ColumnsRow(r.chunk, r.chunkidx)
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
    x = Tables.ColumnsRow[]
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
    x = Vector{Tables.ColumnsRow}(undef, n)
    n = read!(r, x)
    resize!(x, n)
    x
end

parse_opts(qc, df=nothing) = Parsers.Options(sentinel=["?"], openquotechar=qc, closequotechar=qc, escapechar='\\', delim=',', quoted=true, comment="%", ignoreemptylines=true, dateformat=df)

function parse_datum(::Type{T}, data::AbstractVector{UInt8}, pos::Integer=1, len::Integer=length(data)-(pos-1), opts1=parse_opts('''), opts2=parse_opts('"')) where {T}
    # first try parsing single-quoted
    res1 = Parsers.xparse(T, data, pos, len, opts1)
    if Parsers.invalid(res1.code)
        # invalid: try again
    elseif T == String && !Parsers.quoted(res1.code) && res1.val.len > 0 && @inbounds data[res1.val.pos] == opts2.oq
        # double quoted: try again
    else
        return res1
    end
    # now try double-quoted
    res2 = Parsers.xparse(T, data, pos, len, opts2)
    if T == String && !Parsers.invalid(res2.code) && !Parsers.quoted(res2.code) && res2.val.len > 0 && @inbounds data[res2.val.pos] == opts1.oq
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
    str = Parsers.getstring(data, Parsers.PosLen(res.val.pos, res.val.len), 0x00)
    if Parsers.escapedstring(res.code)
        str = replace(str, r"\\.?" => parse_escape)
    end
    return str
end

"""
    readcolumns(r::ARFFReader, maxbytes=nothing)

Read the data from `r` into a columnar table.

By default the entire table is read. If `maxbytes` is given, approximately this many bytes
of the input stream is read instead.
"""
function readcolumns(
    r::ARFFReader;
    opts_sq=parse_opts('''),
    opts_dq=parse_opts('"'),
    date_opts_sq=[parse_opts(''', df) for df in r.dateformats],
    date_opts_dq=[parse_opts('"', df) for df in r.dateformats],
    maxbytes=nothing,
    chunkbytes=1<<20,
)
    # initialize columns
    colnames = r.colnames
    colkinds = r.colkinds
    colmissidxs = r.colmissidxs
    pools = r.pools
    ncols = length(colnames)
    Ncols = Vector{Float64}[]
    NXcols = Vector{Union{Missing,Float64}}[]
    Scols = Vector{String}[]
    SXcols = Vector{Union{Missing,String}}[]
    Dcols = Vector{DateTime}[]
    DXcols = Vector{Union{Missing,DateTime}}[]
    Ccols = Vector{CatType}[]
    CXcols = Vector{Union{Missing,CatType}}[]
    cols = Vector[]
    for i in 1:ncols
        kind = colkinds[i]
        missidx = colmissidxs[i]
        if kind == :N
            if missidx == 0
                col = Float64[]
                push!(Ncols, col)
                push!(cols, col)
            else
                col = Union{Missing,Float64}[]
                push!(NXcols, col)
                push!(cols, col)
            end
        elseif kind == :S
            if missidx == 0
                col = String[]
                push!(Scols, col)
                push!(cols, col)
            else
                col = Union{Missing,String}[]
                push!(SXcols, col)
                push!(cols, col)
            end
        elseif kind == :D
            if missidx == 0
                col = DateTime[]
                push!(Dcols, col)
                push!(cols, col)
            else
                col = Union{Missing,DateTime}[]
                push!(DXcols, col)
                push!(cols, col)
            end
        elseif kind == :C
            if missidx == 0
                col = CatType[]
                push!(Ccols, col)
                push!(cols, col)
            else
                col = Union{Missing,CatType}[]
                push!(CXcols, col)
                push!(cols, col)
            end
        else
            error("Not implemented: Columns of kind $kind")
        end
    end
    # reading loop
    nrows = 0
    nbytes = 0
    io = r.state.io
    function checkcode(T, code, pos, tlen, i, n)
        if Parsers.invalid(code)
            error("Could not parse $T at byte $pos")
        elseif i < n ? !Parsers.delimited(code) : !(Parsers.newline(code) || Parsers.eof(code))
            error("Expecting $(i<n ? "delimiter" : "new line") at byte $(pos+tlen)")
        end
    end
    while !eof(io) && (maxbytes === nothing || nbytes < maxbytes)
        # remeber where we started
        offset = position(io)
        # read a chunk
        if chunkbytes === nothing && maxbytes === nothing
            chunk = read(io)
        else
            chunk = read(io, max(0, min(chunkbytes===nothing ? typemax(Int) : chunkbytes, maxbytes===nothing ? typemax(Int) : maxbytes - nbytes)))
        end
        # ensure we get a whole line
        append!(chunk, codeunits(readline(io)))
        # read each line
        pos = 1
        len = length(chunk)
        nbytes += len
        @inbounds while pos ≤ len
            # skip any initial whitespace
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
            # sparse format
            if c == 0x7B
                error("sparse ARFF format not supported at byte $(pos+offset)")
                continue
            end
            # dense format
            iN = iNX = iS = iSX = iD = iDX = iC = iCX = 0
            for i in 1:ncols
                name = colnames[i]
                kind = colkinds[i]
                missidx = colmissidxs[i]
                if kind == :N
                    resN = parse_datum(Float64, chunk, pos, len, opts_sq, opts_dq)
                    # @info "number" resN
                    checkcode(Float64, resN.code, pos+offset, resN.tlen, i, ncols)
                    pos += resN.tlen
                    if missidx == 0
                        iN += 1
                        col = Ncols[iN]
                        if Parsers.sentinel(resN.code)
                            push!(col, NaN)
                        else
                            push!(col, resN.val)
                        end
                    else
                        iNX += 1
                        col = NXcols[iNX]
                        if Parsers.sentinel(resN.code)
                            push!(col, missing)
                        else
                            push!(col, resN.val)
                        end
                    end
                elseif kind == :S
                    resS = parse_datum(String, chunk, pos, len, opts_sq, opts_dq)
                    # @info "string" resS resS.val.pos resS.val.len Parsers.getstring(chunk, resS.val, 0x00)
                    checkcode(String, resS.code, pos+offset, resS.tlen, i, ncols)
                    pos += resS.tlen
                    if missidx == 0
                        iS += 1
                        col = Scols[iS]
                        if Parsers.sentinel(resS.code)
                            error("Missing value in column $name")
                        else
                            push!(col, get_parsed_string(chunk, resS))
                        end
                    else
                        iSX += 1
                        col = SXcols[iSX]
                        if Parsers.sentinel(resS.code)
                            push!(col, missing)
                        else
                            push!(col, get_parsed_string(chunk, resS))
                        end
                    end
                elseif kind == :D
                    resD = parse_datum(DateTime, chunk, pos, len, date_opts_sq[iD+iDX+1], date_opts_dq[iD+iDX+1])
                    # @info "date" resD
                    checkcode(DateTime, resD.code, pos+offset, resD.tlen, i, ncols)
                    pos += resD.tlen
                    if missidx == 0
                        iD += 1
                        col = Dcols[iD]
                        if Parsers.sentinel(resD.code)
                            error("Missing value in column $name")
                        else
                            push!(col, resD.val)
                        end
                    else
                        iDX += 1
                        col = DXcols[iDX]
                        if Parsers.sentinel(resD.code)
                            push!(col, missing)
                        else
                            push!(col, resD.val)
                        end
                    end
                elseif kind == :C
                    resS = parse_datum(String, chunk, pos, len, opts_sq, opts_dq)
                    # @info "categorical" resS
                    checkcode(String, resS.code, pos+offset, resS.tlen, i, ncols)
                    pos += resS.tlen
                    if missidx == 0
                        iC += 1
                        col = Ccols[iC]
                        if Parsers.sentinel(resS.code)
                            error("Missing value in column $name")
                        else
                            pool = pools[iC+iCX]
                            str = get_parsed_string(chunk, resS)
                            if haskey(pool.invindex, str)
                                push!(col, pool[get(pool, str)])
                            else
                                error("invalid nominal $(repr(str)) in column $name, expecting one of $(join(map(repr, pool.levels), ", ", " or "))")
                            end
                        end
                    else
                        iCX += 1
                        col = CXcols[iCX]
                        if Parsers.sentinel(resS.code)
                            push!(col, missing)
                        else
                            pool = pools[iC+iCX]
                            str = get_parsed_string(chunk, resS)
                            if haskey(pool.invindex, str)
                                push!(col, pool[get(pool, str)])
                            else
                                error("invalid nominal $(repr(str)) in column $name, expecting one of $(join(map(repr, pool.levels), ", ", " or "))")
                            end
                        end
                    end
                else
                    error("Not implemented: kind=$kind")
                end
            end
        end
    end
    # construct the output table
    schema = Tables.Schema(r.colnames, r.coltypes)
    dict = Dict(zip(colnames, cols))
    return Tables.DictColumnTable(schema, dict)
end

### SAVING

function write_datum(io::IO, x::AbstractString)
    if eltype(x) != Char
        x = convert(String, x)
    end
    write(io, '"')
    for c in x
        c == '\0' ? write(io, "\\0") :
        c == '"'  ? write(io, "\\\"") :
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
    write(io, '"')
end
write_datum(io::IO, x::Union{Int8,UInt8,Int16,UInt16,Int32,UInt32,Int64,UInt64,Int128,UInt128,BigInt,Float16,Float32,Float64,BigFloat}) = print(io, x)
write_datum(io::IO, x::Bool) = print(io, x ? "1" : "0")
write_datum(io::IO, x::Integer) = write_datum(io, convert(BigInt, x))
write_datum(io::IO, x::Real) = write_datum(io, convert(BigFloat, x))
write_datum(io::IO, x::DateTime) = write_datum(io, Dates.format(x, dateformat"YYYY-mm-dd\THH:MM:SS.sss"))
write_datum(io::IO, x::Date) = write_datum(io, DateTime(x))
write_datum(io::IO, x::CategoricalValue{<:AbstractString}) = write_datum(io, x.pool.levels[x.level])
write_datum(io::IO, ::Missing) = write(io, "?")

function find_levels(rows, ::Val{name}) where {name}
    for row in rows
        x = Tables.getcolumn(row, name)
        x === missing && continue
        return x.pool.levels
    end
end

@generated function write_data(io::IO, rows, ::Val{N}) where {N}
    exs = []
    for i in 1:N
        push!(exs, :(write_datum(io, row[$i])))
        push!(exs, :(print(io, $(i==N ? "\n" : ","))))
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
        elseif type <: Union{Real, Missing}
            println(io, "NUMERIC")
        elseif type <: Union{AbstractString, Missing}
            println(io, "STRING")
        elseif type <: Union{Date, DateTime, Missing}
            println(io, "DATE \"yyyy-MM-dd'T'HH:mm:ss.SSS\"")
        elseif type <: Union{<:CategoricalValue{<:AbstractString},Missing}
            # find the levels of the first non-missing entry
            levels = find_levels(Tables.rows(df), Val(name))
            if levels === nothing || isempty(levels)
                println(io, "{}")
            else
                for (i, level) in enumerate(levels)
                    print(io, i==1 ? "{" : ",")
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

save(filename::AbstractString, df; opts...) = open(io->save(io, df; opts...), filename, "w")

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
Tables.schema(r::ARFFReader) = Tables.Schema(r.colnames, r.coltypes)

### FILEIO INTEGRATION

load(f::File{format"ARFF"}; opts...) = load(open(f), true; opts...)
load(s::Stream{format"ARFF"}; opts...) = load(s.io, false; opts...)

loadstreaming(s::Stream{format"ARFF"}; opts...) = loadstreaming(s.io, false; opts...)

save(f::File{format"ARFF"}, df; opts...) = open(io->save(io, df; opts...), f, "w")

end # module
