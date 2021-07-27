module ARFFFiles

using Base: Char, String, NamedTuple
using Dates, Tables, CategoricalArrays, FileIO

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

Sub-module handling low-level parsing of lines of ARFF files. Main routines are [`parse_header_line`](@ref) and [`parse_data_line`](@ref).
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

    """
        parse_data(s::State, ncols::Int, io::IO, idxs::Vector{Int}, maxbytes::Int=1)

    Parse data items, expecting `ncols` items per row.

    For each item encountered, print "!" followed by the string content of the item to `io`
    and append the position to `idxs`. If the item is missing, instead print "?" to `io`.
    """
    function parse_data(s::State, ncols::Int, io::IO, idxs::Vector{Int}, maxbytes::Int=1_000_000) :: Nothing
        idx0 = position(io)
        while position(io) - idx0 < maxbytes
            skipspace!(s)
            s.eof && break
            if s.char ∉ ('\n', '\r')
                for n in 1:ncols
                    if maybeskip!(s, '?')
                        print(io, '?')
                    else
                        print(io, '!')
                        parse_string(s, io)
                    end
                    skipspace!(s)
                    push!(idxs, position(io))
                    if n < ncols
                        skip!(s, ',')
                        skipspace!(s)
                    end
                end
            end
            s.eof || skipnewline!(s)
        end
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
struct ARFFReader{IO}
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
    # data
    databuf :: IOBuffer
    datastr :: Base.RefValue{String}
    dataidxs :: Vector{Int}
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
function loadstreaming(io::IO, own::Bool=false; missingcols=true, missingnan::Bool=false, categorical::Bool=true)
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
                t = CategoricalValue{String,UInt32}
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
    ARFFReader{typeof(io)}(state, own, header, colnames, collookup, colkinds, colidxs, colmissidxs, coltypes, pools, dateformats, IOBuffer(), Ref(""), Int[])
end

loadstreaming(fn::AbstractString; opts...) = loadstreaming(open(fn), true; opts...)

"""
    load(f, file, ...)
    load(file, ...)

The first form is equivalent to `f(loadstreaming(file, ...))` but ensures that the file is closed afterwards.

The second form is equivalent to `load(read, file, ...)`, which loads the entire ARFF file as a vector of named tuples.
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
load(fn::Union{IO,AbstractString}, args...; opts...) = load(read, fn, args...; opts...)

"""
    ARFFRow

A row of an ARFF data set.
"""
struct ARFFRow <: Tables.AbstractRow
    # metadata
    colnames :: Vector{Symbol}
    collookup :: Dict{Symbol,Int}
    colkinds :: Vector{Symbol}
    colidxs :: Vector{Int}
    colmissidxs :: Vector{Int}
    # data
    missings :: BitVector
    numbers :: Vector{Float64}
    strings :: Vector{String}
    dates :: Vector{DateTime}
    categoricals :: Vector{CategoricalValue{String,UInt32}}
end

Base.@propagate_inbounds function Tables.getcolumn(row::ARFFRow, i::Int)
    # check if the entry is missing
    missidx = getfield(row, :colmissidxs)[i]
    if missidx > 0
        if getfield(row, :missings)[missidx]
            return missing
        end
    end
    # otherwise get the value
    kind = getfield(row, :colkinds)[i]
    idx = getfield(row, :colidxs)[i]
    if kind == :N
        return getfield(row, :numbers)[idx]
    elseif kind == :S
        return getfield(row, :strings)[idx]
    elseif kind == :D
        return getfield(row, :dates)[idx]
    elseif kind == :C
        return getfield(row, :categoricals)[idx]
    else
        @assert false
    end
end

Base.@propagate_inbounds function Tables.getcolumn(row::ARFFRow, ::Type{T}, i::Int, name::Symbol) where {T}
    # check if the entry is missing
    if Missing <: T
        missidx = getfield(row, :colmissidxs)[i]
        if missidx > 0
            if getfield(row, :missings)[missidx]
                return missing
            end
        end
    end
    # otherwise get the value
    kind = getfield(row, :colkinds)[i]
    idx = getfield(row, :colidxs)[i]
    if Float64 <: T && kind == :N
        return getfield(row, :numbers)[idx]
    elseif String <: T && kind == :S
        return getfield(row, :strings)[idx]
    elseif DateTime <: T && kind == :D
        return getfield(row, :dates)[idx]
    elseif CategoricalValue{String,UInt32} <: T && kind == :C
        return getfield(row, :categoricals)[idx]
    else
        @assert false
    end
end

Base.@propagate_inbounds Tables.getcolumn(row::ARFFRow, name::Symbol) = Tables.getcolumn(row, getfield(row, :collookup)[name])

Tables.columnnames(row::ARFFRow) = getfield(row, :colnames)

"""
    nextrow(r::ARFFReader{names, types}) :: Union{Nothing, NamedTuple{names, types}}

The next row of data from the given `ARFFReader`, or `nothing` if everything has been read.
"""
function nextrow(r::ARFFReader) :: Union{Nothing, ARFFRow}
    # ensure the data buffer has data
    if length(r.dataidxs) ≤ 1
        empty!(r.dataidxs)
        push!(r.dataidxs, position(r.databuf))
        Parsing.parse_data(r.state, length(r.colnames), r.databuf, r.dataidxs)
        if length(r.dataidxs) ≤ 1
            return nothing
        end
        r.datastr[] = String(take!(r.databuf))
    end
    # now parse a row of data
    str = r.datastr[]
    idxs = r.dataidxs
    missings = BitVector()
    numbers = Float64[]
    strings = String[]
    dates = DateTime[]
    categoricals = CategoricalValue{String,UInt32}[]
    for (name, kind, missidx) in zip(r.colnames, r.colkinds, r.colmissidxs)
        i0 = @inbounds popfirst!(idxs)
        c = @inbounds str[i0+1]
        if c == '?'
            if missidx > 0
                push!(missings, true)
            elseif kind != :N
                error("found missing data in column $name")
            end
            if kind == :N
                push!(numbers, NaN)
            elseif kind == :S
                push!(strings, "")
            elseif kind == :D
                resize!(dates, length(dates)+1)
            elseif kind == :C
                resize!(categoricals, length(categoricals)+1)
            else
                @assert false
            end
        elseif c == '!'
            if missidx > 0
                push!(missings, false)
            end
            i1 = @inbounds idxs[1]
            substr = SubString(str, i0+2:prevind(str, i1+1))
            if kind == :N
                push!(numbers, parse(Float64, substr))
            elseif kind == :S
                push!(strings, substr)
            elseif kind == :D
                format = r.dateformats[length(dates)+1]
                push!(dates, DateTime(substr, format))
            elseif kind == :C
                pool = r.pools[length(categoricals)+1]
                if haskey(pool.invindex, substr)
                    push!(categoricals, pool[get(pool, substr)])
                else
                    error("invalid nominal $(repr(substr)) in column $name, expecting one of $(join(map(repr, pool.levels), ", ", " or "))")
                end
            else
                @assert false
            end
        else
            @assert false
        end
    end
    return ARFFRow(r.colnames, r.collookup, r.colkinds, r.colidxs, r.colmissidxs, missings, numbers, strings, dates, categoricals)
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

Tables.istable(r::ARFFReader) = true
Tables.rowaccess(r::ARFFReader) = true
Tables.rows(r::ARFFReader) = r
Tables.schema(r::ARFFReader) = Tables.Schema(r.colnames, r.coltypes)

### FILEIO INTEGRATION

load(f::File{format"ARFF"}; opts...) = load(open(f), true; opts...)
load(s::Stream{format"ARFF"}; opts...) = load(s.io, false; opts...)

loadstreaming(s::Stream{format"ARFF"}; opts...) = loadstreaming(s.io, false; opts...)

save(f::File{format"ARFF"}, df; opts...) = open(io->save(io, df; opts...), f, "w")

end # module
