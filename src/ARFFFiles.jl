module ARFFFiles

using Dates, Tables, CategoricalArrays, FileIO

export ARFFType, ARFFNumericType, ARFFStringType, ARFFDateType, ARFFNominalType, ARFFRelation, ARFFAttribute, ARFFDataStart, ARFFHeader, ARFFReader

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

struct ARFFRelation
    name :: String
end

"""
    ARFFAttribute

Represents a single ARFF @attribute.

It has a `name` and a `type` (a [`ARFFType`](@ref)).
"""
struct ARFFAttribute
    name :: String
    type :: ARFFType
end
struct ARFFDataStart end

"""
    Parsing

Sub-module handling low-level parsing of lines of ARFF files. Main routines are [`parse_header_line`](@ref) and [`parse_data_line`](@ref).
"""
module Parsing

    using ..ARFFFiles: ARFFType, ARFFNumericType, ARFFStringType, ARFFDateType, ARFFNominalType, ARFFRelation, ARFFAttribute, ARFFDataStart

    const _SPACE = UInt8(' ')
    const _AT = UInt8('@')
    const _COMMENT = UInt8('%')
    const _DQUO = UInt8('"')
    const _SQUO = UInt8('\'')
    const _SEP = UInt8(',')
    const _MISSING = UInt8('?')
    const _ESC = UInt8('\\')
    const _NOMSTART = UInt8('{')
    const _NOMEND = UInt8('}')
    const _ILLEGAL_RAWCHAR = (_SPACE, _AT, _COMMENT, _DQUO, _SQUO, _SEP, _MISSING, _ESC, _NOMSTART, _NOMEND)
    const _A = (UInt8('a'), UInt8('A'))
    const _B = (UInt8('b'), UInt8('B'))
    const _C = (UInt8('c'), UInt8('C'))
    const _D = (UInt8('d'), UInt8('D'))
    const _E = (UInt8('e'), UInt8('E'))
    const _G = (UInt8('g'), UInt8('G'))
    const _I = (UInt8('i'), UInt8('I'))
    const _L = (UInt8('l'), UInt8('L'))
    const _M = (UInt8('m'), UInt8('M'))
    const _N = (UInt8('n'), UInt8('N'))
    const _O = (UInt8('o'), UInt8('O'))
    const _R = (UInt8('r'), UInt8('R'))
    const _S = (UInt8('s'), UInt8('S'))
    const _T = (UInt8('t'), UInt8('T'))
    const _U = (UInt8('u'), UInt8('U'))

    mutable struct State
        src :: String
        pos :: Int
        lineno :: Int
    end

    Base.length(s::State) = sizeof(s.src)
    Base.@propagate_inbounds Base.getindex(s::State, i::Int) = codeunit(s.src, i)
    @inline hasmore(s::State) = s.pos ≤ length(s)
    @inline inc!(s::State) = (s.pos += 1; nothing)
    @inline curbyte(s::State) = s[s.pos]
    @inline maybeskip!(s::State, c::UInt8) = @inbounds (hasmore(s) && curbyte(s) == c ? (inc!(s); true) : false)
    @inline maybeskip!(s::State, cs::Tuple{Vararg{UInt8}}) = @inbounds (hasmore(s) && curbyte(s) in cs ? (inc!(s); true) : false)
    @inline skip!(s::State, c::UInt8) = @inbounds (hasmore(s) && curbyte(s) == c ? (inc!(s); nothing) : _error_expecting(s, c))
    @inline skip!(s::State, cs::Tuple{Vararg{UInt8}}) = @inbounds (hasmore(s) && curbyte(s) in cs ? (inc!(s); nothing) : _error_expecting(s, cs))
    @inline skip!(s::State, c, cs...) = (skip!(s, c); skip!(s, cs...))

    @noinline _error_expecting(s::State, c::UInt8) = _error(s, "expecting $(repr(Char(c)))")
    @noinline _error_expecting(s::State, cs::Tuple{Vararg{UInt8}}) = _error(s, "expecting one of $(join(map(repr∘Char, cs), " "))")
    @noinline _error_expecting(s::State, what::String) = _error(s, "expecting $what")
    @noinline _error(s::State, msg) = error("line $(s.lineno):$(s.pos): parsing error: $msg")

    skipspace!(s::State) =
        @inbounds while maybeskip!(s, _SPACE); end

    skipcomment!(s::State) =
        @inbounds if maybeskip!(s, _COMMENT); s.pos = length(s)+1; end

    function parse_type(s::State)
        if maybeskip!(s, _N)
            skip!(s, _U, _M, _E, _R, _I, _C)
            return ARFFNumericType()
        elseif maybeskip!(s, _R)
            skip!(s, _E, _A, _L)
            return ARFFNumericType()
        elseif maybeskip!(s, _I)
            skip!(s, _N, _T, _E, _G, _E, _R)
            return ARFFNumericType()
        elseif maybeskip!(s, _S)
            skip!(s, _T, _R, _I, _N, _G)
            return ARFFStringType()
        elseif maybeskip!(s, _D)
            skip!(s, _A, _T, _E)
            skipspace!(s)
            if hasmore(s) && (curbyte(s) in (_SQUO, _DQUO) || curbyte(s) ∉ _ILLEGAL_RAWCHAR)
                fmt = parse_string(s)
                return ARFFDateType(fmt)
            else
                return ARFFDateType()
            end
        elseif maybeskip!(s, _NOMSTART)
            skipspace!(s)
            xs = String[]
            while true
                x = parse_string(s)
                push!(xs, x)
                skipspace!(s)
                if maybeskip!(s, _SEP)
                    skipspace!(s)
                elseif maybeskip!(s, _NOMEND)
                    break
                else
                    _error_expecting(s, (_SEP, _NOMEND))
                end
            end
            return ARFFNominalType(xs)
        else
            _error_expecting(s, "a type")
        end
    end

    function parse_string(s::State)
        io = IOBuffer()
        if maybeskip!(s, _DQUO)
            @inbounds while hasmore(s)
                c = curbyte(s)
                if c == _DQUO
                    inc!(s)
                    break
                elseif c == _ESC
                    error("escape sequences not supported yet")
                else
                    inc!(s)
                    write(io, c)
                end
            end
        elseif maybeskip!(s, _SQUO)
            @inbounds while hasmore(s)
                c = curbyte(s)
                if c == _SQUO
                    inc!(s)
                    break
                elseif c == _ESC
                    error("escapes sequences not supported yet")
                else
                    inc!(s)
                    write(io, c)
                end
            end
        else
            @inbounds while hasmore(s)
                c = curbyte(s)
                if c in _ILLEGAL_RAWCHAR
                    break
                else
                    write(io, c)
                    inc!(s)
                end
            end
            io.size > 0 || _error_expecting(s, "a string")
        end
        String(take!(io))
    end

    function parse_header_line(s::State)
        skipspace!(s)
        r = nothing
        if maybeskip!(s, _AT)
            if maybeskip!(s, _D)
                skip!(s, _A, _T, _A)
                r = ARFFDataStart()
            elseif maybeskip!(s, _A)
                skip!(s, _T, _T, _R, _I, _B, _U, _T, _E)
                skipspace!(s)
                name = parse_string(s)
                skipspace!(s)
                tp = parse_type(s)
                r = ARFFAttribute(name, tp)
            elseif maybeskip!(s, _R)
                skip!(s, _E, _L, _A, _T, _I, _O, _N)
                skipspace!(s)
                name = parse_string(s)
                r = ARFFRelation(name)
            else
                _error(s, "invalid attribute")
            end
            skipspace!(s)
        end
        skipcomment!(s)
        hasmore(s) && _error_expecting(s, r===nothing ? "command or end of string" : "end of string")
        return r
    end

    function parse_datum(s::State)
        if maybeskip!(s, _MISSING)
            return missing
        else
            parse_string(s)
        end
    end

    function parse_data_line(s::State)
        skipspace!(s)
        xs = Union{String, Missing}[]
        while true
            x = parse_datum(s)
            push!(xs, x)
            skipspace!(s)
            if maybeskip!(s, _SEP)
                skipspace!(s)
            else
                break
            end
        end
        skipcomment!(s)
        hasmore(s) && _error_expecting(s, "end of string")
        return xs
    end
end

"""
    parse_header(io::IO)

Parse the ARFF header from `io`, stopping after `@data` is seen.

Returns the [`ARFFHeader`](@ref) and the number of lines read.
"""
function parse_header(io::IO)
    relation = missing
    attributes = ARFFAttribute[]
    lineno = 0
    while !eof(io)
        lineno += 1
        h = Parsing.parse_header_line(Parsing.State(readline(io), 1, lineno))
        if h === nothing
            # blank line
        elseif h isa ARFFRelation
            relation === missing || error("line $lineno: @relation seen twice")
            relation = h.name
        elseif h isa ARFFAttribute
            relation === missing && error("line $lineno: @relation required before @attribute")
            push!(attributes, h)
        elseif h isa ARFFDataStart
            relation === missing && error("line $lineno: @relation required before @data")
            return ARFFHeader(relation, attributes), lineno
        else
            error()
        end
    end
    return error("reached end of file before seeing @data")
end

"""
    parse_entry(T, p, x::Union{Missing,String})

Parse entry `x` to a `T` using parsing information `p`. See [`typeandparser`](@ref).
"""
parse_entry(::Type{T}, ::Any, ::Missing) where {T} = error("missing data found (expecting $T)")
parse_entry(::Type{T}, ::Any, ::Missing) where {T>:Missing} = missing
parse_entry(::Type{T}, ::Nothing, x::String) where {T>:String} = x
parse_entry(::Type{T}, ::Nothing, x::String) where {T>:Float64} = parse(Float64, x)
parse_entry(::Type{T}, fmt::DateFormat, x::String) where {T>:DateTime} = DateTime(x, fmt)
parse_entry(::Type{T}, p::CategoricalPool{String,Int32}, x::String) where {T>:CategoricalValue{String,Int32}} = haskey(p.invindex, x) ? p[get(p, x)] : error("invalid nominal $(repr(x)), expecting one of $(join(map(repr, p.levels), " "))")

"""
    typeandparser(t::ARFFType)

A pair `(T, p)` of a Julia type and extra parsing information corresponding to the given ARFF type.

Then `parse_entry(T, p, x)` parses the ARFF entry `x` to the type `T`.
"""
typeandparser(::ARFFNumericType) = Float64, nothing
typeandparser(t::ARFFDateType) = DateTime, parse_javadateformat(t.format)
typeandparser(::ARFFStringType) = String, nothing
typeandparser(t::ARFFNominalType) = CategoricalValue{String,Int32}, CategoricalPool{String,Int32}(t.classes)

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
    ARFFReader{names, types}

An object holding an IO stream of an ARFF file, used to access its data.

Header information is in the `header` field, of type [`ARFFHeader`](@ref).

It has the following functionality:
- `nextrow(r)` returns the next row of data as a `NamedTuple{names, types}`, or `nothing` if everything has been read.
- `read(r, [n])` reads up to `n` rows as a vector.
- `read!(xs, r)` reads up to `length(xs)` rows into the given vector, returning the number of rows read.
- `close(r)` closes the underlying IO stream, unless it was created with `own=false`.
- `eof(r)` tests whether the IO stream is at the end.
- `thenclose(f, r)` calls `f(r)` and ensures `r` is closed afterwards.
- Iteration yields rows of `r`.
- It satisfies the `Tables.jl` interface, so e.g. `DataFrame(r)` does what you think.
"""
struct ARFFReader{names, types, Parsers}
    io :: IO
    own_io :: Bool
    header :: ARFFHeader
    parsers :: Parsers
    lineno :: Ref{Int}
end

Base.close(r::ARFFReader) =
    r.own_io ? close(r.io) : nothing

Base.eof(r::ARFFReader) =
    eof(r.io)

"""
    loadstreaming(io::IO, own=false; [missingcols=true])
    loadstreaming(filename::AbstractString; [missingcols=true])

An [`ARFFReader`](@ref) object for reading the given ARFF file one record at a time.

Option `missingcols` specifies which columns can contain missing data. It can be `true` (all
columns, the default), `false` (no columns), a set/vector of column names, a single column
name, or a function taking a column name and returning true or false.
"""
loadstreaming(io::IO, own::Bool=false; opts...) =
    loadstreaming(io, own, parse_header(io)...; opts...)

function loadstreaming(io::IO, own::Bool, header::ARFFHeader, lineno::Integer; missingcols=true)
    missingcols =
        missingcols === true ? c->true :
        missingcols === false ? c->false :
        missingcols isa Union{AbstractSet,AbstractVector} ? ∈(missingcols) :
        missingcols isa Symbol ? ==(missingcols) :
        missingcols
    names = Symbol[]
    types = Type[]
    parsers = []
    for a in header.attributes
        n = Symbol(a.name)
        t, p = typeandparser(a.type)
        push!(names, Symbol(a.name))
        push!(types, missingcols(n) ? Union{t, Missing} : t)
        push!(parsers, p)
    end
    ps = Tuple(parsers)
    ARFFReader{Tuple(names), Tuple{types...}, typeof(ps)}(io, own, header, ps, Ref(Int(lineno)))
end

loadstreaming(fn::AbstractString; opts...) = loadstreaming(open(fn), true; opts...)

"""
    load(f, file, ...)
    load(file, ...)

The first form is equivalent to `f(loadstreaming(file, ...))` but ensures that the file is closed afterwards.

The second form is equivalent to `load(read, file, ...)`, which loads the entire ARFF file as a vector of named tuples.
"""
load(f::Base.Callable, args...; opts...) = thenclose(f, loadstreaming(args...; opts...))

"""
    load_header(file, ...)

Equivalent to `load(r->r.header, file, ...)`, which loads just the header from the given file as a `ARFFHeader`.
"""
load_header(fn::Union{IO,AbstractString}, args...; opts...) = load(r->r.header, fn, args...; opts...)
load(fn::Union{IO,AbstractString}, args...; opts...) = load(read, fn, args...; opts...)

thenclose(f, r::ARFFReader) =
    try
        f(r)
    finally
        close(r)
    end

"""
    nextrow(r::ARFFReader{names, types}) :: Union{Nothing, NamedTuple{names, types}}

The next row of data from the given `ARFFReader`, or `nothing` if everything has been read.
"""
function nextrow(r::ARFFReader{names, types, P}) where {names, types, P<:Tuple}
    N = length(P.parameters)
    while !eof(r.io)
        r.lineno[] += 1
        line = readline(r.io)
        if isempty(line)
            @debug "Ignored empty line at $(r.lineno[])"
            continue
        end
        x = Parsing.parse_data_line(Parsing.State(line, 1, r.lineno[]))
        if length(x) == N
            row = ntuple(Val(N)) do i
                parse_entry(types.parameters[i], r.parsers[i], x[i])
            end :: types
            return NamedTuple{names, types}(row)
        elseif !(isempty(x))
            error("line $(r.lineno[]): expected $N fields, found $(length(x))")
        end
    end
    nothing
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

function Base.read(r::ARFFReader{names, types}) where {names, types}
    x = NamedTuple{names, types}[]
    while true
        z = nextrow(r)
        if z === nothing
            return x
        else
            push!(x, z)
        end
    end
end

function Base.read(r::ARFFReader{names, types}, n::Integer) where {names, types}
    x = Vector{NamedTuple{names, types}}(undef, n)
    n = read!(r, x)
    resize!(x, n)
    x
end

### ITERATION

Base.IteratorSize(::Type{<:ARFFReader}) = Base.SizeUnknown()

function Base.iterate(r::ARFFReader, st=nothing)
    x = nextrow(r)
    x === nothing ? nothing : (x, nothing)
end

### TABLES.JL INTEGRATION

Tables.istable(r::ARFFReader) = true
Tables.rowaccess(r::ARFFReader) = true
Tables.rows(r::ARFFReader) = r
Tables.schema(r::ARFFReader{names, types}) where {names, types} = Tables.Schema(names, types)

### FILEIO INTEGRATION

load(f::File{format"ARFF"}; opts...) = load(open(f), true; opts...)
load(s::Stream{format"ARFF"}, own=false; opts...) = load(s.io, own; opts...)

loadstreaming(s::Stream{format"ARFF"}; opts...) = loadstreaming(s.io, false; opts...)

end # module
