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
    Parsing

Sub-module handling low-level parsing of lines of ARFF files. Main routines are [`parse_header_line`](@ref) and [`parse_data_line`](@ref).
"""
module Parsing

    using ..ARFFFiles: ARFFType, ARFFNumericType, ARFFStringType, ARFFDateType, ARFFNominalType, ARFFHeaderItem, ARFFRelation, ARFFAttribute, ARFFDataStart

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
    @inline dec!(s::State) = (s.pos -= 1; nothing)
    @inline prevbyte(s::State) = s[s.pos-1]
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

    function parse_type(s::State) :: ARFFType
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
            if !maybeskip!(s, _NOMEND)
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
            end
            return ARFFNominalType(xs)
        else
            _error_expecting(s, "a type")
        end
    end

    function parse_esc(s::State)
        if hasmore(s)
            c = @inbounds curbyte(s)
            inc!(s)
            c in (_DQUO, _SQUO, _ESC) ? c :
            c == UInt8('0') ? 0x00 :
            c == UInt8('a') ? UInt8('\a') :
            c == UInt8('b') ? UInt8('\b') :
            c == UInt8('e') ? UInt8('\e') :
            c == UInt8('f') ? UInt8('\f') :
            c == UInt8('n') ? UInt8('\n') :
            c == UInt8('r') ? UInt8('\r') :
            c == UInt8('t') ? UInt8('\t') :
            c == UInt8('v') ? UInt8('\v') :
            (dec!(s); _error_expecting(s, "escape character"))
        else
            _error_expecting(s, "escape character")
        end
    end

    function parse_string(s::State) :: String
        io = IOBuffer()
        if maybeskip!(s, (_DQUO, _SQUO))
            q = @inbounds prevbyte(s)
            @inbounds while true
                if hasmore(s)
                    c = curbyte(s)
                    inc!(s)
                    if c == q
                        break
                    elseif c == _ESC
                        write(io, parse_esc(s))
                    elseif c < 0x80
                        write(io, c)
                    else
                        dec!(s)
                        _error_expecting(s, "ASCII character")
                    end
                else
                    _error_expecting(s, "ASCII character")
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

    function parse_header_line(s::State) :: Union{Nothing, ARFFHeaderItem}
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
        hasmore(s) && _error_expecting(s, r===nothing ? "command or end of line" : "end of line")
        return r
    end

    function parse_datum(s::State) :: Union{Missing, String}
        if maybeskip!(s, _MISSING)
            return missing
        else
            parse_string(s)
        end
    end

    function parse_data_line(s::State) :: Vector{Union{String, Missing}}
        xs = Union{String, Missing}[]
        skipspace!(s)
        skipcomment!(s)
        if hasmore(s)
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
            hasmore(s) && _error_expecting(s, "end of line")
        end
        return xs
    end
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
    parse_header(io::IO)

Parse the ARFF header from `io`, stopping after `@data` is seen.

Returns the [`ARFFHeader`](@ref) and the number of lines read.
"""
function parse_header(io::IO) :: Tuple{ARFFHeader, Int}
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

parse_entry(::Type{T}, ::Any, ::Missing) where {T} = error("missing data found (expecting $T)")
parse_entry(::Type{T}, ::Any, ::Missing) where {T>:Missing} = missing
parse_entry(::Type{Float64}, ::Any, ::Missing) = NaN
parse_entry(::Type{T}, ::Nothing, x::String) where {T>:String} = x
parse_entry(::Type{T}, ::Nothing, x::String) where {T>:Float64} = parse(Float64, x)
parse_entry(::Type{T}, fmt::DateFormat, x::String) where {T>:DateTime} = DateTime(x, fmt)
parse_entry(::Type{T}, p::CategoricalPool{String,UInt32}, x::String) where {T>:CategoricalValue{String,UInt32}} = haskey(p.invindex, x) ? p[get(p, x)] : error("invalid nominal $(repr(x)), expecting one of $(join(map(repr, p.levels), " "))")

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
struct ARFFReader{names, types, Parsers, IOType}
    io :: IOType
    own_io :: Bool
    header :: ARFFHeader
    parsers :: Parsers
    lineno :: Base.RefValue{Int}
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

function loadstreaming(io::IO, own::Bool, header::ARFFHeader, lineno::Integer; missingcols=true, missingnan::Bool=false, categorical::Bool=true)
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
        if a.type isa ARFFNumericType
            t = Float64
            p = nothing
        elseif a.type isa ARFFStringType
            t = String
            p = nothing
        elseif a.type isa ARFFNominalType
            if categorical
                t = CategoricalValue{String,UInt32}
                p = CategoricalPool{String,UInt32}(a.type.classes)
            else
                t = String
                p = nothing
            end
        elseif a.type isa ARFFDateType
            t = DateTime
            p = parse_javadateformat(a.type.format)
        else
            error("not implemented")
        end
        if missingcols(n) && !(missingnan && a.type isa ARFFNumericType)
            t = Union{t, Missing}
        end
        push!(names, n)
        push!(types, t)
        push!(parsers, p)
    end
    ps = Tuple(parsers)
    ARFFReader{Tuple(names), Tuple{types...}, typeof(ps), typeof(io)}(io, own, header, ps, Ref(Int(lineno)))
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

@generated function _parse_entries(r::ARFFReader{names, types}, row) where {names, types}
    Ts = types.parameters
    N = length(Ts)
    xs = [Symbol("x", i) for i in 1:N]
    quote
        @assert length(row) == $N
        $([:(@inbounds $(xs[i]) = parse_entry($(Ts[i]), r.parsers[$i], row[$i])) for i in 1:N]...)
        NamedTuple{$names, $types}(($(xs...),))
    end
end

"""
    nextrow(r::ARFFReader{names, types}) :: Union{Nothing, NamedTuple{names, types}}

The next row of data from the given `ARFFReader`, or `nothing` if everything has been read.
"""
function nextrow(r::ARFFReader{names, types}) where {names, types}
    N = length(r.parsers)
    while !eof(r.io)
        r.lineno[] += 1
        x = Parsing.parse_data_line(Parsing.State(readline(r.io), 1, r.lineno[]))
        if length(x) == N
            return _parse_entries(r, x)
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

### SAVING

function write_datum(io::IO, x::Union{String,SubString{String}})
    write(io, UInt8('"'))
    for c in codeunits(x)
        c == 0x00 ? write(io, "\\0") :
        c == UInt8('"') ? write(io, "\\\"") :
        c == UInt8('\\') ? write(io, "\\\\") :
        c == UInt8('\a') ? write(io, "\\a") :
        c == UInt8('\b') ? write(io, "\\b") :
        c == UInt8('\e') ? write(io, "\\e") :
        c == UInt8('\f') ? write(io, "\\f") :
        c == UInt8('\n') ? write(io, "\\n") :
        c == UInt8('\r') ? write(io, "\\r") :
        c == UInt8('\t') ? write(io, "\\t") :
        c == UInt8('\v') ? write(io, "\\v") :
        c < 0x80 ? write(io, c) :
        error("string is not ASCII: $(repr(String(x)))")
    end
    write(io, UInt8('"'))
end
write_datum(io::IO, x::AbstractString) = write_datum(io, convert(String, x))
write_datum(io::IO, x::Union{Int8,UInt8,Int16,UInt16,Int32,UInt32,Int64,UInt64,Int128,UInt128,BigInt,Float16,Float32,Float64,BigFloat}) = print(io, x)
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
            println(io, " {}")
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

function Base.iterate(r::ARFFReader, ::Nothing=nothing)
    x = nextrow(r)
    x === nothing ? nothing : (x, nothing)
end

### TABLES.JL INTEGRATION

Tables.istable(r::ARFFReader) = true
Tables.rowaccess(r::ARFFReader) = true
Tables.rows(r::ARFFReader) = r
Tables.schema(::ARFFReader{names, types}) where {names, types} = Tables.Schema(names, types)

### FILEIO INTEGRATION

load(f::File{format"ARFF"}; opts...) = load(open(f), true; opts...)
load(s::Stream{format"ARFF"}; opts...) = load(s.io, false; opts...)

loadstreaming(s::Stream{format"ARFF"}; opts...) = loadstreaming(s.io, false; opts...)

save(f::File{format"ARFF"}, df; opts...) = open(io->save(io, df; opts...), f, "w")

end # module
