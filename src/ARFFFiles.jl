module ARFFFiles

using Dates, Tables, CategoricalArrays, FileIO

# TODO: Move the parser into this package
import FastParsers
import FastParsers.Examples.ARFF: ARFFType, NumericType, StringType, DateType, NominalType, HeaderEntry, DataStart, Attribute, Relation

function parse_header(io::IO)
    comments = String[]
    relation = missing
    attributes = Attribute[]
    lineno = 1
    while !eof(io)
        r, i = FastParsers.parse(FastParsers.Examples.ARFF.header_line, readline(io), 1, onfail=(s,i)->nothing)
        r===nothing && return (nothing, lineno, i, Meta.ParseError("invalid syntax"))
        h, c = r
        c === nothing || push!(comments, c)
        if h === nothing
            # blank line
        elseif h isa Relation
            relation === missing || return (nothing, lineno, i, Meta.ParseError("relation seen twice"))
            relation = h.name
            empty!(comments)
        elseif h isa Attribute
            relation === missing && return (nothing, lineno, i, Meta.ParseError("relation required before @attribute"))
            push!(attributes, h)
        elseif h isa DataStart
            relation === missing && return (nothing, lineno, i, Meta.ParseError("relation required before @data"))
            return (Header(relation, attributes, comments), lineno, i, nothing)
        else
            error()
        end
        lineno += 1
    end
    return (nothing, lineno, 0, Meta.ParseError("reached end of file"))
end

parse_entry(::Type{T}, ::Any, ::Missing) where {T>:Missing} = missing
parse_entry(::Type{T}, ::Nothing, x::String) where {T>:String} = x
parse_entry(::Type{T}, ::Nothing, x::String) where {T>:Float64} = parse(Float64, x)
parse_entry(::Type{T}, fmt::DateFormat, x::String) where {T>:DateTime} = DateTime(x, fmt)
parse_entry(::Type{T}, p::CategoricalPool{String,Int32}, x::String) where {T>:CategoricalValue{String,Int32}} = p[get(p, x)]

typeandparser(::NumericType) = Float64, nothing
typeandparser(t::DateType) = DateTime, DateFormat(replace(t.format, r"." => c -> c=="M" ? "m" : c=="m" ? "M" : c=="s" ? "S" : c))
typeandparser(::StringType) = String, nothing
typeandparser(t::NominalType) = CategoricalValue{String,Int32}, CategoricalPool{String,Int32}(t.classes)

struct Header
    relation :: String
    attributes :: Vector{Attribute}
    comments :: Vector{String}
end

struct ARFFReader{names, types, Parsers}
    io :: IO
    own_io :: Bool
    header :: Header
    parsers :: Parsers
end

Base.close(r::ARFFReader) =
    r.own_io ? close(r.io) : nothing

Base.eof(r::ARFFReader) =
    eof(r.io)

function loadstreaming(io::IO, own::Bool=false; opts...)
    header, lines, pos, err = parse_header(io)
    err === nothing || throw(err)
    loadstreaming(io, own, header; opts...)
end

function loadstreaming(io::IO, own::Bool, header::Header; missingcols=true)
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
    ARFFReader{Tuple(names), Tuple{types...}, typeof(ps)}(io, own, header, ps)
end

loadstreaming(fn::AbstractString; opts...) = loadstreaming(open(fn), true; opts...)

load(f::Base.Callable, args...; opts...) = thenclose(f, loadstreaming(args...; opts...))

load_header(fn::Union{IO,AbstractString}, args...; opts...) = load(r->r.header, fn, args...; opts...)
load(fn::Union{IO,AbstractString}, args...; opts...) = load(read, fn, args...; opts...)

thenclose(f, r::ARFFReader) =
    try
        f(r)
    finally
        close(r)
    end

function nextrow(r::ARFFReader{names, types, P}) where {names, types, P<:Tuple}
    N = length(P.parameters)
    while !eof(r.io)
        p, i = FastParsers.parse(FastParsers.Examples.ARFF.data_line, readline(r.io), 1, onfail=(s,i)->nothing)
        p===nothing && throw(Meta.ParseError("parsing error"))
        x, c = p
        if length(x) == N
            row = ntuple(Val(N)) do i
                parse_entry(types.parameters[i], r.parsers[i], x[i])
            end :: types
            return NamedTuple{names, types}(row)
        elseif !(isempty(x))
            error("expected $N fields, found $(length(x))")
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
