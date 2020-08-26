module ARFFFiles

using StringParserPEG, Dates, Tables, CategoricalArrays, FileIO

const GRAMMAR = Grammar("""
header_line => ( -(space) & (comment | relation | attribute | datastart | blank) ) {"header_line"}

blank => ( space & space ) {"blank"}
comment => ( r(%.*)r ) {"comment"}
relation => ( -('@relation' | '@RELATION') & -(space) & string & -(space)) {"relation"}
attribute => ( -('@attribute' | '@ATTRIBUTE') & -(space) & string & -(space) & type & -(space)) {"attribute"}
datastart => ( ('@data' | '@DATA') & space ) {"datastart"}

type => type_numeric | type_string | type_date | type_nominal

type_numeric => ( ('numeric' | 'NUMERIC' | 'integer' | 'INTEGER' | 'real' | 'REAL') & space) {"type_numeric"}
type_string => ( ('string' | 'STRING') & space ) {"type_string"}
type_date => ( -('date' | 'DATE') & -(space) & ?(string) ) {"type_date"}
type_nominal => ( -('{') & -(space) & string & -(space) & (type_nominal_end | type_nominal_more) ) {"type_nominal"}
type_nominal_end => ( '}' & space ) {"type_nominal_end"}
type_nominal_more => ( -(',') & -(space) & string & -(space) & (type_nominal_end | type_nominal_more) ) {"type_nominal_more"}

data_line => ( -(space) & (row | comment | blank)) {"data_line"}

row => (-(space) & datum & -(space) & ?(row_more)) {"row"}
row_more => ( -(',') & -(space) & datum & -(space) & ?(row_more) ) {"row_more"}
datum => missing | string
missing => (rstring & space) {"missing"}

string => ( sstring | dstring | rstring )
sstring => ( -('''') & r([^\\']*)r & -('''') ) {"sstring"}
dstring => ( -('"') & r([^\\"]*)r & -('"') ) {"dstring"}
rstring => r([^ \t\\'",{}@]+)r {"rstring"}
space => r([ \t]*)r
""")

parse_header_line(line) = parse(GRAMMAR, line, start=:header_line)

parse_data_line(line) = parse(GRAMMAR, line, start=:data_line)

transformer(n, c, ::MatchRule{:header_line})       = c[1]
transformer(n, c, ::MatchRule{:blank})             = :blank => nothing
transformer(n, c, ::MatchRule{:comment})           = :comment => n.value[2:end]
transformer(n, c, ::MatchRule{:relation})          = :relation => c[1]
transformer(n, c, ::MatchRule{:attribute})         = :attribute => (c[1], c[2])
transformer(n, c, ::MatchRule{:datastart})         = :datastart => nothing
transformer(n, c, ::MatchRule{:type_numeric})      = :numeric => nothing
transformer(n, c, ::MatchRule{:type_string})       = :string => nothing
transformer(n, c, ::MatchRule{:type_date})         = :date => (isempty(c) ? nothing : c[1].children[1])
transformer(n, c, ::MatchRule{:type_nominal})      = :nominal => pushfirst!(c[2], c[1])
transformer(n, c, ::MatchRule{:type_nominal_end})  = String[]
transformer(n, c, ::MatchRule{:type_nominal_more}) = pushfirst!(c[2], c[1])
transformer(n, c, ::MatchRule{:data_line})         = c[1]
transformer(n, c, ::MatchRule{:row})               = :row => (length(c)==1 ? Union{String,Missing}[c[1]] : pushfirst!(c[2].children[1], c[1]))
transformer(n, c, ::MatchRule{:row_more})          = length(c)==1 ? Union{String,Missing}[c[1]] : pushfirst!(c[2].children[1], c[1])
transformer(n, c, ::MatchRule{:missing})           = c[1] == "?" ? missing : c[1]
transformer(n, c, ::MatchRule{:sstring})           = c[1].value
transformer(n, c, ::MatchRule{:dstring})           = c[1].value
transformer(n, c, ::MatchRule{:rstring})           = n.value

transform_line(ast) = transform(transformer, ast)

function parse_header(io::IO)
    comments = String[]
    relation = missing
    attributes = ARFFAttribute[]
    relation_comments = String[]
    lineno = 1
    while !eof(io)
        ast, pos, err = parse_header_line(readline(io))
        err === nothing || return (nothing, lineno, pos, err)
        k, x = transform_line(ast)
        if k == :blank
            # ignore
        elseif k == :comment
            push!(comments, x)
        elseif k == :relation
            relation === missing || return (nothing, lineno, pos, Meta.ParseError("relation seen twice"))
            relation = x
            relation_comments = copy(comments)
            empty!(comments)
        elseif k == :attribute
            relation === missing && return (nothing, lineno, pos, Meta.ParseError("relation required before @attribute"))
            name, (t, y) = x
            if t == :numeric
                type = ARFFNumericType()
            elseif t == :string
                type = ARFFStringType()
            elseif t == :date
                type = ARFFDateType(y)
            elseif t == :nominal
                type = ARFFNominalType(y)
            else
                error()
            end
            push!(attributes, ARFFAttribute(name, type, copy(comments)))
            empty!(comments)
        elseif k == :datastart
            relation === missing && return (nothing, pos, Meta.ParseError("relation required before @data"))
            return (ARFFHeader(relation, attributes, relation_comments, comments), lineno, pos, nothing)
        else
            error()
        end
    end
    return (nothing, lineno, 0, Meta.ParseError("reached end of file"))
end

abstract type ARFFType end

parse_entry(::Type{T}, ::Any, ::Missing) where {T>:Missing} = missing
parse_entry(::Type{T}, ::Nothing, x::String) where {T>:String} = x
parse_entry(::Type{T}, ::Nothing, x::String) where {T>:Float64} = parse(Float64, x)
parse_entry(::Type{T}, fmt::DateFormat, x::String) where {T>:DateTime} = DateTime(x, fmt)
parse_entry(::Type{T}, p::CategoricalPool{String,Int32}, x::String) where {T>:CategoricalValue{String,Int32}} = p[get(p, x)]

struct ARFFNumericType <: ARFFType end
typeandparser(t::ARFFNumericType) = Float64, nothing

struct ARFFDateType <: ARFFType
    format :: String
end
typeandparser(t::ARFFDateType) = DateTime, DateFormat(replace(t.format, r"." => c -> c=="M" ? "m" : c=="m" ? "M" : c=="s" ? "S" : c))

struct ARFFStringType <: ARFFType end
typeandparser(t::ARFFStringType) = String, nothing

struct ARFFNominalType <: ARFFType
    values :: Vector{String}
end
typeandparser(t::ARFFNominalType) = CategoricalValue{String,Int32}, CategoricalPool{String,Int32}(t.values)

struct ARFFAttribute
    name :: String
    type :: ARFFType
    comments :: Vector{String}
end

struct ARFFHeader
    relation :: String
    attributes :: Vector{ARFFAttribute}
    relation_comments :: Vector{String}
    data_comments :: Vector{String}
end

struct ARFFReader{names, types, Parsers}
    io :: IO
    own_io :: Bool
    header :: ARFFHeader
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

function loadstreaming(io::IO, own::Bool, header::ARFFHeader; missingcols=true)
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

function thenclose(f, r::ARFFReader)
    z = try
        f(r)
    catch
        close(r)
        rethrow()
    end
    close(r)
    return z
end

function nextrow(r::ARFFReader{names, types, P}) where {names, types, P<:Tuple}
    N = length(P.parameters)
    while !eof(r.io)
        ast, pos, err = parse_data_line(readline(r.io))
        err === nothing || throw(err)
        k, x = transform_line(ast)
        k :: Symbol
        if k == :row
            x :: Vector{Union{String,Missing}}
            length(x) == N || error("expected $N fields, found $(length(x))")
            row = ntuple(Val(N)) do i
                parse_entry(types.parameters[i], r.parsers[i], x[i])
            end :: types
            return NamedTuple{names, types}(row)
        elseif k in (:blank, :comment)
            continue
        else
            error()
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
