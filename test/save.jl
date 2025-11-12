@testitem "save" begin
    using CategoricalArrays
    using Dates
    include("setup.jl")
    cases = [
        (
            filename = "simple_with_missing.arff",
            df = (
                nums = [1.5, 2.5, NaN],
                ints = Union{Missing, Int}[1, missing, 3],
                strs = Union{Missing, String}["foo", "bar", missing],
                cats = CategoricalArray([missing, "c", "b"], levels = ["a", "b", "c"]),
                dates = Union{Missing, Date}[Date(2020, 1, 2), missing, Date(2022, 4, 5)],
            ),
        ),
        (
            filename = "simple.arff",
            df = (
                nums = [1.5, 2.5, 3.5],
                ints = [1, 2, 3],
                strs = ["foo", "bar", "baz"],
                cats = CategoricalArray(["a", "c", "b"], levels = ["a", "b", "c"]),
                dates = [Date(2020, 1, 2), Date(2021, 3, 4), Date(2022, 4, 5)],
            ),
        ),
    ]
    @testset "$(case.filename)" for case in cases
        path = joinpath(datadir, case.filename)
        if !ispath(path)
            ARFFFiles.save(
                path,
                case.df,
                relation = "test-data",
                comment = "test data file",
            )
        end
        io = IOBuffer()
        ARFFFiles.save(io, case.df, relation = "test-data", comment = "test data file")
        @test String(take!(io)) == read(path, String)
    end
    @testset "boolean columns" begin
        df = (flags = [true, false],)
        io = IOBuffer()
        ARFFFiles.save(io, df, relation = "bools", comment = "")
        expected = "@RELATION 'bools'\n\n@ATTRIBUTE 'flags' NUMERIC\n\n@DATA\n1\n0\n"
        @test String(take!(io)) == expected
    end
end

@testitem "save edge cases" begin
    using CategoricalArrays

    io = IOBuffer()
    ARFFFiles.write_datum(io, SubString("quoted", 1, 6))
    @test String(take!(io)) == "'quoted'"

    struct UInt8String <: AbstractString
        data::Vector{UInt8}
    end
    Base.eltype(::Type{UInt8String}) = UInt8
    Base.convert(::Type{String}, s::UInt8String) = String(s.data)
    Base.String(s::UInt8String) = convert(String, s)
    custom = UInt8String(codeunits("custom"))
    io = IOBuffer()
    ARFFFiles.write_datum(io, custom)
    @test String(take!(io)) == "'custom'"

    missing_only = (allmissing = [missing, missing],)
    io = IOBuffer()
    ARFFFiles.save(io, missing_only, relation = "missing", comment = "")
    text = String(take!(io))
    @test occursin("@ATTRIBUTE 'allmissing' {}", text)

    cat_missing = (cats = CategoricalArray([missing, missing], levels = String[]),)
    io = IOBuffer()
    ARFFFiles.save(io, cat_missing, relation = "cats", comment = "")
    text = String(take!(io))
    @test occursin("@ATTRIBUTE 'cats' {}", text)

    unsupported = (bad = ComplexF64[1 + 1im],)
    @test_throws ErrorException ARFFFiles.save(IOBuffer(), unsupported, relation = "bad", comment = "")
end
