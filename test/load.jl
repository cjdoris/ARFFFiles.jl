@testitem "load_header" begin
    include("setup.jl")
    using ARFFFiles: ARFFRelationalType
    cases = [
        (
            filename = "openml_32_sample.arff",
            header = ARFFHeader(
                "pendigits",
                [
                    [ARFFAttribute("input$i", ARFFNumericType()) for i = 1:16]...,
                    ARFFAttribute("class", ARFFNominalType(map(string, 0:9))),
                ],
            ),
        ),
        (
            filename = "simple_with_missing.arff",
            header = ARFFHeader(
                "test-data",
                [
                    ARFFAttribute("nums", ARFFNumericType()),
                    ARFFAttribute("ints", ARFFNumericType()),
                    ARFFAttribute("strs", ARFFStringType()),
                    ARFFAttribute("cats", ARFFNominalType(["a", "b", "c"])),
                    ARFFAttribute("dates", ARFFDateType("yyyy-MM-dd'T'HH:mm:ss.SSS")),
                ],
            ),
        ),
        (
            filename = "relational_example.arff",
            header = ARFFHeader(
                "relational-example",
                [
                    ARFFAttribute("id", ARFFNumericType()),
                    ARFFAttribute(
                        "measurements",
                        ARFFRelationalType([
                            ARFFAttribute("temp", ARFFNumericType()),
                            ARFFAttribute("flag", ARFFStringType()),
                        ]),
                    ),
                    ARFFAttribute("label", ARFFNominalType(["yes", "no"])),
                ],
            ),
        ),
    ]
    @testset "$(case.filename)" for case in cases
        header = ARFFFiles.load_header(joinpath(datadir, case.filename))
        @test header isa ARFFHeader
        # we don't have == defined yet for ARFFHeader etc so just use string equality
        @test string(header) == string(case.header)
    end
end

@testitem "load" begin
    using CategoricalArrays
    using Dates
    using Tables
    include("setup.jl")
    cases = [
        (
            filename = "openml_32_sample.arff",
            df = (
                input1 = [47.0, 0.0, 19.0, 38.0],
                input2 = [100.0, 89.0, 100.0, 100.0],
                input3 = [27.0, 27.0, 0.0, 37.0],
                input4 = [81.0, 100.0, 61.0, 81.0],
                input5 = [57.0, 42.0, 3.0, 12.0],
                input6 = [37.0, 75.0, 23.0, 55.0],
                input7 = [26.0, 29.0, 48.0, 0.0],
                input8 = [0.0, 45.0, 0.0, 28.0],
                input9 = [0.0, 15.0, 97.0, 52.0],
                input10 = [23.0, 15.0, 27.0, 27.0],
                input11 = [56.0, 37.0, 100.0, 100.0],
                input12 = [53.0, 0.0, 66.0, 42.0],
                input13 = [100.0, 69.0, 62.0, 86.0],
                input14 = [90.0, 2.0, 97.0, 26.0],
                input15 = [40.0, 100.0, 10.0, 65.0],
                input16 = [98.0, 6.0, 81.0, 0.0],
                class = CategoricalArray(["8", "2", "0", "4"], levels = map(string, 0:9)),
            ),
        ),
        (
            filename = "simple_with_missing.arff",
            df = (
                nums = Union{Missing, Float64}[1.5, 2.5, missing],
                ints = Union{Missing, Float64}[1, missing, 3],
                strs = Union{Missing, String}["foo", "bar", missing],
                cats = CategoricalArray([missing, "c", "b"], levels = ["a", "b", "c"]),
                dates = Union{Missing, DateTime}[
                    DateTime(2020, 1, 2),
                    missing,
                    DateTime(2022, 4, 5),
                ],
            ),
        ),
        (
            filename = "simple.arff",
            df = (
                nums = [1.5, 2.5, 3.5],
                ints = [1.0, 2.0, 3.0],
                strs = ["foo", "bar", "baz"],
                cats = CategoricalArray(["a", "c", "b"], levels = ["a", "b", "c"]),
                dates = [
                    DateTime(2020, 1, 2),
                    DateTime(2021, 3, 4),
                    DateTime(2022, 4, 5),
                ],
            ),
        ),
        (
            filename = "sparse_example.arff",
            df = (
                num1 = [1.5, 0.0, 3.0],
                str1 = ["foo", "bar", ""],
                cat1 = CategoricalArray(["yes", "yes", "no"], levels = ["yes", "no"]),
            ),
        ),
    ]
    @testset "$(case.filename)" for case in cases
        df = ARFFFiles.load(NamedTuple, joinpath(datadir, case.filename))
        @test Tables.schema(df) == Tables.schema(case.df)
        @testset "column $k" for k in propertynames(df)
            @test typeof(df[k]) == typeof(case.df[k])
            @test isequal(df[k], case.df[k])
            if case.df[k] isa CategoricalArray
                @test df[k].pool.levels == case.df[k].pool.levels
            end
        end
        @test isequal(df, case.df)
    end
    relational_path = joinpath(datadir, "relational_example.arff")
    @test isequal(
        ARFFFiles.load(NamedTuple, relational_path),
        (
            id = [1.0, 2.0, 3.0],
            measurements = [
                (temp = [1.0, 2.0], flag = ["hot", "cold"]),
                (temp = [3.0], flag = ["warm"]),
                (temp = Float64[], flag = String[]),
            ],
            label = CategoricalArray(["yes", "no", missing], levels = ["yes", "no"]),
        ),
    )
end

@testitem "load missingcols strict" begin
    using CategoricalArrays
    using Tables
    arff = """
    @RELATION strict
    @ATTRIBUTE num NUMERIC
    @ATTRIBUTE str STRING
    @DATA
    % keep this line to cover comment skipping
    1,"alpha"
    ?,"beta"
    """
    table = ARFFFiles.load(NamedTuple, IOBuffer(arff); missingcols=false)
    @test table.num[1] == 1.0
    @test isnan(table.num[2])
    @test table.str == ["alpha", "beta"]

    reader = ARFFFiles.loadstreaming(IOBuffer(arff); missingcols=false)
    chunk = ARFFFiles.readcolumns(reader; chunkbytes=nothing, maxbytes=nothing)
    @test Tuple(Tables.schema(chunk).names) == (:num, :str)
    nums = Tables.getcolumn(chunk, :num)
    @test nums[1] == 1.0
    @test isnan(nums[2])
    close(reader)

    arff_rel = """
    @RELATION combos
    @ATTRIBUTE cat {yes,no}
    @ATTRIBUTE nested RELATIONAL
        @ATTRIBUTE score NUMERIC
    @END nested
    @DATA
    'yes','1.0'
    'no','2.5'
    """
    reader = ARFFFiles.loadstreaming(IOBuffer(arff_rel); missingcols=false)
    relchunk = ARFFFiles.readcolumns(reader)
    cats = Tables.getcolumn(relchunk, :cat)
    expected_cats = CategoricalArrays.CategoricalArray(["yes", "no"], levels = ["yes", "no"])
    @test cats == expected_cats
    nested = Tables.getcolumn(relchunk, :nested)
    @test length(nested) == 2
    @test all(x -> Tables.getcolumn(x, :score)[1] > 0, nested)
    close(reader)
end

@testitem "load invalid data" begin
    # strict missing string column should error
    strict_missing = """
    @RELATION strict-missing
    @ATTRIBUTE str STRING
    @DATA
    ?
    """
    @test_throws ErrorException ARFFFiles.load(NamedTuple, IOBuffer(strict_missing); missingcols=false)

    # invalid nominal choice
    invalid_nominal = """
    @RELATION cls
    @ATTRIBUTE cls {yes,no}
    @DATA
    yes
    maybe
    """
    err = try
        ARFFFiles.load(NamedTuple, IOBuffer(invalid_nominal))
        nothing
    catch ex
        ex
    end
    @test err !== nothing
    @test occursin("Invalid nominal", sprint(showerror, err))

    # sparse row with duplicate column index
    dup_sparse = """
    @RELATION dup
    @ATTRIBUTE num NUMERIC
    @ATTRIBUTE str STRING
    @DATA
    {0 1,0 2}
    """
    @test_throws ErrorException ARFFFiles.load(NamedTuple, IOBuffer(dup_sparse))

    # sparse row omitting a required string column
    strict_sparse = """
    @RELATION strict-sparse
    @ATTRIBUTE num NUMERIC
    @ATTRIBUTE when DATE "yyyy-MM-dd'T'HH:mm:ss"
    @DATA
    {0 1}
    """
    @test_throws ErrorException ARFFFiles.load(NamedTuple, IOBuffer(strict_sparse); missingcols=false)

    # sparse row missing closing brace
    unclosed_sparse = """
    @RELATION broken
    @ATTRIBUTE num NUMERIC
    @DATA
    {0 1
    """
    @test_throws ErrorException ARFFFiles.load(NamedTuple, IOBuffer(unclosed_sparse))

    # unsupported column type in header
    struct DummyType <: ARFFFiles.ARFFType end
    header = ARFFFiles.ARFFHeader("dummy", [ARFFFiles.ARFFAttribute("x", DummyType())])
    @test_throws ErrorException ARFFFiles.loadstreaming(IOBuffer(""), header=header)
end

@testitem "sparse zero fill" begin
    using CategoricalArrays
    using Tables

    arff = """
    @RELATION fill
    @ATTRIBUTE num NUMERIC
    @ATTRIBUTE str STRING
    @ATTRIBUTE cat {foo,bar}
    @DATA
    {0 1.0}
    % inline comment to cover skip loop
    {0 2.0,1 'hi',2 bar}
    """

    logbuf = IOBuffer()
    logger = Base.CoreLogging.SimpleLogger(logbuf, Base.CoreLogging.Warn)
    table = Base.CoreLogging.with_logger(logger) do
        ARFFFiles.load(NamedTuple, IOBuffer(arff))
    end
    @test occursin("Value of string column 'str'", String(take!(logbuf)))
    @test table.num == [1.0, 2.0]
    @test table.str == ["", "hi"]
    cats = table.cat
    @test cats isa CategoricalArray{String,1,UInt32}
    @test cats[1] == "foo"
    @test cats[2] == "bar"
    @test Tables.schema(table).names == (:num, :str, :cat)
end

@testitem "sparse non-missable date errors" begin
    arff = """
    @RELATION missing-date
    @ATTRIBUTE num NUMERIC
    @ATTRIBUTE when DATE "yyyy-MM-dd"
    @DATA
    {0 1.0}
    """
    err = try
        ARFFFiles.load(NamedTuple, IOBuffer(arff))
        nothing
    catch ex
        ex
    end
    @test err !== nothing
    message = sprint(showerror, err)
    @test occursin("Value of non-numeric column 'when'", message)
end

@testitem "readcolumns guard rails" begin
    using ARFFFiles
    using Dates

    reader = ARFFFiles.loadstreaming(IOBuffer("""
    @RELATION bogus
    @ATTRIBUTE good NUMERIC
    @DATA
    1
    """))
    reader.colkinds[1] = :Z
    @test_throws ErrorException ARFFFiles.readcolumns(reader)

    sparse = """
    @RELATION tiny
    @ATTRIBUTE value NUMERIC
    @DATA
    {0 1.0}
    """
    legit = ARFFFiles.loadstreaming(IOBuffer(sparse))
    chunk = collect(codeunits("1"))
    opts = ARFFFiles.Parsing.options('\'')
    @test_throws ErrorException ARFFFiles._readcolumns_readdatum(
        legit,
        Val(:Z),
        chunk,
        1,
        length(chunk),
        0,
        1,
        false,
        1,
        1,
        opts,
        opts,
        Float64[],
        nothing,
    )
end
