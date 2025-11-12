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
