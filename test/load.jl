@testitem "load_header" begin
    cases = [(
        filename = "openml_32_sample.arff",
        header = ARFFHeader(
            "pendigits",
            [
                [ARFFAttribute("input$i", ARFFNumericType()) for i = 1:16]...,
                ARFFAttribute("class", ARFFNominalType(map(string, 0:9))),
            ],
        ),
    )]
    @testset "$(case.filename)" for case in cases
        header = ARFFFiles.load_header(joinpath(@__DIR__, "data", case.filename))
        @test header isa ARFFHeader
        # we don't have == defined yet for ARFFHeader etc so just use string equality
        @test string(header) == string(case.header)
    end
end

@testitem "load" begin
    using CategoricalArrays
    using Tables
    cases = [(
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
    )]
    @testset "$(case.filename)" for case in cases
        df = ARFFFiles.load(NamedTuple, joinpath(@__DIR__, "data", case.filename))
        @test Tables.schema(df) == Tables.schema(case.df)
        @testset "column $k" for k in propertynames(df)
            @test typeof(df[k]) == typeof(case.df[k])
            @test df[k] == case.df[k]
            if case.df[k] isa CategoricalArray
                @test df[k].pool.levels == case.df[k].pool.levels
            end
        end
        @test df == case.df
    end
end
