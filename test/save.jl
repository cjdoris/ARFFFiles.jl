@testitem "save" begin
    using CategoricalArrays
    using Dates
    include("setup.jl")
    cases = [(
        filename = "test_1.arff",
        df = (
            nums = [1.5, 2.5, NaN],
            ints = [1, missing, 3],
            strs = ["foo", "bar", missing],
            cats = CategoricalArray([missing, "c", "b"], levels = ["a", "b", "c"]),
            dates = [Date(2020, 1, 2), missing, Date(2022, 4, 5)],
        ),
    )]
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
end
