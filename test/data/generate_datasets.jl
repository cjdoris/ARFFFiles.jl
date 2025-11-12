#!/usr/bin/env julia
using ARFFFiles
using Dates
using CategoricalArrays

const DATADIR = @__DIR__
const COMMENT = "test data file"

function write_simple_with_missing(path)
    data = (
        nums = [1.5, 2.5, NaN],
        ints = Union{Missing, Int}[1, missing, 3],
        strs = Union{Missing, String}["foo", "bar", missing],
        cats = CategoricalArray([missing, "c", "b"], levels = ["a", "b", "c"]),
        dates = Union{Missing, Date}[Date(2020, 1, 2), missing, Date(2022, 4, 5)],
    )
    ARFFFiles.save(
        path,
        data,
        relation = "test-data",
        comment = COMMENT,
    )
end

function write_simple(path)
    data = (
        nums = [1.5, 2.5, 3.5],
        ints = [1, 2, 3],
        strs = ["foo", "bar", "baz"],
        cats = CategoricalArray(["a", "c", "b"], levels = ["a", "b", "c"]),
        dates = [Date(2020, 1, 2), Date(2021, 3, 4), Date(2022, 4, 5)],
    )
    ARFFFiles.save(
        path,
        data,
        relation = "test-data",
        comment = COMMENT,
    )
end

function write_sparse(path)
    open(path, "w") do io
        println(io, "% $COMMENT")
        println(io)
        print(io, "@RELATION ")
        ARFFFiles.write_datum(io, "sparse-example")
        println(io, "\n")
        for (name, type) in (("num1", "NUMERIC"), ("str1", "STRING"), ("cat1", "{yes,no}"))
            print(io, "@ATTRIBUTE ")
            ARFFFiles.write_datum(io, name)
            println(io, " $type")
        end
        println(io)
        println(io, "@DATA")
        rows = [
            [(0, 1.5), (1, "foo"), (2, "yes")],
            [(1, "bar")],
            [(0, 3.0), (2, "no")],
        ]
        for row in rows
            print(io, "{")
            for (j, (idx, value)) in enumerate(row)
                j > 1 && print(io, ",")
                print(io, idx, " ")
                ARFFFiles.write_datum(io, value)
            end
            println(io, "}")
        end
    end
end

function write_relational(path)
    open(path, "w") do io
        println(io, "% $COMMENT")
        println(io)
        print(io, "@RELATION ")
        ARFFFiles.write_datum(io, "relational-example")
        println(io, "\n")
        print(io, "@ATTRIBUTE ")
        ARFFFiles.write_datum(io, "id")
        println(io, " NUMERIC")
        print(io, "@ATTRIBUTE ")
        ARFFFiles.write_datum(io, "measurements")
        println(io, " RELATIONAL")
        nested_attrs = [
            ("temp", "NUMERIC"),
            ("flag", "STRING"),
        ]
        for (name, type) in nested_attrs
            print(io, "    @ATTRIBUTE ")
            ARFFFiles.write_datum(io, name)
            println(io, " $type")
        end
        print(io, "@END ")
        ARFFFiles.write_datum(io, "measurements")
        println(io)
        print(io, "@ATTRIBUTE ")
        ARFFFiles.write_datum(io, "label")
        println(io, " {yes,no}")
        println(io)
        println(io, "@DATA")
        rows = (
            (id = 1, nested = "1.0,hot\n2.0,cold", label = "yes"),
            (id = 2, nested = "3.0,warm", label = "no"),
            (id = 3, nested = "", label = missing),
        )
        for row in rows
            ARFFFiles.write_datum(io, row.id)
            print(io, ",")
            ARFFFiles.write_datum(io, row.nested)
            print(io, ",")
            ARFFFiles.write_datum(io, row.label)
            println(io)
        end
    end
end

function main()
    mkpath(DATADIR)
    write_simple_with_missing(joinpath(DATADIR, "simple_with_missing.arff"))
    write_simple(joinpath(DATADIR, "simple.arff"))
    write_sparse(joinpath(DATADIR, "sparse_example.arff"))
    write_relational(joinpath(DATADIR, "relational_example.arff"))
end

main()
