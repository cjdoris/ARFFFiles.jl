@testitem "parse_header edge cases" begin
    using ARFFFiles
    using ARFFFiles: ARFFNominalType
    using ARFFFiles.Parsing

    # missing leading @ should trigger expect error
    err = try
        Parsing.parse_header(IOBuffer("@RELATION demo\nnot-a-directive\n"))
        nothing
    catch ex
        ex
    end
    @test err !== nothing
    @test occursin("expecting", sprint(showerror, err))

    # unknown directive should trigger invalid header item
    err = try
        Parsing.parse_header(IOBuffer("@RELATION demo\n@FOO bar\n"))
        nothing
    catch ex
        ex
    end
    @test err !== nothing
    @test occursin("invalid header item", sprint(showerror, err))

    # string parsing supports escaped quotes
    single = Parsing.options('\'')
    double = Parsing.options('"')
    data = codeunits("'demo\\'s'")
    _, relation = Parsing.parse_string(data, 1, length(data), 0, single, double)
    @test relation == "demo's"
    data = codeunits("'escaped\\'name'")
    _, attrname = Parsing.parse_string(data, 1, length(data), 0, single, double)
    @test attrname == "escaped'name"

    # empty nominal list is accepted
    data = codeunits("{}");
    _, attrtype = Parsing.parse_type(data, 1, length(data), 0, single, double)
    @test attrtype isa ARFFNominalType
    @test isempty((attrtype::ARFFNominalType).classes)

    # malformed escape sequences bubble the parse error
    data = codeunits("'unfinished\\")
    @test_throws ErrorException Parsing.parse_string(data, 1, length(data), 0, single, double)
    @test_throws ErrorException Parsing.parse_escape("\\")
end

@testitem "parse_type validations" begin
    using ARFFFiles.Parsing
    using ARFFFiles
    # invalid separators inside nominal braces
    single = Parsing.options('\'')
    double = Parsing.options('"')
    data = codeunits("{yes no}")
    err = try
        Parsing.parse_type(data, 1, length(data), 0, single, double)
        nothing
    catch ex
        ex
    end
    @test err !== nothing
    @test occursin(",' or '}", sprint(showerror, err))

    err = try
        Parsing.parse_type(codeunits("bogus"), 1, length("bogus"), 0, single, double)
        nothing
    catch ex
        ex
    end
    @test err !== nothing
    @test occursin("invalid type", sprint(showerror, err))

    # default DATE attributes fall back to the module default format
    data = codeunits("date")
    _, attrtype = Parsing.parse_type(data, 1, length(data), 0, single, double)
    @test attrtype isa ARFFFiles.ARFFDateType
    @test attrtype.format == ARFFFiles.ARFFDateType().format
end

@testitem "parse_javadateformat edge cases" begin
    using ARFFFiles
    using Dates

    # doubled quotes emit a literal quote
    fmt = ARFFFiles.parse_javadateformat("yy''MM")
    @test Dates.format(Date(2020, 7, 1), fmt) == "20'07"

    # unsupported format character
    @test_throws ErrorException ARFFFiles.parse_javadateformat("Q")

    # unclosed quote reports a descriptive error
    err = try
        ARFFFiles.parse_javadateformat("'open")
        nothing
    catch ex
        ex
    end
    @test err isa ErrorException
    message = sprint(showerror, err)
    @test occursin("unclosed quote", message)
    @test occursin("'open", message)
end

@testitem "parse_datum quote preference" begin
    using ARFFFiles
    using ARFFFiles: Parsing

    single = Parsing.options('\'')
    double = Parsing.options('"')

    # double-quoted strings fall back from the single-quoted parser
    data = codeunits("\"quoted\"")
    res = Parsing.parse_datum(String, data, 1, length(data), single, double)
    @test !Parsing.Parsers.invalid(res.code)
    @test Parsing.Parsers.quoted(res.code)
    @test Parsing.get_parsed_string(data, res) == "quoted"

    # mismatched quotes prefer the single-quoted parse result
    data = codeunits("'dangling\"")
    res1 = Parsing.Parsers.xparse(String, data, 1, length(data), single)
    res = Parsing.parse_datum(String, data, 1, length(data), single, double)
    @test Parsing.Parsers.invalid(res.code)
    @test res.code == res1.code
end
