testdir = joinpath(dirname(dirname(pathof(ARFFFiles))), "test")
datadir = joinpath(testdir, "data")
@assert isdir(testdir)
@assert isdir(datadir)
