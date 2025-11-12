# Agent Notes for ARFFFiles.jl

- This repo uses Julia's workspace-aware test layout. The root `Project.toml` lists the `test` project under `[workspace]`, and the tests themselves live in `test/Project.toml`.
  - Activate and instantiate the full workspace with `julia --project=@. -e 'using Pkg; Pkg.instantiate(workspace=true; allow_autoprecomp=false)'` when dependencies change.
  - Tests should be executed with `julia --project=@. -e 'using Pkg; Pkg.test()'` so that the workspace metadata is respected.
- The `test` project is a plain environment (no package). Activate it from the repository root; `TestItemRunner` will still execute the `@testitem` files via `include`.
- Test fixtures under `test/data` are generated via `test/data/generate_datasets.jl`. Run that script with Julia instead of editing the `.arff` fixtures by hand, and keep the emitted comment string set to `"test data file"` so the save tests comparing against the fixtures continue to pass.
- If additional tooling or conventions become important, append them here so future agents stay up to date. **Keep this file current with any new discoveries.**
- When working on relational columns, remember that nested `ARFFReader`s get reused; reset their buffers (see the `:R` handling in `src/ARFFFiles.jl`) if you make structural changes.
