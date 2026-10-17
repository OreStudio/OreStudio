# CMake model diagnostics

Three checks over the codegen's CMake output, written while the parts were
migrated from hand-maintained `CMakeLists.txt` files to the model. Each one
found a defect that compiling a single target did not:

- `check_link_loss.py` — every link token and its visibility, hand file versus
  generated file, across all part CMake files. A lost library takes its include
  directories with it, and the consumer that needed them fails to compile in a
  different component than the one that lost the link.
- `check_line_loss.py` — every line a hand CMakeLists carried that the generated
  replacement dropped, grouped by content so a whole missing category appears as
  one line rather than as a hundred.
- `rewrite_extras.py` — rebuilds each component's `extra_dependencies` and
  `extra_test_dependencies` blocks from the hand file: every
  `target_link_libraries` call for that block's target, verbatim and in order,
  with the `find_package` lines that define its imported targets.

Run them from the repository root. They compare the working tree against
`git show ed7e0a3a87:<path>`, the commit before the parts were modelled, so
that constant is the base to change when a later migration needs a new one.
`rewrite_extras.py` writes only with `--write`.
