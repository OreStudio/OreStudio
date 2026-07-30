# ORE Engine Binary Packages

Self-contained `ore` engine packages consumed by `ores.compute` --
`ores.compute.wrapper` downloads and extracts one of these tarballs per
node, then spawns the packaged executable directly against a job's
`Input/` directory. See `projects/ores.compute/wrapper/src/app/application.cpp`.

## Quick Start

```bash
# Clone/update the ORE Engine source (outside this repo -- see methodology.txt step 1)
git clone --recurse-submodules git@github.com:OreStudio/Engine.git ~/Development/ORE/Engine
cd ~/Development/ORE/Engine
git remote add upstream git@github.com:OpenSourceRisk/Engine.git

# Build
BOOST=/usr cmake --preset linux-clang-ninja-release
cmake --build build/linux-clang-ninja-release -j "$(nproc)"

# Package, self-test against TA002_IR_Swap, and install into this directory
cd /path/to/OreStudio
./external/ore/tools/package_ore.sh ~/Development/ORE/Engine/build/linux-clang-ninja-release
```

## Data Files

| File | Description |
|------|--------------|
| `ore-<version>-x64-linux.tar.gz` | Self-contained engine package: the `ore` executable, every non-glibc shared library it needs (under `lib/`, RPATH-relative), and the minimal `manifest.json` `ores.compute.wrapper` reads. |
| `ore-<version>-x64-linux.diff-report.txt` | Sign-off report from the package's self-test run: tolerant CSV diff against `TA002_IR_Swap`'s `ExpectedOutput`, flagging any schema (new/removed column) changes for review. |
| `TA002_IR_Swap.tar.gz` | A sample job input (20Y EUR fixed-vs-float IR swap, from `external/ore/examples/Academy/TA002_IR_Swap/Input/`) used to self-test every package build. Not an engine package itself. |
| `manifest.json` | Provenance for every package in this directory: engine commit/date, build flags, checksums, and what superseded what (and why). |

## Regenerating

Never hand-edit a package tarball or its manifest.json. To publish a new
engine version, follow `methodology.txt` in full (clone/build/package/
verify/install/commit) -- in short:

```bash
./external/ore/tools/package_ore.sh <ore-engine-build-dir>
```

The script builds, self-tests against `TA002_IR_Swap`, and installs into
this directory in one step. Review the printed `.diff-report.txt` before
committing, especially if it reports new/removed columns.

## Data Source

ORE Engine, OreStudio's fork of OpenSourceRisk/Engine:
https://github.com/OreStudio/Engine (fork) /
https://github.com/OpenSourceRisk/Engine (upstream).

## Related Files

- Packaging script: `external/ore/tools/package_ore.sh`
- Tolerant CSV comparator: `external/ore/tools/compare_csv.py`
- Methodology: `external/ore/packages/methodology.txt`
- Consuming component: `projects/ores.compute/wrapper/src/app/application.cpp`
- Compute app seed (engine_version/package_uri): `projects/ores.sql/populate/compute/compute_ore_app_seed.sql`
