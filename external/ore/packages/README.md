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
| `ore-<version>-x64-windows.tar.gz` | Windows x64 engine package: `ore.exe`, its redistributable DLL closure, runtime manifest, provenance and licences. |
| `ore-<version>-x64-windows.provenance.json` | Build configuration, exact Engine/submodule revisions, dependency baseline, runtime file hashes and final archive SHA-256. |
| `ore-<version>-x64-linux.diff-report.txt` | Sign-off report from the package's self-test run: tolerant CSV diff against `TA002_IR_Swap`'s `ExpectedOutput`, flagging any schema (new/removed column) changes for review. |
| `TA002_IR_Swap.tar.gz` | A sample job input (20Y EUR fixed-vs-float IR swap, from `external/ore/examples/Academy/TA002_IR_Swap/Input/`) used to self-test every package build. Not an engine package itself. |
| `manifest.json` | Provenance for every package in this directory: engine commit/date, build flags, checksums, and what superseded what (and why). |

## Windows x64

Use a separate checkout of the **existing** OreStudio/Engine fork, not a new
fork or an application submodule. Pin the source to the Linux 1.8.17.0
package's commit so the platform addition does not also upgrade pricing.
The exact commit is authoritative: this package's version tag was initially
provisional, and the source's CMake version fields can lag the package label.

From PowerShell 7 at the OreStudio repository root (VS 2022 C++ tools, CMake,
Git and Python 3.12+ installed):

```powershell
git clone --no-checkout https://github.com/OreStudio/Engine.git ../Engine
git -C ../Engine checkout --detach 3d75a69087911a7e2cf1e6882da7bc27135c0762
git -C ../Engine submodule update --init --recursive
git submodule update --init vcpkg
./external/ore/tools/build_ore_windows.ps1 -SourceDir ../Engine -Jobs 4
./external/ore/tools/package_ore_windows.ps1 -BuildDir ../Engine/build/orestudio-windows-x64-release
```

The build uses `windows-ninja-x64-release` with only the CLI target enabled,
dynamic MSVC/Boost runtimes, and a dedicated pinned vcpkg manifest under
`external/ore/tools/windows/`. Whole-program optimisation is disabled;
`-Jobs` controls Ninja build concurrency. Existing
OreStudio dependency installations are not modified. VS's developer shell is
loaded automatically when needed. A cold build can take hours.

The Windows dependency overlay fixes Boost uBLAS's MSVC iterator-macro
restoration using `push_macro`/`pop_macro`; the unmodified header can fail
when `<span>` follows it. This is the workaround documented in
[Boost uBLAS issue 77](https://github.com/boostorg/ublas/issues/77).
It changes no ORE source or pricing formula. A small C++ compile probe checks
the include sequence before the engine build. Overlay file hashes are
recorded in the build provenance. The manifest includes Boost header modules
used directly by ORE/QuantLib as well as the compiled CMake components.

The packaging command creates `ore-<version>-x64-windows.tar.gz` with
`manifest.json`, `ore.exe`, recursively resolved non-system DLLs beside the
executable, licences and `provenance.json`, all at the extraction root.
Every packaged PE binary must be x64. Missing dependencies fail packaging;
VC runtime DLLs come from VS's redistributable directory rather than relying
on a development machine's installed runtime. Windows system DLLs remain
host dependencies. MSVC builds ORE's own libraries statically; do not assume
the Linux shared-library list applies to Windows.

The actual archive is extracted and run with a fresh sample job and a PATH
containing only Windows system directories. All reference CSVs must match
within `rtol=1e-6`; schema differences are printed for review. The command
only installs an archive after this verification passes, and refuses to
overwrite different bytes under an existing package filename. It writes a
`.diff-report.txt`, `.provenance.json` and `.engine.log` beside the tarball.
For development reruns use `-OutDir` with a fresh output directory. Review
the diff report and add the provenance to this directory's catalogue
`manifest.json` when accepting a package. Keep existing Linux packages.

The runtime manifest preserves the wrapper's current contract:

```json
{"executable": "ore.exe", "args": ["Input/ore.xml"]}
```

Platform identity belongs in the package filename, provenance, and compute
app-version/platform registration, not a new runtime manifest field.

Publish from an authenticated system-tenant OreStudio shell:

```text
compute publish-package ORE 1.8.17.0 x64-windows --wrapper-version 1.0.0 --file external/ore/packages/ore-1.8.17.0-x64-windows.tar.gz
```

This adds the Windows platform to the existing application version while
preserving its Linux platform. Registration and uploading bytes are both
required; copying a tarball into this directory alone does not publish it.
The standard Acme provisioning recipe publishes the bundled Windows package
alongside both Linux packages. The publisher registers the Windows platform
and checksum through the existing service API; no shared SQL seed change is
needed.
Run the compute grid smoke recipe against the version UUID, dispatching
15 copies of `TA002_IR_Swap.tar.gz`, then download and compare a result's
CSVs. Check that all five Windows workers execute successful jobs.
No database recreation is needed to add a platform package.

The Windows worker must preserve native environment entries, including
empty values, when starting the engine while setting `TZ=UTC`. Older wrappers
that convert each environment value to UTF-8 can fail before engine launch
with `size_as_utf8: The parameter is incorrect`; the native environment
handoff in `ores.compute.wrapper` fixes this.

The `ORE Windows engine package` GitHub Actions workflow builds and tests
the same pinned source and uploads the verified archive and reports as CI
artifacts. Run it manually, in line with the upstream policy that PR checks
run locally. It does not publish to a running OreStudio installation.

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
