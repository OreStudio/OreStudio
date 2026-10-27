"""Compile each benchmark client as its own translation unit, report its wall
time and peak memory, then link the benchmark.

usage: python3 build.py <repository root> <libpqxx install root> <output dir>

The flags are the release flags of the project's own build. The libpqxx unit
alone gets -fclang-abi-compat=17, because the GCC-built libpqxx mangles its
constrained templates the way clang 17 did.
"""
import os
import pathlib
import subprocess
import sys
import time

root, pqxx, out = (pathlib.Path(a).resolve() for a in sys.argv[1:4])
here = pathlib.Path(__file__).resolve().parent
vcpkg = root / "build/output/linux-clang-release-ninja/vcpkg_installed/x64-linux"
publish = root / "build/output/linux-clang-release-ninja/publish/lib"
out.mkdir(parents=True, exist_ok=True)

flags = ["-O3", "-std=c++23", "-fPIC", "-Wall", "-Wextra", "-Wno-missing-field-initializers",
         "-Wno-missing-designated-field-initializers", "-Werror",
         f"-I{root}/projects/ores.platform/include", f"-I{here}"]
units = {
    "sqlgen_client": ["-isystem", f"{vcpkg}/include"],
    "pqxx_client": ["-isystem", f"{pqxx}/include", "-isystem", f"{vcpkg}/include",
                    "-fclang-abi-compat=17"],
    "libpq_client": ["-isystem", f"{vcpkg}/include"],
    "main": ["-isystem", f"{vcpkg}/include"],
}

print(f"{'unit':16s} {'wall s':>7s} {'peak MiB':>9s}")
for unit, extra in units.items():
    cmd = ["clang++", *flags, *extra, "-c", str(here / f"{unit}.cpp"), "-o", str(out / f"{unit}.o")]
    began = time.monotonic()
    pid = os.spawnvp(os.P_NOWAIT, cmd[0], cmd)
    _, status, usage = os.wait4(pid, 0)
    if status != 0:
        sys.exit(f"{unit} failed to compile")
    print(f"{unit:16s} {time.monotonic() - began:7.2f} {usage.ru_maxrss / 1024:9.0f}")

subprocess.run(["clang++", *(str(out / f"{u}.o") for u in units), "-o", str(out / "repository_bench"),
                f"-L{vcpkg}/lib", f"-L{pqxx}/lib", f"-L{publish}", f"-Wl,-rpath,{publish}",
                "-lsqlgen", "-lreflectcpp", "-lpqxx", "-lores.platform",
                "-lpq", "-lpgcommon", "-lpgport", "-lssl", "-lcrypto", "-lpthread"], check=True)
print("linked", out / "repository_bench")
