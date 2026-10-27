"""Compile each codec as its own translation unit, report its wall time and
peak memory, then link the benchmark.

usage: python3 build.py <repository root> <output dir>

The flags are the release flags of the project's own build.
"""
import os
import pathlib
import subprocess
import sys
import time

root, out = (pathlib.Path(a).resolve() for a in sys.argv[1:3])
here = pathlib.Path(__file__).resolve().parent
build = root / "build/output/linux-clang-release-ninja"
vcpkg = build / "vcpkg_installed/x64-linux"
publish = build / "publish/lib"
out.mkdir(parents=True, exist_ok=True)

includes = [f"-I{root}/projects/{p}/include" for p in
            ("ores.platform", "ores.utility", "ores.dq/api", "ores.logging")]
flags = ["-O3", "-std=c++23", "-fPIC", "-Wall", "-Wextra", "-Wno-missing-field-initializers",
         "-Wno-missing-designated-field-initializers", "-Werror", *includes, f"-I{here}",
         "-isystem", f"{vcpkg}/include"]

print(f"{'unit':18s} {'wall s':>7s} {'peak MiB':>9s}")
units = ("reflected_codec", "generated_codec", "main")
for unit in units:
    cmd = ["clang++", *flags, "-c", str(here / f"{unit}.cpp"), "-o", str(out / f"{unit}.o")]
    began = time.monotonic()
    pid = os.spawnvp(os.P_NOWAIT, cmd[0], cmd)
    _, status, usage = os.wait4(pid, 0)
    if status != 0:
        sys.exit(f"{unit} failed to compile")
    print(f"{unit:18s} {time.monotonic() - began:7.2f} {usage.ru_maxrss / 1024:9.0f}")

subprocess.run(["clang++", *(str(out / f"{u}.o") for u in units), "-o", str(out / "codec_bench"),
                f"-L{vcpkg}/lib", f"-L{publish}", f"-Wl,-rpath,{publish}",
                "-lores.utility", "-lores.platform", "-lreflectcpp", "-lyyjson", "-lmsgpack-c",
                "-lpthread"], check=True)
print("linked", out / "codec_bench")
