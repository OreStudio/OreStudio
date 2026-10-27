"""Move the price processes from a seeded std::mt19937 to a keyed math::rng_stream."""
import pathlib, re, sys
root = pathlib.Path(sys.argv[1])
q = root / "projects/ores.analytics/quant"
files = sorted((q / "include/ores.analytics.quant/service/processes").glob("*.hpp")) + \
        sorted((q / "src/service/processes").glob("*.cpp")) + \
        [q / "include/ores.analytics.quant/service/process_factory.hpp", q / "src/service/process_factory.cpp",
         q / "include/ores.analytics.quant/math/rate_tree.hpp", q / "src/math/rate_tree.cpp"]
changed = 0
for f in files:
    s = f.read_text(); o = s
    in_math = "/math/" in str(f)
    ty = "rng_stream" if in_math else "math::rng_stream"
    s = re.sub(r"std::uint32_t seed\s*=\s*42", f"{ty} stream", s)
    s = re.sub(r"std::uint32_t seed\b", f"{ty} stream", s)
    s = re.sub(r"std::mt19937&\s*rng\b", f"{ty}& rng", s)
    s = re.sub(r"std::mt19937 rng_;", f"{ty} rng_;", s)
    s = re.sub(r"rng_\(seed\)", "rng_(std::move(stream))", s)
    # a delegating constructor or a factory forwards the stream it was given
    s = re.sub(r"(\(|,\s*)seed(\s*[,)])", r"\1std::move(stream)\2", s)
    s = re.sub(r"@param seed[^\n]*(\n\s*\*\s{3,}[^\n@]*)*", "@param stream the keyed stream the process draws its randomness from.", s)
    if s != o:
        if "rng_stream.hpp" not in s and f.suffix == ".hpp":
            s = re.sub(r'(#include "ores\.analytics\.quant/[^"]+"\n)', r'\1#include "ores.analytics.quant/math/rng_stream.hpp"\n', s, count=1)
        if "std::move" in s and "<utility>" not in s:
            s = re.sub(r"(#include <[a-z_]+>\n)", r"\1#include <utility>\n", s, count=1)
        f.write_text(s); changed += 1
print("files changed:", changed)
