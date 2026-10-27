"""Wrap each argument clang says cannot become a math::rng_stream in price_stream(...)."""
import pathlib, re, sys
log = pathlib.Path(sys.argv[1]).read_text(errors="replace").splitlines()
ORD = {"1st":1,"2nd":2,"3rd":3}
def nth(w): return ORD.get(w) or int(re.match(r"(\d+)", w).group(1))
targets = set()
for i, line in enumerate(log):
    m = re.match(r"(/[^:]+):(\d+):(\d+): error: (no matching (constructor|function)|no viable conversion)", line)
    if not m or "/tests/" not in m.group(1):
        continue
    found = False
    for note in log[i+1:i+40]:
        if re.match(r"/[^:]+:\d+:\d+: error:", note):
            break
        n = re.search(r"no known conversion from .* to '[^']*rng_stream'.* for (\w+) argument", note)
        if n:
            targets.add((m.group(1), int(m.group(2)), int(m.group(3)), nth(n.group(1))))
            found = True
            break
    if not found:
        # A call that left the seed to its default: the one argument missing is the stream.
        for note in log[i+1:i+40]:
            if re.match(r"/[^:]+:\d+:\d+: error:", note):
                break
            a = re.search(r"requires (?:at least )?(\d+) arguments?, but (\d+) (?:were|was) provided", note)
            if a and int(a.group(1)) == int(a.group(2)) + 1:
                targets.add((m.group(1), int(m.group(2)), int(m.group(3)), -int(a.group(1))))
                break
by_file = {}
for f, l, c, k in targets:
    by_file.setdefault(f, []).append((l, c, k))
for f, items in by_file.items():
    p = pathlib.Path(f); lines = p.read_text().split("\n")
    text = "\n".join(lines)
    offsets = [0]
    for ln in lines: offsets.append(offsets[-1] + len(ln) + 1)
    edits = []
    for l, c, k in items:
        pos = offsets[l-1] + c - 1
        open_at = min(x for x in (text.find("(", pos), text.find("{", pos)) if x >= 0)
        depth = 0; start = open_at + 1; args = []; j = open_at
        while True:
            ch = text[j]
            if ch in "([{": depth += 1
            elif ch in ")]}":
                depth -= 1
                if depth == 0:
                    args.append((start, j)); break
            elif ch == "," and depth == 1:
                args.append((start, j)); start = j + 1
            j += 1
        if k < 0:
            # Append the stream after the last argument.
            end = args[-1][1]
            edits.append((end, end))
            continue
        a, b = args[k-1]
        arg = text[a:b]
        lead = len(arg) - len(arg.lstrip()); trail = len(arg) - len(arg.rstrip())
        if "price_stream(" in arg: continue
        edits.append((a + lead, b - trail))
    for a, b in sorted(set(edits), reverse=True):
        if a == b:
            text = text[:a] + ", price_stream(42)" + text[a:]
        else:
            text = text[:a] + "price_stream(" + text[a:b] + ")" + text[b:]
    if '#include "price_stream.hpp"' not in text:
        text = re.sub(r'(#include "ores\.analytics\.quant/[^"]+"\n)', r'\1#include "price_stream.hpp"\n', text, count=1)
    p.write_text(text)
    print(f"{p.name}: {len(set(edits))} arguments wrapped")
