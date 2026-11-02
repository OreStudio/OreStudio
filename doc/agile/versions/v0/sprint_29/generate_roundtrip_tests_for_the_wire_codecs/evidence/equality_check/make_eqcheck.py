"""Write one translation unit per api part that asserts every generated
protocol message is equality comparable.

Run from the repository root: python3 make_eqcheck.py <out_dir>
Then compile them with run_eqcheck.py <out_dir>.
"""
import collections
import glob
import os
import re
import sys

out = sys.argv[1]
groups = collections.defaultdict(list)
headers = (glob.glob('projects/*/*/include/*/messaging/*_protocol.hpp')
           + glob.glob('projects/*/include/*/messaging/*_protocol.hpp'))
for header in headers:
    text = open(header).read()
    if 'AUTO-GENERATED' not in text:
        continue
    namespace = re.search(r'^namespace (ores::[\w:]+) \{', text, re.M)
    names = re.findall(r'friend bool operator==\(const (\w+)&', text)
    if not names:
        continue
    part = header.split('/include/')[0]
    groups[part].append((header.split('/include/', 1)[1], namespace.group(1), names))
total = 0
for part, items in groups.items():
    lines = ['#include <concepts>']
    lines += [f'#include "{path}"' for path, _, _ in items]
    for _, namespace, names in items:
        for name in names:
            lines.append(f'static_assert(std::equality_comparable<{namespace}::{name}>, '
                         f'"{namespace}::{name}");')
            total += 1
    tu = part.replace('projects/', '').replace('/', '_') + '.cpp'
    open(os.path.join(out, tu), 'w').write('\n'.join(lines) + '\n')
print(len(groups), 'parts', total, 'messages')
