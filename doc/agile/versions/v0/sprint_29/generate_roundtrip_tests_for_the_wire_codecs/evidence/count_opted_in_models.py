"""Count models that opt in to the wire codec, and those with a generator.

Run from the repository root: python3 <this file>
"""
import collections
import glob
import os
import re

total = with_generator = 0
without = collections.Counter()
for path in glob.glob('projects/ores.*/modeling/*.org'):
    text = open(path).read()
    if 'ores.cpp.wire-codec.enabled: true' not in text:
        continue
    kind = re.search(r'^#\+type: (\S+)', text, re.M)
    name = os.path.basename(path)[:-4].split('.')[-1]
    component = path.split('/')[1]
    total += 1
    if glob.glob(f'projects/{component}/*/include/*/generators/{name}_generator.hpp'):
        with_generator += 1
    else:
        without[kind.group(1) if kind else '?'] += 1
print(f'opted in: {total}, with a generator: {with_generator}')
for kind, n in without.most_common():
    print(f'without a generator: {kind} {n}')
