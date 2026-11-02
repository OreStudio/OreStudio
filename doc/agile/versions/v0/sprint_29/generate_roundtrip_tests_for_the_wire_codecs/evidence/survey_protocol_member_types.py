"""Count member types across the generated protocol headers.

Run from the repository root: python3 <this file>
"""
import collections
import glob
import re

counts = collections.Counter()
member = re.compile(r'    ((?:std::|ores::|[a-z_:]+)[\w:<>, ]*?) (\w+)(?: = [^;]+)?;$')
for path in glob.glob('projects/*/*/include/*/messaging/*_protocol.hpp'):
    text = open(path).read()
    if 'AUTO-GENERATED' not in text:
        continue
    for body in re.findall(r'\nstruct \w+ \{(.*?)\n\};', text, re.S):
        for line in body.split('\n'):
            m = member.match(line)
            if m and 'static' not in line and 'using' not in line:
                t = re.sub(r'ores::\w+::domain::\w+', 'DOMAIN', m.group(1))
                counts[re.sub(r'ores::\w+::messaging::\w+', 'MESSAGE', t)] += 1
for t, n in counts.most_common(40):
    print(n, t)
print('total', sum(counts.values()))
