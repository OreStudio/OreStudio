import concurrent.futures, glob, json, os, shlex, subprocess, sys
S = sys.argv[1]
cc = json.load(open('build/output/linux-clang-release-ninja/compile_commands.json'))
args = []
for e in cc:
    a = shlex.split(e['command']); i = 0
    while i < len(a):
        t = a[i]
        if t == '-isystem':
            args += [t, a[i+1]]; i += 2; continue
        if t.startswith('-I') or (t.startswith('-D') and not t.endswith('_LIBRARY') and '_EXPORTS' not in t):
            args.append(t)
        i += 1
flat = []; seen = set(); i = 0
while i < len(args):
    key = (args[i], args[i+1]) if args[i] == '-isystem' else (args[i],)
    if key not in seen:
        seen.add(key); flat += list(key)
    i += len(key)
extra = sys.argv[2:]
def run(tu):
    r = subprocess.run(['clang++', '-std=c++23', '-fsyntax-only', '-ferror-limit=0', *extra, *flat, tu],
                       capture_output=True, text=True)
    open(tu + '.log', 'w').write(r.stderr)
    return r.returncode, os.path.basename(tu)
with concurrent.futures.ThreadPoolExecutor(4) as ex:
    for code, name in sorted(ex.map(run, sorted(glob.glob(S + '/*.cpp')))):
        print(code, name)
