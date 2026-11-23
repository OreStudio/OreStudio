"""Count the route pairs the gateway and the BFF answer.

Reads the BFF's route table from its server source and the gateway's from the
route builders in projects/ores.http, then reports every BFF pair with no
gateway twin. Run from the repository root:

    python3 doc/agile/versions/v0/sprint_30/serve_one_screen_from_the_new_server-remaining/evidence/route_pair_census.py

The patterns are textual. A route registered through a variable, a macro or a
call split across lines is not counted, so the gateway total is a floor and a
gateway route may exist that this script cannot see. A BFF path with a regular
expression or a wildcard is normalised only at its :name and {name} positions,
so it would read as unmatched.
"""

import glob
import re

BFF_SERVER = 'projects/ores.web/packages/bff/src/server.ts'
GATEWAY = 'projects/ores.http/**/*.cpp'

METHODS = {
    'get': 'GET', 'post': 'POST', 'put': 'PUT', 'patch': 'PATCH',
    'delete_': 'DELETE', 'del': 'DELETE', 'remove': 'DELETE',
}


def normalise(path):
    path = re.sub(r':[A-Za-z_]+', '{}', path)
    path = re.sub(r'\{[^}]+\}', '{}', path)
    return path.rstrip('/') or '/'


def bff_routes():
    text = open(BFF_SERVER, encoding='utf-8').read()
    return {(m.group(1).upper(), m.group(2)) for m in
            re.finditer(r'server\.(get|post|put|patch|delete)\(\s*[\'"]([^\'"]+)',
                        text)}


def gateway_routes():
    routes = set()
    for name in glob.glob(GATEWAY, recursive=True):
        if '/tests/' in name:
            continue
        text = open(name, encoding='utf-8', errors='ignore').read()
        for m in re.finditer(
                r'router->(get|post|put|patch|delete_|del|remove)\("([^"]+)"', text):
            routes.add((METHODS[m.group(1)], m.group(2)))
    return routes


def main():
    bff = bff_routes()
    gateway = gateway_routes()
    twins = {(method, normalise(path)) for method, path in gateway}
    missing = sorted((method, path) for method, path in bff
                     if (method, normalise(path)) not in twins)
    print(f'BFF route pairs:     {len(bff)}')
    print(f'gateway route pairs: {len(gateway)}')
    print(f'BFF pairs with no gateway twin: {len(missing)}')
    for method, path in missing:
        print(f'  {method:6} {path}')


if __name__ == '__main__':
    main()
