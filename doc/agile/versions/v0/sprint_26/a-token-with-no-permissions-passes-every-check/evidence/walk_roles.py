"""Walk each role through the BFF and print one status per route.

Signs in as every role the walk covers, calls the same routes, and prints a
table of HTTP statuses. A change the role should not make is attempted on a
walk account, never on an administrator.
"""
import http.cookiejar, json, sys, urllib.request

BFF = 'http://127.0.0.1:21402'
ROLES = [
    ('SuperAdmin', 'admin', sys.argv[1]),
    ('TenantAdmin', 'acme_admin@acme_corporation', sys.argv[2]),
] + [(r, f'walk_{r.lower()}@acme_corporation', sys.argv[3])
     for r in ('Viewer', 'Trading', 'Sales', 'Support', 'Operations')]

ROUTES = [
    ('GET', '/api/session', None),
    ('GET', '/api/parties', None),
    ('GET', '/api/accounts', None),
    ('GET', '/api/login-info', None),
    ('GET', '/api/sessions', None),
    ('GET', '/api/change-reasons', None),
    ('GET', '/api/seed-profiles', None),
    ('GET', '/api/tenants', None),
    ('POST', '/api/accounts/{target}/lock', {}),
    ('POST', '/api/accounts/{target}/unlock', {}),
    ('POST', '/api/provision-party', {'fullName': 'Walk Party', 'shortCode': 'WALKP'}),
]


def opener():
    return urllib.request.build_opener(
        urllib.request.HTTPCookieProcessor(http.cookiejar.CookieJar()))


def call(op, method, path, body=None):
    data = None if body is None else json.dumps(body).encode()
    req = urllib.request.Request(BFF + path, data=data, method=method,
                                 headers={'content-type': 'application/json'})
    try:
        with op.open(req) as r:
            return r.status, json.loads(r.read() or b'null')
    except urllib.error.HTTPError as e:
        try:
            payload = json.loads(e.read() or b'null')
        except ValueError:
            payload = None
        return e.code, payload


def target_id(op):
    status, body = call(op, 'GET', '/api/accounts')
    if status != 200:
        return None
    for a in body.get('accounts', []):
        if a.get('username') == 'walk_sales':
            return a.get('id') or a.get('accountId')
    return None


def sign_in(op, principal, password):
    status, body = call(op, 'POST', '/api/session', {'username': principal, 'password': password})
    if status == 200 and isinstance(body, dict) and body.get('outcome') == 'party-required':
        status, body = call(op, 'POST', '/api/session/party',
                            {'partyId': body['availableParties'][0]['id']})
    return status


admin = opener()
sign_in(admin, 'acme_admin@acme_corporation', sys.argv[2])
target = target_id(admin) or 'unknown'

print('target ' + target, file=sys.stderr)
print('route\t' + '\t'.join(r for r, _, _ in ROLES))
results = {}
for role, principal, password in ROLES:
    op = opener()
    results[role] = {'sign in': sign_in(op, principal, password)}
    for method, path, body in ROUTES:
        s, payload = call(op, method, path.replace('{target}', target), body)
        results[role][f'{method} {path}'] = s
for key in ['sign in'] + [f'{m} {p}' for m, p, _ in ROUTES]:
    print(key + '\t' + '\t'.join(str(results[r][key]) for r, _, _ in ROLES))
