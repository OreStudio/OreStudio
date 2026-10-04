"""Read the roster and the parties page through the BFF, live.

Bootstraps the administrator, provisions Acme and one test tenant, then reads
the routes the task moved and prints what each answered. Passwords are the
command-line arguments: the administrator's, then the tenant administrators'.
"""
import http.cookiejar, json, sys, urllib.request

BFF = 'http://127.0.0.1:21402'
ADMIN_PASSWORD, TENANT_PASSWORD = sys.argv[1], sys.argv[2]


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
            return e.code, json.loads(e.read() or b'null')
        except ValueError:
            return e.code, None


def sign_in(op, principal, password):
    status, body = call(op, 'POST', '/api/session', {'username': principal, 'password': password})
    if status == 200 and isinstance(body, dict) and body.get('outcome') == 'party-required':
        status, body = call(op, 'POST', '/api/session/party',
                            {'partyId': body['availableParties'][0]['id']})
    return status


def provision(op, code, name, hostname):
    return call(op, 'POST', '/api/provision-tenant', {
        'profileCode': 'acme_demo', 'tenantCode': code, 'tenantName': name,
        'tenantHostname': hostname, 'adminUsername': f'{code}_admin',
        'adminEmail': f'admin@{hostname}', 'adminPassword': TENANT_PASSWORD})[0]


admin = opener()
print('bootstrap', call(admin, 'POST', '/api/bootstrap/administrator', {
    'principal': 'admin', 'password': ADMIN_PASSWORD, 'email': 'admin@system.ores'})[0])
print('sign in', sign_in(admin, 'admin', ADMIN_PASSWORD))
print('provision acme', provision(admin, 'acme_corporation', 'Acme Corporation', 'acme_corporation'))


def roster(query):
    status, body = call(admin, 'GET', '/api/tenants' + query)
    if status != 200:
        return status, body
    rows = [(t['code'], t['type'], t['status'], (t.get('setup') or {}).get('status'))
            for t in body['tenants']]
    return status, {'rows': rows, 'total': body['totalCount'],
                    'hiddenTest': body['hiddenTestCount'],
                    'setupUnavailable': body['setupUnavailable']}


for query in ['', '?includeTest=true', '?search=ACME', '?search=zzz_nothing',
              '?type=system', '?status=active', '?limit=1&offset=0']:
    print('roster' + query, *roster(query))

tenant = opener()
print('tenant admin sign in', sign_in(tenant, 'acme_corporation_admin@acme_corporation',
                                      TENANT_PASSWORD))
status, body = call(tenant, 'GET', '/api/tenants')
print('roster as tenant admin', status)
for query in ['?offset=0&limit=5', '?offset=5&limit=5']:
    status, body = call(tenant, 'GET', '/api/parties' + query)
    if status == 200:
        print('parties' + query, status, body['totalCount'],
              [(p['code'], p['parentName'] if p['parentId'] else None) for p in body['parties']])
    else:
        print('parties' + query, status, body)
