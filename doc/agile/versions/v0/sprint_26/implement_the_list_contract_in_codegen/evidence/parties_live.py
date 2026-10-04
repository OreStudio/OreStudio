"""Read the parties page in pages of three, live, and print each row's parent.

A row whose parent sits on another page names it only if the second read by
id found it. The argument is the Acme administrator's password.
"""
import http.cookiejar, json, sys, urllib.request

BFF = 'http://127.0.0.1:21402'
op = urllib.request.build_opener(urllib.request.HTTPCookieProcessor(http.cookiejar.CookieJar()))


def call(method, path, body=None):
    req = urllib.request.Request(BFF + path, method=method,
                                 data=None if body is None else json.dumps(body).encode(),
                                 headers={'content-type': 'application/json'})
    with op.open(req) as r:
        return json.loads(r.read() or b'null')


answer = call('POST', '/api/session', {'username': 'acme_corporation_admin@acme_corporation',
                                       'password': sys.argv[1]})
if answer.get('outcome') == 'party-required':
    call('POST', '/api/session/party', {'partyId': answer['availableParties'][0]['id']})
codes = {}
pages = []
offset = 0
while True:
    page = call('GET', f'/api/parties?offset={offset}&limit=3')
    pages.append(page)
    for p in page['parties']:
        codes[p['id']] = p['code']
    offset += 3
    if offset >= page['totalCount']:
        break
print('total', pages[0]['totalCount'])
for number, page in enumerate(pages):
    on_page = {p['id'] for p in page['parties']}
    for p in page['parties']:
        where = '-' if p['parentId'] is None else ('same page' if p['parentId'] in on_page else 'other page')
        print(number, p['code'], '| parent:', codes.get(p['parentId'], p['parentId']), '|', where,
              '| named:', p['parentName'])
