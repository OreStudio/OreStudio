"""Independent Decimal reference over raw NATS snapshots; no agent code imports."""
import json
import sys
from datetime import datetime
from decimal import Decimal, localcontext
from pathlib import Path

if len(sys.argv) != 3:
    raise SystemExit('Pass private raw snapshot JSON and private reference output JSON.')
data = json.loads(Path(sys.argv[1]).read_text(encoding='utf-8'), parse_float=Decimal)
run = data['run']
party, tenant = run['party_id'], run['tenant_id']
started = datetime.fromisoformat(run['started_at'].replace('Z', '+00:00'))

def owned(rows):
    selected = [r for r in rows if r['party_id'] == party and r['tenant_id'] == tenant]
    assert all(datetime.fromisoformat(r['recorded_at'].replace('Z', '+00:00')) <= started for r in selected)
    return selected

trades = {r['id'] for r in owned(data['trades'])}
books = {r['id']: r for r in owned(data['books'])}
portfolios = {r['id']: r for r in owned(data['portfolios'])}
metrics = {}
for metric in data['metrics']:
    assert metric['party_id'] == party and metric['tenant_id'] == tenant
    assert metric['report_instance_id'] == run['id'] and metric['report_definition_id'] == run['definition_id']
    assert metric['as_of'][:10] == run['priced_at'][:10]
    assert metric['source'] == 'npv.csv' and metric['family'] == 'npv_report' and metric['measure'] == 'NPV(Base)'
    assert metric['subject_type'] == 'trade' and not metric['scenario'] and not metric['factor']
    assert metric['subject_id'] not in metrics
    metrics[metric['subject_id']] = metric

def within(book, portfolio):
    current, visited = book['parent_portfolio_id'], set()
    while current and current != '00000000-0000-0000-0000-000000000000':
        assert current not in visited
        visited.add(current)
        if current == portfolio:
            return True
        current = portfolios[current]['parent_portfolio_id']
    return False

def decimal_text(value):
    if value == 0:
        return '0'
    text = format(value, 'f')
    return text.rstrip('0').rstrip('.') if '.' in text else text

output = []
for kind, nodes in [('book', books), ('portfolio', portfolios)]:
    for identity, selected in nodes.items():
        included = {identity} if kind == 'book' else {key for key, book in books.items() if within(book, identity)}
        bookings = [b for b in data['tradeBookings'] if b['trade_id'] in trades and b['book_id'] in included]
        assert all(b['tenant_id'] == tenant and datetime.fromisoformat(b['recorded_at'].replace('Z', '+00:00')) <= started for b in bookings)
        ids = {b['trade_id'] for b in bookings}
        assert len(ids) == len(bookings)
        if not ids:
            continue
        currencies = {json.loads(metrics[i]['qualifiers'])['BaseCurrency'] for i in ids}
        assert len(currencies) == 1
        currency = next(iter(currencies))
        assert len(currency) == 3 and currency.isalpha() and currency.isupper()
        values = {i: Decimal(str(metrics[i]['value'])) for i in ids}
        with localcontext() as context:
            context.prec = 1000
            total = sum(values.values(), Decimal(0))
        output.append({'scopeType': kind, 'scope': selected['name'], 'scopeId': identity,
                       'runId': run['id'], 'membershipAt': run['started_at'], 'currency': currency, 'total': decimal_text(total),
                       'expectedTradeCount': len(ids),
                       'contributors': {i: decimal_text(value) for i, value in values.items()}})
Path(sys.argv[2]).write_text(json.dumps(output, indent=2) + '\n', encoding='utf-8', newline='\n')
print(json.dumps({'outputPath': sys.argv[2], 'verifiedScopes': len(output)}))
