"""Independent sensitivity.csv/Decimal reconciliation; no agent imports."""
import csv
import json
import sys
from collections import defaultdict
from datetime import datetime
from decimal import Decimal, localcontext
from pathlib import Path

if len(sys.argv) != 4:
    raise SystemExit('Pass raw NATS snapshot, actual sensitivity.csv and private reference JSON.')
data = json.loads(Path(sys.argv[1]).read_text(encoding='utf-8'), parse_float=Decimal)
run = data['run']; tenant, party = run['tenant_id'], run['party_id']
started = datetime.fromisoformat(run['started_at'].replace('Z', '+00:00'))
def owned(rows):
    result = [r for r in rows if r['tenant_id'] == tenant and r['party_id'] == party]
    assert all(datetime.fromisoformat(r['recorded_at'].replace('Z','+00:00')) <= started for r in result)
    return result
books = {r['id']: r for r in owned(data['books'])}
portfolios = {r['id']: r for r in owned(data['portfolios'])}
trades = {r['id'] for r in owned(data['trades'])}
with Path(sys.argv[2]).open(encoding='utf-8', newline='') as source:
    lines = list(csv.reader(source))
headings = [h.removeprefix('#') for h in lines[0]]
csv_rows = {index+2: dict(zip(headings, row)) for index,row in enumerate(lines[1:])}
assert len({m['result_id'] for m in data['metrics']}) == 1, 'This reference expects one engine result.'
for metric in data['metrics']:
    assert metric['tenant_id'] == tenant and metric['party_id'] == party
    assert metric['report_instance_id'] == run['id'] and metric['report_definition_id'] == run['definition_id']
    assert metric['as_of'][:10] == run['priced_at'][:10]
    assert metric['source'] == 'sensitivity.csv' and metric['family'] == 'sensitivity_report'
    source = csv_rows[metric['line']]; q = json.loads(metric['qualifiers'])
    assert metric['subject_type'] == 'trade' and metric['subject_id'] == source['TradeId']
    assert Decimal(str(metric['value'])) == Decimal(source[metric['measure']])
    assert metric['currency'] == source['Currency'] == q['Currency']
    for key in ['IsPar','Factor_1','ShiftSize_1','ShiftSize_2']:
        assert q[key] == source[key]
    assert q.get('Factor_2','') == source['Factor_2']
    assert q['IsPar'] == 'false' and Decimal(q['ShiftSize_1']) > 0
assert len(data['metrics']) == len(csv_rows)*2, 'Every CSV Delta/Gamma must be stored once.'
def within(book, target):
    parent=book['parent_portfolio_id']; seen=set()
    while parent:
        assert parent in portfolios and parent not in seen
        seen.add(parent)
        if parent == target: return True
        parent=portfolios[parent]['parent_portfolio_id']
    return False
def text(number):
    value=format(number,'f')
    return value.rstrip('0').rstrip('.') if '.' in value else value
output=[]
for kind,nodes in [('book',books),('portfolio',portfolios)]:
    for identity,scope in nodes.items():
        included={identity} if kind == 'book' else {k for k,b in books.items() if within(b,identity)}
        bookings=[b for b in data['tradeBookings'] if b['book_id'] in included and b['trade_id'] in trades]
        assert all(b['tenant_id'] == tenant and datetime.fromisoformat(b['recorded_at'].replace('Z','+00:00')) <= started for b in bookings)
        expected={b['trade_id'] for b in bookings}; assert len(expected)==len(bookings)
        if not expected: continue
        buckets=defaultdict(dict)
        for m in data['metrics']:
            if m['subject_id'] not in expected: continue
            q=json.loads(m['qualifiers'])
            key=(m['measure'],q['Factor_1'],text(Decimal(q['ShiftSize_1'])),q.get('Factor_2',''),text(Decimal(q.get('ShiftSize_2','0'))),m['currency'],m['scenario'])
            assert m['subject_id'] not in buckets[key]
            buckets[key][m['subject_id']]=Decimal(str(m['value']))
        rows=[]
        for key,values in sorted(buckets.items()):
            complete=set(values)==expected
            with localcontext() as context:
                context.prec=1000;total=text(sum(values.values(),Decimal(0))) if complete else None
            rows.append(dict(zip(['measure','factor','shift','factor2','shift2','currency','scenario'],key),complete=complete,total=total,valued=len(values),missing=len(expected-values.keys()),contributors={k:text(v) for k,v in values.items()}))
        output.append({'scopeType':kind,'scope':scope['name'],'scopeId':identity,'runId':run['id'],'membershipAt':run['started_at'],'expected':len(expected),'buckets':rows})
Path(sys.argv[3]).write_text(json.dumps(output,indent=2)+'\n',encoding='utf-8',newline='\n')
print(json.dumps({'scopes':len(output),'csvDeltaGammaFigures':len(data['metrics']),'completeScopeBuckets':sum(b['complete'] for s in output for b in s['buckets'])}))
