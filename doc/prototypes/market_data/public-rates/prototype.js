/*
 * Static prototype of Public rates: the real observed values, the publisher that
 * owns them and the feed that delivered them. Mock data only; no request leaves
 * the page. Every state is a query string.
 *
 *   ?tab=providers|availability|compare|fetch
 *   ?provider=<code>&rtab=overview|underlyings|fixings|history
 *   ?class=fx|ir  ?pair=<PAIR>  ?item=<IR item>  ?filter=...
 *   ?paused=muted|hidden  ?terms=withdraw
 *   ?fetch=at|progress|done
 */

const q = new URLSearchParams(location.search);
const tab = q.get('tab') ?? 'providers';
const rtab = q.get('rtab') ?? 'overview';
const providerCode = q.get('provider');
const pausedVariant = q.get('paused') ?? 'muted';
const withdrawn = q.get('terms') === 'withdraw';
const fetchState = q.get('fetch') ?? 'at';
const filter = q.get('filter') ?? 'all';
const search = (q.get('find') ?? '').toLowerCase();
let klass = q.get('class') ?? 'fx';
let pair = q.get('pair') ?? 'EUR/USD';
let irItem = q.get('item') ?? '€STR';

const DATES = ['2026-10-15', '2026-10-16', '2026-10-19', '2026-10-20', '2026-10-21',
    '2026-10-22', '2026-10-23', '2026-10-26', '2026-10-27', '2026-10-28'];

/* The feeds, as Connectivity configures them: one per (publisher, class). */
const SOURCES = {
    'ecb-frankfurter': { supplier: 'frankfurter', session: 'frankfurter-eu', service: 'end_of_day', provider: 'ecb', class: 'fx', adapter: 'frankfurter', kind: 'live', state: 'serving', last: '19:24' },
    'ecb-ecb-data': { supplier: 'ecb-data-portal', session: 'ecb-sdw', service: 'end_of_day', provider: 'ecb', class: 'ir', adapter: 'ecb-sdw', kind: 'live', state: 'serving', last: '17:02' },
    'imf-frankfurter': { supplier: 'frankfurter', session: 'frankfurter-eu', service: 'end_of_day', provider: 'imf', class: 'fx', adapter: 'frankfurter', kind: 'live', state: 'serving', last: '19:24' },
    'boc-frankfurter': { supplier: 'frankfurter', session: 'frankfurter-eu', service: 'end_of_day', provider: 'boc', class: 'fx', adapter: 'frankfurter', kind: 'live', state: 'serving', last: '19:20' },
    'tcmb-frankfurter': { supplier: 'frankfurter', session: 'frankfurter-eu', service: 'end_of_day', provider: 'tcmb', class: 'fx', adapter: 'frankfurter', kind: 'live', state: 'not serving', last: '2026-10-27' },
    'norges-frankfurter': { supplier: 'frankfurter', session: 'frankfurter-eu', service: 'end_of_day', provider: 'norges', class: 'fx', adapter: 'frankfurter', kind: 'live', state: 'serving', last: null },
    'boe-frankfurter': { supplier: 'frankfurter', session: 'frankfurter-eu', service: 'end_of_day', provider: 'boe', class: 'fx', adapter: 'frankfurter', kind: 'live', state: 'serving', last: '15:00' },
    'boe-boe': { supplier: 'boe', session: 'boe-stats', service: 'end_of_day', provider: 'boe', class: 'ir', adapter: 'boe-stats', kind: 'live', state: 'serving', last: '15:00' },
    'fred-frankfurter': { supplier: 'frankfurter', session: 'frankfurter-eu', service: 'end_of_day', provider: 'fred', class: 'fx', adapter: 'frankfurter', kind: 'live', state: 'serving', last: '19:22' },
    'fred-api': { supplier: 'fred', session: 'fred-api', service: 'end_of_day', provider: 'fred', class: 'ir', adapter: 'fred-api', kind: 'live', state: 'serving', last: '19:22' },
    'ust-treasury': { supplier: 'treasury', session: 'treasury', service: 'end_of_day', provider: 'ust', class: 'ir', adapter: 'treasury-feed', kind: 'live', state: 'serving', last: '18:05' },
};

const FIXING = { 'EUR/USD': 1.1638, 'EUR/CAD': 1.5638, 'EUR/GBP': 0.8646, 'CAD/EUR': 0.6392, 'CAD/USD': 1.3848,
    'USD/CHF': 0.8321, 'USD/JPY': 0.8360, 'TRY/USD': 42.70, 'TRY/EUR': 49.55, 'NOK/EUR': 11.62, 'NOK/USD': 10.82,
    'GBP/USD': 1.3373, 'GBP/EUR': 1.1734 };

const PROVIDERS = [
    { code: 'ecb', name: 'European Central Bank', kind: 'reference', classes: ['fx', 'ir'],
      terms: { accepted: true, version: 'v3', who: 'super_admin', at: '2026-01-15 10:22' },
      sources: { fx: 'ecb-frankfurter', ir: 'ecb-ecb-data' },
      series: { 'EUR/USD': [1.1605, 1.1609, 1.1612, 1.1614, 1.1618, 1.1620, 1.1622, 1.1626, 1.1631, 1.1638],
                'EUR/CAD': [1.5590, 1.5601, 1.5610, 1.5614, 1.5618, 1.5620, 1.5622, 1.5626, 1.5631, 1.5638],
                'EUR/GBP': [0.8620, 0.8624, 0.8627, 0.8630, 0.8632, 0.8634, 0.8637, 0.8640, 0.8643, 0.8646] },
      ir: [{ name: '€STR', kind: 'rate' }, { name: 'EUR AAA spot curve', kind: 'curve' }] },
    { code: 'imf', name: 'International Monetary Fund', kind: 'reference', classes: ['fx'],
      terms: { accepted: true, version: 'v1', who: 'super_admin', at: '2026-09-01 09:00' },
      sources: { fx: 'imf-frankfurter' },
      series: { 'USD/EUR': [0.8616, 0.8613, 0.8611, 0.8609, 0.8607, 0.8606, 0.8604, 0.8602, 0.8600, 0.8597],
                'USD/CHF': [0.8290, 0.8294, 0.8297, 0.8300, 0.8303, 0.8306, 0.8309, 0.8312, 0.8316, 0.8321] } },
    { code: 'boc', name: 'Bank of Canada', kind: 'reference', classes: ['fx'],
      terms: { accepted: true, version: 'v2', who: 'super_admin', at: '2026-10-26 11:20' },
      sources: { fx: 'boc-frankfurter' },
      series: { 'CAD/USD': [1.3820, 1.3824, 1.3827, 1.3830, 1.3833, 1.3836, 1.3839, 1.3842, 1.3845, 1.3848],
                'CAD/EUR': [0.6410, 0.6408, 0.6406, 0.6404, 0.6402, 0.6400, 0.6398, 0.6396, 0.6394, 0.6392] } },
    { code: 'tcmb', name: 'Central Bank of Turkiye', kind: 'reference', classes: ['fx'],
      terms: { accepted: true, version: 'v1', who: 'super_admin', at: '2026-10-20 08:10' },
      sources: { fx: 'tcmb-frankfurter' },
      series: { 'TRY/USD': [42.10, 42.18, 42.25, 42.31, 42.38, 42.44, 42.50, 42.57, 42.63, 42.70] } },
    { code: 'norges', name: 'Norges Bank', kind: 'reference', classes: ['fx'],
      terms: { accepted: true, version: 'v1', who: 'super_admin', at: '2026-10-18 09:00' },
      sources: { fx: 'norges-frankfurter' }, series: {} },
    { code: 'boe', name: 'Bank of England', kind: 'reference', classes: ['fx', 'ir'],
      terms: { accepted: true, version: 'v1', who: 'super_admin', at: '2026-10-18 14:00' },
      sources: { fx: 'boe-frankfurter', ir: 'boe-boe' },
      series: { 'GBP/USD': [1.3348, 1.3351, 1.3354, 1.3357, 1.3360, 1.3362, 1.3364, 1.3367, 1.3370, 1.3373],
                'GBP/EUR': [1.1730, 1.1731, 1.1732, 1.1732, 1.1733, 1.1733, 1.1734, 1.1734, 1.1734, 1.1734] },
      ir: [{ name: 'SONIA', kind: 'rate' }, { name: 'GBP gilt curve', kind: 'curve' }] },
    { code: 'fred', name: 'Federal Reserve (H.10)', kind: 'reference', classes: ['fx', 'ir'],
      terms: { accepted: true, version: 'v1', who: 'super_admin', at: '2026-10-02 14:00' },
      sources: { fx: 'fred-frankfurter', ir: 'fred-api' },
      series: { 'EUR/USD': [1.1610, 1.1613, 1.1616, 1.1619, 1.1621, 1.1623, 1.1626, 1.1629, 1.1633, 1.1640],
                'USD/JPY': [0.8325, 0.8330, 0.8334, 0.8338, 0.8341, 0.8345, 0.8348, 0.8352, 0.8356, 0.8360] },
      ir: [{ name: 'SOFR', kind: 'rate' }, { name: 'H.15 Treasury par curve', kind: 'curve' }] },
    { code: 'ust', name: 'US Treasury', kind: 'reference', classes: ['ir'],
      terms: { accepted: true, version: 'v1', who: 'super_admin', at: '2026-10-05 10:00' },
      sources: { ir: 'ust-treasury' }, series: {},
      ir: [{ name: 'US Treasury par curve', kind: 'curve' }] },
];

const STATES = { serving: ['ok', 'serving'], 'not serving': ['bad', 'not serving'] };

const fn = {
    esc: (t) => String(t).replace(/[&<>"]/g, (c) => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;' }[c])),
    href: (changes) => {
        const next = new URLSearchParams(q);
        for (const [k, v] of Object.entries(changes)) { if (v === null) next.delete(k); else next.set(k, v); }
        return `?${next.toString()}`;
    },
    at: (code) => PROVIDERS.find((p) => p.code === code),
    sourceState: (name) => {
        const s = SOURCES[name];
        return `<span class="badge ${STATES[s.state]?.[0] ?? 'never'}"><i class="dot"></i>${STATES[s.state]?.[1] ?? s.state}</span>`;
    },
    sourceCell: (p) => Object.entries(p.sources).map(([cls, name]) =>
        `<div><span class="badge">${cls.toUpperCase()}</span> <a class="mono" href="../connectivity/index.html?source=${name}&stab=overview">${name}</a> ${fn.sourceState(name)}</div>`).join(''),
    items: (p) => Object.keys(p.series).length + (p.ir?.length ?? 0),
    sourceOf: (p, cls) => p.sources[cls] ?? null,
};

function renderList() {
    hidePanels('tab-providers');
    const kinds = ['serving', 'not serving', 'terms'];
    document.querySelector('#filters').innerHTML = ['all', ...kinds].map((k) =>
        `<a class="act" href="${fn.href({ tab: 'providers', filter: k === 'all' ? null : k })}">${k}</a>`).join('');
    let shown = PROVIDERS.filter((p) => filter === 'all' || !p.terms.accepted && filter === 'terms'
        || filter !== 'terms' && Object.values(p.sources).some((s) => SOURCES[s].state === filter));
    if (search) shown = shown.filter((p) => (p.name + p.code).toLowerCase().includes(search));
    document.querySelector('#providers tbody').innerHTML = shown.map((p) => `
        <tr>
          <td class="provider">${fn.esc(p.name)}<div class="faint mono">${p.code} · ${p.kind}</div></td>
          <td class="num">${fn.items(p)}</td>
          <td>${p.terms.accepted ? `<span class="badge ok">${p.terms.version}</span>` : '<span class="badge warn">not accepted</span>'}</td>
          <td>${fn.sourceCell(p)}</td>
          <td class="when">${Object.keys(p.series).length ? DATES[DATES.length - 1] : (p.ir?.length ? '2026-10-28' : '—')}</td>
          <td class="when">${Object.values(p.sources).map((s) => SOURCES[s].last ?? '—').join(' · ')}</td>
          <td class="acts"><a class="act" href="${fn.href({ tab: null, provider: p.code, rtab: 'overview' })}">Open</a></td>
        </tr>`).join('');
    document.querySelector('#providers-sub').textContent = `${PROVIDERS.length} publishers`;
}

function chart(values) {
    const w = 860, h = 220, pad = 34, min = Math.min(...values), max = Math.max(...values);
    const x = (i) => pad + (i * (w - 2 * pad)) / Math.max(DATES.length - 1, 1);
    const y = (v) => h - pad - ((v - min) / Math.max(max - min, 1e-9)) * (h - 2 * pad);
    const pts = values.map((v, i) => `${x(i).toFixed(1)},${y(v).toFixed(1)}`).join(' ');
    return `<svg class="chart" viewBox="0 0 ${w} ${h}" preserveAspectRatio="none"><line class="axis" x1="${pad}" y1="${h - pad}" x2="${w - pad}" y2="${h - pad}"></line><polyline class="line s0" points="${pts}"></polyline></svg>`;
}

function renderProviderRecord() {
    const p = fn.at(providerCode);
    if (!p) { location.search = fn.href({ tab: 'providers', provider: null }); return; }
    hidePanels(null);
    document.querySelector('#record').hidden = false;
    document.querySelector('#rec-crumb').textContent = p.name;
    document.querySelector('#rec-title').textContent = p.name;
    document.querySelector('#rec-meta').innerHTML = `<span class="mono faint">${p.code}</span> · ${p.kind} · ${p.classes.map((c) => c.toUpperCase()).join(', ')} · ${p.terms.accepted ? `terms ${p.terms.version}` : '<span class="badge warn">not accepted</span>'}`;
    document.querySelector('#rec-acts').innerHTML = `<a class="act" href="#">Accept terms <span class="faint">· design pending</span></a>
        <a class="act" href="?tab=fetch&provider=${p.code}">Fetch</a>`;
    document.querySelectorAll('[data-rtab]').forEach((b) => b.setAttribute('aria-selected', String(b.dataset.rtab === rtab)));
    const body = document.querySelector('#rec-body');
    const detail = (rows) => `<div class="kv">${rows.map(([k, v]) => `<span class="k">${k}</span><span>${v}</span>`).join('')}</div>`;
    const sourcesTable = `<table class="list"><thead><tr><th>Class</th><th>Feed</th><th>Supplier</th><th>Session</th><th>Service</th><th>State</th><th>Last delivery</th><th class="acts"></th></tr></thead><tbody>${Object.entries(p.sources).map(([cls, name]) => {
        const s = SOURCES[name];
        return `<tr><td>${cls.toUpperCase()}</td><td class="mono">${name}</td><td>${s.supplier}</td><td class="mono">${s.session}</td><td>${s.service}</td>
            <td>${fn.sourceState(name)}</td><td class="when">${s.last ?? '—'}</td>
            <td class="acts"><a class="act" href="../connectivity/index.html?source=${name}&stab=overview">Configure <span class="faint">· in Connectivity</span></a></td></tr>`;
    }).join('')}</tbody></table>`;

    if (rtab === 'overview') {
        body.innerHTML = `<div class="card"><div class="card-head"><h2>Publisher</h2><p>the terms, and the feed that serves each class</p></div><div class="card-body">${detail([
            ['Code', `<span class="mono">${p.code}</span>`],
            ['Kind', p.kind],
            ['Asset classes', p.classes.map((c) => c.toUpperCase()).join(', ')],
            ['Terms', p.terms.accepted ? `accepted ${p.terms.version} by ${p.terms.who} · ${p.terms.at}` : 'not accepted'],
            ['Underlyings', String(fn.items(p))],
        ])}</div></div>
        <div class="card" style="margin-top:12px"><div class="card-head"><h2>Feeds</h2><p>one feed for each class; a party routes one feed per class</p></div>${sourcesTable}</div>`;
    } else if (rtab === 'underlyings') {
        const fx = Object.keys(p.series).map((x) => `<tr><td class="mono">${x}</td><td>pair</td><td class="mono">${p.sources.fx}</td>
            <td class="when">${DATES[0]}</td><td class="when">${DATES[DATES.length - 1]}</td><td class="num">${p.series[x].length}</td>
            <td class="num">${p.code === 'ecb' && x === 'EUR/CAD' ? 2 : 0}</td><td class="acts"><a class="act" href="${fn.href({ provider: p.code, rtab: 'fixings', pair: x })}">Open</a></td></tr>`).join('');
        const ir = (p.ir ?? []).map((it) => `<tr><td class="mono">${it.name}</td><td>${it.kind}</td><td class="mono">${p.sources.ir}</td>
            <td class="when">2026-01-02</td><td class="when">2026-10-28</td><td class="num">${it.kind === 'curve' ? '10 pillars' : 204}</td>
            <td class="num">0</td><td class="acts"><span class="faint">IR series</span></td></tr>`).join('');
        body.innerHTML = `<div class="card"><div class="card-head"><h2>Underlyings</h2><p>${fn.items(p)} items, by class, each with its feed</p></div>
          <table class="list"><thead><tr><th>Item</th><th>Shape</th><th>Feed</th><th>First</th><th>Last</th><th>Values</th><th>Gaps</th><th class="acts"></th></tr></thead>
          <tbody><tr><td colspan="8" class="faint">FX</td></tr>${fx || '<tr><td colspan="8" class="faint">No FX item.</td></tr>'}
          <tr><td colspan="8" class="faint">Interest rates</td></tr>${ir || '<tr><td colspan="8" class="faint">No IR item.</td></tr>'}</tbody></table>
          <div class="faint" style="margin-top:8px;padding:0 14px 12px">The feed is provenance: the same publisher's FX and IR arrive through different feeds, and the values keep the publisher's identity.</div></div>`;
    } else if (rtab === 'fixings') {
        const values = p.series[pair] ?? p.series[Object.keys(p.series)[0]];
        const chosen = p.series[pair] ? pair : Object.keys(p.series)[0];
        if (!values) { body.innerHTML = '<div class="card"><div class="card-body"><div class="notice">No fixing stored for this provider yet.</div></div></div>'; return; }
        const table = DATES.map((d, i) => `<tr><td class="when">${d}</td><td class="num">${values[i].toFixed(4)}</td>
            <td class="mono faint">${p.sources.fx}</td><td class="mono faint">oresmd://fx/${chosen.split('/')[0]}?type=fixing&amp;index=fx&amp;source=${p.code.toUpperCase()}&amp;ccy=${chosen.split('/')[1]}</td></tr>`).reverse().join('');
        body.innerHTML = `<div class="card"><div class="card-head"><h2>${chosen}</h2><p>${values.length} fixings, delivered by ${p.sources.fx}</p>
            <span class="filters"><select>${Object.keys(p.series).map((x) => `<option ${x === chosen ? 'selected' : ''}>${x}</option>`).join('')}</select></span></div>
          <div class="card-body">${chart(values)}
          <table class="list" style="margin-top:10px"><thead><tr><th>Date</th><th>Value</th><th>Feed</th><th>Item</th></tr></thead><tbody>${table}</tbody></table></div></div>`;
    } else {
        body.innerHTML = `<div class="card"><div class="card-head"><h2>History</h2><p>the provider and its sources</p></div><div class="card-body"><div class="timeline">
          <div class="row"><span class="at">2026-10-01 09:00</span><span class="what"><b>Provider created</b> · v1 · by super_admin</span></div>
          <div class="row"><span class="at">2026-10-28 17:02</span><span class="what"><b>Source routed</b> · ${p.sources.ir ?? p.sources.fx} · by super_admin</span></div>
        </div></div></div>`;
    }
}

function renderAvailability() {
    hidePanels('tab-availability');
    const irItems = [...new Set(PROVIDERS.flatMap((p) => (p.ir ?? []).map((i) => i.name)))];
    if (klass === 'ir') {
        if (!irItems.includes(irItem)) irItem = irItems[0];
        document.querySelector('#av-pair').innerHTML = irItems.map((x) => `<option ${x === irItem ? 'selected' : ''}>${x}</option>`).join('');
        document.querySelector('#av-pair-label').textContent = irItem;
        const rows = PROVIDERS.filter((p) => (p.ir ?? []).some((i) => i.name === irItem)).map((p) => {
            const it = p.ir.find((i) => i.name === irItem);
            const s = p.sources.ir;
            return `<tr><td class="provider">${fn.esc(p.name)}<div class="faint mono">${p.code}</div></td><td>${it.kind}</td>
              <td class="mono">${s ?? '—'}</td><td>${s ? fn.sourceState(s) : ''}</td><td class="when">2026-01-02</td>
              <td class="when">2026-10-28</td><td class="num">${it.kind === 'curve' ? '10 pillars' : 204}</td></tr>`;
        }).join('');
        document.querySelector('#av-body').innerHTML = `<table class="list"><thead><tr><th>Publisher</th><th>Shape</th><th>Feed</th><th>State</th><th>First</th><th>Last</th><th>Values</th></tr></thead><tbody>${rows}</tbody></table>`;
        return;
    }
    const pairs = [...new Set(PROVIDERS.flatMap((p) => Object.keys(p.series)))].sort();
    if (!pairs.includes(pair)) pair = pairs[0];
    document.querySelector('#av-pair').innerHTML = pairs.map((x) => `<option ${x === pair ? 'selected' : ''}>${x}</option>`).join('');
    document.querySelector('#av-pair-label').textContent = pair;
    const rows = PROVIDERS.filter((p) => p.series[pair]).map((p) => {
        const s = p.sources.fx;
        return `<tr><td class="provider">${fn.esc(p.name)}<div class="faint mono">${p.code}</div></td><td class="mono">${s}</td>
          <td>${fn.sourceState(s)}</td><td class="when">${DATES[0]}</td><td class="when">${DATES[DATES.length - 1]}</td>
          <td class="num">${p.series[pair].length}</td><td class="num">0</td></tr>`;
    }).join('');
    document.querySelector('#av-body').innerHTML = `<table class="list"><thead><tr><th>Publisher</th><th>Feed</th><th>State</th><th>First</th><th>Last</th><th>Values</th><th>Gaps</th></tr></thead><tbody>${rows}</tbody></table>`;
}

function renderCompare() {
    hidePanels('tab-compare');
    const pairs = [...new Set(PROVIDERS.flatMap((p) => Object.keys(p.series)))].filter((x) =>
        PROVIDERS.filter((p) => p.series[x]).length > 1).sort();
    if (!pairs.includes(pair)) pair = pairs[0];
    document.querySelector('#cmp-pair').innerHTML = pairs.map((x) => `<option ${x === pair ? 'selected' : ''}>${x}</option>`).join('');
    const holders = PROVIDERS.filter((p) => p.series[pair]);
    const bps = (a, b) => ((a - b) / b) * 10000;
    const base = holders[0].series[pair];
    const rows = DATES.map((d, i) => `<tr><td class="when">${d}</td>${holders.map((p, k) => {
        const v = p.series[pair][i];
        return `<td class="num">${v.toFixed(4)} <span class="faint">${k === 0 ? 'base' : `${bps(v, base[i]) >= 0 ? '+' : ''}${bps(v, base[i]).toFixed(1)} bps`}</span></td>`;
    }).join('')}<td class="num">${bps(Math.max(...holders.map((p) => p.series[pair][i])), Math.min(...holders.map((p) => p.series[pair][i]))).toFixed(1)}</td></tr>`).join('');
    document.querySelector('#cmp-sub').textContent = `${holders.length} providers · the base is ${holders[0].code.toUpperCase()}`;
    document.querySelector('#cmp-body').innerHTML = chart(base) +
        `<table class="list" style="margin-top:10px"><thead><tr><th>Date</th>${holders.map((p) => `<th>${p.code.toUpperCase()}</th>`).join('')}<th>Spread bps</th></tr></thead><tbody>${rows}</tbody></table>
        <div class="faint" style="margin-top:8px">Each publisher's value comes from the feed its class routes. A comparison is across publishers, never across feeds of one publisher.</div>`;
}

function renderFetch() {
    hidePanels('tab-fetch');
    const box = document.querySelector('#fetch-body');
    if (fetchState === 'progress') {
        box.innerHTML = `<div class="dialog"><h2>Fetch in progress</h2><div class="progress"><i style="width:72.6%"></i></div>
            <div class="faint">218 of 300 days fetched, oldest first · a day already held is skipped</div>
            <div class="foot"><a class="act" href="${fn.href({ tab: 'fetch', fetch: null })}">Stop</a></div></div>`;
        return;
    }
    if (fetchState === 'done') {
        box.innerHTML = `<div class="dialog"><h2>Fetch done</h2><div>300 days asked for: <b>1,204 stored</b>, 96 already held.</div>
            <div class="foot"><a class="act" href="${fn.href({ tab: 'fetch', fetch: null })}">Close</a></div></div>`;
        return;
    }
    box.innerHTML = `<div class="dialog"><h2>The scope of a fetch</h2>
        <div class="fields">
          <label>When<select><option>Today</option><option>Yesterday</option><option selected>Range</option></select></label>
          <label>From<input value="2026-10-01"></label><label>To<input value="2026-10-28"></label>
          <label>Publisher<select><option>Every publisher</option>${PROVIDERS.map((p) => `<option>${p.name}</option>`).join('')}</select></label>
          <label>Class<select><option>FX</option><option>IR</option></select></label>
          <label>Underlyings<select><option>Every item</option><option>One item</option></select></label>
        </div>
        <div class="faint">The fetch asks the feed that serves the class. It does not change the feed's scope: that is configured in Connectivity.</div>
        <div class="foot"><a class="act primary" href="${fn.href({ tab: 'fetch', fetch: 'progress' })}">Fetch</a></div></div>`;
}

function hidePanels(except) {
    document.querySelectorAll('[role="tabpanel"]').forEach((x) => { x.hidden = x.id !== except; });
    document.querySelector('#record').hidden = true;
}

function wire() {
    document.querySelectorAll('[data-tab]').forEach((b) => b.addEventListener('click', () => {
        location.search = fn.href({ tab: b.dataset.tab, provider: null, rtab: null });
    }));
    document.querySelectorAll('[data-rtab]').forEach((b) => b.addEventListener('click', () => {
        location.search = fn.href({ provider: providerCode, rtab: b.dataset.rtab, tab: null });
    }));
    document.querySelector('#av-class').value = klass;
    document.querySelector('#av-class').addEventListener('change', (e) => { location.search = fn.href({ tab: 'availability', class: e.target.value }); });
    document.querySelector('#av-pair').addEventListener('change', (e) => {
        const key = klass === 'ir' ? 'item' : 'pair';
        location.search = fn.href({ tab: 'availability', [key]: e.target.value });
    });
    document.querySelector('#cmp-pair').addEventListener('change', (e) => { location.search = fn.href({ tab: 'compare', pair: e.target.value }); });
    document.querySelector('#search').addEventListener('change', (e) => { location.search = fn.href({ tab: 'providers', find: e.target.value || null }); });
    document.querySelector('#search').value = q.get('find') ?? '';
    document.querySelector('#refresh').addEventListener('click', () => location.reload());
}

if (providerCode) renderProviderRecord();
else {
    if (tab === 'availability') renderAvailability();
    else if (tab === 'compare') renderCompare();
    else if (tab === 'fetch') renderFetch();
    else renderList();
}
wire();
