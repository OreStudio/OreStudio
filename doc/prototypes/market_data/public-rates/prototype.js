/*
 * Static prototype of Public rates: the observed values, the publisher that owns
 * them and the feed that delivered them. Mock data only; no request leaves the
 * page. Every state is a query string.
 *
 *   ?tab=providers|availability|compare|fetch
 *   ?class=fx|ir  ?state=fresh|late|no data  ?find=<text>
 *   ?provider=<code>&rtab=overview|underlyings|fixings|history&class=fx
 *   ?pair=<PAIR>  ?item=<IR item>  ?fetch=at|progress|done
 */

const q = new URLSearchParams(location.search);
const tab = q.get('tab') ?? 'providers';
const rtab = q.get('rtab') ?? 'overview';
const providerCode = q.get('provider');
const klass = q.get('class') ?? 'all';
const stateFilter = q.get('state') ?? 'all';
const find = (q.get('find') ?? '').toLowerCase();
const fetchState = q.get('fetch') ?? 'at';
const fetchProvider = q.get('fetch-provider') ?? 'ecb';
let pair = q.get('pair') ?? 'EUR/USD';
let irItem = q.get('item') ?? '€STR';

const DATES = ['2026-10-15', '2026-10-16', '2026-10-19', '2026-10-20', '2026-10-21',
    '2026-10-22', '2026-10-23', '2026-10-26', '2026-10-27', '2026-10-28'];

/* One row is one publisher on one class: the grain of the route and of freshness. */
const PUBLISHERS = [
    { code: 'ecb', name: 'European Central Bank', kind: 'reference', classes: ['fx', 'ir'],
      terms: { accepted: true, version: 'v3', who: 'super_admin', at: '2026-01-15 10:22' },
      series: { 'EUR/USD': [1.1605, 1.1609, 1.1612, 1.1614, 1.1618, 1.1620, 1.1622, 1.1626, 1.1631, 1.1638],
                'EUR/CAD': [1.5590, 1.5601, 1.5610, 1.5614, 1.5618, 1.5620, 1.5622, 1.5626, 1.5631, 1.5638],
                'EUR/GBP': [0.8620, 0.8624, 0.8627, 0.8630, 0.8632, 0.8634, 0.8637, 0.8640, 0.8643, 0.8646] },
      ir: [{ name: '€STR', kind: 'rate' }, { name: 'EUR AAA spot curve', kind: 'curve' }],
      rows: [
        { cls: 'fx', feed: 'ecb-frankfurter', supplier: 'frankfurter', session: 'frankfurter-eu', service: 'end_of_day',
          routed: true, alternatives: [{ feed: 'ecb-bloomberg', supplier: 'bloomberg' }],
          last: '2026-10-28', at: '19:24', freshness: 'today', state: 'fresh' },
        { cls: 'ir', feed: 'ecb-ecb-data', supplier: 'ecb-data-portal', session: 'ecb-sdw', service: 'end_of_day',
          routed: true, alternatives: [], last: '2026-10-28', at: '17:02', freshness: 'today', state: 'fresh' } ] },
    { code: 'imf', name: 'International Monetary Fund', kind: 'reference', classes: ['fx'],
      terms: { accepted: true, version: 'v1', who: 'super_admin', at: '2026-09-01 09:00' },
      series: { 'USD/EUR': [0.8616, 0.8613, 0.8611, 0.8609, 0.8607, 0.8606, 0.8604, 0.8602, 0.8600, 0.8597],
                'USD/CHF': [0.8290, 0.8294, 0.8297, 0.8300, 0.8303, 0.8306, 0.8309, 0.8312, 0.8316, 0.8321] },
      rows: [{ cls: 'fx', feed: 'imf-frankfurter', supplier: 'frankfurter', session: 'frankfurter-eu', service: 'end_of_day',
        routed: true, alternatives: [], last: '2026-10-28', at: '19:24', freshness: 'today', state: 'fresh' }] },
    { code: 'boc', name: 'Bank of Canada', kind: 'reference', classes: ['fx'],
      terms: { accepted: true, version: 'v2', who: 'super_admin', at: '2026-10-26 11:20' },
      series: { 'CAD/USD': [1.3820, 1.3824, 1.3827, 1.3830, 1.3833, 1.3836, 1.3839, 1.3842, 1.3845, 1.3848],
                'CAD/EUR': [0.6410, 0.6408, 0.6406, 0.6404, 0.6402, 0.6400, 0.6398, 0.6396, 0.6394, 0.6392] },
      rows: [{ cls: 'fx', feed: 'boc-frankfurter', supplier: 'frankfurter', session: 'frankfurter-eu', service: 'end_of_day',
        routed: true, alternatives: [], last: '2026-10-27', at: '19:20', freshness: 'yesterday', state: 'late' }] },
    { code: 'tcmb', name: 'Central Bank of Turkiye', kind: 'reference', classes: ['fx'],
      terms: { accepted: true, version: 'v1', who: 'super_admin', at: '2026-10-20 08:10' },
      series: { 'TRY/USD': [42.10, 42.18, 42.25, 42.31, 42.38, 42.44, 42.50, 42.57, 42.63, 42.70] },
      rows: [{ cls: 'fx', feed: 'tcmb-frankfurter', supplier: 'frankfurter', session: 'frankfurter-eu', service: 'end_of_day',
        routed: true, alternatives: [], last: '2026-10-27', at: '16:02', freshness: '2 days', state: 'late' }] },
    { code: 'norges', name: 'Norges Bank', kind: 'reference', classes: ['fx'],
      terms: { accepted: true, version: 'v1', who: 'super_admin', at: '2026-10-18 09:00' },
      series: {},
      rows: [{ cls: 'fx', feed: 'norges-frankfurter', supplier: 'frankfurter', session: 'frankfurter-eu', service: 'end_of_day',
        routed: true, alternatives: [], last: null, at: '—', freshness: '—', state: 'no data' }] },
    { code: 'boe', name: 'Bank of England', kind: 'reference', classes: ['fx', 'ir'],
      terms: { accepted: true, version: 'v1', who: 'super_admin', at: '2026-10-18 14:00' },
      series: { 'GBP/USD': [1.3348, 1.3351, 1.3354, 1.3357, 1.3360, 1.3362, 1.3364, 1.3367, 1.3370, 1.3373],
                'GBP/EUR': [1.1730, 1.1731, 1.1732, 1.1732, 1.1733, 1.1733, 1.1734, 1.1734, 1.1734, 1.1734] },
      ir: [{ name: 'SONIA', kind: 'rate' }, { name: 'GBP gilt curve', kind: 'curve' }],
      rows: [
        { cls: 'fx', feed: 'boe-frankfurter', supplier: 'frankfurter', session: 'frankfurter-eu', service: 'end_of_day',
          routed: true, alternatives: [], last: '2026-10-28', at: '15:00', freshness: 'today', state: 'fresh' },
        { cls: 'ir', feed: 'boe-boe', supplier: 'boe', session: 'boe-stats', service: 'end_of_day',
          routed: true, alternatives: [], last: '2026-10-28', at: '15:00', freshness: 'today', state: 'fresh' } ] },
    { code: 'fred', name: 'Federal Reserve (H.10)', kind: 'reference', classes: ['fx', 'ir'],
      terms: { accepted: true, version: 'v1', who: 'super_admin', at: '2026-10-02 14:00' },
      series: { 'EUR/USD': [1.1610, 1.1613, 1.1616, 1.1619, 1.1621, 1.1623, 1.1626, 1.1629, 1.1633, 1.1640],
                'USD/JPY': [0.8325, 0.8330, 0.8334, 0.8338, 0.8341, 0.8345, 0.8348, 0.8352, 0.8356, 0.8360] },
      ir: [{ name: 'SOFR', kind: 'rate' }, { name: 'H.15 Treasury par curve', kind: 'curve' }],
      rows: [
        { cls: 'fx', feed: 'fred-frankfurter', supplier: 'frankfurter', session: 'frankfurter-eu', service: 'end_of_day',
          routed: true, alternatives: [], last: '2026-10-28', at: '19:22', freshness: 'today', state: 'fresh' },
        { cls: 'ir', feed: 'fred-api', supplier: 'fred', session: 'fred-api', service: 'end_of_day',
          routed: true, alternatives: [], last: '2026-10-28', at: '19:22', freshness: 'today', state: 'fresh' } ] },
    { code: 'ust', name: 'US Treasury', kind: 'reference', classes: ['ir'],
      terms: { accepted: true, version: 'v1', who: 'super_admin', at: '2026-10-05 10:00' },
      series: {},
      ir: [{ name: 'US Treasury par curve', kind: 'curve' }],
      rows: [{ cls: 'ir', feed: 'ust-treasury', supplier: 'treasury', session: 'treasury', service: 'end_of_day',
        routed: true, alternatives: [], last: '2026-10-28', at: '18:05', freshness: 'today', state: 'fresh' }] },
];

const STATE_TONE = { fresh: ['ok', 'fresh'], late: ['warn', 'late'], 'no data': ['never', 'no data'], 'no route': ['warn', 'no route'] };

const fn = {
    esc: (t) => String(t).replace(/[&<>"]/g, (c) => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;' }[c])),
    href: (changes) => {
        const next = new URLSearchParams(q);
        for (const [k, v] of Object.entries(changes)) { if (v === null) next.delete(k); else next.set(k, v); }
        return `?${next.toString()}`;
    },
    at: (code) => PUBLISHERS.find((p) => p.code === code),
    rows: () => PUBLISHERS.flatMap((p) => p.rows.map((r) => ({ publisher: p, ...r }))),
    classItems: (p, cls) => cls === 'fx' ? Object.keys(p.series) : (p.ir ?? []).map((i) => i.name),
    counts: (p, cls) => fn.classItems(p, cls).length,
    badge: (state) => `<span class="badge ${STATE_TONE[state]?.[0] ?? 'never'}"><i class="dot"></i>${STATE_TONE[state]?.[1] ?? state}</span>`,
    lastCell: (r) => r.last ? `${r.last} <span class="faint">${r.at}</span>` : '<span class="faint">—</span>',
    rowOf: (p, cls) => p.rows.find((r) => r.cls === cls),
};

function hidePanels(except) {
    document.querySelectorAll('[role="tabpanel"]').forEach((x) => { x.hidden = x.id !== except; });
    document.querySelector('#record').hidden = true;
}

function renderList() {
    hidePanels('tab-providers');
    document.querySelectorAll('[data-tab]').forEach((b) => b.setAttribute('aria-selected', String(b.dataset.tab === 'providers')));
    const classes = ['all', 'fx', 'ir'];
    const states = ['all', 'fresh', 'late', 'no data'];
    document.querySelector('#filters').innerHTML =
        `<span class="seg" role="group" aria-label="Asset class">${classes.map((c) =>
            `<a class="${klass === c ? 'primary' : ''}" href="${fn.href({ tab: 'providers', provider: null, rtab: null, class: c === 'all' ? null : c })}">${c === 'all' ? 'ALL' : c.toUpperCase()}</a>`).join('')}</span>
         <select id="state-filter" aria-label="Freshness state">${states.map((s) =>
            `<option value="${s}" ${stateFilter === s ? 'selected' : ''}>${s === 'all' ? 'Any state' : s}</option>`).join('')}</select>`;
    document.querySelector('#state-filter').addEventListener('change', (e) => {
        location.search = fn.href({ tab: 'providers', provider: null, rtab: null, state: e.target.value === 'all' ? null : e.target.value });
    });
    let rows = fn.rows();
    if (klass !== 'all') rows = rows.filter((r) => r.cls === klass);
    if (stateFilter !== 'all') rows = rows.filter((r) => r.state === stateFilter);
    if (find) rows = rows.filter((r) => (r.publisher.name + r.publisher.code + r.feed).toLowerCase().includes(find));

    document.querySelector('#providers tbody').innerHTML = rows.map((r) => {
        const p = r.publisher;
        return `<tr>
          <td class="provider">${fn.esc(p.name)}<div class="faint mono">${p.code} · ${p.kind}</div></td>
          <td><span class="badge">${r.cls.toUpperCase()}</span></td>
          <td class="num">${fn.counts(p, r.cls)}</td>
          <td class="mono">${r.feed}${r.alternatives.length ? ` <span class="badge">+${r.alternatives.length} alt</span>` : ''}</td>
          <td>${fn.esc(r.supplier)}</td>
          <td>${r.service}</td>
          <td class="when">${fn.lastCell(r)}</td>
          <td>${fn.esc(r.freshness)}</td>
          <td>${fn.badge(r.state)}</td>
          <td class="acts"><a class="act" href="${fn.href({ tab: null, provider: p.code, rtab: 'overview', class: r.cls })}">Open</a></td>
        </tr>`;
    }).join('') || '<tr><td colspan="10" class="faint">No publisher matches.</td></tr>';
    document.querySelector('#providers-sub').textContent = `${rows.length} rows · one publisher on one class`;
    document.querySelector('#legend').innerHTML = ['fresh', 'late', 'no data', 'no route'].map((s) =>
        `<div>${fn.badge(s)}<span>${s === 'fresh' ? 'The expected fixing is held and arrived on time.' :
            s === 'late' ? 'The expected fixing is missing; read the last fixing and ask for a fetch.' :
            s === 'no data' ? 'No value is stored; the feed is configured but never delivered.' :
            'The publisher has no feed routed for this class.'}</span></div>`).join('')
        + '<div class="faint">A row is one publisher on one class: the grain of the route and of freshness. Freshness comes from the feed schedule, not from the session state.</div>';
}

function chart(values) {
    const w = 860, h = 220, pad = 34, min = Math.min(...values), max = Math.max(...values);
    const x = (i) => pad + (i * (w - 2 * pad)) / Math.max(DATES.length - 1, 1);
    const y = (v) => h - pad - ((v - min) / Math.max(max - min, 1e-9)) * (h - 2 * pad);
    const pts = values.map((v, i) => `${x(i).toFixed(1)},${y(v).toFixed(1)}`).join(' ');
    return `<svg class="chart" viewBox="0 0 ${w} ${h}" preserveAspectRatio="none"><line class="axis" x1="${pad}" y1="${h - pad}" x2="${w - pad}" y2="${h - pad}"></line><polyline class="line s0" points="${pts}"></polyline></svg>`;
}

function renderRecord() {
    const p = fn.at(providerCode);
    if (!p) { location.search = fn.href({ tab: 'providers', provider: null }); return; }
    hidePanels(null);
    document.querySelector('#record').hidden = false;
    const cls = p.classes.includes(klass) ? klass : p.classes[0];
    const row = fn.rowOf(p, cls);
    document.querySelector('#rec-crumb').textContent = p.name;
    document.querySelector('#rec-title').textContent = p.name;
    document.querySelector('#rec-meta').innerHTML = `<span class="mono faint">${p.code}</span> · ${p.kind} · ${p.classes.map((c) => c.toUpperCase()).join(', ')} · ${p.terms.accepted ? `terms ${p.terms.version}` : '<span class="badge warn">terms not accepted</span>'}`
        + (row ? ` · <span class="badge">${cls.toUpperCase()}</span> ${fn.badge(row.state)} ${fn.esc(row.freshness)}` : '');
    const classPicker = p.classes.length > 1
        ? `<label class="faint" style="margin-left:8px">Class <select id="class-pick">${p.classes.map((c) => `<option value="${c}" ${c === cls ? 'selected' : ''}>${c.toUpperCase()}</option>`).join('')}</select></label>`
        : '';
    document.querySelector('#rec-acts').innerHTML = `<a class="act" href="../connectivity/index.html?publisher=${p.code}">Configure <span class="faint">· in Connectivity</span></a>
        <a class="act" href="${fn.href({ tab: 'fetch', provider: null, rtab: null, 'fetch-provider': p.code, class: cls })}">Fetch</a>${classPicker}`;
    document.querySelectorAll('[data-rtab]').forEach((b) => b.setAttribute('aria-selected', String(b.dataset.rtab === rtab)));
    const body = document.querySelector('#rec-body');
    const detail = (rows) => `<div class="kv">${rows.map(([k, v]) => `<span class="k">${k}</span><span>${v}</span>`).join('')}</div>`;
    const readsTable = `<table class="list"><thead><tr><th>Class</th><th>Reads</th><th>Supplier</th><th>Session</th><th>Service</th><th>Last fixing</th><th>Freshness</th><th>State</th><th>Alternative</th></tr></thead><tbody>${p.rows.map((r) =>
        `<tr><td>${r.cls.toUpperCase()}</td><td class="mono"><a href="${fn.href({ provider: p.code, rtab: 'overview', class: r.cls })}">${r.feed}</a></td>
         <td>${fn.esc(r.supplier)}</td><td class="mono faint">${r.session}</td><td>${r.service}</td>
         <td class="when">${fn.lastCell(r)}</td><td>${fn.esc(r.freshness)}</td><td>${fn.badge(r.state)}</td>
         <td class="faint">${r.alternatives.length ? r.alternatives.map((a) => `${a.feed} (${a.supplier})`).join(', ') : '—'}</td></tr>`).join('')}</tbody></table>`;

    if (rtab === 'overview') {
        body.innerHTML = `<div class="card"><div class="card-head"><h2>Publisher</h2><p>the terms, and the feed read for each class</p></div><div class="card-body">${detail([
            ['Code', `<span class="mono">${p.code}</span>`],
            ['Kind', p.kind],
            ['Asset classes', p.classes.map((c) => c.toUpperCase()).join(', ')],
            ['Terms', p.terms.accepted ? `accepted ${p.terms.version} by ${p.terms.who} · ${p.terms.at}` : 'not accepted'],
            ['Underlyings', String(p.classes.reduce((n, c) => n + fn.counts(p, c), 0))],
        ])}<div class="faint" style="margin-top:8px">The feed is the route's choice; the alternatives are other feeds of the same publisher and class, not read by this party.</div></div></div>
        <div class="card" style="margin-top:12px"><div class="card-head"><h2>Reads</h2><p>one feed for each class</p></div>${readsTable}</div>`;
    } else if (rtab === 'underlyings') {
        const fxRows = cls === 'fx' ? Object.keys(p.series).map((x) => `<tr><td class="mono">${x}</td><td>pair</td><td class="mono">${row.feed}</td>
            <td>${fn.esc(row.supplier)}</td><td class="when">${DATES[0]}</td><td class="when">${DATES[DATES.length - 1]}</td>
            <td class="num">${p.series[x].length}</td><td class="num">0</td><td>${fn.badge(row.state)}</td>
            <td class="acts"><a class="act" href="${fn.href({ provider: p.code, rtab: 'fixings', class: 'fx', pair: x })}">Open</a></td></tr>`).join('') : '';
        const irRows = cls === 'ir' ? (p.ir ?? []).map((it) => `<tr><td class="mono">${it.name}</td><td>${it.kind}</td><td class="mono">${row.feed}</td>
            <td>${fn.esc(row.supplier)}</td><td class="when">2026-01-02</td><td class="when">2026-10-28</td>
            <td class="num">${it.kind === 'curve' ? '10 pillars' : 204}</td><td class="num">0</td><td>${fn.badge(row.state)}</td>
            <td class="acts"><span class="faint">IR series</span></td></tr>`).join('') : '';
        body.innerHTML = `<div class="card"><div class="card-head"><h2>Underlyings</h2><p>${cls.toUpperCase()} items, each with the feed that delivered it</p></div>
          <table class="list"><thead><tr><th>Item</th><th>Shape</th><th>Feed</th><th>Supplier</th><th>First</th><th>Last</th><th>Values</th><th>Gaps</th><th>State</th><th class="acts"></th></tr></thead>
          <tbody>${fxRows || irRows || '<tr><td colspan="10" class="faint">No item in this class.</td></tr>'}</tbody></table>
          <div class="faint" style="margin-top:8px;padding:0 14px 12px">The feed is provenance: the value belongs to the publisher, and the feed says which path delivered it.</div></div>`;
    } else if (rtab === 'fixings') {
        if (cls === 'ir') {
            body.innerHTML = `<div class="card"><div class="card-head"><h2>${row.feed}</h2><p>a rate is one value a day; a curve is a set of pillars of the same day</p></div>
              <div class="card-body"><div class="notice">The IR series is read as pillars, not as a single line. Its values are listed under Underlyings and read through the same fixings read.</div></div></div>`;
            return;
        }
        const chosen = p.series[pair] ? pair : Object.keys(p.series)[0];
        const values = p.series[chosen];
        if (!values) { body.innerHTML = '<div class="card"><div class="card-body"><div class="notice">No fixing stored for this publisher and class yet.</div></div></div>'; return; }
        const table = DATES.map((d, i) => `<tr><td class="when">${d}</td><td class="num">${values[i].toFixed(4)}</td>
            <td class="mono faint">${row.feed}</td><td class="mono faint">oresmd://fx/${chosen.split('/')[0]}?type=fixing&amp;index=fx&amp;source=${p.code.toUpperCase()}&amp;ccy=${chosen.split('/')[1]}</td></tr>`).reverse().join('');
        body.innerHTML = `<div class="card"><div class="card-head"><h2>${chosen}</h2><p>${values.length} fixings, delivered by ${row.feed} (${row.supplier})</p>
            <span class="filters"><select>${Object.keys(p.series).map((x) => `<option ${x === chosen ? 'selected' : ''}>${x}</option>`).join('')}</select></span></div>
          <div class="card-body">${chart(values)}
          <table class="list" style="margin-top:10px"><thead><tr><th>Date</th><th>Value</th><th>Feed</th><th>Item</th></tr></thead><tbody>${table}</tbody></table></div></div>`;
    } else {
        body.innerHTML = `<div class="card"><div class="card-head"><h2>History</h2><p>terms and feeds, read only here</p></div><div class="card-body"><div class="timeline">
          <div class="row"><span class="at">2026-10-01 09:00</span><span class="what"><b>Publisher created</b> · v1 · by super_admin</span></div>
          <div class="row"><span class="at">${p.terms.at}</span><span class="what"><b>Terms accepted</b> · ${p.terms.version} · by ${p.terms.who}</span></div>
          <div class="row"><span class="at">2026-10-28 17:02</span><span class="what"><b>Feed routed</b> · ${row.feed} · by super_admin</span></div>
        </div></div></div>`;
    }
}

function renderAvailability() {
    hidePanels('tab-availability');
    const acls = klass === 'ir' ? 'ir' : 'fx';
    const items = acls === 'ir'
        ? [...new Set(PUBLISHERS.flatMap((p) => (p.ir ?? []).map((i) => i.name)))]
        : [...new Set(PUBLISHERS.flatMap((p) => Object.keys(p.series)))].sort();
    if (acls === 'ir') { if (!items.includes(irItem)) irItem = items[0]; }
    else if (!items.includes(pair)) pair = items[0];
    document.querySelector('#av-class').value = acls;
    document.querySelector('#av-pair').innerHTML = items.map((x) => `<option ${x === (acls === 'ir' ? irItem : pair) ? 'selected' : ''}>${x}</option>`).join('');
    const rows = PUBLISHERS.flatMap((p) => p.rows.filter((r) => r.cls === acls).map((r) => ({ p, r })))
        .filter(({ p }) => acls === 'ir' ? (p.ir ?? []).some((i) => i.name === irItem) : p.series[pair]);
    document.querySelector('#av-body').innerHTML = `<table class="list"><thead><tr><th>Publisher</th><th>Feed</th><th>Supplier</th><th>Service</th><th>First</th><th>Last</th><th>Values</th><th>Gaps</th><th>Freshness</th><th>State</th></tr></thead><tbody>${rows.map(({ p, r }) => {
        const first = acls === 'ir' ? '2026-01-02' : DATES[0];
        const last = acls === 'ir' ? '2026-10-28' : DATES[DATES.length - 1];
        const count = acls === 'ir' ? ((p.ir ?? []).find((i) => i.name === irItem)?.kind === 'curve' ? '10 pillars' : 204) : p.series[pair].length;
        return `<tr><td class="provider">${fn.esc(p.name)}<div class="faint mono">${p.code}</div></td>
          <td class="mono">${r.feed}</td><td>${fn.esc(r.supplier)}</td><td>${r.service}</td>
          <td class="when">${first}</td><td class="when">${last}</td><td class="num">${count}</td><td class="num">0</td>
          <td>${fn.esc(r.freshness)}</td><td>${fn.badge(r.state)}</td></tr>`;
    }).join('') || '<tr><td colspan="10" class="faint">No publisher holds this item.</td></tr>'}</tbody></table>
      <div class="faint" style="margin-top:8px">Coverage and gaps come from the same fixings read. The feed is the one the party routes for that publisher and class.</div>`;
}

function renderCompare() {
    hidePanels('tab-compare');
    const pairs = [...new Set(PUBLISHERS.flatMap((p) => Object.keys(p.series)))].filter((x) =>
        PUBLISHERS.filter((p) => p.series[x] && p.rows.some((r) => r.cls === 'fx' && r.routed)).length > 1).sort();
    if (!pairs.includes(pair)) pair = pairs[0];
    document.querySelector('#cmp-pair').innerHTML = pairs.map((x) => `<option ${x === pair ? 'selected' : ''}>${x}</option>`).join('');
    const holders = PUBLISHERS.filter((p) => p.series[pair] && p.rows.some((r) => r.cls === 'fx' && r.routed));
    const bps = (a, b) => ((a - b) / b) * 10000;
    const base = holders[0].series[pair];
    const rows = DATES.map((d, i) => `<tr><td class="when">${d}</td>${holders.map((p, k) => {
        const v = p.series[pair][i];
        return `<td class="num">${v.toFixed(4)} <span class="faint">${k === 0 ? 'base' : `${bps(v, base[i]) >= 0 ? '+' : ''}${bps(v, base[i]).toFixed(1)} bps`}</span></td>`;
    }).join('')}<td class="num">${bps(Math.max(...holders.map((p) => p.series[pair][i])), Math.min(...holders.map((p) => p.series[pair][i]))).toFixed(1)}</td></tr>`).join('');
    const heads = holders.map((p) => {
        const r = p.rows.find((x) => x.cls === 'fx');
        return `${p.code.toUpperCase()}<div class="faint">${r.feed} · ${r.supplier}</div>`;
    }).join('</th><th>');
    document.querySelector('#cmp-sub').textContent = `${holders.length} publishers · the base is ${holders[0].code.toUpperCase()}`;
    document.querySelector('#cmp-body').innerHTML = chart(base) +
        `<table class="list" style="margin-top:10px"><thead><tr><th>Date</th><th>${heads}</th><th>Spread bps</th></tr></thead><tbody>${rows}</tbody></table>
        <div class="faint" style="margin-top:8px">Each column is the publisher's routed feed. A comparison is across publishers, never across feeds of one publisher.</div>`;
}

function renderFetch() {
    hidePanels('tab-fetch');
    const box = document.querySelector('#fetch-body');
    const p = fn.at(fetchProvider) ?? PUBLISHERS[0];
    const cls = p.classes.includes(klass) ? klass : p.classes[0];
    const r = fn.rowOf(p, cls);
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
    const realTime = r.service === 'real_time';
    box.innerHTML = `<div class="dialog"><h2>The scope of a fetch</h2>
        <div class="fields">
          <label>Publisher<select id="fetch-publisher">${PUBLISHERS.map((x) => `<option value="${x.code}" ${x.code === p.code ? 'selected' : ''}>${fn.esc(x.name)}</option>`).join('')}</select></label>
          <label>Class<select>${p.classes.map((c) => `<option ${c === cls ? 'selected' : ''}>${c.toUpperCase()}</option>`).join('')}</select></label>
          <label>Feed<input value="${r.feed} · ${r.supplier} · ${r.service}" disabled></label>
          <label>When<select><option>Today</option><option>Yesterday</option><option selected>Range</option></select></label>
          <label>From<input value="2026-10-01"></label><label>To<input value="2026-10-28"></label>
          <label>Underlyings<select><option>Every item of the class</option><option>One item</option></select></label>
        </div>
        <div class="faint">This asks ${r.feed} on ${r.session} (${r.supplier}, ${r.service}) for ${fn.counts(p, cls)} ${cls.toUpperCase()} items.</div>
        ${realTime
            ? '<div class="notice">A real-time feed cannot be backfilled by a range. Its gaps are a session incident; read the session health in Connectivity.</div>'
            : `<div class="foot"><a class="act primary" href="${fn.href({ tab: 'fetch', fetch: 'progress' })}">Fetch</a></div>
               <div class="faint">Only an accepted publisher is asked. The fetch does not change the feed's scope; that is configured in Connectivity.</div>`}
      </div>`;
    box.querySelector('#fetch-publisher').addEventListener('change', (e) => {
        location.search = fn.href({ tab: 'fetch', 'fetch-provider': e.target.value });
    });
}

function wire() {
    document.querySelectorAll('[data-tab]').forEach((b) => b.addEventListener('click', () => {
        location.search = fn.href({ tab: b.dataset.tab, provider: null, rtab: null });
    }));
    document.querySelectorAll('[data-rtab]').forEach((b) => b.addEventListener('click', () => {
        location.search = fn.href({ provider: providerCode, rtab: b.dataset.rtab, tab: null });
    }));
    const pick = document.querySelector('#class-pick');
    if (pick) pick.addEventListener('change', (e) => {
        location.search = fn.href({ provider: providerCode, rtab: rtab, class: e.target.value });
    });
    document.querySelector('#av-class').addEventListener('change', (e) => {
        location.search = fn.href({ tab: 'availability', class: e.target.value });
    });
    document.querySelector('#av-pair').addEventListener('change', (e) => {
        location.search = fn.href({ tab: 'availability', [(q.get('class') === 'ir') ? 'item' : 'pair']: e.target.value });
    });
    document.querySelector('#cmp-pair').addEventListener('change', (e) => {
        location.search = fn.href({ tab: 'compare', pair: e.target.value });
    });
    document.querySelector('#search').addEventListener('change', (e) => {
        location.search = fn.href({ tab: 'providers', find: e.target.value || null });
    });
    document.querySelector('#search').value = q.get('find') ?? '';
    document.querySelector('#refresh').addEventListener('click', () => location.reload());
}

if (providerCode) renderRecord();
else if (tab === 'availability') renderAvailability();
else if (tab === 'compare') renderCompare();
else if (tab === 'fetch') renderFetch();
else renderList();
wire();
