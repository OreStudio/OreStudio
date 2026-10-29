/*
 * Static prototype of the Official rates screens: the provider list, the
 * provider record and its tabs, and the area's availability, compare and fetch.
 * Every state is a query string, so a reviewer opens it by address. Mock data only.
 *
 *   ?tab=providers|availability|compare|fetch
 *   ?provider=<code>&rtab=overview|underlyings|fixings|health|history
 *   ?pair=<PAIR>  ?filter=live|late|down|never|paused|terms
 *   ?paused=muted|hidden|resumed   ?terms=withdraw
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
let pair = q.get('pair') ?? 'EUR/USD';

const DATES = ['2026-10-15', '2026-10-16', '2026-10-19', '2026-10-20', '2026-10-21',
    '2026-10-22', '2026-10-23', '2026-10-26', '2026-10-27', '2026-10-28'];

const PROVIDERS = [
    { code: 'ecb', name: 'European Central Bank', kind: 'reference',
      terms: { accepted: true, version: 'v3', who: 'super_admin', at: '2026-01-15 10:22' },
      adapter: { state: 'serving', hb: '19:24', served: 44, refused: 0, unknown: 0 },
      last: { pair: 'EUR/CAD', date: '2026-10-28', received: '19:23', expected: 'today, on time', onTime: true },
      credit: 'Source: ECB statistics', storage: true, credential: 'none', rhythm: 'daily, 16:00 CET',
      series: { 'EUR/USD': [1.1605, 1.1609, 1.1612, 1.1614, 1.1618, 1.1620, 1.1622, 1.1626, 1.1631, 1.1638],
                'EUR/CAD': [1.5590, 1.5601, 1.5610, 1.5614, 1.5618, 1.5620, 1.5622, 1.5626, 1.5631, 1.5638],
                'EUR/GBP': [0.8620, 0.8624, 0.8627, 0.8630, 0.8632, 0.8634, 0.8637, 0.8640, 0.8643, 0.8646] } },
    { code: 'boc', name: 'Bank of Canada', kind: 'reference',
      terms: { accepted: true, version: 'v2', who: 'super_admin', at: '2026-10-26 11:20' },
      adapter: { state: 'serving', hb: '19:24', served: 27, refused: 0, unknown: 0 },
      last: { pair: 'CAD/EUR', date: '2026-10-27', received: '19:20', expected: 'today, late', onTime: false },
      credit: 'Source: Bank of Canada', storage: true, credential: 'none', rhythm: 'daily, 16:30 ET',
      series: { 'CAD/USD': [1.3820, 1.3824, 1.3827, 1.3830, 1.3833, 1.3836, 1.3839, 1.3842, 1.3845, 1.3848],
                'CAD/EUR': [0.6410, 0.6408, 0.6406, 0.6404, 0.6402, 0.6400, 0.6398, 0.6396, 0.6394, 0.6392] } },
    { code: 'imf', name: 'International Monetary Fund', kind: 'reference',
      terms: { accepted: !withdrawn, version: withdrawn ? 'v2' : 'v1', who: 'super_admin', at: '2026-09-01 09:00' },
      adapter: { state: 'serving', hb: '19:24', served: 48, refused: 0, unknown: 0 },
      last: { pair: 'EUR/USD', date: '2026-10-28', received: '19:24', expected: 'today, on time', onTime: true },
      credit: 'Source: IMF', storage: !withdrawn, credential: 'none', rhythm: 'daily, 12:00 ET',
      series: { 'EUR/USD': [1.1598, 1.1601, 1.1606, 1.1607, 1.1611, 1.1614, 1.1616, 1.1620, 1.1624, 1.1630],
                'USD/CHF': [0.8290, 0.8294, 0.8297, 0.8300, 0.8303, 0.8306, 0.8309, 0.8312, 0.8316, 0.8321] } },
    { code: 'fred', name: 'Federal Reserve (H.10)', kind: 'reference',
      terms: { accepted: true, version: 'v1', who: 'super_admin', at: '2026-10-02 14:00' },
      adapter: { state: 'serving', hb: '19:24', served: 31, refused: 0, unknown: 0 },
      last: { pair: 'EUR/USD', date: '2026-10-28', received: '19:22', expected: 'today, on time', onTime: true },
      credit: 'Source: Federal Reserve Board', storage: true, credential: 'none', rhythm: 'daily, 16:15 ET',
      series: { 'EUR/USD': [1.1610, 1.1613, 1.1616, 1.1619, 1.1621, 1.1623, 1.1626, 1.1629, 1.1633, 1.1640],
                'EUR/GBP': [0.8618, 0.8621, 0.8625, 0.8628, 0.8630, 0.8633, 0.8635, 0.8638, 0.8641, 0.8644] } },
    { code: 'tcmb', name: 'Central Bank of Turkiye', kind: 'reference',
      terms: { accepted: true, version: 'v1', who: 'super_admin', at: '2026-10-20 08:10' },
      adapter: { state: 'down', hb: '16:02', served: 21, refused: 3, unknown: 0 },
      last: null, credit: 'Source: TCMB', storage: true, credential: 'none', rhythm: 'daily, 15:30 TRT',
      series: { 'TRY/USD': [42.10, 42.18, 42.25, 42.31, 42.38, 42.44, 42.50, 42.57, 42.63, 42.70] } },
    { code: 'norges', name: 'Norges Bank', kind: 'reference',
      terms: { accepted: true, version: 'v1', who: 'super_admin', at: '2026-10-18 09:00' },
      adapter: { state: 'serving', hb: '19:24', served: 12, refused: 0, unknown: 0 },
      last: null, credit: 'Source: Norges Bank', storage: true, credential: 'none', rhythm: 'daily, 14:00 CET',
      series: {} },
    { code: 'boe', name: 'Bank of England', kind: 'reference',
      terms: { accepted: true, version: 'v1', who: 'super_admin', at: '2026-10-18 14:00' },
      adapter: { state: 'serving', hb: '19:24', served: 34, refused: 0, unknown: 0 },
      paused: true,
      last: { pair: 'GBP/USD', date: '2026-10-28', received: '15:00', expected: 'today, on time', onTime: true },
      credit: 'Source: Bank of England', storage: true, credential: 'none', rhythm: 'daily, 15:00 GMT',
      series: { 'GBP/USD': [1.3348, 1.3351, 1.3354, 1.3357, 1.3360, 1.3362, 1.3364, 1.3367, 1.3370, 1.3373] } },
];

const SENTENCES = {
    live: 'Serving. The last fixing is today\'s and it arrived on time.',
    late: 'Late. The publication day has passed with no fixing. Read the adapter\'s heartbeat, then ask for a fetch of the missing days.',
    down: 'Not serving. The adapter\'s heartbeat stopped, so nothing is fetched. Restart it, or find out whether its terms were withdrawn.',
    never: 'Never fixed. The terms are accepted and the adapter serves, but no value of this publisher is stored yet.',
    paused: 'Paused by a person. The publisher is not fetched. Its stored fixings and every feed that started from one stay.',
    terms: 'Terms not accepted. The publisher is not fetched until a person accepts the version shown.',
};
const TONE = { live: ['ok', 'serving'], late: ['warn', 'late'], down: ['bad', 'not serving'],
    never: ['never', 'never fixed'], paused: ['off', 'paused'], terms: ['warn', 'not accepted'] };

const fn = {
    esc: (t) => String(t).replace(/[&<>"]/g, (c) => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;' }[c])),
    href: (changes) => {
        const next = new URLSearchParams(q);
        for (const [k, v] of Object.entries(changes)) { if (v === null) next.delete(k); else next.set(k, v); }
        return `?${next.toString()}`;
    },
    badge: (kind) => `<span class="badge ${TONE[kind][0]}"><i class="dot"></i>${TONE[kind][1]}</span>`,
    status: (p) => {
        if (!p.terms.accepted) return 'terms';
        if (p.paused && pausedVariant !== 'resumed') return 'paused';
        if (p.adapter.state !== 'serving') return 'down';
        if (!p.last) return 'never';
        return p.last.onTime ? 'live' : 'late';
    },
    at: (p) => PROVIDERS.find((x) => x.code === p) ?? null,
    pairs: () => [...new Set(PROVIDERS.flatMap((p) => Object.keys(p.series)))].sort(),
    holders: (pair) => PROVIDERS.filter((p) => p.series[pair]),
};

function listRow(p) {
    const kind = fn.status(p);
    return `<tr>
        <td class="provider">${fn.esc(p.name)}<div class="faint mono">${p.code}</div></td>
        <td class="num">${Object.keys(p.series).length || '—'}</td>
        <td>${p.terms.accepted ? `<span class="badge ok">${p.terms.version}</span>` : `<span class="badge warn">not accepted</span>`}</td>
        <td>${p.last ? `<div class="fresh"><span class="when">${p.last.date}</span><span class="sub">${p.last.pair}</span></div>` : '<span class="faint">no fixing ever</span>'}</td>
        <td>${fn.badge(kind)}</td>
        <td>${p.adapter.state === 'serving' ? `<span class="faint mono">hb ${p.adapter.hb}</span>` : `<span class="badge bad">hb ${p.adapter.hb}</span>`}</td>
        <td class="acts"><a class="act" href="${fn.href({ tab: null, provider: p.code, rtab: 'overview' })}">Open</a></td>
    </tr>`;
}

function renderList() {
    document.querySelector('#list-header').hidden = false;
    document.querySelector('#record').hidden = true;
    document.querySelectorAll('[data-tab]').forEach((b) => b.setAttribute('aria-selected', String(b.dataset.tab === tab)));

    const kinds = ['live', 'late', 'down', 'never', 'paused', 'terms'];
    document.querySelector('#filters').innerHTML = ['all', ...kinds].map((k) =>
        `<a class="act" href="${fn.href({ tab: 'providers', filter: k === 'all' ? null : k })}">${k === 'all' ? 'All' : TONE[k][1]}</a>`).join('');
    const counts = Object.fromEntries(kinds.map((k) => [k, 0]));
    PROVIDERS.forEach((p) => counts[fn.status(p)]++);
    document.querySelector('#k-total').textContent = PROVIDERS.length;
    document.querySelector('#k-serving').textContent = counts.live;
    document.querySelector('#k-late').textContent = counts.late;
    document.querySelector('#k-down').textContent = counts.down;
    document.querySelector('#k-paused').textContent = counts.paused;

    let shown = PROVIDERS.filter((p) => filter === 'all' || fn.status(p) === filter);
    if (search) shown = shown.filter((p) => (p.name + p.code).toLowerCase().includes(search));
    if (pausedVariant === 'hidden') shown = shown.filter((p) => fn.status(p) !== 'paused');
    document.querySelector('#providers tbody').innerHTML = shown.map(listRow).join('');
    document.querySelector('#providers-sub').textContent = `${PROVIDERS.length} providers · ${counts.paused} paused`;
    document.querySelector('#legend').innerHTML = kinds.map((k) => `<div>${fn.badge(k)}<span>${SENTENCES[k]}</span></div>`).join('');
}

function renderAvailability() {
    document.querySelector('#list-header').hidden = false;
    document.querySelector('#record').hidden = true;
    document.querySelectorAll('[data-tab]').forEach((b) => b.setAttribute('aria-selected', String(b.dataset.tab === tab)));
    const pairs = fn.pairs();
    if (!pairs.includes(pair)) pair = pairs[0];
    document.querySelector('#av-pair').innerHTML = pairs.map((x) => `<option ${x === pair ? 'selected' : ''}>${x}</option>`).join('');
    const rows = fn.holders(pair).map((p) => {
        const values = p.series[pair];
        return `<tr><td class="provider">${fn.esc(p.name)}<div class="faint mono">${p.code}</div></td>
            <td class="when">${DATES[0]}</td><td class="when">${DATES[DATES.length - 1]}</td>
            <td class="num">${values.length}</td><td class="num">${p.code === 'ecb' ? 2 : 0}</td>
            <td>${fn.badge(fn.status(p))}</td></tr>`;
    }).join('');
    document.querySelector('#av-body').innerHTML = `<table class="list"><thead><tr><th>Publisher</th><th>First</th><th>Last</th><th>Values</th><th>Gaps</th><th>State</th></tr></thead><tbody>${rows}</tbody></table>
        <div class="faint" style="margin-top:8px">A publisher whose terms are not accepted is a row with no data and the reason. Coverage and gaps come from the same fixings read the publisher page uses.</div>`;
}

function chart(values) {
    const w = 860, h = 220, pad = 34, min = Math.min(...values), max = Math.max(...values);
    const x = (i) => pad + (i * (w - 2 * pad)) / Math.max(DATES.length - 1, 1);
    const y = (v) => h - pad - ((v - min) / Math.max(max - min, 1e-9)) * (h - 2 * pad);
    const pts = values.map((v, i) => `${x(i).toFixed(1)},${y(v).toFixed(1)}`).join(' ');
    return `<svg class="chart" viewBox="0 0 ${w} ${h}" preserveAspectRatio="none"><line class="axis" x1="${pad}" y1="${h - pad}" x2="${w - pad}" y2="${h - pad}"></line><polyline class="line s0" points="${pts}"></polyline></svg>`;
}

function renderCompare() {
    document.querySelector('#list-header').hidden = false;
    document.querySelector('#record').hidden = true;
    document.querySelectorAll('[data-tab]').forEach((b) => b.setAttribute('aria-selected', String(b.dataset.tab === tab)));
    const pairs = fn.pairs().filter((p) => fn.holders(p).filter((x) => x.series[p]).length > 1);
    if (!pairs.includes(pair)) pair = pairs[0];
    document.querySelector('#cmp-pair').innerHTML = pairs.map((x) => `<option ${x === pair ? 'selected' : ''}>${x}</option>`).join('');
    const holders = fn.holders(pair);
    const bps = (a, b) => ((a - b) / b) * 10000;
    const base = holders[0].series[pair];
    const rows = DATES.map((d, i) => `<tr><td class="when">${d}</td>${holders.map((p, k) => {
        const v = p.series[pair][i];
        return `<td class="num">${v.toFixed(4)} <span class="faint">${k === 0 ? 'base' : `${bps(v, base[i]) >= 0 ? '+' : ''}${bps(v, base[i]).toFixed(1)} bps`}</span></td>`;
    }).join('')}<td class="num">${(bps(Math.max(...holders.map((p) => p.series[pair][i])), Math.min(...holders.map((p) => p.series[pair][i])))).toFixed(1)}</td></tr>`).join('');
    document.querySelector('#cmp-sub').textContent = `${holders.length} publishers · the base is ${holders[0].code.toUpperCase()}`;
    document.querySelector('#cmp-body').innerHTML = chart(base) +
        `<table class="list" style="margin-top:10px"><thead><tr><th>Date</th>${holders.map((p) => `<th>${p.code.toUpperCase()}</th>`).join('')}<th>Spread bps</th></tr></thead><tbody>${rows}</tbody></table>`;
}

function renderFetch() {
    document.querySelector('#list-header').hidden = false;
    document.querySelector('#record').hidden = true;
    document.querySelectorAll('[data-tab]').forEach((b) => b.setAttribute('aria-selected', String(b.dataset.tab === tab)));
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
          <label>Publishers<select><option>Every publisher</option><option>One publisher</option></select></label>
          <label>Underlyings<select><option>Every pair</option><option>One pair</option></select></label>
        </div>
        <div class="faint">Only an accepted publisher is asked. The refresh request is not on the adapter contract yet; the design task names it.</div>
        <div class="foot"><a class="act primary" href="${fn.href({ tab: 'fetch', fetch: 'progress' })}">Fetch</a></div></div>`;
}

function renderRecord() {
    const p = fn.at(providerCode);
    if (!p) { location.search = fn.href({ tab: 'providers', provider: null }); return; }
    document.querySelector('#list-header').hidden = true;
    document.querySelectorAll('[role="tabpanel"]').forEach((s) => { s.hidden = true; });
    document.querySelector('#record').hidden = false;
    const kind = fn.status(p);
    document.querySelector('#rec-crumb').textContent = p.name;
    document.querySelector('#rec-title').textContent = p.name;
    document.querySelector('#rec-meta').innerHTML = `<span class="mono faint">${p.code}</span> · ${p.kind} · version ${p.terms.version} · ${fn.badge(kind)}`;
    document.querySelector('#rec-acts').innerHTML = `
        ${p.terms.accepted ? '' : '<a class="act primary" href="#">Accept terms</a>'}
        <a class="act" href="#">Pause <span class="faint">· design pending</span></a>
        <a class="act" href="?tab=fetch">Fetch</a>`;
    document.querySelectorAll('[data-rtab]').forEach((b) => b.setAttribute('aria-selected', String(b.dataset.rtab === rtab)));

    const pairs = Object.keys(p.series);
    const body = document.querySelector('#rec-body');
    if (rtab === 'overview') {
        body.innerHTML = `<div class="card"><div class="card-head"><h2>Details</h2><p>the provider record</p></div>
          <div class="card-body"><div class="kv">
            <span class="k">Code</span><span class="mono">${p.code}</span>
            <span class="k">Kind</span><span>${p.kind}</span>
            <span class="k">Terms version</span><span>${p.terms.version} ${p.terms.accepted ? fn.badge('live') : fn.badge('terms')}</span>
            <span class="k">Terms credit</span><span>${fn.esc(p.credit)}</span>
            <span class="k">Allows storage</span><span>${p.storage ? 'yes' : 'no'}</span>
            <span class="k">Credential</span><span class="mono">${fn.esc(p.credential)}</span>
            <span class="k">Expected rhythm</span><span>${fn.esc(p.rhythm)} <span class="badge warn">not modelled yet</span></span>
            <span class="k">Accepted by</span><span>${p.terms.accepted ? `${p.terms.who} · ${p.terms.at}` : 'nobody'}</span>
            <span class="k">Underlyings</span><span>${pairs.length} stored</span>
          </div></div></div>`;
    } else if (rtab === 'underlyings') {
        const rows = pairs.map((x) => `<tr><td class="mono">${x}</td><td class="when">${DATES[0]}</td>
            <td class="when">${DATES[DATES.length - 1]}</td><td class="num">${p.series[x].length}</td>
            <td class="num">${p.code === 'ecb' && x === 'EUR/CAD' ? 2 : 0}</td>
            <td class="acts"><a class="act" href="${fn.href({ provider: p.code, rtab: 'fixings', pair: x })}">Open</a></td></tr>`).join('');
        body.innerHTML = `<div class="card"><div class="card-head"><h2>Underlyings</h2><p>${pairs.length} pairs this publisher fixes</p></div>
          <table class="list"><thead><tr><th>Pair</th><th>First</th><th>Last</th><th>Values</th><th>Gaps</th><th class="acts"></th></tr></thead><tbody>${rows || '<tr><td colspan="6" class="faint">No fixing stored yet.</td></tr>'}</tbody></table></div>`;
    } else if (rtab === 'fixings') {
        const values = p.series[pair] ?? p.series[pairs[0]];
        const chosen = p.series[pair] ? pair : pairs[0];
        if (!values) { body.innerHTML = '<div class="card"><div class="card-body"><div class="notice">No fixing stored for this publisher yet.</div></div></div>'; return; }
        const table = DATES.map((d, i) => `<tr><td class="when">${d}</td><td class="num">${values[i].toFixed(4)}</td><td class="mono faint">oresmd://fx/${chosen.split('/')[0]}?type=fixing&amp;index=fx&amp;source=${p.code.toUpperCase()}&amp;ccy=${chosen.split('/')[1]}</td></tr>`).reverse().join('');
        body.innerHTML = `<div class="card"><div class="card-head"><h2>${chosen}</h2><p>${values.length} fixings in the window</p>
            <span class="filters"><select>${pairs.map((x) => `<option ${x === chosen ? 'selected' : ''}>${x}</option>`).join('')}</select></span></div>
          <div class="card-body">${chart(values)}
          <table class="list" style="margin-top:10px"><thead><tr><th>Date</th><th>Value</th><th>Item</th></tr></thead><tbody>${table}</tbody></table></div></div>
          <div class="card" style="margin-top:12px"><div class="card-head"><h2>Where a feed started</h2></div><div class="card-body">
          <div class="notice"><div class="title">Started from the ${p.code.toUpperCase()} fixing of ${values[values.length - 1].toFixed(4)} on ${DATES[DATES.length - 1]}</div>
          <div class="faint">the value this publisher's last run took</div></div></div></div>`;
    } else if (rtab === 'health') {
        body.innerHTML = `<div class="card"><div class="card-head"><h2>Adapter</h2><p>the official adapter, serving every publisher</p></div>
          <div class="card-body"><div class="kv">
            <span class="k">State</span><span>${p.adapter.state}</span>
            <span class="k">Heartbeat</span><span class="mono">${p.adapter.hb}</span>
            <span class="k">Ids served</span><span class="num">${p.adapter.served}</span>
            <span class="k">Ids refused</span><span class="num">${p.adapter.refused}</span>
            <span class="k">Ids unknown</span><span class="num">${p.adapter.unknown}</span>
            <span class="k">Last fixing</span><span>${p.last ? `${p.last.date} · received ${p.last.received}` : 'none stored'}</span>
          </div><div class="faint" style="margin-top:8px">The adapter read answers per adapter, not per publisher; this view filters it to the ids this publisher owns.</div></div></div>`;
    } else {
        const events = [['v1', p.terms.at, 'Terms accepted', p.terms.who], ['v2', '2026-10-26 11:20', 'Terms accepted', 'super_admin'],
            ['v3', '2026-10-28 09:00', 'Terms accepted', 'super_admin'], ['—', '2026-10-28 09:12', 'Provider changed: credit updated', 'super_admin']];
        body.innerHTML = `<div class="card"><div class="card-head"><h2>History</h2><p>the shared timeline</p></div>
          <div class="card-body"><div class="timeline">${events.map(([v, at, what, who]) =>
            `<div class="row"><span class="at">${at}</span><span class="what"><b>${what}</b> · ${v} · by ${who}</span></div>`).join('')}</div></div></div>`;
    }
}

function wire() {
    document.querySelectorAll('[data-tab]').forEach((b) => b.addEventListener('click', () => {
        location.search = fn.href({ tab: b.dataset.tab, provider: null, rtab: null });
    }));
    document.querySelectorAll('[data-rtab]').forEach((b) => b.addEventListener('click', () => {
        location.search = fn.href({ provider: providerCode, rtab: b.dataset.rtab, tab: null });
    }));
    document.querySelector('#av-pair').addEventListener('change', (e) => { location.search = fn.href({ tab: 'availability', pair: e.target.value }); });
    document.querySelector('#cmp-pair').addEventListener('change', (e) => { location.search = fn.href({ tab: 'compare', pair: e.target.value }); });
    document.querySelector('#search').addEventListener('change', (e) => { location.search = fn.href({ tab: 'providers', find: e.target.value || null }); });
    document.querySelector('#search').value = q.get('find') ?? '';
    document.querySelector('#refresh').addEventListener('click', () => location.reload());
}

if (providerCode) {
    renderRecord();
} else if (tab === 'availability') {
    renderAvailability();
} else if (tab === 'compare') {
    renderCompare();
} else if (tab === 'fetch') {
    renderFetch();
} else {
    renderList();
}
wire();
