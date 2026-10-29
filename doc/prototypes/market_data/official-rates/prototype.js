/*
 * Static prototype: mock providers, items and fixings, and the six surfaces the
 * story builds. Every state is a query string so a reviewer opens it by address.
 * No framework, no build, no request; every value is invented.
 *
 *   ?tab=providers|publisher|availability|compare|fetch|config
 *   ?provider=<code>  ?pair=<PAIR>  ?filter=all|serving|late|down|paused
 *   ?paused=muted|hidden|resumed   ?terms=withdraw   ?fetch=at|progress|done
 */

const q = new URLSearchParams(location.search);
const tab = q.get('tab') ?? 'providers';
const pausedVariant = q.get('paused') ?? 'muted';
const withdrawn = q.get('terms') === 'withdraw';
const fetchState = q.get('fetch') ?? 'at';
const filter = q.get('filter') ?? 'all';
const search = (q.get('find') ?? '').toLowerCase();
const selectedProvider = q.get('provider') ?? 'ecb';
let selectedPair = q.get('pair') ?? 'EUR/USD';

const PROVIDERS = [
    { code: 'ecb', name: 'European Central Bank', pairs: ['EUR/USD', 'EUR/CAD', 'EUR/GBP'],
      terms: { accepted: true, version: 'v3', who: 'super_admin', at: '2026-01-15 10:22' },
      adapter: { state: 'serving', hb: '19:24' },
      last: { pair: 'EUR/CAD', date: '2026-10-28', received: '19:23', expected: 'today, on time', onTime: true },
      kind: 'reference', credit: 'Source: ECB statistics', storage: true, credential: 'none', schedule: 'daily, 16:00 CET' },
    { code: 'boc', name: 'Bank of Canada', pairs: ['CAD/USD', 'CAD/EUR'],
      terms: { accepted: true, version: 'v2', who: 'super_admin', at: '2026-10-26 11:20' },
      adapter: { state: 'serving', hb: '19:24' },
      last: { pair: 'CAD/EUR', date: '2026-10-27', received: '19:20', expected: 'today, late', onTime: false },
      kind: 'reference', credit: 'Source: Bank of Canada', storage: true, credential: 'none', schedule: 'daily, 16:30 ET' },
    { code: 'imf', name: 'International Monetary Fund', pairs: ['EUR/USD', 'USD/CHF', 'USD/JPY'],
      terms: { accepted: !withdrawn, version: withdrawn ? 'v2' : 'v1', who: 'super_admin', at: '2026-09-01 09:00' },
      adapter: { state: 'serving', hb: '19:24' },
      last: { pair: 'EUR/USD', date: '2026-10-28', received: '19:24', expected: 'today, on time', onTime: true },
      kind: 'reference', credit: 'Source: IMF', storage: !withdrawn, credential: 'none', schedule: 'daily, 12:00 ET' },
    { code: 'fred', name: 'Federal Reserve (H.10)', pairs: ['EUR/USD', 'USD/JPY', 'EUR/GBP'],
      terms: { accepted: true, version: 'v1', who: 'super_admin', at: '2026-10-02 14:00' },
      adapter: { state: 'serving', hb: '19:24' },
      last: { pair: 'EUR/USD', date: '2026-10-28', received: '19:22', expected: 'today, on time', onTime: true },
      kind: 'reference', credit: 'Source: Federal Reserve Board', storage: true, credential: 'none', schedule: 'daily, 16:15 ET' },
    { code: 'tcmb', name: 'Central Bank of Turkiye', pairs: ['TRY/USD', 'TRY/EUR'],
      terms: { accepted: true, version: 'v1', who: 'super_admin', at: '2026-10-20 08:10' },
      adapter: { state: 'down', hb: '16:02' },
      last: null,
      kind: 'reference', credit: 'Source: TCMB', storage: true, credential: 'none', schedule: 'daily, 15:30 TRT' },
    { code: 'norges', name: 'Norges Bank', pairs: ['NOK/EUR', 'NOK/USD'],
      terms: { accepted: true, version: 'v1', who: 'super_admin', at: '2026-10-18 09:00' },
      adapter: { state: 'serving', hb: '19:24' },
      last: null,
      kind: 'reference', credit: 'Source: Norges Bank', storage: true, credential: 'none', schedule: 'daily, 14:00 CET' },
    { code: 'boe', name: 'Bank of England', pairs: ['GBP/USD', 'GBP/EUR'],
      terms: { accepted: true, version: 'v1', who: 'super_admin', at: '2026-10-18 14:00' },
      adapter: { state: 'serving', hb: '19:24' },
      paused: true,
      last: { pair: 'GBP/USD', date: '2026-10-28', received: '15:00', expected: 'today, on time', onTime: true },
      kind: 'reference', credit: 'Source: Bank of England', storage: true, credential: 'none', schedule: 'daily, 15:00 GMT' },
];

const DATES = ['2026-10-15', '2026-10-16', '2026-10-19', '2026-10-20', '2026-10-21',
    '2026-10-22', '2026-10-23', '2026-10-26', '2026-10-27', '2026-10-28'];

const SERIES = {
    'ecb|EUR/USD': [1.1605, 1.1609, 1.1612, 1.1614, 1.1618, 1.1620, 1.1622, 1.1626, 1.1631, 1.1638],
    'imf|EUR/USD': [1.1598, 1.1601, 1.1606, 1.1607, 1.1611, 1.1614, 1.1616, 1.1620, 1.1624, 1.1630],
    'fred|EUR/USD': [1.1610, 1.1613, 1.1616, 1.1619, 1.1621, 1.1623, 1.1626, 1.1629, 1.1633, 1.1640],
    'ecb|EUR/CAD': [1.5590, 1.5601, 1.5610, 1.5614, 1.5618, 1.5620, 1.5622, 1.5626, 1.5631, 1.5638],
    'boc|CAD/EUR': [0.6410, 0.6408, 0.6406, 0.6404, 0.6402, 0.6400, 0.6398, 0.6396, 0.6394, 0.6392],
    'ecb|EUR/GBP': [0.8620, 0.8624, 0.8627, 0.8630, 0.8632, 0.8634, 0.8637, 0.8640, 0.8643, 0.8646],
    'fred|EUR/GBP': [0.8618, 0.8621, 0.8625, 0.8628, 0.8630, 0.8633, 0.8635, 0.8638, 0.8641, 0.8644],
    'imf|USD/CHF': [0.8290, 0.8294, 0.8297, 0.8300, 0.8303, 0.8306, 0.8309, 0.8312, 0.8316, 0.8321],
    'fred|USD/JPY': [0.8325, 0.8330, 0.8334, 0.8338, 0.8341, 0.8345, 0.8348, 0.8352, 0.8356, 0.8360],
    'boe|GBP/USD': [1.3348, 1.3351, 1.3354, 1.3357, 1.3360, 1.3362, 1.3364, 1.3367, 1.3370, 1.3373],
    'tcmb|TRY/USD': [42.10, 42.18, 42.25, 42.31, 42.38, 42.44, 42.50, 42.57, 42.63, 42.70],
};

const MONTHS = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct'];
const COVERAGE = {
    'EUR/USD': {
        ecb: { first: '2026-02-03', last: '2026-10-28', count: 187, gaps: 2, held: [0, 20, 22, 21, 20, 22, 23, 21, 22, 16] },
        imf: { first: '2026-01-06', last: '2026-10-28', count: 203, gaps: 0, held: [21, 20, 22, 21, 20, 22, 23, 21, 22, 11] },
        fred: { first: '2026-03-02', last: '2026-10-28', count: 168, gaps: 5, held: [0, 0, 22, 21, 20, 22, 21, 21, 22, 19] },
    },
};

const SENTENCES = {
    live: 'Serving. The last fixing is today\'s and it arrived on time.',
    late: 'Late. The publication day has passed with no fixing. Read the adapter\'s heartbeat, then ask for a fetch of the missing days.',
    down: 'Not serving. The adapter\'s heartbeat stopped, so nothing is fetched. Restart it, or find out whether its terms were withdrawn.',
    never: 'Never fixed. The terms are accepted and the adapter serves, but no value of this publisher is stored yet.',
    paused: 'Paused by a person. The publisher is not fetched. Its stored fixings and every feed that started from one stay.',
    terms: 'Terms not accepted. The publisher is not fetched until a person accepts the version shown.',
};
const STATE_BADGE = { live: ['ok', 'serving'], late: ['warn', 'late'], down: ['bad', 'not serving'],
    never: ['never', 'never fixed'], paused: ['off', 'paused'], terms: ['warn', 'not accepted'] };

const fn = {
    esc: (t) => String(t).replace(/[&<>"]/g, (c) => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;' }[c])),
    href: (changes) => {
        const next = new URLSearchParams(q);
        for (const [k, v] of Object.entries(changes)) { if (v === null) next.delete(k); else next.set(k, v); }
        return `?${next.toString()}`;
    },
    badge: (kind) => `<span class="badge ${STATE_BADGE[kind][0]}"><i class="dot"></i>${STATE_BADGE[kind][1]}</span>`,
    status: (p) => {
        if (!p.terms.accepted) return 'terms';
        if ((p.code === 'boe' && pausedVariant !== 'resumed') || (Boolean(p.paused) && pausedVariant !== 'resumed')) return 'paused';
        if (p.adapter.state !== 'serving') return 'down';
        if (!p.last) return 'never';
        return p.last.onTime ? 'live' : 'late';
    },
    series: (code, pair) => SERIES[`${code}|${pair}`] ?? null,
    providersOf: (pair) => PROVIDERS.filter((p) => p.pairs.includes(pair)),
    pairs: () => [...new Set(PROVIDERS.flatMap((p) => p.pairs))].sort(),
};

function termsCell(p) {
    if (!p.terms.accepted) {
        return withdrawn && p.code === 'imf'
            ? '<span class="badge bad">v2 withdraws storage</span>'
            : `<span class="badge warn">not accepted · ${p.terms.version}</span>`;
    }
    return `<span class="badge ok">accepted ${p.terms.version}</span> <span class="faint">${fn.esc(p.terms.who)}</span>`;
}

function row(p) {
    const kind = fn.status(p);
    return `<tr class="${pausedVariant === 'muted' && kind === 'paused' ? 'quiet' : ''}">
        <td class="provider">${fn.esc(p.name)}<div class="faint">${fn.esc(p.pairs.join(', '))}</div></td>
        <td>${termsCell(p)}</td>
        <td>${p.last ? `<div class="fresh"><span class="when">${p.last.date}</span><span class="sub">${fn.esc(p.last.pair)}</span></div>`
            : '<div class="fresh"><span class="when faint">—</span><span class="sub">no fixing ever</span></div>'}</td>
        <td class="when">${p.last ? fn.esc(p.last.received) : '—'}</td>
        <td class="when">${p.last ? fn.esc(p.last.expected) : '—'}</td>
        <td>${fn.badge(kind)}</td>
        <td>${p.adapter.state === 'serving'
            ? `<span class="faint mono">hb ${fn.esc(p.adapter.hb)}</span>`
            : `<span class="badge bad">hb ${fn.esc(p.adapter.hb)}</span>`}</td>
        <td class="acts">
          <a class="act" href="${fn.href({ tab: 'publisher', provider: p.code })}">Open</a>
          ${kind === 'terms' ? '<a class="act primary" href="#">Review terms</a>'
            : kind === 'paused' ? `<a class="act" href="${fn.href({ paused: 'resumed' })}">Resume</a>`
            : '<a class="act" href="#">Pause <span class="faint">· design pending</span></a>'}
        </td>
    </tr>`;
}

function renderProviders() {
    const kinds = ['live', 'late', 'down', 'never', 'paused', 'terms'];
    document.querySelector('#filters').innerHTML = ['all', ...kinds].map((k) =>
        `<a class="act" href="${fn.href({ tab: 'providers', filter: k === 'all' ? null : k })}">${k === 'all' ? 'All' : STATE_BADGE[k][1]}</a>`).join('');

    const counts = Object.fromEntries(kinds.map((k) => [k, 0]));
    PROVIDERS.forEach((p) => counts[fn.status(p)]++);
    document.querySelector('#k-total').textContent = PROVIDERS.length;
    document.querySelector('#k-serving').textContent = counts.live;
    document.querySelector('#k-late').textContent = counts.late;
    document.querySelector('#k-down').textContent = counts.down;
    document.querySelector('#k-paused').textContent = counts.paused;
    document.querySelector('#k-items').textContent = fn.pairs().length;

    let shown = PROVIDERS.filter((p) => filter === 'all' || fn.status(p) === filter);
    if (search) shown = shown.filter((p) => (p.name + p.pairs.join(' ')).toLowerCase().includes(search));
    if (pausedVariant === 'hidden') shown = shown.filter((p) => fn.status(p) !== 'paused');

    document.querySelector('#providers tbody').innerHTML = shown.map(row).join('');
    document.querySelector('#providers-sub').textContent =
        `${PROVIDERS.length} providers · ${counts.paused} paused · the expected rhythm is mock data`;

    const withdraw = document.querySelector('#withdraw');
    withdraw.hidden = !withdrawn;
    withdraw.innerHTML = withdrawn ? `<div class="notice">
        <div class="title">The IMF's terms v2 withdraw permission to store</div>
        <div>Accepting it is the moment the fixings go. Deleting them removes 1,204 values and leaves 2 feeds
        that started from one saying where they started, from a value that is gone.</div>
        <div class="foot" style="margin-top:8px"><button class="act">Decline v2 and keep the values</button>
        <button class="act primary">Accept v2 and delete 1,204 fixings</button></div></div>` : '';

    document.querySelector('#legend').innerHTML = kinds.map((kind) =>
        `<div>${fn.badge(kind)}<span>${SENTENCES[kind]}</span></div>`).join('') +
        '<div class="faint">An adapter is never paused. A row\'s badge is the row\'s state, read from the terms, the adapter and the fixings together.</div>';
}

function chartFor(seriesByProvider) {
    const w = 860, h = 220, pad = 34;
    const all = seriesByProvider.flatMap((s) => s.values);
    const min = Math.min(...all), max = Math.max(...all);
    const x = (i) => pad + (i * (w - 2 * pad)) / Math.max(DATES.length - 1, 1);
    const y = (v) => h - pad - ((v - min) / Math.max(max - min, 1e-9)) * (h - 2 * pad);
    const lines = seriesByProvider.map((s, k) =>
        `<polyline class="line s${k % 3}" points="${s.values.map((v, i) => `${x(i).toFixed(1)},${y(v).toFixed(1)}`).join(' ')}"></polyline>`).join('');
    const labels = DATES.map((d, i) => (i % 3 === 0 || i === DATES.length - 1)
        ? `<text x="${x(i)}" y="${h - 8}">${d.slice(5)}</text>` : '').join('');
    return `<svg class="chart" viewBox="0 0 ${w} ${h}" preserveAspectRatio="none" role="img" aria-label="The fixings over the window">
        <line class="axis" x1="${pad}" y1="${h - pad}" x2="${w - pad}" y2="${h - pad}"></line>${lines}${labels}</svg>`;
}

function renderPublisher() {
    const provider = PROVIDERS.find((p) => p.code === selectedProvider) ?? PROVIDERS[0];
    const pair = provider.pairs.includes(selectedPair) ? selectedPair : provider.pairs[0];
    document.querySelector('#pub-provider').innerHTML = PROVIDERS.map((p) =>
        `<option value="${p.code}" ${p.code === provider.code ? 'selected' : ''}>${fn.esc(p.name)}</option>`).join('');
    document.querySelector('#pub-pair').innerHTML = provider.pairs.map((x) =>
        `<option ${x === pair ? 'selected' : ''}>${x}</option>`).join('');
    document.querySelector('#pub-title').textContent = `${provider.name} — ${pair}`;
    const values = fn.series(provider.code, pair);
    document.querySelector('#pub-sub').textContent = values
        ? `${provider.kind} · ${values.length} fixings in the window`
        : 'no fixing stored for this pair';

    if (!values) {
        document.querySelector('#pub-body').innerHTML = `<div class="notice"><div class="title">No fixing stored</div>
            <div>${fn.status(provider) === 'paused'
                ? 'The publisher is paused, so nothing new arrives. Values it stored before the pause stay.'
                : 'The terms are accepted and the adapter serves, but no value of this pair is stored yet. Ask for a fetch, or read the heartbeat.'}</div></div>`;
        document.querySelector('#pub-start').hidden = true;
        return;
    }
    const rows = DATES.map((date, i) => `<tr><td class="when">${date}</td><td class="num">${values[i].toFixed(4)}</td>
        <td class="mono faint">oresmd://fx/${pair.split('/')[0]}?type=fixing&amp;index=fx&amp;source=${provider.code.toUpperCase()}&amp;ccy=${pair.split('/')[1]}</td>
        <td class="faint">${provider.last?.received ?? ''}</td></tr>`).reverse().join('');
    document.querySelector('#pub-body').innerHTML = chartFor([{ values }]) +
        `<table class="list" style="margin-top:12px"><thead><tr><th>Date</th><th>Value</th><th>Item</th><th>Received</th></tr></thead><tbody>${rows}</tbody></table>
         <div class="faint" style="margin-top:8px">A date with no value is a gap; a value on a stored date with a new version is a correction. Both are read from the same series.</div>`;
    const start = document.querySelector('#pub-start');
    start.hidden = false;
    start.innerHTML = `<div class="card-head"><h2>Where a feed started</h2><p>the read the feed page already answers</p></div>
        <div class="card-body"><div class="notice"><div class="title">Started from the ${provider.code.toUpperCase()} fixing of ${values[values.length - 1].toFixed(4)} on ${DATES[DATES.length - 1]}</div>
        <div class="uri">oresmd://fx/${pair.split('/')[0]}?type=fixing&amp;index=fx&amp;source=${provider.code.toUpperCase()}&amp;ccy=${pair.split('/')[1]}</div>
        <div class="faint">${values.length} fixings over the window measured its spread · <a href="#">the feed that started here</a></div></div></div>`;
}

function renderAvailability() {
    if (!fn.pairs().includes(selectedPair)) selectedPair = fn.pairs()[0];
    document.querySelector('#av-pair').innerHTML = fn.pairs().map((pair) => `<option ${pair === selectedPair ? 'selected' : ''}>${pair}</option>`).join('');
    document.querySelector('#av-pair-label').textContent = selectedPair;
    const publishers = fn.providersOf(selectedPair);
    const coverage = COVERAGE[selectedPair];
    const rows = publishers.map((p) => {
        const c = coverage?.[p.code];
        const kind = fn.status(p);
        const cells = c
            ? c.held.map((n, i) => `<td class="cell ${n === 0 ? 'none' : (c.gaps && i === 3 ? 'gap' : 'ok')}">${n || '—'}</td>`).join('')
            : MONTHS.map(() => '<td class="cell none">—</td>').join('');
        return `<tr><td class="provider">${fn.esc(p.name)}<div class="faint">${fn.badge(kind)}</div></td>
            <td class="when">${c?.first ?? '—'}</td><td class="when">${c?.last ?? '—'}</td>
            <td class="num">${c?.count ?? 0}</td><td class="num">${c?.gaps ?? '—'}</td>${cells}</tr>`;
    }).join('');
    document.querySelector('#av-body').innerHTML = `<table class="matrix"><thead><tr><th>Publisher</th><th>First</th><th>Last</th><th>Values</th><th>Gaps</th>
        ${MONTHS.map((m) => `<th>${m}</th>`).join('')}</tr></thead><tbody>${rows}</tbody></table>
        <div class="faint" style="margin-top:10px">Each month cell is the number of fixings held. A dash is a month with none. A publisher whose terms are not accepted is a row with no data and the reason. Coverage for a pair with no monthly mock falls back to empty cells.</div>`;
}

function renderCompare() {
    const pairs = fn.pairs().filter((pair) => fn.providersOf(pair).filter((p) => fn.series(p.code, pair)).length > 1);
    if (!pairs.includes(selectedPair)) selectedPair = pairs[0];
    document.querySelector('#cmp-pair').innerHTML = pairs.map((pair) => `<option ${pair === selectedPair ? 'selected' : ''}>${pair}</option>`).join('');
    const publishers = fn.providersOf(selectedPair).filter((p) => fn.series(p.code, selectedPair));
    const seriesByProvider = publishers.map((p) => ({ code: p.code, values: fn.series(p.code, selectedPair) }));
    const base = seriesByProvider[0];
    const bps = (a, b) => ((a - b) / b) * 10000;
    const rows = DATES.map((date, i) => {
        const values = seriesByProvider.map((s) => s.values[i]);
        const diffs = values.map((v) => bps(v, base.values[i]));
        const spread = Math.max(...values) - Math.min(...values);
        return `<tr><td class="when">${date}</td>${values.map((v, k) => `<td class="num">${v.toFixed(4)} <span class="faint">${k === 0 ? 'base' : `${diffs[k] >= 0 ? '+' : ''}${diffs[k].toFixed(1)} bps`}</span></td>`).join('')}
            <td class="num">${spread.toFixed(4)}</td><td class="num">${bps(Math.max(...values), Math.min(...values)).toFixed(1)}</td></tr>`;
    }).join('');
    const lastDiffs = seriesByProvider.map((s) => bps(s.values[s.values.length - 1], base.values[base.values.length - 1]));
    const off = lastDiffs.map(Math.abs).indexOf(Math.max(...lastDiffs.map(Math.abs)));
    document.querySelector('#cmp-sub').textContent = `${publishers.length} publishers · ${DATES.length} common dates · the base is ${publishers[0].code.toUpperCase()}`;
    document.querySelector('#cmp-body').innerHTML = chartFor(seriesByProvider) +
        `<div class="legend-line">${publishers.map((p, k) => `<span><i style="background:${['var(--accent)', 'var(--warn)', 'var(--up)'][k % 3]}"></i>${fn.esc(p.name)}</span>`).join('')}</div>
        <table class="list" style="margin-top:12px"><thead><tr><th>Date</th>${publishers.map((p) => `<th>${fn.esc(p.code.toUpperCase())}</th>`).join('')}<th>Spread</th><th>Spread bps</th></tr></thead><tbody>${rows}</tbody></table>
        <div class="notice" style="margin-top:12px"><div class="title">${publishers[off].name} is furthest from the rest on ${DATES[DATES.length - 1]}</div>
        <div>${lastDiffs[off] >= 0 ? '+' : ''}${lastDiffs[off].toFixed(1)} basis points against the base. A person checks the publisher's own site before using its value.</div></div>`;
}

function renderFetch() {
    const box = document.querySelector('#fetch-body');
    if (fetchState === 'progress') {
        box.innerHTML = `<div class="dialog"><h2>Fetch in progress</h2>
            <div class="fields"><span class="mono">every provider · every pair · 2026-10-01 → 2026-10-28</span></div>
            <div class="progress" role="progressbar" aria-valuenow="218" aria-valuemin="0" aria-valuemax="300"><i style="width:72.6%"></i></div>
            <div class="faint">218 of 300 days fetched, oldest first · a day already held is skipped</div>
            <div class="foot"><a class="act" href="${fn.href({ tab: 'fetch', fetch: null })}">Stop</a></div></div>`;
        return;
    }
    if (fetchState === 'done') {
        box.innerHTML = `<div class="dialog"><h2>Fetch done</h2>
            <div>300 days asked for across 6 providers and 12 pairs: <b>1,204 stored</b>, 96 already held. The store skipped a value it had.</div>
            <div class="faint">A feed that started from a publisher is untouched: its start is the run's, not the store's.</div>
            <div class="foot"><a class="act" href="${fn.href({ tab: 'fetch', fetch: null })}">Close</a></div></div>`;
        return;
    }
    box.innerHTML = `<div class="dialog">
        <h2>The scope of a fetch</h2>
        <div class="fields">
          <label>When<select><option>Today</option><option>Yesterday</option><option selected>Range</option></select></label>
          <label>From<input value="2026-10-01"></label>
          <label>To<input value="2026-10-28"></label>
          <label>Publishers<select><option>Every publisher</option><option>One publisher</option></select></label>
          <label>Underlyings<select><option>Every pair</option><option>One pair</option></select></label>
        </div>
        <div class="faint">This asks 6 providers for 12 pairs over 28 days: about 2,016 values. Only an accepted publisher is asked.</div>
        <div class="foot"><a class="act primary" href="${fn.href({ tab: 'fetch', fetch: 'progress' })}">Fetch</a></div>
        <div class="faint">The refresh request is not on the adapter contract yet; the design task names it. Until then this button cannot exist, and the screen offers only the window the service fetches itself.</div>
      </div>`;
}

function renderConfig() {
    const provider = PROVIDERS.find((p) => p.code === selectedProvider) ?? PROVIDERS[0];
    document.querySelector('#cfg-provider').innerHTML = PROVIDERS.map((p) =>
        `<option value="${p.code}" ${p.code === provider.code ? 'selected' : ''}>${fn.esc(p.name)}</option>`).join('');
    document.querySelector('#cfg-title').textContent = `${provider.name} — configuration`;
    const accepted = provider.terms.accepted;
    document.querySelector('#cfg-body').innerHTML = `<div class="kv">
        <span class="k">Code</span><span class="mono">${provider.code}</span>
        <span class="k">Name</span><span>${fn.esc(provider.name)}</span>
        <span class="k">Kind</span><span>${provider.kind}</span>
        <span class="k">Terms version</span><span>${provider.terms.version} ${accepted ? fn.badge('live') : fn.badge('terms')}</span>
        <span class="k">Terms credit</span><span>${fn.esc(provider.credit)}</span>
        <span class="k">Allows storage</span><span>${provider.storage ? 'yes' : 'no'}</span>
        <span class="k">Credential</span><span class="mono">${fn.esc(provider.credential)}</span>
        <span class="k">Expected rhythm</span><span>${fn.esc(provider.schedule)} <span class="badge warn">not modelled yet</span></span>
        <span class="k">Accepted by</span><span>${accepted ? `${fn.esc(provider.terms.who)} · ${provider.terms.at}` : 'nobody'}</span>
        <span class="k">Items</span><span>${provider.pairs.map((x) => `<span class="badge">${x}</span>`).join(' ')}</span>
      </div>`;
    const events = [
        ['v1', provider.terms.at, 'Terms accepted', provider.terms.who],
        ['v2', '2026-10-26 11:20', 'Terms accepted', 'super_admin'],
        ['v3', provider.code === 'ecb' ? '2026-10-28 09:00' : '2026-10-27 15:10', 'Terms accepted', 'super_admin'],
        ['—', '2026-10-28 09:12', 'Provider changed: credit updated', 'super_admin'],
    ];
    document.querySelector('#cfg-history').innerHTML = `<div class="timeline">${events.map(([v, at, what, who]) =>
        `<div class="row"><span class="at">${at}</span><span class="what"><b>${what}</b> · ${v} · by ${who}</span></div>`).join('')}</div>
        <div class="faint" style="margin-top:10px">The standard history tab: every version and every acceptance, with the person and the time.</div>`;
}

function wire() {
    document.querySelectorAll('[data-q]').forEach((b) => {
        b.setAttribute('aria-checked', String(b.dataset.v === pausedVariant));
        b.addEventListener('click', () => { location.search = fn.href({ [b.dataset.q]: b.dataset.v }); });
    });
    document.querySelectorAll('[data-tab]').forEach((b) => {
        b.setAttribute('aria-selected', String(b.dataset.tab === tab));
        b.addEventListener('click', () => { location.search = fn.href({ tab: b.dataset.tab }); });
    });
    document.querySelector('#pub-provider').addEventListener('change', (e) => {
        location.search = fn.href({ tab: 'publisher', provider: e.target.value, pair: null });
    });
    document.querySelector('#pub-pair').addEventListener('change', (e) => {
        location.search = fn.href({ tab: 'publisher', pair: e.target.value });
    });
    document.querySelector('#av-pair').addEventListener('change', (e) => {
        location.search = fn.href({ tab: 'availability', pair: e.target.value });
    });
    document.querySelector('#cmp-pair').addEventListener('change', (e) => {
        location.search = fn.href({ tab: 'compare', pair: e.target.value });
    });
    document.querySelector('#cfg-provider').addEventListener('change', (e) => {
        location.search = fn.href({ tab: 'config', provider: e.target.value });
    });
    document.querySelector('#search').addEventListener('change', (e) => {
        location.search = fn.href({ tab: 'providers', find: e.target.value || null });
    });
    document.querySelector('#search').value = q.get('find') ?? '';
    document.querySelector('#refresh').addEventListener('click', () => location.reload());
    document.querySelectorAll('[role="tabpanel"]').forEach((s) => {
        s.hidden = s.id !== `tab-${tab}`;
    });
}

renderProviders();
renderPublisher();
renderAvailability();
renderCompare();
renderFetch();
renderConfig();
wire();
