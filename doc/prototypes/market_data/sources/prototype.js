/*
 * Static prototype of Market data sources: the providers, the sources that are
 * adapter instances of them, the route that picks one source for a party, and
 * the adapters the service tracks. Mock data only; no request leaves the page and
 * every endpoint and value is invented.
 *
 *   ?tab=sources|adapters
 *   ?source=<name>&stab=overview|adapter|routes|history
 */

const q = new URLSearchParams(location.search);
const tab = q.get('tab') ?? 'sources';
const sourceName = q.get('source');
const stab = q.get('stab') ?? 'overview';
const search = (q.get('find') ?? '').toLowerCase();

const PROVIDERS = [
    { code: 'ecb', name: 'European Central Bank', kind: 'reference', credential: 'none', classes: ['fx', 'ir'] },
    { code: 'imf', name: 'International Monetary Fund', kind: 'reference', credential: 'none', classes: ['fx'] },
    { code: 'boc', name: 'Bank of Canada', kind: 'reference', credential: 'none', classes: ['fx'] },
    { code: 'tcmb', name: 'Central Bank of Turkiye', kind: 'reference', credential: 'none', classes: ['fx'] },
    { code: 'norges', name: 'Norges Bank', kind: 'reference', credential: 'none', classes: ['fx'] },
    { code: 'boe', name: 'Bank of England', kind: 'reference', credential: 'none', classes: ['fx', 'ir'] },
    { code: 'fred', name: 'Federal Reserve (H.10)', kind: 'reference', credential: 'vault:marketdata/fred', classes: ['fx', 'ir'] },
    { code: 'ust', name: 'US Treasury', kind: 'reference', credential: 'none', classes: ['ir'] },
    { code: 'bloomberg', name: 'Bloomberg', kind: 'data_vendor', credential: 'vault:marketdata/bloomberg/prod', classes: ['fx', 'ir'] },
    { code: 'lseg', name: 'LSEG', kind: 'data_vendor', credential: 'vault:marketdata/lseg', classes: ['fx', 'ir'] },
];

const SOURCES = [
    { name: 'ecb-frankfurter', provider: 'ecb', kind: 'live', isDefault: true, classes: ['fx'],
      description: 'The ECB reference rates read through Frankfurter by the official adapter.',
      endpoint: 'https://api.frankfurter.app', path: '/v2/providers/ECB/rates', poll: '60 min', backfill: '400 days',
      state: 'serving', last: '19:24', hb: '19:24', served: 44, refused: 0, unknown: 0, lastTick: '19:24',
      parties: ['System Party'] },
    { name: 'ecb-ecb-data', provider: 'ecb', kind: 'live', isDefault: false, classes: ['ir'],
      description: 'The ECB Data Portal series: €STR and the EUR AAA spot curve.',
      endpoint: 'https://data-api.ecb.europa.eu', path: '/service/data/ESTR', poll: '60 min', backfill: '400 days',
      state: 'serving', last: '17:02', hb: '17:02', served: 12, refused: 0, unknown: 0, lastTick: '17:02',
      parties: [] },
    { name: 'imf-frankfurter', provider: 'imf', kind: 'live', isDefault: true, classes: ['fx'],
      description: 'The IMF representative rates read through Frankfurter by the official adapter.',
      endpoint: 'https://api.frankfurter.app', path: '/v2/providers/IMF/rates', poll: '60 min', backfill: '400 days',
      state: 'serving', last: '19:24', hb: '19:24', served: 48, refused: 0, unknown: 0, lastTick: '19:24',
      parties: ['System Party'] },
    { name: 'boc-frankfurter', provider: 'boc', kind: 'live', isDefault: true, classes: ['fx'],
      description: 'The Bank of Canada rates read through Frankfurter by the official adapter.',
      endpoint: 'https://api.frankfurter.app', path: '/v2/providers/BOC/rates', poll: '60 min', backfill: '400 days',
      state: 'serving', last: '19:20', hb: '19:24', served: 27, refused: 0, unknown: 0, lastTick: '19:20',
      parties: ['System Party'] },
    { name: 'tcmb-frankfurter', provider: 'tcmb', kind: 'live', isDefault: true, classes: ['fx'],
      description: 'The Central Bank of Turkiye rates read through Frankfurter by the official adapter.',
      endpoint: 'https://api.frankfurter.app', path: '/v2/providers/TCMB/rates', poll: '60 min', backfill: '400 days',
      state: 'not serving', last: '2026-10-27', hb: '16:02', served: 21, refused: 3, unknown: 0, lastTick: '2026-10-27',
      parties: ['System Party'] },
    { name: 'norges-frankfurter', provider: 'norges', kind: 'live', isDefault: true, classes: ['fx'],
      description: 'The Norges Bank rates read through Frankfurter by the official adapter.',
      endpoint: 'https://api.frankfurter.app', path: '/v2/providers/NB/rates', poll: '60 min', backfill: '400 days',
      state: 'serving', last: null, hb: '19:24', served: 12, refused: 0, unknown: 0, lastTick: null,
      parties: ['System Party'] },
    { name: 'boe-frankfurter', provider: 'boe', kind: 'live', isDefault: true, classes: ['fx'],
      description: 'The Bank of England rates read through Frankfurter by the official adapter.',
      endpoint: 'https://api.frankfurter.app', path: '/v2/providers/BOE/rates', poll: '60 min', backfill: '400 days',
      state: 'serving', last: '15:00', hb: '19:24', served: 34, refused: 0, unknown: 0, lastTick: '15:00',
      parties: ['System Party'] },
    { name: 'fred-frankfurter', provider: 'fred', kind: 'live', isDefault: true, classes: ['fx'],
      description: 'The Federal Reserve H.10 rates read through Frankfurter by the official adapter.',
      endpoint: 'https://api.frankfurter.app', path: '/v2/providers/FED/rates', poll: '60 min', backfill: '400 days',
      state: 'serving', last: '19:22', hb: '19:24', served: 31, refused: 0, unknown: 0, lastTick: '19:22',
      parties: ['System Party'] },
    { name: 'fred-api', provider: 'fred', kind: 'live', isDefault: false, classes: ['ir'],
      description: 'The FRED series: SOFR and the US Treasury par curve.',
      endpoint: 'https://api.stlouisfed.org', path: '/fred/series/observations', poll: '60 min', backfill: '400 days',
      state: 'serving', last: '19:22', hb: '19:22', served: 9, refused: 0, unknown: 0, lastTick: '19:22',
      parties: [] },
    { name: 'boe-boe', provider: 'boe', kind: 'live', isDefault: false, classes: ['ir'],
      description: 'The Bank of England statistics: SONIA and the gilt curve.',
      endpoint: 'https://www.bankofengland.co.uk/boeapps/database', path: '/series', poll: '60 min', backfill: '400 days',
      state: 'serving', last: '15:00', hb: '15:00', served: 6, refused: 0, unknown: 0, lastTick: '15:00',
      parties: [] },
    { name: 'ust-treasury', provider: 'ust', kind: 'live', isDefault: true, classes: ['ir'],
      description: 'The US Treasury par yield curve, read from the Treasury data feed.',
      endpoint: 'https://home.treasury.gov/resource-center/data-chart-center', path: '/interest-rates', poll: '60 min',
      backfill: '400 days', state: 'serving', last: '18:05', hb: '18:05', served: 12, refused: 0, unknown: 0,
      lastTick: '18:05', parties: ['System Party'] },
    { name: 'bloomberg-a', provider: 'bloomberg', kind: 'live', isDefault: true, classes: ['fx', 'ir'],
      description: 'The Bloomberg production session.',
      endpoint: 'https://api.bloomberg.com', path: 'session', poll: 'not polled', backfill: '400 days',
      state: 'serving', last: '19:23', hb: '19:23', served: 120, refused: 0, unknown: 1, lastTick: '19:23',
      parties: ['System Party'] },
    { name: 'bloomberg-b', provider: 'bloomberg', kind: 'test', isDefault: false, classes: ['fx', 'ir'],
      description: 'The Bloomberg test session, for a new mapping or a new field.',
      endpoint: 'https://api.test.bloomberg.com', path: 'session', poll: 'not polled', backfill: '30 days',
      state: 'not serving', last: '—', hb: '—', served: 0, refused: 0, unknown: 0, lastTick: '—',
      parties: [] },
    { name: 'lseg-simulated', provider: 'lseg', kind: 'simulated', isDefault: true, classes: ['fx', 'ir'],
      description: 'The simulated LSEG feed the synthetic runner drives.',
      endpoint: 'none', path: 'none', poll: 'not polled', backfill: 'not applicable',
      state: 'serving', last: 'now', hb: 'now', served: 64, refused: 0, unknown: 0, lastTick: 'now',
      parties: ['System Party'] },
];

const TONE = { serving: ['ok', 'serving'], 'not serving': ['bad', 'not serving'] };
const KIND_TONE = { live: 'ok', test: 'warn', simulated: 'off', replay: 'off', derived: 'off' };

const fn = {
    esc: (t) => String(t).replace(/[&<>"]/g, (c) => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;' }[c])),
    href: (changes) => {
        const next = new URLSearchParams(q);
        for (const [k, v] of Object.entries(changes)) { if (v === null) next.delete(k); else next.set(k, v); }
        return `?${next.toString()}`;
    },
    state: (s) => `<span class="badge ${s.state === 'serving' ? 'ok' : 'bad'}"><i class="dot"></i>${s.state}</span>`,
    provider: (code) => PROVIDERS.find((p) => p.code === code),
    source: (name) => SOURCES.find((s) => s.name === name),
};

function hidePanels(except) {
    document.querySelectorAll('[role="tabpanel"]').forEach((x) => { x.hidden = x.id !== except; });
    document.querySelector('#source-record').hidden = true;
}

function renderSources() {
    hidePanels('tab-sources');
    document.querySelectorAll('[data-tab]').forEach((b) => b.setAttribute('aria-selected', String(b.dataset.tab === 'sources')));
    const shown = SOURCES.filter((s) => {
        const p = fn.provider(s.provider);
        return !search || s.name.includes(search) || p.code.includes(search) || p.name.toLowerCase().includes(search);
    });
    document.querySelector('#sources tbody').innerHTML = shown.map((s) => {
        const p = fn.provider(s.provider);
        return `<tr>
          <td class="provider">${s.name}${s.isDefault ? ' <span class="badge">default</span>' : ''}</td>
          <td>${fn.esc(p.name)}<div class="faint mono">${p.code} · ${p.kind}</div></td>
          <td><span class="badge ${KIND_TONE[s.kind] ?? 'off'}">${s.kind}</span></td>
          <td>${s.isDefault ? 'yes' : '<span class="faint">no</span>'}</td>
          <td>${s.classes.map((c) => `<span class="badge">${c.toUpperCase()}</span>`).join(' ')}</td>
          <td>${fn.state(s)}</td>
          <td class="when">${s.last ?? '<span class="faint">never</span>'}</td>
          <td class="acts"><a class="act" href="${fn.href({ tab: null, source: s.name, stab: 'overview' })}">Open</a></td>
        </tr>`;
    }).join('');
    document.querySelector('#sources-note').textContent =
        'A provider is whoever owns the ids and the terms: a central bank, a treasury, or a data vendor. A source is one adapter instance of a provider: ecb-frankfurter and bloomberg-a are two sources, and a provider may have several. One source of each provider is the default, and a new subscription routes to it. A party routes exactly one source for each provider.';
}

function renderAdapters() {
    hidePanels('tab-adapters');
    document.querySelectorAll('[data-tab]').forEach((b) => b.setAttribute('aria-selected', String(b.dataset.tab === 'adapters')));
    document.querySelector('#adapters tbody').innerHTML = SOURCES.map((s) => `<tr>
        <td class="provider">${s.name}<div class="faint">adapter id</div></td>
        <td>${s.name}</td>
        <td>${fn.state(s)}</td>
        <td class="when">${s.hb}</td>
        <td class="num">${s.served}</td>
        <td class="num">${s.refused}</td>
        <td class="num">${s.unknown}</td>
        <td class="when">${s.lastTick ?? '<span class="faint">never</span>'}</td>
      </tr>`).join('');
    document.querySelector('#adapters-note').textContent =
        'The adapter id equals the source name. The service tracks each adapter by its id: state, heartbeat, the ids it served and refused, the ids it does not know, and the age of its last tick. An adapter knows no tenant and no party.';
}

function renderSourceRecord() {
    const s = fn.source(sourceName);
    if (!s) { location.search = fn.href({ tab: 'sources', source: null, stab: null }); return; }
    const p = fn.provider(s.provider);
    hidePanels(null);
    document.querySelector('#source-record').hidden = false;
    document.querySelector('#src-crumb').textContent = s.name;
    document.querySelector('#src-title').textContent = s.name;
    document.querySelector('#src-meta').innerHTML = `<span class="mono faint">${s.provider}</span> · ${s.kind}${s.isDefault ? ' · default' : ''} · ${fn.state(s)}`;
    document.querySelector('#src-acts').innerHTML = `<a class="act" href="#">Edit <span class="faint">· design pending</span></a>
        <a class="act" href="#">${s.state === 'serving' ? 'Stop' : 'Start'}</a>`;
    document.querySelectorAll('[data-stab]').forEach((b) => b.setAttribute('aria-selected', String(b.dataset.stab === stab)));
    const body = document.querySelector('#src-body');
    const detail = (rows) => `<div class="kv">${rows.map(([k, v]) => `<span class="k">${k}</span><span>${v}</span>`).join('')}</div>`;

    if (stab === 'overview') {
        body.innerHTML = `<div class="card"><div class="card-head"><h2>Overview</h2><p>the source record</p></div><div class="card-body">${detail([
            ['Name', `<span class="mono">${s.name}</span>`],
            ['Provider', `${fn.esc(p.name)} <span class="faint mono">${p.code} · ${p.kind}</span>`],
            ['Kind', `<span class="badge ${KIND_TONE[s.kind] ?? 'off'}">${s.kind}</span>`],
            ['Default', s.isDefault ? 'yes' : 'no'],
            ['Asset classes', s.classes.map((c) => c.toUpperCase()).join(', ')],
            ['Description', fn.esc(s.description)],
            ['Credential reference', `<span class="mono">${fn.esc(p.credential)}</span> <span class="faint">on the provider</span>`],
        ])}<div class="faint" style="margin-top:8px">The source is a row. Its name is a label and its kind is data a person can read; no code branches on the kind. The credential belongs to the provider.</div></div></div>`;
    } else if (stab === 'adapter') {
        body.innerHTML = `<div class="card"><div class="card-head"><h2>Adapter</h2><p>the runtime the service sees, for adapter id ${s.name}</p></div><div class="card-body">${detail([
            ['State', s.state],
            ['Heartbeat', `<span class="mono">${s.hb}</span>`],
            ['Ids served', String(s.served)],
            ['Ids refused', String(s.refused)],
            ['Ids unknown', String(s.unknown)],
            ['Last tick', s.lastTick ?? 'never'],
        ])}</div></div>
        <div class="card" style="margin-top:12px"><div class="card-head"><h2>Transport configuration</h2><p>what the adapter needs to run</p></div><div class="card-body">${detail([
            ['Endpoint', `<span class="mono">${fn.esc(s.endpoint)}</span>`],
            ['Path', `<span class="mono">${fn.esc(s.path)}</span>`],
            ['Poll interval', s.poll],
            ['Backfill window', s.backfill],
        ])}<div class="faint" style="margin-top:8px"><span class="badge warn">not persisted yet</span> The endpoint, poll interval and backfill window are command-line flags today. The story <em>Persist the adapter transport on the source</em> moves them onto this record.</div></div></div>`;
    } else if (stab === 'routes') {
        const others = SOURCES.filter((x) => x.provider === s.provider && x.name !== s.name);
        const parties = s.parties.length
            ? `<table class="list"><thead><tr><th>Party</th><th>Provider</th><th>Source</th><th>Since</th></tr></thead><tbody>${s.parties.map((party) =>
                `<tr><td>${party}</td><td class="mono">${s.provider}</td><td class="mono">${s.name}</td><td class="when">2026-10-28</td></tr>`).join('')}</tbody></table>`
            : '<div class="notice">No party routes this source. A provider with several sources has one of them routed, and a switch is one new route.</div>';
        body.innerHTML = `<div class="card"><div class="card-head"><h2>Routes</h2><p>the party that reads this source for ${p.code}</p></div><div class="card-body">${parties}
          <div class="faint" style="margin-top:8px">A party has at most one route for each provider, so it reads one source and one value of each item. A switch of source is one new route and nothing else.</div></div></div>
          ${others.length ? `<div class="card" style="margin-top:12px"><div class="card-head"><h2>Other sources of ${p.code}</h2><p>a party reads one of them</p></div>
          <table class="list"><thead><tr><th>Source</th><th>Kind</th><th>Classes</th><th>State</th><th class="acts"></th></tr></thead><tbody>${others.map((x) =>
            `<tr><td class="mono">${x.name}${x.isDefault ? ' <span class="badge">default</span>' : ''}</td><td>${x.kind}</td>
             <td>${x.classes.map((c) => c.toUpperCase()).join(', ')}</td><td>${fn.state(x)}</td>
             <td class="acts"><a class="act" href="${fn.href({ source: x.name, stab: 'overview', tab: null })}">Open</a></td></tr>`).join('')}</tbody></table>
          <div class="faint" style="margin-top:8px;padding:0 14px 12px">A party routes one source for each provider, so a provider whose FX and IR arrive through different sources cannot serve both to one party. The IR story settles whether a source serves every class or the route rule changes.</div></div>` : ''}`;
    } else {
        const events = [
            ['v1', '2026-10-01 09:00', 'Source created', 'super_admin'],
            ['v2', '2026-10-20 11:12', s.kind === 'test' ? 'Kind changed to test' : 'Default set for the provider', 'super_admin'],
            ['—', '2026-10-28 19:00', 'Route set to System Party', 'super_admin'],
        ];
        body.innerHTML = `<div class="card"><div class="card-head"><h2>History</h2><p>the shared timeline</p></div><div class="card-body"><div class="timeline">${events.map(([v, at, what, who]) =>
            `<div class="row"><span class="at">${at}</span><span class="what"><b>${what}</b> · ${v} · by ${who}</span></div>`).join('')}</div></div></div>`;
    }
}

function wire() {
    document.querySelectorAll('[data-tab]').forEach((b) => b.addEventListener('click', () => {
        location.search = fn.href({ tab: b.dataset.tab, source: null, stab: null });
    }));
    document.querySelectorAll('[data-stab]').forEach((b) => b.addEventListener('click', () => {
        location.search = fn.href({ source: sourceName, stab: b.dataset.stab, tab: null });
    }));
    document.querySelector('#search').addEventListener('change', (e) => {
        location.search = fn.href({ tab: 'sources', find: e.target.value || null });
    });
    document.querySelector('#search').value = q.get('find') ?? '';
    document.querySelector('#refresh').addEventListener('click', () => location.reload());
}

if (sourceName) renderSourceRecord();
else if (tab === 'adapters') renderAdapters();
else renderSources();
wire();
