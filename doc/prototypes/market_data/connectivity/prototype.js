/*
 * Static prototype of Connectivity: the configured providers, the adapters the
 * code provides, and the sources that join them. Mock data only; no request
 * leaves the page and every endpoint, credential and value is invented.
 *
 *   ?tab=providers|adapters|sources
 *   ?provider=<code>&ptab=overview|sources|history
 *   ?source=<name>&stab=overview|transport|underlyings|schedule|runtime|routes|history
 *   ?add=1&provider=<code>
 */

const q = new URLSearchParams(location.search);
const tab = q.get('tab') ?? 'providers';
const providerCode = q.get('provider');
const ptab = q.get('ptab') ?? 'overview';
const sourceName = q.get('source');
const stab = q.get('stab') ?? 'overview';
const adding = q.get('add') === '1';

const PROVIDERS = [
    { code: 'ecb', name: 'European Central Bank', kind: 'reference', classes: ['fx', 'ir'],
      terms: { accepted: true, version: 'v3', who: 'super_admin', at: '2026-01-15 10:22' }, credential: 'none' },
    { code: 'imf', name: 'International Monetary Fund', kind: 'reference', classes: ['fx'],
      terms: { accepted: true, version: 'v1', who: 'super_admin', at: '2026-09-01 09:00' }, credential: 'none' },
    { code: 'boc', name: 'Bank of Canada', kind: 'reference', classes: ['fx'],
      terms: { accepted: true, version: 'v2', who: 'super_admin', at: '2026-10-26 11:20' }, credential: 'none' },
    { code: 'tcmb', name: 'Central Bank of Turkiye', kind: 'reference', classes: ['fx'],
      terms: { accepted: true, version: 'v1', who: 'super_admin', at: '2026-10-20 08:10' }, credential: 'none' },
    { code: 'norges', name: 'Norges Bank', kind: 'reference', classes: ['fx'],
      terms: { accepted: true, version: 'v1', who: 'super_admin', at: '2026-10-18 09:00' }, credential: 'none' },
    { code: 'boe', name: 'Bank of England', kind: 'reference', classes: ['fx', 'ir'],
      terms: { accepted: true, version: 'v1', who: 'super_admin', at: '2026-10-18 14:00' }, credential: 'none' },
    { code: 'fred', name: 'Federal Reserve (H.10)', kind: 'reference', classes: ['fx', 'ir'],
      terms: { accepted: true, version: 'v1', who: 'super_admin', at: '2026-10-02 14:00' }, credential: 'vault:marketdata/fred' },
    { code: 'ust', name: 'US Treasury', kind: 'reference', classes: ['ir'],
      terms: { accepted: true, version: 'v1', who: 'super_admin', at: '2026-10-05 10:00' }, credential: 'none' },
    { code: 'bloomberg', name: 'Bloomberg', kind: 'data_vendor', classes: ['fx', 'ir'],
      terms: { accepted: true, version: 'v1', who: 'super_admin', at: '2026-10-01 09:00' }, credential: 'vault:marketdata/bloomberg/prod' },
    { code: 'lseg', name: 'LSEG', kind: 'data_vendor', classes: ['fx', 'ir'],
      terms: { accepted: true, version: 'v1', who: 'super_admin', at: '2026-10-01 09:00' }, credential: 'vault:marketdata/lseg' },
];

const ADAPTERS = [
    { code: 'frankfurter', name: 'Frankfurter', version: '1.4.0', process: 'ores.marketdata.official',
      description: "Reads a publisher's daily rates from a Frankfurter endpoint.", classes: ['fx'],
      fields: [['url', 'URL', true, 'https://api.frankfurter.app'], ['provider_key', 'Provider key', true, 'ECB'],
               ['backfill_days', 'Backfill days', false, '400'], ['timeout_ms', 'Timeout (ms)', false, '10000']] },
    { code: 'ecb-sdw', name: 'ECB Data Portal', version: '1.0.0', process: 'ores.marketdata.official',
      description: 'Reads ECB Data Portal series: rates and curve pillars.', classes: ['ir'],
      fields: [['url', 'URL', true, 'https://data-api.ecb.europa.eu'], ['series_key', 'Series key', true, 'ESTR'],
               ['backfill_days', 'Backfill days', false, '400']] },
    { code: 'fred-api', name: 'FRED', version: '2.1.0', process: 'ores.marketdata.official',
      description: 'Reads FRED series by id.', classes: ['fx', 'ir'],
      fields: [['url', 'URL', true, 'https://api.stlouisfed.org'], ['api_key_ref', 'API key reference', true, 'env:FRED_API_KEY'],
               ['series_ids', 'Series ids', true, 'SOFR, DGS10']] },
    { code: 'boe-stats', name: 'Bank of England statistics', version: '1.3.0', process: 'ores.marketdata.official',
      description: 'Reads Bank of England statistics series.', classes: ['fx', 'ir'],
      fields: [['url', 'URL', true, 'https://www.bankofengland.co.uk/boeapps/database'], ['series_codes', 'Series codes', true, 'IUDSOIA']] },
    { code: 'treasury-feed', name: 'US Treasury feed', version: '1.0.0', process: 'ores.marketdata.official',
      description: 'Reads the US Treasury par yield curve.', classes: ['ir'],
      fields: [['url', 'URL', true, 'https://home.treasury.gov/resource-center/data-chart-center'], ['curve_year', 'Curve year', true, '2026']] },
    { code: 'bloomberg-api', name: 'Bloomberg session', version: '3.2.0', process: 'ores.marketdata.official',
      description: 'Reads a Bloomberg session by ticker and field.', classes: ['fx', 'ir'],
      fields: [['endpoint', 'Endpoint', true, 'https://api.bloomberg.com'], ['api_key_ref', 'API key reference', true, 'vault:marketdata/bloomberg/prod#api_key'],
               ['api_secret_ref', 'API secret reference', true, 'vault:marketdata/bloomberg/prod#api_secret']] },
    { code: 'lseg-api', name: 'LSEG session', version: '2.8.0', process: 'ores.marketdata.official',
      description: 'Reads an LSEG session by RIC.', classes: ['fx', 'ir'],
      fields: [['endpoint', 'Endpoint', true, 'https://api.lseg.com'], ['credential_ref', 'Credential reference', true, 'vault:marketdata/lseg']] },
    { code: 'simulation', name: 'Simulator', version: '1.0.0', process: 'ores.marketdata.synthetic',
      description: 'Runs the synthetic generators the runner authors.', classes: ['fx', 'ir'], fields: [] },
];

const SOURCES = [
    { name: 'ecb-frankfurter', provider: 'ecb', adapter: 'frankfurter', kind: 'live', isDefault: true, classes: ['fx'],
      description: 'The ECB reference rates read through Frankfurter by the official adapter.',
      scope: { mode: 'all', items: ['EUR/USD', 'EUR/CAD', 'EUR/GBP'] },
      transport: { endpoint: 'https://api.frankfurter.app', path: '/v2/providers/ECB/rates',
                   settings: { backfill_days: '400', timeout_ms: '10000' } },
      credential: 'none',
      schedule: { enabled: true, rhythm: 'Daily at 16:00 CET', next: '2026-10-29 16:00', last: '2026-10-28 19:24 · ok',
                  job: 'marketdata.refresh.ecb-frankfurter' },
      runtime: { state: 'serving', hb: '19:24', served: 44, refused: 0, unknown: 0, lastTick: '19:24' },
      routes: [{ party: 'System Party', class: 'fx' }], last: '19:24' },
    { name: 'ecb-ecb-data', provider: 'ecb', adapter: 'ecb-sdw', kind: 'live', isDefault: false, classes: ['ir'],
      description: 'The ECB Data Portal series: €STR and the EUR AAA spot curve.',
      scope: { mode: 'items', items: ['€STR', 'EUR AAA spot curve'] },
      transport: { endpoint: 'https://data-api.ecb.europa.eu', path: '/service/data',
                   settings: { series_key: 'ESTR', backfill_days: '400' } },
      credential: 'none',
      schedule: { enabled: true, rhythm: 'Daily at 16:00 CET', next: '2026-10-29 16:00', last: '2026-10-28 17:02 · ok',
                  job: 'marketdata.refresh.ecb-ecb-data' },
      runtime: { state: 'serving', hb: '17:02', served: 12, refused: 0, unknown: 0, lastTick: '17:02' },
      routes: [{ party: 'System Party', class: 'ir' }], last: '17:02' },
    { name: 'imf-frankfurter', provider: 'imf', adapter: 'frankfurter', kind: 'live', isDefault: true, classes: ['fx'],
      description: 'The IMF representative rates read through Frankfurter by the official adapter.',
      scope: { mode: 'all', items: ['USD/EUR', 'USD/CHF'] },
      transport: { endpoint: 'https://api.frankfurter.app', path: '/v2/providers/IMF/rates', settings: { backfill_days: '400' } },
      credential: 'none',
      schedule: { enabled: true, rhythm: 'Daily at 16:00 CET', next: '2026-10-29 16:00', last: '2026-10-28 19:24 · ok',
                  job: 'marketdata.refresh.imf-frankfurter' },
      runtime: { state: 'serving', hb: '19:24', served: 48, refused: 0, unknown: 0, lastTick: '19:24' },
      routes: [{ party: 'System Party', class: 'fx' }], last: '19:24' },
    { name: 'boc-frankfurter', provider: 'boc', adapter: 'frankfurter', kind: 'live', isDefault: true, classes: ['fx'],
      description: 'The Bank of Canada rates read through Frankfurter by the official adapter.',
      scope: { mode: 'all', items: ['CAD/USD', 'CAD/EUR'] },
      transport: { endpoint: 'https://api.frankfurter.app', path: '/v2/providers/BOC/rates', settings: { backfill_days: '400' } },
      credential: 'none',
      schedule: { enabled: true, rhythm: 'Daily at 16:00 CET', next: '2026-10-29 16:00', last: '2026-10-28 19:20 · late',
                  job: 'marketdata.refresh.boc-frankfurter' },
      runtime: { state: 'serving', hb: '19:24', served: 27, refused: 0, unknown: 0, lastTick: '19:20' },
      routes: [{ party: 'System Party', class: 'fx' }], last: '19:20' },
    { name: 'tcmb-frankfurter', provider: 'tcmb', adapter: 'frankfurter', kind: 'live', isDefault: true, classes: ['fx'],
      description: 'The Central Bank of Turkiye rates read through Frankfurter by the official adapter.',
      scope: { mode: 'all', items: ['TRY/USD', 'TRY/EUR'] },
      transport: { endpoint: 'https://api.frankfurter.app', path: '/v2/providers/TCMB/rates', settings: { backfill_days: '400' } },
      credential: 'none',
      schedule: { enabled: true, rhythm: 'Daily at 15:30 TRT', next: '2026-10-29 15:30', last: '2026-10-27 16:02 · failed',
                  job: 'marketdata.refresh.tcmb-frankfurter' },
      runtime: { state: 'not serving', hb: '16:02', served: 21, refused: 3, unknown: 0, lastTick: '2026-10-27' },
      routes: [{ party: 'System Party', class: 'fx' }], last: '2026-10-27' },
    { name: 'norges-frankfurter', provider: 'norges', adapter: 'frankfurter', kind: 'live', isDefault: true, classes: ['fx'],
      description: 'The Norges Bank rates read through Frankfurter by the official adapter.',
      scope: { mode: 'all', items: ['NOK/EUR', 'NOK/USD'] },
      transport: { endpoint: 'https://api.frankfurter.app', path: '/v2/providers/NB/rates', settings: { backfill_days: '400' } },
      credential: 'none',
      schedule: { enabled: true, rhythm: 'Daily at 14:00 CET', next: '2026-10-29 14:00', last: '2026-10-28 19:24 · ok',
                  job: 'marketdata.refresh.norges-frankfurter' },
      runtime: { state: 'serving', hb: '19:24', served: 12, refused: 0, unknown: 0, lastTick: '2026-10-27' },
      routes: [{ party: 'System Party', class: 'fx' }], last: null },
    { name: 'boe-frankfurter', provider: 'boe', adapter: 'frankfurter', kind: 'live', isDefault: true, classes: ['fx'],
      description: 'The Bank of England rates read through Frankfurter by the official adapter.',
      scope: { mode: 'all', items: ['GBP/USD', 'GBP/EUR'] },
      transport: { endpoint: 'https://api.frankfurter.app', path: '/v2/providers/BOE/rates', settings: { backfill_days: '400' } },
      credential: 'none',
      schedule: { enabled: true, rhythm: 'Daily at 15:00 GMT', next: '2026-10-29 15:00', last: '2026-10-28 15:00 · ok',
                  job: 'marketdata.refresh.boe-frankfurter' },
      runtime: { state: 'serving', hb: '19:24', served: 34, refused: 0, unknown: 0, lastTick: '15:00' },
      routes: [{ party: 'System Party', class: 'fx' }], last: '15:00' },
    { name: 'boe-boe', provider: 'boe', adapter: 'boe-stats', kind: 'live', isDefault: false, classes: ['ir'],
      description: 'The Bank of England statistics: SONIA and the gilt curve.',
      scope: { mode: 'items', items: ['SONIA', 'GBP gilt curve'] },
      transport: { endpoint: 'https://www.bankofengland.co.uk/boeapps/database', path: '/series',
                   settings: { series_codes: 'IUDSOIA' } },
      credential: 'none',
      schedule: { enabled: true, rhythm: 'Daily at 15:00 GMT', next: '2026-10-29 15:00', last: '2026-10-28 15:00 · ok',
                  job: 'marketdata.refresh.boe-boe' },
      runtime: { state: 'serving', hb: '15:00', served: 6, refused: 0, unknown: 0, lastTick: '15:00' },
      routes: [{ party: 'System Party', class: 'ir' }], last: '15:00' },
    { name: 'fred-frankfurter', provider: 'fred', adapter: 'frankfurter', kind: 'live', isDefault: true, classes: ['fx'],
      description: 'The Federal Reserve H.10 rates read through Frankfurter by the official adapter.',
      scope: { mode: 'all', items: ['EUR/USD', 'USD/JPY', 'EUR/GBP'] },
      transport: { endpoint: 'https://api.frankfurter.app', path: '/v2/providers/FED/rates', settings: { backfill_days: '400' } },
      credential: 'vault:marketdata/fred',
      schedule: { enabled: true, rhythm: 'Daily at 16:15 ET', next: '2026-10-29 16:15', last: '2026-10-28 19:22 · ok',
                  job: 'marketdata.refresh.fred-frankfurter' },
      runtime: { state: 'serving', hb: '19:24', served: 31, refused: 0, unknown: 0, lastTick: '19:22' },
      routes: [{ party: 'System Party', class: 'fx' }], last: '19:22' },
    { name: 'fred-api', provider: 'fred', adapter: 'fred-api', kind: 'live', isDefault: false, classes: ['ir'],
      description: 'The FRED series: SOFR and the H.15 Treasury par curve.',
      scope: { mode: 'items', items: ['SOFR', 'H.15 Treasury par curve'] },
      transport: { endpoint: 'https://api.stlouisfed.org', path: '/fred/series/observations',
                   settings: { api_key_ref: 'vault:marketdata/fred', series_ids: 'SOFR, DGS10' } },
      credential: 'vault:marketdata/fred',
      schedule: { enabled: true, rhythm: 'Daily at 16:15 ET', next: '2026-10-29 16:15', last: '2026-10-28 19:22 · ok',
                  job: 'marketdata.refresh.fred-api' },
      runtime: { state: 'serving', hb: '19:22', served: 9, refused: 0, unknown: 0, lastTick: '19:22' },
      routes: [{ party: 'System Party', class: 'ir' }], last: '19:22' },
    { name: 'ust-treasury', provider: 'ust', adapter: 'treasury-feed', kind: 'live', isDefault: true, classes: ['ir'],
      description: 'The US Treasury par yield curve, read from the Treasury data feed.',
      scope: { mode: 'all', items: ['US Treasury par curve'] },
      transport: { endpoint: 'https://home.treasury.gov/resource-center/data-chart-center', path: '/interest-rates',
                   settings: { curve_year: '2026' } },
      credential: 'none',
      schedule: { enabled: true, rhythm: 'Daily at 18:00 ET', next: '2026-10-29 18:00', last: '2026-10-28 18:05 · ok',
                  job: 'marketdata.refresh.ust-treasury' },
      runtime: { state: 'serving', hb: '18:05', served: 12, refused: 0, unknown: 0, lastTick: '18:05' },
      routes: [{ party: 'System Party', class: 'ir' }], last: '18:05' },
    { name: 'bloomberg-a', provider: 'bloomberg', adapter: 'bloomberg-api', kind: 'live', isDefault: true, classes: ['fx', 'ir'],
      description: 'The Bloomberg production session.',
      scope: { mode: 'items', items: ['vendor: EURUSD Curncy', 'vendor: SOFRRATE Index'] },
      transport: { endpoint: 'https://api.bloomberg.com', path: 'session',
                   settings: { api_key_ref: 'vault:marketdata/bloomberg/prod#api_key' } },
      credential: 'vault:marketdata/bloomberg/prod',
      schedule: { enabled: true, rhythm: 'Continuous session', next: '—', last: '2026-10-28 19:23 · ok',
                  job: 'marketdata.refresh.bloomberg-a' },
      runtime: { state: 'serving', hb: '19:23', served: 120, refused: 0, unknown: 1, lastTick: '19:23' },
      routes: [{ party: 'System Party', class: 'fx' }], last: '19:23' },
    { name: 'bloomberg-b', provider: 'bloomberg', adapter: 'bloomberg-api', kind: 'test', isDefault: false, classes: ['fx', 'ir'],
      description: 'The Bloomberg test session, for a new mapping or a new field.',
      scope: { mode: 'items', items: [] },
      transport: { endpoint: 'https://api.test.bloomberg.com', path: 'session',
                   settings: { api_key_ref: 'vault:marketdata/bloomberg/test#api_key' } },
      credential: 'vault:marketdata/bloomberg/test',
      schedule: { enabled: false, rhythm: 'On demand', next: '—', last: 'never', job: '—' },
      runtime: { state: 'not serving', hb: '—', served: 0, refused: 0, unknown: 0, lastTick: '—' },
      routes: [], last: '—' },
    { name: 'lseg-simulated', provider: 'lseg', adapter: 'simulation', kind: 'simulated', isDefault: true, classes: ['fx', 'ir'],
      description: 'The simulated LSEG feed the synthetic runner drives.',
      scope: { mode: 'all', items: ['simulated fx', 'simulated ir'] },
      transport: { endpoint: 'none', path: 'none', settings: {} },
      credential: 'none',
      schedule: { enabled: true, rhythm: 'Continuous, the scheduler drives it', next: '—', last: '2026-10-28 19:24 · ok',
                  job: 'marketdata.refresh.lseg-simulated' },
      runtime: { state: 'serving', hb: '19:24', served: 64, refused: 0, unknown: 0, lastTick: '19:24' },
      routes: [{ party: 'System Party', class: 'fx' }, { party: 'System Party', class: 'ir' }], last: 'now' },
];

const fn = {
    esc: (t) => String(t).replace(/[&<>"]/g, (c) => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;' }[c])),
    href: (changes) => {
        const next = new URLSearchParams(q);
        for (const [k, v] of Object.entries(changes)) { if (v === null) next.delete(k); else next.set(k, v); }
        return `?${next.toString()}`;
    },
    provider: (code) => PROVIDERS.find((p) => p.code === code),
    adapter: (code) => ADAPTERS.find((a) => a.code === code),
    source: (name) => SOURCES.find((s) => s.name === name),
    state: (s) => `<span class="badge ${s.runtime.state === 'serving' ? 'ok' : 'bad'}"><i class="dot"></i>${s.runtime.state}</span>`,
    sourcesOf: (code) => SOURCES.filter((s) => s.provider === code),
};

function hidePanels(except) {
    document.querySelectorAll('[role="tabpanel"]').forEach((x) => { x.hidden = x.id !== except; });
    document.querySelector('#provider-record').hidden = true;
    document.querySelector('#source-record').hidden = true;
    document.querySelector('#add-source').hidden = true;
}

function renderProviders() {
    hidePanels('tab-providers');
    document.querySelectorAll('[data-tab]').forEach((b) => b.setAttribute('aria-selected', String(b.dataset.tab === 'providers')));
    document.querySelector('#providers tbody').innerHTML = PROVIDERS.map((p) => {
        const sources = fn.sourcesOf(p.code);
        return `<tr>
          <td class="provider">${fn.esc(p.name)}<div class="faint mono">${p.code} · ${p.kind}</div></td>
          <td>${p.kind}</td>
          <td>${p.classes.map((c) => `<span class="badge">${c.toUpperCase()}</span>`).join(' ')}</td>
          <td>${p.terms.accepted ? `<span class="badge ok">accepted ${p.terms.version}</span>` : `<span class="badge warn">not accepted</span>`}</td>
          <td class="num">${sources.length}</td>
          <td class="mono faint">${p.credential}</td>
          <td class="acts"><a class="act" href="${fn.href({ tab: null, provider: p.code, ptab: 'overview' })}">Open</a></td>
        </tr>`;
    }).join('');
    document.querySelector('#providers-note').textContent =
        'A provider owns the ids, the terms and the credential reference. It may have several sources: one per asset class, or a test beside production. One source of each provider is the default.';
}

function renderAdapters() {
    hidePanels('tab-adapters');
    document.querySelectorAll('[data-tab]').forEach((b) => b.setAttribute('aria-selected', String(b.dataset.tab === 'adapters')));
    document.querySelector('#adapters tbody').innerHTML = ADAPTERS.map((a) => {
        const used = SOURCES.filter((s) => s.adapter === a.code);
        const providers = [...new Set(used.map((s) => s.provider))];
        return `<tr>
          <td class="provider">${fn.esc(a.name)}<div class="faint mono">${a.code} · ${a.process}</div></td>
          <td class="mono">${a.version}</td>
          <td>${fn.esc(a.description)}</td>
          <td>${a.classes.map((c) => `<span class="badge">${c.toUpperCase()}</span>`).join(' ')}</td>
          <td class="faint">${a.fields.length ? a.fields.map(([f]) => `<span class="mono">${f}</span>`).join(', ') : 'nothing: the simulator makes its own values'}</td>
          <td class="num">${used.length}</td>
          <td>${providers.length ? providers.map((c) => `<a href="${fn.href({ provider: c, ptab: 'sources', tab: null })}">${c}</a>`).join(', ') : '<span class="faint">none</span>'}</td>
        </tr>`;
    }).join('');
    document.querySelector('#adapters-note').textContent =
        'The adapters are code. The release installs this catalogue, with each module\'s version, description and configuration schema. A person adds a source that uses one; nobody adds an adapter.';
}

function renderSources() {
    hidePanels('tab-sources');
    document.querySelectorAll('[data-tab]').forEach((b) => b.setAttribute('aria-selected', String(b.dataset.tab === 'sources')));
    document.querySelector('#sources tbody').innerHTML = SOURCES.map((s) => {
        const p = fn.provider(s.provider);
        return `<tr>
          <td class="provider">${s.name}${s.isDefault ? ' <span class="badge">default</span>' : ''}</td>
          <td>${fn.esc(p.name)}<div class="faint mono">${p.code}</div></td>
          <td class="mono">${s.adapter}</td>
          <td>${s.kind}</td>
          <td>${s.classes.map((c) => `<span class="badge">${c.toUpperCase()}</span>`).join(' ')}</td>
          <td>${s.scope.mode === 'all' ? `all ${s.classes.map((c) => c.toUpperCase()).join('/')} items` : `${s.scope.items.length} selected`}</td>
          <td class="faint">${s.schedule.enabled ? fn.esc(s.schedule.rhythm) : 'off'}</td>
          <td>${fn.state(s)}</td>
          <td class="acts"><a class="act" href="${fn.href({ tab: null, source: s.name, stab: 'overview' })}">Open</a></td>
        </tr>`;
    }).join('');
    document.querySelector('#sources-note').textContent =
        'A source is one adapter instance of a provider. It carries its own transport, its own scope of underlyings and its own schedule. Two sources of one provider are how FX and IR arrive, or how a test session sits beside production.';
}

function renderProviderRecord() {
    const p = fn.provider(providerCode);
    if (!p) { location.search = fn.href({ tab: 'providers', provider: null, ptab: null }); return; }
    hidePanels(null);
    document.querySelector('#provider-record').hidden = false;
    document.querySelector('#prv-crumb').textContent = p.name;
    document.querySelector('#prv-title').textContent = p.name;
    document.querySelector('#prv-meta').innerHTML = `<span class="mono faint">${p.code}</span> · ${p.kind} · ${p.classes.map((c) => c.toUpperCase()).join(', ')} · ${p.terms.accepted ? `terms ${p.terms.version}` : '<span class="badge warn">terms not accepted</span>'}`;
    document.querySelector('#prv-acts').innerHTML = `<a class="act primary" href="${fn.href({ add: '1', provider: p.code, tab: null, ptab: null })}">Add a source</a>
        <a class="act" href="#">Terms <span class="faint">· design pending</span></a>`;
    document.querySelectorAll('[data-ptab]').forEach((b) => b.setAttribute('aria-selected', String(b.dataset.ptab === ptab)));
    const body = document.querySelector('#prv-body');
    const detail = (rows) => `<div class="kv">${rows.map(([k, v]) => `<span class="k">${k}</span><span>${v}</span>`).join('')}</div>`;
    const sources = fn.sourcesOf(p.code);
    const table = sources.length ? `<table class="list"><thead><tr><th>Source</th><th>Adapter</th><th>Kind</th><th>Classes</th><th>Schedule</th><th>State</th><th class="acts"></th></tr></thead><tbody>${sources.map((s) =>
        `<tr><td class="mono">${s.name}${s.isDefault ? ' <span class="badge">default</span>' : ''}</td><td class="mono">${s.adapter}</td>
         <td>${s.kind}</td><td>${s.classes.map((c) => c.toUpperCase()).join(', ')}</td>
         <td class="faint">${s.schedule.enabled ? fn.esc(s.schedule.rhythm) : 'off'}</td><td>${fn.state(s)}</td>
         <td class="acts"><a class="act" href="${fn.href({ source: s.name, stab: 'overview', provider: null, ptab: null })}">Open</a></td></tr>`).join('')}</tbody></table>`
        : '<div class="notice">No source is configured for this provider yet.</div>';

    if (ptab === 'overview') {
        body.innerHTML = `<div class="card"><div class="card-head"><h2>Provider</h2><p>the terms and the facts every source inherits</p></div><div class="card-body">${detail([
            ['Code', `<span class="mono">${p.code}</span>`],
            ['Kind', p.kind],
            ['Asset classes', p.classes.map((c) => c.toUpperCase()).join(', ')],
            ['Terms', p.terms.accepted ? `accepted ${p.terms.version} by ${p.terms.who} · ${p.terms.at}` : 'not accepted'],
            ['Credential reference', `<span class="mono">${p.credential}</span>`],
            ['Sources', String(sources.length)],
        ])}<div class="faint" style="margin-top:8px">The provider owns the credential reference unless a source names its own, which is how a test session differs from production.</div></div></div>
        <div class="card" style="margin-top:12px"><div class="card-head"><h2>Sources</h2><p>one per asset class, or a test beside production</p>
          <span class="filters"><a class="act" href="${fn.href({ add: '1', provider: p.code, ptab: null, provider2: null, tab: null })}">Add a source</a></span></div>
          ${table}</div>`;
    } else if (ptab === 'sources') {
        const available = ADAPTERS.filter((a) => a.classes.some((c) => p.classes.includes(c))
            && !sources.some((s) => s.adapter === a.code && s.kind !== 'test'));
        body.innerHTML = `<div class="card"><div class="card-head"><h2>Sources</h2><p>what is configured, and what could be</p></div>${table}
          <div class="card-body"><div class="faint">One source of each provider is the default; a party routes one source for each asset class.</div></div></div>
          <div class="card" style="margin-top:12px"><div class="card-head"><h2>Adapters available</h2><p>modules the code provides for this provider's classes, with no source yet</p></div>
          <table class="list"><thead><tr><th>Adapter</th><th>Version</th><th>Classes</th><th>Why it fits</th></tr></thead><tbody>${available.length ? available.map((a) =>
            `<tr><td class="mono">${a.code}</td><td>${a.version}</td><td>${a.classes.map((c) => c.toUpperCase()).join(', ')}</td>
             <td class="faint">${fn.esc(a.description)}</td></tr>`).join('') : '<tr><td colspan="4" class="faint">Every adapter that fits this provider already has a source.</td></tr>'}</tbody></table></div>`;
    } else {
        body.innerHTML = `<div class="card"><div class="card-head"><h2>History</h2><p>the provider and its sources</p></div><div class="card-body"><div class="timeline">
          <div class="row"><span class="at">2026-10-01 09:00</span><span class="what"><b>Provider created</b> · v1 · by super_admin</span></div>
          <div class="row"><span class="at">2026-10-18 14:00</span><span class="what"><b>Terms accepted</b> · ${p.terms.version} · by ${p.terms.who}</span></div>
          <div class="row"><span class="at">2026-10-28 17:02</span><span class="what"><b>Source added</b> · ${sources.length > 1 ? sources[1].name : sources[0]?.name ?? 'none'} · by super_admin</span></div>
        </div></div></div>`;
    }
}

function renderSourceRecord() {
    const s = fn.source(sourceName);
    if (!s) { location.search = fn.href({ tab: 'sources', source: null, stab: null }); return; }
    const p = fn.provider(s.provider);
    const a = fn.adapter(s.adapter);
    hidePanels(null);
    document.querySelector('#source-record').hidden = false;
    document.querySelector('#src-crumb').textContent = s.name;
    document.querySelector('#src-title').textContent = s.name;
    document.querySelector('#src-meta').innerHTML = `<span class="mono faint">${s.provider}</span> · ${s.adapter} ${a.version} · ${s.kind}${s.isDefault ? ' · default' : ''} · ${fn.state(s)}`;
    document.querySelector('#src-acts').innerHTML = `<a class="act" href="#">Edit <span class="faint">· design pending</span></a>
        <a class="act" href="#">${s.runtime.state === 'serving' ? 'Stop' : 'Start'}</a>`;
    document.querySelectorAll('[data-stab]').forEach((b) => b.setAttribute('aria-selected', String(b.dataset.stab === stab)));
    const body = document.querySelector('#src-body');
    const detail = (rows) => `<div class="kv">${rows.map(([k, v]) => `<span class="k">${k}</span><span>${v}</span>`).join('')}</div>`;

    if (stab === 'overview') {
        body.innerHTML = `<div class="card"><div class="card-head"><h2>Source</h2><p>one adapter instance of a provider</p></div><div class="card-body">${detail([
            ['Name', `<span class="mono">${s.name}</span>`],
            ['Provider', `${fn.esc(p.name)} <span class="faint mono">${p.code}</span>`],
            ['Adapter', `<span class="mono">${s.adapter}</span> ${a.version}`],
            ['Kind', s.kind],
            ['Default', s.isDefault ? 'yes' : 'no'],
            ['Asset classes', s.classes.map((c) => c.toUpperCase()).join(', ')],
            ['Description', fn.esc(s.description)],
        ])}<div class="faint" style="margin-top:8px">The source's name is its adapter id. The provider owns the terms; the source owns the transport, the scope and the schedule.</div></div></div>`;
    } else if (stab === 'transport') {
        body.innerHTML = `<div class="card"><div class="card-head"><h2>Transport</h2><p>what the adapter needs to reach this provider</p></div><div class="card-body">${detail([
            ['Endpoint', `<span class="mono">${fn.esc(s.transport.endpoint)}</span>`],
            ['Path', `<span class="mono">${fn.esc(s.transport.path)}</span>`],
            ...Object.entries(s.transport.settings).map(([k, v]) => [k, `<span class="mono">${fn.esc(v)}</span>`]),
            ['Credential reference', `<span class="mono">${fn.esc(s.credential)}</span> <span class="faint">source, falling back to provider</span>`],
        ])}</div></div>
        <div class="card" style="margin-top:12px"><div class="card-head"><h2>What ${a.code} requires</h2><p>the schema the screen builds this form from</p></div>
          <table class="list"><thead><tr><th>Field</th><th>Type</th><th>Required</th><th>Example</th></tr></thead><tbody>${a.fields.length ? a.fields.map(([f, label, req, ex]) =>
            `<tr><td class="mono">${f}</td><td>${fn.esc(label)}</td><td>${req ? 'yes' : 'no'}</td><td class="faint mono">${fn.esc(ex)}</td></tr>`).join('') : '<tr><td colspan="4" class="faint">This adapter needs nothing configured.</td></tr>'}</tbody></table></div>`;
    } else if (stab === 'underlyings') {
        const rows = s.scope.items.map((item) => `<tr><td class="mono">${fn.esc(item)}</td><td>${s.classes.map((c) => c.toUpperCase()).join(', ')}</td>
            <td>${s.scope.mode === 'all' ? '<span class="badge ok">all</span>' : '<span class="badge ok">selected</span>'}</td></tr>`).join('');
        body.innerHTML = `<div class="card"><div class="card-head"><h2>Underlyings</h2><p>what this source fetches for ${p.code}</p>
          <span class="filters"><span class="faint">${s.scope.mode === 'all' ? 'every item the provider publishes' : 'a chosen set'}</span></span></div>
          <table class="list"><thead><tr><th>Item</th><th>Class</th><th>Included</th></tr></thead><tbody>${rows || '<tr><td colspan="3" class="faint">No underlying is selected yet.</td></tr>'}</tbody></table>
          <div class="card-body"><div class="faint">This is where the scope is chosen: all items of a class, or a set. No item belongs to two sources of one provider. Public rates reads this selection; it does not change it.</div></div></div>`;
    } else if (stab === 'schedule') {
        body.innerHTML = `<div class="card"><div class="card-head"><h2>Schedule</h2><p>the job that asks the adapter to fetch this source</p></div><div class="card-body">${detail([
            ['Enabled', s.schedule.enabled ? 'yes' : 'no'],
            ['Rhythm', fn.esc(s.schedule.rhythm)],
            ['Scheduler job', `<span class="mono">${fn.esc(s.schedule.job)}</span>`],
            ['Next run', s.schedule.next],
            ['Last run', fn.esc(s.schedule.last)],
        ])}<div class="faint" style="margin-top:8px">The scheduler owns the rhythm and sends the refresh request naming this source. The Fetch button in Public rates sends the same request now.</div></div></div>`;
    } else if (stab === 'runtime') {
        body.innerHTML = `<div class="card"><div class="card-head"><h2>Runtime</h2><p>the adapter, as the service sees it</p></div><div class="card-body">${detail([
            ['Adapter id', `<span class="mono">${s.name}</span>`],
            ['State', s.runtime.state],
            ['Heartbeat', `<span class="mono">${s.runtime.hb}</span>`],
            ['Ids served', String(s.runtime.served)],
            ['Ids refused', String(s.runtime.refused)],
            ['Ids unknown', String(s.runtime.unknown)],
            ['Last tick', s.runtime.lastTick ?? 'never'],
        ])}</div></div>`;
    } else if (stab === 'routes') {
        const others = SOURCES.filter((x) => x.provider === s.provider && x.name !== s.name);
        body.innerHTML = `<div class="card"><div class="card-head"><h2>Routes</h2><p>the party that reads this source, and for which class</p></div><div class="card-body">
          ${s.routes.length ? `<table class="list"><thead><tr><th>Party</th><th>Provider</th><th>Asset class</th><th>Source</th><th>Since</th></tr></thead><tbody>${s.routes.map((r) =>
            `<tr><td>${r.party}</td><td class="mono">${s.provider}</td><td>${r.class.toUpperCase()}</td><td class="mono">${s.name}</td><td class="when">2026-10-28</td></tr>`).join('')}</tbody></table>`
            : '<div class="notice">No party routes this source. A test source is configured but not routed.</div>'}
          <div class="faint" style="margin-top:8px">A party routes one source for each provider and asset class, so it reads one value of each item. A switch of source is one new route.</div></div></div>
          ${others.length ? `<div class="card" style="margin-top:12px"><div class="card-head"><h2>Other sources of ${p.code}</h2><p>one per class, or a test beside production</p></div>
          <table class="list"><thead><tr><th>Source</th><th>Adapter</th><th>Kind</th><th>Classes</th><th>State</th><th class="acts"></th></tr></thead><tbody>${others.map((x) =>
            `<tr><td class="mono">${x.name}${x.isDefault ? ' <span class="badge">default</span>' : ''}</td><td class="mono">${x.adapter}</td>
             <td>${x.kind}</td><td>${x.classes.map((c) => c.toUpperCase()).join(', ')}</td><td>${fn.state(x)}</td>
             <td class="acts"><a class="act" href="${fn.href({ source: x.name, stab: 'overview', tab: null })}">Open</a></td></tr>`).join('')}</tbody></table></div>` : ''}`;
    } else {
        body.innerHTML = `<div class="card"><div class="card-head"><h2>History</h2><p>the shared timeline</p></div><div class="card-body"><div class="timeline">
          <div class="row"><span class="at">2026-10-01 09:00</span><span class="what"><b>Source created</b> · v1 · by super_admin</span></div>
          <div class="row"><span class="at">2026-10-20 11:12</span><span class="what"><b>Scope set</b> · ${s.scope.mode === 'all' ? 'all items of the class' : `${s.scope.items.length} items`} · by super_admin</span></div>
          <div class="row"><span class="at">2026-10-28 09:00</span><span class="what"><b>Schedule set</b> · ${fn.esc(s.schedule.rhythm)} · by super_admin</span></div>
        </div></div></div>`;
    }
}

function renderAddSource() {
    hidePanels(null);
    document.querySelector('#add-source').hidden = false;
    const p = fn.provider(providerCode) ?? PROVIDERS[0];
    const fit = ADAPTERS.filter((a) => a.classes.some((c) => p.classes.includes(c)));
    const body = document.querySelector('#add-body');
    body.innerHTML = `<div class="card"><div class="card-head"><h2>1. Provider and adapter</h2><p>the adapter list is filtered to the provider's classes</p></div>
      <div class="card-body"><div class="fields">
        <label>Provider<select>${PROVIDERS.map((x) => `<option ${x.code === p.code ? 'selected' : ''}>${fn.esc(x.name)}</option>`).join('')}</select></label>
        <label>Adapter<select>${fit.map((a) => `<option>${a.code} ${a.version} — ${fn.esc(a.description)}</option>`).join('')}</select></label>
        <label>Kind<select><option>live</option><option>test</option><option>simulated</option><option>replay</option></select></label>
      </div></div></div>
      <div class="card" style="margin-top:12px"><div class="card-head"><h2>2. Transport</h2><p>built from the adapter's schema</p></div>
      <div class="card-body"><div class="fields">
        <label>Endpoint<input value="https://api.frankfurter.app"></label>
        <label>Provider key<input value="ECB"></label>
        <label>Backfill days<input value="400"></label>
        <label>Credential reference<input value="none"></label>
      </div><div class="faint">A secret is a reference. The value is encrypted at rest and never returned by a read.</div></div></div>
      <div class="card" style="margin-top:12px"><div class="card-head"><h2>3. Underlyings</h2><p>what this source fetches</p></div>
      <div class="card-body"><div class="fields">
        <label>Scope<select><option>All items of the class</option><option>A chosen set</option></select></label>
        <label>Asset class<select>${p.classes.map((c) => `<option>${c.toUpperCase()}</option>`).join('')}</select></label>
      </div><div class="faint">FX: EUR/USD, EUR/CAD, EUR/GBP. This source owns them; no other source of ${p.code} may.</div></div></div>
      <div class="card" style="margin-top:12px"><div class="card-head"><h2>4. Schedule and route</h2><p>when to fetch, and which party reads it</p></div>
      <div class="card-body"><div class="fields">
        <label>Rhythm<select><option>Daily at 16:00 CET</option><option>Continuous</option><option>On demand</option></select></label>
        <label>Party<select><option>System Party</option></select></label>
        <label>Asset class<select>${p.classes.map((c) => `<option>${c.toUpperCase()}</option>`).join('')}</select></label>
      </div><div class="faint">The scheduler job is created for the source. The route is one per party, provider and class.</div></div></div>
      <div class="card" style="margin-top:12px"><div class="card-body"><div class="foot">
        <a class="act" href="${fn.href({ tab: 'sources', add: null, provider: null })}">Cancel</a>
        <a class="act primary" href="${fn.href({ tab: 'sources', add: null, provider: null })}">Create source <span class="faint">· design pending</span></a>
      </div></div></div>`;
}

function wire() {
    document.querySelectorAll('[data-tab]').forEach((b) => b.addEventListener('click', () => {
        location.search = fn.href({ tab: b.dataset.tab, provider: null, ptab: null, source: null, stab: null, add: null });
    }));
    document.querySelectorAll('[data-ptab]').forEach((b) => b.addEventListener('click', () => {
        location.search = fn.href({ provider: providerCode, ptab: b.dataset.ptab, tab: null, source: null, stab: null, add: null });
    }));
    document.querySelectorAll('[data-stab]').forEach((b) => b.addEventListener('click', () => {
        location.search = fn.href({ source: sourceName, stab: b.dataset.stab, tab: null, provider: null, ptab: null, add: null });
    }));
    document.querySelector('#refresh').addEventListener('click', () => location.reload());
}

if (adding) renderAddSource();
else if (sourceName) renderSourceRecord();
else if (providerCode) renderProviderRecord();
else if (tab === 'adapters') renderAdapters();
else if (tab === 'sources') renderSources();
else renderProviders();
wire();
