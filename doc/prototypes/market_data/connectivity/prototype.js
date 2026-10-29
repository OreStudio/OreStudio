/*
 * Static prototype of Connectivity: publishers, suppliers, adapters, sessions and
 * feeds. Mock data only; no request leaves the page.
 *
 *   ?tab=publishers|suppliers|adapters|feeds
 *   ?publisher=<code>&pbtab=overview|feeds|history
 *   ?supplier=<code>&sutab=overview|sessions|history
 *   ?session=<name>&setab=overview|feeds|runtime|history
 *   ?feed=<name>&ftab=overview|underlyings|schedule|routes|history
 *   ?add-session=1&supplier=<code>   ?add-feed=1&publisher=<code>
 */

const q = new URLSearchParams(location.search);
const tab = q.get('tab') ?? 'publishers';
const publisherCode = q.get('publisher');
const pbtab = q.get('pbtab') ?? 'overview';
const supplierCode = q.get('supplier');
const sutab = q.get('sutab') ?? 'overview';
const sessionName = q.get('session');
const setab = q.get('setab') ?? 'overview';
const feedName = q.get('feed');
const ftab = q.get('ftab') ?? 'overview';
const addSession = q.get('add-session') === '1';
const addFeed = q.get('add-feed') === '1';

const SERVICES = ['real_time', 'delayed', 'end_of_day', 'reference', 'historical'];
const ENVS = ['production', 'test', 'dr', 'simulated'];

const PUBLISHERS = [
    { code: 'ecb', name: 'European Central Bank', kind: 'reference', classes: ['fx', 'ir'],
      terms: { accepted: true, version: 'v3', who: 'super_admin', at: '2026-01-15 10:22' } },
    { code: 'imf', name: 'International Monetary Fund', kind: 'reference', classes: ['fx'],
      terms: { accepted: true, version: 'v1', who: 'super_admin', at: '2026-09-01 09:00' } },
    { code: 'boc', name: 'Bank of Canada', kind: 'reference', classes: ['fx'],
      terms: { accepted: true, version: 'v2', who: 'super_admin', at: '2026-10-26 11:20' } },
    { code: 'tcmb', name: 'Central Bank of Turkiye', kind: 'reference', classes: ['fx'],
      terms: { accepted: true, version: 'v1', who: 'super_admin', at: '2026-10-20 08:10' } },
    { code: 'norges', name: 'Norges Bank', kind: 'reference', classes: ['fx'],
      terms: { accepted: true, version: 'v1', who: 'super_admin', at: '2026-10-18 09:00' } },
    { code: 'boe', name: 'Bank of England', kind: 'reference', classes: ['fx', 'ir'],
      terms: { accepted: true, version: 'v1', who: 'super_admin', at: '2026-10-18 14:00' } },
    { code: 'fred', name: 'Federal Reserve (H.10)', kind: 'reference', classes: ['fx', 'ir'],
      terms: { accepted: true, version: 'v1', who: 'super_admin', at: '2026-10-02 14:00' } },
    { code: 'ust', name: 'US Treasury', kind: 'reference', classes: ['ir'],
      terms: { accepted: true, version: 'v1', who: 'super_admin', at: '2026-10-05 10:00' } },
    { code: 'bloomberg', name: 'Bloomberg', kind: 'data_vendor', classes: ['fx', 'ir'],
      terms: { accepted: true, version: 'v1', who: 'super_admin', at: '2026-10-01 09:00' } },
    { code: 'lseg', name: 'LSEG', kind: 'data_vendor', classes: ['fx', 'ir'],
      terms: { accepted: true, version: 'v1', who: 'super_admin', at: '2026-10-01 09:00' } },
];

const ADAPTERS = [
    { code: 'frankfurter', name: 'Frankfurter', version: '1.4.0', process: 'ores.marketdata.official',
      description: "Reads a publisher's daily rates from a Frankfurter endpoint.", classes: ['fx'],
      services: ['end_of_day', 'reference'],
      fields: [['url', 'URL', true, 'https://api.frankfurter.app'], ['backfill_days', 'Backfill days', false, '400'],
               ['timeout_ms', 'Timeout (ms)', false, '10000']] },
    { code: 'ecb-sdw', name: 'ECB Data Portal', version: '1.0.0', process: 'ores.marketdata.official',
      description: 'Reads ECB Data Portal series: rates and curve pillars.', classes: ['ir'],
      services: ['end_of_day', 'reference'],
      fields: [['url', 'URL', true, 'https://data-api.ecb.europa.eu'], ['backfill_days', 'Backfill days', false, '400']] },
    { code: 'fred-api', name: 'FRED', version: '2.1.0', process: 'ores.marketdata.official',
      description: 'Reads FRED series by id.', classes: ['fx', 'ir'], services: ['end_of_day', 'reference'],
      fields: [['url', 'URL', true, 'https://api.stlouisfed.org'], ['api_key_ref', 'API key reference', true, 'vault:marketdata/fred']] },
    { code: 'boe-stats', name: 'Bank of England statistics', version: '1.3.0', process: 'ores.marketdata.official',
      description: 'Reads Bank of England statistics series.', classes: ['fx', 'ir'], services: ['end_of_day', 'reference'],
      fields: [['url', 'URL', true, 'https://www.bankofengland.co.uk/boeapps/database'], ['backfill_days', 'Backfill days', false, '400']] },
    { code: 'treasury-feed', name: 'US Treasury feed', version: '1.0.0', process: 'ores.marketdata.official',
      description: 'Reads the US Treasury par yield curve.', classes: ['ir'], services: ['end_of_day'],
      fields: [['url', 'URL', true, 'https://home.treasury.gov/resource-center/data-chart-center'], ['curve_year', 'Curve year', true, '2026']] },
    { code: 'bloomberg-api', name: 'Bloomberg session', version: '3.2.0', process: 'ores.marketdata.official',
      description: 'Reads a Bloomberg session by ticker and field.', classes: ['fx', 'ir'],
      services: ['real_time', 'delayed', 'end_of_day', 'reference'],
      fields: [['endpoint', 'Endpoint', true, 'https://api.bloomberg.com'], ['api_key_ref', 'API key reference', true, 'vault:marketdata/bloomberg/prod#api_key'],
               ['api_secret_ref', 'API secret reference', true, 'vault:marketdata/bloomberg/prod#api_secret']] },
    { code: 'lseg-api', name: 'LSEG session', version: '2.8.0', process: 'ores.marketdata.official',
      description: 'Reads an LSEG session by RIC.', classes: ['fx', 'ir'], services: ['real_time', 'reference'],
      fields: [['endpoint', 'Endpoint', true, 'https://api.lseg.com'], ['credential_ref', 'Credential reference', true, 'vault:marketdata/lseg']] },
    { code: 'simulation', name: 'Simulator', version: '1.0.0', process: 'ores.marketdata.synthetic',
      description: 'Runs the synthetic generators the runner authors.', classes: ['fx', 'ir'],
      services: ['real_time', 'end_of_day'], fields: [] },
];

const SUPPLIERS = [
    { code: 'frankfurter', name: 'Frankfurter', kind: 'aggregator', contract: 'open service, no contract',
      support: 'community', description: 'An open service that republishes many publishers\' daily rates over one endpoint.' },
    { code: 'bloomberg', name: 'Bloomberg', kind: 'vendor', contract: 'vault:contracts/bloomberg',
      support: 'Bloomberg support', description: 'The Bloomberg session; also a publisher of its own composites.' },
    { code: 'lseg', name: 'LSEG', kind: 'vendor', contract: 'vault:contracts/lseg',
      support: 'LSEG support', description: 'The LSEG session.' },
    { code: 'ecb-data-portal', name: 'ECB Data Portal', kind: 'authority', contract: 'none',
      support: 'ECB', description: 'The ECB\'s own data API; supplier and publisher are the same party.' },
    { code: 'fred', name: 'FRED', kind: 'authority', contract: 'none',
      support: 'St. Louis Fed', description: 'The Federal Reserve Bank of St. Louis data service.' },
    { code: 'boe', name: 'Bank of England statistics', kind: 'authority', contract: 'none',
      support: 'Bank of England', description: 'The Bank of England statistics database.' },
    { code: 'treasury', name: 'US Treasury', kind: 'authority', contract: 'none',
      support: 'US Treasury', description: 'The US Treasury data feed.' },
    { code: 'synthetic', name: 'Synthetic simulator', kind: 'internal', contract: 'none',
      support: 'platform team', description: 'The simulator the synthetic runner drives.' },
];

const SESSIONS = [
    { name: 'frankfurter-eu', supplier: 'frankfurter', adapter: 'frankfurter', environment: 'production',
      endpoint: 'https://api.frankfurter.app', credential: 'none',
      settings: { backfill_days: '400', timeout_ms: '10000' }, enabled: true,
      services: ['end_of_day', 'reference'],
      description: 'The shared Frankfurter session for every public publisher it delivers.',
      runtime: { state: 'serving', hb: '19:24', served: 189, refused: 0, unknown: 0, lastTick: '19:24' } },
    { name: 'bloomberg-prod', supplier: 'bloomberg', adapter: 'bloomberg-api', environment: 'production',
      endpoint: 'https://api.bloomberg.com', credential: 'vault:marketdata/bloomberg/prod',
      settings: { api_key_ref: 'vault:marketdata/bloomberg/prod#api_key', api_secret_ref: 'vault:marketdata/bloomberg/prod#api_secret' },
      enabled: true, services: ['real_time', 'delayed', 'end_of_day', 'reference'],
      description: 'The Bloomberg production session.',
      runtime: { state: 'serving', hb: '19:23', served: 120, refused: 0, unknown: 1, lastTick: '19:23' } },
    { name: 'bloomberg-test', supplier: 'bloomberg', adapter: 'bloomberg-api', environment: 'test',
      endpoint: 'https://api.test.bloomberg.com', credential: 'vault:marketdata/bloomberg/test',
      settings: { api_key_ref: 'vault:marketdata/bloomberg/test#api_key' }, enabled: false,
      services: ['real_time', 'end_of_day'],
      description: 'The Bloomberg test session, for a new mapping or a new field.',
      runtime: { state: 'not serving', hb: '—', served: 0, refused: 0, unknown: 0, lastTick: '—' } },
    { name: 'lseg-prod', supplier: 'lseg', adapter: 'lseg-api', environment: 'production',
      endpoint: 'https://api.lseg.com', credential: 'vault:marketdata/lseg',
      settings: { credential_ref: 'vault:marketdata/lseg' }, enabled: true,
      services: ['real_time', 'reference'], description: 'The LSEG production session.',
      runtime: { state: 'serving', hb: '19:20', served: 40, refused: 0, unknown: 0, lastTick: '19:20' } },
    { name: 'ecb-sdw', supplier: 'ecb-data-portal', adapter: 'ecb-sdw', environment: 'production',
      endpoint: 'https://data-api.ecb.europa.eu', credential: 'none',
      settings: { backfill_days: '400' }, enabled: true, services: ['end_of_day', 'reference'],
      description: 'The ECB Data Portal session for €STR and the EUR AAA spot curve.',
      runtime: { state: 'serving', hb: '17:02', served: 12, refused: 0, unknown: 0, lastTick: '17:02' } },
    { name: 'fred-api', supplier: 'fred', adapter: 'fred-api', environment: 'production',
      endpoint: 'https://api.stlouisfed.org', credential: 'vault:marketdata/fred',
      settings: { api_key_ref: 'vault:marketdata/fred' }, enabled: true, services: ['end_of_day', 'reference'],
      description: 'The FRED session for SOFR and the H.15 curve.',
      runtime: { state: 'serving', hb: '19:22', served: 9, refused: 0, unknown: 0, lastTick: '19:22' } },
    { name: 'boe-stats', supplier: 'boe', adapter: 'boe-stats', environment: 'production',
      endpoint: 'https://www.bankofengland.co.uk/boeapps/database', credential: 'none',
      settings: { backfill_days: '400' }, enabled: true, services: ['end_of_day', 'reference'],
      description: 'The Bank of England statistics session for SONIA and the gilt curve.',
      runtime: { state: 'serving', hb: '15:00', served: 6, refused: 0, unknown: 0, lastTick: '15:00' } },
    { name: 'treasury', supplier: 'treasury', adapter: 'treasury-feed', environment: 'production',
      endpoint: 'https://home.treasury.gov/resource-center/data-chart-center', credential: 'none',
      settings: { curve_year: '2026' }, enabled: true, services: ['end_of_day'],
      description: 'The US Treasury session for the par yield curve.',
      runtime: { state: 'serving', hb: '18:05', served: 12, refused: 0, unknown: 0, lastTick: '18:05' } },
    { name: 'synthetic-sim', supplier: 'synthetic', adapter: 'simulation', environment: 'simulated',
      endpoint: 'none', credential: 'none', settings: {}, enabled: true, services: ['real_time', 'end_of_day'],
      description: 'The simulator session the synthetic runner drives.',
      runtime: { state: 'serving', hb: '19:24', served: 64, refused: 0, unknown: 0, lastTick: '19:24' } },
];

const FEEDS = [
    { name: 'ecb-frankfurter', publisher: 'ecb', session: 'frankfurter-eu', service: 'end_of_day', providerKey: 'ECB',
      isDefault: true, classes: ['fx'], scope: { mode: 'all', items: ['EUR/USD', 'EUR/CAD', 'EUR/GBP'] },
      description: 'The ECB reference rates read through Frankfurter.',
      schedule: { enabled: true, rhythm: 'Daily at 16:00 CET', next: '2026-10-29 16:00', last: '2026-10-28 19:24 · ok',
                  job: 'marketdata.refresh.ecb-frankfurter' },
      routes: [{ party: 'System Party', class: 'fx' }], last: '19:24' },
    { name: 'ecb-ecb-data', publisher: 'ecb', session: 'ecb-sdw', service: 'end_of_day', providerKey: 'ESTR',
      isDefault: false, classes: ['ir'], scope: { mode: 'items', items: ['€STR', 'EUR AAA spot curve'] },
      description: 'The ECB Data Portal series: €STR and the EUR AAA spot curve.',
      schedule: { enabled: true, rhythm: 'Daily at 16:00 CET', next: '2026-10-29 16:00', last: '2026-10-28 17:02 · ok',
                  job: 'marketdata.refresh.ecb-ecb-data' },
      routes: [{ party: 'System Party', class: 'ir' }], last: '17:02' },
    { name: 'ecb-bloomberg', publisher: 'ecb', session: 'bloomberg-prod', service: 'end_of_day', providerKey: 'ECB',
      isDefault: false, classes: ['fx'], scope: { mode: 'all', items: ['EUR/USD', 'EUR/CAD', 'EUR/GBP'] },
      description: 'The ECB rates as Bloomberg republishes them, for a supplier switch.',
      schedule: { enabled: false, rhythm: 'On demand', next: '—', last: 'never', job: '—' },
      routes: [], last: '—' },
    { name: 'imf-frankfurter', publisher: 'imf', session: 'frankfurter-eu', service: 'end_of_day', providerKey: 'IMF',
      isDefault: true, classes: ['fx'], scope: { mode: 'all', items: ['USD/EUR', 'USD/CHF'] },
      description: 'The IMF representative rates read through Frankfurter.',
      schedule: { enabled: true, rhythm: 'Daily at 16:00 CET', next: '2026-10-29 16:00', last: '2026-10-28 19:24 · ok',
                  job: 'marketdata.refresh.imf-frankfurter' },
      routes: [{ party: 'System Party', class: 'fx' }], last: '19:24' },
    { name: 'boc-frankfurter', publisher: 'boc', session: 'frankfurter-eu', service: 'end_of_day', providerKey: 'BOC',
      isDefault: true, classes: ['fx'], scope: { mode: 'all', items: ['CAD/USD', 'CAD/EUR'] },
      description: 'The Bank of Canada rates read through Frankfurter.',
      schedule: { enabled: true, rhythm: 'Daily at 16:00 CET', next: '2026-10-29 16:00', last: '2026-10-28 19:20 · late',
                  job: 'marketdata.refresh.boc-frankfurter' },
      routes: [{ party: 'System Party', class: 'fx' }], last: '19:20' },
    { name: 'tcmb-frankfurter', publisher: 'tcmb', session: 'frankfurter-eu', service: 'end_of_day', providerKey: 'TCMB',
      isDefault: true, classes: ['fx'], scope: { mode: 'all', items: ['TRY/USD', 'TRY/EUR'] },
      description: 'The Central Bank of Turkiye rates read through Frankfurter.',
      schedule: { enabled: true, rhythm: 'Daily at 15:30 TRT', next: '2026-10-29 15:30', last: '2026-10-27 16:02 · failed',
                  job: 'marketdata.refresh.tcmb-frankfurter' },
      routes: [{ party: 'System Party', class: 'fx' }], last: '2026-10-27' },
    { name: 'norges-frankfurter', publisher: 'norges', session: 'frankfurter-eu', service: 'end_of_day', providerKey: 'NB',
      isDefault: true, classes: ['fx'], scope: { mode: 'all', items: ['NOK/EUR', 'NOK/USD'] },
      description: 'The Norges Bank rates read through Frankfurter.',
      schedule: { enabled: true, rhythm: 'Daily at 14:00 CET', next: '2026-10-29 14:00', last: '2026-10-28 19:24 · ok',
                  job: 'marketdata.refresh.norges-frankfurter' },
      routes: [{ party: 'System Party', class: 'fx' }], last: null },
    { name: 'boe-frankfurter', publisher: 'boe', session: 'frankfurter-eu', service: 'end_of_day', providerKey: 'BOE',
      isDefault: true, classes: ['fx'], scope: { mode: 'all', items: ['GBP/USD', 'GBP/EUR'] },
      description: 'The Bank of England rates read through Frankfurter.',
      schedule: { enabled: true, rhythm: 'Daily at 15:00 GMT', next: '2026-10-29 15:00', last: '2026-10-28 15:00 · ok',
                  job: 'marketdata.refresh.boe-frankfurter' },
      routes: [{ party: 'System Party', class: 'fx' }], last: '15:00' },
    { name: 'boe-boe', publisher: 'boe', session: 'boe-stats', service: 'end_of_day', providerKey: 'IUDSOIA',
      isDefault: false, classes: ['ir'], scope: { mode: 'items', items: ['SONIA', 'GBP gilt curve'] },
      description: 'The Bank of England statistics: SONIA and the gilt curve.',
      schedule: { enabled: true, rhythm: 'Daily at 15:00 GMT', next: '2026-10-29 15:00', last: '2026-10-28 15:00 · ok',
                  job: 'marketdata.refresh.boe-boe' },
      routes: [{ party: 'System Party', class: 'ir' }], last: '15:00' },
    { name: 'fred-frankfurter', publisher: 'fred', session: 'frankfurter-eu', service: 'end_of_day', providerKey: 'FED',
      isDefault: true, classes: ['fx'], scope: { mode: 'all', items: ['EUR/USD', 'USD/JPY', 'EUR/GBP'] },
      description: 'The Federal Reserve H.10 rates read through Frankfurter.',
      schedule: { enabled: true, rhythm: 'Daily at 16:15 ET', next: '2026-10-29 16:15', last: '2026-10-28 19:22 · ok',
                  job: 'marketdata.refresh.fred-frankfurter' },
      routes: [{ party: 'System Party', class: 'fx' }], last: '19:22' },
    { name: 'fred-api', publisher: 'fred', session: 'fred-api', service: 'end_of_day', providerKey: 'SOFR, DGS10',
      isDefault: false, classes: ['ir'], scope: { mode: 'items', items: ['SOFR', 'H.15 Treasury par curve'] },
      description: 'The FRED series: SOFR and the H.15 Treasury par curve.',
      schedule: { enabled: true, rhythm: 'Daily at 16:15 ET', next: '2026-10-29 16:15', last: '2026-10-28 19:22 · ok',
                  job: 'marketdata.refresh.fred-api' },
      routes: [{ party: 'System Party', class: 'ir' }], last: '19:22' },
    { name: 'ust-treasury', publisher: 'ust', session: 'treasury', service: 'end_of_day', providerKey: '2026',
      isDefault: true, classes: ['ir'], scope: { mode: 'all', items: ['US Treasury par curve'] },
      description: 'The US Treasury par yield curve.',
      schedule: { enabled: true, rhythm: 'Daily at 18:00 ET', next: '2026-10-29 18:00', last: '2026-10-28 18:05 · ok',
                  job: 'marketdata.refresh.ust-treasury' },
      routes: [{ party: 'System Party', class: 'ir' }], last: '18:05' },
    { name: 'bbg-eurusd', publisher: 'bloomberg', session: 'bloomberg-prod', service: 'real_time', providerKey: 'EURUSD Curncy',
      isDefault: true, classes: ['fx'], scope: { mode: 'items', items: ['vendor: EURUSD Curncy', 'vendor: SOFRRATE Index'] },
      description: 'Bloomberg\'s own EUR/USD and SOFR values.',
      schedule: { enabled: true, rhythm: 'Continuous, demand driven', next: '—', last: '2026-10-28 19:23 · ok',
                  job: '—' },
      routes: [{ party: 'System Party', class: 'fx' }], last: '19:23' },
    { name: 'lseg-eurusd', publisher: 'lseg', session: 'lseg-prod', service: 'real_time', providerKey: 'EURUSD=',
      isDefault: true, classes: ['fx'], scope: { mode: 'items', items: ['vendor: EURUSD='] },
      description: 'LSEG\'s own EUR/USD value.',
      schedule: { enabled: true, rhythm: 'Continuous, demand driven', next: '—', last: '2026-10-28 19:20 · ok',
                  job: '—' },
      routes: [{ party: 'System Party', class: 'fx' }], last: '19:20' },
    { name: 'lseg-simulated', publisher: 'lseg', session: 'synthetic-sim', service: 'end_of_day', providerKey: 'sim',
      isDefault: false, classes: ['fx', 'ir'], scope: { mode: 'all', items: ['simulated fx', 'simulated ir'] },
      description: 'The simulated LSEG feed the synthetic runner drives.',
      schedule: { enabled: true, rhythm: 'Continuous, the scheduler drives it', next: '—', last: '2026-10-28 19:24 · ok',
                  job: 'marketdata.refresh.lseg-simulated' },
      routes: [], last: 'now' },
];

const fn = {
    esc: (t) => String(t).replace(/[&<>"]/g, (c) => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;' }[c])),
    href: (changes) => {
        const next = new URLSearchParams(q);
        for (const [k, v] of Object.entries(changes)) { if (v === null) next.delete(k); else next.set(k, v); }
        return `?${next.toString()}`;
    },
    publisher: (code) => PUBLISHERS.find((p) => p.code === code),
    supplier: (code) => SUPPLIERS.find((s) => s.code === code),
    adapter: (code) => ADAPTERS.find((a) => a.code === code),
    session: (name) => SESSIONS.find((s) => s.name === name),
    feed: (name) => FEEDS.find((f) => f.name === name),
    feedsOfPublisher: (code) => FEEDS.filter((f) => f.publisher === code),
    sessionsOfSupplier: (code) => SESSIONS.filter((s) => s.supplier === code),
    feedsOfSession: (name) => FEEDS.filter((f) => f.session === name),
    state: (s) => `<span class="badge ${s.runtime.state === 'serving' ? 'ok' : 'bad'}"><i class="dot"></i>${s.runtime.state}</span>`,
    feedState: (f) => {
        const s = fn.session(f.session);
        return `<span class="badge ${s.runtime.state === 'serving' ? 'ok' : 'bad'}"><i class="dot"></i>${s.runtime.state === 'serving' ? 'serving' : 'not serving'}</span>`;
    },
};

function hidePanels(except) {
    document.querySelectorAll('[role="tabpanel"]').forEach((x) => { x.hidden = x.id !== except; });
    ['#publisher-record', '#supplier-record', '#session-record', '#feed-record', '#add-session', '#add-feed']
        .forEach((id) => { document.querySelector(id).hidden = true; });
}

function renderPublishers() {
    hidePanels('tab-publishers');
    document.querySelectorAll('[data-tab]').forEach((b) => b.setAttribute('aria-selected', String(b.dataset.tab === 'publishers')));
    document.querySelector('#publishers tbody').innerHTML = PUBLISHERS.map((p) => {
        const feeds = fn.feedsOfPublisher(p.code);
        const suppliers = new Set(feeds.map((f) => fn.session(f.session).supplier));
        return `<tr>
          <td class="provider">${fn.esc(p.name)}<div class="faint mono">${p.code} · ${p.kind}</div></td>
          <td>${p.kind}</td>
          <td>${p.classes.map((c) => `<span class="badge">${c.toUpperCase()}</span>`).join(' ')}</td>
          <td>${p.terms.accepted ? `<span class="badge ok">accepted ${p.terms.version}</span>` : '<span class="badge warn">not accepted</span>'}</td>
          <td class="num">${feeds.length}</td>
          <td class="num">${suppliers.size}</td>
          <td class="acts"><a class="act" href="${fn.href({ tab: null, publisher: p.code, pbtab: 'overview' })}">Open</a></td>
        </tr>`;
    }).join('');
    document.querySelector('#publishers-note').textContent =
        'A publisher owns the ids, the credit and the terms. It may be reached through more than one supplier, and each path is a feed.';
}

function renderTree() {
    hidePanels('tab-suppliers');
    document.querySelectorAll('[data-tab]').forEach((b) => b.setAttribute('aria-selected', String(b.dataset.tab === 'suppliers')));
    document.querySelector('#tree').innerHTML = SUPPLIERS.map((sup) => {
        const sessions = fn.sessionsOfSupplier(sup.code);
        const feeds = sessions.reduce((n, s) => n + fn.feedsOfSession(s.name).length, 0);
        const node = `<details ${sup.code === 'frankfurter' ? 'open' : ''}>
          <summary><span class="node"><span class="label">${fn.esc(sup.name)}</span>
            <span class="faint">${sup.kind} · ${sessions.length} sessions · ${feeds} feeds</span>
            <span class="acts"><a class="act" href="${fn.href({ add_session: null, 'add-session': '1', supplier: sup.code, tab: null })}">New session</a>
            <a class="act" href="${fn.href({ tab: null, supplier: sup.code, sutab: 'overview' })}">Open</a></span></span></summary>
          <ul>${sessions.map((s) => {
            const a = fn.adapter(s.adapter);
            const fds = fn.feedsOfSession(s.name);
            return `<li><details ${s.name === 'frankfurter-eu' ? 'open' : ''}>
              <summary><span class="node"><span class="label mono">${s.name}</span>
                <span class="faint">${a.code} ${a.version} · ${s.environment} · ${fn.esc(s.endpoint)} · ${fn.state(s)}</span>
                <span class="acts"><a class="act" href="${fn.href({ add_feed: null, 'add-feed': '1', session: s.name, tab: null })}">Add feed</a>
                <a class="act" href="${fn.href({ tab: null, session: s.name, setab: 'overview' })}">Open</a></span></span></summary>
              <ul>${fds.length ? fds.map((f) => {
                const p = fn.publisher(f.publisher);
                return `<li><span class="node"><span class="mono">${f.name}</span>
                  <span class="faint">→ ${fn.esc(p.name)} · ${f.service} · ${f.scope.mode === 'all' ? 'all items' : `${f.scope.items.length} items`} · ${f.schedule.enabled ? fn.esc(f.schedule.rhythm) : 'schedule off'}</span>
                  <span class="acts"><a class="act" href="${fn.href({ tab: null, feed: f.name, ftab: 'overview' })}">Open</a></span></span></li>`;
              }).join('') : '<li class="faint">No feed comes off this session yet.</li>'}</ul>
            </details></li>`;
          }).join('') || '<li class="faint">No session for this supplier yet.</li>'}</ul>
        </details>`;
        return node;
    }).join('');
    document.querySelector('#suppliers-note').textContent =
        'The tree is how delivery works: a supplier has sessions, a session offers services, and a feed picks one publisher, one service, a scope and a schedule. The adapter is a property of the session.';
}

function renderAdapters() {
    hidePanels('tab-adapters');
    document.querySelectorAll('[data-tab]').forEach((b) => b.setAttribute('aria-selected', String(b.dataset.tab === 'adapters')));
    document.querySelector('#adapters tbody').innerHTML = ADAPTERS.map((a) => {
        const sessions = SESSIONS.filter((s) => s.adapter === a.code);
        return `<tr>
          <td class="provider">${fn.esc(a.name)}<div class="faint mono">${a.code} · ${a.process}</div></td>
          <td class="mono">${a.version}</td>
          <td>${fn.esc(a.description)}</td>
          <td>${a.classes.map((c) => `<span class="badge">${c.toUpperCase()}</span>`).join(' ')}</td>
          <td class="faint mono">${a.services.join(', ')}</td>
          <td class="faint">${a.fields.length ? a.fields.map(([f]) => `<span class="mono">${f}</span>`).join(', ') : 'nothing: the simulator makes its own values'}</td>
          <td class="num">${sessions.length}</td>
        </tr>`;
    }).join('');
    document.querySelector('#adapters-note').textContent =
        'The adapters are code. The release installs this catalogue with version, description, services and configuration schema. A person configures a session of one; nobody adds an adapter.';
}

function renderFeeds() {
    hidePanels('tab-feeds');
    document.querySelectorAll('[data-tab]').forEach((b) => b.setAttribute('aria-selected', String(b.dataset.tab === 'feeds')));
    document.querySelector('#feeds tbody').innerHTML = FEEDS.map((f) => {
        const p = fn.publisher(f.publisher);
        const s = fn.session(f.session);
        const sup = fn.supplier(s.supplier);
        return `<tr>
          <td class="provider">${f.name}${f.isDefault ? ' <span class="badge">default</span>' : ''}</td>
          <td>${fn.esc(p.name)}<div class="faint mono">${p.code}</div></td>
          <td class="mono"><a href="${fn.href({ tab: null, session: f.session, setab: 'overview' })}">${f.session}</a></td>
          <td>${fn.esc(sup.name)}</td>
          <td>${f.service}</td>
          <td>${f.scope.mode === 'all' ? `all ${f.classes.map((c) => c.toUpperCase()).join('/')} items` : `${f.scope.items.length} selected`}</td>
          <td class="faint">${f.schedule.enabled ? fn.esc(f.schedule.rhythm) : 'off'}</td>
          <td>${fn.feedState(f)}</td>
          <td class="acts"><a class="act" href="${fn.href({ tab: null, feed: f.name, ftab: 'overview' })}">Open</a></td>
        </tr>`;
    }).join('');
    document.querySelector('#feeds-note').textContent =
        'A feed is one publisher, on one session, on one service, with its own provider key, scope and schedule. Its state follows the session; whether its data is fresh is a Public rates question.';
}

function renderPublisherRecord() {
    const p = fn.publisher(publisherCode);
    if (!p) { location.search = fn.href({ tab: 'publishers', publisher: null, pbtab: null }); return; }
    hidePanels(null);
    document.querySelector('#publisher-record').hidden = false;
    document.querySelector('#pub-crumb').textContent = p.name;
    document.querySelector('#pub-title').textContent = p.name;
    document.querySelector('#pub-meta').innerHTML = `<span class="mono faint">${p.code}</span> · ${p.kind} · ${p.classes.map((c) => c.toUpperCase()).join(', ')} · ${p.terms.accepted ? `terms ${p.terms.version}` : '<span class="badge warn">not accepted</span>'}`;
    document.querySelector('#pub-acts').innerHTML = `<a class="act" href="${fn.href({ 'add-feed': '1', publisher: p.code, tab: null, pbtab: null })}">Add a feed</a>
        <a class="act" href="#">Terms <span class="faint">· design pending</span></a>`;
    document.querySelectorAll('[data-pbtab]').forEach((b) => b.setAttribute('aria-selected', String(b.dataset.pbtab === pbtab)));
    const body = document.querySelector('#pub-body');
    const detail = (rows) => `<div class="kv">${rows.map(([k, v]) => `<span class="k">${k}</span><span>${v}</span>`).join('')}</div>`;
    const feeds = fn.feedsOfPublisher(p.code);
    const table = `<table class="list"><thead><tr><th>Feed</th><th>Supplier</th><th>Session</th><th>Service</th><th>Classes</th><th>Default</th><th class="acts"></th></tr></thead><tbody>${feeds.length ? feeds.map((f) => {
        const s = fn.session(f.session); const sup = fn.supplier(s.supplier);
        return `<tr><td class="mono">${f.name}</td><td>${fn.esc(sup.name)}</td><td class="mono">${f.session}</td><td>${f.service}</td>
          <td>${f.classes.map((c) => c.toUpperCase()).join(', ')}</td><td>${f.isDefault ? 'yes' : '<span class="faint">no</span>'}</td>
          <td class="acts"><a class="act" href="${fn.href({ feed: f.name, ftab: 'overview', publisher: null, pbtab: null })}">Open</a></td></tr>`;
    }).join('') : '<tr><td colspan="7" class="faint">No feed for this publisher yet.</td></tr>'}</tbody></table>`;

    if (pbtab === 'overview') {
        body.innerHTML = `<div class="card"><div class="card-head"><h2>Publisher</h2><p>the content owner and its terms</p></div><div class="card-body">${detail([
            ['Code', `<span class="mono">${p.code}</span>`],
            ['Kind', p.kind],
            ['Asset classes', p.classes.map((c) => c.toUpperCase()).join(', ')],
            ['Terms', p.terms.accepted ? `accepted ${p.terms.version} by ${p.terms.who} · ${p.terms.at}` : 'not accepted'],
            ['Feeds', String(feeds.length)],
            ['Suppliers', [...new Set(feeds.map((f) => fn.session(f.session).supplier))].join(', ') || 'none'],
        ])}</div></div><div class="card" style="margin-top:12px"><div class="card-head"><h2>Feeds</h2><p>every path to this publisher's values</p></div>${table}</div>`;
    } else if (pbtab === 'feeds') {
        body.innerHTML = `<div class="card"><div class="card-head"><h2>Feeds</h2><p>one per supplier and class, or a test beside production</p>
          <span class="filters"><a class="act" href="${fn.href({ 'add-feed': '1', publisher: p.code, pbtab: null, tab: null })}">Add a feed</a></span></div>${table}</div>`;
    } else {
        body.innerHTML = `<div class="card"><div class="card-head"><h2>History</h2><p>the publisher and its feeds</p></div><div class="card-body"><div class="timeline">
          <div class="row"><span class="at">2026-10-01 09:00</span><span class="what"><b>Publisher created</b> · v1 · by super_admin</span></div>
          <div class="row"><span class="at">2026-10-18 14:00</span><span class="what"><b>Terms accepted</b> · ${p.terms.version} · by ${p.terms.who}</span></div>
        </div></div></div>`;
    }
}

function renderSupplierRecord() {
    const sup = fn.supplier(supplierCode);
    if (!sup) { location.search = fn.href({ tab: 'suppliers', supplier: null, sutab: null }); return; }
    hidePanels(null);
    document.querySelector('#supplier-record').hidden = false;
    document.querySelector('#sup-crumb').textContent = sup.name;
    document.querySelector('#sup-title').textContent = sup.name;
    document.querySelector('#sup-meta').innerHTML = `<span class="mono faint">${sup.code}</span> · ${sup.kind} · contract ${fn.esc(sup.contract)}`;
    document.querySelector('#sup-acts').innerHTML = `<a class="act" href="${fn.href({ 'add-session': '1', supplier: sup.code, tab: null, sutab: null })}">New session</a>
        <a class="act" href="#">Contract <span class="faint">· design pending</span></a>`;
    document.querySelectorAll('[data-sutab]').forEach((b) => b.setAttribute('aria-selected', String(b.dataset.sutab === sutab)));
    const body = document.querySelector('#sup-body');
    const detail = (rows) => `<div class="kv">${rows.map(([k, v]) => `<span class="k">${k}</span><span>${v}</span>`).join('')}</div>`;
    const sessions = fn.sessionsOfSupplier(sup.code);
    const table = `<table class="list"><thead><tr><th>Session</th><th>Adapter</th><th>Environment</th><th>Endpoint</th><th>Feeds</th><th>State</th><th class="acts"></th></tr></thead><tbody>${sessions.length ? sessions.map((s) => {
        const a = fn.adapter(s.adapter);
        return `<tr><td class="mono">${s.name}</td><td>${a.code} <span class="faint">${a.version}</span></td><td>${s.environment}</td>
          <td class="mono faint">${fn.esc(s.endpoint)}</td><td class="num">${fn.feedsOfSession(s.name).length}</td><td>${fn.state(s)}</td>
          <td class="acts"><a class="act" href="${fn.href({ session: s.name, setab: 'overview', supplier: null, sutab: null })}">Open</a></td></tr>`;
    }).join('') : '<tr><td colspan="7" class="faint">No session for this supplier yet.</td></tr>'}</tbody></table>`;

    if (sutab === 'overview') {
        body.innerHTML = `<div class="card"><div class="card-head"><h2>Supplier</h2><p>the party we contract with and connect to</p></div><div class="card-body">${detail([
            ['Code', `<span class="mono">${sup.code}</span>`],
            ['Kind', sup.kind],
            ['Contract', `<span class="mono">${fn.esc(sup.contract)}</span>`],
            ['Support', fn.esc(sup.support)],
            ['Description', fn.esc(sup.description)],
            ['Sessions', String(sessions.length)],
        ])}</div></div><div class="card" style="margin-top:12px"><div class="card-head"><h2>Sessions</h2><p>one configured adapter each</p></div>${table}</div>`;
    } else if (sutab === 'sessions') {
        body.innerHTML = `<div class="card"><div class="card-head"><h2>Sessions</h2><p>test beside production, or one per region</p>
          <span class="filters"><a class="act" href="${fn.href({ 'add-session': '1', supplier: sup.code, sutab: null, tab: null })}">New session</a></span></div>${table}</div>`;
    } else {
        body.innerHTML = `<div class="card"><div class="card-head"><h2>History</h2><p>the supplier and its sessions</p></div><div class="card-body"><div class="timeline">
          <div class="row"><span class="at">2026-10-01 09:00</span><span class="what"><b>Supplier created</b> · v1 · by super_admin</span></div>
          <div class="row"><span class="at">2026-10-20 11:12</span><span class="what"><b>Contract recorded</b> · ${fn.esc(sup.contract)} · by super_admin</span></div>
        </div></div></div>`;
    }
}

function renderSessionRecord() {
    const s = fn.session(sessionName);
    if (!s) { location.search = fn.href({ tab: 'suppliers', session: null, setab: null }); return; }
    const sup = fn.supplier(s.supplier);
    const a = fn.adapter(s.adapter);
    hidePanels(null);
    document.querySelector('#session-record').hidden = false;
    document.querySelector('#ses-crumb').textContent = s.name;
    document.querySelector('#ses-title').textContent = s.name;
    document.querySelector('#ses-meta').innerHTML = `<span class="mono faint">${s.supplier}</span> · ${a.code} ${a.version} · ${s.environment} · ${fn.state(s)}`;
    document.querySelector('#ses-acts').innerHTML = `<a class="act" href="#">Edit <span class="faint">· design pending</span></a>
        <a class="act" href="#">${s.enabled ? 'Stop' : 'Start'}</a>`;
    document.querySelectorAll('[data-setab]').forEach((b) => b.setAttribute('aria-selected', String(b.dataset.setab === setab)));
    const body = document.querySelector('#ses-body');
    const detail = (rows) => `<div class="kv">${rows.map(([k, v]) => `<span class="k">${k}</span><span>${v}</span>`).join('')}</div>`;
    const feeds = fn.feedsOfSession(s.name);

    if (setab === 'overview') {
        body.innerHTML = `<div class="card"><div class="card-head"><h2>Session</h2><p>one configured adapter at ${fn.esc(sup.name)}</p></div><div class="card-body">${detail([
            ['Name', `<span class="mono">${s.name}</span>`],
            ['Supplier', `<a href="${fn.href({ supplier: sup.code, sutab: 'overview', session: null })}">${fn.esc(sup.name)}</a>`],
            ['Adapter', `<span class="mono">${s.adapter}</span> ${a.version}`],
            ['Environment', s.environment],
            ['Endpoint', `<span class="mono">${fn.esc(s.endpoint)}</span>`],
            ['Credential reference', `<span class="mono">${fn.esc(s.credential)}</span>`],
            ['Services', s.services.join(', ')],
            ['Enabled', s.enabled ? 'yes' : 'no'],
            ['Description', fn.esc(s.description)],
        ])}<div class="faint" style="margin-top:8px">The endpoint, the credential and the settings are entered once here. A feed adds only its publisher, service, key, scope and schedule.</div></div></div>
        <div class="card" style="margin-top:12px"><div class="card-head"><h2>Settings</h2><p>the fields the adapter schema asks for</p></div>
          <table class="list"><thead><tr><th>Field</th><th>Type</th><th>Required</th><th>Value</th></tr></thead><tbody>${a.fields.length ? a.fields.map(([f, label, req, ex]) =>
            `<tr><td class="mono">${f}</td><td>${fn.esc(label)}</td><td>${req ? 'yes' : 'no'}</td><td class="mono">${fn.esc(s.settings[f] ?? ex)}</td></tr>`).join('') : '<tr><td colspan="4" class="faint">This adapter needs nothing configured.</td></tr>'}</tbody></table></div>`;
    } else if (setab === 'feeds') {
        body.innerHTML = `<div class="card"><div class="card-head"><h2>Feeds</h2><p>what comes off this session</p>
          <span class="filters"><a class="act" href="${fn.href({ 'add-feed': '1', session: s.name, setab: null, tab: null })}">Add feed</a></span></div>
          <table class="list"><thead><tr><th>Feed</th><th>Publisher</th><th>Service</th><th>Provider key</th><th>Scope</th><th>Schedule</th><th class="acts"></th></tr></thead><tbody>${feeds.length ? feeds.map((f) => {
            const p = fn.publisher(f.publisher);
            return `<tr><td class="mono">${f.name}${f.isDefault ? ' <span class="badge">default</span>' : ''}</td><td>${fn.esc(p.name)}</td>
              <td>${f.service}</td><td class="mono">${fn.esc(f.providerKey)}</td>
              <td>${f.scope.mode === 'all' ? 'all' : `${f.scope.items.length} selected`}</td>
              <td class="faint">${f.schedule.enabled ? fn.esc(f.schedule.rhythm) : 'off'}</td>
              <td class="acts"><a class="act" href="${fn.href({ feed: f.name, ftab: 'overview', session: null, setab: null })}">Open</a></td></tr>`;
          }).join('') : '<tr><td colspan="7" class="faint">No feed comes off this session yet.</td></tr>'}</tbody></table></div>`;
    } else if (setab === 'runtime') {
        body.innerHTML = `<div class="card"><div class="card-head"><h2>Runtime</h2><p>the session, as the service sees it</p></div><div class="card-body">${detail([
            ['Adapter id', `<span class="mono">${s.name}</span>`],
            ['State', s.runtime.state],
            ['Heartbeat', `<span class="mono">${s.runtime.hb}</span>`],
            ['Ids served', String(s.runtime.served)],
            ['Ids refused', String(s.runtime.refused)],
            ['Ids unknown', String(s.runtime.unknown)],
            ['Last tick', s.runtime.lastTick ?? 'never'],
        ])}<div class="faint" style="margin-top:8px">Health is per session. A publisher that is late while the session serves is a data gap, and Public rates reads it.</div></div></div>`;
    } else {
        body.innerHTML = `<div class="card"><div class="card-head"><h2>History</h2><p>the shared timeline</p></div><div class="card-body"><div class="timeline">
          <div class="row"><span class="at">2026-10-01 09:00</span><span class="what"><b>Session created</b> · v1 · by super_admin</span></div>
          <div class="row"><span class="at">2026-10-20 11:12</span><span class="what"><b>Endpoint changed</b> · ${fn.esc(s.endpoint)} · by super_admin</span></div>
          <div class="row"><span class="at">2026-10-28 09:00</span><span class="what"><b>Credential rotated</b> · ${fn.esc(s.credential)} · by super_admin</span></div>
        </div></div></div>`;
    }
}

function renderFeedRecord() {
    const f = fn.feed(feedName);
    if (!f) { location.search = fn.href({ tab: 'feeds', feed: null, ftab: null }); return; }
    const p = fn.publisher(f.publisher);
    const s = fn.session(f.session);
    const sup = fn.supplier(s.supplier);
    const a = fn.adapter(s.adapter);
    hidePanels(null);
    document.querySelector('#feed-record').hidden = false;
    document.querySelector('#fd-crumb').textContent = f.name;
    document.querySelector('#fd-title').textContent = f.name;
    document.querySelector('#fd-meta').innerHTML = `<span class="mono faint">${f.publisher}</span> · ${f.session} (${a.code}) · ${f.service} · ${f.isDefault ? 'default' : 'not default'} · ${fn.feedState(f)}`;
    document.querySelector('#fd-acts').innerHTML = `<a class="act" href="#">Edit <span class="faint">· design pending</span></a>
        <a class="act" href="#">${f.schedule.enabled ? 'Disable schedule' : 'Enable schedule'}</a>`;
    document.querySelectorAll('[data-ftab]').forEach((b) => b.setAttribute('aria-selected', String(b.dataset.ftab === ftab)));
    const body = document.querySelector('#fd-body');
    const detail = (rows) => `<div class="kv">${rows.map(([k, v]) => `<span class="k">${k}</span><span>${v}</span>`).join('')}</div>`;

    if (ftab === 'overview') {
        body.innerHTML = `<div class="card"><div class="card-head"><h2>Feed</h2><p>one publisher on one session and one service</p></div><div class="card-body">${detail([
            ['Name', `<span class="mono">${f.name}</span>`],
            ['Publisher', `${fn.esc(p.name)} <span class="faint mono">${p.code}</span>`],
            ['Supplier', fn.esc(sup.name)],
            ['Session', `<a class="mono" href="${fn.href({ session: f.session, setab: 'overview', feed: null })}">${f.session}</a> <span class="faint">${a.code} ${a.version} · ${s.environment}</span>`],
            ['Service', f.service],
            ['Provider key', `<span class="mono">${fn.esc(f.providerKey)}</span>`],
            ['Default', f.isDefault ? 'yes' : 'no'],
            ['Asset classes', f.classes.map((c) => c.toUpperCase()).join(', ')],
            ['Description', fn.esc(f.description)],
        ])}<div class="faint" style="margin-top:8px">The session owns the endpoint and the credential. The feed owns the publisher, the service, the provider key, the scope and the schedule.</div></div></div>`;
    } else if (ftab === 'underlyings') {
        const rows = f.scope.items.map((item) => `<tr><td class="mono">${fn.esc(item)}</td><td>${f.classes.map((c) => c.toUpperCase()).join(', ')}</td>
            <td>${f.scope.mode === 'all' ? '<span class="badge ok">all</span>' : '<span class="badge ok">selected</span>'}</td></tr>`).join('');
        body.innerHTML = `<div class="card"><div class="card-head"><h2>Underlyings</h2><p>what this feed fetches for ${p.code}</p></div>
          <table class="list"><thead><tr><th>Item</th><th>Class</th><th>Included</th></tr></thead><tbody>${rows || '<tr><td colspan="3" class="faint">No underlying is selected yet.</td></tr>'}</tbody></table>
          <div class="card-body"><div class="faint">All items of a class, or a chosen set. No item belongs to two feeds of one publisher. Public rates reads this selection; it does not change it.</div></div></div>`;
    } else if (ftab === 'schedule') {
        body.innerHTML = `<div class="card"><div class="card-head"><h2>Schedule</h2><p>the job that asks the session to fetch this feed</p></div><div class="card-body">${detail([
            ['Enabled', f.schedule.enabled ? 'yes' : 'no'],
            ['Rhythm', fn.esc(f.schedule.rhythm)],
            ['Scheduler job', `<span class="mono">${fn.esc(f.schedule.job)}</span>`],
            ['Next run', f.schedule.next],
            ['Last run', fn.esc(f.schedule.last)],
        ])}<div class="faint" style="margin-top:8px">The service decides the shape: end_of_day is scheduled, real_time is demand driven, historical is a backfill. The Fetch button in Public rates sends the same request now.</div></div></div>`;
    } else if (ftab === 'routes') {
        const others = FEEDS.filter((x) => x.publisher === f.publisher && x.name !== f.name);
        body.innerHTML = `<div class="card"><div class="card-head"><h2>Routes</h2><p>the party that reads this feed, and for which class</p></div><div class="card-body">
          ${f.routes.length ? `<table class="list"><thead><tr><th>Party</th><th>Publisher</th><th>Asset class</th><th>Feed</th><th>Since</th></tr></thead><tbody>${f.routes.map((r) =>
            `<tr><td>${r.party}</td><td class="mono">${f.publisher}</td><td>${r.class.toUpperCase()}</td><td class="mono">${f.name}</td><td class="when">2026-10-28</td></tr>`).join('')}</tbody></table>`
            : '<div class="notice">No party routes this feed. It is configured but not read.</div>'}
          <div class="faint" style="margin-top:8px">A party routes one feed for each publisher and asset class. A supplier switch is one new route.</div></div></div>
          ${others.length ? `<div class="card" style="margin-top:12px"><div class="card-head"><h2>Other feeds of ${p.code}</h2><p>another supplier, or another service</p></div>
          <table class="list"><thead><tr><th>Feed</th><th>Supplier</th><th>Session</th><th>Service</th><th>Default</th><th class="acts"></th></tr></thead><tbody>${others.map((x) => {
            const xs = fn.session(x.session); const xsup = fn.supplier(xs.supplier);
            return `<tr><td class="mono">${x.name}</td><td>${fn.esc(xsup.name)}</td><td class="mono">${x.session}</td><td>${x.service}</td>
              <td>${x.isDefault ? 'yes' : '<span class="faint">no</span>'}</td>
              <td class="acts"><a class="act" href="${fn.href({ feed: x.name, ftab: 'overview', tab: null })}">Open</a></td></tr>`;
          }).join('')}</tbody></table></div>` : ''}`;
    } else {
        body.innerHTML = `<div class="card"><div class="card-head"><h2>History</h2><p>the shared timeline</p></div><div class="card-body"><div class="timeline">
          <div class="row"><span class="at">2026-10-01 09:00</span><span class="what"><b>Feed created</b> · on ${f.session} · by super_admin</span></div>
          <div class="row"><span class="at">2026-10-20 11:12</span><span class="what"><b>Scope set</b> · ${f.scope.mode === 'all' ? 'all items of the class' : `${f.scope.items.length} items`} · by super_admin</span></div>
          <div class="row"><span class="at">2026-10-28 09:00</span><span class="what"><b>Schedule set</b> · ${fn.esc(f.schedule.rhythm)} · by super_admin</span></div>
        </div></div></div>`;
    }
}

function renderAddSession() {
    hidePanels(null);
    document.querySelector('#add-session').hidden = false;
    const sup = fn.supplier(supplierCode) ?? SUPPLIERS[0];
    const a = ADAPTERS[0];
    document.querySelector('#add-session-body').innerHTML = `<div class="card"><div class="card-head"><h2>1. Supplier and adapter</h2><p>the adapter list is the code the release provides</p></div>
      <div class="card-body"><div class="fields">
        <label>Supplier<select>${SUPPLIERS.map((x) => `<option ${x.code === sup.code ? 'selected' : ''}>${fn.esc(x.name)}</option>`).join('')}</select></label>
        <label>Adapter<select>${ADAPTERS.map((x) => `<option>${x.code} ${x.version} — ${fn.esc(x.description)}</option>`).join('')}</select></label>
        <label>Name<input value="frankfurter-eu"></label>
        <label>Environment<select>${ENVS.map((e) => `<option>${e}</option>`).join('')}</select></label>
      </div></div></div>
      <div class="card" style="margin-top:12px"><div class="card-head"><h2>2. Endpoint, credential and settings</h2><p>entered once; every feed off this session shares them</p></div>
      <div class="card-body"><div class="fields">
        <label>Endpoint<input value="${fn.esc(a.fields[0]?.[3] ?? 'none')}"></label>
        <label>Credential reference<input value="none"></label>
        ${a.fields.slice(1).map(([f, label, , ex]) => `<label>${fn.esc(label)}<input value="${fn.esc(ex)}"></label>`).join('')}
      </div><div class="faint">A secret is a reference. The value is encrypted at rest and never returned by a read.</div></div></div>
      <div class="card" style="margin-top:12px"><div class="card-body"><div class="foot">
        <a class="act" href="${fn.href({ tab: 'suppliers', 'add-session': null, supplier: null })}">Cancel</a>
        <a class="act" href="#">Test connection <span class="faint">· design pending</span></a>
        <a class="act primary" href="${fn.href({ tab: 'suppliers', 'add-session': null, supplier: null })}">Create session <span class="faint">· design pending</span></a>
      </div></div></div>`;
}

function renderAddFeed() {
    hidePanels(null);
    document.querySelector('#add-feed').hidden = false;
    const p = fn.publisher(publisherCode) ?? PUBLISHERS[0];
    const fits = SESSIONS.filter((s) => s.enabled && fn.adapter(s.adapter).classes.some((c) => p.classes.includes(c)));
    document.querySelector('#add-feed-body').innerHTML = `<div class="card"><div class="card-head"><h2>1. Publisher, session and service</h2><p>the session is the supplier's; the feed is ours</p></div>
      <div class="card-body"><div class="fields">
        <label>Publisher<select>${PUBLISHERS.map((x) => `<option ${x.code === p.code ? 'selected' : ''}>${fn.esc(x.name)}</option>`).join('')}</select></label>
        <label>Session<select>${fits.map((s) => `<option>${s.name} — ${s.supplier} · ${s.environment}</option>`).join('')}</select></label>
        <label>Service<select>${SERVICES.map((s) => `<option>${s}</option>`).join('')}</select></label>
        <label>Provider key<input value="${p.code.toUpperCase()}"></label>
        <label>Default<select><option>No</option><option>Yes</option></select></label>
      </div><div class="faint">Only an enabled session whose adapter fits the publisher's classes is offered.</div></div></div>
      <div class="card" style="margin-top:12px"><div class="card-head"><h2>2. Underlyings</h2><p>what this feed fetches</p></div>
      <div class="card-body"><div class="fields">
        <label>Scope<select><option>All items of the class</option><option>A chosen set</option></select></label>
        <label>Asset class<select>${p.classes.map((c) => `<option>${c.toUpperCase()}</option>`).join('')}</select></label>
      </div><div class="faint">No item may belong to two feeds of ${p.code}.</div></div></div>
      <div class="card" style="margin-top:12px"><div class="card-head"><h2>3. Schedule and route</h2><p>when to fetch, and which party reads it</p></div>
      <div class="card-body"><div class="fields">
        <label>Rhythm<select><option>Daily at 16:00 CET</option><option>Continuous, demand driven</option><option>On demand</option></select></label>
        <label>Party<select><option>System Party</option></select></label>
        <label>Asset class<select>${p.classes.map((c) => `<option>${c.toUpperCase()}</option>`).join('')}</select></label>
      </div><div class="faint">The scheduler job is created for the feed. The route is one per party, publisher and class.</div></div></div>
      <div class="card" style="margin-top:12px"><div class="card-body"><div class="foot">
        <a class="act" href="${fn.href({ tab: 'feeds', 'add-feed': null, publisher: null })}">Cancel</a>
        <a class="act primary" href="${fn.href({ tab: 'feeds', 'add-feed': null, publisher: null })}">Create feed <span class="faint">· design pending</span></a>
      </div></div></div>`;
}

function wire() {
    document.querySelectorAll('[data-tab]').forEach((b) => b.addEventListener('click', () => {
        location.search = fn.href({ tab: b.dataset.tab, publisher: null, pbtab: null, supplier: null, sutab: null,
            session: null, setab: null, feed: null, ftab: null, 'add-session': null, 'add-feed': null });
    }));
    document.querySelectorAll('[data-pbtab]').forEach((b) => b.addEventListener('click', () => {
        location.search = fn.href({ publisher: publisherCode, pbtab: b.dataset.pbtab, tab: null, 'add-feed': null });
    }));
    document.querySelectorAll('[data-sutab]').forEach((b) => b.addEventListener('click', () => {
        location.search = fn.href({ supplier: supplierCode, sutab: b.dataset.sutab, tab: null, 'add-session': null });
    }));
    document.querySelectorAll('[data-setab]').forEach((b) => b.addEventListener('click', () => {
        location.search = fn.href({ session: sessionName, setab: b.dataset.setab, tab: null, 'add-feed': null });
    }));
    document.querySelectorAll('[data-ftab]').forEach((b) => b.addEventListener('click', () => {
        location.search = fn.href({ feed: feedName, ftab: b.dataset.ftab, tab: null });
    }));
    document.querySelector('#refresh').addEventListener('click', () => location.reload());
}

if (addSession) renderAddSession();
else if (addFeed) renderAddFeed();
else if (feedName) renderFeedRecord();
else if (sessionName) renderSessionRecord();
else if (supplierCode) renderSupplierRecord();
else if (publisherCode) renderPublisherRecord();
else if (tab === 'suppliers') renderTree();
else if (tab === 'adapters') renderAdapters();
else if (tab === 'feeds') renderFeeds();
else renderPublishers();
wire();
