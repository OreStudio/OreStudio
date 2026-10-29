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

const plural = (n, w) => `${n} ${w}${n === 1 ? '' : 's'}`;

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

function esc(t) { return fn.esc(t); }

function itemsLink(cls, href, label, meta) {
    return `<a class="${cls}" href="${href}">${label}</a>${meta ? ` <span class="faint">${meta}</span>` : ''}`;
}

function renderTree() {
    const sel = selectedNode();
    document.querySelector('#tree').innerHTML = SUPPLIERS.map((sup) => {
        const sessions = fn.sessionsOfSupplier(sup.code);
        const feeds = sessions.reduce((n, x) => n + fn.feedsOfSession(x.name).length, 0);
        const open = sel && (sel.type === 'supplier' && sel.id === sup.code
            || sel.type === 'session' && fn.session(sel.id)?.supplier === sup.code
            || sel.type === 'feed' && fn.session(fn.feed(sel.id)?.session)?.supplier === sup.code);
        return `<details ${open ? 'open' : ''}>
          <summary><span class="node"><a class="${sel && sel.type === 'supplier' && sel.id === sup.code ? 'sel' : ''}"
            href="?supplier=${sup.code}">${esc(sup.name)}</a>
            <span class="faint">${sup.kind} · ${plural(sessions.length, 'session')} · ${plural(feeds, 'feed')}</span></span></summary>
          <ul>${sessions.map((x) => {
            const a = fn.adapter(x.adapter);
            const fds = fn.feedsOfSession(x.name);
            const sopen = sel && (sel.type === 'session' && sel.id === x.name || sel.type === 'feed' && fn.feed(sel.id)?.session === x.name);
            return `<li><details ${sopen ? 'open' : ''}>
              <summary><span class="node"><a class="${sel && sel.type === 'session' && sel.id === x.name ? 'sel' : ''}"
                href="?session=${x.name}">${x.name}</a>
                <span class="faint">${a.code} · ${x.environment} · ${x.runtime.state}</span></span></summary>
              <ul>${fds.length ? fds.map((f) => `<li><a class="${sel && sel.type === 'feed' && sel.id === f.name ? 'sel' : ''}"
                href="?feed=${f.name}">${f.name}</a> <span class="faint">→ ${fn.publisher(f.publisher).name} · ${f.service}</span></li>`).join('')
                : '<li class="faint">no feed yet</li>'}</ul>
            </details></li>`;
          }).join('') || '<li class="faint">no session yet</li>'}</ul>
        </details>`;
    }).join('');

    document.querySelector('#pubtree').innerHTML = PUBLISHERS.map((p) => {
        const feeds = fn.feedsOfPublisher(p.code);
        const open = sel && (sel.type === 'publisher' && sel.id === p.code || sel.type === 'feed' && fn.feed(sel.id)?.publisher === p.code);
        return `<details ${open ? 'open' : ''}>
          <summary><span class="node"><a class="${sel && sel.type === 'publisher' && sel.id === p.code ? 'sel' : ''}"
            href="?publisher=${p.code}">${esc(p.name)}</a>
            <span class="faint">${p.classes.map((c) => c.toUpperCase()).join('/')} · ${plural(feeds.length, 'feed')}</span></span></summary>
          <ul>${feeds.length ? feeds.map((f) => `<li><a class="${sel && sel.type === 'feed' && sel.id === f.name ? 'sel' : ''}"
            href="?feed=${f.name}">${f.name}</a> <span class="faint">${f.service} · ${fn.supplier(fn.session(f.session).supplier).name}</span></li>`).join('')
            : '<li class="faint">no feed yet</li>'}</ul>
        </details>`;
    }).join('');
}

function selectedNode() {
    if (feedName) return { type: 'feed', id: feedName };
    if (sessionName) return { type: 'session', id: sessionName };
    if (supplierCode) return { type: 'supplier', id: supplierCode };
    if (publisherCode) return { type: 'publisher', id: publisherCode };
    return null;
}

function paneHeader(crumbs, title, meta, actions) {
    return `<div class="pane-head">
      <div class="crumbs">${crumbs}</div>
      <div><h1>${title}</h1><div class="meta">${meta}</div></div>
      <span class="actions">${actions}</span>
    </div>`;
}

function tabLinks(base, links, current) {
    return `<nav class="tabs" role="tablist">${links.map(([id, label]) =>
        `<a class="act ${id === current ? 'primary' : ''}" href="${base}&tab=${id}">${label}</a>`).join('')}</nav>`;
}

const detail = (rows) => `<div class="kv">${rows.map(([k, v]) => `<span class="k">${k}</span><span>${v}</span>`).join('')}</div>`;

function renderDefault() {
    const feeds = FEEDS.length, sessions = SESSIONS.length;
    document.querySelector('#pane').innerHTML = paneHeader('Connectivity',
        'Delivery overview',
        'pick a supplier, session or feed on the left; the pane shows its list or its details',
        `<a class="act" href="?view=adapters">Adapters</a>`)
        + `<div class="card"><div class="card-head"><h2>What Connectivity holds</h2><p>the delivery side of market data</p></div>
          <div class="card-body"><div class="kv">
            <span class="k">Publishers</span><span>${PUBLISHERS.length}</span>
            <span class="k">Suppliers</span><span>${SUPPLIERS.length}</span>
            <span class="k">Sessions</span><span>${sessions}</span>
            <span class="k">Feeds</span><span>${feeds}</span>
            <span class="k">Adapters</span><span>${ADAPTERS.length}</span>
          </div>
          <div class="faint" style="margin-top:8px">The tree shows how delivery works: supplier → session → feed. The Publishers tree shows the other axis: a publisher's feeds across suppliers.</div></div></div>`;
}

function renderAdaptersPane() {
    document.querySelector('#pane').innerHTML = paneHeader('Connectivity / Adapters',
        'Adapters', 'the code modules the release provides; read only',
        `<a class="act" href="?">Back to delivery</a>`)
        + `<div class="card"><div class="card-head"><h2>Adapter catalogue</h2><p>version, services and the configuration a session must provide</p></div>
          <table class="list"><thead><tr><th>Adapter</th><th>Version</th><th>Description</th><th>Classes</th><th>Services</th><th>Schema</th><th>Sessions</th></tr></thead><tbody>${ADAPTERS.map((a) => {
            const sessions = SESSIONS.filter((x) => x.adapter === a.code);
            return `<tr><td class="provider">${esc(a.name)}<div class="faint mono">${a.code} · ${a.process}</div></td>
              <td class="mono">${a.version}</td><td>${esc(a.description)}</td>
              <td>${a.classes.map((c) => `<span class="badge">${c.toUpperCase()}</span>`).join(' ')}</td>
              <td class="faint mono">${a.services.join(', ')}</td>
              <td class="faint">${a.fields.length ? a.fields.map(([f]) => `<span class="mono">${f}</span>`).join(', ') : 'nothing'}</td>
              <td class="num">${sessions.length}</td></tr>`;
          }).join('')}</tbody></table>
          <div class="card-body faint">A person configures a session of an adapter, then the feeds that come off it. Nobody adds an adapter from a screen.</div></div>`;
}

function renderSupplierPane() {
    const sup = fn.supplier(supplierCode);
    if (!sup) { location.search = '?'; return; }
    const sessions = fn.sessionsOfSupplier(sup.code);
    const feeds = sessions.flatMap((x) => fn.feedsOfSession(x.name));
    const publishers = [...new Set(feeds.map((f) => f.publisher))];
    const serving = sessions.filter((x) => x.runtime.state === 'serving').length;
    const services = [...new Set(sessions.flatMap((x) => x.services))];
    const lastDelivery = sessions.map((x) => x.runtime.lastTick).filter(Boolean).sort().pop() ?? '—';

    const alerts = [];
    sessions.forEach((x) => {
        if (x.runtime.state !== 'serving') alerts.push(['bad', `Session ${x.name} is not serving`, `?session=${x.name}&tab=runtime`]);
    });
    feeds.forEach((f) => {
        if (f.schedule.last.includes('failed') || f.schedule.last.includes('late'))
            alerts.push(['warn', `${f.name}: last run ${f.schedule.last}`, `?feed=${f.name}&tab=schedule`]);
        if (!f.routes.length) alerts.push(['warn', `${f.name} is configured but not routed`, `?feed=${f.name}&tab=routes`]);
    });
    publishers.forEach((code) => {
        const p = fn.publisher(code);
        if (!p.terms.accepted) alerts.push(['warn', `${p.name} terms are not accepted`, `?publisher=${p.code}`]);
    });

    const kpis = `<div class="strip">
        <div class="metric"><span class="label">Sessions</span><div class="v">${sessions.length}</div><span class="sub">${serving} serving</span></div>
        <div class="metric"><span class="label">Feeds</span><div class="v">${feeds.length}</div><span class="sub">${feeds.filter((f) => f.schedule.enabled).length} scheduled</span></div>
        <div class="metric"><span class="label">Publishers</span><div class="v">${publishers.length}</div><span class="sub">aggregated by ${sup.name}</span></div>
        <div class="metric"><span class="label">Services</span><div class="v">${services.length}</div><span class="sub">${services.join(', ')}</span></div>
        <div class="metric"><span class="label">Attention</span><div class="v ${alerts.length ? 'warn' : 'up'}">${alerts.length}</div><span class="sub">${alerts.length ? 'see the list below' : 'nothing needs a person'}</span></div>
        <div class="metric"><span class="label">Last delivery</span><div class="v">${lastDelivery}</div><span class="sub">from any session</span></div>
      </div>`;

    const sessionsCard = `<div class="card"><div class="card-head"><h2>Sessions</h2><p>where the adapter runs, and whether it serves</p></div>
      <table class="list"><thead><tr><th>Session</th><th>Env</th><th>Adapter</th><th>State</th><th>Heartbeat</th><th>Feeds</th><th class="acts"></th></tr></thead><tbody>${sessions.length ? sessions.map((x) => {
        const a = fn.adapter(x.adapter);
        return `<tr><td class="mono"><a href="?session=${x.name}">${x.name}</a></td><td>${x.environment}</td>
          <td class="mono">${a.code} ${a.version}</td><td>${fn.state(x)}</td><td class="when">${x.runtime.hb}</td>
          <td class="num">${fn.feedsOfSession(x.name).length}</td>
          <td class="acts"><a class="act" href="?session=${x.name}">Open</a></td></tr>`;
      }).join('') : '<tr><td colspan="7" class="faint">No session.</td></tr>'}</tbody></table></div>`;

    const coverageCard = `<div class="card"><div class="card-head"><h2>Coverage</h2><p>publishers served, and what they carry</p></div>
      <table class="list"><thead><tr><th>Publisher</th><th>Classes</th><th>Feeds</th><th>Service</th></tr></thead><tbody>${publishers.length ? publishers.map((code) => {
        const p = fn.publisher(code);
        const fds = feeds.filter((f) => f.publisher === code);
        return `<tr><td class="mono"><a href="?publisher=${p.code}">${p.code}</a></td>
          <td>${p.classes.map((c) => c.toUpperCase()).join(', ')}</td><td class="num">${fds.length}</td>
          <td class="faint">${[...new Set(fds.map((f) => f.service))].join(', ')}</td></tr>`;
      }).join('') : '<tr><td colspan="4" class="faint">No publisher is served.</td></tr>'}</tbody></table></div>`;

    const attentionCard = `<div class="card"><div class="card-head"><h2>Needs attention</h2><p>what a person would look at first</p></div>
      <div class="card-body">${alerts.length ? alerts.map(([tone, text, href]) =>
        `<div class="alert"><span class="badge ${tone}">${tone === 'bad' ? 'down' : 'warning'}</span><a href="${href}">${esc(text)}</a></div>`).join('')
        : '<div class="faint">Nothing. Every session serves, every feed has run, and every feed is routed.</div>'}</div></div>`;

    const feedsCard = `<div class="card full"><div class="card-head"><h2>Feeds</h2><p>each publisher's data, its service, its rhythm and its last delivery</p>
        <span class="filters"><a class="act" href="?add-feed=1&supplier=${sup.code}">Add a feed</a></span></div>
      <table class="list"><thead><tr><th>Feed</th><th>Publisher</th><th>Service</th><th>Schedule</th><th>Last run</th><th>Last delivery</th><th>Session</th><th class="acts"></th></tr></thead><tbody>${feeds.length ? feeds.map((f) => {
        const p = fn.publisher(f.publisher);
        const trouble = f.schedule.last.includes('failed') || f.schedule.last.includes('late');
        return `<tr><td class="mono"><a href="?feed=${f.name}">${f.name}</a>${f.isDefault ? ' <span class="badge">default</span>' : ''}</td>
          <td>${esc(p.name)}</td><td>${f.service}</td>
          <td class="faint">${f.schedule.enabled ? esc(f.schedule.rhythm) : 'off'}</td>
          <td class="when">${esc(f.schedule.last)}${trouble ? ' <span class="badge warn">late</span>' : ''}</td>
          <td class="when">${f.last ?? '—'}</td><td>${fn.feedState(f)}</td>
          <td class="acts"><a class="act" href="?feed=${f.name}">Open</a></td></tr>`;
      }).join('') : '<tr><td colspan="8" class="faint">No feed.</td></tr>'}</tbody></table></div>`;

    const contractCard = `<div class="card"><div class="card-head"><h2>Contract</h2><p>who we deal with, and where to ask</p></div><div class="card-body">${detail([
        ['Kind', sup.kind],
        ['Contract', `<span class="mono">${esc(sup.contract)}</span>`],
        ['Support', esc(sup.support)],
        ['Description', esc(sup.description)],
        ['Sessions', `${sessions.length} (${serving} serving)`],
    ])}</div></div>`;

    const activityCard = `<div class="card full"><div class="card-head"><h2>Recent activity</h2><p>the shared timeline, newest first</p></div><div class="card-body"><div class="timeline">
        <div class="row"><span class="at">2026-10-28 19:24</span><span class="what"><b>${feeds[0]?.name ?? 'feed'} delivered</b> · by adapter</span></div>
        <div class="row"><span class="at">2026-10-28 09:00</span><span class="what"><b>Credential rotated</b> · by super_admin</span></div>
        <div class="row"><span class="at">2026-10-20 11:12</span><span class="what"><b>Endpoint changed</b> · by super_admin</span></div>
        <div class="row"><span class="at">2026-10-01 09:00</span><span class="what"><b>Supplier created</b> · by super_admin</span></div>
      </div></div></div>`;

    const headline = sup.kind === 'aggregator'
        ? `aggregates ${plural(publishers.length, 'publisher')} over ${plural(sessions.length, 'session')}`
        : `${sup.kind} · ${plural(sessions.length, 'session')} · ${plural(feeds.length, 'feed')}`;

    document.querySelector('#pane').innerHTML = paneHeader(
        `Connectivity / Suppliers / ${esc(sup.name)}`, esc(sup.name),
        `${esc(headline)} · contract ${esc(sup.contract)} · ${alerts.length ? `${alerts.length} needing attention` : 'all clear'}`,
        `<a class="act primary" href="?add-session=1&supplier=${sup.code}">New session</a><a class="act" href="#">Contract</a>`)
        + kpis
        + `<div class="dash">${sessionsCard}${coverageCard}${attentionCard}${contractCard}${feedsCard}${activityCard}</div>`;
}

function renderPublisherPane() {
    const p = fn.publisher(publisherCode);
    if (!p) { location.search = '?'; return; }
    const feeds = fn.feedsOfPublisher(p.code);
    const suppliers = [...new Set(feeds.map((f) => fn.session(f.session).supplier))];
    document.querySelector('#pane').innerHTML = paneHeader(
        `Connectivity / Publishers / ${esc(p.name)}`, esc(p.name),
        `${p.kind} · ${p.classes.map((c) => c.toUpperCase()).join(', ')} · ${plural(feeds.length, 'feed')}`,
        `<a class="act primary" href="?add-feed=1&publisher=${p.code}">Add a feed</a><a class="act" href="#">Terms</a>`)
        + `<div class="card"><div class="card-head"><h2>Publisher</h2><p>the content owner and its terms</p></div>
          <div class="card-body">${detail([
            ['Code', `<span class="mono">${p.code}</span>`],
            ['Kind', p.kind],
            ['Asset classes', p.classes.map((c) => c.toUpperCase()).join(', ')],
            ['Terms', p.terms.accepted ? `accepted ${p.terms.version} by ${p.terms.who} · ${p.terms.at}` : 'not accepted'],
            ['Suppliers', suppliers.map((id) => fn.supplier(id).name).join(', ') || 'none'],
          ])}</div></div>
          <div class="card"><div class="card-head"><h2>Feeds</h2><p>every path to this publisher's values</p></div>
          <table class="list"><thead><tr><th>Feed</th><th>Supplier</th><th>Session</th><th>Service</th><th>Classes</th><th>Default</th><th class="acts"></th></tr></thead><tbody>${feeds.length ? feeds.map((f) => {
            const s = fn.session(f.session); const sup = fn.supplier(s.supplier);
            return `<tr><td class="mono"><a href="?feed=${f.name}">${f.name}</a></td><td>${esc(sup.name)}</td><td class="mono">${f.session}</td>
              <td>${f.service}</td><td>${f.classes.map((c) => c.toUpperCase()).join(', ')}</td><td>${f.isDefault ? 'yes' : '<span class="faint">no</span>'}</td>
              <td class="acts"><a class="act" href="?feed=${f.name}">Open</a></td></tr>`;
          }).join('') : '<tr><td colspan="7" class="faint">No feed yet.</td></tr>'}</tbody></table></div>`;
}

function renderSessionPane() {
    const s = fn.session(sessionName);
    if (!s) { location.search = '?'; return; }
    const sup = fn.supplier(s.supplier);
    const a = fn.adapter(s.adapter);
    const tab = q.get('tab') ?? 'overview';
    const feeds = fn.feedsOfSession(s.name);
    const head = paneHeader(`Connectivity / Suppliers / ${esc(sup.name)} / ${s.name}`, s.name,
        `<span class="mono faint">${s.supplier}</span> · ${a.code} ${a.version} · ${s.environment} · ${fn.state(s)}`,
        `<a class="act primary" href="?add-feed=1&session=${s.name}">Add feed</a><a class="act" href="#">Edit</a><a class="act" href="#">${s.enabled ? 'Stop' : 'Start'}</a>`);
    let body = '';
    if (tab === 'feeds') {
        body = `<div class="card"><div class="card-head"><h2>Feeds</h2><p>what comes off this session</p></div>
          <table class="list"><thead><tr><th>Feed</th><th>Publisher</th><th>Service</th><th>Key</th><th>Scope</th><th>Schedule</th><th class="acts"></th></tr></thead><tbody>${feeds.length ? feeds.map((f) => {
            const p = fn.publisher(f.publisher);
            return `<tr><td class="mono"><a href="?feed=${f.name}">${f.name}</a>${f.isDefault ? ' <span class="badge">default</span>' : ''}</td>
              <td>${esc(p.name)}</td><td>${f.service}</td><td class="mono">${esc(f.providerKey)}</td>
              <td>${f.scope.mode === 'all' ? 'all' : `${f.scope.items.length} selected`}</td>
              <td class="faint">${f.schedule.enabled ? esc(f.schedule.rhythm) : 'off'}</td>
              <td class="acts"><a class="act" href="?feed=${f.name}">Open</a></td></tr>`;
          }).join('') : '<tr><td colspan="7" class="faint">No feed yet.</td></tr>'}</tbody></table></div>`;
    } else if (tab === 'runtime') {
        body = `<div class="card"><div class="card-head"><h2>Runtime</h2><p>the session, as the service sees it</p></div><div class="card-body">${detail([
            ['Adapter id', `<span class="mono">${s.name}</span>`],
            ['State', s.runtime.state],
            ['Heartbeat', `<span class="mono">${s.runtime.hb}</span>`],
            ['Ids served', String(s.runtime.served)],
            ['Ids refused', String(s.runtime.refused)],
            ['Ids unknown', String(s.runtime.unknown)],
            ['Last tick', s.runtime.lastTick ?? 'never'],
        ])}<div class="faint" style="margin-top:8px">Health is per session. A publisher that is late while the session serves is a data gap; Public rates reads it.</div></div></div>`;
    } else if (tab === 'history') {
        body = `<div class="card"><div class="card-head"><h2>History</h2><p>the shared timeline</p></div><div class="card-body"><div class="timeline">
          <div class="row"><span class="at">2026-10-01 09:00</span><span class="what"><b>Session created</b> · v1 · by super_admin</span></div>
          <div class="row"><span class="at">2026-10-20 11:12</span><span class="what"><b>Endpoint changed</b> · ${esc(s.endpoint)} · by super_admin</span></div>
          <div class="row"><span class="at">2026-10-28 09:00</span><span class="what"><b>Credential rotated</b> · ${esc(s.credential)} · by super_admin</span></div>
        </div></div></div>`;
    } else {
        body = `<div class="card"><div class="card-head"><h2>Session</h2><p>the transport every feed off it shares</p></div><div class="card-body">${detail([
            ['Name', `<span class="mono">${s.name}</span>`],
            ['Supplier', `<a href="?supplier=${sup.code}">${esc(sup.name)}</a>`],
            ['Adapter', `<span class="mono">${s.adapter}</span> ${a.version}`],
            ['Environment', s.environment],
            ['Endpoint', `<span class="mono">${esc(s.endpoint)}</span>`],
            ['Credential reference', `<span class="mono">${esc(s.credential)}</span>`],
            ['Services', s.services.join(', ')],
            ['Enabled', s.enabled ? 'yes' : 'no'],
        ])}</div></div>
        <div class="card"><div class="card-head"><h2>Settings</h2><p>the fields the adapter schema asks for</p></div>
          <table class="list"><thead><tr><th>Field</th><th>Type</th><th>Required</th><th>Value</th></tr></thead><tbody>${a.fields.length ? a.fields.map(([f, label, req, ex]) =>
            `<tr><td class="mono">${f}</td><td>${esc(label)}</td><td>${req ? 'yes' : 'no'}</td><td class="mono">${esc(s.settings[f] ?? ex)}</td></tr>`).join('') : '<tr><td colspan="4" class="faint">nothing</td></tr>'}</tbody></table></div>`;
    }
    document.querySelector('#pane').innerHTML = head + tabLinks(`?session=${s.name}`, [['overview', 'Overview'], ['feeds', 'Feeds'], ['runtime', 'Runtime'], ['history', 'History']], tab) + body;
}

function renderFeedPane() {
    const f = fn.feed(feedName);
    if (!f) { location.search = '?'; return; }
    const p = fn.publisher(f.publisher);
    const s = fn.session(f.session);
    const sup = fn.supplier(s.supplier);
    const a = fn.adapter(s.adapter);
    const tab = q.get('tab') ?? 'overview';
    const head = paneHeader(`Connectivity / Suppliers / ${esc(sup.name)} / ${s.name} / ${f.name}`, f.name,
        `<span class="mono faint">${f.publisher}</span> · ${f.session} · ${f.service} · ${f.isDefault ? 'default' : 'not default'} · ${fn.feedState(f)}`,
        `<a class="act" href="#">Edit</a><a class="act" href="#">${f.schedule.enabled ? 'Disable schedule' : 'Enable schedule'}</a>`);
    let body = '';
    if (tab === 'underlyings') {
        const rows = f.scope.items.map((item) => `<tr><td class="mono">${esc(item)}</td><td>${f.classes.map((c) => c.toUpperCase()).join(', ')}</td>
            <td>${f.scope.mode === 'all' ? '<span class="badge ok">all</span>' : '<span class="badge ok">selected</span>'}</td></tr>`).join('');
        body = `<div class="card"><div class="card-head"><h2>Underlyings</h2><p>what this feed fetches for ${p.code}</p></div>
          <table class="list"><thead><tr><th>Item</th><th>Class</th><th>Included</th></tr></thead><tbody>${rows || '<tr><td colspan="3" class="faint">No item yet.</td></tr>'}</tbody></table>
          <div class="card-body faint">All items of a class, or a chosen set. No item belongs to two feeds of one publisher. Public rates reads this selection; it does not change it.</div></div>`;
    } else if (tab === 'schedule') {
        body = `<div class="card"><div class="card-head"><h2>Schedule</h2><p>the job that asks the session to fetch this feed</p></div><div class="card-body">${detail([
            ['Enabled', f.schedule.enabled ? 'yes' : 'no'],
            ['Rhythm', esc(f.schedule.rhythm)],
            ['Scheduler job', `<span class="mono">${esc(f.schedule.job)}</span>`],
            ['Next run', f.schedule.next],
            ['Last run', esc(f.schedule.last)],
        ])}<div class="faint" style="margin-top:8px">The service decides the shape: end_of_day is scheduled, real_time is demand driven, historical is a backfill. The Fetch button in Public rates sends the same request now.</div></div></div>`;
    } else if (tab === 'routes') {
        const others = FEEDS.filter((x) => x.publisher === f.publisher && x.name !== f.name);
        body = `<div class="card"><div class="card-head"><h2>Routes</h2><p>the party that reads this feed, and for which class</p></div><div class="card-body">
          ${f.routes.length ? `<table class="list"><thead><tr><th>Party</th><th>Publisher</th><th>Class</th><th>Feed</th></tr></thead><tbody>${f.routes.map((r) =>
            `<tr><td>${r.party}</td><td class="mono">${f.publisher}</td><td>${r.class.toUpperCase()}</td><td class="mono">${f.name}</td></tr>`).join('')}</tbody></table>`
            : '<div class="notice">No party routes this feed. It is configured but not read.</div>'}
          <div class="faint" style="margin-top:8px">A party routes one feed for each publisher and class. A supplier switch is one new route.</div></div></div>
          ${others.length ? `<div class="card"><div class="card-head"><h2>Other feeds of ${p.code}</h2><p>another supplier, or another service</p></div>
          <table class="list"><thead><tr><th>Feed</th><th>Supplier</th><th>Session</th><th>Service</th><th>Default</th><th class="acts"></th></tr></thead><tbody>${others.map((x) => {
            const xs = fn.session(x.session); const xsup = fn.supplier(xs.supplier);
            return `<tr><td class="mono"><a href="?feed=${x.name}">${x.name}</a></td><td>${esc(xsup.name)}</td><td class="mono">${x.session}</td><td>${x.service}</td>
              <td>${x.isDefault ? 'yes' : '<span class="faint">no</span>'}</td><td class="acts"><a class="act" href="?feed=${x.name}">Open</a></td></tr>`;
          }).join('')}</tbody></table></div>` : ''}`;
    } else if (tab === 'history') {
        body = `<div class="card"><div class="card-head"><h2>History</h2><p>the shared timeline</p></div><div class="card-body"><div class="timeline">
          <div class="row"><span class="at">2026-10-01 09:00</span><span class="what"><b>Feed created</b> · on ${f.session} · by super_admin</span></div>
          <div class="row"><span class="at">2026-10-20 11:12</span><span class="what"><b>Scope set</b> · ${f.scope.mode === 'all' ? 'all items of the class' : `${f.scope.items.length} items`} · by super_admin</span></div>
          <div class="row"><span class="at">2026-10-28 09:00</span><span class="what"><b>Schedule set</b> · ${esc(f.schedule.rhythm)} · by super_admin</span></div>
        </div></div></div>`;
    } else {
        body = `<div class="card"><div class="card-head"><h2>Feed</h2><p>one publisher on one session and one service</p></div><div class="card-body">${detail([
            ['Name', `<span class="mono">${f.name}</span>`],
            ['Publisher', `${esc(p.name)} <span class="faint mono">${p.code}</span>`],
            ['Supplier', esc(sup.name)],
            ['Session', `<a class="mono" href="?session=${f.session}">${f.session}</a> <span class="faint">${a.code} ${a.version} · ${s.environment}</span>`],
            ['Service', f.service],
            ['Provider key', `<span class="mono">${esc(f.providerKey)}</span>`],
            ['Default', f.isDefault ? 'yes' : 'no'],
            ['Asset classes', f.classes.map((c) => c.toUpperCase()).join(', ')],
            ['Description', esc(f.description)],
        ])}<div class="faint" style="margin-top:8px">The session owns the endpoint and the credential. The feed owns the publisher, the service, the key, the scope and the schedule.</div></div></div>`;
    }
    document.querySelector('#pane').innerHTML = head + tabLinks(`?feed=${f.name}`, [['overview', 'Overview'], ['underlyings', 'Underlyings'], ['schedule', 'Schedule'], ['routes', 'Routes'], ['history', 'History']], tab) + body;
}

function renderAddSession() {
    const sup = fn.supplier(supplierCode) ?? SUPPLIERS[0];
    const a = ADAPTERS[0];
    document.querySelector('#pane').innerHTML = paneHeader(`Connectivity / Suppliers / ${esc(sup.name)} / New session`, 'New session',
        'one configured adapter at a supplier: environment, endpoint, credential and settings',
        `<a class="act" href="?supplier=${sup.code}">Cancel</a>`)
        + `<div class="card"><div class="card-head"><h2>1. Supplier and adapter</h2><p>the adapter list is the code the release provides</p></div>
          <div class="card-body"><div class="fields">
            <label>Supplier<select>${SUPPLIERS.map((x) => `<option ${x.code === sup.code ? 'selected' : ''}>${esc(x.name)}</option>`).join('')}</select></label>
            <label>Adapter<select>${ADAPTERS.map((x) => `<option>${x.code} ${x.version} — ${esc(x.description)}</option>`).join('')}</select></label>
            <label>Name<input value="frankfurter-eu"></label>
            <label>Environment<select>${ENVS.map((e) => `<option>${e}</option>`).join('')}</select></label>
          </div></div></div>
          <div class="card"><div class="card-head"><h2>2. Endpoint, credential and settings</h2><p>entered once; every feed off this session shares them</p></div>
          <div class="card-body"><div class="fields">
            <label>Endpoint<input value="${esc(a.fields[0]?.[3] ?? 'none')}"></label>
            <label>Credential reference<input value="none"></label>
            ${a.fields.slice(1).map(([f, label, , ex]) => `<label>${esc(label)}<input value="${esc(ex)}"></label>`).join('')}
          </div><div class="faint">A secret is a reference. The value is encrypted at rest and never returned by a read.</div></div></div>
          <div class="card"><div class="card-body"><div class="foot">
            <a class="act" href="?supplier=${sup.code}">Cancel</a>
            <a class="act" href="#">Test connection <span class="faint">· design pending</span></a>
            <a class="act primary" href="?supplier=${sup.code}">Create session <span class="faint">· design pending</span></a>
          </div></div></div>`;
}

function renderAddFeed() {
    const p = fn.publisher(publisherCode) ?? PUBLISHERS[0];
    const fits = SESSIONS.filter((x) => x.enabled
        && fn.adapter(x.adapter).classes.some((c) => p.classes.includes(c))
        && (!supplierCode || x.supplier === supplierCode));
    document.querySelector('#pane').innerHTML = paneHeader(`Connectivity / Publishers / ${esc(p.name)} / New feed`, 'New feed',
        'a publisher, a session, a service, a scope and a schedule',
        `<a class="act" href="?publisher=${p.code}">Cancel</a>`)
        + `<div class="card"><div class="card-head"><h2>1. Publisher, session and service</h2><p>the session is the supplier's; the feed is ours</p></div>
          <div class="card-body"><div class="fields">
            <label>Publisher<select>${PUBLISHERS.map((x) => `<option ${x.code === p.code ? 'selected' : ''}>${esc(x.name)}</option>`).join('')}</select></label>
            <label>Session<select>${fits.map((x) => `<option ${x.name === sessionName ? 'selected' : ''}>${x.name} — ${x.supplier} · ${x.environment}</option>`).join('')}</select></label>
            <label>Service<select>${SERVICES.map((x) => `<option>${x}</option>`).join('')}</select></label>
            <label>Provider key<input value="${p.code.toUpperCase()}"></label>
            <label>Default<select><option>No</option><option>Yes</option></select></label>
          </div><div class="faint">Only an enabled session whose adapter fits the publisher's classes is offered.</div></div></div>
          <div class="card"><div class="card-head"><h2>2. Underlyings</h2><p>what this feed fetches</p></div>
          <div class="card-body"><div class="fields">
            <label>Scope<select><option>All items of the class</option><option>A chosen set</option></select></label>
            <label>Asset class<select>${p.classes.map((c) => `<option>${c.toUpperCase()}</option>`).join('')}</select></label>
          </div><div class="faint">No item may belong to two feeds of ${p.code}.</div></div></div>
          <div class="card"><div class="card-head"><h2>3. Schedule and route</h2><p>when to fetch, and which party reads it</p></div>
          <div class="card-body"><div class="fields">
            <label>Rhythm<select><option>Daily at 16:00 CET</option><option>Continuous, demand driven</option><option>On demand</option></select></label>
            <label>Party<select><option>System Party</option></select></label>
            <label>Asset class<select>${p.classes.map((c) => `<option>${c.toUpperCase()}</option>`).join('')}</select></label>
          </div><div class="faint">The scheduler job is created for the feed. The route is one per party, publisher and class.</div></div></div>
          <div class="card"><div class="card-body"><div class="foot">
            <a class="act" href="?publisher=${p.code}">Cancel</a>
            <a class="act primary" href="?publisher=${p.code}">Create feed <span class="faint">· design pending</span></a>
          </div></div></div>`;
}

function wire() {
    document.querySelector('#find').addEventListener('input', (e) => {
        const term = e.target.value.toLowerCase();
        document.querySelectorAll('#tree > details, #pubtree > details').forEach((d) => {
            d.style.display = d.textContent.toLowerCase().includes(term) ? '' : 'none';
        });
    });
    document.querySelector('#refresh').addEventListener('click', () => location.reload());
}

renderTree();
if (addSession) renderAddSession();
else if (addFeed) renderAddFeed();
else if (feedName) renderFeedPane();
else if (sessionName) renderSessionPane();
else if (supplierCode) renderSupplierPane();
else if (publisherCode) renderPublisherPane();
else if (q.get('view') === 'adapters') renderAdaptersPane();
else renderDefault();
wire();
