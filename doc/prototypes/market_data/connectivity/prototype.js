/*
 * Static prototype of Connectivity: the drivers that speak an external language,
 * and the connections that configure one instance of each. Mock data only; no
 * request leaves the page and every endpoint and secret is invented.
 *
 *   ?tab=connections|drivers
 *   ?connection=<id>&ctab=overview|credentials|providers|routing|health|history
 */

const q = new URLSearchParams(location.search);
const tab = q.get('tab') ?? 'connections';
const connectionId = q.get('connection');
const ctab = q.get('ctab') ?? 'overview';

const DRIVERS = [
    { code: 'frankfurter', name: 'Frankfurter', speaks: 'HTTPS and JSON', needs: 'base URL, publisher key path' },
    { code: 'bloomberg', name: 'Bloomberg', speaks: 'the Bloomberg API', needs: 'endpoint, credential, publisher keys' },
    { code: 'lseg', name: 'LSEG', speaks: 'the LSEG API', needs: 'endpoint, credential, publisher keys' },
    { code: 'ecb-data', name: 'ECB Data Portal', speaks: 'the ECB data API (SDW)', needs: 'endpoint, series keys' },
    { code: 'fred', name: 'FRED', speaks: 'the FRED API', needs: 'endpoint, credential, series ids' },
    { code: 'boe', name: 'Bank of England', speaks: 'the BoE statistics API', needs: 'endpoint, series codes' },
    { code: 'treasury', name: 'US Treasury', speaks: 'the Treasury data feed', needs: 'endpoint, curve year' },
    { code: 'synthetic', name: 'Synthetic simulator', speaks: 'its own generators', needs: 'nothing: no endpoint, no credential' },
];

const CONNECTIONS = [
    { id: 'frankfurter', name: 'Frankfurter', driver: 'frankfurter', endpoint: 'https://api.frankfurter.app',
      credential: 'none', rhythm: 'daily, 16:00 CET', poll: '60 min', backfill: '400 days', classes: ['fx'],
      state: 'serving', providers: ['ecb', 'boc', 'imf', 'fred', 'tcmb', 'norges', 'boe'], destination: 'System Party',
      last: '19:24', secrets: [] },
    { id: 'bloomberg-prod', name: 'Bloomberg production', driver: 'bloomberg', endpoint: 'https://api.bloomberg.com',
      credential: 'vault:marketdata/bloomberg/prod', rhythm: 'streaming, always on', poll: 'not polled', backfill: '400 days',
      classes: ['fx'], state: 'serving', providers: ['ecb'], destination: 'System Party', last: '19:23',
      secrets: [{ name: 'api key', masked: '•••• 7f2a', set: '2026-10-01' }, { name: 'api secret', masked: '•••• c91b', set: '2026-10-01' }] },
    { id: 'bloomberg-test', name: 'Bloomberg test', driver: 'bloomberg', endpoint: 'https://api.test.bloomberg.com',
      credential: 'vault:marketdata/bloomberg/test', rhythm: 'streaming, always on', poll: 'not polled', backfill: '30 days',
      classes: ['fx'], state: 'not serving', providers: [], destination: 'none', last: '—',
      secrets: [{ name: 'api key', masked: 'not set', set: '—' }] },
    { id: 'ecb-data', name: 'ECB Data Portal', driver: 'ecb-data', endpoint: 'https://data-api.ecb.europa.eu',
      credential: 'none', rhythm: 'daily, 16:00 CET', poll: '60 min', backfill: '400 days', classes: ['ir'],
      state: 'serving', providers: ['ecb'], destination: 'System Party', last: '17:02', secrets: [] },
    { id: 'fred-api', name: 'FRED', driver: 'fred', endpoint: 'https://api.stlouisfed.org',
      credential: 'vault:marketdata/fred', rhythm: 'daily, 16:15 ET', poll: '60 min', backfill: '400 days',
      classes: ['fx', 'ir'], state: 'serving', providers: ['fred'], destination: 'System Party', last: '19:22',
      secrets: [{ name: 'api key', masked: '•••• 4d10', set: '2026-09-12' }] },
    { id: 'boe-api', name: 'Bank of England statistics', driver: 'boe', endpoint: 'https://www.bankofengland.co.uk/boeapps/database',
      credential: 'none', rhythm: 'daily, 15:00 GMT', poll: '60 min', backfill: '400 days', classes: ['fx', 'ir'],
      state: 'serving', providers: ['boe'], destination: 'System Party', last: '15:00', secrets: [] },
    { id: 'treasury', name: 'US Treasury', driver: 'treasury', endpoint: 'https://home.treasury.gov/resource-center/data-chart-center',
      credential: 'none', rhythm: 'daily, 18:00 ET', poll: '60 min', backfill: '400 days', classes: ['ir'],
      state: 'serving', providers: ['ust'], destination: 'System Party', last: '18:05', secrets: [] },
    { id: 'synthetic', name: 'Synthetic feeds', driver: 'synthetic', endpoint: 'none',
      credential: 'none', rhythm: 'continuous, the scheduler drives it', poll: 'not polled', backfill: 'not applicable',
      classes: ['fx', 'ir'], state: 'serving', providers: ['lseg (simulated)', 'bloomberg (simulated)'],
      destination: 'System Party', last: 'now', secrets: [] },
];

const PROVIDERS = [
    { code: 'ecb', name: 'European Central Bank', classes: ['fx', 'ir'], items: 5, last: '2026-10-28', state: 'serving' },
    { code: 'boc', name: 'Bank of Canada', classes: ['fx'], items: 2, last: '2026-10-27', state: 'late' },
    { code: 'imf', name: 'International Monetary Fund', classes: ['fx'], items: 2, last: '2026-10-28', state: 'serving' },
    { code: 'fred', name: 'Federal Reserve (H.10)', classes: ['fx', 'ir'], items: 4, last: '2026-10-28', state: 'serving' },
    { code: 'tcmb', name: 'Central Bank of Turkiye', classes: ['fx'], items: 1, last: null, state: 'down' },
    { code: 'norges', name: 'Norges Bank', classes: ['fx'], items: 0, last: null, state: 'never' },
    { code: 'boe', name: 'Bank of England', classes: ['fx', 'ir'], items: 3, last: '2026-10-28', state: 'paused' },
    { code: 'ust', name: 'US Treasury', classes: ['ir'], items: 1, last: '2026-10-28', state: 'serving' },
];

const TONE = { serving: ['ok', 'serving'], late: ['warn', 'late'], down: ['bad', 'not serving'],
    never: ['never', 'never fixed'], paused: ['off', 'paused'] };

const fn = {
    esc: (t) => String(t).replace(/[&<>"]/g, (c) => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;' }[c])),
    href: (changes) => {
        const next = new URLSearchParams(q);
        for (const [k, v] of Object.entries(changes)) { if (v === null) next.delete(k); else next.set(k, v); }
        return `?${next.toString()}`;
    },
    badge: (state) => `<span class="badge ${TONE[state]?.[0] ?? 'never'}"><i class="dot"></i>${TONE[state]?.[1] ?? state}</span>`,
};

function hidePanels(except) {
    document.querySelectorAll('[role="tabpanel"]').forEach((x) => { x.hidden = x.id !== except; });
    document.querySelector('#conn-record').hidden = true;
}

function renderConnections() {
    hidePanels('tab-connections');
    document.querySelectorAll('[data-tab]').forEach((b) => b.setAttribute('aria-selected', String(b.dataset.tab === 'connections')));
    document.querySelector('#connections tbody').innerHTML = CONNECTIONS.map((c) => `
        <tr>
          <td class="provider">${fn.esc(c.name)}<div class="faint mono">${c.id}</div></td>
          <td class="mono">${c.driver}</td>
          <td class="mono faint">${fn.esc(c.endpoint)}</td>
          <td>${(c.classes ?? ['fx']).map((x) => `<span class="badge">${x.toUpperCase()}</span>`).join(' ')}</td>
          <td class="num">${c.providers.length}</td>
          <td>${fn.esc(c.destination)}</td>
          <td>${c.state === 'serving' ? fn.badge('serving') : fn.badge('down')}</td>
          <td class="when">${c.last}</td>
          <td class="acts"><a class="act" href="${fn.href({ tab: null, connection: c.id, ctab: 'overview' })}">Open</a></td>
        </tr>`).join('');
    document.querySelector('#connections-note').textContent =
        'A connection is a driver configured as one instance: an endpoint, its credentials, a rhythm and the destination its values land in. Two connections of one driver are how a test and a production endpoint coexist. The synthetic connection has no endpoint and no credential; the scheduler drives it.';
}

function renderDrivers() {
    hidePanels('tab-drivers');
    document.querySelectorAll('[data-tab]').forEach((b) => b.setAttribute('aria-selected', String(b.dataset.tab === 'drivers')));
    document.querySelector('#drivers tbody').innerHTML = DRIVERS.map((d) => {
        const used = CONNECTIONS.filter((c) => c.driver === d.code);
        return `<tr><td class="provider">${fn.esc(d.name)}<div class="faint mono">${d.code}</div></td>
          <td>${d.speaks}</td><td>${d.needs}</td>
          <td>${used.length ? used.map((c) => `<a href="${fn.href({ connection: c.id, ctab: 'overview', tab: null })}">${fn.esc(c.name)}</a>`).join(', ') : '<span class="faint">none</span>'}</td></tr>`;
    }).join('');
    document.querySelector('#drivers-note').textContent =
        'A driver is code, not configuration; nobody adds one from a screen. The catalogue is read only, and it exists so a person can see what a connection must say before they add one.';
}

function renderConnectionRecord() {
    const c = CONNECTIONS.find((x) => x.id === connectionId);
    if (!c) { location.search = fn.href({ tab: 'connections', connection: null, ctab: null }); return; }
    hidePanels(null);
    document.querySelector('#conn-record').hidden = false;
    document.querySelector('#conn-crumb').textContent = c.name;
    document.querySelector('#conn-title').textContent = c.name;
    document.querySelector('#conn-meta').innerHTML = `<span class="mono faint">${c.id}</span> · driver ${c.driver} · ${c.state === 'serving' ? fn.badge('serving') : fn.badge('down')}`;
    document.querySelector('#conn-acts').innerHTML = `<a class="act" href="#">Edit configuration <span class="faint">· design pending</span></a>
        <a class="act" href="#">${c.state === 'serving' ? 'Stop' : 'Start'}</a>`;
    document.querySelectorAll('[data-ctab]').forEach((b) => b.setAttribute('aria-selected', String(b.dataset.ctab === ctab)));
    const body = document.querySelector('#conn-body');
    const detail = (rows) => `<div class="kv">${rows.map(([k, v]) => `<span class="k">${k}</span><span>${v}</span>`).join('')}</div>`;
    const classes = (c.classes ?? ['fx']).map((x) => x.toUpperCase()).join(', ');

    if (ctab === 'overview') {
        body.innerHTML = `<div class="card"><div class="card-head"><h2>Configuration</h2><p>what the driver needs to run</p></div><div class="card-body">${detail([
            ['Endpoint', `<span class="mono">${fn.esc(c.endpoint)}</span>`],
            ['Driver', c.driver],
            ['Asset classes', classes],
            ['Credential reference', `<span class="mono">${fn.esc(c.credential)}</span>`],
            ['Expected rhythm', c.rhythm],
            ['Poll interval', c.poll],
            ['Backfill window', c.backfill],
            ['Destination', c.destination],
            ['Providers', String(c.providers.length)],
        ])}<div class="faint" style="margin-top:8px">The endpoint and the rhythm are rows the driver reads. A secret is a reference; its value is encrypted at rest, write-only, and never returned here.</div></div></div>`;
    } else if (ctab === 'credentials') {
        body.innerHTML = `<div class="card"><div class="card-head"><h2>Credentials</h2><p>masked; the value never leaves the service</p></div><div class="card-body">
          ${c.secrets.length ? `<table class="list"><thead><tr><th>Secret</th><th>Value</th><th>Set</th><th class="acts"></th></tr></thead><tbody>${c.secrets.map((x) => `<tr><td>${x.name}</td><td class="mono">${x.masked}</td><td class="when">${x.set}</td><td class="acts"><a class="act" href="#">Rotate <span class="faint">· design pending</span></a></td></tr>`).join('')}</tbody></table>` : '<div class="notice">This driver needs no secret. The synthetic simulator makes its own values, and Frankfurter is open.</div>'}
          <div class="faint" style="margin-top:8px">A secret is set by writing it once; the screen shows a mask and the date. It is encrypted at rest and never returned by a read.</div></div></div>`;
    } else if (ctab === 'providers') {
        body.innerHTML = `<div class="card"><div class="card-head"><h2>Providers</h2><p>the publishers this connection reaches</p></div>
          <table class="list"><thead><tr><th>Provider</th><th>Classes</th><th>Underlyings</th><th>Last fixing</th><th>State</th><th class="acts"></th></tr></thead><tbody>${c.providers.map((code) => {
            const p = PROVIDERS.find((x) => x.code === code);
            if (!p) return `<tr><td>${fn.esc(code)}</td><td class="faint">simulated</td><td class="num">—</td><td class="faint">simulated</td><td>${fn.badge('serving')}</td><td class="acts"><span class="faint">simulated provider</span></td></tr>`;
            return `<tr><td class="provider">${fn.esc(p.name)}<div class="faint mono">${p.code}</div></td>
              <td>${p.classes.map((x) => `<span class="badge">${x.toUpperCase()}</span>`).join(' ')}</td>
              <td class="num">${p.items}</td>
              <td>${p.last ?? '<span class="faint">none</span>'}</td><td>${fn.badge(p.state)}</td>
              <td class="acts"><a class="act" href="../official-rates/index.html?provider=${p.code}&rtab=overview">Open <span class="faint">· in Rates</span></a></td></tr>`;
        }).join('') || '<tr><td colspan="6" class="faint">No provider is mapped to this connection yet.</td></tr>'}</tbody></table>
          <div class="faint" style="margin-top:8px;padding:0 14px 12px">A provider belongs to the connection that delivers it. A publisher reached two ways is two connections over one provider.</div></div>`;
    } else if (ctab === 'routing') {
        body.innerHTML = `<div class="card"><div class="card-head"><h2>Routing</h2><p>where the values land</p></div><div class="card-body">${detail([
            ['Destination', c.destination],
            ['Party', c.destination === 'none' ? '—' : 'System Party'],
            ['Store', 'the durable fixings table'],
            ['Entitlement', 'the party reads only its own series'],
        ])}<div class="faint" style="margin-top:8px">Pointing the stream is the last step: the connection reaches the provider, and the destination names the party and the store its values fill.</div></div></div>`;
    } else if (ctab === 'health') {
        body.innerHTML = `<div class="card"><div class="card-head"><h2>Health</h2><p>the connection, as the service sees it</p></div><div class="card-body">${detail([
            ['State', c.state === 'serving' ? 'serving' : 'not serving'],
            ['Last delivery', c.last],
            ['Providers served', String(c.providers.length)],
            ['Endpoint', `<span class="mono">${fn.esc(c.endpoint)}</span>`],
        ])}</div></div>`;
    } else {
        const events = c.id === 'synthetic'
            ? [['v1', '2026-10-20 09:00', 'Connection created', 'super_admin'], ['—', '2026-10-28 19:00', 'Destination set to System Party', 'super_admin']]
            : [['v1', '2026-10-01 09:00', 'Connection created', 'super_admin'], ['v2', '2026-10-20 11:12', 'Endpoint changed', 'super_admin'], ['—', '2026-10-28 19:00', 'Credential rotated', 'super_admin']];
        body.innerHTML = `<div class="card"><div class="card-head"><h2>History</h2><p>the shared timeline</p></div><div class="card-body"><div class="timeline">${events.map(([v, at, what, who]) =>
            `<div class="row"><span class="at">${at}</span><span class="what"><b>${what}</b> · ${v} · by ${who}</span></div>`).join('')}</div></div></div>`;
    }
}

function wire() {
    document.querySelectorAll('[data-tab]').forEach((b) => b.addEventListener('click', () => {
        location.search = fn.href({ tab: b.dataset.tab, connection: null, ctab: null });
    }));
    document.querySelectorAll('[data-ctab]').forEach((b) => b.addEventListener('click', () => {
        location.search = fn.href({ connection: connectionId, ctab: b.dataset.ctab, tab: null });
    }));
    document.querySelector('#refresh').addEventListener('click', () => location.reload());
}

hidePanels('tab-' + tab);
if (connectionId) renderConnectionRecord();
else if (tab === 'drivers') renderDrivers();
else renderConnections();
wire();
