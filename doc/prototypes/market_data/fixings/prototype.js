/*
 * Static prototype of Fixings: every FX and IR fixing series the platform
 * states, one row each, and the day it reaches. Mock data only; no request
 * leaves the page. Every state is a query string.
 *
 *   ?class=fx|ir   ?currency=<code>   ?source=<code>   ?find=<text>
 *   ?state=empty   ?series=<oresmd uri>
 */

const q = new URLSearchParams(location.search);
const filterClass = q.get('class') ?? 'all';
const filterCurrency = q.get('currency') ?? 'all';
const filterSource = q.get('source') ?? 'all';
const filterOnlyEmpty = q.get('state') === 'empty';
const find = (q.get('find') ?? '').toLowerCase();
const openSeries = q.get('series');

/* One row is one series: the grain of a fixing, and of the day it reaches. */
const SERIES = [
    { uri: 'oresmd://ir/USD?type=fixing&index=ibor&name=SOFR&tenor=1M', label: 'USD SOFR 1M',
      cls: 'ir', currency: 'USD', source: 'derived-a', provider: 'derived',
      latest: '0.039436', day: '2026-11-26', days: 520 },
    { uri: 'oresmd://ir/COP?type=fixing&index=ibor&name=IBR&tenor=3M', label: 'COP IBR 3M',
      cls: 'ir', currency: 'COP', source: 'derived-a', provider: 'derived',
      latest: '0.089571', day: '2026-11-26', days: 520 },
    { uri: 'oresmd://ir/JPY?type=fixing&index=ibor&name=EYTIBOR&tenor=3M', label: 'JPY EYTIBOR 3M',
      cls: 'ir', currency: 'JPY', source: 'derived-a', provider: 'derived',
      latest: '0.003522', day: '2026-11-26', days: 520 },
    { uri: 'oresmd://ir/KRW?type=fixing&index=ibor&name=GENERIC&tenor=3M', label: 'KRW GENERIC 3M',
      cls: 'ir', currency: 'KRW', source: 'derived-a', provider: 'derived',
      latest: '0.029411', day: '2026-11-26', days: 520 },
    { uri: 'oresmd://ir/PLN?type=fixing&index=ibor&name=WIBOR&tenor=6M', label: 'PLN WIBOR 6M',
      cls: 'ir', currency: 'PLN', source: 'derived-a', provider: 'derived',
      latest: '0.056003', day: '2026-11-26', days: 520 },
    { uri: 'oresmd://fx/EUR?type=fixing&index=fx&source=WMR1000NYC&ccy=USD', label: 'EUR/USD',
      cls: 'fx', currency: 'EUR/USD', source: 'simulated-fixings-a', provider: 'simulated-fixings',
      latest: '1.090100', day: '2026-11-26', days: 82 },
    { uri: 'oresmd://fx/EUR?type=fixing&index=fx&source=ECB&ccy=CAD', label: 'EUR/CAD',
      cls: 'fx', currency: 'EUR/CAD', source: 'ecb-frankfurter', provider: 'ecb',
      latest: '1.563800', day: '2026-11-25', days: 44 },
    { uri: 'oresmd://fx/USD?type=fixing&index=fx&source=IMF&ccy=EUR', label: 'USD/EUR',
      cls: 'fx', currency: 'USD/EUR', source: 'imf-frankfurter', provider: 'imf',
      latest: '0.859700', day: '2026-11-25', days: 48 },
    { uri: 'oresmd://fx/CAD?type=fixing&index=fx&source=BOC&ccy=USD', label: 'CAD/USD',
      cls: 'fx', currency: 'CAD/USD', source: 'boc-frankfurter', provider: 'boc',
      latest: '1.384800', day: '2026-11-24', days: 27 },
    { uri: 'oresmd://fx/TRY?type=fixing&index=fx&source=TCMB&ccy=USD', label: 'TRY/USD',
      cls: 'fx', currency: 'TRY/USD', source: 'tcmb-frankfurter', provider: 'tcmb',
      latest: '42.700000', day: '2026-11-24', days: 21 },
    /* A series that ended: the curve behind it stopped, and nothing was made up. */
    { uri: 'oresmd://ir/GBP?type=fixing&index=ibor&name=SONIA&tenor=1M', label: 'GBP SONIA 1M',
      cls: 'ir', currency: 'GBP', source: 'derived-a', provider: 'derived',
      latest: '0.041200', day: '2026-11-18', days: 512 },
    /* A series the platform holds and never stated: a row, and a count of zero. */
    { uri: 'oresmd://ir/CHF?type=fixing&index=ibor&name=SARON&tenor=3M', label: 'CHF SARON 3M',
      cls: 'ir', currency: 'CHF', source: '', provider: 'derived',
      latest: '', day: '', days: 0 }
];

const fn = {
    url(changes) {
        const next = new URLSearchParams(q);
        for (const [k, v] of Object.entries(changes)) {
            if (v === null) next.delete(k); else next.set(k, v);
        }
        const s = next.toString();
        return s ? '?' + s : '?';
    },
    currencies() {
        const out = new Set();
        for (const s of SERIES) for (const c of s.currency.split('/')) out.add(c);
        return [...out].sort();
    },
    sources() {
        return [...new Set(SERIES.map((s) => s.source).filter(Boolean))].sort();
    }
};

function matches(s) {
    if (filterClass !== 'all' && s.cls !== filterClass) return false;
    if (filterCurrency !== 'all' && !s.currency.split('/').includes(filterCurrency)) return false;
    if (filterSource !== 'all' && s.source !== filterSource) return false;
    if (filterOnlyEmpty && s.days !== 0) return false;
    if (find) {
        const hay = (s.label + ' ' + s.uri + ' ' + s.source + ' ' + s.provider).toLowerCase();
        if (!hay.includes(find)) return false;
    }
    return true;
}

function renderFilters() {
    const el = document.getElementById('filters');
    const option = (value, label, current) =>
        `<option value="${value}"${value === current ? ' selected' : ''}>${label}</option>`;
    el.innerHTML =
        `<select id="f-class">${['all', 'fx', 'ir'].map((c) => option(c, c === 'all' ? 'Every class' : c.toUpperCase(), filterClass)).join('')}</select>` +
        `<select id="f-currency">${option('all', 'Every currency', filterCurrency)}${fn.currencies().map((c) => option(c, c, filterCurrency)).join('')}</select>` +
        `<select id="f-source">${option('all', 'Every source', filterSource)}${fn.sources().map((c) => option(c, c, filterSource)).join('')}</select>`;
    document.getElementById('f-class').onchange = (e) => location.search = fn.url({ class: e.target.value === 'all' ? null : e.target.value });
    document.getElementById('f-currency').onchange = (e) => location.search = fn.url({ currency: e.target.value === 'all' ? null : e.target.value });
    document.getElementById('f-source').onchange = (e) => location.search = fn.url({ source: e.target.value === 'all' ? null : e.target.value });
    document.getElementById('find').value = q.get('find') ?? '';
    document.getElementById('find').onchange = (e) => location.search = fn.url({ find: e.target.value || null });
}

function renderList() {
    const rows = SERIES.filter(matches);
    const body = document.querySelector('#series tbody');
    body.innerHTML = rows.map((s) => `
      <tr class="${s.days === 0 ? 'quiet' : ''}">
        <td><span class="provider">${s.label}</span><div class="muted" style="font-size:.72rem">${s.uri}</div></td>
        <td><span class="badge ${s.cls === 'ir' ? 'ok' : 'off'}">${s.cls.toUpperCase()}</span></td>
        <td>${s.currency}</td>
        <td>${s.source || '<span class="muted">—</span>'}</td>
        <td>${s.provider}</td>
        <td class="num">${s.latest || '<span class="muted">—</span>'}</td>
        <td class="when">${s.day || '<span class="badge never">no day</span>'}</td>
        <td class="num">${s.days}</td>
        <td class="acts"><a class="act" href="${fn.url({ series: s.label })}">Open</a></td>
      </tr>`).join('');

    const empty = SERIES.filter((s) => s.days === 0).length;
    document.getElementById('sub').textContent =
        `${rows.length} of ${SERIES.length} series · ${empty} hold no day · the platform states what it observed`;

    document.getElementById('legend').innerHTML = `
      <div><span class="badge never">no day</span> a series the platform holds and never stated: it is a row, and its count is zero</div>
      <div><span class="muted">—</span> a series that ended keeps its latest day and its count, because a fixing is a fact about a date</div>
      <div>This screen states observations and offers no authorisation: a day the platform never stated is entered by a party, in its own fixings manager.</div>`;
}

function renderSeriesPage() {
    const s = SERIES.find((one) => one.uri === openSeries || one.label === openSeries);
    if (!s) return false;
    document.querySelector('main > section.card').hidden = true;
    const page = document.getElementById('series-page');
    page.hidden = false;
    document.getElementById('sp-title').textContent = s.label;
    document.getElementById('sp-sub').textContent = s.uri;
    const days = s.days === 0 ?
        '<p class="muted">This series holds no day. The platform holds it and never stated a value for it.</p>' :
        `<p>${s.days} days, the latest on ${s.day} at ${s.latest}.</p>`;
    document.getElementById('sp-body').innerHTML = days + `
      <table class="list">
        <thead><tr><th>Day</th><th>Value</th><th>Source</th></tr></thead>
        <tbody>${(s.days === 0 ? [] : [
            { day: s.day, value: s.latest, source: s.source },
            { day: '2026-11-25', value: s.latest, source: s.source },
            { day: '2026-11-24', value: s.latest, source: s.source }
        ]).map((r) => `<tr><td class="when">${r.day}</td><td class="num">${r.value}</td><td>${r.source}</td></tr>`).join('')}</tbody>
      </table>
      <p class="muted" style="margin-top:10px">The series page is the one the platform already has: this prototype only shows where a row leads.</p>`;
    return true;
}

renderFilters();
if (!renderSeriesPage()) renderList();
