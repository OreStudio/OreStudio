/*
 * A static prototype: mock providers and fixings, and the screen's states chosen
 * by the query string so a reviewer opens each one by address. No framework, no
 * build, no request. The values are invented.
 *
 *   ?state=live|late|down|never    show one provider state alone
 *   ?paused=muted|hidden           a paused provider kept in the list, or removed
 *   ?fresh=quiet|strong            two freshness lines, side by side for review
 *   ?backfill=none|progress|fill   a range at rest, running, and done
 */

const q = new URLSearchParams(location.search);
const pausedVariant = q.get('paused') ?? 'muted';
const freshVariant = q.get('fresh') ?? 'quiet';
const backfillState = q.get('backfill') ?? 'none';
const showState = q.get('state');

const FIXINGS = {
    ecb: [
        ['2026-10-20', '1.5534'], ['2026-10-21', '1.5548'], ['2026-10-22', '1.5561'],
        ['2026-10-23', '1.5570'], ['2026-10-26', '1.5574'], ['2026-10-27', '1.5591'],
        ['2026-10-28', '1.5638'],
    ],
    boc: [
        ['2026-10-20', '1.4021'], ['2026-10-21', '1.4038'], ['2026-10-22', '1.4019'],
        ['2026-10-23', '1.4042'], ['2026-10-26', '1.4055'], ['2026-10-27', '1.4061'],
    ],
    imf: [
        ['2026-10-20', '1.5530'], ['2026-10-21', '1.5545'], ['2026-10-22', '1.5558'],
        ['2026-10-23', '1.5568'], ['2026-10-26', '1.5571'], ['2026-10-27', '1.5588'],
        ['2026-10-28', '1.5637'],
    ],
};

const PROVIDERS = [
    {
        code: 'ecb', name: 'European Central Bank', items: 'EUR/CAD, EUR/USD, EUR/GBP',
        terms: { accepted: true, version: 'v3', who: 'super_admin', at: '2026-10-28 09:00' },
        adapter: { state: 'serving', heartbeat: '19:24', refused: 0 },
        last: { date: '2026-10-28', received: '19:23 today', expected: 'today, on time', onTime: true },
        item: 'FX-ECB-EUR-CAD',
    },
    {
        code: 'boc', name: 'Bank of Canada', items: 'CAD/EUR, CAD/USD',
        terms: { accepted: true, version: 'v2', who: 'super_admin', at: '2026-10-26 11:20' },
        adapter: { state: 'serving', heartbeat: '19:24', refused: 0 },
        last: { date: '2026-10-27', received: '19:20 yesterday', expected: 'today, late', onTime: false },
        item: 'FX-BOC-CAD-EUR',
    },
    {
        code: 'imf', name: 'International Monetary Fund', items: 'EUR/USD, USD/CHF, USD/JPY',
        terms: { accepted: false, version: 'v1', who: '', at: '' },
        adapter: { state: 'serving', heartbeat: '19:24', refused: 0 },
        last: { date: '2026-10-28', received: '19:24 today', expected: 'today, on time', onTime: true },
        item: 'FX-IMF-EUR-USD',
    },
    {
        code: 'tcmb', name: 'Central Bank of Turkiye', items: 'TRY/USD, TRY/EUR',
        terms: { accepted: true, version: 'v1', who: 'super_admin', at: '2026-10-20 08:10' },
        adapter: { state: 'down', heartbeat: '16:02', refused: 3 },
        last: null,
        item: 'FX-TCMB-TRY-USD',
    },
    {
        code: 'boe', name: 'Bank of England', items: 'GBP/USD, GBP/EUR',
        terms: { accepted: true, version: 'v1', who: 'super_admin', at: '2026-10-18 14:00' },
        adapter: { state: 'serving', heartbeat: '19:24', refused: 0 },
        paused: true,
        last: { date: '2026-10-28', received: '15:00 today', expected: 'today, on time', onTime: true },
        item: 'FX-BOE-GBP-USD',
    },
];

const SENTENCES = {
    live: 'Serving. The last fixing is today\'s and it arrived on time.',
    late: 'Late. The publisher\'s day has passed with no fixing. Read the adapter\'s heartbeat, then ask for a backfill of the missing days.',
    down: 'Not serving. The adapter\'s heartbeat stopped, so nothing is being fetched. Restart it, or find out whether its terms were withdrawn.',
    never: 'Never fixed. The terms are accepted and the adapter serves, but no value of this provider is stored yet.',
    paused: 'Paused by a person. The provider is not fetched. Its stored fixings and every feed that started from one stay as they are.',
    terms: 'Terms not accepted. The provider is not fetched until a person accepts the version shown.',
};

const STATE_BADGE = {
    live: ['ok', 'serving'], late: ['warn', 'late'], down: ['bad', 'not serving'],
    never: ['never', 'never fixed'], paused: ['off', 'paused'], terms: ['warn', 'not accepted'],
};

function statusOf(p) {
    if (!p.terms.accepted) return 'terms';
    if (p.paused) return 'paused';
    if (p.adapter.state !== 'serving') return 'down';
    if (!p.last) return 'never';
    if (!p.last.onTime) return 'late';
    return 'live';
}

function esc(text) {
    return String(text).replace(/[&<>"]/g, (c) => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;' }[c]));
}

function href(changes) {
    const next = new URLSearchParams(q);
    for (const [key, value] of Object.entries(changes)) {
        if (value === null) next.delete(key); else next.set(key, value);
    }
    return `?${next.toString()}`;
}

function badge(kind) {
    const [tone, text] = STATE_BADGE[kind];
    return `<span class="badge ${tone}"><i class="dot"></i>${text}</span>`;
}

function termsCell(p) {
    if (!p.terms.accepted) return `<span class="badge warn">not accepted · ${p.terms.version}</span>`;
    return `<span class="badge ok">accepted ${p.terms.version}</span> <span class="faint">${esc(p.terms.who)}</span>`;
}

function freshCell(p) {
    if (!p.last) {
        return `<div class="fresh"><span class="when faint">—</span><span class="sub">no fixing ever</span></div>`;
    }
    const when = freshVariant === 'strong'
        ? `<span class="when">${esc(p.last.date)} · ${esc(p.last.received.replace(' today', '').replace(' yesterday', ''))}</span>`
        : `<span class="when">${esc(p.last.date)}</span>`;
    return `<div class="fresh ${freshVariant === 'strong' ? 'strong' : ''}">${when}<span class="sub">${esc(p.last.received)}</span></div>`;
}

function rowActs(p, kind) {
    if (kind === 'terms') return `<button class="act primary">Review terms</button>`;
    if (kind === 'paused') return `<button class="act" data-pause="${p.code}">Resume</button>`;
    return `<button class="act">Open</button><button class="act" data-pause="${p.code}">Pause</button>`;
}

function renderProviders() {
    const body = document.querySelector('#providers tbody');
    let shown = PROVIDERS.map((p) => ({ ...p, kind: statusOf(p) }));
    if (showState) shown = shown.filter((p) => p.kind === showState);
    const paused = shown.filter((p) => p.kind === 'paused');
    const hiddenPaused = pausedVariant === 'hidden' ? paused.length : 0;
    if (pausedVariant === 'hidden') shown = shown.filter((p) => p.kind !== 'paused');

    body.innerHTML = shown.map((p) => `
        <tr data-code="${p.code}" class="${showState ? 'on' : ''} ${poked(p)}">
            <td class="provider">${esc(p.name)}<div class="faint">${esc(p.items)}</div></td>
            <td>${termsCell(p)}</td>
            <td>${freshCell(p)}</td>
            <td class="when">${p.last ? esc(p.last.expected) : '—'}</td>
            <td>${badge(p.kind)}</td>
            <td>${p.adapter.state === 'serving'
                ? `<span class="faint mono">hb ${esc(p.adapter.heartbeat)}</span>`
                : `<span class="badge bad">hb ${esc(p.adapter.heartbeat)}</span>`}</td>
            <td class="acts">${rowActs(p, p.kind)}</td>
        </tr>`).join('');

    const count = shown.length;
    document.querySelector('#providers-sub').textContent = showState
        ? `showing the ${showState} state · ${count} provider${count === 1 ? '' : 's'}`
        : `${PROVIDERS.length} providers · ${paused.length ? `${paused.length} paused` : 'none paused'}`;
    if (hiddenPaused > 0) {
        document.querySelector('#providers-sub').innerHTML +=
            ` · <a href="${href({ paused: 'muted' })}">show the paused one</a>`;
    }
    document.querySelectorAll('[data-pause]').forEach((b) => b.addEventListener('click', () => {
        const p = PROVIDERS.find((x) => x.code === b.dataset.pause);
        p.paused = !p.paused;
        renderAll();
    }));
}

function poked(p) {
    return pausedVariant === 'muted' && p.kind === 'paused' ? 'quiet' : '';
}

function renderLegend() {
    const kinds = showState ? [showState] : ['live', 'late', 'down', 'never', 'paused', 'terms'];
    document.querySelector('#legend').innerHTML = kinds.map((kind) => `
        <div>${badge(kind)}<span>${SENTENCES[kind]}</span></div>`).join('') +
        `<div class="faint">An adapter is never paused: it serves every provider it was given. The badge on a row is the row's state, read from the terms, the adapter and the fixings together.</div>`;
}

function chart(values) {
    const w = 800, h = 180, pad = 24;
    const numbers = values.map(([, v]) => Number(v));
    const min = Math.min(...numbers), max = Math.max(...numbers);
    const x = (i) => pad + (i * (w - 2 * pad)) / Math.max(values.length - 1, 1);
    const y = (v) => h - pad - ((v - min) / Math.max(max - min, 1e-9)) * (h - 2 * pad);
    const points = values.map(([, v], i) => `${x(i).toFixed(1)},${y(Number(v)).toFixed(1)}`).join(' ');
    const area = `${pad},${h - pad} ${points} ${w - pad},${h - pad}`;
    const ticks = values.map(([d, v], i) => `<circle class="pt" cx="${x(i)}" cy="${y(Number(v))}" r="2.5"></circle>`).join('');
    return `<svg class="chart" viewBox="0 0 ${w} ${h}" preserveAspectRatio="none" role="img" aria-label="The fixing over the window">
        <line class="axis" x1="${pad}" y1="${h - pad}" x2="${w - pad}" y2="${h - pad}"></line>
        <polygon class="area" points="${area}"></polygon>
        <polyline class="line" points="${points}"></polyline>${ticks}
        <text x="${pad}" y="${h - 6}">${esc(values[0][0])}</text>
        <text x="${w - pad}" y="${h - 6}" text-anchor="end">${esc(values[values.length - 1][0])}</text>
    </svg>`;
}

function renderProviderPage() {
    const p = PROVIDERS[0];
    document.querySelector('#provider-title').textContent = `${p.name} — EUR/CAD`;
    document.querySelector('#provider-sub').textContent =
        `${p.item} · ${FIXINGS.ecb.length} fixings in the window`;
    const rows = [...FIXINGS.ecb].reverse().map(([date, value]) => `
        <tr><td class="when">${esc(date)}</td><td class="num">${esc(value)}</td>
        <td class="mono faint">oresmd://fx/EUR?type=fixing&amp;index=fx&amp;source=ECB&amp;ccy=CAD</td>
        <td class="faint">19:2${Math.floor(Math.random() * 5)}</td></tr>`).join('');
    document.querySelector('#provider-body').innerHTML = `
        ${chart(FIXINGS.ecb)}
        <table class="list" style="margin-top:12px">
          <thead><tr><th>Date</th><th>Value</th><th>Item</th><th>Received</th></tr></thead>
          <tbody>${rows}</tbody>
        </table>
        <div style="margin-top:14px" class="label">One pair, as each provider publishes it</div>
        <div style="display:flex;gap:18px;margin-top:4px" class="mono">
          <span>ECB <b>1.5638</b></span>
          <span>BOC <b>1/1.4061 = 1.5638</b> <span class="faint">inverted</span></span>
          <span>IMF <b>1.5637</b></span>
        </div>
        <div class="notice" style="margin-top:14px">
          <div class="title">Where this run started</div>
          <div>Started from the ECB fixing of <b>1.5638</b> on 2026-10-28, received 19:23 today.</div>
          <div class="uri">oresmd://fx/EUR?type=fixing&amp;index=fx&amp;source=ECB&amp;ccy=CAD</div>
          <div class="faint">64 fixings over 90 days measured its spread. <a href="#">the feed that started here</a></div>
        </div>`;
}

function renderBackfill() {
    const box = document.querySelector('#backfill');
    if (backfillState === 'progress') {
        box.innerHTML = `
            <div class="dialog">
              <h2>Backfill European Central Bank — EUR/CAD</h2>
              <div class="fields"><span class="mono">2026-01-01 → 2026-10-28 · 300 days</span></div>
              <div class="progress" role="progressbar" aria-valuenow="218" aria-valuemin="0" aria-valuemax="300"><i style="width:72.6%"></i></div>
              <div class="faint">218 of 300 days fetched · oldest first · a day already held is skipped</div>
              <div class="foot"><button class="act">Stop</button></div>
            </div>`;
        return;
    }
    if (backfillState === 'fill') {
        box.innerHTML = `
            <div class="dialog">
              <h2>Backfill European Central Bank — EUR/CAD</h2>
              <div>Done. 300 days asked for, <b>280 stored</b>, 20 already held: the store skipped a value it had.</div>
              <div class="faint">The feed that started from this provider is untouched: its start is the run's, not the store's.</div>
              <div class="foot"><a class="act" href="${href({ backfill: null })}">Close</a></div>
            </div>`;
        return;
    }
    box.innerHTML = `
        <div class="dialog">
          <h2>Ask for a range of European Central Bank — EUR/CAD</h2>
          <div class="fields">
            <label>From<input value="2026-01-01"></label>
            <label>To<input value="2026-10-28"></label>
            <button class="act primary" id="fetch">Fetch 300 days</button>
          </div>
          <div class="faint">A chosen range is a request to the adapter, which the contract does not carry yet. Until it does, this screen offers only the window the service fetches on its own.</div>
        </div>`;
    document.querySelector('#fetch').addEventListener('click', () => {
        location.search = href({ backfill: 'progress' });
    });
}

function renderControls() {
    document.querySelectorAll('[data-q]').forEach((b) => {
        const value = b.dataset.q === 'paused' ? pausedVariant : freshVariant;
        b.setAttribute('aria-checked', String(b.dataset.v === value));
        b.addEventListener('click', () => { location.search = href({ [b.dataset.q]: b.dataset.v }); });
    });
    document.querySelectorAll('[data-state]').forEach((b) => b.addEventListener('click', () => {
        location.search = href({ state: b.dataset.state });
    }));
    document.querySelector('#history').addEventListener('click', () => { location.search = href({ state: null }); });
    document.querySelector('#refresh').addEventListener('click', () => renderAll());
}

function renderAll() {
    renderProviders();
    renderLegend();
    renderProviderPage();
    renderBackfill();
}

renderControls();
renderAll();
