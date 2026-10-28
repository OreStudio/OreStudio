/*
 * A static prototype: mock providers and fixings, and the screen's states chosen
 * by the query string so a reviewer opens each one by address. No framework, no
 * build, no request. Every value is invented.
 *
 *   ?state=live|late|down|never|paused|terms   show one state alone
 *   ?paused=muted|hidden                        a paused provider kept, or removed
 *   ?fresh=quiet|strong                         the two freshness lines
 *   ?compare=1                                  the competing answers side by side
 *   ?provider=<code>                            the provider page shows that provider
 *   ?pause=<code> / ?resume=<code>              a pause, held in the address
 *   ?terms=withdraw                             a terms version that withdraws storage
 *   ?backfill=none|progress|fill                a range at rest, running, done
 */

const q = new URLSearchParams(location.search);
const pausedVariant = q.get('paused') ?? 'muted';
const freshVariant = q.get('fresh') ?? 'quiet';
const backfillState = q.get('backfill') ?? 'none';
const showState = q.get('state');
const compare = q.get('compare') === '1';
const withdrawnTerms = q.get('terms') === 'withdraw';
const selected = q.get('provider') ?? 'ecb';
const pausedByQuery = new Set([...q.getAll('pause')]);
const resumedByQuery = new Set([...q.getAll('resume')]);

const FIXINGS = {
    ecb: [
        ['2026-10-20', '1.5534'], ['2026-10-21', '1.5548'], ['2026-10-22', '1.5561'],
        ['2026-10-23', '1.5570'], ['2026-10-26', '1.5574'], ['2026-10-27', '1.5591'],
        ['2026-10-28', '1.5638'],
    ],
    boc: [
        ['2026-10-20', '0.6401'], ['2026-10-21', '0.6394'], ['2026-10-22', '0.6402'],
        ['2026-10-23', '0.6390'], ['2026-10-26', '0.6388'], ['2026-10-27', '0.6384'],
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
        terms: { accepted: true, version: 'v3', who: 'super_admin' },
        adapter: { state: 'serving', heartbeat: '19:24', refused: 0 },
        last: { date: '2026-10-28', received: '19:23', expected: 'today, on time', onTime: true },
        item: 'FX-ECB-EUR-CAD', pair: 'EUR/CAD', value: '1.5638',
    },
    {
        code: 'boc', name: 'Bank of Canada', items: 'CAD/EUR, CAD/USD',
        terms: { accepted: true, version: 'v2', who: 'super_admin' },
        adapter: { state: 'serving', heartbeat: '19:24', refused: 0 },
        last: { date: '2026-10-27', received: '19:20', expected: 'today, late', onTime: false },
        item: 'FX-BOC-CAD-EUR', pair: 'CAD/EUR', value: '0.6384',
    },
    {
        code: 'imf', name: 'International Monetary Fund', items: 'EUR/USD, USD/CHF, USD/JPY',
        terms: { accepted: !withdrawnTerms, version: withdrawnTerms ? 'v2' : 'v1', who: 'super_admin' },
        adapter: { state: 'serving', heartbeat: '19:24', refused: 0 },
        last: { date: '2026-10-28', received: '19:24', expected: 'today, on time', onTime: true },
        item: 'FX-IMF-EUR-USD', pair: 'EUR/USD', value: '1.5637',
    },
    {
        code: 'tcmb', name: 'Central Bank of Turkiye', items: 'TRY/USD, TRY/EUR',
        terms: { accepted: true, version: 'v1', who: 'super_admin' },
        adapter: { state: 'down', heartbeat: '16:02', refused: 3 },
        last: null,
        item: 'FX-TCMB-TRY-USD', pair: 'TRY/USD', value: null,
    },
    {
        code: 'norges', name: 'Norges Bank', items: 'NOK/EUR, NOK/USD',
        terms: { accepted: true, version: 'v1', who: 'super_admin' },
        adapter: { state: 'serving', heartbeat: '19:24', refused: 0 },
        last: null,
        item: 'FX-NORGES-NOK-EUR', pair: 'NOK/EUR', value: null,
    },
    {
        code: 'boe', name: 'Bank of England', items: 'GBP/USD, GBP/EUR',
        terms: { accepted: true, version: 'v1', who: 'super_admin' },
        adapter: { state: 'serving', heartbeat: '19:24', refused: 0 },
        paused: true,
        last: { date: '2026-10-28', received: '15:00', expected: 'today, on time', onTime: true },
        item: 'FX-BOE-GBP-USD', pair: 'GBP/USD', value: '1.3362',
    },
];

const SENTENCES = {
    live: 'Serving. The last fixing is today\'s and it arrived on time.',
    late: 'Late. The publisher\'s day has passed with no fixing. Read the adapter\'s heartbeat, then ask for a backfill of the missing days.',
    down: 'Not serving. The adapter\'s heartbeat stopped, so nothing is fetched. Restart it, or find out whether its terms were withdrawn.',
    never: 'Never fixed. The terms are accepted and the adapter serves, but no value of this provider is stored yet.',
    paused: 'Paused by a person. The provider is not fetched. Its stored fixings and every feed that started from one stay as they are.',
    terms: 'Terms not accepted. The provider is not fetched until a person accepts the version shown.',
};

const STATE_BADGE = {
    live: ['ok', 'serving'], late: ['warn', 'late'], down: ['bad', 'not serving'],
    never: ['never', 'never fixed'], paused: ['off', 'paused'], terms: ['warn', 'not accepted'],
};

function isPaused(p) {
    if (resumedByQuery.has(p.code)) return false;
    return Boolean(p.paused) || pausedByQuery.has(p.code);
}

function statusOf(p) {
    if (!p.terms.accepted) return 'terms';
    if (isPaused(p)) return 'paused';
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
    if (!p.terms.accepted) {
        return withdrawnTerms && p.code === 'imf'
            ? `<span class="badge bad">v2 withdraws storage</span>`
            : `<span class="badge warn">not accepted · ${p.terms.version}</span>`;
    }
    return `<span class="badge ok">accepted ${p.terms.version}</span> <span class="faint">${esc(p.terms.who)}</span>`;
}

function freshness(p, variant) {
    if (!p.last) return `<div class="fresh"><span class="when faint">—</span><span class="sub">no fixing ever</span></div>`;
    return `<div class="fresh ${variant === 'strong' ? 'strong' : ''}">
        <span class="when">${esc(p.last.date)}</span><span class="sub">${esc(p.last.received)}</span></div>`;
}

function rowActs(p, kind) {
    if (kind === 'paused') return `<button class="act" data-act="resume" data-code="${p.code}">Resume</button>`;
    if (kind === 'terms') return `<button class="act primary">Review terms</button>`;
    return `<button class="act" data-act="open" data-code="${p.code}">Open</button>
            <button class="act" data-act="pause" data-code="${p.code}">Pause</button>`;
}

function row(p, variant) {
    const kind = statusOf(p);
    return `<tr class="${showState ? 'on' : ''} ${pausedVariant === 'muted' && kind === 'paused' ? 'quiet' : ''}">
        <td class="provider">${esc(p.name)}<div class="faint">${esc(p.items)}</div></td>
        <td>${termsCell(p)}</td>
        <td>${freshness(p, variant)}</td>
        <td class="when">${p.last ? esc(p.last.expected) : '—'}</td>
        <td>${badge(kind)}</td>
        <td>${p.adapter.state === 'serving'
            ? `<span class="faint mono">hb ${esc(p.adapter.heartbeat)}</span>`
            : `<span class="badge bad">hb ${esc(p.adapter.heartbeat)}</span>`}</td>
        <td class="acts">${rowActs(p, kind)}</td>
    </tr>`;
}

function comparePanel() {
    if (!compare) return '';
    const paused = PROVIDERS.find((p) => isPaused(p));
    return `<div class="card"><div class="card-head"><h2>Competing answers</h2>
        <p>the two shapes the choice is between, side by side</p></div>
      <div class="card-body compare">
        <div><div class="label">Paused · kept</div>
          <div class="notice" style="background:var(--raised);border-color:var(--line)">
            <div class="title">${esc(paused.name)} ${badge('paused')}</div>
            <div class="faint">the row stays, dimmed, with Resume; its fixings, its chart and its pair on the comparison stay too</div>
          </div></div>
        <div><div class="label">Paused · hidden</div>
          <div class="faint">the row leaves the list; 1 provider paused · <a href="${href({ paused: 'muted' })}">show</a></div></div>
        <div><div class="label">Freshness · quiet</div>${freshness(paused, 'quiet')}</div>
        <div><div class="label">Freshness · strong</div>${freshness(paused, 'strong')}</div>
      </div></div>`;
}

function renderProviders() {
    let shown = PROVIDERS.map((p) => ({ ...p, kind: statusOf(p) }));
    if (showState) shown = shown.filter((p) => p.kind === showState);
    const paused = shown.filter((p) => p.kind === 'paused');
    if (pausedVariant === 'hidden') shown = shown.filter((p) => p.kind !== 'paused');

    document.querySelector('#providers tbody').innerHTML =
        shown.map((p) => row(p, freshVariant)).join('');

    const sub = showState
        ? `showing the ${showState} state · ${shown.length} provider(s)`
        : `${PROVIDERS.length} providers · ${paused.length ? `${paused.length} paused` : 'none paused'}`;
    document.querySelector('#providers-sub').innerHTML = sub +
        (pausedVariant === 'hidden' && paused.length
            ? ` · <a href="${href({ paused: 'muted' })}">show the paused one</a>` : '');

    document.querySelectorAll('[data-act]').forEach((b) => b.addEventListener('click', () => {
        const code = b.dataset.code;
        const target = b.dataset.act === 'pause' ? { pause: code, resume: null }
            : b.dataset.act === 'resume' ? { resume: code, pause: null }
            : { provider: code };
        location.search = href(target);
    }));

    const withdraw = document.querySelector('#withdraw');
    withdraw.hidden = !withdrawnTerms;
    withdraw.innerHTML = withdrawnTerms ? `
        <div class="notice">
          <div class="title">International Monetary Fund's terms v2 withdraw permission to store</div>
          <div>Accepting it is the moment the fixings must go. Deleting them removes 1,204 values and
          leaves 2 feeds that started from one saying where they started, from a value that is gone.</div>
          <div class="foot" style="margin-top:8px">
            <button class="act">Keep the values and decline v2</button>
            <button class="act primary">Accept v2 and delete 1,204 fixings</button>
          </div>
        </div>` : '';
}

function renderLegend() {
    const kinds = showState ? [showState] : ['live', 'late', 'down', 'never', 'paused', 'terms'];
    document.querySelector('#legend').innerHTML = kinds.map((kind) => `
        <div>${badge(kind)}<span>${SENTENCES[kind]}</span></div>`).join('') +
        `<div class="faint">An adapter is never paused: it serves every provider it was given. A row's badge
         is the row's state, read from the terms, the adapter and the fixings together.</div>`;
}

function chart(values) {
    const w = 800, h = 180, pad = 24;
    const numbers = values.map(([, v]) => Number(v));
    const min = Math.min(...numbers), max = Math.max(...numbers);
    const x = (i) => pad + (i * (w - 2 * pad)) / Math.max(values.length - 1, 1);
    const y = (v) => h - pad - ((v - min) / Math.max(max - min, 1e-9)) * (h - 2 * pad);
    const points = values.map(([, v], i) => `${x(i).toFixed(1)},${y(Number(v)).toFixed(1)}`).join(' ');
    const ticks = values.map(([, v], i) => `<circle class="pt" cx="${x(i)}" cy="${y(Number(v))}" r="2.5"></circle>`).join('');
    return `<svg class="chart" viewBox="0 0 ${w} ${h}" preserveAspectRatio="none" role="img" aria-label="The fixing over the window">
        <line class="axis" x1="${pad}" y1="${h - pad}" x2="${w - pad}" y2="${h - pad}"></line>
        <polygon class="area" points="${pad},${h - pad} ${points} ${w - pad},${h - pad}"></polygon>
        <polyline class="line" points="${points}"></polyline>${ticks}
        <text x="${pad}" y="${h - 6}">${esc(values[0][0])}</text>
        <text x="${w - pad}" y="${h - 6}" text-anchor="end">${esc(values[values.length - 1][0])}</text>
    </svg>`;
}

function renderProviderPage() {
    const p = PROVIDERS.find((x) => x.code === selected) ?? PROVIDERS[0];
    const values = FIXINGS[p.code];
    document.querySelector('#provider-title').textContent = `${p.name} — ${p.pair}`;
    document.querySelector('#provider-sub').textContent = values
        ? `${p.item} · ${values.length} fixings in the window`
        : `${p.item} · no fixing stored`;
    const body = [];
    if (values) {
        body.push(chart(values));
        body.push(`<table class="list" style="margin-top:12px">
          <thead><tr><th>Date</th><th>Value</th><th>Item</th><th>Received</th></tr></thead>
          <tbody>${[...values].reverse().map(([date, value]) => `
            <tr><td class="when">${esc(date)}</td><td class="num">${esc(value)}</td>
            <td class="mono faint">oresmd://fx/${p.pair.split('/')[0]}?type=fixing&amp;index=fx&amp;source=${p.code.toUpperCase()}&amp;ccy=${p.pair.split('/')[1]}</td>
            <td class="faint">${esc(p.last?.received ?? '')}</td></tr>`).join('')}</tbody></table>`);
        body.push(`<div style="margin-top:14px" class="label">One pair, as each provider publishes it</div>
          <div style="display:flex;gap:18px;margin-top:4px" class="mono">
            <span>ECB <b>1.5638</b></span>
            <span>BOC <b>0.63926</b> → 1/0.63926 = <b>1.5643</b> <span class="faint">inverted</span></span>
            <span>IMF <b>1.5637</b></span>
          </div>`);
        body.push(`<div class="notice" style="margin-top:14px">
            <div class="title">Where this run started</div>
            <div>Started from the ${esc(p.code.toUpperCase())} fixing of <b>${esc(p.value)}</b> on ${esc(p.last.date)}, received ${esc(p.last.received)}.</div>
            <div class="uri">oresmd://fx/${p.pair.split('/')[0]}?type=fixing&amp;index=fx&amp;source=${p.code.toUpperCase()}&amp;ccy=${p.pair.split('/')[1]}</div>
            <div class="faint">64 fixings over 90 days measured its spread. <a href="#">the feed that started here</a></div>
          </div>`);
    } else {
        body.push(`<div class="notice"><div class="title">No fixing stored for ${esc(p.name)}</div>
          <div>${statusOf(p) === 'paused'
            ? 'The provider is paused, so nothing new arrives. The values it stored before the pause are shown above, if any.'
            : 'The provider is accepted and its adapter serves, but no value has been stored yet. Ask for a backfill, or check the adapter\'s heartbeat.'}</div></div>`);
    }
    document.querySelector('#provider-body').innerHTML = body.join('');
}

function renderBackfill() {
    const box = document.querySelector('#backfill');
    if (backfillState === 'progress') {
        box.innerHTML = `<div class="dialog"><h2>Backfill European Central Bank — EUR/CAD</h2>
            <div class="fields"><span class="mono">2026-01-01 → 2026-10-28 · 300 days</span></div>
            <div class="progress" role="progressbar" aria-valuenow="218" aria-valuemin="0" aria-valuemax="300"><i style="width:72.6%"></i></div>
            <div class="faint">218 of 300 days fetched · oldest first · a day already held is skipped</div>
            <div class="foot"><a class="act" href="${href({ backfill: null })}">Stop</a></div></div>`;
        return;
    }
    if (backfillState === 'fill') {
        box.innerHTML = `<div class="dialog"><h2>Backfill European Central Bank — EUR/CAD</h2>
            <div>Done. 300 days asked for, <b>280 stored</b>, 20 already held: the store skipped a value it had.</div>
            <div class="faint">The feed that started from this provider is untouched: its start is the run's, not the store's.</div>
            <div class="foot"><a class="act" href="${href({ backfill: null })}">Close</a></div></div>`;
        return;
    }
    box.innerHTML = `<div class="dialog"><h2>Ask for a range of European Central Bank — EUR/CAD</h2>
        <div class="fields">
          <label>From<input value="2026-01-01"></label>
          <label>To<input value="2026-10-28"></label>
          <button class="act primary" id="fetch">Fetch 300 days</button>
        </div>
        <div class="faint">A chosen range is a request to the adapter, which the contract does not carry yet.
        Until it does, this screen offers only the window the service fetches on its own.</div></div>`;
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
    document.querySelector('#refresh').addEventListener('click', () => renderAll());
}

function renderAll() {
    document.querySelector('#compare').innerHTML = comparePanel();
    renderProviders();
    renderLegend();
    renderProviderPage();
    renderBackfill();
}

renderControls();
renderAll();
