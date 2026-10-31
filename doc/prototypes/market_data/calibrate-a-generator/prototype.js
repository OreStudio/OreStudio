/* Calibration screen prototype — throwaway. Mock data, no server, no build. */
'use strict';

const CONFIG = {
  pair: 'EUR/USD',
  base: 'EUR',
  quote: 'USD',
  source: 'synthetic-eur-usd',
  process: 'Ornstein–Uhlenbeck',
  ticksPerHour: 12,
  baselineHourly: 0.00045, // the configured mixture's hourly spread
  fixedLevel: 1.1,
};

/** A deterministic daily return series, so the measured volatility is stable. */
function returnsFor(amplitude, phase, n) {
  const out = [];
  for (let i = 0; i < n; i += 1) {
    const drift = 0.00008 * Math.sin((i + phase) / 7.3);
    const cycle = Math.sin((i + phase) / 2.1) * 0.6 + Math.cos((i + phase) / 5.7) * 0.4;
    out.push(amplitude * cycle + drift);
  }
  return out;
}

const CATALOGUE = [
  {
    label: 'ECB · EUR/USD',
    item: 'oresmd://fx/EUR?type=fixing&index=fx&source=ECB&ccy=USD',
    pair: 'EUR/USD',
    provider: 'ecb',
    providerName: 'European Central Bank',
    terms: 'accepted',
    credit: 'Source: ECB statistics',
    policy: 'latest_on_or_before_clock',
    window: 90,
    fixing: { date: '2026-10-30', value: '1.1523', received: '2026-10-30T16:00:00Z' },
    amplitude: 0.0042,
    phase: 3,
  },
  {
    label: 'IMF · EUR/USD',
    item: 'oresmd://fx/EUR?type=fixing&index=fx&source=IMF&ccy=USD',
    pair: 'EUR/USD',
    provider: 'imf',
    providerName: 'International Monetary Fund',
    terms: 'accepted',
    credit: 'Source: IMF statistics',
    policy: 'latest_on_or_before_clock',
    window: 90,
    fixing: { date: '2026-10-24', value: '1.1498', received: '2026-10-24T12:00:00Z' },
    amplitude: 0.0051,
    phase: 11,
  },
  {
    label: 'ECB · USD/EUR (the other way round)',
    item: 'oresmd://fx/USD?type=fixing&index=fx&source=ECB&ccy=EUR',
    pair: 'USD/EUR',
    provider: 'ecb',
    providerName: 'European Central Bank',
    terms: 'accepted',
    credit: 'Source: ECB statistics',
    policy: 'latest_on_or_before_clock',
    window: 60,
    fixing: { date: '2026-10-30', value: '0.8678', received: '2026-10-30T16:00:00Z' },
    amplitude: 0.0038,
    phase: 21,
  },
  {
    label: 'BoC · EUR/USD',
    item: 'oresmd://fx/EUR?type=fixing&index=fx&source=BOC&ccy=USD',
    pair: 'EUR/USD',
    provider: 'boc',
    providerName: 'Bank of Canada',
    terms: 'not accepted',
    credit: 'Terms not accepted',
    policy: 'latest_on_or_before_clock',
    window: 90,
    fixing: { date: '2026-10-30', value: '1.1511', received: '2026-10-30T20:00:00Z' },
    amplitude: 0.0046,
    phase: 31,
  },
  {
    label: 'TCMB · EUR/USD',
    item: 'oresmd://fx/EUR?type=fixing&index=fx&source=TCMB&ccy=USD',
    pair: 'EUR/USD',
    provider: 'tcmb',
    providerName: 'Central Bank of Türkiye',
    terms: 'accepted',
    credit: 'Source: TCMB',
    policy: 'latest_on_or_before_clock',
    window: 30,
    fixing: null,
    amplitude: 0.008,
    phase: 41,
  },
];

const EXTRA = [
  {
    label: 'ECB · EUR/GBP',
    item: 'oresmd://fx/EUR?type=fixing&index=fx&source=ECB&ccy=GBP',
    pair: 'EUR/GBP',
    provider: 'ecb',
    providerName: 'European Central Bank',
    terms: 'accepted',
    credit: 'Source: ECB statistics',
    policy: 'latest_on_or_before_clock',
    window: 45,
    fixing: { date: '2026-10-30', value: '0.8691', received: '2026-10-30T16:00:00Z' },
    amplitude: 0.0035,
    phase: 51,
  },
  {
    label: 'IMF · EUR/JPY',
    item: 'oresmd://fx/EUR?type=fixing&index=fx&source=IMF&ccy=JPY',
    pair: 'EUR/JPY',
    provider: 'imf',
    providerName: 'International Monetary Fund',
    terms: 'accepted',
    credit: 'Source: IMF statistics',
    policy: 'exact',
    window: 90,
    fixing: { date: '2026-10-31', value: '162.41', received: '2026-10-31T09:00:00Z' },
    amplitude: 0.006,
    phase: 61,
  },
];

const RUNS = [
  { at: '2026-10-31 09:12', item: 'ECB · EUR/USD', value: '1.1523', scale: '×1.13', note: 'scheduled start' },
  { at: '2026-10-30 16:04', item: 'ECB · EUR/USD', value: '1.1523', scale: '×1.13', note: 'restart after a pause' },
  { at: '2026-10-30 08:55', item: 'IMF · EUR/USD', value: '1.1498', scale: '×1.27', note: 'ECB item had no fixing yet' },
];

let state = {
  variant: 'a',
  clock: '2026-10-31',
  priceSource: 'fixing',
  fixedLevel: CONFIG.fixedLevel,
  items: CATALOGUE.map((entry, index) => ({ ...entry, rank: index + 1, returns: returnsFor(entry.amplitude, entry.phase, 180) })),
  selectedRank: 1,
};

/* ---------------------------------------------------------------- helpers */

const el = (html) => {
  const t = document.createElement('template');
  t.innerHTML = html.trim();
  return t.content.firstElementChild;
};

const esc = (value) =>
  String(value).replace(/[&<>"']/g, (c) => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;' })[c]);

const fmt = (n, digits = 4) => Number(n).toFixed(digits);
const pct = (n, digits = 3) => `${(n * 100).toFixed(digits)}%`;
const when = (iso) => (iso ? iso.slice(0, 10) + ' ' + iso.slice(11, 16) : '—');

const sorted = () => [...state.items].sort((a, b) => a.rank - b.rank);

function measure(item) {
  const window = Math.max(2, Math.min(180, Number(item.window) || 2));
  const series = item.returns.slice(-window);
  const mean = series.reduce((sum, r) => sum + r, 0) / series.length;
  const variance = series.reduce((sum, r) => sum + (r - mean) ** 2, 0) / (series.length - 1 || 1);
  const daily = Math.sqrt(variance);
  const hourly = daily / Math.sqrt(24);
  return {
    days: series.length,
    daily,
    hourly,
    scale: hourly / CONFIG.baselineHourly,
  };
}

/** The runner's resolution, mirrored: rank order, terms, policy, fixing, inversion. */
function resolve() {
  const trail = [];
  if (state.priceSource === 'fixed') {
    return {
      fromFixing: false,
      level: Number(state.fixedLevel),
      reason: 'the price source is a fixed level, so no fixing is read',
      trail: [{ ok: false, label: 'No fixing read', reason: 'the price source is a fixed level' }],
      volatility: null,
    };
  }
  for (const item of sorted()) {
    const rank = `#${item.rank} ${item.label}`;
    if (item.terms !== 'accepted') {
      trail.push({ ok: false, label: rank, reason: `terms not accepted for ${item.providerName}` });
      continue;
    }
    if (!item.fixing) {
      trail.push({ ok: false, label: rank, reason: 'no fixing is stored for this item' });
      continue;
    }
    if (item.policy === 'exact' && item.fixing.date !== state.clock) {
      trail.push({ ok: false, label: rank, reason: `it holds an exact fixing for ${item.fixing.date}, not ${state.clock}` });
      continue;
    }
    if (item.policy !== 'exact' && item.fixing.date > state.clock) {
      trail.push({ ok: false, label: rank, reason: `its latest fixing, ${item.fixing.date}, is after the clock` });
      continue;
    }
    const reversed = item.pair !== CONFIG.pair;
    const level = reversed ? 1 / Number(item.fixing.value) : Number(item.fixing.value);
    const volatility = measure(item);
    trail.push({
      ok: true,
      label: rank,
      reason: `latest fixing on or before the clock: ${item.fixing.value} on ${item.fixing.date}`,
    });
    return { fromFixing: true, item, level, reversed, volatility, trail, reason: '' };
  }
  return {
    fromFixing: false,
    level: Number(state.fixedLevel),
    reason: 'no ranked item has a usable fixing, so the run keeps its fixed level',
    trail,
    volatility: null,
  };
}

/* ---------------------------------------------------------------- pieces */

function startCard(result) {
  const quantity = fmt(result.level, 4);
  const sub = result.fromFixing
    ? `${esc(result.item.label)} · fixing ${esc(result.item.fixing.date)} · received ${esc(when(result.item.fixing.received))}`
    : esc(result.reason);
  const vol = result.volatility
    ? `<div class="fact"><span class="k">Measured volatility</span><span class="v mono">daily ${pct(result.volatility.daily)} · hourly ${pct(result.volatility.hourly)} over ${result.volatility.days} days</span></div>
       <div class="fact"><span class="k">Applied to the mixture</span><span class="v mono">×${result.volatility.scale.toFixed(2)} the configured spread</span></div>`
    : `<div class="fact"><span class="k">Measured volatility</span><span class="v note">Not measured: no fixing was read.</span></div>`;
  return `
  <section class="card start-card">
    <h2>A run that starts now will take</h2>
    <div class="start-top">
      <div>
        <div class="start-value">${quantity}</div>
        <div class="start-sub">${sub}</div>
      </div>
      <div class="start-badge">
        <span class="pill ${result.fromFixing ? 'ok' : 'warn'}">${result.fromFixing ? 'from a public fixing' : 'kept its fixed level'}</span>
        ${result.reversed ? '<span class="pill muted">read as 1/x</span>' : ''}
        ${result.fromFixing ? `<span class="pill muted">${esc(result.item.provider)}</span>` : ''}
      </div>
    </div>
    <div class="facts">
      <div class="fact wide"><span class="k">Item</span><span class="v mono">${result.fromFixing ? esc(result.item.item) : '—'}</span></div>
      <div class="fact"><span class="k">Provider</span><span class="v">${result.fromFixing ? `${esc(result.item.providerName)}<br><span class="note">${esc(result.item.credit)}</span>` : '—'}</span></div>
      ${vol}
      <div class="fact"><span class="k">Ticks</span><span class="v mono">${CONFIG.ticksPerHour}/hour · ${CONFIG.process}</span></div>
    </div>
    ${result.reversed ? '<p class="note">The item quotes USD/EUR and the generator quotes EUR/USD, so the value is read as 1/x. The run states this; the source value is not changed.</p>' : ''}
    ${result.fromFixing ? '' : '<p class="note warn">This is a stated fallback. No vintage is read; vintage is retired.</p>'}
  </section>`;
}

function itemState(result, item) {
  if (result.fromFixing && result.item.rank === item.rank) {
    return { cls: 'chosen', pill: '<span class="pill ok">starts here</span>' };
  }
  if (item.terms !== 'accepted') {
    return { cls: 'skipped', pill: '<span class="pill bad">terms not accepted</span>' };
  }
  if (!item.fixing) {
    return { cls: 'skipped', pill: '<span class="pill muted">no fixing</span>' };
  }
  if (item.policy === 'exact' && item.fixing.date !== state.clock) {
    return { cls: 'skipped', pill: '<span class="pill warn">wrong date</span>' };
  }
  if (item.policy !== 'exact' && item.fixing.date > state.clock) {
    return { cls: 'skipped', pill: '<span class="pill warn">after the clock</span>' };
  }
  if (!result.fromFixing) {
    return { cls: 'skipped', pill: '<span class="pill muted">usable, but no run started</span>' };
  }
  return { cls: 'skipped', pill: '<span class="pill muted">not reached</span>' };
}

function why(item, result) {
  if (result.fromFixing && result.item.rank === item.rank) {
    return result.reversed ? 'chosen, read as 1/x' : 'chosen';
  }
  if (item.terms !== 'accepted') return `terms not accepted for ${item.providerName}`;
  if (!item.fixing) return 'no fixing is stored for this item';
  if (item.policy === 'exact' && item.fixing.date !== state.clock) return `holds an exact fixing for ${item.fixing.date}, not ${state.clock}`;
  if (item.policy !== 'exact' && item.fixing.date > state.clock) return `its latest fixing, ${item.fixing.date}, is after the clock`;
  return 'a higher-ranked item was chosen first';
}

function controls(item, count) {
  return `
    <div class="row-actions">
      <button class="mini" data-act="up" data-rank="${item.rank}" title="Move up" aria-label="Move up" ${item.rank === 1 ? 'disabled' : ''}>↑</button>
      <button class="mini" data-act="down" data-rank="${item.rank}" title="Move down" aria-label="Move down" ${item.rank === count ? 'disabled' : ''}>↓</button>
      <button class="mini" data-act="remove" data-rank="${item.rank}" title="Remove" aria-label="Remove">×</button>
    </div>`;
}

function policySelect(item) {
  return `
    <select class="policy" data-act="policy" data-rank="${item.rank}">
      <option value="latest_on_or_before_clock" ${item.policy === 'latest_on_or_before_clock' ? 'selected' : ''}>latest on or before clock</option>
      <option value="exact" ${item.policy === 'exact' ? 'selected' : ''}>exact date</option>
    </select>`;
}

function windowInput(item) {
  return `<input class="window" type="number" min="2" max="180" value="${item.window}" data-act="window" data-rank="${item.rank}" title="Window, in days">`;
}

function trailList(result) {
  const rows = result.trail
    .map(
      (step) =>
        `<li class="${step.ok ? 'ok' : 'skip'}"><b>${esc(step.label)}</b> — ${esc(step.reason)}</li>`,
    )
    .join('');
  const last = result.fromFixing
    ? `<li class="ok"><b>Start</b> — takes ${fmt(result.level, 4)} from ${esc(result.item.label)}${result.reversed ? ', read as 1/x' : ''}</li>`
    : `<li class="skip"><b>Start</b> — keeps the fixed level ${fmt(Number(state.fixedLevel), 4)}</li>`;
  return `<ul class="trail">${rows}${last}</ul>`;
}

/* ---------------------------------------------------------------- variant A */

function renderA(result) {
  const count = state.items.length;
  const rows = sorted()
    .map((item) => {
      const s = itemState(result, item);
      return `<tr class="${s.cls}">
        <td class="mono">${item.rank}</td>
        <td><b>${esc(item.label)}</b><span class="uri mono">${esc(item.item)}</span><div class="why">${esc(why(item, result))}</div></td>
        <td>${policySelect(item)}</td>
        <td>${windowInput(item)} <span class="note">d</span></td>
        <td>${s.pill}</td>
        <td>${controls(item, count)}</td>
      </tr>`;
    })
    .join('');
  return `
  <p class="thesis"><b>A · Resolution first.</b> The start is the answer a person came for; the ranked items are its working.</p>
  <div class="grid-a">
    <div class="stack">
      ${startCard(result)}
      <section class="card">
        <h2>Ranked fixing items · ${count}</h2>
        <table>
          <thead><tr><th>#</th><th>Item and provider</th><th>Policy</th><th>Window</th><th>State</th><th></th></tr></thead>
          <tbody>${rows}</tbody>
        </table>
        <p class="note">A rank is a preference, not a guarantee: the first item with a usable fixing starts the run. Up and down reorder, × removes.</p>
        <p><button class="act" data-act="add">Add from the catalogue</button></p>
      </section>
    </div>
    <div class="stack">
      <section class="card">
        <h2>Resolution trail</h2>
        ${trailList(result)}
      </section>
      <section class="card ${result.fromFixing ? '' : 'fallback-card'}">
        <h2>Fixed level</h2>
        <p class="value mono" style="font-size:22px">${fmt(Number(state.fixedLevel), 4)}</p>
        <p class="note">${result.fromFixing ? 'Not used: an item has a fixing. It is kept for a pair no source covers, and the run says so when it keeps it.' : 'Used: no ranked item has a usable fixing. The screen states why, and never reads a vintage.'}</p>
      </section>
    </div>
  </div>`;
}

/* ---------------------------------------------------------------- variant B */

function renderB(result) {
  const count = state.items.length;
  const cards = sorted()
    .map((item) => {
      const s = itemState(result, item);
      const fixing = item.fixing
        ? `${fmt(item.fixing.value, 4)} · ${item.fixing.date} · received ${when(item.fixing.received)}`
        : 'none stored';
      return `<article class="ladder-card ${s.cls}">
        <div class="rank">${item.rank}</div>
        <div class="ladder-body">
          <div class="head"><h3>${esc(item.label)}</h3><span class="who">${esc(item.providerName)}</span>${s.pill}</div>
          <div class="mono" style="margin-top:6px;color:var(--ink-3);font-size:11px">${esc(item.item)}</div>
          <div class="ladder-meta"><span>fixing <b>${fixing}</b></span><span>volatility window <b>${item.window} days</b></span></div>
          <div class="ladder-why">${esc(why(item, result))}</div>
        </div>
        <div class="ladder-controls">
          ${policySelect(item)}
          <div>${windowInput(item)} <span class="note">days</span></div>
          ${controls(item, count)}
        </div>
      </article>`;
    })
    .join('');
  return `
  <p class="thesis"><b>B · Ladder.</b> Each item carries its own reason, beside the item, so the trail is read locally.</p>
  <div class="grid-a">
    <div class="stack">
      <section class="card">
        <h2>Ranked items · ${count}</h2>
        <div class="ladder">${cards}</div>
        <p style="margin-top:14px"><button class="act" data-act="add">Add from the catalogue</button></p>
      </section>
      <section class="card">
        <h2>Resolution trail</h2>
        ${trailList(result)}
      </section>
    </div>
    <div class="stack">
      ${startCard(result)}
      <section class="card ${result.fromFixing ? '' : 'fallback-card'}">
        <h2>Fixed level</h2>
        <p class="mono" style="font-size:22px">${fmt(Number(state.fixedLevel), 4)}</p>
        <p class="note">${result.fromFixing ? 'Not used: an item has a fixing.' : 'Used: no item has a usable fixing. No vintage is read.'}</p>
      </section>
    </div>
  </div>`;
}

/* ---------------------------------------------------------------- variant C */

function renderC(result) {
  const selected = state.items.find((item) => item.rank === state.selectedRank) ?? sorted()[0];
  const chips = sorted()
    .map((item) => {
      const s = itemState(result, item);
      return `<button class="chip ${s.cls} ${item.rank === selected.rank ? 'sel' : ''}" data-act="select" data-rank="${item.rank}">
        <span class="n">${item.rank}</span>
        <span>${esc(item.label)}</span>
        ${s.pill}
      </button>`;
    })
    .join('');
  const chosen = result.fromFixing;
  const history = RUNS.map(
    (run) => `<tr><td class="mono">${esc(run.at)}</td><td>${esc(run.item)}</td><td class="mono">${esc(run.value)}</td><td class="mono">${esc(run.scale)}</td><td class="note">${esc(run.note)}</td></tr>`,
  ).join('');
  return `
  <p class="thesis"><b>C · Flow.</b> The start is a pipeline: items into the resolver into the process, with the run's own record as the evidence.</p>
  <div class="flow">
    <div class="stack">
      <section class="card">
        <h2>From the fixings to the process</h2>
        <div class="diagram">
          <div class="col">
            <div class="col-title">Ranked items</div>
            ${chips}
          </div>
          <div class="arrow ${chosen ? '' : 'off'}"></div>
          <div class="col">
            <div class="node">
              <h3>Resolver</h3>
              <div class="big">${fmt(result.level, 4)}</div>
              <div class="sub">${chosen ? `${esc(result.item.provider)} · ${esc(result.item.fixing.date)}${result.reversed ? ' · 1/x' : ''}` : 'fixed level, no fixing read'}</div>
              ${result.volatility ? `<div class="sub">hourly σ ${pct(result.volatility.hourly)} · ×${result.volatility.scale.toFixed(2)}</div>` : '<div class="sub">no volatility measured</div>'}
            </div>
          </div>
          <div class="arrow ${chosen ? '' : 'off'}"></div>
          <div class="col">
            <div class="node feed">
              <h3>Process and feed</h3>
              <div class="big" style="font-size:15px">EUR/USD</div>
              <div class="sub">${esc(CONFIG.process)} · ${CONFIG.ticksPerHour} ticks/hour</div>
              <div class="sub mono">${esc(CONFIG.source)}</div>
            </div>
          </div>
        </div>
        <p class="note" style="margin-top:12px">A chip is one ranked item. The resolver walks the ranks and takes the first usable fixing; the process receives the level and the scaled spread.</p>
      </section>
      <section class="card">
        <h2>Last runs</h2>
        <table>
          <thead><tr><th>Started</th><th>Item taken</th><th>Value</th><th>Scale</th><th>Why</th></tr></thead>
          <tbody>${history}</tbody>
        </table>
        <p class="note">The trail is the feed's own record: restarting a run states the item it took, or that it kept its level.</p>
      </section>
    </div>
    <div class="stack">
      <section class="card inspector">
        <h2>Inspector</h2>
        <div class="kv">
          <span class="k">Item</span><span class="v mono">${esc(selected.item)}</span>
          <span class="k">Pair on the item</span><span class="v mono">${esc(selected.pair)}</span>
          <span class="k">Provider</span><span class="v">${esc(selected.providerName)}</span>
          <span class="k">Terms</span><span class="v">${esc(selected.terms)}</span>
          <span class="k">Credit</span><span class="v">${esc(selected.credit)}</span>
          <span class="k">Policy</span><span class="v">${esc(selected.policy === 'exact' ? 'exact date' : 'latest on or before clock')}</span>
          <span class="k">Window</span><span class="v mono">${selected.window} days</span>
          <span class="k">Fixing</span><span class="v mono">${selected.fixing ? `${fmt(selected.fixing.value, 4)} · ${selected.fixing.date}` : 'none'}</span>
          <span class="k">Received</span><span class="v mono">${selected.fixing ? when(selected.fixing.received) : '—'}</span>
          <span class="k">Read as</span><span class="v">${selected.rank === (chosen ? result.item.rank : -1) && result.reversed ? '1/x' : 'as published'}</span>
        </div>
        <div style="margin-top:14px;display:flex;gap:8px;flex-wrap:wrap">
          ${policySelect(selected)}
          ${windowInput(selected)}
          ${controls(selected, state.items.length)}
        </div>
        <p style="margin-top:14px"><button class="act" data-act="add">Add from the catalogue</button></p>
      </section>
      <section class="card ${result.fromFixing ? '' : 'fallback-card'}">
        <h2>Fixed level</h2>
        <p class="mono" style="font-size:22px">${fmt(Number(state.fixedLevel), 4)}</p>
        <p class="note">${result.fromFixing ? 'Not used.' : 'Used: no item has a usable fixing.'} Vintage is retired and is not read.</p>
      </section>
    </div>
  </div>`;
}

/* ---------------------------------------------------------------- actions */

function setVariant(variant) {
  state.variant = variant;
  const url = new URL(window.location.href);
  url.searchParams.set('variant', variant);
  window.history.replaceState({}, '', url);
  render();
}

function normalise() {
  state.items.sort((a, b) => a.rank - b.rank);
  state.items.forEach((item, index) => {
    item.rank = index + 1;
  });
}

function act(kind, rank) {
  const item = state.items.find((entry) => entry.rank === rank);
  if (kind === 'up' && item) {
    const above = state.items.find((entry) => entry.rank === rank - 1);
    if (above) {
      above.rank = rank;
      item.rank = rank - 1;
    }
  }
  if (kind === 'down' && item) {
    const below = state.items.find((entry) => entry.rank === rank + 1);
    if (below) {
      below.rank = rank;
      item.rank = rank + 1;
    }
  }
  if (kind === 'remove' && item) {
    state.items = state.items.filter((entry) => entry.rank !== rank);
    if (state.selectedRank === rank) state.selectedRank = 1;
  }
  if (kind === 'add') {
    const next = EXTRA.find((entry) => !state.items.some((current) => current.item === entry.item));
    if (next) {
      state.items.push({
        ...next,
        rank: state.items.length + 1,
        returns: returnsFor(next.amplitude, next.phase, 180),
      });
    }
  }
  if (kind === 'select' && item) state.selectedRank = rank;
  normalise();
  render();
}

/* ---------------------------------------------------------------- render */

function render() {
  const result = resolve();
  document.getElementById('variant-a').innerHTML = renderA(result);
  document.getElementById('variant-b').innerHTML = renderB(result);
  document.getElementById('variant-c').innerHTML = renderC(result);
  document.querySelectorAll('.variant').forEach((node, index) => {
    node.classList.toggle('active', ['a', 'b', 'c'][index] === state.variant);
  });
  document.querySelectorAll('.proto-switch button').forEach((button) => {
    button.setAttribute('aria-selected', String(button.dataset.variant === state.variant));
  });
  document.getElementById('clock').value = state.clock;
  document.getElementById('priceSource').value = state.priceSource;
  document.getElementById('fixedLevel').value = state.fixedLevel;
  document.getElementById('controlNote').innerHTML =
    state.priceSource === 'fixed'
      ? '<b>Fixed level mode:</b> no fixing is read. Use it only for a pair no source covers.'
      : 'The resolver reads the ranked items in order, as of the clock.';
  document.getElementById('stateBody').textContent = JSON.stringify(
    {
      variant: state.variant,
      clock: state.clock,
      priceSource: state.priceSource,
      fixedLevel: Number(state.fixedLevel),
      items: sorted().map((item) => ({
        rank: item.rank,
        item: item.item,
        pair: item.pair,
        provider: item.provider,
        terms: item.terms,
        policy: item.policy,
        window: item.window,
        fixing: item.fixing,
      })),
      resolution: {
        fromFixing: result.fromFixing,
        level: Number(result.level.toFixed(6)),
        item: result.fromFixing ? result.item.item : null,
        inverted: Boolean(result.reversed),
        volatility: result.volatility
          ? {
              days: result.volatility.days,
              daily: Number(result.volatility.daily.toFixed(6)),
              hourly: Number(result.volatility.hourly.toFixed(6)),
              scale: Number(result.volatility.scale.toFixed(4)),
            }
          : null,
        trail: result.trail,
        reason: result.reason,
      },
    },
    null,
    2,
  );
  document.getElementById('state').open = false;
}

function onChange(event) {
  const target = event.target;
  if (target.id === 'clock') state.clock = target.value;
  if (target.id === 'priceSource') state.priceSource = target.value;
  if (target.id === 'fixedLevel') state.fixedLevel = Number(target.value) || 0;
  if (target.dataset.act === 'policy') {
    const item = state.items.find((entry) => entry.rank === Number(target.dataset.rank));
    if (item) item.policy = target.value;
  }
  if (target.dataset.act === 'window') {
    const item = state.items.find((entry) => entry.rank === Number(target.dataset.rank));
    if (item) item.window = Math.max(2, Math.min(180, Number(target.value) || 2));
  }
  render();
}

function onClick(event) {
  const button = event.target.closest('[data-act]');
  if (!button) return;
  const kind = button.dataset.act;
  if (kind === 'policy' || kind === 'window') return;
  act(kind, Number(button.dataset.rank));
}

document.addEventListener('DOMContentLoaded', () => {
  const params = new URLSearchParams(window.location.search);
  const variant = params.get('variant');
  if (['a', 'b', 'c'].includes(variant)) state.variant = variant;
  document.body.addEventListener('click', onClick);
  document.body.addEventListener('change', onChange);
  document.getElementById('resolve').addEventListener('click', render);
  document.querySelectorAll('.proto-switch button').forEach((button) => {
    button.addEventListener('click', () => setVariant(button.dataset.variant));
  });
  render();
});
