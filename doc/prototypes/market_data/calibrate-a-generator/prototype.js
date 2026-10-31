/* Calibration screen prototype — throwaway. Mock data, no server, no build. */
'use strict';

const PROVIDERS = {
  ecb: { name: 'European Central Bank', terms: 'accepted', credit: 'Source: ECB statistics' },
  imf: { name: 'International Monetary Fund', terms: 'accepted', credit: 'Source: IMF statistics' },
  boc: { name: 'Bank of Canada', terms: 'not accepted', credit: 'Terms not accepted' },
  tcmb: { name: 'Central Bank of Türkiye', terms: 'accepted', credit: 'Source: TCMB' },
};

const ITEMS = [
  { id: 'ecb-eurusd', provider: 'ecb', pair: 'EUR/USD', value: '1.1523', date: '2026-10-30', received: '2026-10-30T16:00:00Z', amplitude: 0.0042, phase: 3 },
  { id: 'ecb-eurusd-31', provider: 'ecb', pair: 'EUR/USD', value: '1.1560', date: '2026-10-31', received: '2026-10-31T16:00:00Z', amplitude: 0.0042, phase: 3 },
  { id: 'ecb-usdeur', provider: 'ecb', pair: 'USD/EUR', value: '0.8678', date: '2026-10-30', received: '2026-10-30T16:00:00Z', amplitude: 0.0038, phase: 21 },
  { id: 'imf-eurusd', provider: 'imf', pair: 'EUR/USD', value: '1.1498', date: '2026-10-24', received: '2026-10-24T12:00:00Z', amplitude: 0.0051, phase: 11 },
  { id: 'boc-eurusd', provider: 'boc', pair: 'EUR/USD', value: '1.1511', date: '2026-10-30', received: '2026-10-30T20:00:00Z', amplitude: 0.0046, phase: 31 },
  { id: 'ecb-eurgbp', provider: 'ecb', pair: 'EUR/GBP', value: '0.8691', date: '2026-10-30', received: '2026-10-30T16:00:00Z', amplitude: 0.0035, phase: 51 },
  { id: 'ecb-eurchf', provider: 'ecb', pair: 'EUR/CHF', value: '0.9412', date: '2026-10-30', received: '2026-10-30T16:00:00Z', amplitude: 0.0029, phase: 55 },
  { id: 'ecb-eurjpy', provider: 'ecb', pair: 'EUR/JPY', value: '162.41', date: '2026-10-30', received: '2026-10-30T16:00:00Z', amplitude: 0.006, phase: 61 },
  { id: 'imf-eurjpy', provider: 'imf', pair: 'EUR/JPY', value: '162.10', date: '2026-10-30', received: '2026-10-30T12:00:00Z', amplitude: 0.0064, phase: 63 },
  { id: 'imf-usdjpy', provider: 'imf', pair: 'USD/JPY', value: '149.80', date: '2026-10-30', received: '2026-10-30T12:00:00Z', amplitude: 0.0056, phase: 71 },
  { id: 'imf-usdgbp', provider: 'imf', pair: 'USD/GBP', value: '0.7890', date: '2026-10-30', received: '2026-10-30T12:00:00Z', amplitude: 0.0049, phase: 75 },
  { id: 'boc-gbpusd', provider: 'boc', pair: 'GBP/USD', value: '1.2710', date: '2026-10-30', received: '2026-10-30T20:00:00Z', amplitude: 0.0044, phase: 81 },
];

const FEEDS = [
  { name: 'synthetic-eur-usd', pair: 'EUR/USD', fixedLevel: 1.1, process: 'Ornstein–Uhlenbeck', ticks: 12 },
  { name: 'synthetic-gbp-usd', pair: 'GBP/USD', fixedLevel: 1.27, process: 'Ornstein–Uhlenbeck', ticks: 6 },
  { name: 'synthetic-usd-jpy', pair: 'USD/JPY', fixedLevel: 150, process: 'Ornstein–Uhlenbeck', ticks: 12 },
  { name: 'synthetic-eur-gbp', pair: 'EUR/GBP', fixedLevel: 0.87, process: 'Ornstein–Uhlenbeck', ticks: 6 },
  { name: 'synthetic-eur-chf', pair: 'EUR/CHF', fixedLevel: 0.94, process: 'Ornstein–Uhlenbeck', ticks: 6 },
  { name: 'synthetic-aud-usd', pair: 'AUD/USD', fixedLevel: 0.66, process: 'Ornstein–Uhlenbeck', ticks: 4 },
];

const EXTRA_FEEDS = [
  { name: 'synthetic-nzd-usd', pair: 'NZD/USD', fixedLevel: 0.59, process: 'Ornstein–Uhlenbeck', ticks: 4 },
  { name: 'synthetic-usd-cad', pair: 'USD/CAD', fixedLevel: 1.36, process: 'Ornstein–Uhlenbeck', ticks: 4 },
];

const BASELINE_HOURLY = 0.00045;
const RECALCULATIONS = [
  { at: '2026-10-31 09:12', scope: 'all FX feeds', resolved: '5 from a fixing', fallback: '1 kept its level', actor: 'scheduler' },
  { at: '2026-10-30 16:04', scope: 'all FX feeds', resolved: '5 from a fixing', fallback: '1 kept its level', actor: 'scheduler' },
  { at: '2026-10-30 08:55', scope: 'EUR/USD only', resolved: '1 from a fixing', fallback: 'none', actor: 'marco' },
];

const DEFAULT_POLICY = {
  assetClass: 'FX',
  windowDays: 90,
  allowInverse: true,
  fallback: 'keep_fixed',
  providerOrder: ['ecb', 'imf', 'boc'],
};

function returnsFor(amplitude, phase, n) {
  const out = [];
  for (let i = 0; i < n; i += 1) {
    const drift = 0.00008 * Math.sin((i + phase) / 7.3);
    const cycle = Math.sin((i + phase) / 2.1) * 0.6 + Math.cos((i + phase) / 5.7) * 0.4;
    out.push(amplitude * cycle + drift);
  }
  return out;
}

const CATALOGUE = ITEMS.map((item) => ({ ...item, returns: returnsFor(item.amplitude, item.phase, 180) }));

let state = {
  screen: 'list',
  policyMode: 'automatic',
  clock: '2026-10-30',
  policy: { ...DEFAULT_POLICY, providerOrder: [...DEFAULT_POLICY.providerOrder] },
  feeds: FEEDS.map((feed) => ({ ...feed, overrideItem: null, overrideWindow: null, explicit: null })),
  recalc: [...RECALCULATIONS],
  filter: 'all',
  feedName: 'synthetic-eur-usd',
  feedTab: 'record',
  selectedCandidate: null,
};

/* ---------------------------------------------------------------- helpers */

const esc = (value) =>
  String(value).replace(/[&<>"']/g, (c) => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;' })[c]);
const fmt = (n, digits = 4) => Number(n).toFixed(digits);
const pct = (n, digits = 3) => `${(n * 100).toFixed(digits)}%`;
const when = (iso) => (iso ? iso.slice(0, 10) + ' ' + iso.slice(11, 16) : '—');
const uriOf = (item) => `oresmd://fx/${item.pair.split('/')[0]}?type=fixing&index=fx&source=${item.provider.toUpperCase()}&ccy=${item.pair.split('/')[1]}`;
const feedOf = (name) => state.feeds.find((feed) => feed.name === name) ?? state.feeds[0];
const windowOf = (feed) => feed.overrideWindow ?? state.policy.windowDays;

function measure(item, windowDays) {
  const window = Math.max(2, Math.min(180, Number(windowDays) || 2));
  const series = item.returns.slice(-window);
  const mean = series.reduce((sum, r) => sum + r, 0) / series.length;
  const variance = series.reduce((sum, r) => sum + (r - mean) ** 2, 0) / (series.length - 1 || 1);
  const daily = Math.sqrt(variance);
  const hourly = daily / Math.sqrt(24);
  return { days: series.length, daily, hourly, scale: hourly / BASELINE_HOURLY };
}

/** The candidate list for a feed's pair, derived from the catalogue and the policy. */
function candidatesFor(feed) {
  const [base, quote] = feed.pair.split('/');
  const order = state.policy.providerOrder;
  return CATALOGUE.map((item) => {
    const [itemBase, itemQuote] = item.pair.split('/');
    const direct = itemBase === base && itemQuote === quote;
    const inverse = itemBase === quote && itemQuote === base;
    if (!direct && !(inverse && state.policy.allowInverse)) return null;
    const provider = PROVIDERS[item.provider];
    const afterClock = item.date > state.clock;
    const usable = provider.terms === 'accepted' && !afterClock;
    const rank = order.indexOf(item.provider);
    return { ...item, providerInfo: provider, direct, inverse, usable, order: rank < 0 ? 99 : rank };
  })
    .filter(Boolean)
    .sort((a, b) => {
      if (a.order !== b.order) return a.order - b.order;
      if (a.direct !== b.direct) return a.direct ? -1 : 1;
      return b.date.localeCompare(a.date);
    });
}

function skipReason(candidate) {
  if (candidate.providerInfo.terms !== 'accepted') return `terms not accepted for ${candidate.providerInfo.name}`;
  if (candidate.date > state.clock) return `its fixing is after the clock (${candidate.date})`;
  return 'a higher-ranked candidate was chosen first';
}

/** Resolve one feed: the pinned item first, then the policy order. */
function resolveFeed(feed) {
  const candidates = candidatesFor(feed);
  const inScope = (candidate) => feed.explicit === null || feed.explicit.includes(candidate.id);
  const ordered = candidates.filter(inScope);
  if (feed.overrideItem) {
    const pinned = ordered.findIndex((candidate) => candidate.id === feed.overrideItem);
    if (pinned > 0) ordered.unshift(ordered.splice(pinned, 1)[0]);
  }
  const trail = [];
  for (const candidate of ordered) {
    if (!candidate.usable) {
      trail.push({ ok: false, label: `${candidate.providerInfo.name} · ${candidate.pair}`, reason: skipReason(candidate) });
      continue;
    }
    const level = candidate.inverse ? 1 / Number(candidate.value) : Number(candidate.value);
    const volatility = measure(candidate, windowOf(feed));
    trail.push({
      ok: true,
      label: `${candidate.providerInfo.name} · ${candidate.pair}`,
      reason: `fixing ${candidate.value} on ${candidate.date}${candidate.inverse ? ', read as 1/x' : ''}`,
    });
    return { feed, candidates, fromFixing: true, candidate, level, volatility, trail };
  }
  const blocked = state.policy.fallback === 'refuse';
  return {
    feed,
    candidates,
    fromFixing: false,
    level: blocked ? null : Number(feed.fixedLevel),
    volatility: null,
    trail,
    blocked,
    reason: blocked
      ? 'no candidate has a usable fixing, and the policy refuses to start without one'
      : 'no candidate has a usable fixing, so the feed keeps its fixed level',
  };
}

function resolveAll() {
  return state.feeds.map(resolveFeed);
}

/* ---------------------------------------------------------------- pieces */

function badge(kind, text) {
  return `<span class="badge ${kind}">${text}</span>`;
}

function card({ title, sub = '', aside = '', cls = '', body }) {
  return `<section class="card ${cls}"><div class="card-head"><h2>${title}</h2>${sub ? `<p>${sub}</p>` : ''}${aside}</div><div class="card-body">${body}</div></section>`;
}

function startCard(result) {
  const kind = result.fromFixing ? 'ok' : result.blocked ? 'bad' : 'warn';
  const label = result.fromFixing ? 'from a public fixing' : result.blocked ? 'blocked by the policy' : 'kept its fixed level';
  const sub = result.fromFixing
    ? `${esc(result.candidate.providerInfo.name)} · ${esc(result.candidate.pair)} · fixing ${esc(result.candidate.date)} · received ${esc(when(result.candidate.received))}`
    : esc(result.reason);
  const vol = result.volatility
    ? `<div class="fact"><span class="k">Measured volatility</span><span class="v mono">daily ${pct(result.volatility.daily)} · hourly ${pct(result.volatility.hourly)} over ${result.volatility.days} days</span></div>
       <div class="fact"><span class="k">Applied to the mixture</span><span class="v mono">×${result.volatility.scale.toFixed(2)} the configured spread</span></div>`
    : `<div class="fact"><span class="k">Measured volatility</span><span class="v note">Not measured: no fixing was read.</span></div>`;
  const body = `
    <div class="start-top">
      <div>
        <div class="start-value">${result.level === null ? '—' : fmt(result.level, 4)}</div>
        <div class="start-sub">${sub}</div>
      </div>
      <div class="start-badge">
        ${badge(kind, label)}
        ${result.candidate && result.candidate.inverse ? badge('off', 'read as 1/x') : ''}
        ${result.candidate ? badge('off', result.candidate.provider) : ''}
      </div>
    </div>
    <div class="facts">
      <div class="fact wide"><span class="k">Item</span><span class="v mono">${result.candidate ? esc(uriOf(result.candidate)) : '—'}</span></div>
      <div class="fact"><span class="k">Provider</span><span class="v">${result.candidate ? `${esc(result.candidate.providerInfo.name)}<br><span class="note">${esc(result.candidate.providerInfo.credit)}</span>` : '—'}</span></div>
      ${vol}
      <div class="fact"><span class="k">Window and ticks</span><span class="v mono">${windowOf(result.feed)} days · ${result.feed.ticks}/hour</span></div>
    </div>
    ${result.candidate && result.candidate.inverse ? '<p class="note">The item quotes the pair the other way round, so the level is read as 1/x. The source value is not changed.</p>' : ''}
    ${result.fromFixing ? '' : '<p class="note warn">This is a stated ' + (result.blocked ? 'refusal' : 'fallback') + '. No vintage is read; vintage is retired.</p>'}`;
  return card({ title: 'A run that starts now will take', cls: 'start-card', body });
}

function measuresOf(candidate, windowDays) {
  const window = Math.max(2, Math.min(180, Number(windowDays) || 2));
  const series = candidate.returns.slice(-window);
  const mean = series.reduce((sum, r) => sum + r, 0) / series.length;
  const variance = series.reduce((sum, r) => sum + (r - mean) ** 2, 0) / (series.length - 1 || 1);
  const daily = Math.sqrt(variance);
  const hourly = daily / Math.sqrt(24);
  const sd = daily || 1;
  const jumps = series.filter((r) => Math.abs(r) > 3 * sd).length;
  const latest = candidate.date;
  const age = Math.max(0, Math.round((Date.parse(state.clock) - Date.parse(latest)) / 86400000));
  const others = CATALOGUE.filter(
    (item) => item.pair === candidate.pair && item.provider !== candidate.provider && PROVIDERS[item.provider].terms === 'accepted',
  );
  const dispersion = others.length
    ? (others.map((item) => Math.abs(Number(item.value) - Number(candidate.value)) / Number(candidate.value) * 10000))
    : [];
  return {
    window,
    samples: series.length,
    daily,
    hourly,
    scale: hourly / BASELINE_HOURLY,
    drift: mean * 252,
    min: Math.min(...series),
    max: Math.max(...series),
    jumps,
    age,
    dispersion,
    series,
  };
}

function sparkline(series) {
  const width = 300;
  const height = 64;
  const max = Math.max(...series.map((v) => Math.abs(v)), 1e-6);
  const step = series.length > 1 ? width / (series.length - 1) : width;
  const points = series.map((value, index) => [index * step, height / 2 - (value / max) * (height / 2 - 4)]);
  const line = points.map(([x, y], index) => `${index === 0 ? 'M' : 'L'}${x.toFixed(1)},${y.toFixed(1)}`).join(' ');
  const area = `${line} L${width},${height / 2} L0,${height / 2} Z`;
  return `<svg class="spark" viewBox="0 0 ${width} ${height}" role="img" aria-label="Daily log returns over the window">
    <line x1="0" y1="${height / 2}" x2="${width}" y2="${height / 2}" class="axis"></line>
    <path d="${area}" class="area"></path>
    <path d="${line}" class="line"></path>
  </svg>`;
}

function measureBadge(used) {
  return used ? badge('ok', 'used today') : badge('off', 'future input');
}

function measuresCard(result) {
  const candidate = result.candidate ?? result.candidates.find((entry) => entry.direct && entry.usable) ?? result.candidates[0];
  if (!candidate) {
    return card({ title: 'Measures behind the start', sub: 'no candidate has a series', body: '<p class="note">No fixing history covers this pair, so there is nothing to measure.</p>' });
  }
  const m = measuresOf(candidate, windowOf(result.feed));
  const dispersion = m.dispersion.length
    ? `${m.dispersion.map((value) => value.toFixed(0)).join(' / ')} bp against ${CATALOGUE.filter((item) => item.pair === candidate.pair && item.provider !== candidate.provider && PROVIDERS[item.provider].terms === 'accepted').map((item) => item.provider.toUpperCase()).join(', ')}`
    : 'no second accepted provider for this pair';
  return card({
    title: 'Measures behind the start',
    sub: `${candidate.provider.toUpperCase()} · ${candidate.pair} · ${m.window} days`,
    aside: badge('off', 'information'),
    body: `<div class="measure-metrics">
      <div class="metric"><span class="k">Realised volatility</span><span class="v">${pct(m.daily)}<span class="sub"> daily · ${pct(m.hourly)} hourly</span></span></div>
      <div class="metric"><span class="k">Drift, annualised</span><span class="v">${(m.drift * 100).toFixed(1)}%</span></div>
      <div class="metric"><span class="k">Daily range</span><span class="v mono">${pct(m.min)} … ${pct(m.max)}</span></div>
      <div class="metric"><span class="k">Samples</span><span class="v">${m.samples}<span class="sub"> of ${m.window} days · ${m.age} day${m.age === 1 ? '' : 's'} old</span></span></div>
    </div>
    ${sparkline(m.series)}
    <ul class="measure-list">
      <li><span>Volatility sets the mixture scale</span>${measureBadge(true)}</li>
      <li><span>Drift could tilt the process's mean</span>${measureBadge(false)}</li>
      <li><span>Jumps above three deviations: ${m.jumps}</span>${measureBadge(false)}</li>
      <li><span>Provider dispersion: ${dispersion}</span>${measureBadge(false)}</li>
      <li><span>Freshness and coverage could widen the window or skip a source</span>${measureBadge(false)}</li>
    </ul>
    <p class="note">The measures describe the fixings; only the realised volatility changes the calibration today. The rest are recorded for a later policy that learns from them.</p>`,
  });
}

function trailList(result) {
  const rows = result.trail
    .map((step) => `<li class="${step.ok ? 'ok' : 'skip'}"><b>${esc(step.label)}</b> — ${esc(step.reason)}</li>`)
    .join('');
  const last = result.fromFixing
    ? `<li class="ok"><b>Start</b> — takes ${fmt(result.level, 4)} from ${esc(result.candidate.providerInfo.name)}${result.candidate.inverse ? ', read as 1/x' : ''}</li>`
    : `<li class="skip"><b>Start</b> — ${result.blocked ? 'does not start' : `keeps the fixed level ${fmt(Number(result.feed.fixedLevel), 4)}`}</li>`;
  return `<ul class="trail">${rows}${last}</ul>`;
}

function metrics(results) {
  const fromFixing = results.filter((result) => result.fromFixing).length;
  const fallback = results.filter((result) => !result.fromFixing && !result.blocked).length;
  const blocked = results.filter((result) => result.blocked).length;
  const overrides = state.feeds.filter((feed) => feed.overrideItem || feed.overrideWindow).length;
  return `<div class="metrics">
    <div class="metric"><span class="k">Feeds</span><span class="v">${state.feeds.length}</span></div>
    <div class="metric"><span class="k">From a fixing</span><span class="v up">${fromFixing}</span></div>
    <div class="metric"><span class="k">Kept a level</span><span class="v warn">${fallback}</span></div>
    <div class="metric"><span class="k">Blocked</span><span class="v down">${blocked}</span></div>
    <div class="metric"><span class="k">Overrides</span><span class="v">${overrides}</span></div>
  </div>`;
}

function policyCard() {
  const automatic = state.policyMode === 'automatic';
  const fallback = state.policy.fallback === 'refuse' ? 'refuse without a fixing' : 'keep the fixed level';
  const summary = `${state.policy.providerOrder.map((code) => PROVIDERS[code].name).join(' → ')} · ${state.policy.windowDays} days · ${state.policy.allowInverse ? 'inverse allowed' : 'inverse not allowed'} · ${fallback}`;
  if (automatic) {
    return card({
      title: 'FX calibration',
      sub: 'automatic, from the fixing catalogue',
      aside: badge('off', 'automatic'),
      body: `<p class="note">With no policy, every FX feed calibrates from the catalogue: providers in their catalogue order, a 90-day window, an inverse allowed, and the fixed level when no source covers the pair.</p>
      <p class="policy-summary mono">${esc(summary)}</p>
      <p style="margin-top:10px"><button class="act" data-act="customise">Customise this class</button></p>
      <p class="note" style="margin-top:6px">Optional. Nothing waits on it, and a feed may still pin its own item.</p>`,
    });
  }
  const order = state.policy.providerOrder
    .map(
      (code, index) => `<div class="order-row">
        <span class="rank">${index + 1}</span>
        <span class="order-name">${esc(PROVIDERS[code].name)} <span class="note">· ${esc(PROVIDERS[code].terms === 'accepted' ? code : code + ' · terms not accepted')}</span></span>
        <span class="row-actions">
          <button class="mini" data-act="order-up" data-provider="${code}" aria-label="Move up" ${index === 0 ? 'disabled' : ''}>↑</button>
          <button class="mini" data-act="order-down" data-provider="${code}" aria-label="Move down" ${index === state.policy.providerOrder.length - 1 ? 'disabled' : ''}>↓</button>
        </span>
      </div>`,
    )
    .join('');
  const fallbackOptions = [
    ['keep_fixed', 'Keep the fixed level, and say so'],
    ['refuse', 'Refuse to start the feed'],
  ]
    .map(([value, text]) => `<option value="${value}" ${state.policy.fallback === value ? 'selected' : ''}>${text}</option>`)
    .join('');
  return card({
    title: 'FX calibration',
    sub: 'this class has a custom policy',
    aside: badge('off', 'custom'),
    body: `<div class="policy-fields">
      <label>Volatility window, in days
        <input type="number" id="policyWindow" min="2" max="180" value="${state.policy.windowDays}">
      </label>
      <label>When no candidate has a fixing
        <select id="policyFallback">${fallbackOptions}</select>
      </label>
      <label class="check"><input type="checkbox" id="policyInverse" ${state.policy.allowInverse ? 'checked' : ''}> Read a pair the other way round as 1/x</label>
    </div>
    <div class="subhead">Provider order</div>
    <div class="order">${order}</div>
    <p class="note" style="margin-top:10px">The order decides which accepted provider wins when several carry the pair. A feed needs nothing of its own unless it overrides.</p>
    <p style="margin-top:8px"><button class="act" data-act="reset-policy">Return to automatic</button></p>`,
  });
}

function needsAttention(result) {
  return !result.fromFixing || result.feed.overrideItem || result.feed.overrideWindow || result.feed.explicit !== null;
}

function isOverridden(result) {
  return Boolean(result.feed.overrideItem || result.feed.overrideWindow || result.feed.explicit !== null);
}

function visibleFeeds(results) {
  if (state.filter === 'attention') return results.filter(needsAttention);
  if (state.filter === 'overridden') return results.filter(isOverridden);
  return results;
}

function feedTable(results) {
  const shown = visibleFeeds(results);
  const rows = shown
    .map((result) => {
      const stateBadge = result.fromFixing
        ? badge('ok', 'from a fixing')
        : result.blocked
          ? badge('bad', 'blocked')
          : badge('warn', 'kept a level');
      const source = result.candidate
        ? `${esc(result.candidate.provider)} · ${esc(result.candidate.pair)} · ${esc(result.candidate.date)}`
        : '—';
      const override = result.feed.overrideItem || result.feed.overrideWindow ? badge('off', 'override') : '';
      return `<tr>
        <td><b>${esc(result.feed.pair)}</b><div class="why mono">${esc(result.feed.name)}</div></td>
        <td class="num">${result.level === null ? '—' : fmt(result.level, 4)}</td>
        <td>${source}</td>
        <td>${stateBadge} ${override}</td>
        <td class="acts"><button class="act" data-act="open-feed" data-feed="${esc(result.feed.name)}">Open</button></td>
      </tr>`;
    })
    .join('');
  return `<table class="list">
    <thead><tr><th>Feed</th><th>Start</th><th>Source</th><th>State</th><th></th></tr></thead>
    <tbody>${rows || '<tr><td colspan="5" class="note">No feed matches this filter.</td></tr>'}</tbody>
  </table>`;
}

function candidateTable(result) {
  const feed = result.feed;
  const explicit = feed.explicit !== null;
  const rows = result.candidates
    .map((candidate, index) => {
      const chosen = result.candidate && result.candidate.id === candidate.id;
      const inScope = !explicit || feed.explicit.includes(candidate.id);
      const stateBadge = chosen
        ? badge('ok', 'starts here')
        : !inScope
          ? badge('off', 'not in the list')
          : !candidate.usable
            ? badge(candidate.providerInfo.terms !== 'accepted' ? 'bad' : 'warn', candidate.providerInfo.terms !== 'accepted' ? 'terms not accepted' : 'after the clock')
            : badge('off', 'not reached');
      const direction = candidate.direct ? 'direct' : candidate.inverse ? '1/x' : '—';
      return `<tr class="${chosen ? 'on' : 'quiet'}">
        ${explicit ? `<td><input type="checkbox" data-act="include" data-id="${candidate.id}" ${inScope ? 'checked' : ''} aria-label="Include ${esc(candidate.id)}"></td>` : '<td class="num">' + (index + 1) + '</td>'}
        <td><b>${esc(candidate.providerInfo.name)}</b><span class="uri wrap mono">${esc(uriOf(candidate))}</span><div class="why">${esc(chosen ? 'chosen first' : !explicit || feed.explicit.includes(candidate.id) ? skipReason(candidate) : 'excluded by the feed\'s explicit list')}</div></td>
        <td>${direction}</td>
        <td class="num">${fmt(candidate.value, candidate.value.includes('.') ? 4 : 2)}</td>
        <td class="num">${esc(candidate.date)}</td>
        <td>${stateBadge}</td>
      </tr>`;
    })
    .join('');
  return `<table class="list">
    <thead><tr>${explicit ? '<th>In</th>' : '<th>#</th>'}<th>${explicit ? 'Candidate' : 'Derived candidate'}</th><th>Read as</th><th>Fixing</th><th>Date</th><th>State</th></tr></thead>
    <tbody>${rows}</tbody>
  </table>
  ${explicit ? '<p class="note" style="margin-top:8px">This feed names its items. The resolver keeps this order and reads only the checked ones. Clear the list to return to the derived candidates.</p>' : ''}`;
}

function feedControls(result) {
  const feed = result.feed;
  const automatic = state.policyMode === 'automatic';
  const explicit = feed.explicit !== null;
  const overridden = Boolean(feed.overrideItem || feed.overrideWindow || explicit);
  const options = [`<option value="">Pin an item (optional)</option>`]
    .concat(candidatesFor(feed).map((candidate) => `<option value="${candidate.id}" ${feed.overrideItem === candidate.id ? 'selected' : ''}>${esc(candidate.provider)} · ${esc(candidate.pair)}</option>`))
    .join('');
  const windows = [['', 'Policy window'], ['30', '30 days'], ['60', '60 days'], ['90', '90 days']]
    .map(([value, text]) => `<option value="${value}" ${String(feed.overrideWindow ?? '') === value ? 'selected' : ''}>${text}</option>`)
    .join('');
  const kind = overridden ? 'warn' : 'off';
  const label = overridden ? 'override' : automatic ? 'automatic' : 'inherits';
  const text = overridden
    ? explicit
      ? 'This feed names its items explicitly.'
      : 'This feed overrides the class calibration.'
    : automatic
      ? 'Calibrated automatically from the catalogue. Nothing was configured for this feed.'
      : `Inherits the class policy: ${state.policy.providerOrder.map((code) => PROVIDERS[code].name).join(' → ')}, window ${state.policy.windowDays} days.`;
  return `<div class="banner ${overridden ? 'warn' : ''}">
    <span class="badge ${kind}">${label}</span>
    <span>${text}</span>
    <span class="inline-actions">
      <select id="pinItem">${options}</select>
      <select id="pinWindow">${windows}</select>
      <button class="act" data-act="toggle-explicit">${explicit ? 'Use the derived list' : 'Choose items explicitly'}</button>
    </span>
  </div>`;
}

/* ---------------------------------------------------------------- screens */

function filterBar(results) {
  const attention = results.filter(needsAttention).length;
  const overrides = results.filter(isOverridden).length;
  return `<span class="filter-bar">
    <button class="act ${state.filter === 'all' ? 'primary' : ''}" data-act="filter" data-filter="all">All ${results.length}</button>
    <button class="act ${state.filter === 'attention' ? 'primary' : ''}" data-act="filter" data-filter="attention">Needs attention ${attention}</button>
    <button class="act ${state.filter === 'overridden' ? 'primary' : ''}" data-act="filter" data-filter="overridden">Overridden ${overrides}</button>
  </span>`;
}

function policyLine() {
  const automatic = state.policyMode === 'automatic';
  const fallback = state.policy.fallback === 'refuse' ? 'refuse without a fixing' : 'keep the fixed level';
  const summary = `${state.policy.providerOrder.map((code) => PROVIDERS[code].name).join(' → ')} · ${state.policy.windowDays} days · ${state.policy.allowInverse ? 'inverse allowed' : 'inverse not allowed'} · ${fallback}`;
  return `<div class="policy-line">
    <span class="badge ${automatic ? 'off' : 'warn'}">${automatic ? 'automatic' : 'custom policy'}</span>
    <span class="mono">${esc(summary)}</span>
    <button class="act" data-act="open-policy">The policy</button>
  </div>`;
}

function renderList(results) {
  const fromFixing = results.filter((result) => result.fromFixing).length;
  const kept = results.filter((result) => !result.fromFixing && !result.blocked).length;
  const blocked = results.filter((result) => result.blocked).length;
  const overrides = results.filter(isOverridden).length;
  const addable = EXTRA_FEEDS.some((candidate) => !state.feeds.some((feed) => feed.pair === candidate.pair));
  const body = card({
    title: 'All FX feeds',
    sub: 'every feed is calibrated; the list shows where it landed',
    aside: `${filterBar(results)}<button class="act" data-act="add-feed" ${addable ? '' : 'disabled'}>Add a feed</button>`,
    body: `<div class="summary-line">
        <span><b>${results.length}</b> feeds</span>
        <span><b>${fromFixing}</b> from a fixing</span>
        <span><b>${kept}</b> kept a level</span>
        <span><b>${blocked}</b> blocked</span>
        <span><b>${overrides}</b> overridden</span>
      </div>
      ${feedTable(results)}
      <p class="note" style="margin-top:10px">Nothing here selects feeds for calibration: every feed resolves, and a new feed enters with its pair only.</p>`,
  });
  return `${policyLine()}${body}`;
}

function feedFacts(result) {
  const overridden = isOverridden(result);
  return `<div class="kv">
    <span class="k">Pair</span><span class="v mono">${esc(result.feed.pair)}</span>
    <span class="k">Feed</span><span class="v mono">${esc(result.feed.name)}</span>
    <span class="k">Process</span><span class="v">${esc(result.feed.process)}</span>
    <span class="k">Ticks</span><span class="v mono">${result.feed.ticks}/hour</span>
    <span class="k">Fixed level</span><span class="v mono">${fmt(Number(result.feed.fixedLevel), 4)}</span>
    <span class="k">Policy</span><span class="v">${overridden ? 'overridden on this feed' : state.policyMode === 'automatic' ? 'defaults' : 'inherited'}</span>
  </div>`;
}

function flowCard(result) {
  const chips = result.candidates
    .map((candidate) => `<div class="chip ${result.candidate && result.candidate.id === candidate.id ? 'chosen' : 'skipped'}">
      <span class="n">${candidate.order + 1}</span><span>${esc(candidate.provider)} · ${esc(candidate.pair)}</span><span class="badge ${candidate.usable ? 'ok' : 'off'}">${candidate.usable ? 'usable' : 'no'}</span>
    </div>`)
    .join('');
  return card({
    title: `How ${result.feed.pair} resolves`,
    sub: 'policy, candidates, resolver, process',
    body: `<div class="diagram">
      <div class="col"><div class="col-title">${state.policyMode === 'automatic' ? 'Automatic defaults' : 'Class policy'}</div>
        <div class="node"><h3>${state.policyMode === 'automatic' ? 'Defaults' : 'Policy'}</h3><div class="sub">${state.policy.providerOrder.join(' → ')}</div><div class="sub">window ${state.policy.windowDays} days</div><div class="sub">${state.policy.allowInverse ? 'inverse allowed' : 'inverse not allowed'} · ${state.policy.fallback === 'refuse' ? 'refuse without a fixing' : 'keep the fixed level'}</div></div>
      </div>
      <div class="arrow"></div>
      <div class="col"><div class="col-title">Candidates for ${esc(result.feed.pair)}</div>${chips || '<p class="note">No candidate: the pair has no accepted fixing.</p>'}</div>
      <div class="arrow ${result.fromFixing ? '' : 'off'}"></div>
      <div class="col"><div class="node feed"><h3>Resolver</h3><div class="big">${result.level === null ? '—' : fmt(result.level, 4)}</div><div class="sub">${result.fromFixing ? `${esc(result.candidate.provider)} · ${esc(result.candidate.date)}${result.candidate.inverse ? ' · 1/x' : ''}` : 'no fixing'}</div><div class="sub">${result.volatility ? `×${result.volatility.scale.toFixed(2)}` : 'not measured'}</div></div></div>
    </div>
    <p class="note" style="margin-top:10px">One policy or the defaults, one candidate list per pair, one recorded start per feed. No feed carries a handwritten list.</p>`,
  });
}

function recalcTable() {
  const rows = state.recalc
    .map((run) => `<tr><td class="num">${esc(run.at)}</td><td>${esc(run.scope)}</td><td>${esc(run.resolved)}</td><td>${esc(run.fallback)}</td><td class="note">${esc(run.actor)}</td></tr>`)
    .join('');
  return `<table class="list"><thead><tr><th>When</th><th>Scope</th><th>Resolved</th><th>Fallback</th><th>By</th></tr></thead><tbody>${rows}</tbody></table>`;
}

function renderFeed(results) {
  const result = results.find((entry) => entry.feed.name === state.feedName) ?? results[0];
  const tab = state.feedTab;
  const record = `
    ${feedControls(result)}
    <div class="grid-a">
      <div class="stack">
        ${startCard(result)}
        ${card({
          title: 'Derived candidates',
          sub: state.policy.providerOrder.join(' → ') + ` · window ${windowOf(result.feed)} days`,
          body: `${candidateTable(result)}<p class="note" style="margin-top:10px">The list is derived from the pair and the order. Nothing here is authored per feed unless it is overridden.</p>`,
        })}
      </div>
      <div class="stack">
        ${card({ title: 'Resolution trail', body: trailList(result) })}
        ${measuresCard(result)}
        ${card({ title: 'This feed', body: feedFacts(result) })}
      </div>
    </div>`;
  const flow = `
    <div class="grid-a">
      <div class="stack">${flowCard(result)}${card({ title: 'Resolution trail', body: trailList(result) })}</div>
      <div class="stack">
        ${card({ title: 'What it inherits', body: `<p class="note">${state.policyMode === 'automatic' ? 'The automatic defaults:' : 'The FX class policy:'}</p><p class="mono" style="margin-top:6px">${esc(state.policy.providerOrder.join(' → '))} · ${state.policy.windowDays} days</p><p style="margin-top:10px"><button class="act" data-act="open-policy">Open the class policy</button></p>` })}
        ${card({ title: 'Recent recalculations', body: recalcTable() })}
      </div>
    </div>`;
  return `
  <p style="margin-bottom:8px"><button class="act" data-act="back-to-list">← All FX feeds</button></p>
  <div class="record-head">
    <h2>${esc(result.feed.pair)}</h2>
    <span class="note mono">${esc(result.feed.name)}</span>
    <span class="badge off">FX</span>
  </div>
  <nav class="tabs" role="tablist" aria-label="Feed detail">
    <button role="tab" data-act="feed-tab" data-tab="record" aria-selected="${tab === 'record'}">Feed record</button>
    <button role="tab" data-act="feed-tab" data-tab="flow" aria-selected="${tab === 'flow'}">Flow</button>
  </nav>
  ${tab === 'flow' ? flow : record}`;
}

function renderPolicy(results) {
  return `
  <p style="margin-bottom:8px"><button class="act" data-act="back-to-list">← All FX feeds</button></p>
  <div class="record-head"><h2>FX calibration policy</h2><span class="badge ${state.policyMode === 'automatic' ? 'off' : 'warn'}">${state.policyMode === 'automatic' ? 'automatic' : 'custom'}</span></div>
  <div class="banner" style="margin:10px 0 14px">
    <span class="badge off">class-wide</span>
    <span>This policy applies to every FX feed. A change here changes all of them; a feed's own exceptions stay on its record.</span>
  </div>
  <div class="grid-a">
    <div class="stack">${policyCard()}</div>
    <div class="stack">${metrics(results)}</div>
  </div>
  ${card({ title: 'Coverage under this policy', sub: 'every feed of the class', body: feedTable(results) })}
  ${card({ title: 'Recent recalculations', sub: 'every recalculation is class-wide', body: recalcTable() })}`;
}

/* ---------------------------------------------------------------- actions */

function setScreen(screen) {
  state.screen = screen;
  const url = new URL(window.location.href);
  url.searchParams.set('screen', screen);
  window.history.replaceState({}, '', url);
  render();
}

function setScope(feedName) {
  state.feedName = feedName;
  state.feedTab = 'record';
  setScreen('feed');
}

function reorderProvider(code, delta) {
  const order = state.policy.providerOrder;
  const index = order.indexOf(code);
  const target = index + delta;
  if (index < 0 || target < 0 || target >= order.length) return;
  [order[index], order[target]] = [order[target], order[index]];
  render();
}

function act(kind, target) {
  if (kind === 'order-up') reorderProvider(target, -1);
  if (kind === 'order-down') reorderProvider(target, 1);
  if (kind === 'open-feed') setScope(target);
  if (kind === 'back-to-list') {
    setScreen('list');
    return;
  }
  if (kind === 'open-policy') {
    setScreen('policy');
    return;
  }
  if (kind === 'feed-tab') {
    state.feedTab = target;
    const url = new URL(window.location.href);
    url.searchParams.set('screen', 'feed');
    url.searchParams.set('tab', target);
    window.history.replaceState({}, '', url);
    render();
    return;
  }
  if (kind === 'filter') {
    state.filter = target;
    render();
    return;
  }
  if (kind === 'add-feed') {
    const next = EXTRA_FEEDS.find((candidate) => !state.feeds.some((feed) => feed.pair === candidate.pair));
    if (next) {
      state.feeds.push({ ...next, overrideItem: null, overrideWindow: null, explicit: null });
      state.feedName = next.name;
    }
    render();
  }
  if (kind === 'recalculate') {
    const results = resolveAll();
    const fromFixing = results.filter((result) => result.fromFixing).length;
    const kept = results.filter((result) => !result.fromFixing && !result.blocked).length;
    const blocked = results.filter((result) => result.blocked).length;
    state.recalc.unshift({
      at: '2026-10-30 12:00',
      scope: 'all FX feeds',
      resolved: `${fromFixing} from a fixing`,
      fallback: `${kept} kept a level${blocked ? `, ${blocked} blocked` : ''}`,
      actor: 'you',
    });
    render();
  }
  if (kind === 'toggle-explicit') {
    const feed = feedOf(state.feedName);
    feed.explicit = feed.explicit === null ? candidatesFor(feed).map((candidate) => candidate.id) : null;
    render();
  }
  if (kind === 'customise') {
    state.policyMode = 'custom';
    render();
  }
  if (kind === 'reset-policy') {
    state.policyMode = 'automatic';
    state.policy = { ...DEFAULT_POLICY, providerOrder: [...DEFAULT_POLICY.providerOrder] };
    state.feeds = state.feeds.map((feed) => ({ ...feed, overrideItem: null, overrideWindow: null, explicit: null }));
    render();
  }
}

/* ---------------------------------------------------------------- render */

function render() {
  const results = resolveAll();
  document.getElementById('screen-list').innerHTML = renderList(results);
  document.getElementById('screen-feed').innerHTML = renderFeed(results);
  document.getElementById('screen-policy').innerHTML = renderPolicy(results);
  document.querySelectorAll('.screen').forEach((node, index) => {
    node.classList.toggle('active', ['list', 'feed', 'policy'][index] === state.screen);
  });
  document.querySelectorAll('.proto-switch button').forEach((button) => {
    button.setAttribute('aria-selected', String(button.dataset.screen === state.screen));
  });
  document.getElementById('clock').value = state.clock;
  const feedOptions = state.feeds
    .map((feed) => `<option value="${feed.name}" ${feed.name === state.feedName ? 'selected' : ''}>${feed.pair} · ${feed.name}</option>`)
    .join('');
  document.getElementById('feed').innerHTML = feedOptions;
  document.getElementById('feedPick').style.display = state.screen === 'feed' ? '' : 'none';
  document.getElementById('feedCount').textContent = `${state.feeds.length} feeds`;
  document.getElementById('controlNote').innerHTML =
    state.policy.fallback === 'refuse'
      ? 'The policy refuses to start a feed with no usable fixing.'
      : 'The policy keeps a fixed level when no candidate has a fixing.';
  document.getElementById('stateBody').textContent = JSON.stringify(
    {
      screen: state.screen,
      policyMode: state.policyMode,
      clock: state.clock,
      policy: state.policy,
      recalculationCount: state.recalc.length,
      filter: state.filter,
      feedTab: state.feedTab,
      feeds: state.feeds.map((feed) => {
        const result = results.find((entry) => entry.feed.name === feed.name);
        return {
          name: feed.name,
          pair: feed.pair,
          override: { item: feed.overrideItem, window: feed.overrideWindow, explicit: feed.explicit },
          start: {
            fromFixing: result.fromFixing,
            blocked: result.blocked,
            level: result.level === null ? null : Number(result.level.toFixed(6)),
            item: result.candidate ? result.candidate.id : null,
            inverted: Boolean(result.candidate && result.candidate.inverse),
            windowDays: windowOf(feed),
          },
        };
      }),
    },
    null,
    2,
  );
  document.getElementById('state').open = false;
}

function onChange(event) {
  const target = event.target;
  if (target.id === 'clock') state.clock = target.value;
  if (target.id === 'feed') state.feedName = target.value;
  if (target.id === 'policyWindow') state.policy.windowDays = Math.max(2, Math.min(180, Number(target.value) || 2));
  if (target.id === 'policyInverse') state.policy.allowInverse = target.checked;
  if (target.id === 'policyFallback') state.policy.fallback = target.value;
  if (target.id === 'pinItem') feedOf(state.feedName).overrideItem = target.value || null;
  if (target.id === 'pinWindow') feedOf(state.feedName).overrideWindow = target.value === '' ? null : Number(target.value);
  render();
}

function onClick(event) {
  const button = event.target.closest('[data-act]');
  if (!button) return;
  const kind = button.dataset.act;
  const target = button.dataset.provider ?? button.dataset.feed ?? button.dataset.filter ?? button.dataset.tab;
  if (kind === 'include') {
    const feed = feedOf(state.feedName);
    if (feed.explicit !== null) {
      const id = button.dataset.id;
      feed.explicit = button.checked
        ? [...new Set([...feed.explicit, id])]
        : feed.explicit.filter((entry) => entry !== id);
      render();
    }
    return;
  }
  if (['order-up', 'order-down', 'open-feed', 'customise', 'reset-policy', 'add-feed', 'recalculate', 'toggle-explicit', 'back-to-list', 'filter', 'feed-tab', 'open-policy'].includes(kind)) {
    act(kind, target);
  }
}

document.addEventListener('DOMContentLoaded', () => {
  const params = new URLSearchParams(window.location.search);
  const screen = params.get('screen');
  if (['list', 'feed', 'policy'].includes(screen)) state.screen = screen;
  const tab = params.get('tab');
  if (['record', 'flow'].includes(tab)) state.feedTab = tab;
  document.body.addEventListener('click', onClick);
  document.body.addEventListener('change', onChange);
  document.getElementById('resolve').addEventListener('click', () => act('recalculate'));
  document.querySelectorAll('.proto-switch button').forEach((button) => {
    button.addEventListener('click', () => setScreen(button.dataset.screen));
  });
  render();
});
