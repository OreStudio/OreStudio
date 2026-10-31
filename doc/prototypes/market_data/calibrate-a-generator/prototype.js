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
  variant: 'a',
  policyMode: 'automatic',
  clock: '2026-10-31',
  policy: { ...DEFAULT_POLICY, providerOrder: [...DEFAULT_POLICY.providerOrder] },
  feeds: FEEDS.map((feed) => ({ ...feed, overrideItem: null, overrideWindow: null })),
  feedName: 'synthetic-eur-usd',
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
    .sort((a, b) => (a.order !== b.order ? a.order - b.order : a.direct === b.direct ? 0 : a.direct ? -1 : 1));
}

function skipReason(candidate) {
  if (candidate.providerInfo.terms !== 'accepted') return `terms not accepted for ${candidate.providerInfo.name}`;
  if (candidate.date > state.clock) return `its fixing is after the clock (${candidate.date})`;
  return 'a higher-ranked candidate was chosen first';
}

/** Resolve one feed: the pinned item first, then the policy order. */
function resolveFeed(feed) {
  const candidates = candidatesFor(feed);
  const ordered = [...candidates];
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

function feedTable(results) {
  const rows = results
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
    <tbody>${rows}</tbody>
  </table>`;
}

function candidateTable(result) {
  const rows = result.candidates
    .map((candidate, index) => {
      const chosen = result.candidate && result.candidate.id === candidate.id;
      const stateBadge = chosen
        ? badge('ok', 'starts here')
        : !candidate.usable
          ? badge(candidate.providerInfo.terms !== 'accepted' ? 'bad' : 'warn', candidate.providerInfo.terms !== 'accepted' ? 'terms not accepted' : 'after the clock')
          : badge('off', 'not reached');
      const direction = candidate.direct ? 'direct' : candidate.inverse ? '1/x' : '—';
      return `<tr class="${chosen ? 'on' : 'quiet'}">
        <td class="num">${index + 1}</td>
        <td><b>${esc(candidate.providerInfo.name)}</b><span class="uri wrap mono">${esc(uriOf(candidate))}</span><div class="why">${esc(chosen ? 'chosen first' : skipReason(candidate))}</div></td>
        <td>${direction}</td>
        <td class="num">${fmt(candidate.value, candidate.value.includes('.') ? 4 : 2)}</td>
        <td class="num">${esc(candidate.date)}</td>
        <td>${stateBadge}</td>
      </tr>`;
    })
    .join('');
  return `<table class="list">
    <thead><tr><th>#</th><th>Derived candidate</th><th>Read as</th><th>Fixing</th><th>Date</th><th>State</th></tr></thead>
    <tbody>${rows}</tbody>
  </table>`;
}

function feedControls(result) {
  const feed = result.feed;
  const automatic = state.policyMode === 'automatic';
  const overridden = Boolean(feed.overrideItem || feed.overrideWindow);
  const options = [`<option value="">Pin an item (optional)</option>`]
    .concat(candidatesFor(feed).map((candidate) => `<option value="${candidate.id}" ${feed.overrideItem === candidate.id ? 'selected' : ''}>${esc(candidate.provider)} · ${esc(candidate.pair)}</option>`))
    .join('');
  const windows = [['', 'Policy window'], ['30', '30 days'], ['60', '60 days'], ['90', '90 days']]
    .map(([value, text]) => `<option value="${value}" ${String(feed.overrideWindow ?? '') === value ? 'selected' : ''}>${text}</option>`)
    .join('');
  const kind = overridden ? 'warn' : 'off';
  const label = overridden ? 'override' : automatic ? 'automatic' : 'inherits';
  const text = overridden
    ? 'This feed overrides the class calibration.'
    : automatic
      ? 'Calibrated automatically from the catalogue. Nothing was configured for this feed.'
      : `Inherits the class policy: ${state.policy.providerOrder.map((code) => PROVIDERS[code].name).join(' → ')}, window ${state.policy.windowDays} days.`;
  return `<div class="banner ${overridden ? 'warn' : ''}">
    <span class="badge ${kind}">${label}</span>
    <span>${text}</span>
    <span class="inline-actions">
      <select id="pinItem">${options}</select>
      <select id="pinWindow">${windows}</select>
    </span>
  </div>`;
}

/* ---------------------------------------------------------------- variant A */

function renderA(results) {
  const sample = results.find((result) => result.feed.name === state.feedName) ?? results[0];
  const body = card({ title: 'Feed coverage', sub: 'resolved from the policy, or the defaults, at the market clock', body: `${metrics(results)}${feedTable(results)}` });
  return `
  <p class="thesis"><b>A · Policy first.</b> A person may set the market's calibration once; with nothing set, the defaults calibrate every feed.</p>
  <div class="grid-a">
    <div class="stack">${policyCard()}${body}</div>
    <div class="stack">
      ${card({ title: `How the ${state.policyMode === 'automatic' ? 'defaults' : 'policy'} resolve ${sample.feed.pair}`, sub: sample.fromFixing ? 'one example, at the clock' : 'no candidate applies', body: trailList(sample) })}
      ${card({ title: 'Fixed level', body: `<p class="start-value" style="font-size:1.25rem">${fmt(Number(sample.feed.fixedLevel), 4)}</p><p class="note">The fixed level is per feed and used only when no candidate has a fixing. The policy, or the defaults, say whether to keep it or refuse.</p><p style="margin-top:8px"><button class="act" data-act="open-feed" data-feed="${esc(sample.feed.name)}">Open ${esc(sample.feed.pair)}</button></p>` })}
    </div>
  </div>`;
}

/* ---------------------------------------------------------------- variant B */

function renderB(results) {
  const result = results.find((entry) => entry.feed.name === state.feedName) ?? results[0];
  const overridden = Boolean(result.feed.overrideItem || result.feed.overrideWindow);
  return `
  <p class="thesis"><b>B · Feed inherits.</b> The feed page shows what the defaults or the policy derived; an exception is optional.</p>
  <div class="grid-a">
    <div class="stack">
      ${feedControls(result)}
      ${startCard(result)}
      ${card({
        title: 'Derived candidates',
        sub: state.policy.providerOrder.join(' → ') + ` · window ${windowOf(result.feed)} days`,
        body: `${candidateTable(result)}<p class="note" style="margin-top:10px">The list is derived from the pair and the provider order. Nothing here is authored per feed unless it is overridden.</p>`,
      })}
    </div>
    <div class="stack">
      ${card({ title: 'Resolution trail', body: trailList(result) })}
      ${card({
        title: 'This feed',
        body: `<div class="kv">
          <span class="k">Pair</span><span class="v mono">${esc(result.feed.pair)}</span>
          <span class="k">Feed</span><span class="v mono">${esc(result.feed.name)}</span>
          <span class="k">Process</span><span class="v">${esc(result.feed.process)}</span>
          <span class="k">Ticks</span><span class="v mono">${result.feed.ticks}/hour</span>
          <span class="k">Fixed level</span><span class="v mono">${fmt(Number(result.feed.fixedLevel), 4)}</span>
          <span class="k">Policy</span><span class="v">${overridden ? 'overridden on this feed' : state.policyMode === 'automatic' ? 'defaults' : 'inherited'}</span>
        </div>`,
      })}
    </div>
  </div>`;
}

/* ---------------------------------------------------------------- variant C */

function renderC(results) {
  const result = results.find((entry) => entry.feed.name === state.feedName) ?? results[0];
  const chips = result.candidates
    .map((candidate) => `<div class="chip ${result.candidate && result.candidate.id === candidate.id ? 'chosen' : 'skipped'}">
      <span class="n">${candidate.order + 1}</span><span>${esc(candidate.provider)} · ${esc(candidate.pair)}</span><span class="badge ${candidate.usable ? 'ok' : 'off'}">${candidate.usable ? 'usable' : 'no'}</span>
    </div>`)
    .join('');
  const history = RECALCULATIONS.map(
    (run) => `<tr><td class="num">${esc(run.at)}</td><td>${esc(run.scope)}</td><td>${esc(run.resolved)}</td><td>${esc(run.fallback)}</td><td class="note">${esc(run.actor)}</td></tr>`,
  ).join('');
  return `
  <p class="thesis"><b>C · Flow.</b> The policy derives candidates, the resolver takes one, and every feed is recorded against the run.</p>
  ${metrics(results)}
  <div class="flow">
    <div class="stack">
      ${card({
        title: 'From the policy to a feed',
        sub: 'one feed shown, every feed follows the same path',
        body: `<div class="diagram">
          <div class="col"><div class="col-title">FX policy</div>
            <div class="node"><h3>${state.policyMode === 'automatic' ? 'Automatic' : 'Custom policy'}</h3><div class="sub">${state.policy.providerOrder.join(' → ')}</div><div class="sub">window ${state.policy.windowDays} days</div><div class="sub">${state.policy.allowInverse ? 'inverse allowed' : 'inverse not allowed'} · ${state.policy.fallback === 'refuse' ? 'refuse without a fixing' : 'keep the fixed level'}</div></div>
          </div>
          <div class="arrow"></div>
          <div class="col"><div class="col-title">Candidates for ${esc(result.feed.pair)}</div>${chips || '<p class="note">No candidate: the pair has no accepted fixing.</p>'}</div>
          <div class="arrow ${result.fromFixing ? '' : 'off'}"></div>
          <div class="col"><div class="node feed"><h3>Resolver</h3><div class="big">${result.level === null ? '—' : fmt(result.level, 4)}</div><div class="sub">${result.fromFixing ? `${esc(result.candidate.provider)} · ${esc(result.candidate.date)}${result.candidate.inverse ? ' · 1/x' : ''}` : 'no fixing'}</div><div class="sub">${result.volatility ? `×${result.volatility.scale.toFixed(2)}` : 'not measured'}</div></div></div>
        </div>
        <p class="note" style="margin-top:10px">One policy, one candidate list per pair, one recorded start per feed. No feed carries a handwritten list.</p>`,
      })}
      ${card({ title: 'Recalculations', sub: 'the policy applied to every feed', body: `<table class="list"><thead><tr><th>When</th><th>Scope</th><th>Resolved</th><th>Fallback</th><th>By</th></tr></thead><tbody>${history}</tbody></table>` })}
    </div>
    <div class="stack">
      ${card({ title: 'This feed', body: `<div class="kv"><span class="k">Pair</span><span class="v mono">${esc(result.feed.pair)}</span><span class="k">Feed</span><span class="v mono">${esc(result.feed.name)}</span><span class="k">Exception</span><span class="v">${result.feed.overrideItem || result.feed.overrideWindow ? 'overridden' : 'none'}</span></div><p style="margin-top:10px"><button class="act" data-act="open-feed" data-feed="${esc(result.feed.name)}">Open ${esc(result.feed.pair)}</button></p>` })}
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

function setScope(feedName) {
  state.feedName = feedName;
  setVariant('b');
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
  if (kind === 'customise') {
    state.policyMode = 'custom';
    render();
  }
  if (kind === 'reset-policy') {
    state.policyMode = 'automatic';
    state.policy = { ...DEFAULT_POLICY, providerOrder: [...DEFAULT_POLICY.providerOrder] };
    state.feeds = state.feeds.map((feed) => ({ ...feed, overrideItem: null, overrideWindow: null }));
    render();
  }
}

/* ---------------------------------------------------------------- render */

function render() {
  const results = resolveAll();
  document.getElementById('variant-a').innerHTML = renderA(results);
  document.getElementById('variant-b').innerHTML = renderB(results);
  document.getElementById('variant-c').innerHTML = renderC(results);
  document.querySelectorAll('.variant').forEach((node, index) => {
    node.classList.toggle('active', ['a', 'b', 'c'][index] === state.variant);
  });
  document.querySelectorAll('.proto-switch button').forEach((button) => {
    button.setAttribute('aria-selected', String(button.dataset.variant === state.variant));
  });
  document.getElementById('clock').value = state.clock;
  const feedOptions = state.feeds
    .map((feed) => `<option value="${feed.name}" ${feed.name === state.feedName ? 'selected' : ''}>${feed.pair} · ${feed.name}</option>`)
    .join('');
  document.getElementById('feed').innerHTML = feedOptions;
  document.getElementById('feedCount').textContent = `${state.feeds.length} feeds`;
  document.getElementById('controlNote').innerHTML =
    state.policy.fallback === 'refuse'
      ? 'The policy refuses to start a feed with no usable fixing.'
      : 'The policy keeps a fixed level when no candidate has a fixing.';
  document.getElementById('stateBody').textContent = JSON.stringify(
    {
      variant: state.variant,
      policyMode: state.policyMode,
      clock: state.clock,
      policy: state.policy,
      feeds: state.feeds.map((feed) => {
        const result = results.find((entry) => entry.feed.name === feed.name);
        return {
          name: feed.name,
          pair: feed.pair,
          override: { item: feed.overrideItem, window: feed.overrideWindow },
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
  const target = button.dataset.provider ?? button.dataset.feed;
  if (kind === 'order-up' || kind === 'order-down' || kind === 'open-feed' || kind === 'customise' || kind === 'reset-policy') {
    act(kind, target);
  }
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
