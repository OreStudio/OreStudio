'use strict';

/* A small model of the service's market, run in the page with a fixed seed: the
   calm/active chain, the shared heat, tau, and each feed's law in market time.
   It is mock data shaped like the service's own reads, not the service. */

const $ = (s, el = document) => el.querySelector(s);
const $$ = (s, el = document) => [...el.querySelectorAll(s)];

function mulberry32(a) {
  return () => {
    a |= 0; a = (a + 0x6d2b79f5) | 0;
    let t = Math.imul(a ^ (a >>> 15), 1 | a);
    t = (t + Math.imul(t ^ (t >>> 7), 61 | t)) ^ t;
    return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
  };
}
const rand = mulberry32(42);

/* Numbers print as a person would write them: six significant figures at
   most, no trailing float noise. 0.0016666666666666668 is 0.00166667. */
function clean(x, sig = 6) {
  if (!Number.isFinite(x)) return '—';
  return String(Number(x.toPrecision(sig)));
}
function seconds(s) {
  if (!Number.isFinite(s)) return '—';
  if (s >= 3600) return `${clean(s / 3600, 3)} h`;
  if (s >= 60) return `${clean(s / 60, 3)} min`;
  if (s >= 1) return `${clean(s, 3)} s`;
  return `${clean(s * 1000, 3)} ms`;
}
function clock(t) {
  const d = new Date(Date.UTC(2026, 9, 28, 9, 30, 0) + t * 1000);
  return d.toISOString().slice(11, 19);
}

// --- The market -------------------------------------------------------------

const cfg = {
  wake: 1 / 600,
  calm: 1 / 300,
  activeMult: 2,
  jump: 0.5,
  decay: 20,
  speed: 1440,
  tail: 0,
};
const calmMult = (c) => {
  const p = c.wake / (c.wake + c.calm);
  return (1 - p * c.activeMult) / (1 - p);
};

const FEEDS = [
  ['synthetic.bloomberg.eurusd', 'fx', 'hawkes', 2.0, 1.2],
  ['synthetic.bloomberg.gbpusd', 'fx', 'hawkes', 1.5, 1.0],
  ['synthetic.bloomberg.usdjpy', 'fx', 'poisson', 1.6, 1.1],
  ['synthetic.lseg.audusd', 'fx', 'poisson', 1.0, 0.9],
  ['synthetic.lseg.usdchf', 'fx', 'periodic', 1.0, 1.0],
  ['synthetic.lseg.usdcad', 'fx', 'poisson', 0.8, 1.0],
  ['synthetic.lseg.eurgbp', 'fx', 'hawkes', 0.9, 1.3],
  ['synthetic.demo.usdinr', 'fx', 'periodic', 0.5, 1.0],
  ['synthetic.demo.usdsek', 'fx', 'poisson', 0.5, 0.8],
  ['synthetic.lseg.chfjpy', 'fx', 'periodic', 0.5, 1.0],
  ['synthetic.demo.usdsofr', 'ir', 'periodic', 1 / 20, 1.0],
  ['synthetic.demo.eurestr', 'ir', 'poisson', 1 / 15, 1.0],
  ['synthetic.demo.gbpsonia', 'ir', 'periodic', 1 / 30, 1.0],
  ['synthetic.demo.jpytona', 'ir', 'poisson', 1 / 25, 0.7],
  ['synthetic.demo.chfsaron', 'ir', 'periodic', 1 / 60, 1.0],
  ['synthetic.demo.twdtaibor', 'ir', 'poisson', 1 / 40, 1.0],
].map(([name, cls, mode, rate, sens], i) => ({
  i, name, cls, mode, rate, sens,
  // A Hawkes feed's branching ratio and decay, as its own row sets them.
  n: 0.6, beta: 2,
  excess: 0, acc: 0, mark: 0, lastT: null, lastTau: 0,
}));

const sim = {
  t: -3600, tau: 0, state: 'calm', pinned: null, heat: 0, simulated: 0,
  // The scripted history turns active 95 s before the page opens and calm 40 s
  // later, so the two-minute zoom opens on a turn; before and after, the chain draws.
  script: [{ t: -95, state: 'active' }, { t: -40, state: 'calm' }], nextTurn: -3410, enteredAt: -3600, lastBy: 'the chain', lastChange: null,
  events: [], paceSamples: [], tpsSamples: [], recentTicks: 0,
  // One bin a second: each feed's ticks, the mean pace and whether it was active,
  // which the folded hour is drawn from.
  bins: [], bin: null,
  late: 0.6, lateMax: 3.1,
};
const R = FEEDS.reduce((s, f) => s + f.rate, 0);
const mult = () => ((sim.pinned || sim.state) === 'active' ? cfg.activeMult : calmMult(cfg));
const pace = () => mult() + sim.heat / R;

function record(kind, feed = null, extra = {}) {
  sim.events.push({
    kind, feed, t: sim.t, tau: sim.tau, state: sim.pinned || sim.state,
    pace: pace(), heat: sim.heat, ...extra,
  });
  if (sim.events.length > 65536) sim.events.splice(0, sim.events.length - 65536);
}

/* The market's intensity λ(t): the ticks a second every feed's law asks for now,
   its rate times its sensitivity times the pace, with a Hawkes feed's excitation
   on top. The same units as the ticks a second seen, so the two share an axis. */
function lawRate(f) {
  if (f.mode !== 'hawkes') return f.rate;
  return f.rate * (1 - f.n) + f.excess;
}
function intensity() {
  return FEEDS.reduce((sum, f) => sum + lawRate(f) * f.sens, 0) * pace();
}

function drawGap(f) {
  return f.mode === 'periodic' ? 1 / f.rate : -Math.log(1 - rand()) / f.rate;
}
FEEDS.forEach((f) => {
  f.mark = f.mode === 'periodic' ? rand() / f.rate : 0;
  record('add', f);
});

function step(dt) {
  sim.t += dt;
  const sec = Math.floor(sim.t);
  if (!sim.bin || sim.bin.sec !== sec) {
    if (sim.bin) sim.bins.push(sim.bin);
    sim.bin = { sec, counts: new Array(FEEDS.length).fill(0), pace: 0, n: 0, active: 0 };
  }
  sim.bin.pace += pace();
  sim.bin.n += 1;
  if ((sim.pinned || sim.state) === 'active') sim.bin.active += 1;
  const dTau = pace() * dt;
  sim.tau += dTau;
  sim.simulated += cfg.speed * dTau;
  sim.heat *= Math.exp(-cfg.decay * dt);

  const forced = sim.script.length && sim.t >= sim.script[0].t ? sim.script.shift().state : null;
  if (!sim.pinned && (forced ? forced !== sim.state : sim.t >= sim.nextTurn)) {
    sim.state = sim.state === 'calm' ? 'active' : 'calm';
    sim.enteredAt = sim.t;
    const r = sim.state === 'calm' ? cfg.wake : cfg.calm;
    sim.nextTurn = sim.t + -Math.log(1 - rand()) / r;
    record('transition');
  }

  for (const f of FEEDS) {
    const dLaw = f.sens * dTau;
    let fired = false;
    if (f.mode === 'periodic') {
      f.mark -= dLaw;
      if (f.mark <= 0) { fired = true; f.mark += 1 / f.rate; }
      f.acc = 1;
    } else {
      const mu = f.mode === 'hawkes' ? f.rate * (1 - f.n) : f.rate;
      const lam = mu + f.excess;
      f.acc += lam * dLaw;
      fired = rand() < lam * dLaw;
      f.excess *= Math.exp(-f.beta * dLaw);
      if (fired && f.mode === 'hawkes') f.excess += f.n * f.beta;
    }
    if (fired) {
      sim.heat += cfg.jump;
      sim.recentTicks += 1;
      sim.bin.counts[f.i] += 1;
      const wall = f.lastT == null ? null : sim.t - f.lastT;
      record('tick', f, { gap: sim.tau - f.lastTau, wall, comp: f.acc });
      f.lastT = sim.t; f.lastTau = sim.tau; f.acc = 0;
    }
  }
}

let sampleClock = 0;
let tpsClock = 0;
function advance(wall) {
  const dt = 0.02;
  for (let k = 0; k < Math.round(wall / dt); k++) {
    step(dt);
    sampleClock += dt; tpsClock += dt;
    if (sampleClock >= 0.25) {
      sampleClock = 0;
      sim.paceSamples.push({ t: sim.t, tau: sim.tau, mult: mult(), heat: sim.heat / R, state: sim.pinned || sim.state, lam: intensity() });
    }
    if (tpsClock >= 1) {
      sim.tpsSamples.push({ t: sim.t, v: sim.recentTicks / tpsClock });
      sim.recentTicks = 0; tpsClock = 0;
      sim.late = 0.4 + rand() * 0.5;
      sim.lateMax = Math.max(sim.lateMax * 0.98, sim.late + rand() * 2.5);
    }
  }
  const keep = sim.t - 400;
  sim.paceSamples = sim.paceSamples.filter((s) => s.t >= keep);
  sim.tpsSamples = sim.tpsSamples.filter((s) => s.t >= keep);
  if (sim.bins.length > 3700) sim.bins.splice(0, sim.bins.length - 3700);
}
advance(3600);

// --- The page's own state ----------------------------------------------------

const ui = {
  tab: 'now', persona: 'system', win: 120, axis: 'clock',
  classes: new Set(['fx', 'ir']), frozen: null, picked: FEEDS[0].name,
  kinds: new Set(['transition', 'pin', 'unpin', 'configure', 'add', 'remove']),
  draft: {},
  foldSeries: 'all', foldFeed: '', foldSpan: 60, foldBins: [], foldValues: [],
};
const mayChange = () => ui.persona === 'system';

function toast(text) {
  $('#toast-text').textContent = text;
  $('#toast').classList.add('show');
  clearTimeout(toast.timer);
  toast.timer = setTimeout(() => $('#toast').classList.remove('show'), 2600);
}

// --- Strip -------------------------------------------------------------------

function renderStrip() {
  const s = sim.pinned || sim.state;
  const b = $('#m-state');
  b.textContent = s; b.className = `badge ${s}`;
  $('#m-forced').hidden = !sim.pinned;
  $('#m-mult').textContent = `×${clean(mult(), 3)}`;
  $('#m-pace').textContent = `×${pace().toFixed(2)}`;
  $('#m-gain').textContent = clean(cfg.jump / cfg.decay, 3);
  const tps = sim.tpsSamples.at(-1);
  $('#m-tps').textContent = tps ? tps.v.toFixed(1) : '—';
  $('#m-speed').textContent = clean(cfg.speed);
  $('#m-late').textContent = `${sim.late.toFixed(1)} ms`;
  $('#m-late-max').textContent = `at most ${sim.lateMax.toFixed(1)} ms`;
  $('#live-text').textContent = `running · ${FEEDS.length} feeds`;
}

// --- Now ---------------------------------------------------------------------

function svgEl(svg, html) { svg.innerHTML = html; }

function renderPace() {
  const W = 800, H = 220, pad = 22, span = 120, t1 = sim.t, t0 = t1 - span;
  const xs = (t) => ((t - t0) / span) * W;
  const pts = sim.paceSamples.filter((p) => p.t >= t0);
  const top = Math.max(2.4, ...pts.map((p) => p.mult + p.heat)) * 1.1;
  const ys = (v) => H - pad - (v / top) * (H - pad - 6);
  let bands = '';
  let start = null;
  pts.forEach((p, k) => {
    if (p.state === 'active' && start == null) start = p.t;
    if ((p.state !== 'active' || k === pts.length - 1) && start != null) {
      bands += `<rect x="${xs(start)}" y="0" width="${Math.max(1, xs(p.t) - xs(start))}" height="${H - pad}" fill="rgba(210,153,34,.10)"/>`;
      start = null;
    }
  });
  const reg = pts.map((p) => `${xs(p.t).toFixed(1)},${ys(p.mult).toFixed(1)}`);
  const tot = pts.map((p) => `${xs(p.t).toFixed(1)},${ys(p.mult + p.heat).toFixed(1)}`);
  const base = `${xs(pts.at(-1)?.t ?? t1)},${ys(0)} ${xs(pts[0]?.t ?? t0)},${ys(0)}`;
  let grid = '';
  [0, 1, 2].forEach((v) => {
    grid += `<line x1="0" x2="${W}" y1="${ys(v)}" y2="${ys(v)}" stroke="#21262d"/><text x="4" y="${ys(v) - 3}">×${v}</text>`;
  });
  [-120, -90, -60, -30, 0].forEach((s) => {
    grid += `<text x="${Math.min(W - 26, xs(t1 + s) + 3)}" y="${H - 6}">${s === 0 ? 'now' : `${s}s`}</text>`;
  });
  svgEl($('#fig-pace'), `${bands}${grid}
    <polygon points="${reg.join(' ')} ${base}" fill="rgba(88,166,255,.22)"/>
    <polygon points="${tot.join(' ')} ${reg.slice().reverse().join(' ')}" fill="rgba(210,153,34,.55)"/>
    <polyline points="${tot.join(' ')}" fill="none" stroke="#58a6ff" stroke-width="1.5"/>`);

  const T = sim.tpsSamples.filter((p) => p.t >= t0);
  const tmax = Math.max(1, ...T.map((p) => p.v), ...pts.map((p) => p.lam)) * 1.1;
  const yt = (v) => 116 - (v / tmax) * 106;
  const bars = T.map((p) => `<rect x="${(xs(p.t - 1)).toFixed(1)}" y="${yt(p.v).toFixed(1)}" width="${(W / span - 1).toFixed(1)}" height="${(116 - yt(p.v)).toFixed(1)}" fill="rgba(63,185,80,.45)"/>`).join('');
  const lam = pts.map((p) => `${xs(p.t).toFixed(1)},${yt(p.lam).toFixed(1)}`).join(' ');
  svgEl($('#fig-tps'), `${bands.replaceAll(`height="${H - pad}"`, 'height="116"')}<line x1="0" x2="${W}" y1="116" y2="116" stroke="#21262d"/>
    <text x="4" y="12">${clean(tmax / 1.1, 3)}/s</text>${bars}
    <polyline points="${lam}" fill="none" stroke="#58a6ff" stroke-width="1.6"/>`);
}

const TELL = {
  transition: (e) => `the chain turned ${e.state}`,
  pin: (e) => `a person pinned it ${e.state}`,
  unpin: () => 'handed back to the chain',
  configure: (e) => e.detail,
  add: (e) => `${e.feed.name} joined`,
  remove: (e) => `${e.feed.name} left`,
};

function renderNow() {
  renderPace();
  const forced = sim.pinned || 'auto';
  $$('#force-controls button').forEach((b) => b.setAttribute('aria-pressed', String(b.dataset.mode === forced)));
  $('#force-controls').hidden = !mayChange();
  $('#force-readonly').hidden = mayChange();
  $('#k-by').textContent = sim.pinned ? `a person, until it is handed back` : 'the chain, turning on its own';
  $('#k-for').textContent = seconds(sim.t - sim.enteredAt);
  $('#k-spell').textContent = `calm ${seconds(1 / cfg.wake)} · active ${seconds(1 / cfg.calm)}, on average`;
  const p = cfg.wake / (cfg.wake + cfg.calm);
  $('#k-share').textContent = `${clean(p * 100, 3)}% at ×${clean(cfg.activeMult)}, calm ×${clean(calmMult(cfg), 3)}`;
  $('#k-last').textContent = sim.lastChange ? `${clock(sim.lastChange.t)} · ${TELL[sim.lastChange.kind](sim.lastChange)}` : 'nothing since the run started';
  const log = sim.events.filter((e) => e.kind !== 'tick').slice(-8).reverse();
  $('#now-log').innerHTML = log.map((e) => `<tr><td>${clock(e.t)}</td><td><span class="kind ${e.kind}">${e.kind}</span></td><td>${TELL[e.kind](e)}</td><td class="num">${e.tau.toFixed(2)}</td></tr>`).join('');
}

function pin(mode) {
  if (!mayChange()) return;
  sim.pinned = mode === 'auto' ? null : mode;
  if (sim.pinned) sim.enteredAt = sim.t;
  record(mode === 'auto' ? 'unpin' : 'pin');
  sim.lastChange = sim.events.at(-1);
  toast(mode === 'auto' ? 'The market is back with the chain.' : `The market is held ${mode}.`);
  render();
}

// --- Settings ----------------------------------------------------------------

const FIELDS = {
  calmSpell: { get: () => 1 / cfg.wake, label: 'a calm spell', unit: ' s' },
  activeSpell: { get: () => 1 / cfg.calm, label: 'an active spell', unit: ' s' },
  jump: { get: () => cfg.jump, label: 'heat a tick adds', unit: '' },
  decay: { get: () => cfg.decay, label: 'heat decay', unit: '/s' },
  speed: { get: () => cfg.speed, label: 'market speed', unit: '' },
  tail: { get: () => cfg.tail, label: 'tail loading', unit: '' },
};
const draftValue = (k) => (k in ui.draft ? Number(ui.draft[k]) : FIELDS[k].get());

/* The rules the service applies, run as the person types, each naming the
   number as the service's refusal would. */
function problems() {
  const v = Object.fromEntries(Object.keys(FIELDS).map((k) => [k, draftValue(k)]));
  const out = {};
  for (const k of Object.keys(v)) if (!Number.isFinite(v[k]) || (k in ui.draft && ui.draft[k].trim() === '')) out[k] = 'a number';
  if (!out.calmSpell && !(v.calmSpell > 0)) out.calmSpell = 'must be positive';
  if (!out.activeSpell && !(v.activeSpell > 0)) out.activeSpell = 'must be positive';
  if (!out.decay && !(v.decay > 0)) out.decay = 'must be positive';
  if (!out.jump && v.jump < 0) out.jump = 'must not be negative';
  if (!out.speed && !(v.speed > 0)) out.speed = 'must be positive';
  if (!out.tail && (v.tail < 0 || v.tail > 10)) out.tail = 'between 0 and 10';
  if (!out.jump && !out.decay && v.jump / v.decay >= 1) out.decay = `loop gain ${clean(v.jump / v.decay, 3)}, must be below 1`;
  const p = v.activeSpell / (v.calmSpell + v.activeSpell);
  if (!out.calmSpell && !out.activeSpell && p * cfg.activeMult >= 1) {
    out.activeSpell = `active ${clean(p * 100, 3)}% of the time; at ×${cfg.activeMult} it must be under ${clean(100 / cfg.activeMult, 3)}%`;
  }
  return { v, out };
}

function renderSettings() {
  const { v, out } = problems();
  const ro = !mayChange();
  for (const k of Object.keys(FIELDS)) {
    const input = $(`#f-${k}`);
    const field = input.closest('.field');
    if (document.activeElement !== input) input.value = k in ui.draft ? ui.draft[k] : clean(FIELDS[k].get());
    input.readOnly = ro;
    const changed = k in ui.draft && Number(ui.draft[k]) !== FIELDS[k].get();
    field.classList.toggle('changed', changed && !out[k]);
    field.classList.toggle('bad', !!out[k]);
    $(`[data-was="${k}"]`).textContent = changed ? `was ${clean(FIELDS[k].get())}${FIELDS[k].unit}` : '';
  }
  const imp = {
    calmSpell: out.calmSpell ? out.calmSpell : `${clean(1 / v.calmSpell)} /s`,
    activeSpell: out.activeSpell ? out.activeSpell : `${clean(1 / v.activeSpell)} /s`,
    jump: out.jump ? out.jump : clean(v.jump / v.decay, 3),
    decay: out.decay ? out.decay : seconds(Math.LN2 / v.decay),
    speed: out.speed ? out.speed : seconds(60 * v.speed),
    tail: out.tail ? out.tail : clean(v.tail * Math.log(cfg.activeMult), 3),
  };
  for (const [k, text] of Object.entries(imp)) $(`[data-implies="${k}"]`).textContent = text;

  const p = v.activeSpell / (v.calmSpell + v.activeSpell);
  const cm = (1 - p * cfg.activeMult) / (1 - p);
  $('#regime-derived').innerHTML = `
    <div><span>Active share</span><span>${clean(p * 100, 3)}%</span></div>
    <div><span>Active multiplier</span><span>×${clean(cfg.activeMult)} · fixed</span></div>
    <div><span>Calm multiplier</span><span>${cm > 0 ? `×${clean(cm, 3)}` : 'none: the mean pace cannot be one'}</span></div>
    <div><span>Mean pace</span><span>×1, always</span></div>`;

  const gain = v.jump / v.decay;
  $('#gain-bar').style.width = `${Math.min(100, Math.max(0, gain * 100))}%`;
  $('#gain-text').textContent = Number.isFinite(gain) ? `loop gain ${clean(gain, 3)} · ${gain < 1 ? 'settles' : 'runs away'}` : 'loop gain —';

  const tilt = (pc) => v.tail * Math.log(pc);
  $('#tail-derived').innerHTML = `
    <div><span>Tilt, active (×${clean(cfg.activeMult)})</span><span>${clean(tilt(cfg.activeMult), 3)}</span></div>
    <div><span>Tilt, calm (×${clean(cm > 0 ? cm : 0.5, 3)})</span><span>${clean(tilt(cm > 0 ? cm : 0.5), 3)}</span></div>
    <div><span>What a tilt does</span><span>spreads the stdevs; mean and variance hold</span></div>`;

  const changes = Object.keys(ui.draft).filter((k) => Number(ui.draft[k]) !== FIELDS[k].get());
  const bad = Object.keys(out).length > 0;
  $('#diff').hidden = changes.length === 0;
  $('#diff').innerHTML = changes.map((k) => `<span>${FIELDS[k].label} ${clean(FIELDS[k].get())} → ${clean(Number(ui.draft[k]))}${FIELDS[k].unit}</span>`).join('');
  $('#apply').disabled = ro || changes.length === 0 || bad;
  $('#discard').disabled = ro || Object.keys(ui.draft).length === 0;
  $('#apply').hidden = $('#discard').hidden = ro;
  $('#settings-readonly').hidden = !ro;
  $('#refusal').hidden = !bad;
  $('#refusal').textContent = bad ? `Not applied yet: ${Object.entries(out).map(([k, m]) => `${FIELDS[k].label} — ${m}`).join('; ')}.` : '';
}

function apply() {
  const { v, out } = problems();
  if (Object.keys(out).length) return;
  const changes = Object.keys(ui.draft).filter((k) => Number(ui.draft[k]) !== FIELDS[k].get());
  const detail = changes.map((k) => `${FIELDS[k].label} ${clean(FIELDS[k].get())} → ${clean(v[k])}`).join(', ');
  // The dwell in progress keeps its unit draw, so it ends at the new rate's share of it.
  const rateNow = sim.state === 'calm' ? cfg.wake : cfg.calm;
  Object.assign(cfg, { wake: 1 / v.calmSpell, calm: 1 / v.activeSpell, jump: v.jump, decay: v.decay, speed: v.speed, tail: v.tail });
  const rateNew = sim.state === 'calm' ? cfg.wake : cfg.calm;
  sim.nextTurn = sim.t + (sim.nextTurn - sim.t) * rateNow / rateNew;
  record('configure', null, { detail });
  sim.lastChange = sim.events.at(-1);
  ui.draft = {};
  toast(`Applied: ${detail}.`);
  render();
}

// --- Arrivals ----------------------------------------------------------------

function renderRaster() {
  const now = ui.frozen ?? sim.t;
  const t0 = now - ui.win;
  const feeds = FEEDS.filter((f) => ui.classes.has(f.cls));
  const ticks = sim.events.filter((e) => e.kind === 'tick' && e.t >= t0 && e.t <= now);
  const samples = sim.paceSamples.filter((p) => p.t >= t0 && p.t <= now);
  const tau0 = samples[0]?.tau ?? 0;
  const tau1 = samples.at(-1)?.tau ?? 1;
  const x = ui.axis === 'clock'
    ? (e) => ((e.t - t0) / ui.win) * 1000
    : (e) => ((e.tau - tau0) / Math.max(1e-9, tau1 - tau0)) * 1000;

  let bands = '';
  let start = null;
  samples.forEach((p, k) => {
    if (p.state === 'active' && start == null) start = p;
    if ((p.state !== 'active' || k === samples.length - 1) && start) {
      bands += `<rect x="${x(start)}" y="0" width="${Math.max(1, x(p) - x(start))}" height="100" fill="rgba(210,153,34,.13)"/>`;
      start = null;
    }
  });

  const heatMax = Math.max(0.01, ...samples.map((p) => p.heat)) * 1.15;
  const heatLine = samples.map((p) => `${x(p).toFixed(1)},${(38 - (p.heat / heatMax) * 32).toFixed(1)}`).join(' ');
  const multLine = samples.map((p) => `${x(p).toFixed(1)},${(38 - (p.mult / 2.4) * 32).toFixed(1)}`).join(' ');

  const elapsedTau = tau1 - tau0;
  let html = `<div class="hdr"><div class="label">Feed · law</div><div class="label">${ui.axis === 'clock' ? `Last ${ui.win} s, clock time` : `Market time τ, ${clean(elapsedTau, 3)} units`}</div><div class="label" style="text-align:right">Seen / asked</div></div>`;
  html += `<div class="strip-name"><span class="label">The heat</span><span class="faint" style="font-size:.66rem">and the regime</span></div>
    <div class="strip-fig"><svg viewBox="0 0 1000 40" preserveAspectRatio="none" style="display:block;width:100%;height:40px">${bands.replaceAll('height="100"', 'height="40"')}
      <polyline points="${multLine}" fill="none" stroke="rgba(88,166,255,.55)" stroke-width="1" vector-effect="non-scaling-stroke"/>
      <polyline points="${heatLine}" fill="none" stroke="#d29922" stroke-width="1.4" vector-effect="non-scaling-stroke"/></svg></div>
    <div class="strip-end">${clean(heatMax / 1.15, 2)} peak</div>`;

  feeds.forEach((f, k) => {
    const mine = ticks.filter((e) => e.feed === f);
    const asked = f.rate * f.sens * elapsedTau;
    const low = mine.length < asked * 0.6;
    const marks = mine.map((e) => `<rect x="${x(e).toFixed(1)}" y="5" width="2.2" height="12" fill="var(--class-${f.cls})"/>`).join('');
    const odd = k % 2 ? ' row-odd' : '';
    html += `<div class="name${odd}" data-feed="${f.name}"><i style="background:var(--class-${f.cls})"></i>${f.name.replace('synthetic.', '')}<span class="law">${f.mode}</span></div>
      <div class="lane${odd}" data-feed="${f.name}"><svg viewBox="0 0 1000 22" preserveAspectRatio="none">${bands.replaceAll('height="100"', 'height="22"')}${marks}</svg></div>
      <div class="count${odd}" data-feed="${f.name}"><span class="${low ? 'low' : ''}">${mine.length}</span><span class="faint">/ ${clean(asked, 3)}</span></div>`;
  });

  const ticksX = ui.axis === 'clock'
    ? [0, 0.25, 0.5, 0.75, 1].map((r) => [r * 1000, r === 1 ? 'now' : `-${clean(ui.win * (1 - r), 3)}s`])
    : [0, 0.25, 0.5, 0.75, 1].map((r) => [r * 1000, `τ ${clean(tau0 + r * elapsedTau, 4)}`]);
  html += `<div></div><div class="axis"><svg viewBox="0 0 1000 22" preserveAspectRatio="none">${ticksX.map(([px]) => `<line x1="${px}" x2="${px}" y1="0" y2="5" stroke="#4a5568" vector-effect="non-scaling-stroke"/>`).join('')}</svg>
    <div style="display:flex;justify-content:space-between;font:.66rem var(--mono);color:var(--ink-faint);padding:0 2px 6px">${ticksX.map(([, l]) => `<span>${l}</span>`).join('')}</div></div><div></div>`;
  $('#raster').innerHTML = html;
}

/* A sequential ramp for a count on the dark ground: from the sunk surface up
   through the accent to the news gold, so a cell's colour reads as "more"
   without a second hue claiming a meaning. */
const RAMP = ['#16181d', '#1d2a3d', '#26466b', '#3a6ea5', '#58a6ff', '#a7c7e7', '#e3b341'];
function ramp(v) {
  const x = Math.max(0, Math.min(1, v)) * (RAMP.length - 1);
  const i = Math.min(RAMP.length - 2, Math.floor(x));
  const f = x - i;
  const a = RAMP[i].match(/\w\w/g).map((h) => parseInt(h, 16));
  const b = RAMP[i + 1].match(/\w\w/g).map((h) => parseInt(h, 16));
  return `rgb(${a.map((c, k) => Math.round(c + (b[k] - c) * f)).join(',')})`;
}

function foldValue(b) {
  if (ui.foldFeed !== '') return b.counts[Number(ui.foldFeed)];
  if (ui.foldSeries === 'pace') return b.n ? b.pace / b.n : 0;
  return FEEDS.reduce((sum, f) => sum + (ui.foldSeries === 'all' || f.cls === ui.foldSeries ? b.counts[f.i] : 0), 0);
}

function renderFold() {
  const nowMin = Math.floor(sim.t / 60);
  const cols = ui.foldSpan;
  const firstMin = nowMin - cols + 1;
  const bins = sim.bins.filter((b) => Math.floor(b.sec / 60) >= firstMin);
  const values = bins.map(foldValue);
  // The top of the scale is a high percentile rather than the maximum, so one
  // burst does not wash every other cell out to the bottom of the ramp.
  const sorted = values.slice().sort((a, b) => a - b);
  const top = Math.max(ui.foldSeries === 'pace' && ui.foldFeed === '' ? 0.1 : 1, sorted[Math.floor(sorted.length * 0.98)] ?? 1);
  const W = 600 / cols;
  const H = 300 / 60;
  let cells = '';
  bins.forEach((b, k) => {
    const col = Math.floor(b.sec / 60) - firstMin;
    const row = ((b.sec % 60) + 60) % 60;
    cells += `<rect x="${(col * W).toFixed(2)}" y="${(row * H).toFixed(2)}" width="${(W + 0.3).toFixed(2)}" height="${(H + 0.3).toFixed(2)}" fill="${ramp(values[k] / top)}" data-k="${k}"/>`;
  });
  $('#fold').innerHTML = cells;
  ui.foldBins = bins; ui.foldValues = values;

  let states = '';
  for (let c = 0; c < cols; c++) {
    const inCol = bins.filter((b) => Math.floor(b.sec / 60) - firstMin === c);
    const share = inCol.length ? inCol.reduce((s2, b) => s2 + b.active / Math.max(1, b.n), 0) / inCol.length : 0;
    if (share > 0) states += `<rect x="${c * W}" y="0" width="${W + 0.3}" height="10" fill="rgba(210,153,34,${(0.15 + 0.7 * share).toFixed(2)})"/>`;
  }
  $('#fold-state').innerHTML = states;

  const label = (m) => clock(m * 60).slice(0, 5);
  const marks = [0, 0.25, 0.5, 0.75, 1].map((r) => label(firstMin + Math.round(r * (cols - 1))));
  marks[4] = 'now';
  $('#fold-x').innerHTML = marks.map((m) => `<span>${m}</span>`).join('');
  $('#fold-ramp').style.background = `linear-gradient(to top, ${RAMP.join(',')})`;
  $('#fold-max').textContent = ui.foldSeries === 'pace' && ui.foldFeed === '' ? `×${clean(top, 3)}` : `${clean(top, 3)}/s`;
}

function feedTip(f) {
  const law = f.mode === 'hawkes' ? `Hawkes, branching ${f.n}, decay ${f.beta}/τ` : f.mode === 'poisson' ? 'Poisson' : 'periodic';
  return `<b>${f.name}</b><br>${law}<br>rate ${clean(f.rate, 3)}/s × sensitivity ${clean(f.sens)}<br>now ${clean(f.rate * f.sens * pace(), 3)}/s at pace ×${pace().toFixed(2)}`;
}

// --- Laws --------------------------------------------------------------------

const FAMILIES = {
  periodic: { title: 'Periodic', formula: 'gap = 1 / rate', reads: 'One tick each nominal period, from a random phase. The gaps are all one.', stable: 'always' },
  poisson: { title: 'Poisson', formula: 'λ = rate', reads: 'Memoryless: a gap is an exponential draw, so ticks neither cluster nor space out.', stable: 'always' },
  hawkes: { title: 'Hawkes', formula: 'λ(t) = μ + Σ n·β·e^(−β(t − tₖ))', reads: 'Each tick raises the intensity, which decays: ticks come in bursts. μ = rate·(1 − n) keeps the mean at the rate.', stable: 'branching n < 1' },
};
ui.laws = new Set(['periodic', 'poisson', 'hawkes']);

function renderLaws() {
  $('#law-families').innerHTML = Object.entries(FAMILIES).map(([k, f]) => {
    const n = FEEDS.filter((g) => g.mode === k).length;
    return `<div class="card"><div class="card-head"><div><h2>${f.title}</h2><p>${f.reads}</p></div><div class="act"><span class="v mono" style="font:700 1.2rem var(--mono)">${n}</span><span class="label">feeds</span></div></div>
      <div class="kv"><div><span>Intensity</span><span>${f.formula}</span></div><div><span>Stable when</span><span>${f.stable}</span></div></div></div>`;
  }).join('');
  const t0 = sim.t - 60;
  $('#law-rows').innerHTML = FEEDS.filter((f) => ui.laws.has(f.mode)).map((f) => {
    const marks = sim.events.filter((e) => e.kind === 'tick' && e.feed === f && e.t >= t0)
      .map((e) => `<rect x="${(((e.t - t0) / 60) * 120).toFixed(1)}" y="2" width="1.5" height="12" fill="var(--class-${f.cls})"/>`).join('');
    const hawkes = f.mode === 'hawkes';
    return `<tr><td><i style="display:inline-block;width:7px;height:7px;border-radius:2px;margin-right:7px;background:var(--class-${f.cls})"></i>${f.name.replace('synthetic.', '')}</td>
      <td>${f.mode}</td><td class="num">${clean(f.rate * 3600, 4)}</td><td class="num">${clean(f.sens)}</td>
      <td class="num">${hawkes ? clean(f.n) : '—'}</td><td class="num">${hawkes ? clean(f.beta) : '—'}</td>
      <td>${hawkes ? (f.n < 1 ? '<span class="kind add">stable</span>' : '<span class="kind remove">runs away</span>') : '<span class="faint">always</span>'}</td>
      <td><svg viewBox="0 0 120 16" style="width:120px;height:16px;background:var(--sunk);border-radius:3px">${marks}</svg></td>
      <td><a href="../control-how-a-market-ticks/" title="The feed's Arrivals tab, as the earlier prototype draws it">Arrivals tab →</a></td></tr>`;
  }).join('');
}

// --- Fit ---------------------------------------------------------------------

function renderFit() {
  const t0 = sim.t - 120;
  const s0 = sim.paceSamples.find((p) => p.t >= t0);
  const elapsedTau = sim.tau - (s0?.tau ?? 0);
  $('#fit-rows').innerHTML = FEEDS.map((f) => {
    const seen = sim.events.filter((e) => e.kind === 'tick' && e.feed === f && e.t >= t0).length;
    const asked = f.rate * f.sens * elapsedTau;
    const r = asked > 0 ? seen / asked : 0;
    const pos = Math.max(0, Math.min(100, 50 + Math.log2(Math.max(r, 1e-3)) * 25));
    return `<tr class="pick${f.name === ui.picked ? ' sel' : ''}" data-pick="${f.name}"><td><i style="display:inline-block;width:7px;height:7px;border-radius:2px;margin-right:7px;background:var(--class-${f.cls})"></i>${f.name.replace('synthetic.', '')}</td>
      <td>${f.mode}</td><td class="num">${clean(f.rate, 3)} × ${clean(f.sens)}</td><td class="num">${seen}</td><td class="num">${clean(asked, 3)}</td>
      <td class="num">${clean(r, 3)}<span class="ratio" title="log scale, one in the middle"><i style="left:${pos}%"></i></span></td></tr>`;
  }).join('');

  const f = FEEDS.find((g) => g.name === ui.picked);
  const comps = sim.events.filter((e) => e.kind === 'tick' && e.feed === f && e.t >= sim.t - 300 && e.wall != null).map((e) => e.comp).sort((a, b) => a - b);
  $('#qq-title').textContent = `${f.name.replace('synthetic.', '')}: gaps against Exp(1)`;
  $('#qq-n').textContent = `${comps.length} gaps, last 5 minutes`;
  const S = 300, P = 30, max = Math.max(4, ...(comps.length ? [comps.at(-1)] : [0])) * 1.05;
  const sx = (v) => P + (v / max) * (S - P - 8);
  const sy = (v) => S - P - (v / max) * (S - P - 8);
  let body = `<line x1="${sx(0)}" y1="${sy(0)}" x2="${sx(max)}" y2="${sy(max)}" stroke="#4a5568" stroke-dasharray="4 4"/>
    <line x1="${P}" x2="${S - 4}" y1="${S - P}" y2="${S - P}" stroke="#30363d"/><line x1="${P}" x2="${P}" y1="4" y2="${S - P}" stroke="#30363d"/>
    <text x="${S - 70}" y="${S - 10}">Exp(1)</text><text x="4" y="14">gaps</text>`;
  if (comps.length >= 5) {
    const n = comps.length;
    body += comps.map((c, i) => `<circle cx="${sx(-Math.log(1 - (i + 0.5) / n))}" cy="${sy(c)}" r="2.6" fill="var(--class-${f.cls})" opacity=".8"/>`).join('');
  } else {
    body += `<text x="${P + 10}" y="${S / 2}">too few ticks for a plot</text>`;
  }
  svgEl($('#fig-qq'), body);

  const tps = sim.tpsSamples.at(-1)?.v ?? 0;
  $('#health').innerHTML = [
    ['Ticks a second', tps.toFixed(1), 'smoothed'],
    ['Heap', String(FEEDS.length), 'entries, live and stale'],
    ['Lateness', `${sim.late.toFixed(1)} ms`, 'smoothed over ten ticks'],
    ['Latest tick', `${sim.lateMax.toFixed(1)} ms`, 'since the last read'],
    ['Series', String(FEEDS.length), 'enrolled with the heat'],
    ['Enrolled rate', `${clean(R, 3)}/s`, 'the heat is shared over it'],
    ['Run seed', '42', 'keys every stream'],
  ].map(([l, v, s]) => `<div class="metric"><span class="label">${l}</span><span class="v">${v}</span><span class="sub">${s}</span></div>`).join('');
}

// --- Trace -------------------------------------------------------------------

const KINDS = ['tick', 'transition', 'pin', 'unpin', 'configure', 'add', 'remove'];
$('#kinds').innerHTML = KINDS.map((k) => `<button data-kind="${k}" aria-pressed="${ui.kinds.has(k)}">${k}</button>`).join('');

function renderTrace() {
  const q = $('#trace-search').value.trim().toLowerCase();
  const rows = [];
  let total = 0;
  for (let k = sim.events.length - 1; k >= 0; k--) {
    const e = sim.events[k];
    if (!ui.kinds.has(e.kind)) continue;
    if (q && !(e.feed && e.feed.name.includes(q))) continue;
    total += 1;
    if (rows.length < 300) rows.push(e);
  }
  $('#trace-rows').innerHTML = rows.map((e) => `<tr><td>${clock(e.t)}</td><td class="num">${e.tau.toFixed(3)}</td><td><span class="kind ${e.kind}">${e.kind}</span></td>
    <td>${e.feed ? e.feed.name.replace('synthetic.', '') : '—'}</td><td>${e.state}</td><td class="num">×${e.pace.toFixed(2)}</td>
    <td class="num">${e.kind === 'tick' ? clean(e.gap, 3) : ''}</td><td class="num">${e.kind === 'tick' && e.wall != null ? seconds(e.wall) : ''}</td></tr>`).join('');
  $('#trace-count').textContent = `${total} decisions match · the newest 300 shown`;
}

// --- Wiring ------------------------------------------------------------------

function render() {
  renderStrip();
  if (ui.tab === 'now') renderNow();
  if (ui.tab === 'settings') renderSettings();
  if (ui.tab === 'arrivals') { renderFold(); renderRaster(); }
  if (ui.tab === 'laws') renderLaws();
  if (ui.tab === 'fit') renderFit();
  if (ui.tab === 'trace') renderTrace();
}

function openTab(name) {
  ui.tab = name;
  $$('nav.tabs button').forEach((b) => b.setAttribute('aria-selected', String(b.dataset.tab === name)));
  $$('[role="tabpanel"]').forEach((p) => { p.hidden = p.id !== `tab-${name}`; });
  history.replaceState(null, '', `#${name}`);
  render();
}

$$('nav.tabs button').forEach((b) => b.addEventListener('click', () => openTab(b.dataset.tab)));
$$('[data-goto]').forEach((b) => b.addEventListener('click', () => openTab(b.dataset.goto)));
$$('#force-controls button').forEach((b) => b.addEventListener('click', () => pin(b.dataset.mode)));
$$('[data-persona]').forEach((b) => b.addEventListener('click', () => {
  ui.persona = b.dataset.persona;
  $$('[data-persona]').forEach((c) => c.setAttribute('aria-checked', String(c === b)));
  ui.draft = {};
  render();
}));
Object.keys(FIELDS).forEach((k) => {
  $(`#f-${k}`).addEventListener('input', (ev) => { ui.draft[k] = ev.target.value; renderSettings(); });
  $(`#f-${k}`).addEventListener('blur', () => renderSettings());
});
$('#apply').addEventListener('click', apply);
$('#discard').addEventListener('click', () => { ui.draft = {}; renderSettings(); });
$$('#win button').forEach((b) => b.addEventListener('click', () => {
  ui.win = Number(b.dataset.win);
  $$('#win button').forEach((c) => c.setAttribute('aria-checked', String(c === b)));
  renderRaster();
}));
$$('#axis button').forEach((b) => b.addEventListener('click', () => {
  ui.axis = b.dataset.axis;
  $$('#axis button').forEach((c) => c.setAttribute('aria-checked', String(c === b)));
  renderRaster();
}));
$$('#classes button').forEach((b) => b.addEventListener('click', () => {
  const c = b.dataset.class;
  if (ui.classes.has(c) && ui.classes.size > 1) ui.classes.delete(c); else ui.classes.add(c);
  $$('#classes button').forEach((d) => d.setAttribute('aria-pressed', String(ui.classes.has(d.dataset.class))));
  renderRaster();
}));
$('#freeze').addEventListener('click', (ev) => {
  ui.frozen = ui.frozen == null ? sim.t : null;
  ev.target.setAttribute('aria-pressed', String(ui.frozen != null));
  ev.target.textContent = ui.frozen == null ? 'Freeze' : 'Follow now';
  renderRaster();
});
$('#raster').addEventListener('mousemove', (ev) => {
  const cell = ev.target.closest('[data-feed]');
  $$('#raster .hover').forEach((c) => c.classList.remove('hover'));
  const tip = $('#tip');
  if (!cell) { tip.hidden = true; return; }
  $$(`#raster [data-feed="${cell.dataset.feed}"]`).forEach((c) => c.classList.add('hover'));
  tip.innerHTML = feedTip(FEEDS.find((f) => f.name === cell.dataset.feed));
  tip.hidden = false;
  tip.style.left = `${Math.min(window.innerWidth - 310, ev.clientX + 14)}px`;
  tip.style.top = `${ev.clientY + 14}px`;
});
$('#fold-feed').innerHTML += FEEDS.map((f) => `<option value="${f.i}">${f.name.replace('synthetic.', '')}</option>`).join('');
$('#fold-series').addEventListener('change', (ev) => { ui.foldSeries = ev.target.value; ui.foldFeed = ''; $('#fold-feed').value = ''; renderFold(); });
$('#fold-feed').addEventListener('change', (ev) => { ui.foldFeed = ev.target.value; renderFold(); });
$$('#fold-span button').forEach((b) => b.addEventListener('click', () => {
  ui.foldSpan = Number(b.dataset.span);
  $$('#fold-span button').forEach((c) => c.setAttribute('aria-checked', String(c === b)));
  renderFold();
}));
$('#fold').addEventListener('mousemove', (ev) => {
  const k = ev.target.dataset?.k;
  const tip = $('#tip');
  if (k == null) { tip.hidden = true; return; }
  const b = ui.foldBins[Number(k)];
  const v = ui.foldValues[Number(k)];
  const what = ui.foldFeed !== '' ? FEEDS[Number(ui.foldFeed)].name : $('#fold-series').selectedOptions[0].textContent;
  tip.innerHTML = `<b>${clock(b.sec)}</b><br>${what}: ${ui.foldSeries === 'pace' && ui.foldFeed === '' ? `×${v.toFixed(2)}` : `${v} ticks`}<br>${b.active ? 'active' : 'calm'} · pace ×${(b.pace / b.n).toFixed(2)}`;
  tip.hidden = false;
  tip.style.left = `${Math.min(window.innerWidth - 310, ev.clientX + 14)}px`;
  tip.style.top = `${ev.clientY + 14}px`;
});
$('#fold').addEventListener('mouseleave', () => { $('#tip').hidden = true; });
$('#raster').addEventListener('mouseleave', () => { $('#tip').hidden = true; });
$('#fit-rows').addEventListener('click', (ev) => {
  const row = ev.target.closest('[data-pick]');
  if (row) { ui.picked = row.dataset.pick; renderFit(); }
});
$$('#kinds button').forEach((b) => b.addEventListener('click', () => {
  const k = b.dataset.kind;
  if (ui.kinds.has(k)) ui.kinds.delete(k); else ui.kinds.add(k);
  b.setAttribute('aria-pressed', String(ui.kinds.has(k)));
  renderTrace();
}));
$('#trace-search').addEventListener('input', renderTrace);

setInterval(() => {
  advance(0.5);
  // The form is redrawn only when its numbers could have changed, so typing is never interrupted.
  if (ui.tab === 'settings') { renderStrip(); return; }
  if (ui.tab === 'trace' && document.activeElement === $('#trace-search')) { renderStrip(); return; }
  render();
}, 500);

$$('#law-filter button').forEach((b) => b.addEventListener('click', () => {
  const k = b.dataset.law;
  if (ui.laws.has(k) && ui.laws.size > 1) ui.laws.delete(k); else ui.laws.add(k);
  $$('#law-filter button').forEach((c) => c.setAttribute('aria-pressed', String(ui.laws.has(c.dataset.law))));
  renderLaws();
}));

openTab(['now', 'settings', 'arrivals', 'laws', 'fit', 'trace'].includes(location.hash.slice(1)) ? location.hash.slice(1) : 'now');
