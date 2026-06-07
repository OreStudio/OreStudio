/*
 * Tiny SVG chart components — no chart library, just enough for the
 * sprint panels: bar charts and a cumulative line (burn-up).
 */
import { h } from 'https://esm.sh/preact@10.25.4';
import htm from 'https://esm.sh/htm@3.1.1';

const html = htm.bind(h);

const W = 360, H = 180, PAD = { t: 12, r: 10, b: 34, l: 30 };
const IW = W - PAD.l - PAD.r;
const IH = H - PAD.t - PAD.b;

/** Vertical bar chart: data = [{label, value, color?}]. */
export function BarChart({ data, title }) {
  const max = Math.max(1, ...data.map(d => d.value));
  const bw = IW / data.length;
  const fallback = 'var(--accent, #58a6ff)';
  return html`
    <div class="panel">
      <div class="panel-title">${title}</div>
      <svg viewBox="0 0 ${W} ${H}" preserveAspectRatio="xMidYMid meet">
        ${data.map((d, i) => {
          const bh = (d.value / max) * IH;
          const x = PAD.l + i * bw + bw * 0.15;
          const y = PAD.t + IH - bh;
          return html`
            <g>
              <rect x=${x} y=${y} width=${bw * 0.7} height=${bh}
                    fill=${d.color || fallback} rx="2">
                <title>${d.label}: ${d.value}</title>
              </rect>
              <text x=${x + bw * 0.35} y=${H - PAD.b + 14}
                    text-anchor="middle" class="chart-label"
                    transform="rotate(-35 ${x + bw * 0.35} ${H - PAD.b + 14})">
                ${d.label}
              </text>
              ${d.value > 0 && html`
                <text x=${x + bw * 0.35} y=${y - 4}
                      text-anchor="middle" class="chart-value">${d.value}</text>`}
            </g>`;
        })}
      </svg>
    </div>`;
}

/** Cumulative line chart: points = [{x: 'YYYY-MM-DD', y}], sorted. */
export function LineChart({ points, title, color = 'var(--state-done)' }) {
  if (!points.length) return html`
    <div class="panel">
      <div class="panel-title">${title}</div>
      <div class="panel-empty">no dated completions yet</div>
    </div>`;

  const xs = points.map(p => new Date(p.x).getTime());
  const x0 = Math.min(...xs), x1 = Math.max(...xs, x0 + 1);
  const yMax = Math.max(1, ...points.map(p => p.y));
  const px = t => PAD.l + ((t - x0) / (x1 - x0)) * IW;
  const py = v => PAD.t + IH - (v / yMax) * IH;

  // Step line: completions accumulate per day.
  let d = `M ${px(xs[0])} ${py(0)}`;
  points.forEach((p, i) => {
    d += ` L ${px(xs[i])} ${py(i === 0 ? 0 : points[i - 1].y)}`;
    d += ` L ${px(xs[i])} ${py(p.y)}`;
  });

  const fmt = t => new Date(t).toISOString().slice(5, 10);
  return html`
    <div class="panel">
      <div class="panel-title">${title}</div>
      <svg viewBox="0 0 ${W} ${H}" preserveAspectRatio="xMidYMid meet">
        <path d=${d} fill="none" stroke=${color} stroke-width="2"/>
        ${points.map(p => html`
          <circle cx=${px(new Date(p.x).getTime())} cy=${py(p.y)} r="3" fill=${color}>
            <title>${p.x}: ${p.y} done</title>
          </circle>`)}
        <text x=${PAD.l} y=${H - PAD.b + 14} class="chart-label">${fmt(x0)}</text>
        <text x=${W - PAD.r} y=${H - PAD.b + 14} text-anchor="end" class="chart-label">${fmt(x1)}</text>
        <text x=${PAD.l - 6} y=${PAD.t + 8} text-anchor="end" class="chart-value">${yMax}</text>
      </svg>
    </div>`;
}

/**
 * Clickable stacked bars — one bar per timeline bucket, one segment
 * per event category. A ⚠ above a bar flags buckets whose snapshot
 * reports problems.
 */
export function StackedBars({ buckets, selected, onPick, title }) {
  const max = Math.max(1, ...buckets.map(b =>
    b.segments.reduce((n, s) => n + s.value, 0)));
  const bw = IW / Math.max(1, buckets.length);
  return html`
    <div class="panel">
      <div class="panel-title">${title}</div>
      <svg viewBox="0 0 ${W} ${H}" preserveAspectRatio="xMidYMid meet">
        ${buckets.map((b, i) => {
          const total = b.segments.reduce((n, s) => n + s.value, 0);
          const x = PAD.l + i * bw + bw * 0.12;
          let y = PAD.t + IH;
          const segs = b.segments.map(s => {
            const h = (s.value / max) * IH;
            y -= h;
            return { ...s, y, h };
          });
          return html`
            <g class="tl-bar ${i === selected ? 'selected' : ''}"
               onClick=${() => onPick(i)}>
              <rect x=${x - bw * 0.06} y=${PAD.t - 4}
                    width=${bw * 0.88} height=${IH + 8}
                    fill="transparent" stroke=${i === selected ? 'var(--accent)' : 'none'}
                    stroke-width="1" rx="4"/>
              ${segs.map(s => html`
                <rect x=${x} y=${s.y} width=${bw * 0.76} height=${s.h}
                      fill=${s.color} rx="1">
                  <title>${b.label}: ${s.label} ${s.value}</title>
                </rect>`)}
              ${b.flag && html`
                <text x=${x + bw * 0.38} y=${Math.min(...segs.map(s => s.y), PAD.t + IH) - 5}
                      text-anchor="middle" class="chart-flag">⚠</text>`}
              <text x=${x + bw * 0.38} y=${H - PAD.b + 14}
                    text-anchor="middle" class="chart-label">${b.label}</text>
              ${total > 0 && html`
                <text x=${x + bw * 0.38} y=${PAD.t + IH + 12} visibility="hidden"></text>`}
            </g>`;
        })}
      </svg>
    </div>`;
}
