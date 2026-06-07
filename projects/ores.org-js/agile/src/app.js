/*
 * Agile board PoC — Preact UI over the org-roam graph export.
 *
 * Layout follows the classic sprint-dashboard hierarchy: summary
 * tiles, chart panels, then the kanban board with epic filters and
 * click-through drawers. Org id-links render as live links: targets
 * inside the agile tree navigate within the app (switching sprint if
 * needed); anything else opens its published site page.
 */
import { h, render } from 'https://esm.sh/preact@10.25.4';
import { useState, useEffect, useMemo } from 'https://esm.sh/preact@10.25.4/hooks';
import htm from 'https://esm.sh/htm@3.1.1';
import { loadAgileIndex, loadSprint, loadBacklog, loadTimelineBuckets, velocity, burnup } from './data.js';
import { section, fieldTable, linkText } from './orgparse.js';
import { BarChart, LineChart, StackedBars } from './charts.js';
import { orgToHtml } from './render.js';

const html = htm.bind(h);

// Board columns are STORY states (#+todo: BACKLOG STARTED | DONE
// ABANDONED). DISCOVERED exists only on tasks, so it has a badge style
// below but no column.
const COLUMNS = ['BACKLOG', 'STARTED', 'BLOCKED', 'DONE', 'ABANDONED'];
const STATE_CLASS = {
  BACKLOG: 'backlog', DISCOVERED: 'backlog', STARTED: 'started',
  BLOCKED: 'blocked', DONE: 'done', ABANDONED: 'abandoned',
};
const STATE_COLOR = {
  BACKLOG: 'var(--state-backlog)', STARTED: 'var(--state-started)',
  BLOCKED: 'var(--state-blocked)', DONE: 'var(--state-done)',
  ABANDONED: 'var(--state-abandoned)',
};

function Badge({ state }) {
  return html`<span class="badge ${STATE_CLASS[state] || 'unknown'}">${state}</span>`;
}

/*
 * Render org text with live links. id: targets call onNav; https
 * targets open externally; everything else degrades to plain text.
 */
function OrgText({ text, onNav }) {
  const parts = [];
  let last = 0;
  const re = /\[\[([^\][]+)\](?:\[([^\]]*)\])?\]/g;
  let m;
  while ((m = re.exec(text)) !== null) {
    if (m.index > last) parts.push(text.slice(last, m.index));
    const [, target, desc] = m;
    const label = desc || target;
    if (target.startsWith('id:')) {
      const id = target.slice(3).toUpperCase();
      parts.push(html`<a class="id-link" onClick=${e => { e.stopPropagation(); onNav(id); }}>${label}</a>`);
    } else if (/^https?:/.test(target)) {
      parts.push(html`<a class="ext-link" href=${target} target="_blank">${label}</a>`);
    } else {
      parts.push(label);
    }
    last = m.index + m[0].length;
  }
  if (last < text.length) parts.push(text.slice(last));
  return html`<${'span'}>${parts}<//>`;
}

function Progress({ tasks }) {
  if (!tasks.length) return null;
  const done = tasks.filter(t => t.state === 'DONE').length;
  const pct = Math.round(100 * done / tasks.length);
  return html`
    <div class="progress" title="${done}/${tasks.length} tasks done">
      <div class="progress-bar" style="width:${pct}%"></div>
      <span class="progress-label">${done}/${tasks.length}</span>
    </div>`;
}

/* Summary tiles: the at-a-glance row above the board. */
function Tiles({ stories }) {
  const tasks = stories.flatMap(s => s.tasks);
  const count = (xs, st) => xs.filter(x => x.state === st).length;
  const tdone = count(tasks, 'DONE');
  const pct = tasks.length ? Math.round(100 * tdone / tasks.length) : 0;
  const tiles = [
    { label: 'stories', value: stories.length },
    { label: 'stories done', value: count(stories, 'DONE'), cls: 'done' },
    { label: 'in flight', value: count(stories, 'STARTED'), cls: 'started' },
    { label: 'blocked', value: count(stories, 'BLOCKED'), cls: 'blocked' },
    { label: 'tasks done', value: `${tdone}/${tasks.length}` },
    { label: 'tasks complete', value: `${pct}%`, cls: pct >= 70 ? 'done' : pct >= 30 ? 'started' : 'blocked' },
  ];
  return html`
    <div class="tiles">
      ${tiles.map(t => html`
        <div class="tile">
          <div class="tile-value ${t.cls || ''}">${t.value}</div>
          <div class="tile-label">${t.label}</div>
        </div>`)}
    </div>`;
}

function Panels({ model, vel }) {
  const bu = useMemo(() => burnup(model), [model]);
  const tasks = model.stories.flatMap(s => s.tasks);
  const status = COLUMNS
    .map(c => ({ label: c.toLowerCase(), value: tasks.filter(t => t.state === c).length,
                 color: STATE_COLOR[c] }))
    .filter(d => d.value > 0);
  return html`
    <div class="panels">
      <${LineChart} points=${bu} title="Tasks completed (burn-up)" />
      <${BarChart} data=${vel.slice(-8)} title="Velocity — stories done per sprint" />
      <${BarChart} data=${status} title="Tasks by state" />
    </div>`;
}

/* Epic chips: theme paths from the sprint's Stories subtree. */
function EpicChips({ stories, epic, onPick }) {
  const epics = useMemo(() => {
    const m = new Map();
    for (const s of stories) {
      const key = s.theme || '(no epic)';
      m.set(key, (m.get(key) || 0) + 1);
    }
    return [...m.entries()].sort((a, b) => b[1] - a[1]);
  }, [stories]);
  if (epics.length < 2) return null;
  return html`
    <div class="chips">
      ${epics.map(([name, n]) => html`
        <button class="chip ${epic === name ? 'active' : ''}"
                onClick=${() => onPick(epic === name ? null : name)}>
          ${name} <span class="count">${n}</span>
        </button>`)}
    </div>`;
}

/* Deterministic pastel per epic so related cards share a hue. */
function themeColor(theme) {
  let hue = 0;
  for (const c of theme || 'x') hue = (hue * 31 + c.charCodeAt(0)) % 360;
  return `hsl(${hue}, 70%, 72%)`;
}

/*
 * Age line, uncoloured: created date can long predate actual work
 * (captures promoted from the backlog), so days-open is indicative
 * only. Measuring from the first task start would be more honest —
 * revisit if the signal matters.
 */
function CardAge({ story }) {
  const closed = story.state === 'DONE' || story.state === 'ABANDONED';
  const days = Math.max(0, Math.round((Date.now() - new Date(story.created)) / 86400000));
  return html`
    <div class="card-age">
      ${story.created}${closed ? '' : ` · open ${days}d`}
    </div>`;
}

function StoryCard({ story, onSelect }) {
  return html`
    <div class="card" style="border-left: 3px solid ${themeColor(story.theme)}"
         onClick=${e => { e.stopPropagation(); onSelect(story); }}>
      <div class="card-title" style="color: ${themeColor(story.theme)}">${story.title}</div>
      ${story.theme && html`<div class="card-theme">${story.theme}</div>`}
      ${story.created && html`<${CardAge} story=${story} />`}
      <${Progress} tasks=${story.tasks} />
    </div>`;
}

function FieldRows({ fields, skip = [], onNav }) {
  return Object.entries(fields)
    .filter(([k]) => !skip.includes(k))
    .map(([k, v]) => html`
      <tr><th>${k}</th><td><${OrgText} text=${v} onNav=${onNav} /></td></tr>`);
}

function ProseSection({ doc, title, onNav }) {
  const s = section(doc, title);
  if (!s || (!s.prose.length && !s.items.length)) return null;
  return html`
    <h3>${title}</h3>
    ${s.prose.map(p => html`<p><${OrgText} text=${p} onNav=${onNav} /></p>`)}
    ${s.items.length ? html`
      <ul>${s.items.map(i => html`<li><${OrgText} text=${i} onNav=${onNav} /></li>`)}</ul>` : null}`;
}

/*
 * Whole-document rendering via uniorg (same parser family as the
 * knowledge graph). id: links navigate in-app; external links open in
 * a new tab.
 */
function OrgDoc({ doc, onNav }) {
  const rendered = useMemo(() => {
    try { return orgToHtml(doc.raw || ''); }
    catch (e) { return `<p class="muted">render failed: ${e}</p>`; }
  }, [doc]);
  const onClick = e => {
    const a = e.target.closest('a');
    if (!a) return;
    const href = a.getAttribute('href') || '';
    if (href.startsWith('id:')) {
      e.preventDefault();
      e.stopPropagation();
      onNav(href.slice(3).toUpperCase());
    } else {
      a.target = '_blank';
    }
  };
  // Trust boundary: graphdata.json is generated from this repo's own
  // org files, so the rendered HTML is first-party and unsanitised by
  // design. If ?data= ever points at an untrusted source, sanitise here.
  return html`<div class="org-render" onClick=${onClick}
                   dangerouslySetInnerHTML=${{ __html: rendered }}></div>`;
}

function DocLink({ doc }) {
  if (!doc.url) return null;
  return html`<a class="doc-link" href=${doc.url} target="_blank">open doc ↗</a>`;
}

function TaskDetail({ task, onBack, onNav }) {
  const doc = task.doc;
  return html`
    <div class="detail">
      <button class="back" onClick=${onBack}>← Story</button>
      <div class="detail-header">
        <${Badge} state=${task.state} />
        <h2>${task.title}</h2>
      </div>
      <p class="muted">${doc.keywords.description || ''}</p>
      <${OrgDoc} doc=${doc} onNav=${onNav} />
      <${DocLink} doc=${doc} />
    </div>`;
}

function StoryDetail({ story, task, onSelectTask, onClose, onNav }) {
  if (task) {
    return html`
      <div class="drawer" onClick=${e => e.stopPropagation()}>
        <button class="close" onClick=${onClose}>✕</button>
        <${TaskDetail} task=${task} onBack=${() => onSelectTask(null)} onNav=${onNav} />
      </div>`;
  }

  const doc = story.doc;
  return html`
    <div class="drawer" onClick=${e => e.stopPropagation()}>
      <button class="close" onClick=${onClose}>✕</button>
      <div class="detail">
        <div class="detail-header">
          <${Badge} state=${story.state} />
          <h2>${story.title}</h2>
        </div>
        <p class="muted">${doc.keywords.description || ''}</p>
        <h3>Tasks</h3>
        <ul class="task-list">
          ${story.tasks.map(t => html`
            <li class="task-item" onClick=${() => onSelectTask(t)}
                title=${t.state === 'BLOCKED' && t.waiting ? `Waiting on: ${t.waiting}` : ''}>
              <${Badge} state=${t.state} />
              <span>${t.title}</span>
            </li>`)}
        </ul>
        <${OrgDoc} doc=${doc} onNav=${onNav} />
        <${DocLink} doc=${doc} />
      </div>
    </div>`;
}

function SprintHeader({ sprint, onOpen }) {
  if (!sprint) return null;
  return html`
    <div class="sprint-header">
      <h1><a class="sprint-link" onClick=${e => { e.stopPropagation(); onOpen(); }}>
        ${linkText(sprint.keywords.title || '')}</a></h1>
    </div>`;
}

function SprintDetail({ sprint, onClose, onNav }) {
  const state = (fieldTable(sprint, 'Status')['state'] || '').trim().toUpperCase();
  return html`
    <div class="drawer" onClick=${e => e.stopPropagation()}>
      <button class="close" onClick=${onClose}>✕</button>
      <div class="detail">
        <div class="detail-header">
          ${state && html`<${Badge} state=${state} />`}
          <h2>${linkText(sprint.keywords.title || '')}</h2>
        </div>
        <p class="muted">${sprint.keywords.description || ''}</p>
        <${OrgDoc} doc=${sprint} onNav=${onNav} />
        <${DocLink} doc=${sprint} />
      </div>
    </div>`;
}

function Board({ stories, filter, epic, onSelect }) {
  const visible = useMemo(() => {
    let v = stories;
    if (epic) v = v.filter(s => (s.theme || '(no epic)') === epic);
    const f = filter.trim().toLowerCase();
    if (f) v = v.filter(s =>
      s.title.toLowerCase().includes(f) ||
      (s.theme || '').toLowerCase().includes(f) ||
      s.tasks.some(t => t.title.toLowerCase().includes(f)));
    return v;
  }, [stories, filter, epic]);

  return html`
    <div class="board">
      ${COLUMNS.map(col => {
        const cards = visible.filter(s => s.state === col);
        return html`
          <div class="column">
            <div class="column-header ${STATE_CLASS[col]}">
              ${col} <span class="count">${cards.length}</span>
            </div>
            ${cards.map(s => html`<${StoryCard} story=${s} onSelect=${onSelect} />`)}
          </div>`;
      })}
    </div>`;
}

function CaptureDetail({ item, onClose, onNav }) {
  const doc = item.doc;
  return html`
    <div class="drawer" onClick=${e => e.stopPropagation()}>
      <button class="close" onClick=${onClose}>✕</button>
      <div class="detail">
        <div class="detail-header">
          <span class="badge backlog">CAPTURE</span>
          <h2>${item.title}</h2>
        </div>
        <p class="muted">${item.description}</p>
        <${OrgDoc} doc=${doc} onNav=${onNav} />
        <${DocLink} doc=${doc} />
      </div>
    </div>`;
}

const BUCKET_COLOR = {
  inbox: '#c792ea',      // violet
  next: '#4dd0c4',       // teal
  deferred: '#f78c6c',   // coral
  discarded: '#e57fb3',  // magenta
};

function BacklogBoard({ buckets, filter, onSelect }) {
  const match = it => {
    const f = filter.trim().toLowerCase();
    if (!f) return true;
    return it.title.toLowerCase().includes(f) ||
           it.description.toLowerCase().includes(f);
  };
  return html`
    <div class="board backlog-board">
      ${buckets.map(b => {
        const items = b.items.filter(match);
        return html`
          <div class="column">
            <div class="column-header" style="color: ${BUCKET_COLOR[b.name]}">
              ${b.name.toUpperCase()} <span class="count">${items.length}</span>
            </div>
            ${items.map(it => html`
              <div class="card" style="border-left: 3px solid ${BUCKET_COLOR[b.name]}"
                   onClick=${e => { e.stopPropagation(); onSelect({ ...it, bucket: b.name }); }}>
                <div class="card-title" style="color: ${BUCKET_COLOR[b.name]}">${it.title}</div>
                <div class="card-theme">${it.description.slice(0, 90)}</div>
              </div>`)}
          </div>`;
      })}
    </div>`;
}

const TL_COLORS = {
  stories: 'var(--accent)', tasks: 'var(--state-done)',
  captures: '#c792ea', prs: '#f78c6c',
};

function TimelineView({ buckets, selected, onPick, onNav }) {
  if (!buckets.length) return html`
    <div class="loading">No timeline snapshots yet — they are written by
    the snapshot skill (see the agile timeline story).</div>`;
  const chart = buckets.map(b => ({
    label: b.label,
    flag: b.hasProblems,
    segments: Object.entries(b.counts).map(([k, v]) => ({
      label: k, value: v, color: TL_COLORS[k] })),
  }));
  const b = buckets[selected];
  return html`
    <div class="timeline-view">
      <div class="tl-chart">
        <${StackedBars} buckets=${chart} selected=${selected}
                        onPick=${onPick} title="Events per bucket — click a bar" />
        <div class="tl-legend">
          ${Object.entries(TL_COLORS).map(([k, c]) => html`
            <span class="tl-key"><span class="tl-swatch" style="background:${c}"></span>${k}</span>`)}
          <span class="tl-key">⚠ problems reported</span>
        </div>
      </div>
      <div class="tl-snapshot">
        <div class="tl-snapshot-header">
          <h2>${b.from} → ${b.to}</h2>
          ${b.hasProblems && html`<span class="badge blocked">PROBLEMS</span>`}
          <span class="muted">${b.sprint.replaceAll('_', ' ')}</span>
        </div>
        <${OrgDoc} doc=${b.doc} onNav=${onNav} />
        <${DocLink} doc=${b.doc} />
      </div>
    </div>`;
}

function App() {
  const [index, setIndex] = useState(null);
  const [view, setView] = useState('sprints'); // 'sprints' | 'backlog' | 'timeline'
  const [sprintName, setSprintName] = useState(null);
  const [selected, setSelected] = useState(null); // {story, task|null}
  const [capture, setCapture] = useState(null);
  const [tlSelected, setTlSelected] = useState(null);
  const [showSprint, setShowSprint] = useState(false);
  const [pendingId, setPendingId] = useState(null);
  const [filter, setFilter] = useState('');
  const [epic, setEpic] = useState(null);
  const [error, setError] = useState(null);

  useEffect(() => {
    loadAgileIndex().then(ix => {
      setIndex(ix);
      const first = ix.versions[0]?.sprints[0];
      if (first) setSprintName(first.name);
    }).catch(e => setError(String(e)));
  }, []);

  const model = useMemo(() => {
    if (!index || !sprintName) return null;
    const entry = index.versions
      .flatMap(v => v.sprints).find(s => s.name === sprintName);
    return entry ? loadSprint(entry) : null;
  }, [index, sprintName]);

  const vel = useMemo(() => index ? velocity(index) : [], [index]);
  const buckets = useMemo(
    () => index ? loadBacklog(index.backlog) : [], [index]);
  const tlBuckets = useMemo(
    () => index ? loadTimelineBuckets(index.timeline ?? []) : [], [index]);

  useEffect(() => { setEpic(null); setShowSprint(false); }, [sprintName]);

  /*
   * Cross-document navigation: agile targets switch sprint (or to the
   * backlog view) and open the right drawer; anything else opens its
   * published page.
   */
  const onNav = id => {
    if (!index) return;
    const loc = index.locById.get(id);
    if (loc?.bucket) {
      setView('backlog');
      setPendingId(id);
    } else if (loc) {
      setView('sprints');
      if (loc.sprint !== sprintName) setSprintName(loc.sprint);
      setPendingId(id);
    } else {
      const node = index.byId.get(id);
      if (node?.file) window.open(node.file, '_blank');
    }
  };

  useEffect(() => {
    if (!pendingId) return;
    if (view === 'backlog') {
      for (const b of buckets) {
        const item = b.items.find(i => i.doc.id === pendingId);
        if (item) { setCapture(item); break; }
      }
      setPendingId(null);
      return;
    }
    if (!model) return;
    for (const story of model.stories) {
      if (story.doc.id === pendingId) {
        setSelected({ story, task: null });
        setPendingId(null);
        return;
      }
      const task = story.tasks.find(t => t.doc.id === pendingId);
      if (task) {
        setSelected({ story, task });
        setPendingId(null);
        return;
      }
    }
    if (model.sprint && model.sprint.id === pendingId) setShowSprint(true);
    setPendingId(null);
  }, [pendingId, model, view, buckets]);

  if (error) return html`<div class="error">${error}</div>`;
  if (!index) return html`<div class="loading">Loading graph data…</div>`;

  const sprints = index.versions.flatMap(v =>
    v.sprints.map(s => ({ ...s, version: v.name })));

  return html`
    <div class="app" onClick=${() => { setSelected(null); setCapture(null); setShowSprint(false); }}>
      <div class="toolbar" onClick=${e => e.stopPropagation()}>
        <div class="view-toggle">
          <button class=${view === 'sprints' ? 'active' : ''}
                  onClick=${() => setView('sprints')}>Sprints</button>
          <button class=${view === 'backlog' ? 'active' : ''}
                  onClick=${() => setView('backlog')}>Backlog</button>
          <button class=${view === 'timeline' ? 'active' : ''}
                  onClick=${() => setView('timeline')}>Timeline</button>
        </div>
        ${view === 'sprints' && html`
          <select value=${sprintName} onChange=${e => setSprintName(e.target.value)}>
            ${sprints.map(s => html`
              <option value=${s.name}>${s.version} / ${s.name.replace('_', ' ')}</option>`)}
          </select>`}
        <input type="search" placeholder="filter…"
               value=${filter} onInput=${e => setFilter(e.target.value)} />
      </div>
      ${view === 'sprints' && model && html`
        <${SprintHeader} sprint=${model.sprint} onOpen=${() => setShowSprint(true)} />
        <${Tiles} stories=${model.stories} />
        <${Panels} model=${model} vel=${vel} />
        <${EpicChips} stories=${model.stories} epic=${epic} onPick=${setEpic} />
        <${Board} stories=${model.stories} filter=${filter} epic=${epic}
                  onSelect=${story => setSelected({ story, task: null })} />
      `}
      ${view === 'backlog' && html`
        <${BacklogBoard} buckets=${buckets} filter=${filter} onSelect=${setCapture} />
      `}
      ${view === 'timeline' && html`
        <${TimelineView} buckets=${tlBuckets}
                         selected=${tlSelected ?? Math.max(0, tlBuckets.length - 1)}
                         onPick=${setTlSelected} onNav=${onNav} />
      `}
      ${selected && view === 'sprints' && html`
        <${StoryDetail} story=${selected.story} task=${selected.task}
                        onSelectTask=${t => setSelected({ ...selected, task: t })}
                        onClose=${() => setSelected(null)} onNav=${onNav} />`}
      ${showSprint && view === 'sprints' && model?.sprint && html`
        <${SprintDetail} sprint=${model.sprint} onClose=${() => setShowSprint(false)} onNav=${onNav} />`}
      ${capture && view === 'backlog' && html`
        <${CaptureDetail} item=${capture} onClose=${() => setCapture(null)} onNav=${onNav} />`}
    </div>`;
}

render(html`<${App} />`, document.getElementById('root'));
