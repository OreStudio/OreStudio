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
import { loadAgileIndex, loadSprint, velocity, burnup } from './data.js';
import { section, fieldTable, linkText } from './orgparse.js';
import { BarChart, LineChart } from './charts.js';

const html = htm.bind(h);

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
    { label: 'sprint progress', value: `${pct}%`, cls: pct >= 70 ? 'done' : pct >= 30 ? 'started' : 'blocked' },
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

function StoryCard({ story, onSelect }) {
  return html`
    <div class="card" onClick=${() => onSelect(story)}>
      <div class="card-title">${story.title}</div>
      ${story.theme && html`<div class="card-theme">${story.theme}</div>`}
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

function DocLink({ doc }) {
  if (!doc.url) return null;
  return html`<a class="doc-link" href=${doc.url} target="_blank">open doc ↗</a>`;
}

function TaskDetail({ task, onBack, onNav }) {
  const doc = task.doc;
  const fields = fieldTable(doc, 'Status');
  return html`
    <div class="detail">
      <button class="back" onClick=${onBack}>← back to story</button>
      <div class="detail-header">
        <${Badge} state=${task.state} />
        <h2>${task.title}</h2>
      </div>
      <p class="muted">${doc.keywords.description || ''}</p>
      <table class="fields"><${FieldRows} fields=${fields} skip=${['state']} onNav=${onNav} /></table>
      <${ProseSection} doc=${doc} title="Goal" onNav=${onNav} />
      <${ProseSection} doc=${doc} title="Acceptance" onNav=${onNav} />
      <${ProseSection} doc=${doc} title="Plan" onNav=${onNav} />
      <${ProseSection} doc=${doc} title="Notes" onNav=${onNav} />
      <${ProseSection} doc=${doc} title="Result" onNav=${onNav} />
      <${DocLink} doc=${doc} />
    </div>`;
}

function StoryDetail({ story, task, onSelectTask, onClose, onNav }) {
  if (task) return html`
    <div class="drawer" onClick=${e => e.stopPropagation()}>
      <button class="close" onClick=${onClose}>✕</button>
      <${TaskDetail} task=${task} onBack=${() => onSelectTask(null)} onNav=${onNav} />
    </div>`;

  const doc = story.doc;
  const fields = fieldTable(doc, 'Status');
  return html`
    <div class="drawer" onClick=${e => e.stopPropagation()}>
      <button class="close" onClick=${onClose}>✕</button>
      <div class="detail">
        <div class="detail-header">
          <${Badge} state=${story.state} />
          <h2>${story.title}</h2>
        </div>
        ${story.theme && html`<p class="muted">${story.theme}</p>`}
        <p class="muted">${doc.keywords.description || ''}</p>
        <table class="fields"><${FieldRows} fields=${fields} skip=${['state']} onNav=${onNav} /></table>
        <${ProseSection} doc=${doc} title="Goal" onNav=${onNav} />
        <${ProseSection} doc=${doc} title="Acceptance" onNav=${onNav} />
        <h3>Tasks</h3>
        <ul class="task-list">
          ${story.tasks.map(t => html`
            <li class="task-item" onClick=${() => onSelectTask(t)}>
              <${Badge} state=${t.state} />
              <span>${t.title}</span>
            </li>`)}
        </ul>
        <${ProseSection} doc=${doc} title="Decisions" onNav=${onNav} />
        <${ProseSection} doc=${doc} title="Out of scope" onNav=${onNav} />
        <${DocLink} doc=${doc} />
      </div>
    </div>`;
}

function SprintHeader({ sprint, onNav }) {
  if (!sprint) return null;
  const mission = section(sprint, 'Mission');
  const fields = fieldTable(sprint, 'Status');
  return html`
    <div class="sprint-header">
      <h1>${linkText(sprint.keywords.title || '')}</h1>
      ${mission && html`<p class="mission"><${OrgText} text=${mission.prose.join(' ')} onNav=${onNav} /></p>`}
      <p class="muted">
        ${fields['start'] ? `started ${linkText(fields['start'])}` : ''}
        ${fields['now'] ? ` — ${linkText(fields['now'])}` : ''}
      </p>
    </div>`;
}

function Board({ stories, filter, epic, onSelect }) {
  const visible = useMemo(() => {
    let v = stories;
    if (epic) v = v.filter(s => (s.theme || '(no epic)') === epic);
    const f = filter.trim().toLowerCase();
    if (f) v = v.filter(s =>
      s.title.toLowerCase().includes(f) ||
      s.theme.toLowerCase().includes(f) ||
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

function App() {
  const [index, setIndex] = useState(null);
  const [sprintName, setSprintName] = useState(null);
  const [selected, setSelected] = useState(null); // {story, task|null}
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

  useEffect(() => { setEpic(null); }, [sprintName]);

  /*
   * Cross-document navigation: agile targets switch sprint and open
   * the right drawer; anything else opens its published page.
   */
  const onNav = id => {
    if (!index) return;
    const loc = index.locById.get(id);
    if (loc) {
      if (loc.sprint !== sprintName) setSprintName(loc.sprint);
      setPendingId(id);
    } else {
      const node = index.byId.get(id);
      if (node?.file) window.open(node.file, '_blank');
    }
  };

  useEffect(() => {
    if (!pendingId || !model) return;
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
    setPendingId(null); // sprint doc or unknown: just land on the sprint
  }, [pendingId, model]);

  if (error) return html`<div class="error">${error}</div>`;
  if (!index) return html`<div class="loading">Loading graph data…</div>`;

  const sprints = index.versions.flatMap(v =>
    v.sprints.map(s => ({ ...s, version: v.name })));

  return html`
    <div class="app" onClick=${() => setSelected(null)}>
      <div class="toolbar" onClick=${e => e.stopPropagation()}>
        <select value=${sprintName} onChange=${e => setSprintName(e.target.value)}>
          ${sprints.map(s => html`
            <option value=${s.name}>${s.version} / ${s.name.replace('_', ' ')}</option>`)}
        </select>
        <input type="search" placeholder="filter stories and tasks…"
               value=${filter} onInput=${e => setFilter(e.target.value)} />
      </div>
      ${model && html`
        <${SprintHeader} sprint=${model.sprint} onNav=${onNav} />
        <${Tiles} stories=${model.stories} />
        <${Panels} model=${model} vel=${vel} />
        <${EpicChips} stories=${model.stories} epic=${epic} onPick=${setEpic} />
        <${Board} stories=${model.stories} filter=${filter} epic=${epic}
                  onSelect=${story => setSelected({ story, task: null })} />
      `}
      ${selected && html`
        <${StoryDetail} story=${selected.story} task=${selected.task}
                        onSelectTask=${t => setSelected({ ...selected, task: t })}
                        onClose=${() => setSelected(null)} onNav=${onNav} />`}
    </div>`;
}

render(html`<${App} />`, document.getElementById('root'));
