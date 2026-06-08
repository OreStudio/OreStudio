/*
 * Data loading for the agile board.
 *
 * Single source: graphdata.json, the org-roam database export that
 * also drives the knowledge-graph app (see ores-org-roam-export.el).
 * Each node carries the full raw org source in `content`, so the
 * agile model is built by filtering the graph to the agile subtree
 * and parsing each node's org text client-side.
 */
import { parseOrg, fieldTable, linkText, idLink, section } from './orgparse.js';

/**
 * The graph export lives next door in the graph app, both locally
 * (projects/ores.org-js/graph/) and deployed (/OreStudio/graph/).
 * Overridable with ?data=<url> for unusual layouts — trusted sources
 * only: the fetched org content renders via dangerouslySetInnerHTML
 * unsanitised (see OrgDoc in app.js).
 */
export function graphDataUrl() {
  return new URLSearchParams(location.search).get('data')
    ?? '../graph/graphdata.json';
}

const AGILE_RE = /doc\/agile\/versions\/([^/]+)\/(sprint_\d+)\//;
const TIMELINE_RE = /doc\/agile\/versions\/[^/]+\/(sprint_\d+)\/timeline\/([^/]+)\.html$/;
const BACKLOG_RE = /doc\/agile\/product_backlog\/(inbox|next|deferred|discarded)\//;
export const BUCKETS = ['inbox', 'next', 'deferred', 'discarded'];

/**
 * Fetch the graph export and group file-level agile nodes into
 * versions and sprints: { versions: [{name, sprints: [{name, nodes}]}],
 * byId, locById }. byId maps every node id (agile or not) to its node,
 * for cross-document navigation; locById maps agile node ids to their
 * {version, sprint}. Nodes are parsed lazily, per sprint, by loadSprint.
 */
export async function loadAgileIndex() {
  const res = await fetch(graphDataUrl());
  if (!res.ok) throw new Error(`graphdata fetch failed: ${res.status}`);
  const graph = await res.json();

  const versions = new Map();
  const timeline = [];
  const backlog = new Map(BUCKETS.map(b => [b, []]));
  const byId = new Map();
  const byUrl = new Map();
  const locById = new Map();
  for (const node of graph.nodes) {
    if (node.id) byId.set(node.id.toUpperCase(), node);
    if (node.level === 0 && node.file) byUrl.set(node.file, node);
    if (node.level !== 0 || !node.content) continue;
    const tl = (node.file || '').match(TIMELINE_RE);
    if (tl) {
      timeline.push({ node, sprint: tl[1], name: tl[2] });
      continue;
    }
    const b = (node.file || '').match(BACKLOG_RE);
    if (b) {
      const bucket = b[1];
      if (node.id) locById.set(node.id.toUpperCase(), { bucket });
      backlog.get(bucket).push(node);
      continue;
    }
    const m = (node.file || '').match(AGILE_RE);
    if (!m) continue;
    const [, version, sprint] = m;
    if (node.id) locById.set(node.id.toUpperCase(), { version, sprint });
    if (!versions.has(version)) versions.set(version, new Map());
    const sprints = versions.get(version);
    if (!sprints.has(sprint)) sprints.set(sprint, []);
    sprints.get(sprint).push(node);
  }

  const out = [];
  for (const [name, sprints] of [...versions.entries()].sort()) {
    const ss = [...sprints.entries()]
      .sort((a, b) => b[0].localeCompare(a[0], undefined, { numeric: true }))
      .map(([sname, nodes]) => ({ name: sname, nodes }));
    out.push({ name, sprints: ss });
  }
  timeline.sort((a, b) => a.name.localeCompare(b.name));
  return { versions: out, timeline, backlog, byId, byUrl, locById };
}

/**
 * Parse the backlog buckets into:
 *   [{name, items: [{doc, title, description}]}]
 */
export function loadBacklog(backlog) {
  return BUCKETS.map(name => ({
    name,
    items: (backlog.get(name) || [])
      .map(node => {
        const doc = parseOrg(node.content);
        doc.url = node.file;
        doc.raw = node.content;
        return {
          doc,
          title: linkText(doc.keywords.title || ''),
          description: doc.keywords.description || '',
        };
      })
      .sort((a, b) => a.title.localeCompare(b.title)),
  }));
}

/** State of a story/task doc: the Status table's State row. */
function docState(doc) {
  const f = fieldTable(doc, 'Status');
  return (f['state'] || '').trim().toUpperCase() || 'UNKNOWN';
}

function stripPrefix(title, prefix) {
  return title.startsWith(prefix) ? title.slice(prefix.length).trim() : title;
}

/**
 * Parse one sprint's nodes and assemble the model:
 *   { sprint, stories: [{doc, state, title, theme, url, tasks}] }
 */
export function loadSprint(sprintEntry) {
  let sprintDoc = null;
  const storiesByDir = new Map();
  const tasksByDir = new Map();

  for (const node of sprintEntry.nodes) {
    const doc = parseOrg(node.content);
    doc.url = node.file; // site HTML page for this doc
    doc.raw = node.content;
    const type = (doc.keywords.type || '').toLowerCase();
    const dir = (node.file || '').replace(/\/[^/]*$/, '');
    if (type === 'sprint') {
      sprintDoc = doc;
    } else if (type === 'story') {
      storiesByDir.set(dir, doc);
    } else if (type === 'task') {
      if (!tasksByDir.has(dir)) tasksByDir.set(dir, []);
      tasksByDir.get(dir).push(doc);
    }
  }

  const stories = [];
  for (const [dir, doc] of storiesByDir) {
    const tasks = (tasksByDir.get(dir) || [])
      .map(t => ({
        doc: t,
        title: stripPrefix(linkText(t.keywords.title || ''), 'Task:'),
        state: docState(t),
        waiting: linkText(fieldTable(t, 'Status')['waiting on']
                          || t.keywords.blocked_on || ''),
      }))
      .sort((a, b) => a.title.localeCompare(b.title));
    stories.push({
      doc,
      dir,
      title: stripPrefix(linkText(doc.keywords.title || ''), 'Story:'),
      state: docState(doc),
      created: doc.keywords.created || '',
      tasks,
    });
  }
  stories.sort((a, b) => a.title.localeCompare(b.title));

  // Theme/epic per story from the sprint's * Stories subtree tables.
  const themes = new Map(); // story id -> heading path under Stories
  if (sprintDoc) {
    for (const s of sprintDoc.sections) {
      if (!s.path.startsWith('Stories')) continue;
      for (const tbl of s.tables) {
        for (const row of tbl.rows.concat(tbl.header ? [tbl.header] : [])) {
          const id = idLink(row[0] || '');
          if (id) themes.set(id, s.path.replace(/^Stories(\s*\/\s*)?/, ''));
        }
      }
    }
  }
  for (const st of stories) {
    st.theme = themes.get(st.doc.id) || '';
  }

  return { sprint: sprintDoc, stories };
}

/**
 * Cross-sprint velocity: stories DONE per sprint, oldest first.
 * Cheap regex scan over node content — no full parse needed. Assumes
 * the codegen's lowercase `#+type:` keyword; parseOrg itself is
 * case-insensitive, so revisit if the docs ever vary in case.
 */
export function velocity(index) {
  const out = [];
  for (const v of index.versions) {
    for (const s of [...v.sprints].reverse()) {
      let done = 0;
      for (const node of s.nodes) {
        if (!/^#\+type: story$/m.test(node.content)) continue;
        const m = node.content.match(/\|\s*State\s*\|\s*([A-Za-z]+)/);
        if (m && m[1].toUpperCase() === 'DONE') done++;
      }
      out.push({ label: s.name.replace('sprint_', 's'), value: done });
    }
  }
  return out;
}

/**
 * Sprint velocity data: tasks completed per sprint day (1–7).
 * Reads #+start_date and #+end_date from the sprint doc keywords.
 * Returns {days, taskData} where taskData is [{label, value, color}]
 * suitable for BarChart. Days beyond today are rendered at 40% opacity.
 * Returns null when #+start_date is absent.
 */
export function velocityData(model) {
  if (!model.sprint) return null;
  const startDate = model.sprint.keywords.start_date;
  if (!startDate) return null;

  const start = new Date(startDate + 'T00:00:00');
  const today = new Date();

  const days = Array.from({ length: 7 }, (_, i) => {
    const d = new Date(start);
    d.setDate(d.getDate() + i);
    return {
      date: d.toISOString().slice(0, 10),
      label: `D${i + 1}`,
      isFuture: d > today,
    };
  });

  const byDate = new Map();
  for (const story of model.stories) {
    const s = section(story.doc, 'Tasks');
    if (!s) continue;
    for (const tbl of s.tables) {
      for (const row of tbl.rows) {
        const state = (row[1] || '').trim().toUpperCase();
        const end = (row[3] || '').trim();
        if (state === 'DONE' && /^\d{4}-\d{2}-\d{2}$/.test(end))
          byDate.set(end, (byDate.get(end) || 0) + 1);
      }
    }
  }

  const taskData = days.map(d => ({
    label: d.label,
    value: byDate.get(d.date) || 0,
    color: d.isFuture ? 'rgba(88,166,255,0.4)' : 'var(--accent, #58a6ff)',
  }));

  return { days, taskData };
}

/**
 * Burn-up for one sprint: cumulative DONE tasks by End date, read from
 * each story's * Tasks table (columns: task, state, start, end, desc).
 */
export function burnup(model) {
  const ends = [];
  for (const story of model.stories) {
    const s = section(story.doc, 'Tasks');
    if (!s) continue;
    for (const tbl of s.tables) {
      for (const row of tbl.rows) {
        // Columns by position: [0] task, [1] state, [2] start,
        // [3] end, [4] description — fixed by the codegen template.
        const state = (row[1] || '').trim().toUpperCase();
        const end = (row[3] || '').trim();
        if (state === 'DONE' && /^\d{4}-\d{2}-\d{2}$/.test(end)) ends.push(end);
      }
    }
  }
  ends.sort();
  const byDay = new Map();
  for (const d of ends) byDay.set(d, (byDay.get(d) || 0) + 1);
  let cum = 0;
  return [...byDay.entries()].map(([x, n]) => ({ x, y: (cum += n) }));
}


/**
 * Parse timeline snapshot nodes into buckets for the timeline view:
 *   [{doc, name, sprint, from, to, counts, hasProblems}]
 * Window comes from the <ISO-from>-<ISO-to> filename; counts from the
 * snapshot's section tables; hasProblems from a non-empty problems
 * section (ignoring an explicit "None observed").
 */
export function loadTimelineBuckets(timeline) {
  const fmt = w =>
    `${w.slice(0, 4)}-${w.slice(4, 6)}-${w.slice(6, 8)} ` +
    `${w.slice(9, 11)}:${w.slice(11, 13)}`;
  return timeline.map(t => {
    const doc = parseOrg(t.node.content);
    doc.url = t.node.file;
    doc.raw = t.node.content;
    const m = t.name.match(/^(\d{8}T\d{4})-(\d{8}T\d{4})$/);
    const counts = {};
    for (const [key, title] of [
      ['stories', 'Stories'], ['tasks', 'Tasks'],
      ['captures', 'Captures'], ['prs', 'Pull requests']]) {
      const s = section(doc, title);
      counts[key] = s && s.tables.length ? s.tables[0].rows.length : 0;
    }
    const probs = section(doc, 'Problems and suspicious decisions');
    const real = probs
      ? [...probs.items, ...probs.prose]
          .filter(x => !/^none observed/i.test(x.trim()))
      : [];
    return {
      doc,
      name: t.name,
      sprint: t.sprint,
      from: m ? fmt(m[1]) : t.name,
      to: m ? fmt(m[2]) : '',
      // Axis label: end time HH:MM for a well-formed window, else the
      // raw filename so a non-standard snapshot name stays legible.
      label: m ? fmt(m[2]).slice(11) : t.name,
      counts,
      hasProblems: real.length > 0,
    };
  });
}
