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
 * Overridable with ?data=<url> for unusual layouts.
 */
export function graphDataUrl() {
  return new URLSearchParams(location.search).get('data')
    ?? '../graph/graphdata.json';
}

const AGILE_RE = /doc\/agile\/versions\/([^/]+)\/(sprint_\d+)\//;

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
  const byId = new Map();
  const locById = new Map();
  for (const node of graph.nodes) {
    if (node.id) byId.set(node.id.toUpperCase(), node);
    if (node.level !== 0 || !node.content) continue;
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
  return { versions: out, byId, locById };
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
      }))
      .sort((a, b) => a.title.localeCompare(b.title));
    stories.push({
      doc,
      dir,
      title: stripPrefix(linkText(doc.keywords.title || ''), 'Story:'),
      state: docState(doc),
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
 * Cheap regex scan over node content — no full parse needed.
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
