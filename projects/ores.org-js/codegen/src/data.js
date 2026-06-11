/*
 * Data loading for the codegen model dashboard.
 *
 * Reads graphdata.json (the shared org-roam export), filters to the
 * 10 MASD codegen types, and returns nodes + links ready for d3-force.
 */

export function graphDataUrl() {
  return new URLSearchParams(location.search).get('data')
    ?? '../graph/graphdata.json';
}

const TYPE_RE = /^#\+type:\s*(\S+)/m;
const TEMPLATE_PATH = 'projects/ores.codegen/library/templates/';

export const CODEGEN_TYPES = new Set([
  'ores.codegen.entity',
  'ores.codegen.lookup_entity',
  'ores.codegen.field_group',
  'ores.codegen.table',
  'ores.codegen.junction',
  'ores.codegen.service_registry',
  'ores.codegen.module',
  'ores.codegen.facet_catalogue',
  'ores.codegen.component_catalogue',
  'ores.codegen.modeline_catalogue',
]);

/** OMC-based colour mapping (from story Decisions section). */
export const TYPE_COLOR = {
  'ores.codegen.entity':              '#2da44e',
  'ores.codegen.field_group':         '#1abc9c',
  'ores.codegen.lookup_entity':       '#0075ca',
  'ores.codegen.table':               '#4a90d9',
  'ores.codegen.junction':            '#e05c8a',
  'ores.codegen.service_registry':    '#d93b00',
  'ores.codegen.module':              '#8b949e',
  'ores.codegen.facet_catalogue':     '#7c3aed',
  'ores.codegen.component_catalogue': '#7c3aed',
  'ores.codegen.modeline_catalogue':  '#7c3aed',
};

/** Human-readable short labels for the legend. */
export const TYPE_LABEL = {
  'ores.codegen.entity':              'entity',
  'ores.codegen.field_group':         'field_group',
  'ores.codegen.lookup_entity':       'lookup_entity',
  'ores.codegen.table':               'table',
  'ores.codegen.junction':            'junction',
  'ores.codegen.service_registry':    'service_registry',
  'ores.codegen.module':              'module',
  'ores.codegen.facet_catalogue':     'generator config ×3',
  'ores.codegen.component_catalogue': null, // grouped with facet_catalogue in legend
  'ores.codegen.modeline_catalogue':  null,
};

/**
 * Fetch graphdata.json and return codegen model:
 *   { nodes: [{id, title, type, color, file, description, content}],
 *     links: [{source, target}],
 *     byId:  Map<id, node> }
 */
export async function loadCodegenGraph() {
  const res = await fetch(graphDataUrl());
  if (!res.ok) throw new Error(`graphdata fetch failed: ${res.status}`);
  const graph = await res.json();

  const nodes = [];
  const byId = new Map();

  for (const n of graph.nodes) {
    if (n.level !== 0 || !n.content) continue;
    const m = n.content.match(TYPE_RE);
    if (!m) continue;
    const type = m[1];
    if (!CODEGEN_TYPES.has(type)) continue;

    const description = (n.content.match(/^#\+description:\s*(.+)/m) || [])[1] || '';
    const node = {
      id: (n.id || '').toUpperCase(),
      title: (n.content.match(/^#\+title:\s*(.+)/m) || [])[1] || n.title || '',
      type,
      color: TYPE_COLOR[type] || '#8b949e',
      file: n.file || '',
      description,
      content: n.content,
    };
    nodes.push(node);
    if (node.id) byId.set(node.id, node);
  }

  const nodeIds = new Set(nodes.map(n => n.id));
  const links = graph.links
    .filter(l => {
      const s = (l.source || '').toUpperCase();
      const t = (l.target || '').toUpperCase();
      return nodeIds.has(s) && nodeIds.has(t);
    })
    .map(l => ({ source: l.source.toUpperCase(), target: l.target.toUpperCase() }));

  return { nodes, links, byId };
}

/** True if the node is a literate Mustache template file. */
export function isTemplate(node) {
  return node.file.includes(TEMPLATE_PATH);
}

/** Extract raw Mustache source blocks from a node's content field. */
export function extractMustacheSource(content) {
  const blocks = [];
  const re = /^#\+begin_src mustache\s*\n([\s\S]*?)^#\+end_src/gm;
  let m;
  while ((m = re.exec(content)) !== null) blocks.push(m[1]);
  return blocks.join('\n---\n');
}

/**
 * Escape a string for safe insertion as text content.
 * Use on any user-data before putting it in the DOM.
 */
export function escHtml(str) {
  return str
    .replace(/&/g, '&amp;')
    .replace(/</g, '&lt;')
    .replace(/>/g, '&gt;')
    .replace(/"/g, '&quot;');
}
