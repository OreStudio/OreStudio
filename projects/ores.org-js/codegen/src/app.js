/*
 * Entry point for the Codegen Model Dashboard.
 *
 * Wires data loading, graph rendering, legend, sidebar, and toolbar.
 */
import { loadCodegenGraph, TYPE_COLOR, TYPE_LABEL } from './data.js';
import { initGraph } from './graph.js';
import { renderSidebar } from './sidebar.js';

const statsEl   = document.getElementById('stats');
const legendEl  = document.getElementById('legend');
const sidebarEl = document.getElementById('sidebar');
const svgEl     = document.getElementById('graph');
const labelsChk = document.getElementById('toggle-labels');

let graphCtrl = null;
let codegenGraph = null;

async function main() {
  statsEl.textContent = 'Loading…';

  try {
    codegenGraph = await loadCodegenGraph();
  } catch (err) {
    statsEl.textContent = `Error: ${err.message}`;
    return;
  }

  const { nodes, links, byId } = codegenGraph;
  statsEl.textContent = `${nodes.length} nodes · ${links.length} links`;

  renderLegend(legendEl);

  graphCtrl = initGraph(svgEl, nodes, links, {
    showLabels: labelsChk.checked,
    onSelect(node) {
      renderSidebar(sidebarEl, node, byId);
    },
  });

  labelsChk.addEventListener('change', () => {
    graphCtrl.setLabels(labelsChk.checked);
  });

  // Sidebar chip navigation
  document.addEventListener('node-navigate', e => {
    const node = byId.get(e.detail.id);
    if (!node) return;
    renderSidebar(sidebarEl, node, byId);
    graphCtrl.selectById(node.id);
  });
}

function renderLegend(el) {
  el.innerHTML = '';
  const seen = new Set();
  for (const [type, label] of Object.entries(TYPE_LABEL)) {
    if (!label) continue; // grouped (component_catalogue, modeline_catalogue)
    if (seen.has(label)) continue;
    seen.add(label);
    const color = TYPE_COLOR[type];
    const item = document.createElement('div');
    item.className = 'legend-item';
    item.innerHTML = `<span class="legend-dot" style="background:${color}"></span>${label}`;
    el.appendChild(item);
  }
}

main();
