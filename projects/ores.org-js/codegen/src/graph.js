/*
 * d3-force graph for the codegen model dashboard.
 *
 * Renders nodes as coloured circles and links as grey lines.
 * Supports pan/zoom, click-to-select, and label toggling.
 */
import * as d3 from 'https://cdn.jsdelivr.net/npm/d3@7/+esm';
import { TYPE_COLOR } from './data.js';

/** Type-cluster target positions (fractional [0,1] → scaled at init). */
const CLUSTER_POS = {
  'ores.codegen.entity':              [0.5, 0.4],
  'ores.codegen.field_group':         [0.55, 0.55],
  'ores.codegen.lookup_entity':       [0.25, 0.4],
  'ores.codegen.table':               [0.2, 0.55],
  'ores.codegen.junction':            [0.5, 0.65],
  'ores.codegen.service_registry':    [0.75, 0.7],
  'ores.codegen.module':              [0.75, 0.25],
  'ores.codegen.facet_catalogue':     [0.3, 0.75],
  'ores.codegen.component_catalogue': [0.5, 0.8],
  'ores.codegen.modeline_catalogue':  [0.7, 0.8],
};

const NODE_RADIUS = 7;
const CLUSTER_STRENGTH = 0.08;

export function initGraph(svgEl, nodes, links, { onSelect, showLabels }) {
  const width = svgEl.clientWidth || 900;
  const height = svgEl.clientHeight || 600;

  const svg = d3.select(svgEl);
  svg.selectAll('*').remove();

  const root = svg.append('g').attr('class', 'root');

  svg.call(
    d3.zoom()
      .scaleExtent([0.2, 4])
      .on('zoom', e => root.attr('transform', e.transform))
  );

  // Deep-copy nodes/links so d3 can mutate them
  const simNodes = nodes.map(n => ({ ...n }));
  const idToSim = new Map(simNodes.map(n => [n.id, n]));
  const simLinks = links
    .map(l => ({ source: idToSim.get(l.source), target: idToSim.get(l.target) }))
    .filter(l => l.source && l.target);

  // Seed positions by cluster to reduce non-determinism
  for (const n of simNodes) {
    const [fx, fy] = CLUSTER_POS[n.type] || [0.5, 0.5];
    n.x = fx * width + (Math.random() - 0.5) * 60;
    n.y = fy * height + (Math.random() - 0.5) * 60;
  }

  const simulation = d3.forceSimulation(simNodes)
    .force('link', d3.forceLink(simLinks).id(n => n.id).distance(50).strength(0.4))
    .force('charge', d3.forceManyBody().strength(-180))
    .force('collide', d3.forceCollide(NODE_RADIUS + 3))
    .force('clusterX', d3.forceX(n => (CLUSTER_POS[n.type] || [0.5])[0] * width).strength(CLUSTER_STRENGTH))
    .force('clusterY', d3.forceY(n => (CLUSTER_POS[n.type] || [0.5, 0.5])[1] * height).strength(CLUSTER_STRENGTH));

  const link = root.append('g')
    .selectAll('line')
    .data(simLinks)
    .join('line')
    .attr('class', 'link');

  const node = root.append('g')
    .selectAll('g')
    .data(simNodes)
    .join('g')
    .attr('class', 'node')
    .call(drag(simulation))
    .on('click', (e, d) => {
      e.stopPropagation();
      root.selectAll('.node').classed('selected', false);
      d3.select(e.currentTarget).classed('selected', true);
      onSelect(d);
    });

  node.append('circle')
    .attr('r', NODE_RADIUS)
    .attr('fill', n => n.color);

  const labels = node.append('text')
    .attr('dy', '0.35em')
    .attr('dx', NODE_RADIUS + 3)
    .text(n => shorten(n.title));

  // Click on blank SVG → deselect
  svg.on('click', () => {
    root.selectAll('.node').classed('selected', false);
    onSelect(null);
  });

  simulation.on('tick', () => {
    link
      .attr('x1', l => l.source.x)
      .attr('y1', l => l.source.y)
      .attr('x2', l => l.target.x)
      .attr('y2', l => l.target.y);
    node.attr('transform', n => `translate(${n.x},${n.y})`);
  });

  return {
    setLabels(visible) {
      labels.attr('display', visible ? null : 'none');
    },
    selectById(id) {
      root.selectAll('.node').classed('selected', n => n.id === id);
    },
  };
}

function drag(simulation) {
  return d3.drag()
    .on('start', (e, d) => {
      if (!e.active) simulation.alphaTarget(0.3).restart();
      d.fx = d.x; d.fy = d.y;
    })
    .on('drag', (e, d) => { d.fx = e.x; d.fy = e.y; })
    .on('end', (e, d) => {
      if (!e.active) simulation.alphaTarget(0);
      d.fx = null; d.fy = null;
    });
}

function shorten(title) {
  const t = title.replace(/^(?:Task|Story|Sprint):\s*/i, '');
  return t.length > 30 ? t.slice(0, 28) + '…' : t;
}
