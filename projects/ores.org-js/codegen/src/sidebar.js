/*
 * Sidebar panel for the codegen model dashboard.
 *
 * Shows type badge, title, description, and link chips for any node.
 * For template org files adds a collapsible Mustache source pane.
 */
import { isTemplate, extractMustacheSource, escHtml } from './data.js';

const SITE_ROOT = '/OreStudio/';

export function renderSidebar(el, node, byId) {
  if (!node) {
    el.classList.add('hidden');
    el.innerHTML = '';
    return;
  }
  el.classList.remove('hidden');

  const siteUrl = node.file
    ? SITE_ROOT + node.file.replace(/\.org$/, '.html')
    : null;

  const shortType = node.type.replace('ores.codegen.', '');

  let html = `
    <div>
      <span class="sb-type-badge" style="background:${escHtml(node.color)}">${escHtml(shortType)}</span>
    </div>
    <div class="sb-title">${escHtml(node.title)}</div>
  `;

  if (node.description) {
    html += `<div class="sb-description">${escHtml(node.description)}</div>`;
  }

  if (siteUrl) {
    html += `<a class="sb-view-link" href="${escHtml(siteUrl)}" target="_blank">View page ↗</a>`;
  }

  // Outgoing + incoming link chips
  const outgoing = collectLinks(node.content, byId);
  if (outgoing.length) {
    html += `<div class="sb-section-label">Links to</div><div class="sb-chips">`;
    for (const linked of outgoing) {
      const cls = isTemplate(linked) ? ' template' : '';
      html += `<span class="sb-chip${cls}" data-id="${escHtml(linked.id)}">${escHtml(shorten(linked.title))}</span>`;
    }
    html += `</div>`;
  }

  // Mustache source pane for template nodes
  if (isTemplate(node)) {
    const src = extractMustacheSource(node.content);
    if (src) {
      html += `
        <details class="mustache-source">
          <summary>Mustache source</summary>
          <pre>${escHtml(src)}</pre>
        </details>
      `;
    }
  }

  el.innerHTML = html;

  // Wire chip clicks
  el.querySelectorAll('.sb-chip[data-id]').forEach(chip => {
    chip.addEventListener('click', () => {
      chip.dispatchEvent(new CustomEvent('node-navigate', {
        bubbles: true,
        detail: { id: chip.dataset.id },
      }));
    });
  });
}

function shorten(title) {
  const t = title.replace(/^(?:Task|Story|Sprint):\s*/i, '');
  return t.length > 28 ? t.slice(0, 26) + '…' : t;
}

/** Collect codegen nodes linked from this node's content via [[id:UUID]] refs. */
function collectLinks(content, byId) {
  if (!content) return [];
  const linked = [];
  const seen = new Set();
  const re = /\[\[id:([0-9A-Fa-f-]{36})\]/g;
  let m;
  while ((m = re.exec(content)) !== null) {
    const id = m[1].toUpperCase();
    if (seen.has(id)) continue;
    seen.add(id);
    const n = byId.get(id);
    if (n) linked.push(n);
  }
  return linked;
}
