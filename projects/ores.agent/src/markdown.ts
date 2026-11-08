// SPDX-License-Identifier: GPL-3.0-or-later
import MarkdownIt from 'markdown-it';

const markdown = new MarkdownIt({ html: false, breaks: true, linkify: false, typographer: false });
// Replies must not load remote content, including tracking images.
markdown.disable('image');
// Table alignment uses inline styles by default, which our CSP blocks.
markdown.renderer.rules.th_open = () => '<th>';
markdown.renderer.rules.td_open = () => '<td>';

export function renderMarkdown(answer: string): string {
    return markdown.render(answer);
}

export function markdownText(value: unknown): string {
    return String(value).replace(/\r?\n|\r/g, ' ').replace(/[\\`*{}\[\]()#+.!_|>~<&]/g, '\\$&');
}

export function markdownTable(headers: string[], rows: unknown[][]): string {
    if (!rows.length) return '';
    const row = (cells: unknown[]) => '| ' + cells.map(markdownText).join(' | ') + ' |';
    return [row(headers), row(headers.map(() => '---')), ...rows.map(row)].join('\n');
}
