/** -*- mode: typescript-ts-mode; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 *
 */

import type { Element, ElementContent, Root, RootContent } from 'hast';
import rehypeSanitize, { defaultSchema, type Options as SanitizeSchema } from 'rehype-sanitize';
import rehypeStringify from 'rehype-stringify';
import { unified } from 'unified';
import uniorgParse from 'uniorg-parse';
import uniorg2rehype from 'uniorg-rehype';

export interface RenderOptions {
    /** Where an id: link points. The default is a fragment, #org-id-<ID>. */
    readonly idHref?: (id: string) => string;
    /** When set, a bare #1234 in prose links to this repository's issue 1234. */
    readonly githubRepo?: string;
}

type Node = Root | RootContent;

const SAFE_SCHEMA: SanitizeSchema = {
    ...defaultSchema,
    attributes: {
        ...defaultSchema.attributes,
        a: [...(defaultSchema.attributes?.['a'] ?? []), 'dataOrgId'],
    },
};

function isContent(node: RootContent): node is ElementContent {
    return node.type !== 'doctype';
}

function isElement(node: Node): node is Element {
    return node.type === 'element';
}

function walk(node: Node, visit: (element: Element) => void): void {
    if (isElement(node)) visit(node);
    if ('children' in node) {
        for (const child of node.children) walk(child, visit);
    }
}

/** Point id: links at the reader's route and make file: links relative. */
function rewriteLinks(tree: Root, idHref: (id: string) => string): void {
    walk(tree, (element) => {
        for (const key of ['href', 'src'] as const) {
            const value = element.properties[key];
            if (typeof value !== 'string') continue;
            if (key === 'href' && value.toLowerCase().startsWith('id:')) {
                const id = value.slice(3).toUpperCase();
                element.properties[key] = idHref(id);
                element.properties['dataOrgId'] = id;
            } else if (value.toLowerCase().startsWith('file:')) {
                const target = value.slice(5);
                // A leading double slash would name another host.
                if (/^[\\/]{2}/.test(target)) delete element.properties[key];
                else element.properties[key] = target;
            }
        }
    });
}

function linkIssues(tree: Root, repo: string): void {
    const pattern = /#(\d{3,6})\b/g;
    const convert = (children: ElementContent[]): ElementContent[] => {
        const out: ElementContent[] = [];
        for (const child of children) {
            if (child.type === 'text') {
                let last = 0;
                for (const match of child.value.matchAll(pattern)) {
                    if (match.index > last) {
                        out.push({ type: 'text', value: child.value.slice(last, match.index) });
                    }
                    out.push({
                        type: 'element',
                        tagName: 'a',
                        properties: { href: `${repo}/issues/${match[1] ?? ''}` },
                        children: [{ type: 'text', value: match[0] }],
                    });
                    last = match.index + match[0].length;
                }
                out.push(last === 0 ? child : { type: 'text', value: child.value.slice(last) });
            } else if (
                child.type === 'element' &&
                child.tagName !== 'a' &&
                child.tagName !== 'code' &&
                child.tagName !== 'pre'
            ) {
                child.children = convert(child.children);
                out.push(child);
            } else {
                out.push(child);
            }
        }
        return out;
    };
    tree.children = convert(tree.children.filter(isContent));
}

/**
 * The org parser slows sharply on a paragraph that is full of emphasis
 * markers, and a deep nest of them overflows its stack. A real paragraph has
 * a few markers. A paragraph past the limit shows as fixed-width text, which
 * the parser leaves alone. Lines in a source or example block hold code and
 * are not counted, because the parser reads them as plain text.
 */
const MAX_MARKERS_PER_PARAGRAPH = 300;
const MAX_SOURCE_LENGTH = 2_000_000;
const MARKER = /[*/_=~+]|\[\[/g;
const RAW_BLOCK_BEGIN = /^\s*#\+begin_(?:src|example|export|comment)\b/i;
const RAW_BLOCK_END = /^\s*#\+end_(?:src|example|export|comment)\b/i;

function guardParagraphs(source: string): string {
    const lines = source.split('\n');
    const out: string[] = [];
    let paragraph: string[] = [];
    let markers = 0;
    let inRawBlock = false;
    const flush = () => {
        const over = markers > MAX_MARKERS_PER_PARAGRAPH;
        for (const line of paragraph) out.push(over ? `: ${line}` : line);
        paragraph = [];
        markers = 0;
    };
    for (const line of lines) {
        if (inRawBlock) {
            out.push(line);
            if (RAW_BLOCK_END.test(line)) inRawBlock = false;
            continue;
        }
        if (RAW_BLOCK_BEGIN.test(line)) {
            flush();
            out.push(line);
            inRawBlock = true;
            continue;
        }
        if (line.trim() === '') {
            flush();
            out.push(line);
            continue;
        }
        paragraph.push(line);
        markers += (line.match(MARKER) ?? []).length;
    }
    flush();
    return out.join('\n');
}

function escapeHtml(text: string): string {
    return text
        .replace(/&/g, '&amp;')
        .replace(/</g, '&lt;')
        .replace(/>/g, '&gt;')
        .replace(/"/g, '&quot;');
}

const HEADER_KEYWORD = /^#\+[A-Za-z_][A-Za-z0-9_-]*:/;

/**
 * Take away the file-level drawer and the #+keyword lines that open the
 * document, which a page shows elsewhere. A keyword line later in the text,
 * or inside a block, stays.
 */
function stripHeader(source: string): string {
    const text = source
        .replace(/^\uFEFF/, '')
        .replace(/^:PROPERTIES:\r?\n(?:.*\r?\n)*?:END:\r?\n/, '');
    const lines = text.split('\n');
    let at = 0;
    while (at < lines.length) {
        const line = lines[at] ?? '';
        if (!HEADER_KEYWORD.test(line) && line.trim() !== '') break;
        at += 1;
    }
    const head = lines.slice(0, at).filter((line) => !HEADER_KEYWORD.test(line));
    return [...head, ...lines.slice(at)].join('\n');
}

/**
 * Render org text as HTML that is safe to place in a page. Raw HTML in the
 * source shows as text, and a link or an attribute that could run script is
 * removed.
 */
export function renderOrgHtml(source: string, options: RenderOptions = {}): string {
    if (source.length > MAX_SOURCE_LENGTH) {
        return `<pre>${escapeHtml(source.slice(0, MAX_SOURCE_LENGTH))}</pre><p>This text is cut at ${MAX_SOURCE_LENGTH} characters.</p>`;
    }
    const idHref = options.idHref ?? ((id: string) => `#org-id-${id}`);
    const repo = options.githubRepo;
    try {
        const processor = unified()
            .use(uniorgParse)
            .use(uniorg2rehype)
            .use(() => (tree: Root) => {
                rewriteLinks(tree, idHref);
                if (repo !== undefined) linkIssues(tree, repo);
            })
            .use(rehypeSanitize, SAFE_SCHEMA)
            .use(rehypeStringify);
        return String(processor.processSync(guardParagraphs(stripHeader(source))));
    } catch {
        return `<pre>${escapeHtml(source)}</pre>`;
    }
}
