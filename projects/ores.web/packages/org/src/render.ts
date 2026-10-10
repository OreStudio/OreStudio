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
                element.properties[key] = value.slice(5);
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
 * The org parser slows sharply on a line that is full of emphasis markers,
 * and a deep nest of them overflows its stack. A real line has a few markers.
 * A line past the limit shows as fixed-width text, which the parser leaves
 * alone.
 */
const MAX_MARKERS_PER_LINE = 300;
const MAX_SOURCE_LENGTH = 2_000_000;

function guardLines(source: string): string {
    return source
        .split('\n')
        .map((line) => {
            if (line.length < MAX_MARKERS_PER_LINE) return line;
            const markers = (line.match(/[*/_=~+]|\[\[/g) ?? []).length;
            return markers > MAX_MARKERS_PER_LINE ? `: ${line}` : line;
        })
        .join('\n');
}

function escapeHtml(text: string): string {
    return text
        .replace(/&/g, '&amp;')
        .replace(/</g, '&lt;')
        .replace(/>/g, '&gt;')
        .replace(/"/g, '&quot;');
}

/** Take away the file-level drawer and the #+keyword lines, which a page shows elsewhere. */
function stripHeader(source: string): string {
    return source
        .replace(/^:PROPERTIES:\r?\n(?:.*\r?\n)*?:END:\r?\n/, '')
        .replace(/^#\+[A-Za-z_][A-Za-z0-9_-]*:.*\r?\n/gm, '');
}

/**
 * Render org text as HTML that is safe to place in a page. Raw HTML in the
 * source shows as text, and a link or an attribute that could run script is
 * removed.
 */
export function renderOrgHtml(source: string, options: RenderOptions = {}): string {
    if (source.length > MAX_SOURCE_LENGTH) {
        return `<pre>${escapeHtml(source.slice(0, MAX_SOURCE_LENGTH))}</pre>`;
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
        return String(processor.processSync(guardLines(stripHeader(source))));
    } catch {
        return `<pre>${escapeHtml(source)}</pre>`;
    }
}
