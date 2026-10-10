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

import type { Element, Root, RootContent } from 'hast';
import rehypeParse from 'rehype-parse';
import { unified } from 'unified';
import { describe, expect, it } from 'vitest';
import { renderOrgHtml } from './render.js';

/** Parse the output as a browser would, so a test sees elements and not text. */
function elementsOf(html: string): Element[] {
    const tree = unified().use(rehypeParse, { fragment: true }).parse(html);
    const found: Element[] = [];
    const visit = (node: Root | RootContent): void => {
        if (node.type === 'element') found.push(node);
        if ('children' in node) node.children.forEach(visit);
    };
    visit(tree);
    return found;
}

describe('inline markup', () => {
    it('renders emphasis, code and links', () => {
        const html = renderOrgHtml(
            'A *bold* and /italic/ with =verbatim= and ~code~ and [[https://example.org][a site]].\n',
        );
        expect(html).toContain('<strong>bold</strong>');
        expect(html).toContain('<em>italic</em>');
        expect(html).toContain('verbatim</code>');
        expect(html).toContain('code</code>');
        expect(html).toContain('<a href="https://example.org">a site</a>');
    });

    it('escapes angle brackets in ordinary text', () => {
        const html = renderOrgHtml('1 < 2 and a <b>tag</b>\n');
        expect(html).not.toContain('<b>');
        expect(html).toContain('&#x3C;b>');
    });
});

describe('blocks', () => {
    it('renders headings, lists and tables', () => {
        const html = renderOrgHtml('* Title\n\n- one\n- two\n\n| a | b |\n|---+---|\n| 1 | 2 |\n');
        expect(html).toContain('<h1');
        expect(html).toContain('<li>');
        expect(html).toContain('<table>');
        expect(html).toContain('<th>a</th>');
        expect(html).toContain('<td>2</td>');
    });

    it('renders a src block as pre and code', () => {
        const html = renderOrgHtml('#+begin_src sh\n./compass.sh check\n#+end_src\n');
        expect(html).toContain('<pre>');
        expect(html).toContain('./compass.sh check');
    });
});

describe('the page header', () => {
    it('takes away the file drawer and the keyword lines', () => {
        const html = renderOrgHtml(
            ':PROPERTIES:\n:ID: ABC\n:END:\n#+title: T\n#+type: test_scenario\n\nBody text\n',
        );
        expect(html).not.toContain('ABC');
        expect(html).not.toContain('test_scenario');
        expect(html).toContain('Body text');
    });
});

describe('links', () => {
    it('points an id link at the reader and names the id', () => {
        const html = renderOrgHtml('See [[id:ab12][the story]].\n');
        expect(html).toContain('href="#org-id-AB12"');
        expect(html).toContain('data-org-id="AB12"');
        const custom = renderOrgHtml('See [[id:ab12][the story]].\n', {
            idHref: (id) => `/docs/${id}`,
        });
        expect(custom).toContain('href="/docs/AB12"');
    });

    it('makes a file link relative', () => {
        const html = renderOrgHtml('[[file:shot_01.png]]\n');
        expect(html).toContain('src="shot_01.png"');
        expect(html).not.toContain('file:');
    });

    it('links a bare issue number only when asked to', () => {
        const text = 'Fixed in #3061, see `#12` and [[https://x.org][#9999 here]].\n';
        expect(renderOrgHtml(text)).not.toContain('/issues/');
        const html = renderOrgHtml(
            'Fixed in #3061 and ~#4444~ and [[https://x.org][#9999 here]].\n',
            {
                githubRepo: 'https://github.com/OreStudio/OreStudio',
            },
        );
        expect(html).toContain(
            '<a href="https://github.com/OreStudio/OreStudio/issues/3061">#3061</a>',
        );
        expect(html).not.toContain('issues/4444');
        expect(html).not.toContain('issues/9999');
    });
});

describe('hostile input', () => {
    const attacks = [
        '[[javascript:alert(1)][click]]\n',
        '[[JaVaScRiPt:alert(1)][click]]\n',
        '[[data:text/html;base64,PHNjcmlwdD4=][x]]\n',
        '#+begin_export html\n<script>alert(1)</script>\n#+end_export\n',
        'text @@html:<img src=x onerror=alert(1)>@@ text\n',
        '<img src=x onerror=alert(1)>\n',
        '<script>alert(1)</script>\n',
        '[[https://x.org" onmouseover="alert(1)][x]]\n',
        '* <script>alert(1)</script>\n',
        '| <script>alert(1)</script> |\n',
        '#+begin_src html\n<script>alert(1)</script>\n#+end_src\n',
        '[[id:"><script>alert(1)</script>][x]]\n',
    ];

    it.each(attacks)('puts no script, handler or script link in the output of %j', (source) => {
        const html = renderOrgHtml(source);
        for (const element of elementsOf(html)) {
            expect(element.tagName).not.toBe('script');
            for (const [name, value] of Object.entries(element.properties)) {
                expect(name.toLowerCase().startsWith('on'), `${element.tagName}.${name}`).toBe(
                    false,
                );
                if (name === 'href' || name === 'src') {
                    const url = String(value).trim().toLowerCase();
                    expect(url.startsWith('javascript:')).toBe(false);
                    expect(url.startsWith('data:')).toBe(false);
                    expect(url.startsWith('vbscript:')).toBe(false);
                }
            }
        }
    });

    it('does not throw on odd input', () => {
        expect(() => renderOrgHtml('')).not.toThrow();
        expect(() => renderOrgHtml('[[' + 'a'.repeat(5000))).not.toThrow();
    });

    it('shows a line of nested markers as text, quickly, instead of overflowing', () => {
        for (const line of ['*'.repeat(5000), '/'.repeat(5000), '*a '.repeat(2000)]) {
            const started = Date.now();
            const html = renderOrgHtml(`before\n${line}\nafter\n`);
            expect(Date.now() - started).toBeLessThan(2000);
            expect(html).toContain('before');
            expect(html).toContain('after');
        }
    });

    it('keeps a long ordinary line as ordinary prose', () => {
        const html = renderOrgHtml(`${'word '.repeat(3000)}and *bold*\n`);
        expect(html).toContain('<strong>bold</strong>');
    });

    it('falls back to an escaped block for a source that is too large', () => {
        const html = renderOrgHtml('<b>' + 'x'.repeat(2_100_000));
        expect(html.startsWith('<pre>&lt;b&gt;')).toBe(true);
        expect(html).not.toContain('<b>');
    });
});

describe('a real scenario', () => {
    it('renders the single-client fixture without raw markup leaking', async () => {
        const { readFileSync } = await import('node:fs');
        const text = readFileSync(
            new URL('./fixtures/single_client_scenario.org', import.meta.url),
            'utf8',
        );
        const html = renderOrgHtml(text, { idHref: (id) => `/docs/${id}` });
        expect(html).toContain('Capture the General tab');
        expect(html).toContain('href="/docs/');
        expect(html).not.toContain('[[');
        expect(html).not.toContain(':PROPERTIES:');
    });
});
