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

/**
 * A line-indexed outline of an org document.
 *
 * The parser reads the shape that ores.codegen and compass write: a
 * :PROPERTIES: drawer, a block of #+keyword lines, then nested sections that
 * hold prose, lists and tables. It is not a full org parser. Inline markup
 * is the renderer's job.
 *
 * Every part of the outline names the lines it came from, so a writer can
 * replace one table or one section in place and leave every other byte of the
 * document alone. Line numbers are zero based. A range is [first, end), and a
 * table's range is [firstLine, lastLine] with both ends included.
 */

/** A table, with the lines it occupies. */
export interface OrgTable {
    /** The first row, when a separator line follows it. Otherwise null. */
    readonly header: readonly string[] | null;
    /** Every row after the header, separators left out. */
    readonly rows: readonly (readonly string[])[];
    readonly firstLine: number;
    readonly lastLine: number;
}

/** A list item. A wrapped item has its continuation lines joined by a space. */
export interface OrgItem {
    readonly text: string;
    readonly indent: number;
    readonly line: number;
}

export interface OrgSection {
    readonly level: number;
    /** The heading without its stars, its todo keyword and its tags. */
    readonly title: string;
    /** The todo keyword the document declares and the heading carries. */
    readonly todo: string | null;
    readonly tags: readonly string[];
    /** The titles of this section and its parents, joined by " / ". */
    readonly path: string;
    /** The :ID: of the section's own property drawer. */
    readonly id: string | null;
    readonly properties: ReadonlyMap<string, string>;
    readonly headingLine: number;
    /** The first line after the heading and its property drawer. */
    readonly bodyStart: number;
    /** The line of the next heading of any level. The body excludes child sections. */
    readonly bodyEnd: number;
    /** The line of the next heading at this level or above. The subtree includes children. */
    readonly subtreeEnd: number;
    readonly tables: readonly OrgTable[];
    readonly items: readonly OrgItem[];
    readonly prose: readonly string[];
}

export interface OrgDoc {
    /** The document's lines without their line ends. */
    readonly lines: readonly string[];
    /** The line end the document uses, so a writer can keep it. */
    readonly eol: '\n' | '\r\n';
    /** The :ID: of the file-level property drawer, upper cased. */
    readonly id: string | null;
    readonly properties: ReadonlyMap<string, string>;
    /** The #+keyword lines before the first heading, keyed in lower case. */
    readonly keywords: ReadonlyMap<string, string>;
    readonly filetags: readonly string[];
    readonly todo: { readonly open: readonly string[]; readonly done: readonly string[] };
    /** The prose between the keywords and the first heading, joined by a space. */
    readonly preamble: string;
    /** The line of the first heading, or the line count when there is none. */
    readonly headerEnd: number;
    readonly sections: readonly OrgSection[];
}

const HEADING = /^(\*+) (.*)$/;
const KEYWORD = /^#\+([A-Za-z_][A-Za-z0-9_-]*):\s*(.*)$/;
const PROPERTY = /^:([^:\s]+):\s*(.*)$/;
const DRAWER_START = /^:[A-Za-z_][A-Za-z0-9_-]*:$/;
const BLOCK_BEGIN = /^#\+begin_([A-Za-z0-9_-]+)/i;
const BLOCK_END = /^#\+end_([A-Za-z0-9_-]+)/i;
const LIST_ITEM = /^(\s*)(?:[-+]|\d+[.)])\s+(.*)$/;
const TAGS = /\s+:([\w@#%]+(?::[\w@#%]+)*):\s*$/;

/** Strip org links to their description, or to their target when bare. */
export function linkText(s: string): string {
    return s
        .replace(/\[\[([^\]]*)\]\[([^\]]*)\]\]/g, '$2')
        .replace(/\[\[([^\]]*)\]\]/g, '$1')
        .trim();
}

/** The first id: link target in a string, upper cased, or null. */
export function idLink(s: string): string | null {
    const match = /\[\[id:([0-9A-Fa-f-]+)\]/.exec(s);
    return match?.[1] === undefined ? null : match[1].toUpperCase();
}

function isSeparator(line: string): boolean {
    return /^\|[-+|]*$/.test(line.replace(/\s/g, '')) && line.includes('-');
}

function cells(line: string): string[] {
    const inner = line.trim().replace(/^\|/, '').replace(/\|$/, '');
    return inner.split('|').map((cell) => cell.trim());
}

/** Read a property drawer that starts at `at`. Returns the lines it spans. */
function readDrawer(
    lines: readonly string[],
    at: number,
): { properties: Map<string, string>; end: number } | null {
    if (lines[at]?.trim() !== ':PROPERTIES:') return null;
    const properties = new Map<string, string>();
    let i = at + 1;
    while (i < lines.length && lines[i]?.trim() !== ':END:') {
        const match = PROPERTY.exec((lines[i] ?? '').trim());
        if (match?.[1] !== undefined)
            properties.set(match[1].toLowerCase(), (match[2] ?? '').trim());
        i += 1;
    }
    return { properties, end: Math.min(i + 1, lines.length) };
}

function parseTodo(value: string): { open: string[]; done: string[] } {
    const [open = '', done = ''] = value.split('|');
    const words = (s: string) => s.split(/\s+/).filter(Boolean);
    return { open: words(open), done: words(done) };
}

interface Body {
    tables: OrgTable[];
    items: OrgItem[];
    prose: string[];
}

/** Read the lines of one section body. Block contents are left alone. */
function readBody(lines: readonly string[], start: number, end: number): Body {
    const body: Body = { tables: [], items: [], prose: [] };
    let table: { rows: string[][]; separatorAt: number; firstLine: number } | null = null;
    let lastItem: { text: string; indent: number; line: number } | null = null;
    let block: string | null = null;

    const closeTable = (lastLine: number) => {
        if (table === null) return;
        const rows = table.rows;
        const hasHeader = table.separatorAt === 1;
        body.tables.push({
            header: hasHeader ? (rows[0] ?? []) : null,
            rows: hasHeader ? rows.slice(1) : rows,
            firstLine: table.firstLine,
            lastLine,
        });
        table = null;
    };
    const closeItem = () => {
        if (lastItem !== null) body.items.push(lastItem);
        lastItem = null;
    };

    for (let i = start; i < end; i += 1) {
        const raw = lines[i] ?? '';
        const t = raw.trim();

        if (block !== null) {
            const close = BLOCK_END.exec(t);
            if (close?.[1]?.toLowerCase() === block) block = null;
            continue;
        }
        const open = BLOCK_BEGIN.exec(t);
        if (open?.[1] !== undefined) {
            closeTable(i - 1);
            closeItem();
            block = open[1].toLowerCase();
            continue;
        }
        if (t.startsWith('|')) {
            closeItem();
            if (table === null) table = { rows: [], separatorAt: -1, firstLine: i };
            if (isSeparator(t)) {
                if (table.separatorAt < 0) table.separatorAt = table.rows.length;
            } else {
                table.rows.push(cells(t));
            }
            continue;
        }
        closeTable(i - 1);

        if (t === '') {
            closeItem();
            continue;
        }
        if (DRAWER_START.test(t)) {
            closeItem();
            while (i + 1 < end && (lines[i + 1] ?? '').trim() !== ':END:') i += 1;
            i += 1;
            continue;
        }
        if (t.startsWith('#+') || t === '#' || t.startsWith('# ')) {
            closeItem();
            continue;
        }
        const item = LIST_ITEM.exec(raw);
        if (item !== null) {
            closeItem();
            lastItem = { text: (item[2] ?? '').trim(), indent: (item[1] ?? '').length, line: i };
            continue;
        }
        const indent = raw.length - raw.trimStart().length;
        if (lastItem !== null && indent > lastItem.indent) {
            lastItem.text = `${lastItem.text} ${t}`;
            continue;
        }
        closeItem();
        body.prose.push(t);
    }
    closeTable(end - 1);
    closeItem();
    return body;
}

/** Parse the title of a heading into its todo keyword, its text and its tags. */
function readHeading(
    raw: string,
    todoWords: ReadonlySet<string>,
): { title: string; todo: string | null; tags: string[] } {
    let rest = raw.trim();
    let tags: string[] = [];
    const tagged = TAGS.exec(rest);
    if (tagged?.[1] !== undefined) {
        tags = tagged[1].split(':');
        rest = rest.slice(0, tagged.index).trim();
    }
    const first = rest.split(/\s+/)[0] ?? '';
    if (first !== '' && todoWords.has(first)) {
        return { title: rest.slice(first.length).trim(), todo: first, tags };
    }
    return { title: rest, todo: null, tags };
}

/** Parse org text into an outline. It never throws on text. */
export function parseOrg(text: string): OrgDoc {
    const eol = text.includes('\r\n') ? '\r\n' : '\n';
    const lines = text.split(/\r?\n/);
    if (lines.length > 0 && lines[lines.length - 1] === '') lines.pop();

    let i = 0;
    const drawer = readDrawer(lines, 0);
    const properties = drawer?.properties ?? new Map<string, string>();
    if (drawer !== null) i = drawer.end;
    const idValue = properties.get('id');
    const id = idValue === undefined || idValue === '' ? null : idValue.toUpperCase();

    const isHeading = (n: number) => HEADING.test(lines[n] ?? '');
    const keywords = new Map<string, string>();
    const open: string[] = [];
    const done: string[] = [];
    let filetags: string[] = [];
    const preamble: string[] = [];
    for (; i < lines.length && !isHeading(i); i += 1) {
        const line = lines[i] ?? '';
        const keyword = KEYWORD.exec(line);
        if (keyword?.[1] !== undefined) {
            const key = keyword[1].toLowerCase();
            const value = (keyword[2] ?? '').trim();
            if (key === 'filetags') {
                filetags = value
                    .split(':')
                    .map((tag) => tag.trim())
                    .filter(Boolean);
            } else if (key === 'todo') {
                const words = parseTodo(value);
                open.push(...words.open);
                done.push(...words.done);
            }
            keywords.set(key, value);
            continue;
        }
        if (line.trim() !== '') preamble.push(line.trim());
    }
    const headerEnd = i;
    const todoWords = new Set([...open, ...done]);

    interface Draft {
        level: number;
        raw: string;
        headingLine: number;
    }
    const drafts: Draft[] = [];
    for (let n = headerEnd; n < lines.length; n += 1) {
        const match = HEADING.exec(lines[n] ?? '');
        if (match?.[1] !== undefined) {
            drafts.push({ level: match[1].length, raw: match[2] ?? '', headingLine: n });
        }
    }

    const sections: OrgSection[] = [];
    const stack: string[] = [];
    drafts.forEach((draft, index) => {
        const next = drafts[index + 1];
        const bodyEnd = next === undefined ? lines.length : next.headingLine;
        let subtreeEnd = lines.length;
        for (let k = index + 1; k < drafts.length; k += 1) {
            const later = drafts[k];
            if (later !== undefined && later.level <= draft.level) {
                subtreeEnd = later.headingLine;
                break;
            }
        }
        const heading = readHeading(draft.raw, todoWords);
        while (stack.length >= draft.level) stack.pop();
        stack.push(heading.title);

        const sectionDrawer = readDrawer(lines, draft.headingLine + 1);
        const bodyStart = sectionDrawer === null ? draft.headingLine + 1 : sectionDrawer.end;
        const sectionProps = sectionDrawer?.properties ?? new Map<string, string>();
        const sectionId = sectionProps.get('id');
        const body = readBody(lines, Math.min(bodyStart, bodyEnd), bodyEnd);
        sections.push({
            level: draft.level,
            title: heading.title,
            todo: heading.todo,
            tags: heading.tags,
            path: stack.join(' / '),
            id: sectionId === undefined || sectionId === '' ? null : sectionId.toUpperCase(),
            properties: sectionProps,
            headingLine: draft.headingLine,
            bodyStart: Math.min(bodyStart, bodyEnd),
            bodyEnd,
            subtreeEnd,
            tables: body.tables,
            items: body.items,
            prose: body.prose,
        });
    });

    return {
        lines,
        eol,
        id,
        properties,
        keywords,
        filetags,
        todo: { open, done },
        preamble: preamble.join(' '),
        headerEnd,
        sections,
    };
}

/** The first section with this title, ignoring case. Search one subtree when `within` is given. */
export function findSection(
    doc: OrgDoc,
    title: string,
    within?: OrgSection,
): OrgSection | undefined {
    const wanted = title.toLowerCase();
    return doc.sections.find(
        (section) =>
            section.title.toLowerCase() === wanted &&
            (within === undefined ||
                (section.headingLine > within.headingLine &&
                    section.headingLine < within.subtreeEnd)),
    );
}

/** The sections one level below this one, in document order. */
export function sectionChildren(doc: OrgDoc, parent: OrgSection): OrgSection[] {
    return doc.sections.filter(
        (section) =>
            section.level === parent.level + 1 &&
            section.headingLine > parent.headingLine &&
            section.headingLine < parent.subtreeEnd,
    );
}

/** The raw lines of a section's own body, without its drawer or its child sections. */
export function sectionBody(doc: OrgDoc, section: OrgSection): string[] {
    return doc.lines.slice(section.bodyStart, section.bodyEnd);
}

/** Read the rows of a section's first table into a map from lower-case field to value. The header names the columns, so it is not a field. */
export function fieldTable(section: OrgSection): ReadonlyMap<string, string> {
    const out = new Map<string, string>();
    const table = section.tables[0];
    if (table === undefined) return out;
    for (const row of table.rows) {
        const field = row[0];
        if (field !== undefined && field !== '' && row.length >= 2) {
            out.set(field.toLowerCase(), row[1] ?? '');
        }
    }
    return out;
}
