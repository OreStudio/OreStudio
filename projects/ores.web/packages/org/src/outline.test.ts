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

import { existsSync, readdirSync, readFileSync, statSync } from 'node:fs';
import { join } from 'node:path';
import { describe, expect, it } from 'vitest';
import {
    fieldTable,
    findSection,
    idLink,
    linkText,
    parseOrg,
    sectionBody,
    sectionChildren,
} from './index.js';

const fixture = (name: string): string =>
    readFileSync(new URL(`./fixtures/${name}`, import.meta.url), 'utf8');

describe('a multi-client scenario made by compass', () => {
    const doc = parseOrg(fixture('multi_client_scenario.org'));

    it('reads the file-level metadata', () => {
        expect(doc.id).toBe('B12A8474-32FF-45B7-BFD8-FF1B42845DAD');
        expect(doc.keywords.get('type')).toBe('test_scenario');
        expect(doc.keywords.get('title')).toBe(
            'Test Scenario: Verify counterparty Qt UI end-to-end post-NATS',
        );
        expect(doc.filetags).toEqual([
            'commission-party-counterparty-party-status',
            'sprint_23',
            'v0',
        ]);
        expect(doc.todo.open).toEqual(['PENDING']);
        expect(doc.todo.done).toEqual(['PASSED', 'FAILED']);
    });

    it('reads the scenario info as a field table', () => {
        const info = fieldTable(findSection(doc, 'Scenario Info')!);
        expect(info.get('clients')).toBe('blue, red');
        expect(info.get('state')).toBe('PENDING');
        expect(idLink(info.get('verifies task') ?? '')).toBe(
            '8DC4ABA3-B053-4C40-B575-6EDFCF3C86DE',
        );
        expect(linkText(info.get('parent story') ?? '')).toBe(
            'Commission: party, counterparty, and party_status',
        );
    });

    it('nests the steps under their client and the result under its step', () => {
        const steps = findSection(doc, 'Steps')!;
        const clients = sectionChildren(doc, steps);
        expect(clients.map((c) => c.title)).toEqual(['blue', 'red']);
        const blue = clients[0]!;
        const blueSteps = sectionChildren(doc, blue);
        expect(blueSteps).toHaveLength(10);
        expect(blueSteps[0]?.title).toBe('Connect and log in');
        const result = sectionChildren(doc, blueSteps[0]!)[0]!;
        expect(result.title).toBe('Result');
        expect(result.path).toBe('Steps / blue / Connect and log in / Result');
        expect(fieldTable(result).get('status')).toBe('PASS');
        expect(sectionChildren(doc, clients[1]!)).toHaveLength(3);
    });

    it('reads the overall results table', () => {
        const results = fieldTable(findSection(doc, 'Results')!);
        expect(results.get('status')).toBe('FAILED');
        expect(results.get('worktree')).toBe('prime_origin');
    });

    it('leaves a src block alone', () => {
        const step = findSection(doc, 'Connect and log in')!;
        const body = sectionBody(doc, step);
        expect(body.some((line) => line.startsWith('#+begin_src sh'))).toBe(true);
        expect(step.prose.join(' ')).not.toContain('barclays_system_provision.ores\n');
        expect(step.prose.some((line) => line.startsWith('./compass.sh'))).toBe(false);
    });
});

describe('a single-client scenario made by compass', () => {
    const doc = parseOrg(fixture('single_client_scenario.org'));

    it('puts the steps straight under Steps', () => {
        const steps = sectionChildren(doc, findSection(doc, 'Steps')!);
        expect(steps.map((s) => s.title)).toEqual([
            'Capture the General tab',
            'Capture the Formatting tab',
            'Capture the Rounding tab',
            'Capture the Provenance tab',
            'Move the captured screenshots into assets/images and update the manual',
        ]);
        const result = sectionChildren(doc, steps[1]!)[0]!;
        expect(fieldTable(result).get('notes')).toContain('step2');
    });
});

describe('line positions', () => {
    const doc = parseOrg(fixture('multi_client_scenario.org'));

    it('slices every section back to its heading', () => {
        for (const section of doc.sections) {
            expect(doc.lines[section.headingLine]).toMatch(/^\*+ /);
            expect(section.bodyStart).toBeGreaterThan(section.headingLine);
            expect(section.bodyEnd).toBeGreaterThanOrEqual(section.bodyStart);
            expect(section.subtreeEnd).toBeGreaterThanOrEqual(section.bodyEnd);
        }
    });

    it('ends a subtree at the next heading of the same level or above', () => {
        const blue = findSection(doc, 'blue')!;
        const red = findSection(doc, 'red')!;
        expect(blue.subtreeEnd).toBe(red.headingLine);
        expect(blue.bodyEnd).toBeLessThan(blue.subtreeEnd);
    });

    it('names the lines a table occupies', () => {
        const info = findSection(doc, 'Scenario Info')!;
        const table = info.tables[0]!;
        expect(doc.lines[table.firstLine]).toMatch(/^\| Field/);
        expect(doc.lines[table.lastLine]).toMatch(/^\| State/);
        expect(table.header).toEqual(['Field', 'Value']);
    });

    it('lets a writer replace a table and keep every other line', () => {
        const result = findSection(doc, 'Result')!;
        const table = result.tables[0]!;
        const replaced = [
            ...doc.lines.slice(0, table.firstLine),
            '| Field  | Value |',
            '|--------+-------|',
            '| Status | FAIL |',
            ...doc.lines.slice(table.lastLine + 1),
        ];
        const again = parseOrg(replaced.join(doc.eol) + doc.eol);
        expect(fieldTable(findSection(again, 'Result')!).get('status')).toBe('FAIL');
        expect(again.sections.map((s) => s.title)).toEqual(doc.sections.map((s) => s.title));
    });

    it('keeps the line end the file uses', () => {
        expect(doc.eol).toBe('\n');
        expect(parseOrg('* A\r\n\r\ntext\r\n').eol).toBe('\r\n');
        expect(parseOrg('* A\r\n\r\ntext\r\n').sections[0]?.prose).toEqual(['text']);
    });
});

describe('tables', () => {
    it('takes a header only when a separator follows the first row', () => {
        const doc = parseOrg('* T\n\n| a | b |\n|---+---|\n| 1 | 2 |\n\n| x | y |\n| z | w |\n');
        const [withHeader, without] = doc.sections[0]!.tables;
        expect(withHeader?.header).toEqual(['a', 'b']);
        expect(withHeader?.rows).toEqual([['1', '2']]);
        expect(without?.header).toBeNull();
        expect(without?.rows).toEqual([
            ['x', 'y'],
            ['z', 'w'],
        ]);
    });

    it('does not turn the header row into a field', () => {
        const doc = parseOrg('* T\n| Field | Value |\n|---+---|\n| Status | PASS |\n');
        const fields = fieldTable(doc.sections[0]!);
        expect([...fields.keys()]).toEqual(['status']);
    });
});

describe('lists', () => {
    it('reads bullets and numbers and joins a wrapped item', () => {
        const doc = parseOrg('* L\n- one\n- two that\n  wraps here\n+ three\n1. four\n2) five\n');
        expect(doc.sections[0]?.items.map((i) => i.text)).toEqual([
            'one',
            'two that wraps here',
            'three',
            'four',
            'five',
        ]);
    });

    it('does not join prose that follows a blank line', () => {
        const doc = parseOrg('* L\n- one\n\n  later prose\n');
        expect(doc.sections[0]?.items.map((i) => i.text)).toEqual(['one']);
        expect(doc.sections[0]?.prose).toEqual(['later prose']);
    });
});

describe('blocks', () => {
    it('ignores tables and lists inside a block, and a comma-escaped star line', () => {
        const doc = parseOrg(
            '* A\n#+begin_src org\n,* not a heading\n| not | a table |\n- not an item\n#+end_src\n- real\n',
        );
        expect(doc.sections.map((s) => s.title)).toEqual(['A']);
        expect(doc.sections[0]?.tables).toEqual([]);
        expect(doc.sections[0]?.items.map((i) => i.text)).toEqual(['real']);
    });

    it('ends a block at the next heading, as org does, and starts clean after it', () => {
        const doc = parseOrg('* A\n#+begin_example\n| x |\n* B\n| y |\n');
        expect(doc.sections.map((s) => s.title)).toEqual(['A', 'B']);
        expect(doc.sections[0]?.tables).toEqual([]);
        expect(doc.sections[1]?.tables[0]?.rows).toEqual([['y']]);
    });
});

describe('headings and drawers', () => {
    it('reads a todo keyword and tags', () => {
        const doc = parseOrg('#+todo: OPEN | CLOSED\n* CLOSED Fix it   :a:b:\n* Plain\n');
        expect(doc.sections[0]).toMatchObject({
            todo: 'CLOSED',
            title: 'Fix it',
            tags: ['a', 'b'],
        });
        expect(doc.sections[1]).toMatchObject({ todo: null, title: 'Plain', tags: [] });
    });

    it('reads the property drawer of a section', () => {
        const doc = parseOrg('* A\n:PROPERTIES:\n:ID: ab-12\n:END:\nbody\n');
        const section = doc.sections[0]!;
        expect(section.id).toBe('AB-12');
        expect(section.prose).toEqual(['body']);
        expect(section.bodyStart).toBe(4);
    });

    it('reads the file drawer, whatever its property names', () => {
        const doc = parseOrg(
            ':PROPERTIES:\n:ID: abc\n:ores.sql.x.enabled: true\n:END:\n#+title: T\n',
        );
        expect(doc.id).toBe('ABC');
        expect(doc.properties.get('ores.sql.x.enabled')).toBe('true');
    });

    it('skips comments and other drawers in a body', () => {
        const doc = parseOrg('* A\n# a comment\n:LOGBOOK:\n- noise\n:END:\nkept\n');
        expect(doc.sections[0]?.prose).toEqual(['kept']);
        expect(doc.sections[0]?.items).toEqual([]);
    });
});

describe('hostile and odd input', () => {
    it('does not let a keyword named like a prototype key pollute anything', () => {
        const doc = parseOrg('#+__proto__: x\n#+constructor: y\n* A\n');
        expect(doc.keywords.get('__proto__')).toBe('x');
        expect(({} as Record<string, unknown>)['x']).toBeUndefined();
        expect(Object.keys({})).toEqual([]);
    });

    it('survives empty input, a lone heading and a heading with no title', () => {
        expect(parseOrg('').sections).toEqual([]);
        expect(parseOrg('* ').sections[0]?.title).toBe('');
        expect(parseOrg('*').sections).toEqual([]);
        expect(parseOrg(':PROPERTIES:\n:ID: x').id).toBe('X');
    });

    it('survives a very long line', () => {
        const long = 'a'.repeat(200_000);
        expect(parseOrg(`* A\n${long}\n`).sections[0]?.prose[0]).toHaveLength(200_000);
    });
});

describe('the compass scenario corpus', () => {
    const root = new URL('../../../../../doc/agile/versions/', import.meta.url).pathname;
    const walk = (dir: string): string[] =>
        readdirSync(dir).flatMap((name) => {
            const path = join(dir, name);
            if (statSync(path).isDirectory()) return walk(path);
            return name.startsWith('scenario_') && name.endsWith('.org') ? [path] : [];
        });

    it.skipIf(!existsSync(root))('parses every scenario doc in the repository', () => {
        const files = walk(root);
        expect(files.length).toBeGreaterThan(50);
        for (const file of files) {
            const doc = parseOrg(readFileSync(file, 'utf8'));
            expect(doc.keywords.get('type'), file).toBe('test_scenario');
            expect(findSection(doc, 'Steps'), file).toBeDefined();
            for (const section of doc.sections) {
                expect(section.bodyEnd, file).toBeGreaterThanOrEqual(section.bodyStart);
                expect(section.subtreeEnd, file).toBeGreaterThanOrEqual(section.bodyEnd);
            }
        }
    });
});
