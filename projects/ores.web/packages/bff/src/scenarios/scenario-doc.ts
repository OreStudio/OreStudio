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

import {
    fieldTable,
    findSection,
    idLink,
    linkText,
    parseOrg,
    sectionBody,
    sectionChildren,
    type OrgDoc,
    type OrgSection,
} from '@ores/org';

/**
 * A test scenario, read from the org doc that the compass test_scenario
 * template creates. ores.web cannot call compass, so this file is the
 * contract: it reads that format, and it writes a run back into it.
 *
 * A step is a heading under `* Steps`. A single-client scenario keeps its
 * steps directly under it. A multi-client scenario nests them under one
 * heading per client. A step's outcome lives in a `Result` child heading with
 * a Field/Value table.
 */

export type StepStatus = 'PENDING' | 'PASS' | 'FAIL' | 'DROPPED';
export type ScenarioState = 'PENDING' | 'PASSED' | 'FAILED';

/** Names one step. A single-client scenario has a null client. */
export interface StepRef {
    readonly client: string | null;
    readonly title: string;
}

export interface ScenarioStep extends StepRef {
    /** The step's body without its Result, and without org comment lines. */
    readonly instructions: readonly string[];
    readonly status: StepStatus;
    readonly notes: string;
}

/** The overall `* Results` table. A field the doc leaves blank is an empty string. */
export interface ScenarioRunRecord {
    readonly status: string;
    readonly completedAt: string;
    readonly branch: string;
    readonly commit: string;
    readonly worktree: string;
}

export interface DocLink {
    readonly id: string;
    readonly title: string;
}

export interface Scenario {
    readonly id: string;
    readonly title: string;
    readonly description: string;
    readonly state: ScenarioState;
    readonly target: string;
    readonly story: DocLink | null;
    readonly task: DocLink | null;
    readonly clients: readonly string[];
    readonly beforeYouStart: readonly string[];
    readonly steps: readonly ScenarioStep[];
    readonly run: ScenarioRunRecord;
}

/**
 * What a tester's save carries. It names every step it writes. A note lives
 * in one table cell, so a line end comes back as "; " and a pipe as "/".
 */
export interface StepOutcome extends StepRef {
    readonly status: StepStatus;
    readonly notes: string;
}

/**
 * A tester's save. It carries no overall state: the state follows from the
 * steps once the save is in. A scenario closes when no step is pending, and
 * then it is FAILED if any step failed, else PASSED. `completedAt` is written
 * only when the save closes the scenario.
 */
export interface RunInput {
    readonly steps: readonly StepOutcome[];
    readonly completedAt: string;
    readonly branch: string;
    readonly commit: string;
    readonly worktree: string;
}

export class ScenarioFormatError extends Error {
    constructor(message: string) {
        super(message);
        this.name = 'ScenarioFormatError';
    }
}

/** A save names a step that the doc does not have. */
export class UnknownStepError extends Error {
    readonly step: StepRef;

    constructor(step: StepRef) {
        super(`The scenario has no step "${step.title}"${clientSuffix(step)}.`);
        this.name = 'UnknownStepError';
        this.step = step;
    }
}

/** A save names a step whose title the doc repeats under one client. */
export class AmbiguousStepError extends Error {
    readonly step: StepRef;

    constructor(step: StepRef) {
        super(`The scenario has more than one step "${step.title}"${clientSuffix(step)}.`);
        this.name = 'AmbiguousStepError';
        this.step = step;
    }
}

/** A save names one step twice. */
export class DuplicateOutcomeError extends Error {
    readonly step: StepRef;

    constructor(step: StepRef) {
        super(`The save names the step "${step.title}"${clientSuffix(step)} more than once.`);
        this.name = 'DuplicateOutcomeError';
        this.step = step;
    }
}

function clientSuffix(step: StepRef): string {
    return step.client === null ? '' : ` under client "${step.client}"`;
}

const STEP_STATUSES: ReadonlySet<string> = new Set(['PENDING', 'PASS', 'FAIL', 'DROPPED']);

function stepStatus(raw: string): StepStatus {
    const word = raw.trim().split(/\s+/)[0]?.toUpperCase() ?? '';
    if (word === 'PASSED') return 'PASS';
    if (word === 'FAILED') return 'FAIL';
    return STEP_STATUSES.has(word) ? (word as StepStatus) : 'PENDING';
}

function scenarioState(raw: string): ScenarioState {
    const word = raw.trim().split(/\s+/)[0]?.toUpperCase() ?? '';
    if (word === 'PASSED' || word === 'FAILED') return word;
    return 'PENDING';
}

/**
 * The Qt runner wrote only the Results table, and left the State row at
 * PENDING. A PASSED or FAILED result therefore outranks the State row.
 */
function closedState(raw: string): ScenarioState | null {
    const state = scenarioState(raw);
    return state === 'PENDING' ? null : state;
}

function link(value: string | undefined): DocLink | null {
    if (value === undefined) return null;
    const id = idLink(value);
    return id === null ? null : { id, title: linkText(value) };
}

/** An org comment line starts with a hash and a space, or is a lone hash. */
function isComment(line: string): boolean {
    return line === '#' || line.startsWith('# ');
}

function trimBlank(lines: string[]): string[] {
    let first = 0;
    let last = lines.length;
    while (first < last && (lines[first] ?? '').trim() === '') first += 1;
    while (last > first && (lines[last - 1] ?? '').trim() === '') last -= 1;
    return lines.slice(first, last);
}

interface Located {
    readonly ref: StepRef;
    readonly section: OrgSection;
    readonly result: OrgSection | undefined;
}

/**
 * The steps of the doc, in order. A level-two heading under `* Steps` that has
 * a child other than `Result` is a client. Otherwise it is a step.
 */
function locateSteps(doc: OrgDoc, steps: OrgSection): Located[] {
    const out: Located[] = [];
    const resultOf = (step: OrgSection) =>
        sectionChildren(doc, step).find((child) => child.title.toLowerCase() === 'result');
    for (const child of sectionChildren(doc, steps)) {
        const below = sectionChildren(doc, child);
        const isClient = below.some((s) => s.title.toLowerCase() !== 'result');
        if (!isClient) {
            out.push({
                ref: { client: null, title: child.title },
                section: child,
                result: resultOf(child),
            });
            continue;
        }
        for (const step of below) {
            if (step.title.toLowerCase() === 'result') continue;
            out.push({
                ref: { client: child.title, title: step.title },
                section: step,
                result: resultOf(step),
            });
        }
    }
    return out;
}

/** Read a scenario doc. It throws ScenarioFormatError when the doc is not one. */
export function parseScenario(text: string): Scenario {
    const doc = parseOrg(text);
    if (doc.keywords.get('type') !== 'test_scenario' || doc.id === null) {
        throw new ScenarioFormatError('The document is not a test scenario with an :ID:.');
    }
    const steps = findSection(doc, 'Steps');
    if (steps === undefined) throw new ScenarioFormatError('The scenario has no Steps section.');

    const info = findSection(doc, 'Scenario Info');
    const fields = info === undefined ? new Map<string, string>() : fieldTable(info);
    const results = findSection(doc, 'Results');
    const resultFields = results === undefined ? new Map<string, string>() : fieldTable(results);
    const before = findSection(doc, 'Before you start');

    const located = locateSteps(doc, steps);
    const clients = [
        ...new Set(located.flatMap((l) => (l.ref.client === null ? [] : [l.ref.client]))),
    ];
    const title = (doc.keywords.get('title') ?? '').replace(/^Test Scenario:\s*/i, '');

    return {
        id: doc.id,
        title,
        description: doc.keywords.get('description') ?? '',
        state:
            closedState(resultFields.get('status') ?? '') ??
            scenarioState(fields.get('state') ?? ''),
        target:
            linkText(fields.get('target screen') ?? '') ||
            linkText(fields.get('target dialog') ?? '') ||
            (doc.keywords.get('target_dialog') ?? ''),
        story: link(fields.get('parent story')),
        task: link(fields.get('verifies task')),
        clients,
        beforeYouStart:
            before === undefined
                ? []
                : trimBlank(sectionBody(doc, before).filter((line) => !isComment(line))),
        steps: located.map(({ ref, section, result }) => {
            const outcome = result === undefined ? new Map<string, string>() : fieldTable(result);
            return {
                ...ref,
                instructions: trimBlank(
                    sectionBody(doc, section).filter((line) => !isComment(line)),
                ),
                status: stepStatus(outcome.get('status') ?? ''),
                notes: outcome.get('notes') ?? '',
            };
        }),
        run: {
            status: (resultFields.get('status') ?? '').trim(),
            completedAt: (resultFields.get('completed at') ?? '').trim(),
            branch: (resultFields.get('branch') ?? '').trim(),
            commit: (resultFields.get('commit') ?? '').trim(),
            worktree: (resultFields.get('worktree') ?? '').trim(),
        },
    };
}

/** A table cell cannot hold a line end, and a pipe would split it. */
function cell(value: string): string {
    return value.replace(/\r?\n/g, '; ').replace(/\|/g, '/').trim();
}

interface Splice {
    readonly begin: number;
    readonly end: number;
    readonly lines: string[];
}

const ROW_FIELD = /^\|\s*([^|]*?)\s*\|/;

/**
 * Set rows of a field table in place. A row the table has keeps its field
 * cell and takes the new value. A row it lacks goes in after the last row. A
 * null value removes the row. Rows that are not named stay as they are.
 */
function setRows(
    lines: readonly string[],
    first: number,
    last: number,
    updates: readonly (readonly [string, string | null])[],
    pad: number,
): string[] {
    const body = lines.slice(first, last + 1);
    for (const [field, value] of updates) {
        const at = body.findIndex(
            (line) => ROW_FIELD.exec(line)?.[1]?.toLowerCase() === field.toLowerCase(),
        );
        if (at >= 0) {
            if (value === null) {
                body.splice(at, 1);
            } else {
                const head = /^(\|[^|]*\|)/.exec(body[at] ?? '')?.[1] ?? `| ${field} |`;
                body[at] = `${head} ${cell(value)} |`;
            }
        } else if (value !== null) {
            body.push(`| ${field.padEnd(pad)} | ${cell(value)} |`);
        }
    }
    return body;
}

/** A Result heading one level below its step, with the table the template writes. */
function newResultSection(level: number, outcome: StepOutcome): string[] {
    const stars = '*'.repeat(level);
    const rows = [`| Status | ${outcome.status} |`];
    if (outcome.notes.trim() !== '') rows.push(`| Notes  | ${cell(outcome.notes)} |`);
    return [`${stars} Result`, '', '| Field  | Value |', '|--------+-------|', ...rows, ''];
}

export interface RecordedRun {
    readonly text: string;
    /** The state the scenario has after the save. */
    readonly state: ScenarioState;
    /** The steps this save wrote, in the order the doc has them. */
    readonly changed: readonly StepRef[];
}

/**
 * Write a run into a scenario doc's text and return the new text. Only the
 * named steps' Result tables, the overall Results table and the State row
 * change. Every other line stays. A step that is missing or ambiguous refuses
 * the whole save, and the text is not changed.
 */
export function recordRun(text: string, input: RunInput): RecordedRun {
    const doc = parseOrg(text);
    if (doc.keywords.get('type') !== 'test_scenario') {
        throw new ScenarioFormatError('The document is not a test scenario.');
    }
    const stepsSection = findSection(doc, 'Steps');
    const results = findSection(doc, 'Results');
    if (stepsSection === undefined)
        throw new ScenarioFormatError('The scenario has no Steps section.');
    if (results === undefined)
        throw new ScenarioFormatError('The scenario has no Results section.');

    const located = locateSteps(doc, stepsSection);
    const splices: Splice[] = [];
    const written = new Set<Located>();

    for (const outcome of input.steps) {
        const matches = located.filter(
            (l) => l.ref.client === outcome.client && l.ref.title === outcome.title,
        );
        if (matches.length === 0) throw new UnknownStepError(outcome);
        if (matches.length > 1) throw new AmbiguousStepError(outcome);
        const [step] = matches;
        if (step === undefined) continue;
        if (written.has(step)) throw new DuplicateOutcomeError(outcome);
        written.add(step);

        const table = step.result?.tables[0];
        if (step.result !== undefined && table !== undefined) {
            splices.push({
                begin: table.firstLine,
                end: table.lastLine + 1,
                lines: setRows(
                    doc.lines,
                    table.firstLine,
                    table.lastLine,
                    [
                        ['Status', outcome.status],
                        ['Notes', outcome.notes.trim() === '' ? null : outcome.notes],
                    ],
                    6,
                ),
            });
        } else if (step.result !== undefined) {
            splices.push({
                begin: step.result.bodyStart,
                end: step.result.bodyStart,
                lines: ['', ...newResultSection(step.result.level, outcome).slice(2)],
            });
        } else {
            const at = step.section.subtreeEnd;
            const needsGap = at > 0 && (doc.lines[at - 1] ?? '').trim() !== '';
            splices.push({
                begin: at,
                end: at,
                lines: [
                    ...(needsGap ? [''] : []),
                    ...newResultSection(step.section.level + 1, outcome),
                ],
            });
        }
    }

    const outcomeOf = new Map<Located, StepStatus>();
    for (const outcome of input.steps) {
        const step = located.find(
            (l) => l.ref.client === outcome.client && l.ref.title === outcome.title,
        );
        if (step !== undefined) outcomeOf.set(step, outcome.status);
    }
    const statuses = located.map(
        (l) =>
            outcomeOf.get(l) ??
            stepStatus(l.result === undefined ? '' : (fieldTable(l.result).get('status') ?? '')),
    );
    const state: ScenarioState = statuses.includes('PENDING')
        ? 'PENDING'
        : statuses.includes('FAIL')
          ? 'FAILED'
          : 'PASSED';

    const resultsTable = results.tables[0];
    const run: [string, string][] = [
        ['Status', state],
        ['Completed at', state === 'PENDING' ? '' : input.completedAt],
        ['Branch', input.branch],
        ['Commit', input.commit],
        ['Worktree', input.worktree],
    ];
    if (resultsTable !== undefined) {
        splices.push({
            begin: resultsTable.firstLine,
            end: resultsTable.lastLine + 1,
            lines: setRows(doc.lines, resultsTable.firstLine, resultsTable.lastLine, run, 13),
        });
    } else {
        splices.push({
            begin: results.bodyStart,
            end: results.bodyStart,
            lines: [
                '',
                '| Field         | Value |',
                '|---------------+-------|',
                ...run.map(([field, value]) => `| ${field.padEnd(13)} | ${cell(value)} |`),
                '',
            ],
        });
    }

    const info = findSection(doc, 'Scenario Info');
    const infoTable = info?.tables[0];
    if (infoTable !== undefined) {
        splices.push({
            begin: infoTable.firstLine,
            end: infoTable.lastLine + 1,
            lines: setRows(
                doc.lines,
                infoTable.firstLine,
                infoTable.lastLine,
                [['State', state]],
                13,
            ),
        });
    }

    const lines = [...doc.lines];
    for (const splice of splices.sort((a, b) => b.begin - a.begin)) {
        lines.splice(splice.begin, splice.end - splice.begin, ...splice.lines);
    }
    return {
        text: lines.join(doc.eol) + doc.eol,
        state,
        changed: located.filter((l) => written.has(l)).map((l) => l.ref),
    };
}
