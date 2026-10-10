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

import { randomUUID } from 'node:crypto';
import {
    chmod,
    open,
    readdir,
    readFile,
    realpath,
    rename,
    stat,
    unlink,
    writeFile,
} from 'node:fs/promises';
import { dirname, join, sep } from 'node:path';
import { parseOrg } from '@ores/org';
import { parseScenario, recordRun, type RunInput, type Scenario } from './scenario-doc.js';
import {
    InvalidDocIdError,
    PathEscapeError,
    ScenarioNotFoundError,
    type DocText,
    type RecordResult,
    type ScenarioStore,
    type ScenarioSummary,
} from './scenario-store.js';

export interface FileStoreOptions {
    /** The least time between two scans of the tree. Zero scans on every miss. */
    readonly rescanAfterMs?: number;
}

interface IndexEntry {
    readonly path: string;
    readonly title: string;
    readonly type: string | null;
    readonly mtimeMs: number;
    readonly size: number;
}

const HEAD_BYTES = 4096;
const SCAN_BATCH = 64;
const DOC_ID = /^[0-9A-Fa-f]{8}(?:-[0-9A-Fa-f]{4}){3}-[0-9A-Fa-f]{12}$/;

/** A directory a scan does not enter. */
function skipDirectory(name: string): boolean {
    return name === 'node_modules' || name.startsWith('.');
}

async function readHead(file: string): Promise<string> {
    const handle = await open(file, 'r');
    try {
        const buffer = Buffer.alloc(HEAD_BYTES);
        const { bytesRead } = await handle.read(buffer, 0, HEAD_BYTES, 0);
        return buffer.toString('utf8', 0, bytesRead);
    } finally {
        await handle.close();
    }
}

/**
 * Scenarios kept as org files under one root. The files are the ones the
 * compass test_scenario template creates. A scan reads the head of every
 * .org file under the root to learn its :ID:, and a lookup by id finds only
 * a file the scan found, so no id can name a file outside the root. A scan
 * reads only regular files and real directories, so it never follows a
 * symbolic link.
 */
export class FileScenarioStore implements ScenarioStore {
    private readonly root: string;
    private readonly rescanAfterMs: number;
    private realRoot: string | undefined;
    private index = new Map<string, IndexEntry>();
    private lastScan = Number.NEGATIVE_INFINITY;
    private readonly queues = new Map<string, Promise<unknown>>();
    private readonly summaries = new Map<
        string,
        { readonly mtimeMs: number; readonly size: number; readonly summary: ScenarioSummary }
    >();

    constructor(root: string, options: FileStoreOptions = {}) {
        this.root = root;
        this.rescanAfterMs = options.rescanAfterMs ?? 10_000;
    }

    async list(): Promise<ScenarioSummary[]> {
        await this.scan();
        const out: ScenarioSummary[] = [];
        for (const [id, entry] of this.index) {
            if (entry.type !== 'test_scenario') continue;
            const cached = this.summaries.get(entry.path);
            if (cached?.mtimeMs === entry.mtimeMs && cached.size === entry.size) {
                out.push(cached.summary);
                continue;
            }
            let scenario: Scenario;
            try {
                scenario = parseScenario(await this.readFile(entry.path));
            } catch {
                continue;
            }
            const summary = summarise(id, entry.path, scenario);
            this.summaries.set(entry.path, {
                mtimeMs: entry.mtimeMs,
                size: entry.size,
                summary,
            });
            out.push(summary);
        }
        return out.sort((a, b) => a.path.localeCompare(b.path));
    }

    async read(id: string): Promise<Scenario | null> {
        const doc = await this.readDoc(id);
        if (doc === null || doc.type !== 'test_scenario') return null;
        return parseScenario(doc.text);
    }

    async readDoc(id: string): Promise<DocText | null> {
        const key = normalise(id);
        const found = await this.resolve(key);
        if (found === null) return null;
        return {
            id: key,
            title: found.entry.title,
            type: found.entry.type,
            path: found.entry.path,
            text: found.text,
        };
    }

    async record(id: string, input: RunInput): Promise<RecordResult> {
        const key = normalise(id);
        const entry = this.index.get(key);
        const queueKey = entry?.path ?? key;
        const previous = this.queues.get(queueKey) ?? Promise.resolve();
        const next = previous.catch(() => undefined).then(() => this.write(key, input));
        this.queues.set(queueKey, next);
        const clear = () => {
            if (this.queues.get(queueKey) === next) this.queues.delete(queueKey);
        };
        next.then(clear, clear);
        return next;
    }

    private async write(key: string, input: RunInput): Promise<RecordResult> {
        const found = await this.resolve(key);
        if (found === null || found.entry.type !== 'test_scenario') {
            throw new ScenarioNotFoundError(key);
        }
        const recorded = recordRun(found.text, input);
        const absolute = await this.absolute(found.entry.path);
        const mode = (await stat(absolute)).mode;
        const temporary = join(dirname(absolute), `.${randomUUID()}.tmp`);
        try {
            await writeFile(temporary, recorded.text, { encoding: 'utf8', flag: 'wx' });
            await chmod(temporary, mode & 0o777);
            await rename(temporary, absolute);
            const written = await stat(absolute);
            this.index.set(key, { ...found.entry, mtimeMs: written.mtimeMs, size: written.size });
        } catch (error) {
            await unlink(temporary).catch(() => undefined);
            throw error;
        }
        return { scenario: parseScenario(recorded.text), changed: recorded.changed };
    }

    private async rootPath(): Promise<string> {
        this.realRoot ??= await realpath(this.root);
        return this.realRoot;
    }

    /** The real path of an indexed file, which must sit inside the root. */
    private async absolute(path: string): Promise<string> {
        const root = await this.rootPath();
        const real = await realpath(join(root, path));
        if (real !== root && !real.startsWith(root + sep)) throw new PathEscapeError(path);
        return real;
    }

    private async readFile(path: string): Promise<string> {
        return readFile(await this.absolute(path), 'utf8');
    }

    /** Find a doc by id and read it. A miss rescans the tree once. */
    private async resolve(key: string): Promise<{ entry: IndexEntry; text: string } | null> {
        for (let attempt = 0; attempt < 2; attempt += 1) {
            const entry = this.index.get(key);
            if (entry !== undefined) {
                try {
                    return { entry, text: await this.readFile(entry.path) };
                } catch (error) {
                    if ((error as NodeJS.ErrnoException).code !== 'ENOENT') throw error;
                    this.index.delete(key);
                }
            }
            if (!(await this.scan())) return null;
        }
        return null;
    }

    /** Scan the tree for .org files. Returns false when the last scan is too recent. */
    private async scan(): Promise<boolean> {
        if (Date.now() - this.lastScan < this.rescanAfterMs) return false;
        this.lastScan = Date.now();
        const root = await this.rootPath();
        const files: string[] = [];
        const walk = async (relative: string): Promise<void> => {
            const entries = await readdir(join(root, relative), { withFileTypes: true });
            for (const entry of entries) {
                const path = relative === '' ? entry.name : `${relative}/${entry.name}`;
                if (entry.isDirectory()) {
                    if (!skipDirectory(entry.name)) await walk(path);
                } else if (entry.isFile() && entry.name.endsWith('.org')) {
                    files.push(path);
                }
            }
        };
        await walk('');
        files.sort();

        const known = new Map<string, [string, IndexEntry]>();
        for (const [id, entry] of this.index) known.set(entry.path, [id, entry]);

        const next = new Map<string, IndexEntry>();
        for (let at = 0; at < files.length; at += SCAN_BATCH) {
            const batch = files.slice(at, at + SCAN_BATCH);
            const found = await Promise.all(
                batch.map(async (path): Promise<[string, IndexEntry] | null> => {
                    const file = join(root, path);
                    const { mtimeMs, size } = await stat(file);
                    const before = known.get(path);
                    if (before?.[1].mtimeMs === mtimeMs && before[1].size === size) return before;
                    const doc = parseOrg(await readHead(file).catch(() => ''));
                    if (doc.id === null) return null;
                    return [
                        doc.id,
                        {
                            path,
                            title: doc.keywords.get('title') ?? '',
                            type: doc.keywords.get('type') ?? null,
                            mtimeMs,
                            size,
                        },
                    ];
                }),
            );
            for (const item of found) {
                if (item !== null && !next.has(item[0])) next.set(item[0], item[1]);
            }
        }
        this.index = next;
        return true;
    }
}

function normalise(id: string): string {
    if (!DOC_ID.test(id)) throw new InvalidDocIdError(id);
    return id.toUpperCase();
}

function summarise(id: string, path: string, scenario: Scenario): ScenarioSummary {
    const count = (status: string) => scenario.steps.filter((s) => s.status === status).length;
    return {
        id,
        title: scenario.title,
        description: scenario.description,
        path,
        state: scenario.state,
        phase: scenario.state === 'PENDING' ? 'waiting' : 'done',
        target: scenario.target,
        story: scenario.story,
        task: scenario.task,
        clients: scenario.clients,
        steps: {
            total: scenario.steps.length,
            pending: count('PENDING'),
            pass: count('PASS'),
            fail: count('FAIL'),
            dropped: count('DROPPED'),
        },
        completedAt: scenario.run.completedAt,
    };
}
