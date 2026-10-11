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

import { execFile } from 'node:child_process';
import { constants } from 'node:fs';
import { open, realpath } from 'node:fs/promises';
import { join } from 'node:path';
import { normalizeState, type AgileState } from './agile-model.js';

/**
 * The work trees of the repository and the work item each is on. It replaces
 * the fleet strip of ores.dsh_kanban. A tree's work item is the last entry of
 * its `.journal.org`, which compass writes, so no other tree's agile docs are
 * read. The list of trees comes from git, and a caller never names a path.
 */

export interface JournalEntry {
    readonly storyId: string;
    readonly storyTitle: string;
    readonly taskId: string;
    readonly taskTitle: string;
    readonly state: AgileState;
    readonly branch: string;
    /** The pull request number, or 0 when the entry has none. */
    readonly pr: number;
}

export interface WorkTree {
    /** The directory name without the `ores_dev_` prefix, as `#+environment:` writes it. */
    readonly label: string;
    readonly branch: string;
    /** This is the work tree the BFF reads the agile tree of. */
    readonly isSession: boolean;
    /** The work item of the tree, or null when its journal has none. */
    readonly current: JournalEntry | null;
}

export interface WorkTreeSource {
    /** Every work tree of the repository, sorted by label and capped. */
    trees(): Promise<WorkTree[]>;
}

/** The most trees one answer carries. */
export const MAX_TREES = 40;

const LINK = /\[\[id:([0-9A-Fa-f-]+)\]\[([^\]]*)\]\]/;

/** The last entry of a journal, as text, or null when there is none. */
function lastEntry(journal: string): string | null {
    const lines = journal.split('\n');
    for (let index = lines.length - 1; index >= 0; index -= 1) {
        if (/^\* /.test(lines[index] ?? '')) return lines.slice(index).join('\n');
    }
    return null;
}

/**
 * The work item of the last entry of a journal. The story is the heading's
 * link and the task is the `Task` line's link. An entry with neither is no
 * work item.
 */
export function parseJournalEntry(journal: string): JournalEntry | null {
    const entry = lastEntry(journal);
    if (entry === null) return null;
    const [heading = '', ...rest] = entry.split('\n');
    const story = LINK.exec(heading);
    const field = (name: string): string =>
        rest
            .find((line) => line.trim().startsWith(`- ${name} ::`))
            ?.trim()
            .slice(`- ${name} ::`.length)
            .trim() ?? '';
    const task = LINK.exec(field('Task'));
    if (story === null && task === null) return null;
    const pr = /^#?(\d+)$/.exec(field('PR'));
    return {
        storyId: (story?.[1] ?? '').toUpperCase(),
        storyTitle: story?.[2] ?? '',
        taskId: (task?.[1] ?? '').toUpperCase(),
        taskTitle: task?.[2] ?? '',
        state: normalizeState(field('State')),
        branch: field('Branch'),
        pr: pr?.[1] === undefined ? 0 : Number(pr[1]),
    };
}

export interface ListedTree {
    readonly root: string;
    readonly branch: string;
}

/** The trees of `git worktree list --porcelain`, leaving out the bare repository. */
export function parseWorktreeList(output: string): ListedTree[] {
    const trees: ListedTree[] = [];
    for (const block of output.split('\n\n')) {
        const lines = block.split('\n');
        const root = lines.find((line) => line.startsWith('worktree '))?.slice('worktree '.length);
        if (root === undefined || root === '' || lines.includes('bare')) continue;
        const branch = lines.find((line) => line.startsWith('branch refs/heads/'));
        trees.push({
            root,
            branch: branch === undefined ? '' : branch.slice('branch refs/heads/'.length),
        });
    }
    return trees;
}

/** The label of a work tree: its directory name without the `ores_dev_` prefix. */
export function labelOf(root: string): string {
    const name = root.replace(/\/+$/, '').split('/').pop() ?? '';
    return name.startsWith('ores_dev_') ? name.slice('ores_dev_'.length) : name;
}

/** Runs git with fixed arguments in a directory and answers its output, or null on any failure. */
export type GitRunner = (args: readonly string[], cwd: string) => Promise<string | null>;

const GIT_TIMEOUT_MS = 10_000;
const JOURNAL_TAIL_BYTES = 64 * 1024;

export const runGit: GitRunner = (args, cwd) =>
    new Promise((resolve) => {
        execFile(
            'git',
            [...args],
            { cwd, timeout: GIT_TIMEOUT_MS, maxBuffer: 8 * 1024 * 1024 },
            (error, stdout) => resolve(error === null ? stdout : null),
        );
    });

/**
 * The tail of a tree's journal. The journal only grows, so the last entry is in
 * the tail. A journal that is a link, or is missing, reads as none.
 */
async function readJournalTail(root: string): Promise<string | null> {
    let handle;
    try {
        handle = await open(join(root, '.journal.org'), constants.O_RDONLY | constants.O_NOFOLLOW);
    } catch {
        return null;
    }
    try {
        const info = await handle.stat();
        if (!info.isFile()) return null;
        const start = Math.max(0, info.size - JOURNAL_TAIL_BYTES);
        const buffer = Buffer.alloc(info.size - start);
        await handle.read(buffer, 0, buffer.length, start);
        const text = buffer.toString('utf8');
        // A cut tail starts in the middle of a line, so the first line is dropped.
        return start > 0 ? text.slice(text.indexOf('\n') + 1) : text;
    } catch {
        return null;
    } finally {
        await handle.close();
    }
}

/** The real path of a directory, or the path as given when it cannot be resolved. */
async function resolved(path: string): Promise<string> {
    try {
        return await realpath(path);
    } catch {
        return path;
    }
}

export class GitWorkTreeSource implements WorkTreeSource {
    private readonly docRoot: string;
    private readonly git: GitRunner;

    constructor(docRoot: string, git: GitRunner = runGit) {
        this.docRoot = docRoot;
        this.git = git;
    }

    async trees(): Promise<WorkTree[]> {
        const [top, list] = await Promise.all([
            this.git(['rev-parse', '--show-toplevel'], this.docRoot),
            this.git(['worktree', 'list', '--porcelain'], this.docRoot),
        ]);
        if (list === null) return [];
        // Git can print the same directory in two forms when the checkout is
        // reached through a link, so the two are compared by their real paths.
        const own = await resolved(top?.trim() ?? '');
        const listed = parseWorktreeList(list)
            .map((tree) => ({ ...tree, label: labelOf(tree.root) }))
            .sort((a, b) => (a.label < b.label ? -1 : a.label > b.label ? 1 : 0))
            .slice(0, MAX_TREES);
        return Promise.all(
            listed.map(async (tree) => {
                const [journal, root] = await Promise.all([
                    readJournalTail(tree.root),
                    resolved(tree.root),
                ]);
                return {
                    label: tree.label,
                    branch: tree.branch,
                    isSession: root === own,
                    current: journal === null ? null : parseJournalEntry(journal),
                };
            }),
        );
    }
}
