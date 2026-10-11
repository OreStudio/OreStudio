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

import { mkdir, mkdtemp, rm, symlink, writeFile } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { afterEach, beforeEach, describe, expect, it } from 'vitest';
import {
    GitWorkTreeSource,
    MAX_TREES,
    labelOf,
    parseJournalEntry,
    parseWorktreeList,
    type GitRunner,
} from './work-trees.js';

const ENTRY = `* 2026-10-11 19:12 — [[id:62313bc5-7e9d-4aaf-b74c-53ae63b3d943][Browse the agile tree in ores.web]]
  - Task :: [[id:aaaaaaaa-0000-4000-8000-000000000001][Show the work trees on the board]]
  - State :: STARTED
  - Branch :: feature/show-the-work-trees-on-the-board
  - PR :: #3140
`;

describe('parseJournalEntry', () => {
    it('reads the story, the task, the state, the branch and the pull request of the last entry', () => {
        const old = ENTRY.replace('Show the work trees', 'Old task').replace('STARTED', 'DONE');
        expect(parseJournalEntry(`${old}\n${ENTRY}`)).toEqual({
            storyId: '62313BC5-7E9D-4AAF-B74C-53AE63B3D943',
            storyTitle: 'Browse the agile tree in ores.web',
            taskId: 'AAAAAAAA-0000-4000-8000-000000000001',
            taskTitle: 'Show the work trees on the board',
            state: 'STARTED',
            branch: 'feature/show-the-work-trees-on-the-board',
            pr: 3140,
        });
    });

    it('gives pull request zero for none, and an unknown state for a word it does not know', () => {
        const entry = parseJournalEntry(ENTRY.replace('#3140', 'none').replace('STARTED', 'LATER'));
        expect(entry).toMatchObject({ pr: 0, state: 'UNKNOWN' });
    });

    it('is null for an empty journal or an entry with no link', () => {
        expect(parseJournalEntry('')).toBeNull();
        expect(parseJournalEntry('* 2026-10-11 19:12 — nothing\n  - State :: DONE\n')).toBeNull();
    });
});

describe('parseWorktreeList', () => {
    const porcelain = [
        'worktree /r/ores.git\nbare',
        'worktree /r/ores_dev_a\nHEAD abc\nbranch refs/heads/feature/a',
        'worktree /r/ores_dev_b\nHEAD def\ndetached',
    ].join('\n\n');

    it('lists each tree with its branch, leaves out the bare repository, and has no branch when detached', () => {
        expect(parseWorktreeList(porcelain)).toEqual([
            { root: '/r/ores_dev_a', branch: 'feature/a' },
            { root: '/r/ores_dev_b', branch: '' },
        ]);
    });
});

describe('labelOf', () => {
    it('drops the ores_dev_ prefix and a trailing slash', () => {
        expect(labelOf('/r/ores_dev_eager_maxwell/')).toBe('eager_maxwell');
        expect(labelOf('/r/tmp_hotfix_bff')).toBe('tmp_hotfix_bff');
    });
});

describe('GitWorkTreeSource', () => {
    let base: string;
    beforeEach(async () => {
        base = await mkdtemp(join(tmpdir(), 'trees-'));
    });
    afterEach(async () => {
        await rm(base, { recursive: true, force: true });
    });

    async function tree(name: string, journal?: string): Promise<string> {
        const root = join(base, name);
        await mkdir(root, { recursive: true });
        if (journal !== undefined) await writeFile(join(root, '.journal.org'), journal);
        return root;
    }

    const runner =
        (own: string, roots: readonly { root: string; branch: string }[]): GitRunner =>
        async (args) =>
            args[0] === 'rev-parse'
                ? `${own}\n`
                : roots
                      .map((entry) => `worktree ${entry.root}\nbranch refs/heads/${entry.branch}`)
                      .join('\n\n');

    it('lists the trees by label, marks its own, and reads each journal', async () => {
        const b = await tree('ores_dev_b', ENTRY);
        const a = await tree('ores_dev_a');
        const source = new GitWorkTreeSource(
            '/doc',
            runner(b, [
                { root: b, branch: 'feature/b' },
                { root: a, branch: 'feature/a' },
            ]),
        );
        const trees = await source.trees();
        expect(trees.map((entry) => entry.label)).toEqual(['a', 'b']);
        expect(trees[0]).toMatchObject({ isSession: false, current: null, branch: 'feature/a' });
        expect(trees[1]).toMatchObject({ isSession: true });
        expect(trees[1]?.current?.taskTitle).toBe('Show the work trees on the board');
    });

    it('reads no journal that is a link', async () => {
        const real = await tree('real', ENTRY);
        const root = await tree('ores_dev_c');
        await symlink(join(real, '.journal.org'), join(root, '.journal.org'));
        const trees = await new GitWorkTreeSource(
            '/doc',
            runner(root, [{ root, branch: 'feature/c' }]),
        ).trees();
        expect(trees[0]?.current).toBeNull();
    });

    it('reads the end of a long journal', async () => {
        const filler =
            `* 2026-01-01 00:00 — [[id:11111111-0000-4000-8000-000000000000][Old]]\n${'x'.repeat(100)}\n`.repeat(
                2000,
            );
        const root = await tree('ores_dev_d', `${filler}${ENTRY}`);
        const trees = await new GitWorkTreeSource(
            '/doc',
            runner(root, [{ root, branch: 'feature/d' }]),
        ).trees();
        expect(trees[0]?.current?.taskTitle).toBe('Show the work trees on the board');
    });

    it('caps the list, and is empty when git cannot run', async () => {
        const many = Array.from({ length: MAX_TREES + 5 }, (_, i) => ({
            root: join(base, `ores_dev_t${String(i).padStart(3, '0')}`),
            branch: 'b',
        }));
        expect(await new GitWorkTreeSource('/doc', runner('', many)).trees()).toHaveLength(
            MAX_TREES,
        );
        expect(await new GitWorkTreeSource('/doc', async () => null).trees()).toEqual([]);
    });
});
