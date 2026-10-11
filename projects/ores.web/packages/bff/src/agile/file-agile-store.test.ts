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
import { existsSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join, resolve } from 'node:path';
import { afterEach, beforeEach, describe, expect, it } from 'vitest';
import { FileAgileStore } from './file-agile-store.js';
import { InvalidSprintNameError } from './agile-store.js';

let root: string;

beforeEach(async () => {
    root = await mkdtemp(join(tmpdir(), 'agile-store-'));
});
afterEach(async () => {
    await rm(root, { recursive: true, force: true });
});

function org(id: string, keywords: Record<string, string>, state?: string): string {
    const lines = [':PROPERTIES:', `:ID: ${id}`, ':END:'];
    for (const [key, value] of Object.entries(keywords)) lines.push(`#+${key}: ${value}`);
    if (state !== undefined) {
        lines.push('', '* Status', '', '| Field | Value |', '|-+-|', `| State | ${state} |`);
    }
    return `${lines.join('\n')}\n`;
}

async function put(path: string, text: string): Promise<void> {
    const file = join(root, path);
    await mkdir(join(file, '..'), { recursive: true });
    await writeFile(file, text);
}

async function sprint(name: string, version = 'v0'): Promise<void> {
    await put(
        `agile/versions/${version}/${name}/sprint.org`,
        org('11111111-1111-4111-8111-111111111111', {
            title: `Sprint ${name.slice(7)}`,
            start_date: '2026-10-05',
            end_date: '2026-10-12',
        }),
    );
}

const NOW = () => new Date('2026-10-11T12:00:00Z');

describe('the sprints', () => {
    it('are listed newest first, with 9 before 10', async () => {
        await sprint('sprint_9');
        await sprint('sprint_10');
        await sprint('sprint_2');
        const store = new FileAgileStore(root, { now: NOW });
        expect((await store.sprints()).map((ref) => ref.name)).toEqual([
            'sprint_10',
            'sprint_9',
            'sprint_2',
        ]);
    });

    it('are none when the tree has no agile folder', async () => {
        const store = new FileAgileStore(root, { now: NOW });
        expect(await store.sprints()).toEqual([]);
        expect(await store.board()).toBeNull();
    });
});

describe('a board', () => {
    it('reads the stories and tasks of the newest sprint by default', async () => {
        await sprint('sprint_26');
        await sprint('sprint_27');
        await put(
            'agile/versions/v0/sprint_27/alpha/story.org',
            org('aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa', { title: 'Story: Alpha' }, 'STARTED'),
        );
        await put(
            'agile/versions/v0/sprint_27/alpha/task_one.org',
            org('c1', { title: 'Task: One', branch: 'feature/one', pr: '5' }, 'DONE'),
        );
        await put('agile/versions/v0/sprint_27/alpha/notes.org', 'not a task');
        await put(
            'agile/versions/v0/sprint_26/old/story.org',
            org('bbbbbbbb-bbbb-4bbb-8bbb-bbbbbbbbbbbb', { title: 'Story: Old' }, 'DONE'),
        );
        const store = new FileAgileStore(root, { now: NOW });
        const board = await store.board();
        expect(board?.sprint).toMatchObject({ name: 'sprint_27', dayOfSprint: 7 });
        expect(board?.stories.map((story) => story.title)).toEqual(['Alpha']);
        expect(board?.stories[0]?.tasks.map((task) => task.title)).toEqual(['One']);
        expect((await store.board('sprint_26'))?.stories.map((story) => story.title)).toEqual([
            'Old',
        ]);
    });

    it('is null for a sprint that does not exist', async () => {
        await sprint('sprint_27');
        expect(await new FileAgileStore(root, { now: NOW }).board('sprint_5')).toBeNull();
    });

    it('refuses a name that is not a sprint folder name, so a path is never read', async () => {
        const store = new FileAgileStore(root, { now: NOW });
        for (const name of ['../sprint_27', 'sprint_27/../..', '/etc', 'sprint_', 'x']) {
            await expect(store.board(name), name).rejects.toBeInstanceOf(InvalidSprintNameError);
        }
    });

    it('skips a story folder that is a link, and a story file that is a link', async () => {
        await sprint('sprint_27');
        await put(
            'outside/story.org',
            org('eeeeeeee-eeee-4eee-8eee-eeeeeeeeeeee', { title: 'Story: Out' }, 'STARTED'),
        );
        await symlink(join(root, 'outside'), join(root, 'agile/versions/v0/sprint_27/linked'));
        await put(
            'real/story.org',
            org('ffffffff-ffff-4fff-8fff-ffffffffffff', { title: 'Story: Real' }, 'STARTED'),
        );
        await mkdir(join(root, 'agile/versions/v0/sprint_27/filelink'), { recursive: true });
        await symlink(
            join(root, 'real/story.org'),
            join(root, 'agile/versions/v0/sprint_27/filelink/story.org'),
        );
        const board = await new FileAgileStore(root, { now: NOW }).board();
        expect(board?.stories).toEqual([]);
    });

    it('shows an edit at the next read', async () => {
        await sprint('sprint_27');
        const path = 'agile/versions/v0/sprint_27/alpha/story.org';
        await put(
            path,
            org('aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa', { title: 'Story: Alpha' }, 'STARTED'),
        );
        const store = new FileAgileStore(root, { now: NOW });
        expect((await store.board())?.stories[0]?.state).toBe('STARTED');
        await put(
            path,
            org('aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa', { title: 'Story: Alpha edited' }, 'DONE'),
        );
        const again = (await store.board())?.stories[0];
        expect(again).toMatchObject({ state: 'DONE', title: 'Alpha edited' });
    });
});

describe('the backlog', () => {
    it('lists the four buckets, each newest first, with captures and story folders', async () => {
        await put(
            'agile/product_backlog/inbox/old.org',
            org('a1', { title: 'Old capture', created: '2026-09-01' }),
        );
        await put(
            'agile/product_backlog/inbox/new.org',
            org('a2', { title: 'New capture', created: '2026-10-11', description: 'Why.' }),
        );
        await put(
            'agile/product_backlog/next/big/story.org',
            org('a3', { title: 'Story: Big', created: '2026-10-01' }, 'BACKLOG'),
        );
        await put(
            'agile/product_backlog/next/big/task_one.org',
            org('a4', { title: 'Task: One' }, 'BACKLOG'),
        );
        await put(
            'agile/product_backlog/next/big/task_two.org',
            org('a5', { title: 'Task: Two' }, 'BACKLOG'),
        );
        await put('agile/product_backlog/inbox.org', 'the index is not a capture');
        const backlog = await new FileAgileStore(root, { now: NOW }).backlog();
        expect(backlog.buckets.map((bucket) => bucket.id)).toEqual([
            'inbox',
            'next',
            'deferred',
            'discarded',
        ]);
        expect(backlog.buckets[0]?.items.map((item) => item.title)).toEqual([
            'New capture',
            'Old capture',
        ]);
        expect(backlog.buckets[0]?.items[0]).toMatchObject({
            kind: 'capture',
            description: 'Why.',
            tasks: 0,
        });
        expect(backlog.buckets[1]?.items[0]).toMatchObject({
            kind: 'story',
            title: 'Big',
            tasks: 2,
        });
        expect(backlog.buckets[2]?.items).toEqual([]);
    });
});

const REAL_DOC = resolve(__dirname, '../../../../../../doc');

describe.skipIf(!existsSync(join(REAL_DOC, 'agile', 'versions')))('the real agile tree', () => {
    it('reads the newest sprint with stories that all have an id, a title and a state', async () => {
        const store = new FileAgileStore(REAL_DOC);
        const board = await store.board();
        expect(board).not.toBeNull();
        expect(board?.stories.length).toBeGreaterThan(0);
        for (const story of board?.stories ?? []) {
            expect(story.id, story.path).toMatch(/^[0-9A-F-]{36}$/);
            expect(story.title, story.path).not.toBe('');
            expect(story.state, story.path).not.toBe('UNKNOWN');
        }
        expect((await store.backlog()).buckets.every((bucket) => bucket.items.length >= 0)).toBe(
            true,
        );
    });
});
