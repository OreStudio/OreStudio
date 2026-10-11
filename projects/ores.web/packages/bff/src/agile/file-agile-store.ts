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

import { lstat, readdir, readFile } from 'node:fs/promises';
import { join } from 'node:path';
import { parseOrg } from '@ores/org';
import { buildBoard, type Board, type StoryFiles } from './agile-model.js';
import {
    BACKLOG_BUCKETS,
    InvalidSprintNameError,
    type AgileStore,
    type Backlog,
    type BacklogItem,
    type SprintRef,
} from './agile-store.js';

export interface FileAgileStoreOptions {
    /** The clock that gives the day of the sprint. Tests set it. */
    readonly now?: () => Date;
}

const SPRINT_NAME = /^sprint_\d+$/;
const VERSION_NAME = /^v\d+$/;
const TASK_FILE = /^task_(.+)\.org$/;

/** A sort key that puts `sprint_9` before `sprint_10`. */
function numberOf(name: string): number {
    return Number(name.replace(/\D+/g, ''));
}

/**
 * The agile tree kept as org files under one doc root, in the layout compass
 * creates: `agile/versions/<version>/<sprint>/sprint.org`, a folder for each
 * story with its `story.org` and `task_*.org`, and the backlog buckets under
 * `agile/product_backlog/`. Every path is built from a name that a directory
 * listing returned and that matched a strict pattern, so a caller's text never
 * becomes a path. Only regular files and real directories are read, so a
 * symbolic link is never followed.
 *
 * File text is cached by modification time and size, so a board that nobody
 * edited costs one stat for each file.
 */
export class FileAgileStore implements AgileStore {
    private readonly root: string;
    private readonly now: () => Date;
    private readonly texts = new Map<
        string,
        { readonly mtimeMs: number; readonly size: number; readonly text: string }
    >();

    constructor(root: string, options: FileAgileStoreOptions = {}) {
        this.root = root;
        this.now = options.now ?? (() => new Date());
    }

    async sprints(): Promise<SprintRef[]> {
        const base = join(this.root, 'agile', 'versions');
        const out: SprintRef[] = [];
        for (const version of await this.directories(base, VERSION_NAME)) {
            for (const name of await this.directories(join(base, version), SPRINT_NAME)) {
                const path = `agile/versions/${version}/${name}/sprint.org`;
                const text = await this.readText(join(base, version, name, 'sprint.org'));
                if (text === null) continue;
                const title = parseOrg(text).keywords.get('title') ?? name;
                out.push({ version, name, title, path });
            }
        }
        return out.sort(
            (a, b) =>
                numberOf(b.version) - numberOf(a.version) || numberOf(b.name) - numberOf(a.name),
        );
    }

    async board(sprint?: string): Promise<Board | null> {
        if (sprint !== undefined && !SPRINT_NAME.test(sprint)) {
            throw new InvalidSprintNameError(sprint);
        }
        const all = await this.sprints();
        const found = sprint === undefined ? all[0] : all.find((ref) => ref.name === sprint);
        if (found === undefined) return null;
        const dir = join(this.root, 'agile', 'versions', found.version, found.name);
        const sprintText = await this.readText(join(dir, 'sprint.org'));
        if (sprintText === null) return null;
        const stories: StoryFiles[] = [];
        for (const slug of await this.directories(dir)) {
            const story = await this.readText(join(dir, slug, 'story.org'));
            if (story === null) continue;
            const tasks: { slug: string; text: string }[] = [];
            for (const file of await this.files(join(dir, slug), TASK_FILE)) {
                const text = await this.readText(join(dir, slug, file));
                const taskSlug = TASK_FILE.exec(file)?.[1];
                if (text !== null && taskSlug !== undefined) tasks.push({ slug: taskSlug, text });
            }
            stories.push({
                slug,
                dirPath: `agile/versions/${found.version}/${found.name}/${slug}/`,
                story,
                tasks,
            });
        }
        return buildBoard({
            version: found.version,
            name: found.name,
            path: found.path,
            sprint: sprintText,
            stories,
            today: this.now().toISOString().slice(0, 10),
        });
    }

    async backlog(): Promise<Backlog> {
        const buckets = [];
        for (const id of BACKLOG_BUCKETS) {
            const dir = join(this.root, 'agile', 'product_backlog', id);
            const prefix = `agile/product_backlog/${id}/`;
            const items: BacklogItem[] = [];
            for (const file of await this.files(dir, /\.org$/)) {
                const text = await this.readText(join(dir, file));
                if (text === null) continue;
                items.push(summarise(text, `${prefix}${file}`, 'capture', 0));
            }
            for (const slug of await this.directories(dir)) {
                const text = await this.readText(join(dir, slug, 'story.org'));
                if (text === null) continue;
                const tasks = (await this.files(join(dir, slug), TASK_FILE)).length;
                items.push(summarise(text, `${prefix}${slug}/story.org`, 'story', tasks));
            }
            items.sort(
                (a, b) =>
                    (a.created < b.created ? 1 : a.created > b.created ? -1 : 0) ||
                    a.title.localeCompare(b.title),
            );
            buckets.push({ id, items });
        }
        return { buckets };
    }

    /** The real directories in a folder, by name, and those that match the pattern when one is given. */
    private async directories(dir: string, pattern?: RegExp): Promise<string[]> {
        return this.entries(dir, (entry) => entry.isDirectory(), pattern);
    }

    /** The regular files in a folder, by name, that match the pattern. */
    private async files(dir: string, pattern: RegExp): Promise<string[]> {
        return this.entries(dir, (entry) => entry.isFile(), pattern);
    }

    private async entries(
        dir: string,
        keep: (entry: import('node:fs').Dirent) => boolean,
        pattern?: RegExp,
    ): Promise<string[]> {
        let entries: import('node:fs').Dirent[];
        try {
            entries = await readdir(dir, { withFileTypes: true });
        } catch (error) {
            if ((error as NodeJS.ErrnoException).code === 'ENOENT') return [];
            throw error;
        }
        return entries
            .filter(
                (entry) =>
                    keep(entry) &&
                    !entry.name.startsWith('.') &&
                    (pattern === undefined || pattern.test(entry.name)),
            )
            .map((entry) => entry.name)
            .sort();
    }

    /** The text of a regular file, or null when there is none. A link is not a file. */
    private async readText(file: string): Promise<string | null> {
        let info;
        try {
            info = await lstat(file);
        } catch (error) {
            if ((error as NodeJS.ErrnoException).code === 'ENOENT') return null;
            throw error;
        }
        if (!info.isFile()) return null;
        const cached = this.texts.get(file);
        if (cached?.mtimeMs === info.mtimeMs && cached.size === info.size) return cached.text;
        const text = await readFile(file, 'utf8');
        this.texts.set(file, { mtimeMs: info.mtimeMs, size: info.size, text });
        return text;
    }
}

function summarise(
    text: string,
    path: string,
    kind: BacklogItem['kind'],
    tasks: number,
): BacklogItem {
    const doc = parseOrg(text);
    const title = (doc.keywords.get('title') ?? '').replace(/^(Story|Task):\s*/i, '').trim();
    return {
        id: (doc.id ?? '').toUpperCase(),
        title: title === '' ? (path.split('/').pop() ?? path) : title,
        description: (doc.keywords.get('description') ?? '').trim(),
        created: (doc.keywords.get('created') ?? '').trim(),
        kind,
        path,
        tasks,
    };
}
