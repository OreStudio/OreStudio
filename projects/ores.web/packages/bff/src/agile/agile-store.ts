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

import type { Charts } from './agile-charts.js';
import type { Board } from './agile-model.js';

/**
 * Where the agile tree is read from. The BFF talks to this interface, so a
 * database store can replace the files. No implementation calls compass, and
 * none writes: compass stays the only way an agile doc changes.
 */

export interface SprintRef {
    readonly version: string;
    /** The sprint folder, such as `sprint_27`. */
    readonly name: string;
    readonly title: string;
    /** The path of the sprint doc under the doc root, with forward slashes. */
    readonly path: string;
}

export const BACKLOG_BUCKETS = ['inbox', 'next', 'deferred', 'discarded'] as const;
export type BacklogBucketId = (typeof BACKLOG_BUCKETS)[number];

export interface BacklogItem {
    readonly id: string;
    readonly title: string;
    readonly description: string;
    readonly created: string;
    /** A capture is one doc. A story is a folder with a story doc and tasks. */
    readonly kind: 'capture' | 'story';
    readonly path: string;
    /** The tasks of a story, and zero for a capture. */
    readonly tasks: number;
}

export interface BacklogBucket {
    readonly id: BacklogBucketId;
    readonly items: readonly BacklogItem[];
}

export interface Backlog {
    readonly buckets: readonly BacklogBucket[];
}

export interface AgileStore {
    /** The sprints under the doc root, newest first. */
    sprints(): Promise<SprintRef[]>;
    /**
     * One sprint's board, or null when no sprint has the name. With no name
     * the newest sprint answers, and null means there is none.
     */
    board(sprint?: string): Promise<Board | null>;
    /**
     * The chart series of one sprint, or null when no sprint has the name. The
     * velocity runs over every sprint. With no name the newest sprint answers.
     */
    charts(sprint?: string): Promise<Charts | null>;
    /** The four backlog buckets, each newest first. */
    backlog(): Promise<Backlog>;
}

/** The sprint name is not a sprint folder name. A path is never a name. */
export class InvalidSprintNameError extends Error {
    constructor(name: string) {
        super(`"${name.slice(0, 80)}" is not a sprint name.`);
        this.name = 'InvalidSprintNameError';
    }
}
