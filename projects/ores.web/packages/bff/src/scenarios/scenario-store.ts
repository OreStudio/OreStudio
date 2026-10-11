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

import type { ImageKind } from './image.js';
import type { Scenario, RunInput, ScenarioState, StepRef, DocLink } from './scenario-doc.js';

/**
 * Where scenarios and the docs they point at are kept. The BFF talks to this
 * interface, so a database store can replace the files. No implementation
 * calls compass.
 */

/** Any org doc that has an :ID:. */
export interface DocSummary {
    readonly id: string;
    readonly title: string;
    readonly type: string | null;
    /** The doc's path under the doc root, with forward slashes. */
    readonly path: string;
}

export interface DocText extends DocSummary {
    readonly text: string;
}

export interface StepCounts {
    readonly total: number;
    readonly pending: number;
    readonly pass: number;
    readonly fail: number;
    readonly dropped: number;
}

export interface ScenarioSummary {
    readonly id: string;
    readonly title: string;
    readonly description: string;
    readonly path: string;
    /** The sprint folder of the path, such as `sprint_27`, or null outside a sprint. */
    readonly sprint: string | null;
    /** The date of the doc's `#+created:` line, or empty. */
    readonly created: string;
    readonly state: ScenarioState;
    /** A scenario waits for a tester until it is PASSED, FAILED or abandoned. */
    readonly phase: 'waiting' | 'done' | 'abandoned';
    readonly target: string;
    readonly story: DocLink | null;
    readonly task: DocLink | null;
    readonly clients: readonly string[];
    readonly steps: StepCounts;
    readonly completedAt: string;
}

export interface RecordResult {
    readonly scenario: Scenario;
    readonly changed: readonly StepRef[];
}

/** An image the store kept: its name, which the step's note links to. */
export interface SavedImage {
    readonly name: string;
}

/** An image kept beside a scenario doc, with what its bytes are. */
export interface ImageFile {
    readonly bytes: Uint8Array;
    readonly kind: ImageKind;
}

export interface ScenarioStore {
    /** Every scenario under the doc root, newest id order is not promised. */
    list(): Promise<ScenarioSummary[]>;
    /** One scenario with its steps, or null when no scenario has the id. */
    read(id: string): Promise<Scenario | null>;
    /** Any doc by its :ID:, or null. */
    readDoc(id: string): Promise<DocText | null>;
    /**
     * Write a run into a scenario. Two saves do not merge: each save writes
     * the steps it names, so the last save to name a step wins that step.
     */
    record(id: string, input: RunInput): Promise<RecordResult>;
    /**
     * Keep a screenshot of step number `step` (counted from one) beside the
     * scenario. The store builds the name from the scenario and the step, and
     * decides the kind of image from its bytes.
     */
    saveImage(id: string, step: number, bytes: Uint8Array): Promise<SavedImage>;
    /**
     * An image kept beside the scenario doc, by its file name, or null. Only a
     * regular file that is a PNG, JPEG, GIF or WebP image is served, so a
     * scenario's folder gives away nothing else.
     */
    readImage(id: string, name: string): Promise<ImageFile | null>;
}

/** The id is not a document id. A path is never an id. */
export class InvalidDocIdError extends Error {
    constructor(id: string) {
        super(`"${id.slice(0, 80)}" is not a document id.`);
        this.name = 'InvalidDocIdError';
    }
}

export class ScenarioNotFoundError extends Error {
    constructor(id: string) {
        super(`No scenario has the id ${id}.`);
        this.name = 'ScenarioNotFoundError';
    }
}

/** A file resolves outside the doc root. */
export class PathEscapeError extends Error {
    constructor(path: string) {
        super(`${path} is outside the doc root.`);
        this.name = 'PathEscapeError';
    }
}

/** The bytes are not an image the store accepts. */
export class InvalidImageError extends Error {
    readonly reason: 'empty' | 'not-image' | 'too-large';

    constructor(reason: 'empty' | 'not-image' | 'too-large') {
        super(
            reason === 'too-large'
                ? 'The image is too large.'
                : reason === 'empty'
                  ? 'The image is empty.'
                  : 'The file is not a PNG, JPEG, GIF or WebP image.',
        );
        this.name = 'InvalidImageError';
        this.reason = reason;
    }
}

/** The step number is not one of the scenario's steps. */
export class StepNumberError extends Error {
    constructor(step: number, count: number) {
        super(`The scenario has ${count} steps, so there is no step ${step}.`);
        this.name = 'StepNumberError';
    }
}
