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

import { describe, expect, it } from 'vitest';
import {
    IMAGE_MEDIA_TYPES,
    MAX_IMAGE_BYTES,
    imageUploadRequestSchema,
    imageUploadResponseSchema,
    runRequestSchema,
    scenarioStateOf,
} from './qa.js';

describe('the state of a scenario from its steps', () => {
    it('stays pending while any step is pending', () => {
        expect(scenarioStateOf(['PASS', 'PENDING'])).toBe('PENDING');
        expect(scenarioStateOf(['FAIL', 'PENDING'])).toBe('PENDING');
        expect(scenarioStateOf(['PENDING'])).toBe('PENDING');
    });

    it('is failed once nothing is pending and a step failed', () => {
        expect(scenarioStateOf(['PASS', 'FAIL'])).toBe('FAILED');
        expect(scenarioStateOf(['FAIL'])).toBe('FAILED');
    });

    it('is passed once nothing is pending and nothing failed', () => {
        expect(scenarioStateOf(['PASS', 'PASS'])).toBe('PASSED');
        expect(scenarioStateOf(['PASS', 'DROPPED'])).toBe('PASSED');
    });

    it('stays pending when nothing passed or failed', () => {
        expect(scenarioStateOf([])).toBe('PENDING');
        expect(scenarioStateOf(['DROPPED', 'DROPPED'])).toBe('PENDING');
    });
});

describe('a save', () => {
    const step = { client: null, title: 'Open the tab', status: 'PASS', notes: '' };

    it('may leave out the build it ran against', () => {
        expect(runRequestSchema.safeParse({ steps: [step] }).success).toBe(true);
    });

    it('may name the build', () => {
        const named = { steps: [step], branch: 'feature/x', commit: 'abc1234', worktree: 'w' };
        expect(runRequestSchema.safeParse(named).success).toBe(true);
    });

    it('carries no state and no time', () => {
        expect(runRequestSchema.safeParse({ steps: [step], state: 'PASSED' }).success).toBe(false);
        expect(runRequestSchema.safeParse({ steps: [step], completedAt: 'x' }).success).toBe(false);
    });
});

describe('a screenshot upload', () => {
    it('is a media type and the bytes in base64, and nothing else', () => {
        expect(
            imageUploadRequestSchema.safeParse({ mediaType: 'image/png', data: 'AAAA' }).success,
        ).toBe(true);
        expect(imageUploadRequestSchema.safeParse({ mediaType: 'image/png' }).success).toBe(false);
        expect(
            imageUploadRequestSchema.safeParse({ mediaType: 'image/png', data: '' }).success,
        ).toBe(false);
        expect(
            imageUploadRequestSchema.safeParse({ mediaType: 'image/png', data: 'AAAA', name: 'x' })
                .success,
        ).toBe(false);
    });

    it('answers the name of the file', () => {
        expect(imageUploadResponseSchema.parse({ name: 'a_step1_20261011_090000.png' }).name).toBe(
            'a_step1_20261011_090000.png',
        );
    });

    it('accepts four kinds of image and no SVG, within eight megabytes', () => {
        expect([...IMAGE_MEDIA_TYPES]).toEqual([
            'image/png',
            'image/jpeg',
            'image/gif',
            'image/webp',
        ]);
        expect(IMAGE_MEDIA_TYPES).not.toContain('image/svg+xml');
        expect(MAX_IMAGE_BYTES).toBe(8 * 1024 * 1024);
    });
});
