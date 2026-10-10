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
import { MAX_IMAGE_BYTES } from '@ores/contracts';
import { carriesFiles, filesOf, imageProblem, type Transfer } from './images.js';

const png = new File([new Uint8Array([1])], 'shot.png', { type: 'image/png' });
const jpg = new File([new Uint8Array([2])], 'shot.jpg', { type: 'image/jpeg' });
const text = new File(['hello'], 'notes.txt', { type: 'text/plain' });

const item = (kind: string, type: string, file: File | null) => ({
    kind,
    type,
    getAsFile: () => file,
});

describe('the files in a paste or a drop', () => {
    it('takes a file from the items of a paste', () => {
        const transfer: Transfer = { items: [item('file', 'image/png', png)] };
        expect(filesOf(transfer)).toEqual([png]);
    });

    it('takes every file of a drop, images and others, for the page to judge', () => {
        expect(filesOf({ files: [png, text, jpg] })).toEqual([png, text, jpg]);
    });

    it('does not count one file twice when items and files carry it both', () => {
        const transfer: Transfer = { items: [item('file', 'image/png', png)], files: [png] };
        expect(filesOf(transfer)).toEqual([png]);
    });

    it('leaves pasted text alone, so the note gets it', () => {
        const transfer: Transfer = {
            items: [item('string', 'text/plain', null), item('string', 'text/html', null)],
            files: [],
        };
        expect(filesOf(transfer)).toEqual([]);
    });

    it('finds nothing in no transfer, and in an item whose file is missing', () => {
        expect(filesOf(null)).toEqual([]);
        expect(filesOf(undefined)).toEqual([]);
        expect(filesOf({ items: [item('file', 'image/png', null)] })).toEqual([]);
    });

    it('knows that a drag carries files before the drop', () => {
        expect(carriesFiles({ types: ['Files'] })).toBe(true);
        expect(carriesFiles({ types: ['text/plain'] })).toBe(false);
        expect(carriesFiles(null)).toBe(false);
    });
});

describe('a file that should not be sent', () => {
    it('is a file that is not an image', () => {
        expect(imageProblem({ type: 'text/plain', size: 10 })).toBe('not-image');
        expect(imageProblem({ type: '', size: 10 })).toBe('not-image');
    });

    it('is an SVG, a BMP or a TIFF, which the server does not keep', () => {
        for (const type of ['image/svg+xml', 'image/bmp', 'image/tiff']) {
            expect(imageProblem({ type, size: 10 })).toBe('not-image');
        }
    });

    it('is no problem for any of the four kinds, in any case', () => {
        for (const type of ['image/png', 'image/jpeg', 'image/gif', 'image/webp', 'IMAGE/PNG']) {
            expect(imageProblem({ type, size: 10 })).toBeNull();
        }
    });

    it('is an image over the limit, and an image at the limit is sent', () => {
        expect(imageProblem({ type: 'image/png', size: MAX_IMAGE_BYTES + 1 })).toBe('too-large');
        expect(imageProblem({ type: 'image/png', size: MAX_IMAGE_BYTES })).toBeNull();
    });

    it('is no problem for an ordinary screenshot', () => {
        expect(imageProblem({ type: 'image/png', size: 200_000 })).toBeNull();
    });
});
