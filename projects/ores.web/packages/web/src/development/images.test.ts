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
import {
    ClipboardProblem,
    attachedImages,
    carriesFiles,
    filesOf,
    imageProblem,
    imagesFromClipboard,
    plainNote,
    type ClipboardLike,
    type Transfer,
} from './images.js';

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

    it('counts a file once when the items and the files both carry it', () => {
        const transfer: Transfer = { items: [item('file', 'image/png', png)], files: [png] };
        expect(filesOf(transfer)).toEqual([png]);
    });

    it('counts a pasted screenshot once, though the browser gives a new File object for the item and for the file', () => {
        const same = () =>
            new File([new Uint8Array([1, 2, 3])], 'image.png', { type: 'image/png' });
        const transfer: Transfer = { items: [item('file', 'image/png', same())], files: [same()] };
        const files = filesOf(transfer);
        expect(files).toHaveLength(1);
        expect(files[0]?.size).toBe(3);
    });

    it('counts two screenshots as two, each once', () => {
        const copy = (name: string) => new File([new Uint8Array([1])], name, { type: 'image/png' });
        const transfer: Transfer = {
            items: [
                item('file', 'image/png', copy('a.png')),
                item('file', 'image/png', copy('b.png')),
            ],
            files: [copy('a.png'), copy('b.png')],
        };
        expect(filesOf(transfer).map((f) => f.name)).toEqual(['a.png', 'b.png']);
    });

    it('falls back to the files when the items carry none, as a dropped file can', () => {
        expect(filesOf({ items: [item('string', 'text/plain', null)], files: [png, jpg] })).toEqual(
            [png, jpg],
        );
        expect(filesOf({ files: [png] })).toEqual([png]);
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

describe('the images a note links', () => {
    it('lists each image a note links, in order, once, as the Qt tool wrote them', () => {
        const notes =
            'It stayed blank.\n[[file:scenario_a_step2_20261011_014118.png]]\n[[file:scenario_a_step2_20261011_014118_2.png]]\n[[file:scenario_a_step2_20261011_014118.png]]';
        expect(attachedImages(notes)).toEqual([
            'scenario_a_step2_20261011_014118.png',
            'scenario_a_step2_20261011_014118_2.png',
        ]);
    });

    it('leaves out a link to a text file, a doc, a path or an SVG', () => {
        const notes =
            '[[file:notes.txt]] [[id:ABC][a doc]] [[file:../x.png]] [[file:a/b.png]] [[file:v.svg]] [[https://x.example/y.png]]';
        expect(attachedImages(notes)).toEqual([]);
    });

    it('finds nothing in a note with no link, and in an empty note', () => {
        expect(attachedImages('plain text')).toEqual([]);
        expect(attachedImages('')).toEqual([]);
    });

    it('finds an image link beside other text on a line', () => {
        expect(attachedImages('see [[file:a.png]] and [[file:b.JPG]] too')).toEqual([
            'a.png',
            'b.JPG',
        ]);
    });
});

describe('the images on the clipboard', () => {
    const item = (
        types: string[],
        blob: Blob = new Blob([new Uint8Array([1, 2])], { type: types[0] ?? '' }),
    ) => ({
        types,
        getType: async () => blob,
    });
    const clipboard = (items: ReturnType<typeof item>[]): ClipboardLike => ({
        read: async () => items,
    });
    const problem = async (promise: Promise<unknown>) =>
        promise.then(
            () => null,
            (error: unknown) => (error instanceof ClipboardProblem ? error.reason : error),
        );

    it('gives a screenshot on the clipboard as a PNG file', async () => {
        const files = await imagesFromClipboard(clipboard([item(['image/png'])]));
        expect(files).toHaveLength(1);
        expect(files[0]?.type).toBe('image/png');
        expect(files[0]?.name).toBe('clipboard.png');
        expect(files[0]?.size).toBe(2);
    });

    it('takes the image of an item that holds text as well', async () => {
        const files = await imagesFromClipboard(
            clipboard([
                item(
                    ['text/html', 'image/png'],
                    new Blob([new Uint8Array([9])], { type: 'image/png' }),
                ),
            ]),
        );
        expect(files.map((f) => f.name)).toEqual(['clipboard.png']);
    });

    it('names a file by what it is, and takes more than one image', async () => {
        const jpeg = item(['image/jpeg'], new Blob([new Uint8Array([1])], { type: 'image/jpeg' }));
        const webp = item(['image/webp'], new Blob([new Uint8Array([1])], { type: 'image/webp' }));
        const files = await imagesFromClipboard(clipboard([jpeg, webp]));
        expect(files.map((f) => f.name)).toEqual(['clipboard.jpg', 'clipboard.webp']);
    });

    it('says the clipboard holds no image when it holds only text, or nothing', async () => {
        expect(await problem(imagesFromClipboard(clipboard([item(['text/plain'])])))).toBe('empty');
        expect(await problem(imagesFromClipboard(clipboard([])))).toBe('empty');
    });

    it('says the browser refused when the tester or the browser denies the clipboard', async () => {
        const denied: ClipboardLike = {
            read: async () => {
                throw Object.assign(new Error('denied'), { name: 'NotAllowedError' });
            },
        };
        expect(await problem(imagesFromClipboard(denied))).toBe('denied');
    });

    it('says the clipboard holds no image when the read fails for another reason', async () => {
        const broken: ClipboardLike = {
            read: async () => {
                throw Object.assign(new Error('no data'), { name: 'DataError' });
            },
        };
        expect(await problem(imagesFromClipboard(broken))).toBe('empty');
    });

    it('says the clipboard cannot be read when the browser has none', async () => {
        expect(await problem(imagesFromClipboard(undefined))).toBe('unavailable');
        expect(await problem(imagesFromClipboard({} as ClipboardLike))).toBe('unavailable');
    });
});

describe('the words of a note without the links to its images', () => {
    it('is empty for a note that is only image links', () => {
        expect(plainNote('[[file:a.png]]')).toBe('');
        expect(plainNote('[[file:a.png]]; [[file:b.png]]')).toBe('');
    });

    it('drops the links and the separators that held them, and keeps the words', () => {
        expect(plainNote('The tab stayed blank.; [[file:a.png]]; [[file:b.png]]')).toBe(
            'The tab stayed blank.',
        );
        expect(plainNote('[[file:a.png]]; It was red.')).toBe('It was red.');
    });

    it('keeps the words on both sides of a link, in order', () => {
        expect(plainNote('Before.; [[file:a.png]]; After.')).toBe('Before.; After.');
    });

    it('keeps a link that is not to an image, because it is part of what was written', () => {
        expect(plainNote('see [[file:notes.txt]] and [[id:X][a doc]]')).toBe(
            'see [[file:notes.txt]] and [[id:X][a doc]]',
        );
    });

    it('leaves a note with no link as it is, and an empty note empty', () => {
        expect(plainNote('A plain note; with a semicolon.')).toBe(
            'A plain note; with a semicolon.',
        );
        expect(plainNote('')).toBe('');
    });
});
