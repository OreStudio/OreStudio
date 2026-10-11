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
import { detectImage, isImageName, mediaTypeOf } from './image.js';

const bytes = (...values: number[]): Uint8Array => Uint8Array.from(values);
const ascii = (text: string): number[] => [...text].map((c) => c.charCodeAt(0));

describe('detecting an image from its bytes', () => {
    it('knows a PNG, a JPEG, a GIF and a WebP image by their signatures', () => {
        expect(detectImage(bytes(0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a, 0))).toBe('png');
        expect(detectImage(bytes(0xff, 0xd8, 0xff, 0xe0))).toBe('jpg');
        expect(detectImage(bytes(...ascii('GIF89a'), 1, 0))).toBe('gif');
        expect(detectImage(bytes(...ascii('GIF87a'), 1, 0))).toBe('gif');
        expect(detectImage(bytes(...ascii('RIFF'), 1, 2, 3, 4, ...ascii('WEBPVP8 ')))).toBe('webp');
    });

    it('refuses text, an SVG, a script and a document', () => {
        for (const text of [
            'hello',
            '<svg xmlns="http://www.w3.org/2000/svg"/>',
            '<script>1</script>',
            '%PDF-1.7',
            'PK\u0003\u0004',
        ]) {
            expect(detectImage(bytes(...ascii(text)))).toBeNull();
        }
    });

    it('refuses empty bytes, a cut signature and a RIFF file that is not WebP', () => {
        expect(detectImage(new Uint8Array())).toBeNull();
        expect(detectImage(bytes(0x89, 0x50, 0x4e))).toBeNull();
        expect(detectImage(bytes(...ascii('RIFF'), 1, 2, 3, 4, ...ascii('WAVEfmt ')))).toBeNull();
    });

    it('does not take a name or a media type into account', () => {
        expect(detectImage(bytes(...ascii('not an image.png')))).toBeNull();
    });
});

describe('the name of an image beside a scenario doc', () => {
    it('is a plain file name with a picture extension, in any case', () => {
        for (const name of [
            'scenario_x_step2_20261011_014118.png',
            'a-b.c_d.JPG',
            'shot.jpeg',
            'x.gif',
            'x.WEBP',
            '1.png',
        ]) {
            expect(isImageName(name), name).toBe(true);
        }
    });

    it('is not a path, a hidden file, another kind of file or an SVG', () => {
        for (const name of [
            '../secret.png',
            'a/b.png',
            'a\\b.png',
            '.hidden.png',
            '.png',
            'notes.txt',
            'x.svg',
            'x.png.exe',
            'x.png ',
            ' x.png',
            'x',
            '',
            '%2e%2e%2fx.png',
            'x\0.png',
        ]) {
            expect(isImageName(name), JSON.stringify(name)).toBe(false);
        }
    });

    it('gives each kind its media type', () => {
        expect(mediaTypeOf('png')).toBe('image/png');
        expect(mediaTypeOf('jpg')).toBe('image/jpeg');
        expect(mediaTypeOf('gif')).toBe('image/gif');
        expect(mediaTypeOf('webp')).toBe('image/webp');
    });
});
