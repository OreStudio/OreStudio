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

/**
 * Telling an image from other bytes.
 *
 * A screenshot is kept beside a scenario doc, under a name the server builds,
 * so what counts is what the bytes are. A media type the browser claims is not
 * trusted: a file is an image when it starts with the signature of a PNG, a
 * JPEG, a GIF or a WebP image, and its extension follows from that signature.
 * An SVG is not accepted, because it can carry script.
 */

export type ImageKind = 'png' | 'jpg' | 'gif' | 'webp';

function startsWith(bytes: Uint8Array, signature: readonly number[], at = 0): boolean {
    return signature.every((byte, i) => bytes[at + i] === byte);
}

const PNG = [0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a];
const JPEG = [0xff, 0xd8, 0xff];
const GIF87 = [0x47, 0x49, 0x46, 0x38, 0x37, 0x61];
const GIF89 = [0x47, 0x49, 0x46, 0x38, 0x39, 0x61];
const RIFF = [0x52, 0x49, 0x46, 0x46];
const WEBP = [0x57, 0x45, 0x42, 0x50];

/** The kind of image the bytes are, or null when they are none of the accepted kinds. */
export function detectImage(bytes: Uint8Array): ImageKind | null {
    if (startsWith(bytes, PNG)) return 'png';
    if (startsWith(bytes, JPEG)) return 'jpg';
    if (startsWith(bytes, GIF87) || startsWith(bytes, GIF89)) return 'gif';
    if (startsWith(bytes, RIFF) && startsWith(bytes, WEBP, 8)) return 'webp';
    return null;
}
