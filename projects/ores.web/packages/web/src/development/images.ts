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

import { IMAGE_MEDIA_TYPES, MAX_IMAGE_BYTES } from '@ores/contracts';

/**
 * What the runner does with a pasted or dropped file before the server sees
 * it. The server is the judge of what an image is. These checks only save
 * the tester a round trip for a file that plainly is not one.
 */

/** The parts of a clipboard or a drop that the runner reads. */
export interface Transfer {
    readonly files?: ArrayLike<File> | null;
    readonly items?: ArrayLike<{
        readonly kind: string;
        readonly type: string;
        getAsFile(): File | null;
    }> | null;
    readonly types?: ArrayLike<string> | readonly string[] | null;
}

/**
 * The files a paste or a drop carries, whatever they are. Pasted text carries
 * none, so the note keeps it. A file that is not an image is still handed on,
 * so the tester is told it was refused and is not left wondering.
 */
export function filesOf(transfer: Transfer | null | undefined): File[] {
    if (transfer === null || transfer === undefined) return [];
    const found: File[] = [];
    const take = (file: File | null): void => {
        if (file !== null && !found.includes(file)) found.push(file);
    };
    for (const item of Array.from(transfer.items ?? [])) {
        if (item.kind === 'file') take(item.getAsFile());
    }
    for (const file of Array.from(transfer.files ?? [])) take(file);
    return found;
}

/** Whether a drag carries files, which is all a drag can say before the drop. */
export function carriesFiles(transfer: Transfer | null | undefined): boolean {
    return Array.from(transfer?.types ?? []).includes('Files');
}

/**
 * Why a file should not be sent, or null when it should be. Only the four kinds
 * the server keeps pass, so an SVG or a BMP is refused here with the same words
 * the server would use.
 */
export function imageProblem(file: {
    readonly type: string;
    readonly size: number;
}): 'not-image' | 'too-large' | null {
    if (!(IMAGE_MEDIA_TYPES as readonly string[]).includes(file.type.toLowerCase())) {
        return 'not-image';
    }
    return file.size > MAX_IMAGE_BYTES ? 'too-large' : null;
}

/** The limit in whole megabytes, for a message. */
export const MAX_IMAGE_MEGABYTES = MAX_IMAGE_BYTES / (1024 * 1024);
