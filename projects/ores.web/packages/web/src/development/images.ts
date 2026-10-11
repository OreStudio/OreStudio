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

import { IMAGE_MEDIA_TYPES, MAX_IMAGE_BYTES, isImageName } from '@ores/contracts';

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

/**
 * The names of the images a note links, in the order it links them, each once.
 * A tester or the old Qt tool writes a link as `[[file:name.png]]`. Only a link
 * whose name is that of an image file counts, so a link to a text file or to
 * another doc shows no thumbnail.
 */
export function attachedImages(notes: string): readonly string[] {
    const names: string[] = [];
    for (const match of notes.matchAll(/\[\[file:([^\]]+)\]\]/g)) {
        const name = match[1] ?? '';
        if (isImageName(name) && !names.includes(name)) names.push(name);
    }
    return names;
}

/** Why the clipboard gave no image. */
export type ClipboardReason = 'unavailable' | 'denied' | 'empty';

export class ClipboardProblem extends Error {
    readonly reason: ClipboardReason;

    constructor(reason: ClipboardReason) {
        super(`The clipboard gave no image: ${reason}.`);
        this.name = 'ClipboardProblem';
        this.reason = reason;
    }
}

/** The part of the browser's clipboard that the runner reads. */
export interface ClipboardLike {
    read(): Promise<
        ArrayLike<{
            readonly types: readonly string[];
            getType(type: string): Promise<Blob>;
        }>
    >;
}

const EXTENSIONS: Readonly<Record<string, string>> = {
    'image/png': 'png',
    'image/jpeg': 'jpg',
    'image/gif': 'gif',
    'image/webp': 'webp',
};

/**
 * The images on the clipboard, as files. A screenshot taken to the clipboard,
 * such as one from GNOME, is a PNG there, and a paste with Ctrl+V reaches the
 * page only when a field has the focus. Reading the clipboard from a button
 * needs the browser's leave, which it asks the tester for once.
 *
 * It throws a ClipboardProblem when the browser has no clipboard to read, when
 * the tester or the browser refuses it, or when the clipboard holds no image.
 */
export async function imagesFromClipboard(clipboard: ClipboardLike | undefined): Promise<File[]> {
    if (clipboard === undefined || typeof clipboard.read !== 'function') {
        throw new ClipboardProblem('unavailable');
    }
    let items: Awaited<ReturnType<ClipboardLike['read']>>;
    try {
        items = await clipboard.read();
    } catch (error) {
        const name = (error as { name?: unknown } | null)?.name;
        throw new ClipboardProblem(
            name === 'NotAllowedError' || name === 'SecurityError' ? 'denied' : 'empty',
        );
    }
    const files: File[] = [];
    for (const item of Array.from(items)) {
        const type = item.types.find((candidate) => candidate.startsWith('image/'));
        if (type === undefined) continue;
        const blob = await item.getType(type);
        const kind = blob.type === '' ? type : blob.type;
        files.push(new File([blob], `clipboard.${EXTENSIONS[kind] ?? 'png'}`, { type: kind }));
    }
    if (files.length === 0) throw new ClipboardProblem('empty');
    return files;
}
