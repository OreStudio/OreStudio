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

import { z } from 'zod';
import type { AuthenticatedCaller } from '../account-operations.js';
import { OperationFailedError } from '../errors.js';
import type { ListImagesRequest } from '../generated/assets/protocol/image_protocol.js';
import { SUBJECTS, resultEnvelopeSchema } from '../operations.js';

/**
 * An image, and the pictures and flags that are images.
 *
 * Flags and pictures are not separate concepts in ORE Studio: a country or an
 * account carries an `image_id`, and the image lives in the assets service.
 *
 * The bytes arrive in whatever form the codec chose for a byte vector, so all
 * three plausible shapes are accepted and normalised at the boundary. Guessing
 * one would produce a reader that works against one codec and silently returns
 * nothing against another.
 */
const imageBytes = z.union([
    z.string(),
    z.array(z.number().int().min(0).max(255)),
    z.instanceof(Uint8Array),
]);

const listImagesReplySchema = z.object({
    result: resultEnvelopeSchema,
    images: z
        .array(
            z.object({
                id: z.string(),
                mime_type: z.string().default('image/svg+xml'),
                data: imageBytes,
            }),
        )
        .default([]),
});

/** One image's bytes and what they are. */
export interface ImageContent {
    readonly imageId: string;
    readonly mimeType: string;
    readonly bytes: Buffer;
}

/**
 * The bytes, as a buffer.
 *
 * A string is already the image text, an array of numbers is the bytes one by
 * one, and a `Uint8Array` is the codec's own binary type. All three end up the
 * same way, so nothing downstream has to care which arrived.
 */
function toBuffer(data: z.infer<typeof imageBytes>): Buffer {
    if (typeof data === 'string') return Buffer.from(data, 'binary');
    return Buffer.from(data);
}

/**
 * The images named, in the caller's tenant.
 *
 * Row-level security scopes the read from the caller's token, so an image of
 * another tenant is not answered, and an identifier nothing holds is left out.
 */
export async function readImages(
    caller: AuthenticatedCaller,
    imageIds: readonly string[],
): Promise<ImageContent[]> {
    if (imageIds.length === 0) {
        return [];
    }
    const request: ListImagesRequest = {
        offset: 0,
        limit: imageIds.length,
        order: { field: '', descending: false },
        filter: { id_one_of: [...imageIds] },
    };
    const reply = await caller.callAuthenticated(
        SUBJECTS.listImages,
        request,
        listImagesReplySchema,
    );
    if (reply.result.outcome !== 'ok') {
        throw new OperationFailedError(SUBJECTS.listImages, reply.result.message);
    }
    return reply.images.map((image) => ({
        imageId: image.id,
        mimeType: image.mime_type === '' ? 'image/svg+xml' : image.mime_type,
        bytes: toBuffer(image.data),
    }));
}
