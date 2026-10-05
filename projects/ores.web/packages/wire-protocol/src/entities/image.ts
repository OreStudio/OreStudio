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
import { subjects as imageOperationsSubjects } from '../generated/assets/protocol/image_operations_protocol.js';
import { SUBJECTS, decidedResultSchema, resultEnvelopeSchema } from '../operations.js';
import type { DecidedResult } from '../operations.js';

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
        filter: { id_one_of: [...imageIds], search: null },
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

/** What a chooser shows of an image: its identifier, its code and its words, not its bytes. */
export interface ImageSummary {
    readonly imageId: string;
    readonly code: string;
    readonly description: string;
}

const imagePageReplySchema = z.looseObject({
    result: resultEnvelopeSchema,
    images: z
        .array(z.looseObject({ id: z.string(), code: z.string(), description: z.string() }))
        .default([]),
    total: z.int().nonnegative().default(0),
});

/**
 * One page of the caller's images, in code order, searched on the server by
 * code and description. The bytes are left out of the answer: a chooser draws
 * each image from its own address.
 */
export async function listImageSummaries(
    caller: AuthenticatedCaller,
    page: { readonly offset: number; readonly limit: number; readonly search: string },
): Promise<{ readonly images: readonly ImageSummary[]; readonly total: number }> {
    const reply = await caller.callAuthenticated(
        SUBJECTS.listImages,
        {
            offset: page.offset,
            limit: page.limit,
            order: { field: '', descending: false },
            filter: page.search === '' ? null : { id_one_of: null, search: page.search },
        },
        imagePageReplySchema,
    );
    if (reply.result.outcome !== 'ok') {
        throw new OperationFailedError(SUBJECTS.listImages, reply.result.message);
    }
    return {
        images: reply.images.map((image) => ({
            imageId: image.id,
            code: image.code,
            description: image.description,
        })),
        total: reply.total,
    };
}

/**
 * The rule an upload must satisfy, as the validator that enforces it states
 * it.
 *
 * A picker that states the rule reads it here rather than keeping a copy, so
 * the rule the picker shows is the rule the server applies.
 */
export interface ImageUploadPolicy {
    readonly formats: readonly string[];
    readonly maxSizeBytes: number;
    readonly minWidth: number;
    readonly minHeight: number;
}

const uploadPolicyReplySchema = z.object({
    result: resultEnvelopeSchema,
    formats: z.array(z.string()).default([]),
    max_size_bytes: z.int().nonnegative().default(0),
    min_width: z.int().nonnegative().default(0),
    min_height: z.int().nonnegative().default(0),
});

/** Reads the rule an uploaded image must satisfy. */
export async function readImageUploadPolicy(
    caller: AuthenticatedCaller,
): Promise<ImageUploadPolicy> {
    const reply = await caller.callAuthenticated(
        imageOperationsSubjects.get_image_upload_policy_request,
        {},
        uploadPolicyReplySchema,
    );
    if (reply.result.outcome !== 'ok') {
        throw new OperationFailedError(
            imageOperationsSubjects.get_image_upload_policy_request,
            reply.result.message,
        );
    }
    return {
        formats: reply.formats,
        maxSizeBytes: reply.max_size_bytes,
        minWidth: reply.min_width,
        minHeight: reply.min_height,
    };
}

/** What an upload answered: the result, and the stored image's id when ok. */
export interface ImageUploadReply {
    readonly result: DecidedResult;
    readonly imageId: string;
}

/** The upload's answer as the BFF serves it, in the camelCase the browser reads. */
export const imageUploadViewSchema = z.object({
    result: decidedResultSchema,
    imageId: z.string().default(''),
});

export type ImageUploadView = z.infer<typeof imageUploadViewSchema>;

/** The rule as it reaches the picker, in the camelCase the browser reads. */
export const imageUploadPolicyViewSchema = z.object({
    formats: z.array(z.string()).default([]),
    maxSizeBytes: z.int().nonnegative().default(0),
    minWidth: z.int().nonnegative().default(0),
    minHeight: z.int().nonnegative().default(0),
});

/**
 * Uploads one image and returns its id.
 *
 * The upload sets no photo: the id it answers rides into the write that
 * references it. A refused image is answered with a code the picker branches
 * on, so the reply is returned whole rather than thrown.
 */
export async function uploadImage(
    caller: AuthenticatedCaller,
    input: { readonly mimeType: string; readonly data: string },
): Promise<ImageUploadReply> {
    const reply = await caller.callAuthenticated(
        imageOperationsSubjects.upload_image_request,
        { mime_type: input.mimeType, data: input.data },
        z.object({
            result: decidedResultSchema,
            image_id: z.string().default(''),
        }),
    );
    return { result: reply.result, imageId: reply.image_id };
}
