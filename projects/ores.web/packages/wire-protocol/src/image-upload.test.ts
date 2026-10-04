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
import type { AuthenticatedCaller } from './account-operations.js';
import { readImageUploadPolicy, uploadImage } from './entities/image.js';
import { OperationFailedError } from './errors.js';

/**
 * The upload a profile picture goes through: the rule it must satisfy, and
 * the upload itself. A refused image is an answer, so the picker can name the
 * rule it broke rather than reporting a failure.
 */

const IMAGE_ID = '55555555-5555-5555-5555-555555555555';

interface Recorded {
    subject: string;
    body: unknown;
}

function callerAnswering(answer: unknown, sent: Recorded[] = []): AuthenticatedCaller {
    return {
        async callAuthenticated(
            subject: string,
            body: unknown,
            schema: { parse: (value: unknown) => unknown },
        ): Promise<unknown> {
            sent.push({ subject, body });
            return schema.parse(answer);
        },
    } as unknown as AuthenticatedCaller;
}

const okResult = { outcome: 'ok', code: '', message: '', fields: [] };

describe('readImageUploadPolicy', () => {
    it('reads the rule the server enforces', async () => {
        const sent: Recorded[] = [];
        const policy = await readImageUploadPolicy(
            callerAnswering(
                {
                    result: okResult,
                    formats: ['image/png', 'image/jpeg', 'image/webp'],
                    max_size_bytes: 2097152,
                    min_width: 128,
                    min_height: 128,
                },
                sent,
            ),
        );

        expect(sent[0]?.subject).toBe('assets.v1.images.upload-policy');
        expect(sent[0]?.body).toEqual({});
        expect(policy).toEqual({
            formats: ['image/png', 'image/jpeg', 'image/webp'],
            maxSizeBytes: 2097152,
            minWidth: 128,
            minHeight: 128,
        });
    });

    it('raises when the policy was not answered ok', async () => {
        const caller = callerAnswering({
            result: { outcome: 'failed', code: 'internal_error', message: 'no policy configured' },
        });
        await expect(readImageUploadPolicy(caller)).rejects.toThrow(OperationFailedError);
        await expect(readImageUploadPolicy(caller)).rejects.toThrow('no policy configured');
    });
});

describe('uploadImage', () => {
    it('sends the media type and the base64 bytes', async () => {
        const sent: Recorded[] = [];
        await uploadImage(
            callerAnswering({ result: okResult, image_id: IMAGE_ID }, sent),
            { mimeType: 'image/png', data: 'aGVsbG8=' },
        );

        expect(sent[0]?.subject).toBe('assets.v1.images.upload');
        expect(sent[0]?.body).toEqual({ mime_type: 'image/png', data: 'aGVsbG8=' });
    });

    it('answers the id of the stored image', async () => {
        const reply = await uploadImage(
            callerAnswering({ result: okResult, image_id: IMAGE_ID }),
            { mimeType: 'image/jpeg', data: 'aGVsbG8=' },
        );

        expect(reply.result.outcome).toBe('ok');
        expect(reply.imageId).toBe(IMAGE_ID);
    });

    it('answers a refused image with its code rather than raising', async () => {
        const reply = await uploadImage(
            callerAnswering({
                result: {
                    outcome: 'invalid',
                    code: 'unsupported_media_type',
                    message: 'Only PNG, JPEG and WebP are accepted.',
                    fields: [],
                },
                image_id: '',
            }),
            { mimeType: 'image/gif', data: 'aGVsbG8=' },
        );

        expect(reply.result.outcome).toBe('invalid');
        expect(reply.result.code).toBe('unsupported_media_type');
        expect(reply.imageId).toBe('');
    });

    it('answers an oversized image the same way', async () => {
        const reply = await uploadImage(
            callerAnswering({
                result: { outcome: 'invalid', code: 'image_too_large', message: '', fields: [] },
                image_id: '',
            }),
            { mimeType: 'image/png', data: 'aGVsbG8=' },
        );

        expect(reply.result.code).toBe('image_too_large');
    });
});
