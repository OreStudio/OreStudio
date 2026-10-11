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

import { apiErrorSchema, type ApiError } from '@ores/wire-protocol/browser';
import { trail } from '../log/clientLog.js';

/**
 * The browser's transport.
 *
 * One place decides how a request is made and how a failure is turned into
 * something a component can show. Every response is parsed with a schema at the
 * call site, so this layer only deals with the mechanics.
 *
 * Note what a request can carry: an identity, and nothing about where the
 * application connects. That is fixed by the deployment.
 */

/** A failure the UI can present, already narrowed from the error contract. */
export class ApiFailure extends Error {
    readonly code: ApiError['code'];
    readonly status: number;
    /** What was being done: the method and the path, without the query a person typed. */
    readonly operation: string;
    /** The id the BFF gave the request, which is on its log lines for it. */
    readonly requestId: string;

    constructor(
        status: number,
        body: ApiError,
        context?: { operation: string; requestId: string },
    ) {
        super(body.message);
        this.name = 'ApiFailure';
        this.code = body.code;
        this.status = status;
        this.operation = context?.operation ?? '';
        this.requestId = context?.requestId ?? '';
    }
}

const requests = trail('request');

/** The method and the path of a request, which names the operation without its values. */
export function operationOf(path: string, init: RequestInit): string {
    const bare = path.split('?')[0] ?? path;
    return `${(init.method ?? 'GET').toUpperCase()} ${bare}`;
}

/** Writes a failed request to the trail, with everything needed to find it in the log. */
function recordFailure(failure: ApiFailure): void {
    const facts = {
        operation: failure.operation,
        status: failure.status,
        code: failure.code,
        request_id: failure.requestId,
        route: window.location.pathname,
        reason: failure.message,
    };
    // A 401 is how a signed-out browser learns it is signed out, and is not news.
    if (failure.status === 401) requests.debug('request refused', facts);
    else if (failure.status >= 500 || failure.status === 0) requests.error('request failed', facts);
    else requests.warn('request refused', facts);
}

export function parseJson(text: string): unknown {
    try {
        return JSON.parse(text) as unknown;
    } catch {
        throw new ApiFailure(500, { code: 'internal', message: 'The server sent invalid JSON.' });
    }
}

/** Sends a request and returns the decoded body, or throws an {@link ApiFailure}. */
export async function request(path: string, init: RequestInit): Promise<unknown> {
    const operation = operationOf(path, init);
    let response: Response;
    try {
        response = await fetch(path, {
            // The session cookie is HttpOnly and must ride along.
            credentials: 'same-origin',
            ...init,
        });
    } catch {
        const failure = new ApiFailure(
            0,
            { code: 'upstream-unavailable', message: 'Cannot reach the server.' },
            { operation, requestId: '' },
        );
        recordFailure(failure);
        throw failure;
    }

    const text = await response.text();
    const payload: unknown = text.length === 0 ? null : parseJson(text);

    if (!response.ok) {
        const parsed = apiErrorSchema.safeParse(payload);
        const failure = new ApiFailure(
            response.status,
            parsed.success
                ? parsed.data
                : { code: 'internal', message: `Request failed with status ${response.status}` },
            { operation, requestId: response.headers.get('x-request-id') ?? '' },
        );
        recordFailure(failure);
        throw failure;
    }
    return payload;
}
