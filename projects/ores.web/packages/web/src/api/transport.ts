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

  constructor(status: number, body: ApiError) {
    super(body.message);
    this.name = 'ApiFailure';
    this.code = body.code;
    this.status = status;
  }
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
  let response: Response;
  try {
    response = await fetch(path, {
      // The session cookie is HttpOnly and must ride along.
      credentials: 'same-origin',
      ...init,
    });
  } catch {
    throw new ApiFailure(0, { code: 'upstream-unavailable', message: 'Cannot reach the server.' });
  }

  const text = await response.text();
  const payload: unknown = text.length === 0 ? null : parseJson(text);

  if (!response.ok) {
    const parsed = apiErrorSchema.safeParse(payload);
    throw new ApiFailure(
      response.status,
      parsed.success
        ? parsed.data
        : { code: 'internal', message: `Request failed with status ${response.status}` },
    );
  }
  return payload;
}
