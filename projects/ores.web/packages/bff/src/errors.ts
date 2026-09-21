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

import {
  NotAuthenticatedError,
  OperationFailedError,
  RequestTimeoutError,
  ServerError,
  ServiceUnavailableError,
  SessionExpiredError,
  TransportError,
} from '@ores/wire-protocol';
import type { ApiError } from '@ores/wire-protocol';

/**
 * Maps an internal failure onto the browser-facing error contract.
 *
 * The mapping lives in one place so every route reports the same way, and so
 * a protocol error type added later cannot silently become a 500. Anything
 * unrecognised is a genuine bug and is reported as `internal`, with the real
 * error logged rather than sent to the browser.
 */

export class HttpFailure extends Error {
  readonly status: number;
  readonly body: ApiError;

  constructor(status: number, body: ApiError) {
    super(body.message);
    this.name = 'HttpFailure';
    this.status = status;
    this.body = body;
  }
}

export function notAuthenticated(): HttpFailure {
  return new HttpFailure(401, {
    code: 'not-authenticated',
    message: 'Sign in to continue.',
  });
}

export function invalidCredentials(message: string): HttpFailure {
  return new HttpFailure(401, {
    code: 'invalid-credentials',
    message: message.length > 0 ? message : 'Invalid username or password.',
  });
}

export function invalidRequest(message: string): HttpFailure {
  return new HttpFailure(400, { code: 'invalid-request', message });
}

/**
 * The deployment has not been provisioned yet.
 *
 * Its own code rather than a 401, because it is not a credential problem: there
 * are no accounts to be wrong about. A caller that cannot tell the two apart
 * shows "invalid username or password" to somebody whose only mistake was being
 * the first person to arrive.
 */
export function bootstrapRequired(): HttpFailure {
  return new HttpFailure(409, {
    code: 'bootstrap-mode',
    message:
      'This deployment is in bootstrap mode: it has not been provisioned yet, ' +
      'so there are no accounts to sign in with. An administrator must ' +
      'complete the setup wizard first.',
  });
}

/**
 * Translates any thrown value into an {@link HttpFailure}.
 *
 * Returns the original failure when it already is one, so a route can throw a
 * specific status and have it preserved.
 */
export function toHttpFailure(error: unknown): HttpFailure {
  if (error instanceof HttpFailure) {
    return error;
  }
  if (error instanceof SessionExpiredError) {
    return new HttpFailure(401, {
      code: 'session-expired',
      message: 'Your session has ended. Sign in again.',
    });
  }
  if (error instanceof NotAuthenticatedError) {
    return notAuthenticated();
  }
  if (error instanceof ServerError) {
    if (error.code === 'forbidden') {
      return new HttpFailure(403, { code: 'forbidden', message: 'You do not have access to this.' });
    }
    /*
     * The server's own code, in the message.
     *
     * A refusal with no reason is a refusal nobody can act on: an operator sees
     * "the server refused" and has nothing to look up. The code is the one piece
     * of the server's answer that says which rule was applied.
     */
    return new HttpFailure(502, {
      code: 'upstream-unavailable',
      message: `The server refused the request (${error.code}).`,
    });
  }
  if (error instanceof OperationFailedError) {
    return new HttpFailure(409, { code: 'invalid-request', message: error.message });
  }
  if (error instanceof RequestTimeoutError) {
    return new HttpFailure(504, {
      code: 'upstream-timeout',
      message: 'The server did not answer in time.',
    });
  }
  if (error instanceof ServiceUnavailableError) {
    return new HttpFailure(503, {
      code: 'upstream-unavailable',
      message: 'That service is not running.',
    });
  }
  if (error instanceof TransportError) {
    return new HttpFailure(503, {
      code: 'upstream-unavailable',
      message: 'Cannot reach the message bus.',
    });
  }
  return new HttpFailure(500, {
    code: 'internal',
    message: 'Something went wrong.',
  });
}
