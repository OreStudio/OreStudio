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
import {
  MalformedResponseError,
  NotAuthenticatedError,
  OperationFailedError,
  RequestTimeoutError,
  ServiceUnavailableError,
  SessionExpiredError,
  TransportError,
} from '@ores/wire-protocol';
import { invalidCredentials, toHttpFailure } from './errors.js';

/**
 * The error mapping is what a browser actually sees, so each protocol failure
 * is asserted to land on the status and code the UI branches on. An
 * unrecognised error must become a 500 with a generic message, never a leak of
 * an internal detail.
 */
describe('toHttpFailure', () => {
  it('maps an ended session to 401 session-expired', () => {
    const failure = toHttpFailure(new SessionExpiredError('max_session_exceeded', 'x'));
    expect(failure.status).toBe(401);
    expect(failure.body.code).toBe('session-expired');
  });

  it('maps a missing session to 401 not-authenticated', () => {
    const failure = toHttpFailure(new NotAuthenticatedError('no session'));
    expect(failure.status).toBe(401);
    expect(failure.body.code).toBe('not-authenticated');
  });

  it('maps a slow server to 504', () => {
    const failure = toHttpFailure(new RequestTimeoutError('x', 30_000));
    expect(failure.status).toBe(504);
    expect(failure.body.code).toBe('upstream-timeout');
  });

  it('maps no responders to 503', () => {
    const failure = toHttpFailure(new ServiceUnavailableError('x'));
    expect(failure.status).toBe(503);
    expect(failure.body.code).toBe('upstream-unavailable');
  });

  it('maps a lost connection to 503', () => {
    const failure = toHttpFailure(new TransportError('connection lost'));
    expect(failure.status).toBe(503);
  });

  it('keeps an operation failure message, which the server wrote for a human', () => {
    const failure = toHttpFailure(new OperationFailedError('x', 'account is in use'));
    expect(failure.status).toBe(409);
    expect(failure.body.message).toBe('account is in use');
  });

  it('reports an unrecognised error as an opaque 500', () => {
    const failure = toHttpFailure(new MalformedResponseError('internal detail'));
    expect(failure.status).toBe(500);
    expect(failure.body.code).toBe('internal');
    // The internal message must not reach the browser.
    expect(failure.body.message).not.toContain('internal detail');
  });

  it('passes an existing failure through unchanged', () => {
    const original = invalidCredentials('nope');
    expect(toHttpFailure(original)).toBe(original);
  });
});
