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
 * Errors raised by the protocol layer.
 *
 * Every failure crossing this boundary is a {@link ProtocolError} subclass so
 * callers can match exhaustively instead of inspecting message strings. The
 * `X-Error` values the server sends (`token_expired`, `forbidden`,
 * `bad_request`, `unauthorized`) map one-to-one onto subclasses; see
 * `ores.service/messaging/handler_helpers.hpp` in the C++ checkout.
 */

/** The `X-Error` header name the server sets on an empty error reply. */
export const X_ERROR_HEADER = 'X-Error';

/** Server-reported error codes carried in the `X-Error` header. */
export type ServerErrorCode =
  | 'unauthorized'
  | 'token_expired'
  | 'forbidden'
  | 'bad_request'
  | 'max_session_exceeded';

/** Base class for every protocol failure. */
export class ProtocolError extends Error {
  constructor(message: string, options?: { cause?: unknown }) {
    super(message, options);
    this.name = new.target.name;
  }
}

/** The transport could not reach, or lost, the NATS server. */
export class TransportError extends ProtocolError {}

/** No live connection exists, so the request was never sent. */
export class NotConnectedError extends TransportError {}

/** A request was issued without a session token. */
export class NotAuthenticatedError extends ProtocolError {}

/** No service is subscribed to the subject. */
export class ServiceUnavailableError extends ProtocolError {
  readonly subject: string;

  constructor(subject: string, options?: { cause?: unknown }) {
    super(`No responder for subject ${subject}`, options);
    this.subject = subject;
  }
}

/** The server did not answer inside the client timeout. */
export class RequestTimeoutError extends ProtocolError {
  readonly subject: string;
  readonly timeoutMs: number;

  constructor(subject: string, timeoutMs: number) {
    super(`Request to ${subject} timed out after ${timeoutMs}ms`);
    this.subject = subject;
    this.timeoutMs = timeoutMs;
  }
}

/** The reply body was not decodable as the expected response type. */
export class MalformedResponseError extends ProtocolError {}

/** The server rejected the request with an `X-Error` header. */
export class ServerError extends ProtocolError {
  readonly code: ServerErrorCode;
  readonly subject: string;

  constructor(code: ServerErrorCode, subject: string) {
    super(`Server rejected ${subject} with ${code}`);
    this.code = code;
    this.subject = subject;
  }
}

/**
 * The server accepted the request and reported a failure in the body.
 *
 * Many operations answer with `success: false` and a message rather than an
 * `X-Error` header, so a caller that only checks for thrown transport errors
 * would treat a rejected write as a success. This carries the server's own
 * message to the caller.
 */
export class OperationFailedError extends ProtocolError {
  readonly subject: string;

  constructor(subject: string, message: string) {
    super(message.length > 0 ? message : `Operation on ${subject} failed`);
    this.subject = subject;
  }
}

/** The session token is expired, or the session outlived its maximum. */
export class SessionExpiredError extends ServerError {
  constructor(code: 'token_expired' | 'max_session_exceeded', subject: string) {
    super(code, subject);
  }
}

/**
 * Maps a raw `X-Error` value onto the matching error.
 *
 * An unrecognised code still fails the call; it surfaces as
 * {@link MalformedResponseError} rather than being mistaken for success.
 */
export function serverErrorFor(code: string, subject: string): ProtocolError {
  switch (code) {
    case 'token_expired':
    case 'max_session_exceeded':
      return new SessionExpiredError(code, subject);
    case 'unauthorized':
    case 'forbidden':
    case 'bad_request':
      return new ServerError(code, subject);
    default:
      return new MalformedResponseError(`Unknown X-Error code ${JSON.stringify(code)}`, {
        cause: { subject },
      });
  }
}
