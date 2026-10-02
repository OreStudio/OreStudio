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
 * The session is real, and what it is acting in may not do this.
 *
 * Not a 401: signing in again changes nothing, because the caller is already
 * who they say they are. The distinction matters to a screen, which should say
 * what is refused rather than send somebody back to a door that will let them
 * through to the same refusal.
 */
export function notPermitted(message: string): HttpFailure {
    return new HttpFailure(403, { code: 'forbidden', message });
}

/** The thing the address names does not exist, or is not one this route serves. */
export function notFound(message: string): HttpFailure {
    return new HttpFailure(404, { code: 'not-found', message });
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
 * The deployment already has its administrator, so it is not in bootstrap mode.
 *
 * Refused rather than attempted, because the function behind the create is the
 * one that grants SuperAdmin and it does not itself check the flag: an
 * unauthenticated request that could run it twice would be a way to make a
 * second super user with no session.
 */
export function bootstrapComplete(): HttpFailure {
    return new HttpFailure(409, {
        code: 'bootstrap-complete',
        message:
            'This deployment already has an administrator, so it is not in ' +
            'bootstrap mode. Sign in instead.',
    });
}

/**
 * A registration the server refused.
 *
 * The refusal keeps the server's own code rather than flattening to one
 * sentence, because the door branches on it: a tenant that nominates no default
 * role needs different words from a deployment that does not accept
 * registrations at all, and each names a different thing for an administrator
 * to fix.
 *
 * The status follows the code rather than being one value for all of them. A
 * name already in use is a conflict, a weak password is a bad request, and a
 * deployment that does not accept registrations at all is a refusal of the
 * request rather than a fault in it. A code this build does not know is
 * reported as a refusal of its own, so a new server-side code reads as a
 * refusal rather than as a client defect.
 */
export function signupRefused(code: string, message: string): HttpFailure {
    const known: Record<string, { readonly status: number; readonly code: ApiError['code'] }> = {
        username_taken: { status: 409, code: 'username-taken' },
        email_taken: { status: 409, code: 'email-taken' },
        weak_password: { status: 400, code: 'weak-password' },
        invalid_request: { status: 400, code: 'invalid-request' },
        signup_disabled: { status: 403, code: 'signups-disabled' },
        signup_requires_authorization: { status: 403, code: 'signup-requires-authorization' },
        no_registration_destination: { status: 403, code: 'no-registration-destination' },
        no_default_role: { status: 403, code: 'no-default-role' },
    };
    const refusal = known[code] ?? { status: 403, code: 'signup-refused' as const };
    return new HttpFailure(refusal.status, {
        code: refusal.code,
        message: message.length > 0 ? message : 'The registration was refused.',
    });
}

/**
 * A caller that has asked too often in the window.
 *
 * Its own code rather than a 401, because the credential was never the
 * question: the request was fine and the caller asked too many of them.
 */
export function tooManyRequests(what: string): HttpFailure {
    return new HttpFailure(429, {
        code: 'too-many-requests',
        message: `Too many ${what}. Wait a minute and try again.`,
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
            return new HttpFailure(403, {
                code: 'forbidden',
                message: 'You do not have access to this.',
            });
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
