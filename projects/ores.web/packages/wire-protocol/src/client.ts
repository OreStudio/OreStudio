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
import type { WireFormat } from './codec.js';
import { WireCodec } from './codec.js';
import type { PartySummary } from './domain.js';
import {
  NotAuthenticatedError,
  ServerError,
  SessionExpiredError,
  type ProtocolError,
  type ServerErrorCode,
} from './errors.js';
import {
  SUBJECTS,
  bootstrapStatusResponseSchema,
  emptyRequestSchema,
  loginRequestSchema,
  loginResponseSchema,
  logoutResponseSchema,
  partyRequestSchema,
  partyResponseSchema,
  refreshResponseSchema,
  accountPageSchema,
  httpInfoResponseSchema,
  listAccountsRequestSchema,
  type LoginResponse,
  type WireAccountPage,
} from './operations.js';
import { subjects as bootstrapSubjects } from './generated/iam/protocol/bootstrap_protocol.js';
import type { Transport } from './transport.js';
import { resolveHeaders } from './headers.js';
import type { HeaderSource } from './headers.js';
import { nodeIdGenerator, tracingHeaders, type IdGenerator } from './ids.js';
import { LIVE_WORKSPACE_ID } from './primitives.js';

/** How long the client waits for each kind of call. */
export interface Timeouts {
  /** Quick reads: lookups, list pages. */
  readonly fastMs: number;
  /** Server-side work such as provisioning or imports. */
  readonly slowMs: number;
}

export const DEFAULT_TIMEOUTS: Timeouts = {
  fastMs: 30_000,
  slowMs: 120_000,
};

/** Everything needed to open a login session. */
export interface LoginCredentials {
  readonly principal: string;
  readonly password: string;
}

/** A session that a party has been chosen for, and that can be used for calls. */
export interface ActiveSession {
  readonly kind: 'active';
  readonly token: string;
  readonly accountId: string;
  readonly tenantId: string;
  readonly tenantName: string;
  readonly username: string;
  readonly email: string;
  readonly party: PartySummary;
  readonly availableParties: readonly PartySummary[];
  readonly accessLifetimeSeconds: number;
  readonly passwordResetRequired: boolean;
  readonly sessionId: string;
}

/** A login that the server accepted but that still needs a party. */
export interface PartySelectionRequired {
  readonly kind: 'party-selection-required';
  readonly accountId: string;
  readonly tenantId: string;
  readonly tenantName: string;
  readonly username: string;
  readonly email: string;
  readonly availableParties: readonly PartySummary[];
  readonly defaultPartyId: string | null;
  readonly passwordResetRequired: boolean;
  readonly accessLifetimeSeconds: number;
  readonly sessionId: string;
}

/** A login the server rejected. */
export interface LoginRejected {
  readonly kind: 'rejected';
  readonly message: string;
}

export type LoginOutcome = ActiveSession | PartySelectionRequired | LoginRejected;

export interface OresClientOptions {
  readonly transport: Transport;
  readonly format?: WireFormat;
  readonly timeouts?: Partial<Timeouts>;
  /** Injected for tests; defaults to the real clock. */
  readonly now?: () => Date;
  /** Injected for tests and for hosts without Node's crypto. */
  readonly generateId?: IdGenerator;
}

/** A request context attached to every authenticated call. */
export interface WorkspaceContext {
  readonly workspaceId: string;
  /** Ancestor chain, nearest first, for inherited definitions. */
  readonly resolutionOrder?: readonly string[];
}

interface SessionState {
  token: string;
  /** IAM session id, forwarded as `Nats-Session-Id` on every authenticated call. */
  sessionId: string;
  refreshInFlight: Promise<string> | undefined;
  workspace: WorkspaceContext | undefined;
}

/**
 * The typed client for the ORE Studio bus.
 *
 * It owns the login lifecycle and the session token, and it maps every reply
 * through a schema before handing it to a caller. It never exposes the token
 * to a caller that does not need it.
 */
export class OresClient {
  readonly #transport: Transport;
  readonly #codec: WireCodec;
  readonly #timeouts: Timeouts;
  readonly #now: () => Date;
  readonly #generateId: IdGenerator;
  #session: SessionState | undefined;

  constructor(options: OresClientOptions) {
    this.#transport = options.transport;
    this.#codec = new WireCodec(options.format ?? 'msgpack');
    this.#timeouts = { ...DEFAULT_TIMEOUTS, ...options.timeouts };
    this.#now = options.now ?? (() => new Date());
    this.#generateId = options.generateId ?? nodeIdGenerator;
  }

  get transport(): Transport {
    return this.#transport;
  }

  /** True once a token exists, whether or not a party has been chosen. */
  get hasToken(): boolean {
    return this.#session !== undefined;
  }

  /**
   * The IAM session id for the current login, or an empty string.
   *
   * The server wants this on every authenticated call as `Nats-Session-Id`,
   * and a caller that manages a session across processes needs it to carry a
   * pending party selection forward. It is available as soon as `login`
   * returns, before any party has been chosen.
   */
  get currentSessionId(): string {
    return this.#session?.sessionId ?? '';
  }

  /**
   * Sends `login` and classifies the reply.
   *
   * The server uses a nil `selected_party_id` to mean "party picker
   * outstanding", so the caller must resolve that before calling anything
   * authenticated.
   */
  /**
   * Whether the deployment still needs provisioning.
   *
   * Asked before a login, not after it. A deployment in bootstrap mode has no
   * accounts to sign in with, so the answer decides whether a login is worth
   * attempting at all: the Qt client checked this first and never reached the
   * credential form.
   */
  async bootstrapStatus(): Promise<{
    isInBootstrapMode: boolean;
    message: string;
  }> {
    const reply = await this.#call(
      bootstrapSubjects.bootstrap_status_request,
      emptyRequestSchema.parse({}),
      bootstrapStatusResponseSchema,
      { timeoutMs: this.#timeouts.fastMs },
    );
    return {
      isInBootstrapMode: reply.is_in_bootstrap_mode,
      message: reply.message,
    };
  }

  async login(credentials: LoginCredentials): Promise<LoginOutcome> {
    const body = loginRequestSchema.parse(credentials);
    const reply = await this.#call(SUBJECTS.login, body, loginResponseSchema, {
      timeoutMs: this.#timeouts.fastMs,
    });

    if (!reply.success || reply.token.length === 0) {
      return {
        kind: 'rejected',
        message: reply.errorMessage.length > 0 ? reply.errorMessage : reply.message,
      };
    }

    this.#session = {
      token: reply.token,
      sessionId: reply.sessionId,
      refreshInFlight: undefined,
      workspace: undefined,
    };

    if (reply.selectedPartyId.length > 0) {
      const party = reply.availableParties.find((candidate) => candidate.id === reply.selectedPartyId);
      if (party !== undefined) {
        return toActive(reply, party);
      }
    }

    return {
      kind: 'party-selection-required',
      accountId: reply.accountId,
      tenantId: reply.tenantId,
      tenantName: reply.tenantName,
      username: reply.username,
      email: reply.email,
      availableParties: reply.availableParties,
      defaultPartyId: reply.defaultPartyId.length > 0 ? reply.defaultPartyId : null,
      passwordResetRequired: reply.passwordResetRequired,
      accessLifetimeSeconds: reply.accessLifetimeSeconds,
      sessionId: reply.sessionId,
    };
  }

  /**
   * Chooses the party for the session the login opened.
   *
   * Only the single-use token from `login` is accepted here; a token that
   * already has a party must use {@link switchParty}.
   */
  async selectParty(input: {
    readonly partyId: string;
    readonly expected: PartySelectionRequired;
  }): Promise<ActiveSession> {
    const reply = await this.#authenticatedCall(
      SUBJECTS.selectParty,
      partyRequestSchema.parse({ party_id: input.partyId }),
      partyResponseSchema,
      { timeoutMs: this.#timeouts.fastMs },
    );

    if (!reply.success || reply.token.length === 0) {
      throw new SessionExpiredError('token_expired', SUBJECTS.selectParty);
    }
    this.#replaceToken(reply.token);

    const party = input.expected.availableParties.find((candidate) => candidate.id === input.partyId);
    if (party === undefined) {
      throw new NotAuthenticatedError(
        `Server accepted party ${input.partyId} but did not list it`,
      );
    }

    return {
      kind: 'active',
      token: reply.token,
      accountId: input.expected.accountId,
      tenantId: input.expected.tenantId,
      tenantName: reply.tenantName.length > 0 ? reply.tenantName : input.expected.tenantName,
      username: reply.username.length > 0 ? reply.username : input.expected.username,
      email: input.expected.email,
      party,
      availableParties: input.expected.availableParties,
      accessLifetimeSeconds: reply.accessLifetimeSeconds,
      passwordResetRequired: input.expected.passwordResetRequired,
      sessionId: input.expected.sessionId,
    };
  }

  /** Re-scopes an already-active session to another party. */
  async switchParty(input: {
    readonly partyId: string;
    readonly availableParties: readonly PartySummary[];
  }): Promise<{ readonly token: string; readonly party: PartySummary; readonly accessLifetimeSeconds: number }> {
    const reply = await this.#authenticatedCall(
      SUBJECTS.switchParty,
      partyRequestSchema.parse({ party_id: input.partyId }),
      partyResponseSchema,
      { timeoutMs: this.#timeouts.fastMs },
    );
    if (!reply.success || reply.token.length === 0) {
      throw new SessionExpiredError('token_expired', SUBJECTS.switchParty);
    }
    this.#replaceToken(reply.token);

    const party = input.availableParties.find((candidate) => candidate.id === input.partyId);
    if (party === undefined) {
      throw new NotAuthenticatedError(`Server accepted party ${input.partyId} but did not list it`);
    }
    return {
      token: reply.token,
      party,
      accessLifetimeSeconds: reply.accessLifetimeSeconds,
    };
  }

  /** Ends the session, best effort. The local state is cleared either way. */
  async logout(): Promise<void> {
    if (this.#session !== undefined) {
      await this.#authenticatedCall(SUBJECTS.logout, {}, logoutResponseSchema, {
        timeoutMs: 3_000,
      }).catch(() => undefined);
    }
    this.#session = undefined;
  }

  /**
   * Exchanges the current token for a fresh one.
   *
   * Concurrent callers share one refresh, so a burst of expired requests does
   * not storm the server.
   *
   * @throws {NotAuthenticatedError} when there is no session to refresh.
   * @throws {SessionExpiredError} when the server refuses the refresh.
   */
  async refresh(): Promise<string> {
    const session = this.#requireSession();
    session.refreshInFlight ??= this.#performRefresh(session).finally(() => {
      session.refreshInFlight = undefined;
    });
    return session.refreshInFlight;
  }

  /**
   * Listens for changes to an entity.
   *
   * The payload is a change notification: when it happened, which records, and
   * whose they are. The time and the records are handed on. The time says whether
   * what is on screen is older than what exists; the records say how many, so a
   * screen can say so, and which, so it can badge them without comparing
   * timestamps.
   *
   * Nothing is authenticated here. These are published events, not replies, so
   * there is no session to present and no failure to report: a subscription that
   * cannot be made is a screen that does not hear about changes, which is the
   * behaviour it had before.
   */
  subscribeToEvents(
    relative: string,
    onEvent: (change: { readonly at: string; readonly ids: readonly string[] }) => void,
  ): () => void {
    const subscribe = this.#transport.subscribe;
    if (subscribe === undefined) {
      return () => undefined;
    }
    return subscribe.call(this.#transport, relative, (payload) => {
      try {
        const decoded = this.#codec.decodeAs(payload, changeEventSchema);
        onEvent({ at: decoded.timestamp, ids: decoded.alpha2_codes });
      } catch {
        // An event this build does not understand is one it cannot act on.
      }
    });
  }

  /** Sets the workspace context sent on every subsequent authenticated call. */
  setWorkspace(context: WorkspaceContext | undefined): void {
    if (this.#session !== undefined) {
      this.#session.workspace = context;
    }
  }

  /** Lists one page of accounts. */
  async listAccounts(input: {
    readonly offset?: number;
    readonly limit?: number;
  } = {}): Promise<WireAccountPage> {
    return this.#authenticatedCall(
      SUBJECTS.listAccounts,
      listAccountsRequestSchema.parse(input),
      accountPageSchema,
      { timeoutMs: this.#timeouts.fastMs },
    );
  }

  /**
   * Issues an authenticated call against an explicit subject.
   *
   * Exposed so the account operations in `account-operations.ts` can be plain
   * functions over a narrow interface instead of methods that grow the client
   * one subject at a time.
   */
  callAuthenticated<Schema extends z.ZodType>(
    subject: string,
    body: unknown,
    schema: Schema,
  ): Promise<z.infer<Schema>> {
    return this.#authenticatedCall(subject, body, schema, {
      timeoutMs: this.#timeouts.fastMs,
    });
  }

  /**
   * Discovers the companion HTTP server address.
   *
   * The Qt client makes this call immediately after login. It is best effort:
   * a deployment without an HTTP server simply has no base URL.
   */
  async discoverHttpBaseUrl(): Promise<string | null> {
    const reply = await this.#authenticatedCall(
      SUBJECTS.httpInfo,
      {},
      httpInfoResponseSchema,
      { timeoutMs: this.#timeouts.fastMs },
    ).catch(() => null);
    if (reply === null || !reply.success || reply.base_url.length === 0) {
      return null;
    }
    return reply.base_url;
  }

  async close(): Promise<void> {
    this.#session = undefined;
    await this.#transport.close();
  }

  /** Exposed so callers can build a proactive refresh timer. */
  get token(): string {
    return this.#requireSession().token;
  }

  async #performRefresh(session: SessionState): Promise<string> {
    const reply = await this.#call(SUBJECTS.refresh, {}, refreshResponseSchema, {
      timeoutMs: this.#timeouts.fastMs,
      headers: { Authorization: `Bearer ${session.token}` },
    });
    if (!reply.success || reply.token.length === 0) {
      // The server reports a session that outlived its maximum as a body
      // field, never as X-Error. Both spellings mean re-authentication.
      throw new SessionExpiredError('max_session_exceeded', SUBJECTS.refresh);
    }
    session.token = reply.token;
    return reply.token;
  }

  async #call<Schema extends z.ZodType>(
    subject: string,
    body: unknown,
    schema: Schema,
    options: {
      readonly timeoutMs: number;
      readonly headers?: HeaderSource;
    },
  ): Promise<z.infer<Schema>> {
    const headers = resolveHeaders(options.headers);
    const reply = await this.#transport.request(
      subject,
      this.#codec.encode(body),
      headers,
      options.timeoutMs,
    );
    return this.#codec.decodeAs(reply.body, schema);
  }

  async #authenticatedCall<Schema extends z.ZodType>(
    subject: string,
    body: unknown,
    schema: Schema,
    options: { readonly timeoutMs: number },
  ): Promise<z.infer<Schema>> {
    const session = this.#requireSession();
    const reply = await this.#transport.request(
      subject,
      this.#codec.encode(body),
      this.#authenticatedHeaders(session),
      options.timeoutMs,
    );

    const serverError = serverErrorCode(reply.headers);
    if (serverError === undefined) {
      return this.#codec.decodeAs(reply.body, schema);
    }
    if (serverError === 'token_expired') {
      await this.refresh();
      return this.#retryAuthenticated(subject, body, schema, options.timeoutMs);
    }
    throw errorForServerCode(serverError, subject);
  }

  async #retryAuthenticated<Schema extends z.ZodType>(
    subject: string,
    body: unknown,
    schema: Schema,
    timeoutMs: number,
  ): Promise<z.infer<Schema>> {
    const session = this.#requireSession();
    const reply = await this.#transport.request(
      subject,
      this.#codec.encode(body),
      this.#authenticatedHeaders(session),
      timeoutMs,
    );
    const serverError = serverErrorCode(reply.headers);
    if (serverError !== undefined) {
      throw errorForServerCode(serverError, subject);
    }
    return this.#codec.decodeAs(reply.body, schema);
  }

  #authenticatedHeaders(session: SessionState): Record<string, string> {
    // A fresh trace key per top-level operation, matching ClientManager.
    const headers: Record<string, string> = {
      ...tracingHeaders(session.sessionId, this.#generateId),
      Authorization: `Bearer ${session.token}`,
      'X-Workspace-Id': session.workspace?.workspaceId ?? LIVE_WORKSPACE_ID,
    };
    if (session.workspace?.resolutionOrder !== undefined) {
      headers['X-Workspace-Resolution'] = session.workspace.resolutionOrder.join(',');
    }
    return headers;
  }

  #requireSession(): SessionState {
    if (this.#session === undefined) {
      throw new NotAuthenticatedError('No session: call login() first');
    }
    return this.#session;
  }

  #replaceToken(token: string): void {
    this.#requireSession().token = token;
  }
}

/**
 * A change notification, as the services publish it.
 *
 * Every member has a default: an event is news rather than a contract, and a
 * notification that cannot be read is worth less than one that can be read
 * partially. A missing time means the screen cannot tell whether it is stale, so
 * it does nothing, which is the safe direction.
 */
const changeEventSchema = z.object({
  timestamp: z.string().default(''),
  alpha2_codes: z.array(z.string()).default([]),
  tenant_id: z.string().default(''),
});

/**
 * The failure a server error code means.
 *
 * Every code used to be reported as an expired session, including a refusal to
 * authorise. That is a costly lie: an expired session is something a person can
 * act on by signing in again, and a refusal is not, so the one thing the message
 * told them to do was the one thing that could not help. It also cost real time
 * here, where a permission problem read as a session problem for hours.
 *
 * The codes the server can send are enumerated, so a code with no case here is a
 * protocol change rather than a runtime surprise, and it is carried through as
 * itself rather than renamed.
 */
function errorForServerCode(code: ServerErrorCode, subject: string): ProtocolError {
  switch (code) {
    case 'token_expired':
    case 'max_session_exceeded':
      return new SessionExpiredError(code, subject);
    case 'unauthorized':
      return new NotAuthenticatedError(`The server requires authentication for ${subject}`);
    default:
      // `forbidden` and `bad_request` are the server understanding the request
      // and declining it, which is not a session problem.
      return new ServerError(code, subject);
  }
}

/** Reads the `X-Error` header, if the server set one. */
function serverErrorCode(
  headers: Readonly<Record<string, string>>,
): ServerErrorCode | undefined {
  const code = headers['X-Error'];
  return code === undefined ? undefined : (code as ServerErrorCode);
}

function toActive(reply: LoginResponse, party: PartySummary): ActiveSession {
  return {
    kind: 'active',
    token: reply.token,
    accountId: reply.accountId,
    tenantId: reply.tenantId,
    tenantName: reply.tenantName,
    username: reply.username,
    email: reply.email,
    party,
    availableParties: reply.availableParties,
    accessLifetimeSeconds: reply.accessLifetimeSeconds,
    passwordResetRequired: reply.passwordResetRequired,
    sessionId: reply.sessionId,
  };
}

/** Convenience for callers that only need the error type. */
export type { ProtocolError };
export { emptyRequestSchema };
