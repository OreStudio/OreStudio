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

import { connect, headers as natsHeaders, type NatsConnection, type MsgHdrs } from 'nats';
import { RequestTimeoutError, ServiceUnavailableError, TransportError } from './errors.js';
import { CONTENT_ENCODING_HEADER, GZIP_ENCODING } from './codec.js';

/** Headers attached to an outbound request. */
export type RequestHeaders = Readonly<Record<string, string>>;

/** A decoded reply from the bus. */
export interface Reply {
  readonly subject: string;
  /** Empty when the server replied with an error header and no body. */
  readonly body: Uint8Array;
  readonly headers: RequestHeaders;
}

/**
 * The subset of bus behaviour the domain layer depends on.
 *
 * Keeping this narrow lets tests drive the client with a scripted transport,
 * and leaves room for a future in-browser transport (WebSocket to NATS) or a
 * Tauri IPC transport without touching the domain layer.
 */
export interface Transport {
  request(subject: string, body: Uint8Array, headers: RequestHeaders, timeoutMs: number): Promise<Reply>;
  close(): Promise<void>;
  /**
   * Listens on a subject, and returns the way to stop.
   *
   * Optional because a transport that cannot listen is still a usable transport
   * for everything that asks and answers; the capability is checked rather than
   * assumed.
   */
  subscribe?(relative: string, onMessage: (payload: Uint8Array) => void): () => void;
}

/** mTLS material, as file paths or inline PEM. */
export interface TlsMaterial {
  /** Path to the CA bundle, or its PEM text. */
  readonly ca: string;
  /** Path to the client certificate, or its PEM text. */
  readonly cert: string;
  /** Path to the client private key, or its PEM text. */
  readonly key: string;
}

export interface NatsTransportOptions {
  /** Server address, for example {@code nats://localhost:21805}. */
  readonly server: string;
  /**
   * Subject prefix prepended to every relative subject, for example
   * {@code ores.dev.festive.dijkstra}. The server rejects a client with no
   * prefix, so this is required by construction.
   */
  readonly subjectPrefix: string;
  readonly tls: TlsMaterial;
  readonly name?: string;
  readonly reconnectWaitMs?: number;
  readonly maxReconnectAttempts?: number;
}

/**
 * NATS transport over TCP with mutual TLS.
 *
 * The deployment's server config sets {@code verify: true}, so the client
 * certificate is mandatory. That is also why a browser cannot host this
 * transport directly and a BFF owns the connection instead.
 */
export class NatsTransport implements Transport {
  readonly #options: NatsTransportOptions;
  #connection: NatsConnection | undefined;

  constructor(options: NatsTransportOptions) {
    if (options.subjectPrefix.length === 0) {
      throw new TransportError('subjectPrefix is required: the server rejects unprefixed clients');
    }
    this.#options = options;
  }

  /** True once the connection is live; false before connect and after close. */
  get connected(): boolean {
    return this.#connection !== undefined && !this.#connection.isClosed();
  }

  get subjectPrefix(): string {
    return this.#options.subjectPrefix;
  }

  async connect(): Promise<void> {
    if (this.connected) {
      return;
    }
    try {
      this.#connection = await connect({
        servers: this.#options.server,
        name: this.#options.name ?? 'ores.web.bff',
        tls: {
          ca: this.#options.tls.ca,
          cert: this.#options.tls.cert,
          key: this.#options.tls.key,
        },
        reconnect: true,
        ...(this.#options.reconnectWaitMs === undefined
          ? {}
          : { reconnectTimeWait: this.#options.reconnectWaitMs }),
        ...(this.#options.maxReconnectAttempts === undefined
          ? {}
          : { maxReconnectAttempts: this.#options.maxReconnectAttempts }),
      });
    } catch (cause) {
      throw new TransportError(`Could not connect to ${this.#options.server}`, { cause });
    }
  }

  /** Prepends the configured prefix, matching {@code client::make_subject}. */
  absoluteSubject(relative: string): string {
    return `${this.#options.subjectPrefix}.${relative}`;
  }

  /**
   * Listens on a subject, and returns the way to stop.
   *
   * The first thing here that is not a request. Change notifications are
   * published rather than asked for, so there is nothing to reply to and no
   * timeout: the handler is called for as long as the subscription lives.
   *
   * Messages are read in a subscription loop that ends when the subscription
   * does, which is what makes the returned function sufficient to clean up.
   * The payload is handed over undecoded, because what a message means belongs
   * to whoever subscribed and not to the transport.
   */
  subscribe(relative: string, onMessage: (payload: Uint8Array) => void): () => void {
    const connection = this.#connection;
    if (connection === undefined || connection.isClosed()) {
      throw new TransportError('Cannot subscribe: not connected');
    }

    const subscription = connection.subscribe(this.absoluteSubject(relative));

    // Started and not awaited: this runs for the life of the subscription, and
    // the caller's next line must not wait for a message that may never come.
    void (async () => {
      for await (const message of subscription) {
        onMessage(message.data);
      }
    })().catch(() => {
      // A closed connection ends the loop, which is how this is meant to stop.
    });

    return () => {
      subscription.unsubscribe();
    };
  }

  async request(
    subject: string,
    body: Uint8Array,
    headers: RequestHeaders,
    timeoutMs: number,
  ): Promise<Reply> {
    const connection = this.#connection;
    if (connection === undefined || connection.isClosed()) {
      throw new TransportError('Not connected');
    }

    const absolute = this.absoluteSubject(subject);
    const msgHeaders = toMsgHeaders(headers);
    let reply;
    try {
      reply = await connection.request(absolute, body, {
        timeout: timeoutMs,
        ...(msgHeaders === undefined ? {} : { headers: msgHeaders }),
      });
    } catch (cause) {
      throw translateRequestFailure(cause, absolute, timeoutMs);
    }

    // A reply with no body and no headers is a NATS status message, which
    // means nothing is subscribed to the subject.
    if (reply.data.length === 0 && reply.headers === undefined) {
      throw new ServiceUnavailableError(absolute);
    }

    const responseHeaders = headersToRecord(reply.headers);
    return {
      subject: reply.subject,
      body: await decompressBody(reply.data, responseHeaders),
      headers: responseHeaders,
    };
  }

  async close(): Promise<void> {
    const connection = this.#connection;
    this.#connection = undefined;
    if (connection !== undefined) {
      await connection.drain().catch(() => connection.close());
    }
  }
}

/** Builds the header block for a request. */
function toMsgHeaders(headers: RequestHeaders): MsgHdrs | undefined {
  const keys = Object.keys(headers);
  if (keys.length === 0) {
    // Pre-login calls carry no headers at all, matching the C++ client.
    return undefined;
  }
  const block = natsHeaders();
  for (const key of Object.keys(headers)) {
    const value = headers[key];
    if (value !== undefined) {
      block.set(key, value);
    }
  }
  return block;
}

/** nats.js signals these by code rather than by class. */
function translateRequestFailure(cause: unknown, subject: string, timeoutMs: number): Error {
  const code = (cause as { code?: string } | undefined)?.code;
  if (code === 'TIMEOUT') {
    return new RequestTimeoutError(subject, timeoutMs);
  }
  if (code === 'NO_RESPONDERS') {
    return new ServiceUnavailableError(subject, { cause });
  }
  return new TransportError(`Request to ${subject} failed: ${describe(cause)}`, { cause });
}

function describe(cause: unknown): string {
  return cause instanceof Error ? cause.message : String(cause);
}

/** Reads the reply's headers into a plain record. */
function headersToRecord(headers: MsgHdrs | undefined): RequestHeaders {
  if (headers === undefined) {
    return {};
  }
  const record: Record<string, string> = {};
  for (const key of headers.keys()) {
    record[key] = headers.get(key);
  }
  return record;
}

/**
 * Undoes the transparent compression the server applies to large bodies.
 *
 * The C++ client does the same in `extract_message`, so every layer above the
 * transport sees the original bytes regardless of how they travelled. The
 * server gzips only when the body is at or above its threshold, so most
 * replies pass through unchanged.
 */
async function decompressBody(
  body: Uint8Array,
  responseHeaders: RequestHeaders,
): Promise<Uint8Array> {
  if (responseHeaders[CONTENT_ENCODING_HEADER] !== GZIP_ENCODING || body.length === 0) {
    return body;
  }
  const decompressed = new DecompressionStream('gzip');
  const source = new ReadableStream<Uint8Array>({
    start(controller) {
      controller.enqueue(body);
      controller.close();
    },
  });
  const stream = source.pipeThrough(decompressed) as ReadableStream<Uint8Array>;
  return new Uint8Array(await new Response(stream).arrayBuffer());
}
