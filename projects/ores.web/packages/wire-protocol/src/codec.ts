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

import { decode, encode, type DecoderOptions, type EncoderOptions } from '@msgpack/msgpack';
import { z } from 'zod';
import { MalformedResponseError } from './errors.js';

/** gzip marker the server sets on compressed bodies. */
export const CONTENT_ENCODING_HEADER = 'X-Content-Encoding';
export const GZIP_ENCODING = 'gzip';

/** The two body encodings the server supports, chosen at process start. */
export type WireFormat = 'msgpack' | 'json';

const MSGPACK_ENCODE_OPTIONS: EncoderOptions = {
  /**
   * `false` keeps plain numbers as numbers. The server encodes integers with
   * msgpack-c's smallest-fit rule, and decoding to `bigint` would make every
   * id and count awkward to compare.
   */
  useBigInt64: false,
  /** Omit `undefined` keys, matching how C++ drops an empty `std::optional`. */
  ignoreUndefined: true,
  sortKeys: false,
};

const MSGPACK_DECODE_OPTIONS: DecoderOptions = {
  useBigInt64: false,
};

/**
 * Encodes and decodes NATS bodies.
 *
 * The server fixes one format per process from {@code ORES_NATS_WIRE_FORMAT},
 * with no per-message negotiation, so the codec is likewise fixed at
 * construction. msgpack is the deployment default.
 */
export class WireCodec {
  readonly #format: WireFormat;

  constructor(format: WireFormat) {
    this.#format = format;
  }

  get format(): WireFormat {
    return this.#format;
  }

  encode(value: unknown): Uint8Array {
    switch (this.#format) {
      case 'msgpack':
        return encode(value, MSGPACK_ENCODE_OPTIONS);
      case 'json':
        return new TextEncoder().encode(JSON.stringify(value));
    }
  }

  /**
   * Decodes a body into an unvalidated value.
   *
   * The result is `unknown` on purpose: callers parse it with a schema before
   * any field is trusted.
   */
  decode(body: Uint8Array): unknown {
    try {
      switch (this.#format) {
        case 'msgpack':
          return decode(body, MSGPACK_DECODE_OPTIONS);
        case 'json':
          return JSON.parse(new TextDecoder().decode(body)) as unknown;
      }
    } catch (cause) {
      throw new MalformedResponseError(`Body is not valid ${this.#format}`, { cause });
    }
  }

  /** Decodes and validates in one step. */
  decodeAs<Schema extends z.ZodType>(body: Uint8Array, schema: Schema): z.infer<Schema> {
    const parsed = schema.safeParse(this.decode(body));
    if (!parsed.success) {
      throw new MalformedResponseError(
        `Body does not match the expected shape: ${z.prettifyError(parsed.error)}`,
        { cause: parsed.error },
      );
    }
    return parsed.data;
  }
}
