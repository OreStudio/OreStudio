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
import { WireCodec } from './codec.js';
import { MalformedResponseError } from './errors.js';
import { z } from 'zod';

describe('WireCodec', () => {
  it('encodes an empty struct as the empty msgpack map', () => {
    const codec = new WireCodec('msgpack');
    // 0x80 is the fixmap header with no entries. The server's empty request
    // structs (logout, refresh, http info) must arrive exactly like this.
    expect([...codec.encode({})]).toEqual([0x80]);
  });

  it('encodes a string field as fixstr plus its bytes', () => {
    const codec = new WireCodec('msgpack');
    const bytes = [...codec.encode({ principal: 'ab' })];
    // map(1), "principal", str(2), 'a', 'b'
    expect(bytes).toEqual([0x81, 0xa9, 112, 114, 105, 110, 99, 105, 112, 97, 108, 0xa2, 97, 98]);
  });

  it('round-trips nested values through msgpack', () => {
    const codec = new WireCodec('msgpack');
    const value = {
      success: true,
      total_available_count: 26,
      names: ['a', 'b'],
      nested: { id: '00000000-0000-0000-0000-000000000001' },
    };
    expect(codec.decode(codec.encode(value))).toEqual(value);
  });

  it('round-trips through json', () => {
    const codec = new WireCodec('json');
    const value = { success: false, message: 'nope' };
    expect(codec.decode(codec.encode(value))).toEqual(value);
  });

  it('reports undecodable bodies as a malformed response', () => {
    const codec = new WireCodec('msgpack');
    expect(() => codec.decode(new Uint8Array([0xc1]))).toThrow(MalformedResponseError);
  });

  it('reports schema mismatches as a malformed response', () => {
    const codec = new WireCodec('msgpack');
    const body = codec.encode({ success: 'not-a-boolean' });
    expect(() => codec.decodeAs(body, z.object({ success: z.boolean() }))).toThrow(
      MalformedResponseError,
    );
  });

  it('returns the validated value on a schema match', () => {
    const codec = new WireCodec('msgpack');
    const body = codec.encode({ success: true });
    expect(codec.decodeAs(body, z.object({ success: z.boolean() }))).toEqual({ success: true });
  });
});
