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
  fromWireTimestamp,
  isUuid,
  isWireTimestamp,
  toWireTimestamp,
  uuid,
  wireTimestamp,
} from './primitives.js';

describe('uuid', () => {
  it('accepts the canonical lowercase form', () => {
    expect(isUuid('aef9c708-6d94-4d63-b315-0d6402cc5b46')).toBe(true);
  });

  it('rejects an uppercase form the server never writes', () => {
    expect(isUuid('AEF9C708-6D94-4D63-B315-0D6402CC5B46')).toBe(false);
  });

  it('rejects a truncated value', () => {
    expect(isUuid('aef9c708-6d94-4d63-b315')).toBe(false);
  });

  it('throws a TypeError when branding invalid input', () => {
    expect(() => uuid('nope')).toThrow(TypeError);
  });
});

describe('wire timestamp', () => {
  it('accepts the server spelling', () => {
    expect(isWireTimestamp('2026-09-18 15:50:44Z')).toBe(true);
  });

  it('rejects an ISO spelling with a T separator', () => {
    expect(isWireTimestamp('2026-09-18T15:50:44Z')).toBe(false);
  });

  it('rejects a date that does not exist', () => {
    expect(isWireTimestamp('2026-02-30 00:00:00Z')).toBe(false);
  });

  it('rejects an out-of-range time', () => {
    expect(isWireTimestamp('2026-02-01 24:00:00Z')).toBe(false);
  });

  it('converts a Date to the wire spelling with second resolution', () => {
    const instant = new Date(Date.UTC(2026, 8, 18, 15, 50, 44, 512));
    expect(toWireTimestamp(instant)).toBe('2026-09-18 15:50:44Z');
  });

  it('parses back to the same instant', () => {
    const value = wireTimestamp('2026-09-18 15:50:44Z');
    expect(fromWireTimestamp(value).toISOString()).toBe('2026-09-18T15:50:44.000Z');
  });
});
