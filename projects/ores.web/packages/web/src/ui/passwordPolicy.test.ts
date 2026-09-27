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
import { assessPassword } from './passwordPolicy.js';

describe('assessPassword', () => {
  it('rates an empty password zero', () => {
    expect(assessPassword('')).toMatchObject({ valid: false, strength: 0 });
  });

  it('names each rule a password misses', () => {
    const result = assessPassword('abcdefghijkl');
    expect(result.valid).toBe(false);
    expect([...result.met].sort()).toEqual(['length', 'lower']);
  });

  it('accepts a password that meets every server rule', () => {
    expect(assessPassword('Abcdefgh123!')).toMatchObject({ valid: true, strength: 3 });
  });

  it('rates a longer valid password strongest', () => {
    expect(assessPassword('Abcdefgh123!wxyz').strength).toBe(4);
  });

  it('counts only the server special characters', () => {
    expect(assessPassword('Abcdefgh1234~').met.has('special')).toBe(false);
  });
});
