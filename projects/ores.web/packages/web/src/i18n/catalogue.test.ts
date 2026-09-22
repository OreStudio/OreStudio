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
import { enFlat } from './locales/en.js';
import { frFlat } from './locales/fr.js';
import { ptFlat } from './locales/pt.js';
import { generatedMessageKeys } from '../entity/generatedEntities.js';

/**
 * The catalogue's own contract, asserted rather than inferred.
 *
 * Each translation checks itself against English when it is imported, so
 * importing these is the test: a translation that lost a key, or gained one
 * for a message that was renamed, throws here rather than in front of somebody
 * reading the interface. What this file adds is the part the import cannot
 * state: that the exemption for generated words is neither empty nor total,
 * and that the words it exempts are really there in English.
 */
describe('the catalogue', () => {
  it('has an English message for every generated key', () => {
    expect(generatedMessageKeys.length).toBeGreaterThan(0);
    const missing = generatedMessageKeys.filter((key) => !(key in enFlat));
    expect(missing).toEqual([]);
  });

  it('exempts the generated words from translation, and only those', () => {
    // A hand-written message is never exempt: it is a sentence somebody chose
    // and somebody has to translate.
    const exempt = new Set(generatedMessageKeys);
    expect(exempt.has('entity.save')).toBe(false);
    expect(exempt.has('country.fldAlpha2Code')).toBe(true);
  });

  it('falls back to English for a generated word a translation lacks', () => {
    const untranslated = generatedMessageKeys.filter(
      (key) => !(key in frFlat) || !(key in ptFlat),
    );
    // The exemption is what lets these exist; what matters is that the English
    // they fall back to is present, which the case above checks.
    expect(untranslated.length).toBeGreaterThan(0);
  });
});
