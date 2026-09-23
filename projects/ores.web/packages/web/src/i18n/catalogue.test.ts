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
import { createTranslator } from './translate.js';
import type { Language } from './languages.js';

/**
 * The catalogue's own contract, asserted rather than inferred.
 *
 * Each translation checks itself against English when it is imported, so
 * importing these is most of the test: a translation that lost a key, or gained
 * one for a message that was renamed, throws there rather than in front of
 * somebody reading the interface. What this file adds is the part an import
 * cannot state: that no language is left without a catalogue, and that English
 * is the source the others fall back to rather than a peer of them.
 */
const TRANSLATED: Readonly<Record<Exclude<Language, 'en'>, Record<string, string>>> = {
  fr: frFlat,
  pt: ptFlat,
};

describe('the catalogue', () => {
  it('agrees with English on the key set, in every language', () => {
    const english = Object.keys(enFlat).sort();
    expect(english.length).toBeGreaterThan(0);
    for (const [language, flat] of Object.entries(TRANSLATED)) {
      expect({ language, keys: Object.keys(flat).sort() }).toEqual({
        language,
        keys: english,
      });
    }
  });

  it('is sourced in English, which a translation falls back to', () => {
    // A translation that carries one message, deliberately: every other key has
    // to come from English, and the message it does carry has to win.
    const partial: Record<string, string> = { 'app.name': 'ORE Studio (fr)' };
    const translator = createTranslator('fr', enFlat, partial);
    expect(translator.t('app.name')).toBe('ORE Studio (fr)');
    expect(translator.t('landing.signUp')).toBe('Sign up');
  });
});
