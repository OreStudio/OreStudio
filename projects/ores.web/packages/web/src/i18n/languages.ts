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
 * The languages this interface speaks.
 *
 * Three, deliberately, from the beginning. One language hides every problem
 * there is with translation: a missing key looks like a working screen, and a
 * layout that only works because English words are short looks like a working
 * layout. Three is enough to make those failures visible while the interface is
 * still small enough to fix them cheaply.
 */
export const LANGUAGES = ['en', 'pt', 'fr'] as const;

export type Language = (typeof LANGUAGES)[number];

/** The language a message is written in first, and the fallback for the rest. */
export const SOURCE_LANGUAGE: Language = 'en';

export interface LanguageInfo {
  readonly code: Language;
  /** The name in the language itself, which is how a person recognises it. */
  readonly name: string;
  /** The name in English, for a person who does not read the language. */
  readonly englishName: string;
}

export const LANGUAGE_INFO: Readonly<Record<Language, LanguageInfo>> = {
  en: { code: 'en', name: 'English', englishName: 'English' },
  pt: { code: 'pt', name: 'Português', englishName: 'Portuguese' },
  fr: { code: 'fr', name: 'Français', englishName: 'French' },
};

export function isLanguage(value: string): value is Language {
  return (LANGUAGES as readonly string[]).includes(value);
}

/**
 * The language to start in.
 *
 * The browser's own preferences come first, so a Portuguese speaker gets
 * Portuguese without asking. Matching is by primary subtag, so `pt-BR` and
 * `pt-PT` both find `pt`.
 */
export function detectLanguage(preferred: readonly string[]): Language {
  for (const tag of preferred) {
    const primary = tag.split('-')[0]?.toLowerCase();
    if (primary !== undefined && isLanguage(primary)) {
      return primary;
    }
  }
  return SOURCE_LANGUAGE;
}
