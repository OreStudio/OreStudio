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

import { detectLanguage, type Language } from './languages.js';

/**
 * Remembering which language a person chose.
 *
 * Stored per browser rather than per account, because the choice is about the
 * person reading the screen and not about the record they are looking at. A
 * signed-out visitor has a language too.
 */
const STORAGE_KEY = 'ores.web.language';

export function readStoredLanguage(): Language | undefined {
  try {
    const stored = globalThis.localStorage?.getItem(STORAGE_KEY);
    return stored === null || stored === undefined ? undefined : detectLanguage([stored]);
  } catch {
    // Storage can be unavailable, in a private window or with a policy against
    // it. A missing preference is not worth failing over.
    return undefined;
  }
}

export function storeLanguage(language: Language): void {
  try {
    globalThis.localStorage?.setItem(STORAGE_KEY, language);
  } catch {
    // As above: the choice simply does not persist.
  }
}

/** The language to start in: the stored choice, then the browser's, then English. */
export function initialLanguage(): Language {
  const stored = readStoredLanguage();
  if (stored !== undefined) {
    return stored;
  }
  const preferred =
    globalThis.navigator?.languages ?? [globalThis.navigator?.language ?? 'en'];
  return detectLanguage(preferred);
}
