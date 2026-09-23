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
import { SOURCE_LANGUAGE, type Language } from './languages.js';

/**
 * Translation, with the key set checked at build time.
 *
 * The English messages are the source of truth. Every other language is checked
 * against them by a schema, so a missing key fails the tests rather than falling
 * back silently to English in front of a user who cannot read it. That check is
 * the whole reason this is a small module rather than a library: a translation
 * that is missing is a defect, and it should be as loud as a type error.
 *
 * Messages are keyed by a dot path, `entity.accounts.title`. Keys are grouped in
 * the catalogue by area, so a hundred entities do not produce a hundred keys in
 * one flat namespace.
 */

/**
 * The catalogue shape.
 *
 * Values are either a message or a group of messages. The recursion is what lets
 * the catalogue be nested while the keys stay flat strings.
 */
export type Catalogue = {
  readonly [key: string]: string | Catalogue;
};

/** The English catalogue, and therefore the shape every other one must match. */
export type SourceCatalogue = Catalogue;

/**
 * Flattens a nested catalogue into dot paths.
 *
 * Exported because the consistency test needs the same view of the keys that
 * the runtime does, and deriving it twice is how the two drift.
 */
export function flatten(catalogue: Catalogue, prefix = ''): Record<string, string> {
  const flat: Record<string, string> = {};
  for (const [key, value] of Object.entries(catalogue)) {
    const path = prefix.length === 0 ? key : `${prefix}.${key}`;
    if (typeof value === 'string') {
      flat[path] = value;
    } else {
      Object.assign(flat, flatten(value, path));
    }
  }
  return flat;
}

/** The flat key set of the English catalogue. */
export type MessageKey = string;

export interface InterpolationValues {
  readonly [name: string]: string | number;
}

/**
 * Substitutes `{name}` placeholders.
 *
 * A placeholder with no value is left as written rather than replaced with an
 * empty string, so a missing value is visible in the interface instead of
 * silently producing a sentence with a hole in it.
 */
export function interpolate(message: string, values: InterpolationValues): string {
  return message.replace(/\{(\w+)\}/g, (match, name: string) => {
    const value = values[name];
    return value === undefined ? match : String(value);
  });
}

export interface Translator {
  /** The active language. */
  readonly language: Language;
  /**
   * Translates a key.
   *
   * Falls back to the source language, then to the key itself. Returning the key
   * is deliberate: a screen showing `entity.accounts.title` is a screen somebody
   * reports, where an empty string is a screen somebody ignores.
   */
  readonly t: (key: string, values?: InterpolationValues) => string;
  /** The message for a count, choosing between singular and plural forms. */
  readonly plural: (
    key: string,
    count: number,
    values?: InterpolationValues,
  ) => string;
}

/**
 * Chooses between singular and plural using the platform's own rules.
 *
 * `Intl.PluralRules` knows that English has two forms and that French puts zero
 * in the singular, which a hand-written `count === 1` gets wrong. Languages with
 * more than two forms need more than two keys; the rule here is that a key with
 * no plural suffix is used for every count.
 */
export function selectPluralForm(
  catalogue: Record<string, string>,
  key: string,
  count: number,
  language: Language,
): string | undefined {
  const rules = new Intl.PluralRules(language);
  const category = rules.select(count);
  return catalogue[`${key}.${category}`] ?? catalogue[`${key}.other`] ?? catalogue[key];
}

/** Builds a translator for one language against the source catalogue. */
export function createTranslator(
  language: Language,
  source: Record<string, string>,
  target: Record<string, string>,
): Translator {
  const lookup = (key: string): string | undefined =>
    // The active language first, then the source, then nothing.
    (language === SOURCE_LANGUAGE ? target[key] : target[key] ?? source[key]) ?? undefined;

  return {
    language,
    t: (key, values) => {
      const message = lookup(key) ?? key;
      return values === undefined ? message : interpolate(message, values);
    },
    plural: (key, count, values) => {
      const chosen =
        selectPluralForm(target, key, count, language) ??
        (language === SOURCE_LANGUAGE ? undefined : selectPluralForm(source, key, count, language)) ??
        key;
      return values === undefined
        ? interpolate(chosen, { count })
        : interpolate(chosen, { count, ...values });
    },
  };
}

/**
 * Builds the schema that checks a translation against the source.
 *
 * Every key in the source must be present, and no key may be present that the
 * source does not have, because an extra key is a key somebody translated after
 * the message was renamed. There is no exemption: every message in the
 * catalogue is a sentence somebody chose, so every one of them is translated.
 */
export function catalogueSchema(
  source: Record<string, string>,
): z.ZodType<Record<string, string>> {
  const keys = Object.keys(source);
  return z
    .record(z.string(), z.string())
    .superRefine((value, ctx) => {
      const missing = keys.filter((key) => !(key in value));
      const extra = Object.keys(value).filter((key) => !(key in source));
      if (missing.length > 0) {
        ctx.addIssue({
          code: 'custom',
          message: `Missing ${missing.length} key(s): ${missing.slice(0, 10).join(', ')}`,
        });
      }
      if (extra.length > 0) {
        ctx.addIssue({
          code: 'custom',
          message: `Unknown ${extra.length} key(s): ${extra.slice(0, 10).join(', ')}`,
        });
      }
    });
}
