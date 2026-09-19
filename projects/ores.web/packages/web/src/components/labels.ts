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
 * Turning an identifier into something a person can read.
 *
 * Entity and shortcut names are not in the catalogue up front. There are over a
 * hundred entities and only a few are built, and inventing translations for the
 * rest would put words nobody chose into a language file and make the translation
 * check meaningless. So an entity's name is its identifier until somebody
 * translates it, and the identifier is turned into words here.
 *
 * When an entity is built, its name and description are added to the catalogue
 * under the key the identifier already produces, and this stops being reached for
 * that entity. Nothing moves; the words simply appear.
 */

/** Words that are spoken as letters or that humanising would mangle. */
const SPECIAL: Readonly<Record<string, string>> = {
  iam: 'IAM',
  dq: 'Data quality',
  cds: 'CDS',
  crm: 'CRM',
  ibor: 'IBOR',
  ois: 'OIS',
  fra: 'FRA',
  ir: 'IR',
  id: 'ID',
  uuid: 'UUID',
  api: 'API',
  totp: 'TOTP',
  fpml: 'FpML',
  csv: 'CSV',
  xml: 'XML',
};

/**
 * Splits a camelCase or kebab-case identifier into words and capitalises the
 * first.
 *
 * `currencyPair` becomes `Currency pair`, `market-data` becomes `Market data`,
 * `legalEntityIdentifier` becomes `Legal entity identifier`.
 */
export function humanise(id: string): string {
  const special = SPECIAL[id.toLowerCase()];
  if (special !== undefined) return special;

  const words = id
    .replace(/([a-z0-9])([A-Z])/g, '$1 $2')
    .replace(/[_-]+/g, ' ')
    .trim()
    .split(/\s+/)
    .filter((word) => word.length > 0);

  if (words.length === 0) return id;

  const [first, ...rest] = words as [string, ...string[]];
  const head = first.charAt(0).toUpperCase() + first.slice(1).toLowerCase();
  const tail = rest.map((word) => {
    const known = SPECIAL[word.toLowerCase()];
    return known ?? word.toLowerCase();
  });
  return [head, ...tail].join(' ');
}

/**
 * A translated name, or the humanised identifier when there is no translation.
 *
 * The translator returns the key when a message is missing, which is the right
 * behaviour for a label somebody will report and the wrong behaviour for a menu
 * entry. This turns the second case into words.
 */
export function translatedOrHumanised(
  t: (key: string) => string,
  key: string,
  id: string,
): string {
  const value = t(key);
  return value === key ? humanise(id) : value;
}
