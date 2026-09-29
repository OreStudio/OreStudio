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
 * What a form proposes, and the shape a tenant code must have.
 *
 * These are proposals, not rules: the server refuses a code that does not hold
 * the shape below, so the form applies the same shape while a person types and
 * never offers a value the server will reject. The rest is the convenience the
 * old client had — a display name proposes a code, a code proposes a hostname —
 * and each one stops the moment somebody types in the field it proposed.
 */

/** The longest tenant code the server accepts. */
export const TENANT_CODE_MAX_LENGTH = 50;

/** Whether a code is one the server will accept. */
export function isTenantCode(code: string): boolean {
    return /^[a-z][a-z0-9_]{0,49}$/.test(code);
}

/**
 * The code a display name proposes.
 *
 * Lowercased; runs of spaces and hyphens become underscores; everything else
 * that is not a lowercase letter, a digit or an underscore is dropped; a
 * leading non-letter is stripped, because a code starts with a letter; and the
 * result is truncated to the length the server accepts.
 */
export function codeFromName(name: string): string {
    const slug = name
        .trim()
        .toLowerCase()
        .replace(/[ -]+/g, '_')
        .replace(/[^a-z0-9_]/g, '');
    const first = slug.search(/[a-z]/);
    return (first < 0 ? '' : slug.slice(first)).slice(0, TENANT_CODE_MAX_LENGTH);
}

/**
 * What a code becomes while somebody types it.
 *
 * The same shape `isTenantCode` states, applied rather than judged: the value
 * is lowercased, everything outside lowercase letters, digits and underscores
 * is dropped, a leading non-letter is dropped because a code starts with a
 * letter, and the result stops at the length the server accepts. A field that
 * let somebody type what the server refuses would offer them a value that
 * cannot be created, and the refusal would arrive a screen later.
 */
export function codeAsTyped(value: string): string {
    const kept = [...value.toLowerCase()].filter(
        (character) =>
            (character >= 'a' && character <= 'z') ||
            (character >= '0' && character <= '9') ||
            character === '_',
    );
    while (kept.length > 0 && kept[0] !== undefined && kept[0] < 'a') {
        kept.shift();
    }
    return kept.join('').slice(0, TENANT_CODE_MAX_LENGTH);
}

/** The hostname a code proposes. A deployment serves the tenant on its code. */
export function hostnameFromCode(code: string): string {
    return code;
}

/**
 * The hostname a legal entity's name proposes.
 *
 * A tenant built around an entity is served somewhere, and the name it is
 * known by is the best guess available: lowercased, with everything that is
 * not a letter or a digit dropped, and the public suffix of the world the
 * entity came from appended. It is a placeholder that a person replaces with
 * the hostname their deployment actually serves.
 */
export function hostnameFromName(name: string): string {
    const host = name
        .trim()
        .toLowerCase()
        .replace(/[^a-z0-9]+/g, '');
    return host === '' ? '' : `${host}.com`;
}

/** The administrator's address, from the name it signs in with and the tenant. */
export function emailFromPrincipal(principal: string, hostname: string): string {
    if (principal === '' || hostname === '') {
        return '';
    }
    return `${principal}@${hostname}`;
}

/**
 * The corporate suffixes a registered name may end with.
 *
 * Mirrored from `ores_utility_strip_corporate_suffix_fn`, which is the rule the
 * deployment applies when it generates a short code for an imported legal
 * entity. A name is stripped of these before a code is proposed from it, so
 * "BARCLAYS PLC" proposes the code of "BARCLAYS" rather than of the two words.
 */
const DOTTED_CORPORATE_SUFFIXES = [
    'S.C.A.',
    'S.R.L.',
    'S.P.A.',
    'S.A.',
    'B.V.',
    'N.V.',
    'K.K.',
    'S.E.',
] as const;

const WORD_CORPORATE_SUFFIXES = [
    'AND CO.',
    '& CO.',
    'CO.',
    'PLC',
    'LLC',
    'LLP',
    'LTD',
    'INC',
    'CORP',
    'AG',
    'NV',
    'AB',
    'ASA',
    'GMBH',
    'MBH',
    'BV',
    'BHD',
    'SRL',
    'SPA',
    'SARL',
    'KG',
    'OYJ',
    'TBK',
    'LP',
    'SE',
    'SA',
    'AS',
] as const;

function escapeRegExp(text: string): string {
    return text.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
}

function stripCorporateSuffix(name: string): string {
    let result = name.trim();
    for (const suffix of DOTTED_CORPORATE_SUFFIXES) {
        if (result.toUpperCase().endsWith(` ${suffix.toUpperCase()}`)) {
            result = result.slice(0, result.length - suffix.length).trim();
        }
    }
    for (const suffix of WORD_CORPORATE_SUFFIXES) {
        result = result.replace(new RegExp(`\\s+${escapeRegExp(suffix)}\\s*$`, 'i'), '');
    }
    return result.trim();
}

/**
 * The short code a registered name proposes.
 *
 * A party's short code is a mnemonic that has to be unique within the tenant,
 * so this proposes one and the person changes it: a single word contributes its
 * first letter and then its consonants, two words contribute three letters
 * each, and three or more contribute two each. The rule is the deployment's own
 * — the codes its imported parties carry were made by it — so a party added
 * here is named the same way as one the deployment already holds.
 */
export function partyCodeFromName(name: string): string {
    const cleaned = stripCorporateSuffix(name)
        .toUpperCase()
        .replace(/[^A-Z ]/g, '')
        .replace(/\s+/g, ' ')
        .trim();
    if (cleaned === '') {
        return 'UNKNWN';
    }
    const words = cleaned.split(' ');
    let code: string;
    if (words.length === 1) {
        const word = words[0] ?? '';
        const consonants = word.replace(/[AEIOU]/g, '');
        code = word.slice(0, 1) + consonants.slice(1);
    } else if (words.length === 2) {
        code = (words[0] ?? '').slice(0, 3) + (words[1] ?? '').slice(0, 3);
    } else {
        code = words
            .slice(0, 3)
            .map((word) => word.slice(0, 2))
            .join('');
    }
    return code.slice(0, 6).padEnd(3, 'X');
}
