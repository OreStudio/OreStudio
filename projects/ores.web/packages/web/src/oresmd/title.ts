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

import type { OresmdParseResult, OresmdSchema } from '@ores/contracts';
import { seriesOfUrl } from './identity.js';

/** What a title needs of a series: its scope, its type and quote, and its identity fields. */
export interface TitledIdentity {
    /** Only the raw fallback label names it. A title does not. */
    readonly asset?: string;
    readonly scope: string;
    readonly instrument_type: string;
    readonly quote_type: string;
    readonly fields: readonly { readonly name: string; readonly text: string }[];
}

const BRACES = /\{(\w+)\}/g;

/**
 * A series in words, such as "AUD/USD spot rate".
 *
 * The schema holds a title for each instrument type, with the scope written as
 * {subject} and any identity field as {field_name}, and the words for each quote
 * type. This fills the braces from the identity, puts the quote words after
 * them, and lists the identity fields the title does not name behind a dot, so
 * two series that differ only in such a field still read differently. It holds
 * no instrument name of its own. A type the schema does not know, or a series that
 * lacks a field its title names, has no title, because a title with a gap reads as
 * broken. The caller then falls back to the raw identity.
 */
export function seriesTitle(schema: OresmdSchema, identity: TitledIdentity): string | undefined {
    const instrument = schema.instruments.find((row) => row.code === identity.instrument_type);
    if (instrument === undefined) return undefined;
    const named = new Set<string>();
    let gap = false;
    const filled = instrument.title.replace(BRACES, (_, name: string) => {
        named.add(name);
        const text =
            name === 'subject'
                ? identity.scope
                : identity.fields.find((field) => field.name === name)?.text;
        gap = gap || text === undefined || text === '';
        return text ?? '';
    });
    if (gap) return undefined;
    const quote = schema.quote_titles[schema.quotes.indexOf(identity.quote_type)] ?? '';
    const head = `${filled} ${quote}`.replace(/\s+/g, ' ').trim();
    const rest = identity.fields.filter((field) => !named.has(field.name));
    if (rest.length === 0) return head;
    return `${head} · ${rest.map((field) => `${field.name}=${field.text}`).join(', ')}`;
}

/** A series by its identity words, for when no title fits: the words, then the fields that tell it apart. */
export function rawLabel(identity: TitledIdentity): string {
    const words = [identity.asset, identity.scope, identity.instrument_type, identity.quote_type]
        .filter((word) => word !== undefined)
        .join(' / ');
    if (identity.fields.length === 0) return words;
    return `${words} · ${identity.fields.map((field) => `${field.name}=${field.text}`).join(', ')}`;
}

/** The title of a series, or its raw identity until the schema is known or when no title fits. */
export function titleOrRaw(schema: OresmdSchema | undefined, identity: TitledIdentity): string {
    return (schema === undefined ? undefined : seriesTitle(schema, identity)) ?? rawLabel(identity);
}

/** The title of the series a parsed URL names, or nothing for a fixing or a type the schema lacks. */
export function titleOfUrl(schema: OresmdSchema, parts: OresmdParseResult): string | undefined {
    const named = seriesOfUrl(schema, parts);
    if (!named.ok) return undefined;
    const { scope, instrumentType, quoteType, fields } = named.form;
    return seriesTitle(schema, {
        scope,
        instrument_type: instrumentType,
        quote_type: quoteType,
        fields: fields
            .split('\n')
            .filter((line) => line !== '')
            .map((line) => {
                const at = line.indexOf('=');
                return { name: line.slice(0, at), text: line.slice(at + 1) };
            }),
    });
}
