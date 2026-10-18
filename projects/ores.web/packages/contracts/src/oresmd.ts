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

/**
 * The oresmd URI as the browser reads and writes it. The server's codec holds
 * the grammar. These shapes carry a URI and its parts to and from it, and the
 * limits that keep a request small. They are the same on both sides, so a shape
 * change fails loudly on whichever side is stale.
 */

/** The most characters a URI, a subject or a field's text may hold. The server's own limit. */
export const ORESMD_MAX_TEXT = 2048;
/** The most fields a compose request may state. The server's own limit. */
export const ORESMD_MAX_FIELDS = 64;

export const oresmdFieldSchema = z.object({ name: z.string(), text: z.string() });
export type OresmdField = z.infer<typeof oresmdFieldSchema>;

export const oresmdFieldSpecSchema = z.object({
    name: z.string(),
    role: z.string(),
    may_be_none: z.boolean(),
    kind: z.string(),
    codes: z.array(z.string()),
});

export const oresmdInstrumentSchema = z.object({
    code: z.string(),
    ore_name: z.string(),
    /** The words that name a series of the type, with {subject} and {field_name} to fill. */
    title: z.string(),
    asset: z.string(),
    subject: z.string(),
    fields: z.array(oresmdFieldSpecSchema),
});

export const oresmdIndexFamilySchema = z.object({
    code: z.string(),
    asset: z.string(),
    subject: z.string(),
    fields: z.array(z.object({ name: z.string(), optional: z.boolean() })),
});

/** What a URI may hold: every instrument type with its fields, the quotes and the index families. */
export const oresmdSchemaSchema = z.object({
    assets: z.array(z.string()),
    instruments: z.array(oresmdInstrumentSchema),
    quotes: z.array(z.string()),
    /** The words for each quote type, in the order of `quotes`. Empty for none. */
    quote_titles: z.array(z.string()),
    indexes: z.array(oresmdIndexFamilySchema),
});
export type OresmdSchema = z.infer<typeof oresmdSchemaSchema>;

export const oresmdParseRequestSchema = z.strictObject({
    uri: z.string().max(ORESMD_MAX_TEXT),
});

/**
 * The parts of a URI the codec accepts, or the codec's words for refusing it. A
 * refusal is an answer, so the status is still 200 and `success` is false.
 */
export const oresmdParseResultSchema = z.object({
    success: z.boolean(),
    message: z.string(),
    kind: z.string(),
    instrument: z.string(),
    quote: z.string(),
    asset: z.string(),
    subject: z.string(),
    fields: z.array(oresmdFieldSchema),
});
export type OresmdParseResult = z.infer<typeof oresmdParseResultSchema>;

/** The most URIs one batch read may carry: the widest page a paged list draws. */
export const ORESMD_MAX_BATCH = 200;

/**
 * Asks the codec to read many URIs in one call. A list of rows names one URI each,
 * and asks about them together rather than once a row.
 */
export const oresmdParseManyRequestSchema = z.strictObject({
    uris: z.array(z.string().max(ORESMD_MAX_TEXT)).max(ORESMD_MAX_BATCH),
});
export type OresmdParseManyRequest = z.infer<typeof oresmdParseManyRequestSchema>;

/** The parts of each URI, in the order asked. A refused URI takes its own answer. */
export const oresmdParseManyResultSchema = z.object({
    parts: z.array(oresmdParseResultSchema),
});
export type OresmdParseManyResult = z.infer<typeof oresmdParseManyResultSchema>;

/**
 * The parts of a URI to write. The browser states the structure and the codec
 * decides whether the parts make a URI, so the kind, the instrument and the quote
 * are text here and not a list the browser holds.
 */
export const oresmdComposeRequestSchema = z.strictObject({
    kind: z.string().max(ORESMD_MAX_TEXT),
    instrument: z.string().max(ORESMD_MAX_TEXT),
    quote: z.string().max(ORESMD_MAX_TEXT).default(''),
    subject: z.string().max(ORESMD_MAX_TEXT).default(''),
    fields: z
        .array(
            z.strictObject({
                name: z.string().max(ORESMD_MAX_TEXT),
                text: z.string().max(ORESMD_MAX_TEXT),
            }),
        )
        .max(ORESMD_MAX_FIELDS)
        .default([]),
});
export type OresmdComposeRequest = z.input<typeof oresmdComposeRequestSchema>;

/** The URI in the codec's own spelling, or why the parts make none. */
export const oresmdComposeResultSchema = z.object({
    success: z.boolean(),
    message: z.string(),
    uri: z.string(),
});
export type OresmdComposeResult = z.infer<typeof oresmdComposeResultSchema>;
