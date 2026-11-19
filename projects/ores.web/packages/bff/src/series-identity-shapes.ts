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

import type { FastifyInstance, FastifyRequest } from 'fastify';
import {
    subjects as shapeSubjects,
    type ListSeriesIdentityShapesResponse,
} from '@ores/wire-protocol/generated/marketdata/protocol/series_identity_shape_protocol';
import {
    subjects as entrySubjects,
    type ListSeriesIdentityShapeEntriesResponse,
} from '@ores/wire-protocol/generated/marketdata/protocol/series_identity_shape_entry_protocol';
import {
    subjects as ruleSubjects,
    type ListSeriesClassificationRulesResponse,
} from '@ores/wire-protocol/generated/marketdata/protocol/series_classification_rule_protocol';
import { readAll, text } from './refdata-calls.js';
import type { LiveSession } from './sessions.js';

/*
 * What the oresmd editor needs to mark a URI's fields: the classification rules
 * that name a URI's family, and each field a family requires or forbids.
 *
 * A shape names a family by code, and its entries name the fields. The editor
 * reads them joined, one row per field with its family's parts, so it never
 * holds a code. The shapes and their entries are read and written as records
 * through the record routes; this module only reads.
 */

export function registerSeriesIdentityShapeRoutes(
    server: FastifyInstance,
    requireSession: (request: FastifyRequest) => LiveSession,
): void {
    /**
     * The rules that give a series its subclass, which the oresmd editor reads
     * to name the family of the URI on screen and so the shape it is held to.
     */
    server.get('/api/marketdata/series-classification-rules', async (request) => {
        const session = requireSession(request);
        const rows = await readAll<ListSeriesClassificationRulesResponse>(
            session,
            ruleSubjects.list_series_classification_rules_request,
            'rules',
            { as_of: null },
        );
        return {
            rules: rows.map((rule) => ({
                instrument: text(rule['instrument']),
                quote: text(rule['quote']),
                key_kind: text(rule['key_kind']),
                series_subclass_code: text(rule['series_subclass_code']),
            })),
        };
    });

    /** Every field a family requires or forbids, each with its family's parts. */
    server.get('/api/marketdata/series-identity-shape-fields', async (request) => {
        const session = requireSession(request);
        const [shapes, entries] = await Promise.all([
            readAll<ListSeriesIdentityShapesResponse>(
                session,
                shapeSubjects.list_series_identity_shapes_request,
                'shapes',
                { as_of: null },
            ),
            readAll<ListSeriesIdentityShapeEntriesResponse>(
                session,
                entrySubjects.list_series_identity_shape_entries_request,
                'entries',
                { as_of: null },
            ),
        ]);
        const families = new Map(shapes.map((shape) => [text(shape['code']), shape]));
        return {
            fields: entries.flatMap((entry) => {
                const shape = families.get(text(entry['shape_code']));
                return shape === undefined
                    ? []
                    : [
                          {
                              asset_class: text(shape['asset_class']),
                              series_subclass: text(shape['series_subclass']),
                              index_family: text(shape['index_family']),
                              field: text(entry['field']),
                              requirement: text(entry['requirement']),
                              description: text(entry['description']),
                          },
                      ];
            }),
        };
    });
}
