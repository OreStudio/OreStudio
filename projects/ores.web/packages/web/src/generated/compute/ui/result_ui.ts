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
 */
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: ts_ui.ts.mustache
 * To modify, update the template and regenerate.
 */
/**
 * Labels and headers are translation keys, not English. The English words
 * are in the locale catalogue under the same keys.
 *
 * The key shapes are derived, so regeneration cannot invent a key:
 * <entity>.fld<Member> for a field label, <entity>.col<Member> for a
 * column header, <entity>.<member>Ph for a placeholder, and
 * <entity>.type.<value> for a combo option.
 *
 * Field grouping into tabs is deliberately absent: it is a domain
 * judgement the model does not carry, so regeneration cannot invent it. The
 * model cannot express a grouping yet, so every field renders in one group.
 */
import type { ColumnMeta, FieldMeta } from '../../../ui-contract.js';

/**
 * The fields of a result, in the order the model declares them.
 */
export const resultFields: readonly FieldMeta[] = [];

/**
 * The table columns, in display order. A column hidden by default is
 * offered through the column menu.
 */
export const resultColumns: readonly ColumnMeta[] = [
    {
        name: 'workunit_id',
        headerKey: 'result.colWorkunitId',
        style: 'mono_left',
        hidden: false,
        width: 160,
    },
    {
        name: 'host_id',
        headerKey: 'result.colHostId',
        style: 'mono_left',
        hidden: false,
        width: 160,
    },
    {
        name: 'server_state',
        headerKey: 'result.colServerState',
        style: 'mono_center',
        hidden: false,
        width: 70,
    },
    {
        name: 'outcome',
        headerKey: 'result.colOutcome',
        style: 'mono_center',
        hidden: false,
        width: 70,
    },
    {
        name: 'error_message',
        headerKey: 'result.colErrorMessage',
        style: 'text_left',
        hidden: false,
        width: 160,
    },
    {
        name: 'output_uri',
        headerKey: 'result.colOutputUri',
        style: 'text_left',
        hidden: false,
        width: 200,
    },
    {
        name: 'received_at',
        headerKey: 'result.colReceivedAt',
        style: 'mono_left',
        hidden: false,
        width: 140,
        temporal: true,
    },
    {
        name: 'version',
        headerKey: 'result.colVersion',
        style: 'mono_center',
        hidden: true,
        width: 60,
    },
    {
        name: 'modified_by',
        headerKey: 'result.colModifiedBy',
        style: 'text_left',
        hidden: true,
        width: 120,
    }
];

/**
 * The collection name the list request uses, and the display field.
 *
 * Emitted so the sidebar and the lookup selects do not have to know it
 * separately.
 */
export const resultMeta = {
    entity: 'result',
    collection: 'results',
    displayField: 'modified_by',
    keyField: 'modified_by',
    columns: resultColumns,
    fields: resultFields,
} as const;
/**
 * The entity's own words, in English, keyed the way the catalogue is.
 *
 * The model states them: the detail field's label, the column's header, the
 * placeholder, the title and the brief. They are emitted here rather than
 * written into a catalogue by hand, so a label the model changes changes in
 * one place, and a language that has no translation yet falls back to these
 * rather than to a key nobody can read.
 */
export const resultMessages = {
        result: {
            title: 'Results',
            singular: 'result',
            newTitle: 'New result',
            description: 'Bridges the workunit definition and the actual execution on a grid node. Tracks PGMQ lease state, server-side lifecycle (Inactive/Unsent/InProgress/Done), and the location of output data. The BOINC equivalent of \'result\'. Change-reason exception (recorded in the codegen drift loop): result is a machine-written, list-only entity — the grid machinery writes results and there is no human edit flow — so has_change_reason_cache is explicitly false, overriding the profile default. Generator-signature exception (recorded in the codegen drift loop): the pre-drift handcrafted generator took a workunit_id parameter (generate_synthetic_result(workunit_id, ctx)). The template signature takes only the generation context, and the sole consumer (the result eventing integration test) now links the FK by member assignment after generation. No model knob or paste block is needed for the parameterized overload; the template shape is the sanctioned surface.',
            colWorkunitId: 'Workunit ID',
            colHostId: 'Host ID',
            colServerState: 'State',
            colOutcome: 'Outcome',
            colErrorMessage: 'Error',
            colOutputUri: 'Output URI',
            colReceivedAt: 'Received At',
            colVersion: 'Version',
            colModifiedBy: 'Modified By',
        }
};
