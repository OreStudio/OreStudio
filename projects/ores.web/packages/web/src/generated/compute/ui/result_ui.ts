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
 * judgement the model does not carry. See result_field_groups.ts
 * beside this file.
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
