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
 * The fields of a host, in the order the model declares them.
 *
 * `external_id` is the natural key, so it is editable when creating
 * and read-only afterwards.
 */
export const hostFields: readonly FieldMeta[] = [
    {
        name: 'external_id',
        labelKey: 'host.fldExternalId',
        control: 'line_edit',
        required: true,
        isKey: true,
        readOnlyAfterCreate: true,
        nullable: false,
        placeholderKey: 'host.externalIdPh',
    }
];

/**
 * The table columns, in display order. A column hidden by default is
 * offered through the column menu.
 */
export const hostColumns: readonly ColumnMeta[] = [
    {
        name: 'display_name',
        headerKey: 'host.colDisplayName',
        style: 'text_left',
        hidden: false,
        width: 160,
    },
    {
        name: 'external_id',
        headerKey: 'host.colExternalId',
        style: 'text_left',
        hidden: false,
        width: 180,
    },
    {
        name: 'location',
        headerKey: 'host.colLocation',
        style: 'text_left',
        hidden: false,
        width: 120,
    },
    {
        name: 'cpu_count',
        headerKey: 'host.colCpuCount',
        style: 'mono_center',
        hidden: false,
        width: 60,
    },
    {
        name: 'ram_mb',
        headerKey: 'host.colRamMb',
        style: 'mono_center',
        hidden: false,
        width: 80,
    },
    {
        name: 'gpu_type',
        headerKey: 'host.colGpuType',
        style: 'text_left',
        hidden: false,
        width: 100,
    },
    {
        name: 'last_rpc_time',
        headerKey: 'host.colLastRpcTime',
        style: 'mono_left',
        hidden: false,
        width: 140,
        temporal: true,
    },
    {
        name: 'credit_total',
        headerKey: 'host.colCreditTotal',
        style: 'mono_center',
        hidden: false,
        width: 80,
    },
    {
        name: 'version',
        headerKey: 'host.colVersion',
        style: 'mono_center',
        hidden: true,
        width: 60,
    },
    {
        name: 'modified_by',
        headerKey: 'host.colModifiedBy',
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
export const hostMeta = {
    entity: 'host',
    collection: 'hosts',
    displayField: 'external_id',
    keyField: 'external_id',
    columns: hostColumns,
    fields: hostFields,
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
export const hostMessages = {
        host: {
            title: 'Compute Hosts',
            singular: 'host',
            newTitle: 'New host',
            description: 'Represents a physical or virtual machine that participates in the BOINC-inspired compute grid. Tracks hardware capabilities, heartbeat, and accumulated credit.',
            fldExternalId: 'External Id',
            externalIdPh: 'Enter compute host external id',
            colDisplayName: 'Name',
            colExternalId: 'Host ID',
            colLocation: 'Location',
            colCpuCount: 'CPUs',
            colRamMb: 'RAM (MB)',
            colGpuType: 'GPU',
            colLastRpcTime: 'Last Heartbeat',
            colCreditTotal: 'Credits',
            colVersion: 'Version',
            colModifiedBy: 'Modified By',
        }
};
