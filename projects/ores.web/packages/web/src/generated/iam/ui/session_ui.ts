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
 * The fields of a session, in the order the model declares them.
 */
export const sessionFields: readonly FieldMeta[] = [
    {
        name: 'account_id',
        labelKey: 'session.fldAccountId',
        control: 'line_edit',
        required: false,
        isKey: false,
        nullable: false,
    },
    {
        name: 'start_time',
        labelKey: 'session.fldStartTime',
        control: 'date',
        required: false,
        isKey: false,
        nullable: false,
    },
    {
        name: 'end_time',
        labelKey: 'session.fldEndTime',
        control: 'date',
        required: false,
        isKey: false,
        nullable: false,
    },
    {
        name: 'client_ip',
        labelKey: 'session.fldClientIp',
        control: 'line_edit',
        required: false,
        isKey: false,
        nullable: false,
    },
    {
        name: 'client_identifier',
        labelKey: 'session.fldClientIdentifier',
        control: 'line_edit',
        required: false,
        isKey: false,
        nullable: false,
    },
    {
        name: 'country_code',
        labelKey: 'session.fldCountryCode',
        control: 'line_edit',
        required: false,
        isKey: false,
        nullable: false,
    },
    {
        name: 'protocol',
        labelKey: 'session.fldProtocol',
        control: 'line_edit',
        required: false,
        isKey: false,
        nullable: false,
    }
];

/**
 * The table columns, in display order. A column hidden by default is
 * offered through the column menu.
 */
export const sessionColumns: readonly ColumnMeta[] = [
    {
        name: 'account_id',
        headerKey: 'session.colAccountId',
        style: 'text_left',
        hidden: false,
        width: 300,
    },
    {
        name: 'start_time',
        headerKey: 'session.colStartTime',
        style: 'mono_left',
        hidden: false,
        width: 160,
        temporal: true,
    },
    {
        name: 'end_time',
        headerKey: 'session.colEndTime',
        style: 'mono_left',
        hidden: false,
        width: 160,
        temporal: true,
    },
    {
        name: 'client_ip',
        headerKey: 'session.colClientIp',
        style: 'text_left',
        hidden: false,
        width: 150,
    },
    {
        name: 'country_code',
        headerKey: 'session.colCountryCode',
        style: 'text_left',
        hidden: false,
        width: 90,
    },
    {
        name: 'protocol',
        headerKey: 'session.colProtocol',
        style: 'text_left',
        hidden: false,
        width: 110,
    },
    {
        name: 'bytes_sent',
        headerKey: 'session.colBytesSent',
        style: 'mono_center',
        hidden: false,
        width: 110,
    },
    {
        name: 'bytes_received',
        headerKey: 'session.colBytesReceived',
        style: 'mono_center',
        hidden: false,
        width: 110,
    }
];

/**
 * The collection name the list request uses, and the display field.
 *
 * Emitted so the sidebar and the lookup selects do not have to know it
 * separately.
 */
export const sessionMeta = {
    entity: 'session',
    collection: 'sessions',
    displayField: '',
    keyField: 'id',
    columns: sessionColumns,
    fields: sessionFields,
} as const;
