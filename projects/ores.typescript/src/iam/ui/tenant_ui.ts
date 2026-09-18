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
 * judgement the model does not carry. See tenant_field_groups.ts
 * beside this file.
 */
import type { ColumnMeta, FieldMeta } from '../../ui-contract.js';

/**
 * The fields of a tenant, in the order the model declares them.
 *
 * `code` is the natural key, so it is editable when creating
 * and read-only afterwards.
 */
export const tenantFields: readonly FieldMeta[] = [
    {
        name: 'code',
        labelKey: 'tenant.fldCode',
        control: 'line_edit',
        required: true,
        isKey: true,
        readOnlyAfterCreate: true,
        nullable: false,
        placeholderKey: 'tenant.codePh',
    },
    {
        name: 'name',
        labelKey: 'tenant.fldName',
        control: 'line_edit',
        required: true,
        isKey: false,
        nullable: false,
        placeholderKey: 'tenant.namePh',
    },
    {
        name: 'type',
        labelKey: 'tenant.fldType',
        control: 'line_edit',
        required: false,
        isKey: false,
        nullable: false,
        placeholderKey: 'tenant.typePh',
    },
    {
        name: 'hostname',
        labelKey: 'tenant.fldHostname',
        control: 'line_edit',
        required: false,
        isKey: false,
        nullable: false,
        placeholderKey: 'tenant.hostnamePh',
    },
    {
        name: 'status',
        labelKey: 'tenant.fldStatus',
        control: 'line_edit',
        required: false,
        isKey: false,
        nullable: false,
        placeholderKey: 'tenant.statusPh',
    }
];

/**
 * The table columns, in display order. A column hidden by default is
 * offered through the column menu.
 */
export const tenantColumns: readonly ColumnMeta[] = [
    {
        name: 'code',
        headerKey: 'tenant.colCode',
        style: 'text_left',
        hidden: false,
        width: 120,
    },
    {
        name: 'name',
        headerKey: 'tenant.colName',
        style: 'text_left',
        hidden: false,
        width: 200,
    },
    {
        name: 'type',
        headerKey: 'tenant.colType',
        style: 'text_left',
        hidden: false,
        width: 100,
    },
    {
        name: 'hostname',
        headerKey: 'tenant.colHostname',
        style: 'text_left',
        hidden: false,
        width: 180,
    },
    {
        name: 'status',
        headerKey: 'tenant.colStatus',
        style: 'text_left',
        hidden: false,
        width: 100,
    },
    {
        name: 'version',
        headerKey: 'tenant.colVersion',
        style: 'mono_center',
        hidden: true,
        width: 80,
    },
    {
        name: 'modified_by',
        headerKey: 'tenant.colModifiedBy',
        style: 'text_left',
        hidden: true,
        width: 120,
    },
    {
        name: 'recorded_at',
        headerKey: 'tenant.colRecordedAt',
        style: 'mono_left',
        hidden: true,
        width: 150,
        temporal: true,
    }
];

/**
 * The collection name the list request uses, and the display field.
 *
 * Emitted so the sidebar and the lookup selects do not have to know it
 * separately.
 */
export const tenantMeta = {
    entity: 'tenant',
    collection: 'tenants',
    displayField: 'name',
    keyField: 'code',
    columns: tenantColumns,
    fields: tenantFields,
} as const;
