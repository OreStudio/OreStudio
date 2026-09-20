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
 * judgement the model does not carry, so regeneration cannot invent it.
 * An entity that needs tabs states them as its descriptor's `fieldGroups`.
 */
import type { ColumnMeta, FieldMeta } from '../../../ui-contract.js';

/**
 * The fields of a app_version, in the order the model declares them.
 *
 * `wrapper_version` is the natural key, so it is editable when creating
 * and read-only afterwards.
 */
export const appVersionFields: readonly FieldMeta[] = [
    {
        name: 'wrapper_version',
        labelKey: 'app_version.fldWrapperVersion',
        control: 'line_edit',
        required: true,
        isKey: true,
        readOnlyAfterCreate: true,
        nullable: false,
        placeholderKey: 'app_version.wrapperVersionPh',
    }
];

/**
 * The table columns, in display order. A column hidden by default is
 * offered through the column menu.
 */
export const appVersionColumns: readonly ColumnMeta[] = [
    {
        name: 'app_id',
        headerKey: 'app_version.colAppId',
        style: 'mono_left',
        hidden: false,
        width: 160,
    },
    {
        name: 'wrapper_version',
        headerKey: 'app_version.colWrapperVersion',
        style: 'text_left',
        hidden: false,
        width: 120,
    },
    {
        name: 'engine_version',
        headerKey: 'app_version.colEngineVersion',
        style: 'text_left',
        hidden: false,
        width: 120,
    },
    {
        name: 'min_ram_mb',
        headerKey: 'app_version.colMinRamMb',
        style: 'mono_center',
        hidden: false,
        width: 90,
    },
    {
        name: 'version',
        headerKey: 'app_version.colVersion',
        style: 'mono_center',
        hidden: true,
        width: 60,
    },
    {
        name: 'modified_by',
        headerKey: 'app_version.colModifiedBy',
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
export const appVersionMeta = {
    entity: 'app_version',
    collection: 'app_versions',
    displayField: 'wrapper_version',
    keyField: 'wrapper_version',
    columns: appVersionColumns,
    fields: appVersionFields,
} as const;
