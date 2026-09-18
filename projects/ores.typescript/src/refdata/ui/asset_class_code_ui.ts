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
 * judgement the model does not carry. See assetClassCode_field_groups.ts
 * beside this file.
 */
import type { ColumnMeta, FieldMeta } from '../../ui-contract.js';

/**
 * The fields of a asset_class_code, in the order the model declares them.
 *
 * `code` is the natural key, so it is editable when creating
 * and read-only afterwards.
 */
export const assetClassCodeFields: readonly FieldMeta[] = [
    {
        name: 'code',
        labelKey: 'asset_class_code.fldCode',
        control: 'line_edit',
        required: true,
        isKey: true,
        readOnlyAfterCreate: true,
        nullable: false,
        placeholderKey: 'asset_class_code.codePh',
    },
    {
        name: 'name',
        labelKey: 'asset_class_code.fldName',
        control: 'line_edit',
        required: true,
        isKey: false,
        nullable: false,
        placeholderKey: 'asset_class_code.namePh',
    },
    {
        name: 'description',
        labelKey: 'asset_class_code.fldDescription',
        control: 'text_edit',
        required: true,
        isKey: false,
        nullable: false,
        placeholderKey: 'asset_class_code.descriptionPh',
    },
    {
        name: 'display_order',
        labelKey: 'asset_class_code.fldDisplayOrder',
        control: 'spin_box',
        required: true,
        isKey: false,
        nullable: false,
        min: 0,
        max: 9999,
    }
];

/**
 * The table columns, in display order. A column hidden by default is
 * offered through the column menu.
 */
export const assetClassCodeColumns: readonly ColumnMeta[] = [
    {
        name: 'code',
        headerKey: 'asset_class_code.colCode',
        style: 'text_left',
        hidden: false,
    },
    {
        name: 'name',
        headerKey: 'asset_class_code.colName',
        style: 'text_left',
        hidden: false,
    },
    {
        name: 'description',
        headerKey: 'asset_class_code.colDescription',
        style: 'text_left',
        hidden: true,
    },
    {
        name: 'display_order',
        headerKey: 'asset_class_code.colDisplayOrder',
        style: 'mono_center',
        hidden: false,
        width: 70,
    },
    {
        name: 'version',
        headerKey: 'asset_class_code.colVersion',
        style: 'mono_center',
        hidden: true,
        width: 70,
    },
    {
        name: 'modified_by',
        headerKey: 'asset_class_code.colModifiedBy',
        style: 'text_left',
        hidden: true,
    },
    {
        name: 'recorded_at',
        headerKey: 'asset_class_code.colRecordedAt',
        style: 'mono_left',
        hidden: true,
        temporal: true,
    }
];

/**
 * The collection name the list request uses, and the display field.
 *
 * Emitted so the sidebar and the lookup selects do not have to know it
 * separately.
 */
export const assetClassCodeMeta = {
    entity: 'asset_class_code',
    collection: 'classes',
    displayField: 'name',
    keyField: 'code',
    columns: assetClassCodeColumns,
    fields: assetClassCodeFields,
} as const;
