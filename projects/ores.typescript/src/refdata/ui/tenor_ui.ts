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
 * judgement the model does not carry. See tenor_field_groups.ts
 * beside this file.
 */
import type { ColumnMeta, FieldMeta } from '../../ui-contract.js';

/**
 * The fields of a tenor, in the order the model declares them.
 *
 * `code` is the natural key, so it is editable when creating
 * and read-only afterwards.
 */
export const tenorFields: readonly FieldMeta[] = [
    {
        name: 'code',
        labelKey: 'tenor.fldCode',
        control: 'line_edit',
        required: true,
        isKey: true,
        readOnlyAfterCreate: true,
        nullable: false,
        placeholderKey: 'tenor.codePh',
    },
    {
        name: 'display_name',
        labelKey: 'tenor.fldDisplayName',
        control: 'line_edit',
        required: true,
        isKey: false,
        nullable: false,
        placeholderKey: 'tenor.displayNamePh',
    },
    {
        name: 'description',
        labelKey: 'tenor.fldDescription',
        control: 'text_edit',
        required: false,
        isKey: false,
        nullable: true,
        placeholderKey: 'tenor.descriptionPh',
    },
    {
        name: 'sort_order',
        labelKey: 'tenor.fldSortOrder',
        control: 'spin_box',
        required: true,
        isKey: false,
        nullable: false,
        min: 0,
        max: 9999,
    },
    {
        name: 'kind',
        labelKey: 'tenor.fldKind',
        control: 'dynamic_combo',
        required: true,
        isKey: false,
        nullable: false,
        lookup: { collection: 'kinds', valueField: 'code', labelField: 'description' },
        codeDomain: 'tenor_kind',
    },
    {
        name: 'unit',
        labelKey: 'tenor.fldUnit',
        control: 'dynamic_combo',
        required: true,
        isKey: false,
        nullable: false,
        lookup: { collection: 'units', valueField: 'code', labelField: 'description' },
        codeDomain: 'tenor_unit',
    },
    {
        name: 'multiplier',
        labelKey: 'tenor.fldMultiplier',
        control: 'spin_box',
        required: false,
        isKey: false,
        nullable: true,
        min: -1,
        max: 9999,
    }
];

/**
 * The table columns, in display order. A column hidden by default is
 * offered through the column menu.
 */
export const tenorColumns: readonly ColumnMeta[] = [
    {
        name: 'code',
        headerKey: 'tenor.colCode',
        style: 'text_left',
        hidden: false,
        width: 100,
    },
    {
        name: 'display_name',
        headerKey: 'tenor.colDisplayName',
        style: 'text_left',
        hidden: false,
        width: 160,
    },
    {
        name: 'description',
        headerKey: 'tenor.colDescription',
        style: 'text_left',
        hidden: true,
        width: 300,
    },
    {
        name: 'sort_order',
        headerKey: 'tenor.colSortOrder',
        style: 'mono_center',
        hidden: false,
        width: 90,
    },
    {
        name: 'kind',
        headerKey: 'tenor.colKind',
        style: 'badge_centered',
        hidden: false,
        width: 90,
        codeDomain: 'tenor_kind',
    },
    {
        name: 'unit',
        headerKey: 'tenor.colUnit',
        style: 'badge_centered',
        hidden: false,
        width: 90,
        codeDomain: 'tenor_unit',
    },
    {
        name: 'version',
        headerKey: 'tenor.colVersion',
        style: 'mono_center',
        hidden: true,
        width: 80,
    },
    {
        name: 'modified_by',
        headerKey: 'tenor.colModifiedBy',
        style: 'text_left',
        hidden: true,
        width: 120,
    },
    {
        name: 'recorded_at',
        headerKey: 'tenor.colRecordedAt',
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
export const tenorMeta = {
    entity: 'tenor',
    collection: 'tenors',
    displayField: 'code',
    keyField: 'code',
    columns: tenorColumns,
    fields: tenorFields,
} as const;
