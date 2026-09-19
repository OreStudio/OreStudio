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
 * judgement the model does not carry. See businessUnitType_field_groups.ts
 * beside this file.
 */
import type { ColumnMeta, FieldMeta } from '../../../ui-contract.js';

/**
 * The fields of a business_unit_type, in the order the model declares them.
 *
 * `code` is the natural key, so it is editable when creating
 * and read-only afterwards.
 */
export const businessUnitTypeFields: readonly FieldMeta[] = [
    {
        name: 'code',
        labelKey: 'business_unit_type.fldCode',
        control: 'line_edit',
        required: true,
        isKey: true,
        readOnlyAfterCreate: true,
        nullable: false,
        placeholderKey: 'business_unit_type.codePh',
    },
    {
        name: 'name',
        labelKey: 'business_unit_type.fldName',
        control: 'line_edit',
        required: true,
        isKey: false,
        nullable: false,
        placeholderKey: 'business_unit_type.namePh',
    },
    {
        name: 'coding_scheme_code',
        labelKey: 'business_unit_type.fldCodingSchemeCode',
        control: 'line_edit',
        required: true,
        isKey: false,
        nullable: false,
        placeholderKey: 'business_unit_type.codingSchemeCodePh',
    },
    {
        name: 'level',
        labelKey: 'business_unit_type.fldLevel',
        control: 'spin_box',
        required: false,
        isKey: false,
        nullable: false,
        min: 0,
        max: 9999,
    },
    {
        name: 'description',
        labelKey: 'business_unit_type.fldDescription',
        control: 'text_edit',
        required: false,
        isKey: false,
        nullable: false,
        placeholderKey: 'business_unit_type.descriptionPh',
    }
];

/**
 * The table columns, in display order. A column hidden by default is
 * offered through the column menu.
 */
export const businessUnitTypeColumns: readonly ColumnMeta[] = [
    {
        name: 'code',
        headerKey: 'business_unit_type.colCode',
        style: 'badge_centered',
        hidden: false,
        width: 120,
        codeDomain: 'business_unit_type',
    },
    {
        name: 'name',
        headerKey: 'business_unit_type.colName',
        style: 'text_left',
        hidden: false,
        width: 200,
    },
    {
        name: 'coding_scheme_code',
        headerKey: 'business_unit_type.colCodingSchemeCode',
        style: 'text_left',
        hidden: false,
        width: 150,
    },
    {
        name: 'level',
        headerKey: 'business_unit_type.colLevel',
        style: 'mono_center',
        hidden: false,
        width: 80,
    },
    {
        name: 'description',
        headerKey: 'business_unit_type.colDescription',
        style: 'text_left',
        hidden: true,
        width: 250,
    },
    {
        name: 'version',
        headerKey: 'business_unit_type.colVersion',
        style: 'mono_center',
        hidden: true,
        width: 80,
    },
    {
        name: 'modified_by',
        headerKey: 'business_unit_type.colModifiedBy',
        style: 'text_left',
        hidden: true,
        width: 120,
    },
    {
        name: 'recorded_at',
        headerKey: 'business_unit_type.colRecordedAt',
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
export const businessUnitTypeMeta = {
    entity: 'business_unit_type',
    collection: 'business_unit_types',
    displayField: 'name',
    keyField: 'code',
    columns: businessUnitTypeColumns,
    fields: businessUnitTypeFields,
} as const;
