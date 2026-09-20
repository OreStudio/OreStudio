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
 * The fields of a country, in the order the model declares them.
 *
 * `alpha2_code` is the natural key, so it is editable when creating
 * and read-only afterwards.
 */
export const countryFields: readonly FieldMeta[] = [
    {
        name: 'alpha2_code',
        labelKey: 'country.fldAlpha2Code',
        control: 'line_edit',
        required: true,
        isKey: true,
        readOnlyAfterCreate: true,
        nullable: false,
        placeholderKey: 'country.alpha2CodePh',
    },
    {
        name: 'alpha3_code',
        labelKey: 'country.fldAlpha3Code',
        control: 'line_edit',
        required: true,
        isKey: false,
        nullable: false,
        placeholderKey: 'country.alpha3CodePh',
    },
    {
        name: 'numeric_code',
        labelKey: 'country.fldNumericCode',
        control: 'line_edit',
        required: true,
        isKey: false,
        nullable: false,
        placeholderKey: 'country.numericCodePh',
    },
    {
        name: 'name',
        labelKey: 'country.fldName',
        control: 'line_edit',
        required: true,
        isKey: false,
        nullable: false,
        placeholderKey: 'country.namePh',
    },
    {
        name: 'official_name',
        labelKey: 'country.fldOfficialName',
        control: 'line_edit',
        required: true,
        isKey: false,
        nullable: false,
        placeholderKey: 'country.officialNamePh',
    }
];

/**
 * The table columns, in display order. A column hidden by default is
 * offered through the column menu.
 */
export const countryColumns: readonly ColumnMeta[] = [
    {
        name: 'alpha2_code',
        headerKey: 'country.colAlpha2Code',
        style: 'icon_text_left',
        hidden: false,
        width: 80,
        flag: true,
    },
    {
        name: 'alpha3_code',
        headerKey: 'country.colAlpha3Code',
        style: 'text_left',
        hidden: false,
        width: 80,
    },
    {
        name: 'numeric_code',
        headerKey: 'country.colNumericCode',
        style: 'text_left',
        hidden: false,
        width: 80,
    },
    {
        name: 'name',
        headerKey: 'country.colName',
        style: 'text_left',
        hidden: false,
        width: 200,
    },
    {
        name: 'official_name',
        headerKey: 'country.colOfficialName',
        style: 'text_left',
        hidden: false,
        width: 200,
    },
    {
        name: 'version',
        headerKey: 'country.colVersion',
        style: 'mono_center',
        hidden: true,
        width: 80,
    },
    {
        name: 'modified_by',
        headerKey: 'country.colModifiedBy',
        style: 'text_left',
        hidden: true,
        width: 120,
    },
    {
        name: 'recorded_at',
        headerKey: 'country.colRecordedAt',
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
export const countryMeta = {
    entity: 'country',
    collection: 'countries',
    displayField: 'name',
    keyField: 'alpha2_code',
    columns: countryColumns,
    fields: countryFields,
} as const;
