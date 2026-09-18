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
 * judgement the model does not carry. See partyIdScheme_field_groups.ts
 * beside this file.
 */
import type { ColumnMeta, FieldMeta } from '../../ui-contract.js';

/**
 * The fields of a party_id_scheme, in the order the model declares them.
 *
 * `code` is the natural key, so it is editable when creating
 * and read-only afterwards.
 */
export const partyIdSchemeFields: readonly FieldMeta[] = [
    {
        name: 'code',
        labelKey: 'party_id_scheme.fldCode',
        control: 'line_edit',
        required: true,
        isKey: true,
        readOnlyAfterCreate: true,
        nullable: false,
        placeholderKey: 'party_id_scheme.codePh',
    },
    {
        name: 'name',
        labelKey: 'party_id_scheme.fldName',
        control: 'line_edit',
        required: true,
        isKey: false,
        nullable: false,
        placeholderKey: 'party_id_scheme.namePh',
    },
    {
        name: 'description',
        labelKey: 'party_id_scheme.fldDescription',
        control: 'text_edit',
        required: true,
        isKey: false,
        nullable: false,
        placeholderKey: 'party_id_scheme.descriptionPh',
    },
    {
        name: 'coding_scheme_code',
        labelKey: 'party_id_scheme.fldCodingSchemeCode',
        control: 'line_edit',
        required: false,
        isKey: false,
        nullable: true,
        placeholderKey: 'party_id_scheme.codingSchemeCodePh',
    },
    {
        name: 'display_order',
        labelKey: 'party_id_scheme.fldDisplayOrder',
        control: 'spin_box',
        required: false,
        isKey: false,
        nullable: false,
        min: 0,
        max: 9999,
    },
    {
        name: 'max_cardinality',
        labelKey: 'party_id_scheme.fldMaxCardinality',
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
export const partyIdSchemeColumns: readonly ColumnMeta[] = [
    {
        name: 'code',
        headerKey: 'party_id_scheme.colCode',
        style: 'text_left',
        hidden: false,
        width: 150,
    },
    {
        name: 'name',
        headerKey: 'party_id_scheme.colName',
        style: 'text_left',
        hidden: false,
        width: 200,
    },
    {
        name: 'description',
        headerKey: 'party_id_scheme.colDescription',
        style: 'text_left',
        hidden: true,
        width: 300,
    },
    {
        name: 'coding_scheme_code',
        headerKey: 'party_id_scheme.colCodingSchemeCode',
        style: 'text_left',
        hidden: false,
        width: 150,
    },
    {
        name: 'display_order',
        headerKey: 'party_id_scheme.colDisplayOrder',
        style: 'mono_center',
        hidden: false,
        width: 80,
    },
    {
        name: 'version',
        headerKey: 'party_id_scheme.colVersion',
        style: 'mono_center',
        hidden: true,
        width: 80,
    },
    {
        name: 'modified_by',
        headerKey: 'party_id_scheme.colModifiedBy',
        style: 'text_left',
        hidden: true,
        width: 120,
    },
    {
        name: 'recorded_at',
        headerKey: 'party_id_scheme.colRecordedAt',
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
export const partyIdSchemeMeta = {
    entity: 'party_id_scheme',
    collection: 'schemes',
    displayField: 'name',
    keyField: 'code',
    columns: partyIdSchemeColumns,
    fields: partyIdSchemeFields,
} as const;
