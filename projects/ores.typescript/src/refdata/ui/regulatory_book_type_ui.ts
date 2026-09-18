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
 * judgement the model does not carry. See regulatoryBookType_field_groups.ts
 * beside this file.
 */
import type { ColumnMeta, FieldMeta } from '../../ui-contract.js';

/**
 * The fields of a regulatory_book_type, in the order the model declares them.
 *
 * `code` is the natural key, so it is editable when creating
 * and read-only afterwards.
 */
export const regulatoryBookTypeFields: readonly FieldMeta[] = [
    {
        name: 'code',
        labelKey: 'regulatory_book_type.fldCode',
        control: 'line_edit',
        required: true,
        isKey: true,
        readOnlyAfterCreate: true,
        nullable: false,
        placeholderKey: 'regulatory_book_type.codePh',
    },
    {
        name: 'name',
        labelKey: 'regulatory_book_type.fldName',
        control: 'line_edit',
        required: true,
        isKey: false,
        nullable: false,
        placeholderKey: 'regulatory_book_type.namePh',
    },
    {
        name: 'description',
        labelKey: 'regulatory_book_type.fldDescription',
        control: 'text_edit',
        required: false,
        isKey: false,
        nullable: false,
        placeholderKey: 'regulatory_book_type.descriptionPh',
    }
];

/**
 * The table columns, in display order. A column hidden by default is
 * offered through the column menu.
 */
export const regulatoryBookTypeColumns: readonly ColumnMeta[] = [
    {
        name: 'code',
        headerKey: 'regulatory_book_type.colCode',
        style: 'text_left',
        hidden: false,
    },
    {
        name: 'name',
        headerKey: 'regulatory_book_type.colName',
        style: 'text_left',
        hidden: false,
    },
    {
        name: 'description',
        headerKey: 'regulatory_book_type.colDescription',
        style: 'text_left',
        hidden: true,
    },
    {
        name: 'display_order',
        headerKey: 'regulatory_book_type.colDisplayOrder',
        style: 'mono_center',
        hidden: false,
        width: 70,
    },
    {
        name: 'version',
        headerKey: 'regulatory_book_type.colVersion',
        style: 'mono_center',
        hidden: true,
        width: 70,
    },
    {
        name: 'modified_by',
        headerKey: 'regulatory_book_type.colModifiedBy',
        style: 'text_left',
        hidden: true,
    },
    {
        name: 'recorded_at',
        headerKey: 'regulatory_book_type.colRecordedAt',
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
export const regulatoryBookTypeMeta = {
    entity: 'regulatory_book_type',
    collection: 'types',
    displayField: 'name',
    keyField: 'code',
    columns: regulatoryBookTypeColumns,
    fields: regulatoryBookTypeFields,
} as const;
