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
 * judgement the model does not carry. See leiEntity_field_groups.ts
 * beside this file.
 */
import type { ColumnMeta, FieldMeta } from '../../../ui-contract.js';

/**
 * The fields of a lei_entity, in the order the model declares them.
 *
 * `lei` is the natural key, so it is editable when creating
 * and read-only afterwards.
 */
export const leiEntityFields: readonly FieldMeta[] = [
    {
        name: 'lei',
        labelKey: 'lei_entity.fldLei',
        control: 'line_edit',
        required: true,
        isKey: true,
        readOnlyAfterCreate: true,
        nullable: false,
        placeholderKey: 'lei_entity.leiPh',
    }
];

/**
 * The table columns, in display order. A column hidden by default is
 * offered through the column menu.
 */
export const leiEntityColumns: readonly ColumnMeta[] = [
    {
        name: 'lei',
        headerKey: 'lei_entity.colLei',
        style: 'text_left',
        hidden: false,
        width: 150,
    },
    {
        name: 'entity_legal_name',
        headerKey: 'lei_entity.colLegalName',
        style: 'text_left',
        hidden: false,
        width: 250,
    },
    {
        name: 'entity_entity_status',
        headerKey: 'lei_entity.colStatus',
        style: 'text_left',
        hidden: false,
        width: 120,
    }
];

/**
 * The collection name the list request uses, and the display field.
 *
 * Emitted so the sidebar and the lookup selects do not have to know it
 * separately.
 */
export const leiEntityMeta = {
    entity: 'lei_entity',
    collection: 'entities',
    displayField: 'lei',
    keyField: 'lei',
    columns: leiEntityColumns,
    fields: leiEntityFields,
} as const;
