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
 * judgement the model does not carry. See badgeDefinition_field_groups.ts
 * beside this file.
 */
import type { ColumnMeta, FieldMeta } from '../../../ui-contract.js';

/**
 * The fields of a badge_definition, in the order the model declares them.
 *
 * `code` is the natural key, so it is editable when creating
 * and read-only afterwards.
 */
export const badgeDefinitionFields: readonly FieldMeta[] = [
    {
        name: 'code',
        labelKey: 'badge_definition.fldCode',
        control: 'line_edit',
        required: true,
        isKey: true,
        readOnlyAfterCreate: true,
        nullable: false,
        placeholderKey: 'badge_definition.codePh',
    },
    {
        name: 'name',
        labelKey: 'badge_definition.fldName',
        control: 'line_edit',
        required: true,
        isKey: false,
        nullable: false,
        placeholderKey: 'badge_definition.namePh',
    },
    {
        name: 'description',
        labelKey: 'badge_definition.fldDescription',
        control: 'text_edit',
        required: true,
        isKey: false,
        nullable: false,
        placeholderKey: 'badge_definition.descriptionPh',
    },
    {
        name: 'background_colour',
        labelKey: 'badge_definition.fldBackgroundColour',
        control: 'colour',
        required: true,
        isKey: false,
        nullable: false,
        placeholderKey: 'badge_definition.backgroundColourPh',
    },
    {
        name: 'text_colour',
        labelKey: 'badge_definition.fldTextColour',
        control: 'colour',
        required: true,
        isKey: false,
        nullable: false,
        placeholderKey: 'badge_definition.textColourPh',
    },
    {
        name: 'severity_code',
        labelKey: 'badge_definition.fldSeverityCode',
        control: 'dynamic_combo',
        required: true,
        isKey: false,
        nullable: false,
        lookup: { collection: 'severities', valueField: 'code', labelField: 'description' },
    },
    {
        name: 'display_order',
        labelKey: 'badge_definition.fldDisplayOrder',
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
export const badgeDefinitionColumns: readonly ColumnMeta[] = [
    {
        name: 'code',
        headerKey: 'badge_definition.colCode',
        style: 'text_left',
        hidden: false,
        width: 150,
    },
    {
        name: 'name',
        headerKey: 'badge_definition.colName',
        style: 'text_left',
        hidden: false,
        width: 150,
    },
    {
        name: 'description',
        headerKey: 'badge_definition.colDescription',
        style: 'text_left',
        hidden: true,
        width: 250,
    },
    {
        name: 'background_colour',
        headerKey: 'badge_definition.colBackgroundColour',
        style: 'badge_centered',
        hidden: false,
        width: 100,
    },
    {
        name: 'text_colour',
        headerKey: 'badge_definition.colTextColour',
        style: 'badge_centered',
        hidden: false,
        width: 80,
    },
    {
        name: 'severity_code',
        headerKey: 'badge_definition.colSeverityCode',
        style: 'text_left',
        hidden: false,
        width: 100,
    },
    {
        name: 'display_order',
        headerKey: 'badge_definition.colDisplayOrder',
        style: 'mono_center',
        hidden: false,
        width: 80,
    },
    {
        name: 'version',
        headerKey: 'badge_definition.colVersion',
        style: 'mono_center',
        hidden: true,
        width: 80,
    },
    {
        name: 'modified_by',
        headerKey: 'badge_definition.colModifiedBy',
        style: 'text_left',
        hidden: true,
        width: 120,
    },
    {
        name: 'recorded_at',
        headerKey: 'badge_definition.colRecordedAt',
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
export const badgeDefinitionMeta = {
    entity: 'badge_definition',
    collection: 'definitions',
    displayField: 'name',
    keyField: 'code',
    columns: badgeDefinitionColumns,
    fields: badgeDefinitionFields,
} as const;
