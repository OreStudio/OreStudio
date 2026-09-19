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
 * judgement the model does not carry. See artefactType_field_groups.ts
 * beside this file.
 */
import type { ColumnMeta, FieldMeta } from '../../../ui-contract.js';

/**
 * The fields of a artefact_type, in the order the model declares them.
 *
 * `code` is the natural key, so it is editable when creating
 * and read-only afterwards.
 */
export const artefactTypeFields: readonly FieldMeta[] = [
    {
        name: 'code',
        labelKey: 'artefact_type.fldCode',
        control: 'line_edit',
        required: true,
        isKey: true,
        readOnlyAfterCreate: true,
        nullable: false,
        placeholderKey: 'artefact_type.codePh',
    },
    {
        name: 'name',
        labelKey: 'artefact_type.fldName',
        control: 'line_edit',
        required: true,
        isKey: false,
        nullable: false,
        placeholderKey: 'artefact_type.namePh',
    },
    {
        name: 'description',
        labelKey: 'artefact_type.fldDescription',
        control: 'text_edit',
        required: false,
        isKey: false,
        nullable: true,
        placeholderKey: 'artefact_type.descriptionPh',
    },
    {
        name: 'artefact_table',
        labelKey: 'artefact_type.fldArtefactTable',
        control: 'line_edit',
        required: false,
        isKey: false,
        nullable: true,
        placeholderKey: 'artefact_type.artefactTablePh',
    },
    {
        name: 'target_table',
        labelKey: 'artefact_type.fldTargetTable',
        control: 'line_edit',
        required: false,
        isKey: false,
        nullable: true,
        placeholderKey: 'artefact_type.targetTablePh',
    },
    {
        name: 'target_subject',
        labelKey: 'artefact_type.fldTargetSubject',
        control: 'line_edit',
        required: false,
        isKey: false,
        nullable: true,
        placeholderKey: 'artefact_type.targetSubjectPh',
    },
    {
        name: 'display_order',
        labelKey: 'artefact_type.fldDisplayOrder',
        control: 'spin_box',
        required: false,
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
export const artefactTypeColumns: readonly ColumnMeta[] = [
    {
        name: 'code',
        headerKey: 'artefact_type.colCode',
        style: 'text_left',
        hidden: false,
        width: 150,
    },
    {
        name: 'name',
        headerKey: 'artefact_type.colName',
        style: 'text_left',
        hidden: false,
        width: 200,
    },
    {
        name: 'artefact_table',
        headerKey: 'artefact_type.colArtefactTable',
        style: 'text_left',
        hidden: false,
        width: 200,
    },
    {
        name: 'target_table',
        headerKey: 'artefact_type.colTargetTable',
        style: 'text_left',
        hidden: false,
        width: 200,
    },
    {
        name: 'target_subject',
        headerKey: 'artefact_type.colTargetSubject',
        style: 'text_left',
        hidden: false,
        width: 220,
    },
    {
        name: 'display_order',
        headerKey: 'artefact_type.colDisplayOrder',
        style: 'mono_center',
        hidden: false,
        width: 80,
    },
    {
        name: 'version',
        headerKey: 'artefact_type.colVersion',
        style: 'mono_center',
        hidden: true,
        width: 80,
    },
    {
        name: 'modified_by',
        headerKey: 'artefact_type.colModifiedBy',
        style: 'text_left',
        hidden: true,
        width: 120,
    },
    {
        name: 'recorded_at',
        headerKey: 'artefact_type.colRecordedAt',
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
export const artefactTypeMeta = {
    entity: 'artefact_type',
    collection: 'types',
    displayField: 'name',
    keyField: 'code',
    columns: artefactTypeColumns,
    fields: artefactTypeFields,
} as const;
