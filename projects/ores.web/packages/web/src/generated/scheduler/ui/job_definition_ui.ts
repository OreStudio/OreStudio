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
 * judgement the model does not carry. See jobDefinition_field_groups.ts
 * beside this file.
 */
import type { ColumnMeta, FieldMeta } from '../../../ui-contract.js';

/**
 * The fields of a job_definition, in the order the model declares them.
 *
 * `job_name` is the natural key, so it is editable when creating
 * and read-only afterwards.
 */
export const jobDefinitionFields: readonly FieldMeta[] = [
    {
        name: 'job_name',
        labelKey: 'job_definition.fldJobName',
        control: 'line_edit',
        required: true,
        isKey: true,
        readOnlyAfterCreate: true,
        nullable: false,
        placeholderKey: 'job_definition.jobNamePh',
    },
    {
        name: 'description',
        labelKey: 'job_definition.fldDescription',
        control: 'text_edit',
        required: false,
        isKey: false,
        nullable: false,
        placeholderKey: 'job_definition.descriptionPh',
    },
    {
        name: 'command',
        labelKey: 'job_definition.fldCommand',
        control: 'text_edit',
        required: true,
        isKey: false,
        nullable: false,
        placeholderKey: 'job_definition.commandPh',
    },
    {
        name: 'schedule_expression',
        labelKey: 'job_definition.fldScheduleExpression',
        control: 'line_edit',
        required: true,
        isKey: false,
        nullable: false,
        placeholderKey: 'job_definition.scheduleExpressionPh',
    },
    {
        name: 'database_name',
        labelKey: 'job_definition.fldDatabaseName',
        control: 'line_edit',
        required: true,
        isKey: false,
        nullable: false,
        placeholderKey: 'job_definition.databaseNamePh',
    }
];

/**
 * The table columns, in display order. A column hidden by default is
 * offered through the column menu.
 */
export const jobDefinitionColumns: readonly ColumnMeta[] = [
    {
        name: 'job_name',
        headerKey: 'job_definition.colJobName',
        style: 'text_left',
        hidden: false,
        width: 200,
    },
    {
        name: 'description',
        headerKey: 'job_definition.colDescription',
        style: 'text_left',
        hidden: true,
        width: 250,
    },
    {
        name: 'schedule_expression',
        headerKey: 'job_definition.colSchedule',
        style: 'text_left',
        hidden: false,
        width: 150,
    },
    {
        name: 'database_name',
        headerKey: 'job_definition.colDatabaseName',
        style: 'text_left',
        hidden: false,
        width: 150,
    },
    {
        name: 'is_active',
        headerKey: 'job_definition.colActive',
        style: 'text_left',
        hidden: false,
        width: 80,
    },
    {
        name: 'version',
        headerKey: 'job_definition.colVersion',
        style: 'mono_center',
        hidden: true,
        width: 60,
    },
    {
        name: 'modified_by',
        headerKey: 'job_definition.colModifiedBy',
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
export const jobDefinitionMeta = {
    entity: 'job_definition',
    collection: 'definitions',
    displayField: 'job_name',
    keyField: 'job_name',
    columns: jobDefinitionColumns,
    fields: jobDefinitionFields,
} as const;
