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
 * judgement the model does not carry, so regeneration cannot invent it. The
 * model cannot express a grouping yet, so every field renders in one group.
 */
import type { ColumnMeta, FieldMeta } from '../../../ui-contract.js';

/**
 * The fields of a report_definition, in the order the model declares them.
 *
 * `name` is the natural key, so it is editable when creating
 * and read-only afterwards.
 */
export const reportDefinitionFields: readonly FieldMeta[] = [
    {
        name: 'name',
        labelKey: 'report_definition.fldName',
        control: 'line_edit',
        required: true,
        isKey: true,
        readOnlyAfterCreate: true,
        nullable: false,
        placeholderKey: 'report_definition.namePh',
    },
    {
        name: 'description',
        labelKey: 'report_definition.fldDescription',
        control: 'text_edit',
        required: false,
        isKey: false,
        nullable: false,
        placeholderKey: 'report_definition.descriptionPh',
    },
    {
        name: 'report_type',
        labelKey: 'report_definition.fldReportType',
        control: 'line_edit',
        required: true,
        isKey: false,
        nullable: false,
        placeholderKey: 'report_definition.reportTypePh',
    },
    {
        name: 'schedule_expression',
        labelKey: 'report_definition.fldScheduleExpression',
        control: 'line_edit',
        required: true,
        isKey: false,
        nullable: false,
        placeholderKey: 'report_definition.scheduleExpressionPh',
    },
    {
        name: 'concurrency_policy',
        labelKey: 'report_definition.fldConcurrencyPolicy',
        control: 'line_edit',
        required: true,
        isKey: false,
        nullable: false,
        placeholderKey: 'report_definition.concurrencyPolicyPh',
    }
];

/**
 * The table columns, in display order. A column hidden by default is
 * offered through the column menu.
 */
export const reportDefinitionColumns: readonly ColumnMeta[] = [
    {
        name: 'name',
        headerKey: 'report_definition.colName',
        style: 'text_left',
        hidden: false,
        width: 200,
    },
    {
        name: 'report_type',
        headerKey: 'report_definition.colReportType',
        style: 'text_left',
        hidden: false,
        width: 100,
    },
    {
        name: 'schedule_expression',
        headerKey: 'report_definition.colScheduleExpression',
        style: 'text_left',
        hidden: false,
        width: 150,
    },
    {
        name: 'concurrency_policy',
        headerKey: 'report_definition.colConcurrencyPolicy',
        style: 'text_left',
        hidden: false,
        width: 100,
    },
    {
        name: 'version',
        headerKey: 'report_definition.colVersion',
        style: 'mono_center',
        hidden: true,
        width: 60,
    },
    {
        name: 'modified_by',
        headerKey: 'report_definition.colModifiedBy',
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
export const reportDefinitionMeta = {
    entity: 'report_definition',
    collection: 'definitions',
    displayField: 'name',
    keyField: 'name',
    columns: reportDefinitionColumns,
    fields: reportDefinitionFields,
} as const;
/**
 * The entity's own words, in English, keyed the way the catalogue is.
 *
 * The model states them: the detail field's label, the column's header, the
 * placeholder, the title and the brief. They are emitted here rather than
 * written into a catalogue by hand, so a label the model changes changes in
 * one place, and a language that has no translation yet falls back to these
 * rather than to a key nobody can read.
 */
export const reportDefinitionMessages = {
        report_definition: {
            title: 'Report Definitions',
            singular: 'report definition',
            newTitle: 'New report definition',
            description: 'The persistent template for a report. Describes what to run, when to run it, and how to handle concurrent executions. Type-specific configuration (e.g. risk parameters) lives in a separate table keyed by report_definition_id. Lifecycle is managed through the report_definition_lifecycle FSM machine. fsm_state_id points to the current state in ores_dq_fsm_states_tbl. scheduler_job_id links to ores_scheduler_job_definitions_tbl.id and is set by the scheduler service when the definition is activated (state: active). It is cleared when the definition is suspended or archived.',
            fldName: 'Name',
            namePh: 'Enter report definition name',
            fldDescription: 'Description',
            descriptionPh: 'Enter a description',
            fldReportType: 'Report Type',
            reportTypePh: 'e.g. risk',
            fldScheduleExpression: 'Schedule (cron)',
            scheduleExpressionPh: 'e.g. 0 6 * * 1',
            fldConcurrencyPolicy: 'Concurrency Policy',
            concurrencyPolicyPh: 'e.g. skip',
            colName: 'Name',
            colReportType: 'Type',
            colScheduleExpression: 'Schedule',
            colConcurrencyPolicy: 'Concurrency',
            colVersion: 'Version',
            colModifiedBy: 'Modified By',
        }
};
