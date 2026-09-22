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
 * The fields of a yield_curve_process_parameter_definition, in the order the model declares them.
 *
 * `parameter_name` is the natural key, so it is editable when creating
 * and read-only afterwards.
 */
export const yieldCurveProcessParameterDefinitionFields: readonly FieldMeta[] = [
    {
        name: 'process_type_code',
        labelKey: 'yield_curve_process_parameter_definition.fldProcessTypeCode',
        control: 'line_edit',
        required: true,
        isKey: false,
        nullable: false,
        placeholderKey: 'yield_curve_process_parameter_definition.processTypeCodePh',
    },
    {
        name: 'parameter_name',
        labelKey: 'yield_curve_process_parameter_definition.fldParameterName',
        control: 'line_edit',
        required: true,
        isKey: true,
        readOnlyAfterCreate: true,
        nullable: false,
        placeholderKey: 'yield_curve_process_parameter_definition.parameterNamePh',
    },
    {
        name: 'display_name',
        labelKey: 'yield_curve_process_parameter_definition.fldDisplayName',
        control: 'line_edit',
        required: true,
        isKey: false,
        nullable: false,
        placeholderKey: 'yield_curve_process_parameter_definition.displayNamePh',
    },
    {
        name: 'symbol',
        labelKey: 'yield_curve_process_parameter_definition.fldSymbol',
        control: 'line_edit',
        required: false,
        isKey: false,
        nullable: true,
        placeholderKey: 'yield_curve_process_parameter_definition.symbolPh',
    },
    {
        name: 'short_label',
        labelKey: 'yield_curve_process_parameter_definition.fldShortLabel',
        control: 'line_edit',
        required: true,
        isKey: false,
        nullable: false,
        placeholderKey: 'yield_curve_process_parameter_definition.shortLabelPh',
    },
    {
        name: 'description',
        labelKey: 'yield_curve_process_parameter_definition.fldDescription',
        control: 'text_edit',
        required: true,
        isKey: false,
        nullable: false,
        placeholderKey: 'yield_curve_process_parameter_definition.descriptionPh',
    },
    {
        name: 'data_type',
        labelKey: 'yield_curve_process_parameter_definition.fldDataType',
        control: 'line_edit',
        required: true,
        isKey: false,
        nullable: false,
        placeholderKey: 'yield_curve_process_parameter_definition.dataTypePh',
    },
    {
        name: 'default_value',
        labelKey: 'yield_curve_process_parameter_definition.fldDefaultValue',
        control: 'line_edit',
        required: false,
        isKey: false,
        nullable: false,
    },
    {
        name: 'min_value',
        labelKey: 'yield_curve_process_parameter_definition.fldMinValue',
        control: 'line_edit',
        required: false,
        isKey: false,
        nullable: true,
        placeholderKey: 'yield_curve_process_parameter_definition.minValuePh',
    },
    {
        name: 'max_value',
        labelKey: 'yield_curve_process_parameter_definition.fldMaxValue',
        control: 'line_edit',
        required: false,
        isKey: false,
        nullable: true,
        placeholderKey: 'yield_curve_process_parameter_definition.maxValuePh',
    },
    {
        name: 'display_order',
        labelKey: 'yield_curve_process_parameter_definition.fldDisplayOrder',
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
export const yieldCurveProcessParameterDefinitionColumns: readonly ColumnMeta[] = [
    {
        name: 'process_type_code',
        headerKey: 'yield_curve_process_parameter_definition.colProcessTypeCode',
        style: 'text_left',
        hidden: false,
        width: 140,
    },
    {
        name: 'parameter_name',
        headerKey: 'yield_curve_process_parameter_definition.colParameterName',
        style: 'text_left',
        hidden: false,
        width: 120,
    },
    {
        name: 'display_name',
        headerKey: 'yield_curve_process_parameter_definition.colDisplayName',
        style: 'text_left',
        hidden: false,
        width: 180,
    },
    {
        name: 'symbol',
        headerKey: 'yield_curve_process_parameter_definition.colSymbol',
        style: 'text_left',
        hidden: false,
        width: 60,
    },
    {
        name: 'short_label',
        headerKey: 'yield_curve_process_parameter_definition.colShortLabel',
        style: 'text_left',
        hidden: false,
        width: 140,
    },
    {
        name: 'description',
        headerKey: 'yield_curve_process_parameter_definition.colDescription',
        style: 'text_left',
        hidden: true,
        width: 300,
    },
    {
        name: 'data_type',
        headerKey: 'yield_curve_process_parameter_definition.colDataType',
        style: 'text_left',
        hidden: false,
        width: 80,
    },
    {
        name: 'default_value',
        headerKey: 'yield_curve_process_parameter_definition.colDefaultValue',
        style: 'text_left',
        hidden: false,
        width: 90,
    },
    {
        name: 'min_value',
        headerKey: 'yield_curve_process_parameter_definition.colMinValue',
        style: 'text_left',
        hidden: false,
        width: 80,
    },
    {
        name: 'max_value',
        headerKey: 'yield_curve_process_parameter_definition.colMaxValue',
        style: 'text_left',
        hidden: false,
        width: 80,
    },
    {
        name: 'display_order',
        headerKey: 'yield_curve_process_parameter_definition.colDisplayOrder',
        style: 'mono_center',
        hidden: false,
        width: 90,
    },
    {
        name: 'version',
        headerKey: 'yield_curve_process_parameter_definition.colVersion',
        style: 'mono_center',
        hidden: true,
        width: 70,
    },
    {
        name: 'modified_by',
        headerKey: 'yield_curve_process_parameter_definition.colModifiedBy',
        style: 'text_left',
        hidden: true,
        width: 120,
    },
    {
        name: 'recorded_at',
        headerKey: 'yield_curve_process_parameter_definition.colRecordedAt',
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
export const yieldCurveProcessParameterDefinitionMeta = {
    entity: 'yield_curve_process_parameter_definition',
    collection: 'parameter_definitions',
    displayField: 'parameter_name',
    keyField: 'parameter_name',
    columns: yieldCurveProcessParameterDefinitionColumns,
    fields: yieldCurveProcessParameterDefinitionFields,
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
export const yieldCurveProcessParameterDefinitionMessages = {
        yield_curve_process_parameter_definition: {
            title: 'Yield Curve Process Parameter Definitions',
            singular: 'yield curve process parameter definition',
            newTitle: 'New yield curve process parameter definition',
            description: 'Reference data table describing, per yield_curve_process_type, the named parameters that process accepts and their validation bounds. This is the "vocabulary" half of the row-based parameter architecture: an ir_curve_generation_config stores its process parameters as ir_curve_generation_config_process_parameter_value rows, one per definition, and the mapping layer materialises those rows into the strongly-typed process-parameter structs of ores.analytics.quant (two_factor_gaussian_params, vasicek_params, ...). The (process_type_code, parameter_name) pair uniquely identifies a parameter; four fields drive the parameter table: display_name (the English name), symbol (the Greek letter, where one is conventional), short_label (the layperson name shown in Simple mode) and description (the rich tooltip text). min_value/ max_value (NULL = unbounded) plus default_value drive the dialog\'s spin-box ranges and pre-fill. Why this exists as a table rather than hardcoded structs: it makes the parameter vocabulary queryable and extensible -- adding a new model or parameter is a seed-data change, not a schema or code change, and the same vocabulary drives the DB (validation of value rows), the mapping layer (expected parameter names), and the UI (rows to display). Managed by the system tenant as read-only reference data; tenant users never edit definitions, only the values of their own configs.',
            fldProcessTypeCode: 'Process Type',
            processTypeCodePh: 'e.g. VASICEK, TWO_FACTOR_GAUSSIAN',
            fldParameterName: 'Parameter',
            parameterNamePh: 'e.g. kappa_x',
            fldDisplayName: 'Display Name',
            displayNamePh: 'e.g. Mean reversion speed',
            fldSymbol: 'Symbol',
            symbolPh: 'e.g. κ; empty when none',
            fldShortLabel: 'Short Label',
            shortLabelPh: 'e.g. Reversion speed',
            fldDescription: 'Description',
            descriptionPh: 'What this parameter means',
            fldDataType: 'Data Type',
            dataTypePh: 'double',
            fldDefaultValue: 'Default Value',
            fldMinValue: 'Min Value',
            minValuePh: 'Unbounded if empty',
            fldMaxValue: 'Max Value',
            maxValuePh: 'Unbounded if empty',
            fldDisplayOrder: 'Display Order',
            colProcessTypeCode: 'Process Type',
            colParameterName: 'Parameter',
            colDisplayName: 'Display Name',
            colSymbol: 'Symbol',
            colShortLabel: 'Short Label',
            colDescription: 'Description',
            colDataType: 'Data Type',
            colDefaultValue: 'Default',
            colMinValue: 'Min',
            colMaxValue: 'Max',
            colDisplayOrder: 'Display Order',
            colVersion: 'Version',
            colModifiedBy: 'Modified By',
            colRecordedAt: 'Recorded At',
        }
};
