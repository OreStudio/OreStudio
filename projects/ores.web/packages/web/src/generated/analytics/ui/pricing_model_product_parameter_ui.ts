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
 * The fields of a pricing_model_product_parameter, in the order the model declares them.
 *
 * `parameter_name` is the natural key, so it is editable when creating
 * and read-only afterwards.
 */
export const pricingModelProductParameterFields: readonly FieldMeta[] = [
    {
        name: 'parameter_scope',
        labelKey: 'pricing_model_product_parameter.fldParameterScope',
        control: 'static_combo',
        required: true,
        isKey: false,
        nullable: false,
    },
    {
        name: 'parameter_name',
        labelKey: 'pricing_model_product_parameter.fldParameterName',
        control: 'line_edit',
        required: true,
        isKey: true,
        readOnlyAfterCreate: true,
        nullable: false,
        placeholderKey: 'pricing_model_product_parameter.parameterNamePh',
    },
    {
        name: 'parameter_value',
        labelKey: 'pricing_model_product_parameter.fldParameterValue',
        control: 'line_edit',
        required: true,
        isKey: false,
        nullable: false,
        placeholderKey: 'pricing_model_product_parameter.parameterValuePh',
    }
];

/**
 * The table columns, in display order. A column hidden by default is
 * offered through the column menu.
 */
export const pricingModelProductParameterColumns: readonly ColumnMeta[] = [
    {
        name: 'parameter_scope',
        headerKey: 'pricing_model_product_parameter.colParameterScope',
        style: 'text_left',
        hidden: false,
        width: 80,
    },
    {
        name: 'parameter_name',
        headerKey: 'pricing_model_product_parameter.colParameterName',
        style: 'text_left',
        hidden: false,
        width: 200,
    },
    {
        name: 'parameter_value',
        headerKey: 'pricing_model_product_parameter.colParameterValue',
        style: 'text_left',
        hidden: false,
        width: 200,
    },
    {
        name: 'version',
        headerKey: 'pricing_model_product_parameter.colVersion',
        style: 'mono_center',
        hidden: true,
        width: 70,
    },
    {
        name: 'modified_by',
        headerKey: 'pricing_model_product_parameter.colModifiedBy',
        style: 'text_left',
        hidden: true,
        width: 120,
    },
    {
        name: 'recorded_at',
        headerKey: 'pricing_model_product_parameter.colRecordedAt',
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
export const pricingModelProductParameterMeta = {
    entity: 'pricing_model_product_parameter',
    collection: 'parameters',
    displayField: 'parameter_name',
    keyField: 'parameter_name',
    columns: pricingModelProductParameterColumns,
    fields: pricingModelProductParameterFields,
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
export const pricingModelProductParameterMessages = {
        pricing_model_product_parameter: {
            title: 'Pricing Model Product Parameters',
            singular: 'pricing model product parameter',
            newTitle: 'New pricing model product parameter',
            description: 'Stores model parameters, engine parameters, and global parameters as normalised rows for granular diffing. Product-scoped parameters have pricing_model_product_id set; global parameters have it NULL with parameter_scope = \'global\'.',
            fldParameterScope: 'Scope',
            fldParameterName: 'Name',
            parameterNamePh: 'Parameter name (e.g. Calibration, Reversion, sy)',
            fldParameterValue: 'Value',
            parameterValuePh: 'Parameter value (e.g. Bootstrap, 0.03, Y)',
            colParameterScope: 'Scope',
            colParameterName: 'Name',
            colParameterValue: 'Value',
            colVersion: 'Version',
            colModifiedBy: 'Modified By',
            colRecordedAt: 'Recorded At',
        }
};
