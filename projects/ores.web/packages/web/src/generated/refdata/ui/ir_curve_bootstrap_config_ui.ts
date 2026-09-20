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
 * The fields of a ir_curve_bootstrap_config, in the order the model declares them.
 */
export const irCurveBootstrapConfigFields: readonly FieldMeta[] = [
    {
        name: 'source_series_id',
        labelKey: 'ir_curve_bootstrap_config.fldSourceSeriesId',
        control: 'line_edit',
        required: true,
        isKey: false,
        nullable: false,
    },
    {
        name: 'output_series_id',
        labelKey: 'ir_curve_bootstrap_config.fldOutputSeriesId',
        control: 'line_edit',
        required: true,
        isKey: false,
        nullable: false,
    },
    {
        name: 'curve_family_role',
        labelKey: 'ir_curve_bootstrap_config.fldCurveFamilyRole',
        control: 'line_edit',
        required: true,
        isKey: false,
        nullable: false,
    },
    {
        name: 'discount_curve_config_id',
        labelKey: 'ir_curve_bootstrap_config.fldDiscountCurveConfigId',
        control: 'line_edit',
        required: false,
        isKey: false,
        nullable: false,
    },
    {
        name: 'interpolation_method',
        labelKey: 'ir_curve_bootstrap_config.fldInterpolationMethod',
        control: 'line_edit',
        required: true,
        isKey: false,
        nullable: false,
    },
    {
        name: 'day_count_convention',
        labelKey: 'ir_curve_bootstrap_config.fldDayCountConvention',
        control: 'line_edit',
        required: true,
        isKey: false,
        nullable: false,
    },
    {
        name: 'split_tenor_code',
        labelKey: 'ir_curve_bootstrap_config.fldSplitTenorCode',
        control: 'line_edit',
        required: true,
        isKey: false,
        nullable: false,
    }
];

/**
 * The table columns, in display order. A column hidden by default is
 * offered through the column menu.
 */
export const irCurveBootstrapConfigColumns: readonly ColumnMeta[] = [
    {
        name: 'output_series_id',
        headerKey: 'ir_curve_bootstrap_config.colOutputSeriesId',
        style: 'mono_left',
        hidden: false,
    },
    {
        name: 'curve_family_role',
        headerKey: 'ir_curve_bootstrap_config.colCurveFamilyRole',
        style: 'text_left',
        hidden: false,
        width: 100,
    },
    {
        name: 'discount_curve_config_id',
        headerKey: 'ir_curve_bootstrap_config.colDiscountCurveConfigId',
        style: 'mono_left',
        hidden: false,
    },
    {
        name: 'interpolation_method',
        headerKey: 'ir_curve_bootstrap_config.colInterpolationMethod',
        style: 'text_left',
        hidden: false,
        width: 140,
    },
    {
        name: 'day_count_convention',
        headerKey: 'ir_curve_bootstrap_config.colDayCountConvention',
        style: 'text_left',
        hidden: false,
        width: 100,
    },
    {
        name: 'split_tenor_code',
        headerKey: 'ir_curve_bootstrap_config.colSplitTenorCode',
        style: 'text_left',
        hidden: false,
        width: 90,
    },
    {
        name: 'version',
        headerKey: 'ir_curve_bootstrap_config.colVersion',
        style: 'mono_center',
        hidden: true,
        width: 70,
    },
    {
        name: 'modified_by',
        headerKey: 'ir_curve_bootstrap_config.colModifiedBy',
        style: 'text_left',
        hidden: true,
    },
    {
        name: 'recorded_at',
        headerKey: 'ir_curve_bootstrap_config.colRecordedAt',
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
export const irCurveBootstrapConfigMeta = {
    entity: 'ir_curve_bootstrap_config',
    collection: 'ir_curve_bootstrap_configs',
    displayField: '',
    keyField: 'id',
    columns: irCurveBootstrapConfigColumns,
    fields: irCurveBootstrapConfigFields,
} as const;
