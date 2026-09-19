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
 * judgement the model does not carry. See irCurveGenerationConfig_field_groups.ts
 * beside this file.
 */
import type { ColumnMeta, FieldMeta } from '../../../ui-contract.js';

/**
 * The fields of a ir_curve_generation_config, in the order the model declares them.
 */
export const irCurveGenerationConfigFields: readonly FieldMeta[] = [
    {
        name: 'currency_code',
        labelKey: 'ir_curve_generation_config.fldCurrencyCode',
        control: 'line_edit',
        required: true,
        isKey: false,
        nullable: false,
        placeholderKey: 'ir_curve_generation_config.currencyCodePh',
    },
    {
        name: 'index_family',
        labelKey: 'ir_curve_generation_config.fldIndexFamily',
        control: 'static_combo',
        required: true,
        isKey: false,
        nullable: false,
        options: [
            { value: 'libor', labelKey: 'ir_curve_generation_config.type.libor' },
            { value: 'euribor', labelKey: 'ir_curve_generation_config.type.euribor' },
            { value: 'sofr', labelKey: 'ir_curve_generation_config.type.sofr' },
            { value: 'estr', labelKey: 'ir_curve_generation_config.type.estr' },
            { value: 'sonia', labelKey: 'ir_curve_generation_config.type.sonia' },
            { value: 'tona', labelKey: 'ir_curve_generation_config.type.tona' },
            { value: 'saron', labelKey: 'ir_curve_generation_config.type.saron' },
            { value: 'aonia', labelKey: 'ir_curve_generation_config.type.aonia' },
            { value: 'corra', labelKey: 'ir_curve_generation_config.type.corra' },
            { value: 'honia', labelKey: 'ir_curve_generation_config.type.honia' },
            { value: 'sora', labelKey: 'ir_curve_generation_config.type.sora' },
            { value: 'swestr', labelKey: 'ir_curve_generation_config.type.swestr' },
            { value: 'nowa', labelKey: 'ir_curve_generation_config.type.nowa' },
            { value: 'kofr', labelKey: 'ir_curve_generation_config.type.kofr' },
            { value: 'mibor', labelKey: 'ir_curve_generation_config.type.mibor' },
            { value: 'zaronia', labelKey: 'ir_curve_generation_config.type.zaronia' },
            { value: 'destr', labelKey: 'ir_curve_generation_config.type.destr' },
            { value: 'polonia', labelKey: 'ir_curve_generation_config.type.polonia' },
            { value: 'nzonia', labelKey: 'ir_curve_generation_config.type.nzonia' },
            { value: 'shibor', labelKey: 'ir_curve_generation_config.type.shibor' },
            { value: 'tiie', labelKey: 'ir_curve_generation_config.type.tiie' },
            { value: 'taibor', labelKey: 'ir_curve_generation_config.type.taibor' },
        ],
    },
    {
        name: 'tenor',
        labelKey: 'ir_curve_generation_config.fldTenor',
        control: 'dynamic_combo',
        required: false,
        isKey: false,
        nullable: false,
        lookup: { collection: 'tenors', valueField: 'code', labelField: 'display_name' },
    },
    {
        name: 'role',
        labelKey: 'ir_curve_generation_config.fldRole',
        control: 'static_combo',
        required: true,
        isKey: false,
        nullable: false,
        options: [
            { value: 'discount', labelKey: 'ir_curve_generation_config.type.discount' },
            { value: 'projection', labelKey: 'ir_curve_generation_config.type.projection' },
            { value: 'self_discounting', labelKey: 'ir_curve_generation_config.type.self_discounting' },
        ],
    },
    {
        name: 'process_type',
        labelKey: 'ir_curve_generation_config.fldProcessType',
        control: 'line_edit',
        required: true,
        isKey: false,
        nullable: false,
        placeholderKey: 'ir_curve_generation_config.processTypePh',
    },
    {
        name: 'ticks_per_hour',
        labelKey: 'ir_curve_generation_config.fldTicksPerHour',
        control: 'spin_box',
        required: true,
        isKey: false,
        nullable: false,
        min: 0,
        max: 9999,
    },
    {
        name: 'fixed_leg_payment_frequency_code',
        labelKey: 'ir_curve_generation_config.fldFixedLegPaymentFrequencyCode',
        control: 'line_edit',
        required: true,
        isKey: false,
        nullable: false,
        placeholderKey: 'ir_curve_generation_config.fixedLegPaymentFrequencyCodePh',
    },
    {
        name: 'enabled',
        labelKey: 'ir_curve_generation_config.fldEnabled',
        control: 'check_box',
        required: false,
        isKey: false,
        nullable: false,
    },
    {
        name: 'auto_start',
        labelKey: 'ir_curve_generation_config.fldAutoStart',
        control: 'check_box',
        required: false,
        isKey: false,
        nullable: false,
    },
    {
        name: 'price_source',
        labelKey: 'ir_curve_generation_config.fldPriceSource',
        control: 'line_edit',
        required: true,
        isKey: false,
        nullable: false,
        placeholderKey: 'ir_curve_generation_config.priceSourcePh',
    },
    {
        name: 'vintage_source',
        labelKey: 'ir_curve_generation_config.fldVintageSource',
        control: 'line_edit',
        required: false,
        isKey: false,
        nullable: false,
        placeholderKey: 'ir_curve_generation_config.vintageSourcePh',
    },
    {
        name: 'vintage_date',
        labelKey: 'ir_curve_generation_config.fldVintageDate',
        control: 'line_edit',
        required: false,
        isKey: false,
        nullable: false,
        placeholderKey: 'ir_curve_generation_config.vintageDatePh',
    },
    {
        name: 'description',
        labelKey: 'ir_curve_generation_config.fldDescription',
        control: 'text_edit',
        required: false,
        isKey: false,
        nullable: true,
        placeholderKey: 'ir_curve_generation_config.descriptionPh',
    }
];

/**
 * The table columns, in display order. A column hidden by default is
 * offered through the column menu.
 */
export const irCurveGenerationConfigColumns: readonly ColumnMeta[] = [
    {
        name: 'currency_code',
        headerKey: 'ir_curve_generation_config.colCurrencyCode',
        style: 'text_left',
        hidden: false,
        width: 100,
    },
    {
        name: 'index_family',
        headerKey: 'ir_curve_generation_config.colIndexFamily',
        style: 'text_left',
        hidden: false,
        width: 100,
    },
    {
        name: 'tenor',
        headerKey: 'ir_curve_generation_config.colTenor',
        style: 'text_left',
        hidden: false,
        width: 80,
    },
    {
        name: 'role',
        headerKey: 'ir_curve_generation_config.colRole',
        style: 'text_left',
        hidden: false,
        width: 100,
    },
    {
        name: 'process_type',
        headerKey: 'ir_curve_generation_config.colProcessType',
        style: 'text_left',
        hidden: false,
        width: 100,
    },
    {
        name: 'ticks_per_hour',
        headerKey: 'ir_curve_generation_config.colTicksPerHour',
        style: 'mono_center',
        hidden: false,
        width: 80,
    },
    {
        name: 'fixed_leg_payment_frequency_code',
        headerKey: 'ir_curve_generation_config.colFixedLegPaymentFrequencyCode',
        style: 'text_left',
        hidden: false,
        width: 140,
    },
    {
        name: 'enabled',
        headerKey: 'ir_curve_generation_config.colEnabled',
        style: 'text_left',
        hidden: false,
        width: 70,
    },
    {
        name: 'auto_start',
        headerKey: 'ir_curve_generation_config.colAutoStart',
        style: 'text_left',
        hidden: false,
        width: 80,
    },
    {
        name: 'price_source',
        headerKey: 'ir_curve_generation_config.colPriceSource',
        style: 'text_left',
        hidden: false,
        width: 100,
    },
    {
        name: 'vintage_source',
        headerKey: 'ir_curve_generation_config.colVintageSource',
        style: 'text_left',
        hidden: false,
        width: 140,
    },
    {
        name: 'vintage_date',
        headerKey: 'ir_curve_generation_config.colVintageDate',
        style: 'text_left',
        hidden: false,
        width: 110,
    },
    {
        name: 'version',
        headerKey: 'ir_curve_generation_config.colVersion',
        style: 'mono_center',
        hidden: true,
        width: 70,
    },
    {
        name: 'modified_by',
        headerKey: 'ir_curve_generation_config.colModifiedBy',
        style: 'text_left',
        hidden: true,
        width: 120,
    },
    {
        name: 'recorded_at',
        headerKey: 'ir_curve_generation_config.colRecordedAt',
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
export const irCurveGenerationConfigMeta = {
    entity: 'ir_curve_generation_config',
    collection: 'ir_curve_generation_configs',
    displayField: '',
    keyField: 'id',
    columns: irCurveGenerationConfigColumns,
    fields: irCurveGenerationConfigFields,
} as const;
