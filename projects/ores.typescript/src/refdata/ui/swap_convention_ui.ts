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
 * judgement the model does not carry. See swapConvention_field_groups.ts
 * beside this file.
 */
import type { ColumnMeta, FieldMeta } from '../../ui-contract.js';

/**
 * The fields of a swap_convention, in the order the model declares them.
 */
export const swapConventionFields: readonly FieldMeta[] = [
    {
        name: 'fixed_frequency',
        labelKey: 'swap_convention.fldFixedFrequency',
        control: 'line_edit',
        required: true,
        isKey: false,
        nullable: false,
        placeholderKey: 'swap_convention.fixedFrequencyPh',
    },
    {
        name: 'fixed_day_count_fraction',
        labelKey: 'swap_convention.fldFixedDayCountFraction',
        control: 'line_edit',
        required: true,
        isKey: false,
        nullable: false,
        placeholderKey: 'swap_convention.fixedDayCountFractionPh',
    },
    {
        name: 'index',
        labelKey: 'swap_convention.fldIndex',
        control: 'line_edit',
        required: true,
        isKey: false,
        nullable: false,
        placeholderKey: 'swap_convention.indexPh',
    },
    {
        name: 'fixed_calendar',
        labelKey: 'swap_convention.fldFixedCalendar',
        control: 'line_edit',
        required: false,
        isKey: false,
        nullable: true,
        placeholderKey: 'swap_convention.fixedCalendarPh',
    },
    {
        name: 'fixed_convention',
        labelKey: 'swap_convention.fldFixedConvention',
        control: 'line_edit',
        required: false,
        isKey: false,
        nullable: true,
        placeholderKey: 'swap_convention.fixedConventionPh',
    },
    {
        name: 'float_frequency',
        labelKey: 'swap_convention.fldFloatFrequency',
        control: 'line_edit',
        required: false,
        isKey: false,
        nullable: true,
        placeholderKey: 'swap_convention.floatFrequencyPh',
    },
    {
        name: 'sub_periods_coupon_type',
        labelKey: 'swap_convention.fldSubPeriodsCouponType',
        control: 'line_edit',
        required: false,
        isKey: false,
        nullable: true,
        placeholderKey: 'swap_convention.subPeriodsCouponTypePh',
    }
];

/**
 * The table columns, in display order. A column hidden by default is
 * offered through the column menu.
 */
export const swapConventionColumns: readonly ColumnMeta[] = [
    {
        name: 'id',
        headerKey: 'swap_convention.colId',
        style: 'text_left',
        hidden: false,
        width: 220,
    },
    {
        name: 'fixed_frequency',
        headerKey: 'swap_convention.colFixedFrequency',
        style: 'text_left',
        hidden: false,
        width: 100,
    },
    {
        name: 'fixed_day_count_fraction',
        headerKey: 'swap_convention.colFixedDayCountFraction',
        style: 'text_left',
        hidden: false,
        width: 100,
    },
    {
        name: 'index',
        headerKey: 'swap_convention.colIndex',
        style: 'text_left',
        hidden: false,
        width: 160,
    },
    {
        name: 'version',
        headerKey: 'swap_convention.colVersion',
        style: 'mono_center',
        hidden: true,
        width: 80,
    },
    {
        name: 'modified_by',
        headerKey: 'swap_convention.colModifiedBy',
        style: 'text_left',
        hidden: true,
        width: 120,
    },
    {
        name: 'recorded_at',
        headerKey: 'swap_convention.colRecordedAt',
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
export const swapConventionMeta = {
    entity: 'swap_convention',
    collection: 'swap_conventions',
    displayField: 'id',
    keyField: 'id',
    columns: swapConventionColumns,
    fields: swapConventionFields,
} as const;
