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
/**
 * The entity's own words, in English, keyed the way the catalogue is.
 *
 * The model states them: the detail field's label, the column's header, the
 * placeholder, the title and the brief. They are emitted here rather than
 * written into a catalogue by hand, so a label the model changes changes in
 * one place, and a language that has no translation yet falls back to these
 * rather than to a key nobody can read.
 */
export const swapConventionMessages = {
        swap_convention: {
            title: 'Swap Conventions',
            singular: 'swap convention',
            newTitle: 'New swap convention',
            description: 'Defines the fixed-leg schedule and the floating-leg index for a standard interest rate swap. Used by ORE to bootstrap par swap rates off a yield curve. Corresponds to the <Swap> element in ORE conventions.xml.',
            fldFixedFrequency: 'Fixed Frequency',
            fixedFrequencyPh: 'e.g. Annual',
            fldFixedDayCountFraction: 'Fixed Day Count Fraction',
            fixedDayCountFractionPh: 'e.g. 30/360',
            fldIndex: 'Index',
            indexPh: 'e.g. EUR-EURIBOR-6M',
            fldFixedCalendar: 'Fixed Calendar',
            fixedCalendarPh: 'e.g. TARGET',
            fldFixedConvention: 'Fixed Convention',
            fixedConventionPh: 'e.g. ModifiedFollowing',
            fldFloatFrequency: 'Float Frequency',
            floatFrequencyPh: 'e.g. Semiannual',
            fldSubPeriodsCouponType: 'Sub-Periods Coupon Type',
            subPeriodsCouponTypePh: 'e.g. Compounding',
            colId: 'Id',
            colFixedFrequency: 'Fixed Freq',
            colFixedDayCountFraction: 'Fixed DCF',
            colIndex: 'Index',
            colVersion: 'Version',
            colModifiedBy: 'Modified By',
            colRecordedAt: 'Recorded At',
        }
};
