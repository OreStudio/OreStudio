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
 * The fields of a ois_convention, in the order the model declares them.
 */
export const oisConventionFields: readonly FieldMeta[] = [
    {
        name: 'index',
        labelKey: 'ois_convention.fldIndex',
        control: 'line_edit',
        required: true,
        isKey: false,
        nullable: false,
        placeholderKey: 'ois_convention.indexPh',
    },
    {
        name: 'spot_lag',
        labelKey: 'ois_convention.fldSpotLag',
        control: 'spin_box',
        required: false,
        isKey: false,
        nullable: false,
        min: 0,
        max: 9999,
    },
    {
        name: 'fixed_day_count_fraction',
        labelKey: 'ois_convention.fldFixedDayCountFraction',
        control: 'line_edit',
        required: true,
        isKey: false,
        nullable: false,
        placeholderKey: 'ois_convention.fixedDayCountFractionPh',
    },
    {
        name: 'fixed_calendar',
        labelKey: 'ois_convention.fldFixedCalendar',
        control: 'line_edit',
        required: false,
        isKey: false,
        nullable: true,
        placeholderKey: 'ois_convention.fixedCalendarPh',
    },
    {
        name: 'payment_lag',
        labelKey: 'ois_convention.fldPaymentLag',
        control: 'spin_box',
        required: false,
        isKey: false,
        nullable: true,
        min: -1,
        max: 9999,
    },
    {
        name: 'fixed_frequency',
        labelKey: 'ois_convention.fldFixedFrequency',
        control: 'line_edit',
        required: false,
        isKey: false,
        nullable: true,
        placeholderKey: 'ois_convention.fixedFrequencyPh',
    },
    {
        name: 'fixed_convention',
        labelKey: 'ois_convention.fldFixedConvention',
        control: 'line_edit',
        required: false,
        isKey: false,
        nullable: true,
        placeholderKey: 'ois_convention.fixedConventionPh',
    },
    {
        name: 'fixed_payment_convention',
        labelKey: 'ois_convention.fldFixedPaymentConvention',
        control: 'line_edit',
        required: false,
        isKey: false,
        nullable: true,
        placeholderKey: 'ois_convention.fixedPaymentConventionPh',
    },
    {
        name: 'rule',
        labelKey: 'ois_convention.fldRule',
        control: 'line_edit',
        required: false,
        isKey: false,
        nullable: true,
        placeholderKey: 'ois_convention.rulePh',
    },
    {
        name: 'payment_calendar',
        labelKey: 'ois_convention.fldPaymentCalendar',
        control: 'line_edit',
        required: false,
        isKey: false,
        nullable: true,
        placeholderKey: 'ois_convention.paymentCalendarPh',
    },
    {
        name: 'rate_cutoff',
        labelKey: 'ois_convention.fldRateCutoff',
        control: 'spin_box',
        required: false,
        isKey: false,
        nullable: true,
        min: -1,
        max: 9999,
    },
    {
        name: 'end_of_month',
        labelKey: 'ois_convention.fldEndOfMonth',
        control: 'check_box',
        required: false,
        isKey: false,
        nullable: true,
        triState: true,
    }
];

/**
 * The table columns, in display order. A column hidden by default is
 * offered through the column menu.
 */
export const oisConventionColumns: readonly ColumnMeta[] = [
    {
        name: 'id',
        headerKey: 'ois_convention.colId',
        style: 'text_left',
        hidden: false,
        width: 200,
    },
    {
        name: 'index',
        headerKey: 'ois_convention.colIndex',
        style: 'text_left',
        hidden: false,
        width: 140,
    },
    {
        name: 'spot_lag',
        headerKey: 'ois_convention.colSpotLag',
        style: 'mono_center',
        hidden: false,
        width: 80,
    },
    {
        name: 'fixed_day_count_fraction',
        headerKey: 'ois_convention.colFixedDayCountFraction',
        style: 'text_left',
        hidden: false,
        width: 100,
    },
    {
        name: 'version',
        headerKey: 'ois_convention.colVersion',
        style: 'mono_center',
        hidden: true,
        width: 80,
    },
    {
        name: 'modified_by',
        headerKey: 'ois_convention.colModifiedBy',
        style: 'text_left',
        hidden: true,
        width: 120,
    },
    {
        name: 'recorded_at',
        headerKey: 'ois_convention.colRecordedAt',
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
export const oisConventionMeta = {
    entity: 'ois_convention',
    collection: 'ois_conventions',
    displayField: 'id',
    keyField: 'id',
    columns: oisConventionColumns,
    fields: oisConventionFields,
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
export const oisConventionMessages = {
        ois_convention: {
            title: 'OIS Conventions',
            singular: 'ois convention',
            newTitle: 'New ois convention',
            description: 'Defines the fixed-leg and overnight floating-leg parameters for an OIS. OIS are used to bootstrap risk-free overnight discount curves (e.g. EONIA, SOFR, SONIA). Corresponds to the <OIS> element in ORE conventions.xml.',
            fldIndex: 'Index',
            indexPh: 'e.g. EUR-EONIA',
            fldSpotLag: 'Spot Lag',
            fldFixedDayCountFraction: 'Fixed Day Count Fraction',
            fixedDayCountFractionPh: 'e.g. ACT/360',
            fldFixedCalendar: 'Fixed Calendar',
            fixedCalendarPh: 'e.g. TARGET',
            fldPaymentLag: 'Payment Lag',
            fldFixedFrequency: 'Fixed Frequency',
            fixedFrequencyPh: 'e.g. Annual',
            fldFixedConvention: 'Fixed Convention',
            fixedConventionPh: 'e.g. ModifiedFollowing',
            fldFixedPaymentConvention: 'Fixed Payment Convention',
            fixedPaymentConventionPh: 'e.g. Following',
            fldRule: 'Rule',
            rulePh: 'e.g. Backward',
            fldPaymentCalendar: 'Payment Calendar',
            paymentCalendarPh: 'e.g. TARGET',
            fldRateCutoff: 'Rate Cutoff',
            fldEndOfMonth: 'End Of Month',
            colId: 'Id',
            colIndex: 'Index',
            colSpotLag: 'Spot Lag',
            colFixedDayCountFraction: 'Fixed DCF',
            colVersion: 'Version',
            colModifiedBy: 'Modified By',
            colRecordedAt: 'Recorded At',
        }
};
