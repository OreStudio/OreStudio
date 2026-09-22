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
 * The fields of a payment_frequency, in the order the model declares them.
 *
 * `code` is the natural key, so it is editable when creating
 * and read-only afterwards.
 */
export const paymentFrequencyFields: readonly FieldMeta[] = [
    {
        name: 'code',
        labelKey: 'payment_frequency.fldCode',
        control: 'line_edit',
        required: true,
        isKey: true,
        readOnlyAfterCreate: true,
        nullable: false,
        placeholderKey: 'payment_frequency.codePh',
    },
    {
        name: 'name',
        labelKey: 'payment_frequency.fldName',
        control: 'line_edit',
        required: true,
        isKey: false,
        nullable: false,
        placeholderKey: 'payment_frequency.namePh',
    },
    {
        name: 'description',
        labelKey: 'payment_frequency.fldDescription',
        control: 'text_edit',
        required: true,
        isKey: false,
        nullable: false,
        placeholderKey: 'payment_frequency.descriptionPh',
    },
    {
        name: 'period_unit',
        labelKey: 'payment_frequency.fldPeriodUnit',
        control: 'dynamic_combo',
        required: true,
        isKey: false,
        nullable: false,
        lookup: { collection: 'units', valueField: 'code', labelField: 'description' },
        codeDomain: 'tenor_unit',
    },
    {
        name: 'period_multiplier',
        labelKey: 'payment_frequency.fldPeriodMultiplier',
        control: 'spin_box',
        required: false,
        isKey: false,
        nullable: true,
        min: -1,
        max: 9999,
    },
    {
        name: 'display_order',
        labelKey: 'payment_frequency.fldDisplayOrder',
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
export const paymentFrequencyColumns: readonly ColumnMeta[] = [
    {
        name: 'code',
        headerKey: 'payment_frequency.colCode',
        style: 'text_left',
        hidden: false,
    },
    {
        name: 'name',
        headerKey: 'payment_frequency.colName',
        style: 'text_left',
        hidden: false,
    },
    {
        name: 'description',
        headerKey: 'payment_frequency.colDescription',
        style: 'text_left',
        hidden: true,
    },
    {
        name: 'period_unit',
        headerKey: 'payment_frequency.colPeriodUnit',
        style: 'badge_centered',
        hidden: false,
        width: 100,
        codeDomain: 'tenor_unit',
    },
    {
        name: 'period_multiplier',
        headerKey: 'payment_frequency.colPeriodMultiplier',
        style: 'mono_center',
        hidden: false,
        width: 100,
    },
    {
        name: 'display_order',
        headerKey: 'payment_frequency.colDisplayOrder',
        style: 'mono_center',
        hidden: false,
        width: 70,
    },
    {
        name: 'version',
        headerKey: 'payment_frequency.colVersion',
        style: 'mono_center',
        hidden: true,
        width: 70,
    },
    {
        name: 'modified_by',
        headerKey: 'payment_frequency.colModifiedBy',
        style: 'text_left',
        hidden: true,
    },
    {
        name: 'recorded_at',
        headerKey: 'payment_frequency.colRecordedAt',
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
export const paymentFrequencyMeta = {
    entity: 'payment_frequency',
    collection: 'payment_frequencies',
    displayField: 'name',
    keyField: 'code',
    columns: paymentFrequencyColumns,
    fields: paymentFrequencyFields,
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
export const paymentFrequencyMessages = {
        payment_frequency: {
            title: 'Payment Frequencies',
            singular: 'payment frequency',
            newTitle: 'New payment frequency',
            description: 'Reference data table for how often a leg\'s cashflows occur, sourced from ORE\'s authoritative frequencyType enumeration (external/ore/xsd/ore_types.xsd) -- Once, Annual, Semiannual, Quarterly, Bimonthly, Monthly, Lunarmonth, Weekly, Daily (the canonical long-form names; ORE also accepts single-letter aliases -- Z/A/S/Q/B/M/L/W/D -- not modelled here since this table\'s code is the long form other components store and display). Each row carries a period_unit/period_multiplier pair reusing [[id:01E76440-B9A5-4D0A-A32C-B0C4A7484B26][tenor_unit]]\'s own vocabulary (DAY/WEEK/MONTH/YEAR, plus the NONE sentinel for Once, which has no periodic step at all -- a single payment at termination), so a caller building a payment schedule (e.g. the IR Curve Template\'s swap fixed leg) can reuse the exact same period-stepping arithmetic ores::refdata::domain::resolve_end_date() already implements for tenors, rather than re-deriving month/day counts from the code string. Not scoped to any single consumer -- this table exists as reusable reference data, the same category as day_count_fraction_type or business_day_convention_type. Managed by the system tenant. This table replaces ores.trading\'s older payment_frequency_type entity outright -- there is no case for two types modelling the same ORE enumeration ("payment frequency conventions" vs "payment frequencies" is a flimsy distinction); every ores.trading column that stored a payment-frequency code (swap_leg, credit_instrument, commodity_instrument, equity_swap_instrument) now validates against this table instead. See the parent story\'s * Decisions for the full reasoning.',
            fldCode: 'Code',
            codePh: 'Enter payment frequency code',
            fldName: 'Name',
            namePh: 'Enter name',
            fldDescription: 'Description',
            descriptionPh: 'Enter a description',
            fldPeriodUnit: 'Period Unit',
            fldPeriodMultiplier: 'Period Multiplier',
            fldDisplayOrder: 'Display Order',
            colCode: 'Code',
            colName: 'Name',
            colDescription: 'Description',
            colPeriodUnit: 'Period Unit',
            colPeriodMultiplier: 'Period Multiplier',
            colDisplayOrder: 'Display Order',
            colVersion: 'Version',
            colModifiedBy: 'Modified By',
            colRecordedAt: 'Recorded At',
        }
};
