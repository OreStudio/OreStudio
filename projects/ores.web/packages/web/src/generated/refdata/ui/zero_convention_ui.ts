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
 * The fields of a zero_convention, in the order the model declares them.
 */
export const zeroConventionFields: readonly FieldMeta[] = [
    {
        name: 'day_count_fraction',
        labelKey: 'zero_convention.fldDayCountFraction',
        control: 'line_edit',
        required: true,
        isKey: false,
        nullable: false,
        placeholderKey: 'zero_convention.dayCountFractionPh',
    },
    {
        name: 'compounding',
        labelKey: 'zero_convention.fldCompounding',
        control: 'line_edit',
        required: false,
        isKey: false,
        nullable: true,
        placeholderKey: 'zero_convention.compoundingPh',
    },
    {
        name: 'compounding_frequency',
        labelKey: 'zero_convention.fldCompoundingFrequency',
        control: 'line_edit',
        required: false,
        isKey: false,
        nullable: true,
        placeholderKey: 'zero_convention.compoundingFrequencyPh',
    },
    {
        name: 'tenor_calendar',
        labelKey: 'zero_convention.fldTenorCalendar',
        control: 'line_edit',
        required: false,
        isKey: false,
        nullable: true,
        placeholderKey: 'zero_convention.tenorCalendarPh',
    },
    {
        name: 'spot_calendar',
        labelKey: 'zero_convention.fldSpotCalendar',
        control: 'line_edit',
        required: false,
        isKey: false,
        nullable: true,
    },
    {
        name: 'roll_convention',
        labelKey: 'zero_convention.fldRollConvention',
        control: 'line_edit',
        required: false,
        isKey: false,
        nullable: true,
    }
];

/**
 * The table columns, in display order. A column hidden by default is
 * offered through the column menu.
 */
export const zeroConventionColumns: readonly ColumnMeta[] = [
    {
        name: 'id',
        headerKey: 'zero_convention.colId',
        style: 'text_left',
        hidden: false,
        width: 200,
    },
    {
        name: 'tenor_based',
        headerKey: 'zero_convention.colTenorBased',
        style: 'text_left',
        hidden: false,
        width: 100,
    },
    {
        name: 'day_count_fraction',
        headerKey: 'zero_convention.colDayCountFraction',
        style: 'text_left',
        hidden: false,
        width: 140,
    },
    {
        name: 'compounding',
        headerKey: 'zero_convention.colCompounding',
        style: 'text_left',
        hidden: false,
        width: 120,
    },
    {
        name: 'tenor_calendar',
        headerKey: 'zero_convention.colTenorCalendar',
        style: 'text_left',
        hidden: false,
        width: 130,
    },
    {
        name: 'version',
        headerKey: 'zero_convention.colVersion',
        style: 'mono_center',
        hidden: true,
        width: 80,
    },
    {
        name: 'modified_by',
        headerKey: 'zero_convention.colModifiedBy',
        style: 'text_left',
        hidden: true,
        width: 120,
    },
    {
        name: 'recorded_at',
        headerKey: 'zero_convention.colRecordedAt',
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
export const zeroConventionMeta = {
    entity: 'zero_convention',
    collection: 'zero_conventions',
    displayField: 'id',
    keyField: 'id',
    columns: zeroConventionColumns,
    fields: zeroConventionFields,
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
export const zeroConventionMessages = {
        zero_convention: {
            title: 'Zero Conventions',
            singular: 'zero convention',
            newTitle: 'New zero convention',
            description: 'Specifies the day count fraction and compounding method used when bootstrapping a discount or zero-rate curve in ORE. Corresponds to the <Zero> element in ORE conventions.xml. The id field is the natural key (ORE <Id> element).',
            fldDayCountFraction: 'Day Count Fraction',
            dayCountFractionPh: 'e.g. ACT/365.FIXED',
            fldCompounding: 'Compounding',
            compoundingPh: 'e.g. Continuous',
            fldCompoundingFrequency: 'Compounding Frequency',
            compoundingFrequencyPh: 'e.g. Annual',
            fldTenorCalendar: 'Tenor Calendar',
            tenorCalendarPh: 'e.g. TARGET',
            fldSpotCalendar: 'Spot Calendar',
            fldRollConvention: 'Roll Convention',
            colId: 'Id',
            colTenorBased: 'Tenor Based',
            colDayCountFraction: 'DCF',
            colCompounding: 'Compounding',
            colTenorCalendar: 'Tenor Calendar',
            colVersion: 'Version',
            colModifiedBy: 'Modified By',
            colRecordedAt: 'Recorded At',
        }
};
