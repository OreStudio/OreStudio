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
 * The fields of a ibor_index_convention, in the order the model declares them.
 */
export const iborIndexConventionFields: readonly FieldMeta[] = [
    {
        name: 'fixing_calendar',
        labelKey: 'ibor_index_convention.fldFixingCalendar',
        control: 'line_edit',
        required: true,
        isKey: false,
        nullable: false,
        placeholderKey: 'ibor_index_convention.fixingCalendarPh',
    },
    {
        name: 'day_count_fraction',
        labelKey: 'ibor_index_convention.fldDayCountFraction',
        control: 'line_edit',
        required: true,
        isKey: false,
        nullable: false,
        placeholderKey: 'ibor_index_convention.dayCountFractionPh',
    },
    {
        name: 'settlement_days',
        labelKey: 'ibor_index_convention.fldSettlementDays',
        control: 'spin_box',
        required: false,
        isKey: false,
        nullable: false,
        min: 0,
        max: 9999,
    },
    {
        name: 'business_day_convention',
        labelKey: 'ibor_index_convention.fldBusinessDayConvention',
        control: 'line_edit',
        required: true,
        isKey: false,
        nullable: false,
        placeholderKey: 'ibor_index_convention.businessDayConventionPh',
    },
    {
        name: 'end_of_month',
        labelKey: 'ibor_index_convention.fldEndOfMonth',
        control: 'check_box',
        required: false,
        isKey: false,
        nullable: false,
    }
];

/**
 * The table columns, in display order. A column hidden by default is
 * offered through the column menu.
 */
export const iborIndexConventionColumns: readonly ColumnMeta[] = [
    {
        name: 'id',
        headerKey: 'ibor_index_convention.colId',
        style: 'text_left',
        hidden: false,
        width: 180,
    },
    {
        name: 'fixing_calendar',
        headerKey: 'ibor_index_convention.colFixingCalendar',
        style: 'text_left',
        hidden: false,
        width: 120,
    },
    {
        name: 'day_count_fraction',
        headerKey: 'ibor_index_convention.colDayCountFraction',
        style: 'text_left',
        hidden: false,
        width: 100,
    },
    {
        name: 'settlement_days',
        headerKey: 'ibor_index_convention.colSettlementDays',
        style: 'mono_center',
        hidden: false,
        width: 110,
    },
    {
        name: 'version',
        headerKey: 'ibor_index_convention.colVersion',
        style: 'mono_center',
        hidden: true,
        width: 80,
    },
    {
        name: 'modified_by',
        headerKey: 'ibor_index_convention.colModifiedBy',
        style: 'text_left',
        hidden: true,
        width: 120,
    },
    {
        name: 'recorded_at',
        headerKey: 'ibor_index_convention.colRecordedAt',
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
export const iborIndexConventionMeta = {
    entity: 'ibor_index_convention',
    collection: 'ibor_index_conventions',
    displayField: 'id',
    keyField: 'id',
    columns: iborIndexConventionColumns,
    fields: iborIndexConventionFields,
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
export const iborIndexConventionMessages = {
        ibor_index_convention: {
            title: 'IBOR Index Conventions',
            singular: 'ibor index convention',
            newTitle: 'New ibor index convention',
            description: 'Defines the fixing calendar, day count, settlement lag, and business day convention for a term IBOR index such as EURIBOR or USD LIBOR. Corresponds to the <IborIndex> element in ORE conventions.xml.',
            fldFixingCalendar: 'Fixing Calendar',
            fixingCalendarPh: 'e.g. TARGET',
            fldDayCountFraction: 'Day Count Fraction',
            dayCountFractionPh: 'e.g. ACT/360',
            fldSettlementDays: 'Settlement Days',
            fldBusinessDayConvention: 'Business Day Convention',
            businessDayConventionPh: 'e.g. ModifiedFollowing',
            fldEndOfMonth: 'End Of Month',
            colId: 'Id',
            colFixingCalendar: 'Fixing Calendar',
            colDayCountFraction: 'DCF',
            colSettlementDays: 'Settlement Days',
            colVersion: 'Version',
            colModifiedBy: 'Modified By',
            colRecordedAt: 'Recorded At',
        }
};
