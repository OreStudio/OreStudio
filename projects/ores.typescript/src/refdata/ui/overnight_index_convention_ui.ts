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
 * judgement the model does not carry. See overnightIndexConvention_field_groups.ts
 * beside this file.
 */
import type { ColumnMeta, FieldMeta } from '../../ui-contract.js';

/**
 * The fields of a overnight_index_convention, in the order the model declares them.
 */
export const overnightIndexConventionFields: readonly FieldMeta[] = [
    {
        name: 'fixing_calendar',
        labelKey: 'overnight_index_convention.fldFixingCalendar',
        control: 'line_edit',
        required: true,
        isKey: false,
        nullable: false,
        placeholderKey: 'overnight_index_convention.fixingCalendarPh',
    },
    {
        name: 'day_count_fraction',
        labelKey: 'overnight_index_convention.fldDayCountFraction',
        control: 'line_edit',
        required: true,
        isKey: false,
        nullable: false,
        placeholderKey: 'overnight_index_convention.dayCountFractionPh',
    },
    {
        name: 'settlement_days',
        labelKey: 'overnight_index_convention.fldSettlementDays',
        control: 'spin_box',
        required: false,
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
export const overnightIndexConventionColumns: readonly ColumnMeta[] = [
    {
        name: 'id',
        headerKey: 'overnight_index_convention.colId',
        style: 'text_left',
        hidden: false,
        width: 180,
    },
    {
        name: 'fixing_calendar',
        headerKey: 'overnight_index_convention.colFixingCalendar',
        style: 'text_left',
        hidden: false,
        width: 130,
    },
    {
        name: 'day_count_fraction',
        headerKey: 'overnight_index_convention.colDayCountFraction',
        style: 'text_left',
        hidden: false,
        width: 100,
    },
    {
        name: 'settlement_days',
        headerKey: 'overnight_index_convention.colSettlementDays',
        style: 'mono_center',
        hidden: false,
        width: 110,
    },
    {
        name: 'version',
        headerKey: 'overnight_index_convention.colVersion',
        style: 'mono_center',
        hidden: true,
        width: 80,
    },
    {
        name: 'modified_by',
        headerKey: 'overnight_index_convention.colModifiedBy',
        style: 'text_left',
        hidden: true,
        width: 120,
    },
    {
        name: 'recorded_at',
        headerKey: 'overnight_index_convention.colRecordedAt',
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
export const overnightIndexConventionMeta = {
    entity: 'overnight_index_convention',
    collection: 'overnight_index_conventions',
    displayField: 'id',
    keyField: 'id',
    columns: overnightIndexConventionColumns,
    fields: overnightIndexConventionFields,
} as const;
