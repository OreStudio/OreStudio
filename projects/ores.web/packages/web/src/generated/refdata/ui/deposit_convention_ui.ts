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
 * The fields of a deposit_convention, in the order the model declares them.
 */
export const depositConventionFields: readonly FieldMeta[] = [
    {
        name: 'index_based',
        labelKey: 'deposit_convention.fldIndexBased',
        control: 'check_box',
        required: false,
        isKey: false,
        nullable: false,
    },
    {
        name: 'index',
        labelKey: 'deposit_convention.fldIndex',
        control: 'line_edit',
        required: false,
        isKey: false,
        nullable: true,
        placeholderKey: 'deposit_convention.indexPh',
    },
    {
        name: 'calendar',
        labelKey: 'deposit_convention.fldCalendar',
        control: 'line_edit',
        required: false,
        isKey: false,
        nullable: true,
        placeholderKey: 'deposit_convention.calendarPh',
    },
    {
        name: 'convention',
        labelKey: 'deposit_convention.fldConvention',
        control: 'line_edit',
        required: false,
        isKey: false,
        nullable: true,
        placeholderKey: 'deposit_convention.conventionPh',
    },
    {
        name: 'day_count_fraction',
        labelKey: 'deposit_convention.fldDayCountFraction',
        control: 'line_edit',
        required: false,
        isKey: false,
        nullable: true,
        placeholderKey: 'deposit_convention.dayCountFractionPh',
    },
    {
        name: 'end_of_month',
        labelKey: 'deposit_convention.fldEndOfMonth',
        control: 'check_box',
        required: false,
        isKey: false,
        nullable: true,
        triState: true,
    },
    {
        name: 'settlement_days',
        labelKey: 'deposit_convention.fldSettlementDays',
        control: 'spin_box',
        required: false,
        isKey: false,
        nullable: true,
        min: -1,
        max: 9999,
    }
];

/**
 * The table columns, in display order. A column hidden by default is
 * offered through the column menu.
 */
export const depositConventionColumns: readonly ColumnMeta[] = [
    {
        name: 'id',
        headerKey: 'deposit_convention.colId',
        style: 'text_left',
        hidden: false,
        width: 200,
    },
    {
        name: 'index_based',
        headerKey: 'deposit_convention.colIndexBased',
        style: 'text_left',
        hidden: false,
        width: 100,
    },
    {
        name: 'index',
        headerKey: 'deposit_convention.colIndex',
        style: 'text_left',
        hidden: false,
        width: 140,
    },
    {
        name: 'calendar',
        headerKey: 'deposit_convention.colCalendar',
        style: 'text_left',
        hidden: false,
        width: 120,
    },
    {
        name: 'day_count_fraction',
        headerKey: 'deposit_convention.colDayCountFraction',
        style: 'text_left',
        hidden: false,
        width: 100,
    },
    {
        name: 'version',
        headerKey: 'deposit_convention.colVersion',
        style: 'mono_center',
        hidden: true,
        width: 80,
    },
    {
        name: 'modified_by',
        headerKey: 'deposit_convention.colModifiedBy',
        style: 'text_left',
        hidden: true,
        width: 120,
    },
    {
        name: 'recorded_at',
        headerKey: 'deposit_convention.colRecordedAt',
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
export const depositConventionMeta = {
    entity: 'deposit_convention',
    collection: 'deposit_conventions',
    displayField: 'id',
    keyField: 'id',
    columns: depositConventionColumns,
    fields: depositConventionFields,
} as const;
