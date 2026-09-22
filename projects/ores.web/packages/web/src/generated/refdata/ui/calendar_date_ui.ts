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
 * The fields of a calendar_date, in the order the model declares them.
 */
export const calendarDateFields: readonly FieldMeta[] = [];

/**
 * The table columns, in display order. A column hidden by default is
 * offered through the column menu.
 */
export const calendarDateColumns: readonly ColumnMeta[] = [
    {
        name: 'date',
        headerKey: 'calendar_date.colDate',
        style: 'text_left',
        hidden: false,
        width: 120,
    },
    {
        name: 'is_business_day',
        headerKey: 'calendar_date.colIsBusinessDay',
        style: 'text_left',
        hidden: false,
        width: 100,
    },
    {
        name: 'source',
        headerKey: 'calendar_date.colSource',
        style: 'text_left',
        hidden: false,
        width: 140,
    }
];

/**
 * The collection name the list request uses, and the display field.
 *
 * Emitted so the sidebar and the lookup selects do not have to know it
 * separately.
 */
export const calendarDateMeta = {
    entity: 'calendar_date',
    collection: 'calendar_dates',
    displayField: 'date',
    keyField: 'date',
    columns: calendarDateColumns,
    fields: calendarDateFields,
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
export const calendarDateMessages = {
        calendar_date: {
            title: 'Browse Holidays',
            singular: 'calendar date',
            newTitle: 'New calendar date',
            description: 'One row per (calendar_code, date), produced by the calendar materialisation service instantiating a [[id:C09DF2B2-0E14-4742-8BAC-5D5842069580][calendar]] template\'s [[id:875E96F6-3FC7-4E0B-8E3C-2AC0F8BD488F][calendar_rule]] / [[id:8B6E1C53-871E-4B6D-BD20-CB3F551B5C46][calendar_exception]] rows over a rolling date range, never edited by hand. Read by every consumer that needs "is date D a business day for calendar C" -- the UI (calendar detail screens, the holiday-aware date picker) and ORE Studio\'s own tenor/schedule date-math -- so the answer is consistent and never requires a live rule evaluation.',
            colDate: 'Date',
            colIsBusinessDay: 'Business Day',
            colSource: 'Source',
        }
};
