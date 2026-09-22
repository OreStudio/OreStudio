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
 * The fields of a calendar_exception, in the order the model declares them.
 */
export const calendarExceptionFields: readonly FieldMeta[] = [
    {
        name: 'calendar_code',
        labelKey: 'calendar_exception.fldCalendarCode',
        control: 'dynamic_combo',
        required: true,
        isKey: false,
        readOnlyAfterCreate: true,
        nullable: false,
        lookup: { collection: 'calendars', valueField: 'code', labelField: 'name' },
    },
    {
        name: 'exception_date',
        labelKey: 'calendar_exception.fldExceptionDate',
        control: 'line_edit',
        required: true,
        isKey: false,
        readOnlyAfterCreate: true,
        nullable: false,
        placeholderKey: 'calendar_exception.exceptionDatePh',
    },
    {
        name: 'is_business_day',
        labelKey: 'calendar_exception.fldIsBusinessDay',
        control: 'check_box',
        required: false,
        isKey: false,
        nullable: false,
    },
    {
        name: 'description',
        labelKey: 'calendar_exception.fldDescription',
        control: 'line_edit',
        required: false,
        isKey: false,
        nullable: true,
        placeholderKey: 'calendar_exception.descriptionPh',
    }
];

/**
 * The table columns, in display order. A column hidden by default is
 * offered through the column menu.
 */
export const calendarExceptionColumns: readonly ColumnMeta[] = [
    {
        name: 'calendar_code',
        headerKey: 'calendar_exception.colCalendarCode',
        style: 'text_left',
        hidden: false,
        width: 100,
    },
    {
        name: 'exception_date',
        headerKey: 'calendar_exception.colExceptionDate',
        style: 'text_left',
        hidden: false,
        width: 100,
    },
    {
        name: 'is_business_day',
        headerKey: 'calendar_exception.colIsBusinessDay',
        style: 'text_left',
        hidden: false,
        width: 100,
    },
    {
        name: 'description',
        headerKey: 'calendar_exception.colDescription',
        style: 'text_left',
        hidden: true,
    },
    {
        name: 'version',
        headerKey: 'calendar_exception.colVersion',
        style: 'mono_center',
        hidden: true,
        width: 70,
    },
    {
        name: 'modified_by',
        headerKey: 'calendar_exception.colModifiedBy',
        style: 'text_left',
        hidden: true,
    }
];

/**
 * The collection name the list request uses, and the display field.
 *
 * Emitted so the sidebar and the lookup selects do not have to know it
 * separately.
 */
export const calendarExceptionMeta = {
    entity: 'calendar_exception',
    collection: 'calendar_exceptions',
    displayField: '',
    keyField: 'id',
    columns: calendarExceptionColumns,
    fields: calendarExceptionFields,
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
export const calendarExceptionMessages = {
        calendar_exception: {
            title: 'Calendar Exceptions',
            singular: 'calendar exception',
            newTitle: 'New calendar exception',
            description: 'One row per one-off override for a single [[id:C09DF2B2-0E14-4742-8BAC-5D5842069580][calendar]] day. Mirrors ores::analytics::quant::domain::calendar_exception plus the usual refdata plumbing. Applies uniformly to *any* calendar -- transcribed or user-authored, based or base-less: a based calendar\'s full override story is entirely calendar.base_calendar_code (which base) plus its own exception rows (what\'s different), no separate "adjustment" concept needed. QuantLib\'s own irregular, year-gated special dates (the UK\'s Jubilee bank holidays, the NYSE\'s special closings) are transcribed here, not folded into a [[id:875E96F6-3FC7-4E0B-8E3C-2AC0F8BD488F][calendar_rule]] row, so a rule stays a timeless, indefinitely-repeating pattern and the genuinely irregular part of a calendar stays flat, no-logic-at-all data.',
            fldCalendarCode: 'Calendar',
            fldExceptionDate: 'Date',
            exceptionDatePh: 'YYYY-MM-DD',
            fldIsBusinessDay: 'Business Day',
            fldDescription: 'Description',
            descriptionPh: 'Optional note',
            colCalendarCode: 'Calendar',
            colExceptionDate: 'Date',
            colIsBusinessDay: 'Business Day',
            colDescription: 'Description',
            colVersion: 'Version',
            colModifiedBy: 'Modified By',
        }
};
