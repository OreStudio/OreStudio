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
 * The fields of a calendar_event, in the order the model declares them.
 */
export const calendarEventFields: readonly FieldMeta[] = [
    {
        name: 'calendar_code',
        labelKey: 'calendar_event.fldCalendarCode',
        control: 'dynamic_combo',
        required: true,
        isKey: false,
        readOnlyAfterCreate: true,
        nullable: false,
        lookup: { collection: 'calendars', valueField: 'code', labelField: 'name' },
    },
    {
        name: 'event_date',
        labelKey: 'calendar_event.fldEventDate',
        control: 'line_edit',
        required: true,
        isKey: false,
        readOnlyAfterCreate: true,
        nullable: false,
        placeholderKey: 'calendar_event.eventDatePh',
    },
    {
        name: 'diary_entry_type',
        labelKey: 'calendar_event.fldDiaryEntryType',
        control: 'dynamic_combo',
        required: true,
        isKey: false,
        readOnlyAfterCreate: true,
        nullable: false,
        lookup: { collection: 'entry_types', valueField: 'code', labelField: 'name' },
    },
    {
        name: 'name',
        labelKey: 'calendar_event.fldName',
        control: 'line_edit',
        required: true,
        isKey: false,
        nullable: false,
        placeholderKey: 'calendar_event.namePh',
    },
    {
        name: 'description',
        labelKey: 'calendar_event.fldDescription',
        control: 'text_edit',
        required: false,
        isKey: false,
        nullable: true,
        placeholderKey: 'calendar_event.descriptionPh',
    },
    {
        name: 'source',
        labelKey: 'calendar_event.fldSource',
        control: 'line_edit',
        required: false,
        isKey: false,
        nullable: true,
        placeholderKey: 'calendar_event.sourcePh',
    }
];

/**
 * The table columns, in display order. A column hidden by default is
 * offered through the column menu.
 */
export const calendarEventColumns: readonly ColumnMeta[] = [
    {
        name: 'calendar_code',
        headerKey: 'calendar_event.colCalendarCode',
        style: 'text_left',
        hidden: false,
        width: 100,
    },
    {
        name: 'event_date',
        headerKey: 'calendar_event.colEventDate',
        style: 'text_left',
        hidden: false,
        width: 100,
    },
    {
        name: 'diary_entry_type',
        headerKey: 'calendar_event.colDiaryEntryType',
        style: 'text_left',
        hidden: false,
        width: 140,
    },
    {
        name: 'name',
        headerKey: 'calendar_event.colName',
        style: 'text_left',
        hidden: false,
    },
    {
        name: 'description',
        headerKey: 'calendar_event.colDescription',
        style: 'text_left',
        hidden: true,
    },
    {
        name: 'source',
        headerKey: 'calendar_event.colSource',
        style: 'text_left',
        hidden: true,
        width: 120,
    },
    {
        name: 'version',
        headerKey: 'calendar_event.colVersion',
        style: 'mono_center',
        hidden: true,
        width: 70,
    },
    {
        name: 'modified_by',
        headerKey: 'calendar_event.colModifiedBy',
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
export const calendarEventMeta = {
    entity: 'calendar_event',
    collection: 'calendar_events',
    displayField: 'name',
    keyField: 'id',
    columns: calendarEventColumns,
    fields: calendarEventFields,
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
export const calendarEventMessages = {
        calendar_event: {
            title: 'Calendar Events',
            singular: 'calendar event',
            newTitle: 'New calendar event',
            description: 'One row per dated diary entry on a [[id:C09DF2B2-0E14-4742-8BAC-5D5842069580][calendar]]: a central-bank meeting, a scheduled data release, or an open-ended other event. One table for all event kinds -- never a table per calendar or per type (settled 2026-08-09, story Decision D1; see [[id:41E0E1FB-1D84-47E0-A417-2633F31F0A2A][Calendar Events]]). The diary_entry_type column classifies the entry via the open-ended [[id:AFAF296D-2962-48CE-A6E1-BFD5229E16C5][diary_entry_type]] vocabulary (holiday, central_bank_meeting, data_release, other). Holidays themselves keep their existing machinery (calendar_rules, calendar_exceptions, calendar_date) -- the holiday type stays in the vocabulary so the whole classification lives in one place, even though its physical home is elsewhere. Template/Instance: an event row is an *instance*; its template is the (calendar, diary_entry_type, name) triple. A worked case: the FOMC\'s eight regularly scheduled meetings per year are entered as a short run of central_bank_meeting instances on the US.FOMC calendar, transcribed from the Fed\'s published calendar with source=\'federalreserve.gov\'. Formulaic recurrence generation is deferred; calendar_rules\'s grammar can later feed a template link if a consumer needs it.',
            fldCalendarCode: 'Calendar',
            fldEventDate: 'Date',
            eventDatePh: 'YYYY-MM-DD',
            fldDiaryEntryType: 'Diary Entry',
            fldName: 'Name',
            namePh: 'Enter event name',
            fldDescription: 'Description',
            descriptionPh: 'Optional note',
            fldSource: 'Source',
            sourcePh: 'Provenance, e.g. federalreserve.gov',
            colCalendarCode: 'Calendar',
            colEventDate: 'Date',
            colDiaryEntryType: 'Diary Entry',
            colName: 'Name',
            colDescription: 'Description',
            colSource: 'Source',
            colVersion: 'Version',
            colModifiedBy: 'Modified By',
        }
};
