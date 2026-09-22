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
 * The fields of a tenor_schedule, in the order the model declares them.
 *
 * `code` is the natural key, so it is editable when creating
 * and read-only afterwards.
 */
export const tenorScheduleFields: readonly FieldMeta[] = [
    {
        name: 'code',
        labelKey: 'tenor_schedule.fldCode',
        control: 'line_edit',
        required: true,
        isKey: true,
        readOnlyAfterCreate: true,
        nullable: false,
        placeholderKey: 'tenor_schedule.codePh',
    },
    {
        name: 'name',
        labelKey: 'tenor_schedule.fldName',
        control: 'line_edit',
        required: true,
        isKey: false,
        nullable: false,
        placeholderKey: 'tenor_schedule.namePh',
    },
    {
        name: 'description',
        labelKey: 'tenor_schedule.fldDescription',
        control: 'text_edit',
        required: true,
        isKey: false,
        nullable: false,
        placeholderKey: 'tenor_schedule.descriptionPh',
    },
    {
        name: 'schedule_source',
        labelKey: 'tenor_schedule.fldScheduleSource',
        control: 'line_edit',
        required: true,
        isKey: false,
        nullable: false,
        placeholderKey: 'tenor_schedule.scheduleSourcePh',
    },
    {
        name: 'calendar_code',
        labelKey: 'tenor_schedule.fldCalendarCode',
        control: 'line_edit',
        required: false,
        isKey: false,
        nullable: true,
        placeholderKey: 'tenor_schedule.calendarCodePh',
    },
    {
        name: 'diary_entry_type',
        labelKey: 'tenor_schedule.fldDiaryEntryType',
        control: 'line_edit',
        required: false,
        isKey: false,
        nullable: true,
        placeholderKey: 'tenor_schedule.diaryEntryTypePh',
    },
    {
        name: 'display_order',
        labelKey: 'tenor_schedule.fldDisplayOrder',
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
export const tenorScheduleColumns: readonly ColumnMeta[] = [
    {
        name: 'code',
        headerKey: 'tenor_schedule.colCode',
        style: 'text_left',
        hidden: false,
    },
    {
        name: 'name',
        headerKey: 'tenor_schedule.colName',
        style: 'text_left',
        hidden: false,
    },
    {
        name: 'description',
        headerKey: 'tenor_schedule.colDescription',
        style: 'text_left',
        hidden: true,
    },
    {
        name: 'schedule_source',
        headerKey: 'tenor_schedule.colScheduleSource',
        style: 'text_left',
        hidden: false,
    },
    {
        name: 'calendar_code',
        headerKey: 'tenor_schedule.colCalendarCode',
        style: 'text_left',
        hidden: false,
        width: 100,
    },
    {
        name: 'diary_entry_type',
        headerKey: 'tenor_schedule.colDiaryEntryType',
        style: 'text_left',
        hidden: false,
        width: 100,
    },
    {
        name: 'display_order',
        headerKey: 'tenor_schedule.colDisplayOrder',
        style: 'mono_center',
        hidden: false,
        width: 70,
    },
    {
        name: 'version',
        headerKey: 'tenor_schedule.colVersion',
        style: 'mono_center',
        hidden: true,
        width: 70,
    },
    {
        name: 'modified_by',
        headerKey: 'tenor_schedule.colModifiedBy',
        style: 'text_left',
        hidden: true,
    },
    {
        name: 'recorded_at',
        headerKey: 'tenor_schedule.colRecordedAt',
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
export const tenorScheduleMeta = {
    entity: 'tenor_schedule',
    collection: 'schedules',
    displayField: 'name',
    keyField: 'code',
    columns: tenorScheduleColumns,
    fields: tenorScheduleFields,
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
export const tenorScheduleMessages = {
        tenor_schedule: {
            title: 'Tenor Schedules',
            singular: 'tenor schedule',
            newTitle: 'New tenor schedule',
            description: 'A named schedule axis a [[id:9A2E4D6B-7C1F-4B8A-A5D3-2F6E9B1C4A87][tenor]] resolves along (story Decision D2: anchor + calendar offset + n steps). Two kinds today, distinguished by schedule_source: - CLOSED_FORM: the dates come from a closed-form rule evaluated code-side. ROLL_QUARTER is the only instance: the first business day after the 20th of March/June/September/December (the IMM quarterly rule). - EVENT_LOOKUP: the dates come from [[id:B20050A5-1245-4944-A328-2A0893C92AEC][calendar_event]] rows on a named calendar, filtered by diary entry type. FOMC_MEETING is the only instance: central_bank_meeting events on US.FOMC. calendar_code and diary_entry_type are null for closed-form schedules (no event store involved) and required for event-lookup ones -- but the binding is documented, not enforced in the schema.',
            fldCode: 'Code',
            codePh: 'Enter tenor schedule code',
            fldName: 'Name',
            namePh: 'Enter name',
            fldDescription: 'Description',
            descriptionPh: 'Enter a description',
            fldScheduleSource: 'Schedule Source',
            scheduleSourcePh: 'CLOSED_FORM or EVENT_LOOKUP',
            fldCalendarCode: 'Calendar',
            calendarCodePh: 'Event-lookup calendar',
            fldDiaryEntryType: 'Diary Entry Type',
            diaryEntryTypePh: 'Event-lookup diary entry type',
            fldDisplayOrder: 'Display Order',
            colCode: 'Code',
            colName: 'Name',
            colDescription: 'Description',
            colScheduleSource: 'Schedule Source',
            colCalendarCode: 'Calendar',
            colDiaryEntryType: 'Diary Entry Type',
            colDisplayOrder: 'Display Order',
            colVersion: 'Version',
            colModifiedBy: 'Modified By',
            colRecordedAt: 'Recorded At',
        }
};
