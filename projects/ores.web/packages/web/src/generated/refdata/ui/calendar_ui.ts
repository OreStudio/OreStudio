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
 * The fields of a calendar, in the order the model declares them.
 *
 * `code` is the natural key, so it is editable when creating
 * and read-only afterwards.
 */
export const calendarFields: readonly FieldMeta[] = [
    {
        name: 'code',
        labelKey: 'calendar.fldCode',
        control: 'line_edit',
        required: true,
        isKey: true,
        readOnlyAfterCreate: true,
        nullable: false,
        placeholderKey: 'calendar.codePh',
    },
    {
        name: 'name',
        labelKey: 'calendar.fldName',
        control: 'line_edit',
        required: true,
        isKey: false,
        nullable: false,
        placeholderKey: 'calendar.namePh',
    },
    {
        name: 'calendar_type',
        labelKey: 'calendar.fldCalendarType',
        control: 'dynamic_combo',
        required: false,
        isKey: false,
        nullable: false,
        lookup: { collection: 'types', valueField: 'code', labelField: 'description' },
        codeDomain: 'calendar_type',
    },
    {
        name: 'country_code',
        labelKey: 'calendar.fldCountryCode',
        control: 'flagged_combo',
        required: false,
        isKey: false,
        nullable: false,
    },
    {
        name: 'source',
        labelKey: 'calendar.fldSource',
        control: 'line_edit',
        required: true,
        isKey: false,
        readOnlyAfterCreate: true,
        nullable: false,
        placeholderKey: 'calendar.sourcePh',
    },
    {
        name: 'is_editable',
        labelKey: 'calendar.fldIsEditable',
        control: 'check_box',
        required: false,
        isKey: false,
        readOnlyAfterCreate: true,
        nullable: false,
    },
    {
        name: 'base_calendar_code',
        labelKey: 'calendar.fldBaseCalendarCode',
        control: 'dynamic_combo',
        required: false,
        isKey: false,
        nullable: true,
        lookup: { collection: 'calendars', valueField: 'code', labelField: 'code' },
    }
];

/**
 * The table columns, in display order. A column hidden by default is
 * offered through the column menu.
 */
export const calendarColumns: readonly ColumnMeta[] = [
    {
        name: 'code',
        headerKey: 'calendar.colCode',
        style: 'text_left',
        hidden: false,
    },
    {
        name: 'name',
        headerKey: 'calendar.colCalendarName',
        style: 'text_left',
        hidden: false,
    },
    {
        name: 'calendar_type',
        headerKey: 'calendar.colCalendarType',
        style: 'badge_centered',
        hidden: false,
        codeDomain: 'calendar_type',
    },
    {
        name: 'country_code',
        headerKey: 'calendar.colCountryCode',
        style: 'icon_text_left',
        hidden: false,
        width: 80,
    },
    {
        name: 'version',
        headerKey: 'calendar.colVersion',
        style: 'mono_center',
        hidden: true,
        width: 70,
    },
    {
        name: 'modified_by',
        headerKey: 'calendar.colModifiedBy',
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
export const calendarMeta = {
    entity: 'calendar',
    collection: 'calendars',
    displayField: 'name',
    keyField: 'code',
    columns: calendarColumns,
    fields: calendarFields,
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
export const calendarMessages = {
        calendar: {
            title: 'Calendars',
            singular: 'calendar',
            newTitle: 'New calendar',
            description: 'Validated enumeration of named date collections consumed by ORE and QuantLib: business-day/holiday calendars (TARGET, UnitedStates, UnitedStates.GovernmentBond, ...), central-bank meeting calendars, and other calendar-shaped reference data. Each row is one concrete QuantLib/ORE calendar token — sub-market variants (e.g. UnitedStates.NYSE vs UnitedStates.GovernmentBond) are separate rows, not a joined variant field, so the code column always matches ORE\'s XML <Calendar> vocabulary verbatim. Classified by [[id:1A454661-81B5-4F8F-93A6-06547412DD84][calendar_type]] and associated with the [[id:88E8E1FB-6F2F-495F-BEC4-8C7ABEF68563][country]] whose calendar it is — supranational calendars (TARGET) use the ZZ sentinel (ISO 3166-1\'s own reserved user-assigned code) rather than a nullable country reference, since no single country owns them.',
            fldCode: 'Code',
            codePh: 'Enter QuantLib calendar code',
            fldName: 'Name',
            namePh: 'Enter calendar name',
            fldCalendarType: 'Type',
            fldCountryCode: 'Country',
            fldSource: 'Source',
            sourcePh: 'quantlib or user',
            fldIsEditable: 'Editable',
            fldBaseCalendarCode: 'Base Calendar',
            colCode: 'Code',
            colCalendarName: 'Name',
            colCalendarType: 'Type',
            colCountryCode: 'Country',
            colVersion: 'Version',
            colModifiedBy: 'Modified By',
        }
};
