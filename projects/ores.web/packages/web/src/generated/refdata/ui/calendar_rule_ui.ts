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
 * The fields of a calendar_rule, in the order the model declares them.
 */
export const calendarRuleFields: readonly FieldMeta[] = [
    {
        name: 'calendar_code',
        labelKey: 'calendar_rule.fldCalendarCode',
        control: 'dynamic_combo',
        required: true,
        isKey: false,
        readOnlyAfterCreate: true,
        nullable: false,
        lookup: { collection: 'calendars', valueField: 'code', labelField: 'name' },
    },
    {
        name: 'kind',
        labelKey: 'calendar_rule.fldKind',
        control: 'line_edit',
        required: true,
        isKey: false,
        nullable: false,
        placeholderKey: 'calendar_rule.kindPh',
    },
    {
        name: 'month',
        labelKey: 'calendar_rule.fldMonth',
        control: 'spin_box',
        required: false,
        isKey: false,
        nullable: true,
        min: -1,
        max: 9999,
    },
    {
        name: 'day',
        labelKey: 'calendar_rule.fldDay',
        control: 'spin_box',
        required: false,
        isKey: false,
        nullable: true,
        min: -1,
        max: 9999,
    },
    {
        name: 'weekday',
        labelKey: 'calendar_rule.fldWeekday',
        control: 'spin_box',
        required: false,
        isKey: false,
        nullable: true,
        min: -1,
        max: 9999,
    },
    {
        name: 'occurrence',
        labelKey: 'calendar_rule.fldOccurrence',
        control: 'spin_box',
        required: false,
        isKey: false,
        nullable: true,
        min: -1,
        max: 9999,
    },
    {
        name: 'day_offset',
        labelKey: 'calendar_rule.fldDayOffset',
        control: 'spin_box',
        required: false,
        isKey: false,
        nullable: true,
        min: -9999,
        max: 9999,
    },
    {
        name: 'shift',
        labelKey: 'calendar_rule.fldShift',
        control: 'line_edit',
        required: true,
        isKey: false,
        nullable: false,
        placeholderKey: 'calendar_rule.shiftPh',
    },
    {
        name: 'effective_from',
        labelKey: 'calendar_rule.fldEffectiveFrom',
        control: 'spin_box',
        required: false,
        isKey: false,
        nullable: true,
        min: -1,
        max: 9999,
    },
    {
        name: 'effective_to',
        labelKey: 'calendar_rule.fldEffectiveTo',
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
export const calendarRuleColumns: readonly ColumnMeta[] = [
    {
        name: 'calendar_code',
        headerKey: 'calendar_rule.colCalendarCode',
        style: 'text_left',
        hidden: false,
        width: 100,
    },
    {
        name: 'kind',
        headerKey: 'calendar_rule.colKind',
        style: 'text_left',
        hidden: false,
        width: 140,
    },
    {
        name: 'month',
        headerKey: 'calendar_rule.colMonth',
        style: 'mono_center',
        hidden: true,
        width: 70,
    },
    {
        name: 'day',
        headerKey: 'calendar_rule.colDay',
        style: 'mono_center',
        hidden: true,
        width: 60,
    },
    {
        name: 'weekday',
        headerKey: 'calendar_rule.colWeekday',
        style: 'mono_center',
        hidden: true,
        width: 70,
    },
    {
        name: 'occurrence',
        headerKey: 'calendar_rule.colOccurrence',
        style: 'mono_center',
        hidden: true,
        width: 90,
    },
    {
        name: 'day_offset',
        headerKey: 'calendar_rule.colDayOffset',
        style: 'mono_center',
        hidden: true,
        width: 90,
    },
    {
        name: 'shift',
        headerKey: 'calendar_rule.colShift',
        style: 'text_left',
        hidden: false,
        width: 140,
    },
    {
        name: 'effective_from',
        headerKey: 'calendar_rule.colEffectiveFrom',
        style: 'mono_center',
        hidden: false,
        width: 70,
    },
    {
        name: 'effective_to',
        headerKey: 'calendar_rule.colEffectiveTo',
        style: 'mono_center',
        hidden: false,
        width: 70,
    },
    {
        name: 'version',
        headerKey: 'calendar_rule.colVersion',
        style: 'mono_center',
        hidden: true,
        width: 70,
    },
    {
        name: 'modified_by',
        headerKey: 'calendar_rule.colModifiedBy',
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
export const calendarRuleMeta = {
    entity: 'calendar_rule',
    collection: 'calendar_rules',
    displayField: '',
    keyField: 'id',
    columns: calendarRuleColumns,
    fields: calendarRuleFields,
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
export const calendarRuleMessages = {
        calendar_rule: {
            title: 'Calendar Rules',
            singular: 'calendar rule',
            newTitle: 'New calendar rule',
            description: 'One row per indefinite, timeless recurring holiday rule for a base-less [[id:C09DF2B2-0E14-4742-8BAC-5D5842069580][calendar]] template -- the QuantLib-transcribed calendars and any base-less user-authored template alike. Mirrors ores::analytics::quant::domain::calendar_rule plus the usual refdata plumbing; a calendar\'s full rule set is the batch input to that pure engine at materialisation time (see the parent task\'s * Revision section for the two-stage rule/materialisation design). One-off irregular dates (jubilees, special closings) are *not* modelled here -- they belong in [[id:8B6E1C53-871E-4B6D-BD20-CB3F551B5C46][calendar_exception]] instead, so a rule stays a timeless, indefinitely-repeating pattern.',
            fldCalendarCode: 'Calendar',
            fldKind: 'Kind',
            kindPh: 'fixed_date/nth_weekday_of_month/last_weekday_of_month/easter_offset',
            fldMonth: 'Month',
            fldDay: 'Day',
            fldWeekday: 'Weekday',
            fldOccurrence: 'Occurrence',
            fldDayOffset: 'Day Offset',
            fldShift: 'Shift',
            shiftPh: 'none/nearest_weekday/roll_forward_to_monday',
            fldEffectiveFrom: 'Eff. From',
            fldEffectiveTo: 'Eff. To',
            colCalendarCode: 'Calendar',
            colKind: 'Kind',
            colMonth: 'Month',
            colDay: 'Day',
            colWeekday: 'Weekday',
            colOccurrence: 'Occurrence',
            colDayOffset: 'Day Offset',
            colShift: 'Shift',
            colEffectiveFrom: 'From',
            colEffectiveTo: 'To',
            colVersion: 'Version',
            colModifiedBy: 'Modified By',
        }
};
