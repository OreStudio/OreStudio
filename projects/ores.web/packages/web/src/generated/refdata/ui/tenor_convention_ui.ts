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
 * The fields of a tenor_convention, in the order the model declares them.
 *
 * `code` is the natural key, so it is editable when creating
 * and read-only afterwards.
 */
export const tenorConventionFields: readonly FieldMeta[] = [
    {
        name: 'code',
        labelKey: 'tenor_convention.fldCode',
        control: 'line_edit',
        required: true,
        isKey: true,
        readOnlyAfterCreate: true,
        nullable: false,
        placeholderKey: 'tenor_convention.codePh',
    },
    {
        name: 'description',
        labelKey: 'tenor_convention.fldDescription',
        control: 'text_edit',
        required: false,
        isKey: false,
        nullable: true,
        placeholderKey: 'tenor_convention.descriptionPh',
    },
    {
        name: 'measured_from',
        labelKey: 'tenor_convention.fldMeasuredFrom',
        control: 'dynamic_combo',
        required: true,
        isKey: false,
        nullable: false,
        lookup: { collection: 'anchors', valueField: 'code', labelField: 'description' },
    },
    {
        name: 'resolution_algorithm',
        labelKey: 'tenor_convention.fldResolutionAlgorithm',
        control: 'dynamic_combo',
        required: true,
        isKey: false,
        nullable: false,
        lookup: { collection: 'algorithms', valueField: 'code', labelField: 'description' },
    }
];

/**
 * The table columns, in display order. A column hidden by default is
 * offered through the column menu.
 */
export const tenorConventionColumns: readonly ColumnMeta[] = [
    {
        name: 'code',
        headerKey: 'tenor_convention.colCode',
        style: 'text_left',
        hidden: false,
        width: 200,
    },
    {
        name: 'description',
        headerKey: 'tenor_convention.colDescription',
        style: 'text_left',
        hidden: true,
        width: 300,
    },
    {
        name: 'measured_from',
        headerKey: 'tenor_convention.colMeasuredFrom',
        style: 'text_left',
        hidden: false,
        width: 130,
    },
    {
        name: 'resolution_algorithm',
        headerKey: 'tenor_convention.colAlgorithm',
        style: 'text_left',
        hidden: false,
        width: 150,
    },
    {
        name: 'version',
        headerKey: 'tenor_convention.colVersion',
        style: 'mono_center',
        hidden: true,
        width: 80,
    },
    {
        name: 'modified_by',
        headerKey: 'tenor_convention.colModifiedBy',
        style: 'text_left',
        hidden: true,
        width: 120,
    },
    {
        name: 'recorded_at',
        headerKey: 'tenor_convention.colRecordedAt',
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
export const tenorConventionMeta = {
    entity: 'tenor_convention',
    collection: 'conventions',
    displayField: 'code',
    keyField: 'code',
    columns: tenorConventionColumns,
    fields: tenorConventionFields,
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
export const tenorConventionMessages = {
        tenor_convention: {
            title: 'Tenor Conventions',
            singular: 'tenor convention',
            newTitle: 'New tenor convention',
            description: 'Persisted catalog of the resolution schemes described in [[id:0AC88EB3-DB7F-4135-9DA6-0ED4583FEC29][Tenor]]\'s "Tenor conventions by curve type" section — spot/forward curves, FX swap curves (near-leg quoting), and credit/CDS curves (IMM-anchored). Each row names the *default* [[id:3F8B6C2A-1D4E-4A7F-9B3C-6E2D8F1A5C90][tenor anchor]] a convention\'s regular PERIOD tenors resolve from, and which *algorithm* governs resolution for the convention at all: ANCHOR_OFFSET (anchor date plus a fixed offset — spot/forward and FX swap conventions) or SCHEDULE_STEP (anchor plus a calendar offset, then n steps along a named [[id:CD180696-6558-469E-8FE5-66BFBB6E3E00][tenor_schedule]] axis — credit/CDS and FOMC conventions, story Decision D2). Which [[id:0AC88EB3-DB7F-4135-9DA6-0ED4583FEC29][tenor]] labels actually belong to a given convention, and any per-tenor anchor or offset override (needed for SPECIAL tenors such as O/N, which resolve differently under the spot/forward convention than under the swap convention, and for every SCHEDULE_STEP tenor, whose resolution row names the schedule axis and step count), is recorded in [[id:E1F5A9C3-6D2B-4E8A-B7F1-3C9D5A2E6B48][Tenor Convention Resolution]], not here.',
            fldCode: 'Code',
            codePh: 'Enter convention code',
            fldDescription: 'Description',
            descriptionPh: 'Enter a description',
            fldMeasuredFrom: 'Measured From',
            fldResolutionAlgorithm: 'Resolution Algorithm',
            colCode: 'Code',
            colDescription: 'Description',
            colMeasuredFrom: 'Measured From',
            colAlgorithm: 'Resolution Algorithm',
            colVersion: 'Version',
            colModifiedBy: 'Modified By',
            colRecordedAt: 'Recorded At',
        }
};
