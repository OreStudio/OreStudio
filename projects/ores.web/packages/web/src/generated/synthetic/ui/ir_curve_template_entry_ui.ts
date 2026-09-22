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
 * The fields of a ir_curve_template_entry, in the order the model declares them.
 */
export const irCurveTemplateEntryFields: readonly FieldMeta[] = [
    {
        name: 'sequence_index',
        labelKey: 'ir_curve_template_entry.fldSequenceIndex',
        control: 'spin_box',
        required: true,
        isKey: false,
        nullable: false,
        placeholderKey: 'ir_curve_template_entry.sequenceIndexPh',
        min: 0,
        max: 9999,
    },
    {
        name: 'start_tenor_code',
        labelKey: 'ir_curve_template_entry.fldStartTenorCode',
        control: 'line_edit',
        required: true,
        isKey: false,
        nullable: false,
        placeholderKey: 'ir_curve_template_entry.startTenorCodePh',
    },
    {
        name: 'end_tenor_code',
        labelKey: 'ir_curve_template_entry.fldEndTenorCode',
        control: 'line_edit',
        required: true,
        isKey: false,
        nullable: false,
        placeholderKey: 'ir_curve_template_entry.endTenorCodePh',
    },
    {
        name: 'instrument_code',
        labelKey: 'ir_curve_template_entry.fldInstrumentCode',
        control: 'line_edit',
        required: true,
        isKey: false,
        nullable: false,
        placeholderKey: 'ir_curve_template_entry.instrumentCodePh',
    }
];

/**
 * The table columns, in display order. A column hidden by default is
 * offered through the column menu.
 */
export const irCurveTemplateEntryColumns: readonly ColumnMeta[] = [
    {
        name: 'sequence_index',
        headerKey: 'ir_curve_template_entry.colSequenceIndex',
        style: 'mono_center',
        hidden: false,
        width: 120,
    },
    {
        name: 'start_tenor_code',
        headerKey: 'ir_curve_template_entry.colStartTenorCode',
        style: 'text_left',
        hidden: false,
        width: 100,
    },
    {
        name: 'end_tenor_code',
        headerKey: 'ir_curve_template_entry.colEndTenorCode',
        style: 'text_left',
        hidden: false,
        width: 100,
    },
    {
        name: 'instrument_code',
        headerKey: 'ir_curve_template_entry.colInstrumentCode',
        style: 'text_left',
        hidden: false,
        width: 150,
    },
    {
        name: 'version',
        headerKey: 'ir_curve_template_entry.colVersion',
        style: 'mono_center',
        hidden: true,
        width: 80,
    },
    {
        name: 'modified_by',
        headerKey: 'ir_curve_template_entry.colModifiedBy',
        style: 'text_left',
        hidden: true,
        width: 120,
    },
    {
        name: 'recorded_at',
        headerKey: 'ir_curve_template_entry.colRecordedAt',
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
export const irCurveTemplateEntryMeta = {
    entity: 'ir_curve_template_entry',
    collection: 'ir_curve_template_entries',
    displayField: '',
    keyField: 'id',
    columns: irCurveTemplateEntryColumns,
    fields: irCurveTemplateEntryFields,
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
export const irCurveTemplateEntryMessages = {
        ir_curve_template_entry: {
            title: 'IR Curve Template Entries',
            singular: 'ir curve template entry',
            newTitle: 'New ir curve template entry',
            description: 'One row of the raw instrument grid (the "Curve Template") an ir_curve_generation_config publishes: which tenor period, priced as which instrument type (deposit-equivalent, FRA-equivalent, swap-equivalent -- see ores.refdata.instrument_code), in what order. Every entry is modelled as a genuine [start, end) period rather than a single maturity label: start_tenor_code/end_tenor_code are both ordinary tenor references resolved through the same ores::refdata::domain::resolve_window/resolve_end_date machinery (see ores.refdata.api/domain/tenor_resolution.hpp). Point instruments (deposits, swaps) set start_tenor_code to \'SPOT\' (a genuine zero-duration PERIOD/DAY tenor already in the catalog, resolving directly to the horizon\'s spot date -- not a null/sentinel hack); interval instruments (FRAs) set it to the period\'s own front tenor (e.g. \'3M\' for a 3x6 FRA whose end_tenor_code is \'6M\'). This symmetric shape is what lets the tenor-collision validator (validate_curve_template in ores.synthetic.api) detect genuine period overlaps via plain windows_overlap(), without a special case for point vs. interval instruments. Entries belong to a parent config via ir_curve_config_id, the same one-config-many-children shape fx_spot_generation_config\'s gmm_component rows use. Every entry\'s published rate is derived from the parent config\'s short-rate process\'s discount_factor() at the tenor\'s maturity -- never an independently-noised value -- so the published tick batch is, by construction, a slice of one internally consistent curve. Party- and tenant-scoped.',
            fldSequenceIndex: 'Sequence Index',
            sequenceIndexPh: 'Enter sequence index',
            fldStartTenorCode: 'Start Tenor',
            startTenorCodePh: 'Enter start tenor code (e.g. SPOT)',
            fldEndTenorCode: 'End Tenor',
            endTenorCodePh: 'Enter end tenor code (e.g. 1Y)',
            fldInstrumentCode: 'Instrument',
            instrumentCodePh: 'Enter instrument code (e.g. Swap)',
            colSequenceIndex: 'Sequence Index',
            colStartTenorCode: 'Start Tenor',
            colEndTenorCode: 'End Tenor',
            colInstrumentCode: 'Instrument',
            colVersion: 'Version',
            colModifiedBy: 'Modified By',
            colRecordedAt: 'Recorded At',
        }
};
