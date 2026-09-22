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
 * The fields of a pricing_engine_type, in the order the model declares them.
 *
 * `code` is the natural key, so it is editable when creating
 * and read-only afterwards.
 */
export const pricingEngineTypeFields: readonly FieldMeta[] = [
    {
        name: 'code',
        labelKey: 'pricing_engine_type.fldCode',
        control: 'line_edit',
        required: true,
        isKey: true,
        readOnlyAfterCreate: true,
        nullable: false,
        placeholderKey: 'pricing_engine_type.codePh',
    },
    {
        name: 'description',
        labelKey: 'pricing_engine_type.fldDescription',
        control: 'text_edit',
        required: false,
        isKey: false,
        nullable: true,
        placeholderKey: 'pricing_engine_type.descriptionPh',
    },
    {
        name: 'instrument_type_code',
        labelKey: 'pricing_engine_type.fldInstrumentTypeCode',
        control: 'dynamic_combo',
        required: false,
        isKey: false,
        nullable: false,
        placeholderKey: 'pricing_engine_type.instrumentTypeCodePh',
        lookup: { collection: 'codes', valueField: 'code', labelField: 'code' },
    }
];

/**
 * The table columns, in display order. A column hidden by default is
 * offered through the column menu.
 */
export const pricingEngineTypeColumns: readonly ColumnMeta[] = [
    {
        name: 'code',
        headerKey: 'pricing_engine_type.colCode',
        style: 'text_left',
        hidden: false,
        width: 220,
    },
    {
        name: 'description',
        headerKey: 'pricing_engine_type.colDescription',
        style: 'text_left',
        hidden: true,
        width: 320,
    },
    {
        name: 'instrument_type_code',
        headerKey: 'pricing_engine_type.colInstrumentTypeCode',
        style: 'text_left',
        hidden: false,
        width: 150,
    },
    {
        name: 'version',
        headerKey: 'pricing_engine_type.colVersion',
        style: 'mono_center',
        hidden: true,
        width: 70,
    },
    {
        name: 'modified_by',
        headerKey: 'pricing_engine_type.colModifiedBy',
        style: 'text_left',
        hidden: true,
        width: 120,
    },
    {
        name: 'recorded_at',
        headerKey: 'pricing_engine_type.colRecordedAt',
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
export const pricingEngineTypeMeta = {
    entity: 'pricing_engine_type',
    collection: 'types',
    displayField: 'code',
    keyField: 'code',
    columns: pricingEngineTypeColumns,
    fields: pricingEngineTypeFields,
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
export const pricingEngineTypeMessages = {
        pricing_engine_type: {
            title: 'Pricing Engine Types',
            singular: 'pricing engine type',
            newTitle: 'New pricing engine type',
            description: 'Classification of products at the granularity needed by the pricing engine to select the correct model and numerical method (e.g. EuropeanSwaption, BermudanSwaption, CMS).',
            fldCode: 'Code',
            codePh: 'Enter pricing engine type code (e.g. EuropeanSwaption)',
            fldDescription: 'Description',
            descriptionPh: 'Enter a human-readable description',
            fldInstrumentTypeCode: 'Instrument Type',
            instrumentTypeCodePh: 'Select an instrument type (or leave blank for none)',
            colCode: 'Code',
            colDescription: 'Description',
            colInstrumentTypeCode: 'Instrument Type',
            colVersion: 'Version',
            colModifiedBy: 'Modified By',
            colRecordedAt: 'Recorded At',
        }
};
