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
 * The fields of a country, in the order the model declares them.
 *
 * `alpha2_code` is the natural key, so it is editable when creating
 * and read-only afterwards.
 */
export const countryFields: readonly FieldMeta[] = [
    {
        name: 'alpha2_code',
        labelKey: 'country.fldAlpha2Code',
        control: 'line_edit',
        required: true,
        isKey: true,
        readOnlyAfterCreate: true,
        nullable: false,
        placeholderKey: 'country.alpha2CodePh',
    },
    {
        name: 'alpha3_code',
        labelKey: 'country.fldAlpha3Code',
        control: 'line_edit',
        required: true,
        isKey: false,
        nullable: false,
        placeholderKey: 'country.alpha3CodePh',
    },
    {
        name: 'numeric_code',
        labelKey: 'country.fldNumericCode',
        control: 'line_edit',
        required: true,
        isKey: false,
        nullable: false,
        placeholderKey: 'country.numericCodePh',
    },
    {
        name: 'name',
        labelKey: 'country.fldName',
        control: 'line_edit',
        required: true,
        isKey: false,
        nullable: false,
        placeholderKey: 'country.namePh',
    },
    {
        name: 'official_name',
        labelKey: 'country.fldOfficialName',
        control: 'line_edit',
        required: true,
        isKey: false,
        nullable: false,
        placeholderKey: 'country.officialNamePh',
    }
];

/**
 * The table columns, in display order. A column hidden by default is
 * offered through the column menu.
 */
export const countryColumns: readonly ColumnMeta[] = [
    {
        name: 'alpha2_code',
        headerKey: 'country.colAlpha2Code',
        style: 'icon_text_left',
        hidden: false,
        width: 80,
        flag: true,
    },
    {
        name: 'alpha3_code',
        headerKey: 'country.colAlpha3Code',
        style: 'text_left',
        hidden: false,
        width: 80,
    },
    {
        name: 'numeric_code',
        headerKey: 'country.colNumericCode',
        style: 'text_left',
        hidden: false,
        width: 80,
    },
    {
        name: 'name',
        headerKey: 'country.colName',
        style: 'text_left',
        hidden: false,
        width: 200,
    },
    {
        name: 'official_name',
        headerKey: 'country.colOfficialName',
        style: 'text_left',
        hidden: false,
        width: 200,
    },
    {
        name: 'version',
        headerKey: 'country.colVersion',
        style: 'mono_center',
        hidden: true,
        width: 80,
    },
    {
        name: 'modified_by',
        headerKey: 'country.colModifiedBy',
        style: 'text_left',
        hidden: true,
        width: 120,
    },
    {
        name: 'recorded_at',
        headerKey: 'country.colRecordedAt',
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
 *
 * The image member is present because the model declares a flag, and the
 * shared screen renders its picker from this member rather than from a
 * field, because no form control edits an image.
 */
export const countryMeta = {
    entity: 'country',
    collection: 'countries',
    displayField: 'name',
    keyField: 'alpha2_code',
    columns: countryColumns,
    fields: countryFields,
    image: { field: 'image_id', kind: 'flag' },
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
export const countryMessages = {
        country: {
            title: 'Countries',
            singular: 'country',
            newTitle: 'New country',
            description: 'ISO 3166-1 country definitions used for reference data. Countries use alpha-2, alpha-3, and numeric codes per the ISO standard.',
            fldAlpha2Code: 'Alpha-2 Code',
            alpha2CodePh: 'Enter country alpha2 code',
            fldAlpha3Code: 'Alpha-3 Code',
            alpha3CodePh: 'Enter country alpha3 code',
            fldNumericCode: 'Numeric Code',
            numericCodePh: 'Enter ISO numeric code',
            fldName: 'Name',
            namePh: 'Enter display name',
            fldOfficialName: 'Official Name',
            officialNamePh: 'Enter official country name',
            colAlpha2Code: 'Alpha-2 Code',
            colAlpha3Code: 'Alpha-3 Code',
            colNumericCode: 'Numeric Code',
            colName: 'Name',
            colOfficialName: 'Official Name',
            colVersion: 'Version',
            colModifiedBy: 'Modified By',
            colRecordedAt: 'Recorded At',
        }
};
