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
 * The fields of a counterparty_identifier, in the order the model declares them.
 *
 * `id_value` is the natural key, so it is editable when creating
 * and read-only afterwards.
 */
export const counterpartyIdentifierFields: readonly FieldMeta[] = [
    {
        name: 'id_scheme',
        labelKey: 'counterparty_identifier.fldIdScheme',
        control: 'dynamic_combo',
        required: true,
        isKey: false,
        nullable: false,
        lookup: { collection: 'schemes', valueField: 'code', labelField: 'description' },
    },
    {
        name: 'id_value',
        labelKey: 'counterparty_identifier.fldIdValue',
        control: 'line_edit',
        required: true,
        isKey: false,
        nullable: false,
        placeholderKey: 'counterparty_identifier.idValuePh',
    },
    {
        name: 'description',
        labelKey: 'counterparty_identifier.fldDescription',
        control: 'line_edit',
        required: false,
        isKey: false,
        nullable: true,
        placeholderKey: 'counterparty_identifier.descriptionPh',
    }
];

/**
 * The table columns, in display order. A column hidden by default is
 * offered through the column menu.
 */
export const counterpartyIdentifierColumns: readonly ColumnMeta[] = [
    {
        name: 'id_scheme',
        headerKey: 'counterparty_identifier.colIdScheme',
        style: 'text_left',
        hidden: false,
        width: 100,
    },
    {
        name: 'id_value',
        headerKey: 'counterparty_identifier.colIdValue',
        style: 'text_left',
        hidden: false,
        width: 180,
    },
    {
        name: 'description',
        headerKey: 'counterparty_identifier.colDescription',
        style: 'text_left',
        hidden: true,
        width: 200,
    },
    {
        name: 'version',
        headerKey: 'counterparty_identifier.colVersion',
        style: 'mono_center',
        hidden: true,
        width: 80,
    },
    {
        name: 'modified_by',
        headerKey: 'counterparty_identifier.colModifiedBy',
        style: 'text_left',
        hidden: true,
        width: 120,
    },
    {
        name: 'recorded_at',
        headerKey: 'counterparty_identifier.colRecordedAt',
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
export const counterpartyIdentifierMeta = {
    entity: 'counterparty_identifier',
    collection: 'counterparty_identifiers',
    displayField: 'id_value',
    keyField: 'id_value',
    columns: counterpartyIdentifierColumns,
    fields: counterpartyIdentifierFields,
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
export const counterpartyIdentifierMessages = {
        counterparty_identifier: {
            title: 'Counterparty Identifiers',
            singular: 'counterparty identifier',
            newTitle: 'New counterparty identifier',
            description: 'External identifiers for counterparties, such as LEI codes, BIC/SWIFT codes, national registration numbers, and tax identifiers. Each counterparty can have multiple identifiers across different schemes.',
            fldIdScheme: 'Scheme',
            fldIdValue: 'Value',
            idValuePh: 'Enter identifier value',
            fldDescription: 'Description',
            descriptionPh: 'Enter description',
            colIdScheme: 'Scheme',
            colIdValue: 'Value',
            colDescription: 'Description',
            colVersion: 'Version',
            colModifiedBy: 'Modified By',
            colRecordedAt: 'Recorded At',
        }
};
