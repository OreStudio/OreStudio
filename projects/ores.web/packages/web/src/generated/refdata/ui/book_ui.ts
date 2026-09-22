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
 * The fields of a book, in the order the model declares them.
 *
 * `name` is the natural key, so it is editable when creating
 * and read-only afterwards.
 */
export const bookFields: readonly FieldMeta[] = [
    {
        name: 'name',
        labelKey: 'book.fldName',
        control: 'line_edit',
        required: true,
        isKey: false,
        nullable: false,
        placeholderKey: 'book.namePh',
    },
    {
        name: 'functional_currency',
        labelKey: 'book.fldFunctionalCurrency',
        control: 'flagged_combo',
        required: false,
        isKey: false,
        nullable: false,
    },
    {
        name: 'gl_account_ref',
        labelKey: 'book.fldGlAccountRef',
        control: 'line_edit',
        required: false,
        isKey: false,
        nullable: true,
        placeholderKey: 'book.glAccountRefPh',
    },
    {
        name: 'cost_center',
        labelKey: 'book.fldCostCenter',
        control: 'line_edit',
        required: false,
        isKey: false,
        nullable: true,
        placeholderKey: 'book.costCenterPh',
    },
    {
        name: 'book_status',
        labelKey: 'book.fldBookStatus',
        control: 'dynamic_combo',
        required: false,
        isKey: false,
        nullable: false,
        lookup: { collection: 'statuses', valueField: 'code', labelField: 'description' },
        codeDomain: 'book_status',
    },
    {
        name: 'regulatory_book_type',
        labelKey: 'book.fldRegulatoryBookType',
        control: 'dynamic_combo',
        required: false,
        isKey: false,
        nullable: false,
        lookup: { collection: 'types', valueField: 'code', labelField: 'description' },
        codeDomain: 'regulatory_book_type',
    },
    {
        name: 'is_sweepable',
        labelKey: 'book.fldIsSweepable',
        control: 'check_box',
        required: false,
        isKey: false,
        nullable: false,
    },
    {
        name: 'rates_centre_code',
        labelKey: 'book.fldRatesCentreCode',
        control: 'flagged_combo',
        required: false,
        isKey: false,
        nullable: false,
    }
];

/**
 * The table columns, in display order. A column hidden by default is
 * offered through the column menu.
 */
export const bookColumns: readonly ColumnMeta[] = [
    {
        name: 'name',
        headerKey: 'book.colName',
        style: 'text_left',
        hidden: false,
        width: 200,
    },
    {
        name: 'functional_currency',
        headerKey: 'book.colFunctionalCurrency',
        style: 'icon_text_left',
        hidden: false,
        width: 130,
    },
    {
        name: 'book_status',
        headerKey: 'book.colBookStatus',
        style: 'badge_centered',
        hidden: false,
        width: 100,
        codeDomain: 'book_status',
    },
    {
        name: 'cost_center',
        headerKey: 'book.colCostCenter',
        style: 'text_left',
        hidden: false,
        width: 120,
    },
    {
        name: 'regulatory_book_type',
        headerKey: 'book.colRegulatoryBookType',
        style: 'badge_centered',
        hidden: false,
        width: 130,
        codeDomain: 'regulatory_book_type',
    },
    {
        name: 'is_sweepable',
        headerKey: 'book.colIsSweepable',
        style: 'badge_centered',
        hidden: false,
        width: 90,
        codeDomain: 'is_sweepable',
    },
    {
        name: 'rates_centre_code',
        headerKey: 'book.colRatesCentreCode',
        style: 'icon_text_left',
        hidden: false,
        width: 110,
    },
    {
        name: 'version',
        headerKey: 'book.colVersion',
        style: 'mono_center',
        hidden: true,
        width: 80,
    },
    {
        name: 'modified_by',
        headerKey: 'book.colModifiedBy',
        style: 'text_left',
        hidden: true,
        width: 120,
    },
    {
        name: 'recorded_at',
        headerKey: 'book.colRecordedAt',
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
export const bookMeta = {
    entity: 'book',
    collection: 'books',
    displayField: 'name',
    keyField: 'name',
    columns: bookColumns,
    fields: bookFields,
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
export const bookMessages = {
        book: {
            title: 'Books',
            singular: 'book',
            newTitle: 'New book',
            description: 'Operational ledger leaves. The only entity that holds trades. Serves as the basis for accounting, ownership, and regulatory capital treatment. Must belong to exactly one portfolio.',
            fldName: 'Name',
            namePh: 'Enter book name',
            fldFunctionalCurrency: 'Functional Currency',
            fldGlAccountRef: 'GL Account Ref',
            glAccountRefPh: 'Enter GL account reference',
            fldCostCenter: 'Cost Center',
            costCenterPh: 'Enter cost center',
            fldBookStatus: 'Status',
            fldRegulatoryBookType: 'Regulatory Book Type',
            fldIsSweepable: 'Sweepable',
            fldRatesCentreCode: 'Rates Centre',
            colName: 'Name',
            colFunctionalCurrency: 'Functional Currency',
            colBookStatus: 'Status',
            colCostCenter: 'Cost Center',
            colRegulatoryBookType: 'Regulatory Book Type',
            colIsSweepable: 'Sweepable',
            colRatesCentreCode: 'Rates Centre',
            colVersion: 'Version',
            colModifiedBy: 'Modified By',
            colRecordedAt: 'Recorded At',
        }
};
