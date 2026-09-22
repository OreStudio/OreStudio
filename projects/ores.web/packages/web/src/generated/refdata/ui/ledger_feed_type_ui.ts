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
 * The fields of a ledger_feed_type, in the order the model declares them.
 *
 * `code` is the natural key, so it is editable when creating
 * and read-only afterwards.
 */
export const ledgerFeedTypeFields: readonly FieldMeta[] = [
    {
        name: 'code',
        labelKey: 'ledger_feed_type.fldCode',
        control: 'line_edit',
        required: true,
        isKey: true,
        readOnlyAfterCreate: true,
        nullable: false,
        placeholderKey: 'ledger_feed_type.codePh',
    },
    {
        name: 'name',
        labelKey: 'ledger_feed_type.fldName',
        control: 'line_edit',
        required: true,
        isKey: false,
        nullable: false,
        placeholderKey: 'ledger_feed_type.namePh',
    },
    {
        name: 'description',
        labelKey: 'ledger_feed_type.fldDescription',
        control: 'text_edit',
        required: false,
        isKey: false,
        nullable: false,
        placeholderKey: 'ledger_feed_type.descriptionPh',
    }
];

/**
 * The table columns, in display order. A column hidden by default is
 * offered through the column menu.
 */
export const ledgerFeedTypeColumns: readonly ColumnMeta[] = [
    {
        name: 'code',
        headerKey: 'ledger_feed_type.colCode',
        style: 'badge_centered',
        hidden: false,
        codeDomain: 'ledger_feed_type',
    },
    {
        name: 'name',
        headerKey: 'ledger_feed_type.colName',
        style: 'text_left',
        hidden: false,
    },
    {
        name: 'description',
        headerKey: 'ledger_feed_type.colDescription',
        style: 'text_left',
        hidden: true,
    },
    {
        name: 'display_order',
        headerKey: 'ledger_feed_type.colDisplayOrder',
        style: 'mono_center',
        hidden: false,
        width: 70,
    },
    {
        name: 'version',
        headerKey: 'ledger_feed_type.colVersion',
        style: 'mono_center',
        hidden: true,
        width: 70,
    },
    {
        name: 'modified_by',
        headerKey: 'ledger_feed_type.colModifiedBy',
        style: 'text_left',
        hidden: true,
    },
    {
        name: 'recorded_at',
        headerKey: 'ledger_feed_type.colRecordedAt',
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
export const ledgerFeedTypeMeta = {
    entity: 'ledger_feed_type',
    collection: 'types',
    displayField: 'name',
    keyField: 'code',
    columns: ledgerFeedTypeColumns,
    fields: ledgerFeedTypeFields,
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
export const ledgerFeedTypeMessages = {
        ledger_feed_type: {
            title: 'Ledger Feed Types',
            singular: 'ledger feed type',
            newTitle: 'New ledger feed type',
            description: 'Reference data table defining how a book\'s ledger balance is fed -- independent of its regulatory type (see regulatory_book_type) or its risk role (see book_purpose_type). Values: \'None\' (not fed from any source book), \'Automatic\' (fed by an automated ledger process), and \'Manual\' (fed by manual entry). Replaces the originally-scoped is_ledger_book/is_manual_ledger_book boolean pair, which allowed an invalid state (manual true while ledger false). Ledger feed types are mutually exclusive -- a book has exactly one at a time -- and managed by the system tenant. See [[id:74AA46EB-64ED-4FD7-B212-AEC164648B84][Book classification]] for the full analysis of why this is a 3-state lookup entity rather than two booleans.',
            fldCode: 'Code',
            codePh: 'Enter ledger feed type code',
            fldName: 'Name',
            namePh: 'Enter display name',
            fldDescription: 'Description',
            descriptionPh: 'Enter a description',
            colCode: 'Code',
            colName: 'Name',
            colDescription: 'Description',
            colDisplayOrder: 'Display Order',
            colVersion: 'Version',
            colModifiedBy: 'Modified By',
            colRecordedAt: 'Recorded At',
        }
};
