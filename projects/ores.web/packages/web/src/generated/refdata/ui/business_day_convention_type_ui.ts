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
 * The fields of a business_day_convention_type, in the order the model declares them.
 *
 * `code` is the natural key, so it is editable when creating
 * and read-only afterwards.
 */
export const businessDayConventionTypeFields: readonly FieldMeta[] = [
    {
        name: 'code',
        labelKey: 'business_day_convention_type.fldCode',
        control: 'line_edit',
        required: true,
        isKey: true,
        readOnlyAfterCreate: true,
        nullable: false,
        placeholderKey: 'business_day_convention_type.codePh',
    },
    {
        name: 'name',
        labelKey: 'business_day_convention_type.fldName',
        control: 'line_edit',
        required: true,
        isKey: false,
        nullable: false,
        placeholderKey: 'business_day_convention_type.namePh',
    },
    {
        name: 'description',
        labelKey: 'business_day_convention_type.fldDescription',
        control: 'line_edit',
        required: false,
        isKey: false,
        nullable: false,
        placeholderKey: 'business_day_convention_type.descriptionPh',
    },
    {
        name: 'display_order',
        labelKey: 'business_day_convention_type.fldDisplayOrder',
        control: 'spin_box',
        required: false,
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
export const businessDayConventionTypeColumns: readonly ColumnMeta[] = [
    {
        name: 'code',
        headerKey: 'business_day_convention_type.colCode',
        style: 'text_left',
        hidden: false,
        width: 200,
    },
    {
        name: 'name',
        headerKey: 'business_day_convention_type.colName',
        style: 'text_left',
        hidden: false,
        width: 220,
    },
    {
        name: 'description',
        headerKey: 'business_day_convention_type.colDescription',
        style: 'text_left',
        hidden: true,
        width: 350,
    },
    {
        name: 'display_order',
        headerKey: 'business_day_convention_type.colDisplayOrder',
        style: 'mono_center',
        hidden: false,
        width: 90,
    },
    {
        name: 'version',
        headerKey: 'business_day_convention_type.colVersion',
        style: 'mono_center',
        hidden: true,
        width: 80,
    },
    {
        name: 'modified_by',
        headerKey: 'business_day_convention_type.colModifiedBy',
        style: 'text_left',
        hidden: true,
        width: 120,
    },
    {
        name: 'recorded_at',
        headerKey: 'business_day_convention_type.colRecordedAt',
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
export const businessDayConventionTypeMeta = {
    entity: 'business_day_convention_type',
    collection: 'types',
    displayField: 'name',
    keyField: 'code',
    columns: businessDayConventionTypeColumns,
    fields: businessDayConventionTypeFields,
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
export const businessDayConventionTypeMessages = {
        business_day_convention_type: {
            title: 'Business Day Convention Types',
            singular: 'business day convention type',
            newTitle: 'New business day convention type',
            description: 'Reference data table defining valid business day conventions used to adjust dates that fall on a non-business day (e.g. for coupon/settlement dates on instrument legs and currency pair conventions). Values are sourced from ORE\'s ore_types.xsd. Moved here from ores.trading (see [[id:0345DCE3-4B85-4132-9A25-E58285632F76][Commission: business_day_convention_type]]) since every consumer of this type — the *_convention entities and now [[id:1B88215B-1FE0-4CAF-B6AB-53F471963CA6][currency_pair_convention]] — lives in ores.refdata, not ores.trading.',
            fldCode: 'Code',
            codePh: 'e.g. ModifiedFollowing',
            fldName: 'Name',
            namePh: 'e.g. Modified Following',
            fldDescription: 'Description',
            descriptionPh: 'Short description',
            fldDisplayOrder: 'Display Order',
            colCode: 'Code',
            colName: 'Name',
            colDescription: 'Description',
            colDisplayOrder: 'Display Order',
            colVersion: 'Version',
            colModifiedBy: 'Modified By',
            colRecordedAt: 'Recorded At',
        }
};
