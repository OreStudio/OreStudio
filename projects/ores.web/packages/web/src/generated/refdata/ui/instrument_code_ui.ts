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
 * The fields of a instrument_code, in the order the model declares them.
 *
 * `code` is the natural key, so it is editable when creating
 * and read-only afterwards.
 */
export const instrumentCodeFields: readonly FieldMeta[] = [
    {
        name: 'code',
        labelKey: 'instrument_code.fldCode',
        control: 'line_edit',
        required: true,
        isKey: true,
        readOnlyAfterCreate: true,
        nullable: false,
        placeholderKey: 'instrument_code.codePh',
    },
    {
        name: 'name',
        labelKey: 'instrument_code.fldName',
        control: 'line_edit',
        required: true,
        isKey: false,
        nullable: false,
        placeholderKey: 'instrument_code.namePh',
    },
    {
        name: 'description',
        labelKey: 'instrument_code.fldDescription',
        control: 'text_edit',
        required: true,
        isKey: false,
        nullable: false,
        placeholderKey: 'instrument_code.descriptionPh',
    },
    {
        name: 'asset_class',
        labelKey: 'instrument_code.fldAssetClass',
        control: 'dynamic_combo',
        required: false,
        isKey: false,
        nullable: true,
        lookup: { collection: 'classes', valueField: 'code', labelField: 'description' },
        codeDomain: 'asset_class',
    },
    {
        name: 'ore_trade_type',
        labelKey: 'instrument_code.fldOreTradeType',
        control: 'line_edit',
        required: false,
        isKey: false,
        nullable: true,
        placeholderKey: 'instrument_code.oreTradeTypePh',
    },
    {
        name: 'curve_role',
        labelKey: 'instrument_code.fldCurveRole',
        control: 'dynamic_combo',
        required: true,
        isKey: false,
        nullable: false,
        lookup: { collection: 'roles', valueField: 'code', labelField: 'description' },
        codeDomain: 'curve_role',
    },
    {
        name: 'display_order',
        labelKey: 'instrument_code.fldDisplayOrder',
        control: 'spin_box',
        required: true,
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
export const instrumentCodeColumns: readonly ColumnMeta[] = [
    {
        name: 'code',
        headerKey: 'instrument_code.colCode',
        style: 'text_left',
        hidden: false,
    },
    {
        name: 'name',
        headerKey: 'instrument_code.colName',
        style: 'text_left',
        hidden: false,
    },
    {
        name: 'description',
        headerKey: 'instrument_code.colDescription',
        style: 'text_left',
        hidden: true,
    },
    {
        name: 'asset_class',
        headerKey: 'instrument_code.colAssetClass',
        style: 'badge_centered',
        hidden: false,
        codeDomain: 'asset_class',
    },
    {
        name: 'ore_trade_type',
        headerKey: 'instrument_code.colOreTradeType',
        style: 'text_left',
        hidden: false,
    },
    {
        name: 'curve_role',
        headerKey: 'instrument_code.colCurveRole',
        style: 'badge_centered',
        hidden: false,
        codeDomain: 'curve_role',
    },
    {
        name: 'display_order',
        headerKey: 'instrument_code.colDisplayOrder',
        style: 'mono_center',
        hidden: false,
        width: 70,
    },
    {
        name: 'version',
        headerKey: 'instrument_code.colVersion',
        style: 'mono_center',
        hidden: true,
        width: 70,
    },
    {
        name: 'modified_by',
        headerKey: 'instrument_code.colModifiedBy',
        style: 'text_left',
        hidden: true,
    },
    {
        name: 'recorded_at',
        headerKey: 'instrument_code.colRecordedAt',
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
export const instrumentCodeMeta = {
    entity: 'instrument_code',
    collection: 'codes',
    displayField: 'name',
    keyField: 'code',
    columns: instrumentCodeColumns,
    fields: instrumentCodeFields,
} as const;
