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
 * The fields of a currency_pair_convention, in the order the model declares them.
 *
 * `pair_code` is the natural key, so it is editable when creating
 * and read-only afterwards.
 */
export const currencyPairConventionFields: readonly FieldMeta[] = [
    {
        name: 'pair_code',
        labelKey: 'currency_pair_convention.fldPairCode',
        control: 'flagged_combo',
        required: true,
        isKey: true,
        readOnlyAfterCreate: true,
        nullable: false,
        placeholderKey: 'currency_pair_convention.pairCodePh',
    },
    {
        name: 'pip_factor',
        labelKey: 'currency_pair_convention.fldPipFactor',
        control: 'line_edit',
        required: true,
        isKey: false,
        nullable: false,
        placeholderKey: 'currency_pair_convention.pipFactorPh',
    },
    {
        name: 'tick_size',
        labelKey: 'currency_pair_convention.fldTickSize',
        control: 'line_edit',
        required: true,
        isKey: false,
        nullable: false,
        placeholderKey: 'currency_pair_convention.tickSizePh',
    },
    {
        name: 'decimal_places',
        labelKey: 'currency_pair_convention.fldDecimalPlaces',
        control: 'spin_box',
        required: true,
        isKey: false,
        nullable: false,
        min: 0,
        max: 9999,
    },
    {
        name: 'business_day_convention',
        labelKey: 'currency_pair_convention.fldBusinessDayConvention',
        control: 'static_combo',
        required: false,
        isKey: false,
        nullable: true,
        options: [
            { value: 'Following', labelKey: 'currency_pair_convention.type.Following' },
            { value: 'ModifiedFollowing', labelKey: 'currency_pair_convention.type.ModifiedFollowing' },
            { value: 'Preceding', labelKey: 'currency_pair_convention.type.Preceding' },
            { value: 'ModifiedPreceding', labelKey: 'currency_pair_convention.type.ModifiedPreceding' },
            { value: 'Unadjusted', labelKey: 'currency_pair_convention.type.Unadjusted' },
            { value: 'HalfMonthModifiedFollowing', labelKey: 'currency_pair_convention.type.HalfMonthModifiedFollowing' },
            { value: 'Nearest', labelKey: 'currency_pair_convention.type.Nearest' },
        ],
        codeDomain: 'currency_pair_convention_business_day_convention',
    },
    {
        name: 'spot_relative',
        labelKey: 'currency_pair_convention.fldSpotRelative',
        control: 'check_box',
        required: false,
        isKey: false,
        nullable: true,
        triState: true,
    },
    {
        name: 'end_of_month',
        labelKey: 'currency_pair_convention.fldEndOfMonth',
        control: 'check_box',
        required: false,
        isKey: false,
        nullable: true,
        triState: true,
    }
];

/**
 * The table columns, in display order. A column hidden by default is
 * offered through the column menu.
 */
export const currencyPairConventionColumns: readonly ColumnMeta[] = [
    {
        name: 'pair_code',
        headerKey: 'currency_pair_convention.colPairCode',
        style: 'icon_text_left',
        hidden: false,
        width: 100,
    },
    {
        name: 'pip_factor',
        headerKey: 'currency_pair_convention.colPipFactor',
        style: 'text_left',
        hidden: false,
        width: 90,
    },
    {
        name: 'tick_size',
        headerKey: 'currency_pair_convention.colTickSize',
        style: 'text_left',
        hidden: false,
        width: 90,
    },
    {
        name: 'decimal_places',
        headerKey: 'currency_pair_convention.colDecimalPlaces',
        style: 'mono_center',
        hidden: false,
        width: 100,
    },
    {
        name: 'business_day_convention',
        headerKey: 'currency_pair_convention.colBusinessDayConvention',
        style: 'badge_centered',
        hidden: false,
        width: 150,
        codeDomain: 'currency_pair_convention_business_day_convention',
    },
    {
        name: 'spot_relative',
        headerKey: 'currency_pair_convention.colSpotRelative',
        style: 'badge_centered',
        hidden: false,
        width: 100,
        codeDomain: 'currency_pair_convention_spot_relative',
    },
    {
        name: 'end_of_month',
        headerKey: 'currency_pair_convention.colEndOfMonth',
        style: 'badge_centered',
        hidden: false,
        width: 100,
        codeDomain: 'currency_pair_convention_end_of_month',
    },
    {
        name: 'version',
        headerKey: 'currency_pair_convention.colVersion',
        style: 'mono_center',
        hidden: true,
        width: 70,
    },
    {
        name: 'modified_by',
        headerKey: 'currency_pair_convention.colModifiedBy',
        style: 'text_left',
        hidden: true,
    },
    {
        name: 'recorded_at',
        headerKey: 'currency_pair_convention.colRecordedAt',
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
export const currencyPairConventionMeta = {
    entity: 'currency_pair_convention',
    collection: 'conventions',
    displayField: 'pair_code',
    keyField: 'pair_code',
    columns: currencyPairConventionColumns,
    fields: currencyPairConventionFields,
} as const;
