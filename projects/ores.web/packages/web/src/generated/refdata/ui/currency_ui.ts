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
 * The fields of a currency, in the order the model declares them.
 *
 * `iso_code` is the natural key, so it is editable when creating
 * and read-only afterwards.
 */
export const currencyFields: readonly FieldMeta[] = [
    {
        name: 'iso_code',
        labelKey: 'currency.fldIsoCode',
        control: 'line_edit',
        required: true,
        isKey: true,
        readOnlyAfterCreate: true,
        nullable: false,
        placeholderKey: 'currency.isoCodePh',
    },
    {
        name: 'name',
        labelKey: 'currency.fldName',
        control: 'line_edit',
        required: true,
        isKey: false,
        nullable: false,
        placeholderKey: 'currency.namePh',
    },
    {
        name: 'numeric_code',
        labelKey: 'currency.fldNumericCode',
        control: 'line_edit',
        required: true,
        isKey: false,
        nullable: false,
        placeholderKey: 'currency.numericCodePh',
    },
    {
        name: 'monetary_nature',
        labelKey: 'currency.fldMonetaryNature',
        control: 'dynamic_combo',
        required: false,
        isKey: false,
        nullable: false,
        lookup: { collection: 'types', valueField: 'code', labelField: 'description' },
        codeDomain: 'monetary_nature',
    },
    {
        name: 'market_tier',
        labelKey: 'currency.fldMarketTier',
        control: 'dynamic_combo',
        required: false,
        isKey: false,
        nullable: false,
        lookup: { collection: 'types', valueField: 'code', labelField: 'description' },
        codeDomain: 'currency_market_tier',
    },
    {
        name: 'symbol',
        labelKey: 'currency.fldSymbol',
        control: 'line_edit',
        required: true,
        isKey: false,
        nullable: false,
        placeholderKey: 'currency.symbolPh',
    },
    {
        name: 'fraction_symbol',
        labelKey: 'currency.fldFractionSymbol',
        control: 'line_edit',
        required: false,
        isKey: false,
        nullable: false,
        placeholderKey: 'currency.fractionSymbolPh',
    },
    {
        name: 'fractions_per_unit',
        labelKey: 'currency.fldFractionsPerUnit',
        control: 'spin_box',
        required: false,
        isKey: false,
        nullable: false,
        min: 0,
        max: 10000,
    },
    {
        name: 'format',
        labelKey: 'currency.fldFormat',
        control: 'line_edit',
        required: false,
        isKey: false,
        nullable: false,
        placeholderKey: 'currency.formatPh',
    },
    {
        name: 'rounding_type',
        labelKey: 'currency.fldRoundingType',
        control: 'dynamic_combo',
        required: false,
        isKey: false,
        nullable: false,
        lookup: { collection: 'types', valueField: 'code', labelField: 'description' },
    },
    {
        name: 'rounding_precision',
        labelKey: 'currency.fldRoundingPrecision',
        control: 'spin_box',
        required: false,
        isKey: false,
        nullable: false,
        min: 0,
        max: 10,
    }
];

/**
 * The table columns, in display order. A column hidden by default is
 * offered through the column menu.
 */
export const currencyColumns: readonly ColumnMeta[] = [
    {
        name: 'iso_code',
        headerKey: 'currency.colIsoCode',
        style: 'icon_text_left',
        hidden: false,
        flag: true,
    },
    {
        name: 'name',
        headerKey: 'currency.colCurrencyName',
        style: 'text_left',
        hidden: false,
    },
    {
        name: 'numeric_code',
        headerKey: 'currency.colNumericCode',
        style: 'text_left',
        hidden: true,
        width: 70,
    },
    {
        name: 'symbol',
        headerKey: 'currency.colSymbol',
        style: 'text_left',
        hidden: false,
        width: 60,
    },
    {
        name: 'fraction_symbol',
        headerKey: 'currency.colFractionSymbol',
        style: 'text_left',
        hidden: true,
        width: 60,
    },
    {
        name: 'fractions_per_unit',
        headerKey: 'currency.colFractionsPerUnit',
        style: 'mono_center',
        hidden: true,
        width: 70,
    },
    {
        name: 'rounding_type',
        headerKey: 'currency.colRoundingType',
        style: 'text_left',
        hidden: true,
    },
    {
        name: 'rounding_precision',
        headerKey: 'currency.colRoundingPrecision',
        style: 'mono_center',
        hidden: false,
        width: 70,
    },
    {
        name: 'format',
        headerKey: 'currency.colFormat',
        style: 'text_left',
        hidden: true,
    },
    {
        name: 'monetary_nature',
        headerKey: 'currency.colMonetaryNature',
        style: 'badge_centered',
        hidden: false,
        codeDomain: 'monetary_nature',
    },
    {
        name: 'market_tier',
        headerKey: 'currency.colMarketTier',
        style: 'badge_centered',
        hidden: false,
        codeDomain: 'currency_market_tier',
    },
    {
        name: 'spot_days',
        headerKey: 'currency.colSpotDays',
        style: 'mono_center',
        hidden: true,
        width: 70,
    },
    {
        name: 'day_basis',
        headerKey: 'currency.colDayBasis',
        style: 'text_left',
        hidden: false,
        width: 80,
    },
    {
        name: 'base_precedence',
        headerKey: 'currency.colBasePrecedence',
        style: 'mono_center',
        hidden: true,
        width: 100,
    },
    {
        name: 'version',
        headerKey: 'currency.colVersion',
        style: 'mono_center',
        hidden: true,
        width: 70,
    },
    {
        name: 'modified_by',
        headerKey: 'currency.colModifiedBy',
        style: 'text_left',
        hidden: true,
    },
    {
        name: 'recorded_at',
        headerKey: 'currency.colRecordedAt',
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
 *
 * The image member is present because the model declares a flag, and the
 * shared screen renders its picker from this member rather than from a
 * field, because no form control edits an image.
 */
export const currencyMeta = {
    entity: 'currency',
    collection: 'currencies',
    displayField: 'name',
    keyField: 'iso_code',
    columns: currencyColumns,
    fields: currencyFields,
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
export const currencyMessages = {
        currency: {
            title: 'Currencies',
            singular: 'currency',
            newTitle: 'New currency',
            description: 'ISO 4217 currency definitions used for reference data. Currencies are managed per-tenant and drive financial calculations, rounding, and display formatting across the system.',
            fldIsoCode: 'ISO Code',
            isoCodePh: 'Enter ISO 4217 code',
            fldName: 'Name',
            namePh: 'Enter currency name',
            fldNumericCode: 'Numeric Code',
            numericCodePh: 'Enter numeric code',
            fldMonetaryNature: 'Asset Class',
            fldMarketTier: 'Market Tier',
            fldSymbol: 'Symbol',
            symbolPh: 'Enter currency symbol',
            fldFractionSymbol: 'Fraction Symbol',
            fractionSymbolPh: 'Enter fraction symbol',
            fldFractionsPerUnit: 'Fractions Per Unit',
            fldFormat: 'Format',
            formatPh: 'Enter display format',
            fldRoundingType: 'Rounding Type',
            fldRoundingPrecision: 'Rounding Precision',
            colIsoCode: 'Code',
            colCurrencyName: 'Currency Name',
            colNumericCode: 'Numeric Code',
            colSymbol: 'Symbol',
            colFractionSymbol: 'Fraction',
            colFractionsPerUnit: 'Per Unit',
            colRoundingType: 'Rounding Type',
            colRoundingPrecision: 'Precision',
            colFormat: 'Format',
            colMonetaryNature: 'Monetary Nature',
            colMarketTier: 'Market Tier',
            colSpotDays: 'Spot Days',
            colDayBasis: 'Day Basis',
            colBasePrecedence: 'Base Precedence',
            colVersion: 'Version',
            colModifiedBy: 'Modified By',
            colRecordedAt: 'Recorded At',
        }
};
