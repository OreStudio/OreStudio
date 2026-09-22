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
 * The fields of a fx_spot_generation_config, in the order the model declares them.
 */
export const fxSpotGenerationConfigFields: readonly FieldMeta[] = [
    {
        name: 'base_currency_code',
        labelKey: 'fx_spot_generation_config.fldBaseCurrencyCode',
        control: 'line_edit',
        required: true,
        isKey: false,
        nullable: false,
        placeholderKey: 'fx_spot_generation_config.baseCurrencyCodePh',
    },
    {
        name: 'quote_currency_code',
        labelKey: 'fx_spot_generation_config.fldQuoteCurrencyCode',
        control: 'line_edit',
        required: true,
        isKey: false,
        nullable: false,
        placeholderKey: 'fx_spot_generation_config.quoteCurrencyCodePh',
    },
    {
        name: 'price_source',
        labelKey: 'fx_spot_generation_config.fldPriceSource',
        control: 'line_edit',
        required: true,
        isKey: false,
        nullable: false,
        placeholderKey: 'fx_spot_generation_config.priceSourcePh',
    },
    {
        name: 'gmm_initial_price',
        labelKey: 'fx_spot_generation_config.fldGmmInitialPrice',
        control: 'line_edit',
        required: false,
        isKey: false,
        nullable: false,
        placeholderKey: 'fx_spot_generation_config.gmmInitialPricePh',
    },
    {
        name: 'ticks_per_hour',
        labelKey: 'fx_spot_generation_config.fldTicksPerHour',
        control: 'spin_box',
        required: true,
        isKey: false,
        nullable: false,
        min: 0,
        max: 9999,
    },
    {
        name: 'process_type',
        labelKey: 'fx_spot_generation_config.fldProcessType',
        control: 'line_edit',
        required: true,
        isKey: false,
        nullable: false,
        placeholderKey: 'fx_spot_generation_config.processTypePh',
    },
    {
        name: 'enabled',
        labelKey: 'fx_spot_generation_config.fldEnabled',
        control: 'check_box',
        required: false,
        isKey: false,
        nullable: false,
    },
    {
        name: 'auto_start',
        labelKey: 'fx_spot_generation_config.fldAutoStart',
        control: 'check_box',
        required: false,
        isKey: false,
        nullable: false,
    },
    {
        name: 'vintage_source',
        labelKey: 'fx_spot_generation_config.fldVintageSource',
        control: 'line_edit',
        required: false,
        isKey: false,
        nullable: false,
        placeholderKey: 'fx_spot_generation_config.vintageSourcePh',
    },
    {
        name: 'vintage_date',
        labelKey: 'fx_spot_generation_config.fldVintageDate',
        control: 'line_edit',
        required: false,
        isKey: false,
        nullable: false,
        placeholderKey: 'fx_spot_generation_config.vintageDatePh',
    }
];

/**
 * The table columns, in display order. A column hidden by default is
 * offered through the column menu.
 */
export const fxSpotGenerationConfigColumns: readonly ColumnMeta[] = [
    {
        name: 'base_currency_code',
        headerKey: 'fx_spot_generation_config.colBaseCurrencyCode',
        style: 'icon_text_left',
        hidden: false,
        width: 100,
    },
    {
        name: 'quote_currency_code',
        headerKey: 'fx_spot_generation_config.colQuoteCurrencyCode',
        style: 'icon_text_left',
        hidden: false,
        width: 100,
    },
    {
        name: 'source_name',
        headerKey: 'fx_spot_generation_config.colSourceName',
        style: 'text_left',
        hidden: false,
        width: 180,
    },
    {
        name: 'ore_key',
        headerKey: 'fx_spot_generation_config.colOreKey',
        style: 'text_left',
        hidden: false,
        width: 180,
    },
    {
        name: 'price_source',
        headerKey: 'fx_spot_generation_config.colPriceSource',
        style: 'text_left',
        hidden: false,
        width: 100,
    },
    {
        name: 'gmm_initial_price',
        headerKey: 'fx_spot_generation_config.colGmmInitialPrice',
        style: 'text_left',
        hidden: false,
        width: 100,
    },
    {
        name: 'ticks_per_hour',
        headerKey: 'fx_spot_generation_config.colTicksPerHour',
        style: 'mono_center',
        hidden: false,
        width: 80,
    },
    {
        name: 'process_type',
        headerKey: 'fx_spot_generation_config.colProcessType',
        style: 'text_left',
        hidden: false,
        width: 100,
    },
    {
        name: 'enabled',
        headerKey: 'fx_spot_generation_config.colEnabled',
        style: 'text_left',
        hidden: false,
        width: 70,
    },
    {
        name: 'auto_start',
        headerKey: 'fx_spot_generation_config.colAutoStart',
        style: 'text_left',
        hidden: false,
        width: 80,
    },
    {
        name: 'vintage_source',
        headerKey: 'fx_spot_generation_config.colVintageSource',
        style: 'text_left',
        hidden: false,
        width: 140,
    },
    {
        name: 'vintage_date',
        headerKey: 'fx_spot_generation_config.colVintageDate',
        style: 'text_left',
        hidden: false,
        width: 110,
    },
    {
        name: 'version',
        headerKey: 'fx_spot_generation_config.colVersion',
        style: 'mono_center',
        hidden: true,
        width: 70,
    },
    {
        name: 'modified_by',
        headerKey: 'fx_spot_generation_config.colModifiedBy',
        style: 'text_left',
        hidden: true,
        width: 120,
    },
    {
        name: 'recorded_at',
        headerKey: 'fx_spot_generation_config.colRecordedAt',
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
export const fxSpotGenerationConfigMeta = {
    entity: 'fx_spot_generation_config',
    collection: 'fx_spot_generation_configs',
    displayField: '',
    keyField: 'id',
    columns: fxSpotGenerationConfigColumns,
    fields: fxSpotGenerationConfigFields,
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
export const fxSpotGenerationConfigMessages = {
        fx_spot_generation_config: {
            title: 'FX Spot Generation Configs',
            singular: 'fx spot generation config',
            newTitle: 'New fx spot generation config',
            description: 'A typed sub-configuration owned by a market_data_generation_config. Describes how to synthesise the tick stream for a single FX spot rate: which ORE market key it produces, the starting price, the tick cadence, and the price process. The price process is a Gaussian Mixture Model whose components are held separately as gmm_component rows. Scoped to a tenant and a party.',
            fldBaseCurrencyCode: 'Base Currency',
            baseCurrencyCodePh: 'Enter base currency code',
            fldQuoteCurrencyCode: 'Quote Currency',
            quoteCurrencyCodePh: 'Enter quote currency code',
            fldPriceSource: 'Price Source',
            priceSourcePh: 'fixed, or vintage',
            fldGmmInitialPrice: 'Initial Price',
            gmmInitialPricePh: 'Required if Price Source is fixed',
            fldTicksPerHour: 'Ticks per Hour',
            fldProcessType: 'Process Type',
            processTypePh: 'geometric, arithmetic, or ornstein_uhlenbeck',
            fldEnabled: 'Enabled',
            fldAutoStart: 'Auto-Start',
            fldVintageSource: 'Vintage Source',
            vintageSourcePh: 'Required if Price Source is vintage, e.g. ore.reference',
            fldVintageDate: 'Vintage Date',
            vintageDatePh: 'Required if Price Source is vintage, YYYY-MM-DD',
            colBaseCurrencyCode: 'Base Currency',
            colQuoteCurrencyCode: 'Quote Currency',
            colSourceName: 'Source Name',
            colOreKey: 'ORE Key',
            colPriceSource: 'Price Source',
            colGmmInitialPrice: 'Initial Price',
            colTicksPerHour: 'Ticks/Hr',
            colProcessType: 'Process Type',
            colEnabled: 'Enabled',
            colAutoStart: 'Auto-Start',
            colVintageSource: 'Vintage Source',
            colVintageDate: 'Vintage Date',
            colVersion: 'Version',
            colModifiedBy: 'Modified By',
            colRecordedAt: 'Recorded At',
        }
};
