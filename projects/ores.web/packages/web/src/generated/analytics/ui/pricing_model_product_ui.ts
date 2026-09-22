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
 * The fields of a pricing_model_product, in the order the model declares them.
 *
 * `pricing_engine_type_code` is the natural key, so it is editable when creating
 * and read-only afterwards.
 */
export const pricingModelProductFields: readonly FieldMeta[] = [
    {
        name: 'pricing_engine_type_code',
        labelKey: 'pricing_model_product.fldPricingEngineTypeCode',
        control: 'dynamic_combo',
        required: true,
        isKey: true,
        readOnlyAfterCreate: true,
        nullable: false,
    },
    {
        name: 'model',
        labelKey: 'pricing_model_product.fldModel',
        control: 'line_edit',
        required: true,
        isKey: false,
        nullable: false,
        placeholderKey: 'pricing_model_product.modelPh',
    },
    {
        name: 'engine',
        labelKey: 'pricing_model_product.fldEngine',
        control: 'line_edit',
        required: true,
        isKey: false,
        nullable: false,
        placeholderKey: 'pricing_model_product.enginePh',
    }
];

/**
 * The table columns, in display order. A column hidden by default is
 * offered through the column menu.
 */
export const pricingModelProductColumns: readonly ColumnMeta[] = [
    {
        name: 'pricing_engine_type_code',
        headerKey: 'pricing_model_product.colPricingEngineTypeCode',
        style: 'text_left',
        hidden: false,
        width: 200,
    },
    {
        name: 'model',
        headerKey: 'pricing_model_product.colModel',
        style: 'text_left',
        hidden: false,
        width: 200,
    },
    {
        name: 'engine',
        headerKey: 'pricing_model_product.colEngine',
        style: 'text_left',
        hidden: false,
        width: 200,
    },
    {
        name: 'version',
        headerKey: 'pricing_model_product.colVersion',
        style: 'mono_center',
        hidden: true,
        width: 70,
    },
    {
        name: 'modified_by',
        headerKey: 'pricing_model_product.colModifiedBy',
        style: 'text_left',
        hidden: true,
        width: 120,
    },
    {
        name: 'recorded_at',
        headerKey: 'pricing_model_product.colRecordedAt',
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
export const pricingModelProductMeta = {
    entity: 'pricing_model_product',
    collection: 'products',
    displayField: 'pricing_engine_type_code',
    keyField: 'pricing_engine_type_code',
    columns: pricingModelProductColumns,
    fields: pricingModelProductFields,
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
export const pricingModelProductMessages = {
        pricing_model_product: {
            title: 'Pricing Model Products',
            singular: 'pricing model product',
            newTitle: 'New pricing model product',
            description: 'Detail row within a pricing model configuration. Each row maps a pricing engine type (e.g. EuropeanSwaption, CMS) to a specific model (e.g. LGM, BlackBachelier) and numerical engine (e.g. Grid, AMC). Product-specific parameters are stored in pricing_model_product_parameters.',
            fldPricingEngineTypeCode: 'Pricing Engine Type',
            fldModel: 'Model',
            modelPh: 'Pricing model (e.g. DiscountedCashflows, LGM, BlackBachelier)',
            fldEngine: 'Engine',
            enginePh: 'Numerical engine (e.g. DiscountingSwapEngine, Grid, AMC)',
            colPricingEngineTypeCode: 'Engine Type',
            colModel: 'Model',
            colEngine: 'Engine',
            colVersion: 'Version',
            colModifiedBy: 'Modified By',
            colRecordedAt: 'Recorded At',
        }
};
