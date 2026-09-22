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
 * The fields of a ir_curve_bootstrap_config, in the order the model declares them.
 */
export const irCurveBootstrapConfigFields: readonly FieldMeta[] = [
    {
        name: 'source_series_id',
        labelKey: 'ir_curve_bootstrap_config.fldSourceSeriesId',
        control: 'line_edit',
        required: true,
        isKey: false,
        nullable: false,
    },
    {
        name: 'output_series_id',
        labelKey: 'ir_curve_bootstrap_config.fldOutputSeriesId',
        control: 'line_edit',
        required: true,
        isKey: false,
        nullable: false,
    },
    {
        name: 'curve_family_role',
        labelKey: 'ir_curve_bootstrap_config.fldCurveFamilyRole',
        control: 'line_edit',
        required: true,
        isKey: false,
        nullable: false,
    },
    {
        name: 'discount_curve_config_id',
        labelKey: 'ir_curve_bootstrap_config.fldDiscountCurveConfigId',
        control: 'line_edit',
        required: false,
        isKey: false,
        nullable: false,
    },
    {
        name: 'interpolation_method',
        labelKey: 'ir_curve_bootstrap_config.fldInterpolationMethod',
        control: 'line_edit',
        required: true,
        isKey: false,
        nullable: false,
    },
    {
        name: 'day_count_convention',
        labelKey: 'ir_curve_bootstrap_config.fldDayCountConvention',
        control: 'line_edit',
        required: true,
        isKey: false,
        nullable: false,
    },
    {
        name: 'split_tenor_code',
        labelKey: 'ir_curve_bootstrap_config.fldSplitTenorCode',
        control: 'line_edit',
        required: true,
        isKey: false,
        nullable: false,
    }
];

/**
 * The table columns, in display order. A column hidden by default is
 * offered through the column menu.
 */
export const irCurveBootstrapConfigColumns: readonly ColumnMeta[] = [
    {
        name: 'output_series_id',
        headerKey: 'ir_curve_bootstrap_config.colOutputSeriesId',
        style: 'mono_left',
        hidden: false,
    },
    {
        name: 'curve_family_role',
        headerKey: 'ir_curve_bootstrap_config.colCurveFamilyRole',
        style: 'text_left',
        hidden: false,
        width: 100,
    },
    {
        name: 'discount_curve_config_id',
        headerKey: 'ir_curve_bootstrap_config.colDiscountCurveConfigId',
        style: 'mono_left',
        hidden: false,
    },
    {
        name: 'interpolation_method',
        headerKey: 'ir_curve_bootstrap_config.colInterpolationMethod',
        style: 'text_left',
        hidden: false,
        width: 140,
    },
    {
        name: 'day_count_convention',
        headerKey: 'ir_curve_bootstrap_config.colDayCountConvention',
        style: 'text_left',
        hidden: false,
        width: 100,
    },
    {
        name: 'split_tenor_code',
        headerKey: 'ir_curve_bootstrap_config.colSplitTenorCode',
        style: 'text_left',
        hidden: false,
        width: 90,
    },
    {
        name: 'version',
        headerKey: 'ir_curve_bootstrap_config.colVersion',
        style: 'mono_center',
        hidden: true,
        width: 70,
    },
    {
        name: 'modified_by',
        headerKey: 'ir_curve_bootstrap_config.colModifiedBy',
        style: 'text_left',
        hidden: true,
    },
    {
        name: 'recorded_at',
        headerKey: 'ir_curve_bootstrap_config.colRecordedAt',
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
export const irCurveBootstrapConfigMeta = {
    entity: 'ir_curve_bootstrap_config',
    collection: 'ir_curve_bootstrap_configs',
    displayField: '',
    keyField: 'id',
    columns: irCurveBootstrapConfigColumns,
    fields: irCurveBootstrapConfigFields,
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
export const irCurveBootstrapConfigMessages = {
        ir_curve_bootstrap_config: {
            title: 'IR Curve Bootstrap Configs',
            singular: 'ir curve bootstrap config',
            newTitle: 'New ir curve bootstrap config',
            description: 'Records *how* a curve is bootstrapped as a named, inspectable artefact -- the "curve template" concept [[id:A26CFA71-21C0-4E98-ABC4-25F0EAD517E3][Multicurve Management]] calls for. Owned by ores.refdata, matching where every other recipe/config entity lives in this codebase (crm_topology_config, curve_role, tenor), not ores.marketdata, which owns only the generic value store the bootstrap output is written into. source_series_id and output_series_id are soft, cross-component references into ores.marketdata\'s market_series table (the raw instrument grid this config bootstraps, and the official curve series it publishes into) -- deliberately not hard FK constraints, matching the same soft-reference principle already used for market_series.derivation_config_id and ir_curve_tick\'s own producer/config identity: the referenced table lives in a different component/schema. output_series_id is minted (the market_series catalog row created) at config-creation time by the owning service, never left null/deferred -- there is no "not yet published" state to guard against. curve_family_role (FUNDING/PROJECTION) and discount_curve_config_id (self-referencing, nil-uuid sentinel for FUNDING) encode [[id:7CB0024B-84FB-4AE0-ADF2-763079E888D5][Multi-Curve Construction]]\'s Funding-before-Projection build-order dependency as data: a PROJECTION config\'s discount_curve_config_id must point at a FUNDING config (strict two-tier, no chaining -- Multi-Curve Construction names basis-linked/cyclic Projection dependencies as an explicit out-of-scope modelling gap, not something this design should silently permit), and must not reference itself. source_series_id (the raw grid this config bootstraps from) and output_series_id (the published curve it writes to) must differ. Without this, curve_republish_service\'s first-publish step would permanently reclassify the raw, externally-fed input series as this config\'s own IR_CURVE_BOOTSTRAP-derived output, and every subsequent bootstrapped observation would be written into the exact series id the raw feed keeps writing ticks into -- silently corrupting the raw data rather than failing loudly. interpolation_method and curve_family_role are small, fixed-vocabulary fields intrinsic to this record (not references to another entity), following the same plain-check-constraint pattern ir_curve_generation_config.role\'s \'self_discounting\'/discount/ projection vocabulary already establishes -- no lookup table for a handful of values with no independent lifecycle of their own. day_count_convention references the existing day_count_fraction_type reference table. split_tenor_code references tenor.code, following the same soft (undeclared, documentation-only) tenor reference ir_curve_template_entry\'s own start_tenor_code/end_tenor_code already use -- tenor validation happens through the tenor_resolution machinery at the application layer, not a per-consumer DB-level FK, consistent with that sibling entity. For a single-segment interpolation_method, split_tenor_code is a genuine value (the curve\'s own last pillar\'s end_tenor_code), not a fabricated sentinel -- matching the spirit of ir_curve_template_entries\'s own \'SPOT\' tenor, which its own doc comment is explicit isn\'t a sentinel hack either.',
            fldSourceSeriesId: 'Source Series Id',
            fldOutputSeriesId: 'Output Series Id',
            fldCurveFamilyRole: 'Curve Family Role',
            fldDiscountCurveConfigId: 'Discount Curve Config Id',
            fldInterpolationMethod: 'Interpolation Method',
            fldDayCountConvention: 'Day Count Convention',
            fldSplitTenorCode: 'Split Tenor Code',
            colOutputSeriesId: 'Output Series',
            colCurveFamilyRole: 'Role',
            colDiscountCurveConfigId: 'Discount Curve Config',
            colInterpolationMethod: 'Interpolation',
            colDayCountConvention: 'Day Count',
            colSplitTenorCode: 'Split Tenor',
            colVersion: 'Version',
            colModifiedBy: 'Modified By',
            colRecordedAt: 'Recorded At',
        }
};
