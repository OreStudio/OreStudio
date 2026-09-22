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
 * The fields of a cds_convention, in the order the model declares them.
 */
export const cdsConventionFields: readonly FieldMeta[] = [
    {
        name: 'calendar',
        labelKey: 'cds_convention.fldCalendar',
        control: 'line_edit',
        required: true,
        isKey: false,
        nullable: false,
        placeholderKey: 'cds_convention.calendarPh',
    },
    {
        name: 'frequency',
        labelKey: 'cds_convention.fldFrequency',
        control: 'line_edit',
        required: true,
        isKey: false,
        nullable: false,
        placeholderKey: 'cds_convention.frequencyPh',
    },
    {
        name: 'payment_convention',
        labelKey: 'cds_convention.fldPaymentConvention',
        control: 'line_edit',
        required: true,
        isKey: false,
        nullable: false,
        placeholderKey: 'cds_convention.paymentConventionPh',
    },
    {
        name: 'rule',
        labelKey: 'cds_convention.fldRule',
        control: 'line_edit',
        required: true,
        isKey: false,
        nullable: false,
        placeholderKey: 'cds_convention.rulePh',
    },
    {
        name: 'day_count_fraction',
        labelKey: 'cds_convention.fldDayCountFraction',
        control: 'line_edit',
        required: true,
        isKey: false,
        nullable: false,
        placeholderKey: 'cds_convention.dayCountFractionPh',
    },
    {
        name: 'settlement_days',
        labelKey: 'cds_convention.fldSettlementDays',
        control: 'spin_box',
        required: false,
        isKey: false,
        nullable: false,
        min: 0,
        max: 9999,
    },
    {
        name: 'upfront_settlement_days',
        labelKey: 'cds_convention.fldUpfrontSettlementDays',
        control: 'spin_box',
        required: false,
        isKey: false,
        nullable: true,
        min: -1,
        max: 9999,
    },
    {
        name: 'last_period_day_count_fraction',
        labelKey: 'cds_convention.fldLastPeriodDayCountFraction',
        control: 'line_edit',
        required: false,
        isKey: false,
        nullable: true,
        placeholderKey: 'cds_convention.lastPeriodDayCountFractionPh',
    },
    {
        name: 'settles_accrual',
        labelKey: 'cds_convention.fldSettlesAccrual',
        control: 'check_box',
        required: false,
        isKey: false,
        nullable: false,
    },
    {
        name: 'pays_at_default_time',
        labelKey: 'cds_convention.fldPaysAtDefaultTime',
        control: 'check_box',
        required: false,
        isKey: false,
        nullable: false,
    }
];

/**
 * The table columns, in display order. A column hidden by default is
 * offered through the column menu.
 */
export const cdsConventionColumns: readonly ColumnMeta[] = [
    {
        name: 'id',
        headerKey: 'cds_convention.colId',
        style: 'text_left',
        hidden: false,
        width: 220,
    },
    {
        name: 'frequency',
        headerKey: 'cds_convention.colFrequency',
        style: 'text_left',
        hidden: false,
        width: 100,
    },
    {
        name: 'rule',
        headerKey: 'cds_convention.colRule',
        style: 'text_left',
        hidden: false,
        width: 120,
    },
    {
        name: 'day_count_fraction',
        headerKey: 'cds_convention.colDayCountFraction',
        style: 'text_left',
        hidden: false,
        width: 100,
    },
    {
        name: 'version',
        headerKey: 'cds_convention.colVersion',
        style: 'mono_center',
        hidden: true,
        width: 80,
    },
    {
        name: 'modified_by',
        headerKey: 'cds_convention.colModifiedBy',
        style: 'text_left',
        hidden: true,
        width: 120,
    },
    {
        name: 'recorded_at',
        headerKey: 'cds_convention.colRecordedAt',
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
export const cdsConventionMeta = {
    entity: 'cds_convention',
    collection: 'cds_conventions',
    displayField: 'id',
    keyField: 'id',
    columns: cdsConventionColumns,
    fields: cdsConventionFields,
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
export const cdsConventionMessages = {
        cds_convention: {
            title: 'CDS Conventions',
            singular: 'cds convention',
            newTitle: 'New cds convention',
            description: 'Defines the settlement lag, premium payment schedule, and accrual rules for a vanilla CDS. Corresponds to the <CDS> element in ORE conventions.xml. The rule field governs IMM dates and standard CDS roll dates.',
            fldCalendar: 'Calendar',
            calendarPh: 'e.g. WeekendsOnly',
            fldFrequency: 'Frequency',
            frequencyPh: 'e.g. Quarterly',
            fldPaymentConvention: 'Payment Convention',
            paymentConventionPh: 'e.g. Following',
            fldRule: 'Rule',
            rulePh: 'e.g. CDS2015',
            fldDayCountFraction: 'Day Count Fraction',
            dayCountFractionPh: 'e.g. ACT/360',
            fldSettlementDays: 'Settlement Days',
            fldUpfrontSettlementDays: 'Upfront Settlement Days',
            fldLastPeriodDayCountFraction: 'Last Period DCF',
            lastPeriodDayCountFractionPh: 'e.g. ACT/360',
            fldSettlesAccrual: 'Settles Accrual',
            fldPaysAtDefaultTime: 'Pays At Default Time',
            colId: 'Id',
            colFrequency: 'Frequency',
            colRule: 'Rule',
            colDayCountFraction: 'DCF',
            colVersion: 'Version',
            colModifiedBy: 'Modified By',
            colRecordedAt: 'Recorded At',
        }
};
