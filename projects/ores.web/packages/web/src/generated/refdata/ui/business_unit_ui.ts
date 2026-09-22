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
 * The fields of a business_unit, in the order the model declares them.
 *
 * `unit_code` is the natural key, so it is editable when creating
 * and read-only afterwards.
 */
export const businessUnitFields: readonly FieldMeta[] = [
    {
        name: 'unit_code',
        labelKey: 'business_unit.fldUnitCode',
        control: 'line_edit',
        required: false,
        isKey: true,
        readOnlyAfterCreate: true,
        nullable: true,
        placeholderKey: 'business_unit.unitCodePh',
    },
    {
        name: 'unit_name',
        labelKey: 'business_unit.fldUnitName',
        control: 'line_edit',
        required: true,
        isKey: false,
        nullable: false,
        placeholderKey: 'business_unit.unitNamePh',
    },
    {
        name: 'business_centre_code',
        labelKey: 'business_unit.fldBusinessCentreCode',
        control: 'line_edit',
        required: false,
        isKey: false,
        nullable: true,
        placeholderKey: 'business_unit.businessCentreCodePh',
    },
    {
        name: 'status',
        labelKey: 'business_unit.fldStatus',
        control: 'static_combo',
        required: false,
        isKey: false,
        nullable: false,
        options: [
            { value: 'Active', labelKey: 'business_unit.type.Active' },
            { value: 'Inactive', labelKey: 'business_unit.type.Inactive' },
            { value: 'Closed', labelKey: 'business_unit.type.Closed' },
        ],
    },
    {
        name: 'unit_type_id',
        labelKey: 'business_unit.fldUnitTypeId',
        control: 'dynamic_combo',
        required: false,
        isKey: false,
        nullable: true,
        lookup: { collection: 'business_unit_types', valueField: 'id', labelField: 'name' },
    }
];

/**
 * The table columns, in display order. A column hidden by default is
 * offered through the column menu.
 */
export const businessUnitColumns: readonly ColumnMeta[] = [
    {
        name: 'unit_code',
        headerKey: 'business_unit.colUnitCode',
        style: 'text_left',
        hidden: false,
        width: 120,
    },
    {
        name: 'unit_name',
        headerKey: 'business_unit.colUnitName',
        style: 'text_left',
        hidden: false,
        width: 250,
    },
    {
        name: 'business_centre_code',
        headerKey: 'business_unit.colBusinessCentreCode',
        style: 'icon_text_left',
        hidden: false,
        width: 130,
    },
    {
        name: 'status',
        headerKey: 'business_unit.colStatus',
        style: 'text_left',
        hidden: false,
        width: 100,
    },
    {
        name: 'version',
        headerKey: 'business_unit.colVersion',
        style: 'mono_center',
        hidden: true,
        width: 80,
    },
    {
        name: 'modified_by',
        headerKey: 'business_unit.colModifiedBy',
        style: 'text_left',
        hidden: true,
        width: 120,
    },
    {
        name: 'recorded_at',
        headerKey: 'business_unit.colRecordedAt',
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
export const businessUnitMeta = {
    entity: 'business_unit',
    collection: 'business_units',
    displayField: 'unit_code',
    keyField: 'unit_code',
    columns: businessUnitColumns,
    fields: businessUnitFields,
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
export const businessUnitMessages = {
        business_unit: {
            title: 'Business Units',
            singular: 'business unit',
            newTitle: 'New business unit',
            description: 'Represents internal organizational units (e.g., desks, departments, branches). Supports hierarchical structure via self-referencing parent_business_unit_id. Each unit belongs to a top-level legal entity (party).',
            fldUnitCode: 'Unit Code',
            unitCodePh: 'Enter unit code',
            fldUnitName: 'Unit Name',
            unitNamePh: 'Enter unit name',
            fldBusinessCentreCode: 'Business Centre',
            businessCentreCodePh: 'Enter business centre code',
            fldStatus: 'Status',
            fldUnitTypeId: 'Unit Type',
            colUnitCode: 'Code',
            colUnitName: 'Name',
            colBusinessCentreCode: 'Business Centre',
            colStatus: 'Status',
            colVersion: 'Version',
            colModifiedBy: 'Modified By',
            colRecordedAt: 'Recorded At',
        }
};
