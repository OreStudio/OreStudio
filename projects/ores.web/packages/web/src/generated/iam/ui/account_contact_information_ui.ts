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
 * The fields of a account_contact_information, in the order the model declares them.
 *
 * `email` is the natural key, so it is editable when creating
 * and read-only afterwards.
 */
export const accountContactInformationFields: readonly FieldMeta[] = [
    {
        name: 'street_line_1',
        labelKey: 'account_contact_information.fldStreetLine1',
        control: 'line_edit',
        required: false,
        isKey: false,
        nullable: true,
        placeholderKey: 'account_contact_information.streetLine1Ph',
    },
    {
        name: 'street_line_2',
        labelKey: 'account_contact_information.fldStreetLine2',
        control: 'line_edit',
        required: false,
        isKey: false,
        nullable: true,
        placeholderKey: 'account_contact_information.streetLine2Ph',
    },
    {
        name: 'city',
        labelKey: 'account_contact_information.fldCity',
        control: 'line_edit',
        required: false,
        isKey: false,
        nullable: true,
        placeholderKey: 'account_contact_information.cityPh',
    },
    {
        name: 'state',
        labelKey: 'account_contact_information.fldState',
        control: 'line_edit',
        required: false,
        isKey: false,
        nullable: true,
        placeholderKey: 'account_contact_information.statePh',
    },
    {
        name: 'country_code',
        labelKey: 'account_contact_information.fldCountryCode',
        control: 'flagged_combo',
        required: false,
        isKey: false,
        nullable: true,
    },
    {
        name: 'postal_code',
        labelKey: 'account_contact_information.fldPostalCode',
        control: 'line_edit',
        required: false,
        isKey: false,
        nullable: true,
        placeholderKey: 'account_contact_information.postalCodePh',
    },
    {
        name: 'phone',
        labelKey: 'account_contact_information.fldPhone',
        control: 'line_edit',
        required: false,
        isKey: false,
        nullable: true,
        placeholderKey: 'account_contact_information.phonePh',
    },
    {
        name: 'email',
        labelKey: 'account_contact_information.fldEmail',
        control: 'line_edit',
        required: false,
        isKey: false,
        nullable: true,
        placeholderKey: 'account_contact_information.emailPh',
    },
    {
        name: 'web_page',
        labelKey: 'account_contact_information.fldWebPage',
        control: 'line_edit',
        required: false,
        isKey: false,
        nullable: true,
        placeholderKey: 'account_contact_information.webPagePh',
    }
];

/**
 * The table columns, in display order. A column hidden by default is
 * offered through the column menu.
 */
export const accountContactInformationColumns: readonly ColumnMeta[] = [
    {
        name: 'street_line_1',
        headerKey: 'account_contact_information.colStreetLine1',
        style: 'text_left',
        hidden: false,
        width: 180,
    },
    {
        name: 'city',
        headerKey: 'account_contact_information.colCity',
        style: 'text_left',
        hidden: false,
        width: 120,
    },
    {
        name: 'country_code',
        headerKey: 'account_contact_information.colCountryCode',
        style: 'icon_text_left',
        hidden: false,
        width: 100,
    },
    {
        name: 'phone',
        headerKey: 'account_contact_information.colPhone',
        style: 'text_left',
        hidden: false,
        width: 140,
    },
    {
        name: 'email',
        headerKey: 'account_contact_information.colEmail',
        style: 'text_left',
        hidden: false,
        width: 180,
    },
    {
        name: 'version',
        headerKey: 'account_contact_information.colVersion',
        style: 'mono_center',
        hidden: true,
        width: 80,
    },
    {
        name: 'modified_by',
        headerKey: 'account_contact_information.colModifiedBy',
        style: 'text_left',
        hidden: true,
        width: 120,
    },
    {
        name: 'recorded_at',
        headerKey: 'account_contact_information.colRecordedAt',
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
export const accountContactInformationMeta = {
    entity: 'account_contact_information',
    collection: 'account_contact_informations',
    displayField: 'email',
    keyField: 'email',
    columns: accountContactInformationColumns,
    fields: accountContactInformationFields,
} as const;
