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
 * The fields of a login_info, in the order the model declares them.
 *
 * `account_id` is the natural key, so it is editable when creating
 * and read-only afterwards.
 */
export const loginInfoFields: readonly FieldMeta[] = [
    {
        name: 'account_id',
        labelKey: 'login_info.fldAccountId',
        control: 'line_edit',
        required: true,
        isKey: true,
        readOnlyAfterCreate: true,
        nullable: false,
        placeholderKey: 'login_info.accountIdPh',
    },
    {
        name: 'last_login',
        labelKey: 'login_info.fldLastLogin',
        control: 'date',
        required: false,
        isKey: false,
        nullable: false,
    },
    {
        name: 'failed_logins',
        labelKey: 'login_info.fldFailedLogins',
        control: 'spin_box',
        required: false,
        isKey: false,
        nullable: false,
        min: 0,
        max: 9999,
    },
    {
        name: 'locked',
        labelKey: 'login_info.fldLocked',
        control: 'check_box',
        required: false,
        isKey: false,
        nullable: false,
    },
    {
        name: 'online',
        labelKey: 'login_info.fldOnline',
        control: 'check_box',
        required: false,
        isKey: false,
        nullable: false,
    },
    {
        name: 'password_reset_required',
        labelKey: 'login_info.fldPasswordResetRequired',
        control: 'check_box',
        required: false,
        isKey: false,
        nullable: false,
    },
    {
        name: 'last_ip',
        labelKey: 'login_info.fldLastIp',
        control: 'line_edit',
        required: false,
        isKey: false,
        nullable: false,
    },
    {
        name: 'last_attempt_ip',
        labelKey: 'login_info.fldLastAttemptIp',
        control: 'line_edit',
        required: false,
        isKey: false,
        nullable: false,
    }
];

/**
 * The table columns, in display order. A column hidden by default is
 * offered through the column menu.
 */
export const loginInfoColumns: readonly ColumnMeta[] = [
    {
        name: 'account_id',
        headerKey: 'login_info.colAccountId',
        style: 'text_left',
        hidden: false,
        width: 300,
    },
    {
        name: 'last_login',
        headerKey: 'login_info.colLastLogin',
        style: 'mono_left',
        hidden: false,
        width: 160,
        temporal: true,
    },
    {
        name: 'failed_logins',
        headerKey: 'login_info.colFailedLogins',
        style: 'mono_center',
        hidden: false,
        width: 100,
    },
    {
        name: 'locked',
        headerKey: 'login_info.colLocked',
        style: 'text_left',
        hidden: false,
        width: 80,
    },
    {
        name: 'online',
        headerKey: 'login_info.colOnline',
        style: 'text_left',
        hidden: false,
        width: 80,
    },
    {
        name: 'password_reset_required',
        headerKey: 'login_info.colPasswordResetRequired',
        style: 'text_left',
        hidden: false,
        width: 140,
    },
    {
        name: 'last_ip',
        headerKey: 'login_info.colLastIp',
        style: 'text_left',
        hidden: false,
        width: 140,
    },
    {
        name: 'last_attempt_ip',
        headerKey: 'login_info.colLastAttemptIp',
        style: 'text_left',
        hidden: false,
        width: 140,
    }
];

/**
 * The collection name the list request uses, and the display field.
 *
 * Emitted so the sidebar and the lookup selects do not have to know it
 * separately.
 */
export const loginInfoMeta = {
    entity: 'login_info',
    collection: 'login_info',
    displayField: 'account_id',
    keyField: 'account_id',
    columns: loginInfoColumns,
    fields: loginInfoFields,
} as const;
