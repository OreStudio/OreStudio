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
 *
 */

import { api } from '../../api/client.js';
import { PERMISSION } from '../../access/permissions.js';
import type { RecordStore } from '../../refdata/store.js';

/**
 * The kinds of cut as a record store.
 *
 * The shared list, the record page and the record dialog read and write through
 * a store, so a kind of cut is one of the record kinds the reporting component
 * holds: the store names the permission namespace the routes check, and the
 * page that draws a kind wraps itself in this one. Nothing about the kind is
 * special enough to build a dialog of its own.
 */

/** The one resource this store holds: the kinds of cut. */
export const CUT_KIND_RESOURCE = 'cut-kinds';

/**
 * The kinds of cut, read one page at a time from the server.
 *
 * A kind is written with the whole of the row the BFF read, because a save
 * carries the version the form was filled from: a kind somebody else changed in
 * between is a conflict and not a silent overwrite.
 */
export const cutKindStore: RecordStore = {
    name: 'reporting',
    registry: async () => [
        {
            key: CUT_KIND_RESOURCE,
            writable: true,
            search: true,
            sortable: ['code', 'name', 'family'],
            writePermission: PERMISSION.cutKindsWrite,
            deletePermission: PERMISSION.cutKindsDelete,
        },
    ],
    page: (resource, page) => api.cutKinds(page),
    records: async () => (await api.cutKinds({ ...emptyPage, limit: kindsOfAFilter })).rows,
    record: async (resource, key) => ({ ...(await api.cutKind(key)).kind }),
    save: async (resource, { write, version, reasonCode, commentary }) => {
        await api.saveCutKind(String(write['code'] ?? ''), {
            version: version ?? 0,
            change: {
                name: String(write['name'] ?? ''),
                description: String(write['description'] ?? ''),
                familyCode: String(write['family_code'] ?? ''),
                approvalKindCode:
                    write['approval_kind_code'] === null ||
                    write['approval_kind_code'] === undefined
                        ? null
                        : String(write['approval_kind_code']),
                outputIsIndicative: write['output_is_indicative'] === true,
                resolutionRuleCode: String(write['resolution_rule_code'] ?? ''),
                retentionMonths: Number(write['retention_months'] ?? 0),
                legalHoldDefault: write['legal_hold_default'] === true,
                displayOrder: Number(write['display_order'] ?? 0),
            },
            reasonCode,
            commentary,
        });
    },
    remove: async (resource, { key, version, reasonCode, commentary }) => {
        await api.removeCutKind(String(key['code'] ?? ''), {
            version: version ?? 0,
            reasonCode,
            commentary,
        });
    },
};

/** How many kinds a filter's choices read, which is one page of the list's own sizes. */
const kindsOfAFilter = 200;

/** The page a read that names no page starts from. */
const emptyPage = { offset: 0, limit: kindsOfAFilter, search: '', sort: '', descending: false };
