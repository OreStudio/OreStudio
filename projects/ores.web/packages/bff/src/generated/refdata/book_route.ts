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
 * Template: ts_bff_route.ts.mustache
 * To modify, update the template and regenerate.
 */
/**
 * How the BFF reaches the book service.
 *
 * The descriptor holds values and no functions: the collection, the key, the
 * members a write record carries, the optional members the requests declare
 * and the subjects. The generic factory builds the canonical envelopes from
 * those, so the entity adds no handler of its own and no envelope of its own.
 */
import { subjects } from '@ores/wire-protocol/generated/refdata/protocol/book_protocol';
import { MINTED_WRITE_DEFAULT, type EntityRouteDescriptor } from '../../entity-routes.js';

export const bookRoute: EntityRouteDescriptor = {
  component: 'refdata',
  entity: 'book',
  collection: 'books',
  keyFields: ['name'],
  writeFields: ['id', 'name', 'description', 'parent_portfolio_id', 'owner_unit_id', 'functional_currency', 'gl_account_ref', 'cost_center', 'book_status', 'regulatory_book_type', 'is_sweepable', 'rates_centre_code'],
  writeDefaults: { id: MINTED_WRITE_DEFAULT, name: '', description: '', parent_portfolio_id: null, owner_unit_id: null, functional_currency: '', gl_account_ref: '', cost_center: '', book_status: '', regulatory_book_type: '', is_sweepable: false, rates_centre_code: '' },
  listHasAsOf: false,
  listHasFilter: true,
  versionsHasFilter: true,
  subjects: {
    list: subjects.list_books_request,
    get: subjects.get_book_request,
    save: subjects.put_book_request,
    remove: subjects.delete_book_request,
    history: subjects.list_book_versions_request,
  },
  rowsField: 'books',
  getRowField: 'book',
  historyRowsField: 'versions',
};
