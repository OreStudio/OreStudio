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
 * How the BFF reaches the currency service.
 *
 * The descriptor holds values and no functions: the collection, the natural
 * key, the members a write record carries and the subjects. The generic
 * factory builds the canonical envelopes from those, so the entity adds no
 * handler of its own and no envelope of its own.
 */
import { subjects } from '@ores/wire-protocol/generated/refdata/protocol/currency_protocol';
import type { EntityRouteDescriptor } from '../../entity-routes.js';

export const currencyRoute: EntityRouteDescriptor = {
  collection: 'currencies',
  key: 'id',
  keyField: 'iso_code',
  rowField: 'currency',
  writeFields: ['iso_code', 'name', 'numeric_code', 'symbol', 'fraction_symbol', 'fractions_per_unit', 'rounding_type', 'rounding_precision', 'format', 'monetary_nature', 'market_tier', 'image_id', 'spot_days', 'day_basis', 'base_precedence'],
  writeDefaults: { iso_code: '', name: '', numeric_code: '', symbol: '', fraction_symbol: '', fractions_per_unit: 0, rounding_type: '', rounding_precision: 0, format: '', monetary_nature: '', market_tier: '', image_id: null, spot_days: 0, day_basis: '', base_precedence: 0 },
  intentFields: {
    reason: 'change_reason_code',
    commentary: 'change_commentary',
  },
  listHasAsOf: true,
  listHasFilter: false,
  versionsHasFilter: true,
  subjects: {
    list: subjects.list_currencies_request,
    get: subjects.get_currency_request,
    save: subjects.put_currency_request,
    remove: subjects.delete_currency_request,
    history: subjects.list_currency_versions_request,
  },
  rowsField: 'currencies',
  historyRowsField: 'versions',
};
