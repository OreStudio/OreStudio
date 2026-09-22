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
 * How the BFF reaches the ois_convention service.
 *
 * The descriptor holds values and no functions: the collection, the key, the
 * members a write record carries, the optional members the requests declare
 * and the subjects. The generic factory builds the canonical envelopes from
 * those, so the entity adds no handler of its own and no envelope of its own.
 */
import { subjects } from '@ores/wire-protocol/generated/refdata/protocol/ois_convention_protocol';
import type { EntityRouteDescriptor } from '../../entity-routes.js';

export const oisConventionRoute: EntityRouteDescriptor = {
  component: 'refdata',
  entity: 'ois_convention',
  collection: 'ois_conventions',
  keyFields: ['id'],
  writeFields: ['id', 'spot_lag', 'index', 'fixed_day_count_fraction', 'fixed_calendar', 'payment_lag', 'end_of_month', 'fixed_frequency', 'fixed_convention', 'fixed_payment_convention', 'rule', 'payment_calendar', 'rate_cutoff'],
  writeDefaults: { id: '', spot_lag: 0, index: '', fixed_day_count_fraction: '', fixed_calendar: null, payment_lag: null, end_of_month: null, fixed_frequency: null, fixed_convention: null, fixed_payment_convention: null, rule: null, payment_calendar: null, rate_cutoff: null },
  listHasAsOf: false,
  listHasFilter: false,
  versionsHasFilter: true,
  subjects: {
    list: subjects.list_ois_conventions_request,
    get: subjects.get_ois_convention_request,
    save: subjects.put_ois_convention_request,
    remove: subjects.delete_ois_convention_request,
    history: subjects.list_ois_convention_versions_request,
  },
  rowsField: 'ois_conventions',
  getRowField: 'ois_convention',
  historyRowsField: 'versions',
};
