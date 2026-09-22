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
 * How the BFF reaches the swap_convention service.
 *
 * The descriptor holds values and no functions: the collection, the key, the
 * members a write record carries, the optional members the requests declare
 * and the subjects. The generic factory builds the canonical envelopes from
 * those, so the entity adds no handler of its own and no envelope of its own.
 */
import { subjects } from '@ores/wire-protocol/generated/refdata/protocol/swap_convention_protocol';
import type { EntityRouteDescriptor } from '../../entity-routes.js';

export const swapConventionRoute: EntityRouteDescriptor = {
  component: 'refdata',
  entity: 'swap_convention',
  collection: 'swap_conventions',
  keyFields: ['id'],
  writeFields: ['id', 'fixed_calendar', 'fixed_frequency', 'fixed_convention', 'fixed_day_count_fraction', 'index', 'float_frequency', 'sub_periods_coupon_type'],
  writeDefaults: { id: '', fixed_calendar: null, fixed_frequency: '', fixed_convention: null, fixed_day_count_fraction: '', index: '', float_frequency: null, sub_periods_coupon_type: null },
  listHasAsOf: false,
  listHasFilter: false,
  versionsHasFilter: true,
  subjects: {
    list: subjects.list_swap_conventions_request,
    get: subjects.get_swap_convention_request,
    save: subjects.put_swap_convention_request,
    remove: subjects.delete_swap_convention_request,
    history: subjects.list_swap_convention_versions_request,
  },
  rowsField: 'swap_conventions',
  getRowField: 'swap_convention',
  historyRowsField: 'versions',
};
