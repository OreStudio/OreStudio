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
 * How the BFF reaches the ir_curve_bootstrap_config service.
 *
 * The descriptor holds values and no functions: the collection, the natural
 * key, the members a write record carries and the subjects. The generic
 * factory builds the canonical envelopes from those, so the entity adds no
 * handler of its own and no envelope of its own.
 */
import { subjects } from '@ores/wire-protocol/generated/refdata/protocol/ir_curve_bootstrap_config_protocol';
import { MINTED_WRITE_DEFAULT, type EntityRouteDescriptor } from '../../entity-routes.js';

export const irCurveBootstrapConfigRoute: EntityRouteDescriptor = {
  collection: 'ir_curve_bootstrap_configs',
  key: 'id',
  keyField: 'id',
  rowField: 'ir_curve_bootstrap_config',
  writeFields: ['id', 'output_series_id', 'source_series_id', 'curve_family_role', 'discount_curve_config_id', 'interpolation_method', 'day_count_convention', 'split_tenor_code'],
  writeDefaults: { id: MINTED_WRITE_DEFAULT, output_series_id: null, source_series_id: null, curve_family_role: '', discount_curve_config_id: null, interpolation_method: '', day_count_convention: '', split_tenor_code: '' },
  intentFields: {
    reason: 'change_reason_code',
    commentary: 'change_commentary',
  },
  listHasAsOf: false,
  listHasFilter: false,
  versionsHasFilter: true,
  subjects: {
    list: subjects.list_ir_curve_bootstrap_configs_request,
    get: subjects.get_ir_curve_bootstrap_config_request,
    save: subjects.put_ir_curve_bootstrap_config_request,
    remove: subjects.delete_ir_curve_bootstrap_config_request,
    history: subjects.list_ir_curve_bootstrap_config_versions_request,
  },
  rowsField: 'bootstrap_configs',
  historyRowsField: 'versions',
};
