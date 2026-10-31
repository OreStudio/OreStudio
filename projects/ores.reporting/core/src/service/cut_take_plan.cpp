/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
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
#include "ores.reporting.core/service/cut_take_plan.hpp"
#include <algorithm>

namespace ores::reporting::service {

domain::cut plan_take(const cut_take_plan& plan,
                      const domain::cut_kind& kind,
                      boost::uuids::uuid id,
                      int take_ordinal) {
    domain::cut cut;
    cut.id = id;

    cut.kind_code = plan.kind_code;
    cut.scope_code = plan.scope_code;
    cut.business_centre_code = plan.business_centre_code;
    cut.party_id = plan.party_id;
    cut.timepoint = plan.timepoint;
    cut.timepoint_timezone = plan.timepoint_timezone;
    cut.cut_time_code = plan.cut_time_code;
    cut.take_ordinal = take_ordinal;

    cut.market_data_snapshot_id = plan.market_data_snapshot_id;
    cut.deal_population_id = plan.deal_population_id;
    cut.reference_data_version = plan.reference_data_version;
    cut.engine_version = plan.engine_version;
    cut.code_version = plan.code_version;

    cut.taken_by = plan.taken_by;
    cut.taken_at = plan.taken_at;

    cut.requires_authorisation = kind.approval_kind_code.has_value();

    cut.retention_months = kind.retention_months;
    cut.retention_destination_code = kind.retention_destination_code;
    cut.legal_hold = kind.legal_hold_default;

    cut.modified_by = plan.taken_by;
    cut.performed_by = plan.taken_by;
    cut.change_reason_code = plan.change_reason_code;
    cut.change_commentary = plan.change_commentary;
    return cut;
}

int next_take_ordinal(const std::vector<domain::cut>& same_occasion) {
    int highest = 0;
    for (const auto& cut : same_occasion)
        highest = std::max(highest, cut.take_ordinal);
    return highest + 1;
}

}
