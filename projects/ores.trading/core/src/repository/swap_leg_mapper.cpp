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
#include "ores.trading.core/repository/swap_leg_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.trading.api/domain/swap_leg_json_io.hpp" // IWYU pragma: keep.
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>

namespace ores::trading::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::swap_leg swap_leg_mapper::map(const swap_leg_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::swap_leg r;
    auto& id = r.identity;
    auto& tm = r.terms;
    auto& au = r.audit;
    id.version = v.version;
    id.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    id.id = boost::lexical_cast<boost::uuids::uuid>(v.id.value());
    id.instrument_id = boost::lexical_cast<boost::uuids::uuid>(v.instrument_id);
    id.leg_number = v.leg_number;
    tm.leg_type_code = v.leg_type_code;
    tm.day_count_fraction_code = v.day_count_fraction_code;
    tm.business_day_convention_code = v.business_day_convention_code;
    tm.payment_frequency_code = v.payment_frequency_code;
    tm.floating_index_code = v.floating_index_code.value_or("");
    tm.fixed_rate = v.fixed_rate.value_or(0.0);
    tm.spread = v.spread.value_or(0.0);
    tm.notional = v.notional;
    tm.currency = v.currency;
    au.modified_by = v.modified_by;
    au.performed_by = v.performed_by;
    au.change_reason_code = v.change_reason_code;
    au.change_commentary = v.change_commentary;
    au.recorded_at = timestamp_to_timepoint(v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

swap_leg_entity swap_leg_mapper::map(const domain::swap_leg& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    swap_leg_entity r;
    const auto& id = v.identity;
    const auto& tm = v.terms;
    const auto& au = v.audit;
    r.id = boost::uuids::to_string(id.id);
    r.tenant_id = id.tenant_id.to_string();
    r.version = id.version;
    r.instrument_id = boost::uuids::to_string(id.instrument_id);
    r.leg_number = id.leg_number;
    r.leg_type_code = tm.leg_type_code;
    r.day_count_fraction_code = tm.day_count_fraction_code;
    r.business_day_convention_code = tm.business_day_convention_code;
    r.payment_frequency_code = tm.payment_frequency_code;
    r.floating_index_code =
        tm.floating_index_code.empty() ? std::nullopt : std::optional(tm.floating_index_code);
    r.fixed_rate = std::optional(tm.fixed_rate);
    r.spread = std::optional(tm.spread);
    r.notional = tm.notional;
    r.currency = tm.currency;
    r.modified_by = au.modified_by;
    r.performed_by = au.performed_by;
    r.change_reason_code = au.change_reason_code;
    r.change_commentary = au.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::swap_leg> swap_leg_mapper::map(const std::vector<swap_leg_entity>& v) {
    return map_vector<swap_leg_entity, domain::swap_leg>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<swap_leg_entity> swap_leg_mapper::map(const std::vector<domain::swap_leg>& v) {
    return map_vector<domain::swap_leg, swap_leg_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
