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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_domain_type_mapper.cpp.mustache
 * To modify, update the template and regenerate.
 */
#include "ores.trading.core/repository/swap_leg_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.trading.api/domain/swap_leg.hpp"
#include "ores.trading.api/domain/swap_leg_json_io.hpp" // IWYU pragma: keep.
#include "ores.trading.core/repository/swap_leg_entity.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/log/sources/severity_feature.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <optional>
#include <vector>

namespace ores::trading::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::swap_leg swap_leg_mapper::map(const swap_leg_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::swap_leg r;
    r.identity.version = v.version;
    r.identity.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.identity.id = boost::lexical_cast<boost::uuids::uuid>(v.id.value());
    r.identity.party_id = boost::lexical_cast<boost::uuids::uuid>(v.party_id);
    r.identity.trade_id = boost::lexical_cast<boost::uuids::uuid>(v.trade_id);
    r.identity.trade_activity_id = boost::lexical_cast<boost::uuids::uuid>(v.trade_activity_id);
    r.identity.leg_number = v.leg_number;
    r.payer = v.payer;
    r.leg_type_code = v.leg_type_code;
    r.day_count_fraction_code = v.day_count_fraction_code;
    r.business_day_convention_code = v.business_day_convention_code;
    r.payment_frequency_code = v.payment_frequency_code;
    r.floating_index_code = v.floating_index_code.value_or("");
    r.currency = v.currency;
    r.audit.modified_by = v.modified_by;
    r.audit.performed_by = v.performed_by;
    r.audit.change_reason_code = v.change_reason_code;
    r.audit.change_commentary = v.change_commentary;
    r.audit.recorded_at = timestamp_to_timepoint(v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

swap_leg_entity swap_leg_mapper::map(const domain::swap_leg& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    swap_leg_entity r;
    r.id = boost::uuids::to_string(v.identity.id);
    r.tenant_id = v.identity.tenant_id.to_string();
    r.version = v.identity.version;
    r.party_id = boost::uuids::to_string(v.identity.party_id);
    r.trade_id = boost::uuids::to_string(v.identity.trade_id);
    r.trade_activity_id = boost::uuids::to_string(v.identity.trade_activity_id);
    r.leg_number = v.identity.leg_number;
    r.payer = v.payer;
    r.leg_type_code = v.leg_type_code;
    r.day_count_fraction_code = v.day_count_fraction_code;
    r.business_day_convention_code = v.business_day_convention_code;
    r.payment_frequency_code = v.payment_frequency_code;
    r.floating_index_code =
        v.floating_index_code.empty() ? std::nullopt : std::optional(v.floating_index_code);
    r.currency = v.currency;
    r.modified_by = v.audit.modified_by;
    r.performed_by = v.audit.performed_by;
    r.change_reason_code = v.audit.change_reason_code;
    r.change_commentary = v.audit.change_commentary;

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
