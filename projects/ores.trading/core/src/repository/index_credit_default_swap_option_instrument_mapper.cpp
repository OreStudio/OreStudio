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
#include "ores.trading.core/repository/index_credit_default_swap_option_instrument_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.trading.api/domain/index_credit_default_swap_option_instrument.hpp"
#include "ores.trading.api/domain/index_credit_default_swap_option_instrument_json_io.hpp" // IWYU pragma: keep.
#include "ores.trading.core/repository/index_credit_default_swap_option_instrument_entity.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/log/sources/severity_feature.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <chrono>
#include <optional>
#include <vector>

namespace ores::trading::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::index_credit_default_swap_option_instrument
index_credit_default_swap_option_instrument_mapper::map(
    const index_credit_default_swap_option_instrument_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::index_credit_default_swap_option_instrument r;
    r.version = v.version;
    r.tenant_id = v.tenant_id;
    r.trade_id = v.trade_id;
    r.trade_activity_id = v.trade_activity_id;
    r.knock_out = v.knock_out;
    r.index_term = v.index_term;
    r.strike_type = v.strike_type;
    r.trade_date = v.trade_date;
    r.front_end_protection_start_date = v.front_end_protection_start_date;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = v.valid_from;

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

index_credit_default_swap_option_instrument_entity
index_credit_default_swap_option_instrument_mapper::map(
    const domain::index_credit_default_swap_option_instrument& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    index_credit_default_swap_option_instrument_entity r;
    r.trade_id = v.trade_id;
    r.tenant_id = v.tenant_id;
    r.version = v.version;
    r.trade_activity_id = v.trade_activity_id;
    r.knock_out = v.knock_out;
    r.index_term = v.index_term;
    r.strike_type = v.strike_type;
    r.trade_date = v.trade_date;
    r.front_end_protection_start_date = v.front_end_protection_start_date;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::index_credit_default_swap_option_instrument>
index_credit_default_swap_option_instrument_mapper::map(
    const std::vector<index_credit_default_swap_option_instrument_entity>& v) {
    return map_vector<index_credit_default_swap_option_instrument_entity,
                      domain::index_credit_default_swap_option_instrument>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<index_credit_default_swap_option_instrument_entity>
index_credit_default_swap_option_instrument_mapper::map(
    const std::vector<domain::index_credit_default_swap_option_instrument>& v) {
    return map_vector<domain::index_credit_default_swap_option_instrument,
                      index_credit_default_swap_option_instrument_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
