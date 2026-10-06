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
#include "ores.trading.core/repository/trade_portfolio_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.trading.api/domain/trade_portfolio.hpp"
#include "ores.trading.api/domain/trade_portfolio_json_io.hpp" // IWYU pragma: keep.
#include "ores.trading.core/repository/trade_portfolio_entity.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/log/sources/severity_feature.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <string>
#include <vector>

namespace ores::trading::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::trade_portfolio trade_portfolio_mapper::map(const trade_portfolio_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::trade_portfolio r;
    r.version = v.version;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.trade_id = boost::lexical_cast<boost::uuids::uuid>(v.trade_id.value());
    r.sequence_number = boost::lexical_cast<int>(v.sequence_number.value());
    r.party_id = boost::lexical_cast<boost::uuids::uuid>(v.party_id);
    r.portfolio_id = boost::lexical_cast<boost::uuids::uuid>(v.portfolio_id);
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = timestamp_to_timepoint(v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

trade_portfolio_entity trade_portfolio_mapper::map(const domain::trade_portfolio& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    trade_portfolio_entity r;
    r.trade_id = boost::uuids::to_string(v.trade_id);
    r.sequence_number = std::to_string(v.sequence_number);
    r.tenant_id = v.tenant_id.to_string();
    r.version = v.version;
    r.party_id = boost::uuids::to_string(v.party_id);
    r.portfolio_id = boost::uuids::to_string(v.portfolio_id);
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::trade_portfolio>
trade_portfolio_mapper::map(const std::vector<trade_portfolio_entity>& v) {
    return map_vector<trade_portfolio_entity, domain::trade_portfolio>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<trade_portfolio_entity>
trade_portfolio_mapper::map(const std::vector<domain::trade_portfolio>& v) {
    return map_vector<domain::trade_portfolio, trade_portfolio_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
