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
#include "ores.trading.core/repository/trade_link_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.trading.api/domain/trade_link.hpp"
#include "ores.trading.api/domain/trade_link_json_io.hpp" // IWYU pragma: keep.
#include "ores.trading.core/repository/trade_link_entity.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/log/sources/severity_feature.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <vector>

namespace ores::trading::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::trade_link trade_link_mapper::map(const trade_link_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::trade_link r;
    r.version = v.version;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.from_trade_id = boost::lexical_cast<boost::uuids::uuid>(v.from_trade_id.value());
    r.to_trade_id = boost::lexical_cast<boost::uuids::uuid>(v.to_trade_id.value());
    r.link_type = v.link_type.value();
    r.trade_activity_id = boost::lexical_cast<boost::uuids::uuid>(v.trade_activity_id);
    r.party_id = boost::lexical_cast<boost::uuids::uuid>(v.party_id);
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = timestamp_to_timepoint(v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

trade_link_entity trade_link_mapper::map(const domain::trade_link& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    trade_link_entity r;
    r.from_trade_id = boost::uuids::to_string(v.from_trade_id);
    r.to_trade_id = boost::uuids::to_string(v.to_trade_id);
    r.link_type = v.link_type;
    r.tenant_id = v.tenant_id.to_string();
    r.version = v.version;
    r.trade_activity_id = boost::uuids::to_string(v.trade_activity_id);
    r.party_id = boost::uuids::to_string(v.party_id);
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::trade_link> trade_link_mapper::map(const std::vector<trade_link_entity>& v) {
    return map_vector<trade_link_entity, domain::trade_link>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<trade_link_entity> trade_link_mapper::map(const std::vector<domain::trade_link>& v) {
    return map_vector<domain::trade_link, trade_link_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
