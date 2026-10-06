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
#include "ores.trading.core/repository/trade_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.trading.api/domain/booking_nature.hpp"
#include "ores.trading.api/domain/counterparty_scope.hpp"
#include "ores.trading.api/domain/entry_channel.hpp"
#include "ores.trading.api/domain/trade.hpp"
#include "ores.trading.api/domain/trade_json_io.hpp" // IWYU pragma: keep.
#include "ores.trading.core/repository/trade_entity.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/log/sources/severity_feature.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <optional>
#include <rfl/enums.hpp>
#include <vector>

namespace ores::trading::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::trade trade_mapper::map(const trade_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::trade r;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.id = boost::lexical_cast<boost::uuids::uuid>(v.id.value());
    r.party_id = boost::lexical_cast<boost::uuids::uuid>(v.party_id);
    r.counterparty_id =
        v.counterparty_id.has_value() ?
            std::optional(boost::lexical_cast<boost::uuids::uuid>(*v.counterparty_id)) :
            std::nullopt;
    r.trade_type = v.trade_type;
    r.counterparty_scope =
        rfl::string_to_enum<ores::trading::domain::counterparty_scope>(v.counterparty_scope)
            .value();
    r.booking_nature =
        rfl::string_to_enum<ores::trading::domain::booking_nature>(v.booking_nature).value();
    r.entry_channel =
        rfl::string_to_enum<ores::trading::domain::entry_channel>(v.entry_channel).value();

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

trade_entity trade_mapper::map(const domain::trade& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    trade_entity r;
    r.id = boost::uuids::to_string(v.id);
    r.tenant_id = v.tenant_id.to_string();
    r.party_id = boost::uuids::to_string(v.party_id);
    r.counterparty_id = v.counterparty_id.has_value() ?
                            std::optional(boost::uuids::to_string(*v.counterparty_id)) :
                            std::nullopt;
    r.trade_type = v.trade_type;
    r.counterparty_scope = rfl::enum_to_string(v.counterparty_scope);
    r.booking_nature = rfl::enum_to_string(v.booking_nature);
    r.entry_channel = rfl::enum_to_string(v.entry_channel);

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::trade> trade_mapper::map(const std::vector<trade_entity>& v) {
    return map_vector<trade_entity, domain::trade>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<trade_entity> trade_mapper::map(const std::vector<domain::trade>& v) {
    return map_vector<domain::trade, trade_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
