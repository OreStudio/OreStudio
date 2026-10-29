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
#include "ores.trading.core/repository/trade_summary_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.trading.api/domain/trade_summary.hpp"
#include "ores.trading.api/domain/trade_summary_json_io.hpp" // IWYU pragma: keep.
#include "ores.trading.core/repository/trade_summary_entity.hpp"
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

domain::trade_summary trade_summary_mapper::map(const trade_summary_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::trade_summary r;
    r.tenant_id = v.tenant_id;
    r.id = v.id;
    r.row_kind = v.row_kind;
    r.party_id = v.party_id;
    r.counterparty_id = v.counterparty_id;
    r.book_id = v.book_id;
    r.instrument_type = v.instrument_type;
    r.status = v.status;
    r.authorisation_status = v.authorisation_status;
    r.trade_date = v.trade_date;
    r.trade_count = v.trade_count;
    r.structure_id = v.structure_id.has_value() ?
                         std::optional(boost::lexical_cast<boost::uuids::uuid>(*v.structure_id)) :
                         std::nullopt;
    r.is_leg = v.is_leg;
    r.search_text = v.search_text;

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

trade_summary_entity trade_summary_mapper::map(const domain::trade_summary& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    trade_summary_entity r;
    r.id = v.id;
    r.tenant_id = v.tenant_id;
    r.row_kind = v.row_kind;
    r.party_id = v.party_id;
    r.counterparty_id = v.counterparty_id;
    r.book_id = v.book_id;
    r.instrument_type = v.instrument_type;
    r.status = v.status;
    r.authorisation_status = v.authorisation_status;
    r.trade_date = v.trade_date;
    r.trade_count = v.trade_count;
    r.structure_id = v.structure_id.has_value() ?
                         std::optional(boost::uuids::to_string(*v.structure_id)) :
                         std::nullopt;
    r.is_leg = v.is_leg;
    r.search_text = v.search_text;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::trade_summary>
trade_summary_mapper::map(const std::vector<trade_summary_entity>& v) {
    return map_vector<trade_summary_entity, domain::trade_summary>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<trade_summary_entity>
trade_summary_mapper::map(const std::vector<domain::trade_summary>& v) {
    return map_vector<domain::trade_summary, trade_summary_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
