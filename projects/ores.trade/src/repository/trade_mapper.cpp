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
#include "ores.trade/repository/trade_mapper.hpp"

#include <boost/uuid/uuid_io.hpp>
#include <boost/lexical_cast.hpp>
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.trade/domain/trade_json_io.hpp" // IWYU pragma: keep.

namespace ores::trade::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::trade
trade_mapper::map(const trade_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::trade r;
    r.version = v.version;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.id = boost::lexical_cast<boost::uuids::uuid>(v.id.value());
    r.external_id = v.external_id.value_or("");
    r.book_id = boost::lexical_cast<boost::uuids::uuid>(v.book_id);
    r.portfolio_id = boost::lexical_cast<boost::uuids::uuid>(v.portfolio_id);
    r.successor_trade_id = v.successor_trade_id.has_value() ? std::optional(boost::lexical_cast<boost::uuids::uuid>(*v.successor_trade_id)) : std::nullopt;
    r.trade_type = v.trade_type;
    r.netting_set_id = v.netting_set_id;
    r.lifecycle_event = v.lifecycle_event;
    r.trade_date = v.trade_date;
    r.execution_timestamp = v.execution_timestamp;
    r.effective_date = v.effective_date;
    r.termination_date = v.termination_date;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = timestamp_to_timepoint(v.valid_from.value());

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

trade_entity
trade_mapper::map(const domain::trade& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    trade_entity r;
    r.id = boost::uuids::to_string(v.id);
    r.tenant_id = v.tenant_id.to_string();
    r.version = v.version;
    r.external_id = v.external_id.empty() ? std::nullopt : std::optional(v.external_id);
    r.book_id = boost::uuids::to_string(v.book_id);
    r.portfolio_id = boost::uuids::to_string(v.portfolio_id);
    r.successor_trade_id = v.successor_trade_id.has_value() ? std::optional(boost::uuids::to_string(*v.successor_trade_id)) : std::nullopt;
    r.trade_type = v.trade_type;
    r.netting_set_id = v.netting_set_id;
    r.lifecycle_event = v.lifecycle_event;
    r.trade_date = v.trade_date;
    r.execution_timestamp = v.execution_timestamp;
    r.effective_date = v.effective_date;
    r.termination_date = v.termination_date;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::trade>
trade_mapper::map(const std::vector<trade_entity>& v) {
    return map_vector<trade_entity, domain::trade>(
        v,
        [](const auto& ve) { return map(ve); },
        lg(),
        "db entities");
}

std::vector<trade_entity>
trade_mapper::map(const std::vector<domain::trade>& v) {
    return map_vector<domain::trade, trade_entity>(
        v,
        [](const auto& ve) { return map(ve); },
        lg(),
        "domain entities");
}

}
