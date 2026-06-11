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
#include "ores.trading.core/repository/vanilla_swap_instrument_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.trading.api/domain/vanilla_swap_instrument_json_io.hpp" // IWYU pragma: keep.
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>

namespace ores::trading::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::vanilla_swap_instrument
vanilla_swap_instrument_mapper::map(const vanilla_swap_instrument_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::vanilla_swap_instrument r;
    r.identity.version = v.version;
    r.identity.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.identity.workspace_id = boost::lexical_cast<boost::uuids::uuid>(v.workspace_id);
    r.identity.instrument_id = boost::lexical_cast<boost::uuids::uuid>(v.instrument_id.value());
    r.identity.trade_type_code = v.trade_type_code;
    r.identity.party_id = boost::lexical_cast<boost::uuids::uuid>(v.party_id);
    r.identity.trade_id = v.trade_id.has_value() ?
                              std::optional(boost::lexical_cast<boost::uuids::uuid>(*v.trade_id)) :
                              std::nullopt;
    r.start_date = v.start_date;
    r.maturity_date = v.maturity_date;
    r.settlement_lag = v.settlement_lag;
    r.netting_set_id = v.netting_set_id.value_or("");
    r.description = v.description.value_or("");
    r.audit.modified_by = v.modified_by;
    r.audit.performed_by = v.performed_by;
    r.audit.change_reason_code = v.change_reason_code;
    r.audit.change_commentary = v.change_commentary;
    if (!v.valid_from)
        throw std::logic_error("Cannot map entity with null valid_from to domain object.");
    r.audit.recorded_at = timestamp_to_timepoint(*v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

vanilla_swap_instrument_entity
vanilla_swap_instrument_mapper::map(const domain::vanilla_swap_instrument& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    vanilla_swap_instrument_entity r;
    r.instrument_id = boost::uuids::to_string(v.identity.instrument_id);
    r.tenant_id = v.identity.tenant_id.to_string();
    r.workspace_id = boost::uuids::to_string(v.identity.workspace_id);
    r.version = v.identity.version;
    r.trade_type_code = v.identity.trade_type_code;
    r.party_id = boost::uuids::to_string(v.identity.party_id);
    r.trade_id = v.identity.trade_id.has_value() ?
                     std::optional(boost::uuids::to_string(*v.identity.trade_id)) :
                     std::nullopt;
    r.start_date = v.start_date;
    r.maturity_date = v.maturity_date;
    r.settlement_lag = v.settlement_lag;
    r.netting_set_id = v.netting_set_id.empty() ? std::nullopt : std::optional(v.netting_set_id);
    r.description = v.description.empty() ? std::nullopt : std::optional(v.description);
    r.modified_by = v.audit.modified_by;
    r.performed_by = v.audit.performed_by;
    r.change_reason_code = v.audit.change_reason_code;
    r.change_commentary = v.audit.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::vanilla_swap_instrument>
vanilla_swap_instrument_mapper::map(const std::vector<vanilla_swap_instrument_entity>& v) {
    return map_vector<vanilla_swap_instrument_entity, domain::vanilla_swap_instrument>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<vanilla_swap_instrument_entity>
vanilla_swap_instrument_mapper::map(const std::vector<domain::vanilla_swap_instrument>& v) {
    return map_vector<domain::vanilla_swap_instrument, vanilla_swap_instrument_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
