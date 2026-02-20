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
#include "ores.trade/repository/trade_identifier_mapper.hpp"

#include <boost/uuid/uuid_io.hpp>
#include <boost/lexical_cast.hpp>
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.trade/domain/trade_identifier_json_io.hpp" // IWYU pragma: keep.

namespace ores::trade::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::trade_identifier
trade_identifier_mapper::map(const trade_identifier_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::trade_identifier r;
    r.version = v.version;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.id = boost::lexical_cast<boost::uuids::uuid>(v.id.value());
    r.trade_id = boost::lexical_cast<boost::uuids::uuid>(v.trade_id);
    r.issuing_party_id = v.issuing_party_id.has_value() ? std::optional(boost::lexical_cast<boost::uuids::uuid>(*v.issuing_party_id)) : std::nullopt;
    r.id_value = v.id_value;
    r.id_type = v.id_type;
    r.id_scheme = v.id_scheme.value_or("");
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = timestamp_to_timepoint(v.valid_from.value());

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

trade_identifier_entity
trade_identifier_mapper::map(const domain::trade_identifier& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    trade_identifier_entity r;
    r.id = boost::uuids::to_string(v.id);
    r.tenant_id = v.tenant_id.to_string();
    r.version = v.version;
    r.trade_id = boost::uuids::to_string(v.trade_id);
    r.issuing_party_id = v.issuing_party_id.has_value() ? std::optional(boost::uuids::to_string(*v.issuing_party_id)) : std::nullopt;
    r.id_value = v.id_value;
    r.id_type = v.id_type;
    r.id_scheme = v.id_scheme.empty() ? std::nullopt : std::optional(v.id_scheme);
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::trade_identifier>
trade_identifier_mapper::map(const std::vector<trade_identifier_entity>& v) {
    return map_vector<trade_identifier_entity, domain::trade_identifier>(
        v,
        [](const auto& ve) { return map(ve); },
        lg(),
        "db entities");
}

std::vector<trade_identifier_entity>
trade_identifier_mapper::map(const std::vector<domain::trade_identifier>& v) {
    return map_vector<domain::trade_identifier, trade_identifier_entity>(
        v,
        [](const auto& ve) { return map(ve); },
        lg(),
        "domain entities");
}

}
