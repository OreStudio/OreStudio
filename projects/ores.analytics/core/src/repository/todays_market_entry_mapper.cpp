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
#include "ores.analytics.core/repository/todays_market_entry_mapper.hpp"
#include "ores.analytics.api/domain/todays_market_entry_json_io.hpp" // IWYU pragma: keep.
#include "ores.database/repository/mapper_helpers.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>

namespace ores::analytics::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::todays_market_entry todays_market_entry_mapper::map(const todays_market_entry_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::todays_market_entry r;
    r.version = v.version;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.id = boost::lexical_cast<boost::uuids::uuid>(v.id.value());
    r.todays_market_config_id = boost::lexical_cast<boost::uuids::uuid>(v.todays_market_config_id);
    r.todays_market_collection_id =
        boost::lexical_cast<boost::uuids::uuid>(v.todays_market_collection_id);
    r.key_attribute = v.key_attribute;
    r.key_value = v.key_value;
    r.key_value_2 = v.key_value_2.value_or("");
    r.target = v.target;
    r.discounting = v.discounting.value_or("");
    r.position = v.position;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = timestamp_to_timepoint(v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

todays_market_entry_entity todays_market_entry_mapper::map(const domain::todays_market_entry& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    todays_market_entry_entity r;
    r.id = boost::uuids::to_string(v.id);
    r.tenant_id = v.tenant_id.to_string();
    r.version = v.version;
    r.todays_market_config_id = boost::uuids::to_string(v.todays_market_config_id);
    r.todays_market_collection_id = boost::uuids::to_string(v.todays_market_collection_id);
    r.key_attribute = v.key_attribute;
    r.key_value = v.key_value;
    r.key_value_2 = v.key_value_2.empty() ? std::nullopt : std::optional(v.key_value_2);
    r.target = v.target;
    r.discounting = v.discounting.empty() ? std::nullopt : std::optional(v.discounting);
    r.position = v.position;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::todays_market_entry>
todays_market_entry_mapper::map(const std::vector<todays_market_entry_entity>& v) {
    return map_vector<todays_market_entry_entity, domain::todays_market_entry>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<todays_market_entry_entity>
todays_market_entry_mapper::map(const std::vector<domain::todays_market_entry>& v) {
    return map_vector<domain::todays_market_entry, todays_market_entry_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
