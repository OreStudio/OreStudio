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
#include "ores.analytics.core/repository/todays_market_collection_kind_mapper.hpp"
#include "ores.analytics.api/domain/todays_market_collection_kind_json_io.hpp" // IWYU pragma: keep.
#include "ores.database/repository/mapper_helpers.hpp"

namespace ores::analytics::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::todays_market_collection_kind
todays_market_collection_kind_mapper::map(const todays_market_collection_kind_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::todays_market_collection_kind r;
    r.version = v.version;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.code = v.code.value();
    r.entry_element = v.entry_element;
    r.key_attribute = v.key_attribute;
    r.key_attribute_2 = v.key_attribute_2;
    r.description = v.description.value_or("");
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = timestamp_to_timepoint(v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

todays_market_collection_kind_entity
todays_market_collection_kind_mapper::map(const domain::todays_market_collection_kind& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    todays_market_collection_kind_entity r;
    r.code = v.code;
    r.tenant_id = v.tenant_id.to_string();
    r.version = v.version;
    r.entry_element = v.entry_element;
    r.key_attribute = v.key_attribute;
    r.key_attribute_2 = v.key_attribute_2;
    r.description = v.description.empty() ? std::nullopt : std::optional(v.description);
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::todays_market_collection_kind> todays_market_collection_kind_mapper::map(
    const std::vector<todays_market_collection_kind_entity>& v) {
    return map_vector<todays_market_collection_kind_entity, domain::todays_market_collection_kind>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<todays_market_collection_kind_entity> todays_market_collection_kind_mapper::map(
    const std::vector<domain::todays_market_collection_kind>& v) {
    return map_vector<domain::todays_market_collection_kind, todays_market_collection_kind_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
