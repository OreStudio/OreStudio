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
#include "ores.marketdata.core/repository/feed_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.marketdata.api/domain/feed.hpp"
#include "ores.marketdata.api/domain/feed_json_io.hpp" // IWYU pragma: keep.
#include "ores.marketdata.core/repository/feed_entity.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/log/sources/severity_feature.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <vector>

namespace ores::marketdata::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::feed feed_mapper::map(const feed_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::feed r;
    r.version = v.version;
    r.tenant_id = v.tenant_id;
    r.id = v.id;
    r.name = v.name;
    r.publisher_code = v.publisher_code;
    r.session_code = v.session_code;
    r.service = v.service;
    r.provider_key = v.provider_key;
    r.is_default = v.is_default;
    r.description = v.description;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = v.valid_from;

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

feed_entity feed_mapper::map(const domain::feed& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    feed_entity r;
    r.id = v.id;
    r.tenant_id = v.tenant_id;
    r.version = v.version;
    r.name = v.name;
    r.publisher_code = v.publisher_code;
    r.session_code = v.session_code;
    r.service = v.service;
    r.provider_key = v.provider_key;
    r.is_default = v.is_default;
    r.description = v.description;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::feed> feed_mapper::map(const std::vector<feed_entity>& v) {
    return map_vector<feed_entity, domain::feed>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<feed_entity> feed_mapper::map(const std::vector<domain::feed>& v) {
    return map_vector<domain::feed, feed_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
