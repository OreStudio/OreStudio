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
#include "ores.dq.core/repository/publication_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.dq.api/domain/publication_json_io.hpp" // IWYU pragma: keep.
#include "ores.platform/time/datetime.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <chrono>
#include <format>
#include <sstream>

namespace ores::dq::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::publication publication_mapper::map(const publication_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::publication r;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.id = boost::lexical_cast<boost::uuids::uuid>(v.id.value());
    r.dataset_id = boost::lexical_cast<boost::uuids::uuid>(v.dataset_id);
    r.dataset_code = v.dataset_code;
    r.mode = v.mode;
    r.target_table = v.target_table;
    r.records_inserted = v.records_inserted;
    r.records_updated = v.records_updated;
    r.records_skipped = v.records_skipped;
    r.records_deleted = v.records_deleted;
    r.published_by = v.published_by;
    r.published_at = timestamp_to_timepoint(std::string_view{v.published_at});

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

publication_entity publication_mapper::map(const domain::publication& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    publication_entity r;
    r.id = boost::uuids::to_string(v.id);
    r.tenant_id = v.tenant_id.to_string();
    r.dataset_id = boost::uuids::to_string(v.dataset_id);
    r.dataset_code = v.dataset_code;
    r.mode = v.mode;
    r.target_table = v.target_table;
    r.records_inserted = v.records_inserted;
    r.records_updated = v.records_updated;
    r.records_skipped = v.records_skipped;
    r.records_deleted = v.records_deleted;
    r.published_by = v.published_by;
    r.published_at = ores::platform::time::datetime::to_iso8601_utc(v.published_at);

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::publication> publication_mapper::map(const std::vector<publication_entity>& v) {
    return map_vector<publication_entity, domain::publication>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<publication_entity> publication_mapper::map(const std::vector<domain::publication>& v) {
    return map_vector<domain::publication, publication_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
