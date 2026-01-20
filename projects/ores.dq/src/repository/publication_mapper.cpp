/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.dq/repository/publication_mapper.hpp"

#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include "ores.database/repository/mapper_helpers.hpp"

namespace ores::dq::repository {

using namespace ores::logging;
using namespace ores::database::repository;

std::ostream& operator<<(std::ostream& s, const publication_entity& v) {
    s << "publication_entity{id=" << v.id.value()
      << ", dataset_code=" << v.dataset_code << "}";
    return s;
}

domain::publication publication_mapper::map(const publication_entity& entity) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << entity;

    domain::publication result;

    boost::uuids::string_generator gen;
    result.id = gen(entity.id.value());
    result.dataset_id = gen(entity.dataset_id);
    result.dataset_code = entity.dataset_code;
    result.mode = domain::publication_mode_from_string(entity.mode)
        .value_or(domain::publication_mode::upsert);
    result.target_table = entity.target_table;
    result.records_inserted = static_cast<std::uint64_t>(entity.records_inserted);
    result.records_updated = static_cast<std::uint64_t>(entity.records_updated);
    result.records_skipped = static_cast<std::uint64_t>(entity.records_skipped);
    result.records_deleted = static_cast<std::uint64_t>(entity.records_deleted);
    result.published_by = entity.published_by;
    result.published_at = timestamp_to_timepoint(entity.published_at);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity.";

    return result;
}

std::vector<domain::publication>
publication_mapper::map(const std::vector<publication_entity>& entities) {
    return map_vector<publication_entity, domain::publication>(
        entities,
        [](const auto& e) { return map(e); },
        lg(),
        "publication entities");
}

publication_entity publication_mapper::to_entity(const domain::publication& domain) {
    publication_entity entity;

    entity.id = boost::uuids::to_string(domain.id);
    entity.dataset_id = boost::uuids::to_string(domain.dataset_id);
    entity.dataset_code = domain.dataset_code;
    entity.mode = to_string(domain.mode);
    entity.target_table = domain.target_table;
    entity.records_inserted = static_cast<std::int64_t>(domain.records_inserted);
    entity.records_updated = static_cast<std::int64_t>(domain.records_updated);
    entity.records_skipped = static_cast<std::int64_t>(domain.records_skipped);
    entity.records_deleted = static_cast<std::int64_t>(domain.records_deleted);
    entity.published_by = domain.published_by;
    entity.published_at = timepoint_to_timestamp(domain.published_at, lg());

    return entity;
}

}
