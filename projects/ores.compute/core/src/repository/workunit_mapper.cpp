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
#include "ores.compute.core/repository/workunit_mapper.hpp"

#include <boost/uuid/uuid_io.hpp>
#include <boost/lexical_cast.hpp>
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.compute.api/domain/workunit_json_io.hpp" // IWYU pragma: keep.

namespace ores::compute::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::workunit
workunit_mapper::map(const workunit_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::workunit r;
    r.version = v.version;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.id = boost::lexical_cast<boost::uuids::uuid>(v.id.value());
    r.batch_id = boost::lexical_cast<boost::uuids::uuid>(v.batch_id);
    r.app_version_id = boost::lexical_cast<boost::uuids::uuid>(v.app_version_id);
    r.input_uri = v.input_uri;
    r.config_uri = v.config_uri.value_or("");
    r.priority = v.priority;
    r.target_redundancy = v.target_redundancy;
    r.canonical_result_id = v.canonical_result_id.has_value() ? boost::lexical_cast<boost::uuids::uuid>(*v.canonical_result_id) : boost::uuids::uuid{};
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    if (!v.valid_from)
        throw std::logic_error("Cannot map entity with null valid_from to domain object.");
    r.recorded_at = timestamp_to_timepoint(*v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

workunit_entity
workunit_mapper::map(const domain::workunit& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    workunit_entity r;
    r.id = boost::uuids::to_string(v.id);
    r.tenant_id = v.tenant_id.to_string();
    r.version = v.version;
    r.batch_id = boost::uuids::to_string(v.batch_id);
    r.app_version_id = boost::uuids::to_string(v.app_version_id);
    r.input_uri = v.input_uri;
    r.config_uri = v.config_uri.empty() ? std::nullopt : std::optional(v.config_uri);
    r.priority = v.priority;
    r.target_redundancy = v.target_redundancy;
    r.canonical_result_id = (v.canonical_result_id == boost::uuids::uuid{}) ? std::nullopt : std::optional(boost::uuids::to_string(v.canonical_result_id));
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::workunit>
workunit_mapper::map(const std::vector<workunit_entity>& v) {
    return map_vector<workunit_entity, domain::workunit>(
        v,
        [](const auto& ve) { return map(ve); },
        lg(),
        "db entities");
}

std::vector<workunit_entity>
workunit_mapper::map(const std::vector<domain::workunit>& v) {
    return map_vector<domain::workunit, workunit_entity>(
        v,
        [](const auto& ve) { return map(ve); },
        lg(),
        "domain entities");
}

}
