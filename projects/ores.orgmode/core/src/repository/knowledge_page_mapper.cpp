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
#include "ores.orgmode.core/repository/knowledge_page_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.orgmode.api/domain/knowledge_page.hpp"
#include "ores.orgmode.api/domain/knowledge_page_json_io.hpp" // IWYU pragma: keep.
#include "ores.orgmode.core/repository/knowledge_page_entity.hpp"
#include <vector>

namespace ores::orgmode::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::knowledge_page knowledge_page_mapper::map(const knowledge_page_entity& v) {
    ORES_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::knowledge_page r;
    r.version = v.version;
    r.tenant_id = v.tenant_id;
    r.id = v.id;
    r.path = v.path;
    r.org = v.org;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = v.valid_from;

    ORES_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

knowledge_page_entity knowledge_page_mapper::map(const domain::knowledge_page& v) {
    ORES_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    knowledge_page_entity r;
    r.id = v.id;
    r.tenant_id = v.tenant_id;
    r.version = v.version;
    r.path = v.path;
    r.org = v.org;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    ORES_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::knowledge_page>
knowledge_page_mapper::map(const std::vector<knowledge_page_entity>& v) {
    return map_vector<knowledge_page_entity, domain::knowledge_page>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<knowledge_page_entity>
knowledge_page_mapper::map(const std::vector<domain::knowledge_page>& v) {
    return map_vector<domain::knowledge_page, knowledge_page_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
