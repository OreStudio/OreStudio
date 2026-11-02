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
#include "ores.trading.core/repository/product_type_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.trading.api/domain/product_type.hpp"
#include "ores.trading.api/domain/product_type_json_io.hpp" // IWYU pragma: keep.
#include "ores.trading.core/repository/product_type_entity.hpp"
#include <boost/log/sources/severity_feature.hpp>
#include <optional>
#include <vector>

namespace ores::trading::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::product_type product_type_mapper::map(const product_type_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::product_type r;
    r.version = v.version;
    r.tenant_id = v.tenant_id;
    r.code = v.code;
    r.name = v.name;
    r.description = v.description.value_or("");
    r.source = v.source.value_or("");
    r.source_ref = v.source_ref.value_or("");
    r.product_family_code = v.product_family_code.value_or("");
    r.construction = v.construction;
    r.has_options = v.has_options;
    r.has_extension = v.has_extension;
    r.instrument_table = v.instrument_table.value_or("");
    r.display_order = v.display_order;
    r.knowledge_note = v.knowledge_note.value_or("");
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = v.valid_from;

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

product_type_entity product_type_mapper::map(const domain::product_type& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    product_type_entity r;
    r.code = v.code;
    r.tenant_id = v.tenant_id;
    r.version = v.version;
    r.name = v.name;
    r.description = v.description.empty() ? std::nullopt : std::optional(v.description);
    r.source = v.source.empty() ? std::nullopt : std::optional(v.source);
    r.source_ref = v.source_ref.empty() ? std::nullopt : std::optional(v.source_ref);
    r.product_family_code =
        v.product_family_code.empty() ? std::nullopt : std::optional(v.product_family_code);
    r.construction = v.construction;
    r.has_options = v.has_options;
    r.has_extension = v.has_extension;
    r.instrument_table =
        v.instrument_table.empty() ? std::nullopt : std::optional(v.instrument_table);
    r.display_order = v.display_order;
    r.knowledge_note = v.knowledge_note.empty() ? std::nullopt : std::optional(v.knowledge_note);
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::product_type>
product_type_mapper::map(const std::vector<product_type_entity>& v) {
    return map_vector<product_type_entity, domain::product_type>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<product_type_entity>
product_type_mapper::map(const std::vector<domain::product_type>& v) {
    return map_vector<domain::product_type, product_type_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
