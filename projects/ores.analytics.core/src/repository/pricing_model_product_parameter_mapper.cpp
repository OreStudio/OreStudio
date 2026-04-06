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
#include "ores.analytics.core/repository/pricing_model_product_parameter_mapper.hpp"

#include <boost/uuid/uuid_io.hpp>
#include <boost/lexical_cast.hpp>
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.analytics.api/domain/pricing_model_product_parameter_json_io.hpp" // IWYU pragma: keep.

namespace ores::analytics::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::pricing_model_product_parameter
pricing_model_product_parameter_mapper::map(
    const pricing_model_product_parameter_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::pricing_model_product_parameter r;
    r.version = v.version;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.id = boost::lexical_cast<boost::uuids::uuid>(v.id.value());
    r.pricing_model_config_id =
        boost::lexical_cast<boost::uuids::uuid>(v.pricing_model_config_id);
    r.pricing_model_product_id = v.pricing_model_product_id.has_value()
        ? std::optional(boost::lexical_cast<boost::uuids::uuid>(
            *v.pricing_model_product_id))
        : std::nullopt;
    r.parameter_scope = v.parameter_scope;
    r.parameter_name = v.parameter_name;
    r.parameter_value = v.parameter_value;
    r.modified_by = v.modified_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    if (!v.valid_from)
        throw std::logic_error("Cannot map entity with null valid_from to domain object.");
    r.recorded_at = timestamp_to_timepoint(*v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

pricing_model_product_parameter_entity
pricing_model_product_parameter_mapper::map(
    const domain::pricing_model_product_parameter& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    pricing_model_product_parameter_entity r;
    r.id = boost::uuids::to_string(v.id);
    r.tenant_id = v.tenant_id.to_string();
    r.version = v.version;
    r.pricing_model_config_id = boost::uuids::to_string(v.pricing_model_config_id);
    r.pricing_model_product_id = v.pricing_model_product_id.has_value()
        ? std::optional(boost::uuids::to_string(*v.pricing_model_product_id))
        : std::nullopt;
    r.parameter_scope = v.parameter_scope;
    r.parameter_name = v.parameter_name;
    r.parameter_value = v.parameter_value;
    r.modified_by = v.modified_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::pricing_model_product_parameter>
pricing_model_product_parameter_mapper::map(
    const std::vector<pricing_model_product_parameter_entity>& v) {
    return map_vector<
        pricing_model_product_parameter_entity,
        domain::pricing_model_product_parameter>(
        v,
        [](const auto& ve) { return map(ve); },
        lg(),
        "db entities");
}

std::vector<pricing_model_product_parameter_entity>
pricing_model_product_parameter_mapper::map(
    const std::vector<domain::pricing_model_product_parameter>& v) {
    return map_vector<
        domain::pricing_model_product_parameter,
        pricing_model_product_parameter_entity>(
        v,
        [](const auto& ve) { return map(ve); },
        lg(),
        "domain entities");
}

}
