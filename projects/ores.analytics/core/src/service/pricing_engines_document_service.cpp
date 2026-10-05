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
#include "ores.analytics.core/service/pricing_engines_document_service.hpp"
#include "ores.analytics.core/repository/pricing_model_config_repository.hpp"
#include "ores.analytics.core/repository/pricing_model_product_parameter_repository.hpp"
#include "ores.analytics.core/repository/pricing_model_product_repository.hpp"
#include "ores.database/repository/document_operations.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <set>

namespace ores::analytics::service {

using namespace ores::analytics::repository;
using ores::database::repository::ids_of;
using ores::database::repository::read_one;
using ores::database::repository::read_where;
using ores::database::repository::stamp_party;

pricing_engines_document_service::pricing_engines_document_service(context ctx)
    : ctx_(std::move(ctx)) {}

void pricing_engines_document_service::save(domain::pricing_engines_document v) {
    stamp_party(ctx_, v);
    pricing_model_config_repository().write(ctx_, v.config);
    pricing_model_product_repository().write(ctx_, v.products);
    pricing_model_product_parameter_repository().write(ctx_, v.parameters);
}

domain::pricing_engines_document
pricing_engines_document_service::get(const boost::uuids::uuid& config_id) {
    domain::pricing_engines_document r;
    r.config =
        read_one(ctx_, pricing_model_config_repository(), "pricing engines document", config_id);
    const auto of_config = [&](const auto& row) {
        return row.pricing_model_config_id == config_id;
    };
    r.products = read_where(ctx_, pricing_model_product_repository(), of_config);
    r.parameters = read_where(ctx_, pricing_model_product_parameter_repository(), of_config);
    return r;
}

void pricing_engines_document_service::remove(const boost::uuids::uuid& id) {
    const auto d = get(id);
    if (!d.parameters.empty())
        pricing_model_product_parameter_repository().remove(ctx_, ids_of(d.parameters));
    if (!d.products.empty())
        pricing_model_product_repository().remove(ctx_, ids_of(d.products));
    pricing_model_config_repository().remove(ctx_, boost::uuids::to_string(d.config.id));
}

std::optional<boost::uuids::uuid> pricing_engines_document_service::find_by_configuration(
    const boost::uuids::uuid& configuration_id) {
    for (const auto& h : pricing_model_config_repository().read_latest(ctx_))
        if (h.configuration_id == configuration_id)
            return h.id;
    return std::nullopt;
}

}
