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
#include "ores.analytics.core/service/todays_market_document_service.hpp"
#include "ores.analytics.core/repository/todays_market_collection_repository.hpp"
#include "ores.analytics.core/repository/todays_market_config_repository.hpp"
#include "ores.analytics.core/repository/todays_market_configuration_binding_repository.hpp"
#include "ores.analytics.core/repository/todays_market_configuration_repository.hpp"
#include "ores.analytics.core/repository/todays_market_entry_repository.hpp"
#include "ores.database/repository/document_operations.hpp"
#include <set>

namespace ores::analytics::service {

using namespace ores::analytics::repository;
using ores::database::repository::read_one;
using ores::database::repository::read_where;
using ores::database::repository::stamp_party;

todays_market_document_service::todays_market_document_service(context ctx)
    : ctx_(std::move(ctx)) {}

void todays_market_document_service::save(domain::todays_market_document v) {
    stamp_party(ctx_, v);
    todays_market_config_repository().write(ctx_, v.config);
    todays_market_collection_repository().write(ctx_, v.collections);
    todays_market_entry_repository().write(ctx_, v.entries);
    todays_market_configuration_repository().write(ctx_, v.configurations);
    todays_market_configuration_binding_repository().write(ctx_, v.bindings);
}

domain::todays_market_document
todays_market_document_service::get(const boost::uuids::uuid& config_id) {
    domain::todays_market_document r;
    r.config =
        read_one(ctx_, todays_market_config_repository(), "today's market document", config_id);
    const auto of_config = [&](const auto& row) {
        return row.todays_market_config_id == config_id;
    };
    r.collections = read_where(ctx_, todays_market_collection_repository(), of_config);
    r.entries = read_where(ctx_, todays_market_entry_repository(), of_config);
    r.configurations = read_where(ctx_, todays_market_configuration_repository(), of_config);
    std::set<boost::uuids::uuid> configurations;
    for (const auto& c : r.configurations)
        configurations.insert(c.id);
    r.bindings =
        read_where(ctx_, todays_market_configuration_binding_repository(), [&](const auto& row) {
            return configurations.contains(row.todays_market_configuration_id);
        });
    return r;
}

std::optional<boost::uuids::uuid>
todays_market_document_service::find_by_configuration(const boost::uuids::uuid& configuration_id) {
    for (const auto& h : todays_market_config_repository().read_latest(ctx_))
        if (h.configuration_id == configuration_id)
            return h.id;
    return std::nullopt;
}

}
