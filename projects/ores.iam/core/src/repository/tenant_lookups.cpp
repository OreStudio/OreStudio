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
#include "ores.iam.core/repository/tenant_lookups.hpp"
#include "ores.database/query_builder/execute.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.iam.api/domain/tenant_json_io.hpp" // IWYU pragma: keep.
#include "ores.iam.core/repository/tenant_entity.hpp"
#include "ores.iam.core/repository/tenant_mapper.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>

namespace ores::iam::repository {

namespace query_builder = ores::database::query_builder;
using namespace ores::database::query_builder::literals;
using namespace ores::logging;
using namespace ores::database::repository;

namespace {

inline static std::string_view tenant_lookups_logger_name = "ores.iam.repository.tenant_lookups";

static auto& tenant_lookups_lg() {
    static auto instance = make_logger(tenant_lookups_logger_name);
    return instance;
}

} // namespace

std::vector<domain::tenant> read_all_active_tenants(const ores::database::context& ctx) {
    const auto max(make_timestamp(MAX_TIMESTAMP, tenant_lookups_lg()));
    const auto query = query_builder::read<std::vector<tenant_entity>> |
                       query_builder::where("valid_to"_c == max.value()) | query_builder::order_by("name"_c);

    return query_builder::execute_read_query<tenant_entity, domain::tenant>(
        ctx,
        query,
        [](const auto& entities) { return tenant_mapper::map(entities); },
        tenant_lookups_lg(),
        "Reading latest tenants");
}

std::vector<domain::tenant> read_active_tenant_by_id(const ores::database::context& ctx,
                                                     const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(tenant_lookups_lg(), debug) << "Reading latest tenant. ID: " << id;

    const auto max(make_timestamp(MAX_TIMESTAMP, tenant_lookups_lg()));
    const auto id_str = boost::lexical_cast<std::string>(id);
    const auto query = query_builder::read<std::vector<tenant_entity>> |
                       query_builder::where("id"_c == id_str && "valid_to"_c == max.value());

    return query_builder::execute_read_query<tenant_entity, domain::tenant>(
        ctx,
        query,
        [](const auto& entities) { return tenant_mapper::map(entities); },
        tenant_lookups_lg(),
        "Reading latest tenant by ID.");
}

std::vector<domain::tenant> read_active_tenant_by_hostname(const ores::database::context& ctx,
                                                           const std::string& hostname) {
    BOOST_LOG_SEV(tenant_lookups_lg(), debug) << "Reading latest tenant by hostname: " << hostname;

    const auto max(make_timestamp(MAX_TIMESTAMP, tenant_lookups_lg()));
    const auto query = query_builder::read<std::vector<tenant_entity>> |
                       query_builder::where("hostname"_c == hostname && "valid_to"_c == max.value());

    return query_builder::execute_read_query<tenant_entity, domain::tenant>(
        ctx,
        query,
        [](const auto& entities) { return tenant_mapper::map(entities); },
        tenant_lookups_lg(),
        "Reading latest tenant by hostname.");
}

std::vector<domain::tenant> read_registration_default_tenant(const ores::database::context& ctx) {
    const auto max(make_timestamp(MAX_TIMESTAMP, tenant_lookups_lg()));
    const auto query = query_builder::read<std::vector<tenant_entity>> |
                       query_builder::where("is_registration_default"_c == true && "valid_to"_c == max.value()) |
                       query_builder::limit(1);

    return query_builder::execute_read_query<tenant_entity, domain::tenant>(
        ctx,
        query,
        [](const auto& entities) { return tenant_mapper::map(entities); },
        tenant_lookups_lg(),
        "Reading the registration default tenant.");
}

} // namespace ores::iam::repository
