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
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.iam.api/domain/tenant_json_io.hpp" // IWYU pragma: keep.
#include "ores.iam.core/repository/tenant_entity.hpp"
#include "ores.iam.core/repository/tenant_mapper.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <sqlgen/postgres.hpp>

namespace ores::iam::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

namespace {

inline static std::string_view logger_name = "ores.iam.repository.tenant_lookups";

static auto& lg() {
    static auto instance = make_logger(logger_name);
    return instance;
}

} // namespace

std::vector<domain::tenant> read_all_active_tenants(const ores::database::context& ctx) {
    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<tenant_entity>> |
                       where("valid_to"_c == max.value()) | order_by("name"_c);

    return execute_read_query<tenant_entity, domain::tenant>(
        ctx,
        query,
        [](const auto& entities) { return tenant_mapper::map(entities); },
        lg(),
        "Reading latest tenants");
}

std::vector<domain::tenant> read_active_tenant_by_id(const ores::database::context& ctx,
                                                     const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest tenant. ID: " << id;

    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto id_str = boost::lexical_cast<std::string>(id);
    const auto query = sqlgen::read<std::vector<tenant_entity>> |
                       where("id"_c == id_str && "valid_to"_c == max.value());

    return execute_read_query<tenant_entity, domain::tenant>(
        ctx,
        query,
        [](const auto& entities) { return tenant_mapper::map(entities); },
        lg(),
        "Reading latest tenant by ID.");
}

std::vector<domain::tenant> read_active_tenant_by_hostname(const ores::database::context& ctx,
                                                           const std::string& hostname) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest tenant by hostname: " << hostname;

    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<tenant_entity>> |
                       where("hostname"_c == hostname && "valid_to"_c == max.value());

    return execute_read_query<tenant_entity, domain::tenant>(
        ctx,
        query,
        [](const auto& entities) { return tenant_mapper::map(entities); },
        lg(),
        "Reading latest tenant by hostname.");
}

} // namespace ores::iam::repository
