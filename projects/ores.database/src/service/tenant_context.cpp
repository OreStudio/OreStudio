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
#include "ores.database/service/tenant_context.hpp"

#include <stdexcept>
#include <sqlgen/postgres.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"

namespace ores::database::service {

namespace {

inline static std::string_view logger_name = "ores.database.service.tenant_context";

[[nodiscard]] auto& lg() {
    using namespace ores::logging;
    static auto instance = make_logger(logger_name);
    return instance;
}

}

bool tenant_context::is_uuid(const std::string& str) {
    // Simple UUID format check: 8-4-4-4-12 with hyphens
    return str.size() == 36 &&
        str[8] == '-' && str[13] == '-' &&
        str[18] == '-' && str[23] == '-';
}

std::string tenant_context::lookup_by_code(context& ctx, const std::string& code) {
    using namespace ores::logging;
    using ores::database::repository::execute_parameterized_string_query;

    BOOST_LOG_SEV(lg(), debug) << "Looking up tenant by code: " << code;

    const auto results = execute_parameterized_string_query(ctx,
        "SELECT ores_iam_tenant_by_code_fn($1)::text",
        {code},
        lg(), "Looking up tenant by code");

    if (results.empty()) {
        throw std::runtime_error("No active tenant found with code: " + code);
    }

    BOOST_LOG_SEV(lg(), debug) << "Resolved tenant code '" << code
                               << "' to ID: " << results[0];
    return results[0];
}

std::string tenant_context::lookup_by_hostname(context& ctx,
    const std::string& hostname) {
    using namespace ores::logging;
    using ores::database::repository::execute_parameterized_string_query;

    BOOST_LOG_SEV(lg(), debug) << "Looking up tenant by hostname: " << hostname;

    const auto results = execute_parameterized_string_query(ctx,
        "SELECT ores_iam_tenant_by_hostname_fn($1)::text",
        {hostname},
        lg(), "Looking up tenant by hostname");

    if (results.empty()) {
        throw std::runtime_error("No active tenant found with hostname: " + hostname);
    }

    BOOST_LOG_SEV(lg(), debug) << "Resolved tenant hostname '" << hostname
                               << "' to ID: " << results[0];
    return results[0];
}

std::string tenant_context::lookup_name(context& ctx,
    const std::string& tenant_id) {
    using namespace ores::logging;
    using ores::database::repository::execute_parameterized_string_query;

    BOOST_LOG_SEV(lg(), debug) << "Looking up tenant name by ID: " << tenant_id;

    const auto results = execute_parameterized_string_query(ctx,
        "SELECT ores_iam_tenant_name_by_id_fn($1::uuid)",
        {tenant_id},
        lg(), "Looking up tenant name by ID");

    if (results.empty()) {
        throw std::runtime_error("No active tenant found with ID: " + tenant_id);
    }

    BOOST_LOG_SEV(lg(), debug) << "Resolved tenant ID '" << tenant_id
                               << "' to name: " << results[0];
    return results[0];
}

void tenant_context::set(context& ctx, const std::string& tenant) {
    using namespace ores::logging;

    BOOST_LOG_SEV(lg(), debug) << "Setting tenant context: " << tenant;

    std::string tenant_id;

    if (is_uuid(tenant)) {
        tenant_id = tenant;
        BOOST_LOG_SEV(lg(), debug) << "Using tenant ID directly: " << tenant_id;
    } else {
        tenant_id = lookup_by_code(ctx, tenant);
    }

    // Validate tenant_id is a proper UUID format to prevent injection
    // (is_uuid check ensures 36 chars with hyphens at correct positions)
    if (!is_uuid(tenant_id)) {
        throw std::runtime_error(
            "Invalid tenant ID format (must be UUID): " + tenant_id);
    }

    // Use connection pool to set tenant context on pool connections.
    // UUID validation above ensures this is safe from SQL injection.
    const std::string sql = "SELECT set_config('app.current_tenant_id', '" +
        tenant_id + "', false)";

    const auto execute_stmt = [&sql](auto&& session) {
        return session->execute(sql);
    };

    const auto r = sqlgen::session(ctx.connection_pool()).and_then(execute_stmt);
    if (!r) {
        BOOST_LOG_SEV(lg(), error) << "Failed to set tenant context: "
                                   << r.error().what();
        throw std::runtime_error("Failed to set tenant context: " +
            std::string(r.error().what()));
    }

    // Also store tenant_id in context for repositories to access directly
    ctx.set_tenant_id(tenant_id);

    BOOST_LOG_SEV(lg(), info) << "Tenant context set to: " << tenant_id;
}

void tenant_context::set_system_tenant(context& ctx) {
    set(ctx, system_tenant_id);
}

}
