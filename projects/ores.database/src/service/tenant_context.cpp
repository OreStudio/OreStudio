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

std::string tenant_context::lookup_by_code(const context& ctx,
    const std::string& code) {
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

std::string tenant_context::lookup_by_hostname(const context& ctx,
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

std::string tenant_context::lookup_name(const context& ctx,
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

namespace {

using namespace ores::logging;

/**
 * @brief Resolves a tenant identifier to a UUID string.
 *
 * If the tenant is already a UUID, returns it directly.
 * Otherwise, looks up the tenant by code.
 */
std::string resolve_tenant_id(const context& ctx, const std::string& tenant) {
    if (tenant_context::is_uuid(tenant)) {
        BOOST_LOG_SEV(lg(), debug) << "Using tenant ID directly: " << tenant;
        return tenant;
    }

    auto tenant_id = tenant_context::lookup_by_code(ctx, tenant);

    // Validate tenant_id is a proper UUID format to prevent injection
    if (!tenant_context::is_uuid(tenant_id)) {
        throw std::runtime_error(
            "Invalid tenant ID format (must be UUID): " + tenant_id);
    }

    return tenant_id;
}

/**
 * @brief Verifies that a tenant context works by acquiring a connection.
 */
void verify_tenant_context(context& ctx, const std::string& tenant_id) {
    const auto r = sqlgen::session(ctx.connection_pool());
    if (!r) {
        BOOST_LOG_SEV(lg(), error) << "Failed to verify tenant context: "
                                   << r.error().what();
        throw std::runtime_error("Failed to set tenant context: " +
            std::string(r.error().what()));
    }
    BOOST_LOG_SEV(lg(), info) << "Tenant context set to: " << tenant_id;
}

}  // anonymous namespace

context tenant_context::with_tenant(const context& ctx, const std::string& tenant) {
    using namespace ores::logging;

    BOOST_LOG_SEV(lg(), debug) << "Creating context with tenant: " << tenant;

    // Resolve tenant code to UUID if needed (now accepts const context)
    const auto tenant_id = resolve_tenant_id(ctx, tenant);

    // Create the new context with the resolved tenant ID
    auto result = ctx.with_tenant(tenant_id);

    // Verify the tenant context works
    verify_tenant_context(result, tenant_id);

    return result;
}

context tenant_context::with_system_tenant(const context& ctx) {
    return with_tenant(ctx, system_tenant_id);
}

}
