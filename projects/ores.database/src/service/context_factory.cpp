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
#include "ores.database/service/context_factory.hpp"
#include "ores.database/domain/database_options.hpp"
#include "ores.database/domain/exceptions.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/schema_fingerprint.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <cstdlib>
#include <rfl/json.hpp>
#include <sstream>
#include <stdexcept>

namespace ores::database {

using namespace ores::logging;

namespace {

struct recorded_schema {
    std::string fingerprint;
    std::string provenance;
};

// The newest row of ores_database_info_tbl, which compass db recreate writes.
// A database without the table or the row was not built by compass db
// recreate, and is reported as having no fingerprint.
recorded_schema read_recorded_schema(const context& ctx, logging::logger_t& lg) {
    try {
        const auto rows = repository::execute_raw_multi_column_query(
            ctx,
            "select schema_fingerprint, git_commit, git_date "
            "from ores_database_info_tbl order by created_at desc limit 1",
            lg,
            "Reading the database schema fingerprint");
        if (rows.empty())
            return {"",
                    "has no ores_database_info_tbl row; it was not built by compass db recreate"};
        const auto& row = rows.front();
        return {row[0].value_or(""),
                "was built from commit " + row[1].value_or("?") + " at " + row[2].value_or("?")};
    } catch (const std::exception& e) {
        return {"", std::string("could not be read for its schema fingerprint: ") + e.what()};
    }
}

}

void context_factory::verify_schema_fingerprint(const context& ctx, const std::string& database) {
    const std::string expected = ORES_SCHEMA_FINGERPRINT;
    const auto recorded = read_recorded_schema(ctx, lg());
    const std::string actual = recorded.fingerprint.empty() ? "(none)" : recorded.fingerprint;

    BOOST_LOG_SEV(lg(), info) << "Schema fingerprint expected by this build: " << expected;
    BOOST_LOG_SEV(lg(), info) << "Schema fingerprint recorded in database " << database << ": "
                              << actual << " (database " << recorded.provenance << ")";
    if (recorded.fingerprint == expected) {
        BOOST_LOG_SEV(lg(), info) << "Schema fingerprint check passed.";
        return;
    }

    const std::string rule(78, '=');
    std::ostringstream msg;
    msg << "\n"
        << rule << "\n"
        << "The database schema does not match this build.\n"
        << "  Expected schema fingerprint (this build): " << expected << "\n"
        << "  Actual schema fingerprint (database)    : " << actual << "\n"
        << "  Database " << database << " " << recorded.provenance << ".\n"
        << "  The code and the database were built from different SQL, so a query\n"
        << "  against a table that moved will fail or return wrong results.\n"
        << "  Fix: compass services stop && compass db recreate -y -k\n";
    /*
     * A deployment must not serve a database its code was not built against,
     * and a developer iterating on a checkout that has moved past its database
     * needs the service to run, so the mismatch can be seen working rather than
     * only read about. The strict answer is therefore asked for by setting
     * ORES_REQUIRE_SCHEMA_MATCH, which is what a deployment and continuous
     * integration do; without it the mismatch is a warning and the service
     * starts.
     */
    if (std::getenv("ORES_REQUIRE_SCHEMA_MATCH") != nullptr) {
        msg << "REFUSING TO START: ORES_REQUIRE_SCHEMA_MATCH is set.\n" << rule;
        BOOST_LOG_SEV(lg(), error) << "FATAL: " << msg.str();
        throw schema_mismatch_exception(msg.str());
    }
    msg << "The service starts anyway: a query against a table that moved will\n"
        << "fail and the rest of the product is unaffected. Set\n"
        << "ORES_REQUIRE_SCHEMA_MATCH=1 to refuse instead.\n"
        << rule;
    BOOST_LOG_SEV(lg(), warn) << "WARNING: " << msg.str();
}

std::ostream& operator<<(std::ostream& s, const context_factory::configuration& v) {
    rfl::json::write(v, s);
    return s;
}

context context_factory::make_context(const configuration& cfg) {
    BOOST_LOG_SEV(lg(), debug) << "Creating context. Configuration: " << cfg;

    if (cfg.service_account.empty()) {
        BOOST_LOG_SEV(lg(), error)
            << "FATAL: service_account is not configured. "
            << "All services must set service_account in " << "context_factory::configuration. "
            << "The performed_by audit field cannot be stamped correctly "
            << "without a service account. Service cannot start.";
        throw std::runtime_error("context_factory: service_account must not be empty");
    }

    const auto credentials = to_credentials(cfg.database_options);

    // The underlying pool probes fail fast; the retry policy (num_attempts,
    // wait, backoff shape) is applied by tenant_aware_pool::acquire().
    sqlgen::ConnectionPoolConfig pool_config{
        .size = cfg.pool_size, .num_attempts = 1, .wait_time_in_seconds = 0};

    auto pool_result = make_connection_pool<context::connection_type>(pool_config, credentials);

    if (!pool_result) {
        throw db_connection_exception("Failed to create connection pool: " +
                                      std::string(pool_result.error().what()));
    }

    pool_acquire_policy policy{.num_attempts = cfg.num_attempts,
                               .wait_time_in_seconds = cfg.wait_time_in_seconds,
                               .strategy = cfg.database_options.pool_backoff == "linear" ?
                                               pool_backoff_strategy::linear :
                                               pool_backoff_strategy::exponential};

    // Convert string tenant to tenant_id, defaulting to system tenant
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();
    if (!cfg.database_options.tenant.empty()) {
        auto tenant_result = utility::uuid::tenant_id::from_string(cfg.database_options.tenant);
        if (!tenant_result) {
            throw std::runtime_error("Invalid tenant ID in configuration: " +
                                     tenant_result.error());
        }
        tenant_id = *tenant_result;
    }

    context r(std::move(*pool_result),
              credentials,
              std::move(tenant_id),
              /*actor=*/"",
              cfg.service_account,
              policy);

    verify_schema_fingerprint(r, cfg.database_options.database);

    BOOST_LOG_SEV(lg(), debug) << "Finished creating context.";
    return r;
}

}
