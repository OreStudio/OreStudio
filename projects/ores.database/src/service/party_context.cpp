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
#include "ores.database/service/party_context.hpp"

#include <stdexcept>
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"

namespace ores::database::service {

namespace {

inline static std::string_view logger_name = "ores.database.service.party_context";

[[nodiscard]] auto& lg() {
    using namespace ores::logging;
    static auto instance = make_logger(logger_name);
    return instance;
}

/**
 * @brief Parses a PostgreSQL UUID array literal into a vector of UUIDs.
 *
 * Expects the format returned by ores_refdata_visible_party_ids_fn:
 * {uuid1,uuid2,...} or an empty result for a missing party.
 */
std::vector<boost::uuids::uuid> parse_uuid_array(const std::string& raw) {
    std::vector<boost::uuids::uuid> result;
    if (raw.empty() || raw == "{}" || raw == "NULL")
        return result;

    // Strip surrounding braces
    const auto start = raw.find('{');
    const auto end = raw.rfind('}');
    if (start == std::string::npos || end == std::string::npos)
        return result;

    boost::uuids::string_generator gen;
    std::string token;
    for (std::size_t i = start + 1; i <= end; ++i) {
        const char c = raw[i];
        if (c == ',' || c == '}') {
            if (!token.empty()) {
                result.push_back(gen(token));
                token.clear();
            }
        } else {
            token += c;
        }
    }
    return result;
}

} // anonymous namespace

context party_context::with_party(const context& ctx,
    const utility::uuid::tenant_id& tenant,
    const boost::uuids::uuid& party,
    const std::string& actor) {
    using namespace ores::logging;
    using ores::database::repository::execute_parameterized_string_query;

    const auto tenant_str = tenant.to_string();
    const auto party_str = boost::uuids::to_string(party);

    BOOST_LOG_SEV(lg(), debug) << "Resolving visible party set for party: "
                               << party_str << " in tenant: " << tenant_str;

    const auto rows = execute_parameterized_string_query(ctx,
        "SELECT ores_refdata_visible_party_ids_fn($1::uuid, $2::uuid)::text",
        {tenant_str, party_str},
        lg(), "Resolving visible party IDs");

    if (rows.empty() || rows[0].empty()) {
        throw std::runtime_error(
            "Party not found: " + party_str + " in tenant " + tenant_str);
    }

    const auto visible_ids = parse_uuid_array(rows[0]);
    if (visible_ids.empty()) {
        throw std::runtime_error(
            "Party not found: " + party_str + " in tenant " + tenant_str);
    }

    BOOST_LOG_SEV(lg(), info) << "Party context: " << party_str
                              << " (" << visible_ids.size() << " visible parties)";

    return ctx.with_party(tenant, party, visible_ids, actor);
}

}
