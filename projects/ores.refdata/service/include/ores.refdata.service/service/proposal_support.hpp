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
#ifndef ORES_REFDATA_SERVICE_SERVICE_PROPOSAL_SUPPORT_HPP
#define ORES_REFDATA_SERVICE_SERVICE_PROPOSAL_SUPPORT_HPP

#include "ores.database/domain/context.hpp"
#include "ores.database/domain/context_transact.hpp"
#include "ores.database/pq/store_error.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <stdexcept>
#include <string>
#include <vector>

/**
 * What every proposal service of the refdata service shares: running a
 * statement on the connection of the transaction a preview or an apply holds,
 * asking the approval policy which parts decide an act, and reading a refusal
 * out of a result.
 */
namespace ores::refdata::service::support {

/**
 * @brief Runs a statement on the connection of the context's transaction.
 *
 * The raw command helper opens a connection of its own, so a savepoint it
 * opened would not exist on the one the writes run on.
 */
inline void in_transaction(const ores::database::context& ctx, const std::string& sql) {
    if (!ctx.active_transaction())
        throw std::logic_error(
            "A statement for the open transaction needs a context bound to one.");
    try {
        const ores::database::pq::text_statement statement({}, sql);
        ores::database::transact(
            ctx, [&](ores::database::pq::transaction& tx) { tx.run(statement, {}); });
    } catch (const ores::database::pq::store_error& e) {
        throw std::runtime_error(e.what());
    }
}

/**
 * @brief The statement that locks a request row for the rest of the transaction.
 *
 * The id is parsed as a uuid and written back in its canonical form, so nothing
 * but a uuid reaches the statement text. A value that is not a uuid throws.
 */
inline std::string lock_request_sql(const std::string& request_id) {
    const auto id = boost::uuids::to_string(boost::uuids::string_generator{}(request_id));
    return "select id from ores_inbox_approval_requests_tbl where id = '" + id +
           "'::uuid and valid_to = ores_utility_infinity_timestamp_fn() for update";
}

/**
 * @brief The parts the policy names for an act, read as the caller.
 *
 * The helper opens its own connection and sets the caller's tenant and party
 * on it, so the tenant's own policy rows are read as well as the system's. Every
 * value is a bound parameter: the operation comes from the caller.
 */
inline std::vector<std::string> policy_parts(const ores::database::context& ctx,
                                             const std::string& entity_type,
                                             const std::string& operation,
                                             const std::vector<std::string>& cols) {
    static auto lg = ores::logging::make_logger("ores.refdata.service.proposal_support");

    std::string list = "{";
    for (std::size_t i = 0; i < cols.size(); ++i)
        list += (i ? ",\"" : "\"") + cols[i] + "\"";
    list += "}";

    std::vector<std::string> codes;
    const auto rows = ores::database::repository::execute_parameterized_multi_column_query(
        ctx,
        "select p from ores_inbox_policy_parts_fn($1, $2, $3::text[]) as p",
        {entity_type, operation, list},
        lg,
        "Reading the parts the policy names");
    for (const auto& row : rows) {
        if (!row.empty() && row[0])
            codes.push_back(*row[0]);
    }
    return codes;
}

/**
 * @brief The refusal a result carries, or empty when the write stood.
 */
inline std::string refusal_of(const ores::utility::domain::result& r) {
    if (r.outcome == ores::utility::domain::outcome::ok)
        return {};
    return r.message.empty() ? r.code : r.message;
}

}

#endif
