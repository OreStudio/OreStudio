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
#ifndef ORES_SERVICE_MESSAGING_APPROVAL_GATE_HPP
#define ORES_SERVICE_MESSAGING_APPROVAL_GATE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.logging/make_logger.hpp"
#include <stdexcept>
#include <string>
#include <vector>

namespace ores::service::messaging {

/**
 * @brief What the approval policy requires of a write, and whether it is met.
 */
enum class gate_verdict {
    /** The policy names no part for the act. The write goes ahead. */
    free,
    /** The policy names a part, and the write carries out an approved request. */
    approved,
    /** The policy names a part, and the write carries out no approved request. */
    required
};

/**
 * @brief How a gated entity's service lets a write through the gate.
 *
 * A service holds one pass. By default it holds none, so a gated write is
 * refused. The pass is set in process by the code that owns the right to set
 * it, never by a field of a request a client sends: the applier names the
 * approved request whose lines it carries out, and the preview marks its writes
 * as a rehearsal because it rolls them back.
 */
struct write_pass {
    /** The approved request whose line this write carries out. */
    std::string request_id;
    /** True when the write runs in a transaction that is never committed. */
    bool rehearsal = false;
};

/**
 * @brief Asks the policy what a write needs.
 *
 * The question is one query, so the policy and the request's state are read
 * together. The caller's tenant and party are set on the connection, so the
 * tenant's own policy rows count as well as the system's.
 *
 * @param entity_type The entity the policy names, such as @c book.
 * @param operation @c put or @c delete.
 * @param columns The columns the write changes. A write that creates a row
 *        changes every column. A name is a plain identifier from the model:
 *        the list is built into an array literal without escaping.
 * @param request_id The approved request the write carries out, or empty.
 */
inline gate_verdict gate_verdict_of(const ores::database::context& ctx,
                                    const std::string& entity_type,
                                    const std::string& operation,
                                    const std::vector<std::string>& columns,
                                    const std::string& request_id) {
    static auto lg = ores::logging::make_logger("ores.service.messaging.approval_gate");

    std::string list = "{";
    for (std::size_t i = 0; i < columns.size(); ++i)
        list += (i ? ",\"" : "\"") + columns[i] + "\"";
    list += "}";

    const auto rows = ores::database::repository::execute_parameterized_multi_column_query(
        ctx,
        "select ores_inbox_gate_fn($1, $2, $3::text[], nullif($4, ''))",
        {entity_type, operation, list, request_id},
        lg,
        "Asking the approval policy what a write needs");
    if (rows.empty() || rows.front().empty() || !rows.front().front())
        throw std::runtime_error("The approval gate gave no answer.");

    const auto& word = *rows.front().front();
    if (word == "free")
        return gate_verdict::free;
    if (word == "approved")
        return gate_verdict::approved;
    return gate_verdict::required;
}

}

#endif
