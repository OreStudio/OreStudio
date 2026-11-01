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
#ifndef ORES_REFDATA_API_WORKFLOW_COUNTERPARTY_APPLY_WORKFLOW_HPP
#define ORES_REFDATA_API_WORKFLOW_COUNTERPARTY_APPLY_WORKFLOW_HPP

#include "ores.refdata.api/export.hpp"
#include "ores.utility/codec/member_count.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include <string>
#include <string_view>
#include <tuple>

namespace ores::workflow::service {
class workflow_registry;
}

namespace ores::refdata::workflow {

inline constexpr std::string_view counterparty_apply_workflow_type = "counterparty_apply_workflow";
inline constexpr std::string_view recheck_counterparty_changes_subject =
    "refdata.v1.steps.recheck_counterparty_changes";
inline constexpr std::string_view apply_counterparty_changes_subject =
    "refdata.v1.steps.apply_counterparty_changes";

/**
 * @brief The request that starts a counterparty apply: the approved request to apply.
 */
struct counterparty_apply_workflow_request {
    std::string request_id;
};

[[nodiscard]] constexpr auto wire_fields(const counterparty_apply_workflow_request&) {
    return std::tuple{
        utility::codec::field{"request_id", &counterparty_apply_workflow_request::request_id}};
}

static_assert(::ores::utility::codec::names_every_member<counterparty_apply_workflow_request>(),
              "counterparty_apply_workflow_request has a member its wire shape does not name");

/**
 * @brief Registers the counterparty_apply_workflow definition.
 *
 * Three steps, each idempotent and each with a deadline:
 *  - recheck reads the stored lines against the live rows, because the rows
 *    may have moved since the raise;
 *  - apply writes every line in one transaction and marks each applied once;
 *  - tell says how it ended to the person who asked.
 *
 * The policy is stop. The work that matters is one transaction that either
 * happened or did not, so the recovery wanted is a retry and not a rollback. A
 * refusal ends the request in apply_failed and is never retried. A failure of
 * the infrastructure leaves the request approved and the run stopped, for the
 * engine's retry verb to resume.
 */
ORES_REFDATA_API_EXPORT void
register_counterparty_apply_workflow(ores::workflow::service::workflow_registry& registry);

}

#endif
