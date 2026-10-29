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
#include "ores.marketdata.service/app/curve_withdrawal.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.inbox.api/domain/approval_request.hpp"
#include "ores.inbox.core/service/approval_lifecycle.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.core/repository/curve_publish_repository.hpp"
#include "ores.marketdata.service/app/curve_notices.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/string_generator.hpp>
#include <chrono>
#include <optional>
#include <stdexcept>
#include <string>
#include <string_view>

namespace ores::marketdata::service::app {

using namespace ores::logging;

namespace {

inline std::string_view curve_withdrawal_logger_name = "ores.marketdata.service.curve_withdrawal";

[[nodiscard]] auto& curve_withdrawal_lg() {
    static auto instance = make_logger(curve_withdrawal_logger_name);
    return instance;
}

/// A refusal the caller reads, in the words the person at the screen reads.
messaging::withdraw_curve_response refuse(std::string why) {
    messaging::withdraw_curve_response response;
    response.success = false;
    response.message = std::move(why);
    return response;
}

/**
 * @brief Whether an approved withdrawal request already ended a set.
 *
 * The single-use rule the publish gate keeps is kept here too: a decision
 * authorises one act, so a request that ended one set ends no other. The read is
 * over the end facts and not over a count, so a record that ended without a
 * request is not mistaken for one that named this request.
 */
bool request_already_ended_a_set(const ores::database::context& ctx,
                                 const std::string& request_id) {
    const auto rows = ores::database::repository::execute_parameterized_multi_column_query(
        ctx,
        "select count(*)::text"
        " from ores_marketdata_curve_publishes_tbl"
        " where tenant_id = $1::uuid"
        "   and withdrawal_request_id = $2::uuid"
        "   and valid_to = ores_utility_infinity_timestamp_fn()",
        {ctx.tenant_id().to_string(), request_id},
        curve_withdrawal_lg(),
        "Reading the sets a withdrawal request ended");
    return !rows.empty() && rows.front()[0] && *rows.front()[0] != "0";
}

/**
 * @brief Whether the named request permits the act, and why not when it does not.
 *
 * The request is read through the inbox's own lifecycle, so the rule that a
 * withdrawal waits for a decision is the inbox's and not a copy of it here.
 */
std::string refusal_of_request(const ores::database::context& ctx, const std::string& request_id) {
    std::optional<ores::inbox::domain::approval_request> request;
    try {
        ores::inbox::service::approval_lifecycle lifecycle(ctx);
        request = lifecycle.request(request_id);
    } catch (const std::exception&) {
        // An id that is not a uuid is not a request: the person named something
        // the record cannot hold, which is a refusal and not a failure.
        return "No approval request has the id " + request_id + ".";
    }
    if (!request)
        return "No approval request has the id " + request_id + ".";

    if (request->kind_code != curve_withdrawal_kind_code)
        return "The request is a " + request->kind_code +
               " request, and a withdrawal is permitted by a " +
               std::string(curve_withdrawal_kind_code) + " request.";

    if (request->state_code != "approved")
        return "The request is " + request->state_code +
               ", and a withdrawal waits until it is approved.";

    if (request_already_ended_a_set(ctx, request_id))
        return "The request already ended a published set, and one decision authorises one "
               "withdrawal. Raise a request for the next one.";

    return {};
}

}

messaging::withdraw_curve_response
withdraw_curve(const ores::database::context& ctx,
               const messaging::withdraw_curve_request& request) {
    if (request.publish_id.empty())
        return refuse(
            "Name the published set to end, so the act ends one set and not any of them.");
    if (request.reason_code.empty())
        return refuse("Say why the set leaves use, so the record keeps a reason against the act.");

    ores::marketdata::repository::curve_publish_repository repo;
    const auto stored = repo.read_latest(ctx, request.publish_id);
    if (stored.empty())
        return refuse("No published set has the id " + request.publish_id + ".");
    auto record = stored.front();

    if (!record.published_at)
        return refuse("That set has not been published, and a candidate is not in use: there is "
                      "nothing to withdraw.");
    if (record.withdrawn_at)
        return refuse("That set was withdrawn at " +
                      ores::platform::time::datetime::to_iso8601_utc(*record.withdrawn_at) +
                      " by " + record.withdrawn_by + ".");
    if (record.superseded_at)
        return refuse("That set was superseded at " +
                      ores::platform::time::datetime::to_iso8601_utc(*record.superseded_at) +
                      " by a later publish, so it is not the set in use.");

    if (!request.approval_request_id.empty()) {
        const auto why = refusal_of_request(ctx, request.approval_request_id);
        if (!why.empty())
            return refuse(why);
        record.withdrawal_request_id =
            boost::uuids::string_generator{}(request.approval_request_id);
    }

    const auto actor = ctx.actor().empty() ? ctx.service_account() : ctx.actor();
    record.withdrawn_at =
        std::chrono::floor<std::chrono::seconds>(std::chrono::system_clock::now());
    record.withdrawn_by = actor;
    record.withdrawn_reason_code = request.reason_code;
    record.modified_by = actor;
    record.performed_by = ctx.service_account();
    record.change_reason_code = "system.curve_withdrawn";
    record.change_commentary = request.comment.empty() ? "Withdrawn by " + actor : request.comment;

    repo.write(ctx,
               record,
               {ores::utility::domain::precondition_kind::must_match_version,
                static_cast<std::uint32_t>(record.version)});

    BOOST_LOG_SEV(curve_withdrawal_lg(), info)
        << "Withdrew published set " << request.publish_id << " of series "
        << record.output_series_id << " as " << request.reason_code;

    // The withdrawal is a change a reader must act on, so it is its own notice.
    // Telling never reverses the act: a failure is logged in the announcer and
    // the answer still states the withdrawal.
    announce_withdrawal(ctx, record, record.as_of);

    messaging::withdraw_curve_response response;
    response.success = true;
    response.withdrawn_at = *record.withdrawn_at;
    response.withdrawn_by = record.withdrawn_by;
    return response;
}

}
