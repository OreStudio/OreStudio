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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51 Franklin
 * Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include "ores.scheduler.core/service/object_expiry_action_handler.hpp"
#include "ores.database/query_builder/execute.hpp"
#include "ores.logging/logger.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.scheduler.core/service/action_payloads.hpp"
#include "ores.storage.api/messaging/objects_protocol.hpp"
#include "ores.storage.api/net/object_keys.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <span>
#include <string>
#include <vector>

namespace ores::scheduler::service {

namespace {

auto& object_expiry_action_handler_lg() {
    static auto instance = make_logger("ores.scheduler.service.object_expiry_action_handler");
    return instance;
}

using namespace ores::logging;

/**
 * @brief The most objects one policy removes in one firing.
 *
 * A prefix can hold more than one run's work, and a firing that removed all of
 * it would hold the store for as long as the prefix is large. The reply states
 * what it left, and the next firing takes the rest.
 */
constexpr std::uint32_t k_expiry_limit = 1000;

/**
 * @brief One due policy, as the database states it.
 */
struct due_policy {
    std::string code;
    std::string target;
    int retention_days = 0;
};

std::vector<due_policy> read_due_policies(const database::context& ctx) {
    const auto rows = database::query_builder::execute_raw_multi_column_query(
        ctx,
        "select policy_code, target, retention_days "
        "from ores_scheduler_due_object_policies_fn()",
        object_expiry_action_handler_lg(),
        "Reading the object policies that are due");

    std::vector<due_policy> policies;
    policies.reserve(rows.size());
    for (const auto& row : rows) {
        if (row.size() < 3 || !row[0] || !row[1] || !row[2])
            continue;
        policies.push_back(due_policy{*row[0], *row[1], std::stoi(*row[2])});
    }
    return policies;
}

void record_run(const database::context& ctx,
                const due_policy& policy,
                std::int64_t removed,
                const std::string& outcome,
                const std::string& detail) {
    database::query_builder::execute_parameterized_command(
        ctx,
        "select ores_scheduler_record_object_run_fn($1, $2, $3::bigint, $4, $5)",
        {policy.code, policy.target, std::to_string(removed), outcome, detail},
        object_expiry_action_handler_lg(),
        "Recording an object expiry run");
}

/**
 * @brief What a run did, in the words the runs list shows.
 */
std::string describe(const storage::messaging::expire_objects_response& answer, bool dry_run) {
    auto text = "the cutoff held " + std::to_string(answer.would_remove) + " object(s) of " +
                std::to_string(answer.would_bytes) + " byte(s)";
    if (dry_run)
        text += "; a dry run removed none";
    else
        text += "; removed " + std::to_string(answer.removed) + " of " +
                std::to_string(answer.removed_bytes) + " byte(s)";
    if (answer.remaining > 0)
        text += "; " + std::to_string(answer.remaining) + " left for the next firing";
    return text;
}

}

object_expiry_action_handler::object_expiry_action_handler(
    ores::nats::service::nats_client& svc_nats)
    : svc_nats_(svc_nats) {}

boost::asio::awaitable<std::expected<void, std::string>>
object_expiry_action_handler::execute(const action_context& ctx) {
    ORES_LOG_SEV(object_expiry_action_handler_lg(), debug)
        << "Executing object expiry for job: " << ctx.job.job_name;

    object_expiry_payload payload;
    try {
        if (!ctx.job.action_payload.empty())
            payload =
                utility::codec::object::read_json<object_expiry_payload>(ctx.job.action_payload);
    } catch (const std::exception& e) {
        const auto msg =
            "Failed to parse action_payload for job '" + ctx.job.job_name + "': " + e.what();
        ORES_LOG_SEV(object_expiry_action_handler_lg(), error) << msg;
        co_return std::unexpected(msg);
    }

    std::vector<due_policy> policies;
    try {
        policies = read_due_policies(ctx.db_ctx);
    } catch (const std::exception& e) {
        co_return std::unexpected(std::string("Reading the due object policies failed: ") +
                                  e.what());
    }

    if (policies.empty()) {
        ORES_LOG_SEV(object_expiry_action_handler_lg(), debug) << "No object policy is due.";
        co_return std::expected<void, std::string>{};
    }

    std::string failures;
    for (const auto& policy : policies) {
        storage::messaging::expire_objects_request req;
        req.bucket = std::string(storage::api::object_keys::ores_bucket);
        req.prefix = policy.target;
        req.older_than =
            std::chrono::system_clock::now() - std::chrono::days(policy.retention_days);
        req.limit = k_expiry_limit;
        req.dry_run = payload.dry_run;

        std::string outcome;
        std::string detail;
        std::int64_t removed = 0;
        try {
            const auto codec = ores::nats::default_wire_codec();
            const auto body = codec.encode(req);
            const auto reply = svc_nats_.authenticated_request(
                std::string(storage::messaging::expire_objects_request::nats_subject),
                std::span<const std::byte>(body.data(), body.size()));

            const auto refusal = reply.headers.find(std::string(ores::nats::headers::x_error));
            if (refusal != reply.headers.end()) {
                throw std::runtime_error("the storage service refused the request: " +
                                         refusal->second);
            }

            const auto answer = codec.decode<storage::messaging::expire_objects_response>(
                std::span<const std::byte>(reinterpret_cast<const std::byte*>(reply.data.data()),
                                           reply.data.size()));
            if (!answer)
                throw std::runtime_error(answer.error().what());
            if (!answer->success)
                throw std::runtime_error(answer->message.empty() ? "the store refused the expiry" :
                                                                   answer->message);

            removed = static_cast<std::int64_t>(answer->removed);
            outcome = payload.dry_run ? "planned" : "applied";
            detail = describe(*answer, payload.dry_run);
        } catch (const std::exception& e) {
            outcome = "failed";
            detail = e.what();
            failures += policy.code + ": " + detail + "; ";
            ORES_LOG_SEV(object_expiry_action_handler_lg(), error)
                << "Expiry of " << policy.target << " failed: " << e.what();
        }

        try {
            record_run(ctx.db_ctx, policy, removed, outcome, detail);
        } catch (const std::exception& e) {
            failures += policy.code + " could not be recorded: " + std::string(e.what()) + "; ";
            ORES_LOG_SEV(object_expiry_action_handler_lg(), error)
                << "Recording the expiry of " << policy.target << " failed: " << e.what();
        }
    }

    if (!failures.empty())
        co_return std::unexpected(failures);

    co_return std::expected<void, std::string>{};
}

}
