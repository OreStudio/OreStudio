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
#include "ores.scheduler.core/service/nats_publish_action_handler.hpp"

#include <span>
#include <rfl.hpp>
#include <rfl/json.hpp>
#include "ores.logging/make_logger.hpp"

namespace ores::scheduler::service {

using namespace ores::logging;

namespace {

auto& lg() {
    static auto instance = make_logger(
        "ores.scheduler.service.nats_publish_action_handler");
    return instance;
}

struct nats_publish_payload {
    std::string subject;
};

} // anonymous namespace

nats_publish_action_handler::nats_publish_action_handler(
    ores::nats::service::client& nats) : nats_(nats) {}

boost::asio::awaitable<std::expected<void, std::string>>
nats_publish_action_handler::execute(const action_context& ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Executing NATS publish action for job: "
                               << ctx.job.job_name;
    try {
        auto parsed = rfl::json::read<nats_publish_payload>(
            ctx.job.action_payload);
        if (!parsed) {
            const auto msg = "Failed to parse action_payload for job '"
                + ctx.job.job_name + "': " + parsed.error().what();
            BOOST_LOG_SEV(lg(), error) << msg;
            co_return std::unexpected(msg);
        }

        const auto& subject = parsed->subject;
        if (subject.empty()) {
            const auto msg = std::string(
                "action_payload missing subject for job '")
                + ctx.job.job_name + "'";
            BOOST_LOG_SEV(lg(), error) << msg;
            co_return std::unexpected(msg);
        }

        nats_.publish(subject, std::span<const std::byte>{});
        BOOST_LOG_SEV(lg(), info) << "NATS publish action succeeded for job: "
                                  << ctx.job.job_name
                                  << " (subject: " << subject << ")";
        co_return std::expected<void, std::string>{};
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "NATS publish action failed for job: "
                                   << ctx.job.job_name << ": " << e.what();
        co_return std::unexpected(std::string(e.what()));
    }
}

} // namespace ores::scheduler::service
