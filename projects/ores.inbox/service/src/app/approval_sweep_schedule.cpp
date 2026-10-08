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
#include "ores.inbox.service/app/approval_sweep_schedule.hpp"
#include "ores.inbox.service/app/application_exception.hpp"
#include "ores.nats/domain/headers.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.scheduler.api/domain/cron_expression.hpp"
#include "ores.scheduler.api/messaging/job_definition_protocol.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.variability.api/messaging/operations_protocol.hpp"
#include <boost/asio/steady_timer.hpp>
#include <boost/asio/this_coro.hpp>
#include <boost/asio/use_awaitable.hpp>
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid.hpp>
#include <algorithm>
#include <chrono>
#include <rfl.hpp>
#include <rfl/json.hpp>
#include <utility>

namespace ores::inbox::service::app {

using namespace ores::logging;
using ores::utility::domain::outcome;

namespace {

/**
 * @brief What the scheduler publishes when a sweep fires.
 *
 * Only the subject is read: the operations take no input, so the body the
 * scheduler adds around it is ignored at the other end.
 */
struct sweep_action_payload {
    std::string subject;
};

/**
 * @brief Sends one authenticated request and reads the answer.
 *
 * A call that raises, is refused by an error header, or answers unreadably is
 * a transport failure: the service on the other end is not answering this
 * question right now, so a later attempt can cure it. An answer that arrives
 * carrying a non-ok outcome is a decision, and is left to the caller.
 */
template <typename Request>
std::expected<typename Request::response_type, std::string>
call(ores::nats::service::nats_client& nats, const Request& request) {
    const auto& codec = ores::nats::default_wire_codec();
    ores::nats::message reply;
    try {
        reply = nats.authenticated_request(Request::nats_subject, codec.encode(request));
    } catch (const std::exception& e) {
        return std::unexpected(std::string(Request::nats_subject) + " did not answer: " + e.what());
    }
    if (const auto it = reply.headers.find(std::string(ores::nats::headers::x_error));
        it != reply.headers.end())
        return std::unexpected(std::string(Request::nats_subject) + " refused: " + it->second);
    auto response = codec.decode<typename Request::response_type>(reply.data);
    if (!response)
        return std::unexpected(std::string(Request::nats_subject) + " answered unreadably");
    return *response;
}

}

approval_sweep_schedule::approval_sweep_schedule(ores::nats::service::nats_client svc_nats,
                                                 sweep described)
    : svc_nats_(std::move(svc_nats))
    , described_(std::move(described)) {}

boost::asio::awaitable<void> approval_sweep_schedule::register_job() {
    // The scheduler may not be listening yet when this service starts, so an
    // unreachable dependency is waited out rather than treated as a
    // misconfiguration. A refusal is not waited out: it will refuse again.
    constexpr int max_attempts = 12;
    constexpr auto retry_delay = std::chrono::seconds(10);

    auto executor = co_await boost::asio::this_coro::executor;
    boost::asio::steady_timer timer(executor);

    for (int attempt = 1;; ++attempt) {
        if (auto attempt_result = try_register_once(); attempt_result) {
            BOOST_LOG_SEV(lg(), info) << "Registered the sweep job '" << described_.job_name
                                      << "' from " << described_.setting_name << ".";
            co_return;
        } else if (!attempt_result.error().retryable) {
            throw application_exception(attempt_result.error().message);
        } else if (attempt == max_attempts) {
            throw application_exception("Could not register the sweep job after " +
                                        std::to_string(max_attempts) +
                                        " attempts: " + attempt_result.error().message);
        } else {
            BOOST_LOG_SEV(lg(), warn)
                << "Could not register the sweep job (attempt " << attempt << " of "
                << max_attempts << "): " << attempt_result.error().message;
        }

        timer.expires_after(retry_delay);
        co_await timer.async_wait(boost::asio::use_awaitable);
    }
}

std::expected<void, approval_sweep_schedule::failure>
approval_sweep_schedule::try_register_once() {
    using ores::scheduler::messaging::job_definition_change;
    using ores::scheduler::messaging::list_job_definitions_request;
    using ores::scheduler::messaging::put_job_definition_request;

    ores::variability::messaging::get_setting_request setting_request;
    setting_request.name = std::string(described_.setting_name);

    const auto setting = call(svc_nats_, setting_request);
    if (!setting)
        return std::unexpected(failure{.retryable = true, .message = setting.error()});
    if (setting->result.outcome != outcome::ok)
        return std::unexpected(
            failure{.message = "The setting " + std::string(described_.setting_name) +
                               " could not be read: " + setting->result.message});
    if (setting->value.empty())
        return std::unexpected(
            failure{.message = "The setting " + std::string(described_.setting_name) +
                               " is empty, so there is no schedule to register."});

    const auto cron = ores::scheduler::domain::cron_expression::from_string(setting->value);
    if (!cron)
        return std::unexpected(failure{.message = "The setting " +
                                                  std::string(described_.setting_name) +
                                                  " is not a cron expression: " + cron.error()});

    // The job is recognised by name, because that is the key the store holds
    // it under and the only thing about it that survives a restart. A job
    // already holding the wanted schedule is left alone, so a restart writes
    // no new version of a row nothing changed.
    list_job_definitions_request list_request;
    list_request.limit = 1000;
    const auto jobs = call(svc_nats_, list_request);
    if (!jobs)
        return std::unexpected(failure{.retryable = true, .message = jobs.error()});
    if (jobs->result.outcome != outcome::ok)
        return std::unexpected(failure{.message = std::string("The scheduler's jobs could not be "
                                                              "listed: ") +
                                                  jobs->result.message});

    const auto& job_name = described_.job_name;
    const auto held = std::ranges::find_if(
        jobs->definitions, [&job_name](const auto& j) { return j.job_name == job_name; });

    if (held != jobs->definitions.end() &&
        held->schedule_expression.to_string() == cron->to_string() && held->is_active &&
        held->action_type == "nats_publish")
        return {};

    job_definition_change change;
    change.write.id =
        held != jobs->definitions.end() ? held->id : boost::uuids::random_generator()();
    change.write.job_name = std::string(described_.job_name);
    change.write.description = described_.description;
    change.write.command = "";
    change.write.schedule_expression = *cron;
    change.write.action_type = "nats_publish";
    change.write.action_payload =
        rfl::json::write(sweep_action_payload{.subject = described_.subject});
    change.write.is_active = true;
    // The component owns the job's identity and replaces whatever holds it,
    // because this runs again on every restart.
    change.precondition.kind = ores::utility::domain::precondition_kind::any;

    const put_job_definition_request put_request{
        .change = std::move(change),
        .intent = {.reason_code = std::string(ores::service::messaging::change_reasons::new_record),
                   .commentary = "Registered by the inbox service from " +
                                 std::string(described_.setting_name) + "."}};

    const auto put = call(svc_nats_, put_request);
    if (!put)
        return std::unexpected(failure{.retryable = true, .message = put.error()});
    if (put->result.outcome != outcome::ok)
        return std::unexpected(
            failure{.message = "The scheduler refused the job: " + put->result.message});
    return {};
}

}
