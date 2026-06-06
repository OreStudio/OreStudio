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
#include "ores.shell/app/commands/bundles_commands.hpp"
#include "ores.dq.api/domain/dataset_bundle_table_io.hpp" // IWYU pragma: keep.
#include "ores.dq.api/messaging/dataset_bundle_protocol.hpp"
#include "ores.dq.api/messaging/publish_bundle_protocol.hpp"
#include "ores.shell/app/command_args.hpp"
#include "ores.shell/app/command_feedback.hpp"
#include "ores.shell/app/commands/workflow_commands.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <cli/cli.h>
#include <chrono>
#include <functional>
#include <ostream>
#include <rfl/json.hpp>

namespace ores::shell::app::commands {

using namespace logging;
using ores::nats::service::nats_client;

namespace {

// Publication dispatch can take a while on large bundles; mirror the
// wizards' generous request timeout.
constexpr std::chrono::minutes publish_timeout(5);
constexpr std::chrono::seconds default_wait_timeout(300);

template <typename Response>
std::optional<Response> do_auth_request(std::ostream& out,
                                        nats_client& session,
                                        const std::string& subject,
                                        const std::string& body,
                                        std::chrono::milliseconds timeout =
                                            std::chrono::seconds(30)) {
    try {
        auto reply = session.authenticated_request(subject, body, timeout);
        std::string data_str(reply.data.begin(), reply.data.end());
        auto result = rfl::json::read<Response>(data_str);
        if (!result) {
            fail(out) << "Failed to parse response" << std::endl;
            return std::nullopt;
        }
        return *result;
    } catch (const std::exception& e) {
        fail(out) << "Request failed: " << e.what() << std::endl;
        return std::nullopt;
    }
}

}

void bundles_commands::register_commands(cli::Menu& root_menu, nats_client& session) {
    auto bundles_menu = std::make_unique<cli::Menu>("bundles");

    bundles_menu->Insert(
        "list",
        [&session](std::ostream& out) {
            process_list(std::ref(out), std::ref(session));
        },
        "List the dataset bundles available for publication");

    bundles_menu->Insert(
        "publish",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_publish(std::ref(out), std::ref(session), args);
        },
        "Publish a dataset bundle (dispatches a workflow; --wait blocks on it)",
        {"code [--wait] [--root-lei <lei>] [--party-id <id>] [--dataset <code>] "
         "[--timeout <seconds>]"});

    root_menu.Insert(std::move(bundles_menu));
}

void bundles_commands::process_list(std::ostream& out, nats_client& session) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating get dataset bundles request.";

    dq::messaging::get_dataset_bundles_request req;
    auto result = do_auth_request<dq::messaging::get_dataset_bundles_response>(
        out, session, std::string(req.nats_subject), rfl::json::write(req));
    if (!result)
        return;

    BOOST_LOG_SEV(lg(), info) << "Successfully retrieved " << result->bundles.size()
                              << " bundles.";
    out << result->bundles << std::endl;
}

void bundles_commands::process_publish(std::ostream& out,
                                       nats_client& session,
                                       const std::vector<std::string>& args) {
    auto parsed = parse_args(args, {
        {.name = "wait"},
        {.name = "root-lei", .requires_value = true, .default_value = ""},
        {.name = "party-id", .requires_value = true, .default_value = ""},
        {.name = "dataset", .requires_value = true, .default_value = ""},
        {.name = "timeout", .requires_value = true,
         .default_value = std::to_string(default_wait_timeout.count())}
    });
    if (!parsed) {
        fail(out) << parsed.error() << std::endl;
        return;
    }
    if (parsed->positionals.size() != 1) {
        fail(out) << "Usage: bundles publish <code> [--wait] [--root-lei <lei>] "
                     "[--party-id <id>] [--dataset <code>] [--timeout <seconds>]"
                  << std::endl;
        return;
    }

    if (!session.is_logged_in()) {
        fail(out) << "Not logged in." << std::endl;
        return;
    }

    // Validate the wait timeout before dispatching anything.
    std::optional<std::chrono::seconds> wait_timeout;
    if (parsed->flag_set("wait")) {
        wait_timeout = parse_positive_seconds(parsed->flag("timeout"));
        if (!wait_timeout) {
            fail(out) << "Timeout must be a positive number of seconds: "
                      << parsed->flag("timeout") << std::endl;
            return;
        }
    }

    const auto& code = parsed->positionals.front();

    dq::messaging::publish_bundle_params params;
    if (!parsed->flag("dataset").empty())
        params.opted_in_datasets.push_back(parsed->flag("dataset"));
    if (!parsed->flag("root-lei").empty())
        params.lei_parties = dq::messaging::lei_parties_params{
            .root_lei = parsed->flag("root-lei")};
    if (!parsed->flag("party-id").empty())
        params.party_id = parsed->flag("party-id");

    dq::messaging::publish_bundle_request req;
    req.bundle_code = code;
    req.mode = dq::domain::publication_mode::upsert;
    req.published_by = session.auth().username;
    req.atomic = true;
    const bool has_params = !params.opted_in_datasets.empty() ||
        params.lei_parties.has_value() || params.party_id.has_value();
    if (has_params)
        req.params_json = dq::messaging::build_params_json(params);

    BOOST_LOG_SEV(lg(), info) << "Publishing bundle: " << code
                              << " (params: " << req.params_json << ")";
    out << "Publishing bundle '" << code << "'..." << std::endl;

    auto result = do_auth_request<dq::messaging::publish_bundle_response>(
        out, session, std::string(req.nats_subject), rfl::json::write(req),
        publish_timeout);
    if (!result)
        return;

    if (!result->success) {
        fail(out) << "Failed to publish bundle: " << result->error_message << std::endl;
        return;
    }

    out << "✓ Dispatched " << result->datasets_dispatched << " dataset(s); workflow instance: "
        << result->instance_id << std::endl;
    BOOST_LOG_SEV(lg(), info) << "Bundle " << code << " dispatched; instance "
                              << result->instance_id;

    if (!wait_timeout) {
        out << "Follow progress with: workflow wait " << result->instance_id << std::endl;
        return;
    }
    workflow_commands::wait_for_instance(out, session, result->instance_id, *wait_timeout);
}

}
