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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_shell_operation_implementation.cpp.mustache
 * To modify, update the template and regenerate.
 */
#include "ores.shell/app/commands/reporting/report_operations_operations_commands.hpp"
#include "ores.reporting.api/messaging/report_operations_protocol.hpp"
#include "ores.shell/app/command_args.hpp"
#include "ores.shell/app/command_feedback.hpp"
#include "ores.shell/app/command_token.hpp"
#include "ores.shell/app/request_helpers.hpp"
#include "ores.shell/app/shell_root_menu.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <cli/cli.h>
#include <cstddef>
#include <functional>
#include <optional>
#include <ostream>
#include <rfl.hpp>
#include <rfl/json.hpp>
#include <stdexcept>
#include <string>
#include <vector>

namespace ores::shell::app::commands {

using namespace logging;
using ores::nats::service::nats_client;

namespace {

/**
 * @brief Split a comma-separated token into the elements of a list field.
 *
 * The token __none__ states the empty list, because the command line cannot
 * carry an empty argument: the tokenizer drops one, and a command whose list
 * field should be empty has no other way to say so.
 */
std::vector<std::string> split_list_token(const std::string& value) {
    if (value == "__none__")
        return {};
    std::vector<std::string> parts;
    std::string current;
    for (const char c : value) {
        if (c == ',') {
            parts.push_back(current);
            current.clear();
        } else {
            current.push_back(c);
        }
    }
    parts.push_back(current);
    return parts;
}

} // namespace

void report_operations_operations_commands::register_commands(cli::Menu& root_menu,
                                                              nats_client& session) {
    auto menu = std::make_unique<cli::Menu>("report_operations");

    menu->Insert(
        "trigger-report-instance",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_trigger_report_instance(std::ref(out), std::ref(session), std::move(args));
        },
        "trigger-report-instance <report_definition_id> <tenant_id> [--job_instance_id <v>]");

    menu->Insert(
        "schedule-report-definitions",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_schedule_report_definitions(std::ref(out), std::ref(session), std::move(args));
        },
        "schedule-report-definitions <ids>");

    menu->Insert(
        "unschedule-report-definitions",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_unschedule_report_definitions(
                std::ref(out), std::ref(session), std::move(args));
        },
        "unschedule-report-definitions <ids>");

    menu->Insert(
        "gather-trades",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_gather_trades(std::ref(out), std::ref(session), std::move(args));
        },
        "gather-trades <report_instance_id> <definition_id> <tenant_id> <correlation_id>");

    menu->Insert(
        "gather-market-data",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_gather_market_data(std::ref(out), std::ref(session), std::move(args));
        },
        "gather-market-data <report_instance_id> <definition_id> <tenant_id> <correlation_id>");

    menu->Insert(
        "assemble-bundle",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_assemble_bundle(std::ref(out), std::ref(session), std::move(args));
        },
        "assemble-bundle <report_instance_id> <definition_id> <tenant_id> <correlation_id> "
        "<trades_storage_key> <market_data_storage_key> [--trade_count <v>] [--series_count <v>]");

    menu->Insert(
        "prepare-ore-package",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_prepare_ore_package(std::ref(out), std::ref(session), std::move(args));
        },
        "prepare-ore-package <report_instance_id> <bundle_id> <tenant_id> <correlation_id> "
        "<trades_storage_key> <market_data_storage_key>");

    menu->Insert(
        "submit-compute",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_submit_compute(std::ref(out), std::ref(session), std::move(args));
        },
        "submit-compute <report_instance_id> <tenant_id> <correlation_id> <tarball_uris>");

    menu->Insert(
        "collect-compute-results",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_collect_compute_results(std::ref(out), std::ref(session), std::move(args));
        },
        "collect-compute-results <report_instance_id> <tenant_id> <correlation_id> <batch_id>");

    menu->Insert(
        "finalise-report",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_finalise_report(std::ref(out), std::ref(session), std::move(args));
        },
        "finalise-report <report_instance_id> <tenant_id> <correlation_id>");

    menu->Insert(
        "fail-report",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_fail_report(std::ref(out), std::ref(session), std::move(args));
        },
        "fail-report <report_instance_id> <tenant_id> <correlation_id> <error_message>");

    menu->Insert(
        "resolve-prepared-input",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_resolve_prepared_input(std::ref(out), std::ref(session), std::move(args));
        },
        "resolve-prepared-input <report_instance_id> <tenant_id> <correlation_id> "
        "<prepared_input_key>");

    menu->Insert(
        "ignore-compute-results",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_ignore_compute_results(std::ref(out), std::ref(session), std::move(args));
        },
        "ignore-compute-results <report_instance_id> <tenant_id> <correlation_id> <batch_id>");

    ores::shell::app::insert_menu(root_menu, std::move(menu));
}

void report_operations_operations_commands::process_trigger_report_instance(
    std::ostream& out, nats_client& session, const std::vector<std::string>& args) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating trigger-report-instance request.";

    using request_type = ores::reporting::messaging::trigger_report_instance_request;

    // Whether the command presents a token is the protocol's own statement, so
    // a message that establishes the session is never asked for one.
    if constexpr (request_type::requires_session) {
        if (!session.is_logged_in()) {
            fail(out) << "You must be logged in to run trigger-report-instance." << std::endl;
            return;
        }
    }

    const std::vector<flag_spec> specs{
        {.name = "job_instance_id", .requires_value = true, .default_value = ""},
    };
    const auto parsed = parse_args(args, specs);
    if (!parsed) {
        fail(out) << parsed.error() << std::endl;
        return;
    }

    constexpr std::size_t positional_count = 2;
    if (parsed->positionals.size() != positional_count) {
        fail(out) << "Expected " << positional_count << " arguments, got "
                  << parsed->positionals.size() << "." << std::endl;
        return;
    }

    request_type req;
    std::size_t next = 0;
    try {
        req.report_definition_id = ores::shell::app::from_token<boost::uuids::uuid>(
            parsed->positionals[next++], "report_definition_id");
        req.tenant_id = ores::shell::app::from_token<boost::uuids::uuid>(
            parsed->positionals[next++], "tenant_id");
        if (const auto& raw_job_instance_id = parsed->flag("job_instance_id");
            !raw_job_instance_id.empty()) {
            req.job_instance_id =
                ores::shell::app::from_token<std::int64_t>(raw_job_instance_id, "job_instance_id");
        }
    } catch (const std::exception& e) {
        fail(out) << e.what() << std::endl;
        return;
    }

    std::optional<ores::reporting::messaging::trigger_report_instance_response> result;
    if constexpr (request_type::requires_session) {
        result = do_auth_request<ores::reporting::messaging::trigger_report_instance_response>(
            out, session, std::string(req.nats_subject), req);
    } else {
        result = do_request<ores::reporting::messaging::trigger_report_instance_response>(
            out, session, std::string(req.nats_subject), req);
    }
    if (!result)
        return;

    out << rfl::json::write(*result) << std::endl;
}

void report_operations_operations_commands::process_schedule_report_definitions(
    std::ostream& out, nats_client& session, const std::vector<std::string>& args) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating schedule-report-definitions request.";

    using request_type = ores::reporting::messaging::schedule_report_definitions_request;

    // Whether the command presents a token is the protocol's own statement, so
    // a message that establishes the session is never asked for one.
    if constexpr (request_type::requires_session) {
        if (!session.is_logged_in()) {
            fail(out) << "You must be logged in to run schedule-report-definitions." << std::endl;
            return;
        }
    }

    const std::vector<flag_spec> specs{};
    const auto parsed = parse_args(args, specs);
    if (!parsed) {
        fail(out) << parsed.error() << std::endl;
        return;
    }

    constexpr std::size_t positional_count = 1;
    if (parsed->positionals.size() != positional_count) {
        fail(out) << "Expected " << positional_count << " arguments, got "
                  << parsed->positionals.size() << "." << std::endl;
        return;
    }

    request_type req;
    std::size_t next = 0;
    try {
        req.ids = split_list_token(parsed->positionals[next++]);
    } catch (const std::exception& e) {
        fail(out) << e.what() << std::endl;
        return;
    }

    std::optional<ores::reporting::messaging::schedule_report_definitions_response> result;
    if constexpr (request_type::requires_session) {
        result = do_auth_request<ores::reporting::messaging::schedule_report_definitions_response>(
            out, session, std::string(req.nats_subject), req);
    } else {
        result = do_request<ores::reporting::messaging::schedule_report_definitions_response>(
            out, session, std::string(req.nats_subject), req);
    }
    if (!result)
        return;

    out << rfl::json::write(*result) << std::endl;
}

void report_operations_operations_commands::process_unschedule_report_definitions(
    std::ostream& out, nats_client& session, const std::vector<std::string>& args) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating unschedule-report-definitions request.";

    using request_type = ores::reporting::messaging::unschedule_report_definitions_request;

    // Whether the command presents a token is the protocol's own statement, so
    // a message that establishes the session is never asked for one.
    if constexpr (request_type::requires_session) {
        if (!session.is_logged_in()) {
            fail(out) << "You must be logged in to run unschedule-report-definitions." << std::endl;
            return;
        }
    }

    const std::vector<flag_spec> specs{};
    const auto parsed = parse_args(args, specs);
    if (!parsed) {
        fail(out) << parsed.error() << std::endl;
        return;
    }

    constexpr std::size_t positional_count = 1;
    if (parsed->positionals.size() != positional_count) {
        fail(out) << "Expected " << positional_count << " arguments, got "
                  << parsed->positionals.size() << "." << std::endl;
        return;
    }

    request_type req;
    std::size_t next = 0;
    try {
        req.ids = split_list_token(parsed->positionals[next++]);
    } catch (const std::exception& e) {
        fail(out) << e.what() << std::endl;
        return;
    }

    std::optional<ores::reporting::messaging::unschedule_report_definitions_response> result;
    if constexpr (request_type::requires_session) {
        result =
            do_auth_request<ores::reporting::messaging::unschedule_report_definitions_response>(
                out, session, std::string(req.nats_subject), req);
    } else {
        result = do_request<ores::reporting::messaging::unschedule_report_definitions_response>(
            out, session, std::string(req.nats_subject), req);
    }
    if (!result)
        return;

    out << rfl::json::write(*result) << std::endl;
}

void report_operations_operations_commands::process_gather_trades(
    std::ostream& out, nats_client& session, const std::vector<std::string>& args) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating gather-trades request.";

    using request_type = ores::reporting::messaging::gather_trades_request;

    // Whether the command presents a token is the protocol's own statement, so
    // a message that establishes the session is never asked for one.
    if constexpr (request_type::requires_session) {
        if (!session.is_logged_in()) {
            fail(out) << "You must be logged in to run gather-trades." << std::endl;
            return;
        }
    }

    const std::vector<flag_spec> specs{};
    const auto parsed = parse_args(args, specs);
    if (!parsed) {
        fail(out) << parsed.error() << std::endl;
        return;
    }

    constexpr std::size_t positional_count = 4;
    if (parsed->positionals.size() != positional_count) {
        fail(out) << "Expected " << positional_count << " arguments, got "
                  << parsed->positionals.size() << "." << std::endl;
        return;
    }

    request_type req;
    std::size_t next = 0;
    try {
        req.report_instance_id = parsed->positionals[next++];
        req.definition_id = parsed->positionals[next++];
        req.tenant_id = parsed->positionals[next++];
        req.correlation_id = parsed->positionals[next++];
    } catch (const std::exception& e) {
        fail(out) << e.what() << std::endl;
        return;
    }

    std::optional<ores::reporting::messaging::gather_trades_result> result;
    if constexpr (request_type::requires_session) {
        result = do_auth_request<ores::reporting::messaging::gather_trades_result>(
            out, session, std::string(req.nats_subject), req);
    } else {
        result = do_request<ores::reporting::messaging::gather_trades_result>(
            out, session, std::string(req.nats_subject), req);
    }
    if (!result)
        return;

    out << rfl::json::write(*result) << std::endl;
}

void report_operations_operations_commands::process_gather_market_data(
    std::ostream& out, nats_client& session, const std::vector<std::string>& args) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating gather-market-data request.";

    using request_type = ores::reporting::messaging::gather_market_data_request;

    // Whether the command presents a token is the protocol's own statement, so
    // a message that establishes the session is never asked for one.
    if constexpr (request_type::requires_session) {
        if (!session.is_logged_in()) {
            fail(out) << "You must be logged in to run gather-market-data." << std::endl;
            return;
        }
    }

    const std::vector<flag_spec> specs{};
    const auto parsed = parse_args(args, specs);
    if (!parsed) {
        fail(out) << parsed.error() << std::endl;
        return;
    }

    constexpr std::size_t positional_count = 4;
    if (parsed->positionals.size() != positional_count) {
        fail(out) << "Expected " << positional_count << " arguments, got "
                  << parsed->positionals.size() << "." << std::endl;
        return;
    }

    request_type req;
    std::size_t next = 0;
    try {
        req.report_instance_id = parsed->positionals[next++];
        req.definition_id = parsed->positionals[next++];
        req.tenant_id = parsed->positionals[next++];
        req.correlation_id = parsed->positionals[next++];
    } catch (const std::exception& e) {
        fail(out) << e.what() << std::endl;
        return;
    }

    std::optional<ores::reporting::messaging::gather_market_data_result> result;
    if constexpr (request_type::requires_session) {
        result = do_auth_request<ores::reporting::messaging::gather_market_data_result>(
            out, session, std::string(req.nats_subject), req);
    } else {
        result = do_request<ores::reporting::messaging::gather_market_data_result>(
            out, session, std::string(req.nats_subject), req);
    }
    if (!result)
        return;

    out << rfl::json::write(*result) << std::endl;
}

void report_operations_operations_commands::process_assemble_bundle(
    std::ostream& out, nats_client& session, const std::vector<std::string>& args) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating assemble-bundle request.";

    using request_type = ores::reporting::messaging::assemble_bundle_request;

    // Whether the command presents a token is the protocol's own statement, so
    // a message that establishes the session is never asked for one.
    if constexpr (request_type::requires_session) {
        if (!session.is_logged_in()) {
            fail(out) << "You must be logged in to run assemble-bundle." << std::endl;
            return;
        }
    }

    const std::vector<flag_spec> specs{
        {.name = "trade_count", .requires_value = true, .default_value = ""},
        {.name = "series_count", .requires_value = true, .default_value = ""},
    };
    const auto parsed = parse_args(args, specs);
    if (!parsed) {
        fail(out) << parsed.error() << std::endl;
        return;
    }

    constexpr std::size_t positional_count = 6;
    if (parsed->positionals.size() != positional_count) {
        fail(out) << "Expected " << positional_count << " arguments, got "
                  << parsed->positionals.size() << "." << std::endl;
        return;
    }

    request_type req;
    std::size_t next = 0;
    try {
        req.report_instance_id = parsed->positionals[next++];
        req.definition_id = parsed->positionals[next++];
        req.tenant_id = parsed->positionals[next++];
        req.correlation_id = parsed->positionals[next++];
        req.trades_storage_key = parsed->positionals[next++];
        req.market_data_storage_key = parsed->positionals[next++];
        if (const auto& raw_trade_count = parsed->flag("trade_count"); !raw_trade_count.empty()) {
            req.trade_count = ores::shell::app::from_token<int>(raw_trade_count, "trade_count");
        }
        if (const auto& raw_series_count = parsed->flag("series_count");
            !raw_series_count.empty()) {
            req.series_count = ores::shell::app::from_token<int>(raw_series_count, "series_count");
        }
    } catch (const std::exception& e) {
        fail(out) << e.what() << std::endl;
        return;
    }

    std::optional<ores::reporting::messaging::assemble_bundle_result> result;
    if constexpr (request_type::requires_session) {
        result = do_auth_request<ores::reporting::messaging::assemble_bundle_result>(
            out, session, std::string(req.nats_subject), req);
    } else {
        result = do_request<ores::reporting::messaging::assemble_bundle_result>(
            out, session, std::string(req.nats_subject), req);
    }
    if (!result)
        return;

    out << rfl::json::write(*result) << std::endl;
}

void report_operations_operations_commands::process_prepare_ore_package(
    std::ostream& out, nats_client& session, const std::vector<std::string>& args) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating prepare-ore-package request.";

    using request_type = ores::reporting::messaging::prepare_ore_package_request;

    // Whether the command presents a token is the protocol's own statement, so
    // a message that establishes the session is never asked for one.
    if constexpr (request_type::requires_session) {
        if (!session.is_logged_in()) {
            fail(out) << "You must be logged in to run prepare-ore-package." << std::endl;
            return;
        }
    }

    const std::vector<flag_spec> specs{};
    const auto parsed = parse_args(args, specs);
    if (!parsed) {
        fail(out) << parsed.error() << std::endl;
        return;
    }

    constexpr std::size_t positional_count = 6;
    if (parsed->positionals.size() != positional_count) {
        fail(out) << "Expected " << positional_count << " arguments, got "
                  << parsed->positionals.size() << "." << std::endl;
        return;
    }

    request_type req;
    std::size_t next = 0;
    try {
        req.report_instance_id = parsed->positionals[next++];
        req.bundle_id = parsed->positionals[next++];
        req.tenant_id = parsed->positionals[next++];
        req.correlation_id = parsed->positionals[next++];
        req.trades_storage_key = parsed->positionals[next++];
        req.market_data_storage_key = parsed->positionals[next++];
    } catch (const std::exception& e) {
        fail(out) << e.what() << std::endl;
        return;
    }

    std::optional<ores::reporting::messaging::prepare_ore_package_result> result;
    if constexpr (request_type::requires_session) {
        result = do_auth_request<ores::reporting::messaging::prepare_ore_package_result>(
            out, session, std::string(req.nats_subject), req);
    } else {
        result = do_request<ores::reporting::messaging::prepare_ore_package_result>(
            out, session, std::string(req.nats_subject), req);
    }
    if (!result)
        return;

    out << rfl::json::write(*result) << std::endl;
}

void report_operations_operations_commands::process_submit_compute(
    std::ostream& out, nats_client& session, const std::vector<std::string>& args) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating submit-compute request.";

    using request_type = ores::reporting::messaging::submit_compute_request;

    // Whether the command presents a token is the protocol's own statement, so
    // a message that establishes the session is never asked for one.
    if constexpr (request_type::requires_session) {
        if (!session.is_logged_in()) {
            fail(out) << "You must be logged in to run submit-compute." << std::endl;
            return;
        }
    }

    const std::vector<flag_spec> specs{};
    const auto parsed = parse_args(args, specs);
    if (!parsed) {
        fail(out) << parsed.error() << std::endl;
        return;
    }

    constexpr std::size_t positional_count = 4;
    if (parsed->positionals.size() != positional_count) {
        fail(out) << "Expected " << positional_count << " arguments, got "
                  << parsed->positionals.size() << "." << std::endl;
        return;
    }

    request_type req;
    std::size_t next = 0;
    try {
        req.report_instance_id = parsed->positionals[next++];
        req.tenant_id = parsed->positionals[next++];
        req.correlation_id = parsed->positionals[next++];
        req.tarball_uris = split_list_token(parsed->positionals[next++]);
    } catch (const std::exception& e) {
        fail(out) << e.what() << std::endl;
        return;
    }

    std::optional<ores::reporting::messaging::submit_compute_result> result;
    if constexpr (request_type::requires_session) {
        result = do_auth_request<ores::reporting::messaging::submit_compute_result>(
            out, session, std::string(req.nats_subject), req);
    } else {
        result = do_request<ores::reporting::messaging::submit_compute_result>(
            out, session, std::string(req.nats_subject), req);
    }
    if (!result)
        return;

    out << rfl::json::write(*result) << std::endl;
}

void report_operations_operations_commands::process_collect_compute_results(
    std::ostream& out, nats_client& session, const std::vector<std::string>& args) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating collect-compute-results request.";

    using request_type = ores::reporting::messaging::collect_compute_results_request;

    // Whether the command presents a token is the protocol's own statement, so
    // a message that establishes the session is never asked for one.
    if constexpr (request_type::requires_session) {
        if (!session.is_logged_in()) {
            fail(out) << "You must be logged in to run collect-compute-results." << std::endl;
            return;
        }
    }

    const std::vector<flag_spec> specs{};
    const auto parsed = parse_args(args, specs);
    if (!parsed) {
        fail(out) << parsed.error() << std::endl;
        return;
    }

    constexpr std::size_t positional_count = 4;
    if (parsed->positionals.size() != positional_count) {
        fail(out) << "Expected " << positional_count << " arguments, got "
                  << parsed->positionals.size() << "." << std::endl;
        return;
    }

    request_type req;
    std::size_t next = 0;
    try {
        req.report_instance_id = parsed->positionals[next++];
        req.tenant_id = parsed->positionals[next++];
        req.correlation_id = parsed->positionals[next++];
        req.batch_id = parsed->positionals[next++];
    } catch (const std::exception& e) {
        fail(out) << e.what() << std::endl;
        return;
    }

    std::optional<ores::reporting::messaging::collect_compute_results_result> result;
    if constexpr (request_type::requires_session) {
        result = do_auth_request<ores::reporting::messaging::collect_compute_results_result>(
            out, session, std::string(req.nats_subject), req);
    } else {
        result = do_request<ores::reporting::messaging::collect_compute_results_result>(
            out, session, std::string(req.nats_subject), req);
    }
    if (!result)
        return;

    out << rfl::json::write(*result) << std::endl;
}

void report_operations_operations_commands::process_finalise_report(
    std::ostream& out, nats_client& session, const std::vector<std::string>& args) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating finalise-report request.";

    using request_type = ores::reporting::messaging::finalise_report_request;

    // Whether the command presents a token is the protocol's own statement, so
    // a message that establishes the session is never asked for one.
    if constexpr (request_type::requires_session) {
        if (!session.is_logged_in()) {
            fail(out) << "You must be logged in to run finalise-report." << std::endl;
            return;
        }
    }

    const std::vector<flag_spec> specs{};
    const auto parsed = parse_args(args, specs);
    if (!parsed) {
        fail(out) << parsed.error() << std::endl;
        return;
    }

    constexpr std::size_t positional_count = 3;
    if (parsed->positionals.size() != positional_count) {
        fail(out) << "Expected " << positional_count << " arguments, got "
                  << parsed->positionals.size() << "." << std::endl;
        return;
    }

    request_type req;
    std::size_t next = 0;
    try {
        req.report_instance_id = parsed->positionals[next++];
        req.tenant_id = parsed->positionals[next++];
        req.correlation_id = parsed->positionals[next++];
    } catch (const std::exception& e) {
        fail(out) << e.what() << std::endl;
        return;
    }

    std::optional<ores::reporting::messaging::finalise_report_result> result;
    if constexpr (request_type::requires_session) {
        result = do_auth_request<ores::reporting::messaging::finalise_report_result>(
            out, session, std::string(req.nats_subject), req);
    } else {
        result = do_request<ores::reporting::messaging::finalise_report_result>(
            out, session, std::string(req.nats_subject), req);
    }
    if (!result)
        return;

    out << rfl::json::write(*result) << std::endl;
}

void report_operations_operations_commands::process_fail_report(
    std::ostream& out, nats_client& session, const std::vector<std::string>& args) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating fail-report request.";

    using request_type = ores::reporting::messaging::fail_report_request;

    // Whether the command presents a token is the protocol's own statement, so
    // a message that establishes the session is never asked for one.
    if constexpr (request_type::requires_session) {
        if (!session.is_logged_in()) {
            fail(out) << "You must be logged in to run fail-report." << std::endl;
            return;
        }
    }

    const std::vector<flag_spec> specs{};
    const auto parsed = parse_args(args, specs);
    if (!parsed) {
        fail(out) << parsed.error() << std::endl;
        return;
    }

    constexpr std::size_t positional_count = 4;
    if (parsed->positionals.size() != positional_count) {
        fail(out) << "Expected " << positional_count << " arguments, got "
                  << parsed->positionals.size() << "." << std::endl;
        return;
    }

    request_type req;
    std::size_t next = 0;
    try {
        req.report_instance_id = parsed->positionals[next++];
        req.tenant_id = parsed->positionals[next++];
        req.correlation_id = parsed->positionals[next++];
        req.error_message = parsed->positionals[next++];
    } catch (const std::exception& e) {
        fail(out) << e.what() << std::endl;
        return;
    }

    std::optional<ores::reporting::messaging::fail_report_result> result;
    if constexpr (request_type::requires_session) {
        result = do_auth_request<ores::reporting::messaging::fail_report_result>(
            out, session, std::string(req.nats_subject), req);
    } else {
        result = do_request<ores::reporting::messaging::fail_report_result>(
            out, session, std::string(req.nats_subject), req);
    }
    if (!result)
        return;

    out << rfl::json::write(*result) << std::endl;
}

void report_operations_operations_commands::process_resolve_prepared_input(
    std::ostream& out, nats_client& session, const std::vector<std::string>& args) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating resolve-prepared-input request.";

    using request_type = ores::reporting::messaging::resolve_prepared_input_request;

    // Whether the command presents a token is the protocol's own statement, so
    // a message that establishes the session is never asked for one.
    if constexpr (request_type::requires_session) {
        if (!session.is_logged_in()) {
            fail(out) << "You must be logged in to run resolve-prepared-input." << std::endl;
            return;
        }
    }

    const std::vector<flag_spec> specs{};
    const auto parsed = parse_args(args, specs);
    if (!parsed) {
        fail(out) << parsed.error() << std::endl;
        return;
    }

    constexpr std::size_t positional_count = 4;
    if (parsed->positionals.size() != positional_count) {
        fail(out) << "Expected " << positional_count << " arguments, got "
                  << parsed->positionals.size() << "." << std::endl;
        return;
    }

    request_type req;
    std::size_t next = 0;
    try {
        req.report_instance_id = parsed->positionals[next++];
        req.tenant_id = parsed->positionals[next++];
        req.correlation_id = parsed->positionals[next++];
        req.prepared_input_key = parsed->positionals[next++];
    } catch (const std::exception& e) {
        fail(out) << e.what() << std::endl;
        return;
    }

    std::optional<ores::reporting::messaging::prepare_ore_package_result> result;
    if constexpr (request_type::requires_session) {
        result = do_auth_request<ores::reporting::messaging::prepare_ore_package_result>(
            out, session, std::string(req.nats_subject), req);
    } else {
        result = do_request<ores::reporting::messaging::prepare_ore_package_result>(
            out, session, std::string(req.nats_subject), req);
    }
    if (!result)
        return;

    out << rfl::json::write(*result) << std::endl;
}

void report_operations_operations_commands::process_ignore_compute_results(
    std::ostream& out, nats_client& session, const std::vector<std::string>& args) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating ignore-compute-results request.";

    using request_type = ores::reporting::messaging::ignore_compute_results_request;

    // Whether the command presents a token is the protocol's own statement, so
    // a message that establishes the session is never asked for one.
    if constexpr (request_type::requires_session) {
        if (!session.is_logged_in()) {
            fail(out) << "You must be logged in to run ignore-compute-results." << std::endl;
            return;
        }
    }

    const std::vector<flag_spec> specs{};
    const auto parsed = parse_args(args, specs);
    if (!parsed) {
        fail(out) << parsed.error() << std::endl;
        return;
    }

    constexpr std::size_t positional_count = 4;
    if (parsed->positionals.size() != positional_count) {
        fail(out) << "Expected " << positional_count << " arguments, got "
                  << parsed->positionals.size() << "." << std::endl;
        return;
    }

    request_type req;
    std::size_t next = 0;
    try {
        req.report_instance_id = parsed->positionals[next++];
        req.tenant_id = parsed->positionals[next++];
        req.correlation_id = parsed->positionals[next++];
        req.batch_id = parsed->positionals[next++];
    } catch (const std::exception& e) {
        fail(out) << e.what() << std::endl;
        return;
    }

    std::optional<ores::reporting::messaging::collect_compute_results_result> result;
    if constexpr (request_type::requires_session) {
        result = do_auth_request<ores::reporting::messaging::collect_compute_results_result>(
            out, session, std::string(req.nats_subject), req);
    } else {
        result = do_request<ores::reporting::messaging::collect_compute_results_result>(
            out, session, std::string(req.nats_subject), req);
    }
    if (!result)
        return;

    out << rfl::json::write(*result) << std::endl;
}

}
