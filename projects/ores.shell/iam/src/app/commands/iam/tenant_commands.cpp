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
 * Template: cpp_shell_command_impl.cpp.mustache
 * To modify, update the template and regenerate.
 */
#include "ores.shell/app/commands/iam/tenant_commands.hpp"
#include "ores.iam.api/messaging/tenant_protocol.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.shell/app/command_args.hpp"
#include "ores.shell/app/command_feedback.hpp"
#include "ores.shell/app/command_token.hpp"
#include "ores.shell/app/request_helpers.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <boost/asio/ip/address.hpp>
#include <boost/uuid/random_generator.hpp>
#include <chrono>
#include <cli/cli.h>
#include <cstddef>
#include <functional>
#include <optional>
#include <ostream>
#include <rfl.hpp>
#include <rfl/json.hpp>
#include <stdexcept>
#include <string>
#include <type_traits>
#include <vector>

namespace ores::shell::app::commands {

using namespace logging;
using ores::nats::service::nats_client;
namespace messaging = ores::iam::messaging;

namespace {

/**
 * @brief Fill one request member from one token.
 *
 * A member's own type decides how its token reads, so the caller states the
 * token and the name it answers to and nothing else. The four cases are the
 * four shapes a token has: a word, a flag, a comma-separated list and
 * everything from_token already converts.
 */
template <typename T>
void read_token(T& target, const std::string& raw, const std::string& name) {
    if constexpr (std::is_same_v<T, std::string>) {
        target = raw;
    } else if constexpr (std::is_same_v<T, bool>) {
        if (raw.empty() || raw == "false") {
            target = false;
        } else if (raw == "true") {
            target = true;
        } else {
            throw std::invalid_argument(name + " must be 'true' or 'false'");
        }
    } else if constexpr (std::is_same_v<T, std::chrono::system_clock::time_point>) {
        target = ores::platform::time::datetime::from_iso8601_utc(raw);
    } else if constexpr (std::is_same_v<T, boost::asio::ip::address>) {
        target = boost::asio::ip::make_address(raw);
    } else if constexpr (std::is_same_v<T, std::vector<std::string>>) {
        target.clear();
        std::string current;
        for (const char c : raw) {
            if (c == ',') {
                target.push_back(current);
                current.clear();
            } else {
                current.push_back(c);
            }
        }
        target.push_back(current);
    } else {
        target = ores::shell::app::from_token<T>(raw, name);
    }
}

/// Apply the page and the order a caller stated, leaving the defaults alone.
template <typename Request>
void apply_page(Request& req, const parsed_args& parsed) {
    if (const auto& raw = parsed.flag("offset"); !raw.empty()) {
        req.offset = ores::shell::app::from_token<std::uint32_t>(raw, "offset");
    }
    if (const auto& raw = parsed.flag("limit"); !raw.empty()) {
        req.limit = ores::shell::app::from_token<std::uint32_t>(raw, "limit");
    }
    if (const auto& raw = parsed.flag("order"); !raw.empty()) {
        req.order.field = raw;
    }
    req.order.descending = parsed.flag_set("desc");
}

} // namespace

void tenant_commands::register_commands(cli::Menu& root_menu, nats_client& session) {
    auto menu = std::make_unique<cli::Menu>("tenants");

    menu->Insert(
        "list",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_list(std::ref(out), std::ref(session), std::move(args));
        },
        "list [--offset <n>] [--limit <n>] [--order <field>] [--desc]");

    menu->Insert(
        "get",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_get(std::ref(out), std::ref(session), std::move(args));
        },
        "get <code>");

    menu->Insert(
        "get-many",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_get_many(std::ref(out), std::ref(session), std::move(args));
        },
        "get-many <code>");

    menu->Insert(
        "add",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_add(std::ref(out), std::ref(session), std::move(args));
        },
        "add <id> <code> <name> <type> <description> <hostname> <status> <reason> <commentary>");

    menu->Insert(
        "set",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_set(std::ref(out), std::ref(session), std::move(args));
        },
        "set <id> <code> <name> <type> <description> <hostname> <status> <reason> <commentary> "
        "[--version <n>]");

    menu->Insert(
        "put-many",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_put_many(std::ref(out), std::ref(session), std::move(args));
        },
        "put-many --count <n> <id> <code> <name> <type> <description> <hostname> <status> <reason> "
        "<commentary>");

    menu->Insert(
        "delete",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_delete(std::ref(out), std::ref(session), std::move(args));
        },
        "delete <code> <reason> <commentary> [--version <n>]");

    menu->Insert(
        "delete-many",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_delete_many(std::ref(out), std::ref(session), std::move(args));
        },
        "delete-many <code> <reason> <commentary>");

    menu->Insert(
        "versions",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_versions(std::ref(out), std::ref(session), std::move(args));
        },
        "versions <code> [--offset <n>] [--limit <n>] [--order <field>] [--desc]");

    menu->Insert(
        "version",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_version(std::ref(out), std::ref(session), std::move(args));
        },
        "version <code> --version <n>");

    root_menu.Insert(std::move(menu));
}

void tenant_commands::process_list(std::ostream& out,
                                   nats_client& session,
                                   const std::vector<std::string>& args) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating list request.";

    using request_type = messaging::list_tenants_request;
    if constexpr (request_type::requires_session) {
        if (!session.is_logged_in()) {
            fail(out) << "You must be logged in to run list." << std::endl;
            return;
        }
    }

    const std::vector<flag_spec> specs{
        {.name = "offset", .requires_value = true, .default_value = ""},
        {.name = "limit", .requires_value = true, .default_value = ""},
        {.name = "order", .requires_value = true, .default_value = ""},
        {.name = "desc", .requires_value = false, .default_value = "false"},
    };
    const auto parsed = parse_args(args, specs);
    if (!parsed) {
        fail(out) << parsed.error() << std::endl;
        return;
    }

    request_type req;
    [[maybe_unused]] std::size_t next = 0;
    try {

        apply_page(req, *parsed);
        if (!parsed->positionals.empty()) {
            fail(out) << "Expected no arguments, got " << parsed->positionals.size() << "."
                      << std::endl;
            return;
        }
    } catch (const std::exception& e) {
        fail(out) << e.what() << std::endl;
        return;
    }

    auto result = do_auth_request<messaging::list_tenants_response>(
        out, session, std::string(req.nats_subject), req);
    if (!result)
        return;

    out << rfl::json::write(*result) << std::endl;
}

void tenant_commands::process_get(std::ostream& out,
                                  nats_client& session,
                                  const std::vector<std::string>& args) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating get request.";

    using request_type = messaging::get_tenant_request;
    if constexpr (request_type::requires_session) {
        if (!session.is_logged_in()) {
            fail(out) << "You must be logged in to run get." << std::endl;
            return;
        }
    }

    const std::vector<flag_spec> specs{};
    const auto parsed = parse_args(args, specs);
    if (!parsed) {
        fail(out) << parsed.error() << std::endl;
        return;
    }

    request_type req;
    [[maybe_unused]] std::size_t next = 0;
    try {

        if (parsed->positionals.size() != 1) {
            fail(out) << "Expected 1 arguments, got " << parsed->positionals.size() << "."
                      << std::endl;
            return;
        }
        read_token(req.key.code, parsed->positionals[next++], "code");
    } catch (const std::exception& e) {
        fail(out) << e.what() << std::endl;
        return;
    }

    auto result = do_auth_request<messaging::get_tenant_response>(
        out, session, std::string(req.nats_subject), req);
    if (!result)
        return;

    out << rfl::json::write(*result) << std::endl;
}

void tenant_commands::process_get_many(std::ostream& out,
                                       nats_client& session,
                                       const std::vector<std::string>& args) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating get-many request.";

    using request_type = messaging::get_many_tenants_request;
    if constexpr (request_type::requires_session) {
        if (!session.is_logged_in()) {
            fail(out) << "You must be logged in to run get-many." << std::endl;
            return;
        }
    }

    const std::vector<flag_spec> specs{};
    const auto parsed = parse_args(args, specs);
    if (!parsed) {
        fail(out) << parsed.error() << std::endl;
        return;
    }

    request_type req;
    [[maybe_unused]] std::size_t next = 0;
    try {

        if (parsed->positionals.empty() || parsed->positionals.size() % 1 != 0) {
            fail(out) << "Expected a multiple of 1 arguments, got " << parsed->positionals.size()
                      << "." << std::endl;
            return;
        }
        for (std::size_t i = 0; i < parsed->positionals.size(); i += 1) {
            messaging::tenant_key key;
            read_token(key.code, parsed->positionals[i + 0], "code");
            req.keys.push_back(std::move(key));
        }
    } catch (const std::exception& e) {
        fail(out) << e.what() << std::endl;
        return;
    }

    auto result = do_auth_request<messaging::get_many_tenants_response>(
        out, session, std::string(req.nats_subject), req);
    if (!result)
        return;

    out << rfl::json::write(*result) << std::endl;
}

void tenant_commands::process_add(std::ostream& out,
                                  nats_client& session,
                                  const std::vector<std::string>& args) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating add request.";

    using request_type = messaging::put_tenant_request;
    if constexpr (request_type::requires_session) {
        if (!session.is_logged_in()) {
            fail(out) << "You must be logged in to run add." << std::endl;
            return;
        }
    }

    const std::vector<flag_spec> specs{};
    const auto parsed = parse_args(args, specs);
    if (!parsed) {
        fail(out) << parsed.error() << std::endl;
        return;
    }

    request_type req;
    [[maybe_unused]] std::size_t next = 0;
    try {

        if (parsed->positionals.size() != 7 + 2) {
            fail(out) << "Expected " << (7 + 2) << " arguments, got " << parsed->positionals.size()
                      << "." << std::endl;
            return;
        }
        req.change.write.id = boost::uuids::random_generator()();
        read_token(req.change.write.code, parsed->positionals[next++], "code");
        read_token(req.change.write.name, parsed->positionals[next++], "name");
        read_token(req.change.write.type, parsed->positionals[next++], "type");
        read_token(req.change.write.description, parsed->positionals[next++], "description");
        read_token(req.change.write.hostname, parsed->positionals[next++], "hostname");
        read_token(req.change.write.status, parsed->positionals[next++], "status");
        req.intent.reason_code = parsed->positionals[next++];
        req.intent.commentary = parsed->positionals[next++];
        req.change.precondition.kind = ores::utility::domain::precondition_kind::must_not_exist;
    } catch (const std::exception& e) {
        fail(out) << e.what() << std::endl;
        return;
    }

    auto result = do_auth_request<messaging::put_tenant_response>(
        out, session, std::string(req.nats_subject), req);
    if (!result)
        return;

    out << rfl::json::write(*result) << std::endl;
}

void tenant_commands::process_set(std::ostream& out,
                                  nats_client& session,
                                  const std::vector<std::string>& args) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating set request.";

    using request_type = messaging::put_tenant_request;
    if constexpr (request_type::requires_session) {
        if (!session.is_logged_in()) {
            fail(out) << "You must be logged in to run set." << std::endl;
            return;
        }
    }

    const std::vector<flag_spec> specs{
        {.name = "version", .requires_value = true, .default_value = ""},
    };
    const auto parsed = parse_args(args, specs);
    if (!parsed) {
        fail(out) << parsed.error() << std::endl;
        return;
    }

    request_type req;
    [[maybe_unused]] std::size_t next = 0;
    try {

        if (parsed->positionals.size() != 7 + 2) {
            fail(out) << "Expected " << (7 + 2) << " arguments, got " << parsed->positionals.size()
                      << "." << std::endl;
            return;
        }
        req.change.write.id = boost::uuids::random_generator()();
        read_token(req.change.write.code, parsed->positionals[next++], "code");
        read_token(req.change.write.name, parsed->positionals[next++], "name");
        read_token(req.change.write.type, parsed->positionals[next++], "type");
        read_token(req.change.write.description, parsed->positionals[next++], "description");
        read_token(req.change.write.hostname, parsed->positionals[next++], "hostname");
        read_token(req.change.write.status, parsed->positionals[next++], "status");
        req.intent.reason_code = parsed->positionals[next++];
        req.intent.commentary = parsed->positionals[next++];
        req.change.precondition.kind = ores::utility::domain::precondition_kind::any;
        if (const auto& raw = parsed->flag("version"); !raw.empty()) {
            req.change.precondition.kind =
                ores::utility::domain::precondition_kind::must_match_version;
            req.change.precondition.version =
                ores::shell::app::from_token<std::uint32_t>(raw, "version");
        }
    } catch (const std::exception& e) {
        fail(out) << e.what() << std::endl;
        return;
    }

    auto result = do_auth_request<messaging::put_tenant_response>(
        out, session, std::string(req.nats_subject), req);
    if (!result)
        return;

    out << rfl::json::write(*result) << std::endl;
}

void tenant_commands::process_put_many(std::ostream& out,
                                       nats_client& session,
                                       const std::vector<std::string>& args) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating put-many request.";

    using request_type = messaging::put_many_tenants_request;
    if constexpr (request_type::requires_session) {
        if (!session.is_logged_in()) {
            fail(out) << "You must be logged in to run put-many." << std::endl;
            return;
        }
    }

    const std::vector<flag_spec> specs{
        {.name = "count", .requires_value = true, .default_value = ""},
    };
    const auto parsed = parse_args(args, specs);
    if (!parsed) {
        fail(out) << parsed.error() << std::endl;
        return;
    }

    request_type req;
    [[maybe_unused]] std::size_t next = 0;
    try {

        const auto count_raw = parsed->flag("count");
        if (count_raw.empty()) {
            fail(out) << "--count is required." << std::endl;
            return;
        }
        const auto change_count = ores::shell::app::from_token<std::uint32_t>(count_raw, "count");
        if (parsed->positionals.size() != change_count * 7 + 2) {
            fail(out) << "Expected " << (change_count * 7 + 2) << " arguments, got "
                      << parsed->positionals.size() << "." << std::endl;
            return;
        }
        for (std::uint32_t i = 0; i < change_count; ++i) {
            messaging::tenant_change change;
            read_token(change.write.id, parsed->positionals[next++], "id");
            read_token(change.write.code, parsed->positionals[next++], "code");
            read_token(change.write.name, parsed->positionals[next++], "name");
            read_token(change.write.type, parsed->positionals[next++], "type");
            read_token(change.write.description, parsed->positionals[next++], "description");
            read_token(change.write.hostname, parsed->positionals[next++], "hostname");
            read_token(change.write.status, parsed->positionals[next++], "status");
            change.precondition.kind = ores::utility::domain::precondition_kind::must_not_exist;
            req.changes.push_back(std::move(change));
        }
        req.intent.reason_code = parsed->positionals[next++];
        req.intent.commentary = parsed->positionals[next++];
    } catch (const std::exception& e) {
        fail(out) << e.what() << std::endl;
        return;
    }

    auto result = do_auth_request<messaging::put_many_tenants_response>(
        out, session, std::string(req.nats_subject), req);
    if (!result)
        return;

    out << rfl::json::write(*result) << std::endl;
}

void tenant_commands::process_delete(std::ostream& out,
                                     nats_client& session,
                                     const std::vector<std::string>& args) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating delete request.";

    using request_type = messaging::delete_tenant_request;
    if constexpr (request_type::requires_session) {
        if (!session.is_logged_in()) {
            fail(out) << "You must be logged in to run delete." << std::endl;
            return;
        }
    }

    const std::vector<flag_spec> specs{
        {.name = "version", .requires_value = true, .default_value = ""},
    };
    const auto parsed = parse_args(args, specs);
    if (!parsed) {
        fail(out) << parsed.error() << std::endl;
        return;
    }

    request_type req;
    [[maybe_unused]] std::size_t next = 0;
    try {

        if (parsed->positionals.size() != 1 + 2) {
            fail(out) << "Expected " << (1 + 2) << " arguments, got " << parsed->positionals.size()
                      << "." << std::endl;
            return;
        }
        read_token(req.removal.key.code, parsed->positionals[next++], "code");
        req.intent.reason_code = parsed->positionals[next++];
        req.intent.commentary = parsed->positionals[next++];
        if (const auto& raw = parsed->flag("version"); !raw.empty()) {
            req.removal.precondition.kind =
                ores::utility::domain::precondition_kind::must_match_version;
            req.removal.precondition.version =
                ores::shell::app::from_token<std::uint32_t>(raw, "version");
        }
    } catch (const std::exception& e) {
        fail(out) << e.what() << std::endl;
        return;
    }

    auto result = do_auth_request<messaging::delete_tenant_response>(
        out, session, std::string(req.nats_subject), req);
    if (!result)
        return;

    out << rfl::json::write(*result) << std::endl;
}

void tenant_commands::process_delete_many(std::ostream& out,
                                          nats_client& session,
                                          const std::vector<std::string>& args) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating delete-many request.";

    using request_type = messaging::delete_many_tenants_request;
    if constexpr (request_type::requires_session) {
        if (!session.is_logged_in()) {
            fail(out) << "You must be logged in to run delete-many." << std::endl;
            return;
        }
    }

    const std::vector<flag_spec> specs{};
    const auto parsed = parse_args(args, specs);
    if (!parsed) {
        fail(out) << parsed.error() << std::endl;
        return;
    }

    request_type req;
    [[maybe_unused]] std::size_t next = 0;
    try {

        if (parsed->positionals.size() < 1 + 2 || (parsed->positionals.size() - 2) % 1 != 0) {
            fail(out) << "Expected a whole number of key groups and an intent, got "
                      << parsed->positionals.size() << "." << std::endl;
            return;
        }
        const std::size_t key_groups = (parsed->positionals.size() - 2) / 1;
        for (std::size_t i = 0; i < key_groups; ++i) {
            messaging::tenant_key key;
            read_token(key.code, parsed->positionals[i * 1 + 0], "code");
            req.removals.push_back(messaging::tenant_removal{.key = std::move(key)});
        }
        req.intent.reason_code = parsed->positionals[parsed->positionals.size() - 2];
        req.intent.commentary = parsed->positionals[parsed->positionals.size() - 1];
    } catch (const std::exception& e) {
        fail(out) << e.what() << std::endl;
        return;
    }

    auto result = do_auth_request<messaging::delete_many_tenants_response>(
        out, session, std::string(req.nats_subject), req);
    if (!result)
        return;

    out << rfl::json::write(*result) << std::endl;
}

void tenant_commands::process_versions(std::ostream& out,
                                       nats_client& session,
                                       const std::vector<std::string>& args) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating versions request.";

    using request_type = messaging::list_tenant_versions_request;
    if constexpr (request_type::requires_session) {
        if (!session.is_logged_in()) {
            fail(out) << "You must be logged in to run versions." << std::endl;
            return;
        }
    }

    const std::vector<flag_spec> specs{
        {.name = "offset", .requires_value = true, .default_value = ""},
        {.name = "limit", .requires_value = true, .default_value = ""},
        {.name = "order", .requires_value = true, .default_value = ""},
        {.name = "desc", .requires_value = false, .default_value = "false"},
    };
    const auto parsed = parse_args(args, specs);
    if (!parsed) {
        fail(out) << parsed.error() << std::endl;
        return;
    }

    request_type req;
    [[maybe_unused]] std::size_t next = 0;
    try {

        if (parsed->positionals.size() != 1) {
            fail(out) << "Expected 1 arguments, got " << parsed->positionals.size() << "."
                      << std::endl;
            return;
        }
        read_token(req.key.code, parsed->positionals[next++], "code");
        apply_page(req, *parsed);
    } catch (const std::exception& e) {
        fail(out) << e.what() << std::endl;
        return;
    }

    auto result = do_auth_request<messaging::list_tenant_versions_response>(
        out, session, std::string(req.nats_subject), req);
    if (!result)
        return;

    out << rfl::json::write(*result) << std::endl;
}

void tenant_commands::process_version(std::ostream& out,
                                      nats_client& session,
                                      const std::vector<std::string>& args) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating version request.";

    using request_type = messaging::get_tenant_version_request;
    if constexpr (request_type::requires_session) {
        if (!session.is_logged_in()) {
            fail(out) << "You must be logged in to run version." << std::endl;
            return;
        }
    }

    const std::vector<flag_spec> specs{
        {.name = "version", .requires_value = true, .default_value = ""},
    };
    const auto parsed = parse_args(args, specs);
    if (!parsed) {
        fail(out) << parsed.error() << std::endl;
        return;
    }

    request_type req;
    [[maybe_unused]] std::size_t next = 0;
    try {

        if (parsed->positionals.size() != 1) {
            fail(out) << "Expected 1 arguments, got " << parsed->positionals.size() << "."
                      << std::endl;
            return;
        }
        read_token(req.key.tenant.code, parsed->positionals[next++], "code");
        req.key.version =
            ores::shell::app::from_token<std::uint32_t>(parsed->flag("version"), "version");
    } catch (const std::exception& e) {
        fail(out) << e.what() << std::endl;
        return;
    }

    auto result = do_auth_request<messaging::get_tenant_version_response>(
        out, session, std::string(req.nats_subject), req);
    if (!result)
        return;

    out << rfl::json::write(*result) << std::endl;
}

}
