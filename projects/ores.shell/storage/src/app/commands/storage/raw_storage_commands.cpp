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
#include "ores.shell/app/commands/storage/raw_storage_commands.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.shell/app/command_args.hpp"
#include "ores.shell/app/command_feedback.hpp"
#include "ores.shell/app/http_base_url.hpp"
#include "ores.shell/app/shell_root_menu.hpp"
#include "ores.storage.api/messaging/objects_protocol.hpp"
#include "ores.storage.core/net/storage_transfer.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <cli/cli.h>
#include <cstdint>
#include <filesystem>
#include <optional>
#include <ostream>
#include <rfl/json.hpp>
#include <string>
#include <utility>
#include <vector>

namespace ores::shell::app::commands {

using namespace logging;
using ores::nats::service::nats_client;
using ores::storage::net::storage_transfer;

namespace {

namespace fs = std::filesystem;

inline static std::string_view logger_name = "ores.shell.app.commands.storage.raw_storage_commands";

auto& lg() {
    static auto instance = make_logger(logger_name);
    return instance;
}

constexpr std::uint32_t default_list_limit = 100;

/**
 * @brief Parse a command's tokens, or say what was wrong with them.
 *
 * A logged-in session is what supplies the bearer token every call carries, so
 * a command asks for one before it asks for anything else.
 */
std::optional<parsed_args> parse_command(std::ostream& out,
                                         nats_client& session,
                                         const std::vector<std::string>& args,
                                         std::size_t expected_positionals,
                                         const std::vector<flag_spec>& specs,
                                         const char* verb) {
    if (!session.is_logged_in()) {
        fail(out) << "You must be logged in to run storage " << verb << "." << std::endl;
        return std::nullopt;
    }

    auto parsed = parse_args(args, specs);
    if (!parsed) {
        fail(out) << parsed.error() << std::endl;
        return std::nullopt;
    }
    if (parsed->positionals.size() != expected_positionals) {
        fail(out) << "Expected " << expected_positionals << " arguments, got "
                  << parsed->positionals.size() << "." << std::endl;
        return std::nullopt;
    }
    return std::move(*parsed);
}

void process_put(std::ostream& out, nats_client& session, const std::vector<std::string>& args) {
    const auto parsed = parse_command(out, session, args, 3, {}, "put");
    if (!parsed)
        return;
    const auto& bucket = parsed->positionals[0];
    const auto& key = parsed->positionals[1];
    const fs::path source(parsed->positionals[2]);

    std::error_code ec;
    if (!fs::is_regular_file(source, ec)) {
        fail(out) << "Not a file: " << source.string() << std::endl;
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Uploading " << source.string() << " to " << bucket << "/" << key;
    try {
        storage_transfer transfer(default_http_base_url(), session.bearer_token());
        transfer.upload(bucket, key, source);
    } catch (const std::exception& e) {
        fail(out) << "Upload failed: " << e.what() << std::endl;
        return;
    }

    out << "✓ Uploaded " << source.string() << " to " << bucket << "/" << key << " ("
        << fs::file_size(source, ec) << " bytes)" << std::endl;
}

void process_get(std::ostream& out, nats_client& session, const std::vector<std::string>& args) {
    const auto parsed = parse_command(out, session, args, 3, {}, "get");
    if (!parsed)
        return;
    const auto& bucket = parsed->positionals[0];
    const auto& key = parsed->positionals[1];
    const fs::path destination(parsed->positionals[2]);

    BOOST_LOG_SEV(lg(), debug) << "Downloading " << bucket << "/" << key << " to "
                               << destination.string();
    try {
        storage_transfer transfer(default_http_base_url(), session.bearer_token());
        transfer.download(bucket, key, destination);
    } catch (const std::exception& e) {
        fail(out) << "Download failed: " << e.what() << std::endl;
        return;
    }

    std::error_code ec;
    out << "✓ Downloaded " << bucket << "/" << key << " to " << destination.string() << " ("
        << fs::file_size(destination, ec) << " bytes)" << std::endl;
}

void process_delete(std::ostream& out, nats_client& session, const std::vector<std::string>& args) {
    const auto parsed = parse_command(out, session, args, 2, {}, "delete");
    if (!parsed)
        return;
    const auto& bucket = parsed->positionals[0];
    const auto& key = parsed->positionals[1];

    BOOST_LOG_SEV(lg(), debug) << "Deleting " << bucket << "/" << key;
    std::string body;
    try {
        storage_transfer transfer(default_http_base_url(), session.bearer_token());
        body = transfer.remove(bucket, key);
    } catch (const std::exception& e) {
        fail(out) << "Delete failed: " << e.what() << std::endl;
        return;
    }

    const auto result = rfl::json::read<ores::storage::messaging::delete_objects_response>(body);
    if (!result) {
        fail(out) << "Cannot read the server's answer: " << body << std::endl;
        return;
    }

    if (result->removed)
        out << "✓ Removed " << bucket << "/" << key << std::endl;
    else
        out << "Nothing at " << bucket << "/" << key << std::endl;
}

void process_list(std::ostream& out, nats_client& session, const std::vector<std::string>& args) {
    const std::vector<flag_spec> specs{
        {.name = "prefix", .requires_value = true, .default_value = ""},
        {.name = "offset", .requires_value = true, .default_value = "0"},
        {.name = "limit",
         .requires_value = true,
         .default_value = std::to_string(default_list_limit)},
    };
    const auto parsed = parse_command(out, session, args, 1, specs, "list");
    if (!parsed)
        return;
    const auto& bucket = parsed->positionals[0];

    const auto offset = parse_uint32(parsed->flag("offset"));
    const auto limit = parse_uint32(parsed->flag("limit"));
    if (!offset || !limit) {
        fail(out) << "offset and limit must be whole numbers" << std::endl;
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Listing " << bucket << " prefix=" << parsed->flag("prefix");
    std::string body;
    try {
        storage_transfer transfer(default_http_base_url(), session.bearer_token());
        body = transfer.list(bucket, parsed->flag("prefix"), *offset, *limit);
    } catch (const std::exception& e) {
        fail(out) << "List failed: " << e.what() << std::endl;
        return;
    }

    const auto listing = rfl::json::read<ores::storage::messaging::list_objects_response>(body);
    if (!listing) {
        fail(out) << "Cannot read the server's answer: " << body << std::endl;
        return;
    }

    for (const auto& object : listing->objects)
        out << object.size_bytes << "\t" << object.key << std::endl;
    out << listing->objects.size() << " of " << listing->total_available_count << " object(s) in "
        << bucket << std::endl;
}

}

void raw_storage_commands::register_commands(cli::Menu& root_menu, nats_client& session) {
    auto menu = std::make_unique<cli::Menu>("storage");

    menu->Insert(
        "put",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_put(std::ref(out), std::ref(session), std::move(args));
        },
        "put <bucket> <key> <local-path>");

    menu->Insert(
        "get",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_get(std::ref(out), std::ref(session), std::move(args));
        },
        "get <bucket> <key> <local-path>");

    menu->Insert(
        "delete",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_delete(std::ref(out), std::ref(session), std::move(args));
        },
        "delete <bucket> <key>");

    menu->Insert(
        "list",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_list(std::ref(out), std::ref(session), std::move(args));
        },
        "list <bucket> [--prefix <v>] [--offset <v>] [--limit <v>]");

    ores::shell::app::insert_menu(root_menu, std::move(menu));
}

}
