/*
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
 */
#include "ores.shell/app/commands/reporting/pricing_engines_commands.hpp"
#include "ores.analytics.api/messaging/pricing_model_config_protocol.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.ore.api/messaging/pricing_engines_protocol.hpp"
#include "ores.shell/app/command_args.hpp"
#include "ores.shell/app/command_feedback.hpp"
#include "ores.shell/app/request_helpers.hpp"
#include "ores.shell/app/shell_root_menu.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <cli/cli.h>
#include <cstdint>
#include <exception>
#include <format>
#include <fstream>
#include <optional>
#include <ostream>
#include <string>
#include <vector>

namespace ores::shell::app::commands {

using ores::nats::service::nats_client;
namespace an = ores::analytics::messaging;
namespace ore = ores::ore::messaging;

namespace {

constexpr std::uint32_t page = 100;

/**
 * @brief The reporting configuration a pricing engine configuration fills, or
 * nothing when no configuration has that id.
 *
 * The screen names a configuration by its own id, and the ORE document is
 * written from the reporting configuration the record fills, which is what
 * analytics finds a document by. The join is made here so a person types the id
 * they read on the screen.
 */
std::optional<std::string>
reporting_configuration_of(std::ostream& out, nats_client& session, const std::string& id) {
    boost::uuids::uuid key;
    try {
        key = boost::uuids::string_generator()(id);
    } catch (const std::exception&) {
        fail(out) << "The configuration is named by an identifier, and '" << id << "' is not one."
                  << std::endl;
        return std::nullopt;
    }

    an::pricing_model_configs_filter filter;
    filter.id_one_of = std::vector<boost::uuids::uuid>{key};
    an::list_pricing_model_configs_request req;
    req.limit = page;
    req.filter = filter;

    const auto response =
        do_request(out, session, req, ores::nats::service::default_request_timeout, true);
    if (!response)
        return std::nullopt;
    if (response->configs.empty()) {
        fail(out) << "No pricing engine configuration has the id " << id << "." << std::endl;
        return std::nullopt;
    }
    return boost::uuids::to_string(response->configs.front().configuration_id);
}

}

void pricing_engines_commands::process_export(std::ostream& out,
                                              nats_client& session,
                                              const std::vector<std::string>& args) {
    const std::vector<flag_spec> specs{
        {.name = "file", .requires_value = true, .default_value = ""},
    };
    const auto parsed = parse_args(args, specs);
    if (!parsed) {
        fail(out) << parsed.error() << std::endl;
        return;
    }
    if (parsed->positionals.size() != 1) {
        fail(out) << "Expected 1 argument, got " << parsed->positionals.size() << "." << std::endl;
        return;
    }
    if (!session.is_logged_in()) {
        fail(out) << "You must be logged in to run export." << std::endl;
        return;
    }

    const auto configuration =
        reporting_configuration_of(out, session, parsed->positionals.front());
    if (!configuration)
        return;

    const ore::export_pricing_engines_request req{.configuration_id = *configuration};
    const auto exported =
        do_request(out, session, req, ores::nats::service::default_request_timeout, true);
    if (!exported)
        return;
    if (!exported->success || exported->files.empty()) {
        fail(out) << (exported->message.empty() ? "The document could not be written." :
                                                  exported->message)
                  << std::endl;
        for (const auto& problem : exported->problems)
            fail(out) << "  " << problem.code << ": " << problem.message << std::endl;
        return;
    }

    write_document(out, parsed->flag("file"), exported->files.front());
}

bool pricing_engines_commands::write_document(std::ostream& out,
                                              const std::string& path,
                                              const ore::ore_document& file) {
    if (path.empty()) {
        out << file.content;
        return true;
    }
    std::ofstream stream(path, std::ios::binary | std::ios::trunc);
    if (!stream) {
        fail(out) << "Cannot write: " << path << std::endl;
        return false;
    }
    stream << file.content;
    stream.close();
    if (!stream) {
        fail(out) << "Failed to write: " << path << std::endl;
        return false;
    }
    out << "Wrote " << path << " as " << file.name << "." << std::endl;
    return true;
}

void pricing_engines_commands::register_commands(cli::Menu& root_menu, nats_client& session) {
    auto menu = std::make_unique<cli::Menu>("pricing_engines");

    menu->Insert("export",
                 [&session](std::ostream& out, std::vector<std::string> args) {
                     process_export(out, session, args);
                 },
                 "Write a pricing engine configuration as the ORE document ORE reads",
                 {"configuration_id [--file <path>]"});

    ores::shell::app::insert_menu(root_menu, std::move(menu));
}

}
