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
#include <sstream>
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

void pricing_engines_commands::process_import(std::ostream& out,
                                              nats_client& session,
                                              const std::vector<std::string>& args) {
    const std::vector<flag_spec> specs{
        {.name = "file", .requires_value = true, .default_value = ""},
        {.name = "new-name", .requires_value = true, .default_value = ""},
        {.name = "new-variant", .requires_value = true, .default_value = ""},
        {.name = "reason", .requires_value = true, .default_value = "system.import"},
        {.name = "commentary", .requires_value = true, .default_value = ""},
        {.name = "dry-run", .requires_value = false, .default_value = "false"},
    };
    const auto parsed = parse_args(args, specs);
    if (!parsed) {
        fail(out) << parsed.error() << std::endl;
        return;
    }
    const auto& file_path = parsed->flag("file");
    const auto& new_name = parsed->flag("new-name");
    if (file_path.empty()) {
        fail(out) << "Name the file with --file <path>." << std::endl;
        return;
    }
    if (parsed->positionals.size() + (new_name.empty() ? 0 : 1) != 1) {
        fail(out) << "Name one configuration: its id, or --new-name <name>." << std::endl;
        return;
    }
    if (!session.is_logged_in()) {
        fail(out) << "You must be logged in to run import." << std::endl;
        return;
    }

    std::ifstream stream(file_path, std::ios::binary);
    if (!stream) {
        fail(out) << "Cannot read: " << file_path << std::endl;
        return;
    }
    std::ostringstream content;
    content << stream.rdbuf();

    // A file put into an existing configuration names that configuration by the
    // id the screen shows; the document is read by the reporting configuration
    // the record fills, so the two are joined here.
    std::string configuration;
    if (!parsed->positionals.empty()) {
        const auto resolved = reporting_configuration_of(out, session, parsed->positionals.front());
        if (!resolved)
            return;
        configuration = *resolved;
    }

    const ore::preview_pricing_engines_import_request preview_request{
        .content = content.str(), .target_configuration_id = configuration};
    const auto preview = do_request(
        out, session, preview_request, ores::nats::service::default_request_timeout, true);
    if (!preview)
        return;
    if (!preview->success) {
        fail(out) << (preview->message.empty() ? "The file could not be read." : preview->message)
                  << std::endl;
        return;
    }

    out << preview->message << std::endl;
    if (preview->root_element != "PricingEngines")
        out << "Root element: " << preview->root_element << std::endl;
    for (const auto& product : preview->products)
        out << "  " << product.type_code << ": model " << product.model << ", engine "
            << product.engine << ", " << product.parameters << " setting(s)" << std::endl;
    for (const auto& name : preview->global_parameters)
        out << "  configuration setting: " << name << std::endl;
    for (const auto& issue : preview->issues)
        out << "  " << issue.kind << ": " << issue.subject << " - " << issue.detail << std::endl;
    out << "Added: " << preview->added << ", changed: " << preview->changed
        << ", held: " << preview->held << ", removed: " << preview->removed << std::endl;
    for (const auto& row : preview->rows)
        out << "  " << row.change << " " << row.key << ": " << row.what << std::endl;

    // A file that omits a row usually omits it by accident, so every removed row
    // is kept unless the person says otherwise. The command states no per-row
    // choice, so it takes the default the screen starts from.
    std::vector<std::string> removals_kept;
    for (const auto& row : preview->rows)
        if (row.change == "removed")
            removals_kept.push_back(row.key);

    if (parsed->flag_set("dry-run")) {
        out << "Dry run: nothing was written." << std::endl;
        return;
    }

    const ore::import_pricing_engines_request import_request{
        .content = preview_request.content,
        .target_configuration_id = configuration,
        .new_configuration_name = new_name,
        .new_configuration_variant = parsed->flag("new-variant"),
        .removals_kept = removals_kept,
        .reason_code = parsed->flag("reason"),
        .commentary = parsed->flag("commentary")};
    const auto imported = do_request(
        out, session, import_request, ores::nats::service::default_request_timeout, true);
    if (!imported)
        return;
    if (!imported->success) {
        fail(out) << (imported->message.empty() ? "The configuration was not written." :
                                                  imported->message)
                  << std::endl;
        for (const auto& error : imported->errors)
            fail(out) << "  " << (error.key.empty() ? std::string("configuration") : error.key)
                      << ": " << error.message << std::endl;
        return;
    }
    out << "Imported into " << imported->configuration_name << " (" << imported->configuration_id
        << "): " << imported->added << " added, " << imported->changed << " changed, "
        << imported->deleted << " deleted, " << imported->kept << " kept." << std::endl;
}

void pricing_engines_commands::register_commands(cli::Menu& root_menu, nats_client& session) {
    auto menu = std::make_unique<cli::Menu>("pricing_engines");

    menu->Insert("export",
                 [&session](std::ostream& out, std::vector<std::string> args) {
                     process_export(out, session, args);
                 },
                 "Write a pricing engine configuration as the ORE document ORE reads",
                 {"configuration_id [--file <path>]"});

    menu->Insert("import",
                 [&session](std::ostream& out, std::vector<std::string> args) {
                     process_import(out, session, args);
                 },
                 "Import an ORE pricing engines document into a configuration, or into a new one",
                 {"[configuration_id | --new-name <name>] --file <path> [--new-variant <variant>] "
                  "[--reason <code>] [--commentary <text>] [--dry-run]"});

    ores::shell::app::insert_menu(root_menu, std::move(menu));
}

}
