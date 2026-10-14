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
#include "ores.shell/app/commands/trading/ore_commands.hpp"
#include "ores.ore.api/messaging/ore_import_protocol.hpp"
#include "ores.ore.api/messaging/run_configuration_protocol.hpp"
#include "ores.ore.api/net/ore_storage.hpp"
#include "ores.ore.core/planner/import_choices.hpp"
#include "ores.ore.core/xml/exporter.hpp"
#include "ores.reporting.api/messaging/report_instance_protocol.hpp"
#include "ores.reporting.api/messaging/report_operations_protocol.hpp"
#include "ores.shell/app/command_args.hpp"
#include "ores.shell/app/command_feedback.hpp"
#include "ores.shell/app/commands/workflow/workflow_run_commands.hpp"
#include "ores.shell/app/http_base_url.hpp"
#include "ores.shell/app/request_helpers.hpp"
#include "ores.shell/app/shell_root_menu.hpp"
#include "ores.storage.api/messaging/objects_protocol.hpp"
#include "ores.storage.core/net/storage_transfer.hpp"
#include "ores.trading.api/messaging/trade_operations_protocol.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <chrono>
#include <cli/cli.h>
#include <filesystem>
#include <fstream>
#include <iterator>
#include <ostream>
#include <sstream>
#include <string>
#include <system_error>
#include <thread>
#include <vector>

namespace ores::shell::app::commands {

using namespace logging;
using ores::nats::service::nats_client;

namespace {

constexpr std::chrono::seconds default_import_timeout(600);
constexpr std::chrono::seconds import_request_timeout(60);
constexpr std::chrono::seconds export_request_timeout(300);
constexpr std::uint32_t default_export_limit = 10000;
constexpr std::size_t max_inline_run_input = 900 * 1024;

std::optional<boost::uuids::uuid>
parse_uuid(std::ostream& out, const std::string& value, std::string_view what) {
    try {
        return boost::uuids::string_generator()(value);
    } catch (const std::exception&) {
        fail(out) << "Invalid " << what << ": " << value << std::endl;
        return std::nullopt;
    }
}


constexpr std::chrono::seconds default_build_packages_timeout(600);
constexpr std::chrono::seconds package_poll_interval(2);
constexpr std::chrono::seconds list_request_timeout(30);

/**
 * @brief The folder a package unpacks into: its file name without the archive suffix.
 */
std::string package_folder(const std::string& key) {
    auto name = std::filesystem::path(key).filename().string();
    for (const std::string_view suffix : {".tar.gz", ".tgz"}) {
        if (name.size() > suffix.size() && name.ends_with(suffix)) {
            name.resize(name.size() - suffix.size());
            break;
        }
    }
    return name;
}

/**
 * @brief Downloads and unpacks every package stored for an instance.
 *
 * @return the folders written, or nothing when the request failed. An empty list means
 * the instance stored no package.
 */
std::optional<std::vector<std::filesystem::path>>
fetch_packages_into(std::ostream& out,
                    nats_client& session,
                    const std::string& report_instance_id,
                    const std::filesystem::path& out_dir) {
    ores::storage::messaging::list_objects_request list;
    list.bucket = std::string(ores::ore::net::ore_storage::bucket);
    list.prefix = ores::ore::net::ore_storage::package_prefix(report_instance_id);
    list.limit = 1000;

    const auto listing = do_auth_request<ores::storage::messaging::list_objects_response>(
        out, session, list.nats_subject, list, list_request_timeout);
    if (!listing)
        return std::nullopt;
    if (!listing->success) {
        fail(out) << "Cannot list the packages: " << listing->message << std::endl;
        return std::nullopt;
    }

    std::vector<std::filesystem::path> folders;
    if (listing->objects.empty())
        return folders;

    ores::storage::net::storage_transfer transfer(default_http_base_url(), session.bearer_token());
    for (const auto& object : listing->objects) {
        const auto folder = out_dir / package_folder(object.key);
        try {
            std::filesystem::create_directories(folder);
            transfer.fetch_and_unpack(list.bucket, object.key, folder);
        } catch (const std::exception& e) {
            fail(out) << "Cannot fetch " << object.key << ": " << e.what() << std::endl;
            return std::nullopt;
        }
        folders.push_back(folder);
    }
    return folders;
}

/**
 * @brief The moment a package instance's age is measured from.
 *
 * A completed instance is as old as its completion; one that never completed is as old
 * as its start. An instance with neither has no age to compare, so it is left alone.
 */
std::optional<std::chrono::system_clock::time_point>
package_age_origin(const ores::reporting::domain::report_instance& instance) {
    if (instance.completed_at)
        return instance.completed_at;
    return instance.started_at;
}

/**
 * @brief Every package instance the caller's tenant can see.
 *
 * Pages through the listing, because the point of the command is the instances that have
 * accumulated rather than the first page of them.
 */
std::optional<std::vector<ores::reporting::domain::report_instance>>
list_package_instances(std::ostream& out, nats_client& session) {
    constexpr std::uint32_t page_size = 500;
    std::vector<ores::reporting::domain::report_instance> all;
    for (std::uint32_t offset = 0;; offset += page_size) {
        ores::reporting::messaging::list_report_instances_request request;
        request.offset = offset;
        request.limit = page_size;
        request.filter = ores::reporting::messaging::report_instances_filter{};
        request.filter->kind = std::string("package");

        const auto reply =
            do_auth_request<ores::reporting::messaging::list_report_instances_response>(
                out, session, request.nats_subject, request, list_request_timeout);
        if (!reply)
            return std::nullopt;
        all.insert(all.end(), reply->instances.begin(), reply->instances.end());
        if (reply->instances.empty() || all.size() >= reply->total)
            break;
    }
    return all;
}

/**
 * @brief Removes every package stored under an instance's prefix.
 *
 * @return false when a listing or a removal failed, so the caller leaves the instance in
 * place rather than deleting the only record of packages it could not remove.
 */
bool remove_stored_packages(std::ostream& out,
                            nats_client& session,
                            const std::string& report_instance_id) {
    ores::storage::messaging::list_objects_request list;
    list.bucket = std::string(ores::ore::net::ore_storage::bucket);
    list.prefix = ores::ore::net::ore_storage::package_prefix(report_instance_id);
    list.limit = 1000;

    const auto listing = do_auth_request<ores::storage::messaging::list_objects_response>(
        out, session, list.nats_subject, list, list_request_timeout);
    if (!listing)
        return false;
    if (!listing->success) {
        fail(out) << "Cannot list the packages of " << report_instance_id << ": "
                  << listing->message << std::endl;
        return false;
    }

    for (const auto& object : listing->objects) {
        ores::storage::messaging::delete_objects_request request;
        request.bucket = list.bucket;
        request.key = object.key;

        const auto reply = do_auth_request<ores::storage::messaging::delete_objects_response>(
            out, session, request.nats_subject, request, list_request_timeout);
        if (!reply)
            return false;
        if (!reply->success) {
            fail(out) << "Cannot remove " << object.key << ": " << reply->message << std::endl;
            return false;
        }
    }
    return true;
}

/**
 * @brief Deletes a report instance by name.
 */
bool remove_instance(std::ostream& out, nats_client& session, const std::string& name) {
    ores::reporting::messaging::delete_report_instance_request request;
    request.removal.key.name = name;
    request.intent.reason_code = "system.admin_reset";
    request.intent.commentary =
        "Remove a package instance, and the packages it stored, once they are old enough.";

    const auto reply =
        do_auth_request<ores::reporting::messaging::delete_report_instance_response>(
            out, session, request.nats_subject, request, import_request_timeout);
    if (!reply)
        return false;
    if (reply->result.outcome != ores::utility::domain::outcome::ok) {
        fail(out) << "Cannot remove report instance " << name << ": " << reply->result.message
                  << std::endl;
        return false;
    }
    return true;
}

}

void ore_commands::register_commands(cli::Menu& root_menu, nats_client& session) {
    auto ore_menu = std::make_unique<cli::Menu>("ore");

    ore_menu->Insert("upload",
                     [&session](std::ostream& out, std::vector<std::string> args) {
                         auto parsed =
                             parse_args(args, {{.name = "request-id", .requires_value = true}});
                         if (!parsed) {
                             fail(out) << parsed.error() << std::endl;
                             return;
                         }
                         if (parsed->positionals.size() != 1) {
                             fail(out) << "Usage: ore upload <src_dir> [--request-id <uuid>]"
                                       << std::endl;
                             return;
                         }
                         process_upload(std::ref(out),
                                        std::ref(session),
                                        parsed->positionals.front(),
                                        parsed->flag("request-id"));
                     },
                     "Pack a directory of ORE documents and upload it to the platform bucket",
                     {"src_dir [--request-id <uuid>]"});

    ore_menu->Insert("import",
                     [&session](std::ostream& out, std::vector<std::string> args) {
                         auto parsed = parse_args(
                             args,
                             {{.name = "party-id", .requires_value = true},
                              {.name = "choices-file", .requires_value = true},
                              {.name = "parent-portfolio-name", .requires_value = true},
                              {.name = "official"},
                              {.name = "timeout",
                               .requires_value = true,
                               .default_value = std::to_string(default_import_timeout.count())}});
                         if (!parsed) {
                             fail(out) << parsed.error() << std::endl;
                             return;
                         }
                         if (parsed->positionals.size() != 1) {
                             fail(out)
                                 << "Usage: ore import <request_id> [--party-id <uuid>] "
                                    "[--parent-portfolio-name <name>] [--choices-file <path>] "
                                    "[--official] [--timeout <seconds>]"
                                 << std::endl;
                             return;
                         }

                         auto timeout = parse_positive_seconds(parsed->flag("timeout"));
                         if (!timeout) {
                             fail(out) << "Timeout must be a positive number of seconds: "
                                       << parsed->flag("timeout") << std::endl;
                             return;
                         }

                         process_import(std::ref(out),
                                        std::ref(session),
                                        parsed->positionals.front(),
                                        parsed->flag("party-id"),
                                        parsed->flag("choices-file"),
                                        parsed->flag("parent-portfolio-name"),
                                        parsed->flag_set("official"),
                                        *timeout);
                     },
                     "Import an uploaded ORE tarball and wait for the workflow to finish",
                     {"request_id [--party-id <uuid>] [--parent-portfolio-name <name>] "
                      "[--choices-file <path>] [--official] [--timeout <seconds>]"});

    ore_menu->Insert(
        "export",
        [&session](std::ostream& out, std::vector<std::string> args) {
            auto parsed = parse_args(args,
                                     {{.name = "node-id", .requires_value = true},
                                      {.name = "limit",
                                       .requires_value = true,
                                       .default_value = std::to_string(default_export_limit)}});
            if (!parsed) {
                fail(out) << parsed.error() << std::endl;
                return;
            }
            if (parsed->positionals.size() != 1) {
                fail(out) << "Usage: ore export <output_file> [--node-id <uuid>] [--limit <n>]"
                          << std::endl;
                return;
            }

            auto limit = parse_uint32(parsed->flag("limit"));
            if (!limit) {
                fail(out) << "Flag --limit must be an unsigned integer: " << parsed->flag("limit")
                          << std::endl;
                return;
            }

            process_export(std::ref(out),
                           std::ref(session),
                           parsed->positionals.front(),
                           parsed->flag("node-id"),
                           *limit);
        },
        "Write a portfolio's trades back out as ORE portfolio XML",
        {"output_file [--node-id <uuid>] [--limit <n>]"});

    ore_menu->Insert("import-run",
                     [&session](std::ostream& out, std::vector<std::string> args) {
                         auto parsed = parse_args(args, {{.name = "name", .requires_value = true}});
                         if (!parsed) {
                             fail(out) << parsed.error() << std::endl;
                             return;
                         }
                         if (parsed->positionals.size() != 2) {
                             fail(out) << "Usage: ore import-run <report_definition_id> <src_dir> "
                                          "[--name <name>]"
                                       << std::endl;
                             return;
                         }
                         process_import_run(std::ref(out),
                                            std::ref(session),
                                            parsed->positionals.at(0),
                                            parsed->positionals.at(1),
                                            parsed->flag("name"));
                     },
                     "Store an ORE input directory's run configuration against a report definition",
                     {"report_definition_id src_dir [--name <name>]"});

    ore_menu->Insert("export-run",
                     [&session](std::ostream& out, std::vector<std::string> args) {
                         auto parsed = parse_args(args, {});
                         if (!parsed) {
                             fail(out) << parsed.error() << std::endl;
                             return;
                         }
                         if (parsed->positionals.size() != 2) {
                             fail(out) << "Usage: ore export-run <report_definition_id> <out_dir>"
                                       << std::endl;
                             return;
                         }
                         process_export_run(std::ref(out),
                                            std::ref(session),
                                            parsed->positionals.at(0),
                                            parsed->positionals.at(1));
                     },
                     "Write a report definition's run configuration into a directory",
                     {"report_definition_id out_dir"});

    ore_menu->Insert(
        "build-packages",
        [&session](std::ostream& out, std::vector<std::string> args) {
            auto parsed = parse_args(
                args,
                {{.name = "timeout",
                  .requires_value = true,
                  .default_value = std::to_string(default_build_packages_timeout.count())}});
            if (!parsed) {
                fail(out) << parsed.error() << std::endl;
                return;
            }
            if (parsed->positionals.size() != 2) {
                fail(out) << "Usage: ore build-packages <report_definition_id> <out_dir> "
                             "[--timeout <seconds>]"
                          << std::endl;
                return;
            }
            const auto timeout = parse_positive_seconds(parsed->flag("timeout"));
            if (!timeout) {
                fail(out) << "Timeout must be a positive number of seconds: "
                          << parsed->flag("timeout") << std::endl;
                return;
            }
            process_build_packages(std::ref(out),
                                   std::ref(session),
                                   parsed->positionals.at(0),
                                   parsed->positionals.at(1),
                                   *timeout);
        },
        "Build the packages the compute grid would receive for a report, and unpack them",
        {"report_definition_id out_dir [--timeout <seconds>]"});

    ore_menu->Insert("fetch-packages",
                     [&session](std::ostream& out, std::vector<std::string> args) {
                         auto parsed = parse_args(args, {});
                         if (!parsed) {
                             fail(out) << parsed.error() << std::endl;
                             return;
                         }
                         if (parsed->positionals.size() != 2) {
                             fail(out) << "Usage: ore fetch-packages <report_instance_id> <out_dir>"
                                       << std::endl;
                             return;
                         }
                         process_fetch_packages(std::ref(out),
                                                std::ref(session),
                                                parsed->positionals.at(0),
                                                parsed->positionals.at(1));
                     },
                     "Download and unpack the packages a report instance stored",
                     {"report_instance_id out_dir"});

    ore_menu->Insert(
        "clean-packages",
        [&session](std::ostream& out, std::vector<std::string> args) {
            auto parsed = parse_args(
                args,
                {{.name = "older-than", .requires_value = true, .default_value = ""},
                 {.name = "dry-run", .requires_value = false, .default_value = "false"}});
            if (!parsed) {
                fail(out) << parsed.error() << std::endl;
                return;
            }
            if (!parsed->positionals.empty()) {
                fail(out) << "Usage: ore clean-packages --older-than <seconds> [--dry-run]"
                          << std::endl;
                return;
            }
            const auto older_than = parse_positive_seconds(parsed->flag("older-than"));
            if (!older_than) {
                fail(out) << "The age must be a positive number of seconds: "
                          << parsed->flag("older-than") << std::endl;
                return;
            }
            process_clean_packages(
                std::ref(out), std::ref(session), *older_than, parsed->flag_set("dry-run"));
        },
        "Remove the package instances that are old enough, and the packages they stored",
        {"--older-than <seconds> [--dry-run]"});

    ores::shell::app::insert_menu(root_menu, std::move(ore_menu));
}

void ore_commands::process_upload(std::ostream& out,
                                  nats_client& session,
                                  const std::string& src_dir,
                                  const std::string& request_id) {
    if (!session.is_logged_in()) {
        fail(out) << "Not logged in." << std::endl;
        return;
    }
    const std::filesystem::path source(src_dir);
    std::error_code ec;
    if (!std::filesystem::is_directory(source, ec)) {
        fail(out) << "Not a directory: " << src_dir << std::endl;
        return;
    }

    const auto effective_id = request_id.empty() ?
                                  boost::uuids::to_string(boost::uuids::random_generator()()) :
                                  request_id;
    const auto bucket = std::string(ores::ore::net::ore_storage::bucket);
    const auto key = ores::ore::net::ore_storage::import_key(effective_id);

    BOOST_LOG_SEV(lg(), info) << "Uploading " << src_dir << " to " << bucket << "/" << key;

    try {
        ores::storage::net::storage_transfer transfer(default_http_base_url(),
                                                      session.bearer_token());
        transfer.pack_and_upload(source, bucket, key);
    } catch (const std::exception& e) {
        fail(out) << "Upload failed: " << e.what() << std::endl;
        return;
    }

    out << "✓ Uploaded " << src_dir << " to " << bucket << "/" << key << std::endl;
    out << "request_id: " << effective_id << std::endl;
}

void ore_commands::process_import(std::ostream& out,
                                  nats_client& session,
                                  const std::string& request_id,
                                  const std::string& party_id,
                                  const std::string& choices_file,
                                  const std::string& parent_portfolio_name,
                                  bool official,
                                  std::chrono::seconds timeout) {
    ores::ore::planner::import_choices choices;

    if (!choices_file.empty()) {
        std::ifstream in(choices_file);
        if (!in) {
            fail(out) << "Cannot read choices file: " << choices_file << std::endl;
            return;
        }
        const std::string json((std::istreambuf_iterator<char>(in)),
                               std::istreambuf_iterator<char>());
        auto parsed = rfl::json::read<ores::ore::planner::import_choices>(json);
        if (!parsed) {
            fail(out) << "Cannot parse choices file: " << parsed.error().what() << std::endl;
            return;
        }
        choices = std::move(*parsed);
    }

    // The flag wins over the file, so a caller can name the wrapping
    // portfolio without writing a choices file.
    if (!parent_portfolio_name.empty()) {
        choices.parent_portfolio_name = parent_portfolio_name;
        choices.create_parent_portfolio = true;
    }

    if (official)
        choices.official = true;

    const auto& party_ref = party_id.empty() ? session.auth().default_party_id : party_id;
    if (party_ref.empty()) {
        fail(out) << "No default party on this account and no --party-id given." << std::endl;
        return;
    }
    auto party_uuid = parse_uuid(out, party_ref, "party ID");
    if (!party_uuid)
        return;
    // The platform scopes every read and write to the session's party and
    // ignores a party a caller names. An import for another party would land
    // in the session's party, so refuse it here. The two are compared as
    // UUIDs, so the spelling of the text does not matter.
    if (!session.auth().default_party_id.empty()) {
        std::ostringstream ignored;
        const auto session_uuid =
            parse_uuid(ignored, session.auth().default_party_id, "session party");
        if (session_uuid && *session_uuid != *party_uuid) {
            fail(out) << "The import runs in the session's party ("
                      << session.auth().default_party_id << "), not " << party_ref
                      << ". Log in to that party first." << std::endl;
            return;
        }
    }
    choices.party_id = *party_uuid;

    ores::ore::messaging::ore_import_request req;
    req.request_id = request_id;
    req.correlation_id = request_id;
    req.import_choices_json = rfl::json::write(choices);

    BOOST_LOG_SEV(lg(), info) << "Starting ORE import for request: " << request_id;

    auto result = do_auth_request<ores::ore::messaging::ore_import_response>(
        out, session, req.nats_subject, req, import_request_timeout);
    if (!result)
        return;
    if (!result->success) {
        fail(out) << "Import failed: " << result->message << std::endl;
        return;
    }

    for (const auto& item_error : result->item_errors)
        fail(out) << item_error.source_file << " / " << item_error.item_id << ": "
                  << item_error.message << std::endl;

    if (result->workflow_instance_id.empty()) {
        out << "✓ Import completed." << std::endl;
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "ORE import workflow dispatched: " << result->workflow_instance_id;
    out << "workflow_instance_id: " << result->workflow_instance_id << std::endl;

    if (!workflow_run_commands::wait_for_instance(
            out, session, result->workflow_instance_id, timeout, 1))
        fail(out) << "ORE import did not complete: see the steps above." << std::endl;
}

void ore_commands::process_export(std::ostream& out,
                                  nats_client& session,
                                  const std::string& output_file,
                                  const std::string& node_id,
                                  std::uint32_t limit) {
    ores::trading::messaging::export_portfolio_request req;
    req.node_id = node_id;
    req.limit = static_cast<int>(limit);

    BOOST_LOG_SEV(lg(), info) << "Requesting portfolio export (node: "
                              << (node_id.empty() ? "all" : node_id) << ")";

    auto result = do_auth_request<ores::trading::messaging::export_portfolio_response>(
        out, session, req.nats_subject, req, export_request_timeout);
    if (!result)
        return;
    if (!result->success) {
        fail(out) << "Export failed: " << result->message << std::endl;
        return;
    }

    const auto xml = ores::ore::xml::exporter::export_portfolio(result->items, result->instruments);

    std::ofstream out_file(output_file, std::ios::binary | std::ios::trunc);
    if (!out_file) {
        fail(out) << "Cannot write: " << output_file << std::endl;
        return;
    }
    out_file << xml;
    out_file.close();
    if (!out_file) {
        fail(out) << "Failed to write: " << output_file << std::endl;
        return;
    }

    out << "✓ Exported " << result->items.size() << " trade(s) to " << output_file << " ("
        << xml.size() << " bytes)." << std::endl;
}

void ore_commands::process_import_run(std::ostream& out,
                                      nats_client& session,
                                      const std::string& report_definition_id,
                                      const std::string& src_dir,
                                      const std::string& name) {
    const std::filesystem::path source(src_dir);
    std::error_code ec;
    if (!std::filesystem::is_directory(source, ec)) {
        fail(out) << "Not a directory: " << src_dir << std::endl;
        return;
    }

    ores::ore::messaging::import_run_configuration_request req;
    req.report_definition_id = report_definition_id;
    auto directory = std::filesystem::absolute(source).lexically_normal();
    if (!directory.has_filename())
        directory = directory.parent_path();
    req.name = name.empty() ? directory.filename().string() : name;
    for (const auto& entry : std::filesystem::directory_iterator(source)) {
        if (!entry.is_regular_file())
            continue;
        std::ifstream in(entry.path(), std::ios::binary);
        req.files.push_back(
            {entry.path().filename().string(),
             std::string(std::istreambuf_iterator<char>(in), std::istreambuf_iterator<char>())});
    }

    // The files travel inline in one NATS message, whose server limit is 1 MB
    // by default, so a larger input is refused here with the reason rather
    // than failing inside NATS.
    std::size_t total = 0;
    for (const auto& f : req.files)
        total += f.content.size();
    if (total > max_inline_run_input) {
        fail(out) << "The input holds " << total << " bytes; a run's input travels inline and may "
                  << "hold at most " << max_inline_run_input << "." << std::endl;
        return;
    }

    auto result = do_auth_request<ores::ore::messaging::import_run_configuration_response>(
        out, session, req.nats_subject, req, import_request_timeout);
    if (!result)
        return;
    if (!result->success) {
        fail(out) << "Import failed: " << result->message << std::endl;
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Run configuration import dispatched: "
                              << result->workflow_instance_id;
    out << "workflow_instance_id: " << result->workflow_instance_id << std::endl;
    if (!workflow_run_commands::wait_for_instance(
            out, session, result->workflow_instance_id, default_import_timeout, 1))
        fail(out) << "Run configuration import did not complete: see the steps above." << std::endl;
}

void ore_commands::process_fetch_packages(std::ostream& out,
                                          nats_client& session,
                                          const std::string& report_instance_id,
                                          const std::string& out_dir) {
    if (!session.is_logged_in()) {
        fail(out) << "Not logged in." << std::endl;
        return;
    }
    if (!parse_uuid(out, report_instance_id, "report instance id"))
        return;

    const auto folders = fetch_packages_into(out, session, report_instance_id, out_dir);
    if (!folders)
        return;
    if (folders->empty()) {
        fail(out) << "Report instance " << report_instance_id
                  << " stored no package. Either it has not prepared its input, or it never "
                     "will: a failed instance stores none."
                  << std::endl;
        return;
    }
    out << "✓ Fetched " << folders->size() << " package(s) for report instance "
        << report_instance_id << ":" << std::endl;
    for (const auto& f : *folders)
        out << "    " << f.string() << std::endl;
}

void ore_commands::process_build_packages(std::ostream& out,
                                          nats_client& session,
                                          const std::string& report_definition_id,
                                          const std::string& out_dir,
                                          std::chrono::seconds timeout) {
    if (!session.is_logged_in()) {
        fail(out) << "Not logged in." << std::endl;
        return;
    }
    const auto definition = parse_uuid(out, report_definition_id, "report definition id");
    const auto tenant = parse_uuid(out, session.auth().tenant_id, "tenant id of the session");
    if (!definition || !tenant)
        return;

    ores::reporting::messaging::trigger_report_instance_request trigger;
    trigger.report_definition_id = *definition;
    trigger.tenant_id = *tenant;
    trigger.kind = std::string("package");

    const auto triggered =
        do_auth_request<ores::reporting::messaging::trigger_report_instance_response>(
            out, session, trigger.nats_subject, trigger, import_request_timeout);
    if (!triggered)
        return;
    if (triggered->result.outcome != ores::utility::domain::outcome::ok ||
        triggered->report_instance_id.empty()) {
        fail(out) << "The package build was refused: " << triggered->result.message << std::endl;
        return;
    }
    const auto instance_id = triggered->report_instance_id;
    out << "Building the packages with report instance " << instance_id << " ..." << std::endl;

    // The instance is terminal once it records when it completed, whether it completed
    // or failed.
    const auto deadline = std::chrono::steady_clock::now() + timeout;
    std::optional<ores::reporting::domain::report_instance> finished;
    while (std::chrono::steady_clock::now() < deadline) {
        ores::reporting::messaging::list_report_instances_request poll;
        poll.limit = 1;
        poll.filter = ores::reporting::messaging::report_instances_filter{
            .id_one_of =
                std::vector<boost::uuids::uuid>{boost::uuids::string_generator()(instance_id)}};
        const auto reply =
            do_auth_request<ores::reporting::messaging::list_report_instances_response>(
                out, session, poll.nats_subject, poll, import_request_timeout);
        if (!reply)
            return;
        if (!reply->instances.empty() && reply->instances.front().completed_at) {
            finished = reply->instances.front();
            break;
        }
        std::this_thread::sleep_for(package_poll_interval);
    }
    if (!finished) {
        fail(out) << "Report instance " << instance_id << " did not finish within "
                  << timeout.count() << " seconds. It may still be running; fetch its packages "
                  << "later with: ore fetch-packages " << instance_id << " " << out_dir
                  << std::endl;
        return;
    }

    const auto folders = fetch_packages_into(out, session, instance_id, out_dir);
    if (!folders)
        return;
    if (folders->empty()) {
        fail(out) << "The build stored no package. The instance says: " << finished->output_message
                  << std::endl;
        return;
    }
    out << "✓ Built " << folders->size() << " package(s) with report instance " << instance_id
        << ":" << std::endl;
    for (const auto& f : *folders)
        out << "    " << f.string() << std::endl;
}

void ore_commands::process_clean_packages(std::ostream& out,
                                          nats_client& session,
                                          std::chrono::seconds older_than,
                                          bool dry_run) {
    if (!session.is_logged_in()) {
        fail(out) << "Not logged in." << std::endl;
        return;
    }

    const auto instances = list_package_instances(out, session);
    if (!instances)
        return;

    // An instance is as old as its completion or its start, so one that is still running
    // is never removed however long ago it was asked for.
    const auto now = std::chrono::system_clock::now();
    std::vector<ores::reporting::domain::report_instance> expired;
    std::size_t undated = 0;
    for (const auto& instance : *instances) {
        const auto origin = package_age_origin(instance);
        if (!origin) {
            ++undated;
            continue;
        }
        if (now - *origin >= older_than)
            expired.push_back(instance);
    }

    const auto undated_note = [&out](std::size_t count) {
        if (count != 0)
            out << "  " << count
                << " instance(s) have neither a start nor a completion to age them by, and "
                   "were left alone."
                << std::endl;
    };

    if (expired.empty()) {
        out << "No package instance is older than " << older_than.count() << " seconds."
            << std::endl;
        undated_note(undated);
        return;
    }

    if (dry_run) {
        out << "Would remove " << expired.size() << " package instance(s):" << std::endl;
        for (const auto& instance : expired)
            out << "    " << instance.name << std::endl;
        undated_note(undated);
        return;
    }

    std::size_t removed = 0;
    for (const auto& instance : expired) {
        // The packages go first. Deleting the instance first would leave its packages
        // with nothing pointing at them, so a failure here keeps the instance for a
        // later run rather than losing track of them.
        if (!remove_stored_packages(out, session, boost::uuids::to_string(instance.id)))
            continue;
        if (!remove_instance(out, session, instance.name))
            continue;
        ++removed;
        out << "✓ Removed " << instance.name << " and the packages it stored" << std::endl;
    }
    out << "Removed " << removed << " of " << expired.size() << " package instance(s)."
        << std::endl;
    undated_note(undated);
}

void ore_commands::process_export_run(std::ostream& out,
                                      nats_client& session,
                                      const std::string& report_definition_id,
                                      const std::string& out_dir) {
    ores::ore::messaging::export_run_configuration_request req;
    req.report_definition_id = report_definition_id;

    auto result = do_auth_request<ores::ore::messaging::export_run_configuration_response>(
        out, session, req.nats_subject, req, export_request_timeout);
    if (!result)
        return;
    if (!result->success) {
        fail(out) << "Export failed: " << result->message << std::endl;
        return;
    }

    const std::filesystem::path target(out_dir);
    std::error_code ec;
    std::filesystem::create_directories(target, ec);
    for (const auto& f : result->files) {
        const auto relative = std::filesystem::path(f.name).lexically_normal();
        const bool climbs = std::ranges::any_of(relative, [](const auto& p) { return p == ".."; });
        if (relative.is_absolute() || relative.has_root_name() || climbs) {
            fail(out) << "Refusing to write outside " << out_dir << ": " << f.name << std::endl;
            return;
        }
        const auto path = target / relative;
        std::filesystem::create_directories(path.parent_path(), ec);
        std::ofstream file(path, std::ios::binary | std::ios::trunc);
        file << f.content;
        if (!file) {
            fail(out) << "Cannot write: " << path.string() << std::endl;
            return;
        }
    }
    out << "✓ Exported " << result->files.size() << " file(s) to " << out_dir << std::endl;
}

}
