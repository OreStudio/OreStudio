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
#include "ores.shell/app/commands/compute_commands.hpp"
#include "ores.compute.api/domain/app.hpp"
#include "ores.compute.api/domain/app_table_io.hpp"
#include "ores.compute.api/domain/app_version.hpp"
#include "ores.compute.api/domain/app_version_platform.hpp"
#include "ores.compute.api/domain/app_version_table_io.hpp"
#include "ores.compute.api/domain/batch_table_io.hpp"
#include "ores.compute.api/domain/host_table_io.hpp"
#include "ores.compute.api/messaging/app_protocol.hpp"
#include "ores.compute.api/messaging/app_version_protocol.hpp"
#include "ores.compute.api/messaging/batch_protocol.hpp"
#include "ores.compute.api/messaging/host_protocol.hpp"
#include "ores.compute.api/messaging/platform_protocol.hpp"
#include "ores.compute.client/client/package_publisher.hpp"
#include "ores.dq.api/domain/change_reason_constants.hpp"
#include "ores.platform/environment/environment.hpp"
#include "ores.shell/app/command_args.hpp"
#include "ores.shell/app/command_feedback.hpp"
#include "ores.shell/app/request_helpers.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <cli/cli.h>
#include <iomanip>
#include <ostream>

namespace ores::shell::app::commands {

using namespace logging;
using ores::nats::service::nats_client;
using ores::platform::environment::environment;

namespace {

constexpr std::string_view default_reason_code =
    dq::domain::change_reason_constants::codes::new_record;

std::string default_http_base_url() {
    return "http://localhost:" + environment::get_value_or_default("ORES_HTTP_PORT", "20600");
}

std::optional<compute::domain::app>
find_app_by_name(std::ostream& out, nats_client& session, const std::string& name) {
    compute::messaging::list_apps_request req;
    req.limit = 1000;
    auto resp = do_request(out, session, req, std::chrono::seconds(30), true);
    if (!resp)
        return std::nullopt;
    for (const auto& app : resp->apps) {
        if (app.name == name)
            return app;
    }
    return std::nullopt;
}

std::optional<compute::domain::app_version> find_app_version(std::ostream& out,
                                                             nats_client& session,
                                                             const boost::uuids::uuid& app_id,
                                                             const std::string& engine_version,
                                                             const std::string& wrapper_version) {
    compute::messaging::list_app_versions_request req;
    req.limit = 1000;
    auto resp = do_request(out, session, req, std::chrono::seconds(30), true);
    if (!resp)
        return std::nullopt;
    for (const auto& v : resp->app_versions) {
        if (v.app_id == app_id && v.engine_version == engine_version &&
            v.wrapper_version == wrapper_version)
            return v;
    }
    return std::nullopt;
}

// std::nullopt on fetch failure, distinct from an empty vector (genuinely
// no platforms published yet) -- callers must abort on nullopt rather than
// treat it as "start fresh", or a transient RPC failure here silently
// wipes every previously published platform on the next save.
std::optional<std::vector<compute::domain::app_version_platform>> existing_platforms(
    std::ostream& out, nats_client& session, const boost::uuids::uuid& app_version_id) {
    compute::messaging::list_app_version_platforms_request req;
    req.app_version_id = boost::uuids::to_string(app_version_id);
    auto resp = do_request(out, session, req, std::chrono::seconds(30), true);
    if (!resp || !resp->success)
        return std::nullopt;
    return resp->platforms;
}

std::optional<boost::uuids::uuid>
resolve_platform_id(std::ostream& out, nats_client& session, const std::string& platform_code) {
    compute::messaging::list_platforms_request req;
    auto resp = do_request(out, session, req, std::chrono::seconds(30), true);
    if (!resp || !resp->success)
        return std::nullopt;
    for (const auto& p : resp->platforms) {
        if (p.code == platform_code)
            return p.id;
    }
    return std::nullopt;
}

}

void compute_commands::register_commands(cli::Menu& root_menu, nats_client& session) {
    auto compute_menu = std::make_unique<cli::Menu>("compute");

    compute_menu->Insert("publish-package",
                         [&session](std::ostream& out, std::vector<std::string> args) {
                             process_publish_package(std::ref(out), std::ref(session), args);
                         },
                         "Publish a compute engine package (app/version/platform/binary)",
                         {"app_name engine_version platform_code [--file <path>] "
                          "[--wrapper-version <v>] [--min-ram-mb <n>] [--http-base-url <url>]"});

    compute_menu->Insert(
        "list-apps",
        [&session](std::ostream& out) {
            process_list_apps(std::ref(out), std::ref(session));
        },
        "List compute apps");

    compute_menu->Insert(
        "list-app-versions",
        [&session](std::ostream& out) {
            process_list_app_versions(std::ref(out), std::ref(session));
        },
        "List compute app versions");

    compute_menu->Insert(
        "list-platforms",
        [&session](std::ostream& out) {
            process_list_platforms(std::ref(out), std::ref(session));
        },
        "List compute platforms");

    compute_menu->Insert(
        "list-hosts",
        [&session](std::ostream& out) {
            process_list_hosts(std::ref(out), std::ref(session));
        },
        "List compute hosts");

    compute_menu->Insert(
        "list-batches",
        [&session](std::ostream& out) {
            process_list_batches(std::ref(out), std::ref(session));
        },
        "List compute batches");

    root_menu.Insert(std::move(compute_menu));
}

void compute_commands::process_publish_package(std::ostream& out,
                                               nats_client& session,
                                               const std::vector<std::string>& args) {
    auto parsed =
        parse_args(args,
                   {{.name = "file", .requires_value = true, .default_value = ""},
                    {.name = "wrapper-version", .requires_value = true, .default_value = "1.0.0"},
                    {.name = "min-ram-mb", .requires_value = true, .default_value = "0"},
                    {.name = "http-base-url",
                     .requires_value = true,
                     .default_value = default_http_base_url()}});
    if (!parsed) {
        fail(out) << parsed.error() << std::endl;
        return;
    }
    if (parsed->positionals.size() != 3) {
        fail(out) << "Usage: compute publish-package <app_name> <engine_version> "
                     "<platform_code> [--file <path>] [--wrapper-version <v>] "
                     "[--min-ram-mb <n>] [--http-base-url <url>]"
                  << std::endl;
        return;
    }
    if (!session.is_logged_in()) {
        fail(out) << "Not logged in." << std::endl;
        return;
    }

    const auto& app_name = parsed->positionals[0];
    const auto& engine_version = parsed->positionals[1];
    const auto& platform_code = parsed->positionals[2];
    const auto wrapper_version = parsed->flag("wrapper-version");
    const auto http_base_url = parsed->flag("http-base-url");

    std::uint32_t min_ram_mb = 0;
    if (const auto n = parse_uint32(parsed->flag("min-ram-mb")))
        min_ram_mb = *n;

    auto file = parsed->flag("file");
    if (file.empty())
        file = "publish/vendor-packages/" + app_name + "-" + engine_version + "-" + platform_code +
               ".tar.gz";

    if (!std::filesystem::exists(file)) {
        fail(out) << "Package file not found: " << file << std::endl;
        return;
    }

    const auto platform_id = resolve_platform_id(out, session, platform_code);
    if (!platform_id) {
        fail(out) << "Unknown platform code: " << platform_code << std::endl;
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Uploading " << file << " for " << app_name << " "
                              << engine_version << " (" << platform_code << ")";

    compute::client::package_publish_result upload;
    try {
        compute::client::package_publisher publisher(http_base_url);
        upload = publisher.publish(app_name, engine_version, platform_code, file);
    } catch (const std::exception& e) {
        fail(out) << "Upload failed: " << e.what() << std::endl;
        return;
    }

    out << "Uploaded: " << upload.package_uri << " (sha256=" << upload.sha256 << ")" << std::endl;

    const auto username = session.auth().username;
    const auto reason_code = std::string(default_reason_code);
    const std::string commentary = "Published via compute publish-package";

    auto existing_app = find_app_by_name(out, session, app_name);

    compute::domain::app app;
    app.id = existing_app ? existing_app->id : boost::uuids::random_generator()();
    app.name = app_name;
    app.description = existing_app ? existing_app->description : app_name;
    app.modified_by = username;
    app.performed_by = username;
    app.change_reason_code = reason_code;
    app.change_commentary = commentary;

    compute::messaging::save_app_request app_req;
    app_req.app = app;
    app_req.change_reason_code = reason_code;
    app_req.change_commentary = commentary;

    auto app_resp = do_request(out, session, app_req, std::chrono::seconds(30), true);
    if (!app_resp || !app_resp->success) {
        fail(out) << "Failed to save app: " << (app_resp ? app_resp->message : "no response")
                  << std::endl;
        return;
    }

    auto existing_version = find_app_version(out, session, app.id, engine_version, wrapper_version);

    compute::domain::app_version ver;
    ver.id = existing_version ? existing_version->id : boost::uuids::random_generator()();
    ver.app_id = app.id;
    ver.wrapper_version = wrapper_version;
    ver.engine_version = engine_version;
    ver.min_ram_mb = min_ram_mb;
    ver.modified_by = username;
    ver.performed_by = username;
    ver.change_reason_code = reason_code;
    ver.change_commentary = commentary;

    compute::domain::app_version_platform row;
    row.app_version_id = ver.id;
    row.platform_id = *platform_id;
    row.platform_code = platform_code;
    row.package_uri = upload.package_uri;
    row.sha256 = upload.sha256;

    // save_app_version_request replaces every platform row for the version
    // wholesale, so preserve platforms already published under this version
    // (e.g. from an earlier publish-package run for a different triplet)
    // rather than overwriting them with this single upload. Abort rather
    // than proceed if the version exists but the fetch itself failed --
    // treating a fetch failure as "no platforms yet" would silently wipe
    // every previously published platform on save.
    std::vector<compute::domain::app_version_platform> platform_rows;
    if (existing_version) {
        auto fetched = existing_platforms(out, session, ver.id);
        if (!fetched) {
            fail(out) << "Failed to fetch existing platforms for " << app_name << " "
                      << engine_version << "; refusing to publish " << platform_code
                      << " without them (would wipe other platforms on save)." << std::endl;
            return;
        }
        platform_rows = std::move(*fetched);
    }
    std::erase_if(platform_rows, [&](const auto& p) { return p.platform_code == platform_code; });
    platform_rows.push_back(row);

    compute::messaging::save_app_version_request ver_req;
    ver_req.app_version = ver;
    ver_req.platforms = std::move(platform_rows);
    ver_req.change_reason_code = reason_code;
    ver_req.change_commentary = commentary;

    auto ver_resp = do_request(out, session, ver_req, std::chrono::seconds(30), true);
    if (!ver_resp || !ver_resp->success) {
        fail(out) << "Failed to save app version: "
                  << (ver_resp ? ver_resp->message : "no response") << std::endl;
        return;
    }

    out << "Published " << app_name << " " << engine_version << " (" << platform_code
        << ") successfully." << std::endl;
}

void compute_commands::process_list_apps(std::ostream& out, nats_client& session) {
    if (!session.is_logged_in()) {
        fail(out) << "Not logged in." << std::endl;
        return;
    }

    compute::messaging::list_apps_request req;
    req.limit = 1000;
    auto resp = do_request(out, session, req, std::chrono::seconds(30), true);
    if (!resp)
        return;

    BOOST_LOG_SEV(lg(), info) << "Successfully retrieved " << resp->apps.size() << " apps.";
    out << resp->apps << std::endl;
}

void compute_commands::process_list_app_versions(std::ostream& out, nats_client& session) {
    if (!session.is_logged_in()) {
        fail(out) << "Not logged in." << std::endl;
        return;
    }

    compute::messaging::list_app_versions_request req;
    req.limit = 1000;
    auto resp = do_request(out, session, req, std::chrono::seconds(30), true);
    if (!resp)
        return;

    BOOST_LOG_SEV(lg(), info) << "Successfully retrieved " << resp->app_versions.size()
                              << " app versions.";
    out << resp->app_versions << std::endl;
}

void compute_commands::process_list_platforms(std::ostream& out, nats_client& session) {
    if (!session.is_logged_in()) {
        fail(out) << "Not logged in." << std::endl;
        return;
    }

    compute::messaging::list_platforms_request req;
    auto resp = do_request(out, session, req, std::chrono::seconds(30), true);
    if (!resp)
        return;
    if (!resp->success) {
        const auto& msg = resp->message.empty() ? "Failed to list platforms." : resp->message;
        BOOST_LOG_SEV(lg(), warn) << msg;
        fail(out) << msg << std::endl;
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Successfully retrieved " << resp->platforms.size()
                              << " platforms.";

    out << std::endl
        << std::left << std::setw(38) << "ID" << std::setw(14) << "Code" << std::setw(36)
        << "Display Name" << std::setw(10) << "OS" << std::setw(10) << "CPU Arch" << std::endl;
    for (const auto& p : resp->platforms) {
        out << std::left << std::setw(38) << boost::uuids::to_string(p.id) << std::setw(14)
            << p.code << std::setw(36) << p.display_name << std::setw(10) << p.os_family
            << std::setw(10) << p.cpu_arch << std::endl;
    }
    out << std::endl;
}

void compute_commands::process_list_hosts(std::ostream& out, nats_client& session) {
    if (!session.is_logged_in()) {
        fail(out) << "Not logged in." << std::endl;
        return;
    }

    compute::messaging::list_hosts_request req;
    req.limit = 1000;
    auto resp = do_request(out, session, req, std::chrono::seconds(30), true);
    if (!resp)
        return;

    BOOST_LOG_SEV(lg(), info) << "Successfully retrieved " << resp->hosts.size() << " hosts.";
    out << resp->hosts << std::endl;
}

void compute_commands::process_list_batches(std::ostream& out, nats_client& session) {
    if (!session.is_logged_in()) {
        fail(out) << "Not logged in." << std::endl;
        return;
    }

    compute::messaging::list_batches_request req;
    req.limit = 1000;
    auto resp = do_request(out, session, req, std::chrono::seconds(30), true);
    if (!resp)
        return;

    BOOST_LOG_SEV(lg(), info) << "Successfully retrieved " << resp->batches.size() << " batches.";
    out << resp->batches << std::endl;
}

}
