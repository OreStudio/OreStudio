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
#include "ores.compute.api/domain/result_table_io.hpp"
#include "ores.compute.api/domain/workunit_table_io.hpp"
#include "ores.compute.api/messaging/app_protocol.hpp"
#include "ores.compute.api/messaging/app_version_protocol.hpp"
#include "ores.compute.api/messaging/batch_protocol.hpp"
#include "ores.compute.api/messaging/host_protocol.hpp"
#include "ores.compute.api/messaging/platform_protocol.hpp"
#include "ores.compute.api/messaging/result_protocol.hpp"
#include "ores.compute.api/messaging/telemetry_protocol.hpp"
#include "ores.compute.api/messaging/workunit_protocol.hpp"
#include "ores.compute.api/net/compute_storage.hpp"
#include "ores.compute.client/client/package_publisher.hpp"
#include "ores.dq.api/domain/change_reason_constants.hpp"
#include "ores.platform/environment/environment.hpp"
#include "ores.shell/app/command_args.hpp"
#include "ores.shell/app/command_feedback.hpp"
#include "ores.shell/app/request_helpers.hpp"
#include "ores.storage/net/storage_transfer.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <chrono>
#include <cli/cli.h>
#include <filesystem>
#include <ostream>
#include <thread>
#include <unordered_set>
#include <utility>

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

// The server's grid-stats function counts a host as online when
// last_rpc_time is within the last 5 minutes
// (ores_compute_grid_stats_fn_create.sql). Keep the shell's online
// window on the same value so the smoke assertion and the
// delete-host guard agree with the server's online_hosts count.
constexpr std::chrono::seconds online_window{300};

// Smoke-test job-count bounds, mirroring the spec's
// config.smoke_min_jobs / config.smoke_max_jobs.
constexpr std::uint32_t smoke_min_jobs = 10;
constexpr std::uint32_t smoke_max_jobs = 20;

// Result outcome codes, per ores.compute.api domain docs: 1=Success,
// 3=ClientError, 4=NoReply.
constexpr int outcome_success = 1;

bool is_online(const compute::domain::host& h) {
    return std::chrono::system_clock::now() - h.last_rpc_time <= online_window;
}

std::optional<compute::domain::batch> find_batch_by_external_ref(std::ostream& out,
                                                                 nats_client& session,
                                                                 const std::string& external_ref) {
    compute::messaging::list_batches_request req;
    req.limit = 1000;
    auto resp = do_request(out, session, req, std::chrono::seconds(30), true);
    if (!resp)
        return std::nullopt;
    for (const auto& b : resp->batches) {
        if (b.external_ref == external_ref)
            return b;
    }
    fail(out) << "Unknown batch external ref: " << external_ref << std::endl;
    return std::nullopt;
}

std::optional<std::vector<compute::domain::workunit>>
workunits_of_batch(std::ostream& out, nats_client& session, const boost::uuids::uuid& batch_id) {
    compute::messaging::list_workunits_request req;
    req.limit = 1000;
    auto resp = do_request(out, session, req, std::chrono::seconds(30), true);
    if (!resp)
        return std::nullopt;
    std::vector<compute::domain::workunit> result;
    for (const auto& w : resp->workunits) {
        if (w.batch_id == batch_id)
            result.push_back(w);
    }
    return result;
}

std::optional<std::vector<compute::domain::result>>
results_of_batch(std::ostream& out, nats_client& session, const boost::uuids::uuid& batch_id) {
    auto wus = workunits_of_batch(out, session, batch_id);
    if (!wus)
        return std::nullopt;
    std::unordered_set<boost::uuids::uuid> workunit_ids;
    for (const auto& w : *wus)
        workunit_ids.insert(w.id);
    compute::messaging::list_results_request req;
    req.limit = 1000;
    auto resp = do_request(out, session, req, std::chrono::seconds(30), true);
    if (!resp)
        return std::nullopt;
    std::vector<compute::domain::result> result;
    for (const auto& r : resp->results) {
        if (workunit_ids.contains(r.workunit_id))
            result.push_back(r);
    }
    return result;
}

// Stored storage URIs look like "<api-prefix>/<bucket>/<key>"; split
// them back into the bucket/key pair the transfer client takes. The
// prefix derives from storage_paths so this stays in sync with
// make_object_path.
std::optional<std::pair<std::string, std::string>> split_storage_uri(const std::string& uri) {
    const std::string prefix = std::string(ores::storage::net::storage_paths::prefix) + "/";
    if (uri.rfind(prefix, 0) != 0)
        return std::nullopt;
    const auto rest = uri.substr(prefix.size());
    const auto slash = rest.find('/');
    if (slash == std::string::npos || slash == 0)
        return std::nullopt;
    return std::make_pair(rest.substr(0, slash), rest.substr(slash + 1));
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
        [&session](std::ostream& out) { process_list_apps(std::ref(out), std::ref(session)); },
        "List compute apps");

    compute_menu->Insert(
        "list-app-versions",
        [&session](std::ostream& out) {
            process_list_app_versions(std::ref(out), std::ref(session));
        },
        "List compute app versions");

    compute_menu->Insert(
        "list-platforms",
        [&session](std::ostream& out) { process_list_platforms(std::ref(out), std::ref(session)); },
        "List compute platforms");

    compute_menu->Insert(
        "list-hosts",
        [&session](std::ostream& out) { process_list_hosts(std::ref(out), std::ref(session)); },
        "List compute hosts");

    compute_menu->Insert(
        "list-batches",
        [&session](std::ostream& out) { process_list_batches(std::ref(out), std::ref(session)); },
        "List compute batches");

    compute_menu->Insert("add-batch",
                         [&session](std::ostream& out, std::vector<std::string> args) {
                             process_add_batch(std::ref(out), std::ref(session), args);
                         },
                         "Create a compute batch",
                         {"<external_ref> <job_count> [--smoke]"});

    compute_menu->Insert("dispatch-batch",
                         [&session](std::ostream& out, std::vector<std::string> args) {
                             process_dispatch_batch(std::ref(out), std::ref(session), args);
                         },
                         "Dispatch a compute batch's jobs",
                         {"<external_ref> <job_count> <app_version_id> <input_tarball>"});

    compute_menu->Insert("list-workunits",
                         [&session](std::ostream& out, std::vector<std::string> args) {
                             process_list_workunits(std::ref(out), std::ref(session), args);
                         },
                         "List compute workunits",
                         {"[--batch <external_ref>]"});

    compute_menu->Insert("list-results",
                         [&session](std::ostream& out, std::vector<std::string> args) {
                             process_list_results(std::ref(out), std::ref(session), args);
                         },
                         "List compute results",
                         {"[--batch <external_ref>]"});

    compute_menu->Insert("grid-stats",
                         [&session](std::ostream& out, std::vector<std::string> args) {
                             process_grid_stats(std::ref(out), std::ref(session), args);
                         },
                         "Show compute grid telemetry",
                         {"[--watch <external_ref>] [--smoke] [--timeout <seconds>]"});

    compute_menu->Insert("delete-host",
                         [&session](std::ostream& out, std::vector<std::string> args) {
                             process_delete_host(std::ref(out), std::ref(session), args);
                         },
                         "Delete a compute host",
                         {"<host_id>"});

    compute_menu->Insert("download-input",
                         [&session](std::ostream& out, std::vector<std::string> args) {
                             process_download_input(std::ref(out), std::ref(session), args);
                         },
                         "Download a workunit's input bundle",
                         {"<workunit_id> <dest_dir>"});

    compute_menu->Insert("download-output",
                         [&session](std::ostream& out, std::vector<std::string> args) {
                             process_download_output(std::ref(out), std::ref(session), args);
                         },
                         "Download a result's output bundle",
                         {"<result_id> <dest_dir>"});

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

    // Provisional output: raw rows until the drift task generates the
    // platform table_io. The reconciliation task replaces this loop
    // with `out << resp->platforms`.
    for (const auto& p : resp->platforms) {
        out << boost::uuids::to_string(p.id) << ' ' << p.code << ' ' << p.display_name << ' '
            << p.os_family << ' ' << p.cpu_arch << std::endl;
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

void compute_commands::process_add_batch(std::ostream& out,
                                         nats_client& session,
                                         const std::vector<std::string>& args) {
    auto parsed =
        parse_args(args, {{.name = "smoke", .requires_value = false, .default_value = "false"}});
    if (!parsed) {
        fail(out) << parsed.error() << std::endl;
        return;
    }
    if (parsed->positionals.size() != 2) {
        fail(out) << "Usage: compute add-batch <external_ref> <job_count> [--smoke]" << std::endl;
        return;
    }
    if (!session.is_logged_in()) {
        fail(out) << "Not logged in." << std::endl;
        return;
    }

    const auto& external_ref = parsed->positionals[0];
    const auto job_count = parse_uint32(parsed->positionals[1]);
    if (!job_count || *job_count == 0) {
        fail(out) << "Job count must be a positive integer." << std::endl;
        return;
    }
    const bool smoke = parsed->flag_set("smoke");
    if (smoke && (*job_count < smoke_min_jobs || *job_count > smoke_max_jobs)) {
        fail(out) << "Smoke batches run " << smoke_min_jobs << " to " << smoke_max_jobs
                  << " jobs; got " << *job_count << "." << std::endl;
        return;
    }

    const auto username = session.auth().username;
    const auto reason_code = std::string(default_reason_code);
    const std::string commentary = "Created via compute add-batch";

    compute::domain::batch batch;
    batch.id = boost::uuids::random_generator()();
    batch.external_ref = external_ref;
    batch.status = "open";
    batch.modified_by = username;
    batch.performed_by = username;
    batch.change_reason_code = reason_code;
    batch.change_commentary = commentary;

    compute::messaging::save_batch_request req;
    req.batch = batch;
    req.change_reason_code = reason_code;
    req.change_commentary = commentary;

    auto resp = do_request(out, session, req, std::chrono::seconds(30), true);
    if (!resp || !resp->success) {
        fail(out) << "Failed to create batch: " << (resp ? resp->message : "no response")
                  << std::endl;
        return;
    }

    out << "Created batch " << boost::uuids::to_string(batch.id) << " (ref " << external_ref << ", "
        << *job_count << " job" << (*job_count == 1 ? "" : "s") << (smoke ? ", smoke" : "") << ")."
        << std::endl;
}

void compute_commands::process_dispatch_batch(std::ostream& out,
                                              nats_client& session,
                                              const std::vector<std::string>& args) {
    auto parsed = parse_args(args, {});
    if (!parsed) {
        fail(out) << parsed.error() << std::endl;
        return;
    }
    if (parsed->positionals.size() != 4) {
        fail(out) << "Usage: compute dispatch-batch <external_ref> <job_count> "
                     "<app_version_id> <input_tarball>"
                  << std::endl;
        return;
    }
    if (!session.is_logged_in()) {
        fail(out) << "Not logged in." << std::endl;
        return;
    }

    const auto& external_ref = parsed->positionals[0];
    const auto job_count = parse_uint32(parsed->positionals[1]);
    if (!job_count || *job_count == 0) {
        fail(out) << "Job count must be a positive integer." << std::endl;
        return;
    }
    const auto& app_version_id = parsed->positionals[2];
    const auto& tarball = parsed->positionals[3];

    const auto batch = find_batch_by_external_ref(out, session, external_ref);
    if (!batch)
        return;
    if (batch->status != "open") {
        fail(out) << "Batch " << external_ref << " is not open (status: " << batch->status
                  << "); only an open batch can be dispatched." << std::endl;
        return;
    }

    // Resolve the app version up front: saving a workunit for an unknown
    // app version still succeeds, but without published platform packages
    // the backend cannot dispatch it -- the assignment events are never
    // published and the jobs sit unsent forever.
    boost::uuids::uuid app_version_uuid;
    bool found_app_version = false;
    compute::messaging::list_app_versions_request av_req;
    av_req.limit = 1000;
    auto av_resp = do_request(out, session, av_req, std::chrono::seconds(30), true);
    if (!av_resp)
        return;
    for (const auto& v : av_resp->app_versions) {
        if (boost::uuids::to_string(v.id) == app_version_id) {
            app_version_uuid = v.id;
            found_app_version = true;
            break;
        }
    }
    if (!found_app_version) {
        fail(out) << "Unknown app version id: " << app_version_id << std::endl;
        return;
    }

    const auto batch_id_str = boost::uuids::to_string(batch->id);

    // The shared input bundle: uploaded once, referenced by every workunit
    // of the batch. The key is batch-scoped, so compute_storage::input_key
    // (workunit-scoped) does not apply; download-input reverses the same
    // convention. The tarball is uploaded verbatim, matching the Qt UI
    // convention of uploading the file as-is.
    const auto key = "input/" + batch_id_str + ".tar.gz";
    const auto input_uri = ores::storage::net::storage_paths::make_object_path(
        ores::compute::net::compute_storage::bucket, key);

    if (!std::filesystem::is_regular_file(tarball)) {
        fail(out) << "Input tarball not found: " << tarball << std::endl;
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Uploading input bundle " << key << " for batch " << external_ref;
    try {
        ores::storage::net::storage_transfer transfer(default_http_base_url());
        transfer.upload(std::string(ores::compute::net::compute_storage::bucket), key, tarball);
    } catch (const std::exception& e) {
        fail(out) << "Input bundle upload failed: " << e.what() << std::endl;
        return;
    }

    const auto username = session.auth().username;
    const auto reason_code = std::string(default_reason_code);
    const std::string commentary = "Dispatched via compute dispatch-batch";

    // One workunit per job; each save dispatches to the grid (the
    // backend creates the result rows and publishes the assignments).
    // The batch stays "open" until every workunit has been saved: if a
    // save fails partway, the batch can be dispatched again instead of
    // being stranded mid-flight with no way to finish it.
    for (std::uint32_t i = 0; i < *job_count; ++i) {
        compute::domain::workunit wu;
        wu.id = boost::uuids::random_generator()();
        wu.batch_id = batch->id;
        wu.app_version_id = app_version_uuid;
        wu.input_uri = input_uri;
        wu.priority = 1;
        wu.target_redundancy = 1;
        wu.canonical_result_id = boost::uuids::uuid{};
        wu.modified_by = username;
        wu.performed_by = username;
        wu.change_reason_code = reason_code;
        wu.change_commentary = commentary;

        compute::messaging::save_workunit_request wu_req;
        wu_req.workunit = wu;
        wu_req.change_reason_code = reason_code;
        wu_req.change_commentary = commentary;
        auto wu_resp = do_request(out, session, wu_req, std::chrono::seconds(30), true);
        if (!wu_resp || !wu_resp->success) {
            fail(out) << "Failed to dispatch job " << (i + 1) << "/" << *job_count << ": "
                      << (wu_resp ? wu_resp->message : "no response") << std::endl;
            return;
        }
        BOOST_LOG_SEV(lg(), info) << "Dispatched job " << (i + 1) << "/" << *job_count
                                  << " (workunit " << boost::uuids::to_string(wu.id) << ")";
    }

    compute::domain::batch dispatched = *batch;
    dispatched.status = "dispatched";
    dispatched.modified_by = username;
    dispatched.performed_by = username;
    dispatched.change_reason_code = reason_code;
    dispatched.change_commentary = commentary;

    compute::messaging::save_batch_request batch_req;
    batch_req.batch = dispatched;
    batch_req.change_reason_code = reason_code;
    batch_req.change_commentary = commentary;
    auto batch_resp = do_request(out, session, batch_req, std::chrono::seconds(30), true);
    if (!batch_resp || !batch_resp->success) {
        fail(out) << "Failed to mark batch dispatched: "
                  << (batch_resp ? batch_resp->message : "no response") << std::endl;
        return;
    }

    out << "Dispatched " << *job_count << " job(s) of batch " << external_ref << " to app version "
        << app_version_id << "." << std::endl;
}

void compute_commands::process_list_workunits(std::ostream& out,
                                              nats_client& session,
                                              const std::vector<std::string>& args) {
    auto parsed =
        parse_args(args, {{.name = "batch", .requires_value = true, .default_value = ""}});
    if (!parsed) {
        fail(out) << parsed.error() << std::endl;
        return;
    }
    if (!session.is_logged_in()) {
        fail(out) << "Not logged in." << std::endl;
        return;
    }

    compute::messaging::list_workunits_request req;
    req.limit = 1000;
    auto resp = do_request(out, session, req, std::chrono::seconds(30), true);
    if (!resp)
        return;

    const auto& batch_ref = parsed->flag("batch");
    std::vector<compute::domain::workunit> filtered;
    if (batch_ref.empty()) {
        filtered = resp->workunits;
    } else {
        const auto batch = find_batch_by_external_ref(out, session, batch_ref);
        if (!batch)
            return;
        for (const auto& w : resp->workunits) {
            if (w.batch_id == batch->id)
                filtered.push_back(w);
        }
    }

    BOOST_LOG_SEV(lg(), info) << "Successfully retrieved " << filtered.size() << " workunits.";
    out << filtered << std::endl;
}

void compute_commands::process_list_results(std::ostream& out,
                                            nats_client& session,
                                            const std::vector<std::string>& args) {
    auto parsed =
        parse_args(args, {{.name = "batch", .requires_value = true, .default_value = ""}});
    if (!parsed) {
        fail(out) << parsed.error() << std::endl;
        return;
    }
    if (!session.is_logged_in()) {
        fail(out) << "Not logged in." << std::endl;
        return;
    }

    const auto& batch_ref = parsed->flag("batch");
    if (!batch_ref.empty()) {
        const auto batch = find_batch_by_external_ref(out, session, batch_ref);
        if (!batch)
            return;
        auto results = results_of_batch(out, session, batch->id);
        if (!results)
            return;
        BOOST_LOG_SEV(lg(), info) << "Successfully retrieved " << results->size() << " results.";
        out << *results << std::endl;
        return;
    }

    compute::messaging::list_results_request req;
    req.limit = 1000;
    auto resp = do_request(out, session, req, std::chrono::seconds(30), true);
    if (!resp)
        return;

    BOOST_LOG_SEV(lg(), info) << "Successfully retrieved " << resp->results.size() << " results.";
    out << resp->results << std::endl;
}

void compute_commands::process_grid_stats(std::ostream& out,
                                          nats_client& session,
                                          const std::vector<std::string>& args) {
    auto parsed = parse_args(args,
                             {{.name = "watch", .requires_value = true, .default_value = ""},
                              {.name = "smoke", .requires_value = false, .default_value = "false"},
                              {.name = "timeout", .requires_value = true, .default_value = "300"}});
    if (!parsed) {
        fail(out) << parsed.error() << std::endl;
        return;
    }
    if (!session.is_logged_in()) {
        fail(out) << "Not logged in." << std::endl;
        return;
    }

    const auto& batch_ref = parsed->flag("watch");
    const bool smoke = parsed->flag_set("smoke");
    if (smoke && batch_ref.empty()) {
        fail(out) << "--smoke requires --watch <external_ref>." << std::endl;
        return;
    }
    const auto timeout = parse_positive_seconds(parsed->flag("timeout"));
    if (!timeout) {
        fail(out) << "Timeout must be a positive number of seconds." << std::endl;
        return;
    }

    std::optional<compute::domain::batch> batch;
    if (!batch_ref.empty()) {
        batch = find_batch_by_external_ref(out, session, batch_ref);
        if (!batch)
            return;
    }

    // Watch mode: poll every 10 seconds until every workunit of the
    // batch has a canonical result -- the batch has drained -- then
    // render the final snapshot. The smoke-test script relies on this
    // loop because the .ores script language has no flow control.
    std::uint32_t workunit_count = 0;
    std::uint32_t terminal_count = 0;
    // Hosts online when the batch drains; the smoke assertion must
    // judge that set, not whoever is online after the wait loop.
    std::vector<compute::domain::host> online_hosts_at_drain;
    if (batch) {
        const auto deadline = std::chrono::steady_clock::now() + *timeout;
        while (true) {
            auto wus = workunits_of_batch(out, session, batch->id);
            if (!wus)
                return;
            workunit_count = static_cast<std::uint32_t>(wus->size());
            terminal_count = static_cast<std::uint32_t>(
                std::count_if(wus->begin(), wus->end(), [](const auto& w) {
                    return w.canonical_result_id != boost::uuids::uuid{};
                }));
            if (workunit_count > 0 && terminal_count == workunit_count) {
                BOOST_LOG_SEV(lg(), info) << "Batch " << batch_ref << " drained (" << terminal_count
                                          << "/" << workunit_count << " workunits).";
                compute::messaging::list_hosts_request hosts_req;
                hosts_req.limit = 1000;
                auto hosts_resp =
                    do_request(out, session, hosts_req, std::chrono::seconds(30), true);
                if (!hosts_resp)
                    return;
                for (const auto& h : hosts_resp->hosts) {
                    if (is_online(h))
                        online_hosts_at_drain.push_back(h);
                }
                break;
            }
            if (workunit_count == 0) {
                fail(out) << "Batch " << batch_ref << " has no workunits; dispatch it first."
                          << std::endl;
                return;
            }
            if (std::chrono::steady_clock::now() >= deadline) {
                fail(out) << "Timed out after " << timeout->count() << "s waiting for batch "
                          << batch_ref << " to drain (" << terminal_count << "/" << workunit_count
                          << " workunits terminal)." << std::endl;
                return;
            }
            std::this_thread::sleep_for(std::chrono::seconds(10));
        }
    }

    compute::messaging::get_grid_stats_request req;
    auto resp = do_request(out, session, req, std::chrono::seconds(30), true);
    if (!resp)
        return;
    if (!resp->success) {
        const auto& msg = resp->message.empty() ? "Failed to fetch grid stats." : resp->message;
        BOOST_LOG_SEV(lg(), warn) << msg;
        fail(out) << msg << std::endl;
        return;
    }

    out << "Grid: " << resp->total_hosts << " hosts (" << resp->online_hosts << " online, "
        << resp->idle_hosts << " idle)" << std::endl;
    out << "Results: " << resp->results_inactive << " inactive, " << resp->results_unsent
        << " unsent, " << resp->results_in_progress << " in progress, " << resp->results_done
        << " done" << std::endl;
    out << "Workunits: " << resp->total_workunits << " total; batches: " << resp->total_batches
        << " total, " << resp->active_batches << " active" << std::endl;
    out << "Outcomes: " << resp->outcomes_success << " success, " << resp->outcomes_client_error
        << " client errors, " << resp->outcomes_no_reply << " no replies" << std::endl;
    out << "Sampled at: " << resp->sampled_at << std::endl;

    // Provisional per-node rows until the drift task generates the
    // node-sample table_io; the reconciliation task swaps this loop
    // for the generated table.
    out << "Nodes:" << std::endl;
    for (const auto& n : resp->node_summaries) {
        out << n.host_id << ' ' << n.tasks_completed << ' ' << n.tasks_since_last << ' '
            << n.avg_task_duration_ms << ' ' << n.input_bytes_fetched << ' '
            << n.output_bytes_uploaded << ' ' << n.seconds_since_hb << std::endl;
    }
    out << std::endl;

    if (batch && smoke) {
        // The drained batch is the smoke verdict: every result has
        // outcome Success, and every host online at the drain
        // transition has at least one result in the batch. A failed
        // check marks the command failed, so the script runner aborts
        // the smoke-test script.
        auto results = results_of_batch(out, session, batch->id);
        if (!results)
            return;

        std::uint32_t success_count = 0;
        for (const auto& r : *results) {
            if (r.outcome == outcome_success)
                ++success_count;
        }
        const bool all_success = !results->empty() && success_count == results->size();

        // The host set captured when the batch drained (see the watch
        // loop above); a fresh fetch here could judge a different set.
        const auto& online_hosts = online_hosts_at_drain;
        std::vector<compute::domain::host> unexercised;
        for (const auto& h : online_hosts) {
            const bool exercised = std::any_of(
                results->begin(), results->end(), [&](const auto& r) { return r.host_id == h.id; });
            if (!exercised)
                unexercised.push_back(h);
        }

        out << "Smoke check for batch " << batch_ref << ":" << std::endl;
        out << "  results: " << success_count << "/" << results->size() << " success" << std::endl;
        out << "  nodes exercised: " << (online_hosts.size() - unexercised.size()) << "/"
            << online_hosts.size() << " online hosts" << std::endl;
        if (all_success && unexercised.empty()) {
            out << "SMOKE PASS" << std::endl;
        } else {
            if (!all_success)
                out << "  FAIL: " << (results->size() - success_count) << " result(s) not success"
                    << std::endl;
            for (const auto& h : unexercised) {
                out << "  FAIL: online host " << h.display_name << " (" << h.external_id
                    << ") has no result in this batch" << std::endl;
            }
            out << "SMOKE FAIL" << std::endl;
            command_feedback::mark_failure();
        }
    }
}

void compute_commands::process_delete_host(std::ostream& out,
                                           nats_client& session,
                                           const std::vector<std::string>& args) {
    auto parsed = parse_args(args, {});
    if (!parsed) {
        fail(out) << parsed.error() << std::endl;
        return;
    }
    if (parsed->positionals.size() != 1) {
        fail(out) << "Usage: compute delete-host <host_id>" << std::endl;
        return;
    }
    if (!session.is_logged_in()) {
        fail(out) << "Not logged in." << std::endl;
        return;
    }

    const auto& host_id = parsed->positionals[0];

    compute::messaging::list_hosts_request req;
    req.limit = 1000;
    auto resp = do_request(out, session, req, std::chrono::seconds(30), true);
    if (!resp)
        return;

    const compute::domain::host* target = nullptr;
    for (const auto& h : resp->hosts) {
        if (boost::uuids::to_string(h.id) == host_id) {
            target = &h;
            break;
        }
    }
    if (!target) {
        fail(out) << "Unknown host id: " << host_id << std::endl;
        return;
    }
    if (is_online(*target)) {
        fail(out) << "Host " << target->display_name << " (" << host_id
                  << ") is online; stop the wrapper before deleting it." << std::endl;
        return;
    }

    const auto username = session.auth().username;
    const auto reason_code = std::string(default_reason_code);
    const std::string commentary = "Deleted via compute delete-host";

    compute::messaging::delete_host_request del_req;
    del_req.id = host_id;
    del_req.change_reason_code = reason_code;
    del_req.change_commentary = commentary;
    auto del_resp = do_request(out, session, del_req, std::chrono::seconds(30), true);
    if (!del_resp || !del_resp->success) {
        fail(out) << "Failed to delete host: " << (del_resp ? del_resp->message : "no response")
                  << std::endl;
        return;
    }
    out << "Deleted host " << target->display_name << " (" << host_id << ")." << std::endl;
}

void compute_commands::process_download_input(std::ostream& out,
                                              nats_client& session,
                                              const std::vector<std::string>& args) {
    auto parsed = parse_args(args, {});
    if (!parsed) {
        fail(out) << parsed.error() << std::endl;
        return;
    }
    if (parsed->positionals.size() != 2) {
        fail(out) << "Usage: compute download-input <workunit_id> <dest_dir>" << std::endl;
        return;
    }
    if (!session.is_logged_in()) {
        fail(out) << "Not logged in." << std::endl;
        return;
    }

    const auto& workunit_id = parsed->positionals[0];
    const auto& dest_dir = parsed->positionals[1];

    compute::messaging::list_workunits_request req;
    req.limit = 1000;
    auto resp = do_request(out, session, req, std::chrono::seconds(30), true);
    if (!resp)
        return;

    const compute::domain::workunit* wu = nullptr;
    for (const auto& w : resp->workunits) {
        if (boost::uuids::to_string(w.id) == workunit_id) {
            wu = &w;
            break;
        }
    }
    if (!wu) {
        fail(out) << "Unknown workunit id: " << workunit_id << std::endl;
        return;
    }
    if (wu->input_uri.empty()) {
        fail(out) << "Workunit " << workunit_id << " has no input bundle." << std::endl;
        return;
    }
    const auto storage = split_storage_uri(wu->input_uri);
    if (!storage) {
        fail(out) << "Workunit " << workunit_id
                  << " has an unrecognised input uri: " << wu->input_uri << std::endl;
        return;
    }

    try {
        ores::storage::net::storage_transfer transfer(default_http_base_url());
        transfer.fetch_and_unpack(storage->first, storage->second, dest_dir);
    } catch (const std::exception& e) {
        fail(out) << "Download failed: " << e.what() << std::endl;
        return;
    }
    out << "Downloaded input bundle of workunit " << workunit_id << " to " << dest_dir << "."
        << std::endl;
}

void compute_commands::process_download_output(std::ostream& out,
                                               nats_client& session,
                                               const std::vector<std::string>& args) {
    auto parsed = parse_args(args, {});
    if (!parsed) {
        fail(out) << parsed.error() << std::endl;
        return;
    }
    if (parsed->positionals.size() != 2) {
        fail(out) << "Usage: compute download-output <result_id> <dest_dir>" << std::endl;
        return;
    }
    if (!session.is_logged_in()) {
        fail(out) << "Not logged in." << std::endl;
        return;
    }

    const auto& result_id = parsed->positionals[0];
    const auto& dest_dir = parsed->positionals[1];

    compute::messaging::list_results_request req;
    req.limit = 1000;
    auto resp = do_request(out, session, req, std::chrono::seconds(30), true);
    if (!resp)
        return;

    const compute::domain::result* target = nullptr;
    for (const auto& r : resp->results) {
        if (boost::uuids::to_string(r.id) == result_id) {
            target = &r;
            break;
        }
    }
    if (!target) {
        fail(out) << "Unknown result id: " << result_id << std::endl;
        return;
    }
    if (target->output_uri.empty()) {
        fail(out) << "Result " << result_id << " has no output bundle yet." << std::endl;
        return;
    }
    const auto storage = split_storage_uri(target->output_uri);
    if (!storage) {
        fail(out) << "Result " << result_id
                  << " has an unrecognised output uri: " << target->output_uri << std::endl;
        return;
    }

    try {
        ores::storage::net::storage_transfer transfer(default_http_base_url());
        transfer.fetch_and_unpack(storage->first, storage->second, dest_dir);
    } catch (const std::exception& e) {
        fail(out) << "Download failed: " << e.what() << std::endl;
        return;
    }
    out << "Downloaded output bundle of result " << result_id << " to " << dest_dir << "."
        << std::endl;
}

}
