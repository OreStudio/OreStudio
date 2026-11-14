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
#include "ores.http/routes/reporting/run_file_routes.hpp"
#include "ores.compute.api/messaging/run_files_protocol.hpp"
#include "ores.compute.api/net/compute_storage.hpp"
#include "ores.http.api/domain/http_request.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http.core/routes/storage_entry_source.hpp"
#include "ores.http.core/routes/storage_file_source.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.ore.core/results/output_manifest.hpp"
#include "ores.reporting.api/messaging/ore_file_category_protocol.hpp"
#include "ores.reporting.api/messaging/ore_file_protocol.hpp"
#include "ores.reporting.api/messaging/report_instance_protocol.hpp"
#include "ores.storage.api/messaging/objects_protocol.hpp"
#include "ores.storage.api/net/object_keys.hpp"
#include "ores.storage.api/net/storage_paths.hpp"
#include "ores.storage.core/filesystem/local_store.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include "ores.utility/compression/gzip.hpp"
#include <algorithm>
#include <fstream>
#include <iterator>
#include <map>
#include <optional>
#include <regex>
#include <sstream>
#include <string>
#include <string_view>
#include <vector>

namespace ores::http::routes::reporting {

namespace {

namespace json = ores::utility::codec::json;
namespace rr = ores::http::net::record_routes;
namespace forwarding = ores::http::forwarding;
namespace reporting_messaging = ores::reporting::messaging;
namespace compute_messaging = ores::compute::messaging;
namespace storage_messaging = ores::storage::messaging;
namespace ore_results = ores::ore::results;

using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::http::domain::http_status;
using ores::nats::service::nats_client;

auto& file_lg() {
    static auto instance = ores::logging::make_logger("ores.http.reporting.run_file_routes");
    return instance;
}

namespace files {

/// The most rows of the ORE catalogue one read takes; the catalogue is closed.
constexpr std::uint32_t catalogue_limit = 1000;

/// The name a file is offered as: the last segment of the path it is stored at.
std::string base_name(std::string_view path) {
    const auto slash = path.find_last_of('/');
    return std::string(slash == std::string_view::npos ? path : path.substr(slash + 1));
}

/// One ORE file the catalogue states.
struct catalogue_file final {
    std::string code;
    std::string name;
    std::string category_code;
    std::string category_name;
    std::string category_description;
    std::optional<std::string> configuration_type_code;
    std::optional<std::string> guide_section;
    std::string origin;
    std::string format;
    std::optional<std::string> schema_ore_file_code;
    std::string typical_filename;
    std::string description;
};

/// One ORE file beside the expression compiled from the pattern it states.
struct catalogue_pattern final {
    catalogue_file file;
    std::regex pattern;
};

/**
 * @brief The ORE files, each with the expression its names match.
 *
 * A catalogue that cannot be read leaves every file unidentified rather than
 * failing the page: the files are the answer and their identity is what is
 * said about them.
 */
std::vector<catalogue_pattern> read_catalogue(const http_request& req, nats_client& session) {
    std::vector<catalogue_pattern> patterns;
    try {
        std::map<std::string, std::pair<std::string, std::string>> category_of;

        reporting_messaging::list_ore_file_categories_request categories;
        categories.limit = catalogue_limit;
        const auto answered = forwarding::forward(req, session, categories);
        for (const auto& category : answered.categories)
            category_of.emplace(category.code, std::pair{category.name, category.description});

        reporting_messaging::list_ore_files_request msg;
        msg.limit = catalogue_limit;
        const auto reply = forwarding::forward(req, session, msg);
        for (const auto& file : reply.files) {
            const auto found = category_of.find(file.category_code);
            catalogue_file row{
                file.code,
                file.name,
                file.category_code,
                found == category_of.end() ? std::string() : found->second.first,
                found == category_of.end() ? std::string() : found->second.second,
                file.configuration_type_code,
                file.guide_section,
                file.origin,
                file.format,
                file.schema_ore_file_code,
                file.typical_filename,
                file.description};
            try {
                patterns.push_back(catalogue_pattern{std::move(row),
                                                     std::regex(file.filename_regex)});
            } catch (const std::regex_error& e) {
                // A row whose expression does not compile must not leave every
                // other file unidentified.
                BOOST_LOG_SEV(file_lg(), ores::logging::warn)
                    << "The ORE file " << file.code
                    << " states an expression that does not compile: " << e.what();
            }
        }
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(file_lg(), ores::logging::warn)
            << "The ORE file catalogue could not be read: " << e.what();
    }
    return patterns;
}

/// The ORE file a name is: the first whose expression matches the file's name.
const catalogue_file* ore_file_of(const std::string& name,
                                  const std::vector<catalogue_pattern>& catalogue) {
    const auto base = base_name(name);
    for (const auto& candidate : catalogue)
        if (std::regex_match(base, candidate.pattern))
            return &candidate.file;
    return nullptr;
}

/**
 * @brief Where one file's bytes are.
 *
 * A file inside a package is the object's key beside the entry's name; a file
 * that is its own object names no entry, and says whether the object holds it
 * gzipped.
 */
struct object_ref final {
    std::string bucket;
    std::string key;
    std::string entry;
    bool gzip = false;
};

/// One file of a run, as the screen reads it.
struct run_file final {
    std::string id;
    std::string direction;
    std::string kind;
    std::string name;
    std::string family;
    std::optional<std::int64_t> bytes;
    std::optional<std::string> sha256;
    std::optional<std::string> job;
    std::string node;
    const catalogue_file* ore_file = nullptr;
    object_ref source;
};

/// One place in the tree, before its count of the files under it is known.
struct tree_node final {
    std::string id;
    std::string parent;
    std::string kind;
    std::string ref;
};

/// The files of a run, with the tree they hang in.
struct run_files final {
    std::vector<tree_node> nodes;
    std::vector<run_file> files;
};

/// The bucket and key an object path names.
bool split_uri(const std::string& uri, std::string& bucket, std::string& key) {
    return ores::storage::net::storage_paths::split_object_path(uri, bucket, key);
}

/**
 * @brief The entries of each package the caller named, keyed by object key.
 *
 * One call answers every package of a run, so a run's archives cost one round
 * trip and not one per workunit.
 */
std::map<std::string, storage_messaging::archive_listing>
entries_of(const http_request& req, nats_client& session, const std::string& bucket,
           const std::vector<std::string>& keys) {
    std::map<std::string, storage_messaging::archive_listing> listings;
    if (keys.empty())
        return listings;
    storage_messaging::list_archive_entries_request msg;
    msg.bucket = bucket;
    msg.keys = keys;
    const auto reply = forwarding::forward(req, session, msg);
    for (const auto& listing : reply.archives)
        listings.emplace(listing.key, listing);
    return listings;
}

/**
 * @brief The files one accepted result wrote, as the manifest beside them says.
 *
 * The node writes the manifest last, so a manifest that exists names files that
 * exist. A result whose manifest cannot be read leaves its archive listed
 * without the files inside it, which is what the archive still is.
 */
std::optional<std::vector<ore_results::output_manifest_file>>
outputs_of(const ores::storage::filesystem::local_store& store, const std::string& result_id) {
    const std::string bucket(ores::storage::api::object_keys::ores_bucket);
    const auto key = ores::compute::net::compute_storage::output_manifest_key(result_id);
    const auto path = store.resolve(bucket, key);
    if (!path)
        return std::nullopt;
    try {
        std::ifstream in(*path, std::ios::binary);
        if (!in)
            return std::nullopt;
        const std::vector<char> packed((std::istreambuf_iterator<char>(in)),
                                       std::istreambuf_iterator<char>());
        const auto text = ores::utility::compression::gzip_decompress(packed);
        return ore_results::located_files(
            ore_results::manifest_from_json(std::string_view(text.data(), text.size())), key);
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(file_lg(), ores::logging::warn)
            << "The output manifest of result " << result_id << " could not be read: " << e.what();
        return std::nullopt;
    }
}

/// The run, when the caller's own token can read it, or nothing.
std::optional<ores::reporting::domain::report_instance>
run_of(const http_request& req, nats_client& session, const std::string& id) {
    reporting_messaging::list_report_instances_request msg;
    msg.limit = 1;
    msg.filter.emplace();
    msg.filter->id_one_of = std::vector{boost::uuids::string_generator()(id)};
    const auto reply = forwarding::forward(req, session, msg);
    rr::require_done(reply.result);
    if (reply.instances.empty())
        return std::nullopt;
    return reply.instances.front();
}

/**
 * @brief The files of one run, and the tree they hang in.
 *
 * The run is the reporting service's, the batch and its workunits are compute's,
 * and what a package holds is storage's, so this reads three answers and states
 * each link once. An answer that states nothing leaves its part of the tree out
 * rather than failing the page.
 */
run_files files_of_run(const http_request& req,
                       nats_client& session,
                       const ores::storage::filesystem::local_store& store,
                       const std::string& run_id) {
    run_files out;
    out.nodes.push_back(tree_node{"", "", "run", run_id});

    compute_messaging::list_run_files_request ask;
    ask.run_id = run_id;
    const auto tree = forwarding::forward(req, session, ask);
    if (tree.batch_id.empty())
        return out;

    const auto batch_node = "batch:" + tree.batch_id;
    out.nodes.push_back(tree_node{batch_node, "", "batch", tree.batch_id});

    // Every input package of the run arrives as one set, so its contents cost
    // one call whatever the run's length.
    std::map<std::string, std::vector<std::string>> keys_of_bucket;
    for (const auto& archive : tree.archives) {
        if (archive.direction != "input")
            continue;
        std::string bucket;
        std::string key;
        if (split_uri(archive.uri, bucket, key))
            keys_of_bucket[bucket].push_back(key);
    }
    std::map<std::string, storage_messaging::archive_listing> listings;
    for (const auto& [bucket, keys] : keys_of_bucket)
        for (auto& [key, listing] : entries_of(req, session, bucket, keys))
            listings.emplace(key, std::move(listing));

    const auto has_node = [&](const std::string& id) {
        return std::ranges::any_of(out.nodes, [&](const tree_node& n) { return n.id == id; });
    };
    const auto add_node = [&](const std::string& id,
                              const std::string& parent,
                              const std::string& kind,
                              const std::string& ref) {
        if (!has_node(id))
            out.nodes.push_back(tree_node{id, parent, kind, ref});
    };

    for (const auto& archive : tree.archives) {
        const auto job_node = "job:" + archive.workunit_id;
        add_node(job_node, batch_node, "job", archive.workunit_id);

        if (archive.direction == "input") {
            const auto node_id = "input:" + archive.workunit_id;
            add_node(node_id, job_node, "input", archive.workunit_id);

            std::string bucket;
            std::string key;
            if (!split_uri(archive.uri, bucket, key)) {
                out.files.push_back(run_file{node_id,
                                             "input",
                                             "archive",
                                             base_name(archive.uri),
                                             std::string(),
                                             std::nullopt,
                                             std::nullopt,
                                             archive.workunit_id,
                                             node_id,
                                             nullptr,
                                             {}});
                continue;
            }

            const auto listing = listings.find(key);
            out.files.push_back(
                run_file{node_id,
                         "input",
                         "archive",
                         base_name(archive.uri),
                         std::string(),
                         listing == listings.end() ?
                             std::nullopt :
                             std::optional<std::int64_t>(
                                 static_cast<std::int64_t>(listing->second.size_bytes)),
                         std::nullopt,
                         archive.workunit_id,
                         node_id,
                         nullptr,
                         object_ref{bucket, key, std::string(), false}});
            if (listing == listings.end())
                continue;
            for (const auto& entry : listing->second.entries)
                out.files.push_back(
                    run_file{node_id + ":" + entry.name,
                             "input",
                             "file",
                             entry.name,
                             std::string(),
                             static_cast<std::int64_t>(entry.size_bytes),
                             std::nullopt,
                             archive.workunit_id,
                             node_id,
                             nullptr,
                             object_ref{bucket, key, entry.name, false}});
            continue;
        }

        const auto node_id = "output:" + archive.result_id;
        add_node(node_id, job_node, "output", archive.result_id);

        std::string bucket;
        std::string key;
        split_uri(archive.uri, bucket, key);
        // The output package is named and not read: the bytes of what it holds
        // are each their own object, and the manifest states each one's size and
        // digest.
        out.files.push_back(run_file{node_id,
                                     "output",
                                     "archive",
                                     base_name(archive.uri),
                                     std::string(),
                                     std::nullopt,
                                     std::nullopt,
                                     archive.workunit_id,
                                     node_id,
                                     nullptr,
                                     object_ref{bucket, key, std::string(), false}});
        const auto stated = outputs_of(store, archive.result_id);
        if (!stated)
            continue;
        for (const auto& entry : *stated)
            out.files.push_back(run_file{
                node_id + ":" + entry.name,
                "output",
                "file",
                entry.name,
                entry.family,
                entry.bytes < 0 ? std::nullopt : std::optional<std::int64_t>(entry.bytes),
                entry.sha256.empty() ? std::nullopt : std::optional<std::string>(entry.sha256),
                archive.workunit_id,
                node_id,
                nullptr,
                object_ref{std::string(ores::storage::api::object_keys::ores_bucket),
                           entry.key,
                           std::string(),
                           true}});
    }
    return out;
}

/// A tree node with its count of the files under it, its descendants included.
struct counted_node final {
    tree_node node;
    std::size_t count = 0;
};

std::vector<counted_node> counted(const std::vector<tree_node>& nodes,
                                  const std::vector<run_file>& files) {
    std::map<std::string, std::string> parent_of;
    for (const auto& node : nodes)
        parent_of.emplace(node.id, node.parent);

    std::map<std::string, std::size_t> counts;
    for (const auto& file : files) {
        std::string at = file.node;
        for (;;) {
            ++counts[at];
            const auto found = parent_of.find(at);
            if (at.empty() || found == parent_of.end() || found->second.empty())
                break;
            at = found->second;
        }
    }

    std::vector<counted_node> out;
    out.reserve(nodes.size());
    for (const auto& node : nodes)
        out.push_back(counted_node{node, counts[node.id]});
    return out;
}

std::string optional_text(const std::optional<std::string>& value) {
    return value ? json::write(*value) : std::string("null");
}

std::string write_file(const run_file& file) {
    std::ostringstream body;
    body << "{\"id\":" << json::write(file.id) << ",\"direction\":" << json::write(file.direction)
         << ",\"kind\":" << json::write(file.kind) << ",\"name\":" << json::write(file.name)
         << ",\"family\":" << json::write(file.family) << ",\"bytes\":"
         << (file.bytes ? std::to_string(*file.bytes) : std::string("null"))
         << ",\"sha256\":" << optional_text(file.sha256) << ",\"job\":" << optional_text(file.job)
         << ",\"node\":" << json::write(file.node) << ",\"ore_file_code\":"
         << (file.ore_file ? json::write(file.ore_file->code) : std::string("null"))
         << ",\"ore_file_name\":"
         << (file.ore_file ? json::write(file.ore_file->name) : std::string("null"))
         << ",\"category_code\":"
         << (file.ore_file ? json::write(file.ore_file->category_code) : std::string("null"))
         << '}';
    return body.str();
}

std::string tree_json(const std::vector<counted_node>& nodes) {
    std::ostringstream body;
    body << '[';
    for (std::size_t i = 0; i < nodes.size(); ++i) {
        if (i != 0)
            body << ',';
        const auto& node = nodes[i].node;
        body << "{\"id\":" << json::write(node.id) << ",\"parent\":" << json::write(node.parent)
             << ",\"kind\":" << json::write(node.kind) << ",\"ref\":" << json::write(node.ref)
             << ",\"count\":" << nodes[i].count << '}';
    }
    body << ']';
    return body.str();
}

std::string files_json(const run_files& run) {
    std::vector<std::string> written;
    written.reserve(run.files.size());
    for (const auto& file : run.files)
        written.push_back(write_file(file));
    return rr::array_of(written);
}

/// The file a screen named, or nothing when the run holds no such file.
const run_file* file_of(const run_files& run, const std::string& id) {
    const auto found = std::ranges::find_if(run.files,
                                            [&](const run_file& f) { return f.id == id; });
    return found == run.files.end() ? nullptr : &*found;
}

/**
 * @brief The bytes of one file, as they are served.
 *
 * A file inside a package is streamed out of it, so the package is never read
 * whole. A file that is its own object is streamed as the object. The one case
 * that is held is an output file, which the node stores gzipped: it is small,
 * it arrives whole, and the reply is the file rather than the object.
 */
http_response body_of(const run_file& file,
                      const ores::storage::filesystem::local_store& store) {
    const auto path = store.resolve(file.source.bucket, file.source.key);
    if (!path)
        rr::refuse(http_status::not_found,
                   "not-found",
                   "The package that holds this file is no longer stored.");

    http_response resp;
    resp.status = http_status::ok;
    resp.content_type = file.kind == "archive" ? "application/gzip" : "application/octet-stream";

    if (!file.source.entry.empty()) {
        auto packed =
            std::make_shared<ores::http::server::routes::storage_entry_source>(*path,
                                                                               file.source.entry);
        if (!packed->found())
            rr::refuse(http_status::not_found,
                       "not-found",
                       "The package no longer holds this file.");
        resp.body_stream = std::move(packed);
        return resp;
    }
    if (!file.source.gzip) {
        resp.body_stream = std::make_shared<ores::http::server::routes::storage_file_source>(*path);
        return resp;
    }

    std::ifstream in(*path, std::ios::binary);
    if (!in)
        rr::refuse(http_status::not_found, "not-found", "The file is no longer stored.");
    const std::vector<char> packed{std::istreambuf_iterator<char>(in),
                                   std::istreambuf_iterator<char>()};
    const auto text = ores::utility::compression::gzip_decompress(packed);
    resp.body = std::string(text.begin(), text.end());
    return resp;
}

/// A name a browser can save the file as.
std::string disposition_for(std::string_view name) {
    std::string safe;
    safe.reserve(name.size());
    for (const auto c : name)
        safe += (c == '"' || c == '\\') ? '_' : c;
    return "attachment; filename=\"" + safe + "\"";
}

}

}

using namespace files;

void run_file_routes::register_routes(
    std::shared_ptr<ores::http::net::router> router,
    std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
    nats_client& session,
    std::string storage_dir) {
    using forwarding::guarded;
    const auto store = std::make_shared<ores::storage::filesystem::local_store>(storage_dir);

    const auto add = [&](ores::http::net::route_builder builder) {
        const auto built = builder.tags({"reporting"}).auth_required().build();
        router->add_route(built);
        registry->register_route(built);
    };

    add(router->get("/api/reporting/runs/{id}/files")
            .summary("The files of one report run, and the tree they hang in")
            .description("The run, its batch, each job of the batch, and each job's input "
                         "package and output. A package that cannot be read leaves its archive "
                         "listed without the files inside it.")
            .handler([&session, store](const http_request& req)
                         -> boost::asio::awaitable<http_response> {
                co_return guarded<compute_messaging::list_run_files_request>([&] {
                    const auto raw = req.get_path_param("id");
                    if (!rr::is_uuid(raw))
                        rr::invalid("The address names an id that is not a uuid.");
                    rr::refuse_unknown_params(req, {});
                    if (!run_of(req, session, raw))
                        rr::refuse(http_status::not_found,
                                   "not-found",
                                   "No report run has this id.");

                    auto run = files_of_run(req, session, *store, raw);
                    const auto catalogue = read_catalogue(req, session);
                    for (auto& file : run.files)
                        file.ore_file = ore_file_of(file.name, catalogue);

                    return http_response::json(
                        "{\"tree\":" + tree_json(counted(run.nodes, run.files)) +
                        ",\"files\":" + files_json(run) + "}");
                });
            }));

    add(router->get("/api/reporting/runs/{id}/files/{file}")
            .summary("The bytes of one file of a report run")
            .description("A file inside a package is copied out of it as it is asked for, so "
                         "reading one result never reads the package that holds it.")
            .handler([&session, store](const http_request& req)
                         -> boost::asio::awaitable<http_response> {
                co_return guarded<compute_messaging::list_run_files_request>([&] {
                    const auto raw = req.get_path_param("id");
                    if (!rr::is_uuid(raw))
                        rr::invalid("The address names an id that is not a uuid.");
                    rr::refuse_unknown_params(req, {});
                    if (!run_of(req, session, raw))
                        rr::refuse(http_status::not_found,
                                   "not-found",
                                   "No report run has this id.");

                    const auto run = files_of_run(req, session, *store, raw);
                    const auto* found = file_of(run, req.get_path_param("file"));
                    if (found == nullptr)
                        rr::refuse(http_status::not_found,
                                   "not-found",
                                   "This run has no such file.");

                    auto resp = body_of(*found, *store);
                    resp.headers["Content-Disposition"] = disposition_for(found->name);
                    resp.headers["Cache-Control"] = "private, no-store";
                    return resp;
                });
            }));
}

}
