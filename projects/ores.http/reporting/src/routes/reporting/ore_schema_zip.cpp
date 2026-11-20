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
#include "ores.http/routes/reporting/ore_schema_zip.hpp"
#include "ores.http.api/domain/http_request.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/domain/http_status.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.logging/logger.hpp"
#include "ores.reporting.api/messaging/ore_file_protocol.hpp"
#include "ores.reporting.api/messaging/ore_file_version_protocol.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <archive.h>
#include <archive_entry.h>
#include <deque>
#include <filesystem>
#include <fstream>
#include <set>
#include <stdexcept>
#include <string>

namespace ores::http::routes::reporting {

namespace {

namespace forwarding = ores::http::forwarding;
namespace rr = ores::http::net::record_routes;
namespace reporting_messaging = ores::reporting::messaging;

using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::http::domain::http_status;
using ores::nats::service::nats_client;

auto& schema_zip_lg() {
    static auto instance = ores::logging::make_logger("ores.http.reporting.ore_schema_zip");
    return instance;
}

/// The sentence a request of this unit states when it names no schema.
constexpr std::string_view no_schema_named = "The address names no schema and engine version.";

/// The markup an include states.
constexpr std::string_view include_markup = "<xs:include";
constexpr std::string_view location_markup = "schemaLocation=\"";

/// The files one include of this content names, in the order the markup states them.
std::vector<std::string> includes_of(const std::string& content) {
    const std::string_view text(content);
    std::vector<std::string> names;
    std::size_t at = text.find(include_markup);
    while (at != std::string::npos) {
        auto cursor = at + include_markup.size();
        const auto whitespace = [&content](std::size_t index) {
            return index < content.size() && (content[index] == ' ' || content[index] == '\t' ||
                                              content[index] == '\r' || content[index] == '\n');
        };
        // The markup names its location first, which is the rule the BFF's own
        // scanner states; a document that orders its attributes otherwise is
        // not a document ORE ships.
        if (!whitespace(cursor)) {
            at = text.find(include_markup, cursor);
            continue;
        }
        while (whitespace(cursor))
            ++cursor;
        if (text.compare(cursor, location_markup.size(), location_markup) == 0) {
            cursor += location_markup.size();
            const auto end = text.find('"', cursor);
            if (end != std::string::npos)
                names.emplace_back(text.substr(cursor, end - cursor));
        }
        at = text.find(include_markup, cursor);
    }
    return names;
}

/**
 * @brief A name a browser can save the archive as, as the BFF stated it.
 *
 * The name is built here from the file's code and the version asked for, so it
 * is printable ASCII: the encoded form and the plain form are the same text,
 * and the two are stated because the rule states both.
 */
std::string disposition_of(const std::string& name) {
    const auto safe = [&name] {
        std::string out;
        out.reserve(name.size());
        for (const char raw : name) {
            const auto c = static_cast<unsigned char>(raw);
            out +=
                (c < 0x20 || c > 0x7E || c == '"' || c == '\\' || c == '%' || c == ';') ? '_' : raw;
        }
        return out;
    }();
    return "attachment; filename=\"" + safe + "\"; filename*=UTF-8''" + safe;
}

/// The contents of one engine version, by the file name the catalogue gives each file.
std::map<std::string, std::string> contents_of(const http_request& req,
                                               nats_client& session,
                                               const std::string& version,
                                               const std::map<std::string, std::string>& name_of) {
    reporting_messaging::list_ore_file_versions_request ask;
    ask.filter.emplace();
    ask.filter->engine_version = version;
    std::map<std::string, std::string> contents;
    for (const auto& row : forwarding::forward_all(
             req, session, ask, &reporting_messaging::list_ore_file_versions_response::versions)) {
        const auto name = name_of.find(row.ore_file_code);
        if (name != name_of.end())
            contents[name->second] = row.content;
    }
    return contents;
}

}

schema_bundle schema_with_includes(const std::string& root,
                                   const std::map<std::string, std::string>& contents) {
    schema_bundle bundle;
    std::set<std::string> seen;
    std::deque<std::string> queue{root};
    while (!queue.empty()) {
        const auto name = queue.front();
        queue.pop_front();
        if (!seen.insert(name).second)
            continue;
        const auto content = contents.find(name);
        if (content == contents.end()) {
            bundle.missing.push_back(name);
            continue;
        }
        bundle.files.push_back({name, content->second});
        for (const auto& included : includes_of(content->second))
            queue.push_back(included);
    }
    return bundle;
}

std::vector<char> zip_of(const std::vector<schema_entry>& files) {
    // A name of its own, so two requests written at once cannot share a file.
    const auto path =
        std::filesystem::temp_directory_path() /
        ("ores-schema-" + boost::uuids::to_string(boost::uuids::random_generator()()) + ".zip");
    struct archive* writer = archive_write_new();
    archive_write_set_format_zip(writer);
    if (archive_write_open_filename(writer, path.string().c_str()) != ARCHIVE_OK) {
        archive_write_free(writer);
        throw std::runtime_error("The schema archive could not be written.");
    }
    for (const auto& file : files) {
        struct archive_entry* entry = archive_entry_new();
        archive_entry_set_pathname(entry, file.name.c_str());
        archive_entry_set_size(entry, static_cast<la_int64_t>(file.content.size()));
        archive_entry_set_filetype(entry, AE_IFREG);
        archive_entry_set_perm(entry, 0644);
        archive_write_header(writer, entry);
        if (!file.content.empty())
            archive_write_data(writer, file.content.data(), file.content.size());
        archive_entry_free(entry);
    }
    archive_write_close(writer);
    archive_write_free(writer);

    std::ifstream in(path, std::ios::binary);
    if (!in)
        throw std::runtime_error("The schema archive could not be read back.");
    std::vector<char> bytes{std::istreambuf_iterator<char>(in), std::istreambuf_iterator<char>()};
    in.close();
    std::error_code ignored;
    std::filesystem::remove(path, ignored);
    return bytes;
}

void ore_schema_zip::register_routes(
    std::shared_ptr<ores::http::net::router> router,
    std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
    nats_client& session) {
    using forwarding::guarded;
    const auto add = [&](ores::http::net::route_builder builder) {
        const auto built = builder.tags({"reporting"}).auth_required().build();
        router->add_route(built);
        registry->register_route(built);
    };

    add(router->get("/api/reporting/ore-files/{code}/schema")
            .summary("An ORE schema and every schema it includes, as one archive")
            .description("The schema is read at the engine version asked for, and so is every "
                         "schema it includes at once or through another, because a schema is "
                         "complete only with its includes. An include the version does not store "
                         "is a refusal that names it rather than an archive that cannot validate "
                         "a document.")
            .query_param("version", "string", "", true, "The engine version whose copies to read")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<reporting_messaging::list_ore_files_request>([&] {
                    rr::refuse_unknown_params(req, {"version"});
                    const auto code = req.get_path_param("code");
                    const auto version = req.get_query_param("version");
                    if (code.empty() || version.empty())
                        rr::invalid(std::string(no_schema_named));

                    const auto catalogue = forwarding::forward_all(
                        req,
                        session,
                        reporting_messaging::list_ore_files_request{},
                        &reporting_messaging::list_ore_files_response::files);
                    std::map<std::string, std::string> name_of;
                    const ores::reporting::domain::ore_file* schema = nullptr;
                    for (const auto& file : catalogue) {
                        name_of[file.code] = file.typical_filename;
                        if (file.code == code && file.format == "xsd")
                            schema = &file;
                    }
                    if (schema == nullptr)
                        rr::refuse(http_status::not_found,
                                   "not-found",
                                   "No ORE file is a schema with this code.");

                    const auto bundle = schema_with_includes(
                        schema->typical_filename, contents_of(req, session, version, name_of));
                    if (bundle.files.empty())
                        rr::refuse(http_status::not_found,
                                   "not-found",
                                   "ORE " + version + " has no stored copy of " +
                                       schema->typical_filename + ".");
                    if (!bundle.missing.empty()) {
                        std::string named;
                        for (const auto& missing : bundle.missing)
                            named += (named.empty() ? "" : ", ") + missing;
                        rr::refuse(http_status::not_found,
                                   "not-found",
                                   "ORE " + version + " has no stored copy of " + named +
                                       ", which " + schema->typical_filename + " includes.");
                    }

                    const auto bytes = zip_of(bundle.files);
                    http_response answer;
                    answer.content_type = "application/zip";
                    answer.body = std::string(bytes.begin(), bytes.end());
                    answer.set_header("content-disposition",
                                      disposition_of(code + "-" + version + ".zip"));
                    answer.set_header("cache-control", "private, no-store");
                    ORES_LOG_SEV(schema_zip_lg(), ores::logging::debug)
                        << "Answering " << bundle.files.size() << " schema file(s) for " << code
                        << " at " << version << ".";
                    return answer;
                });
            }));
}

}
