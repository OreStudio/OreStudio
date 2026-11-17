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
#include "ores.http/routes/compute/app_version_routes.hpp"
#include "ores.compute.api/messaging/app_protocol.hpp"
#include "ores.compute.api/messaging/app_version_platform_protocol.hpp"
#include "ores.compute.api/messaging/app_version_protocol.hpp"
#include "ores.compute.api/messaging/platform_protocol.hpp"
#include "ores.http.api/domain/http_request.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.http/routes/compute/grid_views.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include <boost/asio/awaitable.hpp>
#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <map>
#include <optional>
#include <string>
#include <string_view>
#include <type_traits>
#include <utility>
#include <vector>

namespace ores::http::routes::compute {

namespace {

namespace json = ores::utility::codec::json;
namespace rr = ores::http::net::record_routes;
namespace forwarding = ores::http::forwarding;
namespace grid = ores::http::routes::compute::grid;
namespace compute_domain = ores::compute::domain;
namespace compute_messaging = ores::compute::messaging;

using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::http::domain::http_status;
using ores::nats::service::nats_client;

/// The most text a release note holds.
constexpr std::size_t release_note_max = 20000;

/// The most text a write's commentary holds.
constexpr std::size_t commentary_max = 500;

/// The most rows the catalogue's own lists take; the catalogue is closed.
constexpr std::uint32_t catalogue_limit = 1000;

/// The reason a release note is edited for, unless the screen states another.
constexpr std::string_view note_reason = "common.non_material_update";

/// The note a screen states when it leaves the commentary out.
constexpr std::string_view note_commentary = "Edited the release note";

/// The sentence the note's write states when its body is not the shape it takes.
constexpr std::string_view body_refused =
    "The release note takes up to 20000 characters and the version it read.";

/// The sentence every other writer of the catalogue is refused with.
constexpr std::string_view operator_only = "Only a super administrator edits a release note.";

/// The sentence a route states when no build has the id.
constexpr std::string_view missing_build = "No app version has this id.";

/// The note's write as the screen states it.
struct release_note_body {
    std::string release_note;
    std::optional<int> version;
    std::optional<std::string> commentary;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<release_note_body>) {
    using ores::utility::codec::field;
    return ores::utility::codec::field_list{field{"release_note", &release_note_body::release_note},
                                            field{"version", &release_note_body::version},
                                            field{"commentary", &release_note_body::commentary}};
}

/// One package of a build, with the platform it runs on.
struct package final {
    std::string platform_id;
    std::string code;
    std::string display_name;
    bool is_active = false;
    std::string package_uri;
    std::string sha256;
};

/// One build of the catalogue as its page reads it.
struct page final {
    compute_domain::app_version version;
    std::optional<std::string> app_name;
    std::vector<package> packages;
    std::vector<compute_domain::app_version> versions;
};

/// Whether the caller may write the catalogue.
bool may_write(const http_request& req) {
    return grid::may_read_host(req);
}

/// The name of the app a build belongs to, or nothing when no app carries it.
std::optional<std::string>
app_name_of(const http_request& req, nats_client& session, const boost::uuids::uuid& app_id) {
    compute_messaging::list_apps_request msg;
    msg.limit = 1;
    msg.filter.emplace();
    msg.filter->id_one_of = std::vector{app_id};
    const auto reply = forwarding::forward(req, session, msg);
    rr::require_ok(reply.result);
    if (reply.apps.empty())
        return std::nullopt;
    return reply.apps.front().name;
}

/// Every version of one app, newest recorded first.
std::vector<compute_domain::app_version>
versions_of(const http_request& req, nats_client& session, const boost::uuids::uuid& app_id) {
    compute_messaging::list_by_app_id_app_versions_request msg;
    msg.app_id = app_id;
    msg.limit = catalogue_limit;
    const auto reply = forwarding::forward(req, session, msg);
    rr::require_ok(reply.result);

    auto versions = reply.app_versions;
    std::ranges::stable_sort(versions, [](const auto& left, const auto& right) {
        return left.recorded_at > right.recorded_at;
    });
    return versions;
}

/// The packages of one build, each with the platform it runs on, by platform code.
std::vector<package> packages_of(const http_request& req,
                                 nats_client& session,
                                 const boost::uuids::uuid& app_version_id) {
    compute_messaging::list_by_app_version_id_app_version_platforms_request rows;
    rows.app_version_id = app_version_id;
    rows.limit = catalogue_limit;
    const auto stored = forwarding::forward(req, session, rows);
    rr::require_ok(stored.result);

    compute_messaging::list_platforms_request platforms;
    platforms.limit = catalogue_limit;
    const auto known = forwarding::forward(req, session, platforms);
    rr::require_ok(known.result);

    std::map<boost::uuids::uuid, compute_domain::platform> by_id;
    for (const auto& platform : known.platforms)
        by_id.emplace(platform.id, platform);

    std::vector<package> out;
    out.reserve(stored.app_version_platforms.size());
    for (const auto& row : stored.app_version_platforms) {
        const auto found = by_id.find(row.platform_id);
        const auto known_platform = found != by_id.end();
        out.push_back({rr::as_text(row.platform_id),
                       known_platform ? found->second.code : rr::as_text(row.platform_id),
                       known_platform ? found->second.display_name : std::string(),
                       known_platform && found->second.is_active,
                       row.package_uri,
                       row.sha256});
    }
    std::ranges::stable_sort(
        out, [](const auto& left, const auto& right) { return left.code < right.code; });
    return out;
}

/// The page of one build, or nothing when no build has the id the caller may see.
std::optional<page>
page_of(const http_request& req, nats_client& session, const boost::uuids::uuid& id) {
    compute_messaging::get_app_version_request ask;
    ask.key.id = id;
    const auto reply = forwarding::forward(req, session, ask);
    if (reply.result.outcome == ores::utility::domain::outcome::missing || !reply.app_version)
        return std::nullopt;
    rr::require_ok(reply.result);

    page out;
    out.version = *reply.app_version;
    out.app_name = app_name_of(req, session, out.version.app_id);
    out.versions = versions_of(req, session, out.version.app_id);
    out.packages = packages_of(req, session, out.version.id);
    return out;
}

/// One build as the page states it.
std::string version_json(const compute_domain::app_version& version,
                         const std::optional<std::string>& app_name) {
    return "{\"id\":" + json::write(rr::as_text(version.id)) +
           ",\"app_id\":" + json::write(rr::as_text(version.app_id)) +
           ",\"app_name\":" + json::write(app_name) +
           ",\"wrapper_version\":" + json::write(version.wrapper_version) +
           ",\"engine_version\":" + json::write(version.engine_version) +
           ",\"min_ram_mb\":" + json::write(version.min_ram_mb) +
           ",\"release_note\":" + json::write(version.release_note) +
           ",\"version\":" + json::write(version.version) +
           ",\"modified_by\":" + json::write(version.modified_by) +
           ",\"recorded_at\":" + json::write(version.recorded_at) + "}";
}

/// One package as the page states it.
std::string package_json(const package& package) {
    return "{\"platform_id\":" + json::write(package.platform_id) +
           ",\"code\":" + json::write(package.code) +
           ",\"display_name\":" + json::write(package.display_name) +
           ",\"is_active\":" + json::write(package.is_active) +
           ",\"package_uri\":" + json::write(package.package_uri) +
           ",\"sha256\":" + json::write(package.sha256) + "}";
}

/// One sibling build as the page states it.
std::string sibling_json(const compute_domain::app_version& version) {
    return "{\"id\":" + json::write(rr::as_text(version.id)) +
           ",\"engine_version\":" + json::write(version.engine_version) +
           ",\"wrapper_version\":" + json::write(version.wrapper_version) +
           ",\"recorded_at\":" + json::write(version.recorded_at) + "}";
}

/// The page as the screen reads it.
std::string page_json(const page& read, bool may_edit) {
    std::vector<std::string> packages;
    packages.reserve(read.packages.size());
    for (const auto& entry : read.packages)
        packages.push_back(package_json(entry));

    std::vector<std::string> siblings;
    siblings.reserve(read.versions.size());
    for (const auto& entry : read.versions)
        siblings.push_back(sibling_json(entry));

    return "{\"version\":" + version_json(read.version, read.app_name) +
           ",\"packages\":" + rr::array_of(packages) + ",\"versions\":" + rr::array_of(siblings) +
           ",\"may_write\":" + json::write(may_edit) + "}";
}

/// The note as the screen states it, refused when a member is not the shape it takes.
release_note_body note_of(const http_request& req) {
    const auto body = rr::body_of<release_note_body>(
        req, {"release_note", "version", "commentary"}, std::string(body_refused));
    if (!body.version || *body.version < 0 ||
        rr::characters_of(body.release_note) > release_note_max ||
        (body.commentary && rr::characters_of(*body.commentary) > commentary_max))
        rr::invalid(std::string(body_refused));
    return body;
}

}

void app_version_routes::register_routes(
    std::shared_ptr<ores::http::net::router> router,
    std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
    nats_client& session) {
    using forwarding::guarded;
    const auto add = [&](ores::http::net::route_builder builder) {
        const auto built = builder.tags({"grid"}).auth_required().build();
        router->add_route(built);
        registry->register_route(built);
    };

    add(router->get("/api/operations/grid/app-versions/{id}")
            .summary("One build of the catalogue, with its note, its packages and its siblings")
            .description("The app the build belongs to, the packages the wrapper downloads and "
                         "the other versions of the app, newest first, and whether the caller "
                         "may edit the note.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<compute_messaging::get_app_version_request>([&] {
                    rr::refuse_unknown_params(req, {});
                    const auto found = page_of(req, session, grid::id_of(req));
                    if (!found)
                        rr::refuse(http_status::not_found, "not-found", std::string(missing_build));
                    return http_response::json(page_json(*found, may_write(req)));
                });
            }));

    add(router->put("/api/operations/grid/app-versions/{id}/release-note")
            .summary("Replace one build's release note, against the version the editor read")
            .description("The row is read again here, because the write replaces it whole. The "
                         "version the body states is the guard: a note edited since is refused "
                         "and not overwritten. The answer is the page as it stands after the "
                         "write.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<compute_messaging::put_app_version_request>([&] {
                    rr::refuse_unknown_params(req, {});
                    const auto id = grid::id_of(req);
                    if (!may_write(req))
                        rr::refuse(http_status::forbidden, "forbidden", std::string(operator_only));
                    const auto body = note_of(req);

                    compute_messaging::get_app_version_request ask;
                    ask.key.id = id;
                    const auto current = forwarding::forward(req, session, ask);
                    if (current.result.outcome == ores::utility::domain::outcome::missing ||
                        !current.app_version)
                        rr::refuse(http_status::not_found, "not-found", std::string(missing_build));
                    rr::require_ok(current.result);

                    compute_messaging::app_version_write write;
                    write.id = current.app_version->id;
                    write.app_id = current.app_version->app_id;
                    write.wrapper_version = current.app_version->wrapper_version;
                    write.engine_version = current.app_version->engine_version;
                    write.min_ram_mb = current.app_version->min_ram_mb;
                    write.release_note = body.release_note;

                    ores::utility::domain::change_intent intent;
                    intent.reason_code = std::string(note_reason);
                    intent.commentary = body.commentary.value_or(std::string(note_commentary));

                    const auto written =
                        forwarding::put_at_version<compute_messaging::put_app_version_request>(
                            req,
                            session,
                            std::move(write),
                            static_cast<std::uint32_t>(*body.version),
                            std::move(intent));
                    if (written.result.outcome == ores::utility::domain::outcome::missing ||
                        !written.app_version)
                        rr::refuse(http_status::not_found, "not-found", std::string(missing_build));
                    rr::require_ok(written.result);

                    const auto found = page_of(req, session, id);
                    if (!found)
                        rr::refuse(http_status::not_found, "not-found", std::string(missing_build));
                    return http_response::json(page_json(*found, true));
                });
            }));
}

}
