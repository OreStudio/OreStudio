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
#include "ores.http/routes/refdata/party_page_routes.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.refdata.api/messaging/business_centre_protocol.hpp"
#include "ores.refdata.api/messaging/country_protocol.hpp"
#include "ores.refdata.api/messaging/party_protocol.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include <boost/asio/awaitable.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <charconv>
#include <cstddef>
#include <exception>
#include <limits>
#include <set>
#include <string_view>

namespace ores::http::routes::refdata {

using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::http::domain::http_status;
using ores::nats::service::nats_client;
namespace json = ores::utility::codec::json;
namespace messaging = ores::refdata::messaging;
namespace rr = ores::http::net::record_routes;

namespace {

/// A bound read as the BFF read it: its default when the address leaves it out.
std::optional<std::uint32_t> bound_of(const http_request& req,
                                      const std::string& name,
                                      std::uint32_t fallback,
                                      std::uint32_t least,
                                      std::uint32_t most) {
    const auto text = req.get_query_param(name);
    if (text.empty())
        return fallback;
    std::uint32_t value = 0;
    const auto [end, failed] = std::from_chars(text.data(), text.data() + text.size(), value);
    if (failed != std::errc{} || end != text.data() + text.size() || value < least || value > most)
        return std::nullopt;
    return value;
}

std::string or_null(const std::optional<std::string>& text) {
    return text ? json::write(*text) : std::string("null");
}

/**
 * The flag of each business centre named, by the centre's code. A centre sits
 * in a country and the country carries the flag, so a centre or a country that
 * has none is left out.
 */
std::map<std::string, std::string>
centre_flags(const http_request& req, nats_client& session, const std::set<std::string>& codes) {
    std::map<std::string, std::string> flags;
    if (codes.empty())
        return flags;
    messaging::list_business_centres_request centres_msg;
    centres_msg.limit = static_cast<std::uint32_t>(codes.size());
    centres_msg.filter =
        messaging::business_centres_filter{std::vector<std::string>(codes.begin(), codes.end())};
    const auto centres = ores::http::forwarding::forward(req, session, centres_msg);
    rr::require_done(centres.result);
    std::set<std::string> countries;
    for (const auto& centre : centres.centres)
        if (!centre.country_alpha2_code.empty())
            countries.insert(centre.country_alpha2_code);
    if (countries.empty())
        return flags;
    messaging::list_countries_request countries_msg;
    countries_msg.limit = static_cast<std::uint32_t>(countries.size());
    countries_msg.filter =
        messaging::countries_filter{std::vector<std::string>(countries.begin(), countries.end())};
    const auto read = ores::http::forwarding::forward(req, session, countries_msg);
    rr::require_done(read.result);
    std::map<std::string, std::string> country_flags;
    for (const auto& country : read.countries)
        if (country.image_id)
            country_flags[country.alpha2_code] = boost::uuids::to_string(*country.image_id);
    for (const auto& centre : centres.centres) {
        const auto flag = country_flags.find(centre.country_alpha2_code);
        if (flag != country_flags.end())
            flags[centre.code] = flag->second;
    }
    return flags;
}

/**
 * The flags, or none when they cannot be read. A flag is decoration, so a page
 * whose flags fail is still the page.
 */
std::map<std::string, std::string>
flags_or_none(const http_request& req, nats_client& session, const std::set<std::string>& codes) {
    try {
        return centre_flags(req, session, codes);
    } catch (const rr::refusal&) {
        return {};
    } catch (const std::exception&) {
        return {};
    }
}

}

std::optional<party_page_routes::page_query>
party_page_routes::read_page_query(const http_request& req) {
    const auto offset = bound_of(req, "offset", 0, 0, std::numeric_limits<std::uint32_t>::max());
    const auto limit = bound_of(req, "limit", 20, 1, 1000);
    auto search = req.get_query_param("search");
    auto sort = req.get_query_param("sort");
    if (!offset || !limit || rr::characters_of(search) > 200 || rr::characters_of(sort) > 100)
        return std::nullopt;
    return page_query{std::move(search),
                      std::move(sort),
                      req.get_query_param("descending") == "true",
                      *offset,
                      *limit};
}

std::string party_page_routes::page_json(const std::vector<ores::refdata::domain::party>& parties,
                                         std::uint64_t total,
                                         const std::map<std::string, std::string>& names,
                                         const std::map<std::string, std::string>& flags) {
    const auto found = [](const std::map<std::string, std::string>& in,
                          const std::string& key) -> std::optional<std::string> {
        const auto it = in.find(key);
        return it == in.end() ? std::nullopt : std::optional<std::string>(it->second);
    };
    std::string out = "{\"parties\":[";
    for (std::size_t i = 0; i < parties.size(); ++i) {
        const auto& p = parties[i];
        const auto parent =
            p.parent_party_id ?
                std::optional<std::string>(boost::uuids::to_string(*p.parent_party_id)) :
                std::nullopt;
        out += (i == 0 ? "{" : ",{");
        out += "\"id\":" + json::write(boost::uuids::to_string(p.id));
        out += ",\"code\":" + json::write(p.short_code);
        out += ",\"name\":" + json::write(p.full_name);
        out += ",\"category\":" + json::write(p.party_category);
        out += ",\"type\":" + json::write(p.party_type);
        out += ",\"status\":" + json::write(p.status);
        out += ",\"parentId\":" + or_null(parent);
        out += ",\"parentName\":" + or_null(parent ? found(names, *parent) : std::nullopt);
        out += ",\"businessCentreCode\":" + json::write(p.business_center_code);
        out += ",\"flagImageId\":" + or_null(found(flags, p.business_center_code)) + "}";
    }
    return out + "],\"totalCount\":" + std::to_string(total) + "}";
}

std::string party_page_routes::read_page(const http_request& req,
                                         nats_client& session,
                                         const page_query& query) {
    messaging::list_parties_request page_msg;
    page_msg.offset = query.offset;
    page_msg.limit = query.limit;
    page_msg.order.field = query.sort;
    page_msg.order.descending = query.descending;
    if (!query.search.empty())
        page_msg.filter = messaging::parties_filter{std::nullopt, query.search};
    const auto page = ores::http::forwarding::forward(req, session, page_msg);
    rr::require_done(page.result);

    std::map<std::string, std::string> names;
    for (const auto& p : page.parties)
        names[boost::uuids::to_string(p.id)] = p.full_name;
    std::set<boost::uuids::uuid> elsewhere;
    for (const auto& p : page.parties)
        if (p.parent_party_id && !names.contains(boost::uuids::to_string(*p.parent_party_id)))
            elsewhere.insert(*p.parent_party_id);
    if (!elsewhere.empty()) {
        messaging::list_parties_request parents_msg;
        parents_msg.limit = static_cast<std::uint32_t>(elsewhere.size());
        parents_msg.filter = messaging::parties_filter{
            std::vector<boost::uuids::uuid>(elsewhere.begin(), elsewhere.end()), std::nullopt};
        const auto parents = ores::http::forwarding::forward(req, session, parents_msg);
        rr::require_done(parents.result);
        for (const auto& parent : parents.parties)
            names[boost::uuids::to_string(parent.id)] = parent.full_name;
    }

    std::set<std::string> centres;
    for (const auto& p : page.parties)
        if (!p.business_center_code.empty())
            centres.insert(p.business_center_code);
    return page_json(page.parties, page.total, names, flags_or_none(req, session, centres));
}

void party_page_routes::register_routes(
    std::shared_ptr<ores::http::net::router> router,
    std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
    nats_client& session) {
    const auto built =
        router->get("/api/parties")
            .summary("Read a page of the parties of the session's own tenant")
            .tags({"parties"})
            .auth_required()
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                const auto& claims = req.authenticated_user;
                if (claims && claims->tenant_id && *claims->tenant_id == rr::system_tenant_id)
                    co_return rr::refused(http_status::forbidden,
                                          "forbidden",
                                          "A tenant's parties are read from inside the tenant.");
                const auto query = read_page_query(req);
                if (!query)
                    co_return rr::refused(http_status::bad_request,
                                          "invalid-request",
                                          "The page offset and limit must be whole numbers.");
                co_return ores::http::forwarding::guarded<messaging::list_parties_request>(
                    [&] { return http_response::json(read_page(req, session, *query)); });
            })
            .build();
    router->add_route(built);
    registry->register_route(built);
}

}
