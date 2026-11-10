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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_http_route_web_records.cpp.mustache
 * To modify, update the template and regenerate.
 */
#include "ores.http/routes/refdata/party_contact_information_web_routes.hpp"
#include "ores.nats/domain/headers.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.nats/service/session_expired_error.hpp"
#include "ores.nats/service/timeouts.hpp"
#include "ores.refdata.api/domain/party_contact_information_codec.hpp"
#include "ores.refdata.api/messaging/party_contact_information_protocol.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include "ores.utility/codec/member_count.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <cstdint>
#include <exception>
#include <optional>
#include <stdexcept>
#include <string>
#include <string_view>
#include <tuple>
#include <type_traits>
#include <utility>
#include <vector>

namespace ores::http::routes::refdata {

using namespace logging;
using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::http::domain::http_status;
using ores::nats::service::nats_client;
namespace messaging = ores::refdata::messaging;

namespace {

/// How many rows a whole read asks for at a time.
constexpr std::uint32_t party_contact_information_web_page_size = 1000;

/// A refusal a route states, with the status and the code the browser reads.
struct party_contact_information_web_refusal final {
    http_status status;
    std::string code;
    std::string message;
};

[[noreturn]] void
party_contact_information_web_refuse(http_status status, std::string code, std::string message) {
    throw party_contact_information_web_refusal{status, std::move(code), std::move(message)};
}

[[noreturn]] void party_contact_information_web_invalid(std::string message) {
    party_contact_information_web_refuse(
        http_status::bad_request, "invalid-request", std::move(message));
}

http_response
party_contact_information_web_answer(const party_contact_information_web_refusal& refusal) {
    return http_response::api_error(refusal.status, refusal.code, refusal.message);
}

http_response party_contact_information_web_no_content() {
    http_response response;
    response.status = http_status::no_content;
    return response;
}

/// One parameter read as the member's own type, refused by name when malformed.
template <typename T>
T party_contact_information_web_read(const std::string& raw, const std::string& name) {
    try {
        if constexpr (std::is_same_v<T, std::string>) {
            return raw;
        } else if constexpr (std::is_same_v<T, bool>) {
            if (raw == "true")
                return true;
            if (raw == "false")
                return false;
            throw std::invalid_argument(raw);
        } else {
            return boost::lexical_cast<T>(raw);
        }
    } catch (const std::exception&) {
        party_contact_information_web_invalid("Invalid value for " + name + ": " + raw);
    }
}

/// A code named in the address: 1 to 100 characters once trimmed.
std::string party_contact_information_web_code(const std::string& raw,
                                               const std::string& sentence) {
    const auto first = raw.find_first_not_of(" \t");
    const auto last = raw.find_last_not_of(" \t");
    const auto trimmed =
        first == std::string::npos ? std::string{} : raw.substr(first, last - first + 1);
    if (trimmed.empty() || trimmed.size() > 100)
        party_contact_information_web_invalid(sentence);
    return trimmed;
}

/**
 * @brief Forward one canonical request with the caller's token, and decode the reply.
 *
 * A refusal the service states in its reply header becomes the status it
 * names. An expired session is the caller's to renew.
 */
template <typename Request>
typename Request::response_type party_contact_information_web_call(const http_request& req,
                                                                   nats_client& session,
                                                                   const Request& msg) {
    auto delegated = session.with_delegation(req.get_bearer_token().value_or(std::string{}));
    try {
        const auto reply =
            delegated.authenticated_request(msg.nats_subject,
                                            ores::nats::default_wire_codec().encode(msg),
                                            ores::nats::service::default_request_timeout);
        if (const auto it = reply.headers.find(std::string(ores::nats::headers::x_error));
            it != reply.headers.end()) {
            if (it->second == "forbidden")
                party_contact_information_web_refuse(
                    http_status::forbidden, "forbidden", it->second);
            if (it->second == "bad_request")
                party_contact_information_web_invalid(it->second);
            party_contact_information_web_refuse(
                http_status::unauthorized, "not-authenticated", it->second);
        }
        auto decoded =
            ores::nats::default_wire_codec().decode<typename Request::response_type>(reply.data);
        if (!decoded)
            party_contact_information_web_refuse(http_status::bad_gateway,
                                                 "internal",
                                                 std::string("Failed to decode response: ") +
                                                     decoded.error().what());
        return std::move(*decoded);
    } catch (const ores::nats::service::session_expired_error& e) {
        party_contact_information_web_refuse(
            http_status::unauthorized, "session-expired", e.what());
    }
}

/// The status a refused outcome names. A conflict the service states no words for says what it
/// means.
void party_contact_information_web_require_ok(const ores::utility::domain::result& result) {
    using ores::utility::domain::outcome;
    switch (result.outcome) {
        case outcome::ok:
            return;
        case outcome::conflict:
            party_contact_information_web_refuse(
                http_status::conflict,
                "conflict",
                result.message.empty() ?
                    "This record changed since it was read, or it already exists. "
                    "Reload and try again." :
                    result.message);
        case outcome::invalid:
            party_contact_information_web_invalid(result.message);
        case outcome::denied:
            party_contact_information_web_refuse(
                http_status::forbidden, "forbidden", result.message);
        case outcome::missing:
            party_contact_information_web_refuse(
                http_status::not_found, "not-found", result.message);
        default:
            party_contact_information_web_refuse(
                http_status::bad_gateway, "upstream-unavailable", result.message);
    }
}

/// One row as the service sends it, with the version the screens read on every row.
std::string
party_contact_information_web_row(const ores::refdata::domain::party_contact_information& row) {
    auto text = ores::utility::codec::json::write(row);
    return text;
}

std::string party_contact_information_web_rows(
    const std::vector<ores::refdata::domain::party_contact_information>& rows) {
    std::string out = "[";
    for (std::size_t i = 0; i < rows.size(); ++i) {
        if (i != 0)
            out += ",";
        out += party_contact_information_web_row(rows[i]);
    }
    return out + "]";
}

/// A page request that states no order and no filter.
messaging::list_party_contact_informations_request
party_contact_information_web_page(std::uint32_t offset, std::uint32_t limit) {
    messaging::list_party_contact_informations_request msg;
    msg.offset = offset;
    msg.limit = limit;
    msg.order.field = "";
    msg.order.descending = false;
    return msg;
}

}

/**
 * @brief The bodies the screens send, with their wire fields.
 *
 * They live in a namespace of the unit's own, so the wire_fields overloads of two
 * units in one unity build carry different qualified names.
 */
namespace party_contact_information_web_bodies {

/// What a write states: the row, the version it was read at, and why.
struct party_contact_information_web_write_body final {
    messaging::party_contact_information_write write;
    std::optional<std::uint32_t> version;
    std::string reasonCode;
    std::string commentary;
};

[[nodiscard]] constexpr auto
wire_fields(std::type_identity<party_contact_information_web_write_body>) {
    using ores::utility::codec::field;
    return std::tuple{field{"write", &party_contact_information_web_write_body::write},
                      field{"version", &party_contact_information_web_write_body::version},
                      field{"reasonCode", &party_contact_information_web_write_body::reasonCode},
                      field{"commentary", &party_contact_information_web_write_body::commentary}};
}
static_assert(ores::utility::codec::names_every_member<party_contact_information_web_write_body>());

/// What a removal states: the row's key, the version it was read at, and why.
struct party_contact_information_web_removal_body final {
    messaging::party_contact_information_key key;
    std::optional<std::uint32_t> version;
    std::string reasonCode;
    std::string commentary;
};

[[nodiscard]] constexpr auto
wire_fields(std::type_identity<party_contact_information_web_removal_body>) {
    using ores::utility::codec::field;
    return std::tuple{field{"key", &party_contact_information_web_removal_body::key},
                      field{"version", &party_contact_information_web_removal_body::version},
                      field{"reasonCode", &party_contact_information_web_removal_body::reasonCode},
                      field{"commentary", &party_contact_information_web_removal_body::commentary}};
}
static_assert(
    ores::utility::codec::names_every_member<party_contact_information_web_removal_body>());

}

namespace {

using party_contact_information_web_bodies::party_contact_information_web_removal_body;
using party_contact_information_web_bodies::party_contact_information_web_write_body;

/// A body read as its own shape, or nothing when it states another. An optional member may be left
/// out.
template <typename T>
std::optional<T> party_contact_information_web_body(std::string_view text) {
    auto value = ores::utility::codec::json::read_expected<T>(text);
    if (!value)
        return std::nullopt;
    return std::move(*value);
}

/// A reason is 1 to 200 characters once trimmed.
bool party_contact_information_web_has_reason(const std::string& reason) {
    const auto first = reason.find_first_not_of(" \t");
    return first != std::string::npos && reason.size() <= 200;
}

}

std::string_view party_contact_information_web_routes::catalogue_entry() {
    return R"json({"key":"party-contacts","entityType":"ores.refdata.party_contact_information","keyFields":["id"],"versioned":true,"writable":true,"search":false,"sortable":[],"events":{"component":"refdata","entity":"party_contact_informations"},"writePermission":"refdata::party_contact_informations:write","deletePermission":"refdata::party_contact_informations:delete"})json";
}

boost::asio::awaitable<http_response>
party_contact_information_web_routes::handle_list(const http_request& req, nats_client& session) {
    try {
        if (req.query_params.find("limit") == req.query_params.end()) {
            std::vector<ores::refdata::domain::party_contact_information> rows;
            for (std::uint32_t offset = 0;; offset += party_contact_information_web_page_size) {
                const auto reply = party_contact_information_web_call(
                    req,
                    session,
                    party_contact_information_web_page(offset,
                                                       party_contact_information_web_page_size));
                party_contact_information_web_require_ok(reply.result);
                rows.insert(rows.end(),
                            reply.party_contact_informations.begin(),
                            reply.party_contact_informations.end());
                if (reply.party_contact_informations.size() <
                    party_contact_information_web_page_size)
                    break;
            }
            co_return http_response::json("{\"rows\":" + party_contact_information_web_rows(rows) +
                                          "}");
        }

        const std::string page_sentence = "A page names an offset and a limit of 1 to 1000.";
        const auto raw_offset = req.get_query_param("offset");
        std::uint32_t offset = 0;
        std::uint32_t limit = 0;
        try {
            offset = raw_offset.empty() ? 0 : boost::lexical_cast<std::uint32_t>(raw_offset);
            limit = boost::lexical_cast<std::uint32_t>(req.get_query_param("limit"));
        } catch (const std::exception&) {
            party_contact_information_web_invalid(page_sentence);
        }
        if (limit < 1 || limit > 1000)
            party_contact_information_web_invalid(page_sentence);

        auto msg = party_contact_information_web_page(offset, limit);
        const auto sort = req.get_query_param("sort");
        if (!sort.empty()) {
            constexpr std::string_view sortable[] = {""};
            if (std::find(std::begin(sortable), std::end(sortable) - 1, sort) ==
                std::end(sortable) - 1)
                party_contact_information_web_invalid("party-contacts cannot be ordered by " +
                                                      sort + ".");
            msg.order.field = sort;
        }
        const auto descending = req.get_query_param("descending");
        if (!descending.empty() && descending != "true" && descending != "false")
            party_contact_information_web_invalid(page_sentence);
        msg.order.descending = descending == "true";

        const auto search = req.get_query_param("search");
        if (!search.empty())
            party_contact_information_web_invalid("party-contacts cannot be searched.");

        for (const auto& [name, value] : req.query_params) {
            if (!name.starts_with("f."))
                continue;
            const auto member = name.substr(2);
            constexpr std::string_view filters[] = {"party_id", ""};
            if (std::find(std::begin(filters), std::end(filters) - 1, member) ==
                std::end(filters) - 1)
                party_contact_information_web_invalid("party-contacts cannot be filtered by " +
                                                      member + ".");
        }
        if (const auto raw = req.get_query_param("f.party_id"); !raw.empty()) {
            if (!msg.filter)
                msg.filter = messaging::party_contact_informations_filter{};
            msg.filter->party_id =
                party_contact_information_web_read<boost::uuids::uuid>(raw, "f.party_id");
        }

        const auto reply = party_contact_information_web_call(req, session, msg);
        party_contact_information_web_require_ok(reply.result);
        co_return http_response::json(
            "{\"rows\":" + party_contact_information_web_rows(reply.party_contact_informations) +
            ",\"total\":" + std::to_string(reply.total) + "}");
    } catch (const party_contact_information_web_refusal& refusal) {
        co_return party_contact_information_web_answer(refusal);
    } catch (const std::exception& e) {
        co_return http_response::api_error(http_status::bad_gateway, "internal", e.what());
    }
}

boost::asio::awaitable<http_response>
party_contact_information_web_routes::handle_key(const http_request& req, nats_client& session) {
    try {
        const auto key = party_contact_information_web_code(
            req.get_path_param("key"), "A record is named by a code of 1 to 100 characters.");
        auto msg = party_contact_information_web_page(0, 1);
        msg.filter = messaging::party_contact_informations_filter{};
        msg.filter->id_one_of = std::vector<boost::uuids::uuid>{
            party_contact_information_web_read<boost::uuids::uuid>(key, "key")};
        const auto reply = party_contact_information_web_call(req, session, msg);
        party_contact_information_web_require_ok(reply.result);
        if (reply.party_contact_informations.empty())
            party_contact_information_web_refuse(http_status::not_found,
                                                 "not-found",
                                                 "There is no party-contacts record " + key + ".");
        co_return http_response::json(
            "{\"row\":" +
            party_contact_information_web_row(reply.party_contact_informations.front()) + "}");
    } catch (const party_contact_information_web_refusal& refusal) {
        co_return party_contact_information_web_answer(refusal);
    } catch (const std::exception& e) {
        co_return http_response::api_error(http_status::bad_gateway, "internal", e.what());
    }
}

boost::asio::awaitable<http_response>
party_contact_information_web_routes::handle_by(const http_request& req, nats_client& session) {
    try {
        const auto parent = party_contact_information_web_code(
            req.get_path_param("parent"), "A parent is named by a code of 1 to 100 characters.");
        std::vector<ores::refdata::domain::party_contact_information> rows;
        for (std::uint32_t offset = 0;; offset += party_contact_information_web_page_size) {
            messaging::list_by_party_id_party_contact_informations_request msg;
            msg.party_id = party_contact_information_web_read<boost::uuids::uuid>(parent, "parent");
            msg.scope = ores::utility::domain::scope::direct;
            msg.offset = offset;
            msg.limit = party_contact_information_web_page_size;
            msg.order.field = "";
            msg.order.descending = false;
            const auto reply = party_contact_information_web_call(req, session, msg);
            party_contact_information_web_require_ok(reply.result);
            rows.insert(rows.end(),
                        reply.party_contact_informations.begin(),
                        reply.party_contact_informations.end());
            if (reply.party_contact_informations.size() < party_contact_information_web_page_size)
                break;
        }
        co_return http_response::json("{\"rows\":" + party_contact_information_web_rows(rows) +
                                      "}");
    } catch (const party_contact_information_web_refusal& refusal) {
        co_return party_contact_information_web_answer(refusal);
    } catch (const std::exception& e) {
        co_return http_response::api_error(http_status::bad_gateway, "internal", e.what());
    }
}

boost::asio::awaitable<http_response>
party_contact_information_web_routes::handle_put(const http_request& req, nats_client& session) {
    try {
        const auto body =
            party_contact_information_web_body<party_contact_information_web_write_body>(req.body);
        if (!body || !party_contact_information_web_has_reason(body->reasonCode) ||
            body->commentary.size() > 2000)
            party_contact_information_web_invalid(
                "A write needs the row, the version it was read at, and a reason.");
        messaging::put_party_contact_information_request msg;
        msg.change.write = body->write;
        msg.change.precondition.kind =
            body->version ? ores::utility::domain::precondition_kind::must_match_version :
                            ores::utility::domain::precondition_kind::must_not_exist;
        msg.change.precondition.version = body->version;
        msg.intent.reason_code = body->reasonCode;
        msg.intent.commentary = body->commentary;
        const auto reply = party_contact_information_web_call(req, session, msg);
        party_contact_information_web_require_ok(reply.result);
        co_return party_contact_information_web_no_content();
    } catch (const party_contact_information_web_refusal& refusal) {
        co_return party_contact_information_web_answer(refusal);
    } catch (const std::exception& e) {
        co_return http_response::api_error(http_status::bad_gateway, "internal", e.what());
    }
}

boost::asio::awaitable<http_response>
party_contact_information_web_routes::handle_delete(const http_request& req, nats_client& session) {
    try {
        const auto body =
            party_contact_information_web_body<party_contact_information_web_removal_body>(
                req.body);
        if (!body || !party_contact_information_web_has_reason(body->reasonCode) ||
            body->commentary.size() > 2000)
            party_contact_information_web_invalid("A removal names the row by id, and a reason.");
        messaging::delete_party_contact_information_request msg;
        msg.removal.key = body->key;
        msg.removal.precondition.kind =
            body->version ? ores::utility::domain::precondition_kind::must_match_version :
                            ores::utility::domain::precondition_kind::any;
        msg.removal.precondition.version = body->version;
        msg.intent.reason_code = body->reasonCode;
        msg.intent.commentary = body->commentary;
        const auto reply = party_contact_information_web_call(req, session, msg);
        party_contact_information_web_require_ok(reply.result);
        co_return party_contact_information_web_no_content();
    } catch (const party_contact_information_web_refusal& refusal) {
        co_return party_contact_information_web_answer(refusal);
    } catch (const std::exception& e) {
        co_return http_response::api_error(http_status::bad_gateway, "internal", e.what());
    }
}

void party_contact_information_web_routes::register_routes(
    std::shared_ptr<ores::http::net::router> router,
    std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
    nats_client& session) {
    BOOST_LOG_SEV(lg(), info) << "Registering party-contacts record routes";

    auto list_route =
        router->get("/api/refdata/party-contacts")
            .summary("List party-contacts for a screen")
            .description("One page with its total, or every row when no limit is named.")
            .tags({"refdata"})
            .auth_required()
            .handler([&session](const http_request& req) { return handle_list(req, session); });
    const auto list_built = list_route.build();
    router->add_route(list_built);
    registry->register_route(list_built);

    auto key_route =
        router->get("/api/refdata/party-contacts/key/{key}")
            .summary("Read one party-contacts record for a screen")
            .description("One row, named by its key.")
            .tags({"refdata"})
            .auth_required()
            .handler([&session](const http_request& req) { return handle_key(req, session); });
    const auto key_built = key_route.build();
    router->add_route(key_built);
    registry->register_route(key_built);

    auto by_route =
        router->get("/api/refdata/party-contacts/by/{parent}")
            .summary("List one parent's party-contacts for a screen")
            .description("Every row whose party_id is the parent.")
            .tags({"refdata"})
            .auth_required()
            .handler([&session](const http_request& req) { return handle_by(req, session); });
    const auto by_built = by_route.build();
    router->add_route(by_built);
    registry->register_route(by_built);

    auto put_route =
        router->put("/api/refdata/party-contacts")
            .summary("Write one party-contacts record for a screen")
            .description("A new row with no version, else a correction of the version read.")
            .tags({"refdata"})
            .auth_required()
            .handler([&session](const http_request& req) { return handle_put(req, session); });
    const auto put_built = put_route.build();
    router->add_route(put_built);
    registry->register_route(put_built);

    auto remove_route =
        router->delete_("/api/refdata/party-contacts")
            .summary("Remove one party-contacts record for a screen")
            .description("Removes the row the body's key names.")
            .tags({"refdata"})
            .auth_required()
            .handler([&session](const http_request& req) { return handle_delete(req, session); });
    const auto remove_built = remove_route.build();
    router->add_route(remove_built);
    registry->register_route(remove_built);
}

}
