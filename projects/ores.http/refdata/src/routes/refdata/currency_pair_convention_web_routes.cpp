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
#include "ores.http/routes/refdata/currency_pair_convention_web_routes.hpp"
#include "ores.nats/domain/headers.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.nats/service/session_expired_error.hpp"
#include "ores.nats/service/timeouts.hpp"
#include "ores.refdata.api/domain/currency_pair_convention_codec.hpp"
#include "ores.refdata.api/messaging/currency_pair_convention_protocol.hpp"
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

/// The unit's logger, for the detail a refusal does not show the browser.
auto& currency_pair_convention_web_lg() {
    static auto instance =
        make_logger("ores.http.routes.refdata.currency_pair_convention_web_routes");
    return instance;
}

/// The sentence the browser sees when the route cannot answer; the detail goes to the log.
constexpr std::string_view currency_pair_convention_web_unanswered =
    "The service could not answer this request.";

/// How many rows a whole read asks for at a time.
constexpr std::uint32_t currency_pair_convention_web_page_size = 1000;

/// A refusal a route states, with the status and the code the browser reads.
struct currency_pair_convention_web_refusal final {
    http_status status;
    std::string code;
    std::string message;
};

[[noreturn]] void
currency_pair_convention_web_refuse(http_status status, std::string code, std::string message) {
    throw currency_pair_convention_web_refusal{status, std::move(code), std::move(message)};
}

[[noreturn]] void currency_pair_convention_web_invalid(std::string message) {
    currency_pair_convention_web_refuse(
        http_status::bad_request, "invalid-request", std::move(message));
}

http_response
currency_pair_convention_web_answer(const currency_pair_convention_web_refusal& refusal) {
    return http_response::api_error(refusal.status, refusal.code, refusal.message);
}

http_response currency_pair_convention_web_no_content() {
    http_response response;
    response.status = http_status::no_content;
    return response;
}

/// One parameter read as the member's own type, refused by name when malformed.
template <typename T>
T currency_pair_convention_web_read(const std::string& raw, const std::string& name) {
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
        currency_pair_convention_web_invalid("Invalid value for " + name + ": " + raw);
    }
}

/// The text without its leading and trailing blanks.
std::string currency_pair_convention_web_trimmed(const std::string& raw) {
    constexpr std::string_view blanks = " \t\r\n";
    const auto first = raw.find_first_not_of(blanks);
    if (first == std::string::npos)
        return {};
    return raw.substr(first, raw.find_last_not_of(blanks) - first + 1);
}

/// A code named in the address: 1 to 100 characters once trimmed.
std::string currency_pair_convention_web_code(const std::string& raw, const std::string& sentence) {
    const auto trimmed = currency_pair_convention_web_trimmed(raw);
    if (trimmed.empty() || trimmed.size() > 100)
        currency_pair_convention_web_invalid(sentence);
    return trimmed;
}

/**
 * @brief Forward one canonical request with the caller's token, and decode the reply.
 *
 * A refusal the service states in its reply header becomes the status it
 * names. An expired session is the caller's to renew.
 */
template <typename Request>
typename Request::response_type currency_pair_convention_web_call(const http_request& req,
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
                currency_pair_convention_web_refuse(
                    http_status::forbidden, "forbidden", it->second);
            if (it->second == "bad_request")
                currency_pair_convention_web_invalid(it->second);
            if (it->second == "unauthorized" || it->second == "token_expired" ||
                it->second == "max_session_exceeded")
                currency_pair_convention_web_refuse(
                    http_status::unauthorized, "not-authenticated", it->second);
            BOOST_LOG_SEV(currency_pair_convention_web_lg(), warn)
                << "Unknown service error: " << it->second;
            currency_pair_convention_web_refuse(
                http_status::bad_gateway,
                "upstream-unavailable",
                std::string(currency_pair_convention_web_unanswered));
        }
        auto decoded =
            ores::nats::default_wire_codec().decode<typename Request::response_type>(reply.data);
        if (!decoded) {
            BOOST_LOG_SEV(currency_pair_convention_web_lg(), error)
                << "Failed to decode response: " << decoded.error().what();
            currency_pair_convention_web_refuse(
                http_status::bad_gateway,
                "internal",
                std::string(currency_pair_convention_web_unanswered));
        }
        return std::move(*decoded);
    } catch (const ores::nats::service::session_expired_error& e) {
        currency_pair_convention_web_refuse(http_status::unauthorized, "session-expired", e.what());
    }
}

/// The status a refused outcome names. A conflict the service states no words for says what it
/// means.
void currency_pair_convention_web_require_ok(const ores::utility::domain::result& result) {
    using ores::utility::domain::outcome;
    switch (result.outcome) {
        case outcome::ok:
            return;
        case outcome::conflict:
            currency_pair_convention_web_refuse(
                http_status::conflict,
                "conflict",
                result.message.empty() ?
                    "This record changed since it was read, or it already exists. "
                    "Reload and try again." :
                    result.message);
        case outcome::invalid:
            currency_pair_convention_web_invalid(result.message);
        case outcome::denied:
            currency_pair_convention_web_refuse(
                http_status::forbidden, "forbidden", result.message);
        case outcome::missing:
            currency_pair_convention_web_refuse(
                http_status::not_found, "not-found", result.message);
        default:
            currency_pair_convention_web_refuse(
                http_status::bad_gateway, "upstream-unavailable", result.message);
    }
}

/// One row as the service sends it, with the version the screens read on every row.
std::string
currency_pair_convention_web_row(const ores::refdata::domain::currency_pair_convention& row) {
    auto text = ores::utility::codec::json::write(row);
    return text;
}

std::string currency_pair_convention_web_rows(
    const std::vector<ores::refdata::domain::currency_pair_convention>& rows) {
    std::string out = "[";
    for (std::size_t i = 0; i < rows.size(); ++i) {
        if (i != 0)
            out += ",";
        out += currency_pair_convention_web_row(rows[i]);
    }
    return out + "]";
}

/// A page request that states no order and no filter.
messaging::list_currency_pair_conventions_request
currency_pair_convention_web_page(std::uint32_t offset, std::uint32_t limit) {
    messaging::list_currency_pair_conventions_request msg;
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
namespace currency_pair_convention_web_bodies {

/// What a write states: the row, the version it was read at, and why.
struct currency_pair_convention_web_write_body final {
    messaging::currency_pair_convention_write write;
    std::optional<std::uint32_t> version;
    std::string reasonCode;
    std::string commentary;
};

[[nodiscard]] constexpr auto
wire_fields(std::type_identity<currency_pair_convention_web_write_body>) {
    using ores::utility::codec::field;
    return std::tuple{field{"write", &currency_pair_convention_web_write_body::write},
                      field{"version", &currency_pair_convention_web_write_body::version},
                      field{"reasonCode", &currency_pair_convention_web_write_body::reasonCode},
                      field{"commentary", &currency_pair_convention_web_write_body::commentary}};
}
static_assert(ores::utility::codec::names_every_member<currency_pair_convention_web_write_body>());

/// What a removal states: the row's key, the version it was read at, and why.
struct currency_pair_convention_web_removal_body final {
    messaging::currency_pair_convention_key key;
    std::optional<std::uint32_t> version;
    std::string reasonCode;
    std::string commentary;
};

[[nodiscard]] constexpr auto
wire_fields(std::type_identity<currency_pair_convention_web_removal_body>) {
    using ores::utility::codec::field;
    return std::tuple{field{"key", &currency_pair_convention_web_removal_body::key},
                      field{"version", &currency_pair_convention_web_removal_body::version},
                      field{"reasonCode", &currency_pair_convention_web_removal_body::reasonCode},
                      field{"commentary", &currency_pair_convention_web_removal_body::commentary}};
}
static_assert(
    ores::utility::codec::names_every_member<currency_pair_convention_web_removal_body>());

}

namespace {

using currency_pair_convention_web_bodies::currency_pair_convention_web_removal_body;
using currency_pair_convention_web_bodies::currency_pair_convention_web_write_body;

/// A body read as its own shape, or nothing when it states another. An optional member may be left
/// out.
template <typename T>
std::optional<T> currency_pair_convention_web_body(std::string_view text) {
    auto value = ores::utility::codec::json::read_expected<T>(text);
    if (!value)
        return std::nullopt;
    return std::move(*value);
}

/// A reason is 1 to 200 characters once trimmed.
bool currency_pair_convention_web_has_reason(const std::string& reason) {
    const auto trimmed = currency_pair_convention_web_trimmed(reason);
    return !trimmed.empty() && trimmed.size() <= 200;
}

}

std::string_view currency_pair_convention_web_routes::catalogue_entry() {
    return R"json({"key":"currency-pair-conventions","entityType":"ores.refdata.currency_pair_convention","keyFields":["pair_code"],"versioned":true,"writable":true,"search":false,"sortable":[],"events":{"component":"refdata","entity":"currency_pair_conventions"},"writePermission":"refdata::currency_pair_conventions:write","deletePermission":"refdata::currency_pair_conventions:delete"})json";
}

boost::asio::awaitable<http_response>
currency_pair_convention_web_routes::handle_list(const http_request& req, nats_client& session) {
    try {
        if (req.query_params.find("limit") == req.query_params.end()) {
            std::vector<ores::refdata::domain::currency_pair_convention> rows;
            // A service may cap a page below the size asked for, so the read
            // advances by the rows that arrived and ends on an empty page, or on
            // a short one once it holds the total.
            for (std::uint32_t offset = 0;;) {
                const auto reply = currency_pair_convention_web_call(
                    req,
                    session,
                    currency_pair_convention_web_page(offset,
                                                      currency_pair_convention_web_page_size));
                currency_pair_convention_web_require_ok(reply.result);
                const auto& got = reply.conventions;
                rows.insert(rows.end(), got.begin(), got.end());
                offset += static_cast<std::uint32_t>(got.size());
                if (got.empty() || (got.size() < currency_pair_convention_web_page_size &&
                                    rows.size() >= reply.total))
                    break;
            }
            co_return http_response::json("{\"rows\":" + currency_pair_convention_web_rows(rows) +
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
            currency_pair_convention_web_invalid(page_sentence);
        }
        if (limit < 1 || limit > 1000)
            currency_pair_convention_web_invalid(page_sentence);

        auto msg = currency_pair_convention_web_page(offset, limit);
        const auto sort = req.get_query_param("sort");
        if (!sort.empty()) {
            constexpr std::string_view sortable[] = {""};
            if (std::find(std::begin(sortable), std::end(sortable) - 1, sort) ==
                std::end(sortable) - 1)
                currency_pair_convention_web_invalid(
                    "currency-pair-conventions cannot be ordered by " + sort + ".");
            msg.order.field = sort;
        }
        const auto descending = req.get_query_param("descending");
        if (!descending.empty() && descending != "true" && descending != "false")
            currency_pair_convention_web_invalid(page_sentence);
        msg.order.descending = descending == "true";

        const auto search = req.get_query_param("search");
        if (!search.empty())
            currency_pair_convention_web_invalid("currency-pair-conventions cannot be searched.");

        for (const auto& [name, value] : req.query_params) {
            if (!name.starts_with("f."))
                continue;
            const auto member = name.substr(2);
            constexpr std::string_view filters[] = {""};
            if (std::find(std::begin(filters), std::end(filters) - 1, member) ==
                std::end(filters) - 1)
                currency_pair_convention_web_invalid(
                    "currency-pair-conventions cannot be filtered by " + member + ".");
        }

        const auto reply = currency_pair_convention_web_call(req, session, msg);
        currency_pair_convention_web_require_ok(reply.result);
        co_return http_response::json(
            "{\"rows\":" + currency_pair_convention_web_rows(reply.conventions) +
            ",\"total\":" + std::to_string(reply.total) + "}");
    } catch (const currency_pair_convention_web_refusal& refusal) {
        co_return currency_pair_convention_web_answer(refusal);
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << e.what();
        co_return http_response::api_error(http_status::bad_gateway,
                                           "internal",
                                           std::string(currency_pair_convention_web_unanswered));
    }
}

boost::asio::awaitable<http_response>
currency_pair_convention_web_routes::handle_key(const http_request& req, nats_client& session) {
    try {
        const auto key = currency_pair_convention_web_code(
            req.get_path_param("key"), "A record is named by a code of 1 to 100 characters.");
        auto msg = currency_pair_convention_web_page(0, 1);
        msg.filter = messaging::currency_pair_conventions_filter{};
        msg.filter->pair_code_one_of =
            std::vector<std::string>{currency_pair_convention_web_read<std::string>(key, "key")};
        const auto reply = currency_pair_convention_web_call(req, session, msg);
        currency_pair_convention_web_require_ok(reply.result);
        if (reply.conventions.empty())
            currency_pair_convention_web_refuse(http_status::not_found,
                                                "not-found",
                                                "There is no currency-pair-conventions record " +
                                                    key + ".");
        co_return http_response::json(
            "{\"row\":" + currency_pair_convention_web_row(reply.conventions.front()) + "}");
    } catch (const currency_pair_convention_web_refusal& refusal) {
        co_return currency_pair_convention_web_answer(refusal);
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << e.what();
        co_return http_response::api_error(http_status::bad_gateway,
                                           "internal",
                                           std::string(currency_pair_convention_web_unanswered));
    }
}

boost::asio::awaitable<http_response>
currency_pair_convention_web_routes::handle_put(const http_request& req, nats_client& session) {
    try {
        const auto body =
            currency_pair_convention_web_body<currency_pair_convention_web_write_body>(req.body);
        if (!body || !currency_pair_convention_web_has_reason(body->reasonCode) ||
            body->commentary.size() > 2000)
            currency_pair_convention_web_invalid(
                "A write needs the row, the version it was read at, and a reason.");
        messaging::put_currency_pair_convention_request msg;
        msg.change.write = body->write;
        msg.change.precondition.kind =
            body->version ? ores::utility::domain::precondition_kind::must_match_version :
                            ores::utility::domain::precondition_kind::must_not_exist;
        msg.change.precondition.version = body->version;
        msg.intent.reason_code = body->reasonCode;
        msg.intent.commentary = body->commentary;
        const auto reply = currency_pair_convention_web_call(req, session, msg);
        currency_pair_convention_web_require_ok(reply.result);
        co_return currency_pair_convention_web_no_content();
    } catch (const currency_pair_convention_web_refusal& refusal) {
        co_return currency_pair_convention_web_answer(refusal);
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << e.what();
        co_return http_response::api_error(http_status::bad_gateway,
                                           "internal",
                                           std::string(currency_pair_convention_web_unanswered));
    }
}

boost::asio::awaitable<http_response>
currency_pair_convention_web_routes::handle_delete(const http_request& req, nats_client& session) {
    try {
        const auto body =
            currency_pair_convention_web_body<currency_pair_convention_web_removal_body>(req.body);
        if (!body || !currency_pair_convention_web_has_reason(body->reasonCode) ||
            body->commentary.size() > 2000)
            currency_pair_convention_web_invalid(
                "A removal names the row by pair_code, and a reason.");
        messaging::delete_currency_pair_convention_request msg;
        msg.removal.key = body->key;
        msg.removal.precondition.kind =
            body->version ? ores::utility::domain::precondition_kind::must_match_version :
                            ores::utility::domain::precondition_kind::any;
        msg.removal.precondition.version = body->version;
        msg.intent.reason_code = body->reasonCode;
        msg.intent.commentary = body->commentary;
        const auto reply = currency_pair_convention_web_call(req, session, msg);
        currency_pair_convention_web_require_ok(reply.result);
        co_return currency_pair_convention_web_no_content();
    } catch (const currency_pair_convention_web_refusal& refusal) {
        co_return currency_pair_convention_web_answer(refusal);
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << e.what();
        co_return http_response::api_error(http_status::bad_gateway,
                                           "internal",
                                           std::string(currency_pair_convention_web_unanswered));
    }
}

void currency_pair_convention_web_routes::register_routes(
    std::shared_ptr<ores::http::net::router> router,
    std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
    nats_client& session) {
    BOOST_LOG_SEV(lg(), info) << "Registering currency-pair-conventions record routes";

    auto list_route =
        router->get("/api/refdata/currency-pair-conventions")
            .summary("List currency-pair-conventions for a screen")
            .description("One page with its total, or every row when no limit is named.")
            .tags({"refdata"})
            .auth_required()
            .handler([&session](const http_request& req) { return handle_list(req, session); });
    const auto list_built = list_route.build();
    router->add_route(list_built);
    registry->register_route(list_built);

    auto key_route =
        router->get("/api/refdata/currency-pair-conventions/key/{key}")
            .summary("Read one currency-pair-conventions record for a screen")
            .description("One row, named by its key.")
            .tags({"refdata"})
            .auth_required()
            .handler([&session](const http_request& req) { return handle_key(req, session); });
    const auto key_built = key_route.build();
    router->add_route(key_built);
    registry->register_route(key_built);

    auto put_route =
        router->put("/api/refdata/currency-pair-conventions")
            .summary("Write one currency-pair-conventions record for a screen")
            .description("A new row with no version, else a correction of the version read.")
            .tags({"refdata"})
            .auth_required()
            .handler([&session](const http_request& req) { return handle_put(req, session); });
    const auto put_built = put_route.build();
    router->add_route(put_built);
    registry->register_route(put_built);

    auto remove_route =
        router->delete_("/api/refdata/currency-pair-conventions")
            .summary("Remove one currency-pair-conventions record for a screen")
            .description("Removes the row the body's key names.")
            .tags({"refdata"})
            .auth_required()
            .handler([&session](const http_request& req) { return handle_delete(req, session); });
    const auto remove_built = remove_route.build();
    router->add_route(remove_built);
    registry->register_route(remove_built);
}

}
