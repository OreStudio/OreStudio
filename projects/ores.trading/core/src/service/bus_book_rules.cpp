/** -*- mode: c++-ts-mode; tab-width: 4; indent-tabs-mode: nil -*-
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
#include "ores.trading.core/service/bus_book_rules.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/headers.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.refdata.api/messaging/book_product_type_protocol.hpp"
#include "ores.refdata.api/messaging/book_protocol.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <optional>
#include <span>
#include <unordered_map>
#include <vector>

namespace {

auto& lg() {
    using namespace ores::logging;
    static auto instance = make_logger("ores.trading.service.bus_book_rules");
    return instance;
}

/** The one status under which a book still takes a booking. */
constexpr std::string_view open_status = "Active";

/**
 * Sends one request as the caller and answers its reply.
 *
 * A request that throws, or a reply carrying the error header, is no answer at
 * all: the caller decides what the silence means, and every caller here means
 * `unknown`.
 */
std::optional<ores::nats::message> ask(ores::nats::service::client& nats,
                                       const std::string& subject,
                                       const std::vector<std::byte>& body,
                                       std::string_view actor_token,
                                       const std::string& what) {
    namespace logging = ores::logging;
    std::unordered_map<std::string, std::string> headers;
    if (!actor_token.empty()) {
        headers.emplace(std::string(ores::nats::headers::delegated_authorization),
                        "Bearer " + std::string(actor_token));
    }

    try {
        auto reply = nats.request_sync(
            subject, std::span<const std::byte>(body.data(), body.size()), headers);
        if (reply.headers.find(std::string(ores::nats::headers::x_error)) != reply.headers.end()) {
            BOOST_LOG_SEV(lg(), logging::warn) << "Reading " << what << " was refused.";
            return std::nullopt;
        }
        return reply;
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), logging::warn) << "Reading " << what << " failed: " << e.what();
        return std::nullopt;
    }
}

}

namespace ores::trading::service {

book_rules_port::answer bus_book_rules::of(const boost::uuids::uuid& book_id,
                                           std::string_view product_type_code,
                                           std::string_view actor_token) const {
    using ores::refdata::messaging::book_product_types_filter;
    using ores::refdata::messaging::books_filter;
    using ores::refdata::messaging::list_book_product_types_request;
    using ores::refdata::messaging::list_books_request;
    const auto& codec = ores::nats::default_wire_codec();
    const auto book_text = boost::uuids::to_string(book_id);

    books_filter filter;
    filter.id_one_of = std::vector<boost::uuids::uuid>{book_id};
    list_books_request request;
    request.limit = 1;
    request.filter = filter;

    const auto reply =
        ask(nats_, books_subject_, codec.encode(request), actor_token, "book " + book_text);
    if (!reply) {
        return {};
    }

    const auto decoded = codec.decode<list_books_request::response_type>(reply->data);
    if (!decoded || decoded->result.outcome != decltype(decoded->result.outcome)::ok ||
        decoded->books.empty()) {
        return {};
    }

    const auto& book = decoded->books.front();
    answer result;
    result.book_status =
        book.book_status == std::string(open_status) ? status::open : status::not_open;
    if (!book.restricts_product_types) {
        result.allowed = product_type::allows;
        return result;
    }

    book_product_types_filter types_filter;
    types_filter.book_id = book_id;
    types_filter.product_type_code = std::string(product_type_code);
    list_book_product_types_request types_request;
    types_request.limit = 1;
    types_request.filter = types_filter;

    const auto types_reply = ask(nats_,
                                 product_types_subject_,
                                 codec.encode(types_request),
                                 actor_token,
                                 "the product types of book " + book_text);
    if (!types_reply) {
        return result;
    }

    const auto allowed =
        codec.decode<list_book_product_types_request::response_type>(types_reply->data);
    if (!allowed || allowed->result.outcome != decltype(allowed->result.outcome)::ok) {
        return result;
    }
    result.allowed =
        allowed->book_product_types.empty() ? product_type::refuses : product_type::allows;
    return result;
}

}
