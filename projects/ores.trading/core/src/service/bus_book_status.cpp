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
 *
 */
#include "ores.trading.core/service/bus_book_status.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/headers.hpp"
#include "ores.refdata.api/messaging/book_protocol.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <boost/uuid/uuid_io.hpp>

namespace {

auto& lg() {
    using namespace ores::logging;
    static auto instance = make_logger("ores.trading.service.bus_book_status");
    return instance;
}

/** The one status under which a book still takes a booking. */
constexpr std::string_view open_status = "Active";

}

namespace ores::trading::service {

book_status_port::state bus_book_status::of(const boost::uuids::uuid& book_id,
                                            std::string_view actor_token) const {
    using ores::refdata::messaging::books_filter;
    using ores::refdata::messaging::list_books_request;
    const auto& codec = ores::nats::default_wire_codec();

    books_filter filter;
    filter.id_one_of = std::vector<boost::uuids::uuid>{book_id};
    list_books_request request;
    request.limit = 1;
    request.filter = filter;

    const auto body = codec.encode(request);
    const auto bytes = std::span<const std::byte>(body.data(), body.size());
    std::unordered_map<std::string, std::string> headers;
    if (!actor_token.empty()) {
        headers.emplace(std::string(ores::nats::headers::delegated_authorization),
                        "Bearer " + std::string(actor_token));
    }

    ores::nats::message reply;
    try {
        reply = nats_.request_sync(list_books_request::nats_subject, bytes, headers);
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), ores::logging::warn)
            << "Asking the book's status failed for " << boost::uuids::to_string(book_id) << ": "
            << e.what();
        return state::unknown;
    }
    if (reply.headers.find(std::string(ores::nats::headers::x_error)) != reply.headers.end()) {
        BOOST_LOG_SEV(lg(), ores::logging::warn)
            << "The book read was refused for " << boost::uuids::to_string(book_id);
        return state::unknown;
    }
    const auto decoded = codec.decode<list_books_request::response_type>(reply.data);
    if (!decoded || decoded->result.outcome != decltype(decoded->result.outcome)::ok ||
        decoded->books.empty()) {
        return state::unknown;
    }
    return decoded->books.front().book_status == std::string(open_status) ? state::open :
                                                                            state::not_open;
}

}
