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
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/headers.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.refdata.api/messaging/book_protocol.hpp"
#include "ores.testing/nats_options_helper.hpp"
#include "ores.trading.core/service/bus_book_status.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <atomic>
#include <catch2/catch_test_macros.hpp>
#include <optional>

namespace {

constexpr std::string_view test_subject = "ores.test.books.status";

/** What the answering service saw, so the test can hold the port to it. */
struct seen {
    std::optional<std::string> delegated;
    std::optional<boost::uuids::uuid> asked_about;
};

}

using namespace ores::trading::service;

TEST_CASE("the port asks as the caller and maps the book's status", "[trading][bus]") {
    ores::nats::service::client nats(ores::testing::make_nats_options());
    nats.connect();
    REQUIRE(nats.is_connected());

    const auto book_id = boost::uuids::random_generator()();
    seen observed;
    std::string answer_status = "Active";

    // Answers as refdata would, and records what the port sent.
    auto sub = nats.subscribe(std::string(test_subject), [&](ores::nats::message msg) {
        const auto& codec = ores::nats::default_wire_codec();
        const auto request = codec.decode<ores::refdata::messaging::list_books_request>(msg.data);
        REQUIRE(request.has_value());
        REQUIRE(request->filter.has_value());
        REQUIRE(request->filter->id_one_of.has_value());
        REQUIRE(request->filter->id_one_of->size() == 1);
        observed.asked_about = request->filter->id_one_of->front();

        const auto header =
            msg.headers.find(std::string(ores::nats::headers::delegated_authorization));
        if (header != msg.headers.end()) {
            observed.delegated = header->second;
        }

        ores::refdata::messaging::list_books_response response;
        response.result.outcome = decltype(response.result.outcome)::ok;
        ores::refdata::domain::book book;
        book.id = book_id;
        book.book_status = answer_status;
        response.books.push_back(book);
        response.total = 1;
        const auto body = codec.encode(response);
        nats.publish(msg.reply_subject, std::span<const std::byte>(body.data(), body.size()));
    });

    bus_book_status port(nats, std::string(test_subject));

    SECTION("an open book is open, and the caller travels with the question") {
        answer_status = "Active";
        const auto state = port.of(book_id, "the-callers-token");
        CHECK(state == book_status_port::state::open);
        REQUIRE(observed.asked_about.has_value());
        CHECK(boost::uuids::to_string(*observed.asked_about) == boost::uuids::to_string(book_id));
        REQUIRE(observed.delegated.has_value());
        CHECK(*observed.delegated == "Bearer the-callers-token");
    }

    SECTION("a frozen book is not open") {
        answer_status = "Frozen";
        CHECK(port.of(book_id, "the-callers-token") == book_status_port::state::not_open);
    }

    SECTION("a closed book is not open") {
        answer_status = "Closed";
        CHECK(port.of(book_id, "the-callers-token") == book_status_port::state::not_open);
    }
}
