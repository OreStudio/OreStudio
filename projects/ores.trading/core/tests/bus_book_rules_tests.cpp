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
#include "ores.refdata.api/messaging/book_product_type_protocol.hpp"
#include "ores.refdata.api/messaging/book_protocol.hpp"
#include "ores.testing/nats_options_helper.hpp"
#include "ores.trading.core/service/bus_book_rules.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <atomic>
#include <catch2/catch_test_macros.hpp>
#include <optional>

namespace {

constexpr std::string_view books_subject = "ores.test.books.rules";
constexpr std::string_view product_types_subject = "ores.test.book_product_types.rules";

/** What the answering service saw, so the test can hold the port to it. */
struct seen {
    std::optional<std::string> delegated;
    std::optional<boost::uuids::uuid> asked_about;
    std::optional<std::string> product_type_code;
    std::atomic<int> product_type_reads{0};
};

}

using namespace ores::trading::service;

TEST_CASE("the port asks as the caller and maps the book's rules", "[trading][bus]") {
    ores::nats::service::client nats(ores::testing::make_nats_options());
    nats.connect();
    REQUIRE(nats.is_connected());

    const auto book_id = boost::uuids::random_generator()();
    seen observed;
    std::string answer_status = "Active";
    bool answer_restricts = false;
    bool answer_allows = true;

    // Answers as refdata would, and records what the port sent.
    auto book_sub = nats.subscribe(std::string(books_subject), [&](ores::nats::message msg) {
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
        book.restricts_product_types = answer_restricts;
        response.books.push_back(book);
        response.total = 1;
        const auto body = codec.encode(response);
        nats.publish(msg.reply_subject, std::span<const std::byte>(body.data(), body.size()));
    });

    auto product_type_sub =
        nats.subscribe(std::string(product_types_subject), [&](ores::nats::message msg) {
            const auto& codec = ores::nats::default_wire_codec();
            const auto request =
                codec.decode<ores::refdata::messaging::list_book_product_types_request>(msg.data);
            REQUIRE(request.has_value());
            REQUIRE(request->filter.has_value());
            REQUIRE(request->filter->book_id.has_value());
            CHECK(*request->filter->book_id == book_id);
            REQUIRE(request->filter->product_type_code.has_value());
            observed.product_type_code = *request->filter->product_type_code;
            ++observed.product_type_reads;

            ores::refdata::messaging::list_book_product_types_response response;
            response.result.outcome = decltype(response.result.outcome)::ok;
            if (answer_allows) {
                ores::refdata::domain::book_product_type allowed;
                allowed.book_id = book_id;
                allowed.product_type_code = *request->filter->product_type_code;
                response.book_product_types.push_back(allowed);
                response.total = 1;
            }
            const auto body = codec.encode(response);
            nats.publish(msg.reply_subject, std::span<const std::byte>(body.data(), body.size()));
        });

    bus_book_rules port(nats, std::string(books_subject), std::string(product_types_subject));

    SECTION("an open book that allows everything, and the caller travels with the question") {
        const auto rules = port.of(book_id, "FxForward", "the-callers-token");
        CHECK(rules.book_status == book_rules_port::status::open);
        CHECK(rules.allowed == book_rules_port::product_type::allows);
        // The flag spares the junction read: an unrestricted book answers alone.
        CHECK(observed.product_type_reads.load() == 0);
        REQUIRE(observed.asked_about.has_value());
        CHECK(boost::uuids::to_string(*observed.asked_about) == boost::uuids::to_string(book_id));
        REQUIRE(observed.delegated.has_value());
        CHECK(*observed.delegated == "Bearer the-callers-token");
    }

    SECTION("a frozen book is not open") {
        answer_status = "Frozen";
        CHECK(port.of(book_id, "FxForward", "the-callers-token").book_status ==
              book_rules_port::status::not_open);
    }

    SECTION("a closed book is not open") {
        answer_status = "Closed";
        CHECK(port.of(book_id, "FxForward", "the-callers-token").book_status ==
              book_rules_port::status::not_open);
    }

    SECTION("a restricting book that holds the type allows it") {
        answer_restricts = true;
        const auto rules = port.of(book_id, "FxForward", "the-callers-token");
        CHECK(rules.book_status == book_rules_port::status::open);
        CHECK(rules.allowed == book_rules_port::product_type::allows);
        // The question carries the type the trade states, not the whole set.
        REQUIRE(observed.product_type_code.has_value());
        CHECK(*observed.product_type_code == "FxForward");
    }

    SECTION("a restricting book that holds no row for the type refuses it") {
        answer_restricts = true;
        answer_allows = false;
        const auto rules = port.of(book_id, "FxForward", "the-callers-token");
        CHECK(rules.book_status == book_rules_port::status::open);
        CHECK(rules.allowed == book_rules_port::product_type::refuses);
    }
}
