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
#include "ores.marketdata.api/domain/tick_subjects.hpp"
#include <catch2/catch_test_macros.hpp>
#include <string>

namespace {

const std::string tags("[domain][tick_subjects]");

}

TEST_CASE("a_producer_publishes_on_its_source_subject", tags) {
    CHECK(ores::marketdata::domain::synthetic_tick_subject_for("usd.sofr") ==
          "synthetic.v1.ops.tick.usd.sofr");
}

TEST_CASE("a_subject_token_holds_only_what_a_subject_may", tags) {
    const std::string uri =
        "oresmd://fx/EUR?type=quote&instrument=fx_spot&quote=rate&ccy=USD *>. \t";
    const auto token = ores::marketdata::domain::subject_token(uri);
    for (const char c : token) {
        const bool allowed = (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') ||
                             (c >= '0' && c <= '9') || c == '-' || c == '_' || c == '%';
        CHECK(allowed);
    }
    CHECK(token.find('.') == std::string::npos);
    CHECK(token.find(' ') == std::string::npos);
    CHECK(token.find('*') == std::string::npos);
    CHECK(token.find('>') == std::string::npos);
}

TEST_CASE("a_subject_token_gives_its_text_back", tags) {
    using ores::marketdata::domain::subject_token;
    using ores::marketdata::domain::subject_token_text;
    for (const auto& text : {std::string(""),
                                   std::string("plain-Text_9"),
                                   std::string("oresmd://fx/EUR?type=quote&ccy=USD"),
                                   std::string("a.b c*d>e%f"),
                                   std::string("caf\xc3\xa9"),
                                   std::string("tab\there\nnewline")}) {
        CAPTURE(text);
        CHECK(subject_token_text(subject_token(text)) == text);
    }
}

TEST_CASE("a_subject_token_refuses_text_it_could_not_have_made", tags) {
    using ores::marketdata::domain::subject_token_text;
    CHECK_FALSE(subject_token_text("a.b").has_value());
    CHECK_FALSE(subject_token_text("%").has_value());
    CHECK_FALSE(subject_token_text("%2").has_value());
    CHECK_FALSE(subject_token_text("%ZZ").has_value());
    CHECK_FALSE(subject_token_text("%2e").has_value());
}

TEST_CASE("different_data_never_share_a_token", tags) {
    using ores::marketdata::domain::subject_token;
    CHECK(subject_token("a.b") != subject_token("a%2Eb"));
    CHECK(subject_token("a%b") != subject_token("a b"));
}

TEST_CASE("a_consumer_reads_a_datum_under_its_encoded_oresmd_uri", tags) {
    const std::string uri = "oresmd://fx/EUR?type=quote&instrument=fx_spot&quote=rate&ccy=USD";
    const auto subject = ores::marketdata::domain::market_tick_subject("t", "p", uri);
    CHECK(subject.starts_with(std::string(ores::marketdata::messaging::market_tick::nats_subject) +
                              ".t.p."));
    CHECK(subject.ends_with("." + ores::marketdata::domain::subject_token(uri)));
    CHECK(subject.find('/') == std::string::npos);
}

TEST_CASE("a_party_reads_all_its_data_under_one_wildcard", tags) {
    CHECK(ores::marketdata::domain::market_tick_subject_all("t", "p") ==
          std::string(ores::marketdata::messaging::market_tick::nats_subject) + ".t.p.>");
}
