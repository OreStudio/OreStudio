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
#include "ores.marketdata.api/datum/value.hpp"
#include <catch2/catch_test_macros.hpp>
#include <string>

namespace {

const std::string tags("[datum][value]");

using namespace ores::marketdata::datum;

}

TEST_CASE("a_term_reads_each_kind_and_keeps_its_text", tags) {
    const struct {
        const char* text;
        term::kind kind;
    } cases[] = {{"1Y", term::kind::period},
                 {"0D", term::kind::period},
                 {"1Y6M", term::kind::period},
                 {"6m", term::kind::period},
                 {"2016-12-15", term::kind::date},
                 {"20160915", term::kind::date},
                 {"ON", term::kind::fx_tenor},
                 {"TN", term::kind::fx_tenor},
                 {"SN", term::kind::fx_tenor},
                 {"c1", term::kind::continuation},
                 {"c12", term::kind::continuation}};
    for (const auto& c : cases) {
        INFO("term: " << c.text);
        const auto t = term::parse(c.text);
        REQUIRE(t);
        CHECK(t->which() == c.kind);
        CHECK(t->text() == c.text);
    }
}

TEST_CASE("a_term_refuses_what_is_not_one", tags) {
    for (const auto* text :
         {"", "Y", "1", "1X", "2016-13-01", "2016-02-30", "c", "cX", "ATM", "1Y "}) {
        INFO("text: '" << text << "'");
        CHECK_FALSE(term::parse(text));
    }
}

TEST_CASE("a_decimal_keeps_the_spelling_it_arrived_in", tags) {
    for (const auto* text : {"0.030", "-0.02", "100", "1e-06", "112.5"}) {
        INFO("decimal: " << text);
        const auto d = decimal::parse(text);
        REQUIRE(d);
        CHECK(d->text() == text);
    }
    for (const auto* text : {"", "1.2abc", "inf", "nan", "0x10", " 1"}) {
        INFO("text: '" << text << "'");
        CHECK_FALSE(decimal::parse(text));
    }
}

TEST_CASE("a_code_is_one_token", tags) {
    const auto c = code::parse("Smile");
    REQUIRE(c);
    CHECK(c->text() == "Smile");
    CHECK_FALSE(code::parse(""));
    CHECK_FALSE(code::parse("A/B"));
}

TEST_CASE("a_strike_reads_every_form_of_ores_grammar_and_writes_it_back", tags) {
    const char* forms[] = {"3000",
                           "-0.01",
                           "ATM",
                           "ATMF",
                           "ATM/AtmFwd",
                           "ATM/AtmDeltaNeutral/DEL/Spot",
                           "DEL/Spot/Call/0.25",
                           "DEL/PaFwd/Put/-0.1",
                           "MNY/Spot/1.2",
                           "MNY/Fwd/0.9"};
    for (const auto* text : forms) {
        INFO("strike: " << text);
        const auto s = strike::parse(text);
        REQUIRE(s);
        CHECK(s->text() == text);
    }
}

TEST_CASE("a_strike_names_its_form", tags) {
    const auto atmf = strike::parse("ATMF");
    REQUIRE(atmf);
    const auto* atm = std::get_if<atm_strike>(&atmf->which());
    REQUIRE(atm);
    CHECK(atm->atm_type == "AtmFwd");
    CHECK(atm->shorthand);
    CHECK_FALSE(atm->delta_type);

    const auto del = strike::parse("DEL/Spot/Call/0.25");
    REQUIRE(del);
    const auto* d = std::get_if<delta_strike>(&del->which());
    REQUIRE(d);
    CHECK(d->delta_type == "Spot");
    CHECK(d->option_type == "Call");
    CHECK(d->delta.text() == "0.25");

    // The shorthand and the long form are different keys, so they are different
    // strikes even though ORE reads both as the same ATM.
    CHECK(*strike::parse("ATMF") != *strike::parse("ATM/AtmFwd"));
}

TEST_CASE("a_strike_refuses_what_ores_grammar_refuses", tags) {
    for (const auto* text : {"",
                             "ATM/Forward",
                             "ATM/AtmFwd/DEL",
                             "ATM/AtmFwd/X/Spot",
                             "DEL/Spot/C/0.25",
                             "DEL/Spot/Call",
                             "MNY/Mid/1.2",
                             "MNY/Spot",
                             "WING"}) {
        INFO("text: '" << text << "'");
        CHECK_FALSE(strike::parse(text));
    }
}

TEST_CASE("a_strike_label_reads_every_form_and_writes_it_canonically", tags) {
    const struct {
        const char* text;
        strike_label::form form;
        const char* canonical;
    } cases[] = {{"ATM", strike_label::form::at_the_money, "ATM"},
                 {"atm", strike_label::form::at_the_money, "ATM"},
                 {"25RR", strike_label::form::risk_reversal, "25RR"},
                 {"25rr", strike_label::form::risk_reversal, "25RR"},
                 {"25BF", strike_label::form::butterfly, "25BF"},
                 {"10BF", strike_label::form::butterfly, "10BF"},
                 {"25C", strike_label::form::delta_call, "25C"},
                 {"25P", strike_label::form::delta_put, "25P"},
                 {"0.05", strike_label::form::level, "0.05"},
                 {"-25", strike_label::form::level, "-25"}};
    for (const auto& c : cases) {
        INFO("label: " << c.text);
        const auto l = strike_label::parse(c.text);
        REQUIRE(l);
        CHECK(l->which() == c.form);
        CHECK(l->text() == c.canonical);
    }
}

TEST_CASE("two_spellings_of_one_component_are_one_strike_label", tags) {
    const auto canonical = strike_label::parse("25RR");
    REQUIRE(canonical);
    for (const auto* text : {"25rr", "25Rr", "+25RR", "25.0RR", "25.00RR"}) {
        INFO("label: " << text);
        const auto l = strike_label::parse(text);
        REQUIRE(l);
        CHECK(*l == *canonical);
        CHECK(l->text() == "25RR");
    }
    const auto level = strike_label::parse("25.0");
    REQUIRE(level);
    CHECK(*level == *strike_label::parse("25"));
    CHECK(level->number() == "25");
}

TEST_CASE("a_strike_label_ore_refuses_is_refused", tags) {
    // ORE's strike grammar reads ATMF and 25D, and its FX option quote then
    // refuses them; BF and FLY are not in the grammar at all.
    for (const auto* text :
         {"", "ATMF", "25D", "BF", "RR", "FLY", "WING", "ATM+0.5", "25ATMF", "C", "25RR "}) {
        INFO("text: '" << text << "'");
        CHECK_FALSE(strike_label::parse(text));
    }
}

TEST_CASE("text_of_writes_each_value_as_the_key_does", tags) {
    CHECK(text_of(value{none}).empty());
    CHECK(text_of(value{std::string("Lufthansa")}) == "Lufthansa");
    CHECK(text_of(value{*term::parse("6M")}) == "6M");
    CHECK(text_of(value{*decimal::parse("0.030")}) == "0.030");
    CHECK(text_of(value{*strike::parse("MNY/Spot/1.2")}) == "MNY/Spot/1.2");
    CHECK(text_of(value{*code::parse("F")}) == "F");
    CHECK(text_of(value{*strike_label::parse("25rr")}) == "25RR");
}
