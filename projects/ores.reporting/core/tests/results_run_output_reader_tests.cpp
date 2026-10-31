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
#include "ores.ore.core/results/output_catalogue.hpp"
#include "ores.reporting.core/results/run_output_reader.hpp"
#include <boost/json.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <string>
#include <vector>

namespace {

const std::string run_tags("[results][run_output_reader]");

using ores::reporting::results::output_file;
using ores::reporting::results::read_run_outputs;
using ores::reporting::results::run_stamp;

const auto priced_at = std::chrono::sys_days{std::chrono::year{2026} / 10 / 31};

run_stamp the_run() {
    return run_stamp{.party_id = boost::uuids::random_generator()(),
                     .report_instance_id = boost::uuids::random_generator()(),
                     .report_definition_id = boost::uuids::random_generator()(),
                     .priced_at = priced_at};
}

// The npv family's required columns, with the figures a run states.
std::string npv_file() {
    return "#TradeId,TradeType,Maturity,MaturityTime,NPV,NpvCurrency,NPV(Base),BaseCurrency,"
           "Notional,NotionalCurrency,Notional(Base),NettingSet,CounterParty\n"
           "Swap_20y,Swap,2036-10-31,2.0,1234.5,USD,1100.0,EUR,1000.0,USD,900.0,CPTY_A,"
           "CPTY_A\n";
}

/**
 * @brief A one-payment flows file that states every column the family requires.
 *
 * The family states more columns than a payment needs, and a file that leaves
 * one out is the wrong shape rather than a payment with less to say, so the row
 * is built from the catalogue.
 */
std::string flows_file() {
    const auto* family = ores::ore::results::family_named("flows");
    REQUIRE(family != nullptr);

    // Currency is a qualifier rather than a required column, and the payment
    // needs one to be stated in, so the file states it too.
    auto columns = family->required_columns;
    columns.push_back("Currency");

    std::string header = "#";
    std::string row;
    for (const auto& name : columns) {
        if (header.size() > 1) {
            header += ",";
            row += ",";
        }
        header += name;
        if (name == "TradeId")
            row += "Swap_20y";
        else if (name == "CashflowNo")
            row += "1";
        else if (name == "LegNo")
            row += "0";
        else if (name == "PayDate")
            row += "2027-10-31";
        else if (name == "FlowType")
            row += "Interest";
        else if (name == "Amount")
            row += "25000.0";
        else if (name == "Currency")
            row += "USD";
        else if (ores::ore::results::column_named(*family, name)->kind ==
                 ores::ore::results::output_column_kind::decimal)
            row += "1.0";
        else if (ores::ore::results::column_named(*family, name)->kind ==
                 ores::ore::results::output_column_kind::date)
            row += "2027-10-31";
        else
            row += "x";
    }
    return header + "\n" + row + "\n";
}

}

TEST_CASE("each file of a run is read by its family, and both shapes land apart", run_tags) {
    const auto run = the_run();
    const auto outcome = read_run_outputs({output_file{.name = "npv.csv", .body = npv_file()},
                                           output_file{.name = "flows.csv", .body = flows_file()}},
                                          run);

    REQUIRE(outcome.refusals.empty());
    CHECK(outcome.files_read.size() == 2);

    // npv states three figures for one trade, each named by its heading.
    const auto npv =
        std::ranges::find_if(outcome.metrics, [](const auto& one) { return one.measure == "NPV"; });
    REQUIRE(npv != outcome.metrics.end());
    CHECK(npv->family == "npv");
    CHECK(npv->subject_type == "trade");
    CHECK(npv->subject_id == "Swap_20y");
    CHECK(npv->value == 1234.5);
    CHECK(npv->currency == "USD");
    CHECK(npv->party_id == run.party_id);
    CHECK(npv->report_instance_id == run.report_instance_id);
    CHECK(npv->report_definition_id == run.report_definition_id);

    // The file states no observation, so the run's own date is the observation.
    CHECK(npv->as_of == priced_at);

    // The counterparty is a key that is not the subject, so it travels beside
    // the figure rather than in a column of its own.
    const auto qualifiers = boost::json::parse(npv->qualifiers).as_object();
    CHECK(qualifiers.at("CounterParty").as_string() == "CPTY_A");

    // A payment is not a metric: it lands in the cashflow table, with the keys
    // that name the payment.
    REQUIRE_FALSE(outcome.cashflows.empty());
    const auto amount = std::ranges::find_if(
        outcome.cashflows, [](const auto& one) { return one.measure == "Amount"; });
    REQUIRE(amount != outcome.cashflows.end());
    const auto& flow = *amount;
    CHECK(flow.family == "flows");
    CHECK(flow.subject_id == "Swap_20y");
    CHECK(flow.cashflow_no == "1");
    CHECK(flow.leg_no == "0");
    CHECK(flow.measure == "Amount");
    CHECK(flow.value == 25000.0);
    REQUIRE(flow.pay_date.has_value());
    CHECK(*flow.pay_date == std::chrono::sys_days{std::chrono::year{2027} / 10 / 31});
}

TEST_CASE("a file no family states is counted rather than refused", run_tags) {
    const auto outcome = read_run_outputs(
        {output_file{.name = "something-else.csv", .body = "a,b\n1,2\n"}}, the_run());

    CHECK(outcome.refusals.empty());
    CHECK(outcome.metrics.empty());
    CHECK(outcome.cashflows.empty());
    REQUIRE(outcome.files_not_modelled.size() == 1);
    CHECK(outcome.files_not_modelled.front() == "something-else.csv");
}

TEST_CASE("a file whose row the reader refuses stops the whole run", run_tags) {
    const auto body = std::string("#TradeId,TradeType,Maturity,MaturityTime,NPV,NpvCurrency,"
                                  "NPV(Base),BaseCurrency,Notional,NotionalCurrency,Notional(Base),"
                                  "NettingSet,CounterParty\n"
                                  "Swap_20y,Swap,2036-10-31,2.0,not-a-figure,USD,1100.0,EUR,"
                                  "1000.0,USD,900.0,CPTY_A,CPTY_A\n");
    const auto outcome =
        read_run_outputs({output_file{.name = "npv.csv", .body = body}}, the_run());

    REQUIRE_FALSE(outcome.refusals.empty());
    CHECK(outcome.refusals.front().find("NPV") != std::string::npos);
}

TEST_CASE("a run whose files state no figure loads nothing", run_tags) {
    const auto outcome = read_run_outputs({}, the_run());

    CHECK(outcome.refusals.empty());
    CHECK(outcome.metrics.empty());
    CHECK(outcome.cashflows.empty());
    CHECK(outcome.files_read.empty());
}
