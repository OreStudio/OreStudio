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
#include "ores.logging/make_logger.hpp"
#include "ores.ore.core/domain/domain.hpp"
#include "ores.ore.core/domain/swap_instrument_mapper.hpp"
#include "ores.platform/filesystem/file.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.testing/project_root.hpp"
#include <catch2/catch_approx.hpp>
#include <catch2/catch_test_macros.hpp>

using Catch::Approx;

/**
 * @file xml_swaption_mapper_roundtrip_tests.cpp
 * @brief Thing 3: Mapper fidelity tests for Swaption and CallableSwap.
 */

namespace {

const std::string_view test_suite("ores.ore.swaption.mapper.roundtrip.tests");
const std::string tags("[ore][xml][mapper][roundtrip][swaption]");

using ores::ore::domain::portfolio;
using ores::ore::domain::swap_instrument_mapper;
using namespace ores::logging;

std::filesystem::path example_path(const std::string& filename) {
    return ores::testing::project_root::resolve("external/ore/examples/Products/Example_Trades/" +
                                                filename);
}

ores::ore::domain::trade load_trade(const std::string& filename, std::size_t index = 0) {
    using ores::platform::filesystem::file;
    const auto path = example_path(filename);
    const std::string content = file::read_content(path);
    portfolio p;
    ores::ore::domain::load_data(content, p);
    REQUIRE(p.Trade.size() > index);
    return p.Trade[index];
}

} // namespace

// =============================================================================
// Swaption (European) mapper tests
// =============================================================================


namespace {

// The domain holds a calendar date; the ORE XML holds its ISO-8601 spelling.
[[maybe_unused]] std::string ore_iso(const std::chrono::year_month_day& d) {
    return ores::platform::time::datetime::to_iso8601_date(d);
}

[[maybe_unused]] std::string ore_iso(const std::optional<std::chrono::year_month_day>& d) {
    return d ? ores::platform::time::datetime::to_iso8601_date(*d) : std::string{};
}

} // namespace


/**
 * The leg's economics are rows beside it, so a test reads the child of the
 * leg it is asserting on rather than a column of the leg.
 */
static double leg_rate(const ores::trading::domain::swap_instrument_data& data,
                       int leg_number,
                       const std::string& rate_role) {
    for (const auto& r : data.leg_rates)
        if (r.leg_number == leg_number && r.rate_role == rate_role)
            return r.value;
    return 0.0;
}

TEST_CASE("mapper_roundtrip_swaption_european_forward", tags) {
    auto lg(make_logger(test_suite));
    const auto t = load_trade("IR_Swaption_European.xml", 0);

    const auto result = swap_instrument_mapper::forward_swaption(t);
    const auto& instr = std::get<ores::trading::domain::swaption_instrument>(result.facts);

    CHECK(instr.exercise_type == "European");
    CHECK(ore_iso(instr.expiry_date) == "2033-02-20"); // first exercise date
    CHECK(ore_iso(result.header.maturity_date) == "2043-02-21");
    REQUIRE(result.legs.size() == 2u);
    // leg 0: floating (EUR-EURIBOR-3M)
    CHECK(result.legs[0].leg_type_code == "Floating");
    CHECK(result.legs[0].currency == "EUR");
    CHECK(result.legs[0].floating_index_code == "EUR-EURIBOR-3M");
    // leg 1: fixed (2%)
    CHECK(result.legs[1].leg_type_code == "Fixed");
    CHECK(leg_rate(result, 2, "fixed") == Approx(0.02).epsilon(0.0001));
    BOOST_LOG_SEV(lg, info) << "Swaption European forward-mapper test passed";
}

TEST_CASE("mapper_roundtrip_swaption_european_reverse", tags) {
    auto lg(make_logger(test_suite));
    const auto t = load_trade("IR_Swaption_European.xml", 0);
    const auto result = swap_instrument_mapper::forward_swaption(t);

    const auto reconstructed = swap_instrument_mapper::reverse_swaption(
        result.header,
        std::get<ores::trading::domain::swaption_instrument>(result.facts),
        result.legs,
        result.leg_amounts,
        result.leg_rates,
        result.schedules,
        result.schedule_dates,
        result.options,
        result.option_premiums,
        result.option_exercise_fees,
        result.option_payment_dates,
        result.option_exercise_prices);

    REQUIRE(reconstructed.SwaptionData.operator bool());
    const auto& sd = *reconstructed.SwaptionData;

    // OptionData: style and exercise date round-trip
    REQUIRE(sd.OptionData.operator bool());
    CHECK(std::string(*sd.OptionData->Style) == "European");
    REQUIRE(sd.OptionData->exerciseDatesGroup.operator bool());
    REQUIRE(sd.OptionData->exerciseDatesGroup->ExerciseDates.operator bool());
    REQUIRE(!sd.OptionData->exerciseDatesGroup->ExerciseDates->ExerciseDate.empty());
    CHECK(std::string(sd.OptionData->exerciseDatesGroup->ExerciseDates->ExerciseDate[0]) ==
          "2033-02-20");

    // Two legs reconstructed
    REQUIRE(sd.LegData.size() == 2u);
    CHECK(std::string(*sd.LegData[0].Currency) == "EUR");
    CHECK(std::string(*sd.LegData[1].Currency) == "EUR");
    BOOST_LOG_SEV(lg, info) << "Swaption European reverse-mapper test passed";
}

TEST_CASE("mapper_roundtrip_swaption_option_block", tags) {
    auto lg(make_logger(test_suite));
    const auto t = load_trade("IR_Swaption_European.xml", 0);
    const auto result = swap_instrument_mapper::forward_swaption(t);

    // The document's option element becomes one row and its premium children.
    REQUIRE(result.options.size() == 1u);
    const auto& option = result.options.front();
    CHECK(option.long_short == "Long");
    CHECK(option.style == std::optional<std::string>("European"));
    CHECK(option.settlement == std::optional<std::string>("Cash"));
    REQUIRE(result.option_premiums.size() == 1u);
    const auto& premium = result.option_premiums.front();
    CHECK(premium.sequence_number == 1);
    CHECK(premium.amount.to_double() == Approx(1090000.0).epsilon(0.0001));
    CHECK(premium.currency == "EUR");
    CHECK(ore_iso(premium.pay_date) == "2033-02-20");

    // The exercise date is a shared schedule row under the option owner.
    REQUIRE(result.schedules.size() == 1u);
    CHECK(result.schedules.front().owner_role == "option");
    CHECK(result.schedules.front().owner_number == 1);
    CHECK(result.schedules.front().schedule_role == "exercise_dates");
    CHECK(result.schedules.front().schedule_kind == "dates");
    REQUIRE(result.schedule_dates.size() == 1u);
    CHECK(ore_iso(result.schedule_dates.front().schedule_date) == "2033-02-20");

    const auto reconstructed = swap_instrument_mapper::reverse_swaption(
        result.header,
        std::get<ores::trading::domain::swaption_instrument>(result.facts),
        result.legs,
        result.leg_amounts,
        result.leg_rates,
        result.schedules,
        result.schedule_dates,
        result.options,
        result.option_premiums,
        result.option_exercise_fees,
        result.option_payment_dates,
        result.option_exercise_prices);

    REQUIRE(reconstructed.SwaptionData.operator bool());
    const auto& sd = *reconstructed.SwaptionData;
    REQUIRE(sd.OptionData.operator bool());
    CHECK(std::string(sd.OptionData->LongShort) == "Long");
    CHECK(std::string(*sd.OptionData->Style) == "European");
    CHECK(ores::ore::domain::to_string(*sd.OptionData->Settlement) == "Cash");
    REQUIRE(sd.OptionData->Premiums.operator bool());
    REQUIRE(sd.OptionData->Premiums->Premium.size() == 1u);
    const auto& p = sd.OptionData->Premiums->Premium.front();
    CHECK(static_cast<float>(p.Amount) == Approx(1090000.0f).epsilon(0.0001f));
    CHECK(std::string(p.Currency) == "EUR");
    CHECK(std::string(p.PayDate) == "2033-02-20");
    REQUIRE(sd.OptionData->exerciseDatesGroup.operator bool());
    REQUIRE(sd.OptionData->exerciseDatesGroup->ExerciseDates.operator bool());
    REQUIRE(sd.OptionData->exerciseDatesGroup->ExerciseDates->ExerciseDate.size() == 1u);
    CHECK(std::string(sd.OptionData->exerciseDatesGroup->ExerciseDates->ExerciseDate[0]) ==
          "2033-02-20");
    BOOST_LOG_SEV(lg, info) << "Swaption option-block round-trip test passed";
}

TEST_CASE("mapper_roundtrip_swaption_exercise_price_children", tags) {
    auto lg(make_logger(test_suite));
    auto t = load_trade("IR_Swaption_European.xml", 0);

    // No corpus document states an exercise price list, so the fourth child
    // is proved on a trade that states two prices against two exercise dates.
    ores::ore::domain::optionData_ExercisePrices_t prices;
    static_cast<std::string&>(prices) = "0.01 0.02";
    t.SwaptionData->OptionData->ExercisePrices = std::move(prices);
    ores::ore::domain::date second;
    static_cast<std::string&>(second) = "2034-02-20";
    t.SwaptionData->OptionData->exerciseDatesGroup->ExerciseDates->ExerciseDate.push_back(
        std::move(second));

    const auto result = swap_instrument_mapper::forward_swaption(t);

    REQUIRE(result.option_exercise_prices.size() == 2u);
    CHECK(result.option_exercise_prices[0].sequence_number == 1);
    CHECK(ore_iso(result.option_exercise_prices[0].exercise_date) == "2033-02-20");
    CHECK(result.option_exercise_prices[0].price.to_double() == Approx(0.01).epsilon(0.0000001));
    CHECK(result.option_exercise_prices[1].sequence_number == 2);
    CHECK(ore_iso(result.option_exercise_prices[1].exercise_date) == "2034-02-20");
    CHECK(result.option_exercise_prices[1].price.to_double() == Approx(0.02).epsilon(0.0000001));

    const auto reconstructed = swap_instrument_mapper::reverse_swaption(
        result.header,
        std::get<ores::trading::domain::swaption_instrument>(result.facts),
        result.legs,
        result.leg_amounts,
        result.leg_rates,
        result.schedules,
        result.schedule_dates,
        result.options,
        result.option_premiums,
        result.option_exercise_fees,
        result.option_payment_dates,
        result.option_exercise_prices);

    REQUIRE(reconstructed.SwaptionData.operator bool());
    const auto& sd = *reconstructed.SwaptionData;
    REQUIRE(sd.OptionData.operator bool());
    REQUIRE(sd.OptionData->ExercisePrices.operator bool());
    CHECK(std::string(*sd.OptionData->ExercisePrices) == "0.01 0.02");
    BOOST_LOG_SEV(lg, info) << "Swaption exercise-price round-trip test passed";
}

// =============================================================================
// Swaption (Bermudan) mapper tests
// =============================================================================

TEST_CASE("mapper_roundtrip_swaption_bermudan_forward", tags) {
    auto lg(make_logger(test_suite));
    const auto t = load_trade("IR_Swaption_Bermudan.xml", 0);

    const auto result = swap_instrument_mapper::forward_swaption(t);
    const auto& instr = std::get<ores::trading::domain::swaption_instrument>(result.facts);

    CHECK(instr.exercise_type == "Bermudan");
    // First exercise date from the 6 listed
    CHECK(ore_iso(instr.expiry_date) == "2035-09-23");
    REQUIRE(result.legs.size() == 2u);
    BOOST_LOG_SEV(lg, info) << "Swaption Bermudan forward-mapper test passed";
}

TEST_CASE("forward_swaption_leaves_dates_unset_without_a_schedule", tags) {
    auto lg(make_logger(test_suite));

    // The XSD makes ScheduleData optional, so a leg with no schedule is legal.
    // The instrument then records no dates rather than an invalid date that
    // renders as an empty element.
    using ores::platform::filesystem::file;
    std::string content = file::read_content(example_path("IR_Swaption_European.xml"));
    const std::string open_tag = "<ScheduleData>";
    const std::string close_tag = "</ScheduleData>";
    for (auto begin = content.find(open_tag); begin != std::string::npos;
         begin = content.find(open_tag)) {
        const auto end = content.find(close_tag, begin);
        REQUIRE(end != std::string::npos);
        content.erase(begin, end + close_tag.size() - begin);
    }
    REQUIRE(content.find(open_tag) == std::string::npos);

    portfolio p;
    ores::ore::domain::load_data(content, p);
    REQUIRE(!p.Trade.empty());

    const auto result = swap_instrument_mapper::forward_swaption(p.Trade.front());

    CHECK(!result.header.start_date.has_value());
    CHECK(!result.header.maturity_date.has_value());
    REQUIRE(!result.legs.empty());
}

// =============================================================================
// CallableSwap mapper tests
// =============================================================================

TEST_CASE("mapper_roundtrip_callable_swap_forward", tags) {
    auto lg(make_logger(test_suite));
    const auto t = load_trade("IR_Callable_Swap_Bermudan.xml", 0);

    const auto result = swap_instrument_mapper::forward_callable_swap(t);

    // Each exercise date is one call date row, in the document's order.
    REQUIRE(result.call_dates.size() == 6u);
    CHECK(ore_iso(result.call_dates[0].call_date) == "2035-09-30");
    CHECK(ore_iso(result.call_dates[1].call_date) == "2036-09-29");
    CHECK(ore_iso(result.call_dates[2].call_date) == "2037-09-29");
    CHECK(ore_iso(result.call_dates[3].call_date) == "2038-09-29");
    CHECK(ore_iso(result.call_dates[4].call_date) == "2039-09-30");
    CHECK(ore_iso(result.call_dates[5].call_date) == "2040-09-29");
    for (std::size_t i = 0; i < result.call_dates.size(); ++i)
        CHECK(result.call_dates[i].sequence_number == static_cast<int>(i) + 1);
    CHECK(!result.legs.empty());
    BOOST_LOG_SEV(lg, info) << "CallableSwap forward-mapper test passed, dates="
                            << result.call_dates.size();
}

TEST_CASE("mapper_roundtrip_callable_swap_reverse", tags) {
    auto lg(make_logger(test_suite));
    const auto t = load_trade("IR_Callable_Swap_Bermudan.xml", 0);
    const auto result = swap_instrument_mapper::forward_callable_swap(t);

    const auto reconstructed = swap_instrument_mapper::reverse_callable_swap(
        result.header,
        std::get<ores::trading::domain::callable_swap_instrument>(result.facts),
        result.legs,
        result.leg_amounts,
        result.leg_rates,
        result.call_dates);

    REQUIRE(reconstructed.CallableSwapData.operator bool());
    const auto& cd = *reconstructed.CallableSwapData;

    // The schedule rebuilds from the child rows, date for date and in order.
    REQUIRE(cd.OptionData.operator bool());
    REQUIRE(cd.OptionData->exerciseDatesGroup.operator bool());
    const auto& dates = cd.OptionData->exerciseDatesGroup->ExerciseDates->ExerciseDate;
    REQUIRE(dates.size() == result.call_dates.size());
    for (std::size_t i = 0; i < dates.size(); ++i)
        CHECK(std::string(dates[i]) == ore_iso(result.call_dates[i].call_date));

    // Legs round-trip
    CHECK(!cd.LegData.empty());
    CHECK(cd.LegData.size() == result.legs.size());
    BOOST_LOG_SEV(lg, info) << "CallableSwap reverse-mapper test passed";
}
