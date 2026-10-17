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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include "ores.logging/make_logger.hpp"
#include "ores.ore.core/xml/binding.hpp"
#include "ores.ore.core/xml/generated/calendar_adjustments.hpp"
#include "ores.ore.core/xml/generated/counterparty_information.hpp"
#include "ores.ore.core/xml/generated/currency_config.hpp"
#include "ores.ore.core/xml/generated/ibor_fallback_config.hpp"
#include <catch2/catch_test_macros.hpp>
#include <filesystem>
#include <fstream>
#include <string>
#include <vector>

namespace {

const std::string_view test_suite("ores.ore.tests");
const std::string tags("[ore][xml][binding]");

namespace fs = std::filesystem;
using namespace ores::logging;
using namespace ores::ore::xml;
using ores::ore::xml::generated::calendar_adjustments_document;
using ores::ore::xml::generated::calendaradjustment;
using ores::ore::xml::generated::currency_config;
using ores::ore::xml::generated::currency_config_document;
using ores::ore::xml::generated::ibor_fallback_config;
using ores::ore::xml::generated::counterparty_information;
using ores::ore::xml::generated::counterparty_information_document;
using ores::ore::xml::generated::ibor_fallback_config;
using ores::ore::xml::generated::ibor_fallback_config_document;

// The calendar family's records and tables are generated from the schema by
// projects/ores.ore/scripts/regenerate_xml_bindings.sh, and the generated names
// are the schema's names. Nothing about that family is written by hand here.

// The two shapes below have no generator mapping yet, so they are hand-written
// to exercise the engine directly: a residual field for content the schema does
// not name, and a repeated leaf of a numeric type.

struct notes {
    std::uint16_t present = 0;
    std::vector<residual_element> extras;
};

struct level_list {
    std::uint16_t present = 0;
    std::vector<double> levels;
};

constexpr auto hash_name(std::string_view name) -> std::uint32_t {
    std::uint32_t h = 2166136261u;
    for (const char c : name) {
        h ^= static_cast<std::uint8_t>(c);
        h *= 16777619u;
    }
    return h;
}

auto read_file(const fs::path& path) -> std::string {
    std::ifstream file(path, std::ios::binary);
    return std::string{std::istreambuf_iterator<char>(file), std::istreambuf_iterator<char>()};
}

const field notes_fields[] = {
    {"",
     0,
     field_kind::residual,
     offsetof(notes, extras),
     0,
     false,
     false,
     nullptr,
     nullptr,
     scalar_kind::text,
     nullptr},
};
const record_type notes_type{"Notes", notes_fields, 1};

const field level_list_fields[] = {
    {"Level",
     hash_name("Level"),
     field_kind::repeated_leaf,
     offsetof(level_list, levels),
     0,
     false,
     false,
     nullptr,
     nullptr,
     scalar_kind::real,
     nullptr},
};
const record_type level_list_type{"Levels", level_list_fields, 1};

}

TEST_CASE("content_the_schema_does_not_name_is_captured", tags) {
    auto lg(make_logger(test_suite));

    notes record;
    bind_error error;
    const document_type doc{"Notes", &notes_type};
    const bool bound =
        binder::bind(doc, "<Notes><Anything>hello</Anything></Notes>", &record, error);

    INFO(error.message);
    REQUIRE(bound);
    REQUIRE(record.extras.size() == 1);
    CHECK(record.extras[0].name == "Anything");
    CHECK(record.extras[0].text == "hello");
}

TEST_CASE("content_the_schema_does_not_name_and_cannot_represent_is_refused", tags) {
    auto lg(make_logger(test_suite));

    notes record;
    bind_error error;
    const document_type doc{"Notes", &notes_type};
    const bool bound =
        binder::bind(doc, "<Notes><Nested><Child/></Nested></Notes>", &record, error);

    CHECK_FALSE(bound);
    CHECK(error.message.find("children") != std::string::npos);
}

TEST_CASE("a_repeated_numeric_leaf_fills_a_vector", tags) {
    auto lg(make_logger(test_suite));

    level_list record;
    bind_error error;
    const document_type doc{"Levels", &level_list_type};
    const bool bound =
        binder::bind(doc, "<Levels><Level>1.5</Level><Level>2.5</Level></Levels>", &record, error);

    INFO(error.message);
    REQUIRE(bound);
    REQUIRE(record.levels.size() == 2);
    CHECK(record.levels[0] == 1.5);
    CHECK(record.levels[1] == 2.5);
}

TEST_CASE("a_nested_document_binds_into_one_record", tags) {
    auto lg(make_logger(test_suite));

    const std::string text =
        "<CalendarAdjustments>"
        "<Calendar name=\"TARGET\">"
        "<BaseCalendar>WeekendsOnly</BaseCalendar>"
        "<AdditionalHolidays><Date>2024-01-01</Date><Date>2024-12-25</Date></AdditionalHolidays>"
        "</Calendar>"
        "<Calendar name=\"Empty\"/>"
        "</CalendarAdjustments>";

    calendaradjustment record;
    bind_error error;
    const bool bound = binder::bind(calendar_adjustments_document(), text, &record, error);

    INFO(error.message);
    REQUIRE(bound);
    REQUIRE(record.calendar.size() == 2);
    CHECK(record.calendar[0].name == "TARGET");
    CHECK(record.calendar[0].base_calendar == "WeekendsOnly");
    CHECK(record.calendar[0].additional_holidays.date.size() == 2);
    CHECK(record.calendar[0].additional_holidays.date[0] == "2024-01-01");
    CHECK(record.calendar[0].additional_business_days.date.empty());

    // A child that is absent stays absent, and one that arrives sets its bit.
    CHECK((record.calendar[1].present & 0x2u) == 0u);
    CHECK((record.calendar[0].present & 0x2u) != 0u);
}

TEST_CASE("a_missing_required_attribute_is_refused", tags) {
    auto lg(make_logger(test_suite));

    calendaradjustment record;
    bind_error error;
    const bool bound = binder::bind(calendar_adjustments_document(),
                                    "<CalendarAdjustments><Calendar/></CalendarAdjustments>",
                                    &record,
                                    error);

    CHECK_FALSE(bound);
    CHECK(error.message.find("name") != std::string::npos);
}

TEST_CASE("an_unknown_element_is_refused_rather_than_dropped", tags) {
    auto lg(make_logger(test_suite));

    calendaradjustment record;
    bind_error error;
    const bool bound = binder::bind(calendar_adjustments_document(),
                                    "<CalendarAdjustments><Holiday/></CalendarAdjustments>",
                                    &record,
                                    error);

    CHECK_FALSE(bound);
    CHECK(error.message.find("Holiday") != std::string::npos);
}

TEST_CASE("every_calendar_adjustment_in_the_corpus_binds", tags) {
    auto lg(make_logger(test_suite));

    const auto root = fs::path(ORES_SOURCE_DIR) / "external" / "ore";
    REQUIRE(fs::is_directory(root));

    std::size_t files = 0;
    std::size_t calendars = 0;
    std::string report;

    for (const auto& entry : fs::recursive_directory_iterator(root)) {
        if (!entry.is_regular_file() || entry.path().filename() != "calendaradjustment.xml")
            continue;
        ++files;
        calendaradjustment record;
        bind_error error;
        if (!binder::bind(
                calendar_adjustments_document(), read_file(entry.path()), &record, error)) {
            report += entry.path().string() + ": " + error.message + "\\n";
            continue;
        }
        calendars += record.calendar.size();
    }

    INFO("files: " << files << ", calendars: " << calendars);
    INFO(report);
    CHECK(files > 0);
    CHECK(report.empty());
    CHECK(calendars > 40);
}

TEST_CASE("the_generated_currency_tables_bind_a_document", tags) {
    auto lg(make_logger(test_suite));

    // An enumeration, two integers and an xs:all, so the table exercises the
    // kinds the calendar family does not.
    const std::string text =
        "<CurrencyConfig>"
        "<Currency>"
        "<Name>US Dollar</Name><ISOCode>USD</ISOCode><NumericCode>840</NumericCode>"
        "<Symbol>$</Symbol><FractionSymbol>c</FractionSymbol>"
        "<FractionsPerUnit>100</FractionsPerUnit>"
        "<RoundingType>Closest</RoundingType><RoundingPrecision>2</RoundingPrecision>"
        "</Currency>"
        "</CurrencyConfig>";

    currency_config record;
    bind_error error;
    const bool bound = binder::bind(currency_config_document(), text, &record, error);

    INFO(error.message);
    REQUIRE(bound);
    REQUIRE(record.currency.size() == 1);
    CHECK(record.currency[0].iso_code == "USD");
    CHECK(record.currency[0].numeric_code == 840);
    CHECK(record.currency[0].rounding_precision == 2);
    CHECK(record.currency[0].rounding_type == 2);
    CHECK(record.currency[0].currency_type.empty());
}

TEST_CASE("every_currency_config_in_the_corpus_binds", tags) {
    auto lg(make_logger(test_suite));

    const auto root = fs::path(ORES_SOURCE_DIR) / "external" / "ore";
    REQUIRE(fs::is_directory(root));

    std::size_t files = 0;
    std::size_t currencies = 0;
    std::string report;

    for (const auto& entry : fs::recursive_directory_iterator(root)) {
        if (!entry.is_regular_file())
            continue;
        const auto name = entry.path().filename();
        if (name != "currencies.xml" && name != "currencyconfig.xml")
            continue;
        ++files;
        currency_config record;
        bind_error error;
        if (!binder::bind(currency_config_document(), read_file(entry.path()), &record, error)) {
            report += entry.path().string() + ": " + error.message + "\n";
            continue;
        }
        currencies += record.currency.size();
    }

    INFO("files: " << files << ", currencies: " << currencies);
    INFO(report);
    CHECK(files > 0);
    CHECK(report.empty());
    CHECK(currencies > 1000);
}

TEST_CASE("a_generated_money_field_keeps_the_documents_digits", tags) {
    auto lg(make_logger(test_suite));

    // Spread is xs:float in the schema and money in our model. The document
    // writes -0.00055099999999999995, which no double holds exactly, so a
    // binding that read it as a float would lose the value. It is a decimal
    // here and the digits survive.
    const std::string text =
        "<IborFallbackConfig><Fallbacks><Fallback>"
        "<IborIndex>CHF-LIBOR-SN</IborIndex><RfrIndex>CHF-SARON</RfrIndex>"
        "<Spread>-0.00055099999999999995</Spread><SwitchDate>2022-01-01</SwitchDate>"
        "</Fallback></Fallbacks></IborFallbackConfig>";

    ibor_fallback_config record;
    bind_error error;
    const bool bound = binder::bind(ibor_fallback_config_document(), text, &record, error);

    INFO(error.message);
    REQUIRE(bound);
    REQUIRE(record.fallbacks.fallback.size() == 1);
    CHECK(record.fallbacks.fallback[0].ibor_index == "CHF-LIBOR-SN");
    CHECK(record.fallbacks.fallback[0].switch_date == "2022-01-01");
    CHECK(record.fallbacks.fallback[0].spread.to_string() == "-0.00055099999999999995");
}

TEST_CASE("every_ibor_fallback_document_in_the_corpus_binds", tags) {
    auto lg(make_logger(test_suite));

    const auto root = fs::path(ORES_SOURCE_DIR) / "external" / "ore";
    REQUIRE(fs::is_directory(root));

    std::size_t files = 0;
    std::size_t fallbacks = 0;
    std::string report;

    for (const auto& entry : fs::recursive_directory_iterator(root)) {
        if (!entry.is_regular_file() || entry.path().filename() != "ibor_fallback.xml")
            continue;
        ++files;
        ibor_fallback_config record;
        bind_error error;
        if (!binder::bind(
                ibor_fallback_config_document(), read_file(entry.path()), &record, error)) {
            report += entry.path().string() + ": " + error.message + "\n";
            continue;
        }
        fallbacks += record.fallbacks.fallback.size();
    }

    INFO("files: " << files << ", fallbacks: " << fallbacks);
    INFO(report);
    CHECK(files > 0);
    CHECK(report.empty());
    CHECK(fallbacks > 10);
}

TEST_CASE("a_generated_element_with_text_and_attributes_binds_both", tags) {
    auto lg(make_logger(test_suite));

    // Correlation is xs:simpleContent: the element carries a value of its own
    // beside its attributes, which is the one shape a leaf cannot express.
    const std::string text =
        "<CounterpartyInformation>"
        "<Counterparties><Counterparty>"
        "<CounterpartyId>CPTY_A</CounterpartyId><CreditQuality>IG</CreditQuality>"
        "<BaCvaRiskWeight>0.05</BaCvaRiskWeight>"
        "</Counterparty></Counterparties>"
        "<Correlations>"
        "<Correlation cpty1=\"CPTY_A\" cpty2=\"CPTY_B\">0.75</Correlation>"
        "</Correlations>"
        "</CounterpartyInformation>";

    counterparty_information record;
    bind_error error;
    const bool bound = binder::bind(counterparty_information_document(), text, &record, error);

    INFO(error.message);
    REQUIRE(bound);
    REQUIRE(record.counterparties.counterparty.size() == 1);
    CHECK(record.counterparties.counterparty[0].counterparty_id == "CPTY_A");
    CHECK(record.counterparties.counterparty[0].credit_quality == 1);
    CHECK(record.counterparties.counterparty[0].ba_cva_risk_weight.to_string() == "0.05");

    REQUIRE(record.correlations.correlation.size() == 1);
    CHECK(record.correlations.correlation[0].cpty1 == "CPTY_A");
    CHECK(record.correlations.correlation[0].cpty2 == "CPTY_B");
    CHECK(record.correlations.correlation[0].value == 0.75);
}

TEST_CASE("every_counterparty_document_in_the_corpus_binds", tags) {
    auto lg(make_logger(test_suite));

    const auto root = fs::path(ORES_SOURCE_DIR) / "external" / "ore";
    REQUIRE(fs::is_directory(root));

    std::size_t files = 0;
    std::size_t parties = 0;
    std::string report;

    for (const auto& entry : fs::recursive_directory_iterator(root)) {
        if (!entry.is_regular_file() || entry.path().filename() != "counterparty.xml")
            continue;
        ++files;
        counterparty_information record;
        bind_error error;
        if (!binder::bind(
                counterparty_information_document(), read_file(entry.path()), &record, error)) {
            report += entry.path().string() + ": " + error.message + "\n";
            continue;
        }
        parties += record.counterparties.counterparty.size();
    }

    INFO("files: " << files << ", parties: " << parties);
    INFO(report);
    CHECK(files > 0);
    CHECK(report.empty());
    CHECK(parties > 3);
}
