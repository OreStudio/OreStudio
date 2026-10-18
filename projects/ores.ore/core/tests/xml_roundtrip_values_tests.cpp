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
#include "ores.ore.core/xml/document.hpp"
#include "ores.ore.core/xml/emitter.hpp"
#include "ores.ore.core/xml/generated/calendar_adjustments.hpp"
#include "ores.ore.core/xml/generated/conventions.hpp"
#include "ores.ore.core/xml/generated/counterparty_information.hpp"
#include "ores.ore.core/xml/generated/currency_config.hpp"
#include "ores.ore.core/xml/generated/curve_configuration.hpp"
#include "ores.ore.core/xml/generated/ibor_fallback_config.hpp"
#include "ores.ore.core/xml/generated/netting_set_definitions.hpp"
#include "ores.ore.core/xml/generated/stress_testing.hpp"
#include "ores.ore.core/xml/generated/todays_market.hpp"
#include "ores.ore.core/xml/record_diff.hpp"
#include "ores.ore.core/xml/value_fingerprint.hpp"
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <filesystem>
#include <fstream>
#include <string>
#include <vector>

namespace {

const std::string_view test_suite("ores.ore.tests");
const std::string tags("[ore][xml][roundtrip-values]");

namespace fs = std::filesystem;
using namespace ores::logging;
using namespace ores::ore::xml;
namespace cal = ores::ore::xml::generated::calendar_adjustments;
using cal::calendar_adjustments_document;
using cal::calendaradjustment;
namespace cc = ores::ore::xml::generated::curve_configuration;
namespace cur = ores::ore::xml::generated::currency_config;
using cur::currency_config;
using cur::currency_config_document;
namespace ibor = ores::ore::xml::generated::ibor_fallback_config;
using ibor::ibor_fallback_config;
using ibor::ibor_fallback_config_document;
namespace cnv = ores::ore::xml::generated::conventions;
namespace cp = ores::ore::xml::generated::counterparty_information;
using cp::counterparty_information;
using cp::counterparty_information_document;
namespace nsd = ores::ore::xml::generated::netting_set_definitions;
namespace tm = ores::ore::xml::generated::todays_market;
namespace st = ores::ore::xml::generated::stress_testing;
using tm::todays_market_document;
using tm::todaysmarket;

auto read_file(const fs::path& path) -> std::string {
    std::ifstream file(path, std::ios::binary);
    return std::string(std::istreambuf_iterator<char>(file), std::istreambuf_iterator<char>());
}

/// The root element of a document, without reading it through a binding.
auto root_element(const std::string& text) -> std::string {
    std::size_t at = 0;
    while ((at = text.find('<', at)) != std::string::npos) {
        if (at + 1 < text.size() && (text[at + 1] == '?' || text[at + 1] == '!')) {
            at = text.find('>', at);
            if (at == std::string::npos)
                return {};
            ++at;
            continue;
        }
        const auto end = text.find_first_of(" \t\r\n>/", at + 1);
        if (end == std::string::npos)
            return {};
        return text.substr(at + 1, end - at - 1);
    }
    return {};
}

/**
 * @brief The first value two fingerprints disagree about.
 */
auto fingerprint_difference(const std::vector<value_entry>& left,
                            const std::vector<value_entry>& right) -> std::optional<std::string> {
    for (std::size_t at = 0; at < std::min(left.size(), right.size()); ++at) {
        if (left[at].path != right[at].path)
            return "the source states '" + left[at].path + "' where the writing states '" +
                   right[at].path + "'";
        if (left[at].value != right[at].value)
            return left[at].path + ": '" + left[at].value + "' became '" + right[at].value + "'";
    }
    if (left.size() != right.size()) {
        const auto& extra = left.size() > right.size() ? left : right;
        const auto side = left.size() > right.size() ? "the source" : "the writing";
        return side + std::string(" alone states '") +
               extra[std::min(left.size(), right.size())].path + "'";
    }
    return {};
}

/**
 * @brief Binds, writes, binds the writing, and compares the two records.
 *
 * This is the round trip that matters: the values that came out must be the
 * values that went in. The written text is not compared, because the writing
 * is allowed to order and spell things its own way; the records are.
 */
template <typename Record>
auto round_trip_values(const document_type& doc, std::string_view text, std::string& report)
    -> bool {
    Record first{};
    const auto bound = binder::bind(doc, text, &first);
    if (!bound) {
        report = "the document does not bind: " + bound.error().message;
        return false;
    }

    std::string written;
    const auto emitted = emitter::emit(doc, &first, written);
    if (!emitted) {
        report = "the record does not write: " + emitted.error().message;
        return false;
    }

    Record second{};
    const auto rebound = binder::bind(doc, written, &second);
    if (!rebound) {
        report = "the writing does not bind: " + rebound.error().message;
        return false;
    }

    const auto difference = compare_records(*doc.root, &first, &second);
    if (difference) {
        report = difference->path + ": " + difference->detail;
        return false;
    }

    // The two records agreeing only proves the writing does not lose what the
    // reading kept. The document layer reads both documents without a schema,
    // so a value the reading dropped is still in the source and absent from
    // the writing, and shows up here.
    const auto source = document::read(text);
    if (!source) {
        report = "the source does not read: " + source.error().message;
        return false;
    }
    const auto written_document = document::read(written);
    if (!written_document) {
        report = "the writing does not read: " + written_document.error().message;
        return false;
    }
    if (const auto lost = fingerprint_difference(value_fingerprint(*source),
                                                 value_fingerprint(*written_document))) {
        report = *lost;
        return false;
    }
    return true;
}

/**
 * @brief Proves the round trip over every corpus document of one family.
 *
 * A family is found by the prefix of its file names, because the corpus names
 * the same document differently per example (curveconfig.xml, curveconfig_sabr.xml)
 * and a list of exact names would stop covering a file the corpus adds.
 */
template <typename Record>
auto sweep_values(const fs::path& root,
                  std::string_view family,
                  const document_type& doc,
                  const std::vector<std::string_view>& prefixes,
                  std::size_t& documents,
                  std::string& report,
                  std::size_t* swept = nullptr,
                  std::string_view root_element_name = {}) -> std::size_t {
    std::size_t failures = 0;
    if (swept != nullptr)
        *swept = 0;
    for (const auto& entry : fs::recursive_directory_iterator(root)) {
        if (!entry.is_regular_file())
            continue;
        const auto name = entry.path().filename().string();
        if (!name.ends_with(".xml"))
            continue;
        if (root_element_name.empty()) {
            if (std::none_of(prefixes.begin(), prefixes.end(), [&name](std::string_view prefix) {
                    return name.starts_with(prefix);
                }))
                continue;
        } else if (root_element(read_file(entry.path())) != root_element_name) {
            // The family is found by the root element, so a document under a
            // name no prefix anticipates still counts.
            continue;
        }
        ++documents;
        if (swept != nullptr)
            ++*swept;

        std::string reason;
        if (!round_trip_values<Record>(doc, read_file(entry.path()), reason)) {
            ++failures;
            report += entry.path().string() + " (" + std::string(family) + "): " + reason + "\n";
        }
    }
    return failures;
}

}

TEST_CASE("a_documents_values_survive_the_writing", tags) {
    auto lg(make_logger(test_suite));

    // Every value here is a case the reading has to keep: a decimal the
    // schema calls a float, an attribute, a repeated child, an enumeration,
    // and a value that needs escaping.
    const std::string text = "<CalendarAdjustments>"
                             "<Calendar name=\"a&amp;b\"><BaseCalendar>x &lt; y</BaseCalendar>"
                             "<AdditionalHolidays><Date>2024-01-01</Date><Date>2024-12-25</Date>"
                             "</AdditionalHolidays></Calendar>"
                             "</CalendarAdjustments>";

    std::string reason;
    CHECK(round_trip_values<calendaradjustment>(calendar_adjustments_document(), text, reason));
    CHECK(reason.empty());

    const std::string fallback = "<IborFallbackConfig><Fallbacks><Fallback>"
                                 "<IborIndex>CHF-LIBOR-SN</IborIndex>"
                                 "<RfrIndex>CHF-SARON</RfrIndex>"
                                 "<Spread>-0.00055099999999999995</Spread>"
                                 "<SwitchDate>2022-01-01</SwitchDate>"
                                 "</Fallback></Fallbacks></IborFallbackConfig>";
    CHECK(
        round_trip_values<ibor_fallback_config>(ibor_fallback_config_document(), fallback, reason));
}

TEST_CASE("the_walk_names_the_value_that_changed", tags) {
    auto lg(make_logger(test_suite));

    const std::string text = "<CalendarAdjustments>"
                             "<Calendar name=\"TARGET\">"
                             "<AdditionalHolidays><Date>2024-01-01</Date></AdditionalHolidays>"
                             "</Calendar></CalendarAdjustments>";

    calendaradjustment record{};
    REQUIRE(binder::bind(calendar_adjustments_document(), text, &record));

    calendaradjustment changed = record;
    changed.calendar[0].additional_holidays.date[0] = "2024-02-02";

    const auto difference =
        compare_records(*calendar_adjustments_document().root, &record, &changed);
    REQUIRE(difference);
    CHECK(difference->path == "calendaradjustment.Calendar[0].AdditionalHolidays.Date[0]");
    CHECK(difference->detail == "'2024-01-01' became '2024-02-02'");

    // A field that disappears is named too, rather than compared as an empty
    // value.
    calendaradjustment cleared = record;
    cleared.calendar[0].present = 0;
    const auto lost = compare_records(*calendar_adjustments_document().root, &record, &cleared);
    REQUIRE(lost);
    CHECK(lost->path == "calendaradjustment.Calendar[0]");
    CHECK(lost->detail == "name was lost");
}

TEST_CASE("every_generated_family_keeps_its_values_across_the_writing", tags) {
    auto lg(make_logger(test_suite));

    const auto root = fs::path(ORES_SOURCE_DIR) / "external" / "ore";
    REQUIRE(fs::is_directory(root));

    std::size_t documents = 0;
    std::string report;
    std::size_t failures = 0;

    failures += sweep_values<calendaradjustment>(root,
                                                 "calendar_adjustments",
                                                 calendar_adjustments_document(),
                                                 {"calendaradjustment.xml"},
                                                 documents,
                                                 report);
    failures += sweep_values<currency_config>(root,
                                              "currency_config",
                                              currency_config_document(),
                                              {"currencies.xml", "currencyconfig.xml"},
                                              documents,
                                              report);
    // The corpus names this family's documents curveconfig*.xml, and every
    // one of them is counted, because the census says the harness proves the
    // whole family. A rename or a document that stops matching is a failure
    // here rather than a quieter sweep.
    std::size_t curve_configurations = 0;
    failures += sweep_values<cc::curveconfiguration>(root,
                                                     "curve_configuration",
                                                     cc::curve_configuration_document(),
                                                     {"curveconfig"},
                                                     documents,
                                                     report,
                                                     &curve_configurations);
    INFO("curve configurations: " << curve_configurations);
    CHECK(curve_configurations == 89);
    std::size_t stress_tests = 0;
    failures += sweep_values<st::stresstesting>(root,
                                                "stress_testing",
                                                st::stress_testing_document(),
                                                {},
                                                documents,
                                                report,
                                                &stress_tests,
                                                "StressTesting");
    INFO("stress tests: " << stress_tests);
    CHECK(stress_tests == 19);
    failures += sweep_values<ibor_fallback_config>(root,
                                                   "ibor_fallback_config",
                                                   ibor_fallback_config_document(),
                                                   {"ibor_fallback.xml"},
                                                   documents,
                                                   report);
    failures += sweep_values<counterparty_information>(root,
                                                       "counterparty_information",
                                                       counterparty_information_document(),
                                                       {"counterparty.xml"},
                                                       documents,
                                                       report);
    failures += sweep_values<todaysmarket>(
        root, "todays_market", todays_market_document(), {"todaysmarket.xml"}, documents, report);
    failures += sweep_values<nsd::nettingsetdefinitions>(root,
                                                         "netting_set_definitions",
                                                         nsd::netting_set_definitions_document(),
                                                         {"netting.xml"},
                                                         documents,
                                                         report);
    failures += sweep_values<cnv::conventions>(
        root, "conventions", cnv::conventions_document(), {"conventions.xml"}, documents, report);

    INFO("documents: " << documents);
    INFO(report);
    CHECK(documents > 200);
    CHECK(failures == 0);
}
