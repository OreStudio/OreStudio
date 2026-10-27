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
#include "ores.ore.core/xml/emitter.hpp"
#include "ores.ore.core/xml/generated/calendar_adjustments.hpp"
#include "ores.ore.core/xml/generated/conventions.hpp"
#include "ores.ore.core/xml/generated/counterparty_information.hpp"
#include "ores.ore.core/xml/generated/currency_config.hpp"
#include "ores.ore.core/xml/generated/ibor_fallback_config.hpp"
#include "ores.ore.core/xml/generated/netting_set_definitions.hpp"
#include "ores.ore.core/xml/generated/todays_market.hpp"
#include "test_suite.hpp"
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <filesystem>
#include <fstream>
#include <string>
#include <vector>

namespace {

namespace fs = std::filesystem;
using namespace ores::logging;
using namespace ores::ore::xml;
namespace cal = ores::ore::xml::generated::calendar_adjustments;
using cal::calendar_adjustments_document;
using cal::calendaradjustment;
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
using tm::todays_market_document;
using tm::todaysmarket;

auto read_file(const fs::path& path) -> std::string {
    std::ifstream file(path, std::ios::binary);
    return std::string(std::istreambuf_iterator<char>(file), std::istreambuf_iterator<char>());
}

/**
 * @brief Binds a document, writes it back and binds the writing.
 *
 * @return The emitted text and the second reading, so a caller can compare
 * either the text or the values.
 */
template <typename Record>
auto write_back(const document_type& doc,
                std::string_view text,
                Record& record) -> std::expected<std::string, std::string> {
    const auto bound = binder::bind(doc, text, &record);
    if (!bound)
        return std::unexpected("the document does not bind: " + bound.error().message);

    std::string written;
    const auto emitted = emitter::emit(doc, &record, written);
    if (!emitted)
        return std::unexpected("the record does not write: " + emitted.error().message);

    Record reread{};
    const auto rebound = binder::bind(doc, written, &reread);
    if (!rebound)
        return std::unexpected("the writing does not bind: " + rebound.error().message);

    std::string again;
    const auto reemitted = emitter::emit(doc, &reread, again);
    if (!reemitted)
        return std::unexpected("the second record does not write: " + reemitted.error().message);
    if (again != written)
        return std::unexpected("the writing is not stable: the second writing differs");
    return written;
}

/**
 * @brief Writes back every corpus document of one family.
 *
 * A document counts as written back when it binds, writes, binds again and
 * the second writing is the same text. The first writing is left in the
 * round-trip tree for a validator to read.
 */
template <typename Record>
auto sweep_family(const fs::path& root,
                  const fs::path& output,
                  std::string_view family,
                  const document_type& doc,
                  const std::vector<std::string_view>& names,
                  std::size_t& documents,
                  std::string& report) -> std::size_t {
    std::size_t failures = 0;
    std::size_t index = 0;
    for (const auto& entry : fs::recursive_directory_iterator(root)) {
        if (!entry.is_regular_file())
            continue;
        const auto name = entry.path().filename().string();
        if (std::find(names.begin(), names.end(), name) == names.end())
            continue;
        ++documents;

        Record record{};
        const auto written = write_back(doc, read_file(entry.path()), record);
        if (!written) {
            ++failures;
            report += entry.path().string() + ": " + written.error() + "\n";
            continue;
        }

        const auto target = output / std::string(family) / (std::to_string(index++) + "-" + name);
        std::error_code ignored;
        fs::create_directories(target.parent_path(), ignored);
        std::ofstream file(target, std::ios::binary);
        file << *written;
        if (!file) {
            ++failures;
            report += "could not write " + target.string() + "\n";
        }
    }
    return failures;
}

}

TEST_CASE("a_bound_document_writes_back_in_schema_order", "[ore][xml][emitter]") {
    auto lg(make_logger(test_suite));

    const std::string text = "<CalendarAdjustments>"
                             "<Calendar name=\"TARGET\">"
                             "<BaseCalendar>WeekendsOnly</BaseCalendar>"
                             "<AdditionalHolidays><Date>2024-01-01</Date></AdditionalHolidays>"
                             "</Calendar>"
                             "<Calendar name=\"Empty\"/>"
                             "</CalendarAdjustments>";

    calendaradjustment record;
    const auto written = write_back(calendar_adjustments_document(), text, record);
    REQUIRE(written);

    const std::string expected = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
                                 "<CalendarAdjustments>\n"
                                 "  <Calendar name=\"TARGET\">\n"
                                 "    <BaseCalendar>WeekendsOnly</BaseCalendar>\n"
                                 "    <AdditionalHolidays>\n"
                                 "      <Date>2024-01-01</Date>\n"
                                 "    </AdditionalHolidays>\n"
                                 "  </Calendar>\n"
                                 "  <Calendar name=\"Empty\"/>\n"
                                 "</CalendarAdjustments>\n";
    CHECK(*written == expected);
}

TEST_CASE("a_repeated_choice_member_writes_one_element_per_value", "[ore][xml][emitter]") {
    auto lg(make_logger(test_suite));

    // The schema states Conventions as one choice that repeats, so the same
    // member may appear any number of times. A table that read the member's
    // own maxOccurs would keep one record and merge the values into it.
    const std::string text = "<Conventions>"
                             "<Deposit><Id>USD-DEPOSIT</Id><IndexBased>true</IndexBased>"
                             "<Index>USD-LIBOR</Index></Deposit>"
                             "<Deposit><Id>EUR-DEPOSIT</Id><IndexBased>true</IndexBased>"
                             "<Index>EUR-EURIBOR</Index></Deposit>"
                             "<Swap><Id>USD-SWAP</Id><FixedFrequency>Annual</FixedFrequency>"
                             "<FixedDayCounter>ACT/360</FixedDayCounter>"
                             "<Index>USD-LIBOR-3M</Index></Swap>"
                             "</Conventions>";

    cnv::conventions record;
    const auto written = write_back(cnv::conventions_document(), text, record);
    const std::string reason = written ? std::string{} : written.error();
    INFO(reason);
    REQUIRE(written);

    REQUIRE(record.deposit.size() == 2);
    CHECK(record.deposit[0].id == "USD-DEPOSIT");
    CHECK(record.deposit[1].id == "EUR-DEPOSIT");
    REQUIRE(record.swap.size() == 1);

    std::size_t deposits = 0;
    for (std::size_t at = written->find("<Deposit>"); at != std::string::npos;
         at = written->find("<Deposit>", at + 1))
        ++deposits;
    CHECK(deposits == 2);
}

TEST_CASE("text_that_needs_escaping_round_trips_unchanged", "[ore][xml][emitter]") {
    auto lg(make_logger(test_suite));

    // A calendar name is an attribute; the base calendar is element text. Both
    // carry every character the writer must escape, and a newline that a
    // parser would otherwise normalize away inside an attribute.
    const std::string text = "<CalendarAdjustments>"
                             "<Calendar name=\"a&amp;b&lt;c&gt;d&quot;e&#10;f\">"
                             "<BaseCalendar>x &lt; y &amp;&amp; z &gt; w</BaseCalendar>"
                             "</Calendar>"
                             "</CalendarAdjustments>";

    calendaradjustment record;
    const auto written = write_back(calendar_adjustments_document(), text, record);
    REQUIRE(written);

    CHECK(record.calendar[0].name == "a&b<c>d\"e\nf");
    CHECK(record.calendar[0].base_calendar == "x < y && z > w");
    CHECK(written->find("a&amp;b&lt;c&gt;d&quot;e&#10;f") != std::string::npos);
    CHECK(written->find("x &lt; y &amp;&amp; z &gt; w") != std::string::npos);
}

TEST_CASE("a_decimal_writes_as_plain_text_and_never_as_an_exponent", "[ore][xml][emitter]") {
    auto lg(make_logger(test_suite));

    // Spread is money in our model and xs:float in the schema. The document
    // states it as an exponent, so the writer is the one that has to put the
    // digits in plain form.
    const std::string text = "<IborFallbackConfig><Fallbacks><Fallback>"
                             "<IborIndex>CHF-LIBOR-SN</IborIndex><RfrIndex>CHF-SARON</RfrIndex>"
                             "<Spread>1e-10</Spread><SwitchDate>2022-01-01</SwitchDate>"
                             "</Fallback></Fallbacks></IborFallbackConfig>";

    ibor_fallback_config record;
    const auto written = write_back(ibor_fallback_config_document(), text, record);
    REQUIRE(written);

    CHECK(record.fallbacks.fallback[0].spread.to_string() == "0.0000000001");

    const auto open = written->find("<Spread>");
    const auto close = written->find("</Spread>");
    REQUIRE(open != std::string::npos);
    REQUIRE(close != std::string::npos);
    CHECK(written->substr(open + 8, close - (open + 8)) == "0.0000000001");
}

TEST_CASE("a_required_element_that_is_absent_is_refused", "[ore][xml][emitter]") {
    auto lg(make_logger(test_suite));

    // A record built by hand rather than bound: the Currency element arrived
    // but none of its required children did, so the writing would not validate
    // and must not be made.
    currency_config record;
    record.present = 1u;
    record.currency.emplace_back();
    std::string written;
    const auto emitted = emitter::emit(currency_config_document(), &record, written);

    REQUIRE_FALSE(emitted);
    CHECK(emitted.error().message.find("Name") != std::string::npos);
    CHECK(emitted.error().message.find("required element") != std::string::npos);
    CHECK(written.empty());
}

TEST_CASE("an_enumeration_index_past_its_spellings_is_refused", "[ore][xml][emitter]") {
    auto lg(make_logger(test_suite));

    const std::string text = "<CurrencyConfig><Currency>"
                             "<Name>US Dollar</Name><ISOCode>USD</ISOCode>"
                             "<NumericCode>840</NumericCode><Symbol>$</Symbol>"
                             "<FractionSymbol>c</FractionSymbol>"
                             "<FractionsPerUnit>100</FractionsPerUnit>"
                             "<RoundingType>Closest</RoundingType>"
                             "<RoundingPrecision>2</RoundingPrecision>"
                             "</Currency></CurrencyConfig>";

    currency_config record;
    REQUIRE(binder::bind(currency_config_document(), text, &record));

    record.currency[0].rounding_type = 900;
    std::string written;
    const auto emitted = emitter::emit(currency_config_document(), &record, written);

    REQUIRE_FALSE(emitted);
    CHECK(emitted.error().message.find("spelling") != std::string::npos);
}

TEST_CASE("every_generated_family_writes_back_every_document_in_the_corpus", "[ore][xml][emitter]") {
    auto lg(make_logger(test_suite));

    const auto root = fs::path(ORES_SOURCE_DIR) / "external" / "ore";
    REQUIRE(fs::is_directory(root));

    const auto output = fs::path(ORES_XML_ROUNDTRIP_DIR) / "emitted";
    std::error_code ignored;
    fs::remove_all(output, ignored);

    std::size_t documents = 0;
    std::string report;
    std::size_t failures = 0;

    failures += sweep_family<calendaradjustment>(root,
                                                 output,
                                                 "calendar_adjustments",
                                                 calendar_adjustments_document(),
                                                 {"calendaradjustment.xml"},
                                                 documents,
                                                 report);
    failures += sweep_family<currency_config>(root,
                                              output,
                                              "currency_config",
                                              currency_config_document(),
                                              {"currencies.xml", "currencyconfig.xml"},
                                              documents,
                                              report);
    failures += sweep_family<ibor_fallback_config>(root,
                                                   output,
                                                   "ibor_fallback_config",
                                                   ibor_fallback_config_document(),
                                                   {"ibor_fallback.xml"},
                                                   documents,
                                                   report);
    failures += sweep_family<counterparty_information>(root,
                                                       output,
                                                       "counterparty_information",
                                                       counterparty_information_document(),
                                                       {"counterparty.xml"},
                                                       documents,
                                                       report);
    failures += sweep_family<todaysmarket>(root,
                                           output,
                                           "todays_market",
                                           todays_market_document(),
                                           {"todaysmarket.xml"},
                                           documents,
                                           report);
    failures += sweep_family<nsd::nettingsetdefinitions>(root,
                                                         output,
                                                         "netting_set_definitions",
                                                         nsd::netting_set_definitions_document(),
                                                         {"netting.xml"},
                                                         documents,
                                                         report);
    failures += sweep_family<cnv::conventions>(root,
                                               output,
                                               "conventions",
                                               cnv::conventions_document(),
                                               {"conventions.xml"},
                                               documents,
                                               report);

    INFO("documents: " << documents);
    INFO(report);
    CHECK(documents > 200);
    CHECK(failures == 0);
}
