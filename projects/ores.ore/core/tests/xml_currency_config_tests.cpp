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
#include "ores.ore.core/xml/document_kind.hpp"
#include "ores.ore.core/xml/exporter.hpp"
#include "ores.ore.core/xml/importer.hpp"
#include "ores.testing/project_root.hpp"
#include <catch2/catch_test_macros.hpp>
#include <filesystem>
#include <string>
#include <vector>

namespace {

const std::string_view test_suite("ores.ore.tests");
const std::string tags("[ore][xml]");

using namespace ores::logging;
using ores::refdata::domain::currency;

const std::string text = "<CurrencyConfig><Currency>"
                         "<Name>Japanese Yen</Name><ISOCode>JPY</ISOCode>"
                         "<NumericCode>392</NumericCode><Symbol>\u00a5</Symbol>"
                         "<FractionSymbol>sen</FractionSymbol>"
                         "<FractionsPerUnit>100</FractionsPerUnit>"
                         "<RoundingType>Closest</RoundingType>"
                         "<RoundingPrecision>0</RoundingPrecision>"
                         "<CurrencyType>Major</CurrencyType>"
                         "</Currency></CurrencyConfig>";

/**
 * @brief Imports, exports and imports again, comparing the entities.
 *
 * The whole path a user takes, and the fields the message must carry. The
 * export is read back by the codec rather than by the file system, so the
 * comparison is against what the table wrote.
 */
void require_round_trip(const std::vector<currency>& original) {
    const auto written = ores::ore::xml::exporter::export_currency_config(original);
    CHECK(written.contains("<CurrencyConfig>"));
    const auto reread = ores::ore::xml::importer::import_currency_config_text(written);

    REQUIRE(reread.size() == original.size());
    for (std::size_t i = 0; i < original.size(); ++i) {
        INFO("currency " << i);
        CHECK(reread[i].iso_code == original[i].iso_code);
        CHECK(reread[i].name == original[i].name);
        CHECK(reread[i].numeric_code == original[i].numeric_code);
        CHECK(reread[i].symbol == original[i].symbol);
        CHECK(reread[i].fraction_symbol == original[i].fraction_symbol);
        CHECK(reread[i].fractions_per_unit == original[i].fractions_per_unit);
        CHECK(reread[i].rounding_type == original[i].rounding_type);
        CHECK(reread[i].rounding_precision == original[i].rounding_precision);
        CHECK(reread[i].ore_currency_type == original[i].ore_currency_type);
        CHECK(reread[i].monetary_nature == original[i].monetary_nature);
    }
}

}

TEST_CASE("a_currency_states_its_own_fields", tags) {
    auto lg(make_logger(test_suite));

    const auto imported = ores::ore::xml::importer::import_currency_config_text(text);
    REQUIRE(imported.size() == 1);
    BOOST_LOG_SEV(lg, info) << "currencies: " << imported.size();

    CHECK(imported[0].iso_code == "JPY");
    CHECK(imported[0].name == "Japanese Yen");
    CHECK(imported[0].numeric_code == "392");
    CHECK(imported[0].fractions_per_unit == 100);
    CHECK(imported[0].rounding_type == "Closest");
    CHECK(imported[0].rounding_precision == 0);
    CHECK(imported[0].ore_currency_type == "Major");

    // ORE's token is carried verbatim and the nature is derived from it.
    CHECK(imported[0].monetary_nature == "fiat");

    require_round_trip(imported);
}

TEST_CASE("a_currency_absent_optional_fields_stay_absent", tags) {
    auto lg(make_logger(test_suite));

    const std::string minimal = "<CurrencyConfig><Currency>"
                                "<Name>Gold</Name><ISOCode>XAU</ISOCode>"
                                "<Symbol>oz</Symbol><FractionSymbol>g</FractionSymbol>"
                                "<FractionsPerUnit>1</FractionsPerUnit>"
                                "<RoundingType>Down</RoundingType>"
                                "<RoundingPrecision>0</RoundingPrecision>"
                                "<CurrencyType>Metal</CurrencyType>"
                                "</Currency></CurrencyConfig>";

    const auto imported = ores::ore::xml::importer::import_currency_config_text(minimal);
    REQUIRE(imported.size() == 1);
    CHECK(imported[0].numeric_code.empty());
    CHECK(imported[0].monetary_nature == "commodity");

    require_round_trip(imported);
}

TEST_CASE("every_currency_config_in_the_corpus_round_trips", tags) {
    auto lg(make_logger(test_suite));

    const auto root = ores::testing::project_root::resolve("external/ore/examples");
    REQUIRE(std::filesystem::is_directory(root));

    // The corpus names these documents two ways, so the root element decides,
    // through the function the import scanner shares.
    std::size_t files = 0;
    for (const auto& entry : std::filesystem::recursive_directory_iterator(root)) {
        if (!entry.is_regular_file() || entry.path().extension() != ".xml")
            continue;
        if (ores::ore::xml::detect_document_kind(entry.path()) !=
            ores::ore::xml::document_kind::currency_config)
            continue;
        ++files;
        INFO(entry.path().string());
        const auto imported = ores::ore::xml::importer::import_currency_config(entry.path());
        require_round_trip(imported);
    }
    BOOST_LOG_SEV(lg, info) << "currency configs: " << files;
    CHECK(files == 13);
}
