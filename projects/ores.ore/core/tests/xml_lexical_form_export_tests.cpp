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
#include "ores.ore.core/domain/domain.hpp"
#include "ores.platform/filesystem/file.hpp"
#include "ores.testing/project_root.hpp"
#include <catch2/catch_test_macros.hpp>
#include <string>

/**
 * @file xml_lexical_form_export_tests.cpp
 * @brief Proves the writer prints the digits a money leaf carries.
 *
 * ORE types a rate or an amount as xs:float. The binding keeps the digits the
 * document wrote beside the parsed float, and the writer prints them, so a
 * value leaves the way it came in. A leaf set in code has no digits and
 * prints as its float.
 */

namespace {

const std::string tags("[ore][xml][lexical][export]");

using ores::ore::domain::portfolio;
using ores::ore::domain::stresstesting;

std::string stress_document(const std::string& shift_size) {
    return "<StressTesting><StressTest id=\"one\"><FxSpots><FxSpot ccypair=\"USDEUR\">"
           "<ShiftType>Relative</ShiftType><ShiftSize>" +
           shift_size + "</ShiftSize></FxSpot></FxSpots></StressTest></StressTesting>";
}

std::string saved(const std::string& shift_size) {
    stresstesting document;
    ores::ore::domain::load_data(stress_document(shift_size), document);
    return ores::ore::domain::save_data(document);
}

} // namespace

TEST_CASE("a value the document wrote is written back as it was", tags) {
    CHECK(saved("0.0065").find("<ShiftSize>0.0065</ShiftSize>") != std::string::npos);
}

TEST_CASE("the trailing zeros of a document value survive the export", tags) {
    CHECK(saved("1000000.00").find("<ShiftSize>1000000.00</ShiftSize>") != std::string::npos);
}

TEST_CASE("a value more precise than a float is written as the document wrote it", tags) {
    CHECK(saved("0.123456789012").find("<ShiftSize>0.123456789012</ShiftSize>") !=
          std::string::npos);
}

TEST_CASE("a leaf set in code is written as its float", tags) {
    stresstesting document;
    ores::ore::domain::load_data(stress_document("0.0065"), document);
    auto& entry = document.StressTest.front().FxSpots->FxSpot.front().ShiftSize.front();
    entry.lexical("");

    CHECK(ores::ore::domain::save_data(document).find("<ShiftSize>0.00650000013</ShiftSize>") !=
          std::string::npos);
}

TEST_CASE("a credit money member written in a document is written back as it was", tags) {
    ores::ore::domain::portfolio p;
    ores::ore::domain::load_data(
        ores::platform::filesystem::file::read_content(ores::testing::project_root::resolve(
            "external/ore/examples/Products/Example_Trades/Credit_Index_CDS_Option.xml")),
        p);
    const auto text = ores::ore::domain::save_data(p);

    CHECK(text.find("<Strike>1.0</Strike>") != std::string::npos);
    CHECK(text.find("<UpfrontFee>0.0</UpfrontFee>") != std::string::npos);
}
