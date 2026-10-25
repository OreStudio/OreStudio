/** -*- mode: c++; tab-width: 4; indent-tabs-mode: nil -*-
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
#include <catch2/catch_test_macros.hpp>
#include <boost/iostreams/device/array.hpp>
#include <boost/iostreams/stream.hpp>
#include <cstddef>
#include <fstream>
#include <iterator>
#include <string>
#include <vector>
#include "ores.platform/filesystem/scoped_temp_directory.hpp"
#include "ores.platform/filesystem/scoped_temp_file.hpp"
#include "ores.reporting.core/service/run_results.hpp"
#include "ores.storage.core/filesystem/archiver.hpp"

namespace {

constexpr auto tags = "[reporting][service][results]";

std::vector<std::byte> read_whole(const std::filesystem::path& path) {
    std::ifstream stream(path, std::ios::binary);
    const std::vector<char> bytes((std::istreambuf_iterator<char>(stream)),
                                  std::istreambuf_iterator<char>());
    std::vector<std::byte> out(bytes.size());
    std::transform(bytes.begin(), bytes.end(), out.begin(), [](char c) {
        return static_cast<std::byte>(c);
    });
    return out;
}

/// An archive holding one file at the path the engine writes it to.
std::vector<std::byte> make_archive(const std::string& relative_path, const std::string& content) {
    const ores::platform::filesystem::scoped_temp_directory staged;
    const auto file = staged.path() / relative_path;
    std::filesystem::create_directories(file.parent_path());
    std::ofstream stream(file, std::ios::binary);
    stream << content;
    stream.close();

    const ores::platform::filesystem::scoped_temp_file archive;
    ores::storage::filesystem::archiver::pack(staged.path(), archive.path());
    return read_whole(archive.path());
}

const auto results_csv =
    "TradeId,TradeType,Maturity,MaturityTime,NPV,NpvCurrency,NPV(Base),BaseCurrency,Notional,"
    "NotionalCurrency,Notional(Base),NettingSet,CounterParty\n"
    "221b1606-f5ce-5854-bcff-2338e3cffd54,Bond,2030-05-05,1.0,1234.5,GBP,1234.5,GBP,1000000,GBP,"
    "1000000,NS-1,\"Northwind Capital, Ltd\"\n"
    "42a2f8ff-8bb8-5ca2-bec7-4e83c3afcff3,Bond,2031-05-05,1.0,-250.25,GBP,-250.25,GBP,500000,GBP,"
    "500000,NS-1,Acme Corporation Plc\n";

}

TEST_CASE("a run's results are read by column name, with the counterparty's comma kept", tags) {
    using ores::reporting::service::read_run_results_from_archive;

    const auto read = read_run_results_from_archive(make_archive("Output/npv.csv", results_csv));

    REQUIRE(read.ok());
    REQUIRE(read.rows.size() == 2);
    CHECK(read.rows[0].trade_id == "221b1606-f5ce-5854-bcff-2338e3cffd54");
    CHECK(read.rows[0].trade_type == "Bond");
    CHECK(read.rows[0].npv == "1234.5");
    CHECK(read.rows[0].npv_base == "1234.5");
    CHECK(read.rows[0].notional == "1000000");
    CHECK(read.rows[0].counterparty == "Northwind Capital, Ltd");
    CHECK(read.rows[1].npv == "-250.25");
}

TEST_CASE("a run whose archive holds no npv.csv says so rather than reading nothing", tags) {
    using ores::reporting::service::read_run_results_from_archive;

    const auto read = read_run_results_from_archive(make_archive("Output/log.txt", "nothing here"));

    CHECK_FALSE(read.ok());
    CHECK(read.error.find("npv.csv") != std::string::npos);
    CHECK(read.rows.empty());
}

TEST_CASE("a columns file without the figures says which column it lacks", tags) {
    using ores::reporting::service::read_run_results_from_archive;

    const auto read = read_run_results_from_archive(make_archive("Output/npv.csv", "TradeType\nBond\n"));

    CHECK_FALSE(read.ok());
    CHECK(read.error.find("TradeId") != std::string::npos);
}
