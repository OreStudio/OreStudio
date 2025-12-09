/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.cli/app/application.hpp"

#include <sstream>
#include <filesystem>
#include <catch2/catch_test_macros.hpp>
#include "ores.utility/log/make_logger.hpp"
#include "ores.testing/database_helper.hpp"
#include "ores.testing/test_database_manager.hpp"
#include "ores.cli/config/options.hpp"
#include "ores.cli/config/import_options.hpp"
#include "ores.risk/repository/currency_repository.hpp"

namespace {

const std::string_view test_suite("ores.cli.tests");
const std::string database_table("oresdb.currencies");
const std::string tags("[import]");

}

using namespace ores;
using namespace ores::cli;
using namespace ores::utility::log;

TEST_CASE("import_currencies_from_test_file", tags) {
    auto lg(make_logger(test_suite));

    ores::testing::database_helper h;
    h.truncate_table(database_table);

    const std::filesystem::path test_data_file = "../test_data/currencies/currencies_01.xml";

    BOOST_LOG_SEV(lg, debug) << "Importing currencies from: " << test_data_file;

    CHECK(std::filesystem::exists(test_data_file));

    config::import_options import_cfg;
    import_cfg.target_entity = config::entity::currencies;
    import_cfg.targets = {test_data_file};

    config::options opts;
    opts.importing = import_cfg;

    using testing::test_database_manager;
    opts.database = test_database_manager::make_database_options();
    BOOST_LOG_SEV(lg, debug) << "Options: " << opts;

    std::ostringstream os;
    app::application app(os, opts.database);

    app.run(opts);

    risk::repository::currency_repository repo;
    auto read_currencies = repo.read_latest(h.context());

    BOOST_LOG_SEV(lg, debug) << "Read " << read_currencies.size()
                             << " currencies from database";
    BOOST_LOG_SEV(lg, debug) << "Console output: " << os.str();

    for (const auto& ccy : read_currencies) {
        BOOST_LOG_SEV(lg, debug) << "Currency: " << ccy.iso_code
                                 << " - " << ccy.name;
    }

    CHECK(read_currencies.size() == 2);

    bool found_pgk = false;
    bool found_sos = false;

    for (const auto& ccy : read_currencies) {
        if (ccy.iso_code == "PGK") {
            found_pgk = true;
            CHECK(ccy.name == "Papua New Guinean kina");
            CHECK(ccy.numeric_code == "598");
            CHECK(ccy.fractions_per_unit == 100);
        }
        if (ccy.iso_code == "SOS") {
            found_sos = true;
            CHECK(ccy.name == "Somali shilling");
            CHECK(ccy.numeric_code == "706");
        }
    }

    CHECK(found_pgk);
    CHECK(found_sos);
}

TEST_CASE("import_currencies_from_multiple_files", tags) {
    auto lg(make_logger(test_suite));

    ores::testing::database_helper h;
    h.truncate_table(database_table);

    const std::vector<std::filesystem::path> test_files = {
        "../test_data/currencies/currencies_01.xml",
        "../test_data/currencies/currencies_41.xml"
    };

    BOOST_LOG_SEV(lg, debug) << "Importing currencies from multiple files.";

    config::import_options import_cfg;
    import_cfg.target_entity = config::entity::currencies;
    import_cfg.targets = test_files;

    config::options opts;
    opts.importing = import_cfg;

    using testing::test_database_manager;
    opts.database = test_database_manager::make_database_options();
    BOOST_LOG_SEV(lg, debug) << "Options: " << opts;

    std::ostringstream os;
    app::application app(os, opts.database);

    app.run(opts);

    risk::repository::currency_repository repo;
    auto read_currencies = repo.read_latest(h.context());

    BOOST_LOG_SEV(lg, debug) << "Total currencies in database: "
                             << read_currencies.size();
    BOOST_LOG_SEV(lg, debug) << "Console output: " << os.str();

    for (const auto& ccy : read_currencies)
        BOOST_LOG_SEV(lg, debug) << "Currency: " << ccy.iso_code;

    CHECK(read_currencies.size() >= 2);
}

TEST_CASE("import_and_query_specific_currency", tags) {
    auto lg(make_logger(test_suite));

    ores::testing::database_helper h;
    h.truncate_table(database_table);

    const std::filesystem::path test_data_file = "../test_data/currencies/currencies_01.xml";

    BOOST_LOG_SEV(lg, debug) << "Importing currencies.";

    config::import_options import_cfg;
    import_cfg.target_entity = config::entity::currencies;
    import_cfg.targets = {test_data_file};

    config::options opts;
    opts.importing = import_cfg;

    using testing::test_database_manager;
    opts.database = test_database_manager::make_database_options();
    BOOST_LOG_SEV(lg, debug) << "Options: " << opts;

    std::ostringstream os;
    app::application app(os, opts.database);

    app.run(opts);

    const std::string target_iso = "PGK";
    BOOST_LOG_SEV(lg, debug) << "Querying for currency: " << target_iso;

    risk::repository::currency_repository repo;
    auto pgk_currencies = repo.read_latest(h.context(), target_iso);

    BOOST_LOG_SEV(lg, debug) << "Found " << pgk_currencies.size()
                             << " currencies with ISO code " << target_iso;
    BOOST_LOG_SEV(lg, debug) << "Console output: " << os.str();

    REQUIRE(!pgk_currencies.empty());

    const auto& pgk = pgk_currencies[0];
    BOOST_LOG_SEV(lg, debug) << "Currency details: " << pgk.iso_code
                             << " - " << pgk.name;

    CHECK(pgk.iso_code == "PGK");
    CHECK(pgk.name == "Papua New Guinean kina");
    CHECK(pgk.symbol == "K");
}

TEST_CASE("import_currencies_with_empty_database", tags) {
    auto lg(make_logger(test_suite));

    ores::testing::database_helper h;
    h.truncate_table(database_table);

    BOOST_LOG_SEV(lg, info) << "Verifying empty database";

    risk::repository::currency_repository repo;
    auto initial_currencies = repo.read_latest(h.context());

    BOOST_LOG_SEV(lg, debug) << "Initial currency count: "
                             << initial_currencies.size();

    CHECK(initial_currencies.empty());

    const std::filesystem::path test_data_file = "../test_data/currencies/currencies_01.xml";

    config::import_options import_cfg;
    import_cfg.target_entity = config::entity::currencies;
    import_cfg.targets = {test_data_file};

    config::options opts;
    opts.importing = import_cfg;

    using testing::test_database_manager;
    opts.database = test_database_manager::make_database_options();
    BOOST_LOG_SEV(lg, debug) << "Options: " << opts;

    std::ostringstream os;
    app::application app(os, opts.database);

    app.run(opts);

    auto after_import = repo.read_latest(h.context());

    BOOST_LOG_SEV(lg, debug) << "After import currency count: "
                             << after_import.size();
    BOOST_LOG_SEV(lg, debug) << "Console output: " << os.str();

    CHECK(after_import.size() == 2);
}

TEST_CASE("import_currencies_verify_all_fields", tags) {
    auto lg(make_logger(test_suite));

    ores::testing::database_helper h;
    h.truncate_table(database_table);

    const std::filesystem::path test_data_file = "../test_data/currencies/currencies_01.xml";

    BOOST_LOG_SEV(lg, debug) << "Importing and verifying all currency fields.";

    config::import_options import_cfg;
    import_cfg.target_entity = config::entity::currencies;
    import_cfg.targets = {test_data_file};

    config::options opts;
    opts.importing = import_cfg;

    using testing::test_database_manager;
    opts.database = test_database_manager::make_database_options();
    BOOST_LOG_SEV(lg, debug) << "Options: " << opts;

    std::ostringstream os;
    app::application app(os, opts.database);

    app.run(opts);
    BOOST_LOG_SEV(lg, debug) << "Console output: " << os.str();

    risk::repository::currency_repository repo;
    auto pgk_currencies = repo.read_latest(h.context(), "PGK");
    REQUIRE(!pgk_currencies.empty());

    const auto& pgk = pgk_currencies[0];

    BOOST_LOG_SEV(lg, debug) << "Verifying PGK fields:";
    BOOST_LOG_SEV(lg, debug) << "  Name: " << pgk.name;
    BOOST_LOG_SEV(lg, debug) << "  ISO Code: " << pgk.iso_code;
    BOOST_LOG_SEV(lg, debug) << "  Numeric Code: " << pgk.numeric_code;
    BOOST_LOG_SEV(lg, debug) << "  Symbol: " << pgk.symbol;
    BOOST_LOG_SEV(lg, debug) << "  Fractions per unit: " << pgk.fractions_per_unit;
    BOOST_LOG_SEV(lg, debug) << "  Rounding precision: " << pgk.rounding_precision;

    CHECK(pgk.name == "Papua New Guinean kina");
    CHECK(pgk.iso_code == "PGK");
    CHECK(pgk.numeric_code == "598");
    CHECK(pgk.symbol == "K");
    CHECK(pgk.fractions_per_unit == 100);
    CHECK(pgk.rounding_precision == 2);
}

TEST_CASE("import_currencies_from_api_test_file", tags) {
    auto lg(make_logger(test_suite));

    ores::testing::database_helper h;
    h.truncate_table(database_table);

    const std::filesystem::path test_data_file = "../test_data/currencies/currencies_API.xml";

    if (!std::filesystem::exists(test_data_file)) {
        BOOST_LOG_SEV(lg, warn) << "API test file not found, skipping test";
        return;
    }

    BOOST_LOG_SEV(lg, debug) << "Importing from API test file";

    config::import_options import_cfg;
    import_cfg.target_entity = config::entity::currencies;
    import_cfg.targets = {test_data_file};

    config::options opts;
    opts.importing = import_cfg;

    using testing::test_database_manager;
    opts.database = test_database_manager::make_database_options();
    BOOST_LOG_SEV(lg, debug) << "Options: " << opts;

    std::ostringstream os;
    app::application app(os, opts.database);

    app.run(opts);
    BOOST_LOG_SEV(lg, debug) << "Console output: " << os.str();

    risk::repository::currency_repository repo;
    auto read_currencies = repo.read_latest(h.context());

    BOOST_LOG_SEV(lg, debug) << "Verified " << read_currencies.size()
                             << " currencies in database";

    for (const auto& ccy : read_currencies) {
        BOOST_LOG_SEV(lg, debug) << "Currency: " << ccy.iso_code
                                 << " - " << ccy.name;
    }

    CHECK(!read_currencies.empty());
}
