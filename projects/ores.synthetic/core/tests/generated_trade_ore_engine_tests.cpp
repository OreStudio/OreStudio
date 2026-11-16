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
#include "ores.ore.core/domain/script_library_projection.hpp"
#include "ores.ore.core/log/engine_log.hpp"
#include "ores.platform/environment/environment.hpp"
#include "ores.platform/filesystem/file.hpp"
#include "ores.platform/process/pid.hpp"
#include "ores.testing/project_root.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.trading.core/repository/script_calibration_repository.hpp"
#include "ores.trading.core/repository/script_calibration_strike_repository.hpp"
#include "ores.trading.core/repository/script_coarsening_repository.hpp"
#include "ores.trading.core/repository/script_repository.hpp"
#include "ores.trading.core/repository/script_result_repository.hpp"
#include "ores.trading.core/repository/script_variant_repository.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/asio/io_context.hpp>
#include <boost/process/v2/environment.hpp>
#include <boost/process/v2/process.hpp>
#include <boost/process/v2/start_dir.hpp>
#include <boost/process/v2/stdio.hpp>
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <filesystem>
#include <iterator>
#include <regex>
#include <set>
#include <sstream>
#include <string>
#include <string_view>
#include <vector>

/**
 * @file generated_trade_ore_engine_tests.cpp
 * @brief Runs the ORE engine on the recorded generated trades.
 *
 * Every recorded portfolio goes into one portfolio, which ORE prices with the
 * configuration of its Products example. Each error ORE raises reading or
 * building a trade must be named in the known list, and each line of the list
 * must still be raised, so a family that fixes its trades removes its lines.
 *
 * ORES_ORE_EXECUTABLE names the engine. ORES_ORE_LIBRARY_PATH, when set, is
 * where the engine finds its shared libraries. The test runs only on Linux,
 * because the ORE engine is built only for Linux; elsewhere it skips.
 */

#if defined(__linux__)
namespace {

namespace fs = std::filesystem;

const std::string recorded_dir = "projects/ores.synthetic/core/tests/data/generated_trades";
const std::string known_errors_file =
    "projects/ores.synthetic/core/tests/data/generated_trades_known_ore_errors.txt";
const std::string example_input = "external/ore/examples/Products/Input";

std::string text_of(const fs::path& path) {
    return ores::platform::filesystem::file::read_content(path);
}

/// Every recorded trade, each renamed for its file so the merged ids are unique.
std::string merged_portfolio() {
    std::vector<fs::path> files;
    for (const auto& entry :
         fs::directory_iterator(ores::testing::project_root::resolve(recorded_dir)))
        if (entry.path().extension() == ".xml")
            files.push_back(entry.path());
    std::sort(files.begin(), files.end());

    const std::regex trade_pattern(R"(<Trade\b[\s\S]*?</Trade>)");
    const std::regex id_pattern(R"re(^<Trade\b[^>]*?\bid="[^"]*")re");
    std::string out = "<?xml version=\"1.0\"?>\n<Portfolio>\n";
    for (const auto& file : files) {
        const auto text = text_of(file);
        int count = 0;
        for (auto it = std::sregex_iterator(text.begin(), text.end(), trade_pattern);
             it != std::sregex_iterator();
             ++it) {
            const auto id = file.stem().string() + "-" + std::to_string(++count);
            const auto trade = it->str();
            std::smatch opening;
            REQUIRE(std::regex_search(trade, opening, id_pattern));
            const auto tag = opening.str();
            out += tag.substr(0, tag.rfind("id=\"")) + "id=\"" + id + "\"" +
                   trade.substr(tag.size()) + "\n";
        }
    }
    return out + "</Portfolio>\n";
}

/// The script library the store holds, which is the system tenant's.
ores::ore::domain::script_library_rows script_library_from_store() {
    namespace repo = ores::trading::repository;
    ores::testing::scoped_database_helper h;
    const auto ctx = h.context().with_tenant(ores::utility::uuid::tenant_id::system(), h.db_user());
    ores::ore::domain::script_library_rows rows;
    rows.scripts = repo::script_repository().read_latest(ctx);
    rows.variants = repo::script_variant_repository().read_latest(ctx);
    rows.results = repo::script_result_repository().read_latest(ctx);
    rows.calibrations = repo::script_calibration_repository().read_latest(ctx);
    rows.calibration_strikes = repo::script_calibration_strike_repository().read_latest(ctx);
    rows.coarsenings = repo::script_coarsening_repository().read_latest(ctx);
    return rows;
}

/// The example's configuration, pricing only, so the run builds every trade once.
void stage_input(const fs::path& input) {
    fs::create_directories(input);
    for (const auto& entry :
         fs::directory_iterator(ores::testing::project_root::resolve(example_input)))
        fs::copy_file(entry.path(), input / entry.path().filename());

    const auto ore = input / "ore.xml";
    const std::regex reports(
        R"re(<Analytic type="(cashflow|portfolioDetails)">[\s\S]*?</Analytic>)re");
    ores::platform::filesystem::file::write_content(ore,
                                                    std::regex_replace(text_of(ore), reports, ""));
    ores::platform::filesystem::file::write_content(input / "portfolio.xml", merged_portfolio());

    // The run reads the script library the store holds, not the copy ORE ships,
    // so a script the rows lose fails the run that needs it.
    const auto library = script_library_from_store();
    REQUIRE(library.scripts.size() == 54u);
    ores::platform::filesystem::file::write_content(
        input / "scriptlibrary.xml", ores::ore::domain::write_script_library(library));
}

int run_engine(const std::string& executable, const fs::path& run_dir) {
    namespace bp2 = boost::process::v2;
    std::vector<bp2::environment::key_value_pair> env;
    const bp2::environment::key library_key("LD_LIBRARY_PATH");
    std::string libraries =
        ores::platform::environment::environment::get_value_or_default("ORES_ORE_LIBRARY_PATH", "");
    for (const auto& entry : bp2::environment::current()) {
        if (!libraries.empty() && entry.key() == library_key) {
            libraries += ":" + entry.value().string();
            continue;
        }
        env.emplace_back(entry);
    }
    if (!libraries.empty())
        env.emplace_back("LD_LIBRARY_PATH=" + libraries);

    boost::asio::io_context context;
    bp2::process engine(context,
                        executable,
                        {"Input/ore.xml"},
                        bp2::process_start_dir(run_dir.string()),
                        bp2::process_environment(env),
                        bp2::process_stdio{.in = nullptr, .out = nullptr, .err = nullptr});
    return engine.wait();
}

std::set<std::string> known_errors() {
    std::set<std::string> lines;
    std::istringstream text(text_of(ores::testing::project_root::resolve(known_errors_file)));
    for (std::string line; std::getline(text, line);) {
        if (line.ends_with('\r'))
            line.pop_back();
        if (!line.empty() && !line.starts_with("#"))
            lines.insert(line);
    }
    return lines;
}

/*
 * The kind of an error, from its message alone. A message that names something
 * the run's market or reference data lacks is market data; one that names a
 * pricing engine or builder the configuration lacks is engine configuration.
 * Anything else is the trade's own fault, which no known list may hold.
 */
struct error_pattern {
    std::string_view kind;
    std::regex message;
};

const std::vector<error_pattern>& error_patterns() {
    static const std::vector<error_pattern> patterns = {
        {"market_data", std::regex(R"(^did not find object '.*' of type .* under configuration )")},
        {"market_data", std::regex(R"(^did not find object \S+ in \w+Curves$)")},
        {"market_data", std::regex(R"(^No CreditIndex (reference|constituents) data for )")},
        {"market_data",
         std::regex(
             R"(BondFactory: could not build bond '.*': no reference data given or no suitable builder)")},
        {"engine_configuration",
         std::regex(R"(^No Pricing Engine configuration was provided for trade type )")},
        {"engine_configuration", std::regex(R"(^No EngineBuilder for )")},
    };
    return patterns;
}

std::string kind_of(const std::string& message) {
    for (const auto& pattern : error_patterns())
        if (std::regex_search(message, pattern.message))
            return std::string(pattern.kind);
    return "trade";
}

std::set<std::string> raised_errors(const fs::path& structured_log) {
    using namespace ores::ore::log;
    std::set<std::string> lines;
    for (const auto& event : parse_engine_log_json(text_of(structured_log), "log_structured.json"))
        if (event.scope == "Trade")
            lines.insert(kind_of(event.message) + "\t" + field_of(event, "tradeType") + "\t" +
                         field_of(event, "exceptionType") + "\t" + event.message);
    return lines;
}

std::string lines_of(const std::set<std::string>& lines) {
    std::string out;
    for (const auto& line : lines)
        out += "\n  " + line;
    return out;
}

}
#endif

TEST_CASE("the ORE engine raises only the known errors on the recorded trades",
          "[synthetic][ore][generated][ore_engine][external]") {
#if !defined(__linux__)
    SKIP("The ORE engine is built only for Linux.");
#else
    const auto executable =
        ores::platform::environment::environment::get_value("ORES_ORE_EXECUTABLE");
    if (!executable || executable->empty())
        SKIP("ORES_ORE_EXECUTABLE names no ORE engine.");

    // A failed run leaves its directory behind, so ORE's log can be read.
    const auto run_dir =
        fs::temp_directory_path() / ("ores_generated_trades_ore_engine_" +
                                     std::to_string(ores::platform::process::current_pid()));
    fs::remove_all(run_dir);
    stage_input(run_dir / "Input");

    REQUIRE(run_engine(*executable, run_dir) == 0);
    const auto structured_log = run_dir / "Output" / "log_structured.json";
    REQUIRE(fs::exists(structured_log));

    const auto raised = raised_errors(structured_log);
    const auto known = known_errors();
    std::set<std::string> unexpected;
    std::set<std::string> no_longer_raised;
    std::ranges::set_difference(raised, known, std::inserter(unexpected, unexpected.end()));
    std::ranges::set_difference(
        known, raised, std::inserter(no_longer_raised, no_longer_raised.end()));

    std::set<std::string> of_the_trade;
    for (const auto& line : raised)
        if (line.starts_with("trade\t"))
            of_the_trade.insert(line);

    INFO("raised but not known:" << lines_of(unexpected));
    INFO("known but no longer raised:" << lines_of(no_longer_raised));
    INFO("errors the trade causes, which must be fixed, not listed:" << lines_of(of_the_trade));
    CHECK(unexpected.empty());
    CHECK(no_longer_raised.empty());
    CHECK(of_the_trade.empty());
    fs::remove_all(run_dir);
#endif
}
