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
#include "ores.ore.core/results/metric_reader.hpp"
#include "ores.platform/environment/environment.hpp"
#include "ores.platform/filesystem/file.hpp"
#include "ores.platform/process/pid.hpp"
#include "ores.synthetic.core/service/scripted_twins.hpp"
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
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <cmath>
#include <filesystem>
#include <format>
#include <fstream>
#include <iterator>
#include <map>
#include <optional>
#include <regex>
#include <set>
#include <sstream>
#include <string>
#include <string_view>
#include <tuple>
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
 * The same run values each scripted twin and its native trade, and each pair
 * must agree, or its trade type must be on the known list of the pairs that do
 * not, with the reason.
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
const std::string twin_differences_file =
    "projects/ores.synthetic/core/tests/data/generated_trades_twin_differences.txt";
const std::string twin_values_file =
    "projects/ores.synthetic/core/tests/data/generated_trades_twin_values.tsv";
const std::string example_input = "external/ore/examples/Products/Input";

std::string text_of(const fs::path& path) {
    return ores::platform::filesystem::file::read_content(path);
}

/// A recorded trade: its type and the id it is recorded with.
struct recorded_trade {
    std::string trade_type;
    std::string id;
};

/// The merged portfolio, and each trade it holds by its id in the run.
struct merged_trades {
    std::string portfolio;
    std::map<std::string, recorded_trade> trades;
};

/// Every recorded trade, each renamed for its file so the merged ids are unique.
merged_trades merged_portfolio() {
    std::vector<fs::path> files;
    for (const auto& entry :
         fs::directory_iterator(ores::testing::project_root::resolve(recorded_dir)))
        if (entry.path().extension() == ".xml")
            files.push_back(entry.path());
    std::sort(files.begin(), files.end());

    const std::regex trade_pattern(R"(<Trade\b[\s\S]*?</Trade>)");
    const std::regex id_pattern(R"re(^<Trade\b[^>]*?\bid="([^"]*)")re");
    const std::regex type_pattern(R"(<TradeType>(\w+)</TradeType>)");
    merged_trades out;
    out.portfolio = "<?xml version=\"1.0\"?>\n<Portfolio>\n";
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
            std::smatch type;
            REQUIRE(std::regex_search(trade, type, type_pattern));
            out.trades[id] = {.trade_type = type.str(1), .id = opening.str(1)};
            const auto tag = opening.str();
            out.portfolio += tag.substr(0, tag.rfind("id=\"")) + "id=\"" + id + "\"" +
                             trade.substr(tag.size()) + "\n";
        }
    }
    out.portfolio += "</Portfolio>\n";
    return out;
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
std::map<std::string, recorded_trade> stage_input(const fs::path& input) {
    fs::create_directories(input);
    for (const auto& entry :
         fs::directory_iterator(ores::testing::project_root::resolve(example_input)))
        fs::copy_file(entry.path(), input / entry.path().filename());

    const auto ore = input / "ore.xml";
    const std::regex reports(
        R"re(<Analytic type="(cashflow|portfolioDetails)">[\s\S]*?</Analytic>)re");
    ores::platform::filesystem::file::write_content(ore,
                                                    std::regex_replace(text_of(ore), reports, ""));
    auto merged = merged_portfolio();
    ores::platform::filesystem::file::write_content(input / "portfolio.xml", merged.portfolio);

    // The run reads the script library the store holds, not the copy ORE ships,
    // so a script the rows lose fails the run that needs it.
    const auto library = script_library_from_store();
    REQUIRE(library.scripts.size() == 54u);
    ores::platform::filesystem::file::write_content(
        input / "scriptlibrary.xml", ores::ore::domain::write_script_library(library));
    return std::move(merged.trades);
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

/// What one ORE run over the recorded trades leaves behind.
struct engine_run {
    fs::path dir;
    int exit_code = -1;
    std::string structured_log;
    std::string npv_report;
    std::map<std::string, recorded_trade> trades;
};

/// The one ORE run every test here reads, made on first use.
const engine_run& the_engine_run(const std::string& executable) {
    static const engine_run run = [&] {
        engine_run out;
        // A failed run leaves its directory behind, so ORE's output can be read;
        // a run that succeeds is held in memory and its directory removed.
        out.dir =
            fs::temp_directory_path() / ("ores_generated_trades_ore_engine_" +
                                         std::to_string(ores::platform::process::current_pid()));
        fs::remove_all(out.dir);
        out.trades = stage_input(out.dir / "Input");
        out.exit_code = run_engine(executable, out.dir);
        if (const auto log = out.dir / "Output" / "log_structured.json"; fs::exists(log))
            out.structured_log = text_of(log);
        if (const auto npv = out.dir / "Output" / "npv.csv"; fs::exists(npv))
            out.npv_report = text_of(npv);
        if (out.exit_code == 0 && !out.structured_log.empty() && !out.npv_report.empty())
            fs::remove_all(out.dir);
        return out;
    }();
    return run;
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
        {"market_data", std::regex(R"(needs credit index reference data for ID )")},
        {"market_data",
         std::regex(
             R"(BondFactory: could not build bond '.*': no reference data given or no suitable builder)")},
        // A bond that names its security with no reference data for it in the
        // run, as a CBO's basket bond does, states none of the security's terms.
        {"market_data",
         std::regex(R"(^BondData invalid: missing .* - check if reference data is set up for )")},
        /*
         * A CBO takes its structure from the reference data its structure id
         * names. When the run holds none, ORE lists every structure element as
         * missing; a structure missing only some is the trade's own fault. The
         * list is the one ORE 3d75a6908 states, in cbo.cpp.
         */
        {"market_data",
         std::regex(R"(^CBO \S+ expects SeniorFee SubordinatedFee EquityKicker FeeDayCounter )"
                    R"(Currency DayCounter PaymentConvention ScheduleData BondBasketData )"
                    R"(TrancheData elements$)")},
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

std::set<std::string> raised_errors(const std::string& structured_log) {
    using namespace ores::ore::log;
    std::set<std::string> lines;
    for (const auto& event : parse_engine_log_json(structured_log, "log_structured.json"))
        if (event.scope == "Trade")
            lines.insert(kind_of(event.message) + "\t" + field_of(event, "tradeType") + "\t" +
                         field_of(event, "exceptionType") + "\t" + event.message);
    return lines;
}

/*
 * Rewrites the known list as the errors the run raised, keeping the comments
 * that head the file. A trade's own error is never written: it must be fixed,
 * so the run still fails on it.
 */
void record_known_errors(const std::set<std::string>& raised) {
    const auto path = ores::testing::project_root::resolve(known_errors_file);
    std::string out;
    std::istringstream text(text_of(path));
    for (std::string line; std::getline(text, line) && line.starts_with("#");)
        out += line + "\n";
    for (const auto& line : raised)
        if (!line.starts_with("trade\t"))
            out += line + "\n";
    std::ofstream file(path, std::ios::binary);
    file << out;
    REQUIRE(file.good());
}

std::string lines_of(const std::set<std::string>& lines) {
    std::string out;
    for (const auto& line : lines)
        out += "\n  " + line;
    return out;
}

/// The value ORE gives one trade; a trade it could not build has none.
struct valuation {
    std::optional<double> npv;
    std::string currency;
};

/// A native trade and its scripted twin, as the run values them.
struct twin_pair {
    std::string trade_type;
    std::string native_id;
    valuation native;
    valuation twin;
};

/*
 * Two values agree when they differ by no more than a millionth of the larger,
 * or by a cent where both are near zero. Both trades price on the same engines
 * and inputs, so a pair that agrees agrees to the cent.
 */
constexpr double relative_tolerance = 1e-6;
constexpr double absolute_floor = 0.01;

bool agrees(const twin_pair& pair) {
    if (!pair.native.npv || !pair.twin.npv || pair.native.currency != pair.twin.currency)
        return false;
    const auto a = *pair.native.npv;
    const auto b = *pair.twin.npv;
    return std::abs(a - b) <=
           std::max(relative_tolerance * std::max(std::abs(a), std::abs(b)), absolute_floor);
}

/// Each trade's value by its id in the run. ORE names a trade it could not build Failed.
std::map<std::string, valuation> valuations_of(const std::string& npv_report) {
    const auto read = ores::ore::results::read_metrics_for_file(npv_report, "npv.csv");
    INFO("refusals:" << lines_of({read.refusals.begin(), read.refusals.end()}));
    REQUIRE(read.refusals.empty());
    std::map<std::string, valuation> out;
    for (const auto& metric : read.metrics) {
        if (metric.measure != "NPV")
            continue;
        const auto type = std::ranges::find_if(
            metric.qualifiers, [](const auto& q) { return q.first == "TradeType"; });
        const bool failed = type != metric.qualifiers.end() && type->second == "Failed";
        out[metric.subject] = {.npv = failed ? std::nullopt : std::optional{metric.value},
                               .currency = metric.currency};
    }
    return out;
}

/// Every recorded native trade whose twin is recorded too, with the two values.
std::vector<twin_pair> twin_pairs(const engine_run& run) {
    const auto values = valuations_of(run.npv_report);
    const auto value_of = [&](const std::string& run_id) {
        const auto it = values.find(run_id);
        return it == values.end() ? valuation{} : it->second;
    };
    std::map<std::string, std::string> scripted;
    for (const auto& [run_id, trade] : run.trades)
        if (trade.trade_type == "ScriptedTrade")
            scripted[trade.id] = run_id;

    std::vector<twin_pair> out;
    for (const auto& [run_id, trade] : run.trades) {
        if (trade.trade_type == "ScriptedTrade")
            continue;
        const auto twin_id = boost::uuids::to_string(ores::synthetic::service::twin_id_for(
            boost::uuids::string_generator()(trade.id), trade.trade_type));
        const auto twin = scripted.find(twin_id);
        if (twin == scripted.end())
            continue;
        out.push_back({.trade_type = trade.trade_type,
                       .native_id = trade.id,
                       .native = value_of(run_id),
                       .twin = value_of(twin->second)});
    }
    std::ranges::sort(out, [](const auto& l, const auto& r) {
        return std::tie(l.trade_type, l.native_id) < std::tie(r.trade_type, r.native_id);
    });
    return out;
}

/// The known list: each trade type whose pairs do not agree, and the reason.
std::map<std::string, std::string> twin_differences() {
    std::map<std::string, std::string> out;
    std::istringstream text(text_of(ores::testing::project_root::resolve(twin_differences_file)));
    for (std::string line; std::getline(text, line);) {
        if (line.ends_with('\r'))
            line.pop_back();
        if (line.empty() || line.starts_with("#"))
            continue;
        const auto tab = line.find('\t');
        REQUIRE(tab != std::string::npos);
        out[line.substr(0, tab)] = line.substr(tab + 1);
    }
    return out;
}

std::string figure_of(const valuation& v) {
    return v.npv ? std::format("{:.2f}", *v.npv) : std::string("unvalued");
}

/// Writes each pair's two values and their difference, for a reader to review.
void record_twin_values(const std::vector<twin_pair>& pairs) {
    std::string out =
        "# Each scripted twin's value beside its native trade's, from the ORE run of\n"
        "# the recorded trades. ORES_UPDATE_GENERATED_TRADES=1 rewrites it.\n"
        "trade_type\tnative_id\tnative_npv\tnative_currency\ttwin_npv\t"
        "twin_currency\tdifference\tagrees\n";
    for (const auto& p : pairs) {
        const auto difference = p.native.npv && p.twin.npv ?
                                    std::format("{:.2f}", *p.twin.npv - *p.native.npv) :
                                    std::string("-");
        out += p.trade_type + "\t" + p.native_id + "\t" + figure_of(p.native) + "\t" +
               p.native.currency + "\t" + figure_of(p.twin) + "\t" + p.twin.currency + "\t" +
               difference + "\t" + (agrees(p) ? "yes" : "no") + "\n";
    }
    std::ofstream file(ores::testing::project_root::resolve(twin_values_file), std::ios::binary);
    file << out;
    REQUIRE(file.good());
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

    const auto& run = the_engine_run(*executable);
    INFO("run directory: " << run.dir.string());
    REQUIRE(run.exit_code == 0);
    REQUIRE_FALSE(run.structured_log.empty());

    const auto raised = raised_errors(run.structured_log);
    if (ores::platform::environment::environment::get_value("ORES_UPDATE_GENERATED_TRADES") == "1")
        record_known_errors(raised);
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
#endif
}

TEST_CASE("each scripted twin values as its native trade does, or is known to differ",
          "[synthetic][ore][generated][ore_engine][external]") {
#if !defined(__linux__)
    SKIP("The ORE engine is built only for Linux.");
#else
    const auto executable =
        ores::platform::environment::environment::get_value("ORES_ORE_EXECUTABLE");
    if (!executable || executable->empty())
        SKIP("ORES_ORE_EXECUTABLE names no ORE engine.");

    const auto& run = the_engine_run(*executable);
    INFO("run directory: " << run.dir.string());
    REQUIRE(run.exit_code == 0);
    REQUIRE_FALSE(run.npv_report.empty());

    const auto pairs = twin_pairs(run);
    REQUIRE_FALSE(pairs.empty());
    if (ores::platform::environment::environment::get_value("ORES_UPDATE_GENERATED_TRADES") == "1")
        record_twin_values(pairs);

    std::set<std::string> differing;
    std::string report;
    for (const auto& p : pairs) {
        report += "\n  " + p.trade_type + " " + p.native_id + ": native " + figure_of(p.native) +
                  " " + p.native.currency + ", twin " + figure_of(p.twin) + " " + p.twin.currency;
        if (!agrees(p))
            differing.insert(p.trade_type);
    }
    const auto known = twin_differences();
    std::set<std::string> listed;
    for (const auto& [type, reason] : known) {
        listed.insert(type);
        INFO("the known difference of " << type << " states no reason");
        CHECK_FALSE(reason.empty());
    }
    std::set<std::string> unexplained;
    std::set<std::string> now_agreeing;
    std::ranges::set_difference(differing, listed, std::inserter(unexplained, unexplained.end()));
    std::ranges::set_difference(listed, differing, std::inserter(now_agreeing, now_agreeing.end()));

    INFO("pairs:" << report);
    INFO("differ but not known:" << lines_of(unexplained));
    INFO("known but now agree, so remove them:" << lines_of(now_agreeing));
    CHECK(unexplained.empty());
    CHECK(now_agreeing.empty());
#endif
}
