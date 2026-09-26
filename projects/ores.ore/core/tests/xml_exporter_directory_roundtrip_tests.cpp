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
#include "ores.ore.core/domain/domain.hpp"
#include "ores.ore.core/xml/exporter.hpp"
#include "ores.ore.core/xml/importer.hpp"
#include "ores.platform/filesystem/file.hpp"
#include "ores.testing/project_root.hpp"
#include <catch2/catch_test_macros.hpp>
#include <cstddef>
#include <filesystem>
#include <set>
#include <string>
#include <variant>
#include <vector>

/**
 * @file xml_exporter_directory_roundtrip_tests.cpp
 * @brief Directory-level round trip over the vendored ORE example corpus.
 *
 * Drives exporter::roundtrip over directories under external/ore/examples and
 * asserts that the walk accounts for every file it sees, that each written
 * document reads back as a document of the same kind, and that every mapped
 * trade reaches an output. The per-document suites assert deep field fidelity
 * for the products they name; this suite asserts the walk itself, over the
 * whole corpus, which nothing else does.
 */

namespace {

const std::string tags("[ore][xml][roundtrip][exporter][external]");
namespace fs = std::filesystem;

fs::path examples_dir(const std::string& relative) {
    return ores::testing::project_root::resolve("external/ore/examples/" + relative);
}

/**
 * @brief A directory under the system temp root, cleared on construction and
 * on destruction, so a failed assertion still leaves nothing behind.
 */
struct scratch_dir {
    fs::path path;

    explicit scratch_dir(const std::string& name)
        : path(fs::temp_directory_path() / name) {
        fs::remove_all(path);
    }

    ~scratch_dir() {
        std::error_code ec;
        fs::remove_all(path, ec);
    }

    scratch_dir(const scratch_dir&) = delete;
    scratch_dir& operator=(const scratch_dir&) = delete;
};

/**
 * @brief Documents the walk writes but which cannot be read back, because the
 * export drops a required child element.
 *
 * The walk reports success for these, so the list is the only record. It is
 * asserted for equality rather than tolerance: a name that starts failing
 * fails this suite, and a name that stops failing also fails it until the list
 * shrinks. The defect is tracked as capture 67277DBB-5ABD-4C36-934B-E97144E8910B.
 */
const std::set<std::string>& known_export_gaps() {
    static const std::set<std::string> gaps = {
        "Commodity_APO_Strip_NYMEX_CSX.xml",
        "Commodity_Basis_Swap_NYMEX_CL.xml",
        "Commodity_Option_Strip_NYMEX_NG.xml",
        "Commodity_Swap_LME_AL_AVG_ALT.xml",
        "Commodity_Swap_NYMEX_A7Q.xml",
        "Commodity_Swaption_NYMEX_NG.xml",
        "Credit_RiskParticipationAgreement_on_CallableSwap.xml",
        "Credit_RiskParticipationAgreement_on_Vanilla_Swap.xml",
        "Exotic_EquityAccumulator_single_name.xml",
        "Exotic_EquityTaRF.xml",
        "Exotic_EquityWorstOfBasketSwap.xml",
        "Exotic_FXWorstOfBasketSwap.xml",
        "Exotic_FxAccumulator.xml",
        "Exotic_KnockOutSwap.xml",
        "Exotic_PerformanceOption_01_COM.xml",
        "Exotic_PerformanceOption_01_FX.xml",
        "FX_WorstOfBasketSwap.xml",
    };
    return gaps;
}

/**
 * @brief Counts the trades a document contributes to the walk's mapped tally.
 *
 * Mirrors the rule exporter::roundtrip applies: an instrument that encodes to
 * monostate is a passthrough and does not count as mapped.
 */
std::size_t count_mapped_trades(const fs::path& document) {
    std::size_t mapped = 0;
    for (const auto& item : ores::ore::xml::importer::import_portfolio_with_context(document)) {
        if (!std::holds_alternative<std::monostate>(item.instrument))
            ++mapped;
    }
    return mapped;
}

} // namespace

TEST_CASE("roundtrip mirrors every portfolio in the ORE example corpus", tags) {
    using ores::ore::xml::exporter;
    using ores::platform::filesystem::file;

    const auto input = examples_dir("Products/Example_Trades");
    REQUIRE(fs::exists(input));

    scratch_dir out("ores_ore_directory_roundtrip");
    const auto summary = exporter::roundtrip(input, out.path);

    INFO("total=" << summary.total_xml_files << " skipped=" << summary.skipped << " written="
                  << summary.output_files_written << " mapped=" << summary.trades_mapped
                  << " passthrough=" << summary.trades_passthrough);

    CHECK(summary.total_xml_files > 0);
    CHECK(summary.output_files_written + summary.skipped == summary.total_xml_files);
    CHECK(summary.trades_mapped > 0);

    std::set<std::string> unreadable;
    std::size_t trades_in_outputs = 0;
    for (const auto& entry : fs::recursive_directory_iterator(out.path)) {
        if (!entry.is_regular_file())
            continue;
        if (entry.path().extension() != ".xml")
            continue;

        const auto relative = fs::relative(entry.path(), out.path).string();
        ores::ore::domain::portfolio p;
        try {
            ores::ore::domain::load_data(file::read_content(entry.path()), p);
        } catch (const std::exception&) {
            unreadable.insert(relative);
            continue;
        }
        trades_in_outputs += p.Trade.size();
    }

    if (unreadable != known_export_gaps()) {
        std::string detail = "the set of documents that fail to re-read changed:";
        for (const auto& name : unreadable)
            if (known_export_gaps().count(name) == 0)
                detail += "\n  newly failing: " + name;
        for (const auto& name : known_export_gaps())
            if (unreadable.count(name) == 0)
                detail += "\n  fixed, so remove it from known_export_gaps(): " + name;
        FAIL_CHECK(detail);
    }

    // Every mapped trade has to come back, except the ones carried by the
    // documents above, which cannot be read at all and so come back as nothing.
    std::size_t trades_lost_with_gaps = 0;
    for (const auto& name : known_export_gaps()) {
        const auto source = input / name;
        REQUIRE(fs::exists(source));
        trades_lost_with_gaps += count_mapped_trades(source);
    }
    CHECK(trades_in_outputs + trades_lost_with_gaps ==
          static_cast<std::size_t>(summary.trades_mapped));
}

TEST_CASE("roundtrip classifies a mixed ORE input directory", tags) {
    using ores::ore::xml::exporter;

    const auto input = examples_dir("Input");
    REQUIRE(fs::exists(input));

    scratch_dir out("ores_ore_directory_roundtrip_mixed");
    const auto summary = exporter::roundtrip(input, out.path);

    INFO("total=" << summary.total_xml_files << " skipped=" << summary.skipped << " currencies="
                  << summary.currency_files << " calendars=" << summary.calendar_files
                  << " conventions=" << summary.convention_files);

    CHECK(summary.output_files_written + summary.skipped == summary.total_xml_files);
    CHECK(summary.currency_files >= 1);
    CHECK(summary.calendar_files >= 1);
    CHECK(summary.convention_files >= 1);

    // The corpus carries documents of kinds the walk does not support, so the
    // skip counter has to move. A walk that silently ignored them would pass
    // the accounting check above only by counting them as written.
    CHECK(summary.skipped >= 1);
}
