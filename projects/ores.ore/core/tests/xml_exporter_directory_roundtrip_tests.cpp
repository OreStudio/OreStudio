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
#include "ores.ore.core/domain/portfolio_projection.hpp"
#include "ores.ore.core/xml/importer.hpp"
#include "ores.ore.core/xml/roundtrip.hpp"
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
 * Drives the xml facet's round trip over directories under
 * external/ore/examples and asserts that the walk accounts for every file it
 * sees, that each written document reads back as a document of the same kind,
 * and that every mapped trade reaches an output. The per-document suites
 * assert deep field fidelity for the products they name; this suite asserts
 * the walk itself, over the whole corpus, which nothing else does.
 */

namespace {

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
 * shrinks. The list is empty, which is the state to defend: every family writes
 * what the schema requires, and a first entry means a writer regressed.
 */
const std::set<std::string>& known_export_gaps() {
    static const std::set<std::string> gaps = {};
    return gaps;
}

/**
 * @brief Counts the trades a document contributes to the walk's mapped tally.
 *
 * Mirrors the rule the walk applies: an instrument that encodes to monostate
 * is a passthrough and does not count as mapped.
 */
std::size_t count_mapped_trades(const fs::path& document) {
    std::size_t mapped = 0;
    for (const auto& item : ores::ore::xml::importer::import_portfolio_with_context(document)) {
        if (!std::holds_alternative<std::monostate>(item.instrument))
            ++mapped;
    }
    return mapped;
}

/**
 * @brief Reports every document the walk could not convert.
 *
 * Built into one assertion so a failure names all of them at once rather
 * than stopping at the first.
 */
void check_no_conversion_failures(const ores::ore::xml::roundtrip_summary& summary) {
    if (summary.failures.empty())
        return;
    std::string joined;
    for (const auto& failure : summary.failures)
        joined += "\n  " + failure;
    FAIL_CHECK("documents failed to round trip:" << joined);
}

} // namespace

TEST_CASE("roundtrip mirrors every portfolio in the ORE example corpus",
          "[ore][xml][roundtrip][exporter][external]") {
    using ores::platform::filesystem::file;

    const auto input = examples_dir("Products/Example_Trades");
    REQUIRE(fs::exists(input));

    scratch_dir out("ores_ore_directory_roundtrip");
    const auto summary = ores::ore::xml::roundtrip(input, out.path);

    INFO("total=" << summary.total_xml_files << " unsupported=" << summary.unsupported
                  << " failed=" << summary.failed << " written=" << summary.output_files_written
                  << " mapped=" << summary.trades_mapped
                  << " passthrough=" << summary.trades_passthrough);

    // The count pins the corpus the claim is made over: a dropped directory
    // must fail here rather than shrink the proof quietly.
    CHECK(summary.total_xml_files == 132);
    CHECK(summary.output_files_written + summary.unsupported + summary.failed ==
          summary.total_xml_files);
    CHECK(summary.trades_mapped > 0);
    check_no_conversion_failures(summary);

    std::set<std::string> unreadable;
    std::size_t trades_in_outputs = 0;
    for (const auto& entry : fs::recursive_directory_iterator(out.path)) {
        if (!entry.is_regular_file())
            continue;
        if (entry.path().extension() != ".xml")
            continue;

        const auto relative = fs::relative(entry.path(), out.path).string();
        try {
            trades_in_outputs += ores::ore::domain::portfolio_projection::read(
                                     file::read_content(entry.path()), relative)
                                     .trade.size();
        } catch (const std::exception&) {
            unreadable.insert(relative);
        }
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
    INFO("trades_in_outputs=" << trades_in_outputs << " trades_lost_with_gaps="
                              << trades_lost_with_gaps << " mapped=" << summary.trades_mapped);
    CHECK(trades_in_outputs + trades_lost_with_gaps ==
          static_cast<std::size_t>(summary.trades_mapped));
}

TEST_CASE("roundtrip classifies a mixed ORE input directory",
          "[ore][xml][roundtrip][exporter][external]") {
    const auto input = examples_dir("Input");
    REQUIRE(fs::exists(input));

    scratch_dir out("ores_ore_directory_roundtrip_mixed");
    const auto summary = ores::ore::xml::roundtrip(input, out.path);

    INFO("total=" << summary.total_xml_files << " unsupported=" << summary.unsupported
                  << " failed=" << summary.failed << " currencies=" << summary.currency_files
                  << " calendars=" << summary.calendar_files
                  << " conventions=" << summary.convention_files);

    CHECK(summary.output_files_written + summary.unsupported + summary.failed ==
          summary.total_xml_files);
    CHECK(summary.currency_files >= 1);
    CHECK(summary.calendar_files >= 1);
    CHECK(summary.convention_files >= 1);
    check_no_conversion_failures(summary);

    // The corpus carries documents of kinds the walk does not support, so the
    // unsupported counter has to move. A walk that silently ignored them would
    // pass the accounting check above only by counting them as written.
    CHECK(summary.unsupported >= 1);
}
