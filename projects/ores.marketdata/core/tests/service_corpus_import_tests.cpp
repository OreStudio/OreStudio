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
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.core/repository/market_observations_repository.hpp"
#include "ores.marketdata.core/repository/market_series_repository.hpp"
#include "ores.marketdata.core/service/import_service.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.platform/filesystem/file.hpp"
#include "ores.testing/database_helper.hpp"
#include "ores.testing/project_root.hpp"
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <cstddef>
#include <filesystem>
#include <functional>
#include <map>
#include <sstream>
#include <string>
#include <string_view>
#include <vector>

// The oresmd coverage test walks the ORE corpus and asserts that every key can
// be named and read back -- in memory. This takes the same corpus all the way
// into the database: it imports real files through the import service and then
// reads the rows out again, so that "every sample file reads into the database"
// is a measurement rather than an assumption.
//
// The default case imports a bounded, deterministic sample; a corpus-wide run
// is available under the [.corpus-full] hidden tag, which Catch2 does not run
// unless it is named.

namespace {

const std::string_view test_suite("ores.marketdata.core.tests");
const std::string tags("[corpus][import_service]");

/// A single (date, key, value) triple, as the file carries it.
struct corpus_line {
    std::string date;
    std::string key;
    std::string value;
};

/// The same file-selection rules the coverage test uses.
bool is_market_payload(const std::string& path) {
    const auto name = std::filesystem::path(path).filename().string();
    if (name.find("fixing") != std::string::npos)
        return false;
    if (name.find("market") == std::string::npos)
        return false;
    if (!name.ends_with(".txt") && !name.ends_with(".csv"))
        return false;
    if (path.find("ExpectedOutput") != std::string::npos)
        return false;
    return name.rfind("MD_", 0) != 0;
}

/// Every payload line, skipping comments and blanks.
///
/// ORE text separates with whitespace and CSV with commas, and a line may put
/// the value in either position after the key; both spellings are handled.
std::vector<corpus_line> lines_of(const std::string& content) {
    std::vector<corpus_line> out;
    std::istringstream stream(content);
    std::string line;
    while (std::getline(stream, line)) {
        const auto start = line.find_first_not_of(" \t\r\n");
        if (start == std::string::npos || line[start] == '#')
            continue;
        std::replace(line.begin(), line.end(), ',', ' ');

        std::istringstream fields(line);
        corpus_line parsed;
        std::string key;
        std::string value;
        if (!(fields >> parsed.date >> key >> value))
            continue;
        parsed.date.erase(std::remove(parsed.date.begin(), parsed.date.end(), '-'),
                          parsed.date.end());
        parsed.key = key;
        parsed.value = value;
        out.push_back(std::move(parsed));
    }
    return out;
}

std::vector<std::filesystem::path> market_payloads() {
    std::vector<std::filesystem::path> found;
    const auto root = ores::testing::project_root::resolve("external/ore/examples");
    for (const auto& entry : std::filesystem::recursive_directory_iterator(root)) {
        if (entry.is_regular_file() && is_market_payload(entry.path().string()))
            found.push_back(entry.path());
    }
    std::sort(found.begin(), found.end());
    return found;
}

/// The smallest payloads, until @p budget lines have been taken.
///
/// Bounded rather than capped at a file count, so the default case costs a
/// predictable number of database writes however the corpus grows.
std::vector<std::filesystem::path> sample_payloads(std::size_t budget) {
    const auto all = market_payloads();
    std::vector<std::pair<std::size_t, std::filesystem::path>> sized;
    for (const auto& p : all) {
        const auto content = ores::platform::filesystem::file::read_content(p);
        sized.emplace_back(lines_of(content).size(), p);
    }
    std::sort(sized.begin(), sized.end(),
              [](const auto& a, const auto& b) {
                  return a.first != b.first ? a.first < b.first : a.second < b.second;
              });
    std::vector<std::filesystem::path> chosen;
    std::size_t taken = 0;
    for (const auto& [count, path] : sized) {
        if (count == 0)
            continue;
        if (taken + count > budget && !chosen.empty())
            break;
        chosen.push_back(path);
        taken += count;
    }
    return chosen;
}

/// The source tag this helper stamps on a file's rows.
///
/// The test database helper hands every helper the same tenant -- it is the one
/// named by the environment, not a per-instance one -- so a file's rows have to
/// be told apart from the files imported before it some other way. The source
/// column is exactly that: the import stamps it from the request, and nothing
/// else in the run carries this tag.
std::string source_tag_for(const std::filesystem::path& path) {
    auto name = path.filename().string();
    std::replace(name.begin(), name.end(), '.', '_');
    return "test.corpus." + name + "." + std::to_string(std::hash<std::string>{}(
                                                          path.string()) % 100000);
}

/// Imports @p path and asserts the rows it wrote match the file.
///
/// The check is on the multiset of values rather than on a key-by-key walk: a
/// corpus key splits into the stored (type, metric, qualifier) columns by rules
/// that differ per asset class, so rebuilding the key to look a series up would
/// test the split as much as the import. Comparing the values the file carried
/// against the values the rows hold proves the same thing without that.
///
/// Returns the number of observations the file carried, so a caller can total
/// them across files.
std::size_t import_and_verify(const std::filesystem::path& path,
                              ores::marketdata::service::import_service& svc,
                              ores::testing::database_helper& h) {


    using namespace ores::marketdata;
    const auto content = ores::platform::filesystem::file::read_content(path);
    const auto expected = lines_of(content);
    const auto tag = source_tag_for(path);

    messaging::import_market_data_request req;
    req.market_data_content = content;
    req.source = tag;
    const auto resp = svc.import(req);

    INFO("file: " << path.string());
    REQUIRE(resp.success);
    REQUIRE(resp.observation_count == static_cast<int>(expected.size()));
    CHECK(resp.errors.empty());

    std::vector<std::string> wanted;
    wanted.reserve(expected.size());
    for (const auto& l : expected)
        wanted.push_back(l.value);

    repository::market_series_repository series_repo;
    repository::market_observations_repository obs_repo;
    std::vector<std::string> stored;
    for (const auto& s : series_repo.read_latest(h.context()))
        for (const auto& o : obs_repo.read_latest(h.context(), s.id))
            if (o.source == tag)
                stored.push_back(o.value);

    std::sort(wanted.begin(), wanted.end());
    std::sort(stored.begin(), stored.end());
    CHECK(stored == wanted);

    return expected.size();
}

} // namespace

using namespace ores::logging;
using ores::marketdata::service::import_service;
using ores::testing::database_helper;

TEST_CASE("every_line_of_a_sampled_corpus_file_reaches_the_database", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    ores::nats::service::nats_client auth_nats;
    import_service svc(h.context(), auth_nats);

    const auto sample = sample_payloads(50);
    REQUIRE_FALSE(sample.empty());

    std::size_t total = 0;
    for (const auto& path : sample)
        total += import_and_verify(path, svc, h);

    BOOST_LOG_SEV(lg, info) << "Verified " << total << " line(s) over " << sample.size()
                            << " corpus file(s).";
    CHECK(total > 0);
}

TEST_CASE("every_line_of_the_whole_corpus_reaches_the_database", "[.][corpus-full]") {
    auto lg(make_logger(test_suite));

    database_helper h;
    ores::nats::service::nats_client auth_nats;
    import_service svc(h.context(), auth_nats);

    const auto all = market_payloads();
    REQUIRE_FALSE(all.empty());

    std::size_t total = 0;
    for (const auto& path : all)
        total += import_and_verify(path, svc, h);

    BOOST_LOG_SEV(lg, info) << "Verified " << total << " line(s) over " << all.size()
                            << " corpus file(s).";
    CHECK(total > 0);
}
