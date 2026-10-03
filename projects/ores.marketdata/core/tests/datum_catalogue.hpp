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
#ifndef ORES_MARKETDATA_CORE_TESTS_DATUM_CATALOGUE_HPP
#define ORES_MARKETDATA_CORE_TESTS_DATUM_CATALOGUE_HPP

#include "ores.platform/filesystem/file.hpp"
#include "ores.testing/project_root.hpp"
#include "ores.utility/compression/gzip.hpp"
#include <filesystem>
#include <map>
#include <rfl/json.hpp>
#include <sstream>
#include <stdexcept>
#include <string>
#include <vector>

namespace ores::marketdata::test {

/**
 * @brief One line of the ORE reference catalogue under external/ore/catalogue:
 * a key, whether ORE accepts it, and the fields ORE's parser assigns, every
 * value as text.
 */
using catalogue_line = std::map<std::string, std::string>;

inline std::vector<catalogue_line> catalogue_lines_of(const std::string& content) {
    std::vector<catalogue_line> result;
    std::istringstream stream(content);
    std::string line;
    while (std::getline(stream, line)) {
        if (line.empty())
            continue;
        auto parsed = rfl::json::read<catalogue_line>(line);
        if (!parsed)
            throw std::runtime_error("unreadable catalogue line: " + line);
        result.push_back(std::move(*parsed));
    }
    return result;
}

inline std::filesystem::path catalogue_dir() {
    return ores::testing::project_root::resolve("external/ore/catalogue");
}

/// Every key form ORE's parser documents, with ORE's reading of each.
inline const std::vector<catalogue_line>& catalogue_forms() {
    static const auto result = catalogue_lines_of(
        ores::platform::filesystem::file::read_content(catalogue_dir() / "forms.jsonl"));
    return result;
}

/// Every distinct key in the example corpus, with ORE's reading of each.
inline const std::vector<catalogue_line>& catalogue_corpus() {
    static const auto result = [] {
        const auto compressed =
            ores::platform::filesystem::file::read_content(catalogue_dir() / "corpus.jsonl.gz");
        const auto content = ores::utility::compression::gzip_decompress(compressed);
        return catalogue_lines_of(std::string(content.begin(), content.end()));
    }();
    return result;
}

/// Every accepted form with each of ORE's quote tokens in turn.
inline const std::vector<catalogue_line>& catalogue_quote_matrix() {
    static const auto result = catalogue_lines_of(
        ores::platform::filesystem::file::read_content(catalogue_dir() / "quote_matrix.jsonl"));
    return result;
}

/// Every index form ORE's parseIndex documents, with ORE's reading of each.
inline const std::vector<catalogue_line>& catalogue_index_forms() {
    static const auto result = catalogue_lines_of(
        ores::platform::filesystem::file::read_content(catalogue_dir() / "index_forms.jsonl"));
    return result;
}

/// Every distinct index name the corpus's fixing files carry, with ORE's reading.
inline const std::vector<catalogue_line>& catalogue_index_corpus() {
    static const auto result = catalogue_lines_of(
        ores::platform::filesystem::file::read_content(catalogue_dir() / "index_corpus.jsonl"));
    return result;
}

/// One member of an ORE enum and the key tokens ORE's parser reads as it.
struct enum_member {
    std::string name;
    std::vector<std::string> tokens;
};

/// The members of instrument_types.txt or quote_types.txt.
inline std::vector<enum_member> catalogue_enum(const std::string& file) {
    std::vector<enum_member> result;
    std::istringstream stream(
        ores::platform::filesystem::file::read_content(catalogue_dir() / file));
    std::string line;
    while (std::getline(stream, line)) {
        std::istringstream words(line);
        enum_member m;
        words >> m.name;
        for (std::string token; words >> token;)
            m.tokens.push_back(token);
        if (!m.name.empty())
            result.push_back(std::move(m));
    }
    return result;
}

inline bool accepted_by_ore(const catalogue_line& line) {
    return line.at("accepted") == "true";
}

/// The key with an alias ORE reads replaced by the spelling the codec writes.
inline std::string canonical_key(const std::string& key) {
    const auto first = key.find('/');
    const auto second = key.find('/', first + 1);
    auto type = key.substr(0, first);
    auto quote = key.substr(first + 1, second - first - 1);
    if (type == "FX_SPOT")
        type = "FX";
    if (type == "FX_FWD")
        type = "FXFWD";
    if (quote == "RATE_GVOL")
        quote = "RATE_LNVOL";
    return type + "/" + quote + key.substr(second);
}

}

#endif
