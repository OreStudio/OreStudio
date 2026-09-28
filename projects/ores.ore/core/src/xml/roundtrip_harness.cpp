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
#include "ores.ore.core/xml/roundtrip_harness.hpp"
#include <algorithm>
#include <format>
#include <system_error>

namespace ores::ore::xml {

namespace {

/// How much of each side a difference report shows around the byte.
constexpr std::size_t window = 40;

} // namespace

std::string first_difference(const std::string& lhs, const std::string& rhs) {
    const auto limit = std::min(lhs.size(), rhs.size());
    std::size_t at = 0;
    while (at < limit && lhs[at] == rhs[at])
        ++at;

    if (at == limit && lhs.size() == rhs.size())
        return {};

    const auto line =
        1 + static_cast<std::size_t>(std::count(lhs.begin(), lhs.begin() + at, '\n'));
    const auto from = at > window ? at - window : 0;

    auto excerpt = [&](const std::string& text) {
        if (at >= text.size())
            return std::string("(end of document)");
        return text.substr(from, std::min(window * 2, text.size() - from));
    };

    return std::format("first difference at byte {} (line {}): imported \"{}\", exported \"{}\"",
                       at,
                       line,
                       excerpt(lhs),
                       excerpt(rhs));
}

std::vector<std::filesystem::path> files_of_kind(const std::string& file_prefix,
                                                 const std::filesystem::path& corpus_root) {
    std::vector<std::filesystem::path> files;
    std::error_code ec;
    for (std::filesystem::recursive_directory_iterator it(corpus_root, ec), end;
         it != end && !ec;
         it.increment(ec)) {
        if (!it->is_regular_file(ec))
            continue;
        const auto& path = it->path();
        if (path.extension() != ".xml")
            continue;
        if (path.stem().string().rfind(file_prefix, 0) != 0)
            continue;
        files.push_back(path);
    }

    std::sort(files.begin(), files.end());
    return files;
}

roundtrip_walk walk_kind(const roundtrip_kind& kind, const std::filesystem::path& corpus_root) {
    roundtrip_walk walk;
    walk.kind = kind.name;

    for (const auto& path : files_of_kind(kind.file_prefix, corpus_root)) {
        ++walk.files;
        const auto outcome = kind.check(path);
        if (outcome.passed)
            ++walk.passed;
        else
            walk.failures.push_back(outcome.detail);
    }
    return walk;
}

}
